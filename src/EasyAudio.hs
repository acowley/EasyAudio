-- | A very basic audio playback library leveraging SDL2.
module EasyAudio (easyAudio) where
import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad (void, when)
import Data.Foldable (traverse_)
import Data.Word
import qualified Foreign.C.String as F
import Foreign.C.Types (CInt)
import qualified Foreign.Concurrent as FC
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, mallocForeignPtrBytes)
import qualified Foreign.Marshal.Alloc as F
import qualified Foreign.Marshal.Utils as F
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Audio

-- | An audio buffer along with a current playback position.
data FeederState = FeederState{ _audioData :: ForeignPtr Word8
                              , _audioLen  :: Word32
                              , audioPos   :: Word32 }

-- | The callback function SDL will invoke to push more data into the
-- audio device.
audioFeeder :: MVar (FeederState, IO ())
            -> Ptr ()
            -> Ptr Word8
            -> CInt
            -> IO ()
audioFeeder mv _ buf sz = tryTakeMVar mv >>= traverse_ aux
  where aux (st@(FeederState sound sndLen pos), sig) =
            let n = min (sndLen - pos) (fromIntegral sz)
            in if n == 0
               then sig
               else do withForeignPtr sound $ \soundPtr ->
                         F.copyBytes buf
                                     (plusPtr soundPtr (fromIntegral pos))
                                     (fromIntegral n)
                       void $ tryPutMVar mv (st { audioPos = pos + n }, sig)

-- | Information about an open audio device, including a mechanism to
-- communicate with the callback function that loads data.
data DeviceInfo = DeviceInfo { deviceInfoID     :: SDL.AudioDeviceID
                             , _deviceInfoSpec  :: SDL.AudioSpec
                             , _deviceInfoQueue :: MVar (FeederState, IO ()) }

-- | Open an audio device with the given desired spec.
prepAudioDevice :: SDL.AudioSpec -> IO DeviceInfo
prepAudioDevice s = F.with s $ \srcSpecPtr ->
                      F.alloca $ \dstSpecPtr -> do
                        q <- newEmptyMVar
                        cb <- SDL.mkAudioCallback $ audioFeeder q
                        poke srcSpecPtr s { SDL.audioSpecCallback = cb }
                        DeviceInfo
                          <$> openAudioDevice nullPtr 0 srcSpecPtr dstSpecPtr
                                              SDL.SDL_AUDIO_ALLOW_ANY_CHANGE
                          <*> peek dstSpecPtr
                          <*> pure q

-- | Determine if conversion between two specs is necessary.
specsCompatible :: SDL.AudioSpec -> SDL.AudioSpec -> Bool
specsCompatible (SDL.AudioSpec freq1 fmt1 chan1 _ _ _ _ _)
                (SDL.AudioSpec freq2 fmt2 chan2 _ _ _ _ _)
                    = freq1 == freq2 && fmt1 == fmt2 && chan1 == chan2

-- | Helper to throw an SDL error message if something has gone wrong.
checkError :: String -> IO CInt -> IO ()
checkError msg m = do r <- m
                      when (r < 0)
                           (SDL.getError >>= F.peekCString >>= error . aux)
  where aux = (("Error " ++ msg) ++)

-- | Info about a loaded WAV file: spec, data, length (in bytes).
data AudioClip = AudioClip SDL.AudioSpec (Ptr Word8) Word32

-- | Convert an 'AudioClip' to a destination 'SDL.AudioSpec' if
-- conversion is necessary. Returns a buffer of sound data with the
-- desired spec and registered with the garbage collector, paired with
-- the length of that buffer in bytes.
matchAudioSpecs :: SDL.AudioSpec -> AudioClip -> IO (ForeignPtr Word8, Word32)
matchAudioSpecs dst@(SDL.AudioSpec freqdst fmtdst chandst _ _ _ _ _)
                (AudioClip src@(SDL.AudioSpec freqsrc fmtsrc chansrc _ _ _ _ _)
                           sndBuf
                           len)
    | specsCompatible dst src = fmap (\p -> (p,len))
                                     (FC.newForeignPtr sndBuf (freeWAV sndBuf))
    | otherwise = F.alloca $ \cvtPtr -> do
                    checkError "preparing audio conversion" $
                      buildAudioCVT cvtPtr fmtsrc chansrc freqsrc
                                           fmtdst chandst freqdst
                    cvt <- peek cvtPtr
                    fp <- mallocForeignPtrBytes . fromIntegral $
                          len * fromIntegral (SDL.audioCVTLenMult cvt)
                    withForeignPtr fp $ \bufPtr -> do
                      F.copyBytes bufPtr sndBuf (fromIntegral len)
                      freeWAV sndBuf
                      poke cvtPtr cvt { SDL.audioCVTLen = fromIntegral len
                                      , SDL.audioCVTBuf = bufPtr }
                      checkError "converting audio" $ convertAudio cvtPtr
                      cvt' <- peek cvtPtr
                      fp' <- mallocForeignPtrBytes . fromIntegral $
                             SDL.audioCVTLenCvt cvt'
                      withForeignPtr fp' $ \dstPtr ->
                        F.copyBytes dstPtr bufPtr . fromIntegral $
                          SDL.audioCVTLenCvt cvt'
                      return (fp', fromIntegral $ SDL.audioCVTLenCvt cvt')

-- | Load a WAV file.
loadClip :: FilePath -> IO AudioClip
loadClip f =
    F.withCString f $ \file ->
      F.alloca $ \specPtr ->
      F.alloca $ \soundPtr ->
      F.alloca $ \soundLenPtr ->
        do wr <- loadWAV file specPtr soundPtr soundLenPtr
           when (wr == nullPtr) (error $ "Error Loading WAV: "++f)
           AudioClip <$> peek specPtr <*> peek soundPtr <*> peek soundLenPtr

-- | Initializes EasyAudio. Returns a function for loading sound
-- clips, and an action for closing the audio device. The clip loading
-- function returns a blocking action for playing the loaded audio
-- clip.
easyAudio :: IO (FilePath -> IO (IO ()), IO ())
easyAudio =
    do prevSpec <- newMVar Nothing
       let loadAudioFile f =
             do d <- takeMVar prevSpec
                ac@(AudioClip spec _ _) <- loadClip f
                (DeviceInfo dev spec' q, sig) <- case d of
                  Nothing -> do r <- prepAudioDevice spec
                                let sig = pauseAudioDevice (deviceInfoID r) 1
                                putMVar prevSpec (Just (r, sig))
                                return (r, sig)
                  Just r -> return r
                (buf', len') <- matchAudioSpecs spec' ac
                return $ do putMVar q (FeederState buf' len' 0, sig)
                            pauseAudioDevice dev 0
                            waitForPause dev
                            
           cleanup = takeMVar prevSpec
                     >>= traverse_ (closeAudioDevice . deviceInfoID . fst)
       return (loadAudioFile, cleanup)
  where waitForPause d = do s <- getAudioDeviceStatus d
                            when (s == SDL.SDL_AUDIO_PLAYING) $ do
                              SDL.delay 100
                              waitForPause d
