import Control.Monad (when)
import EasyAudio
import Graphics.UI.SDL as SDL
import Paths_EasyAudio
import System.FilePath

main :: IO ()
main = do r <- SDL.init SDL.SDL_INIT_AUDIO
          when (r < 0) (error "Error initializing SDL")
          putStrLn "Initialized SDL"
          (loadClip, cleanup) <- easyAudio
          audioFilePath <- getDataFileName ("demo" </> "bling.wav")
          vader <- loadClip audioFilePath
          vader
          cleanup
          putStrLn "Quitting SDL"
          SDL.quit
