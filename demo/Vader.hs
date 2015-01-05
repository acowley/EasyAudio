import Control.Monad (when)
import EasyAudio
import Graphics.UI.SDL as SDL

main :: IO ()
main = do r <- SDL.init SDL.SDL_INIT_AUDIO
          when (r < 0) (error "Error initializing SDL")
          putStrLn "Initialized SDL"
          (loadClip', cleanup) <- easyAudio
          vader <- loadClip' "swvader04.wav"
          vader
          cleanup
          putStrLn "Quitting SDL"
          SDL.quit
