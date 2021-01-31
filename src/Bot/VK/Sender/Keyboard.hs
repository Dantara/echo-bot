module Bot.VK.Sender.Keyboard where

import           Data.Text           (Text)
import qualified Data.Text.IO        as Text (readFile)
import           Language.Haskell.TH


getKeyboard :: Q Exp
getKeyboard = LitE . StringL <$> runIO (readFile keyboardPath)
  where
    keyboardPath = "src/Bot/VK/Sender/Keyboard.json"
