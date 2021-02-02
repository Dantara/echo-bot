module Bot.VK.Sender.Keyboard where

import           Language.Haskell.TH


getKeyboard :: Q Exp
getKeyboard = LitE . StringL <$> runIO (readFile keyboardPath)
  where
    keyboardPath = "src/Bot/VK/Sender/Keyboard.json"
