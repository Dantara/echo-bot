module Bot.VK.Sender.Keyboard where

import           Language.Haskell.TH (Exp (LitE), Lit (StringL), Q, runIO)


getKeyboard :: Q Exp
getKeyboard = LitE . StringL <$> runIO (readFile keyboardPath)
  where
    keyboardPath = "src/Bot/VK/Sender/Keyboard.json"
