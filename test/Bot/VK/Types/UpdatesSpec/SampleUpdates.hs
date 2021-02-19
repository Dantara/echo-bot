module Bot.VK.Types.UpdatesSpec.SampleUpdates where

import           Language.Haskell.TH


getSampleUpdates :: Q Exp
getSampleUpdates = LitE . StringL <$> runIO (readFile configPath)
  where
    configPath = "test/Bot/VK/Types/\
                 \UpdatesSpec/sample_updates.json"
