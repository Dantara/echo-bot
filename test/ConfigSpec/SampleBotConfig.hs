module ConfigSpec.SampleBotConfig where

import           Language.Haskell.TH


getSampleConfig :: Q Exp
getSampleConfig = LitE . StringL <$> runIO (readFile configPath)
  where
    configPath = "test/ConfigSpec/sample_bot.config"
