module Bot.VK.Fetcher.LongPollServerSpec.SampleLongPollServer where

import           Language.Haskell.TH


getSampleLPS :: Q Exp
getSampleLPS = LitE . StringL <$> runIO (readFile configPath)
  where
    configPath = "test/Bot/VK/Fetcher/\
                 \LongPollServerSpec/sample_long_poll_server.json"
