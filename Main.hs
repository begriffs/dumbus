-- import qualified Network.Wreq    as W
-- import qualified Text.XML        as X
-- import qualified Text.XML.Cursor as X
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
