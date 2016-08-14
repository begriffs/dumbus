import Protolude hiding (get)

import qualified Network.Wreq as W
import           Control.Lens hiding (element)
import           Data.Time.Clock
import           Text.XML
import           Text.XML.Cursor
-- import           Text.InterpolatedString.Perl6
import           Web.Scotty

type RouteId = Int
type StopId  = Int

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    times <- liftIO $ predictedArrivals 27 3612
    text . show $ times


predictedArrivals :: RouteId -> StopId -> IO [DiffTime]
predictedArrivals route stop = do
  payload <- W.get . toS $ busArrivalUrl route stop
  let cursor = fromDocument . parseText_ def . toS $
                 payload ^. W.responseBody
      predictions = cursor $// element "prediction"
                           >=> attribute "seconds"

  return $ secondsToDiffTime .  fromMaybe 0
                        . readMaybe . toS
                        <$> predictions

 where
  busArrivalUrl :: RouteId -> StopId -> Text
  busArrivalUrl r s =
    "http://webservices.nextbus.com/service/publicXMLFeed?command=predictions&a=sf-muni&r="<>show r<>"&s="<>show s
