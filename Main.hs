import Protolude hiding (get)

import qualified Network.Wreq as W
import           Control.Lens hiding (element)
-- import           Control.Parallel.Strategies
import           Data.Csv
import qualified Data.HashMap.Strict as M
import           Data.String.Here.Interpolated
import           Data.Time.Clock
import qualified Data.Vector as V
import           Text.XML hiding (readFile)
import           Text.XML.Cursor
-- import           Text.InterpolatedString.Perl6
import           Web.Scotty

type RouteId = Int
type StopId  = Int
type Region  = Int
--type Prediction = (DiffTime, RouteId)
data Waypoint = Waypoint {
    wpStop :: StopId
  , wpRoute :: RouteId
  }

main :: IO ()
main = do
  connData <- toS <$> readFile "connections.csv"
  let connections =
        case decode NoHeader connData of
          Left err -> error $ toS err
          Right v ->
            v :: V.Vector (Region, Region, StopId, RouteId)

  scotty 3000 $ do
    get "/" $ do
      times <- liftIO $ predictedArrivals 27 3612
      let list = show times :: Text
      text [i|
        <?xml version="1.0" encoding="UTF-8"?>
        <Response>
          <Say>
            ${list}
          </Say>
        </Response>
      |]

predictedArrivals :: RouteId -> StopId -> IO [DiffTime]
predictedArrivals route stop = do
  payload <- W.get . toS $ busArrivalUrl route stop
  let cursor = fromDocument . parseText_ def . toS $
                 payload ^. W.responseBody
      predictions = cursor $// element "prediction"
                           >=> attribute "seconds"

  return $ secondsToDiffTime
    . fromMaybe 0 . readMaybe . toS <$> predictions

 where
  busArrivalUrl :: RouteId -> StopId -> Text
  busArrivalUrl r s =
    "http://webservices.nextbus.com/service/publicXMLFeed?command=predictions&a=sf-muni&r="<>show r<>"&s="<>show s
