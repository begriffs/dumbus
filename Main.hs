import Protolude hiding (get)

import qualified Network.Wreq as W
import           Control.Lens hiding (element)
import qualified Control.Monad.Parallel as P
import           Data.Csv
import           Data.String.Here.Interpolated
import qualified Data.Vector as V
import           Text.XML hiding (readFile)
import           Text.XML.Cursor
import           Web.Scotty

type RouteId = Int
type StopId  = Int
type Region  = Int
type Minute  = Int
-- data Waypoint = Waypoint {
--     wpStop :: StopId
--   , wpRoute :: RouteId
--   }

main :: IO ()
main = do
  connData <- toS <$> readFile "connections.csv"
  let connections =
        case decode NoHeader connData of
          Left err -> error $ toS err
          Right v -> v :: V.Vector (Region, Region, StopId, RouteId)

  let fromNB = [(r,s) | (f,_,s,r) <- V.toList connections, f == 3]

  scotty 3000 $
    get "/" $ do
      times <- liftIO $ P.mapM (uncurry predictedArrivals) fromNB
      let list = (show $ join times)::Text
      text [i|
        <?xml version="1.0" encoding="UTF-8"?>
        <Response>
          <Say>
            ${list}
          </Say>
        </Response>
      |]

predictedArrivals :: RouteId -> StopId -> IO [Minute]
predictedArrivals route stop = do
  payload <- W.get . toS $ busArrivalUrl route stop
  let cursor = fromDocument . parseText_ def . toS $
                 payload ^. W.responseBody
      predictions = cursor $// element "prediction"
                           >=> attribute "minutes"

  return $ fromMaybe 0 . readMaybe . toS <$> predictions

 where
  busArrivalUrl :: RouteId -> StopId -> Text
  busArrivalUrl r s =
    "http://webservices.nextbus.com/service/publicXMLFeed?command=predictions&a=sf-muni&r="<>show r<>"&s="<>show s
