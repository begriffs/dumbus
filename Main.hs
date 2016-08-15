import Protolude hiding (get)

import qualified Network.Wreq as W
import           Control.Lens hiding (element)
import qualified Control.Monad.Parallel as P
import           Data.Csv
import           Data.String.Here.Interpolated
import qualified Data.Text as T
import qualified Data.Vector as V
import           GHC.Exts (sortWith)
import           Text.XML hiding (readFile)
import           Text.XML.Cursor
import           Web.Scotty

type RouteId = Int
type StopId  = Int
type Region  = Int
type Minute  = Int
type Prediction = (RouteId, Minute)

sayPrediction :: Prediction -> Text
sayPrediction (r, m) = [i|${r} ${m}|]

main :: IO ()
main = do
  connData <- toS <$> readFile "connections.csv"
  let connections =
        case decode NoHeader connData of
          Left err -> error $ toS err
          Right v -> v :: V.Vector (Region, Region, StopId, RouteId)

  let fromNB = [(r,s) | (f,_,s,r) <- V.toList connections, f == 3]

  scotty 3000 $ do
    get "/blip.mp3" $ do
      setHeader "Content-Type" "audio/mpeg"
      file "./static/blip.mp3"

    get "/" $ do
      times <- liftIO $ P.mapM (uncurry predictedArrivals) fromNB
      let nextStops = sortWith snd $ join times
          next = T.intercalate ", " . map sayPrediction $ nextStops
      text [i|
        <?xml version="1.0" encoding="UTF-8"?>
        <Response>
          <Say>
            ${next}
          </Say>
        </Response>
      |]

predictedArrivals :: RouteId -> StopId -> IO [Prediction]
predictedArrivals route stop = do
  payload <- W.get . toS $ busArrivalUrl route stop
  let cursor = fromDocument . parseText_ def . toS $
                 payload ^. W.responseBody
      predictions = cursor $// element "prediction"
                           >=> attribute "minutes"

  return $ (route,) . fromMaybe 0 . readMaybe . toS <$> predictions

 where
  busArrivalUrl :: RouteId -> StopId -> Text
  busArrivalUrl r s =
    "http://webservices.nextbus.com/service/publicXMLFeed?command=predictions&a=sf-muni&r="<>show r<>"&s="<>show s
