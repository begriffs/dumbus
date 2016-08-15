import Protolude hiding (get)

import qualified Network.Wreq as W
import           Control.Lens hiding (element)
import qualified Control.Monad.Parallel as P
import           Data.Csv
import           Text.InterpolatedString.Perl6
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
sayPrediction (r, m) = [qq|$r $m|]

main :: IO ()
main = do
  connData <- toS <$> readFile "connections.csv"
  let connections = V.toList $
        case decode NoHeader connData of
          Left err -> error $ toS err
          Right v -> v :: V.Vector (Region, Region, StopId, RouteId)

  scotty 3000 $ do
    get "/blip.mp3" $ do
      setHeader "Content-Type" "audio/mpeg"
      file "./static/blip.mp3"

    get "/" $ do
      setHeader "Content-Type" "text/xml"
      text [q|
        <Response>
          <Gather timeout="10" action="when" method="GET">
            <Play>blip.mp3</Play>
          </Gather>
        </Response>
      |]

    get "/when" $ do
      setHeader "Content-Type" "text/xml"
      digits <- map (fromMaybe 0 . readMaybe . toS) . T.chunksOf 1 <$>
        rescue (param "Digits") (const $ return "")

      let query = case digits of
            [region] -> [(r,s) | (f,_,s,r) <- connections, f == region]
            [r1,r2] -> [(r,s) | (f,t,s,r) <- connections, f == r1, t == r2]
            _ -> []

      times <- liftIO $ P.mapM (uncurry predictedArrivals) query
      text $ if null query
        then [q|
            <Response>
              <Redirect>../</Redirect>
            </Response>
          |]
        else
          let nextStops = sortWith snd $ join times
              next = T.intercalate ", " . map sayPrediction $ nextStops in
          [qq|
            <Response>
              <Gather timeout="10" action="when" method="GET">
                <Say>$next</Say>
              </Gather>
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
