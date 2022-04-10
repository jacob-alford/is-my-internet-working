module Main
  ( Action
  , component
  , initialState
  , main
  , render
  )
  where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Control.Monad.Rec.Class (forever)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Utility (css)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State = {
  good :: Maybe Boolean,
  updates :: Int
}

data Action = Init | Update Boolean

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval 
      { handleAction = handleAction
      , initialize = Just Init 
      }
    }

initialState :: forall input. input -> State
initialState _ = {
  good: Nothing,
  updates: 0
}

data Connection = Loading | Bad | Good

connectionFromState :: Maybe Boolean -> Connection
connectionFromState Nothing = Loading
connectionFromState (Just good) = if good then Good else Bad

app :: forall w i. Connection -> Array (HH.HTML w i) -> HH.HTML w i
app conn =  HH.div [ css $ "flex flex-wrap flex-row items-center justify-center h-screen transition-colors duration-500 " <> bgColor  ]
  where 
    bgColor :: String
    bgColor = case conn of
      Loading -> "bg-sky-300"
      Bad -> "bg-red-500"
      Good -> "bg-green-400"

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  app (connectionFromState state.good) case (connectionFromState state.good) of
    Loading -> [ HH.div_ [ HH.text "Loading..." ] ]
    Bad -> [ HH.div_ [ HH.text "Not working :-(" ] ]
    Good -> []


handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Init -> do
    _ <- H.subscribe =<< timer Update
    pure unit
  Update good -> 
    H.modify_ \state -> { updates: state.updates + 1, good: Just good }

timer :: forall m a. MonadAff m => (Boolean -> a) -> m (HS.Emitter a)
timer getVal = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Aff.Milliseconds 8000.0
    response <- AX.get AXRF.string "https://httpstat.us/200"
    result <- pure $ case response of
      Left _ -> false
      Right _  -> true
    H.liftEffect $ HS.notify listener (getVal result)
  pure emitter