module Graphics.JsBarcode.Halogen.Component where

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.HTML.Types (HTMLCanvasElement)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (read)
import Data.Maybe (Maybe(..))
import Graphics.JsBarcode (Config, Format(EAN13), mkJsBarcode, defaultConfig)
import Prelude (type (~>), pure, unit, bind, const, ($), ($>), (<<<), (=<<))

import Debug.Trace

type JsBarcodeEffects eff = (dom :: DOM | eff)

type State =
  { element :: Maybe HTMLCanvasElement
  , config :: Config
  , value :: String
  }

data Query a
  = SetElement (Maybe HTMLCanvasElement) a
  | Init a

initialState :: State
initialState = 
  { element: Nothing
  , config: defaultConfig { format = EAN13, displayValue = false }
  , value: ""
  }

component
  :: forall eff
  .  H.Component State Query (Aff (JsBarcodeEffects eff))
component = H.lifecycleComponent
  { render
  , eval
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }
  where
  render :: State -> H.ComponentHTML Query
  render _ =
    HH.canvas
    [ HP.ref (\elm -> H.action $ SetElement $ (either (const Nothing ) pure <<< read <<< toForeign) =<< elm) ]

  eval :: Query ~> H.ComponentDSL State Query (Aff (JsBarcodeEffects eff))
  eval (SetElement elm next) = H.modify (_ { element = elm}) $> next
  eval (Init next) = do
    elm <- H.gets _.element
    config <- H.gets _.config
    value <- H.gets _.value
    case spy elm of
      Nothing -> pure unit
      Just el' -> do
        H.fromEff $ mkJsBarcode el' (spy value) (spy config)
    pure next
