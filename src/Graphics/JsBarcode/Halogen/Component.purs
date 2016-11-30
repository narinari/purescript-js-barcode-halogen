module Graphics.JsBarcode.Halogen.Component where

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Free (class Affable)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML.Types (HTMLCanvasElement)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (read)
import Data.Maybe (Maybe(..))
import Graphics.JsBarcode (Config, Format(EAN13), mkJsBarcode, defaultConfig)
import Prelude (type (~>), pure, unit, bind, const, ($), ($>), (*>), (<<<), (=<<))

type JsBarcodeEffects eff = (dom :: DOM | eff)

type State =
  { element :: Maybe HTMLCanvasElement
  , config :: Config
  , value :: String
  }

data Query a
  = SetElement (Maybe HTMLCanvasElement) a
  | Init a
  | SetCode String a

initialState :: State
initialState = 
  { element: Nothing
  , config: defaultConfig { format = EAN13, displayValue = false }
  , value: ""
  }

component
  :: forall m eff
  . ( MonadAff (JsBarcodeEffects eff) m
    , Affable (JsBarcodeEffects eff) m
    )
  => H.Component State Query m
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
    [ HP.ref (\elm -> H.action $ SetElement $ (either (const Nothing ) pure <<< runExcept <<< read <<< toForeign) =<< elm) ]

  eval :: Query ~> H.ComponentDSL State Query m
  eval (SetElement elm next) = H.modify (_ { element = elm}) $> next
  eval (Init next) = drawBarcode $> next
  eval (SetCode value next) =
    H.modify _ { value = value }
    *> drawBarcode
    $> next
  
  drawBarcode = do
    elm <- H.gets _.element
    config <- H.gets _.config
    value <- H.gets _.value
    case elm of
      Nothing -> pure unit
      Just el' -> do
        H.fromEff $ mkJsBarcode el' value config
