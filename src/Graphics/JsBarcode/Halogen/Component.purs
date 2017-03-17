module Graphics.JsBarcode.Halogen.Component where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Except (runExcept)
import Control.Monad.Eff.Exception (catchException)
import DOM (DOM)
import DOM.Node.Types (Element, readElement)
import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse_)
import Data.Void (Void)
import Graphics.JsBarcode (Config, mkJsBarcode, defaultConfig) as JsBarcode
import Prelude (type (~>), const, unit, pure, bind, when, ($), (<*>), (<$>), (/=), ($>), (>>>), (>>=))

type JsBarcodeEffects eff = (dom :: DOM | eff)

type State =
  { element :: Maybe Element
  , config :: Config
  }

type Config =
  { barcodeConfig :: JsBarcode.Config
  , value :: Maybe String
  }

data Query a
  = Init a
  | HandleInput Input a

type Input = Maybe String

initialState :: JsBarcode.Config -> Input -> State
initialState barcodeConfig value =
  { element: Nothing
  , config:
    { barcodeConfig
    , value: value
    }
  }

component
  :: forall m eff
  . ( MonadAff (JsBarcodeEffects eff) m )
  => Maybe JsBarcode.Config
  -> H.Component HH.HTML Query Input Void m
component config = H.lifecycleComponent
  { initialState: initialState (fromMaybe JsBarcode.defaultConfig config)
  , render
  , eval
  , receiver: HE.input HandleInput
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }

  where
  render :: State -> H.ComponentHTML Query
  render _ =
    HH.canvas
    [ HP.ref (H.RefLabel "barcode") ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (Init next) = do
    elm <- H.getRef (H.RefLabel "barcode")
    H.modify _ { element = elm >>= readElement >>> runExcept >>> either (const Nothing) Just  }
    drawBarcode
    pure next

  eval (HandleInput new next) = do
    old <- H.gets _.config.value
    when (old /= new) do
      H.modify \s -> s { config = s.config { value = new } }
      drawBarcode
    $> next

  drawBarcode = do
    { element, config } <- H.get
    traverse_ H.liftEff $ (\e v -> catchException (const $ pure unit) $ JsBarcode.mkJsBarcode e v config.barcodeConfig) <$> element <*> config.value
