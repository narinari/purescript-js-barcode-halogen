module Main where

import Graphics.JsBarcode.Types
import Graphics.JsBarcode.Halogen.Component as JsBarcode

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE

import CSS.Color (rgb)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(Nothing))
import Halogen.Util (runHalogenAff, awaitBody)
import Prelude (class Eq, class Ord, type (~>), Unit, bind, pure, (=<<), ($))

import Debug.Trace

type MainEffects = H.HalogenEffects ()

data Query a
  = InputCode JsBarcodeSlot String a

type State =
  { sig1Data :: Maybe String
  }

initialState :: State
initialState =
  { sig1Data: Nothing
  }

data JsBarcodeSlot = JsBarcodeSlot String

derive instance jsBarcodeSlotOrd :: Ord JsBarcodeSlot
derive instance jsBarcodeSlotEq :: Eq JsBarcodeSlot

type StateP g = H.ParentState State JsBarcode.State Query JsBarcode.Query g JsBarcodeSlot
type QueryP = Coproduct Query (H.ChildF JsBarcodeSlot JsBarcode.Query)
type MainHTML g = H.ParentHTML JsBarcode.State Query JsBarcode.Query g JsBarcodeSlot
type MainAff = Aff MainEffects
type MainDSL g = H.ParentDSL State JsBarcode.State Query JsBarcode.Query g JsBarcodeSlot

ui :: H.Component (StateP MainAff) QueryP MainAff
ui = H.parentComponent { render, eval, peek: Nothing }
  where
  barcodeConfig = defaultConfig { format = EAN13, background = rgb 220 220 220 }
  render :: State -> MainHTML MainAff
  render state =
    HH.div_
    [ HH.div_
      [ HH.slot slotA
          \_ -> { component: JsBarcode.component
                , initialState: JsBarcode.initialState
                    { config = barcodeConfig, value = "012345678912"}
                }
      ]
    , HH.div_
      [ HH.slot slotB
          \_ -> { component: JsBarcode.component
                , initialState: JsBarcode.initialState
                    { config = barcodeConfig, value = ""}
                }
      ]
    , HH.input
      [ HP.inputType HP.InputText
      , HE.onValueInput $ HE.input $ InputCode slotB
      ]
    ]

  slotA = JsBarcodeSlot "A"
  slotB = JsBarcodeSlot "B"

  eval :: Query ~> MainDSL MainAff
  eval (InputCode ref value next) = do
    traceAny value \_ -> H.query ref $ H.action $ JsBarcode.SetCode value
    pure next
 
main :: Eff MainEffects Unit
main = runHalogenAff $ H.runUI ui (H.parentState initialState) =<< awaitBody
