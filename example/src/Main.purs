module Main where

import Graphics.JsBarcode.Types
import Graphics.JsBarcode.Halogen.Component as JsBarcode
import Halogen as H

import Halogen.HTML.Indexed as HH

import CSS.Color (rgb)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(Nothing))
import Halogen.Util (runHalogenAff, awaitBody)
import Prelude (class Eq, class Ord, type (~>), Unit, pure, (=<<), ($))

type MainEffects = H.HalogenEffects ()

data Query a
  = Noop a

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
                      { config = barcodeConfig, value = "098765432123"}
                  }
        ]
      ]

  slotA = JsBarcodeSlot "A"
  slotB = JsBarcodeSlot "B"

  eval :: Query ~> MainDSL MainAff
  eval (Noop next) = pure next
 
main :: Eff MainEffects Unit
main = runHalogenAff $ H.runUI ui (H.parentState initialState) =<< awaitBody
