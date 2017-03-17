module Main where

import Graphics.JsBarcode.Types
import Graphics.JsBarcode.Halogen.Component as JsBarcode

import Halogen as H
import Halogen.Aff (HalogenEffects, runHalogenAff, awaitBody)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import CSS.Color (rgb)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Void (Void, absurd)
import Halogen.VDom.Driver (runUI)
import Prelude (class Eq, class Ord, type (~>), Unit, unit, const, (=<<), ($), ($>))

type MainEffects = HalogenEffects ()

data Query a
  = InputCode String a

type State = Maybe String

initialState :: State
initialState = Nothing

data JsBarcodeSlot = JsBarcodeSlot String

derive instance jsBarcodeSlotOrd :: Ord JsBarcodeSlot
derive instance jsBarcodeSlotEq :: Eq JsBarcodeSlot

type MainHTML g = H.ParentHTML Query JsBarcode.Query JsBarcodeSlot g
type MainAff = Aff MainEffects
type MainDSL g = H.ParentDSL State Query JsBarcode.Query JsBarcodeSlot Void g

ui :: H.Component HH.HTML Query Unit Void MainAff
ui = H.parentComponent { initialState: const initialState, render, eval, receiver: const Nothing }
  where
  barcodeConfig = defaultConfig { format = EAN13, background = rgb 220 220 220 }
  render :: State -> MainHTML MainAff
  render state =
    HH.div_
    [ HH.div_
      [ HH.slot
          slotA
          (JsBarcode.component (Just barcodeConfig))
          (Just "0071641011519")
          absurd
      ]
    , HH.div_
      [ HH.slot
          slotB
          (JsBarcode.component (Just defaultConfig { format = EAN13, background = rgb 100 100 220 }))
          state
          absurd
      ]
    , HH.input
      [ HP.type_ HP.InputText
      , HE.onValueInput $ HE.input $ InputCode
      ]
    ]

  slotA = JsBarcodeSlot "A"
  slotB = JsBarcodeSlot "B"

  eval :: Query ~> MainDSL MainAff
  eval (InputCode value next) =
    H.put (Just value)
    $> next

main :: Eff MainEffects Unit
main = runHalogenAff $ runUI ui unit =<< awaitBody
