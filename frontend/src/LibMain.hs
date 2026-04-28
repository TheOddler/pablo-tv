{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LibMain where

import Miso
import Miso.Html qualified as H
import Miso.Html.Property qualified as P
import Miso.Lens

-- | Sum type for App events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp defaultEvents app

-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

-- | `component` takes as arguments the initial model, update function, view function
app :: App Int Action
app = component 0 updateModel viewModel

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Effect parent Int Action
updateModel = \case
  AddOne -> this += 1
  SubtractOne -> this -= 1
  SayHelloWorld -> io_ $ do
    alert "Hello World"
    consoleLog "Hello World"

-- | Constructs a virtual DOM from a model
viewModel :: Int -> View Int Action
viewModel x =
  H.div_
    [ P.className "counter"
    ]
    [ H.button_ [H.onClick AddOne] [text "+"],
      text (ms x),
      H.button_ [H.onClick SubtractOne] [text "-"],
      H.br_ [],
      H.button_ [H.onClick SayHelloWorld] [text "Alert Hello World!"]
    ]
