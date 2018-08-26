{-# LANGUAGE TemplateHaskell #-}

module Game where

import Map

data GameState = GameState { _focus :: Position
                           }

makeLenses ''Config