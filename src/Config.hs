{-# LANGUAGE TemplateHaskell #-}

module Config where

import qualified Data.Map.Strict as Map

import Control.Lens
import qualified Graphics.UI.GLFW as GLFW

data Config = Config { _keyTable :: Map.Map String GLFW.Key
			  		 }

makeLenses ''Config

data Key = MenuKey