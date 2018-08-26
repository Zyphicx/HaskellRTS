{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)

import Backend
import Config
import Map

-- Main loop --

main :: IO ()
main = do 
    withWindow 800 600 "RTS game test" $ \window -> do
        let loop = do 
            threadDelay 5000
            exitGame <- exitPressed window
            unless exitGame loop
        loop