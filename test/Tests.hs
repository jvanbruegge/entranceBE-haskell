module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout, stderr)

import Tests.HelperTests (helperTests)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    results <- sequence [helperTests]

    unless (and results) exitFailure
