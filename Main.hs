{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)

import Data.Monoid
import Vision.Primitive (Z (..), (:.) (..), ix2)

import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.IO          as T
import qualified Vision.Image               as I
import qualified Vision.Image.Storage.DevIL as I

main :: IO ()
main = do
    [ path ] <- getArgs

    Right grey <- I.load I.Autodetect path

    T.putStrLn $ toVDHL grey

toVDHL :: I.Grey -> T.Text
toVDHL grey =
    T.intercalate ", \n" [ toLine y | y <- [0.. h-1 ] ] <> ";"
  where
    Z :. h :. w = I.shape grey

    blackWhite = I.map (>= 127) grey :: I.Delayed Bool

    toLine y = "(" <> T.pack [ if p then '1' else '0'
                             | x <- [0..w - 1]
                             , let p = blackWhite I.! ix2 y x ]
                   <> ")"