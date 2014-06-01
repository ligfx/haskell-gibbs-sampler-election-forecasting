-- Run me with runhaskell gibbs.hs
-- Compile me with ghc gibbs.hs -main-is Gibbs

module Gibbs where

import Prelude hiding (putStrLn)

import Gibbs.Helpers

import Data.Aeson (encode)
import Control.Applicative ((<$>))
import Data.ByteString.Lazy.Char8 (putStrLn)
import Data.IntMap (fromList, IntMap)
import Data.Random (normal, RVar, sampleFrom)
import Data.Vector (singleton)
import System.Random.MWC (initialize)

genPref = rollM 100 (`normal` 0.007) 0.49

addBias b pref = let biased = pref + b in
        normal biased $ biased * (1 - biased) / 2000

genPoll pref bias dates =
        fromList <$> (addBias bias . (!!) pref) `zipMapM` dates

fakeData :: RVar ([Double], IntMap Double, IntMap Double)
fakeData = do
        pref <- genPref
        giddyup <- genPoll pref 0.03 [2, 7 .. 100]
        rasputin <- genPoll pref (-0.03) [3, 8 .. 100]
        return (pref, giddyup, rasputin)
        
main = do
        mwc <- initialize $ singleton 14
        (pref, giddyup, rasputin) <- sampleFrom mwc fakeData
        putStrLn $ encode (pref, giddyup, rasputin)