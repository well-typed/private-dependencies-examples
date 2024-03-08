{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, NoMonomorphismRestriction #-}
module Main where

-- The testing library
import Test.Tasty.Bench

import qualified TEXT1.Data.Text as TEXT1
import qualified TEXT1.Data.Text.IO as TEXT1


import qualified TEXT2.Data.Text as TEXT2
import qualified TEXT2.Data.Text.IO as TEXT2

import Control.DeepSeq

data InputEnv a = InputEnv { lorem :: !a }

instance NFData (InputEnv a) where
  rnf InputEnv{} = ()

type T1Env = InputEnv TEXT1.Text
type T2Env = InputEnv TEXT2.Text

mkBench :: T1Env -> T2Env -> (TEXT1.Text -> a) -> (TEXT2.Text -> b) -> [Benchmark]
mkBench t1env t2env t1 t2 =
  [ bench "t1" $ whnf t1 (lorem t1env)
  , bench "t2" $ whnf t2 (lorem t2env) ]

main =
  defaultMain . (:[]) $
    env (InputEnv <$> TEXT1.readFile "lorem.txt") $ \t1env ->
    env (InputEnv <$> TEXT2.readFile "lorem.txt") $ \t2env ->
      let mk_bench = mkBench t1env t2env
      in bgroup "text"
          [ bgroup "Reverse" (mk_bench TEXT1.reverse TEXT2.reverse)
          , bgroup "Length"  (mk_bench TEXT1.length TEXT2.length)
          ]


