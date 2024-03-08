{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, NoMonomorphismRestriction, DataKinds, KindSignatures, FlexibleInstances, TypeApplications #-}
module Main where

-- The testing library
import Test.Tasty.Bench

import qualified TEXT1.Data.Text as TEXT1
import qualified TEXT1.Data.Text.IO as TEXT1
import qualified TEXT1.Control.DeepSeq as TEXT1


import qualified TEXT2.Data.Text as TEXT2
import qualified TEXT2.Data.Text.IO as TEXT2
import qualified TEXT2.Control.DeepSeq as TEXT2

import Control.DeepSeq

data Scope = T1 | T2

data Env (scope :: Scope) a = Env { lorem :: !a }

instance TEXT1.NFData a => NFData (Env T1 a) where
  rnf (Env x) = TEXT1.rnf x

instance TEXT2.NFData a => NFData (Env T2 a) where
  rnf (Env x) = TEXT2.rnf x

type T1Env = Env T1 TEXT1.Text
type T2Env = Env T2 TEXT2.Text

mkBench :: (NFData a, NFData b) => T1Env -> T2Env -> (TEXT1.Text -> a) -> (TEXT2.Text -> b) -> [Benchmark]
mkBench t1env t2env t1 t2 =
  [ bench "t1" $ nf t1 (lorem t1env)
  , bench "t2" $ nf t2 (lorem t2env) ]

main =
  defaultMain . (:[]) $
    env (Env <$> TEXT1.readFile "lorem.txt") $ \t1env ->
    env (Env <$> TEXT2.readFile "lorem.txt") $ \t2env ->
      let mk_bench x y = mkBench t1env t2env (Env @T1 . x) (Env @T2. y)
      in bgroup "text"
          [ bgroup "Reverse" (mk_bench TEXT1.reverse TEXT2.reverse)
          , bgroup "Length"  (mk_bench TEXT1.length TEXT2.length)
          ]


