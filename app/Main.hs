module Main where

import Options.Applicative
import Data.Monoid
import System.Random(newStdGen, randomRs)
import Data.List(unfoldr)
import Criterion.Main (bench, bgroup, env, whnf, defaultMain)
import System.Environment (withArgs)
import SimpleKmeans

main :: IO ()
main = execParser opts >>= run
  where
    parser = Configuration <$> argument auto (metavar "p" <> help "number of points")
                           <*> argument auto (metavar "ll" <> help "points minimal value")
                           <*> argument auto (metavar "lh" <> help "point maximal value")
                           <*> argument auto (metavar "c" <> help "number of clusters")
                           <*> argument auto (metavar "s" <> help "number of steps")
                           <*> flag False True (short 'd' <> long "debug" <> help "print steps")
    opts = info (parser <**> helper) mempty

data Configuration = Configuration { getNbOfPoints        :: Int
                                   , getPointsLowLimit    :: Float
                                   , getPointsHighLimit   :: Float
                                   , getNbOfClusters      :: Int
                                   , getNumberOfSteps     :: Int
                                   , doesPrintSteps       :: Bool
                                   }

run :: Configuration -> IO ()
run (Configuration p ll lh c s d) = do
    points <- take p <$> genPoints ll lh
    let res = take s $ kmeans iterAll mean2 dist2 c points
    if d
      then putStr $ unlines $ map (concatMap (prettifier) . fst) res
      else return ()
    withArgs ["--output", "benchmarks.html"] $ defaultMain [bgroup "kmeans" [bench "main"  $ whnf (const res) ()]]

genPoints :: Float -> Float -> IO [(Float, Float)]
genPoints ll lh = newStdGen >>= return . unfoldr (\(x:y:zs) -> Just ((x, y), zs)) . randomRs (ll, lh)
