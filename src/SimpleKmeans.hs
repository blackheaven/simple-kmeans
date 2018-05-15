module SimpleKmeans
    ( kmeans
    , iterStop
    , iterAll
    , prettifier
    , mean2
    , dist2
    , mean3
    , dist3
    ) where

import Data.List(groupBy, sortBy, minimumBy, foldl1')
import Data.Function(on)

type Cluster a = (a, [a])
type Mean a = [a] -> a
type Distance a b = a -> a -> b
type Step a = ([a], [(Int, a)])
type Loop a = a -> a
type Iterator a b = (Step a -> [Cluster a]) -> Loop (Step a) -> Step a -> b

kmeans :: Ord b => Iterator a c -> Mean a -> Distance a b -> Int -> [a] -> c
kmeans iterator mean distance n = iterator clusterize step . init
  where init = addMean $ zip (cycle [1..n])
        step (m, xs) = addMean (closests distance m . map snd) xs
        clusterize (m, xs) = zip m $ groupByCluster xs
        addMean f = (means mean >>= (,)) . f
        -- init :: [a] -> Step a
        -- step :: Loop (Step a)
        -- clusterize :: Step a -> [Cluster a]
        -- addMean :: (b -> [(Int, a)]) -> b -> Step a

means :: Mean a -> [(Int, a)] -> [a]
means mean = map mean . groupByCluster

groupByCluster :: [(Int, a)] -> [[a]]
groupByCluster = map (map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

closests :: Ord b => Distance a b -> [a] -> [a] -> [(Int, a)]
closests distance means = map $ \x -> (fst $ minimumBy (compare `on` (distance x . snd)) (zip [1..] means), x)

-- Iterators
iterStop :: (Step a -> Bool) -> Iterator a [Cluster a]
iterStop stop clusterize step = clusterize . until stop step

iterAll :: Iterator a [([Cluster a], Step a)]
iterAll clusterize step = iterate (addCluster . step . snd) . addCluster
  where addCluster = clusterize >>= (,)

---- Helpers
prettifier :: Show a => Cluster a -> String
prettifier (m, xs) = unlines $ [replicate (length mean) '-', mean] ++ map show xs
  where mean = "** " ++ show m

mean2 :: Mean (Float, Float)
mean2 xs = (\(x, y) -> (x/l, y/l)) $ foldl1' (\(ax, ay) (bx, by) -> (ax + bx, ay + by)) xs
    where l = fromInteger $ toInteger $ length xs

dist2 :: Distance (Float, Float) Float
dist2 (ax, ay) (bx, by) = sqrt $ ((ax - bx) ** 2) + ((ay - by) ** 2)

mean3 :: Mean (Float, Float, Float)
mean3 xs = (\(x, y, z) -> (x/l, y/l, z/l)) $ foldl1' (\(ax, ay, az) (bx, by, bz) -> (ax + bx, ay + by, az + bz)) xs
    where l = fromInteger $ toInteger $ length xs

dist3 :: Distance (Float, Float, Float) Float
dist3 (ax, ay, az) (bx, by, bz) = sqrt $ ((ax - bx) ** 2) + ((ay - by) ** 2) + ((az - bz) ** 2)
