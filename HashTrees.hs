module HashTrees where

import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Data.List

data HashTree k a = Leaf | HashTree (Digest k) (HashTree k a) (HashTree k a) deriving (Show)

groupsOf _ ([]) = []
groupsOf n xs = case splitAt n xs of
    (a, as) -> a : (groupsOf n as)
    
groupsOf2 xs = map (\(x:y:[]) -> (x,y)) $ groupsOf 2 xs

hashCatInner (h1, h2) = sha1 $ BL.concat [(bytestringDigest h1), (bytestringDigest h2), BL.singleton 1]

hashes = map sha1

singletonTrees xs = map (\x -> HashTree x Leaf Leaf) xs

reduceHashes xs = map (\((HashTree x _ _), (HashTree y _ _)) -> HashTree (hashCatInner (x,y)) Leaf Leaf) (groupsOf2 xs)

combineTrees (HashTree x _ _) (t1, t2) = HashTree x t1 t2

combineTreeLists lower upper = zipWith combineTrees upper (groupsOf2 lower)

buildPreliminaryLevels ([x]) = [[x]]
buildPreliminaryLevels xs = xs : (buildPreliminaryLevels $ (reduceHashes xs))

buildTree' :: [HashTree k t] -> [[HashTree k t]] -> [HashTree k t]
buildTree' acc ([]) = acc
buildTree' acc (x:xs) = buildTree' (combineTreeLists acc x) xs

buildTree = (\x -> buildTree' (head x) (tail x)) . buildPreliminaryLevels . singletonTrees . hashes . (map (\x -> BL.concat [x,BL.singleton 0])) . padHashes

-- Padding functions

nextPowerOf n = 2 ^ (ceiling $ logBase 2 $ fromIntegral n)

padHashes xs = pad' 0 xs where
    pad' n ([]) = [] ++ (take ((nextPowerOf n) - n) $ repeat BL.empty)
    pad' n (x:xs) = x : (pad' (n +1) xs)
  
