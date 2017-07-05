-- | ==  tying-the-knot  principle  for a lens like over behaviour on list partitions 
--
-- 
-- >>> onOdds (tail . cycle) [1..10]
-- [3,2,5,4,7,6,9,8,1,10]
-- 
--
module Data.List.OnPartition (
    -- * general
    onPartitionG ,
    -- * 'List' partition like
    onPartition ,
    -- * by index
    onPartitionIndex ,
    -- * simple ones
    onRights ,
    onOdds 
    )
    where


import Control.Arrow ((***))
import Control.Monad.Fix (fix)
import Data.Bool (bool)

-- | apply a function to the partitioned elements of a list, as a whole
onPartition     :: (a -> Bool)  -- ^ partitioner
                -> ([a] -> [a]) -- ^ analyzer   
                -> [a] 
                -> [a] 
onPartition s  = fmap (map $ either id id) . onPartitionG (flip (bool Left Right) <*> s)

-- | apply a function to the inedex partitioned elements of a list, as a whole
onPartitionIndex :: (Int -> Bool)  -- ^ partitioner
                -> ([a] -> [a])    -- ^ analyzer   
                -> [a] -> [a]
onPartitionIndex s f = map snd . onPartition (s . fst) (uncurry zip . fmap f . unzip) . zip [0..]                


-- | apply a function to the partitioned elements of a list, as a whole, changing their types
onPartitionG    :: (d -> Either c a) -- ^ partitioner  
                -> ([a] -> [b])      -- ^ analyzer   
                -> [d] -> [Either c b]
onPartitionG s f xs = fst . fix $ part xs . f . snd where
    part [] _ = ([],[])
    part (x:xs) rt@(~(r:rs)) = case s x of
        Right x ->  ((Right r:) *** (x:) $ part xs rs)
        Left y -> ((Left y:) *** id $ part xs rt)


-- | apply a function to the Right elements of a list, as a whole, changing their types
onRights 
    :: ([a] -> [b]) -- ^ analyzer
    -> [Either l a] 
    -> [Either l b]
onRights = onPartitionG id 

-- | change the odds
onOdds :: ([a] -> [a])  -- ^ analyzer
        -> [a] 
        -> [a]
onOdds = onPartitionIndex even
