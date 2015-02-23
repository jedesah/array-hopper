--import System
--import Data.List

--main : IO Unit
--main = do
--  [inputFileName] <- getArgs
--  input <- readFile inputFileName
--  let data_ = map read $ lines input
--  let solution = bestHop data_
--  let formatSolution = (++ ",out") . init . tail . show
--  putStr $ maybe "failure" formatSolution solution

--bestHop : List Int -> List Int
--bestHop [] = Just []
--bestHop xs = fmap (0::) $ bestHopImpl xs

bestHopImpl : List Int -> Maybe $ List Int
bestHopImpl [] = Just []
bestHopImpl xs = if ((length $ tail xs) < (head xs))  then Just [1] else Just[2]
    --if ((length $ tail xs) < (head xs)) then (Just [])
    --else
    --  let lookahead = take (head data_) $ tail data in
    --  let maxAjustement = (length lookahead) in
    --  let ajusted = zipWith subtract (iterate (subtract 1) maxAjustement) lookahead in
    --  if (all (<0) ajusted) then Nothing
    --  else
    --    let index = indexOfMaximumBy ajusted in
    --    fmap (\xs -> index : (map (+index) xs)) (bestHopImpl $ drop index data_)

--indexOfMaximumBy : (a -> Ord) -> List a
--indexOfMaximumBy f xs = fst $ maximumBy f $ zip [1..] xs
