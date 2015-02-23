import System.Environment (getArgs)
import Data.List
import Data.Ord
import Data.Maybe

main = do
	[inpFile] <- getArgs
	input <- readFile inpFile
	let data_ = map (\line -> (read line::Int)) $ lines input
	let solution = bestHop data_
	putStr $ maybe "failure" ( (++ ",out") . init . tail . show) solution

bestHop data_
	| null data_ = Just []
	| otherwise = fmap (0:) $ bestHopImpl data_

bestHopImpl :: [Int] -> Maybe [Int]
bestHopImpl data_
	| null data_ = Just []
	| otherwise =
		let lookahead = take (head data_) $ tail data_ in
		if (length lookahead < head data_) then Just [] else
		let maxAjustement = (length lookahead) in
		let ajusted = zipWith subtract (iterate (subtract 1) maxAjustement) lookahead in
		if (all (<0) ajusted) then Nothing else
		let (index, value) = maximumBy (comparing snd) $ zip [1..] ajusted in
		fmap (\xs -> index : (map (+index) xs)) (bestHopImpl $ drop index data_)