import System.Environment (getArgs)
import Data.List
import Data.Ord
import Data.Maybe

main = do
	print $ bestHop [5,6,0,4,2,4,1,0,0,4] 	-- [0,5,9]
	print $ bestHop []						-- []
	print $ bestHop [0]						-- failure
	print $ bestHop [1,0]					-- failure
	print $ bestHop [1]						-- [0]
	print $ bestHop [5]						-- [0]
	print $ bestHop [1,2,4,7,5,3,4,5,8,9,1,2,3,4,5,6,4,3,5,9,7,4,1,0,2,3,4,9] -- [0,1,3,9,18,19]
	print $ bestHop [1,1,1,1]				-- [0,1,2,3]

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