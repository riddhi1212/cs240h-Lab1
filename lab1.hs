import System.Environment
import Data.Map
import Data.List
import Data.Char

main = getArgs >>= parse >>= process

-- Get input
parse []     = getContents
parse fs     = concat `fmap` mapM readFile fs

process  = printDict . countWords . getWords . Data.List.map toLower

-- Get words from text files without punctuation
getWords str = Data.List.filter (/= "") (Data.List.map (removeLeftPunctuation . removeRightPunctuation) (words str))

removeLeftPunctuation "" = ""
removeLeftPunctuation w = if (not . isAlpha . head) w
                            then drop 1 w
			    else w

removeRightPunctuation "" = ""
removeRightPunctuation w = if (not . isAlpha . last) w
                             then (reverse . drop 1 . reverse) w
			     else w

-- Get dictionary with appropriate word counts from list of words
countWords [] = empty
countWords (w:ws) = let
 	              dict = countWords ws
       	            in Data.Map.insert w ((findWithDefault 0 w dict)+1) dict

-- Print output from dictionary
printDict dict = putStr (fst (mapAccumL printDictKey "" (sortBy sortByVal (toList (transformDictKeys dict)))))

compareKeyLen key val result = maximum [length key, result]

transformDictKeys dict = mapKeys (transformKeys maxlen) dict
                         where maxlen = foldrWithKey compareKeyLen 0 dict

transformKeys maxlen key = key ++ (concatMap (\_ -> " ") [1..val])
	      	           where val = maxlen - (length key)

sortByVal (k1, v1) (k2, v2) = if v1 == v2
                                then compare k1 k2
                                else compare v2 v1

printDictKey result (key,val) = (result ++ key ++ " " ++ (concatMap (const "#") [1..val]) ++ "\n", length key)

