--Implementation of UNIX wc utility, called with $ my_wc < file_to_inspect

import qualified Data.String.Utils as SU

main = interact stats
  where
    stats i = (SU.join "\t" $ map (show . ($ i)) [lineC, wordC, charC]) ++ "\n"
    lineC = length . lines
    wordC = length . words
    charC = length

