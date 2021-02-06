findMissingLetter a = until (`notElem` a) succ $ a!!0
