module Sort.Bubble (bubbleSortSteps) where
 
bubbleSortSteps :: [Int] -> [[Int]]
bubbleSortSteps xs = xs : bubbleHelper xs []
 
bubbleHelper :: [Int] -> [[Int]] -> [[Int]]
bubbleHelper xs acc
  | sorted xs = acc ++ []
  | otherwise =
      let next = bubblePass xs
      in bubbleHelper next (acc ++ [next])
 
bubblePass :: [Int] -> [Int]
bubblePass (x:y:rest)
  | x > y     = y : bubblePass (x:rest)
  | otherwise = x : bubblePass (y:rest)
bubblePass xs = xs
 
sorted :: [Int] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:rest) = x <= y && sorted (y:rest)