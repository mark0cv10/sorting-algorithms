module Sort.Quick (quickSortSteps) where
 
quickSortSteps :: [Int] -> [[Int]]
quickSortSteps xs = go xs 0 (length xs - 1)
 
go :: [Int] -> Int -> Int -> [[Int]]
go xs l r
  | l >= r = [xs]
  | otherwise =
      let (arr', stepsPartition, p) = partition xs l r
          leftSteps = go arr' l (p - 1)
          afterLeft = last leftSteps
          rightSteps = go afterLeft (p + 1) r
      in stepsPartition ++ leftSteps ++ rightSteps
 
partition :: [Int] -> Int -> Int -> ([Int], [[Int]], Int)
partition xs l r =
  let pivot = xs !! r
      (arrFinal, _, i, steps) = foldl
            (\(arr, j, i, acc) k ->
                if xs !! k < pivot then
                    let arr' = swap arr i k
                    in (arr', j + 1, i + 1, acc ++ [arr'])
                else
                    (arr, j + 1, i, acc)
            )
            (xs, l, l, [])
            [l..(r - 1)]
      arrPivot = swap arrFinal i r
  in (arrPivot, steps ++ [arrPivot], i)
 
swap :: [a] -> Int -> Int -> [a]
swap xs i j
  | i == j = xs
  | otherwise =
      let xi = xs !! i
          xj = xs !! j
          replaceAt :: Int -> a -> [a] -> [a]
          replaceAt n val lst = take n lst ++ [val] ++ drop (n + 1) lst
          xs' = replaceAt i xj xs
      in replaceAt j xi xs'