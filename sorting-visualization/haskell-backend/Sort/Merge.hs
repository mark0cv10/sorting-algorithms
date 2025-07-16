module Sort.Merge (mergeSortSteps) where
 
mergeSortSteps :: [Int] -> [[Int]]
mergeSortSteps xs = go xs 0 (length xs - 1)
  where
    go lst l r
      | l >= r = [lst]
      | otherwise =
          let m = (l + r) `div` 2
              leftSteps = go lst l m
              afterLeft = last leftSteps
              rightSteps = go afterLeft (m + 1) r
              afterRight = last rightSteps
              mergedSteps = merge afterRight l m r
          in leftSteps ++ rightSteps ++ mergedSteps
 
    merge arr l m r = mergeStep arr l m r []
 
    mergeStep arr l m r acc =
      let left = take (m - l + 1) (drop l arr)
          right = take (r - m) (drop (m + 1) arr)
 
          goMerge i j k curArr steps
            | i >= length left && j >= length right = steps
            | i < length left && (j >= length right || left !! i <= right !! j) =
                let newArr = take k curArr ++ [left !! i] ++ drop (k + 1) curArr
                in goMerge (i + 1) j (k + 1) newArr (steps ++ [newArr])
            | otherwise =
                let newArr = take k curArr ++ [right !! j] ++ drop (k + 1) curArr
                in goMerge i (j + 1) (k + 1) newArr (steps ++ [newArr])
 
      in goMerge 0 0 l arr acc