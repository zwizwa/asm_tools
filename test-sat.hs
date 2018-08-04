import SAT.Mios (CNFDescription (..), solveSAT)

-- Clauses are represented by 1-base variable number * sign.
clauses :: [[Int]]
clauses =
  [[1, 2],
   [1, 3],
   [-1, -2],
   [1, -2, 3],
   [-3]]

desc = CNFDescription
  3       -- number of variables
  5       -- number of clauses
  Nothing -- Maybe pathname

main = do
  asg <- solveSAT desc clauses    -- solveSAT :: Traversable m => CNFDescription -> m [Int] -> IO [Int]
  putStrLn $ if null asg then "unsatisfiable" else show asg
