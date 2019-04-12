module Data.AsmTools.FastCSV where



readCSVFile = do
  content <- readFile "testCasePF.txt"
  line <- lines content
  loop gr line
