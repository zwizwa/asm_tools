import qualified Data.AsmTools.Make
import qualified Data.AsmTools.CSV

main = Data.AsmTools.Make.build "codegen_files" rules

rules :: Data.AsmTools.Make.Rules
rules =
  [((["test.sql"],[]), writeSQL)]


writeSQL ([file],[]) = do
  let table = (["key","val"], [["a","123"], ["b","456"]])
  writeFile file $ Data.AsmTools.CSV.showSQL "test" table
  
