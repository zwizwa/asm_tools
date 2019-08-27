-- Example top level file

-- All code is gatherd in a module to keep it composable.  This file
-- is just a trampoline.

-- FIXME: Test build

import Language.Seq.Examples.FSoc
main = Data.AsmTools.Make.build "f_soc" Langauge.Seq.Examples.FSoc.targets
