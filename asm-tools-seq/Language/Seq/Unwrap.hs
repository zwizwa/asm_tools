-- 20240130: NoMonadFailDesugaring is no longer supported.  The Seq
-- language uses a lot of list deconstruction in places where the
-- pattern cannot fail.  These places now need to be made explicit.

-- Note that this is essentially a language design bug.  This,
-- together with leakageof vector lengths as values really indicates
-- that a redesign is necessary.

module Language.Seq.Unwrap where

munwrap1 (mlist) = do
  list <- mlist
  case list of
    (a:rest) -> return (a,rest)
    _ -> error "munwrap1"
munwrap2 (mlist) = do
  list <- mlist
  case list of
    (a:b:rest) -> return (a,b,rest)
    _ -> error "munwrap2"
munwrap3 (mlist) = do
  list <- mlist
  case list of
    (a:b:c:rest) -> return (a,b,c,rest)
    _ -> error "munwrap3"


