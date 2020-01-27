-- The objective is to create a continuation compiler that can plug
-- into the mini CSP scheduler in uc_tools.  I want to be able to drop
-- some C files into a code base without creating a large dependency.

-- In any case, it is an interesting problem.  I might be able to take
-- some different approaches, especially in the realm of "functional
-- forms", which I currently see as immediately inlined higher order
-- functions.

-- Instead of starting from LC, it seems more appropriate to start
-- from the central data structure: the stack, and the observation
-- that frames can be added to the stack in two ways:

-- 1. Hidden: this gives traditional first order functions.  The
--    hiding is a 'feature'.  It means the function can be hosted at
--    an arbitrary point in the stack.
--
-- 2. Exposed: inlined higher order forms, downward closures.  Maybe
--    even just 'for'.

-- So all this boils down to two wishes:
-- . compile down to plain C without malloc
-- . allow for high abstraction at macro level


-- Ideas:
--
-- 1. Compile via abstract interpretation.
--
-- 2. Represent in C using "maximal" structs to reduce struct clutter.
--
-- 3. Output is transition functions taking input env + select resp,
-- producing output env + select req

-- This needs an implementation.

-- So I don't know if the distinction between environments and stacks
-- is so important.  Let's just put down an ANF style language.  This
-- is for tasks only, so there is no "return value".  A program can
-- only Halt or tail call another program.  Here I am already confused
-- about expressions returning values, and send/receive.  For now,
-- don't use select maybe?  Focus only on S/R.


module Language.Cont where

data Prog = Halt
          | Call Prog


test = ()





