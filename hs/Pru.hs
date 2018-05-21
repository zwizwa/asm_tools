module Pru where



class Monad m => Pru m where
  reg :: String -> m Op
  op2 :: (Op -> Op -> Ins) -> m Op -> m Op -> m ()

data Op = Reg String deriving Show
data Ins = Mov Op Op deriving Show

r10 :: Pru m => m Op
r11 :: Pru m => m Op
r10 = reg "R10"
r11 = reg "R11"

mov :: Pru m => m Op -> m Op -> m ()
mov = op2 Mov

-- Debug: print to console.
instance Pru IO where
  reg r = return $ Reg r
  op2 ins a b = do
    a' <- a ; b' <- b
    putStrLn $ show $ ins a' b'


trace = id

  
