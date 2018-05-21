module Pru where



class Monad m => Pru m where
  reg  :: String -> m Operand
  asm2 :: (Operand -> Operand -> Instruction) -> Operand -> Operand -> m ()

data Operand = Reg String deriving Show

data Instruction = Mov Operand Operand deriving Show

r10 :: Pru m => m Operand
r10 = reg "R10"
r11 :: Pru m => m Operand
r11 = reg "R11"

mov :: Pru m => m Operand -> m Operand -> m ()
mov = op2 Mov

-- Debug: print to console.
instance Pru IO where
  reg r = return $ Reg r
  asm2 o a b = do
    putStrLn $ show $ o a b
    return ()

op2 o a b = do
  a' <- a;
  b' <- b
  asm2 o a' b'
  

trace = id

  
