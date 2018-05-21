{-# LANGUAGE NoMonomorphismRestriction #-}

import Pru

main = do
  putStrLn "As IO:"
  test
  putStrLn "As Trace:"
  putStrLn $ show $ trace test

test = do  
  loop <- label
  mov r10 r11
  mov r11 r10
  jmp loop

  
  
  

  
