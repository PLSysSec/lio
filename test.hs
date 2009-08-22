
module Main where

import LIO.TCB
import LIO.HiStar

--
-- Crap
--

lstdin = lupdate lpure (HSC 1) L3

lgetLine = lio lstdin getLine

us = HSC 99
ul = lupdate lpure us L3

vs = HSC 104
vl = lupdate lpure vs L3

three, four :: Lref HSLabel Int
three = label ul 3
four = label vl 4

privs = HSPrivs [us, vs]


addem = do
  a <- getL three
  b <- getL four
  return $ a + b

a2 = do
  sum <- putL addem
  lputStrLn $ show sum
  

crap = do
  a <- getL three
  (p1, l1) <- newcat L2
  five <- getL $ label l1 5
  -- let five' = unlabel p1 (label l1 5)
  lputStrLn $ show five

foo = do
  x <- three
  y <- four
  return $ x + y

getnum = do
  lputStr "Enter a number: "
  s <- lgetLine
  return (read s :: Int)

mft ~(a, b) = do
  a' <- getnum 
  return (a', a+1)

main = return ()
