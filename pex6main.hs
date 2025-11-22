-- pex6.hs 
-- unKnot Haskell

-- name: Ana Flynn

{- DOCUMENTATION: I used https://hackage.haskell.org/package/CheatSheet-1.5/src/CheatSheet.pdf for syntax 
as well as https://wiki.haskell.org/index.php?title=99_questions/80_to_89 for syntax and logical questions about my pattern matching that i used as well as https://stackoverflow.com/questions/20251254/haskell-pattern-matching-a-list-of-tuples
I used https://en.wikipedia.org/wiki/Reidemeister_move to understand the problem better as well as https://www.researchgate.net/figure/Reidemeister-moves-of-type-I-II-and-III_fig2_274017961
-}

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | isTypeI tripCode = unKnot (makeTypeIMove tripCode)
   | isTypeII tripCode = unKnot (makeTypeIIMove tripCode)
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

-- typeIknot :: [(Char, Char)] -> String
-- typeIknot tripCode

-- detect if a type i knot is there (t/f)
isTypeI :: [(Char, Char)] -> Bool
isTypeI [] = False 
isTypeI [x] = False
isTypeI ((c1,t1):(c2,t2):xs) =
   if c1 == c2
   then True
   else isTypeI ((c2,t2):xs)

-- detect if a type ii knot is there (t/f)
isTypeII :: [(Char, Char)] -> Bool
isTypeII [] = False
isTypeII [x] = False
isTypeII [x,y] = False
isTypeII ((c1,t1):(c2,t2):(c3,t3):(c4,t4):xs) =
   if c1 /= c2 && t1 == t2 -- different letters but same type
      && t3 == t4 && t3 /= t1 -- same type but opposite of the first
      && ( (c3 == c1 && c4 == c2) || (c3 == c2 && c4 == c1))
   then True
   else isTypeII ((c2,t2):(c3,t3):(c4,t4):xs)
isTypeII _ = False

-- make a type i move
makeTypeIMove :: [(Char, Char)] -> [(Char, Char)]
makeTypeIMove [] = []
makeTypeIMove [x] = [x]
makeTypeIMove ((c1,t1):(c2,t2):xs) = 
   if c1 == c2
   then xs -- drop down c1 and c2
   else(c1,t1) : makeTypeIMove ((c2,t2):xs)


-- make a type ii move
makeTypeIIMove :: [(Char, Char)] -> [(Char, Char)]
makeTypeIIMove [] = []
makeTypeIIMove [x] = [x]
makeTypeIIMove [x,y] = [x,y]
makeTypeIIMove [x,y,z] = [x,y,z]
makeTypeIIMove ((c1,t1):(c2,t2):(c3,t3):(c4,t4):xs) = 
   if c1 /= c2 && t1 == t2
      && t3 == t4 && t3 /= t1
      && ( (c3 == c1 && c4 == c2) || (c3 == c2 && c4 == c1) )
   then xs
   else (c1,t1) : makeTypeIIMove ((c2,t2):(c3,t3):(c4,t4):xs)



main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

