module CFG where
-- only for a simple CFG: E -> n | (E+E)

import Data.List

-- The data type definition.  A tree can consist of either a Leaf or a node with a value and two tree.
data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show,Eq)

type Symbol     = Char

-- Some examples of structure in code
t1 = Leaf 'E'                  
l1 = "E" 
t2 = Node '+' (Leaf 'E')(Leaf 'E')
l2 = "(E+E)"
t3 = Node '+' (Leaf 'n')(Node '+' (Leaf 'E')(Leaf 'E'))
l3 = "(n+(E+E))"
t4 = Node '+' (Node '+' (Leaf 'n') (Node '+' (Leaf 'n') (Leaf 'n'))) (Leaf 'n')
l4 = "(n+(n+n))+n)"

-- The size of the tree - the number of items in the tree
size  :: Tree a -> Int
size (Leaf _)           = 1
size (Node _ tl tr)     = 1 + size tl + size tr

-- define the terminal and non-terminal variable
terminals       = ['n','(',')','+']
nonTerminals    = ['E']
symbols         = terminals ++ nonTerminals
startSymbol     = 'E'
grammer         = "CFG: ({E},{n,+,(,)},P,E) \n\t\t where P = E -> n | (E+E)"

isTerminal :: Char -> Bool
isTerminal c = elem c terminals

isNonTerminal :: Char -> Bool
isNonTerminal c = elem c nonTerminals

-- Converts a tree into symbols which is the language of CFG.
treeToList  :: Tree Symbol -> [Symbol]
treeToList (Leaf x)        = [x]
treeToList (Node x xl xr)  = "(" ++ treeToList xl ++ x : treeToList xr ++ ")"

-- scanner the input string list, if all the terminal symbols then return the symbols, otherwise error message comeout.
scanner :: String -> [Symbol]
scanner []                              = []
scanner (' ':xs)                        = scanner xs
scanner (x:xs)          | isTerminal x  = x: (scanner xs)
                        | otherwise     = error "It is not all the terminal symbols."

parser :: [Symbol] -> Bool
parser ts = case parse xs of
                Just ['E'] -> True
                _       -> False
            where xs = replaceFp1 ts     
                 
parse :: [Symbol] -> Maybe [Symbol]
parse ['E']      = Just ['E']
parse ('(':ts)   = findRB ('(':ts) 0                             
parse _          = Nothing

replaceFp1 ::  [Symbol] -> [Symbol]
replaceFp1 xs = [if x == 'n' then 'E' else x | x <- xs]

-- find the first right bracket, and then check the symbol befor, if can then reduced
findRB :: [Symbol] -> Int -> Maybe [Symbol]
findRB xs index =  case (index == last) of
                        True -> case (xs !! index) of
                                        ')' -> replaceFp2 xs index
                                        _   -> Nothing
                        False -> case (xs !! index) of
                                        ')' -> replaceFp2 xs index
                                        _   -> findRB xs (index+1) 
                   where last  = (length xs) -1 
                   
replaceFp2 :: [Symbol] -> Int -> Maybe [Symbol]
replaceFp2 [] _ = Nothing
replaceFp2 xs n | n >= 4        = case (xs !! (n-4)) of 
                                        '(' ->  case (xs !! (n-3)) of 
                                                        'E' ->  case (xs !! (n-2)) of 
                                                                        '+' ->  case (xs !! (n-1)) of 
                                                                                        'E' -> Just (ll++('E':lr'))
                                                                                                where (ll, lr)  = splitAt (n-4) xs
                                                                                                      lr' = drop 5 lr
                                                                                        _   -> Nothing
                                                                        _   -> Nothing
                                                        _ -> Nothing
                                        _ -> Nothing
                | otherwise     = Nothing
                                        
listToTree :: [Symbol] -> Tree Symbol
listToTree [x]        = Leaf x
listToTree ('(': xs)  = Node '+' (listToTree ll) (listToTree lr)
                                where { n = findPlus xs 0 1 ;        -- find the index of + in pair (..+..)
                                        (ll, lr') = splitAt n xs  ;
                                        lr = if (last lr') == ')' then init (tail lr') else error "Can not constrint to a binary tree."   }   
listToTree _          = error "Can not constrint to a binary tree."   

-- count is the number of '(', return the index of '+'                                
findPlus :: [Symbol] -> Int -> Int -> Int
findPlus []       index count =  0                                     -- error, didn't find the '+' we wanted
findPlus ('(':xs) index count =  findPlus xs (index+1) (count+1)
findPlus (')':xs) index count =  findPlus xs (index+1) (count-1)
findPlus ('+':xs) index count =  if count == 1 then index else findPlus xs (index+1) (count)
findPlus (_  :xs) index count =  findPlus xs (index+1) (count)

-- show the tree by indenting each next node and additional tab in, outputs the right side, then the node, then the left side
pict :: Tree Symbol -> IO()
pict t = putStr (pic "" t)
         where pic ind (Leaf x) = ind ++ [x]
               pic ind (Node x tl tr) = pic ('\t':ind) tr ++ "\n" ++ 
                                        ind ++ [x]        ++ "\n" ++
                                        pic ('\t':ind) tl ++ "\n"

-- define the product function
-- we do leftmost derivation
-- E -> n | (E+E)
pf1 :: [Symbol] -> [Symbol]
pf1 ('E': xs) = 'n' : xs
pf1 (x : xs)  = x : (pf1 xs)
pf1 _         = error "It is not a CFG."

pf2 :: [Symbol] -> [Symbol]
pf2 ('E': xs) = "(E+E)" ++ xs
pf2 (x : xs)  = x : (pf2 xs)
pf2 _         = error "It is not a CFG."

main :: IO()
main = do putStrLn grammer
          putStrLn "The start symbol is E."
          play [startSymbol]

play :: [Symbol] -> IO()
play b  | and (map isTerminal b) = 
                do
                        putStrLn "It is a languange of CFG."   
                        pict (listToTree b)
        | otherwise   = 
                do 
                        putStrLn "Please enter next step: 1 is E->n ,and 2 is E -> (E+E)."
                        input <- getLine
                        case (read input) :: Int of
                                1 -> do
                                        let s = pf1 b
                                        putStrLn s
                                        play s
                                2 -> do 
                                        let s = pf2 b
                                        putStrLn s
                                        play s
                                _ -> do
                                        error "It is not a vaild grammer."
                
