{- HOW TO USE
   Enter number of solutions you want. Enter a negative number or 0 if you want all solutions.
   Enter a new line.
   Enter the puzzle. The puzzle can have extra characters that are not specified
   in the variable characters or empty.
-}

module Main where

import Data.Maybe(isJust, fromJust)
import Control.Monad(foldM)
import Data.List(intersperse)

characters = "123456789"
empty = ".0"
boardsize = 9
boxsize = 3

type Matrix a = [[a]]
type Board = Matrix Char
type Choices = [Char]
type Rule1 = Integer
type Rule2 = Integer
type Search = Integer
type Reason = Integer --0, 1, 2
type ChoicesStats = (Choices, Rule1, Rule2, Search)
type Coord = (Int, Int)


{-
   Rule 1: If only one choice left, then eliminate that choice from all the square's peers
   Rule 2: If only one square in a unit has a number x in its choices, then assign
           x to that square
-}

main :: IO ()
main = do str <- getLine
          let int = (read str) :: Int
          input <- getContents
          let board = parseGrid input
          if isJust board
          then do let result = sudoku input
                  if isJust result
                  then do let result1 = (if int > 0
                                        then take int (fromJust result)
                                        else fromJust result)
                              result2 = map (map (map show)) result1
                              justValues = (map (map (map fst4))) result1
                              printed1 = map printMatrix result2
                              printed2 = map printMatrix justValues
                          sequence_ printed1
                          sequence_ printed2
                  else putStrLn "No Solution"
          else putStrLn "Invalid Board"
  where fst4 (a, b, c, d) = a

-- assign assigns a value to a square by eliminating all other possibilities
assign :: Matrix ChoicesStats -> (Coord, Char, Reason) -> Maybe (Matrix ChoicesStats)
assign board (coord, val, reason) = if elem val empty -- if assigning blank char, do nothing
                            then Just board
                            else let removable = filter (/= val) (get board coord)
                                 in foldM eliminate board
                                          (zip3 (repeat coord) removable (repeat reason))

-- eliminate eliminates a possibility from a square. After applying rule 1,
-- it passes the result to rule2

eliminate :: Matrix ChoicesStats -> (Coord, Char, Reason) -> Maybe (Matrix ChoicesStats)
eliminate board (coord, val, reason) =
  if not (elem val (get board coord)) -- Nothing to eliminate
  then Just board
  else
    let newBoard = remove board coord val reason
        newChoices = get newBoard coord
    in do nextBoard <- case newChoices of
                         [] -> Nothing -- Contradiction, no possibility left
                         [x] -> let newBoard2 = foldM eliminate newBoard -- Application of Rule 1
                                         (zip3 (peers coord) (repeat x) (repeat 0))
                                in newBoard2
                         xs -> Just newBoard
          foldM (rule2 val) nextBoard [row coord, col coord, box coord]

-- rule2 checks a group for squares where val is a possibility
-- If there is only one such suqare, then assign it there 
rule2 :: Char -> Matrix ChoicesStats -> [Coord] -> Maybe (Matrix ChoicesStats)
rule2 val board group = let choices = map (get board) group
                            helper = zip group choices
                            helperfilter (coord, choices) = elem val choices
                            result = map fst . filter helperfilter $ helper
                        in if result == [] -- no possible place for val in group => Not solution
                           then Nothing
                           else if ((length result) == 1 --Only one place for val => Rule 2
                                   && not (length (get board (head result)) == 1))
                                then assign board ((head result), val, 1)
                                else Just board

-- sudoku takes string and return a list of solutions with accompanying statistics
-- It string is invalid or there are no solutions, then Nothing is returned
sudoku :: String -> Maybe [Matrix ChoicesStats]
sudoku string = do squares  <- parseGrid string
                   squares2 <- foldM assign startingBoard
                                     (zip3 linearcoord squares (repeat (-1)))
                   let result = search squares2 in
                     if result == [] then Nothing else Just result

-- Starting board is a matrix with all the possibilities and stats at 0
startingBoard = replicate boardsize (replicate boardsize (characters, 0, 0, 0))

-- Search finds a square with the least number of possibilites and
-- enumerates in a list the elimination of each possibilitiy
-- If board is solved, then the board is returned,
-- Else, search runs recursively

search :: Matrix ChoicesStats -> [Matrix ChoicesStats]
search board = let helper = zip linearcoord (concat board)
                   helper2 =  [(l, (x,y), (choices, rule1, rule2, search1)) |
                               ((x,y), (choices, rule1, rule2, search1)) <- helper,
                               let l = length choices,
                               l /= 1]
               in case helper2 of
                    [] -> [board]
                    _  -> let (l, (x,y), (choices, rule1, rule2, search1)) = minimum helper2
                          in  (concat . map (search . fromJust) . filter isJust)
                              [eliminate board ((x,y), val, 2) | val <- choices]

parseGrid :: String -> Maybe [Char]
parseGrid string = let filtered = [x | x <- string, (elem x characters) || (elem x empty)]
                   in  if (length filtered) /= (boardsize * boardsize)
                       then Nothing
                       else Just filtered

changeBoard :: Matrix ChoicesStats -> (ChoicesStats -> ChoicesStats) -> Coord -> Matrix ChoicesStats
changeBoard (xs:xss) f (x,y)
  | x == 0 = (changeY xs f y):xss
  | otherwise = xs:(changeBoard xss f (x-1, y))
  where
    changeY :: [ChoicesStats] -> (ChoicesStats -> ChoicesStats) -> Int -> [ChoicesStats]
    changeY (x:xs) f y
      | y == 0 = (f x):xs
      | otherwise = x:(changeY xs f (y - 1))


remove :: Matrix ChoicesStats -> Coord -> Char -> Reason -> Matrix ChoicesStats
remove board coord val reason = changeBoard board (removeHelper val reason) coord
  where removeHelper val reason t@(choices, rule1, rule2, search)
          | not (val `elem` choices) = t -- Nothing to remove
          | reason == 0  = ((filter (/= val) choices), rule1 + 1, rule2, search)
          | reason == 1  = ((filter (/= val) choices), rule1, rule2 + 1, search)
          | reason == 2  = ((filter (/= val) choices), rule1, rule2, search + 1)
          | otherwise = ((filter (/= val) choices), rule1, rule2, search)

get :: Matrix ChoicesStats -> Coord -> Choices
get board (x,y) = let (choices, _, _, _) = board !! x !! y
                  in choices

incrementReason :: Matrix ChoicesStats -> Coord -> Int -> Matrix ChoicesStats
incrementReason board coord n = changeBoard board (helper n) coord
  where helper n s@(choices, rule1, rule2, search)
          | n == 0 = (choices, rule1 + 1, rule2, search)
          | n == 1 = (choices, rule1, rule2 + 1, search)
          | n == 2 = (choices, rule1, rule2 + 1, search)
          | otherwise = s

printMatrix :: Matrix String -> IO ()
printMatrix board = let linear = (concat (intersperse ["\n"] board)) ++ ["\n\n"] 
                        seq = map putStr linear
                    in sequence_ seq

linearcoord :: [Coord]
linearcoord = [(x, y) | x <- [0..(boardsize - 1)],
                        y <- [0..(boardsize - 1)]]
groupBy x [] = []
groupBy x xs = take x xs : groupBy x (drop x xs)

transpose (xs:[]) = [[x] | x <- xs]
transpose (xs:xss) = zipWith (:) xs (transpose xss)

merge xs = foldr1 (zipWith (++)) xs

removedup [] = []
removedup (x:xs) = x:(filter (/=x) (removedup xs))

rows = groupBy boardsize linearcoord
cols = transpose rows
boxes = (concat . map merge . groupBy boxsize . map (groupBy boxsize)) rows

peers :: Coord -> [Coord]
peers coord = (filter (/= coord) . removedup . concat . filter (elem coord)) (rows ++ cols ++ boxes)

row, col, box :: Coord -> [Coord]
row coord = head $ filter (elem coord) rows
col coord = head $ filter (elem coord) cols
box coord = head $ filter (elem coord) boxes
