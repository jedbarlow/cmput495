-- Copyright (C) 2014 Calvin Beck
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation files
-- (the "Software"), to deal in the Software without restriction,
-- including without limitation the rights to use, copy, modify, merge,
-- publish, distribute, sublicense, and/or sell copies of the Software,
-- and to permit persons to whom the Software is furnished to do so,
-- subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

import Data.List

data Grid = Grid [String]

instance Show Grid where
  show (Grid board) = '\n' : (concat . intersperse "\n-+-+-\n" . map (intersperse '|') $ board) ++ "\n\n"

-- Create the initial empty starting grid.
emptyGrid :: Grid
emptyGrid = Grid (replicate 3 $ replicate 3 ' ')


-- Check if the grid is full.
gridFull :: Grid -> Bool
gridFull (Grid board) = not . (elem ' ') . concat $ board


-- Check if the game is over (there is a winner, or the grid is full).
gameOver :: Grid -> Bool
gameOver grid = isWinner 'x' grid || isWinner 'o' grid || gridFull grid


-- Gets the diagonal from the top left to the bottom right corner.
diagonal :: Grid -> String
diagonal (Grid []) = []
diagonal (Grid board) = (head . head $ board):(diagonal tailGrid)
  where tailGrid = Grid (map tail (tail board))


-- Need to check each row, each column (transpose), and each diagonal
isWinner :: Char -> Grid -> Bool
isWinner c grid@(Grid board) = rowWin board || colWin board || diagWin
  where rowWin = any $ all (c==)
        colWin = rowWin . transpose
        diagWin = (all (c==) $ diagonal grid) || (all (c==) $ diagonal $ Grid (reverse board))


-- Check if the game is a draw.
isDraw :: Grid -> Bool
isDraw grid = gridFull grid && not (isWinner 'x' grid || isWinner 'o' grid)


-- Compute all possible rows for playing a character.
-- Returns an empty list if no plays are possible on this row.
possibleRow :: Char -> String -> [String]
possibleRow c [] = []
possibleRow c (x:xs) = (if x == ' ' then [c:xs] else []) ++ (map (x:) (possibleRow c xs))


-- All possible plays on a grid given a character.
possiblePlays :: Char -> Grid -> [Grid]
possiblePlays c (Grid []) = []
possiblePlays c (Grid (x:xs)) = [Grid (new_row:xs) | new_row <- (possibleRow c x)] ++ nextPlays
  where nextPlays = [Grid (x:board) | (Grid board) <- possiblePlays c subGrid]
        subGrid = (Grid xs)


-- Returns possible tic tac toe grids after a given player takes their
-- turn.
play :: Char -> Grid -> [Grid]
play c grid = if gameOver grid then [grid] else new_boards
  where new_boards = possiblePlays c grid


-- Calculates which player's turn it is.
getTurn :: Grid -> Char
getTurn (Grid board) = if xPieces > oPieces then 'o' else 'x'
  where xPieces = length . filter (=='x') $ allSpaces
        oPieces = length . filter (=='o') $ allSpaces
        allSpaces = concat board


-- Given a grid return all possible moves after the current player
-- goes.
moves :: Grid -> [Grid]
moves grid = play (getTurn grid) grid
