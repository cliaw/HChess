module Chess.Board (
   Board,
   boardsize,
   locMatrix,
   Player,
   Location,
   Piece(..),
   emptyBoard,
   displayBoard,
   addToBoard,
   getBoardPiece,
   initializeBoard,
   onBoard
) where

import Data.Maybe
import Data.List
import Safe

-- Should be one or two
type Player = Int

data Piece =
     Empty
   | Pawn
   | Rook
   | Knight
   | Bishop
   | Queen
   | King
   deriving (Show, Eq)

type Location = (Int, Int)

newtype Board = Board { getBoard :: [[(Piece, Player)]] }
   deriving (Eq)

instance Show Board where
   show (Board b) = '\n' : go b where
      go []     = ""
      go (x:xs) = showCol x ++ "\n" ++ go xs
      showCol [] = ""
      showCol (x:xs) = case x of
         (Empty, _)  -> "XX"
         (Pawn, x)   -> 'P' : show x
         (Rook, x)   -> 'R' : show x
         (Knight, x) -> 'N' : show x
         (Bishop, x) -> 'B' : show x
         (Queen, x)  -> 'Q' : show x
         (King, x)   -> 'K' : show x
         ++ " " ++ showCol xs

boardsize :: Int
boardsize = 8

locMatrix :: [Location]
locMatrix = [(x,y) | x <- [1..boardsize], y <- [1..boardsize]]

emptyBoard :: Board
emptyBoard = Board $ replicate boardsize $ replicate boardsize (Empty, 0)

displayBoard :: Board -> IO ()
displayBoard brd = print brd

-- TODO: Change this to take a list of pieces and a list of locations
-- and update. It will be a lot faster that way.
addToBoard :: Board -> (Piece, Player) -> Location -> Maybe Board
addToBoard (Board b) (p,n) (x,y)
   | onBoard (x,y) = do
      let t = boardsize - y
      row <- b `atMay` t
      let row' = take (x-1) row ++ [(p,n)] ++ drop x row
      return $ Board $ take t b ++ [row'] ++ drop (t+1) b
   | otherwise = Nothing

getBoardPiece :: Board -> Location -> Maybe (Piece, Player)
getBoardPiece (Board b) (x,y) = do
   let n = boardsize - y
   row <- b `atMay` n
   row `atMay` (x-1)

initializeBoard :: Board
initializeBoard = Board $
   [base 2] ++ [pawns 2] ++
   replicate (boardsize - 4) empty ++
   [pawns 1] ++ [base 1]
   where base n = [ (Rook, n)
                  , (Knight, n)
                  , (Bishop, n)
                  , (Queen, n)
                  , (King, n)
                  , (Bishop, n)
                  , (Knight, n)
                  , (Rook, n)
                  ]
         pawns n = replicate boardsize (Pawn, n)
         empty   = replicate boardsize (Empty, -1)

onBoard :: Location -> Bool
onBoard (x,y) = x `elem` [1..boardsize]
             && y `elem` [1..boardsize]
