module Chess.Chess where

-- Implement game play logic and enforce rules!

import Chess.Board

import Data.Maybe
import Data.List (nub)
import Data.Composition ((.:))
import Control.Monad (join)
import Safe

-- Not very efficient...
isInCheck :: Board -> Player -> Bool
isInCheck b n = case findPiece b (King,n) of
   Nothing -> False
   Just l -> l `elem` (go locMatrix)
      where go = nub . join . map (getAllMovesFrom b)

-- TODO: implement this.
isCheckmate :: Player -> Board -> Bool
isCheckmate p b = False

-- Check if cell is occupied by player p
isOccupiedBy :: Board -> Player -> Location -> Bool
isOccupiedBy b p l = case getBoardPiece b l of
   Nothing        -> False
   Just (Empty,_) -> False
   Just (_,x)     -> p == x

findAllPiece :: Board -> (Piece, Player) -> [Location]
findAllPiece b (p,n) = filter f locMatrix
   where f l = Just (p,n) ==  getBoardPiece b l
--
-- Find one instance of a particular piece
findPiece :: Board -> (Piece, Player) -> Maybe Location
findPiece = headMay .: findAllPiece

isOccupied :: Board -> Location -> Bool
isOccupied b l = isOccupiedBy b 1 l || isOccupiedBy b 2 l

-- Determines whether or not it is possible to move from p1 to p2
-- This ignores anything else in the way for now
-- Question.. Keep this or no? Think about it...
-- checkMoveConstraint :: Piece -> (Location -> Location -> Bool)
-- checkMoveConstraint King = \(x1,y1) (x2,y2) -> abs(x1-x2) + abs(y1-y2) == 1

-- There are essentially two immediate ways of determining whether or not a
-- move is valid.
-- One way, which we are doing right now, is to find a move constraint.
-- Another way would be to generate a list, array, hash, etc (basically
-- some collection) and test for membership.
-- In the latter case, we can modify the getAllMoves function. The more I think
-- about it, the more the getAllMoves function makes sense, especially when it
-- comes to speed, but let's do the (what I currently believe is) simple case
-- first.
-- I am still very hesitant in defining this function..
-- checkMoveConstraint :: Board -> (Piece, Player) ->
--                        (Location -> Location -> Bool)

-- Get all moves starting at some location
-- This is to be replaced by get all moves and figure out location by itself
getAllMoves :: Board -> (Piece, Player) -> Location -> [Location]
getAllMoves b (p,n) l = filter (isMoveValid b (p,n) l) xs
   where xs = [(x,y) | x <- [1..boardsize], y <- [1..boardsize]]

getAllMovesFrom :: Board -> Location -> [Location]
getAllMovesFrom b l = case getBoardPiece b l of
   Nothing -> []
   Just p -> getAllMoves b p l

isMoveValid :: Board -> (Piece, Player) -> Location -> Location -> Bool
isMoveValid _ (Empty,_) _ _ = False
isMoveValid b (p,n) l1@(x1,y1) l2@(x2,y2) =
   onBoard (x1, y1) && onBoard (x2, y2) &&
   not (isOccupiedBy b n (x2,y2)) && l1 /= l2 &&
   -- Just (p,n) == getBoardPiece b l1 &&
   case p of
      -- King can only move one direction in each of 8 directions
      King -> abs(x1 - x2) <= 1 && abs(y1 - y2) <= 1

      -- A bishop and rook combined!
      Queen -> (isMoveValid b (Rook,n) l1 l2) ||
               (isMoveValid b (Bishop,n) l1 l2)

      -- A rook can only move horizontally or vertically.
      -- Moreover, a rook cannot go past any occupied square.
      -- It can land on an enemy cell, but not on any cell by its own piece
      Rook -> ((x1 == x2 && y1 /= y2 && goV) ||
               (x1 /= x2 && y1 == y2 && goH))
         where goV = foldr (&&) True $ map f ys
               goH = foldr (&&) True $ map f xs
               f = \l -> not (isOccupied b l)
               ys  = [(x1,y) | y <- [1..boardsize],
                               y > min y1 y2, y < max y1 y2]
               xs  = [(x,y1) | x <- [1..boardsize],
                               x > min x1 x2, x < max x1 x2]

      -- A bishop can move diagonally.
      Bishop -> abs(x1-x2) == abs(y1-y2) && goD
         where goD = foldr (&&) True $ map f xys
               f = \l -> not (isOccupied b l)
               xys = [(x1+d, y1+s*d) | d <- [-boardsize..boardsize],
                                       x1+d > min x1 x2, x1+d < max x1 x2]
               s = signum((x1-x2)*(y1-y2))

      -- For a night d(l1,l2) == 3, where d is the Manhattan distance.
      -- Moreover, both the x coordinates and y coordinates in l1, l2
      -- must change.
      Knight -> x1 /= x2 && y1 /= y2 && abs(x1-x2) + abs(y1-y2) == 3

      -- Logic for P1 and P2 can be made the same if for P2, we rotate board.
      -- A pawn is only ever in row 2 (or row B) at most once, in which
      -- case, it can go up two provided no piece is in the way.
      -- A pawn usually can only move one step at a time unless there is
      -- an enemy piece one diagonal above from it
      Pawn -> (x2' == x1' && y2' == y1'+2 && y2' == 2 &&
               not (isOccupied b (x1,y2+1))) ||
              (x2' == x1' && y2' == y1'+1) ||
              (abs(x2'-x1') == 1 && y2' == y1'+1 &&
               -- The weird bit, but note that other pieces were not rotated
               isOccupiedBy b (3-n) (x2,y2))
         where ((x1',y1'),(x2',y2')) = if n == 2 then (rotate l1,rotate l2)
                                       else (l1, l2)
               rotate (x,y) = (boardsize-x,boardsize-y)

-- This needs to be fixed.. I want move to use the Writer monad and
-- explicitly tell me whether or not the move was made or not, but for now
-- this will do as a test
move :: Board -> (Piece, Player) -> Location -> Location -> Maybe Board
move b (p,n) l1@(x1,y1) l2@(x2,y2)
   | isMoveValid b (p,n) l1 l2 =
      if isJust b' && not (isInCheck (fromJust b') n) then b'
      else Nothing
   | otherwise = Nothing
   where b' = return b >>= \b1 -> addToBoard b1 (Empty,-1) l1
                       >>= \b2 -> addToBoard b2 (p,n)      l2

b = fromJust $ return emptyBoard >>= \b1 -> addToBoard b1 (Rook,1) (1,1)
                                 >>= \b2 -> addToBoard b2 (Pawn,1) (3,4)
                                 >>= \b3 -> addToBoard b3 (Pawn,1) (2,5)
                                 >>= \b4 -> addToBoard b4 (Knight,1) (3,3)
                                 >>= \b5 -> addToBoard b5 (Bishop,1) (4,3)
                                 >>= \b6 -> addToBoard b6 (Pawn,2) (5,2)
                                 >>= \b7 -> addToBoard b7 (Queen,2) (8,8)
--                                  >>= \b8 -> addToBoard b8 (Bishop,2) (8,6)
                                 >>= \b9 -> addToBoard b9 (King,1) (7,1)
                                 >>= \b10 -> addToBoard b10 (Rook,2) (1,6)
xs = [(x,y) | x <- [1..8], y <- [1..8]]
test = getAllMoves b (King,1) (7,1)
