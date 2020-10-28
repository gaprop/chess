module Chess.Control where
import Chess
import Data.Array
import Data.List

--movePiece :: Board -> Piece -> Board

isBetween :: Ord a => (a, a) -> a -> Bool
isBetween (min, max) a = min <= a && a <= max

isInBound :: (Position, Position) -> Position -> Bool
isInBound (min, max) pos = isBetween (fst min, fst max) (fst pos) && 
                           isBetween (snd min, snd max) (snd pos)

validMovesAt :: Board -> Position -> Maybe [Position]
validMovesAt b@(Board board) (x, y) =
  let (Cell c) = cellAt b (x, y)
   in case c of
        Nothing -> Nothing
        Just (Black piece) -> staticPos (+) piece
        Just (White piece) -> staticPos (-) piece
  where 
        staticPos :: (Int -> Int -> Int) -> Figure -> Maybe [Position]
        staticPos f = Just . sort . nub . foldr1 (++) . map validPos . map (relToStatPos f) . movesFor

        relToStatPos :: (Int -> Int -> Int) -> [Position] -> [Position]
        relToStatPos f = map (\(x', y') -> (f x x', f y y'))

        validPos :: [Position] -> [Position]
        validPos = takeWhile $ isInBound ((0, 0), (size, size))

cellAt :: Board -> Position -> Cell
cellAt (Board b) (x, y) = 
  let row = b ! y
  in row ! x

move :: Board -> Position -> Position -> Board
move b@(Board board) from to =
  let cell = cellAt b from
   in Board $ insert2d board [(from, Cell Nothing), (to, cell)]

-- Returns a list of lists. If the piece can not move to some infinity, then there is only one list.
-- Otherwise, it will have a list that goes to infinity in the directions that the piece can move.
-- For instance rook can move forward, backward, sideways infinity.
movesFor :: Figure -> [[Position]]
movesFor Pawn = [[(0, 1), (0, 2), (1, 1), (-1, 1)]]
movesFor Knight = [[(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)]]
movesFor King = [(,) <$> [-1, 0, 1] <*> [-1, 0, 1]]
-- These are the pieces that can move in some infinite direction.
movesFor Rook = [ map ((,) 0) [0..]           -- North direction
                , map (flip (,) 0) [0..]      -- East direction
                , map ((,) 0) [0,-1..]        -- South direction
                , map (flip (,) 0) [0, -1..]  -- West direction
                ]
movesFor Bishop = [ zip [0..] [0..]        -- North east direction
                  , zip [0..] [0, -1..]    -- South east direction
                  , zip [0,-1..] [0, -1..] -- South west direction
                  , zip [0,-1..] [0..]     -- North west direction
                  ]
movesFor Queen = movesFor Rook ++ movesFor Bishop
