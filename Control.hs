module Chess.Control where
import Chess
import Data.Array
import Data.List
import Data.Maybe

isBetween :: Ord a => (a, a) -> a -> Bool
isBetween (min, max) a = min <= a && a <= max

isInBound :: (Position, Position) -> Position -> Bool
isInBound (min, max) pos = isBetween (fst min, fst max) (fst pos) && 
                           isBetween (snd min, snd max) (snd pos)

isEmptyCell :: Board -> Position -> Bool
isEmptyCell b pos = (\(Cell x) -> isNothing x) . cellAt b $ pos

isFriendly :: Board -> Piece -> Position -> Bool
isFriendly board piece pos = 
  let (Cell cell) = cellAt board pos
   in case piece of
        White _ -> case cell of 
                     Just (White _) -> True
                     Just (Black _) -> False
                     Nothing        -> False
        Black _ -> case cell of
                     Just (White _) -> False
                     Just (Black _) -> True
                     Nothing        -> False
        
validMovesAt :: Board -> Position -> [Position]
validMovesAt b@(Board board) (x, y) =
  let (Cell c) = cellAt b (x, y)
   in case c of
        Nothing    -> []
        Just piece -> staticPos piece
  where 
        staticPos :: Piece -> [Position]
        staticPos p = sort . foldr1 (++) . map (validPos p . relToStatPos p) . movesFor . figure $ p

        figure :: Piece -> Figure 
        figure (White p) = p
        figure (Black p) = p

        relToStatPos :: Piece -> [Position] -> [Position]
        relToStatPos (White piece) = map (\(x', y') -> (x - x', y - y'))
        relToStatPos (Black piece) = map (\(x', y') -> (x + x', y + y'))

        validPos :: Piece -> [Position] -> [Position]
        validPos p = filter (not . isFriendly b p) . takeUntil (isEmptyCell b) . inBoundPos

        inBoundPos :: [Position] -> [Position]
        inBoundPos = takeWhile $ isInBound ((0, 0), (size, size))


cellAt :: Board -> Position -> Cell
cellAt (Board b) (x, y) = 
  let row = b ! y
  in row ! x

move :: Board -> Position -> Position -> Board
move b@(Board board) from to =
  let cell = cellAt b from
   in Board $ insert2d board [(from, Cell Nothing), (to, cell)]

-- Returns a list of lists. Each list is a directio that the piece can move.
-- For instance rook can move forward, backward, sideways infinitly.
movesFor :: Figure -> [[Position]]
movesFor Pawn = [[(0, 1)], [(0, 2)], [(1, 1)], [(-1, 1)]]
movesFor Knight = [[(1, 2)], [(2, 1)], [(2, -1)], [(1, -2)], [(-1, -2)], [(-2, -1)], [(-2, 1)], [(-1, 2)]]
movesFor King = map (: []) . filter (\pos -> pos /= (0, 0)) $ (,) <$> [-1, 0, 1] <*> [-1, 0, 1]
-- These are the pieces that can move in some infinite direction.
movesFor Rook = [ map ((,) 0) [1..]           -- North direction
                , map (flip (,) 0) [1..]      -- East direction
                , map ((,) 0) [-1,-2..]        -- South direction
                , map (flip (,) 0) [-1,-2..]  -- West direction
                ]
movesFor Bishop = [ zip [1..] [1..]        -- North east direction
                  , zip [1..] [-1, -2..]    -- South east direction
                  , zip [-1, -2..] [-1, -2..] -- South west direction
                  , zip [-1, -2..] [1..]     -- North west direction
                  ]
movesFor Queen = movesFor Rook ++ movesFor Bishop
