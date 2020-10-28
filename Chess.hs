module Chess where
import Data.Char
import Data.Array

--data Board = Board [[Cell]] deriving(Show)
data Board = Board (Array Int (Array Int Cell)) deriving(Show)

type Position = (Int, Int)

newtype Cell = Cell (Maybe Piece) deriving(Show)

data Piece = Black Figure | White Figure deriving(Show)

data Figure = King
            | Queen
            | Rook
            | Bishop
            | Knight
            | Pawn
            deriving(Show)

size :: Int
size = 7

-- We use FEN notation
-- For white: KQRBNP
-- For black: kqrbnp

showFigure :: Figure -> Char
showFigure King   = 'K'
showFigure Queen  = 'Q'
showFigure Rook   = 'R'
showFigure Bishop = 'B'
showFigure Knight = 'N'
showFigure Pawn   = 'P'

readFigure :: Char -> Figure
readFigure 'K' = King
readFigure 'Q' = Queen
readFigure 'R' = Rook
readFigure 'B' = Bishop
readFigure 'N' = Knight
readFigure 'P' = Pawn


-- Pieces
showPiece :: Piece -> Char
showPiece (Black p) = toLower . showFigure $ p
showPiece (White p) = showFigure p

readPiece :: Char -> Piece
readPiece p
  | isLower p = Black . readFigure . toUpper $ p
  | otherwise = White . readFigure $ p

-- Cell
showCell :: Cell -> Char
showCell (Cell (Just p)) = showPiece p
showCell (Cell Nothing) = ' '

readCell :: Char -> Cell
readCell ' ' = Cell Nothing
readCell p = Cell $ Just $ readPiece p

-- Board
showBoard :: Board -> String
showBoard (Board b) = unlines $ map showRow $ toList b
  where toList = arrayList . fmap arrayList
        showRow = map showCell

readBoard :: String -> Board
readBoard b = Board $ toArray2d $ map readRow $ lines b
  where toArray2d = listArray (0, size) . map (listArray (0, size))
        readRow = map readCell

initBoard :: Board
initBoard = readBoard $ unlines ["rnbqkbnr"
                                ,"pppppppp"
                                ,"        "
                                ,"        "
                                ,"        "
                                ,"        "
                                ,"PPPPPPPP"
                                ,"RNBQKBNR"
                                ]

-- Utilities
arrayList :: Ix i => Array i e -> [e]
arrayList = foldr (:) []

insert2d :: Ix i => Array i (Array i e) -> [((i, i), e)] -> Array i (Array i e)
insert2d arr elms = foldl insert arr elms
        -- Insert the elm at the x, y location of the array.
  where insert arr ((x, y), elm) = arr // [(y, (arr ! y) // [(x, elm)])]
