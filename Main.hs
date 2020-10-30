module Main where
import qualified Chess as C
import Chess.Control
import System.Console.ANSI (clearScreen)
import Data.Char

data Player = White | Black deriving(Show)

data State = State { board :: C.Board
                   , turn :: Player
                   , input :: String
                   , position :: Maybe C.Position} 
                   deriving(Show)

main = play

play :: IO State
play = play' $ State C.initBoard White "" Nothing
  where 
    play' state = 
      if hasWon state 
      then do
        let winner = turn state
        putStrLn $ "The Winner is: " ++ show winner
        return state
      else do
        l <- getLine
        clearScreen
        return $ state { input = l }
        >>= printState
        >>= printHelp
        >>= choosePosition
        >>= printValidPositions
        >>= printTurn
        >>= chooseMove
        >>= printDebug
        >>= play'

hasWon :: State -> Bool
hasWon state =
  let (C.Board board') = board state
   in not $ any (\row -> any (\cell -> isWhiteKing cell) row) board' &&
            any (\row -> any (\cell -> isBlackKing cell) row) board'
  where isWhiteKing :: C.Cell -> Bool
        isWhiteKing (C.Cell (Just (C.White C.King))) = True 
        isWhiteKing _ = False

        isBlackKing :: C.Cell -> Bool
        isBlackKing (C.Cell (Just (C.Black C.King))) = True
        isBlackKing _ = False

printState :: State -> IO State
printState state =
  let board' = board state
      player = turn state
      pos    = position state
  in do
    putStrLn $ "Current player is " ++ show player
    case pos of
      Just p -> putStrLn $ "Choosen position is " ++ show p
      Nothing -> putStrLn $ "Choosen position is nothing"
    putStrLn . C.prettyPrintBoard $ board'
    putStrLn "-------------------------------------------"
    return state

printHelp :: State -> IO State
printHelp state =
  let l = input state
   in case l of
    "h" -> do
      putStrLn "Options:"
      putStrLn "h               Print this screen."
      putStrLn "Any button      Print the current board."
      putStrLn "x y             Pick a piece at the x,y position."
      putStrLn "m               Prints valid moves for the currrently selected position."
      putStrLn "m x y           Move the currrently selected piece to x y."
      putStrLn "t               Prints the current players turn."
      putStrLn "debug           Prints debug information."
      return state
    otherwise -> 
      return state

printTurn :: State -> IO State
printTurn state =
  let l = input state
      player = turn state
   in case l of
        "t" -> do
          putStrLn $ show player
          return state
        otherwise -> return state

printDebug :: State -> IO State
printDebug state =
  let l = input state
   in case l of
        "debug" -> do
          print $ position state
          return state
        otherwise -> return state

chooseMove :: State -> IO State
chooseMove state =
  let inputs = words $ input state
      from   = position state
      board' = board state
      turn'  = turn state
   in if length inputs > 0
   then case (head inputs) of
        "m" | isValidPos $ tail inputs -> move' board' turn' from (getPos $ tail inputs)
        otherwise                      -> return state
   else return state
  where 
        move' :: C.Board -> Player -> Maybe C.Position -> C.Position -> IO State
        move' board' turn' from to =
          case from of
           Just from | 
             to `elem` (validMovesAt board' from) &&
              isCurrentTurnsPiece board' from turn' -> return $ state { board= move board' from to, turn=switchTurn $ turn state }
           otherwise -> do
             putStrLn "Not a valid move."
             return state

        getPos :: [String] -> C.Position
        getPos [p1, p2] = (read p1, read p2)

        
isCurrentTurnsPiece :: C.Board -> C.Position -> Player -> Bool
isCurrentTurnsPiece board pos player =
  let (C.Cell c) = cellAt board pos
   in case (c, player) of
        (Just (C.White _), White) -> True
        (Just (C.Black _), Black) -> True
        otherwise               -> False

choosePosition :: State -> IO State
choosePosition state = 
  let inputs = words $ input state
   in if isValidPos inputs
  then do
    putStrLn $ "Picked position at " ++ (inputs !! 0) ++ " " ++ (inputs !! 1)
    return $ state { position = getPos inputs }
  else do
    return state
    where getPos :: [String] -> Maybe C.Position
          getPos [p1, p2] = Just $ (read p1, read p2)

isValidPos inputs 
 | length inputs == 2 =
   ((== 1) $ length $ (inputs !! 0)) && 
   ((== 1) $ length $ (inputs !! 1)) &&
   (isDigit $ flip (!!) 0 $ (inputs !! 0)) && 
   (isDigit $ flip (!!) 0 $ (inputs !! 1))
 | otherwise = False

-- This function gets a little awkward
printValidPositions :: State -> IO State
printValidPositions state = do
  let l = input state
   in case l of
        "m" -> print'
        otherwise -> return state
  where 
    print' :: IO State
    print' = 
      let pos = position state
          b@(C.Board b')   = board state
          board' = fmap (\row -> fmap C.showCell row) b'
       in case pos of
            Just p -> 
              let moves = C.arrayList . fmap C.arrayList . C.insert2d board' . map (\pos' -> (pos', 'X')) $ validMovesAt b p
               in do putStrLn $ unlines moves
                     return state
            otherwise -> return state

switchTurn :: Player -> Player
switchTurn White = Black
switchTurn Black = White

