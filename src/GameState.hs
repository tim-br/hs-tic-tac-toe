module GameState (
    -- * Export list
    Player(..),
    Cell,
    Board,
    Game(..),
    initGame,
    isBoardFull,
    switchPlayer,
    renderCell,
    makeMove,
    isPositionOccupied,
    gameOver,
    GameResult(..),
) where

data Player = X | O deriving (Eq, Show)
type Cell = Maybe Player
type Board = [[Cell]]

data Game = Game {
    board :: Board,
    currentPlayer :: Player
}

data GameResult = Win Player | Draw | Ongoing
    deriving (Eq, Show)

instance Show Game where
    show game = renderGame game

renderGame :: Game -> String
renderGame game = unlines [
        renderRow 0 game,
        "---------",
        renderRow 1 game,
        "---------",
        renderRow 2 game
        ]

renderRow :: Int -> Game -> String
renderRow row game = unwords [
    renderCell (board game !! row !! 0),
    "|",
    renderCell (board game !! row !! 1),
    "|",
    renderCell (board game !! row !! 2)
    ]

renderCell :: Cell -> String
renderCell Nothing  = " "
renderCell (Just p) = show p

-- Initialize a new game with an empty board and Player X starting
initGame :: Game
initGame = Game {
    board = replicate 3 (replicate 3 Nothing),
    currentPlayer = X
}

-- Function to check if the board is full
isBoardFull :: Board -> Bool
isBoardFull = all (all (/= Nothing))

-- Function to switch the current player
switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

makeMove :: Game -> (Int, Int) -> Game
makeMove game (row, col) =
    let colIndex = col - 1  -- Convert 1-based index to 0-based
        rowIndex = length (board game) - row  -- Flip row for Cartesian coordinates (1-based)
        updatedRow = take colIndex (board game !! rowIndex) ++ 
                    [Just (currentPlayer game)] ++ 
                    drop (colIndex + 1) (board game !! rowIndex)
        updatedBoard = take rowIndex (board game) ++ 
                    [updatedRow] ++ 
                    drop (rowIndex + 1) (board game)
    in game { board = updatedBoard, currentPlayer = (switchPlayer (currentPlayer game)) }

isPositionOccupied :: Game -> (Int, Int) -> Bool
isPositionOccupied game (row, col) =
    let rowIndex = length (board game) - row  -- Adjust row for 1-based Cartesian coordinates
        colIndex = col - 1                      -- Adjust column for 1-based indexing
    in case board game !! rowIndex !! colIndex of
         Nothing -> False  -- Position is not occupied
         Just _  -> True   -- Position is occupied

gameOver :: Game -> GameResult
gameOver game
    | winner (board game) X = Win X
    | winner (board game) O = Win O
    | isBoardFull (board game) = Draw
    | otherwise = Ongoing
    where

      winner :: Board -> Player -> Bool
      winner b player = any (all (== Just player)) (rows b ++ cols b ++ diags b)

      rows :: Board -> Board
      rows b = b

      cols :: Board -> Board
      cols b = [ [b !! r !! c | r <- [0..2]] | c <- [0..2]]

      diags :: Board -> Board
      diags b = [[b !! i !! i | i <- [0..2]], [b !! i !! (2 - i) | i <- [0..2]]]