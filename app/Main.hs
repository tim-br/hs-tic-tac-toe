module Main (main) where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as B
import Control.Concurrent
import qualified Data.ByteString.Char8 as C
import Control.Monad (unless, when, forever)
import Data.ByteString (null)
import Data.Char (isSpace)
import GameState
import Text.Read (readMaybe)

data ConnectionState = ConnectionState {
    username :: String,
    user_socket :: Socket
}

main :: IO ()
main = withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol
    userQueue <- newChan  -- Create a new channel for queuing users
    _ <- forkIO $ pairUsers userQueue
    addrInfos <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }))
                             (Just "127.0.0.1") (Just "2222")
    let addr = head addrInfos
    case addrAddress addr of
        SockAddrInet _ hostAddr -> do
            -- Here, hostAddr is a HostAddress, and we bind using the correct PortNumber and HostAddress
            bind sock (SockAddrInet 2222 hostAddr)
            listen sock 2
            putStrLn "Listening on port 2222"
            acceptConnections sock userQueue
        _ -> putStrLn "Error: Non-IPv4 address received"

pairUsers :: Chan ConnectionState -> IO ()
pairUsers queue = forever $ do
    user1 <- readChan queue  -- Wait for the first user
    sendAll (user_socket user1) (C.pack "Waiting for another user to join...\n")
    user2 <- readChan queue  -- Wait for the second user
    putStrLn $ "Pairing " ++ username user1 ++ " with " ++ username user2
    forkIO $ startInteraction user1 user2

acceptConnections :: Socket -> Chan ConnectionState -> IO ()
acceptConnections sock queue = do
    (conn, peer) <- accept sock
    putStrLn $ "Connection from " ++ show peer
    -- Handle each client connection in a separate thread
    _ <- forkIO $ handleConnection conn queue
    acceptConnections sock queue  -- Loop to accept more connections

handleConnection :: Socket -> Chan ConnectionState ->  IO ()
handleConnection conn queue = do
    sendAll conn (C.pack "Enter username:")
    msg <- recv conn 1024
    unless (B.null msg) $ do
      let username_ = C.unpack $ C.dropWhileEnd isSpace msg
      putStrLn $ "Username received: " ++ username_
      let state = ConnectionState username_ conn
      writeChan queue state

startInteraction :: ConnectionState -> ConnectionState -> IO ()
startInteraction user1 user2 = do
    sendAll (user_socket user1) (C.pack $ "You have been paired with " ++ username user2 ++ ".\n")
    sendAll (user_socket user2) (C.pack $ "You have been paired with " ++ username user1 ++ ".\n")
    let game = initGame
    putStrLn $ "Game state initialized: " ++ show game
    informPlayers user1 user2 game

playerTurn :: Socket -> Game -> String -> IO (Int, Int)
playerTurn sock game msg = do
    sendAll sock (C.pack msg)
    moveStr <- recv sock 1024
    case readMove (C.unpack moveStr) of
      Just (row, col) -> do
        let occupied = isPositionOccupied game (row, col)
        case occupied of
          True -> playerTurn sock game "An X or O is already there.\n Enter your move (row col): "
          False -> return (row, col)
      Nothing -> playerTurn sock game "Incorrect Format.\n Enter your move (row col): "

readMove :: String -> Maybe (Int, Int)
readMove str = case words str of
    [rStr, cStr] -> do
        r <- readMaybe rStr
        c <- readMaybe cStr
        return (r, c)
    _ -> Nothing
-- Inform player one to make a move and player two to wait
informPlayers :: ConnectionState -> ConnectionState -> Game -> IO ()
informPlayers userA userB game = do
    sendAll (user_socket userA) (C.pack $ "Current state\n" ++ show game ++ ".\n")
    sendAll (user_socket userB) (C.pack $ "Current state\n" ++ show game ++ ".\n")
    sendAll (user_socket userA) (C.pack (show (username userA) ++  " please enter a coordinate for X (format 'row col')\n"))
    sendAll (user_socket userB) (C.pack ("Please wait for" ++  show (username userA) ++  "to make their move."))
    move <- playerTurn (user_socket userA) game "Enter your move (row col): "  -- Get move from Player 1
    let nextGame = makeMove game move
    putStrLn $ "Player 1 moved at " ++ show move
    sendAll (user_socket userB) (C.pack $ "Player 1 has moved at " ++ show move ++ ".\nYour turn to move.\n")
    case (gameOver nextGame) of
      Ongoing -> informPlayers userB userA nextGame
      Draw -> do 
        sendAll (user_socket userA) (C.pack $ "Draw!\n" ++ show game ++ ".\n")
        sendAll (user_socket userB) (C.pack $ "Draw!\n" ++ show game ++ ".\n")
      Win player -> do 
        sendAll (user_socket userA) (C.pack $ "Player " ++ show player ++ " has won.\n")
        sendAll (user_socket userA) (C.pack $ "\n" ++ show nextGame ++ ".\n")
        sendAll (user_socket userB) (C.pack $ "Player " ++ show player ++ " has won.\n")
        sendAll (user_socket userB) (C.pack $ "\n" ++ show nextGame ++ ".\n")
        close (user_socket userA)
        close (user_socket userB)