{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

--import qualified Data.ByteString as B
import System.IO
import Control.Concurrent
    ( newChan, readChan, writeChan, forkIO, Chan )
import Control.Monad ( forever)
import Data.Maybe (fromJust)
import GameState
    ( gameOver,
      initGameIO,
      isCurrentPlayer,
      isPositionOccupied,
      makeMove,
      Game,
      GameResult(Win, Ongoing, Draw),
      Player )
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as B
import           Data.X509 (SignedCertificate)
import           Data.X509.CertificateStore (makeCertificateStore, CertificateStore)
import           Data.X509.File (readSignedObject)
import qualified Network.Simple.TCP.TLS as Z
import qualified Network.Socket as NS
import qualified Network.TLS as T
import System.Console.GetOpt
    ( getOpt,
      usageInfo,
      ArgDescr(OptArg, ReqArg),
      ArgOrder(RequireOrder),
      OptDescr(..) )
import           System.Environment (getProgName, getArgs)
import Data.IORef ( IORef, readIORef, writeIORef )

data ConnectionStateTLS = ConnectionStateTLS {
    username_tls :: String,
    user_context :: T.Context,
    closeChan :: Chan String
}

data UserGame = UserGame {
    user :: ConnectionStateTLS,
    otherUser :: Maybe ConnectionStateTLS,
    gameRef :: Maybe (IORef Game)
}

readMove :: String -> Maybe (Int, Int)
readMove str = case words str of
    [rStr, cStr] -> do
        r <- readMaybe rStr
        c <- readMaybe cStr
        return (r, c)
    _ -> Nothing

sendGameState :: T.Context -> Game -> IO ()
sendGameState ctx game = do
    let message = "\n" ++ show game ++ ".\n"
    Z.send ctx (B.pack message)

handleWin :: ConnectionStateTLS -> ConnectionStateTLS -> Player -> Game -> IO ()
handleWin userA userB player nextGame = do
    -- Notify user A of the win
    let winMessageA = "Player " ++ show player ++ " has won.\n"
    Z.send (user_context userA) (B.pack winMessageA)
    sendGameState (user_context userA) nextGame  -- Send updated game state to user A

    -- Notify user B of the win
    let winMessageB = "Player " ++ show player ++ " has won.\n"
    Z.send (user_context userB) (B.pack winMessageB)
    sendGameState (user_context userB) nextGame  -- Send updated game state to user B

    -- Close the TLS connections
    writeChan (closeChan userA) "close"
    writeChan (closeChan userB) "close"
    T.bye (user_context userA)
    T.bye (user_context userB)

notifyPaired :: ConnectionStateTLS -> ConnectionStateTLS -> IO ()
notifyPaired user1 user2 = do
    let message = "You have been paired with " ++ username_tls user1 ++ ".\n"
    Z.send (user_context user2) (B.pack message)

updateGameRef :: IORef Game -> Game -> IO ()
updateGameRef = writeIORef

pairUsersTLS :: Chan UserGame -> IO ()
pairUsersTLS queue = forever $ do
    user1 <- readChan queue  -- Wait for the first user
    Z.send (user_context (user user1)) (B.pack "Waiting for another user to join...\n")
    user2 <- readChan queue  -- Wait for the second user
    putStrLn $ "Pairing " ++ username_tls (user user1) ++ " with " ++ username_tls (user user2)

    notifyPaired (user user1) (user user2)
    notifyPaired (user user2) (user user1)
    -- Initialize game state
    gameRef <- initGameIO (username_tls (user user1)) (username_tls (user user2))
    game <- readIORef gameRef  -- Read the actual game state from the IORef
    putStrLn $ "Game state initialized: " ++ show game
    let user1' = user1 { gameRef = Just gameRef, otherUser = Just (user user2) }
    let user2' = user2 { gameRef = Just gameRef, otherUser = Just (user user1) }

    -- Optionally, send a message to both users that the game is ready
    _ <- forkIO $ consume (user1', user2') processInput
    _ <- forkIO $ consume (user2', user1') processInput
    Z.send (user_context (user user1)) (B.pack "Your game is ready!\n")
    Z.send (user_context (user user2)) (B.pack "Your game is ready!\n")
    sendGameState (user_context (user user1)) game
    sendGameState (user_context (user user2)) game
    Z.send (user_context (user user1)) (B.pack "Enter your move (row col): ")
    Z.send (user_context (user user2)) (B.pack "Waiting for player 1 to move...\n")

processInput :: T.Context -> B.ByteString -> (UserGame, UserGame) -> IO ()
processInput ctx moveStr userGameTuple =  do
  let userGame = fst userGameTuple
  let ctx = user_context (user userGame)
  case gameRef userGame of
    Just ref -> do
      game <- readIORef ref
      let otherUserI = fromJust (otherUser userGame)
      let otherUserCtx = user_context otherUserI
      if isCurrentPlayer game (username_tls (user userGame))
        then
          if B.null moveStr then do
            Z.send ctx (B.pack "Empty input.\nEnter your move (row col): ")
            consume userGameTuple processInput
          else
          case readMove (B.unpack moveStr) of
            Just (row, col) -> do
              let occupied = isPositionOccupied game (row, col)
              if occupied then do
                Z.send ctx (B.pack "An X or O is already there.\n Enter your move (row col): ")
                consume userGameTuple processInput

              else do
                  let nextGame = makeMove game (row, col)
                  updateGameRef (fromJust $ gameRef userGame) nextGame
                  updateGameRef (fromJust $ gameRef (snd userGameTuple)) nextGame
                  putStrLn $ "other player moved at " ++ show (row, col)
                  new_game <- readIORef (fromJust $ gameRef userGame)
                  putStrLn $ "Current game state: \n" ++ show new_game
                  -- Update Player B with Player A's move
                  sendGameState ctx nextGame
                  sendGameState otherUserCtx nextGame
                  --
                  -- Determine if the game is over
                  case gameOver nextGame of
                    Ongoing -> do
                      --informPlayersTLS otherUserI (user userGame) nextGame
                      Z.send otherUserCtx (B.pack ("Other player has moved at " ++ show (row, col) ++ ".\nYour turn to move. Enter your move (row col): "))
                      consume userGameTuple processInput
                    Draw -> do
                      Z.send (user_context (user userGame)) (B.pack "Draw!\n")
                      sendGameState (user_context (user userGame)) game
                      Z.send (user_context (user userGame)) (B.pack "Goodbye.!\n")
                      Z.send otherUserCtx (B.pack "Draw!\n")
                      sendGameState otherUserCtx game
                      Z.send otherUserCtx (B.pack "Goodbye!\n")
                      writeChan (closeChan (user userGame)) "close"
                      writeChan (closeChan (fromJust $ otherUser userGame)) "close"
                      T.bye (user_context (user userGame))
                      T.bye (user_context (fromJust $ otherUser userGame))

                    Win player -> handleWin (user userGame) otherUserI player nextGame

            Nothing -> do
              Z.send (user_context (user userGame)) (B.pack "Incorrect format!\nEnter your move (row col): ")
              consume userGameTuple processInput
        else do
          Z.send (user_context (user userGame)) "Not your turn, please wait.\n"
          sendGameState (user_context (user userGame)) game

    Nothing ->
      putStrLn "No game reference available."

consume :: (UserGame, UserGame) -> (T.Context -> B.ByteString -> (UserGame, UserGame) -> IO ()) -> IO ()
consume userGameTuple k = do
    let userGame = fst userGameTuple
    mbs <- Z.recv (user_context (user userGame))
    case mbs of
        Nothing -> do
          writeChan (closeChan (user userGame)) "over"
          return ()
        Just bs -> k (user_context (user userGame)) bs userGameTuple -- >> consume userGame k

server :: T.Credential -> Z.HostPreference -> NS.ServiceName
       -> Maybe CertificateStore -> IO ()
server cred hp port ycs = do
    putStrLn $ "Listening on port:: " <> port <> " ..."
    userQueue <- newChan
    _ <- forkIO $ pairUsersTLS userQueue
    let ss = Z.makeServerParams cred ycs
    Z.serve ss hp port $ \(ctx, caddr) -> do
       putStrLn $ show caddr <> " joined."
       username_ <- askForUsername ctx
       putStrLn $ "Username: " <> B.unpack username_
       closingChan <- newChan
       let state = ConnectionStateTLS (B.unpack username_) ctx closingChan
       let userGame = UserGame { user = state, gameRef = Nothing, otherUser = Nothing }
       writeChan userQueue userGame
       _ <- readChan closingChan
       putStrLn $ show caddr <> " quit."
  where
    askForUsername :: T.Context -> IO B.ByteString
    askForUsername ctx = do
        Z.send ctx $ B.pack "Welcome, please enter your username:"
        mbs <- Z.recv ctx
        case mbs of
          Nothing -> error "Connection lost while receiving username."
          Just bs -> return bs

main :: IO ()
main = Z.withSocketsDo $ do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case getOpt RequireOrder options args of
      (actions, [hostname,port], _) -> do
        opts <- foldl (>>=) (return defaultOptions) actions
        server (optServerCredentials opts) (Z.Host hostname) port
               (makeCertificateStore <$> optCACert opts)
      (_,_,msgs) -> do
        pn <- getProgName
        let header = "Usage: " <> pn <> " [OPTIONS] HOSTNAME PORT"
        error $ concat msgs ++ usageInfo header options

--------------------------------------------------------------------------------
-- The boring stuff below is related to command line parsing

data Options = Options
  { optServerCertFile     :: FilePath
  , optServerKeyFile      :: FilePath
  , optServerCredentials  :: T.Credential
  , optCACert             :: Maybe [SignedCertificate]
  }

defaultOptions :: Options
defaultOptions = Options
  { optServerCertFile = error "Missing optServerCertFile"
  , optServerKeyFile = error "Missing optServerKeyFile"
  , optServerCredentials = undefined
  , optCACert = Nothing
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option [] ["cert"]   (ReqArg readServerCert "FILE") "Server certificate"
  , Option [] ["key"]    (ReqArg readServerCredentials "FILE") "Server private key"
  , Option [] ["cacert"] (OptArg readCACert     "FILE")
    "CA certificate to verify a client certificate, if given"
  ]

readServerCert :: FilePath -> Options -> IO Options
readServerCert fp opt =
  return opt {optServerCertFile = fp}

readServerCredentials :: FilePath -> Options -> IO Options
readServerCredentials arg opt = do
  ec <- T.credentialLoadX509 (optServerCertFile opt) arg
  case ec of
    Left err -> error err
    Right c ->
      return $ opt { optServerCredentials = c
                   , optServerKeyFile = arg }

readCACert :: Maybe FilePath -> Options -> IO Options
readCACert Nothing    opt = return opt
readCACert (Just arg) opt = do
    certs <- readSignedObject arg
    return $ opt { optCACert = Just certs }