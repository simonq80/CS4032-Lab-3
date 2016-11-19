module Main where
import System.Exit
import System.IO
import System.Environment
import qualified Data.ByteString.Char8 as B8
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString
import Options.Applicative
import Control.Concurrent
import Control.Exception
import Data.List.Split
import Control.Concurrent.MVar
import System.Random

type ChatList = [(Socket, String, String, Int, Int)] -- socket name chat joinid chatid

main = do
    port:_ <- getArgs
    s <- createConnection (read port)
    mvar <- newMVar 0
    chatList <- newMVar []
    connLoop s mvar chatList

createConnection:: Int -> IO Socket
createConnection p = do 
    s <- socket AF_INET Stream 0
    setSocketOption s ReuseAddr 1
    bind s (SockAddrInet (read $ show p) iNADDR_ANY)
    listen s 10
    return s

connLoop :: Socket -> MVar Int -> MVar [(Socket, String, String, Int, Int)] -> IO ()
connLoop s mvar chatList = do
    (soc, socA) <- accept s
    id <- myThreadId
    mval <- takeMVar mvar
    if (mval <=100)
        then putMVar mvar (mval+1)
        else putMVar mvar mval
    if (mval <=100)
        then forkIO $ handleConn mvar (soc, socA) id chatList
        else forkIO $ close soc
    connLoop s mvar chatList

handleConn :: MVar Int -> (Socket, SockAddr) -> ThreadId -> MVar [(Socket, String, String, Int, Int)] -> IO ()
handleConn mvar (s, sa) id chatList = do 
    input <- recv s 4096
    parseMessage s id (B8.unpack input) chatList
    mval <- takeMVar mvar
    tid <- myThreadId
    if (length (B8.unpack input) == 0)
        then putMVar mvar (mval-1)
        else putMVar mvar mval
    if (length (B8.unpack input) == 0)
        then (close s) >> (killThread tid)
        else handleConn mvar (s, sa) id chatList


parseMessage :: Socket -> ThreadId -> String -> MVar [(Socket, String, String, Int, Int)] ->  IO ()
parseMessage s id [] _ = do
    threadDelay 10000
parseMessage s id ('K':'I':'L':'L':'_':'S':'E':'R':'V':'I':'C':'E':_) _ = do
    putStrLn "Shutting Down"
    throwTo id ThreadKilled
parseMessage s _  ('H':'E':'L':'O':m) _ = do
    name <- getSocketName s
    send s  (B8.pack $ ("HELO" ++ m ++ "IP:" ++ ((splitOn ":" (show name))!!0) ++ "\nPort:"++ ((splitOn ":" (show name))!!1) ++ "\nStudentID:13327420\n"))
    putStrLn "Helo text sent"
parseMessage s _ ('J':'O':'I':'N':m) chatList = do
    addClient s m chatList "chat1"
parseMessage s _ ('L':'E':'A':'V':'E':m) chatList = do
    removeClient s m chatList
--parseMessage s _ ('S':'E':'N':'D':m) chatList = do
--    sendMessage s "name" m
parseMessage s _ m _ = do
    putStrLn ("some other message recieved(" ++ m ++ ")")

addClient :: Socket -> String -> MVar [(Socket, String, String, Int, Int)] -> String -> IO ()
addClient s name chatList chatName = do
    list <- takeMVar chatList
    g <- getStdGen
    putMVar chatList ((s, name, chatName, (head (take 1 $ randoms g :: [Int])), 10):list)

removeClient :: Socket -> String -> MVar [(Socket, String, String, Int, Int)] -> IO ()
removeClient s chatName chatList = do
    list <- takeMVar chatList
    putMVar chatList (filter (filterClient s chatName) list) 

filterClient :: Socket -> String -> (Socket, String, String, Int, Int) -> Bool
filterClient s chatName (s1, _, chatName1, _, _) = (s == s1) && (chatName == chatName1)
filterClient _ _ _ = False

