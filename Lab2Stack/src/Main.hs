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

main = do
    port:_ <- getArgs
    s <- createConnection (read port)
    mvar <- newMVar 0
    connLoop s mvar

createConnection:: Int -> IO Socket
createConnection p = do 
    s <- socket AF_INET Stream 0
    setSocketOption s ReuseAddr 1
    bind s (SockAddrInet (read $ show p) iNADDR_ANY)
    listen s 10
    return s

connLoop :: Socket -> MVar Int -> IO ()
connLoop s mvar = do
    (soc, socA) <- accept s
    id <- myThreadId
    mval <- takeMVar mvar
    if (mval <=8)
        then putMVar mvar (mval+1)
        else putMVar mvar mval
    if (mval <=8)
        then forkIO $ handleConn mvar (soc, socA) id
        else forkIO $ close soc
    connLoop s mvar

handleConn :: MVar Int -> (Socket, SockAddr) -> ThreadId -> IO ()
handleConn mvar (s, sa) id = do 
    input <- recv s 4096
    parseMessage s id (B8.unpack input)
    mval <- takeMVar mvar
    tid <- myThreadId
    if (length (B8.unpack input) == 0)
        then putMVar mvar (mval-1)
        else putMVar mvar mval
    if (length (B8.unpack input) == 0)
        then (close s) >> (killThread tid)
        else handleConn mvar (s, sa) id


parseMessage :: Socket -> ThreadId -> String -> IO ()
parseMessage s id [] = do
    threadDelay 10000
parseMessage s id ('K':'I':'L':'L':'_':'S':'E':'R':'V':'I':'C':'E':_) = do
    putStrLn "Shutting Down"
    throwTo id ThreadKilled
parseMessage s _  ('H':'E':'L':'O':m) = do
    name <- getSocketName s
    send s  (B8.pack $ ("HELO" ++ m ++ "IP:" ++ ((splitOn ":" (show name))!!0) ++ "\nPort:"++ ((splitOn ":" (show name))!!1) ++ "\nStudentID:13327420\n"))
    putStrLn "Helo text sent"
parseMessage s _ m = do
    putStrLn ("some other message recieved(" ++ m ++ ")")
