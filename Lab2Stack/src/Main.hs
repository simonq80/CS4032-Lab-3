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
import Text.Regex
import Data.Maybe

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
    putStrLn $ show (B8.unpack input)
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
parseMessage s _ ('J':'O':'I':'N':'_':m) chatList = do
    addClient s (matchRegex (mkRegex "CHATROOM:(.*)\nCLIENT_IP:0\nPORT:0\nCLIENT_NAME:(.*)\n") m) chatList
parseMessage s _ ('L':'E':'A':'V':'E':'_':m) chatList = do
    removeClient s (matchRegex (mkRegex "CHATROOM:(.*)\nJOIN_ID:(.*)\nCLIENT_NAME:(.*)\n") m) chatList
parseMessage s _ ('C':'H':'A':'T':':':m) chatList = do
    sendMessage s (matchRegex (mkRegex "(.*)\nJOINID:(.*)\nCLIENT_NAME:(.*)\nMESSAGE:(.*)\n\n") m) chatList
parseMessage s _ m _ = do
    putStrLn ("some other message recieved(" ++ m ++ ")")

addClient :: Socket -> Maybe [String] -> MVar [(Socket, String, String, Int, Int)] -> IO ()
addClient s Nothing chatList  = do
    send s (B8.pack $ "ERROR_CODE:0\nERROR_DESCRIPTION:Invalid message\n\n")
    putStrLn "Invalid message sent"
addClient s (Just (a:b:_)) chatList  = do
    list <- takeMVar chatList
    putMVar chatList ((s, b, a, (getJoinId s list), (getRoomRef a list)):list)
    name <- getSocketName s
    send s (B8.pack $ "JOINED_CHATROOM:"++a++"\nSERVER_IP:"++((splitOn ":" (show name))!!0)++"\nPORT:"++((splitOn ":" (show name))!!1)++"\nROOM_REF:"++(show (getRoomRef a list))++"\nJOIN_ID:"++(show (getJoinId s list))++"\n\n")
    sendMessage s (Just [(show (getRoomRef a list)), (show (getJoinId s list)), b, "USER HAS JOINED"]) chatList
    cl <- readMVar chatList
    putStrLn $ show $ cl

removeClient :: Socket -> Maybe [String] -> MVar [(Socket, String, String, Int, Int)] -> IO ()
removeClient s Nothing _ = do
    send s (B8.pack $ "ERROR_CODE:0\nERROR_DESCRIPTION:Invalid message\n\n")
    putStrLn "Invalid message sent"
removeClient s (Just (a:b:c:_)) chatList = do
    list <- takeMVar chatList
    putMVar chatList (filter (filterClient s (read a) (read b)) list)
    send s (B8.pack $ "LEFT_CHATROOM:"++(show a)++"\nJOIN_ID:"++(show b)++"\n\n")
    cl <- readMVar chatList
    putStrLn $ show $ cl 

sendMessage :: Socket -> Maybe [String] -> MVar [(Socket, String, String, Int, Int)] -> IO ()
sendMessage s Nothing _ = do
    send s (B8.pack $ "ERROR_CODE:0\nERROR_DESCRIPTION:Invalid message\n\n")
    putStrLn "Invalid message sent"
sendMessage s (Just (ref:jid:name:mess:_)) chatList = do
    list <- readMVar chatList
    sendToRoom (read ref) name mess list

sendToRoom :: Int -> String -> String -> [(Socket, String, String, Int, Int)] -> IO ()
sendToRoom _ _ _ [] = do
    putStrLn "Message sent"
sendToRoom ref name mess ((s, _, _, _, cid):xs) = do
    if(ref == cid)
        then do 
            send s (B8.pack $ "CHAT:"++(show ref)++"\nCLIENT_NAME:"++name++"\nMESSAGE:"++mess++"\n\n")
            name <- getSocketName s
            putStrLn ("message sent to " ++ (show name))
        else return ()
    sendToRoom ref name mess xs
    

filterClient :: Socket -> Int -> Int  -> (Socket, String, String, Int, Int) -> Bool
filterClient s a b (s1, _, _, b1,a1) = (s /= s1) && (a /= a1) && (b /= b1)
filterClient  _ _ _ _ = False

getJoinId :: Socket -> [(Socket, String, String, Int, Int)] -> Int
getJoinId s chatList = let x = findJoinId s chatList in
    fromMaybe (getNewJoinId chatList) x

findJoinId :: Socket -> [(Socket, String, String, Int, Int)] -> Maybe Int
findJoinId _ [] = Nothing
findJoinId s ((s1, _, _, a, _):xs) = if(s == s1)
    then Just a
    else findJoinId s xs  

getNewJoinId :: [(Socket, String, String, Int, Int)] -> Int
getNewJoinId [] = 0
getNewJoinId x = (maximum (map (\(a,b,c,d,e) -> d) x))+1

getRoomRef :: String -> [(Socket, String, String, Int, Int)] -> Int
getRoomRef s chatList = let x = findRoomRef s chatList in
    fromMaybe (getNewRoomRef chatList) x

findRoomRef :: String -> [(Socket, String, String, Int, Int)] -> Maybe Int
findRoomRef _ [] = Nothing
findRoomRef s ((_, _, s1, _, a):xs) = if(s == s1)
    then Just a
    else findRoomRef s xs

getNewRoomRef :: [(Socket, String, String, Int, Int)] -> Int
getNewRoomRef [] = 0
getNewRoomRef x = (maximum (map (\(a,b,c,d,e) -> e) x))+1


