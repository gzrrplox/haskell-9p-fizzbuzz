-- Details.hs 
{-# LANGUAGE GADTs, RecordWildCards #-}
module Details (simpleServer, addFile, addDir, FileTree, RW, read', write') where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as L
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Data.Binary.Get
import Data.Binary.Put
import Data.NineP
import Data.Bits
import Data.Word
import Data.Maybe
import Data.List (find)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Arrow ((&&&))

-- Simple read/write interface for files
class RW a where
    read' :: a -> Int -> Int -> IO String
    write' :: a -> String -> IO (Maybe Int)

-- Internal representation of file hierarchy
data FileTree where 
    Dir :: { name :: String, qid :: Qid, children :: [FileTree] } -> FileTree
    Leaf :: RW a => { name :: String, qid :: Qid, file :: a } -> FileTree


    -- Utilities to create a filetree
addFile :: RW a => String -> a -> FileTree
addFile name = Leaf name (Qid 0 0 0)

addDir :: String -> [FileTree] -> FileTree
addDir name = Dir name (Qid 128 0 0)

createTree :: [FileTree] -> FileTree
createTree = Dir "root" (Qid 128 0 0)


-- Generate unique server side id's for files
generateQids :: FileTree -> IO FileTree
generateQids tree = do
    counter <- newMVar 0
    let incr (Qid a b _) = Qid a b <$> modifyMVar counter (return . ((+1) &&& id))
        gen (Dir a b c) = do
            b' <- incr b
            Dir a b' <$> mapM gen c
        gen (Leaf a b c) = do
            b' <- incr b
            return $ Leaf a b' c
    gen tree


-- Maintains pool of file handles and utilities to modify them
data Env = Env { tree :: FileTree, 
                 env :: MVar (Map Fid FileTree),
                 fileFromFid :: Fid -> IO FileTree,
                 insertFid :: Fid -> FileTree -> IO (),
                 deleteFid :: Fid -> IO () }

buildEnv tree env = Env tree env a b c
    where a fid = withMVar env $ return . (! fid)
          b fid file = modifyMVar_ env $ return . Map.insert fid file
          c fid = modifyMVar_ env $ return . Map.delete fid

type Fid = Word32


-- Generate access modes, access times etc.
genStat Dir{..} = dummyStat name qid $ shiftL 128 24 .|. 0o775
genStat Leaf{..} = dummyStat name qid 0o666
dummyStat n q m = Stat 77 31 q m 0 0 0 n "" "" ""


---------------------------------------------------------
-- Incomplete handling of 9P protocol, use at own risk --
---------------------------------------------------------
respond :: Env -> Msg -> IO Msg

-- Shake hands with everyone
respond _ (Msg TTversion tag (Tversion size body)) = return $ Msg TRversion tag $ Rversion size body

-- Free passage
respond _ (Msg TTauth tag (Tauth afid _ _)) = return $ Msg TRerror tag $ Rerror "No authentication required"

-- Show the way to the root of the synthetic filesystem
respond Env{..} (Msg TTattach tag (Tattach fid _ _ _)) = do 
    insertFid fid tree
    return . Msg TRattach tag . Rattach $ qid tree

-- Access basic information about files
respond Env{..} (Msg TTstat tag (Tstat fid)) = Msg TRstat tag . stat <$> fileFromFid fid
    where stat f = Rstat [genStat f]

-- Forget about files no longer accesed
respond Env{..} (Msg TTclunk tag (Tclunk fid)) = do
    deleteFid fid
    return $ Msg TRclunk tag Rclunk

-- Acquire another handle (fid) of current file
respond Env{..} (Msg TTwalk tag (Twalk fid newfid [])) = do
    insertFid newfid =<< fileFromFid fid
    return $ Msg TRwalk tag (Rwalk [])

-- Move in hierarchy, again new fid. Does not handle subdirectories too well
respond Env{..} (Msg TTwalk tag (Twalk fid newfid [path])) = fileFromFid fid >>= reply . findFile path
    where findFile path (Dir _ _ files) = find ((== path) . name) files
          reply (Just file@(Leaf _ qid _)) = do
                insertFid newfid file
                return $ Msg TRwalk tag (Rwalk [qid])
          reply Nothing = return $ Msg TRwalk tag $ Rerror "File not found!"

-- Open a file for access
respond Env{..} (Msg TTopen tag (Topen fid mode)) = do 
    f <- fileFromFid fid
    return $ Msg TRopen tag $ Ropen (qid f) 0

-- Read a file using methods of RW instance
respond Env{..} (Msg TTread tag (Tread fid offset count)) = Msg TRread tag . Rread <$> (readFile =<< fileFromFid fid)
    where readFile (Dir _ _ children) = return . section offset count . runPut . putListAll . map genStat $ children
          readFile (Leaf _ _ file) = C.pack <$> read' file (fromIntegral offset) (fromIntegral count)
          section off n = C.take (fromIntegral n) . C.drop (fromIntegral off)

-- Write to a file using methods of RW instance, offsets ignored
respond Env{..} (Msg TTwrite tag (Twrite fid offset msg)) = reply <$> (writeFile =<< fileFromFid fid)
    where writeFile (Leaf _ _ file) = write' file $ C.unpack msg
          reply (Just n) = Msg TRwrite tag . Rwrite . fromIntegral $ n
          reply Nothing = Msg TRerror tag $ Rerror "Write failed!"

-- Abort request (blocking read for example), unhandled 
respond Env{..} (Msg TTflush tag (Tflush oldtag)) = return $ Msg TRflush tag Rflush

-- In NineP.hs but unexported, used here for serializing directory reads
putListAll :: (Bin a) => [a] -> Put
putListAll = mapM_ put



-- Simple handler, forks a response thread for every request
dummyHandler sock env = forever $ do 
    req <- runGet get <$> recv sock 8096
    forkIO $ respond env req >>= send sock . runPut . put >> return ()

-- Prepare environment for connection and start communicating
runConn tree (sock, _) = do
    env <- newMVar $ Map.fromList []
    tree' <- generateQids . createTree $ tree
    dummyHandler sock (buildEnv tree' env)

simpleServer port tree = withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    listen sock 2

    forever $ accept sock >>= forkIO . runConn tree
