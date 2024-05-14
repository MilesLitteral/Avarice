
module GPUSemaphore where 
        
    newtype GPUState s a = GPUState { runGPUState :: (s -> (a,s)) }

    instance Monad (GPUState s) where
        return a           = GPUState $ \s -> (a,s)
        (GPUState x) >>= f = GPUState $ \s ->
            let (v,s') = x s
            in runGPUState (f v) s'

    newtype FutT s m a = FutT { runFutT :: (s -> m (a,s)) }

    instance (Monad m) => Monad (FutT s m) where
        return a         = StateT $ \s -> return (a,s)
        (StateT x) >>= f = StateT $ \s -> do
        
        (v,s') <- x s          -- get new value and state
        runFutT (f v) s'       -- pass them to f

    -- | Manifest's Interface for speaking to Futhark
    -- utilizes Manifut to accomplish this
    type MFutT m a = FutT m a
    type MFut      = FutT Identity
    type MFutIO    = FutT IO

    -- | Conversion Functions:
    -- (Word8 <-> Int16)
    -- (A.Array A.S A.Ix1 Word8 <-> A.Array A.S A.Ix1 Int16)
    toInt16 :: Float -> Int16
    toInt16 = fromIntegral . float2Int

    toFloat :: Int16 -> Float
    toFloat = int2Float    . fromIntegral

    chunkToInt16 ::  A.Array A.S A.Ix1 Float  -> A.Array A.D A.Ix1 Int16
    chunkToInt16 a = A.map toInt16 a

    chunkToWord8 ::  A.Array A.S A.Ix1 Int16  -> A.Array A.D A.Ix1 Float
    chunkToWord8 a = A.map toFloat a 

    futharkProcess :: Chan GPUCommand -> IO ()
    futharkProcess chan = do 
        context <- getContext []
        runFutTIn context $ do 
            let {doFutharkProcess = do
                    command <- liftIO $ readChan chan
                    case command of
                        GPUExit -> return () 
                        _ -> do 
                            result <- handleGPUCommand command
                            liftIO $ writeChan chan result --writeChan
                            doFutharkProcess
                }
            doFutharkProcess

    handleGPUCommand :: GPUCommand -> FutIO GPUCommand
    handleGPUCommand command = do 
        case command of
            DiffImages a b -> do
                futA <- toFuthark $ unImage a
                futB <- toFuthark $ unImage b
                
                !futOutput <- E.diffNoAlphaImages futA futB --E.diffImages futA futB
                --run actual futhark diff operation
                -- return result in ImageResp
                result <- fromFuthark futOutput
                return $ ImageResp $ Image result

    createFutharkHandle :: IO (FutHandle)
    createFutharkHandle = do
        chan <- newChan
        mvar <- newMVar chan
        tid  <- forkOS $ futharkProcess chan
        return $ FutHandle tid mvar