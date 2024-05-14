{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Manifest.Utils.Subprocess (
    try',
    initShell,
    Manifest.Utils.Subprocess.startLocalManiapi,
    rebootSystemManiapi,
    Manifest.Utils.Subprocess.startManiex,
    startManiblend,
    startManieye,
    startManigram,
    startTensorBoard,
    startManigramControl,
    startIDE,
    configureManiex
)
where

import Data.Text (Text, unpack, pack)
import Control.Lens
import Control.Exception
import Control.Monad.IO.Class
import GHC.IO.Handle (hGetLine)
import System.Process (StdStream(CreatePipe), CreateProcess, createProcess, proc, cwd, env, std_in, std_out, std_err, shell)
import System.Environment
import System.Directory (getHomeDirectory)

import Manifest.Types
import Manifest.Utils.Log

-- | Test if a Subprocess succeeds or fails on startup
-- This specific version will throw an IOException for 
-- the Subprocess (if it fails) which does not terminate 
-- the main window.
-- Example Use:
-- @
--   orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE  ("Booting Maniex")
--   result <- try' $ (Manifest.Utils.Subprocess.startManiex mdl)
--   case result of
--     Left ex  -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("Maniex: " ++ show ex)
--     Right () -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("Maniex: CHECKLIST OK")
-- @
try' :: IO a ->  IO (Either IOException a)
try' =  try 

initShell :: String -> CreateProcess
initShell str = shell str

-- | Boots the user specified IDE via console command ("atom ./",  "code ./")
-- NOTE: All 'start' functions appends a reference of the process' pipe to the
-- ManifestModel subprocess Lens so they can be closed all at once when the main 
-- window is, or be communicated to later
startIDE :: ManifestModel -> Text -> IO ()
startIDE mdl txt = do
    result <- try' $ createProcess (proc (unpack txt) []){ cwd = Just $ (mdl ^. userHome) ++ "Desktop/manifest", std_out = CreatePipe } --(_, Just hout, _, _) 
    orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE ("Booting IDE")
    case result of
        Left ex         -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("IDE: " ++ show ex)
        Right (_,_,_,p) -> do
            liftIO $ appendLens mdl (append p) (subprocesses)  
            orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("IDE: CHECKLIST OK")

startBinaryInteraction :: ManifestModel -> IO (ManifestEvt)
startBinaryInteraction  mdl = do
    envv   <- getEnvironment
    result <- try' $ createProcess (proc ((mdl ^. userHome) ++ "/venv/blend/bin/python") ["-m", "maniex.run_experiment"]){cwd = Just $ (mdl ^. userHome) ++ "/venv/blend/lib/python3.9/site-packages/maniex/"} { std_out = CreatePipe } --createProcess (proc "this_command_does_not_exist" [])
    orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE   "Booting Maniex"
    case result of
            Left ex         -> do
                orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("Hardware: " ++ show ex)
                return AutoResolve
            Right (_, Just hOut, _, p) -> do
                orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("Hardware: CHECKLIST OK")
                return $ SetBinaryHandles hOut p False ---appendLens mdl (append p) (subprocesses)  
                -- update <- hGetLine hOut hIsEOF
                -- orderedMessage $ ManiLogMessage MANI_LOG_BODY MANI_LOG_WARNING  (show update)
                -- orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("Maniex: CHECKLIST OK")

startTensorBoard :: ManifestModel -> IO ()
startTensorBoard mdl  = do
    result <- try' $ createProcess (proc "tensorboard" ["--logdir", (show $ mdl ^. dataset_dir)]){ cwd = Just $ (mdl ^. userHome) ++ "/Desktop/manifest", std_out = CreatePipe } --(_, Just hout, _, _) 
    orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE ("Booting Tensorboard")
    case result of
        Left ex         -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("Tensorboard: " ++ show ex)
        Right (_,_,_,p) -> do
            liftIO $ appendLens mdl (append p) (subprocesses)  
            orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("Tensorboard: CHECKLIST OK")

-- | Boots an instance of maniapi.server in the "python -m" flavor. 
startLocalManiapi :: ManifestModel -> IO ()
startLocalManiapi  mdl = do
    result <- try' $ createProcess (proc ((mdl ^. userHome) ++ "/venv/blend/bin/python") ["-m", "maniapi.server"]){ cwd = Just $ (mdl ^. userHome) ++ "/Desktop/manifest", std_out = CreatePipe } --(_, Just hout, _, _) 
    orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE   "Communication With ManiAPI Opened"
    case result of
        Left ex         -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("ManiAPI: " ++ show ex)
        Right (_,_,_,p) -> do
            liftIO $ appendLens mdl (append p) (subprocesses)  
            orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("ManiAPI: CHECKLIST OK")

-- | Boots the local instance of maniapi.server in the "systemctl" flavor. 
startSystemManiapi :: ManifestModel -> IO ()
startSystemManiapi  mdl = do
    result <- try' $ createProcess (proc ("sudo") [ "systemctl", "start", "manidef"]){ cwd = Just $ (mdl ^. userHome) ++ "Desktop/manifest"{-, std_out = CreatePipe-} } --(_, Just hout, _, _) 
    orderedMessage $ ManiLogMessage MANI_LOG_BODY MANI_LOG_ZONE   "Communication With ManiAPI Opened"
    case result of
        Left ex         -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("ManiAPI: " ++ show ex)
        Right (_,_,_,p) -> do
            liftIO $ appendLens mdl (append p) (subprocesses)  
            orderedMessage $ ManiLogMessage MANI_LOG_BODY MANI_LOG_ZONE  ("ManiAPI: CHECKLIST OK")

-- | Stops the local instance of maniapi.server in the "systemctl" flavor. 
stopSystemManiapi :: ManifestModel -> IO ()
stopSystemManiapi  mdl = do
    result <- try' $ createProcess (proc ("sudo") ["systemctl", "stop", "manidef"]){ cwd = Just $ (mdl ^. userHome) ++ "Desktop/manifest"{-, std_out = CreatePipe-} } --(_, Just hout, _, _) 
    orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE   "Closing ManiAPI Channel"
    case result of
        Left ex         -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("ManiAPI: " ++ show ex)
        Right (_,_,_,p) -> do
            liftIO $ appendLens mdl (append p) (subprocesses)  
            orderedMessage $ ManiLogMessage MANI_LOG_BODY MANI_LOG_ZONE  ("ManiAPI: SHUTDOWN")

-- |  Monitor the local instance of maniapi.server (only applicable to systemctl setups)
monitorSystemManiapi :: ManifestModel -> IO ()
monitorSystemManiapi  mdl = do
    result <- try' $ createProcess (proc ("journalctl") ["-f", "-u", "manidef"]){ cwd = Just $ (mdl ^. userHome) ++ "Desktop/Manifest", std_out = CreatePipe } --(_, Just hout, _, _) 
    orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE   "Communication With ManiAPI Opened"
    case result of
        Left ex         -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("ManiAPI: " ++ show ex)
        Right (_,_,_,p) -> do
            liftIO $ appendLens mdl (append p) (subprocesses)  
            orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("ManiAPI: CHECKLIST OK")

rebootSystemManiapi :: ManifestModel -> IO ()
rebootSystemManiapi mdl = do
    stopSystemManiapi  mdl
    startSystemManiapi mdl
    orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_WARNING  ("ManiAPI: REBOOTED (SYSTEMCTL)")

-- | Boot IDE and open Maniex/Config.yaml
configureManiex :: ManifestModel -> IO ()
configureManiex  mdl = do
    envv   <- getEnvironment
    result <- try' $ createProcess (proc (unpack $ mdl ^. ide_command) ["config.yaml"]) {cwd = Just $ (mdl ^. userHome) ++ "/venv/blend/lib/python3.9/site-packages/maniex/"} --"code" ["./config.yaml"] --createProcess (proc "this_command_does_not_exist" [])
    orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE   "Booting Maniex-Config"
    case result of
            Left ex         -> do
                orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("Maniex-Config: " ++ show ex)
            Right (_, _, _, p) -> do
                liftIO $ appendLens mdl (append p) (subprocesses)  
                --liftIO $ appendIO mdl (append p) (subprocesses)  
                orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE     ("Maniex-Config: CHECKLIST OK")

                
-- | Boot Maniex 
-- NOTE: Manifest contains a custom Maniex submodule that must be pointed to in manidef,
-- Use install_maniex.sh to help with this 
startManiex :: ManifestModel -> IO ()
startManiex  mdl = do
    envv   <- getEnvironment
    --uncomment the {- (show $ mdl ^. activeManiexScript) ++ ".run_experiment -} below when active model selection is supported
    --by maniex, that line will otherwise cause a trainwreck in the subprocess
    result <- try' $ createProcess (proc ((mdl ^. userHome) ++ "/venv/blend/bin/python") ["-m", {- (show $ mdl ^. activeManiexScript) ++ ".run_experiment -} "maniex.run_experiment", "--datasetName", (unpack $ mdl ^. dataset_name), "--renderFunction", (unpack $ mdl ^. render_function),  "--blendSources", (unpack $ mdl ^. blend_sources), "--datasetDir", (unpack $ mdl ^. dataset_dir), "--outputDir", (unpack $ mdl ^. output_dir),  "--classifierDir", (unpack $ mdl ^. classifier_dir)]){cwd = Just $ (mdl ^. userHome) ++ "/Desktop/manifest"} --createProcess (proc "this_command_does_not_exist" [])
    orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE   "Booting Maniex"
    case result of
            Left ex            -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("Maniex: " ++ show ex)
            Right (_, _, _, p) -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("Maniex: CHECKLIST OK\n")

-- | Boot the Original Manieye
-- CAUTION: You must ln -s your Manieye's location before using this
startManieye :: ManifestModel -> Text -> Text -> IO ()
startManieye  mdl inital predicted = do
    result <- try' $ createProcess (proc "manieye" [unpack inital, unpack predicted]){ cwd = Just $ (mdl ^. userHome) ++ "/Desktop/manifest", std_out = CreatePipe } --(_, Just hout, _, _)
    orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("Manieye(Legacy) Started")
    case result of
        Left ex         -> orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("ManiAPI: " ++ show ex)
        Right (_,_,_,p) -> do
            liftIO $ appendLens mdl (append p) subprocesses  
            orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("Manieye: CHECKLIST OK")

--startManifestMasks
startManigram :: ManifestModel -> IO ()
startManigram mdl = do
    envv   <- getEnvironment
    result <- try' $ createProcess (proc "stack" ["run", "manifest-masks"]){cwd = Just $ (mdl ^. userHome) ++ "/Desktop/manifest"} --createProcess (proc "this_command_does_not_exist" [])
    orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE   "Booting Manigram"
    case result of
            Left ex         -> do
                orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("Manigram: " ++ show ex)
            Right (_,_,_,p) -> do
                liftIO $ appendLens mdl (append p) (subprocesses)  
                orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("Manigram: CHECKLIST OK")
 
startManigramControl :: ManifestModel -> IO ()
startManigramControl mdl = do
    envv   <- getEnvironment
    result <- try' $ createProcess (proc "stack" ["run", "manigram"]){cwd = Just $ (mdl ^. userHome) ++ "/Desktop/manifest"}
    orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE   "Booting Manigram-Control"
    case result of
        Left ex         -> do
            orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("Manigram-Control: " ++ show ex)
        Right (_,_,_,p) -> do
            orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("Manigram-Control: CHECKLIST OK")


-- | Boot ManiBlend (Blender)
startManiblend :: ManifestModel -> IO ()
startManiblend mdl = do
    envv   <- getEnvironment
    result <- try' $ createProcess (proc "blender" []){cwd = Just $ (mdl ^. userHome) ++ "/Desktop/manifest"} --createProcess (proc "this_command_does_not_exist" [])
    orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE   "Booting Maniblend"
    case result of
            Left ex         -> do
                orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("Maniblend: " ++ show ex)
            Right (_,_,_,p) -> do
                orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("Maniblend: CHECKLIST OK")

-- | Spawn PTPd Master Node (will use current machine, which is assumed to be Manifold7/8)
startMasterPTPDaemon :: ManifestModel -> IO ()
startMasterPTPDaemon mdl = do
    envv   <- getEnvironment
    result <- try' $ createProcess (proc "ptpd2" ["-C", "-V", "-E", "-i", "enp70s0", "--masteronly"]){cwd = Just $ (mdl ^. userHome) ++ "/Desktop/manifest"} --createProcess (proc "this_command_does_not_exist" [])
    orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE   "Booting PTP Daemon (master)"
    case result of
            Left ex         -> do
                orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ERROR ("PTPd: " ++ show ex)
            Right (_,_,_,p) -> do
                orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE  ("PTPd: CHECKLIST OK")