{-#OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Int
import Data.Maybe
import Data.Default
import Data.FileEmbed (embedFile)
import Data.Text (Text, pack, unpack)


import Data.List (intersperse, transpose, unfoldr, length, zip)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Internal as Map
import qualified Data.Massiv.Array as A
import qualified Data.Vector.Storable as V

import Control.Lens
import Control.Applicative 
import Control.Concurrent
import Control.Monad.State
import Control.Monad.IO.Class

import System.IO
import System.Random
import System.FilePath
import System.Directory

import Codec.Picture
import Graphics.UI.TinyFileDialogs
import qualified Graphics.UI.TinyFileDialogs as TFD

import Graphics.Rendering.Chart.Easy hiding (spacer, label, textSize)
import Graphics.Rendering.Chart.Backend.Cairo

import Monomer 
import Monomer.Main.Types 
import Monomer.Widgets.Single
import Monomer.Widgets.Singles.Image (ImageCfg)
import Monomer.Main.Types (appFontDefMem)
import Monomer.Core.StyleTypes
import qualified Monomer.Lens as L


--Was Manimer
import Manifest
import Maniga.Type.Record
import Manifest.Types
import Manifest.Utils.Log
import Manifest.Utils.Json
import Manifest.Utils.Chart
import Manifest.Utils.Dialogs
import Manifest.Utils.Subprocess 
import Manifest.Utils.Templating
import Manifest.Utils.Conversion 
import Manifest.Utils.Interfaces.Maniex hiding (Model)
import Manifest.Utils.Interfaces.Maniga
import qualified Manifest.Utils.Chart as CH
import Manifest.Utils.Communication (runFutharkCommand)
import Manifest.Utils.Futhark (createFutharkHandle)

import Manipipe.File.Manifile.IO
import Manipipe.File.Manifile.Type
import qualified Manipipe as MP
import qualified Manipipe.Local.Type as MP
import qualified Manipipe.Local.LocalBlender as MP

import TextShow 

type ManifestWenv = WidgetEnv  ManifestModel ManifestEvt
type ManifestNode = WidgetNode ManifestModel ManifestEvt

{-

TBA: AppEnv Variable that holds the important stuff independent of $PATH
You could also simplify handle initialization with AppEnv's associated Pattern

newtype AppEnv = AppEnv {
  _dbServerCtx :: DBServerCtx,
}

handleEvent appEnv wenv node model evt = case evt of
   -- I can access dbServerCtx where needed
   ...

buildUI appEnv wenv model = widgetTree where
   -- A child widget may need to receive it as a parameter; otherwise, it can be omitted
   ...

main :: IO ()
main = do
    -- Create any computationally expensive that requires one-time initialization
    dbCtx <- makeDbCtx
    let appEnv = AppEnv dbCtx
    startApp model (handleEvent appEnv) (buildUI appEnv) config
  where
    config = []
    model = ...

-}

getNumVectors :: ManieyeModel -> Int
getNumVectors model = case Data.List.length (model ^. imageSources) of
                        0 -> 0
                        _ -> Manifest.Types.length $ (model ^. imageSources) !! 0

buildUI :: ManifestWenv -> ManifestModel -> ManifestNode
buildUI wenv model = widgetTree where
  sectionBg = wenv ^. L.theme . L.sectionColor
  displayBytesImage bImage   = imageMem_ (name $ bImage) (bytes $ bImage) (size $ bImage) [alignCenter, alignMiddle, fitFill]
  manieyeImages              = hstack $ intersperse (spacer_ [width 50]) $ map (displayBytesImage) (model ^. manieyeModel . memImages) 

  maskAnnotationElements = animFadeIn_ [duration 2000] (vstack [
        separatorLine,
        spacer,
        hstack [
          --textField datasetDirTextField,
          textDropdownSV (model ^. selectedDataset) (\x -> UpdatePredictionSource $ dirPath x) (model ^. activeDatasets), --ruined
          --textDropdownS (selectedDataset) (model ^. activeDatasets),
          spacer,
          mainButton "Set Dataset" (SetDatasetDir) -- $ osSelectFolderDialog "Path to Dataset Folder" "/Home/")--unpack $ model ^. datasetDirTextField)
        ],
        spacer,
        separatorLine,
        spacer,
        animFadeIn_ [autoStart, duration 2000] $ hstack [
          manieyeImages
          --animFadeIn_ [autoStart, duration 2000] (box_ [alignCenter, alignMiddle] $ image_    (model  ^. imagePath)  [fitHeight, fitWidth, alignMiddle, alignCenter] `nodeVisible` (model ^. showImages == False && model ^. showManiga == False &&  model ^. showManiexS ==  False && (null (model ^. (manieyeModel . memImages))) ==  True)) `nodeKey` "GroundFloorB" 
          --mainButton "->" (SeekButton True)
        ] `nodeKey` "GroundFloorB",
        spacer,
        hslider_ (manieyeModel . imageIndex) 0 (fromIntegral $ (getNumVectors (model ^. manieyeModel) - 1)) [onChange ChangeImages] `nodeKey` "timelineSlider",
        vstack $ intersperse (spacer) $ map displayBytesImage (model ^. manieyeModel . chartImages),
        spacer,
        box_ [alignCenter, alignMiddle] $ hstack [
          mainButton "<<"  (SeekButton False),
          spacer,
          mainButton ">||"   (PlayBack),
          spacer,
          mainButton ">>"    (SeekButton True)
        ],
        spacer,
        --toggleButton_ remixSettings3Fill 
        box_ [alignCenter] $ hstack [
          spacer,
          (label (pack (show (round (model ^. manieyeModel . imageIndex))))),
          (label (pack ("/" ++ (show (getNumVectors (model ^. manieyeModel) - 1))))) 
        ]
        --
      ]) `nodeKey` "manieyeImages"

  isValidInput = model ^. experimentPath /= "Test"

  sideBar = animFadeIn_ [duration 2000] (vstack (intersperse spacer [
      label_ "GA Parameters" [resizeFactor 1],
      separatorLine,
      vstack [
        label_ "Experiment Path" [resizeFactor 1],
        textField (experimentPath),
        spacer,
        label_ "🆑" [resizeFactor 1] `styleBasic` [textFont "JP", textSize 20],
        spacer,
        numericField (mutationRateValue), 
        spacer,
        label_ "Mutation Baseline" [resizeFactor 1],
        numericField mutationBaseline, 
        spacer,
        label_ "Smooth Sigma" [resizeFactor 1],
        numericField smoothSigmaValue,
        spacer,
        label_ "Bag Size" [resizeFactor 1],
        numericField (bagSizePreset),
        spacer,
        label_ "Nucleotides" [resizeFactor 1],
        numericField nucleotides,
        spacer,
        label_ "Generation Terminus" [resizeFactor 1],
        textField iterationCount,
        spacer,
        label_ "IDE Startup Call" [resizeFactor 1],
        textField ide_command,
        spacer,
        filler,
        button "Run IDE"          (IDE (model ^. ide_command)),
        spacer,
        button "Run Maniga"       (StartManiga (model)),    --`nodeEnabled` False  --ReAdd Later --Cancel
        spacer,
        button "Create Mask Annotations"     (StartManigram (model))
        --button "Stop Maniga"      (StopManiga <processHandle>) 
      ] 
    ])) `nodeKey` "ManigaSideBar"

  sideBarManiex = animFadeIn_ [duration 2000] (vstack (intersperse spacer [
      label_ "TF Parameters"  [resizeFactor 1],
      -- label "📩" `styleBasic` [textFont "JP", textSize 20],
      separatorLine,
      vstack [
        label_ "Render Function" [resizeFactor 1],
        textField render_function, 
        spacer,
        label_ "Dataset Name" [resizeFactor 1],
        textField dataset_name, 
        spacer,
        label_ "Blend Sources" [resizeFactor 1],
        textField blend_sources, 
        spacer,
        label_ "Dataset Dir" [resizeFactor 1],
        textField dataset_dir,
        spacer,
        label_ "Output Dir" [resizeFactor 1],
        textField output_dir, 
        spacer,
        label_ "Classifier Dir" [resizeFactor 1],
        textField classifier_dir, 
        spacer,
        label_ "Model (.py)" [resizeFactor 1],
        textDropdown_ activeManiexScript genTypes maniexModelsType [],
        spacer,
        label_ "Model (.h5)" [resizeFactor 1],
        textField classifier_dir, 
        spacer,
        filler,
        hstack [
          label "Training Only",
          spacer,
          checkbox skipInference,
          spacer,
          separatorLine,
          spacer,
          label "Inference Only",
          spacer,
          checkbox skipTraining
        ],
        spacer, 
        button "Reboot ManiAPI"     (RebootManiex model),
        spacer,
        button "Configure Maniex (YAML)"   (ConfigureManiex model),
        spacer,
        button "Create Mask Annotations"   (StartManigram (model)),
        spacer,
        button "Run IDE"          (IDE (model ^. ide_command)),
        spacer,
        button "Run Maniex"         (StartManiex model), --`nodeEnabled` False -- Re-Add when Elevate Source is tested --Cancel
        spacer,
        button "Run Tensorboard"    (StartTensorBoard model)
      ]
    ])) `nodeKey` "ManiexSideBar"

  inspectorBody = label "Scene Objects"
      
  widgetTree = animFadeIn_ [autoStart, duration 2000] $ vstack [
        vstack [
          hstack [
             --Futhark plus this feature will cause a segfault :(
            spacer,
            filler,
            vgrid [
              animFadeIn_ [autoStart, duration 2000] (vstack[ 
                image_    (model  ^. imagePath)  [fitHeight, fitWidth, alignMiddle, alignCenter] `styleBasic` [padding 35] `nodeKey` "defaultImage" `nodeVisible` (model ^. showImages == False && model ^. showDiffernence == False && model ^. showManiga == False &&  model ^. showManiexS ==  False),
                --image_    (pack $ ((unpack $ model ^. experimentPath) ++ "/outputs/chart.png")) [alignMiddle, alignCenter]     `nodeVisible` (model ^. showCfg == True),  --textArea_ (manigaRecord) [readOnly] `nodeKey` "manigaOutput" `nodeVisible` (model ^. showCfg == True),      --image ((unpack $ experiment ^. experimentPath) ++ "/outputs/chart.png"))
                --image_    (pack $ ("./assets/images/tf.png")) [alignMiddle, alignCenter]     `nodeVisible` (model ^. showManiexS ==  True), 
                --textArea_ (maniexRecord) [readOnly] `nodeKey` "maniexOutput" `nodeVisible` (model ^. showManiexS ==  True), --image ((unpack $ experiment ^. experimentPath) ++ "/outputs/chart.png"))
                spacer,
                hstack [
                  box maskAnnotationElements
                ] `nodeVisible` (model ^. showDiffernence == True)
              ]) `nodeKey` "GroundFloor"
            ],
            spacer,
            scroll sideBar          `nodeVisible` (model ^. showManiga  && model ^. showManiexS == False),
            scroll (sideBarManiex)  `nodeVisible` (model ^. showManiexS && model ^. showManiga  == False)
          ],
          spacer,
          vstack  [
            separatorLine,
            box_ [alignCenter, alignMiddle] $ hstack [
              spacer, 
              toggleButton_ remixSettings3Fill (model ^. showManiexS == True ?  block :? (showManiga)) [onClick (RollOutDrawer "ManigaSideBar"), onChange defaultRollout]                 `styleBasic` [textFont "Remix", textSize 20, textMiddle] `nodeEnabled` (model ^. showManiexS == False)  `nodeKey` "parameterElem" `styleHover` [textColor (Monomer.Widgets.Single.Color 49 14 150 0), textSize 20, textMiddle],
              spacer,
              spacer, 
              spacer,
              spacer,
              toggleButton_ remixDatabaseLine  (model ^. showManiga == True ?  block :? (showManiexS)) [onClick (RollOutDrawer "ManiexSideBar"), onChange defaultRollout]                `styleBasic` [textFont "Remix", textSize 20, textMiddle]  `nodeEnabled` (model ^. showManiga == False) `nodeKey` "inspectorElem" `styleHover` [textColor (Monomer.Widgets.Single.Color 49 14 150 0), textSize 20, textMiddle],
              spacer,
              spacer, 
              spacer,
              spacer,
              toggleButton_ remixEye2Line      showDiffernence       [onClick (RollOutDrawer "manieyeImages"), onChange defaultRollout]   `styleBasic` [textFont "Remix", textSize 20, textMiddle] `nodeKey` "inspectorElem" `styleHover` [textColor (Monomer.Widgets.Single.Color 49 14 150 0), textSize 20, textMiddle],
              spacer
            ] `styleBasic` [padding 20, bgColor sectionBg]
          ]
        ] `styleBasic` [padding 5, bgColor sectionBg]
    ] `nodeKey` "Main"

handleEvent :: ManifestWenv -> ManifestNode -> ManifestModel -> ManifestEvt -> [EventResponse ManifestModel ManifestEvt ManifestModel ()]
handleEvent wenv node model evt  = case evt of
  ManimerInit              -> [ 
    Task $ do 
      orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE "LOADING FUTHARK GPU Context (CUDA)" 
      newCont <- createFutharkHandle
      orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE "FUTHARK GPU Context (CUDA): Success!" 
      return $ SetFutCont newCont,
    Task $ do
      home <- getHomeDirectory
      return $ SetHome home,
    Model $ model  
    -- & userHome   .~ getHomeDirectory
    & imagePath  .~ "./assets/images/bg2.bmp" 
    ]
  SetHome h -> [ Model $ model & userHome .~ h & datasetDirTextField .~ (pack h) & imagePairs .~ [(h ++ "/manieye/data/images/0003.png", Just $ h ++ "/manieye/data/images/0004.png")]]
  CreateAnnotations  mdl   -> [ Task $ do
    loadIntake mdl 
    orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE (show  (model ^. availableAnnotations))
    return $ AutoResolve
    ]

  ExperimentHook hook      -> [ Model $ model  & gaRecords        .~ hook]
  UpdateParameters sig mut -> [ Model $ model  & smoothSigmaValue .~ (fromJust sig) & mutationRateValue .~  mut]
  SetExperimentPath newPath _file value -> [
      Task $ do
        newInputDir <- Manifest.Utils.Conversion.findInputDir $ unpack newPath
        newPredDir  <- Manifest.Utils.Conversion.findPredDir $ unpack newPath
        inputImageMap <- case newInputDir of 
                            Just path -> getImagesWithNum path
                            Nothing   -> return Map.empty
        predImageMap <- case newPredDir of 
                            Just path -> getImagesWithNum path
                            Nothing   -> return Map.empty
        let result = pairImagesByNum inputImageMap predImageMap 
        return $ ManieyeUpdate result
        ]
  SetHover element         -> [ SetFocusOnKey element ]
  Cancel                   -> [Model $ model & showAdaptation .~ False & showInspector .~ False & showSelection .~ False & showUserInput .~ False & showManiexS .~ False ]
  -- SliderChanged value      -> [
  --   Model $ model & currentSlider .~ value,
  --   Task $ do
  --     (inputImg, diffImg, predImg) <- if (value >= (fromIntegral (Prelude.length (model ^. imagePairs))))
  --                   then do
  --                       return (emptyBytesImage, emptyBytesImage, emptyBytesImage)
  --                   else do 
  --                       let (inputPath, predPath) = (model ^. imagePairs) !! (fromFractional value)
  --                       inputBytesImage <- loadBytesImage inputPath
  --                       predBytesImage <- case predPath of
  --                                           Just path -> loadBytesImage path
  --                                           Nothing   -> return emptyBytesImage
  --                       let diffBytesImage = diffImages inputBytesImage predBytesImage 
  --                       return (inputBytesImage, diffBytesImage, predBytesImage)
  --     return $ UpdateImages inputImg diffImg predImg]
  -- CalculateDiff iA iB      -> [
  --   Model $ model & currentSlider .~ value,
  --   Task $ do
  --   inputBytesImage <- loadBytesImage inputPath
  --   predBytesImage  <- case predPath of
  --                       Just path -> loadBytesImage path
  --                       Nothing -> return emptyBytesImage
  --   let diffBytesImage = diffImages inputBytesImage predBytesImage 
  --   return (inputBytesImage, diffBytesImage, predBytesImage)
  --   ]
  AutoResolve              -> [--Task $ do
    --putStrLn "RHEEEEEEEEEEEEEEEEEEEEEEEEE"
    --return Dummy
    ] 
  UpdateIntake   intake    -> [Model $ model & availableAnnotations .~ intake]
  DoDifference   value     -> [
    ]
  AnnotationSelected bl    -> [Model $ model & annotationSelections .~ (append bl (model ^. annotationSelections))]
  IDE ide                  -> [ Task $ do 
    startIDE model ide
    return AutoResolve
    ]
  StartManiex  mdl             -> [ Task $ do
    Manifest.Utils.Subprocess.rebootSystemManiapi mdl
    Manifest.Utils.Subprocess.startManiex mdl
    return AutoResolve
    ]
  StartTensorBoard mdl -> [ Task $ do
    Manifest.Utils.Subprocess.startTensorBoard mdl
    return AutoResolve
    ]
  SetBinaryHandles h ph r       -> [ Model $ model & subprocListeners .~ h &  subprocesses .~ [ph],
    Task $ do
      status <- hIsEOF   (model ^. subprocListeners)
      if status == False
        then do
          hO <- hGetLine (model ^. subprocListeners)
          print hO
        else do
          print("EOF")
      if status == False
        then return (SetBinaryHandles h ph status) 
        else return AutoResolve
    ]    
  StartManiAPI  mdl            -> [ Task $ do 
    --Manimer.Utils.Subprocess.elevateSource -- Needs to be Fixed
    Manifest.Utils.Subprocess.startLocalManiapi mdl
    return AutoResolve
    ]
  StartManiga mdl              -> [Task $ do
    orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE  ("Booting Maniga")
    chan <- newChan 
    tid <- forkIO(startManigaTask mdl chan)
    return $ SetGACont $ GAHandle tid chan
      --return $ ListenGACont tid
    ]
  ListenGACont {-tid-}              -> [Task $ do
      contents <- readChan (gaSem $ fromJust $ model ^. gaContext)
      return $ CaptureManigaRecord contents
    ]
  CaptureManigaRecord   conts       -> [
    -- GARecord {
    -- _grGeneration = 0, 
    -- _grTotalIndividuals = 0, 
    -- _grBestLoss = 0.3064941465854645, 
    -- _grWorstLoss = 0.3245994746685028, 
    -- _grBagSize = 128, 
    -- _grEliteRank = 1.5625e-2, 
    -- _grParentRank = 0.25, 
    -- _grMutationProbability = 2.5e-3, 
    -- _grMutationBaseline = 0.0, 
    -- _grDistributionSigma = 0.3, 
    -- _grCycleLength = 0, 
    -- _grNumSignals = 0, 
    -- _grSignalRate = 0.0, 
    -- _grNumRandomPicks = 64, 
    -- _grSamePickLimit = 4096}

      Model $ model & currentGeneration .~  (conts ^. grGeneration) &  currentLoss .~ (conts ^. grBestLoss) & (currentHistogram .~ (conts ^. grTotalIndividuals)) & smoothSigmaValue .~ (fromFractional $ conts ^. grDistributionSigma) & mutationBaseline .~ (fromFractional $ conts ^. grMutationBaseline) & bagSizePreset .~ (fromIntegral $ conts ^. grBagSize) & mutationRateValue .~ (fromFractional $ conts ^. grMutationProbability) & iterationCount .~ pack "1"     
      --manigaRecord .~ (pack $ show conts)  
    ]
  -- UpdateManiga record          -> [Task $ do
  --   orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_WARNING  "RECORD RECEIVED"
  --   print (model ^. manigaRecord)
  --   return AutoResolve,
  --   Model $ model & manigaRecord .~ (pack $ show (record))]


  --(currentGeneration .~ generation)         :: Generation,
  --(currentLoss       .~ topLoss)            :: Maniga.Type.Base.Loss,
  --(currentHistogram  .~ distributionSigma)  :: Double, 

  StopManiga  p                -> [Task  $ do
      --stopManiga p
      return AutoResolve
    ]
  ManieyeUpdate newImages -> [
    Model $ model & imagePairs .~ newImages & currentHistogram .~ 0
    ]
  SetInitalDatasetPath     -> [ Task $ do 
    sr  <- osSelectFolderDialog "Path to Inital" "./assets/datasets/" 
    case sr of
      Nothing   -> return $ HandleFile
      Just val  -> do
        orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE ("Dataset: " ++ (unpack (val)))
        return $ SetLensPath initialDir val 
        ]
  SetPredictedDatasetPath   -> [ Task $ do 
    sr  <- osSelectFolderDialog "Path to Predicted" "./assets/datasets/" 
    case sr of
      Nothing   -> return $ HandleFile
      Just val  -> do
        orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE ("Dataset: " ++ (unpack (val)))
        return $ SetLensPath predDir val  
        ]
  RefreshImagePairs        -> [ Task $ do 
      inputImageMap <- getImages (unpack $  model ^. initialDir)
      predImageMap  <- getImages (unpack $  model ^. predDir)
      let result = zipImages inputImageMap predImageMap 
      return $ ManieyeUpdate result
      ]
  RebootManiex mdl         -> [ Task $ do
      --print "reboot"
      rebootSystemManiapi mdl
      return AutoResolve
      ]
  ConfigureManiex  mdl      -> [ Task $ do 
      configureManiex mdl
      return AutoResolve 
      ]
  StartManiBlend mdl            -> [ Task $ do 
      result <- try' $ (Manifest.Utils.Subprocess.startManiblend mdl)
      return AutoResolve
    ]
  StartManigram mdl            -> [ Task $ do 
      result <- try' $ (Manifest.Utils.Subprocess.startManigram mdl)
      return AutoResolve
    ]
  SetLensPath     a  s      -> [ Model $ model & a .~ s, Event RefreshImagePairs ] 
  HandleFile                -> [Task $ do
    (notifyPopup "IOException" "Invalid File Path or Cancelled I/O Operation" Graphics.UI.TinyFileDialogs.Warning)
    return AutoResolve
    ]
  SetPath            s      -> [ Model $ model & chartImage        .~  s ]
  SetDatasetPath  i  p      -> [ Model $ model & inputImageMem .~ i & predImageMem .~ p ]
  SetHeatPath        s      -> [ Model $ model & maskedAnnotationsPath  .~ (pack ((unpack $ model ^. experimentName) ++ (unpack s)))  ]
  UpdateAnnotations  s      -> []
  ForceUpdateImage   s      -> [ Model $ model & maskedAnnotationsPath  .~ (pack ((unpack $ model ^. experimentName) ++ (unpack s)))  ]
  Toggle                    -> []
  Dummy                     -> []
  --ManieyeEvts
  SeekButton next -> [Task $ do 
    case next of
      True  -> return $ ChangeImages (if ((model ^. manieyeModel . imageIndex) + 1) >=        (fromIntegral $ getNumVectors (model ^. manieyeModel)) then 0 else ((model ^. manieyeModel . imageIndex) + 1))
      False -> return $ ChangeImages (if ((model ^. manieyeModel . imageIndex) - 1) < 0 then ((fromIntegral $ getNumVectors (model ^. manieyeModel)) - 1) else   ((model ^. manieyeModel . imageIndex) - 1))
    ]
  PlayBack -> [
    Model $ model & playback %~ not,
    Event PlayBackCycle
    -- Task $ (current < terminus) ? (return $ PlayBackCycle) :? (return $ PlayBack False)
    ]
  PlayBackCycle -> case (model ^. playback) of
      True  -> [
        Event $ SeekButton True,
        Task $ do 
          threadDelay 500000
          return PlayBackCycle
        ]--Model $ model & (manieyeModel . imageIndex)  +~ 1
      False -> []--Model $ model & (manieyeModel . imageIndex)  .~ (model ^. manieyeModel . imageIndex)
  SetDatasetDir -> [--be *very careful* when editting this Event as all it's commented code is valid and sometimes preferred
    Task $ do
      filePath <- liftIO $ osSelectFolderDialog "Path to Dataset Folder" (pack $ model ^. userHome)
      case filePath of
        Nothing  -> do
          (notifyPopup "IOException" "Invalid File Path or Cancelled I/O Operation" Graphics.UI.TinyFileDialogs.Warning)
          return $ AutoResolve
        Just fp  -> do
          (Just batchDir)     <- findInputDirectory (unpack fp)
          (Just sceneDir)     <- findSceneDir ((unpack fp))
          batchDirContents    <- (loadDirContents batchDir)
          datasetList         <- loadFolderContents  ((unpack fp) ++ "/Output/" )
          --2022-05-12_15:56:28
          datasetListContents <- mLoadFolderContents ((unpack fp  ++ "/Output/" ++ (datasetList !! 0) ++ "/Predictions/Images/")) -- (model ^. selectedDataset)
          let dataSets = (Prelude.map (Dataset) datasetList)
          let standardSource   =  ManieyeImageFolder $ Prelude.map (\x -> (unpack fp ++ "/Seq/") ++ x) batchDirContents
          let predictedSource  = case datasetListContents of 
                Just lst -> ManieyeImageFolder $ Prelude.map (\x -> (unpack fp  ++ "/Output/" ++ (datasetList !! 0) ++ "/Predictions/Images/" ++ x))  (lst)
                Nothing  -> ManieyeImageNothing
          let differenceSource = ManieyeDifferenceImages ((standardSource == ManieyeImageNothing ? ["./assets/images/broken.png"] :? (images standardSource))) ((predictedSource == ManieyeImageNothing ? ["./assets/images/broken.png"] :? (images predictedSource)))
          --Prelude.map (\x -> (unpack fp ++ "/Seq/") ++ x) --batchDirContents
          --let presenceSource =  ManieyeImageFolder $ Prelude.map (\x -> (unpack fp ++ "/Seq/") ++ x) batchDirContents
          --standardSource   <- createRenderImageSource sceneDir batchDir StandardRender      "Standard"
          --presenceSource   <- createRenderImageSource sceneDir batchDir PresenceRender      "Presence"
          --visSource        <- createRenderImageSource sceneDir batchDir VisualizationRender "Visualization"
               
          orderedMessage $ ManiLogMessage MANI_LOG_HEAD MANI_LOG_ZONE ("InitialContents: " ++ (unpack fp) ++ "/Seq/")
          orderedMessage $ ManiLogMessage MANI_LOG_BODY MANI_LOG_ZONE ("datasetContents: " ++ (unpack fp) ++ "/Output/" )
          --orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE ("Output: " ++ (show predictedSource))
          orderedMessage $ ManiLogMessage MANI_LOG_TAIL MANI_LOG_ZONE ("Difference: " ++ (show differenceSource))

          -- initialContents  <- loadDirContents    ((unpack fp) ++ "/Seq/")
          -- datasetContents  <- loadDirContents    ((unpack fp) ++ "/Output/date-time/Predictions/Images/")
  
          --
          -- let diffRaws     =  Data.List.zip initialContents datasetContents
          -- diffImageList <- mapM (\(a, b) -> do
          --   imgA <- readManiImage ((unpack fp) ++ "/Seq/" ++ a)
          --   imgB <- readManiImage ((unpack fp) ++ "/Output/date-time/Predictions/Images/" ++ b)

          --   (ImageResp result) <- runFutharkCommand (fromJust $ model ^. gpuContext) $ DiffImages imgA imgB
          --   -- writeManiImage ("/tmp/diff/" ++ a ++ ".png") result 
          --   return $ mpImageToBytesImage result $ pack $ a ++ "diff"
          --   ) (take 50 $ diffRaws)
          -- return $ GPUProcess diffRaws fp
          --

          print batchDir
          print $ (unpack fp) ++ "/GroundTruths/" ++  "seq_sv.mani"
          print $ (unpack fp) ++ "/GroundTruths/" ++  "seq_mask.mani"
          masks <- messageBox "Load Masks" "Load Masked SV?" Question TFD.YN_Yes
          case masks of
            YN_No -> do
              (notifyPopup "Warning" "Ignoring Mask SVs" Graphics.UI.TinyFileDialogs.Warning)
              (MP.Mani (svData:: A.Array A.S A.Ix2 Float)) <- MP.loadManifile $ (unpack fp) ++ "/GroundTruths/" ++  "seq_sv.mani" --batchDir  </> "seq_sv.mani"
              let svChartSource     = ManieyeSVChart (A.compute $ A.index' (A.computeAs A.B $ A.innerSlices svData) $ A.Ix1 0)
              return $ UpdateImageSources (unpack fp) (datasetList) [standardSource, differenceSource, predictedSource{-, presenceSource, visSource , ManieyeDifferenceFolder diffImageList-}] [svChartSource]
            YN_Yes -> do
              (MP.Mani masks)  <- MP.loadManifile $ (unpack fp) ++ "/GroundTruths/" ++ "seq_mask.mani" --batchDir  </> "seq_mask.mani"
              (MP.Mani (svData:: A.Array A.S A.Ix2 Float)) <- MP.loadManifile $ (unpack fp) ++ "/GroundTruths/" ++  "seq_sv.mani" --batchDir  </> "seq_sv.mani"
              let maskChartSource   = ManieyeMaskChart masks 
              let svChartSource     = ManieyeSVChart (A.compute $ A.index' (A.computeAs A.B $ A.innerSlices svData) $ A.Ix1 0)
              return $ UpdateImageSources (unpack fp) (datasetList) [standardSource, differenceSource, predictedSource{-, presenceSource, visSource , ManieyeDifferenceFolder diffImageList-}] [maskChartSource, svChartSource]
              --Model $ model & activeDatasets .~ (Prelude.map (Dataset) dirContents)
    ]
  UpdateImageSources fp dirContents newSources  newChartSources -> [
    Model $ model 
      & manieyeModel . datasetDir   .~ fp 
      & manieyeModel . imageSources .~ newSources 
      & manieyeModel . chartSources .~ newChartSources 
      & manieyeModel . memImages    .~ map (\_ -> emptyBytesImage) newSources 
      & manieyeModel . chartImages  .~ map (\_ -> emptyBytesImage) newChartSources 
      & activeDatasets .~ (Prelude.map (Dataset) dirContents),
    Event $ ChangeImages 0
    ]
  UpdatePredDirList newList -> [
    Model $ model & manieyeModel . predictionDirs .~ newList,
    Event $ case (Data.List.length (model ^. manieyeModel . predictionDirs)) of
      0 -> DoNothing
      _ -> SetPredDir $ head (model ^. manieyeModel . predictionDirs) 
    ]
  SetPredDir newDir -> [
    Model $ model & manieyeModel . selectedPrediction .~ newDir,
    Task $ do
      return undefined
    ]
  ChangeImages newIndex -> Model (model & manieyeModel . imageIndex .~ newIndex) 
    :  (map (\d -> Event $ GetNewImage d $ fromFractional newIndex) $ take (Data.List.length $ model ^. manieyeModel . imageSources) [0..])
    ++ (map (\d -> Event $ GetNewChart d $ fromFractional newIndex) $ take (Data.List.length $ model ^. manieyeModel . chartSources) [0..])
  GetNewImage d i -> [Task $ do
    newImage <- getNthImage (model ^?! manieyeModel . imageSources . ix d) i
    case newImage of 
      Nothing ->  do 
        putStrLn "Got nothing for NthImage"
        brokenImage <- loadBytesImage "./assets/images/broken.png"
        return $ SetNewImage d brokenImage --DoNothing
      Just img -> (return $ SetNewImage d img)
    ]
  GetNewChart d i -> [ Task $ do
    newChart <- getNthChart (model ^?! manieyeModel . chartSources . ix d) $ fromIntegral i
    return $ SetNewChart d newChart
    ]
  SetNewImage d img   -> [Model $ model & manieyeModel . memImages . ix d .~ img]
  SetNewChart d chart -> [Model $ model & manieyeModel . chartImages . ix d .~ chart]
  CleanupManieye -> [Task $ do
    mapM
      (\s -> case s of
        ManieyeImageRender lb _ _ _ -> MP.killLocalBlender lb
        _ -> return ()
      ) 
      (model ^. manieyeModel . imageSources) 
    return DoNothing
    ] 
  UpdatePredictionSource src -> [--presume both selectedDataset and activeDatasets is valid
    Task $ do
      print $ "Dataset Path: " ++ (model ^. (manieyeModel . datasetDir))
      print $ "Selected Output Dataset: " ++ src
      print $ "Full Path: " ++ (((model ^.  (manieyeModel . datasetDir))  ++ "/Output/" ++ src ++ "/Predictions/Images/")) 
      (Just batchDir)     <- findInputDirectory (model ^. (manieyeModel . datasetDir))
      (Just sceneDir)     <- findSceneDir ((model ^.  (manieyeModel . datasetDir)))
      batchDirContents    <- (loadDirContents batchDir)
      datasetList         <- loadFolderContents  ((model  ^.  (manieyeModel . datasetDir)) ++ "/Output/" )
      datasetListContents <- mLoadFolderContents (((model ^.  (manieyeModel . datasetDir))  ++ "/Output/" ++ src ++ "/Predictions/Images/")) -- (model ^. selectedDataset)
      let dataSets = (Prelude.map (Dataset) datasetList)
      let standardSource   =  ManieyeImageFolder $ Prelude.map (\x -> ((model ^.  (manieyeModel . datasetDir)) ++ "/Seq/") ++ x) batchDirContents
      let predictedSource  = case datasetListContents of 
            Just lst -> ManieyeImageFolder $ Prelude.map (\x -> ((model ^.  (manieyeModel . datasetDir))  ++ "/Output/" ++ src ++ "/Predictions/Images/" ++ x))  (lst)
            Nothing  -> ManieyeImageNothing
      print "prepare diff"
      print $ "path: " ++ (((model ^.  (manieyeModel . datasetDir))  ++ "/Output/" ++ src ++ "/Predictions/Images/")) 
      print $ show predictedSource
      let differenceSource = ManieyeDifferenceImages (images standardSource) (predictedSource == ManieyeImageNothing ? replicate (Data.List.length $ images standardSource) "./assets/images/broken.png" :? (images predictedSource) )
      return $ UpdateImageSources ((model ^.  (manieyeModel . datasetDir))) (datasetList) [standardSource, differenceSource, predictedSource] [],
      Model $ model & selectedDataset .~ Dataset src
    ] 

  SetFutCont newCont  -> [(Model $ model & gpuContext .~ (Just newCont)), Event $ RollOutDrawer "Main"]
  SetGACont  newCont  -> [Model  $ model & gaContext  .~ (Just newCont), Event $ ListenGACont]
  RollOutDrawer s     -> [Message (WidgetKey (pack s)) AnimationStart]
  DefaultRollOut      -> [Message (WidgetKey (pack "GroundFloor")) AnimationStart,
    Message (WidgetKey (pack "GroundFloorB")) AnimationStart
    ]
  DoNothing -> []

main :: IO ()
main = 
  startApp model handleEvent buildUI config 
  where
      model  = ManifestModel  False False False False False False False False False  [] "" "./assets/images/bg2.bmp"  "./assets/experiments/default"  defaultBytesImage defaultBytesImage defaultBytesImage [] [] [] "./assets/images/testChart_bg.png"  "" 0 0 0 0 0 0 0 0 0 0 BS.empty "./assets/images/initial.png" 0 [] "" "" "" False False "" "" "" "" "" "" "code" "code" "0" "fishman_scene.renderSceneData" "./assets/images/Heat.png" [] [] (pack "Maniga Process Monitoring \n \nSTANDBY") (pack "Maniex Process Monitoring \n \nSTANDBY") "" "" "" False "" (ManieyeModel "" [] "" [] [] [] [] 0) def [None] Nothing stdout Nothing NONE Manifest.Types.ViT  False False False ""
      config = [
        appWindowTitle "Manifest",
        appWindowIcon    "./assets/images/manifest.bmp",
        appTheme darkTheme,
        appWindowResizable True,
        appFontDefFile    "Remix"             "./assets/fonts/remixicon.ttf",
        appFontDefFile    "Regular"           "./assets/fonts/cmuntt.ttf", --"./assets/fonts/Roboto-Regular.ttf",
        -- appFontDefFile    "JP"             "./assets/fonts/og-dcm-emoji.ttf",
        -- appFontDefMem "JP" $(embedFile "./assets/fonts/og-dcm-emoji.ttf"), -- For some Reason it despises this 
        appInitEvent ManimerInit,
        appMaxFps 60,
        --appRenderOnMainThread  
        --appDisposeEvent CleanupManieye
        appExitEvent CleanupManieye
        ]
