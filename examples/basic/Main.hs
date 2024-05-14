{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Lens (at, ixAt, Ixed (ix), (?~), (&), (<>~), (^.), (^?), (.~), to, makeLensesFor, singular, makeLenses)
import Control.Lens.At
import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as S
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, fromGregorian)

import Text.Printf (printf)
import Data.String ( IsString )
-- import Monomer.Hagrid (styleConfig)
import Graphics.UI.TinyFileDialogs
    ( notifyPopup, openFileDialog, saveFileDialog, IconType(..) )
import GHC.Arr (ixmap)

import Avarice (Column (..), ColumnAlign (..), ColumnFooterWidget (..), ColumnSortKey (..), SortDirection (..), hagrid_, initialSort, scrollToRow, showOrdColumn, textColumn, widgetColumn, parseXmlSource)
import Avarice.Data.Script () 
import Monomer

data HGScriptData = HGScriptData --Spider = Spider
  { index     :: Int,
    hgText    :: Text,  --species
    hgChapter :: Text,  --name
    hgColorId :: Int, --Day,   --dateOfBirth
    hgWidth   :: Double --dateOfBirth
  } deriving (Eq, Show)

--TODO: Break up main into a library of relevant modules
data AppModel = AppModel
  { theme :: Theme,
    spiders :: Seq HGScriptData, --Spider,
    columns :: [AppColumn],
    rowToScrollTo :: Int,
    renderedFont  :: Font,
    loadedXML :: [String],
    toggleProjectPrefs :: Bool,
    groupExport        :: Bool,
    doNotAssociate     :: Bool,
    loadedFonts        :: [Font],
    addFont            :: Text
  }
  deriving (Eq, Show)

newtype AppColumn = AppColumn {enabled :: Bool} deriving (Eq, Show)
instance At [Font]

data AppEvent
  = FeedSpider Int
  | EditScript Int Text
  | AddSpider
  | NameColumnResized Int
  | NameColumnSorted SortDirection
  | ScrollToOriginalIndex
  | UpdateRenderedFont Font
  | OpenFile
  | SaveFile
  | OpenProject
  | SaveProject
  | ProcessFile Text
  | ProcessNotification
  | InvalidFile
  | OpenProjectPerfs
  | PromptLoadedFont
  | AddLoadedFont  Font
  | UpdateFonts    Text
  | DummyEvent

makeLensesFor [("enabled", "_enabled")] ''AppColumn
makeLensesFor [
    ("columns", "_columns"),
    ("theme", "_theme"),
    ("loadedXML", "_loadedXML"),
    ("renderedFont", "_renderedFont"),
    ("rowToScrollTo", "_rowToScrollTo"),
    ("toggleProjectPrefs", "_toggleProjectPrefs"),
    ("groupExport",    "_groupExport"),
    ("loadedFonts",    "_loadedFonts"),
    ("doNotAssociate", "_doNotAssociate"),
    ("addFont", "_addFont")
  ] ''AppModel

makeLensesFor [("hgText",  "_hgText")] ''HGScriptData

flipRenderedFont :: (Eq a, IsString a) => a -> AppEvent
flipRenderedFont fs = case fs of
                        "Regular"  -> UpdateRenderedFont "JPN"
                        "JPN"      -> UpdateRenderedFont "Regular"
                        _          -> error "Invalid Font Requested (the requested font lacks a FontDef in AppConfig)"

main :: IO ()
main = startApp model handleEvent buildUI config
  where
    model  =
      AppModel
        { theme   = darkTheme,
          spiders = spiders,
          columns = AppColumn True <$ gridColumns model,
          rowToScrollTo = 0,
          renderedFont  = "Regular",
          loadedXML     = [],
          toggleProjectPrefs = False,
          groupExport    = False,
          doNotAssociate = False,
          loadedFonts    = ["Regular", "JPN", "Add"],
          addFont        = ""
        }
    fonts   = [] --[appFontDef (T.pack $ "new_font_" ++ show [0..]) (model ^. _addFont)]
    config  =
      [ appWindowTitle "Avarice (Pre-Alpha Build)",
        appFontDef "Bold"    "./assets/fonts/Cantarell/Cantarell-Bold.ttf",
        appFontDef "Regular" "./assets/fonts/Cantarell/Cantarell-Regular.ttf",
        appFontDef "JPN"     "./assets/fonts/Aosagi/YDWaosagi.otf",
        appWindowIcon "./assets/logo.png",
        appDisableAutoScale True,
        appWindowState (MainWindowNormal (1200, 1000))
      ] ++ fonts
    spiders  = S.fromFunction numSpiders spider
    spider i =
      HGScriptData
        { index     = i,
          hgText    = "Acromantula",
          hgChapter = T.pack (printf "Son of Aragog %d" (i + 1)),
          hgColorId = fromIntegral i,-- addDays (fromIntegral i) (fromGregorian 1942 3 1),
          hgWidth   = fromIntegral (numSpiders + 2 - i) * 2.3
        }
    numSpiders = 100

updateFonts :: Text -> AppEvent
updateFonts = UpdateFonts

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = if model ^. _toggleProjectPrefs then optionsPerfs else tree
  where
    optionsPerfs =
      themeSwitch_ model.theme [themeClearBg] $
        vstack
          [
            spacer,
            separatorLine,
            spacer,
            button "Return to Grid View" OpenProjectPerfs `styleBasic` [textFont (model ^. _renderedFont)],
            spacer,
            separatorLine,
            spacer,
            label "Set Loaded Fonts"`styleBasic` [textFont (model ^. _renderedFont)],
            spacer,
            -- create function for creating a list of elements, in this instance
            vstack $ map (inputFontEntry . unFont) (model ^. _loadedFonts),
            spacer,
            textField _addFont,
            spacer,
            button "Add Font" PromptLoadedFont,
            spacer,
            separatorLine,
            spacer,
            label  "Export Options"   `styleBasic` [textFont (model ^. _renderedFont)],
            spacer,
            toggleButton "Export All Files Together"  _groupExport,
            spacer,
            toggleButton "Do Not Associate Loaded Files (In Project)" _doNotAssociate,
            spacer,
            separatorLine,
            spacer
          ]
    inputFontEntry s = textFieldV s updateFonts
    tree =
      themeSwitch_ model.theme [themeClearBg] $
        vstack
          [
            spacer,
            separatorLine,
            hstack [
              button "Load Project"    OpenProject `styleBasic` [textFont (model ^. _renderedFont)],
              spacer,
              button "Export Project"  SaveProject `styleBasic` [textFont (model ^. _renderedFont)],
              spacer,
              button "Project Options" OpenProjectPerfs `styleBasic` [textFont (model ^. _renderedFont)]
            ] `styleBasic` [padding 8],
            separatorLine,
            spacer,
            grid `nodeKey` hagridKey `styleBasic` [textFont (model ^. _renderedFont)],
            vstack_
              [childSpacing]
              (separatorLine : themeConfigurer : columnConfigurers <> actionButtons)
              `styleBasic` [padding 8, textFont (model ^. _renderedFont)]
          ]
    grid =
      hagrid_
        [initialSort 1 SortDescending] -- styleConfig (model ^. _renderedFont) --`styleBasic` [textFont (model ^. _renderedFont)]
        (mconcat (zipWith column model.columns (gridColumns model)))
        model.spiders

    column (AppColumn enabled) columnDef =
      [columnDef | enabled]

    themeConfigurer =
      hstack_
        [childSpacing]
        [ labeledRadio_ "Dark Theme"  darkTheme  _theme [textRight] `styleBasic` [textFont (model ^. _renderedFont)],
          labeledRadio_ "Light Theme" lightTheme _theme [textRight] `styleBasic` [textFont (model ^. _renderedFont)]
        ]

    columnConfigurers =
      zipWith columnConfigurer [0 .. length model.columns - 1] (gridColumns model)

    columnConfigurer :: Int -> Column AppEvent HGScriptData -> WidgetNode AppModel AppEvent
    columnConfigurer idx columnDef =
      labeledCheckbox_
        columnDef.name
        (_columns . singular (ix idx) . _enabled)
        [textRight]
        `styleBasic` [textFont (model ^. _renderedFont)]

    actionButtons =
      [ hstack_
          [childSpacing]
          [ label "Scroll to index of unsorted list" `styleBasic` [textFont (model ^. _renderedFont)],
            numericField _rowToScrollTo `styleBasic` [textFont (model ^. _renderedFont)],
            button "Go!" ScrollToOriginalIndex `styleBasic` [textFont (model ^. _renderedFont)]
          ],
        separatorLine,
        hstack_
          [childSpacing]
          [button "Add entry" AddSpider `styleBasic` [textFont (model ^. _renderedFont)], button "Load File" OpenFile,  button "Export File" SaveFile, button "Flip Between Source Text" (flipRenderedFont (model ^. _renderedFont)) `styleBasic` [textFont (model ^. _renderedFont)], label "TESTING: 血粉が…" `styleBasic` [textFont (model ^. _renderedFont)], label "(SRC: EN/JP)" `styleBasic` [textFont (model ^. _renderedFont)]]
      ]

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node model = \case
  FeedSpider idx
    | Just spdr <- S.lookup idx model.spiders ->
        [ Producer (const (putStrLn ("Feeding spider " <> T.unpack spdr.hgText))),
          Model model {spiders = S.update idx spdr {hgWidth = spdr.hgWidth + 1} model.spiders}
        ]
  FeedSpider _ ->
    []
  EditScript idx s
    | Just spdr <- S.lookup idx model.spiders ->
      [ Producer (const (putStrLn ("Editing line " <> show idx <> " " <> T.unpack spdr.hgText))),
        Model model {spiders = S.update idx spdr {hgText = s} model.spiders}
      ]
  EditScript _ s -> [Task $ do
     print $ "Invalid Index Given for Edit Action: " ++ T.unpack s
     return DummyEvent]
  AddSpider  ->
    [Model model {spiders = model.spiders :|> newSpider model}]
  NameColumnResized colWidth ->
    [Producer (const (putStrLn ("Name column was resized: " <> show colWidth)))]
  NameColumnSorted direction ->
    [Producer (const (putStrLn ("Name column was sorted: " <> show direction)))]
  ScrollToOriginalIndex ->
    [scrollToRow (WidgetKey hagridKey) (rowScrollIndex model)]
  UpdateRenderedFont s -> [Model model {renderedFont = s}]
  DummyEvent ->  []
  OpenFile   ->  [Task $ do
     sr <- openFileDialog "Select HGScript(XML)" "./" ["*.xml"] "xml(.xml) files" False
     case sr of
       Nothing  -> return InvalidFile -- Failure Event
       Just val -> return $ ProcessFile (head val)
       ]
  SaveFile   ->  [Task $ do
     sr <- saveFileDialog "Save XML" "./" ["*.xml"] "xml files"
     case sr of
       Nothing  -> return InvalidFile -- Failure Event
       Just _   -> return ProcessNotification
       ]
  OpenProject -> [Task $ do
     sr <- openFileDialog "Select Project" "./" ["*.json"] "json(.json) files" False
     case sr of
       Nothing  -> return InvalidFile -- Failure Event
       Just val -> return $ ProcessFile (head val)
       ]
  SaveProject -> [Task $ do
     sr <- saveFileDialog "Save Project" "./" ["*.json"] "json(.json) files"
     notifyPopup "Project Saved" "Created and/or saved Project file" Info
     case sr of
       Nothing  -> return InvalidFile -- Failure Event
       Just _   -> return ProcessNotification
       ]
  ProcessFile fl -> [Model $ model & _loadedXML <>~ [T.unpack fl], Task $ do
                      parseXmlSource ("C:/Users/Manda/OneDrive/Desktop/avarice/examples/basic/hgtest.xml" :: FilePath)
                      return DummyEvent]
  ProcessNotification -> [Task $ do
    notifyPopup "Project Saved" "Created and/or saved Project file" Info
    return DummyEvent]
  InvalidFile -> [Task $ do
      print "invalid file"
      return DummyEvent
       ]
  OpenProjectPerfs   -> [Model $ model & _toggleProjectPrefs .~ not (model ^. _toggleProjectPrefs)]
  UpdateFonts   tst  -> [Model $ model & _loadedFonts . at 0 ?~ Font tst ]
  AddLoadedFont trt  -> [Model $ model & _loadedFonts <>~ [trt]]
  PromptLoadedFont   -> [Task $ return $ AddLoadedFont (Font $ model ^. _addFont)]

newSpider :: AppModel -> HGScriptData
newSpider model =
  HGScriptData
    { index     = fromIntegral (length model.spiders),
      hgText    = "Extra Spider " <> pack (show (length model.spiders)),
      hgChapter = "Spider plant",
      hgColorId = length model.spiders, --fromGregorian 2022 6 26,
      hgWidth   = 0.01
    }

rowScrollIndex :: AppModel -> Seq (HGScriptData, Int) -> Maybe Int
rowScrollIndex model items =
  snd <$> S.lookup model.rowToScrollTo items

gridColumns :: AppModel -> [Column AppEvent HGScriptData]
gridColumns model = cols
  where
    cols =
      [ (showOrdColumn "Index" (.index))
          { initialWidth = 120,
            align        = ColumnAlignRight,
            footerWidget = CustomFooterWidget (countFooter model)
          },
        (textColumn "Text" (.hgText))
          { initialWidth  = 300,
            resizeHandler = Just NameColumnResized,
            sortHandler   = Just NameColumnSorted
          },

          (widgetColumn "Text Type" editColumn)
          { initialWidth = 200
          },
        --(textColumn "Text Type" (.hgChapter))
          --{ initialWidth = 200
          --},
        (textColumn "Chapter" (T.pack . show . (.hgColorId))) --(T.pack . formatTime defaultTimeLocale "%Y-%m-%d" . (.hgColorId)))
          { initialWidth = 200
          },
        (textColumn "Width" (T.pack . printf "%.2f" . (.hgWidth)))
          { sortKey = SortWith (.hgWidth),
            initialWidth = 200,
            align = ColumnAlignRight
            --footerWidget = CustomFooterWidget sumWeightFooter
          },
        (widgetColumn "Actions" actionsColumn)
          { initialWidth = 100,
            paddingW = 5,
            paddingH = 5
          }
      ]
      
    countFooter model spiders = vstack [
        labelledFooter model "Number of Loaded Scripts: " (T.pack . show . length $ spiders)
      ]

    sumWeightFooter model spiders = tree
      where
        tree = labelledFooter model "SRC Language:" (T.pack $ show $ totalWeightKilos model) --labelledFooter model "Sum" (T.pack (printf "%.2f" totalWeightKilos))
        totalWeightKilos = sum ((.weightKilos) . fst <$> spiders)

    {-
    sumWeightFooter model {-spiders-} = tree
      where
        tree = labelledFooter model "SRC Language:" (T.pack $ show $ totalWeightKilos model) --labelledFooter model "Sum" (T.pack (printf "%.2f" totalWeightKilos))
        totalWeightKilos ms = ms ^. _renderedFont                                            --sum ((.weightKilos) . fst <$> spiders)
    -}

    labelledFooter model labelText text =
      hstack
        [ label labelText `styleBasic` [textFont (model ^. _renderedFont)],
          filler,
          label text
            `styleBasic` [textFont (model ^. _renderedFont)] --`styleBasic` [textFont "Bold"]
        ]
        `styleBasic` [padding 10]

    actionsColumn :: Int -> HGScriptData -> WidgetNode s AppEvent
    actionsColumn idx _spdr = button "Edit" (FeedSpider idx) `styleBasic` [textFont (model ^. _renderedFont)]

    editColumn :: Int -> HGScriptData -> WidgetNode s AppEvent
    editColumn idx _spdr    = textFieldV (_spdr ^. _hgText) (EditScript idx) `styleBasic` [textFont (model ^. _renderedFont)]

hagridKey :: Text
hagridKey = "SpiderHagrid"
