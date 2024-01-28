{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import System.IO ( IOMode(ReadMode), hClose, hGetContents, openFile )  
import Control.Monad ()
import Control.Lens (Ixed (ix), (^.), (.~), makeLensesFor, singular)
import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as S
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time (Day, addDays, defaultTimeLocale, formatTime, fromGregorian)
import Monomer
import Monomer.Hagrid (Column (..), ColumnAlign (..), ColumnFooterWidget (..), ColumnSortKey (..), SortDirection (..), hagrid_, initialSort, scrollToRow, showOrdColumn, textColumn, widgetColumn)
import Text.Printf (printf)
import Data.String ( IsString )

data AppModel = AppModel
  { theme :: Theme,
    spiders :: Seq Spider,
    columns :: [AppColumn],
    rowToScrollTo :: Int,
    renderedFont  :: Font
  }
  deriving (Eq, Show)

newtype AppColumn = AppColumn
  {enabled :: Bool}
  deriving (Eq, Show)

data AppEvent
  = FeedSpider Int
  | AddSpider
  | NameColumnResized Int
  | NameColumnSorted SortDirection
  | ScrollToOriginalIndex
  | UpdateRenderedFont Font

data Spider = Spider
  { index :: Int,
    species :: Text,
    name :: Text,
    dateOfBirth :: Day,
    weightKilos :: Double
  }
  deriving (Eq, Show)

makeLensesFor [("enabled", "_enabled")] ''AppColumn
makeLensesFor [("columns", "_columns"), ("theme", "_theme"), ("renderedFont", "_renderedFont"), ("rowToScrollTo", "_rowToScrollTo")] ''AppModel

xmlParseTest :: IO ()
xmlParseTest = do  
        handle   <- openFile "./examples/basic/hgtest.xml" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
            list        = f singlewords
        print list
        hClose handle   

f :: [String] -> [Int]
f = map read

--
{-
  <hgscript>
  <text width="96">血粉が…</text><newline />
  <text width="336">まるでオーラのように見える…</text><newline />
  <text width="264">喜んで…　いるのか…？</text>
  </hgscript>
-}

{- 
  so the object would have to be something like:
  data Chapter = Chapter {
          events :: [TextEvent]
  }

  data TextEvent = TextEvent {
          id                :: String,
          textBody          :: [String],
          textAttributes    :: TextConfig,
          textTriggers      :: [TextTrigger]
  } deriving(Show, Eq)
-}
--

flipRenderedFont :: (Eq a, IsString a) => a -> AppEvent
flipRenderedFont fs = case fs of
                        "Regular"  -> UpdateRenderedFont "JPN"
                        "JPN"      -> UpdateRenderedFont "Regular"
                        _          -> error "Invalid Font Requested (the requested font lacks a FontDef in AppConfig)"
                        
main :: IO ()
main = startApp model handleEvent buildUI config
  where
    config =
      [ appWindowTitle "Avarice (Pre-Alpha Build)",
        appFontDef "Bold"    "./assets/fonts/Cantarell/Cantarell-Bold.ttf",
        appFontDef "Regular" "./assets/fonts/Cantarell/Cantarell-Regular.ttf",
        appFontDef "JPN"     "./assets/fonts/Aosagi/YDWaosagi.otf",
        appWindowIcon "./assets/logo.png",
        appDisableAutoScale True,
        appWindowState (MainWindowNormal (1200, 1000))
      ]
    model =
      AppModel
        { theme = darkTheme,
          spiders = spiders,
          columns = AppColumn True <$ (gridColumns model),
          rowToScrollTo = 0,
          renderedFont  = "Regular"
        }
    spiders = S.fromFunction numSpiders spider
    spider i =
      Spider
        { index = i,
          species = "Acromantula",
          name = T.pack (printf "Son of Aragog %d" (i + 1)),
          dateOfBirth = addDays (fromIntegral i) (fromGregorian 1942 3 1),
          weightKilos = fromIntegral (numSpiders + 2 - i) * 2.3
        }
    numSpiders = 100

buildUI :: UIBuilder AppModel AppEvent
buildUI _wenv model = tree
  where
    tree =
      themeSwitch_ model.theme [themeClearBg] $
        vstack
          [ 
            grid `nodeKey` hagridKey,
            vstack_
              [childSpacing]
              (themeConfigurer : columnConfigurers <> actionButtons)
              `styleBasic` [padding 8, textFont (model ^. _renderedFont)]
          ] 
    grid =
      hagrid_
        [initialSort 1 SortDescending]
        (mconcat (zipWith column model.columns (gridColumns model)))
        model.spiders

    column (AppColumn enabled) columnDef =
      [columnDef | enabled]

    themeConfigurer =
      hstack_
        [childSpacing]
        [ labeledRadio_ "Dark Theme"  darkTheme  _theme [textRight],
          labeledRadio_ "Light Theme" lightTheme _theme [textRight]
        ]

    columnConfigurers =
      zipWith columnConfigurer [0 .. length model.columns - 1] (gridColumns model)

    columnConfigurer :: Int -> Column AppEvent Spider -> WidgetNode AppModel AppEvent
    columnConfigurer idx columnDef =
      labeledCheckbox_
        columnDef.name
        (_columns . singular (ix idx) . _enabled)
        [textRight]

    actionButtons =
      [ hstack_
          [childSpacing]
          [ label "Scroll to index of unsorted list" `styleBasic` [textFont (model ^. _renderedFont)],
            numericField _rowToScrollTo,
            button "Go!" ScrollToOriginalIndex
          ],
        hstack_
          [childSpacing]
          [button "Add entry" AddSpider, button "Display Source Text Only" (flipRenderedFont (model ^. _renderedFont)), button "Display Translated Text Only" (flipRenderedFont (model ^. _renderedFont)), label "TESTING: 血粉が…"   `styleBasic` [textFont (model ^. _renderedFont)], label "(SRC: EN/JP)"   `styleBasic` [textFont (model ^. _renderedFont)]]
      ]

handleEvent :: EventHandler AppModel AppEvent sp ep
handleEvent _wenv _node model = \case
  FeedSpider idx
    | Just spdr <- S.lookup idx model.spiders ->
        [ Producer (const (putStrLn ("Feeding spider " <> T.unpack spdr.name))),
          Model model {spiders = S.update idx spdr {weightKilos = spdr.weightKilos + 1} model.spiders}
        ]
  FeedSpider _ ->
    []
  AddSpider ->
    [Model model {spiders = model.spiders :|> newSpider model}]
  NameColumnResized colWidth ->
    [Producer (const (putStrLn ("Name column was resized: " <> show colWidth)))]
  NameColumnSorted direction ->
    [Producer (const (putStrLn ("Name column was sorted: " <> show direction)))]
  ScrollToOriginalIndex ->
    [scrollToRow (WidgetKey hagridKey) (rowScrollIndex model)]
  UpdateRenderedFont s -> [Model model {renderedFont = s}]

newSpider :: AppModel -> Spider
newSpider model =
  Spider
    { index = fromIntegral (length model.spiders),
      name = "Extra Spider " <> pack (show (length model.spiders)),
      species = "Spider plant",
      dateOfBirth = fromGregorian 2022 6 26,
      weightKilos = 0.01
    }

rowScrollIndex :: AppModel -> Seq (Spider, Int) -> Maybe Int
rowScrollIndex model items =
  snd <$> S.lookup model.rowToScrollTo items

gridColumns :: AppModel -> [Column AppEvent Spider]
gridColumns model = cols
  where
    cols =
      [ (showOrdColumn "Index" (.index))
          { initialWidth = 120,
            align = ColumnAlignRight,
            footerWidget = CustomFooterWidget (countFooter model)
          },
        (textColumn "Text" (.name))
          { initialWidth = 300,
            resizeHandler = Just NameColumnResized,
            sortHandler = Just NameColumnSorted
          },
        (textColumn "Text Type" (.species))
          { initialWidth = 200
          },
        (textColumn "Chapter" (T.pack . formatTime defaultTimeLocale "%Y-%m-%d" . (.dateOfBirth)))
          { initialWidth = 200
          },
        (textColumn "Width" (T.pack . printf "%.2f" . (.weightKilos)))
          { sortKey = SortWith (.weightKilos),
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

    countFooter model spiders =
      labelledFooter model"Number of Loaded Scripts: " (T.pack . show . length $ spiders)

    sumWeightFooter model spiders = tree
      where
        tree = labelledFooter model "Sum" (T.pack (printf "%.2f" totalWeightKilos))
        totalWeightKilos = sum ((.weightKilos) . fst <$> spiders)

    labelledFooter model labelText text =
      hstack
        [ label labelText `styleBasic` [textFont (model ^. _renderedFont)],
          filler,
          label text
            `styleBasic` [textFont (model ^. _renderedFont)] --`styleBasic` [textFont "Bold"]
        ]
        `styleBasic` [padding 10]

    actionsColumn :: Int -> Spider -> WidgetNode s AppEvent
    actionsColumn idx _spdr =
      button "Edit" (FeedSpider idx)
    
hagridKey :: Text
hagridKey = "SpiderHagrid"
