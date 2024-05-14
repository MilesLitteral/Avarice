module Avarice.Data.Script  (HGScriptRaw(..), TextNewLine(..), TextEvent(..)) where 

newtype HGScriptRaw     = HGScript       { events            :: [TextEvent]  } deriving(Show, Eq)
newtype TextNewLine     = TextNewLine    { newlineContent    :: String       } deriving(Show,  Eq)
newtype TextUnknownTag  = TextUnknownTag { unknownTagContent :: String       }  deriving(Show, Eq)

data    TextEvent       = TextEvent   {
        id                :: String,
        textBody          :: [String]
        --textAttributes  :: TextConfig,
        --textTriggers    :: [TextTrigger]
  } deriving(Show, Eq)


-- Known triggers:
-- unknown_0 = 0
-- text = 1
-- newline = 2
-- change_color = 3
-- placeholder = 4
-- screen_wipe = 5
-- sleep = 6
-- play_sound = 7
-- unknown_11 = 11
-- unknown_tag = 12
-- unknown_14 = 14

-- Known Attributes:
-- width


