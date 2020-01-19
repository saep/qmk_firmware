#!/usr/bin/env stack
-- stack --resolver lts-14.20 --no-nix-pure script --package shake --package rio
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


import Data.String
import Development.Shake
import RIO hiding (Ordering (..))
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Map as Map

-- teensy-loader-cli -w --mcu=atmega32u4 saep.hex

myKeyboard =
  KeyBoard
    [ Layer INSERT insertLeft insertRight,
      Layer GAME gameLeft gameRight,
      Layer MOUSE mouseLeft mouseRight,
      Layer NUMPAREN numParenLeft numParenRight,
      Layer NORMAL normalLeft normalRight,
      Layer FUNCTION functionLeft functionRight
    ]

no = Row NO NO NO NO NO

oneToFive = Row "1" "2" "3" "4" "5"

sixToZero = Row "6" "7" "8" "9" "0"

qToT = Row "Q" "W" "E" "R" "T"

aToG = Row "A" "S" "D" "F" "G"

yToP = Row "Y" "U" "I" "O" "P"

nToSlash = Row "N" "M" "COMMA" "DOT" "SLASH"

zToB = Row "Z" "X" "C" "V" "B"

f1tof5 = Row "F1" "F2" "F3" "F4" "F5"

f8tof12 = Row "F8" "F9" "F10" "F11" "F12"

insertLeft = Side
  { wide = Wide (TG MOUSE) "TAB" "LCTRL" "LSHIFT",
    inner = Inner (Raw "DYN_MACRO_PLAY1") (Raw "LGUI(KC_F1)") (Raw "LGUI(KC_F2)"),
    thumbKeys =
      ThumbLeft
        (LCTL "X")
        (LCTL "C")
        (LCTL "V")
        (LT NORMAL "SPACE")
        (GUI_T "ESCAPE")
        (LGUI "A"),
    rows =
      Rows
        oneToFive
        qToT
        (Row "A" "S" "D" (LT FUNCTION "F") (LT MOUSE "G"))
        zToB
        (Row (Raw "LCTL(KC_LSHIFT)") NO "UNDS" (MO NUMPAREN) "LALT")
  }

insertRight = Side
  { wide = Wide "BSPACE" "BSLASH" (Raw "RCTL_T(KC_ENTER)") "RSHIFT",
    inner = Inner (Raw "DYN_MACRO_PLAY2") (LGUI "P") (TG GAME),
    thumbKeys =
      ThumbRight
        (LALT "LEFT")
        (LALT "RIGHT")
        "PGUP"
        "PGDOWN"
        (GUI_T "LBRACKET")
        (LT NORMAL "RBRACKET"),
    rows =
      Rows
        sixToZero
        yToP
        (Row "H" "J" "K" "L" (LT MOUSE "SCOLON"))
        nToSlash
        (Row (Raw "ALGR_T(KC_BSPACE)") (MO NUMPAREN) (LT FUNCTION "MINUS") (Raw "DYN_REC_STOP") "APPLICATION")
  }

gameLeft = Side
  { wide = Wide "GRAVE" "TAB" "LCTRL" "LSHIFT",
    inner = Inner "6" NO "ESCAPE",
    thumbKeys =
      ThumbLeft
        (LCTL "X")
        (LCTL "C")
        (LCTL "V")
        "SPACE"
        "SPACE"
        NO,
    rows =
      Rows
        oneToFive
        qToT
        aToG
        zToB
        (Row "DQUO" "M" "I" "H" "LALT")
  }

gameRight =
  insertRight
    { inner = Inner NO NO TP
    }

mouseLeft = Side
  { wide = Wide TP "TAB" "LCTRL" "LSHIFT",
    inner = Inner TP TP TP,
    thumbKeys =
      ThumbLeft
        (LCTL "X")
        (LCTL "C")
        (LCTL "V")
        (LT NORMAL "SPACE")
        (TO INSERT)
        (LGUI "A"),
    rows =
      Rows
        no
        (Row "MS_BTN1" "MS_UP" "MS_BTN2" "MS_WH_UP" TP)
        (Row "MS_LEFT" "MS_DOWN" "MS_RIGHT" "MS_WH_DOWN" NO)
        (Row "MS_BTN4" "MS_BTN3" "MS_BTN5" TP TP)
        (Row TP TP TP (TO NUMPAREN) "LALT")
  }

mouseRight = Side
  { wide = Wide "BSPACE" "BSLASH" (Raw "RCTL_T(KC_ENTER)") "RSHIFT",
    inner = Inner NO NO (TO GAME),
    thumbKeys =
      ThumbRight
        "MEDIA_PREV_TRACK"
        "MEDIA_NEXT_TRACK"
        "AUDIO_VOL_UP"
        "AUDIO_VOL_DOWN"
        "AUDIO_MUTE"
        "MEDIA_PLAY_PAUSE",
    rows =
      Rows
        (Row TP "NUMLOCK" "KP_ASTERISK" "KP_SLASH" "KP_MINUS")
        (Row TP "KP_7" "KP_8" "KP_9" "KP_PLUS")
        (Row TP "KP_4" "KP_5" "KP_6" TP)
        (Row TP "KP_1" "KP_2" "KP_3" TP)
        (Row "KP_0" "KP_DOT" "KP_COMMA" TP "APPLICATION")
  }

numParenLeft = Side
  { wide = Wide (TO MOUSE) "TAB" "LCTRL" "LSHIFT",
    inner = Inner (Raw "DYN_REC_START1") NO NO,
    thumbKeys =
      ThumbLeft
        NO
        NO
        NO
        (LT NORMAL "SPACE")
        (TO INSERT)
        TP,
    rows =
      Rows
        no
        (Row "EXLM" "AT" "HASH" "DLR" "PERC")
        oneToFive
        (Row "MINUS" "PLUS" "GRAVE" "DQUO" "UNDS")
        (Row NO NO NO TP NO)
  }

numParenRight = Side
  { wide = wide insertRight,
    inner = Inner (Raw "DYN_REC_START2") NO NO,
    thumbKeys =
      ThumbRight
        TP
        TP
        "PGUP"
        "PGDN"
        "LPRN"
        "RPRN",
    rows =
      Rows
        no
        (Row "CIRC" "AMPR" "ASTERISK" "GRAVE" "TILD")
        sixToZero
        (Row NO "QUOTE" "EQUAL" "LT" "GT")
        (Row "BSPACE" NO NO NO NO)
  }

normalLeft = Side
  { wide = Wide NO "TAB" "LCTRL" "LSHIFT",
    inner = Inner "F6" NO NO,
    thumbKeys = thumbKeys numParenLeft,
    rows =
      Rows
        f1tof5
        (Row NO (LCTL "LEFT") (LCTL "RIGHT") NO NO)
        (Row "HOME" NO (LCTL "X") "END" NO)
        (Row NO "DELETE" NO NO (LCTL "LEFT"))
        (Row NO NO NO NO "LALT")
  }

normalRight = Side
  { wide = wide insertRight,
    inner = Inner "F7" NO NO,
    thumbKeys =
      ThumbRight
        (LALT "LEFT")
        (LALT "RIGHT")
        "PGUP"
        "PGDOWN"
        (GUI_T "LBRACKET")
        TP,
    rows =
      Rows
        f8tof12
        (Row (LCTL "C") (LCTL "Z") NO NO (LCTL "V"))
        (Row "LEFT" "DOWN" "UP" "RIGHT" NO)
        (Row "F3" "PGUP" (LALT "LEFT") (LALT "RIGHT") "F3")
        (Row "PGDN" NO NO NO NO)
  }

functionLeft =
  let t = mapRow (const TP)
   in numParenLeft
        { rows = mapRows t t t t t (rows numParenLeft)
        }

functionRight =
  numParenRight
    { rows =
        Rows
          no
          (Row NO "F9" "F10" "F11" "F12")
          (Row NO "F5" "F6" "F7" "F8")
          (Row NO "F1" "F2" "F3" "F4")
          no
    }

data KeyBoard = KeyBoard [Layer]

data LayerId
  = INSERT
  | MOUSE
  | GAME
  | NUMPAREN
  | NORMAL
  | FUNCTION
  deriving (Eq, Ord)

data K
  = Raw String
  | KeyCode String
  | NO
  | TP
  | -- | Toggle
    TG LayerId
  | -- | Toggle or hold
    TT LayerId
  | -- | switch
    TO LayerId
  | -- | hold
    MO LayerId
  | -- | hold layer - tap K
    LT LayerId K
  | LCTL K
  | LGUI K
  | LALT K
  | GUI_T K

instance IsString K where
  fromString = KeyCode

(!) m k = maybe (error "!") id $ m Map.!? k

toKeyCode :: Map LayerId String -> K -> String
toKeyCode layerIds k =
  let lid l = layerIds ! l
      f n as = n ++ "(" ++ List.intercalate "," as ++ ")"
      r = toKeyCode layerIds
   in case k of
        Raw raw -> raw
        KeyCode c -> "KC_" ++ c
        NO -> "KC_NO"
        TP -> "KC_TRANSPARENT"
        TG l -> f "TG" [lid l]
        TO l -> f "TO" [lid l]
        MO l -> f "MO" [lid l]
        TT l -> f "TT" [lid l]
        LT l k' -> f "LT" [lid l, r k']
        LCTL k' -> f "LCTL" [r k']
        LGUI k' -> f "LGUI" [r k']
        LALT k' -> f "LALT" [r k']
        GUI_T k' -> f "GUI_T" [r k']

class Keymap part where
  gen :: part -> [K]

data Layer
  = Layer
      { layerId :: LayerId,
        left :: Side,
        right :: Side
      }

instance Keymap Layer where
  gen (Layer _ l r) = gen l ++ gen r

data Wide = Wide K K K K

data Inner = Inner K K K

data Row = Row K K K K K

instance Keymap Row where
  gen (Row r1 r2 r3 r4 r5) = [r1, r2, r3, r4, r5]

mapRow :: (K -> K) -> Row -> Row
mapRow f (Row k1 k2 k3 k4 k5) = Row (f k1) (f k2) (f k3) (f k4) (f k5)

mapRows :: (Row -> Row) -> (Row -> Row) -> (Row -> Row) -> (Row -> Row) -> (Row -> Row) -> Rows -> Rows
mapRows f1 f2 f3 f4 f5 (Rows r1 r2 r3 r4 r5) = Rows (f1 r1) (f2 r2) (f3 r3) (f4 r4) (f5 r5)

data Rows = Rows Row Row Row Row Row

data ThumbKeys
  = --   1 2
    -- 4 5 3
    -- 4 5 6
    ThumbLeft K K K K K K
  | -- 1 2
    -- 3 5 6
    -- 4 5 6
    ThumbRight K K K K K K

instance Keymap ThumbKeys where
  gen (ThumbLeft k1 k2 k3 k4 k5 k6) = [k1, k2, k3, k4, k5, k6]
  gen (ThumbRight k1 k2 k3 k4 k5 k6) = [k1, k2, k3, k4, k5, k6]

data Side
  = Side
      { wide :: Wide,
        inner :: Inner,
        thumbKeys :: ThumbKeys,
        rows :: Rows
      }

instance Keymap Side where
  gen (Side (Wide w1 w2 w3 w4) (Inner i1 i2 i3) thumbKeys (Rows r1 r2 r3 r4 r5)) =
    let m = maybe [] return
        mkRow mw r mi = case thumbKeys of
          ThumbLeft {} -> m mw ++ r ++ m mi
          ThumbRight {} -> m mi ++ r ++ m mw
        fingers =
          concat $
            List.zipWith3
              mkRow
              [Just w1, Just w2, Just w3, Just w4, Nothing]
              (map gen [r1, r2, r3, r4, r5])
              [Just i1, Just i2, Nothing, Just i3, Nothing]
     in fingers ++ gen thumbKeys

main = shakeArgs shakeOptions $ do
  let compiledWithMake = ".build/ergodox_ez_saep.hex"
      generated = "./keyboards/ergodox_ez/keymaps/saep/saep_generated.c"
  want ["saep.hex"]
  "saep.hex" %> \out -> do
    need [compiledWithMake]
    cmd ("cp -v" :: String) [compiledWithMake] [out]
  generated %> \out -> do
    Exit _ <- cmd ("rm -f" :: String) [generated]
    need ["saep.hs"]
    writeFileLines generated $ generateMatrix myKeyboard
  compiledWithMake %> \out -> do
    need [generated]
    cmd ("nix-shell" :: String) ["--command", "make ergodox_ez:saep" :: String]

generateMatrix (KeyBoard layers) =
  [ "const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {",
    foldl' go "" layers,
    "};"
  ]
  where
    layerIds = Map.fromList $ zip (map layerId layers) (map show [0 ..])
    go :: String -> Layer -> String
    go ls l =
      ls ++ "[" ++ (layerIds ! layerId l) ++ "] = LAYOUT_ergodox("
        ++ List.intercalate "," (map (toKeyCode layerIds) (gen l))
        ++ "),\n\n"
