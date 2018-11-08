#!/usr/bin/env stack
-- stack --resolver lts-12.16 --no-nix-pure script
{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Development.Shake
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Prelude hiding (Ordering(..))

myKeyboard = KeyBoard
  [ Layer INSERT insertLeft insertRight
  , Layer GAME gameLeft gameRight
  , Layer MOUSE mouseLeft mouseRight
  , Layer XMONAD insertLeft insertRight
  , Layer NUMPAREN insertLeft insertRight
  , Layer NORMAL insertLeft insertRight
  , Layer FUNCTION insertLeft insertRight
  ]

--  [4] = LAYOUT_ergodox(KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_NO,KC_TAB,KC_EXLM,KC_AT,KC_HASH,KC_DLR,KC_PERC,KC_TRANSPARENT,OSM(MOD_LCTL),KC_1,KC_2,KC_3,KC_4,KC_5,KC_LSHIFT,KC_LPRN,KC_RPRN,KC_LBRACKET,KC_RBRACKET,KC_QUOTE,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_SPACE,TO(0),KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_CIRC,KC_AMPR,KC_KP_ASTERISK,KC_GRAVE,KC_TILD,KC_PIPE,KC_6,KC_7,KC_8,KC_9,KC_0,KC_ENTER,KC_TRANSPARENT,KC_DQUO,KC_LCBR,KC_RCBR,KC_LABK,KC_RABK,KC_RSHIFT,KC_BSPACE,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,TO(1),KC_SPACE),
--
--  [5] = LAYOUT_ergodox(KC_TRANSPARENT,KC_F1,KC_F2,KC_F3,KC_F4,KC_F5,KC_F6,KC_TAB,KC_TRANSPARENT,LCTL(KC_RIGHT),LCTL(KC_RIGHT),KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_LCTRL,KC_HOME,KC_TRANSPARENT,LCTL(KC_X),KC_END,KC_TRANSPARENT,KC_LSHIFT,KC_TRANSPARENT,KC_DELETE,KC_TRANSPARENT,KC_TRANSPARENT,LCTL(KC_LEFT),KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_LALT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,GUI_T(KC_SPACE),KC_TRANSPARENT,KC_TRANSPARENT,KC_F7,KC_F8,KC_F9,KC_F10,KC_F11,KC_F12,KC_TRANSPARENT,KC_TRANSPARENT,LCTL(KC_C),LCTL(KC_Z),KC_TRANSPARENT,KC_TRANSPARENT,LCTL(KC_V),KC_TRANSPARENT,KC_LEFT,KC_DOWN,KC_UP,KC_RIGHT,KC_TRANSPARENT,RCTL_T(KC_ENTER),KC_TRANSPARENT,KC_F3,KC_PGUP,LALT(KC_LEFT),LALT(KC_RIGHT),KC_F3,KC_RSHIFT,KC_PGDOWN,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,GUI_T(KC_SPACE)),
--
--  [6] = LAYOUT_ergodox(KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,TO(0),KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_F9,KC_F10,KC_F11,KC_F12,KC_TRANSPARENT,KC_TRANSPARENT,KC_F5,KC_F6,KC_F7,KC_F8,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_F1,KC_F2,KC_F3,KC_F4,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT),

no = Row NO NO NO NO NO
oneToFive = Row "1" "2" "3" "4" "5"
sixToZero = Row "6" "7" "8" "9" "0"
qToT = Row "Q" "W" "E" "R" "T"
aToG = Row "A" "S" "D" "F" "G"
yToP = Row "Y" "U" "I" "O" "P"
nToSlash = Row "N" "M" "COMMA" "DOT" "SLASH"
zToB = Row "Z" "X" "C" "V" "B"

insertLeft = Side
  { wide = Wide (TG MOUSE) "TAB" "LCTRL" "LSPO"
  , inner = Inner "MS_BTN1" (Raw "LGUI(KC_F1)") (Raw "LGUI(KC_F2)")
  , thumbKeys = ThumbLeft       (LCTL "X")           (LCTL "C")
                                                     (LCTL "V")
                (GUI_T "SPACE") (LT NORMAL "ESCAPE") (TT XMONAD)
  , rows = Rows oneToFive
                qToT
                (Row "A" "S" "D" (LT FUNCTION "F") (LT MOUSE "G"))
                zToB
                (Row (Raw "LCTL(KC_LSHIFT)") NO "UNDS" (LT NUMPAREN "GRAVE") (Raw "ALT_T(KC_QUOTE)"))
  }

insertRight = Side
  { wide = Wide "BSPACE" "BSLASH" (Raw "RCTL_T(KC_ENTER)") "RSPC"
  , inner = Inner NO (LGUI "P") (TG GAME)
  , thumbKeys = ThumbRight (LALT "LEFT") (LALT "RIGHT")
                           "PGUP"
                           "PGDOWN"      (LT NORMAL "LBRACKET") (GUI_T "RBRACKET")
  , rows = Rows sixToZero
                yToP
                (Row "H" "J" "K" "L" (LT MOUSE "SCOLON"))
                nToSlash
                (Row (Raw "ALGR_T(KC_BSPACE)") (LT NUMPAREN "EQUAL") "MINUS" NO "APPLICATION")
  }

gameLeft = Side
  { wide = Wide "GRAVE" "TAB" "LCTRL" "LSHIFT"
  , inner = Inner "6" NO "ESCAPE"
  , thumbKeys = ThumbLeft (LCTL "X") (LCTL "C")
                                     (LCTL "V")
                "SPACE"   "SPACE"    NO
  , rows = Rows oneToFive
                qToT
                aToG
                zToB
                (Row "DQUO" "M" "I" "H" "LALT")
  }

gameRight = insertRight
  { inner = Inner NO NO TP
  }

mouseLeft = Side
  { wide = Wide TP TP TP "LSHIFT"
  , inner = Inner TP TP TP
  , thumbKeys = ThumbLeft               (LCTL "X")  (LCTL "C")
                                                    (LCTL "V")
                (Raw "GUI_T(KC_SPACE)") (TO INSERT) (MO XMONAD)
  , rows = Rows no
                (Row "MS_BTN1" "MS_UP" "MS_BTN2" "MS_WH_UP" TP)
                (Row "LCTRL" "MS_LEFT" "MS_DOWN" "MS_RIGHT" "MS_WH_DOWN")
                (Row "MS_BTN4" "MS_BTN3" "MS_BTN5" TP TP)
                (Row TP TP TP (TO NUMPAREN) "LALT")
  }

mouseRight = Side
  { wide = Wide "BSPACE" "BSLASH" (Raw "RCTL_T(KC_ENTER)") "RSHIFT"
  , inner = Inner NO NO (TO GAME)
  , thumbKeys = ThumbRight "MEDIA_PREV_TRACK" "MEDIA_NEXT_TRACK"
                           "AUDIO_VOL_UP"
                           "AUDIO_VOL_DOWN"   "AUDIO_MUTE"       "MEDIA_PLAY_PAUSE"
  , rows = Rows (Row TP "NUMLOCK" "KP_ASTERISK" "KP_SLASH" "KP_MINUS")
                (Row TP "KP_7" "KP_8" "KP_9" "KP_PLUS")
                (Row TP "KP_4" "KP_5" "KP_6" TP)
                (Row TP "KP_1" "KP_2" "KP_3" TP)
                (Row "KP_0" "KP_DOT" "KP_COMMA" TP "APPLICATION")
  }

xmonadLeft = Side
  { wide = Wide (TO MOUSE) (LGUI "Tab") "LCTRL" "LSHIFT"
  , inner = inner insertLeft
  , thumbKeys = ThumbLeft      NO          NO
                                           NO
                (LGUI "SPACE") (TO INSERT) TP
  , rows = Rows no
                (Row (LGUI "Q") (LGUI "W") (LGUI "E") NO (LGUI "T"))
                (Row (LGUI "1") (LGUI "2") (LGUI "3") (LGUI "4") (LGUI "5"))
                (Row NO NO (LGUI "C") (LGUI "RBRACKET") (LGUI "B"))
                (Row NO NO NO NO "LALT")
  }

xmonadRight = Side
  { wide = wide insertRight
  , inner = Inner NO (LGUI "F4") (LGUI "F3")
  , thumbKeys = ThumbRight (LGUI "LEFT") (LGUI "RIGHT")
                           (LGUI "UP")
                           (LGUI "DOWN") (LGUI "B") (LGUI "T")
  , rows = Rows no
                (Row NO NO (LGUI "I") NO NO)
                (Row (LGUI "6") (LGUI "7") (LGUI "8") (LGUI "9") (LGUI "0"))
                no
                (Row (LGUI "SPACE") NO NO NO NO)
  }


data KeyBoard = KeyBoard [Layer]

data LayerId
  = INSERT
  | MOUSE
  | GAME
  | XMONAD
  | NUMPAREN
  | NORMAL
  | FUNCTION
  deriving (Eq, Ord)

data K = Raw String
       | KeyCode String
       | NO
       | TP
       | TG LayerId
       | TT LayerId
       | TO LayerId
       | MO LayerId
       | LT LayerId K
       | LCTL K
       | LGUI K
       | LALT K
       | GUI_T K

instance IsString K where
  fromString = KeyCode

toKeyCode :: Map LayerId String -> K -> String
toKeyCode layerIds k =
  let lid l = layerIds Map.! l
      f n as = n ++ "(" ++ List.intercalate "," as ++ ")"
      r = toKeyCode layerIds
  in case k of
    Raw raw  -> raw
    KeyCode c -> "KC_" ++ c
    NO       -> "KC_NO"
    TP       -> "KC_TRANSPARENT"
    TG l     -> f "TG" [lid l]
    TO l     -> f "TO" [lid l]
    MO l     -> f "MO" [lid l]
    TT l     -> f "TT" [lid l]
    LT l k' -> f "LT" [lid l, r k']
    LCTL k'  -> f "LCTL" [r k']
    LGUI k'  -> f "LGUI" [r k']
    LALT k'  -> f "LALT" [r k']
    GUI_T k'  -> f "GUI_T" [r k']


class Keymap part where
  gen :: part -> [K]

data Layer = Layer
  { layerId :: LayerId
  , left    :: Side
  , right   :: Side
  }

instance Keymap Layer where
  gen (Layer _ l r) = gen l ++ gen r

data Wide = Wide K K K K

data Inner = Inner K K K

data Row = Row K K K K K

instance Keymap Row where
  gen (Row r1 r2 r3 r4 r5) = [r1,r2,r3,r4,r5]

data Rows = Rows Row Row Row Row Row

data ThumbKeys
  --   1 2
  -- 4 5 3
  -- 4 5 6
  = ThumbLeft K K K K K K
  -- 1 2
  -- 3 5 6
  -- 4 5 6
  | ThumbRight K K K K K K

instance Keymap ThumbKeys where
  gen (ThumbLeft k1 k2 k3 k4 k5 k6) = [k1,k2,k3,k4,k5,k6]
  gen (ThumbRight k1 k2 k3 k4 k5 k6) = [k1,k2,k3,k4,k5,k6]

data Side = Side
  { wide      :: Wide
  , inner     :: Inner
  , thumbKeys :: ThumbKeys
  , rows      :: Rows
  }

instance Keymap Side where
  gen (Side (Wide w1 w2 w3 w4) (Inner i1 i2 i3) thumbKeys (Rows r1 r2 r3 r4 r5)) =
    let m = maybe [] return
        mkRow mw r mi = case thumbKeys of
          ThumbLeft{}  -> m mw ++ r ++ m mi
          ThumbRight{} -> m mi ++ r ++ m mw
        fingers = concat $ zipWith3 mkRow
                    [Just w1, Just w2, Just w3, Just w4, Nothing]
                    (map gen [r1,r2,r3,r4,r5])
                    [Just i1, Just i2, Nothing, Just i3, Nothing]
    in fingers ++ gen thumbKeys


main = shakeArgs shakeOptions $ do
  let compiledWithMake = ".build/ergodox_ez_saep.hex"
      generated        = "./keyboards/ergodox_ez/keymaps/saep/saep_generated.c"

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
    cmd ("make" :: String) ["ergodox_ez:saep" :: String]

generateMatrix (KeyBoard layers) =
    [ "const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {"
    , foldl go "" layers
    , "};"
    ]
  where
    layerIds = Map.fromList $ zip (map layerId layers) (map show [0..])
    go :: String -> Layer -> String
    go ls l =
      ls ++ "[" ++ (layerIds Map.! layerId l) ++ "] = LAYOUT_ergodox("
      ++ List.intercalate "," (map (toKeyCode layerIds) (gen l))
      ++ "),\n\n"

