const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
[0] = LAYOUT_ergodox(TG(2),KC_1,KC_2,KC_3,KC_4,KC_5,DYN_MACRO_PLAY1,KC_TAB,KC_Q,KC_W,KC_E,KC_R,KC_T,LGUI(KC_F1),KC_LCTRL,KC_A,KC_S,KC_D,LT(6,KC_F),LT(2,KC_G),KC_LSHIFT,KC_Z,KC_X,KC_C,KC_V,KC_B,LGUI(KC_F2),LCTL(KC_LSHIFT),KC_NO,KC_UNDS,LT(4,KC_GRAVE),ALT_T(KC_QUOTE),LCTL(KC_X),LCTL(KC_C),LCTL(KC_V),LT(5,KC_SPACE),GUI_T(KC_ESCAPE),TT(3),DYN_MACRO_PLAY1,KC_6,KC_7,KC_8,KC_9,KC_0,KC_BSPACE,LGUI(KC_P),KC_Y,KC_U,KC_I,KC_O,KC_P,KC_BSLASH,KC_H,KC_J,KC_K,KC_L,LT(2,KC_SCOLON),RCTL_T(KC_ENTER),TG(1),KC_N,KC_M,KC_COMMA,KC_DOT,KC_SLASH,KC_RSHIFT,ALGR_T(KC_BSPACE),LT(4,KC_EQUAL),KC_MINUS,DYN_REC_STOP,KC_APPLICATION,LALT(KC_LEFT),LALT(KC_RIGHT),KC_PGUP,KC_PGDOWN,GUI_T(KC_LBRACKET),LT(5,KC_RBRACKET)),

[1] = LAYOUT_ergodox(KC_GRAVE,KC_1,KC_2,KC_3,KC_4,KC_5,KC_6,KC_TAB,KC_Q,KC_W,KC_E,KC_R,KC_T,KC_NO,KC_LCTRL,KC_A,KC_S,KC_D,KC_F,KC_G,KC_LSHIFT,KC_Z,KC_X,KC_C,KC_V,KC_B,KC_ESCAPE,KC_DQUO,KC_M,KC_I,KC_H,KC_LALT,LCTL(KC_X),LCTL(KC_C),LCTL(KC_V),KC_SPACE,KC_SPACE,KC_NO,KC_NO,KC_6,KC_7,KC_8,KC_9,KC_0,KC_BSPACE,KC_NO,KC_Y,KC_U,KC_I,KC_O,KC_P,KC_BSLASH,KC_H,KC_J,KC_K,KC_L,LT(2,KC_SCOLON),RCTL_T(KC_ENTER),KC_TRANSPARENT,KC_N,KC_M,KC_COMMA,KC_DOT,KC_SLASH,KC_RSHIFT,ALGR_T(KC_BSPACE),LT(4,KC_EQUAL),KC_MINUS,DYN_REC_STOP,KC_APPLICATION,LALT(KC_LEFT),LALT(KC_RIGHT),KC_PGUP,KC_PGDOWN,GUI_T(KC_LBRACKET),LT(5,KC_RBRACKET)),

[2] = LAYOUT_ergodox(KC_TRANSPARENT,KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,KC_TRANSPARENT,KC_TAB,KC_MS_BTN1,KC_MS_UP,KC_MS_BTN2,KC_MS_WH_UP,KC_TRANSPARENT,KC_TRANSPARENT,KC_LCTRL,KC_MS_LEFT,KC_MS_DOWN,KC_MS_RIGHT,KC_MS_WH_DOWN,KC_NO,KC_LSHIFT,KC_MS_BTN4,KC_MS_BTN3,KC_MS_BTN5,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,KC_TRANSPARENT,TO(4),KC_LALT,LCTL(KC_X),LCTL(KC_C),LCTL(KC_V),LT(5,KC_SPACE),TO(0),MO(3),KC_NO,KC_TRANSPARENT,KC_NUMLOCK,KC_KP_ASTERISK,KC_KP_SLASH,KC_KP_MINUS,KC_BSPACE,KC_NO,KC_TRANSPARENT,KC_KP_7,KC_KP_8,KC_KP_9,KC_KP_PLUS,KC_BSLASH,KC_TRANSPARENT,KC_KP_4,KC_KP_5,KC_KP_6,KC_TRANSPARENT,RCTL_T(KC_ENTER),TO(1),KC_TRANSPARENT,KC_KP_1,KC_KP_2,KC_KP_3,KC_TRANSPARENT,KC_RSHIFT,KC_KP_0,KC_KP_DOT,KC_KP_COMMA,KC_TRANSPARENT,KC_APPLICATION,KC_MEDIA_PREV_TRACK,KC_MEDIA_NEXT_TRACK,KC_AUDIO_VOL_UP,KC_AUDIO_VOL_DOWN,KC_AUDIO_MUTE,KC_MEDIA_PLAY_PAUSE),

[3] = LAYOUT_ergodox(TO(2),KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,DYN_MACRO_PLAY1,LGUI(KC_TAB),LGUI(KC_Q),LGUI(KC_W),LGUI(KC_E),KC_NO,LGUI(KC_T),LGUI(KC_F1),KC_LCTRL,LGUI(KC_1),LGUI(KC_2),LGUI(KC_3),LGUI(KC_4),LGUI(KC_5),KC_LSHIFT,KC_NO,KC_NO,LGUI(KC_C),LGUI(KC_RBRACKET),LGUI(KC_B),LGUI(KC_F2),KC_NO,KC_NO,KC_NO,KC_NO,KC_LALT,KC_NO,KC_NO,KC_NO,LT(5,KC_SPACE),TO(0),KC_TRANSPARENT,KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,TO(2),LGUI(KC_F4),KC_NO,KC_NO,LGUI(KC_I),KC_NO,KC_NO,KC_TAB,LGUI(KC_6),LGUI(KC_7),LGUI(KC_8),LGUI(KC_9),LGUI(KC_0),KC_LCTRL,LGUI(KC_F3),KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,KC_LSHIFT,LGUI(KC_SPACE),KC_NO,KC_NO,KC_NO,KC_NO,LGUI(KC_LEFT),LGUI(KC_RIGHT),LGUI(KC_UP),LGUI(KC_DOWN),LGUI(KC_B),LGUI(KC_T)),

[4] = LAYOUT_ergodox(TO(2),KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,DYN_REC_START1,KC_TAB,KC_EXLM,KC_AT,KC_HASH,KC_DLR,KC_PERC,KC_NO,KC_LCTRL,KC_1,KC_2,KC_3,KC_4,KC_5,KC_LSHIFT,KC_LPRN,KC_RPRN,KC_LBRACKET,KC_RBRACKET,KC_QUOTE,KC_NO,KC_NO,KC_NO,KC_NO,KC_TRANSPARENT,KC_NO,KC_NO,KC_NO,KC_NO,LT(5,KC_SPACE),TO(0),KC_TRANSPARENT,DYN_REC_START2,KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,KC_BSPACE,KC_NO,KC_CIRC,KC_AMPR,KC_ASTERISK,KC_GRAVE,KC_TILD,KC_BSLASH,KC_6,KC_7,KC_8,KC_9,KC_0,RCTL_T(KC_ENTER),KC_NO,KC_DQUO,KC_LCBR,KC_RCBR,KC_LABK,KC_RABK,KC_RSHIFT,KC_BSPACE,KC_NO,KC_NO,KC_NO,KC_NO,LALT(KC_LEFT),LALT(KC_RIGHT),KC_PGUP,KC_PGDOWN,GUI_T(KC_LBRACKET),LT(5,KC_RBRACKET)),

[5] = LAYOUT_ergodox(KC_NO,KC_F1,KC_F2,KC_F3,KC_F4,KC_F5,KC_F6,KC_TAB,KC_NO,LCTL(KC_LEFT),LCTL(KC_RIGHT),KC_NO,KC_NO,KC_NO,KC_LCTRL,KC_HOME,KC_NO,LCTL(KC_X),KC_END,KC_NO,KC_LSHIFT,KC_NO,KC_DELETE,KC_NO,KC_NO,LCTL(KC_LEFT),KC_NO,KC_NO,KC_NO,KC_NO,KC_NO,KC_LALT,KC_NO,KC_NO,KC_NO,LT(5,KC_SPACE),TO(0),KC_TRANSPARENT,KC_F7,KC_F8,KC_F9,KC_F10,KC_F11,KC_F12,KC_BSPACE,KC_NO,LCTL(KC_C),LCTL(KC_Z),KC_NO,KC_NO,LCTL(KC_V),KC_BSLASH,KC_LEFT,KC_DOWN,KC_UP,KC_RIGHT,KC_NO,RCTL_T(KC_ENTER),KC_NO,KC_F3,KC_PGUP,LALT(KC_LEFT),LALT(KC_RIGHT),KC_F3,KC_RSHIFT,KC_PGDN,KC_NO,KC_NO,KC_NO,KC_NO,LALT(KC_LEFT),LALT(KC_RIGHT),KC_PGUP,KC_PGDOWN,GUI_T(KC_LBRACKET),KC_TRANSPARENT),

[6] = LAYOUT_ergodox(TG(2),KC_1,KC_2,KC_3,KC_4,KC_5,DYN_MACRO_PLAY1,KC_TAB,KC_Q,KC_W,KC_E,KC_R,KC_T,LGUI(KC_F1),KC_LCTRL,KC_A,KC_S,KC_D,LT(6,KC_F),LT(2,KC_G),KC_LSHIFT,KC_Z,KC_X,KC_C,KC_V,KC_B,LGUI(KC_F2),LCTL(KC_LSHIFT),KC_NO,KC_UNDS,LT(4,KC_GRAVE),ALT_T(KC_QUOTE),LCTL(KC_X),LCTL(KC_C),LCTL(KC_V),LT(5,KC_SPACE),GUI_T(KC_ESCAPE),TT(3),DYN_MACRO_PLAY1,KC_6,KC_7,KC_8,KC_9,KC_0,KC_BSPACE,LGUI(KC_P),KC_Y,KC_U,KC_I,KC_O,KC_P,KC_BSLASH,KC_H,KC_J,KC_K,KC_L,LT(2,KC_SCOLON),RCTL_T(KC_ENTER),TG(1),KC_N,KC_M,KC_COMMA,KC_DOT,KC_SLASH,KC_RSHIFT,ALGR_T(KC_BSPACE),LT(4,KC_EQUAL),KC_MINUS,DYN_REC_STOP,KC_APPLICATION,LALT(KC_LEFT),LALT(KC_RIGHT),KC_PGUP,KC_PGDOWN,GUI_T(KC_LBRACKET),LT(5,KC_RBRACKET)),


};
