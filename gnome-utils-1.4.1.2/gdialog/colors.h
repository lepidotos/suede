/*
 *  colors.h -- color attribute definitions
 *
 *  AUTHOR: Savio Lam (lam836@cs.cuhk.hk)
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/*
 *   Default color definitions
 *
 *   *_FG = foreground
 *   *_BG = background
 *   *_HL = highlight?
 */
#define SCREEN_FG                    COLOR_WHITE
#define SCREEN_BG                    COLOR_BLUE
#define SCREEN_HL                    FALSE

#define SHADOW_FG                    COLOR_WHITE
#define SHADOW_BG                    COLOR_BLACK
#define SHADOW_HL                    FALSE

#define DIALOG_FG                    COLOR_BLACK
#define DIALOG_BG                    COLOR_CYAN
#define DIALOG_HL                    FALSE

#define TITLE_FG                     COLOR_YELLOW
#define TITLE_BG                     COLOR_CYAN
#define TITLE_HL                     TRUE

#define BORDER_FG                    COLOR_CYAN
#define BORDER_BG                    COLOR_CYAN
#define BORDER_HL                    TRUE

#define BUTTON_ACTIVE_FG             COLOR_WHITE
#define BUTTON_ACTIVE_BG             COLOR_BLUE
#define BUTTON_ACTIVE_HL             TRUE

#define BUTTON_INACTIVE_FG           COLOR_BLACK
#define BUTTON_INACTIVE_BG           COLOR_CYAN
#define BUTTON_INACTIVE_HL           FALSE

#define BUTTON_KEY_ACTIVE_FG         COLOR_WHITE
#define BUTTON_KEY_ACTIVE_BG         COLOR_BLUE
#define BUTTON_KEY_ACTIVE_HL         TRUE

#define BUTTON_KEY_INACTIVE_FG       COLOR_BLACK
#define BUTTON_KEY_INACTIVE_BG       COLOR_CYAN
#define BUTTON_KEY_INACTIVE_HL       FALSE

#define BUTTON_LABEL_ACTIVE_FG       COLOR_WHITE
#define BUTTON_LABEL_ACTIVE_BG       COLOR_BLUE
#define BUTTON_LABEL_ACTIVE_HL       TRUE

#define BUTTON_LABEL_INACTIVE_FG     COLOR_BLACK
#define BUTTON_LABEL_INACTIVE_BG     COLOR_CYAN
#define BUTTON_LABEL_INACTIVE_HL     TRUE

#define INPUTBOX_FG                  COLOR_BLUE
#define INPUTBOX_BG                  COLOR_WHITE
#define INPUTBOX_HL                  FALSE

#define INPUTBOX_BORDER_FG           COLOR_CYAN
#define INPUTBOX_BORDER_BG           COLOR_CYAN
#define INPUTBOX_BORDER_HL           TRUE

#define SEARCHBOX_FG                 COLOR_YELLOW
#define SEARCHBOX_BG                 COLOR_WHITE
#define SEARCHBOX_HL                 TRUE

#define SEARCHBOX_TITLE_FG           COLOR_WHITE
#define SEARCHBOX_TITLE_BG           COLOR_WHITE
#define SEARCHBOX_TITLE_HL           TRUE

#define SEARCHBOX_BORDER_FG          COLOR_RED
#define SEARCHBOX_BORDER_BG          COLOR_WHITE
#define SEARCHBOX_BORDER_HL          FALSE

#define POSITION_INDICATOR_FG        COLOR_RED
#define POSITION_INDICATOR_BG        COLOR_CYAN
#define POSITION_INDICATOR_HL        FALSE

#define MENUBOX_FG                   COLOR_BLACK
#define MENUBOX_BG                   COLOR_CYAN
#define MENUBOX_HL                   FALSE

#define MENUBOX_BORDER_FG            COLOR_CYAN
#define MENUBOX_BORDER_BG            COLOR_CYAN
#define MENUBOX_BORDER_HL            TRUE

#define ITEM_FG                      COLOR_BLACK
#define ITEM_BG                      COLOR_CYAN
#define ITEM_HL                      FALSE

#define ITEM_SELECTED_FG             COLOR_WHITE
#define ITEM_SELECTED_BG             COLOR_BLUE
#define ITEM_SELECTED_HL             FALSE

#define TAG_FG                       COLOR_YELLOW
#define TAG_BG                       COLOR_CYAN
#define TAG_HL                       TRUE

#define TAG_SELECTED_FG              COLOR_WHITE
#define TAG_SELECTED_BG              COLOR_BLUE
#define TAG_SELECTED_HL              FALSE

#define TAG_KEY_FG                   COLOR_RED
#define TAG_KEY_BG                   COLOR_CYAN
#define TAG_KEY_HL                   FALSE

#define TAG_KEY_SELECTED_FG          COLOR_RED
#define TAG_KEY_SELECTED_BG          COLOR_BLUE
#define TAG_KEY_SELECTED_HL          TRUE

#define CHECK_FG                     COLOR_BLACK
#define CHECK_BG                     COLOR_CYAN
#define CHECK_HL                     FALSE

#define CHECK_SELECTED_FG            COLOR_WHITE
#define CHECK_SELECTED_BG            COLOR_CYAN
#define CHECK_SELECTED_HL            TRUE

#define UARROW_FG                    COLOR_GREEN
#define UARROW_BG                    COLOR_CYAN
#define UARROW_HL                    TRUE

#define DARROW_FG                    COLOR_GREEN
#define DARROW_BG                    COLOR_CYAN
#define DARROW_HL                    TRUE

/* End of default color definitions */

#define C_ATTR(x,y)                  ((x ? A_BOLD : 0) | COLOR_PAIR((y)))
#define COLOR_NAME_LEN               10
#define COLOR_COUNT                  8

/*
 * Global variables
 */

typedef struct {
    char name[COLOR_NAME_LEN];
    int value;
} color_names_st;

extern color_names_st color_names[];
extern int color_table[][3];
