/* GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball, Josh MacDonald, 
 * Copyright (C) 1997-1998 Jay Painter <jpaint@serv.net><jpaint@gimp.org>  
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GTK+ Team and others 1997-1999.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/. 
 */

#include <config.h>
#include <eel-clist.h>

#include <stdlib.h>
#include <string.h>
#include <gtk/gtkmain.h>
#include <gtk/gtkbindings.h>
#include <gtk/gtkdnd.h>
#include <gdk/gdkx.h>
#include <gdk/gdkkeysyms.h>

/* length of button_actions array */
#define MAX_BUTTON 5

/* the number rows memchunk expands at a time */
#define CLIST_OPTIMUM_SIZE 64

/* the width of the column resize windows */
#define DRAG_WIDTH  6

/* minimum allowed width of a column */
#define COLUMN_MIN_WIDTH 5

/* this defigns the base grid spacing */
#define CELL_SPACING 1

/* added the horizontal space at the beginning and end of a row*/
#define COLUMN_INSET 3

/* used for auto-scrolling */
#define SCROLL_TIME  100

/* gives the top pixel of the given row in context of
 * the clist's voffset */
#define ROW_TOP_YPIXEL(clist, row) (((clist)->row_height * (row)) + \
				    (((row) + 1) * CELL_SPACING) + \
				    (clist)->voffset)

/* returns the row index from a y pixel location in the 
 * context of the clist's voffset */
#define ROW_FROM_YPIXEL(clist, y)  (((y) - (clist)->voffset) / \
				    ((clist)->row_height + CELL_SPACING))

/* gives the left pixel of the given column in context of
 * the clist's hoffset */
#define COLUMN_LEFT_XPIXEL(clist, colnum)  ((clist)->column[(colnum)].area.x + \
					    (clist)->hoffset)

/* returns the column index from a x pixel location in the 
 * context of the clist's hoffset */
static inline gint
COLUMN_FROM_XPIXEL (EelCList * clist, gint x)
{
  gint i, cx;

  for (i = 0; i < clist->columns; i++)
    if (clist->column[i].visible)
      {
	cx = clist->column[i].area.x + clist->hoffset;

	if (x >= (cx - (COLUMN_INSET + CELL_SPACING)) &&
	    x <= (cx + clist->column[i].area.width + COLUMN_INSET))
	  return i;
      }

  /* no match */
  return -1;
}

/* returns the top pixel of the given row in the context of
 * the list height */
#define ROW_TOP(clist, row)        (((clist)->row_height + CELL_SPACING) * (row))

/* returns the left pixel of the given column in the context of
 * the list width */
#define COLUMN_LEFT(clist, colnum) ((clist)->column[(colnum)].area.x)

/* returns the total height of the list */
#define LIST_HEIGHT(clist)         (((clist)->row_height * ((clist)->rows)) + \
				    (CELL_SPACING * ((clist)->rows + 1)))


/* returns the total width of the list */
static inline gint
LIST_WIDTH (EelCList * clist) 
{
  gint last_column;

  for (last_column = clist->columns - 1;
       last_column >= 0 && !clist->column[last_column].visible; last_column--);

  if (last_column >= 0)
    return (clist->column[last_column].area.x +
	    clist->column[last_column].area.width +
	    COLUMN_INSET + CELL_SPACING);
  return 0;
}

/* returns the GList item for the nth row */
#define	ROW_ELEMENT(clist, row)	(((row) == (clist)->rows - 1) ? \
				 (clist)->row_list_end : \
				 g_list_nth ((clist)->row_list, (row)))


#define EEL_CLIST_CLASS_FW(_widget_) EEL_CLIST_CLASS (((GtkObject*) (_widget_))->klass)

/* redraw the list if it's not frozen */
#define CLIST_UNFROZEN(clist) eel_clist_check_unfrozen (clist)
#define	CLIST_REFRESH(clist)	G_STMT_START { \
  if (CLIST_UNFROZEN (clist)) \
    EEL_CLIST_CLASS_FW (clist)->refresh ((EelCList *) (clist)); \
} G_STMT_END


/* Signals */
enum {
  SELECT_ROW,
  UNSELECT_ROW,
  ROW_MOVE,
  CLICK_COLUMN,
  RESIZE_COLUMN,
  TOGGLE_FOCUS_ROW,
  SELECT_ALL,
  UNSELECT_ALL,
  UNDO_SELECTION,
  START_SELECTION,
  END_SELECTION,
  TOGGLE_ADD_MODE,
  EXTEND_SELECTION,
  SCROLL_VERTICAL,
  SCROLL_HORIZONTAL,
  ABORT_COLUMN_RESIZE,
  LAST_SIGNAL
};

enum {
  SYNC_REMOVE,
  SYNC_INSERT
};

enum {
  ARG_0,
  ARG_N_COLUMNS,
  ARG_SHADOW_TYPE,
  ARG_SELECTION_MODE,
  ARG_ROW_HEIGHT,
  ARG_TITLES_ACTIVE,
  ARG_REORDERABLE,
  ARG_USE_DRAG_ICONS,
  ARG_SORT_TYPE
};

/* EelCList Methods */
static void            eel_clist_class_init             (EelCListClass     *klass);
static void            eel_clist_init                   (EelCList          *clist);

/* GtkObject Methods */
static void            eel_clist_destroy                (GtkObject         *object);
static void            eel_clist_finalize               (GtkObject         *object);
static void            eel_clist_set_arg                (GtkObject         *object,
							 GtkArg            *arg,
							 guint              arg_id);
static void            eel_clist_get_arg                (GtkObject         *object,
							 GtkArg            *arg,
							 guint              arg_id);

/* GtkWidget Methods */
static void            eel_clist_set_scroll_adjustments (EelCList          *clist,
							 GtkAdjustment     *hadjustment,
							 GtkAdjustment     *vadjustment);
static void            eel_clist_realize                (GtkWidget         *widget);
static void            eel_clist_unrealize              (GtkWidget         *widget);
static void            eel_clist_map                    (GtkWidget         *widget);
static void            eel_clist_unmap                  (GtkWidget         *widget);
static void            eel_clist_draw                   (GtkWidget         *widget,
							 GdkRectangle      *area);
static gint            eel_clist_expose                 (GtkWidget         *widget,
							 GdkEventExpose    *event);
static gint            eel_clist_key_press              (GtkWidget         *widget,
							 GdkEventKey       *event);
static gint            eel_clist_button_press           (GtkWidget         *widget,
							 GdkEventButton    *event);
static gint            eel_clist_button_release         (GtkWidget         *widget,
							 GdkEventButton    *event);
static gint            eel_clist_motion                 (GtkWidget         *widget,
							 GdkEventMotion    *event);
static void            eel_clist_size_request           (GtkWidget         *widget,
							 GtkRequisition    *requisition);
static void            eel_clist_size_allocate          (GtkWidget         *widget,
							 GtkAllocation     *allocation);
static void            eel_clist_draw_focus             (GtkWidget         *widget);
static gint            eel_clist_focus_in               (GtkWidget         *widget,
							 GdkEventFocus     *event);
static gint            eel_clist_focus_out              (GtkWidget         *widget,
							 GdkEventFocus     *event);
static gint            eel_clist_focus                  (GtkContainer      *container,
							 GtkDirectionType   direction);
static void            eel_clist_style_set              (GtkWidget         *widget,
							 GtkStyle          *previous_style);
static void            eel_clist_drag_begin             (GtkWidget         *widget,
							 GdkDragContext    *context);
static gint            eel_clist_drag_motion            (GtkWidget         *widget,
							 GdkDragContext    *context,
							 gint               x,
							 gint               y,
							 guint              time);
static void            eel_clist_drag_leave             (GtkWidget         *widget,
							 GdkDragContext    *context,
							 guint              time);
static void            eel_clist_drag_end               (GtkWidget         *widget,
							 GdkDragContext    *context);
static gboolean        eel_clist_drag_drop              (GtkWidget         *widget,
							 GdkDragContext    *context,
							 gint               x,
							 gint               y,
							 guint              time);
static void            eel_clist_drag_data_get          (GtkWidget         *widget,
							 GdkDragContext    *context,
							 GtkSelectionData  *selection_data,
							 guint              info,
							 guint              time);
static void            eel_clist_drag_data_received     (GtkWidget         *widget,
							 GdkDragContext    *context,
							 gint               x,
							 gint               y,
							 GtkSelectionData  *selection_data,
							 guint              info,
							 guint              time);

/* GtkContainer Methods */
static void            eel_clist_set_focus_child        (GtkContainer      *container,
							 GtkWidget         *child);
static void            eel_clist_forall                 (GtkContainer      *container,
							 gboolean           include_internals,
							 GtkCallback        callback,
							 gpointer           callback_data);

/* Selection */
static void            toggle_row                       (EelCList          *clist,
							 gint               row,
							 gint               column,
							 GdkEvent          *event);
static void            real_select_row                  (EelCList          *clist,
							 gint               row,
							 gint               column,
							 GdkEvent          *event);
static void            real_unselect_row                (EelCList          *clist,
							 gint               row,
							 gint               column,
							 GdkEvent          *event);
static void            update_extended_selection        (EelCList          *clist,
							 gint               row);
static GList *         selection_find                   (EelCList          *clist,
							 gint               row_number,
							 GList             *row_list_element);
static void            real_select_all                  (EelCList          *clist);
static void            real_unselect_all                (EelCList          *clist);
static void            real_undo_selection              (EelCList          *clist);
static void            fake_unselect_all                (EelCList          *clist,
							 gint               row);
static void            fake_toggle_row                  (EelCList          *clist,
							 gint               row);
static void            resync_selection                 (EelCList          *clist,
							 GdkEvent          *event);
static void            sync_selection                   (EelCList          *clist,
							 gint               row,
							 gint               mode);
static void            set_anchor                       (EelCList          *clist,
							 gboolean           add_mode,
							 gint               anchor,
							 gint               undo_anchor);
static void            start_selection                  (EelCList          *clist);
static void            end_selection                    (EelCList          *clist);
static void            toggle_add_mode                  (EelCList          *clist);
static void            toggle_focus_row                 (EelCList          *clist);
static void            extend_selection                 (EelCList          *clist,
							 GtkScrollType      scroll_type,
							 gfloat             position,
							 gboolean           auto_start_selection);
static gint            get_selection_info               (EelCList          *clist,
							 gint               x,
							 gint               y,
							 gint              *row,
							 gint              *column);

/* Scrolling */
static void            move_focus_row                   (EelCList          *clist,
							 GtkScrollType      scroll_type,
							 gfloat             position);
static void            scroll_horizontal                (EelCList          *clist,
							 GtkScrollType      scroll_type,
							 gfloat             position);
static void            scroll_vertical                  (EelCList          *clist,
							 GtkScrollType      scroll_type,
							 gfloat             position);
static void            move_horizontal                  (EelCList          *clist,
							 gint               diff);
static void            move_vertical                    (EelCList          *clist,
							 gint               row,
							 gfloat             align);
static gint            horizontal_timeout               (EelCList          *clist);
static gint            vertical_timeout                 (EelCList          *clist);
static void            remove_grab                      (EelCList          *clist);


/* Resize Columns */
static void            draw_xor_line                    (EelCList          *clist);
static gint            new_column_width                 (EelCList          *clist,
							 gint               column,
							 gint              *x);
static void            column_auto_resize               (EelCList          *clist,
							 EelCListRow       *clist_row,
							 gint               column,
							 gint               old_width);
static void            real_resize_column               (EelCList          *clist,
							 gint               column,
							 gint               width);
static void            abort_column_resize              (EelCList          *clist);
static void            cell_size_request                (EelCList          *clist,
							 EelCListRow       *clist_row,
							 gint               column,
							 GtkRequisition    *requisition);

/* Buttons */
static void            column_button_create             (EelCList          *clist,
							 gint               column);
static void            column_button_clicked            (GtkWidget         *widget,
							 gpointer           data);

/* Adjustments */
static void            adjust_adjustments               (EelCList          *clist,
							 gboolean           block_resize);
static void            check_exposures                  (EelCList          *clist);
static void            vadjustment_changed              (GtkAdjustment     *adjustment,
							 gpointer           data);
static void            vadjustment_value_changed        (GtkAdjustment     *adjustment,
							 gpointer           data);
static void            hadjustment_changed              (GtkAdjustment     *adjustment,
							 gpointer           data);
static void            hadjustment_value_changed        (GtkAdjustment     *adjustment,
							 gpointer           data);

/* Drawing */
static void            get_cell_style                   (EelCList          *clist,
							 EelCListRow       *clist_row,
							 gint               state,
							 gint               column,
							 GtkStyle         **style,
							 GdkGC            **fg_gc,
							 GdkGC            **bg_gc);
static void            draw_row                         (EelCList          *clist,
							 GdkRectangle      *area,
							 gint               row,
							 EelCListRow       *clist_row);
static void            draw_rows                        (EelCList          *clist,
							 GdkRectangle      *area);
static void            draw_all                         (EelCList          *clist);
static void            clist_refresh                    (EelCList          *clist);
static void            draw_drag_highlight              (EelCList          *clist,
							 EelCListRow       *dest_row,
							 gint               dest_row_number,
							 EelCListDragPos    drag_pos);
     
/* Size Allocation / Requisition */
static void            size_allocate_title_buttons      (EelCList          *clist);
static void            size_allocate_columns            (EelCList          *clist,
							 gboolean           block_resize);
static gint            list_requisition_width           (EelCList          *clist);

/* Memory Allocation/Distruction Routines */
static EelCListColumn *columns_new                      (EelCList          *clist);
static void            column_title_new                 (EelCList          *clist,
							 gint               column,
							 const gchar       *title);
static void            columns_delete                   (EelCList          *clist);
static EelCListRow *   row_new                          (EelCList          *clist);
static void            row_delete                       (EelCList          *clist,
							 EelCListRow       *clist_row);
static gboolean        set_cell_contents                (EelCList          *clist,
							 EelCListRow       *clist_row,
							 gint               column,
							 EelCellType        type,
							 const gchar       *text,
							 guint8             spacing,
							 GdkPixbuf         *pixbuf);
static gint            real_insert_row                  (EelCList          *clist,
							 gint               row,
							 gchar             *text[]);
static void            real_remove_row                  (EelCList          *clist,
							 gint               row);
static void            real_clear                       (EelCList          *clist);

/* Sorting */
static gint            default_compare                  (EelCList          *clist,
							 gconstpointer      row1,
							 gconstpointer      row2);
static void            real_sort_list                   (EelCList          *clist);
static GList *         eel_clist_merge                  (EelCList          *clist,
							 GList             *a,
							 GList             *b);
static GList *         eel_clist_mergesort              (EelCList          *clist,
							 GList             *list,
							 gint               num);
/* Misc */
static gboolean        title_focus                      (EelCList          *clist,
							 gint               dir);
static void            real_row_move                    (EelCList          *clist,
							 gint               source_row,
							 gint               dest_row);
static gint            column_title_passive_func        (GtkWidget         *widget,
							 GdkEvent          *event,
							 gpointer           data);
static void            drag_dest_cell                   (EelCList          *clist,
							 gint               x,
							 gint               y,
							 EelCListDestInfo  *dest_info);

static GtkContainerClass *parent_class = NULL;
static guint clist_signals[LAST_SIGNAL] = {0};

static GtkTargetEntry clist_target_table = { "gtk-clist-drag-reorder", 0, 0};

GtkType
eel_clist_get_type (void)
{
  static GtkType clist_type = 0;

  if (!clist_type)
    {
      static const GtkTypeInfo clist_info =
      {
	"EelCList",
	sizeof (EelCList),
	sizeof (EelCListClass),
	(GtkClassInitFunc) eel_clist_class_init,
	(GtkObjectInitFunc) eel_clist_init,
	/* reserved_1 */ NULL,
	/* reserved_2 */ NULL,
	(GtkClassInitFunc) NULL,
      };

      clist_type = gtk_type_unique (GTK_TYPE_CONTAINER, &clist_info);
    }

  return clist_type;
}

static void
eel_clist_class_init (EelCListClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkContainerClass *container_class;
  GtkBindingSet *binding_set;

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  container_class = (GtkContainerClass *) klass;

  parent_class = gtk_type_class (GTK_TYPE_CONTAINER);

  gtk_object_add_arg_type ("EelCList::n_columns",
			   GTK_TYPE_UINT,
			   GTK_ARG_READWRITE | GTK_ARG_CONSTRUCT_ONLY,
			   ARG_N_COLUMNS);
  gtk_object_add_arg_type ("EelCList::shadow_type",
			   GTK_TYPE_SHADOW_TYPE,
			   GTK_ARG_READWRITE,
			   ARG_SHADOW_TYPE);
  gtk_object_add_arg_type ("EelCList::selection_mode",
			   GTK_TYPE_SELECTION_MODE,
			   GTK_ARG_READWRITE,
			   ARG_SELECTION_MODE);
  gtk_object_add_arg_type ("EelCList::row_height",
			   GTK_TYPE_UINT,
			   GTK_ARG_READWRITE,
			   ARG_ROW_HEIGHT);
  gtk_object_add_arg_type ("EelCList::reorderable",
			   GTK_TYPE_BOOL,
			   GTK_ARG_READWRITE,
			   ARG_REORDERABLE);
  gtk_object_add_arg_type ("EelCList::titles_active",
			   GTK_TYPE_BOOL,
			   GTK_ARG_READWRITE,
			   ARG_TITLES_ACTIVE);
  gtk_object_add_arg_type ("EelCList::use_drag_icons",
			   GTK_TYPE_BOOL,
			   GTK_ARG_READWRITE,
			   ARG_USE_DRAG_ICONS);
  gtk_object_add_arg_type ("EelCList::sort_type",
			   GTK_TYPE_SORT_TYPE,
			   GTK_ARG_READWRITE,
			   ARG_SORT_TYPE);  
  object_class->set_arg = eel_clist_set_arg;
  object_class->get_arg = eel_clist_get_arg;
  object_class->destroy = eel_clist_destroy;
  object_class->finalize = eel_clist_finalize;


  widget_class->set_scroll_adjustments_signal =
    gtk_signal_new ("set_scroll_adjustments",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (EelCListClass, set_scroll_adjustments),
		    gtk_marshal_NONE__POINTER_POINTER,
		    GTK_TYPE_NONE, 2, GTK_TYPE_ADJUSTMENT, GTK_TYPE_ADJUSTMENT);

  clist_signals[SELECT_ROW] =
    gtk_signal_new ("select_row",
		    GTK_RUN_FIRST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (EelCListClass, select_row),
		    gtk_marshal_NONE__INT_INT_POINTER,
		    GTK_TYPE_NONE, 3,
		    GTK_TYPE_INT,
		    GTK_TYPE_INT,
		    GTK_TYPE_GDK_EVENT);
  clist_signals[UNSELECT_ROW] =
    gtk_signal_new ("unselect_row",
		    GTK_RUN_FIRST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (EelCListClass, unselect_row),
		    gtk_marshal_NONE__INT_INT_POINTER,
		    GTK_TYPE_NONE, 3, GTK_TYPE_INT,
		    GTK_TYPE_INT, GTK_TYPE_GDK_EVENT);
  clist_signals[ROW_MOVE] =
    gtk_signal_new ("row_move",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (EelCListClass, row_move),
		    gtk_marshal_NONE__INT_INT,
		    GTK_TYPE_NONE, 2, GTK_TYPE_INT, GTK_TYPE_INT);
  clist_signals[CLICK_COLUMN] =
    gtk_signal_new ("click_column",
		    GTK_RUN_FIRST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (EelCListClass, click_column),
		    gtk_marshal_NONE__INT,
		    GTK_TYPE_NONE, 1, GTK_TYPE_INT);
  clist_signals[RESIZE_COLUMN] =
    gtk_signal_new ("resize_column",
		    GTK_RUN_LAST,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (EelCListClass, resize_column),
		    gtk_marshal_NONE__INT_INT,
		    GTK_TYPE_NONE, 2, GTK_TYPE_INT, GTK_TYPE_INT);

  clist_signals[TOGGLE_FOCUS_ROW] =
    gtk_signal_new ("toggle_focus_row",
                    GTK_RUN_LAST | GTK_RUN_ACTION,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (EelCListClass, toggle_focus_row),
                    gtk_marshal_NONE__NONE,
                    GTK_TYPE_NONE, 0);
  clist_signals[SELECT_ALL] =
    gtk_signal_new ("select_all",
                    GTK_RUN_LAST | GTK_RUN_ACTION,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (EelCListClass, select_all),
                    gtk_marshal_NONE__NONE,
                    GTK_TYPE_NONE, 0);
  clist_signals[UNSELECT_ALL] =
    gtk_signal_new ("unselect_all",
                    GTK_RUN_LAST | GTK_RUN_ACTION,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (EelCListClass, unselect_all),
                    gtk_marshal_NONE__NONE,
                    GTK_TYPE_NONE, 0);
  clist_signals[UNDO_SELECTION] =
    gtk_signal_new ("undo_selection",
		    GTK_RUN_LAST | GTK_RUN_ACTION,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (EelCListClass, undo_selection),
		    gtk_marshal_NONE__NONE,
		    GTK_TYPE_NONE, 0);
  clist_signals[START_SELECTION] =
    gtk_signal_new ("start_selection",
		    GTK_RUN_LAST | GTK_RUN_ACTION,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (EelCListClass, start_selection),
		    gtk_marshal_NONE__NONE,
		    GTK_TYPE_NONE, 0);
  clist_signals[END_SELECTION] =
    gtk_signal_new ("end_selection",
		    GTK_RUN_LAST | GTK_RUN_ACTION,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (EelCListClass, end_selection),
		    gtk_marshal_NONE__NONE,
		    GTK_TYPE_NONE, 0);
  clist_signals[TOGGLE_ADD_MODE] =
    gtk_signal_new ("toggle_add_mode",
		    GTK_RUN_LAST | GTK_RUN_ACTION,
		    object_class->type,
		    GTK_SIGNAL_OFFSET (EelCListClass, toggle_add_mode),
		    gtk_marshal_NONE__NONE,
		    GTK_TYPE_NONE, 0);
  clist_signals[EXTEND_SELECTION] =
    gtk_signal_new ("extend_selection",
                    GTK_RUN_LAST | GTK_RUN_ACTION,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (EelCListClass, extend_selection),
                    gtk_marshal_NONE__ENUM_FLOAT_BOOL,
                    GTK_TYPE_NONE, 3,
		    GTK_TYPE_SCROLL_TYPE, GTK_TYPE_FLOAT, GTK_TYPE_BOOL);
  clist_signals[SCROLL_VERTICAL] =
    gtk_signal_new ("scroll_vertical",
                    GTK_RUN_LAST | GTK_RUN_ACTION,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (EelCListClass, scroll_vertical),
                    gtk_marshal_NONE__ENUM_FLOAT,
                    GTK_TYPE_NONE, 2, GTK_TYPE_SCROLL_TYPE, GTK_TYPE_FLOAT);
  clist_signals[SCROLL_HORIZONTAL] =
    gtk_signal_new ("scroll_horizontal",
                    GTK_RUN_LAST | GTK_RUN_ACTION,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (EelCListClass, scroll_horizontal),
                    gtk_marshal_NONE__ENUM_FLOAT,
                    GTK_TYPE_NONE, 2, GTK_TYPE_SCROLL_TYPE, GTK_TYPE_FLOAT);
  clist_signals[ABORT_COLUMN_RESIZE] =
    gtk_signal_new ("abort_column_resize",
                    GTK_RUN_LAST | GTK_RUN_ACTION,
                    object_class->type,
                    GTK_SIGNAL_OFFSET (EelCListClass, abort_column_resize),
                    gtk_marshal_NONE__NONE,
                    GTK_TYPE_NONE, 0);
  gtk_object_class_add_signals (object_class, clist_signals, LAST_SIGNAL);

  widget_class->realize = eel_clist_realize;
  widget_class->unrealize = eel_clist_unrealize;
  widget_class->map = eel_clist_map;
  widget_class->unmap = eel_clist_unmap;
  widget_class->draw = eel_clist_draw;
  widget_class->button_press_event = eel_clist_button_press;
  widget_class->button_release_event = eel_clist_button_release;
  widget_class->motion_notify_event = eel_clist_motion;
  widget_class->expose_event = eel_clist_expose;
  widget_class->size_request = eel_clist_size_request;
  widget_class->size_allocate = eel_clist_size_allocate;
  widget_class->key_press_event = eel_clist_key_press;
  widget_class->focus_in_event = eel_clist_focus_in;
  widget_class->focus_out_event = eel_clist_focus_out;
  widget_class->draw_focus = eel_clist_draw_focus;
  widget_class->style_set = eel_clist_style_set;
  widget_class->drag_begin = eel_clist_drag_begin;
  widget_class->drag_end = eel_clist_drag_end;
  widget_class->drag_motion = eel_clist_drag_motion;
  widget_class->drag_leave = eel_clist_drag_leave;
  widget_class->drag_drop = eel_clist_drag_drop;
  widget_class->drag_data_get = eel_clist_drag_data_get;
  widget_class->drag_data_received = eel_clist_drag_data_received;

  /* container_class->add = NULL; use the default GtkContainerClass warning */
  /* container_class->remove=NULL; use the default GtkContainerClass warning */

  container_class->forall = eel_clist_forall;
  container_class->focus = eel_clist_focus;
  container_class->set_focus_child = eel_clist_set_focus_child;

  klass->set_scroll_adjustments = eel_clist_set_scroll_adjustments;
  klass->refresh = clist_refresh;
  klass->select_row = real_select_row;
  klass->unselect_row = real_unselect_row;
  klass->row_move = real_row_move;
  klass->undo_selection = real_undo_selection;
  klass->resync_selection = resync_selection;
  klass->selection_find = selection_find;
  klass->click_column = NULL;
  klass->resize_column = real_resize_column;
  klass->draw_rows = draw_rows;
  klass->draw_row = draw_row;
  klass->draw_all = draw_all;
  klass->draw_drag_highlight = draw_drag_highlight;
  klass->insert_row = real_insert_row;
  klass->remove_row = real_remove_row;
  klass->clear = real_clear;
  klass->sort_list = real_sort_list;
  klass->select_all = real_select_all;
  klass->unselect_all = real_unselect_all;
  klass->fake_unselect_all = fake_unselect_all;
  klass->scroll_horizontal = scroll_horizontal;
  klass->scroll_vertical = scroll_vertical;
  klass->extend_selection = extend_selection;
  klass->toggle_focus_row = toggle_focus_row;
  klass->toggle_add_mode = toggle_add_mode;
  klass->start_selection = start_selection;
  klass->end_selection = end_selection;
  klass->abort_column_resize = abort_column_resize;
  klass->set_cell_contents = set_cell_contents;
  klass->cell_size_request = cell_size_request;

  binding_set = gtk_binding_set_by_class (klass);
  gtk_binding_entry_add_signal (binding_set, GDK_Up, 0,
				"scroll_vertical", 2,
				GTK_TYPE_ENUM, GTK_SCROLL_STEP_BACKWARD,
				GTK_TYPE_FLOAT, 0.0);
  gtk_binding_entry_add_signal (binding_set, GDK_Down, 0,
				"scroll_vertical", 2,
				GTK_TYPE_ENUM, GTK_SCROLL_STEP_FORWARD,
				GTK_TYPE_FLOAT, 0.0);
  gtk_binding_entry_add_signal (binding_set, GDK_Page_Up, 0,
				"scroll_vertical", 2,
				GTK_TYPE_ENUM, GTK_SCROLL_PAGE_BACKWARD,
				GTK_TYPE_FLOAT, 0.0);
  gtk_binding_entry_add_signal (binding_set, GDK_Page_Down, 0,
				"scroll_vertical", 2,
				GTK_TYPE_ENUM, GTK_SCROLL_PAGE_FORWARD,
				GTK_TYPE_FLOAT, 0.0);
  gtk_binding_entry_add_signal (binding_set, GDK_Home, GDK_CONTROL_MASK,
				"scroll_vertical", 2,
				GTK_TYPE_ENUM, GTK_SCROLL_JUMP,
				GTK_TYPE_FLOAT, 0.0);
  gtk_binding_entry_add_signal (binding_set, GDK_End, GDK_CONTROL_MASK,
				"scroll_vertical", 2,
				GTK_TYPE_ENUM, GTK_SCROLL_JUMP,
				GTK_TYPE_FLOAT, 1.0);

  gtk_binding_entry_add_signal (binding_set, GDK_Up, GDK_SHIFT_MASK,
				"extend_selection", 3,
				GTK_TYPE_ENUM, GTK_SCROLL_STEP_BACKWARD,
				GTK_TYPE_FLOAT, 0.0, GTK_TYPE_BOOL, TRUE);
  gtk_binding_entry_add_signal (binding_set, GDK_Down, GDK_SHIFT_MASK,
				"extend_selection", 3,
				GTK_TYPE_ENUM, GTK_SCROLL_STEP_FORWARD,
				GTK_TYPE_FLOAT, 0.0, GTK_TYPE_BOOL, TRUE);
  gtk_binding_entry_add_signal (binding_set, GDK_Page_Up, GDK_SHIFT_MASK,
				"extend_selection", 3,
				GTK_TYPE_ENUM, GTK_SCROLL_PAGE_BACKWARD,
				GTK_TYPE_FLOAT, 0.0, GTK_TYPE_BOOL, TRUE);
  gtk_binding_entry_add_signal (binding_set, GDK_Page_Down, GDK_SHIFT_MASK,
				"extend_selection", 3,
				GTK_TYPE_ENUM, GTK_SCROLL_PAGE_FORWARD,
				GTK_TYPE_FLOAT, 0.0, GTK_TYPE_BOOL, TRUE);
  gtk_binding_entry_add_signal (binding_set, GDK_Home,
				GDK_SHIFT_MASK | GDK_CONTROL_MASK,
				"extend_selection", 3,
				GTK_TYPE_ENUM, GTK_SCROLL_JUMP,
				GTK_TYPE_FLOAT, 0.0, GTK_TYPE_BOOL, TRUE);
  gtk_binding_entry_add_signal (binding_set, GDK_End,
				GDK_SHIFT_MASK | GDK_CONTROL_MASK,
				"extend_selection", 3,
				GTK_TYPE_ENUM, GTK_SCROLL_JUMP,
				GTK_TYPE_FLOAT, 1.0, GTK_TYPE_BOOL, TRUE);

  gtk_binding_entry_add_signal (binding_set, GDK_Left, 0,
				"scroll_horizontal", 2,
				GTK_TYPE_ENUM, GTK_SCROLL_STEP_BACKWARD,
				GTK_TYPE_FLOAT, 0.0);
  gtk_binding_entry_add_signal (binding_set, GDK_Right, 0,
				"scroll_horizontal", 2,
				GTK_TYPE_ENUM, GTK_SCROLL_STEP_FORWARD,
				GTK_TYPE_FLOAT, 0.0);
  gtk_binding_entry_add_signal (binding_set, GDK_Home, 0,
				"scroll_horizontal", 2,
				GTK_TYPE_ENUM, GTK_SCROLL_JUMP,
				GTK_TYPE_FLOAT, 0.0);
  gtk_binding_entry_add_signal (binding_set, GDK_End, 0,
				"scroll_horizontal", 2,
				GTK_TYPE_ENUM, GTK_SCROLL_JUMP,
				GTK_TYPE_FLOAT, 1.0);

  gtk_binding_entry_add_signal (binding_set, GDK_Escape, 0,
				"undo_selection", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_Escape, 0,
				"abort_column_resize", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_space, 0,
				"toggle_focus_row", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_space, GDK_CONTROL_MASK,
				"toggle_add_mode", 0);
  gtk_binding_entry_add_signal (binding_set, '/', GDK_CONTROL_MASK,
				"select_all", 0);
  gtk_binding_entry_add_signal (binding_set, '\\', GDK_CONTROL_MASK,
				"unselect_all", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_Shift_L,
				GDK_RELEASE_MASK | GDK_SHIFT_MASK,
				"end_selection", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_Shift_R,
				GDK_RELEASE_MASK | GDK_SHIFT_MASK,
				"end_selection", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_Shift_L,
				GDK_RELEASE_MASK | GDK_SHIFT_MASK |
				GDK_CONTROL_MASK,
				"end_selection", 0);
  gtk_binding_entry_add_signal (binding_set, GDK_Shift_R,
				GDK_RELEASE_MASK | GDK_SHIFT_MASK |
				GDK_CONTROL_MASK,
				"end_selection", 0);
}

static void
eel_clist_set_arg (GtkObject      *object,
		   GtkArg         *arg,
		   guint           arg_id)
{
  EelCList *clist;

  clist = EEL_CLIST (object);

  switch (arg_id)
    {
    case ARG_N_COLUMNS: /* construct-only arg, only set when !GTK_CONSTRUCTED */
      eel_clist_construct (clist, MAX (1, GTK_VALUE_UINT (*arg)), NULL);
      break;
    case ARG_SHADOW_TYPE:
      eel_clist_set_shadow_type (clist, GTK_VALUE_ENUM (*arg));
      break;
    case ARG_SELECTION_MODE:
      eel_clist_set_selection_mode (clist, GTK_VALUE_ENUM (*arg));
      break;
    case ARG_ROW_HEIGHT:
      eel_clist_set_row_height (clist, GTK_VALUE_UINT (*arg));
      break;
    case ARG_REORDERABLE:
      eel_clist_set_reorderable (clist, GTK_VALUE_BOOL (*arg));
      break;
    case ARG_TITLES_ACTIVE:
      if (GTK_VALUE_BOOL (*arg))
	eel_clist_column_titles_active (clist);
      else
	eel_clist_column_titles_passive (clist);
      break;
    case ARG_USE_DRAG_ICONS:
      eel_clist_set_use_drag_icons (clist, GTK_VALUE_BOOL (*arg));
      break;
    case ARG_SORT_TYPE:
      eel_clist_set_sort_type (clist, GTK_VALUE_ENUM (*arg));
      break;
    }
}

static void
eel_clist_get_arg (GtkObject      *object,
		   GtkArg         *arg,
		   guint           arg_id)
{
  EelCList *clist;

  clist = EEL_CLIST (object);

  switch (arg_id)
    {
      int i;

    case ARG_N_COLUMNS:
      GTK_VALUE_UINT (*arg) = clist->columns;
      break;
    case ARG_SHADOW_TYPE:
      GTK_VALUE_ENUM (*arg) = clist->shadow_type;
      break;
    case ARG_SELECTION_MODE:
      GTK_VALUE_ENUM (*arg) = clist->selection_mode;
      break;
    case ARG_ROW_HEIGHT:
      GTK_VALUE_UINT (*arg) = EEL_CLIST_ROW_HEIGHT_SET(clist) ? clist->row_height : 0;
      break;
    case ARG_REORDERABLE:
      GTK_VALUE_BOOL (*arg) = EEL_CLIST_REORDERABLE (clist);
      break;
    case ARG_TITLES_ACTIVE:
      GTK_VALUE_BOOL (*arg) = TRUE;
      for (i = 0; i < clist->columns; i++)
	if (clist->column[i].button &&
	    !GTK_WIDGET_SENSITIVE (clist->column[i].button))
	  {
	    GTK_VALUE_BOOL (*arg) = FALSE;
	    break;
	  }
      break;
    case ARG_USE_DRAG_ICONS:
      GTK_VALUE_BOOL (*arg) = EEL_CLIST_USE_DRAG_ICONS (clist);
      break;
    case ARG_SORT_TYPE:
      GTK_VALUE_ENUM (*arg) = clist->sort_type;
      break;
    default:
      arg->type = GTK_TYPE_INVALID;
      break;
    }
}

static void
eel_clist_init (EelCList *clist)
{
  clist->flags = 0;

  GTK_WIDGET_UNSET_FLAGS (clist, GTK_NO_WINDOW);
  GTK_WIDGET_SET_FLAGS (clist, GTK_CAN_FOCUS);
  EEL_CLIST_SET_FLAG (clist, CLIST_CHILD_HAS_FOCUS);
  EEL_CLIST_SET_FLAG (clist, CLIST_DRAW_DRAG_LINE);
  EEL_CLIST_SET_FLAG (clist, CLIST_USE_DRAG_ICONS);

  clist->row_mem_chunk = NULL;
  clist->cell_mem_chunk = NULL;

  clist->freeze_count = 0;

  clist->rows = 0;
  clist->row_center_offset = 0;
  clist->row_height = 0;
  clist->row_list = NULL;
  clist->row_list_end = NULL;

  clist->columns = 0;

  clist->title_window = NULL;
  clist->column_title_area.x = 0;
  clist->column_title_area.y = 0;
  clist->column_title_area.width = 1;
  clist->column_title_area.height = 1;

  clist->clist_window = NULL;
  clist->clist_window_width = 1;
  clist->clist_window_height = 1;

  clist->hoffset = 0;
  clist->voffset = 0;

  clist->shadow_type = GTK_SHADOW_IN;
  clist->vadjustment = NULL;
  clist->hadjustment = NULL;

  clist->button_actions[0] = EEL_BUTTON_SELECTS | EEL_BUTTON_DRAGS;
  clist->button_actions[1] = EEL_BUTTON_IGNORED;
  clist->button_actions[2] = EEL_BUTTON_IGNORED;
  clist->button_actions[3] = EEL_BUTTON_IGNORED;
  clist->button_actions[4] = EEL_BUTTON_IGNORED;

  clist->cursor_drag = NULL;
  clist->xor_gc = NULL;
  clist->fg_gc = NULL;
  clist->bg_gc = NULL;
  clist->x_drag = 0;

  clist->selection_mode = GTK_SELECTION_SINGLE;
  clist->selection = NULL;
  clist->selection_end = NULL;
  clist->undo_selection = NULL;
  clist->undo_unselection = NULL;

  clist->focus_row = -1;
  clist->undo_anchor = -1;

  clist->anchor = -1;
  clist->anchor_state = GTK_STATE_SELECTED;
  clist->drag_pos = -1;
  clist->htimer = 0;
  clist->vtimer = 0;

  clist->click_cell.row = -1;
  clist->click_cell.column = -1;

  clist->compare = default_compare;
  clist->sort_type = GTK_SORT_ASCENDING;
  clist->sort_column = 0;
}

/* Constructors */
void
eel_clist_construct (EelCList *clist,
		     gint      columns,
		     gchar    *titles[])
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));
  g_return_if_fail (columns > 0);
  g_return_if_fail (GTK_OBJECT_CONSTRUCTED (clist) == FALSE);

  /* mark the object as constructed */
  gtk_object_constructed (GTK_OBJECT (clist));

  /* initalize memory chunks, if this has not been done by any
   * possibly derived widget
   */
  if (!clist->row_mem_chunk)
    clist->row_mem_chunk = g_mem_chunk_new ("clist row mem chunk",
					    sizeof (EelCListRow),
					    sizeof (EelCListRow) *
					    CLIST_OPTIMUM_SIZE, 
					    G_ALLOC_AND_FREE);

  if (!clist->cell_mem_chunk)
    clist->cell_mem_chunk = g_mem_chunk_new ("clist cell mem chunk",
					     sizeof (EelCell) * columns,
					     sizeof (EelCell) * columns *
					     CLIST_OPTIMUM_SIZE, 
					     G_ALLOC_AND_FREE);

  /* set number of columns, allocate memory */
  clist->columns = columns;
  clist->column = columns_new (clist);

  /* there needs to be at least one column button 
   * because there is alot of code that will break if it
   * isn't there*/
  column_button_create (clist, 0);

  if (titles)
    {
      int i;
      
      EEL_CLIST_SET_FLAG (clist, CLIST_SHOW_TITLES);
      for (i = 0; i < columns; i++)
	eel_clist_set_column_title (clist, i, titles[i]);
    }
  else
    {
      EEL_CLIST_UNSET_FLAG (clist, CLIST_SHOW_TITLES);
    }
}

/* EELLIST PUBLIC INTERFACE
 *   eel_clist_new
 *   eel_clist_new_with_titles
 *   eel_clist_set_hadjustment
 *   eel_clist_set_vadjustment
 *   eel_clist_get_hadjustment
 *   eel_clist_get_vadjustment
 *   eel_clist_set_shadow_type
 *   eel_clist_set_selection_mode
 *   eel_clist_freeze
 *   eel_clist_thaw
 */
GtkWidget*
eel_clist_new (gint columns)
{
  return eel_clist_new_with_titles (columns, NULL);
}
 
GtkWidget*
eel_clist_new_with_titles (gint   columns,
			   gchar *titles[])
{
  GtkWidget *widget;

  widget = gtk_type_new (EEL_TYPE_CLIST);
  eel_clist_construct (EEL_CLIST (widget), columns, titles);

  return widget;
}

void
eel_clist_set_hadjustment (EelCList      *clist,
			   GtkAdjustment *adjustment)
{
  GtkAdjustment *old_adjustment;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));
  if (adjustment)
    g_return_if_fail (GTK_IS_ADJUSTMENT (adjustment));
  
  if (clist->hadjustment == adjustment)
    return;
  
  old_adjustment = clist->hadjustment;

  if (clist->hadjustment)
    {
      gtk_signal_disconnect_by_data (GTK_OBJECT (clist->hadjustment), clist);
      gtk_object_unref (GTK_OBJECT (clist->hadjustment));
    }

  clist->hadjustment = adjustment;

  if (clist->hadjustment)
    {
      gtk_object_ref (GTK_OBJECT (clist->hadjustment));
      gtk_object_sink (GTK_OBJECT (clist->hadjustment));

      gtk_signal_connect (GTK_OBJECT (clist->hadjustment), "changed",
			  (GtkSignalFunc) hadjustment_changed,
			  (gpointer) clist);
      gtk_signal_connect (GTK_OBJECT (clist->hadjustment), "value_changed",
			  (GtkSignalFunc) hadjustment_value_changed,
			  (gpointer) clist);
    }

  if (!clist->hadjustment || !old_adjustment)
    gtk_widget_queue_resize (GTK_WIDGET (clist));
}

GtkAdjustment *
eel_clist_get_hadjustment (EelCList *clist)
{
  g_return_val_if_fail (clist != NULL, NULL);
  g_return_val_if_fail (EEL_IS_CLIST (clist), NULL);

  return clist->hadjustment;
}

void
eel_clist_set_vadjustment (EelCList      *clist,
			   GtkAdjustment *adjustment)
{
  GtkAdjustment *old_adjustment;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));
  if (adjustment)
    g_return_if_fail (GTK_IS_ADJUSTMENT (adjustment));

  if (clist->vadjustment == adjustment)
    return;
  
  old_adjustment = clist->vadjustment;

  if (clist->vadjustment)
    {
      gtk_signal_disconnect_by_data (GTK_OBJECT (clist->vadjustment), clist);
      gtk_object_unref (GTK_OBJECT (clist->vadjustment));
    }

  clist->vadjustment = adjustment;

  if (clist->vadjustment)
    {
      gtk_object_ref (GTK_OBJECT (clist->vadjustment));
      gtk_object_sink (GTK_OBJECT (clist->vadjustment));

      gtk_signal_connect (GTK_OBJECT (clist->vadjustment), "changed",
			  (GtkSignalFunc) vadjustment_changed,
			  (gpointer) clist);
      gtk_signal_connect (GTK_OBJECT (clist->vadjustment), "value_changed",
			  (GtkSignalFunc) vadjustment_value_changed,
			  (gpointer) clist);
    }

  if (!clist->vadjustment || !old_adjustment)
    gtk_widget_queue_resize (GTK_WIDGET (clist));
}

GtkAdjustment *
eel_clist_get_vadjustment (EelCList *clist)
{
  g_return_val_if_fail (clist != NULL, NULL);
  g_return_val_if_fail (EEL_IS_CLIST (clist), NULL);

  return clist->vadjustment;
}

static void
eel_clist_set_scroll_adjustments (EelCList      *clist,
				  GtkAdjustment *hadjustment,
				  GtkAdjustment *vadjustment)
{
  if (clist->hadjustment != hadjustment)
    eel_clist_set_hadjustment (clist, hadjustment);
  if (clist->vadjustment != vadjustment)
    eel_clist_set_vadjustment (clist, vadjustment);
}

void
eel_clist_set_shadow_type (EelCList      *clist,
			   GtkShadowType  type)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  clist->shadow_type = type;

  if (GTK_WIDGET_VISIBLE (clist))
    gtk_widget_queue_resize (GTK_WIDGET (clist));
}

void
eel_clist_set_selection_mode (EelCList         *clist,
			      GtkSelectionMode  mode)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (mode == clist->selection_mode)
    return;

  clist->selection_mode = mode;
  clist->anchor = -1;
  clist->anchor_state = GTK_STATE_SELECTED;
  clist->drag_pos = -1;
  clist->undo_anchor = clist->focus_row;

  g_list_free (clist->undo_selection);
  g_list_free (clist->undo_unselection);
  clist->undo_selection = NULL;
  clist->undo_unselection = NULL;

  switch (mode)
    {
    case GTK_SELECTION_MULTIPLE:
    case GTK_SELECTION_EXTENDED:
      return;
    case GTK_SELECTION_BROWSE:
    case GTK_SELECTION_SINGLE:
      eel_clist_unselect_all (clist);
      break;
    }
}

void
eel_clist_freeze (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  clist->freeze_count++;
}

void
eel_clist_thaw (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (clist->freeze_count)
    {
      clist->freeze_count--;
      if (clist->freeze_count == 0)
	{
	  if (clist->refresh_at_unfreeze_time)
	    EEL_CLIST_CLASS_FW (clist)->refresh (clist);
	  clist->refresh_at_unfreeze_time = FALSE;
	}
    }
}

gboolean
eel_clist_check_unfrozen (EelCList *clist)
{
  if (clist->freeze_count == 0)
    return TRUE;

  clist->refresh_at_unfreeze_time = TRUE;
  return FALSE;
}

/* PUBLIC COLUMN FUNCTIONS
 *   eel_clist_column_titles_show
 *   eel_clist_column_titles_hide
 *   eel_clist_column_title_active
 *   eel_clist_column_title_passive
 *   eel_clist_column_titles_active
 *   eel_clist_column_titles_passive
 *   eel_clist_set_column_title
 *   eel_clist_get_column_title
 *   eel_clist_set_column_widget
 *   eel_clist_set_column_justification
 *   eel_clist_set_column_visibility
 *   eel_clist_set_column_resizeable
 *   eel_clist_set_column_auto_resize
 *   eel_clist_optimal_column_width
 *   eel_clist_set_column_width
 *   eel_clist_set_column_min_width
 *   eel_clist_set_column_max_width
 */
void
eel_clist_column_titles_show (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (!EEL_CLIST_SHOW_TITLES(clist))
    {
      EEL_CLIST_SET_FLAG (clist, CLIST_SHOW_TITLES);
      if (clist->title_window)
	gdk_window_show (clist->title_window);
      gtk_widget_queue_resize (GTK_WIDGET (clist));
    }
}

void 
eel_clist_column_titles_hide (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (EEL_CLIST_SHOW_TITLES(clist))
    {
      EEL_CLIST_UNSET_FLAG (clist, CLIST_SHOW_TITLES);
      if (clist->title_window)
	gdk_window_hide (clist->title_window);
      gtk_widget_queue_resize (GTK_WIDGET (clist));
    }
}

void
eel_clist_column_title_active (EelCList *clist,
			       gint      column)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;
  if (!clist->column[column].button || !clist->column[column].button_passive)
    return;

  clist->column[column].button_passive = FALSE;

  gtk_signal_disconnect_by_func (GTK_OBJECT (clist->column[column].button),
				 (GtkSignalFunc) column_title_passive_func,
				 NULL);

  GTK_WIDGET_SET_FLAGS (clist->column[column].button, GTK_CAN_FOCUS);
  if (GTK_WIDGET_VISIBLE (clist))
    gtk_widget_queue_draw (clist->column[column].button);
}

void
eel_clist_column_title_passive (EelCList *clist,
				gint      column)
{
  GtkButton *button;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;
  if (!clist->column[column].button || clist->column[column].button_passive)
    return;

  button = GTK_BUTTON (clist->column[column].button);

  clist->column[column].button_passive = TRUE;

  if (button->button_down)
    gtk_button_released (button);
  if (button->in_button)
    gtk_button_leave (button);

  gtk_signal_connect (GTK_OBJECT (clist->column[column].button), "event",
		      (GtkSignalFunc) column_title_passive_func, NULL);

  GTK_WIDGET_UNSET_FLAGS (clist->column[column].button, GTK_CAN_FOCUS);
  if (GTK_WIDGET_VISIBLE (clist))
    gtk_widget_queue_draw (clist->column[column].button);
}

void
eel_clist_column_titles_active (EelCList *clist)
{
  gint i;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (!EEL_CLIST_SHOW_TITLES(clist))
    return;

  for (i = 0; i < clist->columns; i++)
    eel_clist_column_title_active (clist, i);
}

void
eel_clist_column_titles_passive (EelCList *clist)
{
  gint i;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (!EEL_CLIST_SHOW_TITLES(clist))
    return;

  for (i = 0; i < clist->columns; i++)
    eel_clist_column_title_passive (clist, i);
}

void
eel_clist_set_column_title (EelCList    *clist,
			    gint         column,
			    const gchar *title)
{
  gint new_button = 0;
  GtkWidget *old_widget;
  GtkWidget *alignment = NULL;
  GtkWidget *label;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;

  /* if the column button doesn't currently exist,
   * it has to be created first */
  if (!clist->column[column].button)
    {
      column_button_create (clist, column);
      new_button = 1;
    }

  column_title_new (clist, column, title);

  /* remove and destroy the old widget */
  old_widget = GTK_BIN (clist->column[column].button)->child;
  if (old_widget)
    gtk_container_remove (GTK_CONTAINER (clist->column[column].button), old_widget);

  /* create new alignment based no column justification */
  switch (clist->column[column].justification)
    {
    case GTK_JUSTIFY_LEFT:
      alignment = gtk_alignment_new (0.0, 0.5, 0.0, 0.0);
      break;

    case GTK_JUSTIFY_RIGHT:
      alignment = gtk_alignment_new (1.0, 0.5, 0.0, 0.0);
      break;

    case GTK_JUSTIFY_CENTER:
      alignment = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
      break;

    case GTK_JUSTIFY_FILL:
      alignment = gtk_alignment_new (0.5, 0.5, 0.0, 0.0);
      break;
    }

  gtk_widget_push_composite_child ();
  label = gtk_label_new (clist->column[column].title);
  gtk_widget_pop_composite_child ();
  gtk_container_add (GTK_CONTAINER (alignment), label);
  gtk_container_add (GTK_CONTAINER (clist->column[column].button), alignment);
  gtk_widget_show (label);
  gtk_widget_show (alignment);

  /* if this button didn't previously exist, then the
   * column button positions have to be re-computed */
  if (GTK_WIDGET_VISIBLE (clist) && new_button)
    size_allocate_title_buttons (clist);
}

gchar *
eel_clist_get_column_title (EelCList *clist,
			    gint      column)
{
  g_return_val_if_fail (clist != NULL, NULL);
  g_return_val_if_fail (EEL_IS_CLIST (clist), NULL);

  if (column < 0 || column >= clist->columns)
    return NULL;

  return clist->column[column].title;
}

void
eel_clist_set_column_widget (EelCList  *clist,
			     gint       column,
			     GtkWidget *widget)
{
  gint new_button = 0;
  GtkWidget *old_widget;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;

  /* if the column button doesn't currently exist,
   * it has to be created first */
  if (!clist->column[column].button)
    {
      column_button_create (clist, column);
      new_button = 1;
    }

  column_title_new (clist, column, NULL);

  /* remove and destroy the old widget */
  old_widget = GTK_BIN (clist->column[column].button)->child;
  if (old_widget)
    gtk_container_remove (GTK_CONTAINER (clist->column[column].button),
			  old_widget);

  /* add and show the widget */
  if (widget)
    {
      gtk_container_add (GTK_CONTAINER (clist->column[column].button), widget);
      gtk_widget_show (widget);
    }

  /* if this button didn't previously exist, then the
   * column button positions have to be re-computed */
  if (GTK_WIDGET_VISIBLE (clist) && new_button)
    size_allocate_title_buttons (clist);
}

GtkWidget *
eel_clist_get_column_widget (EelCList *clist,
			     gint      column)
{
  g_return_val_if_fail (clist != NULL, NULL);
  g_return_val_if_fail (EEL_IS_CLIST (clist), NULL);

  if (column < 0 || column >= clist->columns)
    return NULL;

  if (clist->column[column].button)
    return GTK_BUTTON (clist->column[column].button)->child;

  return NULL;
}

void
eel_clist_set_column_justification (EelCList         *clist,
				    gint              column,
				    GtkJustification  justification)
{
  GtkWidget *alignment;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;

  clist->column[column].justification = justification;

  /* change the alinment of the button title if it's not a
   * custom widget */
  if (clist->column[column].title && clist->column[column].button)
    {
      alignment = GTK_BIN (clist->column[column].button)->child;

      if (alignment != NULL) {
	switch (clist->column[column].justification)
	  {
	  case GTK_JUSTIFY_LEFT:
	    gtk_alignment_set (GTK_ALIGNMENT (alignment), 0.0, 0.5, 0.0, 0.0);
	    break;
	    
	  case GTK_JUSTIFY_RIGHT:
	    gtk_alignment_set (GTK_ALIGNMENT (alignment), 1.0, 0.5, 0.0, 0.0);
	    break;
	    
	  case GTK_JUSTIFY_CENTER:
	    gtk_alignment_set (GTK_ALIGNMENT (alignment), 0.5, 0.5, 0.0, 0.0);
	    break;
	    
	  case GTK_JUSTIFY_FILL:
	    gtk_alignment_set (GTK_ALIGNMENT (alignment), 0.5, 0.5, 0.0, 0.0);
	    break;
	    
	  default:
	    break;
	  }
      }
    }

  if (CLIST_UNFROZEN (clist))
    EEL_CLIST_CLASS_FW (clist)->draw_all (clist);
}

void
eel_clist_set_column_visibility (EelCList *clist,
				 gint      column,
				 gboolean  visible)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;
  if (clist->column[column].visible == visible)
    return;

  /* don't hide last visible column */
  if (!visible)
    {
      gint i;
      gint vis_columns = 0;

      for (i = 0, vis_columns = 0; i < clist->columns && vis_columns < 2; i++)
	if (clist->column[i].visible)
	  vis_columns++;

      if (vis_columns < 2)
	return;
    }

  clist->column[column].visible = visible;

  if (clist->column[column].button)
    {
      if (visible)
	gtk_widget_show (clist->column[column].button);
      else
	gtk_widget_hide (clist->column[column].button);
    }
  
  gtk_widget_queue_resize (GTK_WIDGET(clist));
}

void
eel_clist_set_column_resizeable (EelCList *clist,
				 gint      column,
				 gboolean  resizeable)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;
  if (clist->column[column].resizeable == resizeable)
    return;

  clist->column[column].resizeable = resizeable;
  if (resizeable)
    clist->column[column].auto_resize = FALSE;

  if (GTK_WIDGET_VISIBLE (clist))
    size_allocate_title_buttons (clist);
}

void
eel_clist_set_column_auto_resize (EelCList *clist,
				  gint      column,
				  gboolean  auto_resize)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;
  if (clist->column[column].auto_resize == auto_resize)
    return;

  clist->column[column].auto_resize = auto_resize;
  if (auto_resize)
    {
      clist->column[column].resizeable = FALSE;
      if (!EEL_CLIST_AUTO_RESIZE_BLOCKED(clist))
	{
	  gint width;

	  width = eel_clist_optimal_column_width (clist, column);
	  eel_clist_set_column_width (clist, column, width);
	}
    }

  if (GTK_WIDGET_VISIBLE (clist))
    size_allocate_title_buttons (clist);
}

gint
eel_clist_columns_autosize (EelCList *clist)
{
  gint i;
  gint width;

  g_return_val_if_fail (clist != NULL, 0);
  g_return_val_if_fail (EEL_IS_CLIST (clist), 0);

  eel_clist_freeze (clist);
  width = 0;
  for (i = 0; i < clist->columns; i++)
    {
      eel_clist_set_column_width (clist, i,
				  eel_clist_optimal_column_width (clist, i));

      width += clist->column[i].width;
    }

  eel_clist_thaw (clist);
  return width;
}

gint
eel_clist_optimal_column_width (EelCList *clist,
				gint      column)
{
  GtkRequisition requisition;
  GList *list;
  gint width;

  g_return_val_if_fail (clist != NULL, 0);
  g_return_val_if_fail (EEL_CLIST (clist), 0);

  if (column < 0 || column > clist->columns)
    return 0;

  if (EEL_CLIST_SHOW_TITLES(clist) && clist->column[column].button)
    width = (clist->column[column].button->requisition.width)
#if 0
            + (CELL_SPACING + (2 * COLUMN_INSET))
#endif
		;
  else
    width = 0;

  for (list = clist->row_list; list; list = list->next)
    {
      EEL_CLIST_CLASS_FW (clist)->cell_size_request
	(clist, EEL_CLIST_ROW (list), column, &requisition);
      width = MAX (width, requisition.width);
    }

  return width;
}

void
eel_clist_set_column_width (EelCList *clist,
			    gint      column,
			    gint      width)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;

  gtk_signal_emit (GTK_OBJECT (clist), clist_signals[RESIZE_COLUMN],
		   column, width);
}

void
eel_clist_set_column_min_width (EelCList *clist,
				gint      column,
				gint      min_width)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;
  if (clist->column[column].min_width == min_width)
    return;

  if (clist->column[column].max_width >= 0  &&
      clist->column[column].max_width < min_width)
    clist->column[column].min_width = clist->column[column].max_width;
  else
    clist->column[column].min_width = min_width;

  if (clist->column[column].area.width < clist->column[column].min_width)
    eel_clist_set_column_width (clist, column,clist->column[column].min_width);
}

void
eel_clist_set_column_max_width (EelCList *clist,
				gint      column,
				gint      max_width)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;
  if (clist->column[column].max_width == max_width)
    return;

  if (clist->column[column].min_width >= 0 && max_width >= 0 &&
      clist->column[column].min_width > max_width)
    clist->column[column].max_width = clist->column[column].min_width;
  else
    clist->column[column].max_width = max_width;
  
  if (clist->column[column].area.width > clist->column[column].max_width)
    eel_clist_set_column_width (clist, column,clist->column[column].max_width);
}

/* PRIVATE COLUMN FUNCTIONS
 *   column_auto_resize
 *   real_resize_column
 *   abort_column_resize
 *   size_allocate_title_buttons
 *   size_allocate_columns
 *   list_requisition_width
 *   new_column_width
 *   column_button_create
 *   column_button_clicked
 *   column_title_passive_func
 */
static void
column_auto_resize (EelCList    *clist,
		    EelCListRow *clist_row,
		    gint         column,
		    gint         old_width)
{
  /* resize column if needed for auto_resize */
  GtkRequisition requisition;

  if (!clist->column[column].auto_resize ||
      EEL_CLIST_AUTO_RESIZE_BLOCKED(clist))
    return;

  if (clist_row)
    EEL_CLIST_CLASS_FW (clist)->cell_size_request (clist, clist_row,
						   column, &requisition);
  else
    requisition.width = 0;

  if (requisition.width > clist->column[column].width)
    eel_clist_set_column_width (clist, column, requisition.width);
  else if (requisition.width < old_width &&
	   old_width == clist->column[column].width)
    {
      GList *list;
      gint new_width = 0;

      /* run a "eel_clist_optimal_column_width" but break, if
       * the column doesn't shrink */
      if (EEL_CLIST_SHOW_TITLES(clist) && clist->column[column].button)
	new_width = (clist->column[column].button->requisition.width -
		     (CELL_SPACING + (2 * COLUMN_INSET)));
      else
	new_width = 0;

      for (list = clist->row_list; list; list = list->next)
	{
	  EEL_CLIST_CLASS_FW (clist)->cell_size_request
	    (clist, EEL_CLIST_ROW (list), column, &requisition);
	  new_width = MAX (new_width, requisition.width);
	  if (new_width == clist->column[column].width)
	    break;
	}
      if (new_width < clist->column[column].width)
	eel_clist_set_column_width
	  (clist, column, MAX (new_width, clist->column[column].min_width));
    }
}

static void
real_resize_column (EelCList *clist,
		    gint      column,
		    gint      width)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;
  
  if (width < MAX (COLUMN_MIN_WIDTH, clist->column[column].min_width))
    width = MAX (COLUMN_MIN_WIDTH, clist->column[column].min_width);
  if (clist->column[column].max_width >= 0 &&
      width > clist->column[column].max_width)
    width = clist->column[column].max_width;

  if (clist->column[column].width == width
      && clist->column[column].width_set)
    return;

  clist->column[column].width = width;
  clist->column[column].width_set = TRUE;

  /* FIXME: this is quite expensive to do if the widget hasn't
   *        been size_allocated yet, and pointless. Should
   *        a flag be kept
   */
  size_allocate_columns (clist, TRUE);
  size_allocate_title_buttons (clist);

  CLIST_REFRESH (clist);
}

static void
abort_column_resize (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (!EEL_CLIST_IN_DRAG(clist))
    return;

  EEL_CLIST_UNSET_FLAG (clist, CLIST_IN_DRAG);
  gtk_grab_remove (GTK_WIDGET (clist));
  gdk_pointer_ungrab (GDK_CURRENT_TIME);
  clist->drag_pos = -1;

  if (clist->x_drag >= 0 && clist->x_drag <= clist->clist_window_width - 1)
    draw_xor_line (clist);

  if (EEL_CLIST_ADD_MODE(clist))
    {
      gdk_gc_set_line_attributes (clist->xor_gc, 1, GDK_LINE_ON_OFF_DASH, 0,0);
      gdk_gc_set_dashes (clist->xor_gc, 0, "\4\4", 2);
    }
}

static void
size_allocate_title_buttons (EelCList *clist)
{
  GtkAllocation button_allocation;
  gint last_column;
  gint last_button = 0;
  gint i;

  if (!GTK_WIDGET_REALIZED (clist))
    return;

  button_allocation.x = clist->hoffset;
  button_allocation.y = 0;
  button_allocation.width = 0;
  button_allocation.height = clist->column_title_area.height;

  /* find last visible column */
  for (last_column = clist->columns - 1; last_column >= 0; last_column--)
    if (clist->column[last_column].visible)
      break;

  for (i = 0; i < last_column; i++)
    {
      if (!clist->column[i].visible)
	{
	  last_button = i + 1;
	  gdk_window_hide (clist->column[i].window);
	  continue;
	}

      button_allocation.width += (clist->column[i].area.width +
				  CELL_SPACING + 2 * COLUMN_INSET);

      if (!clist->column[i + 1].button)
	{
	  gdk_window_hide (clist->column[i].window);
	  continue;
	}

      if (clist->column[last_button].button) {
	gtk_widget_size_allocate (clist->column[last_button].button,
				  &button_allocation);
      }
      button_allocation.x += button_allocation.width;
      button_allocation.width = 0;

      if (clist->column[last_button].resizeable)
	{
	  gdk_window_show (clist->column[last_button].window);
	  gdk_window_move_resize (clist->column[last_button].window,
				  button_allocation.x - (DRAG_WIDTH / 2), 
				  0, DRAG_WIDTH,
				  clist->column_title_area.height);
	}
      else
	gdk_window_hide (clist->column[last_button].window);

      last_button = i + 1;
    }

  button_allocation.width += (clist->column[last_column].area.width +
			      2 * (CELL_SPACING + COLUMN_INSET));
  if (clist->column[last_button].button) {
    gtk_widget_size_allocate (clist->column[last_button].button,
			      &button_allocation);
  }

  if (clist->column[last_button].resizeable)
    {
      button_allocation.x += button_allocation.width;

      gdk_window_show (clist->column[last_button].window);
      gdk_window_move_resize (clist->column[last_button].window,
			      button_allocation.x - (DRAG_WIDTH / 2), 
			      0, DRAG_WIDTH, clist->column_title_area.height);
    }
  else
    gdk_window_hide (clist->column[last_button].window);
}

static void
size_allocate_columns (EelCList *clist,
		       gboolean  block_resize)
{
  gint xoffset = CELL_SPACING + COLUMN_INSET;
  gint last_column;
  gint i;

  /* find last visible column and calculate correct column width */
  for (last_column = clist->columns - 1;
       last_column >= 0 && !clist->column[last_column].visible; last_column--);

  if (last_column < 0)
    return;

  for (i = 0; i <= last_column; i++)
    {
      if (!clist->column[i].visible)
	continue;
      clist->column[i].area.x = xoffset;
      if (clist->column[i].width_set)
	{
	  if (!block_resize && EEL_CLIST_SHOW_TITLES(clist) &&
	      clist->column[i].auto_resize && clist->column[i].button)
	    {
	      gint width;

	      width = (clist->column[i].button->requisition.width -
		       (CELL_SPACING + (2 * COLUMN_INSET)));

	      if (width > clist->column[i].width)
		eel_clist_set_column_width (clist, i, width);
	    }

	  clist->column[i].area.width = clist->column[i].width;
	  xoffset += clist->column[i].width + CELL_SPACING + (2* COLUMN_INSET);
	}
      else if (EEL_CLIST_SHOW_TITLES(clist) && clist->column[i].button)
	{
	  clist->column[i].area.width =
	    clist->column[i].button->requisition.width -
	    (CELL_SPACING + (2 * COLUMN_INSET));
	  xoffset += clist->column[i].button->requisition.width;
	}
    }

  clist->column[last_column].area.width = clist->column[last_column].area.width
    + MAX (0, clist->clist_window_width + COLUMN_INSET - xoffset);
}

static gint
list_requisition_width (EelCList *clist) 
{
  gint width = CELL_SPACING;
  gint i;

  for (i = clist->columns - 1; i >= 0; i--)
    {
      if (!clist->column[i].visible)
	continue;

      if (clist->column[i].width_set)
	width += clist->column[i].width + CELL_SPACING + (2 * COLUMN_INSET);
      else if (EEL_CLIST_SHOW_TITLES(clist) && clist->column[i].button)
	width += clist->column[i].button->requisition.width;
    }

  return width;
}

/* this function returns the new width of the column being resized given
 * the column and x position of the cursor; the x cursor position is passed
 * in as a pointer and automagicly corrected if it's beyond min/max limits */
static gint
new_column_width (EelCList *clist,
		  gint      column,
		  gint     *x)
{
  gint xthickness = GTK_WIDGET (clist)->style->klass->xthickness;
  gint width;
  gint cx;
  gint dx;
  gint last_column;

  /* first translate the x position from widget->window
   * to clist->clist_window */
  cx = *x - xthickness;

  for (last_column = clist->columns - 1;
       last_column >= 0 && !clist->column[last_column].visible; last_column--);

  /* calculate new column width making sure it doesn't end up
   * less than the minimum width */
  dx = (COLUMN_LEFT_XPIXEL (clist, column) + COLUMN_INSET +
	(column < last_column) * CELL_SPACING);
  width = cx - dx;

  if (width < MAX (COLUMN_MIN_WIDTH, clist->column[column].min_width))
    {
      width = MAX (COLUMN_MIN_WIDTH, clist->column[column].min_width);
      cx = dx + width;
      *x = cx + xthickness;
    }
  else if (clist->column[column].max_width >= COLUMN_MIN_WIDTH &&
	   width > clist->column[column].max_width)
    {
      width = clist->column[column].max_width;
      cx = dx + clist->column[column].max_width;
      *x = cx + xthickness;
    }      

  if (cx < 0 || cx > clist->clist_window_width)
    *x = -1;

  return width;
}

static void
column_button_create (EelCList *clist,
		      gint      column)
{
  GtkWidget *button;

  gtk_widget_push_composite_child ();
  button = clist->column[column].button = gtk_button_new ();
  gtk_widget_pop_composite_child ();

  if (GTK_WIDGET_REALIZED (clist) && clist->title_window)
    gtk_widget_set_parent_window (clist->column[column].button,
				  clist->title_window);
  gtk_widget_set_parent (button, GTK_WIDGET (clist));

  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      (GtkSignalFunc) column_button_clicked,
		      (gpointer) clist);
  gtk_widget_show (button);
}

static void
column_button_clicked (GtkWidget *widget,
		       gpointer   data)
{
  gint i;
  EelCList *clist;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (data));

  clist = EEL_CLIST (data);

  /* find the column who's button was pressed */
  for (i = 0; i < clist->columns; i++)
    if (clist->column[i].button == widget)
      break;

  gtk_signal_emit (GTK_OBJECT (clist), clist_signals[CLICK_COLUMN], i);
}

static gint
column_title_passive_func (GtkWidget *widget, 
			   GdkEvent  *event,
			   gpointer   data)
{
  g_return_val_if_fail (event != NULL, FALSE);
  
  switch (event->type)
    {
    case GDK_MOTION_NOTIFY:
    case GDK_BUTTON_PRESS:
    case GDK_2BUTTON_PRESS:
    case GDK_3BUTTON_PRESS:
    case GDK_BUTTON_RELEASE:
    case GDK_ENTER_NOTIFY:
    case GDK_LEAVE_NOTIFY:
      return TRUE;
    default:
      break;
    }
  return FALSE;
}


/* PUBLIC CELL FUNCTIONS
 *   eel_clist_get_cell_type
 *   eel_clist_set_text
 *   eel_clist_get_text
 *   eel_clist_set_pixbuf
 *   eel_clist_get_pixbuf
 *   eel_clist_set_pixtext
 *   eel_clist_get_pixtext
 *   eel_clist_set_shift
 */
EelCellType 
eel_clist_get_cell_type (EelCList *clist,
			 gint      row,
			 gint      column)
{
  EelCListRow *clist_row;

  g_return_val_if_fail (clist != NULL, -1);
  g_return_val_if_fail (EEL_IS_CLIST (clist), -1);

  if (row < 0 || row >= clist->rows)
    return -1;
  if (column < 0 || column >= clist->columns)
    return -1;

  clist_row = ROW_ELEMENT (clist, row)->data;

  return clist_row->cell[column].type;
}

void
eel_clist_set_text (EelCList    *clist,
		    gint         row,
		    gint         column,
		    const gchar *text)
{
  EelCListRow *clist_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row >= clist->rows)
    return;
  if (column < 0 || column >= clist->columns)
    return;

  clist_row = ROW_ELEMENT (clist, row)->data;

  /* if text is null, then the cell is empty */
  if (EEL_CLIST_CLASS_FW (clist)->set_cell_contents
      (clist, clist_row, column, EEL_CELL_TEXT, text, 0, NULL))
    {
      /* redraw the list if it's not frozen */
      if (CLIST_UNFROZEN (clist))
	{
	  if (eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE)
	    EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row, clist_row);
	}
    }
}

gint
eel_clist_get_text (EelCList  *clist,
		    gint       row,
		    gint       column,
		    gchar    **text)
{
  EelCListRow *clist_row;

  g_return_val_if_fail (clist != NULL, 0);
  g_return_val_if_fail (EEL_IS_CLIST (clist), 0);

  if (row < 0 || row >= clist->rows)
    return 0;
  if (column < 0 || column >= clist->columns)
    return 0;

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (clist_row->cell[column].type != EEL_CELL_TEXT
      && clist_row->cell[column].type != EEL_CELL_LINK_TEXT)
    return 0;

  if (text)
    *text = EEL_CELL_TEXT (clist_row->cell[column])->text;

  return 1;
}

void
eel_clist_set_pixbuf (EelCList  *clist,
		      gint       row,
		      gint       column,
			   GdkPixbuf *pixbuf)
{
  EelCListRow *clist_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row >= clist->rows)
    return;
  if (column < 0 || column >= clist->columns)
    return;

  clist_row = ROW_ELEMENT (clist, row)->data;
  
  if (EEL_CLIST_CLASS_FW (clist)->set_cell_contents
      (clist, clist_row, column, EEL_CELL_PIXBUF, NULL, 0, pixbuf))
    {
      /* redraw the list if it's not frozen */
      if (CLIST_UNFROZEN (clist))
	{
	  if (eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE)
	    EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row, clist_row);
	}
    }
}

gint
eel_clist_get_pixbuf (EelCList   *clist,
		      gint        row,
		      gint        column,
		      GdkPixbuf **pixbuf)
{
  EelCListRow *clist_row;

  g_return_val_if_fail (clist != NULL, 0);
  g_return_val_if_fail (EEL_IS_CLIST (clist), 0);

  if (row < 0 || row >= clist->rows)
    return 0;
  if (column < 0 || column >= clist->columns)
    return 0;

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (clist_row->cell[column].type != EEL_CELL_PIXBUF)
    return 0;

  if (pixbuf)
  {
    *pixbuf = EEL_CELL_PIXBUF (clist_row->cell[column])->pixbuf;
  }

  return 1;
}

void
eel_clist_set_pixtext (EelCList    *clist,
		       gint         row,
		       gint         column,
		       const gchar *text,
		       guint8       spacing,
		       GdkPixbuf   *pixbuf)
{
  EelCListRow *clist_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row >= clist->rows)
    return;
  if (column < 0 || column >= clist->columns)
    return;

  clist_row = ROW_ELEMENT (clist, row)->data;
  
  if (EEL_CLIST_CLASS_FW (clist)->set_cell_contents
      (clist, clist_row, column, EEL_CELL_PIXTEXT, text, spacing, pixbuf))
    {
      /* redraw the list if it's not frozen */
      if (CLIST_UNFROZEN (clist))
	{
	  if (eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE)
	    EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row, clist_row);
	}
    }
}

gint
eel_clist_get_pixtext (EelCList   *clist,
		       gint        row,
		       gint        column,
		       gchar     **text,
		       guint8     *spacing,
		       GdkPixbuf **pixbuf)
{
  EelCListRow *clist_row;

  g_return_val_if_fail (clist != NULL, 0);
  g_return_val_if_fail (EEL_IS_CLIST (clist), 0);

  if (row < 0 || row >= clist->rows)
    return 0;
  if (column < 0 || column >= clist->columns)
    return 0;

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (clist_row->cell[column].type != EEL_CELL_PIXTEXT)
    return 0;

  if (text)
    *text = EEL_CELL_PIXTEXT (clist_row->cell[column])->text;
  if (spacing)
    *spacing = EEL_CELL_PIXTEXT (clist_row->cell[column])->spacing;
  if (pixbuf)
    *pixbuf = EEL_CELL_PIXTEXT (clist_row->cell[column])->pixbuf;

  return 1;
}

void
eel_clist_set_shift (EelCList *clist,
		     gint      row,
		     gint      column,
		     gint      vertical,
		     gint      horizontal)
{
  GtkRequisition requisition = { 0 };
  EelCListRow *clist_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row >= clist->rows)
    return;
  if (column < 0 || column >= clist->columns)
    return;

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (clist->column[column].auto_resize &&
      !EEL_CLIST_AUTO_RESIZE_BLOCKED(clist))
    EEL_CLIST_CLASS_FW (clist)->cell_size_request (clist, clist_row,
						   column, &requisition);

  clist_row->cell[column].vertical = vertical;
  clist_row->cell[column].horizontal = horizontal;

  column_auto_resize (clist, clist_row, column, requisition.width);

  if (CLIST_UNFROZEN (clist) && eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE)
    EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row, clist_row);
}

/* PRIVATE CELL FUNCTIONS
 *   set_cell_contents
 *   cell_size_request
 */
static gboolean
set_cell_contents (EelCList    *clist,
		   EelCListRow *clist_row,
		   gint         column,
		   EelCellType  type,
		   const gchar *text,
		   guint8       spacing,
		   GdkPixbuf   *pixbuf)
{
  GtkRequisition requisition;

  g_return_val_if_fail (EEL_IS_CLIST (clist), FALSE);
  g_return_val_if_fail (clist_row != NULL, FALSE);

  if (type == clist_row->cell[column].type)
    {
      switch (type)
	{
	case EEL_CELL_EMPTY:
	  return FALSE;
	case EEL_CELL_TEXT:
	case EEL_CELL_LINK_TEXT:
	  if (EEL_CELL_TEXT (clist_row->cell[column])->text == NULL)
	    {
	      if (text == NULL)
		return FALSE;
	    }
	  else
	    {
	      if (text != NULL && strcmp (EEL_CELL_TEXT (clist_row->cell[column])->text, text) == 0)
		return FALSE;
	    }
	  break;
	case EEL_CELL_PIXBUF:
	  if (pixbuf == EEL_CELL_PIXBUF (clist_row->cell[column])->pixbuf)
	    return FALSE;
	  break;
	case EEL_CELL_PIXTEXT:
	  if (pixbuf == EEL_CELL_PIXTEXT (clist_row->cell[column])->pixbuf)
	    {
	      if (EEL_CELL_PIXTEXT (clist_row->cell[column])->text == NULL)
		{
		  if (text == NULL)
		    return FALSE;
		}
	      else
		{
		  if (text != NULL && strcmp (EEL_CELL_PIXTEXT (clist_row->cell[column])->text, text) == 0)
		    return FALSE;
		}
	    }
	  break;
	case EEL_CELL_WIDGET:
	  /* unimplemented */
	  break;
	default:
	  break;
	}
    }

  if (clist->column[column].auto_resize &&
      !EEL_CLIST_AUTO_RESIZE_BLOCKED(clist))
    EEL_CLIST_CLASS_FW (clist)->cell_size_request (clist, clist_row,
							column, &requisition);

  switch (clist_row->cell[column].type)
    {
    case EEL_CELL_EMPTY:
      break;
    case EEL_CELL_TEXT:
    case EEL_CELL_LINK_TEXT:
      g_free (EEL_CELL_TEXT (clist_row->cell[column])->text);
      break;
    case EEL_CELL_PIXBUF:
      gdk_pixbuf_unref (EEL_CELL_PIXBUF (clist_row->cell[column])->pixbuf);
      break;
    case EEL_CELL_PIXTEXT:
      g_free (EEL_CELL_PIXTEXT (clist_row->cell[column])->text);
      gdk_pixbuf_unref (EEL_CELL_PIXTEXT (clist_row->cell[column])->pixbuf);
      break;
    case EEL_CELL_WIDGET:
      /* unimplemented */
      break;
    default:
      break;
    }

  clist_row->cell[column].type = EEL_CELL_EMPTY;

  switch (type)
    {
    case EEL_CELL_TEXT:
    case EEL_CELL_LINK_TEXT:
      if (text)
	{
	  clist_row->cell[column].type = EEL_CELL_TEXT;
	  EEL_CELL_TEXT (clist_row->cell[column])->text = g_strdup (text);
	}
      break;
    case EEL_CELL_PIXBUF:
      if (pixbuf)
	{
	  clist_row->cell[column].type = EEL_CELL_PIXBUF;
	  EEL_CELL_PIXBUF (clist_row->cell[column])->pixbuf = gdk_pixbuf_ref (pixbuf);
	}
      break;
    case EEL_CELL_PIXTEXT:
      if (text && pixbuf)
	{
	  clist_row->cell[column].type = EEL_CELL_PIXTEXT;
	  EEL_CELL_PIXTEXT (clist_row->cell[column])->text = g_strdup (text);
	  EEL_CELL_PIXTEXT (clist_row->cell[column])->spacing = spacing;
	  EEL_CELL_PIXTEXT (clist_row->cell[column])->pixbuf = gdk_pixbuf_ref (pixbuf);
	}
      break;
    default:
      break;
    }

  if (clist->column[column].auto_resize &&
      !EEL_CLIST_AUTO_RESIZE_BLOCKED(clist))
    column_auto_resize (clist, clist_row, column, requisition.width);

  return TRUE;
}

static void
cell_size_request (EelCList       *clist,
		   EelCListRow    *clist_row,
		   gint            column,
		   GtkRequisition *requisition)
{
  GtkStyle *style;
  gint width;
  gint height;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));
  g_return_if_fail (requisition != NULL);

  get_cell_style (clist, clist_row, GTK_STATE_NORMAL, column, &style,
		  NULL, NULL);

  switch (clist_row->cell[column].type)
    {
    case EEL_CELL_TEXT:
    case EEL_CELL_LINK_TEXT:
      requisition->width =
	gdk_string_width (style->font,
			  EEL_CELL_TEXT (clist_row->cell[column])->text);
      requisition->height = style->font->ascent + style->font->descent;
      break;
    case EEL_CELL_PIXTEXT:
      width = gdk_pixbuf_get_height (EEL_CELL_PIXTEXT (clist_row->cell[column])->pixbuf);
      requisition->width = width +
	EEL_CELL_PIXTEXT (clist_row->cell[column])->spacing +
	gdk_string_width (style->font,
			  EEL_CELL_TEXT (clist_row->cell[column])->text);

      height = gdk_pixbuf_get_height (EEL_CELL_PIXTEXT (clist_row->cell[column])->pixbuf);
      requisition->height = MAX (style->font->ascent + style->font->descent,
				 height);
      break;
    case EEL_CELL_PIXBUF:
      requisition->width = gdk_pixbuf_get_width (EEL_CELL_PIXBUF (clist_row->cell[column])->pixbuf);
      requisition->height = gdk_pixbuf_get_height (EEL_CELL_PIXBUF (clist_row->cell[column])->pixbuf);
      break;
    default:
      requisition->width  = 0;
      requisition->height = 0;
      break;
    }

  requisition->width  += clist_row->cell[column].horizontal;
  requisition->height += clist_row->cell[column].vertical;
}

/* PUBLIC INSERT/REMOVE ROW FUNCTIONS
 *   eel_clist_prepend
 *   eel_clist_append
 *   eel_clist_insert
 *   eel_clist_remove
 *   eel_clist_clear
 */
gint
eel_clist_prepend (EelCList    *clist,
		   gchar       *text[])
{
  g_return_val_if_fail (clist != NULL, -1);
  g_return_val_if_fail (EEL_IS_CLIST (clist), -1);
  g_return_val_if_fail (text != NULL, -1);

  return EEL_CLIST_CLASS_FW (clist)->insert_row (clist, 0, text);
}

gint
eel_clist_append (EelCList    *clist,
		  gchar       *text[])
{
  g_return_val_if_fail (clist != NULL, -1);
  g_return_val_if_fail (EEL_IS_CLIST (clist), -1);
  g_return_val_if_fail (text != NULL, -1);

  return EEL_CLIST_CLASS_FW (clist)->insert_row (clist, clist->rows, text);
}

gint
eel_clist_insert (EelCList    *clist,
		  gint         row,
		  gchar       *text[])
{
  g_return_val_if_fail (clist != NULL, -1);
  g_return_val_if_fail (EEL_IS_CLIST (clist), -1);
  g_return_val_if_fail (text != NULL, -1);

  if (row < 0 || row > clist->rows)
    row = clist->rows;

  return EEL_CLIST_CLASS_FW (clist)->insert_row (clist, row, text);
}

void
eel_clist_remove (EelCList *clist,
		  gint      row)
{
  EEL_CLIST_CLASS_FW (clist)->remove_row (clist, row);
}


void
eel_clist_clear (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));
  
  EEL_CLIST_CLASS_FW (clist)->clear (clist);
}

/* PRIVATE INSERT/REMOVE ROW FUNCTIONS
 *   get_ascending_sorted_list_position_for_new_row   
 *   get_descending_sorted_list_position_for_new_row
 *   real_insert_row
 *   real_remove_row
 *   real_clear
 *   real_row_move
 */

/* Returns a row number >= 0 */
static gint
get_ascending_sorted_list_position_for_new_row (EelCList *clist,
					    EelCListRow *new_row)
{
  int current_row, high_row_bound, low_row_bound;
  int compare_result;
  GList *current_row_node;

  if (clist->rows == 0) {
    return 0;
  }

  current_row = clist->rows / 2;
  high_row_bound = clist->rows;
  low_row_bound = 0;

  while (TRUE) {
    current_row_node = g_list_nth (clist->row_list, current_row);
    compare_result = clist->compare (clist, 
				     new_row,
				     EEL_CLIST_ROW (current_row_node));
    
    if (compare_result == 0 ||
	(compare_result > 0 && high_row_bound == current_row + 1)) {
      /* GList starts at 0, rows at 1 */
      return current_row + 1;
    }
    else if (compare_result > 0) {
      g_assert (high_row_bound > current_row);
      low_row_bound = current_row;
      current_row = (current_row + high_row_bound) / 2;
    }
    else if (compare_result < 0) {
      /* Check for case of new row being less than the first row */
      if (current_row == 0) {
	return 0;
      }
      g_assert (low_row_bound < current_row);
      high_row_bound = current_row;
      current_row = (current_row + low_row_bound) / 2;
    }
  }

  g_assert_not_reached ();
  return -1;
}

/* Returns a row number >= 0 */
static gint
get_descending_sorted_list_position_for_new_row (EelCList *clist,
						 EelCListRow *new_row)
{
  int current_row, high_row_bound, low_row_bound;
  int compare_result;
  GList *current_row_node;

  if (clist->rows == 0) {
    return 0;
  }

  current_row = clist->rows / 2;
  high_row_bound = clist->rows;
  low_row_bound = 0;

  while (TRUE) {
    current_row_node = g_list_nth (clist->row_list, current_row);
    compare_result = clist->compare (clist, 
				     new_row,
				     EEL_CLIST_ROW (current_row_node));
    
    if (compare_result == 0 ||
	(compare_result < 0 && high_row_bound == current_row + 1)) {
      /* GList starts at 0, rows at 1 */
      return current_row + 1;
    }
    else if (compare_result < 0) {
      g_assert (high_row_bound > current_row);
      low_row_bound = current_row;
      current_row = (current_row + high_row_bound) / 2;
    }
    else if (compare_result > 0) {
      /* Check for case of new row being less than the first row */
      if (current_row == 0) {
	return 0;
      }
      g_assert (low_row_bound < current_row);
      high_row_bound = current_row;
      current_row = (current_row + low_row_bound) / 2;
    }
  }

  g_assert_not_reached ();
  return -1;
}



static gint
real_insert_row (EelCList *clist,
		 gint      row,
		 gchar    *text[])
{
  gint i;
  EelCListRow *clist_row;

  g_return_val_if_fail (clist != NULL, -1);
  g_return_val_if_fail (EEL_IS_CLIST (clist), -1);
  g_return_val_if_fail (text != NULL, -1);

  /* return if out of bounds */
  if (row < 0 || row > clist->rows)
    return -1;

  /* create the row */
  clist_row = row_new (clist);

  /* set the text in the row's columns */
  for (i = 0; i < clist->columns; i++)
    if (text[i])
      EEL_CLIST_CLASS_FW (clist)->set_cell_contents
	(clist, clist_row, i, EEL_CELL_TEXT, text[i], 0, NULL);

  if (!clist->rows)
    {
      clist->row_list = g_list_append (clist->row_list, clist_row);
      clist->row_list_end = clist->row_list;
    }
  else
    {
      if (EEL_CLIST_AUTO_SORT(clist))   /* override insertion pos */
	{
	  GList *work;

	  row = 0;
	  work = clist->row_list;
	  
	  if (clist->sort_type == GTK_SORT_ASCENDING)
	    {
	      row = get_ascending_sorted_list_position_for_new_row (clist, clist_row);
	    }
	  else
	    {
	      row = get_descending_sorted_list_position_for_new_row (clist, clist_row);
	    }
	}

      /* reset the row end pointer if we're inserting at the end of the list */
      if (row == clist->rows)
	clist->row_list_end = (g_list_append (clist->row_list_end,
					      clist_row))->next;
      else
	clist->row_list = g_list_insert (clist->row_list, clist_row, row);
      
    }
  clist->rows++;
  
  if (row < ROW_FROM_YPIXEL (clist, 0))
    clist->voffset -= (clist->row_height + CELL_SPACING);

  /* syncronize the selection list */
  sync_selection (clist, row, SYNC_INSERT);

  if (clist->rows == 1)
    {
      clist->focus_row = 0;
      if (clist->selection_mode == GTK_SELECTION_BROWSE)
	eel_clist_select_row (clist, 0, -1);
    }

  /* redraw the list if it isn't frozen */
  if (CLIST_UNFROZEN (clist))
    {
      adjust_adjustments (clist, FALSE);

      if (eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE)
        EEL_CLIST_CLASS_FW (clist)->draw_all (clist);
    }

  return row;
}

static void
real_remove_row (EelCList *clist,
		 gint      row)
{
  gint was_visible, was_selected;
  GList *list;
  EelCListRow *clist_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  /* return if out of bounds */
  if (row < 0 || row > (clist->rows - 1))
    return;

  was_visible = (eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE);
  was_selected = 0;

  /* get the row we're going to delete */
  list = ROW_ELEMENT (clist, row);
  g_assert (list != NULL);
  clist_row = list->data;

  /* if we're removing a selected row, we have to make sure
   * it's properly unselected, and then sync up the clist->selected
   * list to reflect the deincrimented indexies of rows after the
   * removal */
  if (clist_row->state == GTK_STATE_SELECTED)
    gtk_signal_emit (GTK_OBJECT (clist), clist_signals[UNSELECT_ROW],
		     row, -1, NULL);

  /* reset the row end pointer if we're removing at the end of the list */
  clist->rows--;
  if (clist->row_list == list)
    clist->row_list = g_list_next (list);
  if (clist->row_list_end == list)
    clist->row_list_end = g_list_previous (list);
  g_list_remove (list, clist_row);

  /*if (clist->focus_row >=0 &&
      (row <= clist->focus_row || clist->focus_row >= clist->rows))
      clist->focus_row--;*/

  if (row < ROW_FROM_YPIXEL (clist, 0))
    clist->voffset += clist->row_height + CELL_SPACING;

  sync_selection (clist, row, SYNC_REMOVE);

  if (clist->selection_mode == GTK_SELECTION_BROWSE && !clist->selection &&
      clist->focus_row >= 0)
    gtk_signal_emit (GTK_OBJECT (clist), clist_signals[SELECT_ROW],
		     clist->focus_row, -1, NULL);

  /* toast the row */
  row_delete (clist, clist_row);

  /* redraw the row if it isn't frozen */
  if (CLIST_UNFROZEN (clist))
    {
      adjust_adjustments (clist, FALSE);

      if (was_visible)
        EEL_CLIST_CLASS_FW (clist)->draw_all (clist);
    }
}

static void
real_clear (EelCList *clist)
{
  GList *list;
  GList *free_list;
  gint i;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  /* free up the selection list */
  g_list_free (clist->selection);
  g_list_free (clist->undo_selection);
  g_list_free (clist->undo_unselection);

  clist->selection = NULL;
  clist->selection_end = NULL;
  clist->undo_selection = NULL;
  clist->undo_unselection = NULL;
  clist->voffset = 0;
  clist->focus_row = -1;
  clist->anchor = -1;
  clist->undo_anchor = -1;
  clist->anchor_state = GTK_STATE_SELECTED;
  clist->drag_pos = -1;

  /* remove all the rows */
  EEL_CLIST_SET_FLAG (clist, CLIST_AUTO_RESIZE_BLOCKED);
  free_list = clist->row_list;
  clist->row_list = NULL;
  clist->row_list_end = NULL;
  clist->rows = 0;
  for (list = free_list; list; list = list->next)
    row_delete (clist, EEL_CLIST_ROW (list));
  g_list_free (free_list);
  EEL_CLIST_UNSET_FLAG (clist, CLIST_AUTO_RESIZE_BLOCKED);
  for (i = 0; i < clist->columns; i++)
    if (clist->column[i].auto_resize)
      {
	if (EEL_CLIST_SHOW_TITLES(clist) && clist->column[i].button)
	  eel_clist_set_column_width
	    (clist, i, (clist->column[i].button->requisition.width -
			(CELL_SPACING + (2 * COLUMN_INSET))));
	else
	  eel_clist_set_column_width (clist, i, 0);
      }
  /* zero-out the scrollbars */
  if (clist->vadjustment)
    {
      gtk_adjustment_set_value (clist->vadjustment, 0.0);
      CLIST_REFRESH (clist);
    }
  else
    gtk_widget_queue_resize (GTK_WIDGET (clist));
}

static void
real_row_move (EelCList *clist,
	       gint      source_row,
	       gint      dest_row)
{
  EelCListRow *clist_row;
  GList *list;
  gint first, last;
  gint d;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (EEL_CLIST_AUTO_SORT(clist))
    return;

  if (source_row < 0 || source_row >= clist->rows ||
      dest_row   < 0 || dest_row   >= clist->rows ||
      source_row == dest_row)
    return;

  /* unlink source row */
  clist_row = ROW_ELEMENT (clist, source_row)->data;
  if (source_row == clist->rows - 1)
    clist->row_list_end = clist->row_list_end->prev;
  clist->row_list = g_list_remove (clist->row_list, clist_row);
  clist->rows--;

  /* relink source row */
  clist->row_list = g_list_insert (clist->row_list, clist_row, dest_row);
  if (dest_row == clist->rows)
    clist->row_list_end = clist->row_list_end->next;
  clist->rows++;

  /* sync selection */
  if (source_row > dest_row)
    {
      first = dest_row;
      last  = source_row;
      d = 1;
    }
  else
    {
      first = source_row;
      last  = dest_row;
      d = -1;
    }

  for (list = clist->selection; list; list = list->next)
    {
      if (list->data == GINT_TO_POINTER (source_row))
	list->data = GINT_TO_POINTER (dest_row);
      else if (first <= GPOINTER_TO_INT (list->data) &&
	       last >= GPOINTER_TO_INT (list->data))
	list->data = GINT_TO_POINTER (GPOINTER_TO_INT (list->data) + d);
    }
  
  if (clist->focus_row == source_row)
    clist->focus_row = dest_row;
  else if (clist->focus_row > first)
    clist->focus_row += d;

  CLIST_REFRESH (clist);
}

/* PUBLIC ROW FUNCTIONS
 *   eel_clist_moveto
 *   eel_clist_set_row_height
 *   eel_clist_set_row_data
 *   eel_clist_set_row_data_full
 *   eel_clist_get_row_data
 *   eel_clist_find_row_from_data
 *   eel_clist_swap_rows
 *   eel_clist_row_move
 *   eel_clist_row_is_visible
 *   eel_clist_set_foreground
 *   eel_clist_set_background
 */
void
eel_clist_moveto (EelCList *clist,
		  gint      row,
		  gint      column,
		  gfloat    row_align,
		  gfloat    col_align)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < -1 || row >= clist->rows)
    return;
  if (column < -1 || column >= clist->columns)
    return;

  row_align = CLAMP (row_align, 0, 1);
  col_align = CLAMP (col_align, 0, 1);

  /* adjust horizontal scrollbar */
  if (clist->hadjustment && column >= 0)
    {
      gint x;

      x = (COLUMN_LEFT (clist, column) - CELL_SPACING - COLUMN_INSET -
	   (col_align * (clist->clist_window_width - 2 * COLUMN_INSET -
			 CELL_SPACING - clist->column[column].area.width)));
      if (x < 0)
	gtk_adjustment_set_value (clist->hadjustment, 0.0);
      else if (x > LIST_WIDTH (clist) - clist->clist_window_width)
	gtk_adjustment_set_value 
	  (clist->hadjustment, LIST_WIDTH (clist) - clist->clist_window_width);
      else
	gtk_adjustment_set_value (clist->hadjustment, x);
    }

  /* adjust vertical scrollbar */
  if (clist->vadjustment && row >= 0)
    move_vertical (clist, row, row_align);
}

void
eel_clist_set_row_height (EelCList *clist,
			  guint     height)
{
  GtkWidget *widget;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  widget = GTK_WIDGET (clist);

  if (height > 0)
    {
      clist->row_height = height;
      EEL_CLIST_SET_FLAG (clist, CLIST_ROW_HEIGHT_SET);
    }
  else
    {
      EEL_CLIST_UNSET_FLAG (clist, CLIST_ROW_HEIGHT_SET);
      clist->row_height = 0;
    }

  if (GTK_WIDGET_REALIZED (clist))
    {
      if (!EEL_CLIST_ROW_HEIGHT_SET(clist))
	{
	  clist->row_height = (widget->style->font->ascent +
			       widget->style->font->descent + 1);
	  clist->row_center_offset = widget->style->font->ascent + 1.5;
	}
      else
	clist->row_center_offset = 1.5 + (clist->row_height +
					  widget->style->font->ascent -
					  widget->style->font->descent - 1) / 2;
    }
      
  CLIST_REFRESH (clist);
}

void
eel_clist_set_row_data (EelCList *clist,
			gint      row,
			gpointer  data)
{
  eel_clist_set_row_data_full (clist, row, data, NULL);
}

void
eel_clist_set_row_data_full (EelCList         *clist,
			     gint              row,
			     gpointer          data,
			     GtkDestroyNotify  destroy)
{
  EelCListRow *clist_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row > (clist->rows - 1))
    return;

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (clist_row->destroy)
    clist_row->destroy (clist_row->data);
  
  clist_row->data = data;
  clist_row->destroy = destroy;
}

gpointer
eel_clist_get_row_data (EelCList *clist,
			gint      row)
{
  EelCListRow *clist_row;

  g_return_val_if_fail (clist != NULL, NULL);
  g_return_val_if_fail (EEL_IS_CLIST (clist), NULL);

  if (row < 0 || row > (clist->rows - 1))
    return NULL;

  clist_row = ROW_ELEMENT (clist, row)->data;
  return clist_row->data;
}

gint
eel_clist_find_row_from_data (EelCList *clist,
			      gpointer  data)
{
  GList *list;
  gint n;

  g_return_val_if_fail (clist != NULL, -1);
  g_return_val_if_fail (EEL_IS_CLIST (clist), -1);

  for (n = 0, list = clist->row_list; list; n++, list = list->next)
    if (EEL_CLIST_ROW (list)->data == data)
      return n;

  return -1;
}

void 
eel_clist_swap_rows (EelCList *clist,
		     gint      row1, 
		     gint      row2)
{
  gint first, last;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));
  g_return_if_fail (row1 != row2);

  if (EEL_CLIST_AUTO_SORT(clist))
    return;

  eel_clist_freeze (clist);

  first = MIN (row1, row2);
  last  = MAX (row1, row2);

  eel_clist_row_move (clist, last, first);
  eel_clist_row_move (clist, first + 1, last);
  
  eel_clist_thaw (clist);
}

void
eel_clist_row_move (EelCList *clist,
		    gint      source_row,
		    gint      dest_row)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (EEL_CLIST_AUTO_SORT(clist))
    return;

  if (source_row < 0 || source_row >= clist->rows ||
      dest_row   < 0 || dest_row   >= clist->rows ||
      source_row == dest_row)
    return;

  gtk_signal_emit (GTK_OBJECT (clist), clist_signals[ROW_MOVE],
		   source_row, dest_row);
}

GtkVisibility
eel_clist_row_is_visible (EelCList *clist,
			  gint      row)
{
  gint top;

  g_return_val_if_fail (clist != NULL, 0);
  g_return_val_if_fail (EEL_IS_CLIST (clist), 0);

  if (row < 0 || row >= clist->rows)
    return GTK_VISIBILITY_NONE;

  if (clist->row_height == 0)
    return GTK_VISIBILITY_NONE;

  if (row < ROW_FROM_YPIXEL (clist, 0))
    return GTK_VISIBILITY_NONE;

  if (row > ROW_FROM_YPIXEL (clist, clist->clist_window_height))
    return GTK_VISIBILITY_NONE;

  top = ROW_TOP_YPIXEL (clist, row);

  if ((top < 0)
      || ((top + clist->row_height) >= clist->clist_window_height))
    return GTK_VISIBILITY_PARTIAL;

  return GTK_VISIBILITY_FULL;
}

void
eel_clist_set_foreground (EelCList *clist,
			  gint      row,
			  GdkColor *color)
{
  EelCListRow *clist_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row >= clist->rows)
    return;

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (color)
    {
      clist_row->foreground = *color;
      clist_row->fg_set = TRUE;
      if (GTK_WIDGET_REALIZED (clist))
	gdk_color_alloc (gtk_widget_get_colormap (GTK_WIDGET (clist)),
			 &clist_row->foreground);
    }
  else
    clist_row->fg_set = FALSE;

  if (CLIST_UNFROZEN (clist) && eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE)
    EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row, clist_row);
}

void
eel_clist_set_background (EelCList *clist,
			  gint      row,
			  GdkColor *color)
{
  EelCListRow *clist_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row >= clist->rows)
    return;

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (color)
    {
      clist_row->background = *color;
      clist_row->bg_set = TRUE;
      if (GTK_WIDGET_REALIZED (clist))
	gdk_color_alloc (gtk_widget_get_colormap (GTK_WIDGET (clist)),
			 &clist_row->background);
    }
  else
    clist_row->bg_set = FALSE;

  if (CLIST_UNFROZEN (clist)
      && (eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE))
    EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row, clist_row);
}

/* PUBLIC ROW/CELL STYLE FUNCTIONS
 *   eel_clist_set_cell_style
 *   eel_clist_get_cell_style
 *   eel_clist_set_row_style
 *   eel_clist_get_row_style
 */
void
eel_clist_set_cell_style (EelCList *clist,
			  gint      row,
			  gint      column,
			  GtkStyle *style)
{
  GtkRequisition requisition = { 0 };
  EelCListRow *clist_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row >= clist->rows)
    return;
  if (column < 0 || column >= clist->columns)
    return;

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (clist_row->cell[column].style == style)
    return;

  if (clist->column[column].auto_resize &&
      !EEL_CLIST_AUTO_RESIZE_BLOCKED(clist))
    EEL_CLIST_CLASS_FW (clist)->cell_size_request (clist, clist_row,
						   column, &requisition);

  if (clist_row->cell[column].style)
    {
      if (GTK_WIDGET_REALIZED (clist))
        gtk_style_detach (clist_row->cell[column].style);
      gtk_style_unref (clist_row->cell[column].style);
    }

  clist_row->cell[column].style = style;

  if (clist_row->cell[column].style)
    {
      gtk_style_ref (clist_row->cell[column].style);
      
      if (GTK_WIDGET_REALIZED (clist))
        clist_row->cell[column].style =
	  gtk_style_attach (clist_row->cell[column].style,
			    clist->clist_window);
    }

  column_auto_resize (clist, clist_row, column, requisition.width);

  /* redraw the list if it's not frozen */
  if (CLIST_UNFROZEN (clist))
    {
      if (eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE)
	EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row, clist_row);
    }
}

GtkStyle *
eel_clist_get_cell_style (EelCList *clist,
			  gint      row,
			  gint      column)
{
  EelCListRow *clist_row;

  g_return_val_if_fail (clist != NULL, NULL);
  g_return_val_if_fail (EEL_IS_CLIST (clist), NULL);

  if (row < 0 || row >= clist->rows || column < 0 || column >= clist->columns)
    return NULL;

  clist_row = ROW_ELEMENT (clist, row)->data;

  return clist_row->cell[column].style;
}

void
eel_clist_set_row_style (EelCList *clist,
			 gint      row,
			 GtkStyle *style)
{
  GtkRequisition requisition;
  EelCListRow *clist_row;
  gint *old_width;
  gint i;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row >= clist->rows)
    return;

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (clist_row->style == style)
    return;

  old_width = g_new (gint, clist->columns);

  if (!EEL_CLIST_AUTO_RESIZE_BLOCKED(clist))
    {
      for (i = 0; i < clist->columns; i++)
	if (clist->column[i].auto_resize)
	  {
	    EEL_CLIST_CLASS_FW (clist)->cell_size_request (clist, clist_row,
							   i, &requisition);
	    old_width[i] = requisition.width;
	  }
    }

  if (clist_row->style)
    {
      if (GTK_WIDGET_REALIZED (clist))
        gtk_style_detach (clist_row->style);
      gtk_style_unref (clist_row->style);
    }

  clist_row->style = style;

  if (clist_row->style)
    {
      gtk_style_ref (clist_row->style);
      
      if (GTK_WIDGET_REALIZED (clist))
        clist_row->style = gtk_style_attach (clist_row->style,
					     clist->clist_window);
    }

  if (EEL_CLIST_AUTO_RESIZE_BLOCKED(clist))
    for (i = 0; i < clist->columns; i++)
      column_auto_resize (clist, clist_row, i, old_width[i]);

  g_free (old_width);

  /* redraw the list if it's not frozen */
  if (CLIST_UNFROZEN (clist))
    {
      if (eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE)
	EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row, clist_row);
    }
}

GtkStyle *
eel_clist_get_row_style (EelCList *clist,
			 gint      row)
{
  EelCListRow *clist_row;

  g_return_val_if_fail (clist != NULL, NULL);
  g_return_val_if_fail (EEL_IS_CLIST (clist), NULL);

  if (row < 0 || row >= clist->rows)
    return NULL;

  clist_row = ROW_ELEMENT (clist, row)->data;

  return clist_row->style;
}

/* PUBLIC SELECTION FUNCTIONS
 *   eel_clist_set_selectable
 *   eel_clist_get_selectable
 *   eel_clist_select_row
 *   eel_clist_unselect_row
 *   eel_clist_select_all
 *   eel_clist_unselect_all
 *   eel_clist_undo_selection
 */
void
eel_clist_set_selectable (EelCList *clist,
			  gint      row,
			  gboolean  selectable)
{
  EelCListRow *clist_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row >= clist->rows)
    return;

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (selectable == clist_row->selectable)
    return;

  clist_row->selectable = selectable;

  if (!selectable && clist_row->state == GTK_STATE_SELECTED)
    {
      if (clist->anchor >= 0 &&
	  clist->selection_mode == GTK_SELECTION_EXTENDED)
	{
	  clist->drag_button = 0;
	  remove_grab (clist);
	  EEL_CLIST_CLASS_FW (clist)->resync_selection (clist, NULL);
	}
      gtk_signal_emit (GTK_OBJECT (clist), clist_signals[UNSELECT_ROW],
		       row, -1, NULL);
    }      
}

gboolean
eel_clist_get_selectable (EelCList *clist,
			  gint      row)
{
  g_return_val_if_fail (clist != NULL, FALSE);
  g_return_val_if_fail (EEL_IS_CLIST (clist), FALSE);

  if (row < 0 || row >= clist->rows)
    return FALSE;

  return EEL_CLIST_ROW (ROW_ELEMENT (clist, row))->selectable;
}

void
eel_clist_select_row (EelCList *clist,
		      gint      row,
		      gint      column)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row >= clist->rows)
    return;
  if (column < -1 || column >= clist->columns)
    return;

  gtk_signal_emit (GTK_OBJECT (clist), clist_signals[SELECT_ROW],
		   row, column, NULL);
}

void
eel_clist_unselect_row (EelCList *clist,
			gint      row,
			gint      column)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row >= clist->rows)
    return;
  if (column < -1 || column >= clist->columns)
    return;

  gtk_signal_emit (GTK_OBJECT (clist), clist_signals[UNSELECT_ROW],
		   row, column, NULL);
}

void
eel_clist_select_all (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  EEL_CLIST_CLASS_FW (clist)->select_all (clist);
}

void
eel_clist_unselect_all (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  EEL_CLIST_CLASS_FW (clist)->unselect_all (clist);
}

void
eel_clist_undo_selection (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (clist->selection_mode == GTK_SELECTION_EXTENDED &&
      (clist->undo_selection || clist->undo_unselection))
    gtk_signal_emit (GTK_OBJECT (clist), clist_signals[UNDO_SELECTION]);
}

/* PRIVATE SELECTION FUNCTIONS
 *   selection_find
 *   toggle_row
 *   fake_toggle_row
 *   toggle_focus_row
 *   toggle_add_mode
 *   real_select_row
 *   real_unselect_row
 *   real_select_all
 *   real_unselect_all
 *   fake_unselect_all
 *   real_undo_selection
 *   set_anchor
 *   resync_selection
 *   update_extended_selection
 *   start_selection
 *   end_selection
 *   extend_selection
 *   sync_selection
 */
static GList *
selection_find (EelCList *clist,
		gint      row_number,
		GList    *row_list_element)
{
  return g_list_find (clist->selection, GINT_TO_POINTER (row_number));
}

static void
toggle_row (EelCList *clist,
	    gint      row,
	    gint      column,
	    GdkEvent *event)
{
  EelCListRow *clist_row;

  switch (clist->selection_mode)
    {
    case GTK_SELECTION_EXTENDED:
    case GTK_SELECTION_MULTIPLE:
    case GTK_SELECTION_SINGLE:
      clist_row = ROW_ELEMENT (clist, row)->data;

      if (!clist_row)
	return;

      if (clist_row->state == GTK_STATE_SELECTED)
	{
	  gtk_signal_emit (GTK_OBJECT (clist), clist_signals[UNSELECT_ROW],
			   row, column, event);
	  return;
	}
    case GTK_SELECTION_BROWSE:
      gtk_signal_emit (GTK_OBJECT (clist), clist_signals[SELECT_ROW],
		       row, column, event);
      break;
    }
}

static void
fake_toggle_row (EelCList *clist,
		 gint      row)
{
  GList *work;

  work = ROW_ELEMENT (clist, row);

  if (!work || !EEL_CLIST_ROW (work)->selectable)
    return;
  
  if (EEL_CLIST_ROW (work)->state == GTK_STATE_NORMAL)
    clist->anchor_state = EEL_CLIST_ROW (work)->state = GTK_STATE_SELECTED;
  else
    clist->anchor_state = EEL_CLIST_ROW (work)->state = GTK_STATE_NORMAL;
  
  if (CLIST_UNFROZEN (clist) &&
      eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE)
    EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row,
					  EEL_CLIST_ROW (work));
}

static void
toggle_focus_row (EelCList *clist)
{
  g_return_if_fail (clist != 0);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if ((gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist)) ||
      clist->focus_row < 0 || clist->focus_row >= clist->rows)
    return;

  switch (clist->selection_mode)
    {
    case  GTK_SELECTION_SINGLE:
    case  GTK_SELECTION_MULTIPLE:
      toggle_row (clist, clist->focus_row, 0, NULL);
      break;
    case GTK_SELECTION_EXTENDED:
      g_list_free (clist->undo_selection);
      g_list_free (clist->undo_unselection);
      clist->undo_selection = NULL;
      clist->undo_unselection = NULL;

      clist->anchor = clist->focus_row;
      clist->drag_pos = clist->focus_row;
      clist->undo_anchor = clist->focus_row;
      
      if (EEL_CLIST_ADD_MODE(clist))
	fake_toggle_row (clist, clist->focus_row);
      else
	EEL_CLIST_CLASS_FW (clist)->fake_unselect_all (clist,clist->focus_row);

      EEL_CLIST_CLASS_FW (clist)->resync_selection (clist, NULL);
      break;
    default:
      break;
    }
}

static void
toggle_add_mode (EelCList *clist)
{
  g_return_if_fail (clist != 0);
  g_return_if_fail (EEL_IS_CLIST (clist));
  
  if ((gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist)) ||
      clist->selection_mode != GTK_SELECTION_EXTENDED)
    return;

  eel_clist_draw_focus (GTK_WIDGET (clist));
  if (!EEL_CLIST_ADD_MODE(clist))
    {
      EEL_CLIST_SET_FLAG (clist, CLIST_ADD_MODE);
      gdk_gc_set_line_attributes (clist->xor_gc, 1,
				  GDK_LINE_ON_OFF_DASH, 0, 0);
      gdk_gc_set_dashes (clist->xor_gc, 0, "\4\4", 2);
    }
  else
    {
      EEL_CLIST_UNSET_FLAG (clist, CLIST_ADD_MODE);
      gdk_gc_set_line_attributes (clist->xor_gc, 1, GDK_LINE_SOLID, 0, 0);
      clist->anchor_state = GTK_STATE_SELECTED;
    }
  eel_clist_draw_focus (GTK_WIDGET (clist));
}

static void
real_select_row (EelCList *clist,
		 gint      row,
		 gint      column,
		 GdkEvent *event)
{
  EelCListRow *clist_row;
  GList *list;
  gint sel_row;
  gboolean row_selected;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row > (clist->rows - 1))
    return;

  switch (clist->selection_mode)
    {
    case GTK_SELECTION_SINGLE:
    case GTK_SELECTION_BROWSE:

      row_selected = FALSE;
      list = clist->selection;

      while (list)
	{
	  sel_row = GPOINTER_TO_INT (list->data);
	  list = list->next;

	  if (row == sel_row)
	    row_selected = TRUE;
	  else
	    gtk_signal_emit (GTK_OBJECT (clist), clist_signals[UNSELECT_ROW], 
			     sel_row, column, event);
	}

      if (row_selected)
	return;
      
    default:
      break;
    }

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (clist_row->state != GTK_STATE_NORMAL || !clist_row->selectable)
    return;

  clist_row->state = GTK_STATE_SELECTED;
  if (!clist->selection)
    {
      clist->selection = g_list_append (clist->selection,
					GINT_TO_POINTER (row));
      clist->selection_end = clist->selection;
    }
  else
    clist->selection_end = 
      g_list_append (clist->selection_end, GINT_TO_POINTER (row))->next;
  
  if (CLIST_UNFROZEN (clist)
      && (eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE))
    EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row, clist_row);
}

static void
real_unselect_row (EelCList *clist,
		   gint      row,
		   gint      column,
		   GdkEvent *event)
{
  EelCListRow *clist_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (row < 0 || row > (clist->rows - 1))
    return;

  clist_row = ROW_ELEMENT (clist, row)->data;

  if (clist_row->state == GTK_STATE_SELECTED)
    {
      clist_row->state = GTK_STATE_NORMAL;

      if (clist->selection_end && 
	  clist->selection_end->data == GINT_TO_POINTER (row))
	clist->selection_end = clist->selection_end->prev;

      clist->selection = g_list_remove (clist->selection,
					GINT_TO_POINTER (row));
      
      if (CLIST_UNFROZEN (clist)
	  && (eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE))
	EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row, clist_row);
    }
}

static void
real_select_all (EelCList *clist)
{
  GList *list;
  gint i;
 
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist))
    return;

  switch (clist->selection_mode)
    {
    case GTK_SELECTION_SINGLE:
    case GTK_SELECTION_BROWSE:
      return;

    case GTK_SELECTION_EXTENDED:
      g_list_free (clist->undo_selection);
      g_list_free (clist->undo_unselection);
      clist->undo_selection = NULL;
      clist->undo_unselection = NULL;
	  
      if (clist->rows &&
	  ((EelCListRow *) (clist->row_list->data))->state !=
	  GTK_STATE_SELECTED)
	fake_toggle_row (clist, 0);

      clist->anchor_state =  GTK_STATE_SELECTED;
      clist->anchor = 0;
      clist->drag_pos = 0;
      clist->undo_anchor = clist->focus_row;
      update_extended_selection (clist, clist->rows);
      EEL_CLIST_CLASS_FW (clist)->resync_selection (clist, NULL);
      return;

    case GTK_SELECTION_MULTIPLE:
      for (i = 0, list = clist->row_list; list; i++, list = list->next)
	{
	  if (((EelCListRow *)(list->data))->state == GTK_STATE_NORMAL)
	    gtk_signal_emit (GTK_OBJECT (clist), clist_signals[SELECT_ROW],
			     i, -1, NULL);
	}
      return;
    }
}

static void
real_unselect_all (EelCList *clist)
{
  GList *list;
  gint i;
 
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist))
    return;

  switch (clist->selection_mode)
    {
    case GTK_SELECTION_BROWSE:
      if (clist->focus_row >= 0)
	{
	  gtk_signal_emit (GTK_OBJECT (clist),
			   clist_signals[SELECT_ROW],
			   clist->focus_row, -1, NULL);
	  return;
	}
      break;
    case GTK_SELECTION_EXTENDED:
      g_list_free (clist->undo_selection);
      g_list_free (clist->undo_unselection);
      clist->undo_selection = NULL;
      clist->undo_unselection = NULL;

      clist->anchor = -1;
      clist->drag_pos = -1;
      clist->undo_anchor = clist->focus_row;
      break;
    default:
      break;
    }

  list = clist->selection;
  while (list)
    {
      i = GPOINTER_TO_INT (list->data);
      list = list->next;
      gtk_signal_emit (GTK_OBJECT (clist),
		       clist_signals[UNSELECT_ROW], i, -1, NULL);
    }
}

static void
fake_unselect_all (EelCList *clist,
		   gint      row)
{
  GList *list;
  GList *work;
  gint i;

  if (row >= 0 && (work = ROW_ELEMENT (clist, row)))
    {
      if (EEL_CLIST_ROW (work)->state == GTK_STATE_NORMAL &&
	  EEL_CLIST_ROW (work)->selectable)
	{
	  EEL_CLIST_ROW (work)->state = GTK_STATE_SELECTED;
	  
	  if (CLIST_UNFROZEN (clist) &&
	      eel_clist_row_is_visible (clist, row) != GTK_VISIBILITY_NONE)
	    EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, row,
						  EEL_CLIST_ROW (work));
	}  
    }

  clist->undo_selection = clist->selection;
  clist->selection = NULL;
  clist->selection_end = NULL;

  for (list = clist->undo_selection; list; list = list->next)
    {
      if ((i = GPOINTER_TO_INT (list->data)) == row ||
	  !(work = g_list_nth (clist->row_list, i)))
	continue;

      EEL_CLIST_ROW (work)->state = GTK_STATE_NORMAL;
      if (CLIST_UNFROZEN (clist) &&
	  eel_clist_row_is_visible (clist, i) != GTK_VISIBILITY_NONE)
	EEL_CLIST_CLASS_FW (clist)->draw_row (clist, NULL, i,
					      EEL_CLIST_ROW (work));
    }
}

static void
real_undo_selection (EelCList *clist)
{
  GList *work;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if ((gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist)) ||
      clist->selection_mode != GTK_SELECTION_EXTENDED)
    return;

  EEL_CLIST_CLASS_FW (clist)->resync_selection (clist, NULL);

  if (!(clist->undo_selection || clist->undo_unselection))
    {
      eel_clist_unselect_all (clist);
      return;
    }

  for (work = clist->undo_selection; work; work = work->next)
    gtk_signal_emit (GTK_OBJECT (clist), clist_signals[SELECT_ROW],
		     GPOINTER_TO_INT (work->data), -1, NULL);

  for (work = clist->undo_unselection; work; work = work->next)
    {
      /* g_print ("unselect %d\n",GPOINTER_TO_INT (work->data)); */
      gtk_signal_emit (GTK_OBJECT (clist), clist_signals[UNSELECT_ROW], 
		       GPOINTER_TO_INT (work->data), -1, NULL);
    }

  if (GTK_WIDGET_HAS_FOCUS(clist) && clist->focus_row != clist->undo_anchor)
    {
      eel_clist_draw_focus (GTK_WIDGET (clist));
      clist->focus_row = clist->undo_anchor;
      eel_clist_draw_focus (GTK_WIDGET (clist));
    }
  else
    clist->focus_row = clist->undo_anchor;
  
  clist->undo_anchor = -1;
 
  g_list_free (clist->undo_selection);
  g_list_free (clist->undo_unselection);
  clist->undo_selection = NULL;
  clist->undo_unselection = NULL;

  if (ROW_TOP_YPIXEL (clist, clist->focus_row) + clist->row_height >
      clist->clist_window_height)
    eel_clist_moveto (clist, clist->focus_row, -1, 1, 0);
  else if (ROW_TOP_YPIXEL (clist, clist->focus_row) < 0)
    eel_clist_moveto (clist, clist->focus_row, -1, 0, 0);
}

static void
set_anchor (EelCList *clist,
	    gboolean  add_mode,
	    gint      anchor,
	    gint      undo_anchor)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));
  
  if (clist->selection_mode != GTK_SELECTION_EXTENDED || clist->anchor >= 0)
    return;

  g_list_free (clist->undo_selection);
  g_list_free (clist->undo_unselection);
  clist->undo_selection = NULL;
  clist->undo_unselection = NULL;

  if (add_mode)
    fake_toggle_row (clist, anchor);
  else
    {
      EEL_CLIST_CLASS_FW (clist)->fake_unselect_all (clist, anchor);
      clist->anchor_state = GTK_STATE_SELECTED;
    }

  clist->anchor = anchor;
  clist->drag_pos = anchor;
  clist->undo_anchor = undo_anchor;
}

static void
resync_selection (EelCList *clist,
		  GdkEvent *event)
{
  gint i;
  gint e;
  gint row;
  GList *list;
  EelCListRow *clist_row;

  if (clist->selection_mode != GTK_SELECTION_EXTENDED)
    return;

  if (clist->anchor < 0 || clist->drag_pos < 0)
    return;

  eel_clist_freeze (clist);

  i = MIN (clist->anchor, clist->drag_pos);
  e = MAX (clist->anchor, clist->drag_pos);

  if (clist->undo_selection)
    {
      list = clist->selection;
      clist->selection = clist->undo_selection;
      clist->selection_end = g_list_last (clist->selection);
      clist->undo_selection = list;
      list = clist->selection;
      while (list)
	{
	  row = GPOINTER_TO_INT (list->data);
	  list = list->next;
	  if (row < i || row > e)
	    {
	      clist_row = g_list_nth (clist->row_list, row)->data;
	      if (clist_row->selectable)
		{
		  clist_row->state = GTK_STATE_SELECTED;
		  gtk_signal_emit (GTK_OBJECT (clist),
				   clist_signals[UNSELECT_ROW],
				   row, -1, event);
		  clist->undo_selection = g_list_prepend
		    (clist->undo_selection, GINT_TO_POINTER (row));
		}
	    }
	}
    }    

  if (clist->anchor < clist->drag_pos)
    {
      for (list = g_list_nth (clist->row_list, i); i <= e;
	   i++, list = list->next)
	if (EEL_CLIST_ROW (list)->selectable)
	  {
	    if (g_list_find (clist->selection, GINT_TO_POINTER(i)))
	      {
		if (EEL_CLIST_ROW (list)->state == GTK_STATE_NORMAL)
		  {
		    EEL_CLIST_ROW (list)->state = GTK_STATE_SELECTED;
		    gtk_signal_emit (GTK_OBJECT (clist),
				     clist_signals[UNSELECT_ROW],
				     i, -1, event);
		    clist->undo_selection =
		      g_list_prepend (clist->undo_selection,
				      GINT_TO_POINTER (i));
		  }
	      }
	    else if (EEL_CLIST_ROW (list)->state == GTK_STATE_SELECTED)
	      {
		EEL_CLIST_ROW (list)->state = GTK_STATE_NORMAL;
		clist->undo_unselection =
		  g_list_prepend (clist->undo_unselection,
				  GINT_TO_POINTER (i));
	      }
	  }
    }
  else
    {
      for (list = g_list_nth (clist->row_list, e); i <= e;
	   e--, list = list->prev)
	if (EEL_CLIST_ROW (list)->selectable)
	  {
	    if (g_list_find (clist->selection, GINT_TO_POINTER(e)))
	      {
		if (EEL_CLIST_ROW (list)->state == GTK_STATE_NORMAL)
		  {
		    EEL_CLIST_ROW (list)->state = GTK_STATE_SELECTED;
		    gtk_signal_emit (GTK_OBJECT (clist),
				     clist_signals[UNSELECT_ROW],
				     e, -1, event);
		    clist->undo_selection =
		      g_list_prepend (clist->undo_selection,
				      GINT_TO_POINTER (e));
		  }
	      }
	    else if (EEL_CLIST_ROW (list)->state == GTK_STATE_SELECTED)
	      {
		EEL_CLIST_ROW (list)->state = GTK_STATE_NORMAL;
		clist->undo_unselection =
		  g_list_prepend (clist->undo_unselection,
				  GINT_TO_POINTER (e));
	      }
	  }
    }
  
  clist->undo_unselection = g_list_reverse (clist->undo_unselection);
  for (list = clist->undo_unselection; list; list = list->next)
    gtk_signal_emit (GTK_OBJECT (clist), clist_signals[SELECT_ROW],
		     GPOINTER_TO_INT (list->data), -1, event);

  clist->anchor = -1;
  clist->drag_pos = -1;

  CLIST_REFRESH (clist);
  eel_clist_thaw (clist);
}

static void
update_extended_selection (EelCList *clist,
			   gint      row)
{
  gint i;
  GList *list;
  GdkRectangle area;
  gint s1 = -1;
  gint s2 = -1;
  gint e1 = -1;
  gint e2 = -1;
  gint y1 = clist->clist_window_height;
  gint y2 = clist->clist_window_height;
  gint h1 = 0;
  gint h2 = 0;
  gint top;

  if (clist->selection_mode != GTK_SELECTION_EXTENDED || clist->anchor == -1)
    return;

  if (row < 0)
    row = 0;
  if (row >= clist->rows)
    row = clist->rows - 1;

  /* extending downwards */
  if (row > clist->drag_pos && clist->anchor <= clist->drag_pos)
    {
      s2 = clist->drag_pos + 1;
      e2 = row;
    }
  /* extending upwards */
  else if (row < clist->drag_pos && clist->anchor >= clist->drag_pos)
    {
      s2 = row;
      e2 = clist->drag_pos - 1;
    }
  else if (row < clist->drag_pos && clist->anchor < clist->drag_pos)
    {
      e1 = clist->drag_pos;
      /* row and drag_pos on different sides of anchor :
	 take back the selection between anchor and drag_pos,
         select between anchor and row */
      if (row < clist->anchor)
	{
	  s1 = clist->anchor + 1;
	  s2 = row;
	  e2 = clist->anchor - 1;
	}
      /* take back the selection between anchor and drag_pos */
      else
	s1 = row + 1;
    }
  else if (row > clist->drag_pos && clist->anchor > clist->drag_pos)
    {
      s1 = clist->drag_pos;
      /* row and drag_pos on different sides of anchor :
	 take back the selection between anchor and drag_pos,
         select between anchor and row */
      if (row > clist->anchor)
	{
	  e1 = clist->anchor - 1;
	  s2 = clist->anchor + 1;
	  e2 = row;
	}
      /* take back the selection between anchor and drag_pos */
      else
	e1 = row - 1;
    }

  clist->drag_pos = row;

  area.x = 0;
  area.width = clist->clist_window_width;

  /* restore the elements between s1 and e1 */
  if (s1 >= 0)
    {
      for (i = s1, list = g_list_nth (clist->row_list, i); i <= e1;
	   i++, list = list->next)
	if (EEL_CLIST_ROW (list)->selectable)
	  {
	    if (EEL_CLIST_CLASS_FW (clist)->selection_find (clist, i, list))
	      EEL_CLIST_ROW (list)->state = GTK_STATE_SELECTED;
	    else
	      EEL_CLIST_ROW (list)->state = GTK_STATE_NORMAL;
	  }

      top = ROW_TOP_YPIXEL (clist, clist->focus_row);

      if (top + clist->row_height <= 0)
	{
	  area.y = 0;
	  area.height = ROW_TOP_YPIXEL (clist, e1) + clist->row_height;
          EEL_CLIST_CLASS_FW (clist)->draw_rows (clist, &area);
	  eel_clist_moveto (clist, clist->focus_row, -1, 0, 0);
	}
      else if (top >= clist->clist_window_height)
	{
	  area.y = ROW_TOP_YPIXEL (clist, s1) - 1;
	  area.height = clist->clist_window_height - area.y;
          EEL_CLIST_CLASS_FW (clist)->draw_rows (clist, &area);
	  eel_clist_moveto (clist, clist->focus_row, -1, 1, 0);
	}
      else if (top < 0)
	eel_clist_moveto (clist, clist->focus_row, -1, 0, 0);
      else if (top + clist->row_height > clist->clist_window_height)
	eel_clist_moveto (clist, clist->focus_row, -1, 1, 0);

      y1 = ROW_TOP_YPIXEL (clist, s1) - 1;
      h1 = (e1 - s1 + 1) * (clist->row_height + CELL_SPACING);
    }

  /* extend the selection between s2 and e2 */
  if (s2 >= 0)
    {
      for (i = s2, list = g_list_nth (clist->row_list, i); i <= e2;
	   i++, list = list->next)
	if (EEL_CLIST_ROW (list)->selectable &&
	    EEL_CLIST_ROW (list)->state != clist->anchor_state)
	  EEL_CLIST_ROW (list)->state = clist->anchor_state;

      top = ROW_TOP_YPIXEL (clist, clist->focus_row);

      if (top + clist->row_height <= 0)
	{
	  area.y = 0;
	  area.height = ROW_TOP_YPIXEL (clist, e2) + clist->row_height;
          EEL_CLIST_CLASS_FW (clist)->draw_rows (clist, &area);
	  eel_clist_moveto (clist, clist->focus_row, -1, 0, 0);
	}
      else if (top >= clist->clist_window_height)
	{
	  area.y = ROW_TOP_YPIXEL (clist, s2) - 1;
	  area.height = clist->clist_window_height - area.y;
          EEL_CLIST_CLASS_FW (clist)->draw_rows (clist, &area);
	  eel_clist_moveto (clist, clist->focus_row, -1, 1, 0);
	}
      else if (top < 0)
	eel_clist_moveto (clist, clist->focus_row, -1, 0, 0);
      else if (top + clist->row_height > clist->clist_window_height)
	eel_clist_moveto (clist, clist->focus_row, -1, 1, 0);

      y2 = ROW_TOP_YPIXEL (clist, s2) - 1;
      h2 = (e2 - s2 + 1) * (clist->row_height + CELL_SPACING);
    }

  area.y = MAX (0, MIN (y1, y2));
  if (area.y > clist->clist_window_height)
    area.y = 0;
  area.height = MIN (clist->clist_window_height, h1 + h2);
  if (s1 >= 0 && s2 >= 0)
    area.height += (clist->row_height + CELL_SPACING);
  EEL_CLIST_CLASS_FW (clist)->draw_rows (clist, &area);
}

static void
start_selection (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist))
    return;

  set_anchor (clist, EEL_CLIST_ADD_MODE(clist), clist->focus_row,
	      clist->focus_row);
}

static void
end_selection (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_FOCUS(clist))
    return;

  EEL_CLIST_CLASS_FW (clist)->resync_selection (clist, NULL);
}

static void
extend_selection (EelCList      *clist,
		  GtkScrollType  scroll_type,
		  gfloat         position,
		  gboolean       auto_start_selection)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if ((gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist)) ||
      clist->selection_mode != GTK_SELECTION_EXTENDED)
    return;

  if (auto_start_selection)
    set_anchor (clist, EEL_CLIST_ADD_MODE(clist), clist->focus_row,
		clist->focus_row);
  else if (clist->anchor == -1)
    return;

  move_focus_row (clist, scroll_type, position);

  if (ROW_TOP_YPIXEL (clist, clist->focus_row) + clist->row_height >
      clist->clist_window_height)
    eel_clist_moveto (clist, clist->focus_row, -1, 1, 0);
  else if (ROW_TOP_YPIXEL (clist, clist->focus_row) < 0)
    eel_clist_moveto (clist, clist->focus_row, -1, 0, 0);

  update_extended_selection (clist, clist->focus_row);
}

static void
sync_selection (EelCList *clist,
		gint      row,
		gint      mode)
{
  GList *list;
  gint d;

  if (mode == SYNC_INSERT)
    d = 1;
  else
    d = -1;
      
  if (clist->focus_row >= row)
    {
      if (d > 0 || clist->focus_row > row)
	clist->focus_row += d;
      if (clist->focus_row == -1 && clist->rows >= 1)
	clist->focus_row = 0;
      else if (clist->focus_row >= clist->rows)
	clist->focus_row = clist->rows - 1;
    }

  EEL_CLIST_CLASS_FW (clist)->resync_selection (clist, NULL);

  g_list_free (clist->undo_selection);
  g_list_free (clist->undo_unselection);
  clist->undo_selection = NULL;
  clist->undo_unselection = NULL;

  clist->anchor = -1;
  clist->drag_pos = -1;
  clist->undo_anchor = clist->focus_row;

  list = clist->selection;

  while (list)
    {
      if (GPOINTER_TO_INT (list->data) >= row)
	list->data = ((gchar*) list->data) + d;
      list = list->next;
    }
}

/* GTKOBJECT
 *   eel_clist_destroy
 *   eel_clist_finalize
 */
static void
eel_clist_destroy (GtkObject *object)
{
  gint i;
  EelCList *clist;

  g_return_if_fail (object != NULL);
  g_return_if_fail (EEL_IS_CLIST (object));

  clist = EEL_CLIST (object);

  /* freeze the list */
  clist->freeze_count++;

  /* get rid of all the rows */
  eel_clist_clear (clist);

  /* Since we don't have a _remove method, unparent the children
   * instead of destroying them so the focus will be unset properly.
   * (For other containers, the _remove method takes care of the
   * unparent) The destroy will happen when the refcount drops
   * to zero.
   */

  /* unref adjustments */
  if (clist->hadjustment)
    {
      gtk_signal_disconnect_by_data (GTK_OBJECT (clist->hadjustment), clist);
      gtk_object_unref (GTK_OBJECT (clist->hadjustment));
      clist->hadjustment = NULL;
    }
  if (clist->vadjustment)
    {
      gtk_signal_disconnect_by_data (GTK_OBJECT (clist->vadjustment), clist);
      gtk_object_unref (GTK_OBJECT (clist->vadjustment));
      clist->vadjustment = NULL;
    }

  remove_grab (clist);

  /* destroy the column buttons */
  for (i = 0; i < clist->columns; i++)
    if (clist->column[i].button)
      {
	gtk_widget_unparent (clist->column[i].button);
	clist->column[i].button = NULL;
      }

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (*GTK_OBJECT_CLASS (parent_class)->destroy) (object);
}

static void
eel_clist_finalize (GtkObject *object)
{
  EelCList *clist;

  g_return_if_fail (object != NULL);
  g_return_if_fail (EEL_IS_CLIST (object));

  clist = EEL_CLIST (object);

  columns_delete (clist);

  g_mem_chunk_destroy (clist->cell_mem_chunk);
  g_mem_chunk_destroy (clist->row_mem_chunk);

  if (GTK_OBJECT_CLASS (parent_class)->finalize)
    (*GTK_OBJECT_CLASS (parent_class)->finalize) (object);
}

/* GTKWIDGET
 *   eel_clist_realize
 *   eel_clist_unrealize
 *   eel_clist_map
 *   eel_clist_unmap
 *   eel_clist_draw
 *   eel_clist_expose
 *   eel_clist_style_set
 *   eel_clist_key_press
 *   eel_clist_button_press
 *   eel_clist_button_release
 *   eel_clist_motion
 *   eel_clist_size_request
 *   eel_clist_size_allocate
 */
static void
eel_clist_realize (GtkWidget *widget)
{
  EelCList *clist;
  GdkWindowAttr attributes;
  GdkGCValues values;
  EelCListRow *clist_row;
  GList *list;
  gint attributes_mask;
  gint border_width;
  gint i;
  gint j;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));

  clist = EEL_CLIST (widget);

  GTK_WIDGET_SET_FLAGS (widget, GTK_REALIZED);

  border_width = GTK_CONTAINER (widget)->border_width;
  
  attributes.window_type = GDK_WINDOW_CHILD;
  attributes.x = widget->allocation.x + border_width;
  attributes.y = widget->allocation.y + border_width;
  attributes.width = widget->allocation.width - border_width * 2;
  attributes.height = widget->allocation.height - border_width * 2;
  attributes.wclass = GDK_INPUT_OUTPUT;
  attributes.visual = gtk_widget_get_visual (widget);
  attributes.colormap = gtk_widget_get_colormap (widget);
  attributes.event_mask = gtk_widget_get_events (widget);
  attributes.event_mask |= (GDK_EXPOSURE_MASK |
			    GDK_BUTTON_PRESS_MASK |
			    GDK_BUTTON_RELEASE_MASK |
			    GDK_KEY_PRESS_MASK |
			    GDK_KEY_RELEASE_MASK);
  attributes_mask = GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP;

  /* main window */
  widget->window = gdk_window_new (gtk_widget_get_parent_window (widget),
				   &attributes, attributes_mask);
  gdk_window_set_user_data (widget->window, clist);

  widget->style = gtk_style_attach (widget->style, widget->window);

  gtk_style_set_background (widget->style, widget->window, GTK_STATE_NORMAL);

  /* column-title window */

  attributes.x = clist->column_title_area.x;
  attributes.y = clist->column_title_area.y;
  attributes.width = clist->column_title_area.width;
  attributes.height = clist->column_title_area.height;
  
  clist->title_window = gdk_window_new (widget->window, &attributes,
					attributes_mask);
  gdk_window_set_user_data (clist->title_window, clist);

  gtk_style_set_background (widget->style, clist->title_window,
			    GTK_STATE_NORMAL);
  gdk_window_show (clist->title_window);

  /* set things up so column buttons are drawn in title window */
  for (i = 0; i < clist->columns; i++)
    if (clist->column[i].button)
      gtk_widget_set_parent_window (clist->column[i].button,
				    clist->title_window);

  /* clist-window */
  attributes.x = (clist->internal_allocation.x +
		  widget->style->klass->xthickness);
  attributes.y = (clist->internal_allocation.y +
		  widget->style->klass->ythickness +
		  clist->column_title_area.height);
  attributes.width = clist->clist_window_width;
  attributes.height = clist->clist_window_height;
  
  clist->clist_window = gdk_window_new (widget->window, &attributes,
					attributes_mask);
  gdk_window_set_user_data (clist->clist_window, clist);

  gdk_window_set_background (clist->clist_window,
			     &widget->style->base[GTK_STATE_NORMAL]);
  gdk_window_show (clist->clist_window);
  gdk_window_get_size (clist->clist_window, &clist->clist_window_width,
		       &clist->clist_window_height);

  /* create resize windows */
  attributes.wclass = GDK_INPUT_ONLY;
  attributes.event_mask = (GDK_BUTTON_PRESS_MASK |
			   GDK_BUTTON_RELEASE_MASK |
			   GDK_POINTER_MOTION_MASK |
			   GDK_POINTER_MOTION_HINT_MASK |
			   GDK_KEY_PRESS_MASK);
  attributes_mask = GDK_WA_CURSOR;
  attributes.cursor = gdk_cursor_new (GDK_SB_H_DOUBLE_ARROW);
  clist->cursor_drag = attributes.cursor;

  attributes.x =  LIST_WIDTH (clist) + 1;
  attributes.y = 0;
  attributes.width = 0;
  attributes.height = 0;

  for (i = 0; i < clist->columns; i++)
    {
      clist->column[i].window = gdk_window_new (clist->title_window,
						&attributes, attributes_mask);
      gdk_window_set_user_data (clist->column[i].window, clist);
    }

  /* This is slightly less efficient than creating them with the
   * right size to begin with, but easier
   */
  size_allocate_title_buttons (clist);

  /* GCs */
  clist->fg_gc = gdk_gc_new (widget->window);
  clist->bg_gc = gdk_gc_new (widget->window);
  
  /* We'll use this gc to do scrolling as well */
  gdk_gc_set_exposures (clist->fg_gc, TRUE);

  values.foreground = (widget->style->white.pixel==0 ?
		       widget->style->black:widget->style->white);
  values.function = GDK_XOR;
  values.subwindow_mode = GDK_INCLUDE_INFERIORS;
  clist->xor_gc = gdk_gc_new_with_values (widget->window,
					  &values,
					  GDK_GC_FOREGROUND |
					  GDK_GC_FUNCTION |
					  GDK_GC_SUBWINDOW);

  /* attach optional row/cell styles, allocate foreground/background colors */
  list = clist->row_list;
  for (i = 0; i < clist->rows; i++)
    {
      clist_row = list->data;
      list = list->next;

      if (clist_row->style)
	clist_row->style = gtk_style_attach (clist_row->style,
					     clist->clist_window);

      if (clist_row->fg_set || clist_row->bg_set)
	{
	  GdkColormap *colormap;

	  colormap = gtk_widget_get_colormap (widget);
	  if (clist_row->fg_set)
	    gdk_color_alloc (colormap, &clist_row->foreground);
	  if (clist_row->bg_set)
	    gdk_color_alloc (colormap, &clist_row->background);
	}
      
      for (j = 0; j < clist->columns; j++)
	if  (clist_row->cell[j].style)
	  clist_row->cell[j].style =
	    gtk_style_attach (clist_row->cell[j].style, clist->clist_window);
    }
}

static void
eel_clist_unrealize (GtkWidget *widget)
{
  gint i;
  EelCList *clist;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));

  clist = EEL_CLIST (widget);

  /* freeze the list */
  clist->freeze_count++;

  if (GTK_WIDGET_MAPPED (widget))
    eel_clist_unmap (widget);

  GTK_WIDGET_UNSET_FLAGS (widget, GTK_MAPPED);

  /* detach optional row/cell styles */
  if (GTK_WIDGET_REALIZED (widget))
    {
      EelCListRow *clist_row;
      GList *list;
      gint j;

      list = clist->row_list;
      for (i = 0; i < clist->rows; i++)
	{
	  clist_row = list->data;
	  list = list->next;

	  if (clist_row->style)
	    gtk_style_detach (clist_row->style);
	  for (j = 0; j < clist->columns; j++)
	    if  (clist_row->cell[j].style)
	      gtk_style_detach (clist_row->cell[j].style);
	}
    }

  gdk_cursor_destroy (clist->cursor_drag);
  gdk_gc_destroy (clist->xor_gc);
  gdk_gc_destroy (clist->fg_gc);
  gdk_gc_destroy (clist->bg_gc);

  for (i = 0; i < clist->columns; i++)
    {
      if (clist->column[i].button)
	gtk_widget_unrealize (clist->column[i].button);
      if (clist->column[i].window)
	{
	  gdk_window_set_user_data (clist->column[i].window, NULL);
	  gdk_window_destroy (clist->column[i].window);
	  clist->column[i].window = NULL;
	}
    }

  gdk_window_set_user_data (clist->clist_window, NULL);
  gdk_window_destroy (clist->clist_window);
  clist->clist_window = NULL;

  gdk_window_set_user_data (clist->title_window, NULL);
  gdk_window_destroy (clist->title_window);
  clist->title_window = NULL;

  clist->cursor_drag = NULL;
  clist->xor_gc = NULL;
  clist->fg_gc = NULL;
  clist->bg_gc = NULL;

  if (GTK_WIDGET_CLASS (parent_class)->unrealize)
    (* GTK_WIDGET_CLASS (parent_class)->unrealize) (widget);
}

static void
eel_clist_map (GtkWidget *widget)
{
  gint i;
  EelCList *clist;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));

  clist = EEL_CLIST (widget);

  if (!GTK_WIDGET_MAPPED (widget))
    {
      GTK_WIDGET_SET_FLAGS (widget, GTK_MAPPED);

      /* map column buttons */
      for (i = 0; i < clist->columns; i++)
	{
	  if (clist->column[i].button &&
	      GTK_WIDGET_VISIBLE (clist->column[i].button) &&
	      !GTK_WIDGET_MAPPED (clist->column[i].button))
	    gtk_widget_map (clist->column[i].button);
	}
      
      for (i = 0; i < clist->columns; i++)
	if (clist->column[i].window && clist->column[i].button)
	  {
	    gdk_window_raise (clist->column[i].window);
	    gdk_window_show (clist->column[i].window);
	  }

      gdk_window_show (clist->title_window);
      gdk_window_show (clist->clist_window);
      gdk_window_show (widget->window);

      /* unfreeze the list */
      clist->freeze_count = 0;
      clist->refresh_at_unfreeze_time = FALSE;
    }
}

static void
eel_clist_unmap (GtkWidget *widget)
{
  gint i;
  EelCList *clist;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));

  clist = EEL_CLIST (widget);

  if (GTK_WIDGET_MAPPED (widget))
    {
      GTK_WIDGET_UNSET_FLAGS (widget, GTK_MAPPED);

      if (gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist))
	{
	  remove_grab (clist);

	  EEL_CLIST_CLASS_FW (widget)->resync_selection (clist, NULL);

	  clist->click_cell.row = -1;
	  clist->click_cell.column = -1;
	  clist->drag_button = 0;

	  if (EEL_CLIST_IN_DRAG(clist))
	    {
	      gpointer drag_data;

	      EEL_CLIST_UNSET_FLAG (clist, CLIST_IN_DRAG);
	      drag_data = gtk_object_get_data (GTK_OBJECT (clist),
					       "gtk-site-data");
	      if (drag_data)
		gtk_signal_handler_unblock_by_data (GTK_OBJECT (clist),
						    drag_data);
	    }
	}

      for (i = 0; i < clist->columns; i++)
	if (clist->column[i].window)
	  gdk_window_hide (clist->column[i].window);

      gdk_window_hide (clist->clist_window);
      gdk_window_hide (clist->title_window);
      gdk_window_hide (widget->window);

      /* unmap column buttons */
      for (i = 0; i < clist->columns; i++)
	if (clist->column[i].button &&
	    GTK_WIDGET_MAPPED (clist->column[i].button))
	  gtk_widget_unmap (clist->column[i].button);

      /* freeze the list */
      clist->freeze_count++;
    }
}

static void
eel_clist_draw (GtkWidget    *widget,
		     GdkRectangle *area)
{
  g_error ("this should not be called, the EelList drawing would be disrupted by this");
}

static gint
eel_clist_expose (GtkWidget      *widget,
		       GdkEventExpose *event)
{
  g_error ("this should not be called, the EelList drawing would be disrupted by this");
  return FALSE;
}

static void
eel_clist_style_set (GtkWidget *widget,
			  GtkStyle  *previous_style)
{
  EelCList *clist;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));

  if (GTK_WIDGET_CLASS (parent_class)->style_set)
    (*GTK_WIDGET_CLASS (parent_class)->style_set) (widget, previous_style);

  clist = EEL_CLIST (widget);

  if (GTK_WIDGET_REALIZED (widget))
    {
      gtk_style_set_background (widget->style, widget->window, widget->state);
      gtk_style_set_background (widget->style, clist->title_window, GTK_STATE_SELECTED);
      gdk_window_set_background (clist->clist_window, &widget->style->base[GTK_STATE_NORMAL]);
    }

  /* Fill in data after widget has correct style */

  /* text properties */
  if (!EEL_CLIST_ROW_HEIGHT_SET(clist))
    {
      clist->row_height = (widget->style->font->ascent +
			   widget->style->font->descent + 1);
      clist->row_center_offset = widget->style->font->ascent + 1.5;
    }
  else
    clist->row_center_offset = 1.5 + (clist->row_height +
				      widget->style->font->ascent -
				      widget->style->font->descent - 1) / 2;

  /* Column widths */
  if (!EEL_CLIST_AUTO_RESIZE_BLOCKED(clist))
    {
      gint width;
      gint i;

      for (i = 0; i < clist->columns; i++)
	if (clist->column[i].auto_resize)
	  {
	    width = eel_clist_optimal_column_width (clist, i);
	    if (width != clist->column[i].width)
	      eel_clist_set_column_width (clist, i, width);
	  }
    }
}

static gint
eel_clist_key_press (GtkWidget   *widget,
		     GdkEventKey *event)
{
  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (EEL_IS_CLIST (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  if (GTK_WIDGET_CLASS (parent_class)->key_press_event &&
      GTK_WIDGET_CLASS (parent_class)->key_press_event (widget, event))
    return TRUE;

  switch (event->keyval)
    {
    case GDK_Tab:
    case GDK_ISO_Left_Tab:
      if (event->state & GDK_SHIFT_MASK)
	return gtk_container_focus (GTK_CONTAINER (widget),
				    GTK_DIR_TAB_BACKWARD);
      else
	return gtk_container_focus (GTK_CONTAINER (widget),
				    GTK_DIR_TAB_FORWARD);
    default:
      break;
    }
  return FALSE;
}

static gint
eel_clist_button_press (GtkWidget      *widget,
			GdkEventButton *event)
{
  gint i;
  EelCList *clist;
  gint x;
  gint y;
  gint row;
  gint column;
  gint button_actions;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (EEL_IS_CLIST (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  clist = EEL_CLIST (widget);

  button_actions = clist->button_actions[event->button - 1];

  if (button_actions == EEL_BUTTON_IGNORED)
    return FALSE;

  /* selections on the list */
  if (event->window == clist->clist_window)
    {
      x = event->x;
      y = event->y;

      if (get_selection_info (clist, x, y, &row, &column))
	{
	  gint old_row = clist->focus_row;

	  if (clist->focus_row == -1)
	    old_row = row;

	  if (event->type == GDK_BUTTON_PRESS)
	    {
	      GdkEventMask mask = ((1 << (4 + event->button)) |
				   GDK_POINTER_MOTION_HINT_MASK |
				   GDK_BUTTON_RELEASE_MASK);

	      if (gdk_pointer_grab (clist->clist_window, FALSE, mask,
				    NULL, NULL, event->time))
		return FALSE;
	      gtk_grab_add (widget);

	      clist->click_cell.row = row;
	      clist->click_cell.column = column;
	      clist->drag_button = event->button;
	    }
	  else
	    {
	      clist->click_cell.row = -1;
	      clist->click_cell.column = -1;

	      clist->drag_button = 0;
	      remove_grab (clist);
	    }

	  if (button_actions & EEL_BUTTON_SELECTS)
	    {
	      if (EEL_CLIST_ADD_MODE(clist))
		{
		  EEL_CLIST_UNSET_FLAG (clist, CLIST_ADD_MODE);
		  if (GTK_WIDGET_HAS_FOCUS(widget))
		    {
		      eel_clist_draw_focus (widget);
		      gdk_gc_set_line_attributes (clist->xor_gc, 1,
						  GDK_LINE_SOLID, 0, 0);
		      clist->focus_row = row;
		      eel_clist_draw_focus (widget);
		    }
		  else
		    {
		      gdk_gc_set_line_attributes (clist->xor_gc, 1,
						  GDK_LINE_SOLID, 0, 0);
		      clist->focus_row = row;
		    }
		}
	      else if (row != clist->focus_row)
		{
		  if (GTK_WIDGET_HAS_FOCUS(widget))
		    {
		      eel_clist_draw_focus (widget);
		      clist->focus_row = row;
		      eel_clist_draw_focus (widget);
		    }
		  else
		    clist->focus_row = row;
		}
	    }

	  if (!GTK_WIDGET_HAS_FOCUS(widget))
	    gtk_widget_grab_focus (widget);

	  if (button_actions & EEL_BUTTON_SELECTS)
	    {
	      switch (clist->selection_mode)
		{
		case GTK_SELECTION_SINGLE:
		case GTK_SELECTION_MULTIPLE:
		  if (event->type != GDK_BUTTON_PRESS)
		    {
		      gtk_signal_emit (GTK_OBJECT (clist),
				       clist_signals[SELECT_ROW],
				       row, column, event);
		      clist->anchor = -1;
		    }
		  else
		    clist->anchor = row;
		  break;
		case GTK_SELECTION_BROWSE:
		  gtk_signal_emit (GTK_OBJECT (clist),
				   clist_signals[SELECT_ROW],
				   row, column, event);
		  break;
		case GTK_SELECTION_EXTENDED:
		  if (event->type != GDK_BUTTON_PRESS)
		    {
		      if (clist->anchor != -1)
			{
			  update_extended_selection (clist, clist->focus_row);
			  EEL_CLIST_CLASS_FW (clist)->resync_selection
			    (clist, (GdkEvent *) event);
			}
		      gtk_signal_emit (GTK_OBJECT (clist),
				       clist_signals[SELECT_ROW],
				       row, column, event);
		      break;
		    }
	      
		  if (event->state & GDK_CONTROL_MASK)
		    {
		      if (event->state & GDK_SHIFT_MASK)
			{
			  if (clist->anchor < 0)
			    {
			      g_list_free (clist->undo_selection);
			      g_list_free (clist->undo_unselection);
			      clist->undo_selection = NULL;
			      clist->undo_unselection = NULL;
			      clist->anchor = old_row;
			      clist->drag_pos = old_row;
			      clist->undo_anchor = old_row;
			    }
			  update_extended_selection (clist, clist->focus_row);
			}
		      else
			{
			  if (clist->anchor == -1)
			    set_anchor (clist, TRUE, row, old_row);
			  else
			    update_extended_selection (clist,
						       clist->focus_row);
			}
		      break;
		    }

		  if (event->state & GDK_SHIFT_MASK)
		    {
		      set_anchor (clist, FALSE, old_row, old_row);
		      update_extended_selection (clist, clist->focus_row);
		      break;
		    }

		  if (clist->anchor == -1)
		    set_anchor (clist, FALSE, row, old_row);
		  else
		    update_extended_selection (clist, clist->focus_row);
		  break;
		default:
		  break;
		}
	    }
	}
      return FALSE;
    }

  /* press on resize windows */
  for (i = 0; i < clist->columns; i++)
    if (clist->column[i].resizeable && clist->column[i].window &&
	event->window == clist->column[i].window)
      {
	gpointer drag_data;

	if (gdk_pointer_grab (clist->column[i].window, FALSE,
			      GDK_POINTER_MOTION_HINT_MASK |
			      GDK_BUTTON1_MOTION_MASK |
			      GDK_BUTTON_RELEASE_MASK,
			      NULL, NULL, event->time))
	  return FALSE;

	gtk_grab_add (widget);
	EEL_CLIST_SET_FLAG (clist, CLIST_IN_DRAG);

	/* block attached dnd signal handler */
	drag_data = gtk_object_get_data (GTK_OBJECT (clist), "gtk-site-data");
	if (drag_data)
	  gtk_signal_handler_block_by_data (GTK_OBJECT (clist), drag_data);

	if (!GTK_WIDGET_HAS_FOCUS(widget))
	  gtk_widget_grab_focus (widget);

	clist->drag_pos = i;
	clist->x_drag = (COLUMN_LEFT_XPIXEL(clist, i) + COLUMN_INSET +
			 clist->column[i].area.width + CELL_SPACING);

	if (EEL_CLIST_ADD_MODE(clist))
	  gdk_gc_set_line_attributes (clist->xor_gc, 1, GDK_LINE_SOLID, 0, 0);
	draw_xor_line (clist);
      }
  return FALSE;
}

static gint
eel_clist_button_release (GtkWidget      *widget,
			  GdkEventButton *event)
{
  EelCList *clist;
  gint button_actions;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (EEL_IS_CLIST (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  clist = EEL_CLIST (widget);

  button_actions = clist->button_actions[event->button - 1];
  if (button_actions == EEL_BUTTON_IGNORED)
    return FALSE;

  /* release on resize windows */
  if (EEL_CLIST_IN_DRAG(clist))
    {
      gpointer drag_data;
      gint width;
      gint x;
      gint i;

      i = clist->drag_pos;
      clist->drag_pos = -1;

      /* unblock attached dnd signal handler */
      drag_data = gtk_object_get_data (GTK_OBJECT (clist), "gtk-site-data");
      if (drag_data)
	gtk_signal_handler_unblock_by_data (GTK_OBJECT (clist), drag_data);

      EEL_CLIST_UNSET_FLAG (clist, CLIST_IN_DRAG);
      gtk_widget_get_pointer (widget, &x, NULL);
      gtk_grab_remove (widget);
      gdk_pointer_ungrab (event->time);

      if (clist->x_drag >= 0)
	draw_xor_line (clist);

      if (EEL_CLIST_ADD_MODE(clist))
	{
	  gdk_gc_set_line_attributes (clist->xor_gc, 1,
				      GDK_LINE_ON_OFF_DASH, 0, 0);
	  gdk_gc_set_dashes (clist->xor_gc, 0, "\4\4", 2);
	}

      width = new_column_width (clist, i, &x);
      eel_clist_set_column_width (clist, i, width);
      return FALSE;
    }

  if (clist->drag_button == event->button)
    {
      gint row;
      gint column;

      clist->drag_button = 0;
      clist->click_cell.row = -1;
      clist->click_cell.column = -1;

      remove_grab (clist);

      if (button_actions & EEL_BUTTON_SELECTS)
	{
	  switch (clist->selection_mode)
	    {
	    case GTK_SELECTION_EXTENDED:
	      if (!(event->state & GDK_SHIFT_MASK) ||
		  !GTK_WIDGET_CAN_FOCUS (widget) ||
		  event->x < 0 || event->x >= clist->clist_window_width ||
		  event->y < 0 || event->y >= clist->clist_window_height)
		EEL_CLIST_CLASS_FW (clist)->resync_selection
		  (clist, (GdkEvent *) event);
	      break;
	    case GTK_SELECTION_SINGLE:
	    case GTK_SELECTION_MULTIPLE:
	      if (get_selection_info (clist, event->x, event->y,
				      &row, &column))
		{
		  if (row >= 0 && row < clist->rows && clist->anchor == row)
		    toggle_row (clist, row, column, (GdkEvent *) event);
		}
	      clist->anchor = -1;
	      break;
	    default:
	      break;
	    }
	}
    }
  return FALSE;
}

static gint
eel_clist_motion (GtkWidget      *widget,
		  GdkEventMotion *event)
{
  EelCList *clist;
  gint x;
  gint y;
  gint row;
  gint new_width;
  gint button_actions = 0;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (EEL_IS_CLIST (widget), FALSE);

  clist = EEL_CLIST (widget);
  if (!(gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist)))
    return FALSE;

  if (clist->drag_button > 0)
    button_actions = clist->button_actions[clist->drag_button - 1];

  if (EEL_CLIST_IN_DRAG(clist))
    {
      if (event->is_hint || event->window != widget->window)
	gtk_widget_get_pointer (widget, &x, NULL);
      else
	x = event->x;
      
      new_width = new_column_width (clist, clist->drag_pos, &x);
      if (x != clist->x_drag)
	{
	  /* x_drag < 0 indicates that the xor line is already invisible */
	  if (clist->x_drag >= 0)
	    draw_xor_line (clist);

	  clist->x_drag = x;

	  if (clist->x_drag >= 0)
	    draw_xor_line (clist);
	}

      if (new_width <= MAX (COLUMN_MIN_WIDTH + 1,
			    clist->column[clist->drag_pos].min_width + 1))
	{
	  if (COLUMN_LEFT_XPIXEL (clist, clist->drag_pos) < 0 && x < 0)
	    eel_clist_moveto (clist, -1, clist->drag_pos, 0, 0);
	  return FALSE;
	}
      if (clist->column[clist->drag_pos].max_width >= COLUMN_MIN_WIDTH &&
	  new_width >= clist->column[clist->drag_pos].max_width)
	{
	  if (COLUMN_LEFT_XPIXEL (clist, clist->drag_pos) + new_width >
	      clist->clist_window_width && x < 0)
	    move_horizontal (clist,
			     COLUMN_LEFT_XPIXEL (clist, clist->drag_pos) +
			     new_width - clist->clist_window_width +
			     COLUMN_INSET + CELL_SPACING);
	  return FALSE;
	}
    }

  if (event->is_hint || event->window != clist->clist_window)
    gdk_window_get_pointer (clist->clist_window, &x, &y, NULL);

  if (EEL_CLIST_REORDERABLE(clist) && button_actions & EEL_BUTTON_DRAGS)
    {
      /* delayed drag start */
      if (event->window == clist->clist_window &&
	  clist->click_cell.row >= 0 && clist->click_cell.column >= 0 &&
	  (y < 0 || y >= clist->clist_window_height ||
	   x < 0 || x >= clist->clist_window_width  ||
	   y < ROW_TOP_YPIXEL (clist, clist->click_cell.row) ||
	   y >= (ROW_TOP_YPIXEL (clist, clist->click_cell.row) +
		 clist->row_height) ||
	   x < COLUMN_LEFT_XPIXEL (clist, clist->click_cell.column) ||
	   x >= (COLUMN_LEFT_XPIXEL(clist, clist->click_cell.column) + 
		 clist->column[clist->click_cell.column].area.width)))
	{
	  GtkTargetList  *target_list;

	  target_list = gtk_target_list_new (&clist_target_table, 1);
	  gtk_drag_begin (widget, target_list, GDK_ACTION_MOVE,
			  clist->drag_button, (GdkEvent *)event);

	}
      return TRUE;
    }

  /* horizontal autoscrolling */
  if (clist->hadjustment && LIST_WIDTH (clist) > clist->clist_window_width &&
      (x < 0 || x >= clist->clist_window_width))
    {
      if (clist->htimer)
	return FALSE;

      clist->htimer = gtk_timeout_add
	(SCROLL_TIME, (GtkFunction) horizontal_timeout, clist);

      if (!((x < 0 && clist->hadjustment->value == 0) ||
	    (x >= clist->clist_window_width &&
	     clist->hadjustment->value ==
	     LIST_WIDTH (clist) - clist->clist_window_width)))
	{
	  if (x < 0)
	    move_horizontal (clist, -1 + (x/2));
	  else
	    move_horizontal (clist, 1 + (x - clist->clist_window_width) / 2);
	}
    }

  if (EEL_CLIST_IN_DRAG(clist))
    return FALSE;

  /* vertical autoscrolling */
  row = ROW_FROM_YPIXEL (clist, y);

  /* don't scroll on last pixel row if it's a cell spacing */
  if (y == clist->clist_window_height - 1 &&
      y == ROW_TOP_YPIXEL (clist, row-1) + clist->row_height)
    return FALSE;

  if (LIST_HEIGHT (clist) > clist->clist_window_height &&
      (y < 0 || y >= clist->clist_window_height))
    {
      if (clist->vtimer)
	return FALSE;

      clist->vtimer = gtk_timeout_add (SCROLL_TIME,
				       (GtkFunction) vertical_timeout, clist);

      if (clist->drag_button &&
	  ((y < 0 && clist->focus_row == 0) ||
	   (y >= clist->clist_window_height &&
	    clist->focus_row == clist->rows - 1)))
	return FALSE;
    }

  row = CLAMP (row, 0, clist->rows - 1);

  if (button_actions & EEL_BUTTON_SELECTS &
      !gtk_object_get_data (GTK_OBJECT (widget), "gtk-site-data"))
    {
      if (row == clist->focus_row)
	return FALSE;

      eel_clist_draw_focus (widget);
      clist->focus_row = row;
      eel_clist_draw_focus (widget);

      switch (clist->selection_mode)
	{
	case GTK_SELECTION_BROWSE:
	  gtk_signal_emit (GTK_OBJECT (clist), clist_signals[SELECT_ROW],
			   clist->focus_row, -1, event);
	  break;
	case GTK_SELECTION_EXTENDED:
	  update_extended_selection (clist, clist->focus_row);
	  break;
	default:
	  break;
	}
    }
  
  if (ROW_TOP_YPIXEL(clist, row) < 0)
    move_vertical (clist, row, 0);
  else if (ROW_TOP_YPIXEL(clist, row) + clist->row_height >
	   clist->clist_window_height)
    move_vertical (clist, row, 1);

  return FALSE;
}

static void
eel_clist_size_request (GtkWidget      *widget,
			GtkRequisition *requisition)
{
  EelCList *clist;
  gint i;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));
  g_return_if_fail (requisition != NULL);

  clist = EEL_CLIST (widget);

  requisition->width = 0;
  requisition->height = 0;

  /* compute the size of the column title (title) area */
  clist->column_title_area.height = 0;
  if (EEL_CLIST_SHOW_TITLES(clist))
    for (i = 0; i < clist->columns; i++)
      if (clist->column[i].button)
	{
	  GtkRequisition child_requisition;
	  
	  gtk_widget_size_request (clist->column[i].button,
				   &child_requisition);
	  clist->column_title_area.height =
	    MAX (clist->column_title_area.height,
		 child_requisition.height);
	}

  requisition->width += (widget->style->klass->xthickness +
			 GTK_CONTAINER (widget)->border_width) * 2;
  requisition->height += (clist->column_title_area.height +
			  (widget->style->klass->ythickness +
			   GTK_CONTAINER (widget)->border_width) * 2);

  /* if (!clist->hadjustment) */
  requisition->width += list_requisition_width (clist);
  /* if (!clist->vadjustment) */
  requisition->height += LIST_HEIGHT (clist);
}

static void
eel_clist_size_allocate (GtkWidget     *widget,
			 GtkAllocation *allocation)
{
  EelCList *clist;
  GtkAllocation clist_allocation;
  gint border_width;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));
  g_return_if_fail (allocation != NULL);

  clist = EEL_CLIST (widget);
  widget->allocation = *allocation;
  border_width = GTK_CONTAINER (widget)->border_width;

  if (GTK_WIDGET_REALIZED (widget))
    {
      gdk_window_move_resize (widget->window,
			      allocation->x + border_width,
			      allocation->y + border_width,
			      allocation->width - border_width * 2,
			      allocation->height - border_width * 2);
    }

  /* use internal allocation structure for all the math
   * because it's easier than always subtracting the container
   * border width */
  clist->internal_allocation.x = 0;
  clist->internal_allocation.y = 0;
  clist->internal_allocation.width = MAX (1, (gint)allocation->width -
					  border_width * 2);
  clist->internal_allocation.height = MAX (1, (gint)allocation->height -
					   border_width * 2);
	
  /* allocate clist window assuming no scrollbars */
  clist_allocation.x = (clist->internal_allocation.x +
			widget->style->klass->xthickness);
  clist_allocation.y = (clist->internal_allocation.y +
			widget->style->klass->ythickness +
			clist->column_title_area.height);
  clist_allocation.width = MAX (1, (gint)clist->internal_allocation.width - 
				(2 * (gint)widget->style->klass->xthickness));
  clist_allocation.height = MAX (1, (gint)clist->internal_allocation.height -
				 (2 * (gint)widget->style->klass->ythickness) -
				 (gint)clist->column_title_area.height);
  
  clist->clist_window_width = clist_allocation.width;
  clist->clist_window_height = clist_allocation.height;
  
  if (GTK_WIDGET_REALIZED (widget))
    {
      gdk_window_move_resize (clist->clist_window,
			      clist_allocation.x,
			      clist_allocation.y,
			      clist_allocation.width,
			      clist_allocation.height);
    }
  
  /* position the window which holds the column title buttons */
  clist->column_title_area.x = widget->style->klass->xthickness;
  clist->column_title_area.y = widget->style->klass->ythickness;
  clist->column_title_area.width = clist_allocation.width;
  
  if (GTK_WIDGET_REALIZED (widget))
    {
      gdk_window_move_resize (clist->title_window,
			      clist->column_title_area.x,
			      clist->column_title_area.y,
			      clist->column_title_area.width,
			      clist->column_title_area.height);
    }
  
  /* column button allocation */
  size_allocate_columns (clist, FALSE);
  size_allocate_title_buttons (clist);

  adjust_adjustments (clist, TRUE);
}

/* GTKCONTAINER
 *   eel_clist_forall
 */
static void
eel_clist_forall (GtkContainer *container,
		  gboolean      include_internals,
		  GtkCallback   callback,
		  gpointer      callback_data)
{
  EelCList *clist;
  int i;

  g_return_if_fail (container != NULL);
  g_return_if_fail (EEL_IS_CLIST (container));
  g_return_if_fail (callback != NULL);

  if (!include_internals)
    return;

  clist = EEL_CLIST (container);
      
  /* callback for the column buttons */
  for (i = 0; i < clist->columns; i++)
    if (clist->column[i].button)
      (*callback) (clist->column[i].button, callback_data);
}

/* PRIVATE DRAWING FUNCTIONS
 *   get_cell_style
 *   draw_row
 *   draw_rows
 *   draw_xor_line
 *   clist_refresh
 */
static void
get_cell_style (EelCList     *clist,
		EelCListRow  *clist_row,
		gint          state,
		gint          column,
		GtkStyle    **style,
		GdkGC       **fg_gc,
		GdkGC       **bg_gc)
{
  gint fg_state;

  if ((state == GTK_STATE_NORMAL) &&
      (GTK_WIDGET (clist)->state == GTK_STATE_INSENSITIVE))
    fg_state = GTK_STATE_INSENSITIVE;
  else
    fg_state = state;

  if (clist_row->cell[column].style)
    {
      if (style)
	*style = clist_row->cell[column].style;
      if (fg_gc)
	*fg_gc = clist_row->cell[column].style->fg_gc[fg_state];
      if (bg_gc) {
	if (state == GTK_STATE_SELECTED)
	  *bg_gc = clist_row->cell[column].style->bg_gc[state];
	else
	  *bg_gc = clist_row->cell[column].style->base_gc[state];
      }
    }
  else if (clist_row->style)
    {
      if (style)
	*style = clist_row->style;
      if (fg_gc)
	*fg_gc = clist_row->style->fg_gc[fg_state];
      if (bg_gc) {
	if (state == GTK_STATE_SELECTED)
	  *bg_gc = clist_row->style->bg_gc[state];
	else
	  *bg_gc = clist_row->style->base_gc[state];
      }
    }
  else
    {
      if (style)
	*style = GTK_WIDGET (clist)->style;
      if (fg_gc)
	*fg_gc = GTK_WIDGET (clist)->style->fg_gc[fg_state];
      if (bg_gc) {
	if (state == GTK_STATE_SELECTED)
	  *bg_gc = GTK_WIDGET (clist)->style->bg_gc[state];
	else
	  *bg_gc = GTK_WIDGET (clist)->style->base_gc[state];
      }

      if (state != GTK_STATE_SELECTED)
	{
	  if (fg_gc && clist_row->fg_set)
	    *fg_gc = clist->fg_gc;
	  if (bg_gc && clist_row->bg_set)
	    *bg_gc = clist->bg_gc;
	}
    }
}

static void
draw_row (EelCList    *clist,
	  GdkRectangle     *area,
	  gint              row,
	  EelCListRow *clist_row)
{
  g_error ("this should not be called, the EelList drawing would be disrupted by this");
}


static void
draw_all (EelCList *clist)
{
  g_error ("this should not be called, the EelList drawing would be disrupted by this");
}

static void
draw_rows (EelCList *clist,
	   GdkRectangle  *area)
{
  g_error ("this should not be called, the EelList drawing would be disrupted by this");
}

static void                          
draw_xor_line (EelCList *clist)
{
  GtkWidget *widget;

  g_return_if_fail (clist != NULL);

  widget = GTK_WIDGET (clist);

  gdk_draw_line (widget->window, clist->xor_gc,
                 clist->x_drag,
		 widget->style->klass->ythickness,
                 clist->x_drag,
                 clist->column_title_area.height +
		 clist->clist_window_height + 1);
}

static void
clist_refresh (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));
  
  if (CLIST_UNFROZEN (clist))
    { 
      adjust_adjustments (clist, FALSE);
      EEL_CLIST_CLASS_FW (clist)->draw_all (clist);
    }
}

/* get cell from coordinates
 *   get_selection_info
 *   eel_clist_get_selection_info
 */
static gint
get_selection_info (EelCList *clist,
		    gint      x,
		    gint      y,
		    gint     *row,
		    gint     *column)
{
  gint trow, tcol;

  g_return_val_if_fail (clist != NULL, 0);
  g_return_val_if_fail (EEL_IS_CLIST (clist), 0);

  /* bounds checking, return false if the user clicked 
   * on a blank area */
  trow = ROW_FROM_YPIXEL (clist, y);
  if (trow >= clist->rows)
    return 0;

  if (row)
    *row = trow;

  tcol = COLUMN_FROM_XPIXEL (clist, x);
  if (tcol >= clist->columns)
    return 0;

  if (column)
    *column = tcol;

  return 1;
}

gint
eel_clist_get_selection_info (EelCList *clist, 
			      gint      x, 
			      gint      y, 
			      gint     *row, 
			      gint     *column)
{
  g_return_val_if_fail (clist != NULL, 0);
  g_return_val_if_fail (EEL_IS_CLIST (clist), 0);
  return get_selection_info (clist, x, y, row, column);
}

/* PRIVATE ADJUSTMENT FUNCTIONS
 *   adjust_adjustments
 *   vadjustment_changed
 *   hadjustment_changed
 *   vadjustment_value_changed
 *   hadjustment_value_changed 
 *   check_exposures
 */
static void
adjust_adjustments (EelCList *clist,
		    gboolean  block_resize)
{
  if (clist->vadjustment)
    {
      clist->vadjustment->page_size = clist->clist_window_height;
      clist->vadjustment->page_increment = clist->clist_window_height / 2;
      clist->vadjustment->step_increment = clist->row_height;
      clist->vadjustment->lower = 0;
      clist->vadjustment->upper = LIST_HEIGHT (clist);

      if (clist->clist_window_height - clist->voffset > LIST_HEIGHT (clist) ||
	  (clist->voffset + (gint)clist->vadjustment->value) != 0)
	{
	  clist->vadjustment->value = MAX (0, (LIST_HEIGHT (clist) -
					       clist->clist_window_height));
	  gtk_signal_emit_by_name (GTK_OBJECT (clist->vadjustment),
				   "value_changed");
	}
      gtk_signal_emit_by_name (GTK_OBJECT (clist->vadjustment), "changed");
    }

  if (clist->hadjustment)
    {
      clist->hadjustment->page_size = clist->clist_window_width;
      clist->hadjustment->page_increment = clist->clist_window_width / 2;
      clist->hadjustment->step_increment = 10;
      clist->hadjustment->lower = 0;
      clist->hadjustment->upper = LIST_WIDTH (clist);

      if (clist->clist_window_width - clist->hoffset > LIST_WIDTH (clist) ||
	  (clist->hoffset + (gint)clist->hadjustment->value) != 0)
	{
	  clist->hadjustment->value = MAX (0, (LIST_WIDTH (clist) -
					       clist->clist_window_width));
	  gtk_signal_emit_by_name (GTK_OBJECT (clist->hadjustment),
				   "value_changed");
	}
      gtk_signal_emit_by_name (GTK_OBJECT (clist->hadjustment), "changed");
    }

  if (!block_resize && (!clist->vadjustment || !clist->hadjustment))
    {
      GtkWidget *widget;
      GtkRequisition requisition;

      widget = GTK_WIDGET (clist);
      gtk_widget_size_request (widget, &requisition);

      if ((!clist->hadjustment &&
	   requisition.width != widget->allocation.width) ||
	  (!clist->vadjustment &&
	   requisition.height != widget->allocation.height))
	gtk_widget_queue_resize (widget);
    }
}

static void
vadjustment_changed (GtkAdjustment *adjustment,
		     gpointer       data)
{
  EelCList *clist;

  g_return_if_fail (adjustment != NULL);
  g_return_if_fail (data != NULL);

  clist = EEL_CLIST (data);
}

static void
hadjustment_changed (GtkAdjustment *adjustment,
		     gpointer       data)
{
  EelCList *clist;

  g_return_if_fail (adjustment != NULL);
  g_return_if_fail (data != NULL);

  clist = EEL_CLIST (data);
}

static void
vadjustment_value_changed (GtkAdjustment *adjustment,
			   gpointer       data)
{
  EelCList *clist;
  GdkRectangle area;
  gint diff, value;

  g_return_if_fail (adjustment != NULL);
  g_return_if_fail (data != NULL);
  g_return_if_fail (EEL_IS_CLIST (data));

  clist = EEL_CLIST (data);

  if (!GTK_WIDGET_DRAWABLE (clist) || adjustment != clist->vadjustment)
    return;

  value = adjustment->value;

  if (value > -clist->voffset)
    {
      /* scroll down */
      diff = value + clist->voffset;

      /* we have to re-draw the whole screen here... */
      if (diff >= clist->clist_window_height)
	{
	  clist->voffset = -value;
          EEL_CLIST_CLASS_FW (clist)->draw_all (clist);
	  return;
	}

      if ((diff != 0) && (diff != clist->clist_window_height))
	gdk_window_copy_area (clist->clist_window, clist->fg_gc,
			      0, 0, clist->clist_window, 0, diff,
			      clist->clist_window_width,
			      clist->clist_window_height - diff);

      area.x = 0;
      area.y = clist->clist_window_height - diff;
      area.width = clist->clist_window_width;
      area.height = diff;
    }
  else
    {
      /* scroll up */
      diff = -clist->voffset - value;

      /* we have to re-draw the whole screen here... */
      if (diff >= clist->clist_window_height)
	{
	  clist->voffset = -value;
          EEL_CLIST_CLASS_FW (clist)->draw_all (clist);
	  return;
	}

      if ((diff != 0) && (diff != clist->clist_window_height))
	gdk_window_copy_area (clist->clist_window, clist->fg_gc,
			      0, diff, clist->clist_window, 0, 0,
			      clist->clist_window_width,
			      clist->clist_window_height - diff);

      area.x = 0;
      area.y = 0;
      area.width = clist->clist_window_width;
      area.height = diff;
    }

  clist->voffset = -value;
  if ((diff != 0) && (diff != clist->clist_window_height))
    check_exposures (clist);

  EEL_CLIST_CLASS_FW (clist)->draw_rows (clist, &area);
}

static void
hadjustment_value_changed (GtkAdjustment *adjustment,
			   gpointer       data)
{
  EelCList *clist;
  GdkRectangle area;
  gint i;
  gint y = 0;
  gint diff = 0;
  gint value;

  g_return_if_fail (adjustment != NULL);
  g_return_if_fail (data != NULL);
  g_return_if_fail (EEL_IS_CLIST (data));

  clist = EEL_CLIST (data);

  if (!GTK_WIDGET_DRAWABLE (clist) || adjustment != clist->hadjustment)
    return;

  value = adjustment->value;

  /* move the column buttons and resize windows */
  for (i = 0; i < clist->columns; i++)
    {
      if (clist->column[i].button)
	{
	  clist->column[i].button->allocation.x -= value + clist->hoffset;
	  
	  if (clist->column[i].button->window)
	    {
	      gdk_window_move (clist->column[i].button->window,
			       clist->column[i].button->allocation.x,
			       clist->column[i].button->allocation.y);
	      
	      if (clist->column[i].window)
		gdk_window_move (clist->column[i].window,
				 clist->column[i].button->allocation.x +
				 clist->column[i].button->allocation.width - 
				 (DRAG_WIDTH / 2), 0); 
	    }
	}
    }

  if (value > -clist->hoffset)
    {
      /* scroll right */
      diff = value + clist->hoffset;
      
      clist->hoffset = -value;
      
      /* we have to re-draw the whole screen here... */
      if (diff >= clist->clist_window_width)
	{
          EEL_CLIST_CLASS_FW (clist)->draw_all (clist);
	  return;
	}

      if (GTK_WIDGET_CAN_FOCUS(clist) && GTK_WIDGET_HAS_FOCUS(clist) &&
	  !EEL_CLIST_CHILD_HAS_FOCUS(clist) && EEL_CLIST_ADD_MODE(clist))
	{
	  y = ROW_TOP_YPIXEL (clist, clist->focus_row);
	      
	  gdk_draw_rectangle (clist->clist_window, clist->xor_gc, FALSE, 0, y,
			      clist->clist_window_width - 1,
			      clist->row_height - 1);
	}
      gdk_window_copy_area (clist->clist_window,
			    clist->fg_gc,
			    0, 0,
			    clist->clist_window,
			    diff,
			    0,
			    clist->clist_window_width - diff,
			    clist->clist_window_height);

      area.x = clist->clist_window_width - diff;
    }
  else
    {
      /* scroll left */
      if (!(diff = -clist->hoffset - value))
	return;

      clist->hoffset = -value;
      
      /* we have to re-draw the whole screen here... */
      if (diff >= clist->clist_window_width)
	{
          EEL_CLIST_CLASS_FW (clist)->draw_all (clist);
	  return;
	}
      
      if (GTK_WIDGET_CAN_FOCUS(clist) && GTK_WIDGET_HAS_FOCUS(clist) &&
	  !EEL_CLIST_CHILD_HAS_FOCUS(clist) && EEL_CLIST_ADD_MODE(clist))
	{
	  y = ROW_TOP_YPIXEL (clist, clist->focus_row);
	  
	  gdk_draw_rectangle (clist->clist_window, clist->xor_gc, FALSE, 0, y,
			      clist->clist_window_width - 1,
			      clist->row_height - 1);
	}

      gdk_window_copy_area (clist->clist_window,
			    clist->fg_gc,
			    diff, 0,
			    clist->clist_window,
			    0,
			    0,
			    clist->clist_window_width - diff,
			    clist->clist_window_height);
	  
      area.x = 0;
    }

  area.y = 0;
  area.width = diff;
  area.height = clist->clist_window_height;

  check_exposures (clist);

  if (GTK_WIDGET_CAN_FOCUS(clist) && GTK_WIDGET_HAS_FOCUS(clist) &&
      !EEL_CLIST_CHILD_HAS_FOCUS(clist))
    {
      if (EEL_CLIST_ADD_MODE(clist))
	{
	  gint focus_row;
	  
	  focus_row = clist->focus_row;
	  clist->focus_row = -1;
          EEL_CLIST_CLASS_FW (clist)->draw_rows (clist, &area);
	  clist->focus_row = focus_row;
	  
	  gdk_draw_rectangle (clist->clist_window, clist->xor_gc,
			      FALSE, 0, y, clist->clist_window_width - 1,
			      clist->row_height - 1);
	  return;
	}
      else
	{
	  gint x0;
	  gint x1;
	  
	  if (area.x == 0)
	    {
	      x0 = clist->clist_window_width - 1;
	      x1 = diff;
	    }
	  else
	    {
	      x0 = 0;
	      x1 = area.x - 1;
	    }
	  
	  y = ROW_TOP_YPIXEL (clist, clist->focus_row);
	  gdk_draw_line (clist->clist_window, clist->xor_gc,
			 x0, y + 1, x0, y + clist->row_height - 2);
	  gdk_draw_line (clist->clist_window, clist->xor_gc,
			 x1, y + 1, x1, y + clist->row_height - 2);
	  
	}
    }
  EEL_CLIST_CLASS_FW (clist)->draw_rows (clist, &area);
}

static void
check_exposures (EelCList *clist)
{
  GdkEvent *event;

  if (!GTK_WIDGET_REALIZED (clist))
    return;

  /* Make sure graphics expose events are processed before scrolling
   * again */
  while ((event = gdk_event_get_graphics_expose (clist->clist_window)) != NULL)
    {
      gtk_widget_event (GTK_WIDGET (clist), event);
      if (event->expose.count == 0)
	{
	  gdk_event_free (event);
	  break;
	}
      gdk_event_free (event);
    }
}

/* PRIVATE 
 * Memory Allocation/Distruction Routines for EelCList stuctures
 *
 * functions:
 *   columns_new
 *   column_title_new
 *   columns_delete
 *   row_new
 *   row_delete
 */
static EelCListColumn *
columns_new (EelCList *clist)
{
  EelCListColumn *column;
  gint i;

  column = g_new (EelCListColumn, clist->columns);

  for (i = 0; i < clist->columns; i++)
    {
      column[i].area.x = 0;
      column[i].area.y = 0;
      column[i].area.width = 0;
      column[i].area.height = 0;
      column[i].title = NULL;
      column[i].button = NULL;
      column[i].window = NULL;
      column[i].width = 0;
      column[i].min_width = -1;
      column[i].max_width = -1;
      column[i].visible = TRUE;
      column[i].width_set = FALSE;
      column[i].resizeable = TRUE;
      column[i].auto_resize = FALSE;
      column[i].button_passive = FALSE;
      column[i].justification = GTK_JUSTIFY_LEFT;
    }

  return column;
}

static void
column_title_new (EelCList    *clist,
		  gint         column,
		  const gchar *title)
{
  if (clist->column[column].title)
    g_free (clist->column[column].title);

  clist->column[column].title = g_strdup (title);
}

static void
columns_delete (EelCList *clist)
{
  gint i;

  for (i = 0; i < clist->columns; i++)
    if (clist->column[i].title)
      g_free (clist->column[i].title);
      
  g_free (clist->column);
}

static EelCListRow *
row_new (EelCList *clist)
{
  int i;
  EelCListRow *clist_row;

  clist_row = g_chunk_new (EelCListRow, clist->row_mem_chunk);
  clist_row->cell = g_chunk_new (EelCell, clist->cell_mem_chunk);

  for (i = 0; i < clist->columns; i++)
    {
      clist_row->cell[i].type = EEL_CELL_EMPTY;
      clist_row->cell[i].vertical = 0;
      clist_row->cell[i].horizontal = 0;
      clist_row->cell[i].style = NULL;
    }

  clist_row->fg_set = FALSE;
  clist_row->bg_set = FALSE;
  clist_row->style = NULL;
  clist_row->selectable = TRUE;
  clist_row->state = GTK_STATE_NORMAL;
  clist_row->data = NULL;
  clist_row->destroy = NULL;

  return clist_row;
}

static void
row_delete (EelCList    *clist,
	    EelCListRow *clist_row)
{
  gint i;

  for (i = 0; i < clist->columns; i++)
    {
      EEL_CLIST_CLASS_FW (clist)->set_cell_contents
	(clist, clist_row, i, EEL_CELL_EMPTY, NULL, 0, NULL);
      if (clist_row->cell[i].style)
	{
	  if (GTK_WIDGET_REALIZED (clist))
	    gtk_style_detach (clist_row->cell[i].style);
	  gtk_style_unref (clist_row->cell[i].style);
	}
    }

  if (clist_row->style)
    {
      if (GTK_WIDGET_REALIZED (clist))
        gtk_style_detach (clist_row->style);
      gtk_style_unref (clist_row->style);
    }

  if (clist_row->destroy)
    clist_row->destroy (clist_row->data);

  g_mem_chunk_free (clist->cell_mem_chunk, clist_row->cell);
  g_mem_chunk_free (clist->row_mem_chunk, clist_row);
}

/* FOCUS FUNCTIONS
 *   eel_clist_focus
 *   eel_clist_draw_focus
 *   eel_clist_focus_in
 *   eel_clist_focus_out
 *   eel_clist_set_focus_child
 *   title_focus
 */
static gint
eel_clist_focus (GtkContainer     *container,
		 GtkDirectionType  direction)
{
  EelCList *clist;
  GtkWidget *focus_child;
  gint old_row;

  g_return_val_if_fail (container != NULL, FALSE);
  g_return_val_if_fail (EEL_IS_CLIST (container), FALSE);

  if (!GTK_WIDGET_IS_SENSITIVE (container))
    return FALSE;
  
  clist = EEL_CLIST (container);
  focus_child = container->focus_child;
  old_row = clist->focus_row;

  switch (direction)
    {
    case GTK_DIR_LEFT:
    case GTK_DIR_RIGHT:
      if (EEL_CLIST_CHILD_HAS_FOCUS(clist))
	{
	  if (title_focus (clist, direction))
	    return TRUE;
	  gtk_container_set_focus_child (container, NULL);
	  return FALSE;
	 }
      gtk_widget_grab_focus (GTK_WIDGET (container));
      return TRUE;
    case GTK_DIR_DOWN:
    case GTK_DIR_TAB_FORWARD:
      if (EEL_CLIST_CHILD_HAS_FOCUS(clist))
	{
	  gboolean tf = FALSE;

	  if (((focus_child && direction == GTK_DIR_DOWN) ||
	       !(tf = title_focus (clist, GTK_DIR_TAB_FORWARD)))
	      && clist->rows)
	    {
	      if (clist->focus_row < 0)
		{
		  clist->focus_row = 0;

		  if ((clist->selection_mode == GTK_SELECTION_BROWSE ||
		       clist->selection_mode == GTK_SELECTION_EXTENDED) &&
		      !clist->selection)
		    gtk_signal_emit (GTK_OBJECT (clist),
				     clist_signals[SELECT_ROW],
				     clist->focus_row, -1, NULL);
		}
	      gtk_widget_grab_focus (GTK_WIDGET (container));
	      return TRUE;
	    }

	  if (tf)
	    return TRUE;
	}
      
      EEL_CLIST_SET_FLAG (clist, CLIST_CHILD_HAS_FOCUS);
      break;
    case GTK_DIR_UP:
    case GTK_DIR_TAB_BACKWARD:
      if (!focus_child &&
	  EEL_CLIST_CHILD_HAS_FOCUS(clist) && clist->rows)
	{
	  if (clist->focus_row < 0)
	    {
	      clist->focus_row = 0;
	      if ((clist->selection_mode == GTK_SELECTION_BROWSE ||
		   clist->selection_mode == GTK_SELECTION_EXTENDED) &&
		  !clist->selection)
		gtk_signal_emit (GTK_OBJECT (clist),
				 clist_signals[SELECT_ROW],
				 clist->focus_row, -1, NULL);
	    }
	  gtk_widget_grab_focus (GTK_WIDGET (container));
	  return TRUE;
	}

      EEL_CLIST_SET_FLAG (clist, CLIST_CHILD_HAS_FOCUS);

      if (title_focus (clist, direction))
	return TRUE;

      break;
    default:
      break;
    }

  gtk_container_set_focus_child (container, NULL);
  return FALSE;
}

static void
eel_clist_draw_focus (GtkWidget *widget)
{
  EelCList *clist;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));

  if (!GTK_WIDGET_DRAWABLE (widget) || !GTK_WIDGET_CAN_FOCUS (widget))
    return;

  clist = EEL_CLIST (widget);
  if (clist->focus_row >= 0)
    gdk_draw_rectangle (clist->clist_window, clist->xor_gc, FALSE,
			0, ROW_TOP_YPIXEL(clist, clist->focus_row),
			clist->clist_window_width - 1,
			clist->row_height - 1);
}

static gint
eel_clist_focus_in (GtkWidget     *widget,
		    GdkEventFocus *event)
{
  EelCList *clist;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (EEL_IS_CLIST (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  GTK_WIDGET_SET_FLAGS (widget, GTK_HAS_FOCUS);
  EEL_CLIST_UNSET_FLAG (widget, CLIST_CHILD_HAS_FOCUS);

  clist = EEL_CLIST (widget);

  if (clist->selection_mode == GTK_SELECTION_BROWSE &&
      clist->selection == NULL && clist->focus_row > -1)
    {
      GList *list;

      list = g_list_nth (clist->row_list, clist->focus_row);
      if (list && EEL_CLIST_ROW (list)->selectable)
	gtk_signal_emit (GTK_OBJECT (clist), clist_signals[SELECT_ROW],
			 clist->focus_row, -1, event);
      else
	gtk_widget_draw_focus (widget);
    }
  else
    gtk_widget_draw_focus (widget);

  return FALSE;
}

static gint
eel_clist_focus_out (GtkWidget     *widget,
		     GdkEventFocus *event)
{
  EelCList *clist;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (EEL_IS_CLIST (widget), FALSE);
  g_return_val_if_fail (event != NULL, FALSE);

  GTK_WIDGET_UNSET_FLAGS (widget, GTK_HAS_FOCUS);
  EEL_CLIST_SET_FLAG (widget, CLIST_CHILD_HAS_FOCUS);

  gtk_widget_draw_focus (widget);
  
  clist = EEL_CLIST (widget);

  EEL_CLIST_CLASS_FW (widget)->resync_selection (clist, (GdkEvent *) event);

  return FALSE;
}

static void
eel_clist_set_focus_child (GtkContainer *container,
			   GtkWidget    *child)
{
  g_return_if_fail (container != NULL);
  g_return_if_fail (EEL_IS_CLIST (container));

  if (child)
    {
      g_return_if_fail (GTK_IS_WIDGET (child));
      EEL_CLIST_SET_FLAG (container, CLIST_CHILD_HAS_FOCUS);
    }

  parent_class->set_focus_child (container, child);
}

static gboolean
title_focus (EelCList *clist,
	     gint      dir)
{
  GtkWidget *focus_child;
  gboolean return_val = FALSE;
  gint last_column;
  gint d = 1;
  gint i = 0;
  gint j;

  if (!EEL_CLIST_SHOW_TITLES(clist))
    return FALSE;

  focus_child = GTK_CONTAINER (clist)->focus_child;

  for (last_column = clist->columns - 1;
       last_column >= 0 && !clist->column[last_column].visible; last_column--)
    ;
  
  switch (dir)
    {
    case GTK_DIR_TAB_BACKWARD:
    case GTK_DIR_UP:
      if (!focus_child || !EEL_CLIST_CHILD_HAS_FOCUS(clist))
	{
	  if (dir == GTK_DIR_UP)
	    i = COLUMN_FROM_XPIXEL (clist, 0);
	  else
	    i = last_column;
	  focus_child = clist->column[i].button;
	  dir = GTK_DIR_TAB_FORWARD;
	}
      else
	d = -1;
      break;
    case GTK_DIR_LEFT:
      d = -1;
      if (!focus_child)
	{
	  i = last_column;
	  focus_child = clist->column[i].button;
	}
      break;
    case GTK_DIR_RIGHT:
      if (!focus_child)
	{
	  i = 0;
	  focus_child = clist->column[i].button;
	}
      break;
    }

  if (focus_child)
    while (i < clist->columns)
      {
	if (clist->column[i].button == focus_child)
	  {
	    if (clist->column[i].button && 
		GTK_WIDGET_VISIBLE (clist->column[i].button) &&
		GTK_IS_CONTAINER (clist->column[i].button) &&
		!GTK_WIDGET_HAS_FOCUS(clist->column[i].button))
	      if (gtk_container_focus 
		  (GTK_CONTAINER (clist->column[i].button), dir))
		{
		  return_val = TRUE;
		  i -= d;
		}
	    if (!return_val && dir == GTK_DIR_UP)
	      return FALSE;
	    i += d;
	    break;
	  }
	i++;
      }

  j = i;

  if (!return_val)
    while (j >= 0 && j < clist->columns)
      {
	if (clist->column[j].button &&
	    GTK_WIDGET_VISIBLE (clist->column[j].button))
	  {
	    if (GTK_IS_CONTAINER (clist->column[j].button) &&
		gtk_container_focus 
		(GTK_CONTAINER (clist->column[j].button), dir))
	      {
		return_val = TRUE;
		break;
	      }
	    else if (GTK_WIDGET_CAN_FOCUS (clist->column[j].button))
	      {
		gtk_widget_grab_focus (clist->column[j].button);
		return_val = TRUE;
		break;
	      }
	  }
	j += d;
      }
  
  if (return_val)
    {
      if (COLUMN_LEFT_XPIXEL (clist, j) < CELL_SPACING + COLUMN_INSET)
	eel_clist_moveto (clist, -1, j, 0, 0);
      else if (COLUMN_LEFT_XPIXEL(clist, j) + clist->column[j].area.width >
	       clist->clist_window_width)
	{
	  if (j == last_column)
	    eel_clist_moveto (clist, -1, j, 0, 0);
	  else
	    eel_clist_moveto (clist, -1, j, 0, 1);
	}
    }
  return return_val;
}

/* PRIVATE SCROLLING FUNCTIONS
 *   move_focus_row
 *   scroll_horizontal
 *   scroll_vertical
 *   move_horizontal
 *   move_vertical
 *   horizontal_timeout
 *   vertical_timeout
 *   remove_grab
 */
static void
move_focus_row (EelCList      *clist,
		GtkScrollType  scroll_type,
		gfloat         position)
{
  GtkWidget *widget;

  g_return_if_fail (clist != 0);
  g_return_if_fail (EEL_IS_CLIST (clist));

  widget = GTK_WIDGET (clist);

  switch (scroll_type)
    {
    case GTK_SCROLL_STEP_BACKWARD:
      if (clist->focus_row <= 0)
	return;
      eel_clist_draw_focus (widget);
      clist->focus_row--;
      eel_clist_draw_focus (widget);
      break;
    case GTK_SCROLL_STEP_FORWARD:
      if (clist->focus_row >= clist->rows - 1)
	return;
      eel_clist_draw_focus (widget);
      clist->focus_row++;
      eel_clist_draw_focus (widget);
      break;
    case GTK_SCROLL_PAGE_BACKWARD:
      if (clist->focus_row <= 0)
	return;
      eel_clist_draw_focus (widget);
      clist->focus_row = MAX (0, clist->focus_row -
			      (2 * clist->clist_window_height -
			       clist->row_height - CELL_SPACING) / 
			      (2 * (clist->row_height + CELL_SPACING)));
      eel_clist_draw_focus (widget);
      break;
    case GTK_SCROLL_PAGE_FORWARD:
      if (clist->focus_row >= clist->rows - 1)
	return;
      eel_clist_draw_focus (widget);
      clist->focus_row = MIN (clist->rows - 1, clist->focus_row + 
			      (2 * clist->clist_window_height -
			       clist->row_height - CELL_SPACING) / 
			      (2 * (clist->row_height + CELL_SPACING)));
      eel_clist_draw_focus (widget);
      break;
    case GTK_SCROLL_JUMP:
      if (position >= 0 && position <= 1)
	{
	  eel_clist_draw_focus (widget);
	  clist->focus_row = position * (clist->rows - 1);
	  eel_clist_draw_focus (widget);
	}
      break;
    default:
      break;
    }
}

static void
scroll_horizontal (EelCList      *clist,
		   GtkScrollType  scroll_type,
		   gfloat         position)
{
  gint column = 0;
  gint last_column;

  g_return_if_fail (clist != 0);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist))
    return;

  for (last_column = clist->columns - 1;
       last_column >= 0 && !clist->column[last_column].visible; last_column--)
    ;

  switch (scroll_type)
    {
    case GTK_SCROLL_STEP_BACKWARD:
      column = COLUMN_FROM_XPIXEL (clist, 0);
      if (COLUMN_LEFT_XPIXEL (clist, column) - CELL_SPACING - COLUMN_INSET >= 0
	  && column > 0)
	column--;
      break;
    case GTK_SCROLL_STEP_FORWARD:
      column = COLUMN_FROM_XPIXEL (clist, clist->clist_window_width);
      if (column < 0)
	return;
      if (COLUMN_LEFT_XPIXEL (clist, column) +
	  clist->column[column].area.width +
	  CELL_SPACING + COLUMN_INSET - 1 <= clist->clist_window_width &&
	  column < last_column)
	column++;
      break;
    case GTK_SCROLL_PAGE_BACKWARD:
    case GTK_SCROLL_PAGE_FORWARD:
      return;
    case GTK_SCROLL_JUMP:
      if (position >= 0 && position <= 1)
	{
	  gint vis_columns = 0;
	  gint i;

	  for (i = 0; i <= last_column; i++)
 	    if (clist->column[i].visible)
	      vis_columns++;

	  column = position * vis_columns;

	  for (i = 0; i <= last_column && column > 0; i++)
	    if (clist->column[i].visible)
	      column--;

	  column = i;
	}
      else
	return;
      break;
    default:
      break;
    }

  if (COLUMN_LEFT_XPIXEL (clist, column) < CELL_SPACING + COLUMN_INSET)
    eel_clist_moveto (clist, -1, column, 0, 0);
  else if (COLUMN_LEFT_XPIXEL (clist, column) + CELL_SPACING + COLUMN_INSET - 1
	   + clist->column[column].area.width > clist->clist_window_width)
    {
      if (column == last_column)
	eel_clist_moveto (clist, -1, column, 0, 0);
      else
	eel_clist_moveto (clist, -1, column, 0, 1);
    }
}

static void
scroll_vertical (EelCList      *clist,
		 GtkScrollType  scroll_type,
		 gfloat         position)
{
  gint old_focus_row;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist))
    return;

  switch (clist->selection_mode)
    {
    case GTK_SELECTION_EXTENDED:
      if (clist->anchor >= 0)
	return;
    case GTK_SELECTION_BROWSE:

      old_focus_row = clist->focus_row;
      move_focus_row (clist, scroll_type, position);

      if (old_focus_row != clist->focus_row)
	{
	  if (clist->selection_mode == GTK_SELECTION_BROWSE)
	    gtk_signal_emit (GTK_OBJECT (clist), clist_signals[UNSELECT_ROW],
			     old_focus_row, -1, NULL);
	  else if (!EEL_CLIST_ADD_MODE(clist))
	    {
	      eel_clist_unselect_all (clist);
	      clist->undo_anchor = old_focus_row;
	    }
	}

      switch (eel_clist_row_is_visible (clist, clist->focus_row))
	{
	case GTK_VISIBILITY_NONE:
	  if (old_focus_row != clist->focus_row &&
	      !(clist->selection_mode == GTK_SELECTION_EXTENDED &&
		EEL_CLIST_ADD_MODE(clist)))
	    gtk_signal_emit (GTK_OBJECT (clist), clist_signals[SELECT_ROW],
			     clist->focus_row, -1, NULL);
	  switch (scroll_type)
	    {
	    case GTK_SCROLL_STEP_BACKWARD:
	    case GTK_SCROLL_PAGE_BACKWARD:
	      eel_clist_moveto (clist, clist->focus_row, -1, 0, 0);
	      break;
	    case GTK_SCROLL_STEP_FORWARD:
	    case GTK_SCROLL_PAGE_FORWARD:
	      eel_clist_moveto (clist, clist->focus_row, -1, 1, 0);
	      break;
	    case GTK_SCROLL_JUMP:
	      eel_clist_moveto (clist, clist->focus_row, -1, 0.5, 0);
	      break;
	    default:
	      break;
	    }
	  break;
	case GTK_VISIBILITY_PARTIAL:
	  switch (scroll_type)
	    {
	    case GTK_SCROLL_STEP_BACKWARD:
	    case GTK_SCROLL_PAGE_BACKWARD:
	      eel_clist_moveto (clist, clist->focus_row, -1, 0, 0);
	      break;
	    case GTK_SCROLL_STEP_FORWARD:
	    case GTK_SCROLL_PAGE_FORWARD:
	      eel_clist_moveto (clist, clist->focus_row, -1, 1, 0);
	      break;
	    case GTK_SCROLL_JUMP:
	      eel_clist_moveto (clist, clist->focus_row, -1, 0.5, 0);
	      break;
	    default:
	      break;
	    }
	default:
	  if (old_focus_row != clist->focus_row &&
	      !(clist->selection_mode == GTK_SELECTION_EXTENDED &&
		EEL_CLIST_ADD_MODE(clist)))
	    gtk_signal_emit (GTK_OBJECT (clist), clist_signals[SELECT_ROW],
			     clist->focus_row, -1, NULL);
	  break;
	}
      break;
    default:
      move_focus_row (clist, scroll_type, position);

      if (ROW_TOP_YPIXEL (clist, clist->focus_row) + clist->row_height >
	  clist->clist_window_height)
	eel_clist_moveto (clist, clist->focus_row, -1, 1, 0);
      else if (ROW_TOP_YPIXEL (clist, clist->focus_row) < 0)
	eel_clist_moveto (clist, clist->focus_row, -1, 0, 0);
      break;
    }
}

static void
move_horizontal (EelCList *clist,
		 gint      diff)
{
  gfloat value;

  if (!clist->hadjustment)
    return;

  value = CLAMP (clist->hadjustment->value + diff, 0.0,
		 clist->hadjustment->upper - clist->hadjustment->page_size);
  gtk_adjustment_set_value(clist->hadjustment, value);
}

static void
move_vertical (EelCList *clist,
	       gint      row,
	       gfloat    align)
{
  gfloat value;

  if (!clist->vadjustment)
    return;

  value = (ROW_TOP_YPIXEL (clist, row) - clist->voffset -
	   align * (clist->clist_window_height - clist->row_height) +
	   (2 * align - 1) * CELL_SPACING);

  if (value + clist->vadjustment->page_size > clist->vadjustment->upper)
    value = clist->vadjustment->upper - clist->vadjustment->page_size;

  gtk_adjustment_set_value(clist->vadjustment, value);
}

static gint
horizontal_timeout (EelCList *clist)
{
  GdkEventMotion event = { 0 };

  GDK_THREADS_ENTER ();

  clist->htimer = 0;

  event.type = GDK_MOTION_NOTIFY;
  event.send_event = TRUE;

  eel_clist_motion (GTK_WIDGET (clist), &event);

  GDK_THREADS_LEAVE ();
  
  return FALSE;
}

static gint
vertical_timeout (EelCList *clist)
{
  GdkEventMotion event = { 0 };

  GDK_THREADS_ENTER ();

  clist->vtimer = 0;

  event.type = GDK_MOTION_NOTIFY;
  event.send_event = TRUE;

  eel_clist_motion (GTK_WIDGET (clist), &event);

  GDK_THREADS_LEAVE ();

  return FALSE;
}

static void
remove_grab (EelCList *clist)
{
  if (GTK_WIDGET_HAS_GRAB (clist))
    {
      gtk_grab_remove (GTK_WIDGET (clist));
      if (gdk_pointer_is_grabbed ())
	gdk_pointer_ungrab (GDK_CURRENT_TIME);
    }

  if (clist->htimer)
    {
      gtk_timeout_remove (clist->htimer);
      clist->htimer = 0;
    }

  if (clist->vtimer)
    {
      gtk_timeout_remove (clist->vtimer);
      clist->vtimer = 0;
    }
}

/* PUBLIC SORTING FUNCTIONS
 * eel_clist_sort
 * eel_clist_set_compare_func
 * eel_clist_set_auto_sort
 * eel_clist_set_sort_type
 * eel_clist_set_sort_column
 */
void
eel_clist_sort (EelCList *clist)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  EEL_CLIST_CLASS_FW (clist)->sort_list (clist);
}

void
eel_clist_set_compare_func (EelCList            *clist,
			    EelCListCompareFunc  cmp_func)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  clist->compare = (cmp_func) ? cmp_func : default_compare;
}

void       
eel_clist_set_auto_sort (EelCList *clist,
			 gboolean  auto_sort)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));
  
  if (EEL_CLIST_AUTO_SORT(clist) && !auto_sort)
    EEL_CLIST_UNSET_FLAG (clist, CLIST_AUTO_SORT);
  else if (!EEL_CLIST_AUTO_SORT(clist) && auto_sort)
    {
      EEL_CLIST_SET_FLAG (clist, CLIST_AUTO_SORT);
      eel_clist_sort (clist);
    }
}

void       
eel_clist_set_sort_type (EelCList    *clist,
			 GtkSortType  sort_type)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));
  
  clist->sort_type = sort_type;
}

void
eel_clist_set_sort_column (EelCList *clist,
			   gint      column)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (column < 0 || column >= clist->columns)
    return;

  clist->sort_column = column;
}

/* PRIVATE SORTING FUNCTIONS
 *   default_compare
 *   real_sort_list
 *   eel_clist_merge
 *   eel_clist_mergesort
 */
static gint
default_compare (EelCList      *clist,
		 gconstpointer  ptr1,
		 gconstpointer  ptr2)
{
  char *text1 = NULL;
  char *text2 = NULL;

  EelCListRow *row1 = (EelCListRow *) ptr1;
  EelCListRow *row2 = (EelCListRow *) ptr2;

  switch (row1->cell[clist->sort_column].type)
    {
    case EEL_CELL_TEXT:
    case EEL_CELL_LINK_TEXT:
      text1 = EEL_CELL_TEXT (row1->cell[clist->sort_column])->text;
      break;
    case EEL_CELL_PIXTEXT:
      text1 = EEL_CELL_PIXTEXT (row1->cell[clist->sort_column])->text;
      break;
    default:
      break;
    }
 
  switch (row2->cell[clist->sort_column].type)
    {
    case EEL_CELL_TEXT:
    case EEL_CELL_LINK_TEXT:
      text2 = EEL_CELL_TEXT (row2->cell[clist->sort_column])->text;
      break;
    case EEL_CELL_PIXTEXT:
      text2 = EEL_CELL_PIXTEXT (row2->cell[clist->sort_column])->text;
      break;
    default:
      break;
    }

  if (!text2)
    return (text1 != NULL);

  if (!text1)
    return -1;

  return strcmp (text1, text2);
}

static void
real_sort_list (EelCList *clist)
{
  GList *list;
  GList *work;
  gint i;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (clist->rows <= 1)
    return;

  if (gdk_pointer_is_grabbed () && GTK_WIDGET_HAS_GRAB (clist))
    return;

  if (clist->anchor != -1 && clist->selection_mode == GTK_SELECTION_EXTENDED)
    {
      EEL_CLIST_CLASS_FW (clist)->resync_selection (clist, NULL);
      g_list_free (clist->undo_selection);
      g_list_free (clist->undo_unselection);
      clist->undo_selection = NULL;
      clist->undo_unselection = NULL;
    }
   
  clist->row_list = eel_clist_mergesort (clist, clist->row_list, clist->rows);

  work = clist->selection;

  for (i = 0, list = clist->row_list; i < clist->rows; i++, list = list->next)
    {
      if (EEL_CLIST_ROW (list)->state == GTK_STATE_SELECTED)
	{
	  work->data = GINT_TO_POINTER (i);
	  work = work->next;
	}
      
      if (i == clist->rows - 1)
	clist->row_list_end = list;
    }

  CLIST_REFRESH (clist);
}

static GList *
eel_clist_merge (EelCList *clist,
		 GList    *a,         /* first list to merge */
		 GList    *b)         /* second list to merge */
{
  GList z = { 0 };                    /* auxiliary node */
  GList *c;
  gint cmp;

  c = &z;

  while (a || b)
    {
      if (a && !b)
	{
	  c->next = a;
	  a->prev = c;
	  c = a;
	  a = a->next;
	  break;
	}
      else if (!a && b)
	{
	  c->next = b;
	  b->prev = c;
	  c = b;
	  b = b->next;
	  break;
	}
      else /* a && b */
	{
	  cmp = clist->compare (clist, EEL_CLIST_ROW (a), EEL_CLIST_ROW (b));
	  if ((cmp >= 0 && clist->sort_type == GTK_SORT_DESCENDING) ||
	      (cmp <= 0 && clist->sort_type == GTK_SORT_ASCENDING) ||
	      (a && !b))
	    {
	      c->next = a;
	      a->prev = c;
	      c = a;
	      a = a->next;
	    }
	  else
	    {
	      c->next = b;
	      b->prev = c;
	      c = b;
	      b = b->next;
	    }
	}
    }

  z.next->prev = NULL;
  return z.next;
}

static GList *
eel_clist_mergesort (EelCList *clist,
		     GList    *list,         /* the list to sort */
		     gint      num)          /* the list's length */
{
  GList *half;
  gint i;

  if (num == 1)
    {
      return list;
    }
  else
    {
      /* move "half" to the middle */
      half = list;
      for (i = 0; i < num / 2; i++)
	half = half->next;

      /* cut the list in two */
      half->prev->next = NULL;
      half->prev = NULL;

      /* recursively sort both lists */
      return eel_clist_merge (clist,
		       eel_clist_mergesort (clist, list, num / 2),
		       eel_clist_mergesort (clist, half, num - num / 2));
    }
}

/************************/

static void
drag_source_info_destroy (gpointer data)
{
  EelCListCellInfo *info = data;

  g_free (info);
}

static void
drag_dest_info_destroy (gpointer data)
{
  EelCListDestInfo *info = data;

  g_free (info);
}

static void
drag_dest_cell (EelCList         *clist,
		gint              x,
		gint              y,
		EelCListDestInfo *dest_info)
{
  GtkWidget *widget;

  widget = GTK_WIDGET (clist);

  dest_info->insert_pos = EEL_CLIST_DRAG_NONE;

  y -= (GTK_CONTAINER (clist)->border_width +
	widget->style->klass->ythickness +
	clist->column_title_area.height);

  dest_info->cell.row = ROW_FROM_YPIXEL (clist, y);
  if (dest_info->cell.row >= clist->rows)
    {
      dest_info->cell.row = clist->rows - 1;
      y = ROW_TOP_YPIXEL (clist, dest_info->cell.row) + clist->row_height;
    }
  if (dest_info->cell.row < -1)
    dest_info->cell.row = -1;

  x -= GTK_CONTAINER (widget)->border_width + widget->style->klass->xthickness;
  dest_info->cell.column = COLUMN_FROM_XPIXEL (clist, x);

  if (dest_info->cell.row >= 0)
    {
      gint y_delta;
      gint h = 0;

      y_delta = y - ROW_TOP_YPIXEL (clist, dest_info->cell.row);
      
      if (EEL_CLIST_DRAW_DRAG_RECT(clist))
	{
	  dest_info->insert_pos = EEL_CLIST_DRAG_INTO;
	  h = clist->row_height / 4;
	}
      else if (EEL_CLIST_DRAW_DRAG_LINE(clist))
	{
	  dest_info->insert_pos = EEL_CLIST_DRAG_BEFORE;
	  h = clist->row_height / 2;
	}

      if (EEL_CLIST_DRAW_DRAG_LINE(clist))
	{
	  if (y_delta < h)
	    dest_info->insert_pos = EEL_CLIST_DRAG_BEFORE;
	  else if (clist->row_height - y_delta < h)
	    dest_info->insert_pos = EEL_CLIST_DRAG_AFTER;
	}
    }
}

static void
eel_clist_drag_begin (GtkWidget	     *widget,
		      GdkDragContext *context)
{
  EelCList *clist;
  EelCListCellInfo *info;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));
  g_return_if_fail (context != NULL);

  clist = EEL_CLIST (widget);

  clist->drag_button = 0;
  remove_grab (clist);

  switch (clist->selection_mode)
    {
    case GTK_SELECTION_EXTENDED:
      update_extended_selection (clist, clist->focus_row);
      EEL_CLIST_CLASS_FW (clist)->resync_selection (clist, NULL);
      break;
    case GTK_SELECTION_SINGLE:
    case GTK_SELECTION_MULTIPLE:
      clist->anchor = -1;
    case GTK_SELECTION_BROWSE:
      break;
    }

  info = g_dataset_get_data (context, "gtk-clist-drag-source");

  if (!info)
    {
      info = g_new (EelCListCellInfo, 1);

      if (clist->click_cell.row < 0)
	clist->click_cell.row = 0;
      else if (clist->click_cell.row >= clist->rows)
	clist->click_cell.row = clist->rows - 1;
      info->row = clist->click_cell.row;
      info->column = clist->click_cell.column;

      g_dataset_set_data_full (context, "gtk-clist-drag-source", info,
			       drag_source_info_destroy);
    }

  if (EEL_CLIST_USE_DRAG_ICONS (clist))
    gtk_drag_set_icon_default (context);
}

static void
eel_clist_drag_end (GtkWidget	   *widget,
		    GdkDragContext *context)
{
  EelCList *clist;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));
  g_return_if_fail (context != NULL);

  clist = EEL_CLIST (widget);

  clist->click_cell.row = -1;
  clist->click_cell.column = -1;
}

static void
eel_clist_drag_leave (GtkWidget      *widget,
		      GdkDragContext *context,
		      guint           time)
{
  EelCList *clist;
  EelCListDestInfo *dest_info;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));
  g_return_if_fail (context != NULL);

  clist = EEL_CLIST (widget);

  dest_info = g_dataset_get_data (context, "gtk-clist-drag-dest");
  
  if (dest_info)
    {
      if (dest_info->cell.row >= 0 &&
	  EEL_CLIST_REORDERABLE(clist) &&
	  gtk_drag_get_source_widget (context) == widget)
	{
	  GList *list;
	  GdkAtom atom = gdk_atom_intern ("gtk-clist-drag-reorder", FALSE);

	  list = context->targets;
	  while (list)
	    {
	      if (atom == GPOINTER_TO_UINT (list->data))
		{
		  EEL_CLIST_CLASS_FW (clist)->draw_drag_highlight
		    (clist,
		     g_list_nth (clist->row_list, dest_info->cell.row)->data,
		     dest_info->cell.row, dest_info->insert_pos);
		  break;
		}
	      list = list->next;
	    }
	}
      g_dataset_remove_data (context, "gtk-clist-drag-dest");
    }
}

static gint
eel_clist_drag_motion (GtkWidget      *widget,
		       GdkDragContext *context,
		       gint            x,
		       gint            y,
		       guint           time)
{
  EelCList *clist;
  EelCListDestInfo new_info;
  EelCListDestInfo *dest_info;

  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (EEL_IS_CLIST (widget), FALSE);

  clist = EEL_CLIST (widget);

  dest_info = g_dataset_get_data (context, "gtk-clist-drag-dest");

  if (!dest_info)
    {
      dest_info = g_new (EelCListDestInfo, 1);

      dest_info->insert_pos  = EEL_CLIST_DRAG_NONE;
      dest_info->cell.row    = -1;
      dest_info->cell.column = -1;

      g_dataset_set_data_full (context, "gtk-clist-drag-dest", dest_info,
			       drag_dest_info_destroy);
    }

  drag_dest_cell (clist, x, y, &new_info);

  if (EEL_CLIST_REORDERABLE (clist))
    {
      GList *list;
      GdkAtom atom = gdk_atom_intern ("gtk-clist-drag-reorder", FALSE);

      list = context->targets;
      while (list)
	{
	  if (atom == GPOINTER_TO_UINT (list->data))
	    break;
	  list = list->next;
	}

      if (list)
	{
	  if (gtk_drag_get_source_widget (context) != widget ||
	      new_info.insert_pos == EEL_CLIST_DRAG_NONE ||
	      new_info.cell.row == clist->click_cell.row ||
	      (new_info.cell.row == clist->click_cell.row - 1 &&
	       new_info.insert_pos == EEL_CLIST_DRAG_AFTER) ||
	      (new_info.cell.row == clist->click_cell.row + 1 &&
	       new_info.insert_pos == EEL_CLIST_DRAG_BEFORE))
	    {
	      if (dest_info->cell.row < 0)
		{
		  gdk_drag_status (context, GDK_ACTION_DEFAULT, time);
		  return FALSE;
		}
	      return TRUE;
	    }
		
	  if (new_info.cell.row != dest_info->cell.row ||
	      (new_info.cell.row == dest_info->cell.row &&
	       dest_info->insert_pos != new_info.insert_pos))
	    {
	      if (dest_info->cell.row >= 0)
		EEL_CLIST_CLASS_FW (clist)->draw_drag_highlight
		  (clist, g_list_nth (clist->row_list,
				      dest_info->cell.row)->data,
		   dest_info->cell.row, dest_info->insert_pos);

	      dest_info->insert_pos  = new_info.insert_pos;
	      dest_info->cell.row    = new_info.cell.row;
	      dest_info->cell.column = new_info.cell.column;
	      
	      EEL_CLIST_CLASS_FW (clist)->draw_drag_highlight
		(clist, g_list_nth (clist->row_list,
				    dest_info->cell.row)->data,
		 dest_info->cell.row, dest_info->insert_pos);

	      gdk_drag_status (context, context->suggested_action, time);
	    }
	  return TRUE;
	}
    }

  dest_info->insert_pos  = new_info.insert_pos;
  dest_info->cell.row    = new_info.cell.row;
  dest_info->cell.column = new_info.cell.column;
  return TRUE;
}

static gboolean
eel_clist_drag_drop (GtkWidget      *widget,
		     GdkDragContext *context,
		     gint            x,
		     gint            y,
		     guint           time)
{
  g_return_val_if_fail (widget != NULL, FALSE);
  g_return_val_if_fail (EEL_IS_CLIST (widget), FALSE);
  g_return_val_if_fail (context != NULL, FALSE);

  if (EEL_CLIST_REORDERABLE (widget) &&
      gtk_drag_get_source_widget (context) == widget)
    {
      GList *list;
      GdkAtom atom = gdk_atom_intern ("gtk-clist-drag-reorder", FALSE);

      list = context->targets;
      while (list)
	{
	  if (atom == GPOINTER_TO_UINT (list->data))
	    return TRUE;
	  list = list->next;
	}
    }
  return FALSE;
}

static void
eel_clist_drag_data_received (GtkWidget        *widget,
			      GdkDragContext   *context,
			      gint              x,
			      gint              y,
			      GtkSelectionData *selection_data,
			      guint             info,
			      guint             time)
{
  EelCList *clist;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));
  g_return_if_fail (context != NULL);
  g_return_if_fail (selection_data != NULL);

  clist = EEL_CLIST (widget);

  if (EEL_CLIST_REORDERABLE (clist) &&
      gtk_drag_get_source_widget (context) == widget &&
      selection_data->target ==
      gdk_atom_intern ("gtk-clist-drag-reorder", FALSE) &&
      selection_data->format == GTK_TYPE_POINTER &&
      selection_data->length == sizeof (EelCListCellInfo))
    {
      EelCListCellInfo *source_info;

      source_info = (EelCListCellInfo *)(selection_data->data);
      if (source_info)
	{
	  EelCListDestInfo dest_info;

	  drag_dest_cell (clist, x, y, &dest_info);

	  if (dest_info.insert_pos == EEL_CLIST_DRAG_AFTER)
	    dest_info.cell.row++;
	  if (source_info->row < dest_info.cell.row)
	    dest_info.cell.row--;
	  if (dest_info.cell.row != source_info->row)
	    eel_clist_row_move (clist, source_info->row, dest_info.cell.row);

	  g_dataset_remove_data (context, "gtk-clist-drag-dest");
	}
    }
}

static void  
eel_clist_drag_data_get (GtkWidget        *widget,
			 GdkDragContext   *context,
			 GtkSelectionData *selection_data,
			 guint             info,
			 guint             time)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (EEL_IS_CLIST (widget));
  g_return_if_fail (context != NULL);
  g_return_if_fail (selection_data != NULL);

  if (selection_data->target ==
      gdk_atom_intern ("gtk-clist-drag-reorder", FALSE))
    {
      EelCListCellInfo *info;

      info = g_dataset_get_data (context, "gtk-clist-drag-source");

      if (info)
	{
	  EelCListCellInfo ret_info;

	  ret_info.row = info->row;
	  ret_info.column = info->column;

	  gtk_selection_data_set (selection_data, selection_data->target,
				  GTK_TYPE_POINTER, (guchar *) &ret_info,
				  sizeof (EelCListCellInfo));
	}
      else
	gtk_selection_data_set (selection_data, selection_data->target,
				GTK_TYPE_POINTER, NULL,	0);
    }
}

static void
draw_drag_highlight (EelCList        *clist,
		     EelCListRow     *dest_row,
		     gint             dest_row_number,
		     EelCListDragPos  drag_pos)
{
  gint y;

  y = ROW_TOP_YPIXEL (clist, dest_row_number) - 1;

  switch (drag_pos)
    {
    case EEL_CLIST_DRAG_NONE:
      break;
    case EEL_CLIST_DRAG_AFTER:
      y += clist->row_height + 1;
    case EEL_CLIST_DRAG_BEFORE:
      gdk_draw_line (clist->clist_window, clist->xor_gc,
		     0, y, clist->clist_window_width, y);
      break;
    case EEL_CLIST_DRAG_INTO:
      gdk_draw_rectangle (clist->clist_window, clist->xor_gc, FALSE, 0, y,
			  clist->clist_window_width - 1, clist->row_height);
      break;
    }
}

void
eel_clist_set_reorderable (EelCList *clist, 
			   gboolean  reorderable)
{
  GtkWidget *widget;

  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if ((EEL_CLIST_REORDERABLE(clist) != 0) == reorderable)
    return;

  widget = GTK_WIDGET (clist);

  if (reorderable)
    {
      EEL_CLIST_SET_FLAG (clist, CLIST_REORDERABLE);
      gtk_drag_dest_set (widget,
			 GTK_DEST_DEFAULT_MOTION | GTK_DEST_DEFAULT_DROP,
			 &clist_target_table, 1, GDK_ACTION_MOVE);
    }
  else
    {
      EEL_CLIST_UNSET_FLAG (clist, CLIST_REORDERABLE);
      gtk_drag_dest_unset (GTK_WIDGET (clist));
    }
}

void
eel_clist_set_use_drag_icons (EelCList *clist,
			      gboolean  use_icons)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));

  if (use_icons != 0)
    EEL_CLIST_SET_FLAG (clist, CLIST_USE_DRAG_ICONS);
  else
    EEL_CLIST_UNSET_FLAG (clist, CLIST_USE_DRAG_ICONS);
}

void
eel_clist_set_button_actions (EelCList *clist,
			      guint     button,
			      guint8    button_actions)
{
  g_return_if_fail (clist != NULL);
  g_return_if_fail (EEL_IS_CLIST (clist));
  
  if (button < MAX_BUTTON)
    {
      if (gdk_pointer_is_grabbed () || GTK_WIDGET_HAS_GRAB (clist))
	{
	  remove_grab (clist);
	  clist->drag_button = 0;
	}

      EEL_CLIST_CLASS_FW (clist)->resync_selection (clist, NULL);

      clist->button_actions[button] = button_actions;
    }
}
