/*
 * GnomeCalendar widget
 * Copyright (C) 1998 the Free Software Foundation
 *
 * Author: Miguel de Icaza (miguel@kernel.org)
 */

#ifndef GNOME_CALENDAR_APP_H
#define GNOME_CALENDAR_APP_H

#include <gnome.h>

#include "calendar.h"

BEGIN_GNOME_DECLS

#define GNOME_CALENDAR(obj)         GTK_CHECK_CAST(obj, gnome_calendar_get_type(), GnomeCalendar)
#define GNOME_CALENDAR_CLASS(class) GTK_CHECK_CAST_CLASS(class, gnome_calendar_get_type(), GnomeCalendarClass)
#define GNOME_IS_CALENDAR(obj)      GTK_CHECK_TYPE(obj, gnome_calendar_get_type())

typedef struct {
	GnomeApp    gnome_app;
	Calendar    *cal;
	time_t      current_day_display;
	time_t      current_week_display;
	time_t      current_month_display;
	time_t      current_year_display;
	
	GtkWidget   *notebook;
	GtkWidget   *day_view;
	GtkWidget   *week_view;
	GtkWidget   *month_view;
	GtkWidget   *year_view;
	GtkWidget   *year_view_sw;
	void        *event_editor;
} GnomeCalendar;

typedef struct {
	GnomeAppClass parent_class;
} GnomeCalendarClass;

guint      gnome_calendar_get_type         	(void);
GtkWidget *gnome_calendar_new              	(char *title);
int        gnome_calendar_load             	(GnomeCalendar *gcal,
						 char *file);
void       gnome_calendar_add_object       	(GnomeCalendar *gcal,
						 iCalObject *obj);
void       gnome_calendar_remove_object    	(GnomeCalendar *gcal,
						 iCalObject *obj);
void       gnome_calendar_next             	(GnomeCalendar *gcal);
void       gnome_calendar_previous         	(GnomeCalendar *gcal);
void       gnome_calendar_goto             	(GnomeCalendar *gcal,
						 time_t new_time);
void       gnome_calendar_dayjump          	(GnomeCalendar *gcal,
						 time_t time);
/* Jumps to the current day */
void       gnome_calendar_goto_today            (GnomeCalendar *gcal);
void       gnome_calendar_tag_calendar          (GnomeCalendar *cal,
						 GtkCalendar *gtk_cal);
char      *gnome_calendar_get_current_view_name (GnomeCalendar *gcal);
void       gnome_calendar_set_view              (GnomeCalendar *gcal,
						 char *page_name);

/* Flags is a bitmask of CalObjectChange values */
void       gnome_calendar_object_changed        (GnomeCalendar *gcal,
						 iCalObject *obj,
						 int flags);

GnomeCalendar *gnome_calendar_locate            (const char *pathname);

/* Notifies the calendar that the time format has changed and it must update all its views */
void gnome_calendar_time_format_changed (GnomeCalendar *gcal);

/* Notifies the calendar that the todo list properties have changed and its time to update the views */
void 
gnome_calendar_colors_changed (GnomeCalendar *gcal);

/* Notifies the calendar that the todo list properties have changed and its time to update the views */
void 
gnome_calendar_todo_properties_changed (GnomeCalendar *gcal);

/* Return the currently displayed time on the currently displayed page */
time_t
gnome_calendar_current_display (GnomeCalendar *gcal);

END_GNOME_DECLS

#endif
