#include <config.h>

#include <gtk/gtk.h>
#include <eel/eel-label.h>
#include <eel/eel-viewport.h>
#include <eel/eel-background.h>
#include <eel/eel-gtk-extensions.h>
#include <eel/eel-gdk-extensions.h>

void widget_set_eel_background_color (GtkWidget *widget, const char *color);

static void
delete_event (GtkWidget *widget, gpointer data)
{
	gtk_main_quit ();
}

void
widget_set_eel_background_color (GtkWidget *widget, const char *color)
{
	EelBackground      *background;

	g_return_if_fail (GTK_IS_WIDGET (widget));
	g_return_if_fail (color != NULL);

	background = eel_get_widget_background (widget);

	eel_background_reset (background);
	eel_background_set_color (background, color);
}

static GtkWidget *
summary_view_button_new (char *label_text)
{
	GtkWidget *button;
	GtkWidget *label;

	button = gtk_button_new ();
	gtk_widget_set_usize (button, 80, -1);

	label = gtk_label_new (label_text);
	gtk_widget_show (label);
	gtk_container_add (GTK_CONTAINER (button), label);

	return button;
}

static GtkWidget *
create_eel_label (const char *text,
		       guint drop_shadow_offset,
		       float xalign,
		       float yalign,
		       gint xpadding,
		       gint ypadding,
		       guint32 text_color,
		       guint32 background_color,
		       const char *tile_name,
		       gint num_larger_sizes,
		       gboolean bold)
{
	GtkWidget *label;

	label = eel_label_new_solid (text,
					  drop_shadow_offset,
					  EEL_RGB_COLOR_WHITE,
					  EEL_RGB_COLOR_BLACK,
					  xalign, yalign, xpadding, ypadding,
					  background_color, NULL);

	if (num_larger_sizes < 0) {
		eel_label_make_smaller (EEL_LABEL (label), ABS (num_larger_sizes));
	}
	if (bold) {
		eel_label_make_bold (EEL_LABEL (label));
	}

	return label;
}

static GtkWidget *
summary_view_item_label_new (char *label_text,
			     int relative_font_size,
			     gboolean bold)
{
	GtkWidget *label;

	label = create_eel_label (label_text,
				       0, 0.5, 0.5, 0, 0,
				       EEL_RGB_COLOR_BLACK,
				       EEL_RGB_COLOR_WHITE,
				       NULL,
				       relative_font_size,
				       bold);

	eel_label_set_wrap (EEL_LABEL (label), TRUE);
	eel_label_set_justify (EEL_LABEL (label), GTK_JUSTIFY_LEFT);
	gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
	eel_label_set_adjust_wrap_on_resize (EEL_LABEL (label), TRUE);

	return label;
}

static GtkWidget *
summary_view_item_body_label_new (char *label_text)
{
	return summary_view_item_label_new (label_text,
					    -2,
					    FALSE);
}

static GtkWidget *
summary_view_item_header_label_new (char *label_text)
{
	return summary_view_item_label_new (label_text,
					    0,
					    TRUE);
}	

static GtkWidget *
create_row (char *header_text, char *item_text, gboolean constrain_width, gboolean constrain_height)
{
	GtkWidget *form_vbox;
	GtkWidget *service_name;
	GtkWidget *service_description;
	GtkWidget *services_row;
	GtkWidget *pane;
	GtkWidget *summary_pane;
	GtkWidget *viewport;

	GtkWidget *icon_box;
	GtkWidget *description_vbox;
	GtkWidget *button_vbox;
	GtkWidget *button_hbox;
	GtkWidget *button;

	form_vbox = gtk_vbox_new (FALSE, 0);
	gtk_widget_show (form_vbox);

	pane = gtk_scrolled_window_new (NULL, NULL);
	if (constrain_width) {
		gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (pane), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
	}
	if (constrain_height) {
		gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (pane), GTK_POLICY_AUTOMATIC, GTK_POLICY_NEVER);
	}
	gtk_widget_show (pane);

	viewport = eel_viewport_new (NULL, NULL);
	if (constrain_width) {
		eel_viewport_set_constrain_width (EEL_VIEWPORT (viewport), TRUE);
	}
	if (constrain_height) {
		eel_viewport_set_constrain_height (EEL_VIEWPORT (viewport), TRUE);
	}
	widget_set_eel_background_color (viewport, "rgb:FFFF/FFFF/FFFF");
	gtk_viewport_set_shadow_type (GTK_VIEWPORT (viewport), GTK_SHADOW_NONE);
	gtk_container_add (GTK_CONTAINER (pane), viewport);
	gtk_widget_show (viewport);

	summary_pane = gtk_vbox_new (FALSE, 0);
	gtk_container_add (GTK_CONTAINER (viewport), summary_pane);
	gtk_widget_show (summary_pane);
	
	gtk_box_pack_start (GTK_BOX (form_vbox), pane, TRUE, TRUE, 0);

	services_row = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (summary_pane), services_row, FALSE, FALSE, 0);
	gtk_widget_show (services_row);

	/* Generate first box with service icon */
	icon_box = gtk_vbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (services_row), icon_box, FALSE, FALSE, 2);
	gtk_widget_show (icon_box);

	/* Generate second box with service title and summary */
	description_vbox = gtk_vbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (services_row), description_vbox, TRUE, TRUE, 0);
	gtk_widget_show (description_vbox);

	/* Header */
	service_name = summary_view_item_header_label_new (header_text);
	gtk_box_pack_start (GTK_BOX (description_vbox), service_name, FALSE, FALSE, 2);
	gtk_widget_show (service_name);

	/* Body */
	service_description = summary_view_item_body_label_new (item_text);
	gtk_box_pack_start (GTK_BOX (description_vbox), service_description, FALSE, FALSE, 2);
	gtk_widget_show (service_description);

	/* Add the redirect button to the third box */
	button_vbox = gtk_vbox_new (TRUE, 0);
	gtk_box_pack_end (GTK_BOX (services_row), button_vbox, FALSE, FALSE, 2);
	gtk_widget_show (button_vbox);

	button_hbox = gtk_hbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (button_vbox), button_hbox, FALSE, FALSE, 2);
	gtk_widget_show (button_hbox);

	button = summary_view_button_new ("Go There");
	gtk_widget_show (button);
	gtk_box_pack_end (GTK_BOX (button_hbox), button, FALSE, FALSE, 3);

	return form_vbox;
}

int
main (int argc, char *argv[])
{
	GtkWidget *vbox;
	GtkWidget *eel_window;
	GtkWidget *row_one;
	GtkWidget *row_two;

	gtk_init (&argc, &argv);

	eel_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_policy (GTK_WINDOW (eel_window), TRUE, TRUE, FALSE);

	gtk_signal_connect (GTK_OBJECT (eel_window), "delete_event", delete_event, NULL);

	vbox = gtk_vbox_new (0, 0);
	gtk_container_add (GTK_CONTAINER (eel_window), vbox);
	gtk_widget_show (vbox);

	row_one = create_row ("Eazel Online Storage", "Store files online and access them through Eel or from any browser.", TRUE, FALSE);
	row_two = create_row ("Eazel Monkey Storage", "Store your monkeys in our protective cages where they will be loved and cared for.", FALSE, TRUE);

	gtk_box_pack_start (GTK_BOX (vbox), row_one, TRUE, TRUE, 0);
	gtk_box_pack_start (GTK_BOX (vbox), row_two, TRUE, TRUE, 0);

	gtk_widget_show_all (eel_window);

	gtk_main ();

	return 0;
}
