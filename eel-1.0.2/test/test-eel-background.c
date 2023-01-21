/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

#include <eel/eel-background.h>
#include <libgnomevfs/gnome-vfs-init.h>
#include <libgnomevfs/gnome-vfs-utils.h>
#include <gtk/gtkctree.h>
#include <gtk/gtkmain.h>
#include <gtk/gtksignal.h>
#include <gtk/gtkwidget.h>
#include <gtk/gtkwindow.h>

#define PATTERNS_DIR "/gnome-source/eel/data/patterns"

int
main  (int argc, char *argv[])
{
	GtkWidget *window, *ctree;
	EelBackground *background;
	char *image_uri;
	char *titles[] = { "test 1", "test 2", "test 3"};

	gtk_init (&argc, &argv);

	gnome_vfs_init ();

	window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	ctree = gtk_ctree_new_with_titles (3, 2, titles);
#if 0
	gtk_container_add (GTK_CONTAINER (window), ctree);
#endif
	gtk_signal_connect (GTK_OBJECT (window), "destroy",
			    gtk_main_quit, NULL);

	background = eel_get_widget_background (window);

	eel_background_set_color (background,
				  "red-blue:h");

	image_uri = gnome_vfs_get_uri_from_local_path (PATTERNS_DIR "/50s.png");

#if 1
	eel_background_set_image_uri (background, image_uri);
#endif
	g_free (image_uri);


	gtk_widget_show_all (window);
	gtk_main ();

	return 0;
}
