#include <config.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "capplet-widget.h"
#include <libgnomeui/gnome-window-icon.h>
#include <locale.h>
#include "da.h"

#define THEME_SWITCHER_VERSION "0.1"

int
main(int argc, char **argv)
{
  GtkWidget *w;
  gint child_pid;
  char *create_rc_error;

	setlocale(LC_ALL, "");
  bindtextdomain (PACKAGE, GNOMELOCALEDIR);
  textdomain (PACKAGE);

  create_rc_error = set_tmp_rc();

  child_pid = do_demo(argc, argv);
  switch (gnome_capplet_init ("theme-switcher-capplet",
			      THEME_SWITCHER_VERSION, argc, argv, NULL, 0, NULL)) {
  case -1:
    if (gtkrc_tmp)
      unlink (gtkrc_tmp);
    exit (1);
  case 1:
    if (gtkrc_tmp)
      unlink (gtkrc_tmp);
    return 0;
  }
  gnome_window_icon_set_default_from_file (GNOME_ICONDIR"/gnome-ccthemes.png");
  w = make_main();
  gtk_widget_show_all(w);
  send_socket();

  if (create_rc_error)
    show_error (create_rc_error, TRUE);
  else
    gtk_main();
  
  /* Pause here until our child exits and the socket can be safely
   * destroyed
   */
  if (child_pid > 0)
    waitpid(child_pid, NULL, 0);
  
  if (gtkrc_tmp)
    unlink (gtkrc_tmp);
  
  return 0;
}
