/* GConf
 * Copyright (C) 1999, 2000 Red Hat Inc.
 * Developed by Havoc Pennington, some code in here borrowed from 
 * gnome-name-server and libgnorba (Elliot Lee)
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
 * This is the per-user configuration daemon.
 * (has debug crap in it now)
 */

#include <config.h>

#include "gconf-internals.h"
#include "gconf-sources.h"
#include "gconf-listeners.h"
#include "gconf-locale.h"
#include "gconf-schema.h"
#include "gconf-glib-private.h"
#include "gconf.h"
#include "gconfd.h"
#include "gconf-database.h"
#include <orb/orbit.h>

#include "GConf.h"

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>
#include <syslog.h>
#include <time.h>

/* This makes hash table safer when debugging */
#ifndef GCONF_ENABLE_DEBUG
#define safe_g_hash_table_insert g_hash_table_insert
#else
static void
safe_g_hash_table_insert(GHashTable* ht, gpointer key, gpointer value)
{
  gpointer oldkey = NULL, oldval = NULL;

  if (g_hash_table_lookup_extended(ht, key, &oldkey, &oldval))
    {
      gconf_log(GCL_WARNING, "Hash key `%s' is already in the table!",
                (gchar*) key);
      return;
    }
  else
    {
      g_hash_table_insert(ht, key, value);
    }
}
#endif

/*
 * Declarations
 */

static void     gconf_main            (void);
static void     gconf_main_quit       (void);
static gboolean gconf_main_is_running (void);

static void logfile_save (void);
static void logfile_read (void);
static void log_client_add (const ConfigListener client);
static void log_client_remove (const ConfigListener client);

static void    add_client            (const ConfigListener  client);
static void    remove_client         (const ConfigListener  client);
static GSList *list_clients          (void);
static void    log_clients_to_string (GString              *str);
static void    drop_old_clients      (void);
static guint   client_count          (void);

static void    enter_shutdown          (void);

static void                 init_databases (void);
static void                 shutdown_databases (void);
static void                 set_default_database (GConfDatabase* db);
static void                 register_database (GConfDatabase* db);
static void                 unregister_database (GConfDatabase* db);
static GConfDatabase*       lookup_database (const gchar *address);
static GConfDatabase*       obtain_database (const gchar *address,
                                             GError **err);
static void                 drop_old_databases (void);
static gboolean             no_databases_in_use (void);

/*
 * Flag indicating that we are shutting down, so return errors
 * on any attempted operation. We do this instead of unregistering with
 * OAF or deactivating the server object, because we want to avoid
 * another gconfd starting up before we finish shutting down.
 */

static gboolean in_shutdown = FALSE;

/* 
 * CORBA goo
 */

static ConfigServer server = CORBA_OBJECT_NIL;
static PortableServer_POA the_poa;
static GConfLock *daemon_lock = NULL;

static ConfigDatabase
gconfd_get_default_database(PortableServer_Servant servant,
                            CORBA_Environment* ev);

static ConfigDatabase
gconfd_get_database(PortableServer_Servant servant,
                    const CORBA_char* address,
                    CORBA_Environment* ev);

static void
gconfd_add_client (PortableServer_Servant servant,
                   const ConfigListener client,
                   CORBA_Environment *ev);

static void
gconfd_remove_client (PortableServer_Servant servant,
                      const ConfigListener client,
                      CORBA_Environment *ev);

static CORBA_long
gconfd_ping(PortableServer_Servant servant, CORBA_Environment *ev);

static void
gconfd_shutdown(PortableServer_Servant servant, CORBA_Environment *ev);

static PortableServer_ServantBase__epv base_epv = {
  NULL,
  NULL,
  NULL
};

static POA_ConfigServer__epv server_epv = { 
  NULL,
  gconfd_get_default_database,
  gconfd_get_database,
  gconfd_add_client,
  gconfd_remove_client,
  gconfd_ping,
  gconfd_shutdown
};

static POA_ConfigServer__vepv poa_server_vepv = { &base_epv, &server_epv };
static POA_ConfigServer poa_server_servant = { NULL, &poa_server_vepv };

static ConfigDatabase
gconfd_get_default_database(PortableServer_Servant servant,
                            CORBA_Environment* ev)
{
  GConfDatabase *db;

  if (gconfd_check_in_shutdown (ev))
    return CORBA_OBJECT_NIL;
  
  db = lookup_database (NULL);

  if (db)
    return CORBA_Object_duplicate (db->objref, ev);
  else
    return CORBA_OBJECT_NIL;
}

static ConfigDatabase
gconfd_get_database(PortableServer_Servant servant,
                    const CORBA_char* address,
                    CORBA_Environment* ev)
{
  GConfDatabase *db;
  GError* error = NULL;  

  if (gconfd_check_in_shutdown (ev))
    return CORBA_OBJECT_NIL;
  
  db = obtain_database (address, &error);

  if (db != NULL)
    return CORBA_Object_duplicate (db->objref, ev);
  else if (gconf_set_exception(&error, ev))
    return CORBA_OBJECT_NIL;
  else
    return CORBA_OBJECT_NIL;
}

static void
gconfd_add_client (PortableServer_Servant servant,
                   const ConfigListener client,
                   CORBA_Environment *ev)
{
  if (gconfd_check_in_shutdown (ev))
    return;
  
  add_client (client);
}

static void
gconfd_remove_client (PortableServer_Servant servant,
                      const ConfigListener client,
                      CORBA_Environment *ev)
{
  if (gconfd_check_in_shutdown (ev))
    return;
  
  remove_client (client);
}

static CORBA_long
gconfd_ping(PortableServer_Servant servant, CORBA_Environment *ev)
{
  if (gconfd_check_in_shutdown (ev))
    return 0;
  
  return getpid();
}

static void
gconfd_shutdown(PortableServer_Servant servant, CORBA_Environment *ev)
{
  if (gconfd_check_in_shutdown (ev))
    return;
  
  gconf_log(GCL_DEBUG, _("Shutdown request received"));

  gconf_main_quit();
}

/*
 * Main code
 */

/* This needs to be called before we register with OAF
 */
static void
gconf_server_load_sources(void)
{
  gchar** addresses;
  GList* tmp;
  gboolean have_writable = FALSE;
  gchar* conffile;
  GConfSources* sources = NULL;
  GError* error = NULL;
  
  conffile = g_strconcat(GCONF_CONFDIR, "/path", NULL);

  addresses = gconf_load_source_path(conffile, NULL);

  g_free(conffile);

#ifdef GCONF_ENABLE_DEBUG
  /* -- Debug only */
  
  if (addresses == NULL)
    {
      gconf_log(GCL_DEBUG, _("gconfd compiled with debugging; trying to load gconf.path from the source directory"));
      conffile = g_strconcat(GCONF_SRCDIR, "/gconf/gconf.path", NULL);
      addresses = gconf_load_source_path(conffile, NULL);
      g_free(conffile);
    }

  /* -- End of Debug Only */
#endif

  if (addresses == NULL)
    {      
      /* Try using the default address xml:readwrite:$(HOME)/.gconf */
      addresses = g_new0(gchar*, 2);

      addresses[0] = g_strconcat("xml:readwrite:", g_get_home_dir(), "/.gconf", NULL);

      addresses[1] = NULL;
      
      gconf_log(GCL_DEBUG, _("No configuration files found, trying to use the default config source `%s'"), addresses[0]);
    }
  
  if (addresses == NULL)
    {
      /* We want to stay alive but do nothing, because otherwise every
         request would result in another failed gconfd being spawned.  
      */
      const gchar* empty_addr[] = { NULL };
      gconf_log(GCL_ERR, _("No configuration sources in the source path, configuration won't be saved; edit "GCONF_CONFDIR"/path"));
      /* don't request error since there aren't any addresses */
      sources = gconf_sources_new_from_addresses(empty_addr, NULL);

      /* Install the sources as the default database */
      set_default_database (gconf_database_new(sources));
    }
  else
    {
      sources = gconf_sources_new_from_addresses((const gchar**)addresses,
                                                 &error);

      if (error != NULL)
        {
          gconf_log(GCL_ERR, _("Error loading some config sources: %s"),
                    error->message);

          g_error_free(error);
          error = NULL;
        }
      
      g_free(addresses);

      g_assert(sources != NULL);

      if (sources->sources == NULL)
        gconf_log(GCL_ERR, _("No config source addresses successfully resolved, can't load or store config data"));
    
      tmp = sources->sources;

      while (tmp != NULL)
        {
          if (((GConfSource*)tmp->data)->flags & GCONF_SOURCE_ALL_WRITEABLE)
            {
              have_writable = TRUE;
              break;
            }

          tmp = g_list_next(tmp);
        }

      /* In this case, some sources may still return TRUE from their writable() function */
      if (!have_writable)
        gconf_log(GCL_WARNING, _("No writable config sources successfully resolved, may not be able to save some configuration changes"));

        
      /* Install the sources as the default database */
      set_default_database (gconf_database_new(sources));
    }
}

static void
signal_handler (int signo)
{
  static gint in_fatal = 0;

  /* avoid loops */
  if (in_fatal > 0)
    return;
  
  ++in_fatal;
  
  switch(signo) {
    /* Fast cleanup only */
  case SIGSEGV:
  case SIGBUS:
  case SIGILL:
    enter_shutdown ();
    gconf_log(GCL_ERR,
              _("Received signal %d, dumping core. Please report a GConf bug."),
              signo);
    abort ();
    break;

  case SIGFPE:
  case SIGPIPE:
    /* Go ahead and try the full cleanup on these,
     * though it could well not work out very well.
     */
    enter_shutdown ();

    /* let the fatal signals interrupt us */
    --in_fatal;
    
    gconf_log (GCL_ERR,
               _("Received signal %d, shutting down abnormally. Please file a GConf bug report."),
               signo);


    if (gconf_main_is_running ())
      gconf_main_quit ();
    
    break;

  case SIGTERM:
  case SIGHUP:
    enter_shutdown ();

    /* let the fatal signals interrupt us */
    --in_fatal;
    
    gconf_log (GCL_INFO,
               _("Received signal %d, shutting down cleanly"), signo);

    if (gconf_main_is_running ())
      gconf_main_quit ();
    break;

  case SIGUSR1:
    /* it'd be nice to log a message here but it's not very safe, so */
    gconf_log_debug_messages = !gconf_log_debug_messages;
    break;
    
  default:
    break;
  }
}

PortableServer_POA
gconf_get_poa ()
{
  return the_poa;
}

static void
log_handler (const gchar   *log_domain,
             GLogLevelFlags log_level,
             const gchar   *message,
             gpointer       user_data)
{
  GConfLogPriority pri = GCL_WARNING;
  
  switch (log_level)
    {
    case G_LOG_LEVEL_ERROR:
    case G_LOG_LEVEL_CRITICAL:
      pri = GCL_ERR;
      break;

    case G_LOG_LEVEL_WARNING:
      pri = GCL_WARNING;
      break;

    case G_LOG_LEVEL_MESSAGE:
    case G_LOG_LEVEL_INFO:
      pri = GCL_INFO;
      break;

    case G_LOG_LEVEL_DEBUG:
      pri = GCL_DEBUG;
      break;

    default:
      break;
    }

  gconf_log (pri, "%s", message);
}

int 
main(int argc, char** argv)
{
  struct sigaction act;
  sigset_t empty_mask;
  /*   PortableServer_ObjectId objid = {0, sizeof("ConfigServer"), "ConfigServer"}; */
  PortableServer_ObjectId* objid;
  CORBA_Environment ev;
  CORBA_ORB orb;
  gchar* logname;
  const gchar* username;
  guint len;
  gchar* ior;
  int exit_code = 0;
  GError *err;
  char *lock_dir;
  char *gconfd_dir;
  int dev_null_fd;
  int write_byte_fd;
  
  /* Now this is an argument parser */
  if (argc > 1)
    write_byte_fd = atoi (argv[1]);
  else
    write_byte_fd = -1;
  
  chdir ("/");

  /* This is so we don't prevent unmounting of devices. We divert
   * all messages to syslog
   */

  if (!g_getenv ("GCONF_DEBUG_OUTPUT"))
    {
      dev_null_fd = open ("/dev/null", O_RDWR);
      if (dev_null_fd >= 0)
        {
	  dup2 (dev_null_fd, 0);
	  dup2 (dev_null_fd, 1);
	  dup2 (dev_null_fd, 2);
	}
    }
  
  umask (022);
  
  gconf_set_daemon_mode(TRUE);
  
  /* Logs */
  username = g_get_user_name();
  len = strlen(username) + strlen("gconfd") + 15;
  logname = g_malloc(len);
  g_snprintf(logname, len, "gconfd (%s-%u)", username, (guint)getpid());

  openlog (logname, LOG_NDELAY, LOG_USER);

  g_log_set_handler (NULL, G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION,
                     log_handler, NULL);
  
  /* openlog() does not copy logname - what total brokenness.
     So we free it at the end of main() */
  
  gconf_log (GCL_INFO, _("starting (version %s), pid %u user '%s'"), 
             VERSION, (guint)getpid(), g_get_user_name());

#ifdef GCONF_ENABLE_DEBUG
  gconf_log (GCL_DEBUG, "GConf was built with debugging features enabled");
#endif
  
  /* Session setup */
  sigemptyset (&empty_mask);
  act.sa_handler = signal_handler;
  act.sa_mask    = empty_mask;
  act.sa_flags   = 0;
  sigaction (SIGTERM,  &act, 0);
  sigaction (SIGILL,  &act, 0);
  sigaction (SIGBUS,  &act, 0);
  sigaction (SIGFPE,  &act, 0);
  sigaction (SIGHUP,  &act, 0);
  sigaction (SIGSEGV, &act, 0);
  sigaction (SIGABRT, &act, 0);

  act.sa_handler = SIG_IGN;
  sigaction (SIGINT, &act, 0);

  CORBA_exception_init(&ev);

  init_databases ();

  orb = gconf_orb_get();
  
  POA_ConfigServer__init(&poa_server_servant, &ev);
  
  the_poa = (PortableServer_POA)CORBA_ORB_resolve_initial_references(orb, "RootPOA", &ev);
  PortableServer_POAManager_activate(PortableServer_POA__get_the_POAManager(the_poa, &ev), &ev);

  objid = PortableServer_POA_activate_object(the_poa, &poa_server_servant, &ev);
  
  server = PortableServer_POA_servant_to_reference(the_poa,
                                                   &poa_server_servant,
                                                   &ev);
  if (CORBA_Object_is_nil(server, &ev)) 
    {
      gconf_log(GCL_ERR, _("Failed to get object reference for ConfigServer"));
      return 1;
    }

  /* Needs to be done before loading sources */
  ior = CORBA_ORB_object_to_string (orb, server, &ev);
  gconf_set_daemon_ior (ior);
  CORBA_free (ior);

  gconfd_dir = gconf_get_daemon_dir ();
  lock_dir = gconf_get_lock_dir ();

  if (mkdir (gconfd_dir, 0700) < 0 && errno != EEXIST)
    gconf_log (GCL_WARNING, _("Failed to create %s: %s"),
               gconfd_dir, g_strerror (errno));
  
  
  err = NULL;
  daemon_lock = gconf_get_lock (lock_dir, &err);

  if (daemon_lock != NULL)
    {
      /* This loads backends and so on. It needs to be done before
       * we can handle any requests, so before we hit the
       * main loop. if daemon_lock == NULL we won't hit the
       * main loop.
       */
      gconf_server_load_sources ();
    }
  
  /* notify caller that we're done either getting the lock
   * or not getting it
   */
  if (write_byte_fd >= 0)
    {
      char buf[1] = { 'g' };
      if (write (write_byte_fd, buf, 1) != 1)
        {
          gconf_log (GCL_ERR, _("Failed to write byte to pipe fd %d so client program may hang: %s"), write_byte_fd, g_strerror (errno));
        }

      close (write_byte_fd);
    }
  
  if (daemon_lock == NULL)
    {
      g_assert (err);

      gconf_log (GCL_WARNING, _("Failed to get lock for daemon, exiting: %s"),
                 err->message);
      g_error_free (err);

      enter_shutdown ();
      shutdown_databases ();
      
      return 1;
    }

  /* Read saved log file, if any */
  logfile_read ();
  
  gconf_main ();

  if (in_shutdown)
    exit_code = 1; /* means someone already called enter_shutdown() */
  
  /* This starts bouncing all incoming requests (and we aren't running
   * the main loop anyway, so they won't get processed)
   */
  enter_shutdown ();

  /* Save current state in logfile (may compress the logfile a good
   * bit)
   */
  logfile_save ();
  
  shutdown_databases ();

  gconfd_locale_cache_drop ();

  /* Now we can release the lock */

  server = CORBA_OBJECT_NIL;

  if (daemon_lock)
    {
      err = NULL;
      gconf_release_lock (daemon_lock, &err);
      if (err != NULL)
        {
          gconf_log (GCL_WARNING, _("Error releasing lockfile: %s"),
                     err->message);
          g_error_free (err);
        }
    }

  daemon_lock = NULL;
  
  gconf_log (GCL_INFO, _("Exiting"));
  
  g_free (logname);
  
  return exit_code;
}

/*
 * Main loop
 */

static GSList* main_loops = NULL;
static guint timeout_id = 0;
static gboolean need_log_cleanup = FALSE;

static gboolean
periodic_cleanup_timeout(gpointer data)
{  
  gconf_log (GCL_DEBUG, "Performing periodic cleanup, expiring cache cruft");
  
  drop_old_clients ();
  drop_old_databases ();

  if (no_databases_in_use () && client_count () == 0)
    {
      gconf_log (GCL_INFO, _("GConf server is not in use, shutting down."));
      gconf_main_quit ();
      return FALSE;
    }
  
  /* expire old locale cache entries */
  gconfd_locale_cache_expire ();

  if (!need_log_cleanup)
    {
      gconf_log (GCL_DEBUG, "No log file saving needed in periodic cleanup handler");
      return TRUE;
    }
  
  /* Compress the running state file */
  logfile_save ();

  need_log_cleanup = FALSE;
  
  return TRUE;
}

void
gconfd_need_log_cleanup (void)
{
  need_log_cleanup = TRUE;
}

static void
gconf_main(void)
{
  GMainLoop* loop;

  loop = g_main_new(TRUE);

  if (main_loops == NULL)
    {
#ifdef GCONF_ENABLE_DEBUG
      gulong timeout_len = 1000*60*1; /* 1 sec * 60 s/min * 1 min */
#else
      gulong timeout_len = 1000*60*15; /* 1 sec * 60 s/min * 2 min */
#endif
      
      g_assert(timeout_id == 0);
      timeout_id = g_timeout_add (timeout_len,
                                  periodic_cleanup_timeout,
                                  NULL);

    }
  
  main_loops = g_slist_prepend(main_loops, loop);

  g_main_run(loop);

  main_loops = g_slist_remove(main_loops, loop);

  if (main_loops == NULL)
    {
      g_assert(timeout_id != 0);
      g_source_remove(timeout_id);
      timeout_id = 0;
    }
  
  g_main_destroy(loop);
}

static void 
gconf_main_quit(void)
{
  g_return_if_fail(main_loops != NULL);

  g_main_quit(main_loops->data);
}

static gboolean
gconf_main_is_running (void)
{
  return main_loops != NULL;
}

/*
 * Database storage
 */

static GList* db_list = NULL;
static GHashTable* dbs_by_address = NULL;
static GConfDatabase *default_db = NULL;

static void
init_databases (void)
{
  gconfd_need_log_cleanup ();
  
  g_assert(db_list == NULL);
  g_assert(dbs_by_address == NULL);
  
  dbs_by_address = g_hash_table_new (g_str_hash, g_str_equal);

  /* Default database isn't in the address hash since it has
     multiple addresses in a stack
  */
}

static void
set_default_database (GConfDatabase* db)
{
  gconfd_need_log_cleanup ();
  
  default_db = db;
  
  /* Default database isn't in the address hash since it has
     multiple addresses in a stack
  */
}

static void
register_database (GConfDatabase *db)
{
  gconfd_need_log_cleanup ();
  
  if (db->sources->sources)
    safe_g_hash_table_insert(dbs_by_address,
                             ((GConfSource*)db->sources->sources->data)->address,
                             db);
  
  db_list = g_list_prepend (db_list, db);
}

static void
unregister_database (GConfDatabase *db)
{
  gconfd_need_log_cleanup ();
  
  if (db->sources->sources)
    g_hash_table_remove(dbs_by_address,
                        ((GConfSource*)(db->sources->sources->data))->address);

  db_list = g_list_remove (db_list, db);

  gconf_database_free (db);
}

static GConfDatabase*
lookup_database (const gchar *address)
{
  if (address == NULL)
    return default_db;
  else
    return g_hash_table_lookup (dbs_by_address, address);
}

static GConfDatabase*
obtain_database (const gchar *address,
                 GError **err)
{
  
  GConfSources* sources;
  const gchar* addresses[] = { NULL, NULL };
  GError* error = NULL;
  GConfDatabase *db;

  addresses[0] = address;
  db = lookup_database (address);

  if (db)
    return db;
  
  sources = gconf_sources_new_from_addresses(addresses, &error);

  if (error != NULL)
    {
      if (err)
        *err = error;
      else
        g_error_free (error);

      return NULL;
    }
  
  if (sources == NULL)
    return NULL;

  db = gconf_database_new (sources);

  register_database (db);

  return db;
}

static void
drop_old_databases(void)
{
  GList *tmp_list;
  GList *dead = NULL;
  GTime now;
  
  now = time(NULL);

  gconf_database_drop_dead_listeners (default_db);
  
  tmp_list = db_list;
  while (tmp_list)
    {
      GConfDatabase* db = tmp_list->data;

      /* Drop any listeners whose clients are gone. */
      gconf_database_drop_dead_listeners (db);
      
      if (db->listeners &&                             /* not already hibernating */
          gconf_listeners_count(db->listeners) == 0 && /* Can hibernate */
          (now - db->last_access) > (60*20))           /* 20 minutes without access */
        {
          dead = g_list_prepend (dead, db);
        }
      
      tmp_list = g_list_next (tmp_list);
    }

  tmp_list = dead;
  while (tmp_list)
    {
      GConfDatabase* db = tmp_list->data;

      unregister_database (db);
            
      tmp_list = g_list_next (tmp_list);
    }

  g_list_free (dead);
}

static void
shutdown_databases (void)
{
  GList *tmp_list;  

  /* This may be called before we init fully,
   * so check that everything != NULL
   */
  
  tmp_list = db_list;

  while (tmp_list)
    {
      GConfDatabase *db = tmp_list->data;

      gconf_database_free (db);
      
      tmp_list = g_list_next (tmp_list);
    }

  g_list_free (db_list);
  db_list = NULL;

  if (dbs_by_address)
    g_hash_table_destroy(dbs_by_address);

  dbs_by_address = NULL;

  if (default_db)
    gconf_database_free (default_db);

  default_db = NULL;
}

static gboolean
no_databases_in_use (void)
{
  /* Only the default database still open, and
   * it has no listeners
   */
  return db_list == NULL &&
    gconf_listeners_count (default_db->listeners) == 0;
}

/*
 * Cleanup
 */

static void 
enter_shutdown(void)
{
  in_shutdown = TRUE;
}


/* Exceptions */

gboolean
gconf_set_exception(GError** error,
                    CORBA_Environment* ev)
{
  GConfError en;

  if (error == NULL)
    return FALSE;

  if (*error == NULL)
    return FALSE;
  
  en = (*error)->code;

  /* success is not supposed to get set */
  g_return_val_if_fail(en != GCONF_ERROR_SUCCESS, FALSE);
  
  {
    ConfigException* ce;

    ce = ConfigException__alloc();
    g_assert(error != NULL);
    g_assert(*error != NULL);
    g_assert((*error)->message != NULL);
    ce->message = CORBA_string_dup((gchar*)(*error)->message); /* cast const */
      
    switch (en)
      {
      case GCONF_ERROR_FAILED:
        ce->err_no = ConfigFailed;
        break;
      case GCONF_ERROR_NO_PERMISSION:
        ce->err_no = ConfigNoPermission;
        break;
      case GCONF_ERROR_BAD_ADDRESS:
        ce->err_no = ConfigBadAddress;
        break;
      case GCONF_ERROR_BAD_KEY:
        ce->err_no = ConfigBadKey;
        break;
      case GCONF_ERROR_PARSE_ERROR:
        ce->err_no = ConfigParseError;
        break;
      case GCONF_ERROR_CORRUPT:
        ce->err_no = ConfigCorrupt;
        break;
      case GCONF_ERROR_TYPE_MISMATCH:
        ce->err_no = ConfigTypeMismatch;
        break;
      case GCONF_ERROR_IS_DIR:
        ce->err_no = ConfigIsDir;
        break;
      case GCONF_ERROR_IS_KEY:
        ce->err_no = ConfigIsKey;
        break;
      case GCONF_ERROR_NO_WRITABLE_DATABASE:
        ce->err_no = ConfigNoWritableDatabase;        
        break;
      case GCONF_ERROR_IN_SHUTDOWN:
        ce->err_no = ConfigInShutdown;
        break;
        
      case GCONF_ERROR_NO_SERVER:
      case GCONF_ERROR_SUCCESS:
      default:
        g_assert_not_reached();
      }

    CORBA_exception_set(ev, CORBA_USER_EXCEPTION,
                        ex_ConfigException, ce);

    gconf_log(GCL_ERR, _("Returning exception: %s"), (*error)->message);
      
    g_error_free(*error);
    *error = NULL;
      
    return TRUE;
  }
}

gboolean
gconfd_check_in_shutdown (CORBA_Environment *ev)
{
  if (in_shutdown)
    {
      ConfigException* ce;
      
      ce = ConfigException__alloc();
      ce->message = CORBA_string_dup("config server is currently shutting down");
      ce->err_no = ConfigInShutdown;

      CORBA_exception_set(ev, CORBA_USER_EXCEPTION,
                          ex_ConfigException, ce);

      return TRUE;
    }
  else
    return FALSE;
}

/*
 * Logging
 */

/*
   The log file records the current listeners we have registered,
   so we can restore them if we exit and restart.

   Basically:

   1) On startup, we parse any logfile and try to restore the
      listeners contained therein. As we restore each listener (give
      clients a new listener ID) we append a removal of the previous
      daemon's listener and the addition of our own listener to the
      logfile; this means that if we crash and have to restore a
      client's listener a second time, we'll have the client's current
      listener ID. If all goes well we then atomically rewrite the
      parsed logfile with the resulting current state, to keep the logfile
      compact.

   2) While running, we keep a FILE* open and whenever we add/remove
      a listener we write a line to the logfile recording it,
      to keep the logfile always up-to-date.

   3) On normal exit, and also periodically (every hour or so, say) we
      atomically write over the running log with our complete current
      state, to keep the running log from growing without bound.
*/

static void
get_log_names (gchar **logdir, gchar **logfile)
{
  *logdir = gconf_concat_dir_and_key (g_get_home_dir (), ".gconfd");
  *logfile = gconf_concat_dir_and_key (*logdir, "saved_state");
}

static void close_append_handle (void);

static FILE* append_handle = NULL;
static guint append_handle_timeout = 0;

static gboolean
close_append_handle_timeout(gpointer data)
{
  close_append_handle ();

  /* uninstall the timeout */
  append_handle_timeout = 0;
  return FALSE;
}

static gboolean
open_append_handle (GError **err)
{
  if (append_handle == NULL)
    {
      gchar *logdir;
      gchar *logfile;

      get_log_names (&logdir, &logfile);
      
      mkdir (logdir, 0700); /* ignore failure, we'll catch failures
                             * that matter on open()
                             */
      
      append_handle = fopen (logfile, "a");

      if (append_handle == NULL)
        {
          gconf_set_error (err,
                           GCONF_ERROR_FAILED,
                           _("Failed to open gconfd logfile; won't be able to restore listeners after gconfd shutdown (%s)"),
                           strerror (errno));
          
          g_free (logdir);
          g_free (logfile);

          return FALSE;
        }
      
      g_free (logdir);
      g_free (logfile);


      {
        const gulong timeout_len = 1000*60*0.5; /* 1 sec * 60 s/min * 0.5 min */

        if (append_handle_timeout != 0)
          g_source_remove (append_handle_timeout);
        
        append_handle_timeout = g_timeout_add (timeout_len,
                                               close_append_handle_timeout,
                                               NULL);
      }
    }

  return TRUE;
}

static void
close_append_handle (void)
{
  if (append_handle)
    {
      if (fclose (append_handle) < 0)
        gconf_log (GCL_WARNING,
                   _("Failed to close gconfd logfile; data may not have been properly saved (%s)"),
                   strerror (errno));

      append_handle = NULL;

      if (append_handle_timeout != 0)
        {
          g_source_remove (append_handle_timeout);
          append_handle_timeout = 0;
        }
    }
}

/* Atomically save our current state, if possible; otherwise
 * leave the running log in place.
 */
static void
logfile_save (void)
{
  GList *tmp_list;
  gchar *logdir = NULL;
  gchar *logfile = NULL;
  gchar *tmpfile = NULL;
  gchar *tmpfile2 = NULL;
  GString *saveme = NULL;
  gint fd = -1;
  
  /* Close the running log */
  close_append_handle ();
  
  get_log_names (&logdir, &logfile);

  mkdir (logdir, 0700); /* ignore failure, we'll catch failures
                         * that matter on open()
                         */

  saveme = g_string_new ("");

  /* Clients */
  log_clients_to_string (saveme);
  
  /* Default database */
  gconf_database_log_listeners_to_string (default_db,
                                          TRUE,
                                          saveme);

  /* Other databases */
  
  tmp_list = db_list;

  while (tmp_list)
    {
      GConfDatabase *db = tmp_list->data;

      gconf_database_log_listeners_to_string (db,
                                              FALSE,
                                              saveme);
      
      tmp_list = g_list_next (tmp_list);
    }
  
  /* Now try saving the string to a temporary file */
  tmpfile = g_strconcat (logfile, ".tmp", NULL);
  
  fd = open (tmpfile, O_WRONLY | O_CREAT | O_TRUNC, 0700);

  if (fd < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Could not open saved state file '%s' for writing: %s"),
                 tmpfile, strerror (errno));
      
      goto out;
    }

 again:
  
  if (write (fd, saveme->str, saveme->len) < 0)
    {
      if (errno == EINTR)
        goto again;
      
      gconf_log (GCL_WARNING,
                 _("Could not write saved state file '%s' fd: %d: %s"),
                 tmpfile, fd, strerror (errno));

      goto out;
    }

  if (close (fd) < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Failed to close new saved state file '%s': %s"),
                 tmpfile, strerror (errno));
      goto out;
    }

  fd = -1;
  
  /* Move the main saved state file aside, if it exists */
  if (gconf_file_exists (logfile))
    {
      tmpfile2 = g_strconcat (logfile, ".orig", NULL);
      if (rename (logfile, tmpfile2) < 0)
        {
          gconf_log (GCL_WARNING,
                     _("Could not move aside old saved state file '%s': %s"),
                     logfile, strerror (errno));
          goto out;
        }
    }

  /* Move the new saved state file into place */
  if (rename (tmpfile, logfile) < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Failed to move new save state file into place: %s"),
                 strerror (errno));

      /* Try to restore old file */
      if (tmpfile2)
        {
          if (rename (tmpfile2, logfile) < 0)
            {
              gconf_log (GCL_WARNING,
                         _("Failed to restore original saved state file that had been moved to '%s': %s"),
                         tmpfile2, strerror (errno));

            }
        }
      
      goto out;
    }

  /* Get rid of original saved state file if everything succeeded */
  if (tmpfile2)
    unlink (tmpfile2);
  
 out:
  if (saveme)
    g_string_free (saveme, TRUE);
  g_free (logdir);
  g_free (logfile);
  g_free (tmpfile);
  g_free (tmpfile2);

  if (fd >= 0)
    close (fd);
}

typedef struct _ListenerLogEntry ListenerLogEntry;

struct _ListenerLogEntry
{
  guint connection_id;
  gchar *ior;
  gchar *address;
  gchar *location;
};

static guint
listener_logentry_hash (gconstpointer v)
{
  const ListenerLogEntry *lle = v;

  return
    (lle->connection_id         & 0xff000000) |
    (g_str_hash (lle->ior)      & 0x00ff0000) |
    (g_str_hash (lle->address)  & 0x0000ff00) |
    (g_str_hash (lle->location) & 0x000000ff);
}

static gboolean
listener_logentry_equal (gconstpointer ap, gconstpointer bp)
{
  const ListenerLogEntry *a = ap;
  const ListenerLogEntry *b = bp;

  return
    a->connection_id == b->connection_id &&
    strcmp (a->location, b->location) == 0 &&
    strcmp (a->ior, b->ior) == 0 &&
    strcmp (a->address, b->address) == 0;
}

/* Return value indicates whether we "handled" this line of text */
static gboolean
parse_listener_entry (GHashTable *entries,
                      gchar      *text)
{
  gboolean add;
  gchar *p;
  gchar *ior;
  gchar *address;
  gchar *location;
  gchar *end;
  guint connection_id;
  GError *err;
  ListenerLogEntry *lle;
  ListenerLogEntry *old;
  
  if (strncmp (text, "ADD", 3) == 0)
    {
      add = TRUE;
      p = text + 3;
    }
  else if (strncmp (text, "REMOVE", 6) == 0)
    {
      add = FALSE;
      p = text + 6;
    }
  else
    {
      return FALSE;
    }
  
  while (*p && isspace (*p))
    ++p;

  errno = 0;
  end = NULL;
  connection_id = strtoul (p, &end, 10);
  if (end == p || errno != 0)
    {
      gconf_log (GCL_DEBUG,
                 "Failed to parse connection ID in saved state file");
      
      return TRUE;
    }

  if (connection_id == 0)
    {
      gconf_log (GCL_DEBUG,
                 "Connection ID 0 in saved state file is not valid");
      return TRUE;
    }
  
  p = end;

  while (*p && isspace (*p))
    ++p;

  err = NULL;
  end = NULL;
  gconf_unquote_string_inplace (p, &end, &err);
  if (err != NULL)
    {
      gconf_log (GCL_DEBUG,
                 "Failed to unquote config source address from saved state file: %s",
                 err->message);

      g_error_free (err);
      
      return TRUE;
    }

  address = p;
  p = end;

  while (*p && isspace (*p))
    ++p;
  
  err = NULL;
  end = NULL;
  gconf_unquote_string_inplace (p, &end, &err);
  if (err != NULL)
    {
      gconf_log (GCL_DEBUG,
                 "Failed to unquote listener location from saved state file: %s",
                 err->message);

      g_error_free (err);
      
      return TRUE;
    }

  location = p;
  p = end;

  while (*p && isspace (*p))
    ++p;
  
  err = NULL;
  end = NULL;
  gconf_unquote_string_inplace (p, &end, &err);
  if (err != NULL)
    {
      gconf_log (GCL_DEBUG,
                 "Failed to unquote IOR from saved state file: %s",
                 err->message);
      
      g_error_free (err);
      
      return TRUE;
    }
  
  ior = p;
  p = end;    

  lle = g_new (ListenerLogEntry, 1);
  lle->connection_id = connection_id;
  lle->address = address;
  lle->ior = ior;
  lle->location = location;

  if (*(lle->address) == '\0' ||
      *(lle->ior) == '\0' ||
      *(lle->location) == '\0')
    {
      gconf_log (GCL_DEBUG,
                 "Saved state file listener entry didn't contain all the fields; ignoring.");

      g_free (lle);

      return TRUE;
    }
  
  old = g_hash_table_lookup (entries, lle);

  if (old)
    {
      if (add)
        {
          gconf_log (GCL_DEBUG,
                     "Saved state file records the same listener added twice; ignoring the second instance");
          goto quit;
        }
      else
        {
          /* This entry was added, then removed. */
          g_hash_table_remove (entries, lle);
          goto quit;
        }
    }
  else
    {
      if (add)
        {
          g_hash_table_insert (entries, lle, lle);
          
          return TRUE;
        }
      else
        {
          gconf_log (GCL_DEBUG,
                     "Saved state file had a removal of a listener that wasn't added; ignoring the removal.");
          goto quit;
        }
    }
  
 quit:
  g_free (lle);

  return TRUE;
}                

/* Return value indicates whether we "handled" this line of text */
static gboolean
parse_client_entry (GHashTable *clients,
                    gchar      *text)
{
  gboolean add;
  gchar *ior;
  GError *err;
  gchar *old;
  gchar *p;
  gchar *end;
  
  if (strncmp (text, "CLIENTADD", 9) == 0)
    {
      add = TRUE;
      p = text + 9;
    }
  else if (strncmp (text, "CLIENTREMOVE", 12) == 0)
    {
      add = FALSE;
      p = text + 12;
    }
  else
    {
      return FALSE;
    }
  
  while (*p && isspace (*p))
    ++p;
  
  err = NULL;
  end = NULL;
  gconf_unquote_string_inplace (p, &end, &err);
  if (err != NULL)
    {
      gconf_log (GCL_DEBUG,
                 "Failed to unquote IOR from saved state file: %s",
                 err->message);
      
      g_error_free (err);
      
      return TRUE;
    }
  
  ior = p;
  p = end;    
  
  old = g_hash_table_lookup (clients, ior);

  if (old)
    {
      if (add)
        {
          gconf_log (GCL_DEBUG,
                     "Saved state file records the same client added twice; ignoring the second instance");
          goto quit;
        }
      else
        {
          /* This entry was added, then removed. */
          g_hash_table_remove (clients, ior);
          goto quit;
        }
    }
  else
    {
      if (add)
        {
          g_hash_table_insert (clients, ior, ior);
          
          return TRUE;
        }
      else
        {
          gconf_log (GCL_DEBUG,
                     "Saved state file had a removal of a client that wasn't added; ignoring the removal.");
          goto quit;
        }
    }
  
 quit:

  return TRUE;
}

static void
restore_client (const gchar *ior)
{
  ConfigListener cl;
  CORBA_Environment ev;
  
  CORBA_exception_init (&ev);
  
  cl = CORBA_ORB_string_to_object (gconf_orb_get (),
                                   (gchar*)ior,
                                   &ev);

  CORBA_exception_free (&ev);
  
  if (CORBA_Object_is_nil (cl, &ev))
    {
      CORBA_exception_free (&ev);

      gconf_log (GCL_DEBUG,
                 "Client in saved state file no longer exists, not restoring it as a client");
      
      return;
    }

  ConfigListener_drop_all_caches (cl, &ev);
  
  if (ev._major != CORBA_NO_EXCEPTION)
    {
      gconf_log (GCL_DEBUG, "Failed to update client in saved state file, probably the client no longer exists");

      goto finished;
    }

  /* Add the client, since it still exists. Note that the client still
   * has the wrong server object reference, so next time it tries to
   * contact the server it will re-add itself; we just live with that,
   * it's not a problem.
   */
  add_client (cl);
  
 finished:
  CORBA_Object_release (cl, &ev);

  CORBA_exception_free (&ev);
}

static void
restore_listener (GConfDatabase* db,
                  ListenerLogEntry *lle)
{
  ConfigListener cl;
  CORBA_Environment ev;
  guint new_cnxn;
  GError *err;
  
  CORBA_exception_init (&ev);
  
  cl = CORBA_ORB_string_to_object (gconf_orb_get (),
                                   lle->ior,
                                   &ev);

  CORBA_exception_free (&ev);
  
  if (CORBA_Object_is_nil (cl, &ev))
    {
      CORBA_exception_free (&ev);

      gconf_log (GCL_DEBUG,
                 "Client in saved state file no longer exists, not updating its listener connections");
      
      return;
    }

  /* "Cancel" the addition of the listener in the saved state file,
   * so that if we reload the saved state file a second time
   * for some reason, we don't try to add this listener that time.
   */

  err = NULL;  
  if (!gconfd_logfile_change_listener (db,
                                       FALSE, /* remove */
                                       lle->connection_id,
                                       cl,
                                       lle->location,
                                       &err))
    {
      gconf_log (GCL_DEBUG,
                 "Failed to cancel previous daemon's listener in saved state file: %s",
                 err->message);
      g_error_free (err);
    }  

  new_cnxn = gconf_database_readd_listener (db, cl, lle->location);

  gconf_log (GCL_DEBUG, "Attempting to update listener from saved state file, old connection %u, new connectin %u", (guint) lle->connection_id, (guint) new_cnxn);
  
  ConfigListener_update_listener (cl,
                                  db->objref,
                                  lle->address,
                                  lle->connection_id,
                                  lle->location,
                                  new_cnxn,
                                  &ev);
  
  if (ev._major != CORBA_NO_EXCEPTION)
    {
      gconf_log (GCL_DEBUG, "Failed to update listener in saved state file, probably the client no longer exists");

      /* listener will get removed next time we try to notify -
       * we already appended a cancel of the listener to the
       * saved state file.
       */
      goto finished;
    }

  /* Successfully notified client of new connection ID, so put that
   * connection ID in the saved state file.
   */
  err = NULL;  
  if (!gconfd_logfile_change_listener (db,
                                       TRUE, /* add */
                                       new_cnxn,
                                       cl,
                                       lle->location,
                                       &err))
    {
      gconf_log (GCL_DEBUG,
                 "Failed to re-add this daemon's listener ID in saved state file: %s",
                 err->message);
      g_error_free (err);
    }

  /* We updated the listener, and logged that to the saved state
   * file. Yay!
   */
  
 finished:
  
  CORBA_Object_release (cl, &ev);

  CORBA_exception_free (&ev);
}

static void
listener_logentry_restore_and_destroy_foreach (gpointer key,
                                               gpointer value,
                                               gpointer data)
{
  ListenerLogEntry *lle = key;
  GConfDatabase *db;
  
  if (strcmp (lle->address, "def") == 0)
    db = default_db;
  else
    db = obtain_database (lle->address, NULL);
  
  if (db == NULL)
    {
      gconf_log (GCL_WARNING,
                 _("Unable to restore a listener on address '%s', couldn't resolve the database"),
                 lle->address);
      return;
    }

  restore_listener (db, lle);

  /* We don't need it anymore */
  g_free (lle);
}

static void
restore_client_foreach (gpointer key,
                        gpointer value,
                        gpointer data)
{
  restore_client (key);
}


#ifndef HAVE_FLOCKFILE
#  define flockfile(f) (void)1
#  define funlockfile(f) (void)1
#  define getc_unlocked(f) getc(f)
#endif /* !HAVE_FLOCKFILE */

static gchar*
read_line (FILE *f)
{
  int c;
  GString *str;
  
  str = g_string_new ("");
  
  flockfile (f);

  while (TRUE)
    {
      c = getc_unlocked (f);

      switch (c)
        {
        case EOF:
          if (ferror (f))
            {
              gconf_log (GCL_ERR,
                         _("Error reading saved state file: %s"),
                         g_strerror (errno));
            }

          /* FALL THRU */
        case '\n':
          goto done;
          break;

        default:
          g_string_append_c (str, c);
          break;
        }
    }

 done:
  funlockfile (f);

  if (str->len == 0)
    {
      g_string_free (str, TRUE);
      return NULL;
    }
  else
    {
      gchar *ret;
      
      ret = str->str;
      g_string_free (str, FALSE);
      return ret;
    }
}

static void
logfile_read (void)
{
  gchar *logfile;
  gchar *logdir;
  GHashTable *entries;
  GHashTable *clients;
  FILE *f;
  gchar *line;
  GSList *lines = NULL;
  
  /* Just for good form */
  close_append_handle ();
  
  get_log_names (&logdir, &logfile);

  f = fopen (logfile, "r");
  
  if (f == NULL)
    {
      gconf_log (GCL_ERR, _("Unable to open saved state file '%s': %s"),
                 logfile, g_strerror (errno));

      goto finished;
    }

  entries = g_hash_table_new (listener_logentry_hash, listener_logentry_equal);
  clients = g_hash_table_new (g_str_hash, g_str_equal);

  line = read_line (f);
  while (line)
    {
      if (!parse_listener_entry (entries, line))
        {
          if (!parse_client_entry (clients, line))
            {
              gconf_log (GCL_DEBUG,
                         "Didn't understand line in saved state file: '%s'", 
                         line);
              g_free (line);
              line = NULL;
            }
        }

      if (line)
        lines = g_slist_prepend (lines, line);
      
      line = read_line (f);
    }
  
  /* Restore clients first */
  g_hash_table_foreach (clients,
                        restore_client_foreach,
                        NULL);
  
  /* Entries that still remain in the listener hash table were added
   * but not removed, so add them in this daemon instantiation and
   * update their listeners with the new connection ID etc.
   */
  g_hash_table_foreach (entries, 
                        listener_logentry_restore_and_destroy_foreach,
                        NULL);

  g_hash_table_destroy (entries);
  g_hash_table_destroy (clients);

  /* Note that we need the strings to remain valid until we are totally
   * finished, because we store pointers to them in the log entry
   * hash.
   */
  g_slist_foreach (lines, (GFunc)g_free, NULL);
  g_slist_free (lines);
  
 finished:
  
  g_free (logfile);
  g_free (logdir);
}

gboolean
gconfd_logfile_change_listener (GConfDatabase *db,
                                gboolean add,
                                guint connection_id,
                                ConfigListener listener,
                                const gchar *where,
                                GError **err)
{
  gchar *ior = NULL;
  gchar *quoted_db_name;
  gchar *quoted_where;
  gchar *quoted_ior;
  
  if (!open_append_handle (err))
    return FALSE;
  
  ior = gconf_object_to_string (listener, err);
  
  if (ior == NULL)
    return FALSE;

  quoted_ior = gconf_quote_string (ior);
  g_free (ior);
  ior = NULL;
  
  if (db == default_db)
    quoted_db_name = gconf_quote_string ("def");
  else
    {
      const gchar *db_name;
      
      db_name = gconf_database_get_persistent_name (db);
      
      quoted_db_name = gconf_quote_string (db_name);
    }

  quoted_where = gconf_quote_string (where);

  /* KEEP IN SYNC with gconf-database.c log to string function */
  if (fprintf (append_handle, "%s %u %s %s %s\n",
               add ? "ADD" : "REMOVE", connection_id,
               quoted_db_name, quoted_where, quoted_ior) < 0)
    goto error;

  if (fflush (append_handle) < 0)
    goto error;

  g_free (quoted_db_name);
  g_free (quoted_ior);
  g_free (quoted_where);
  
  return TRUE;

 error:

  if (add)
    gconf_set_error (err,
                     GCONF_ERROR_FAILED,
                     _("Failed to log addition of listener to gconfd logfile; won't be able to re-add the listener if gconfd exits or shuts down (%s)"),
                     g_strerror (errno));
  else
    gconf_set_error (err,
                     GCONF_ERROR_FAILED,
                     _("Failed to log removal of listener to gconfd logfile; might erroneously re-add the listener if gconfd exits or shuts down (%s)"),
                     g_strerror (errno));

  g_free (quoted_db_name);
  g_free (quoted_ior);
  g_free (quoted_where);

  return FALSE;
}

static void
log_client_change (const ConfigListener client,
                   gboolean add)
{
  gchar *ior = NULL;
  gchar *quoted_ior = NULL;
  GError *err;
  
  err = NULL;
  ior = gconf_object_to_string (client, &err);

  if (err != NULL)
    {
      gconf_log (GCL_WARNING, _("Failed to get IOR for client: %s"),
                 err->message);
      g_error_free (err);
      return;
    }
      
  if (ior == NULL)
    return;

  quoted_ior = gconf_quote_string (ior);
  g_free (ior);
  ior = NULL;
  
  if (!open_append_handle (&err))
    {
      gconf_log (GCL_WARNING, _("Failed to open saved state file: %s"),
                 err->message);

      g_error_free (err);
      
      goto error;
    }

  /* KEEP IN SYNC with log to string function */
  if (fprintf (append_handle, "%s %s\n",
               add ? "CLIENTADD" : "CLIENTREMOVE", quoted_ior) < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Failed to write client add to saved state file: %s"),
                 strerror (errno));
      goto error;
    }

  if (fflush (append_handle) < 0)
    {
      gconf_log (GCL_WARNING,
                 _("Failed to flush client add to saved state file: %s"),
                 strerror (errno));
      goto error;
    }

 error:
  g_free (ior);
  g_free (quoted_ior);
}

static void
log_client_add (const ConfigListener client)
{
  log_client_change (client, TRUE);
}

static void
log_client_remove (const ConfigListener client)
{
  log_client_change (client, FALSE);
}

/*
 * Client handling
 */

static GHashTable *client_table = NULL;

static void
add_client (const ConfigListener client)
{
  gconfd_need_log_cleanup ();
  
  if (client_table == NULL)
    client_table = g_hash_table_new ((GHashFunc) g_CORBA_Object_hash,
                                     (GCompareFunc) g_CORBA_Object_equal);

  if (g_hash_table_lookup (client_table, client))
    {
      /* Ignore this case; it happens normally when we added a client
       * from the logfile, and the client also adds itself
       * when it gets a new server objref.
       */
      return;
    }
  else
    {
      CORBA_Environment ev;
      ConfigListener copy;
      
      CORBA_exception_init (&ev);
      copy = CORBA_Object_duplicate (client, &ev);
      g_hash_table_insert (client_table, copy, copy);
      CORBA_exception_free (&ev);

      log_client_add (client);

      gconf_log (GCL_DEBUG, "Added a new client");
    }
}

static void
remove_client (const ConfigListener client)
{
  ConfigListener old_client;
  CORBA_Environment ev;

  gconfd_need_log_cleanup ();
  
  if (client_table == NULL)
    goto notfound;
  
  old_client = g_hash_table_lookup (client_table, 
                                    client);

  if (old_client == NULL)
    goto notfound;

  g_hash_table_remove (client_table,
                       old_client);

  log_client_remove (old_client);
  
  CORBA_exception_init (&ev);
  CORBA_Object_release (old_client, &ev);
  CORBA_exception_free (&ev);

  return;
  
 notfound:
  gconf_log (GCL_WARNING, _("Some client removed itself from the GConf server when it hadn't been added."));  
}

static void
hash_listify_func(gpointer key, gpointer value, gpointer user_data)
{
  GSList** list_p = user_data;

  *list_p = g_slist_prepend(*list_p, value);
}

static GSList*
list_clients (void)
{
  GSList *clients = NULL;

  if (client_table == NULL)
    return NULL;

  g_hash_table_foreach (client_table, hash_listify_func, &clients);

  return clients;
}

static void
log_clients_foreach (gpointer key, gpointer value, gpointer data)
{
  ConfigListener client;
  gchar *ior = NULL;
  gchar *quoted_ior = NULL;
  GError *err;

  client = value;
  
  err = NULL;
  ior = gconf_object_to_string (client, &err);

  if (err != NULL)
    {
      gconf_log (GCL_WARNING, _("Failed to get IOR for client: %s"),
                 err->message);
      g_error_free (err);
      return;
    }
      
  if (ior == NULL)
    return;

  quoted_ior = gconf_quote_string (ior);
  g_free (ior);
  ior = NULL;

  g_string_append (data, "CLIENTADD ");
  g_string_append (data, quoted_ior);
  g_string_append_c (data, '\n');
  g_free (quoted_ior);
}

static void
log_clients_to_string (GString *str)
{
  if (client_table == NULL)
    return;

  g_hash_table_foreach (client_table, log_clients_foreach, str);
}

static void
drop_old_clients (void)
{
  GSList *clients;
  GSList *tmp;
  
  clients = list_clients ();

  if (clients)
    {
      CORBA_Environment ev;

      CORBA_exception_init (&ev);
      
      tmp = clients;
      while (tmp != NULL)
        {
          ConfigListener cl = tmp->data;
          
          ConfigListener_ping (cl, &ev);

          if (ev._major != CORBA_NO_EXCEPTION)
            {
              gconf_log (GCL_DEBUG, "Removing stale client");
              
              remove_client (cl);
              
              CORBA_exception_free (&ev);
            }
          
          tmp = g_slist_next (tmp);
        }

      g_slist_free (clients);
    }
}

static guint
client_count (void)
{
  if (client_table == NULL)
    return 0;
  else
    return g_hash_table_size (client_table);
}
