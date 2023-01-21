/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*- */

/* eel-font-manager.c - Functions for managing fonts.

   Copyright (C) 2000 Eazel, Inc.

   The Gnome Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The Gnome Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the Gnome Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.

   Authors: Pavel Cisler <pavel@eazel.com>,
            Ramiro Estrugo <ramiro@eazel.com>
*/

#include <config.h>
#include "eel-font-manager.h"

#include "eel-glib-extensions.h"
#include "eel-lib-self-check-functions.h"
#include "eel-string-list.h"
#include "eel-string.h"
#include <ctype.h>
#include <libgnome/gnome-util.h>
#include <libgnomevfs/gnome-vfs-types.h>
#include <libgnomevfs/gnome-vfs-directory.h>
#include <libgnomevfs/gnome-vfs-file-info.h>
#include <libgnomevfs/gnome-vfs-init.h>
#include <libgnomevfs/gnome-vfs-ops.h>
#include <libgnomevfs/gnome-vfs-utils.h>
#include <stdio.h>
#include <sys/stat.h>

#define XLFD_INDEX_FOUNDRY		1
#define XLFD_INDEX_FAMILY		2
#define XLFD_INDEX_WEIGHT		3
#define XLFD_INDEX_SLANT		4
#define XLFD_INDEX_SET_WIDTH		5
#define XLFD_INDEX_CHAR_SET_REGISTRY	13
#define XLFD_INDEX_CHAR_SET_ENCODING	14
#define XLFD_INDEX_MAX			XLFD_INDEX_CHAR_SET_ENCODING

#define FONTS_DIR_FILE_NAME		"fonts.dir"
#define FONTS_ALIAS_FILE_NAME		"fonts.alias"
#define FONTS_SCALE_FILE_NAME		"fonts.scale"

#define POSSIBLE_FONT_SERVER_CONFIG_FILES "/etc/X11/fs/config,/etc/xfs.conf"
#define DEFAULT_FONT_DIRECTORY		EEL_DATADIR "/fonts/urw"
#define USER_FONT_DIRECTORY_NAME	"fonts"

#define EEL_FONT_UNDEFINED		((EelFontType) 0)

#define POSTSCRIPT_FONT_MIME_TYPE	"application/x-font-type1"
#define TRUE_TYPE_FONT_MIME_TYPE	"application/x-font-ttf"

#define SOURCE_FONT_DIRECTORY		EEL_SOURCE_DIRECTORY "/data/fonts/urw"
#define DEFAULT_FONT			"n019003l.pfb"
#define DEFAULT_BOLD_FONT		"n019004l.pfb"

/* These font families are black listed, because they 
 * arent useful at all to display "normal" text - at 
 * least in the context of Eel.
 */
static const char *black_listed_font_families[] = {
	"Bookman",
	"Bookman L",
	"Webdings",
	"Wingdings",
	"OCR",
	"Zapf Dingbats",
	"Dingbats",
	"Symbol",
	"Standard Symbols L"
	"cursor",
	"mincho",
	"gothic",
	"MS Reference 2",
	"MS Reference 1",
	"Marlett",
	"cursor"
};

static const char *black_listed_font_foundries[] = {
 	"greek",
 	"grinet",
	/* Abisource fonts are black listed because they
	 * appear to simply be copies of the URW fonts,
	 * and listing them would waste valuable font picker
	 * space for no purpose.
	 */
	"Abisource"
};

static const char *ignored_font_dir_suffices[] = {
	"unscaled",
	"100dpi",
	"75dpi",
	"misc",
	"abisource/fonts",
	"AbiSuite/fonts",
	"fonts/Speedo",
	"fonts/cyrillic",
	"ISO8859-2/Type1",
	"ISO8859-7/Type1"
};

/*
 * FontDescription:
 *
 * A structure that describes a single font entry;
 *
 */
typedef struct {
	char *file_name;
	EelFontType font_type;
	char *foundry;
	char *family;
	char *weight;
	char *slant;
	char *set_width;
	char *char_set;
	gboolean is_ignored;
} FontDescription;

/*
 * FontDescriptionTable:
 *
 * A table of 0 or more font descriptions.
 *
 */
typedef struct {
	char *directory;
	char *fonts_dir_file;
	char *fonts_alias_file;
	char *fonts_scale_file;
	GList *descriptions;
} FontDescriptionTable;

static gboolean                string_is_valid                          (const char                 *string);
static char *                  file_as_string                           (const char                 *file_name);
static gboolean                directory_contains_file                  (const char                 *directory,
									 const char                 *file_name);
static FontDescription *       font_description_new                     (const char                 *font_file_name,
									 EelFontType                 font_type,
									 const char                 *xlfd_string);
static void                    font_description_free                    (FontDescription            *description);
static gboolean                font_description_table_for_each          (const FontDescriptionTable *description_table,
									 EelFontManagerCallback      callback,
									 gpointer                    callback_data);
static char                   *font_description_get_file_name           (const FontDescription      *description);
static char                   *font_description_get_foundry             (const FontDescription      *description);
static char                   *font_description_get_family              (const FontDescription      *description);
static char                   *font_description_get_weight              (const FontDescription      *description);
static char                   *font_description_get_slant               (const FontDescription      *description);
static char                   *font_description_get_set_width           (const FontDescription      *description);
static char                   *font_description_get_char_set            (const FontDescription      *description);
static FontDescriptionTable *  font_description_table_new               (const char                 *font_directory,
									 const GList                *postscript_font_list,
									 const GList                *true_type_font_list);
static void                    font_description_table_add               (FontDescriptionTable       *description_table,
									 const char                 *line,
									 const GList                *postscript_font_list,
									 const GList                *true_type_font_list);
static EelFontType             font_get_font_type                       (const char                 *font_file_name,
									 const GList                *postscript_font_list,
									 const GList                *true_type_font_list);
static guint                   font_description_table_get_length        (const FontDescriptionTable *description_table);
static const FontDescription * font_description_table_peek_nth          (const FontDescriptionTable *description_table,
									 guint                       n);
static char *                  font_description_table_get_nth_file_name (const FontDescriptionTable *table,
									 guint                       n);
static void                    font_description_table_free              (FontDescriptionTable       *table);
static void                    font_description_table_clear             (FontDescriptionTable       *table);
static const FontDescription * font_description_table_find              (const FontDescriptionTable *description_table,
									 const char                 *file_name);
static gboolean                font_directory_is_ignored                (const char                 *font_directory);
static gboolean                font_foundry_is_ignored                  (const char                 *foundry);
static gboolean                font_family_is_ignored                   (const char                 *family);

static gboolean
string_is_valid (const char *string)
{
	return string && string[0] != '\0';
}

static char *
file_as_string (const char *file_name)
{
	struct stat stat_info;
	FILE *stream;
	char *result;
	size_t num_read;

	g_return_val_if_fail (file_name != NULL, NULL);
	g_return_val_if_fail (g_file_exists (file_name), NULL);

	if (stat (file_name, &stat_info) != 0) {
		return NULL;
	}

	if (stat_info.st_size == 0) {
		return NULL;
	}
	
	stream = fopen (file_name, "r");
	
	if (!stream) {
		return NULL;
	}

	result = g_malloc (sizeof (char) * stat_info.st_size + 1);

	num_read = fread (result, sizeof (char), stat_info.st_size, stream);

	fclose (stream);

	if ((ssize_t)num_read != stat_info.st_size) {
		g_free (result);
		return NULL;
	}

	result[stat_info.st_size] = '\0';

	return result;
}

static void
gnome_vfs_init_if_needed (void)
{
	/* Caching the call go gnome_vfs_initialized to make calling this
	 * trivial. Even if this got called from multiple treads at the same time,
	 * using the static here is not a problem because gnome_vfs_init is
	 * atomic and idempotent.
	 */
	static gboolean initialized = FALSE;
	
	if (!initialized) {
		initialized = gnome_vfs_initialized ();
		if (!initialized) {
			initialized = gnome_vfs_init();
		}
	}
}


static FontDescription *
font_description_new (const char *font_file_name,
		      EelFontType font_type,
		      const char *xlfd_string)
{
	FontDescription *description = NULL;
	EelStringList *tokenized_xlfd;
	char *char_set_registry;
	char *char_set_encoding;

	g_return_val_if_fail (string_is_valid (font_file_name), NULL);
	g_return_val_if_fail (string_is_valid (xlfd_string), NULL);
	g_return_val_if_fail (font_type >= EEL_FONT_POSTSCRIPT, NULL);
	g_return_val_if_fail (font_type <= EEL_FONT_TRUE_TYPE, NULL);

	tokenized_xlfd = eel_string_list_new_from_tokens (xlfd_string, "-", FALSE);

	if (eel_string_list_get_length (tokenized_xlfd) == (XLFD_INDEX_MAX + 1)) {
		description = g_new0 (FontDescription, 1);
 		description->file_name = g_strdup (font_file_name);
 		description->font_type = font_type;
		description->foundry = eel_string_list_nth (tokenized_xlfd, XLFD_INDEX_FOUNDRY);
		description->family = eel_string_list_nth (tokenized_xlfd, XLFD_INDEX_FAMILY);
		description->weight = eel_string_list_nth (tokenized_xlfd, XLFD_INDEX_WEIGHT);
		description->slant = eel_string_list_nth (tokenized_xlfd, XLFD_INDEX_SLANT);
		description->set_width = eel_string_list_nth (tokenized_xlfd, XLFD_INDEX_SET_WIDTH);

		char_set_registry = eel_string_list_nth (tokenized_xlfd, XLFD_INDEX_CHAR_SET_REGISTRY);
		char_set_encoding = eel_string_list_nth (tokenized_xlfd, XLFD_INDEX_CHAR_SET_ENCODING);
		description->char_set = g_strdup_printf ("%s-%s", char_set_registry, char_set_encoding);
		g_free (char_set_registry);
		g_free (char_set_encoding);

		description->is_ignored = 
			font_foundry_is_ignored (description->foundry) || font_family_is_ignored (description->family);
	} else {
		g_warning ("'%s' is not a valid XLFD string", xlfd_string);
	}
	
	eel_string_list_free (tokenized_xlfd);

	return description;
}

static void
font_description_free (FontDescription *description)
{
	g_return_if_fail (description != NULL);

	g_free (description->file_name);
	g_free (description->foundry);
	g_free (description->family);
	g_free (description->weight);
	g_free (description->slant);
	g_free (description->set_width);
	g_free (description->char_set);
	g_free (description);
}

static char *
font_description_get_file_name (const FontDescription *description)
{
	g_return_val_if_fail (description != NULL, NULL);

	return g_strdup (description->file_name);
}

static char *
font_description_get_foundry (const FontDescription *description)
{
	g_return_val_if_fail (description != NULL, NULL);

	return g_strdup (description->foundry);
}

static char *
font_description_get_family (const FontDescription *description)
{
	g_return_val_if_fail (description != NULL, NULL);

	return g_strdup (description->family);
}

static char *
font_description_get_weight (const FontDescription *description)
{
	g_return_val_if_fail (description != NULL, NULL);

	return g_strdup (description->weight);
}

static char *
font_description_get_slant (const FontDescription *description)
{
	g_return_val_if_fail (description != NULL, NULL);

	return g_strdup (description->slant);
}

static char *
font_description_get_set_width (const FontDescription *description)
{
	g_return_val_if_fail (description != NULL, NULL);

	return g_strdup (description->set_width);
}

static char *
font_description_get_char_set (const FontDescription *description)
{
	g_return_val_if_fail (description != NULL, NULL);

	return g_strdup (description->char_set);
}

static guint
font_lists_total_num_fonts (const GList *postscript_font_list,
			    const GList *true_type_font_list)
{
	return g_list_length ((GList *) postscript_font_list)
		+ g_list_length ((GList *) true_type_font_list);
}
	
static void
font_description_table_add (FontDescriptionTable *table,
			    const char *line,
			    const GList *postscript_font_list,
			    const GList *true_type_font_list)
{
	char *font_file_name = NULL;
	FontDescription *description;
	char *xlfd_delimiter;
	char *font_file_full_path;
	EelFontType font_type;
	
	g_return_if_fail (table != NULL);
	g_return_if_fail (string_is_valid (line));
	g_return_if_fail (font_lists_total_num_fonts (postscript_font_list, true_type_font_list) > 0);

	xlfd_delimiter = strpbrk (line, " \t");

	if (xlfd_delimiter == NULL) {
		g_warning ("'%s' is not a valid font description line", line);
		return;
	}

	font_file_name = g_strndup (line, xlfd_delimiter - line);

	while (isspace ((guchar) *xlfd_delimiter)) {
		xlfd_delimiter++;
	}

	font_file_full_path = g_strdup_printf ("%s/%s", table->directory, font_file_name);
	font_type = font_get_font_type (font_file_full_path,
					postscript_font_list,
					true_type_font_list);

	if (font_type != EEL_FONT_UNDEFINED) {
		description = font_description_new (font_file_full_path, font_type, xlfd_delimiter);
		if (description != NULL) {
			table->descriptions = g_list_append (table->descriptions, description);
		}
	}

	g_free (font_file_full_path);
	g_free (font_file_name);
}

static EelFontType
font_get_font_type (const char *font_file_name,
		    const GList *postscript_font_list,
		    const GList *true_type_font_list)
{
	const GList *node;

	g_return_val_if_fail (string_is_valid (font_file_name), EEL_FONT_UNDEFINED);
	g_return_val_if_fail (font_lists_total_num_fonts (postscript_font_list, true_type_font_list) > 0,
			      EEL_FONT_UNDEFINED);

	node = postscript_font_list;
	while (node != NULL) {
		if (eel_istr_is_equal (node->data, font_file_name)) {
			return EEL_FONT_POSTSCRIPT;
		}
		node = node->next;
	}

	node = true_type_font_list;
	while (node != NULL) {
		if (eel_istr_is_equal (node->data, font_file_name)) {
			return EEL_FONT_TRUE_TYPE;
		}
		node = node->next;
	}

	return EEL_FONT_UNDEFINED;
}

static guint
font_description_table_get_length (const FontDescriptionTable *table)
{
	g_return_val_if_fail (table != NULL, 0);
	
	return g_list_length (table->descriptions);
}

static const FontDescription *
font_description_table_peek_nth (const FontDescriptionTable *table,
				 guint n)
{
	g_return_val_if_fail (table != NULL, NULL);
	g_return_val_if_fail (n < font_description_table_get_length (table), NULL);

	return g_list_nth_data (table->descriptions, n);
}

static char *
font_description_table_get_nth_file_name (const FontDescriptionTable *table,
					  guint n)
{
	const FontDescription *description;
	
	g_return_val_if_fail (table != NULL, NULL);
	g_return_val_if_fail (n < font_description_table_get_length (table), NULL);

	description = font_description_table_peek_nth (table, n);
	return g_strdup (description->file_name);
}

static void
font_description_table_free (FontDescriptionTable *table)
{
	g_return_if_fail (table != NULL);

	font_description_table_clear (table);
	g_free (table);
}

static void
font_description_table_clear (FontDescriptionTable *table)
{
	GList *node;
		
	g_return_if_fail (table != NULL);

	node = table->descriptions;
	while (node != NULL) {
		font_description_free (node->data);
		node = node->next;
	}

	g_list_free (table->descriptions);
	table->descriptions = NULL;

	g_free (table->directory);
	table->directory = NULL;

	g_free (table->fonts_dir_file);
	table->fonts_dir_file = NULL;

	g_free (table->fonts_alias_file);
	table->fonts_alias_file = NULL;

	g_free (table->fonts_scale_file);
	table->fonts_scale_file = NULL;
}

static const FontDescription *
font_description_table_find (const FontDescriptionTable *table,
			     const char *file_name)
{
	GList *node;
	const FontDescription *description;
	
	g_return_val_if_fail (table != NULL, NULL);
	g_return_val_if_fail (eel_strlen (file_name) > 0, NULL);
	
	for (node = table->descriptions; node != NULL; node = node->next) {
		g_assert (node->data != NULL);
		description = node->data;

		if (!description->is_ignored && eel_str_is_equal (file_name, description->file_name)) {
			return description;
		}
	}

	return NULL;
}

static gboolean
font_description_table_for_each (const FontDescriptionTable *table,
				 EelFontManagerCallback callback,
				 gpointer callback_data)
{
	GList *node;
	const FontDescription *description;
	gboolean cont = TRUE;
		
	g_return_val_if_fail (table != NULL, TRUE);
	g_return_val_if_fail (callback != NULL, TRUE);

	node = table->descriptions;
	while (node != NULL) {
		g_assert (node->data != NULL);
		description = node->data;
		
		cont = TRUE;

		if (!description->is_ignored) {
			cont = (* callback) (description->file_name,
					     description->font_type,
					     description->foundry,
					     description->family,
					     description->weight,
					     description->slant,
					     description->set_width,
					     description->char_set,
					     callback_data);
		}
		
		node = cont ? node->next : NULL;
	}

	return cont;
}

static FontDescriptionTable *
font_description_table_new (const char *font_directory,
			    const GList *postscript_font_list,
			    const GList *true_type_font_list)
{
	FontDescriptionTable *table;
	char *description_file;
	char *description_contents;
	EelStringList *tokenized_contents;
	int i;
	int length;
	gboolean got_count;
	int count;
	char *line;

	g_return_val_if_fail (string_is_valid (font_directory), NULL);
	g_return_val_if_fail (g_file_test (font_directory, G_FILE_TEST_ISDIR), NULL);

	description_file = g_strdup_printf ("%s/%s", font_directory, FONTS_DIR_FILE_NAME);
	description_contents = file_as_string (description_file);
	
	/* Error reading file, report errors by returning NULL. */
	if (description_contents == NULL) {
		g_free (description_file);
		return NULL;
	}

	tokenized_contents = eel_string_list_new_from_tokens (description_contents, "\n", FALSE);

	/* Get the first line from the file. */
	length = eel_string_list_get_length (tokenized_contents);
	if (length == 0) {
		got_count = FALSE;
	} else {
		got_count = eel_string_list_nth_as_integer (tokenized_contents, 0, &count);
	}

	/* Find out how many font entries are described in this file */
	if (!got_count || count + 1 > length) {
		g_free (description_file);
		g_free (description_contents);
		eel_string_list_free (tokenized_contents);
		return NULL;
	}
	    
	/* Create a new table */
	table = g_new0 (FontDescriptionTable, 1);

	/* Assign the directory and description file */
	table->directory = g_strdup (font_directory);
	table->fonts_dir_file = description_file;

	/* Iterate throught the description file contents */
	for (i = 0; i < count; i++) {
		line = eel_string_list_nth (tokenized_contents, i + 1);
		if (line != NULL) {
			font_description_table_add (table,
						    line,
						    postscript_font_list,
						    true_type_font_list);
		}
		g_free (line);
	}
	eel_string_list_free (tokenized_contents);

	/* Assign the alias file if found */
	if (directory_contains_file (font_directory, FONTS_ALIAS_FILE_NAME)) {
		table->fonts_alias_file = g_strdup_printf ("%s/%s", font_directory, FONTS_ALIAS_FILE_NAME);
	}
	
	/* Assign the alias scale if found */
	if (directory_contains_file (font_directory, FONTS_SCALE_FILE_NAME)) {
		table->fonts_scale_file = g_strdup_printf ("%s/%s", font_directory, FONTS_SCALE_FILE_NAME);
	}

	g_free (description_contents);


	return table;
}

static GnomeVFSResult
collect_fonts_from_directory (const char *font_directory,
			      GList **postscript_font_list,
			      GList **true_type_font_list)
{
	GnomeVFSDirectoryHandle *directory;
	GnomeVFSResult result;
	GnomeVFSFileInfo *info;
	char *directory_uri;

	g_return_val_if_fail (string_is_valid (font_directory), GNOME_VFS_ERROR_BAD_PARAMETERS);
	g_return_val_if_fail (postscript_font_list != NULL, GNOME_VFS_ERROR_BAD_PARAMETERS);
	g_return_val_if_fail (true_type_font_list != NULL, GNOME_VFS_ERROR_BAD_PARAMETERS);
	
	directory_uri = gnome_vfs_get_uri_from_local_path (font_directory);

	*postscript_font_list = NULL;
	*true_type_font_list = NULL;
	
	result = gnome_vfs_directory_open (&directory,
					   directory_uri,
					   GNOME_VFS_FILE_INFO_GET_MIME_TYPE
					   | GNOME_VFS_FILE_INFO_FOLLOW_LINKS,
					   NULL);
	g_free (directory_uri);
	
	if (result != GNOME_VFS_OK) {
		return result;
	}
	
	
	while (TRUE) {
		info = gnome_vfs_file_info_new ();
		result = gnome_vfs_directory_read_next (directory, info);
		if (result == GNOME_VFS_OK
		    && (info->valid_fields & GNOME_VFS_FILE_INFO_FIELDS_MIME_TYPE) != 0) {
			if (strcasecmp (info->mime_type, POSTSCRIPT_FONT_MIME_TYPE) == 0) {
				*postscript_font_list = g_list_prepend (*postscript_font_list,
					g_strconcat (font_directory,
						"/", info->name, NULL));
			} else if (strcasecmp (info->mime_type, TRUE_TYPE_FONT_MIME_TYPE) == 0) {
				*true_type_font_list = g_list_prepend (*true_type_font_list,
					g_strconcat (font_directory,
						"/", info->name, NULL));
			}
		}
		gnome_vfs_file_info_unref (info);
		
		if (result == GNOME_VFS_ERROR_EOF) {
			break;
		}
	}
	
	gnome_vfs_directory_close (directory);
	
	return GNOME_VFS_OK;
}


static void
chop_off_comments (char *line)
{

	/* Terminates a string right at the start of a comment, stripping it from the
	 * string.
	 */
	gboolean saw_escape;
	char *scanner;

	saw_escape = FALSE;
	for (scanner = line; *scanner != '\0'; scanner++) {
		if (saw_escape) {
			saw_escape = FALSE;
			continue;
		}		
		if (*scanner == '\\') {
	
	
			saw_escape = TRUE;
			continue;
		}
		if (*scanner == '#') {
			*scanner = '\0';
			break;
		}
	}
}

static void
next_token (const char *buffer, int from, int *token_start, int *token_end)
{
	gboolean saw_escape;
	const char *scanner;

	g_assert ((int) strlen (buffer) >= from);

	*token_start = -1;
	*token_end = -1;
	
	/* strip white space */
	saw_escape = FALSE;
	for (scanner = buffer + from; *scanner != '\0'; scanner++) {
		if (saw_escape) {
			saw_escape = FALSE;
			continue;
		}		
		if (*scanner == '\\') {
			saw_escape = TRUE;
			continue;
		}
		if (!isspace ((guchar) *scanner) && *scanner != '\n') {
			*token_start = scanner - buffer;
			break;
		}
	}	

	if (*scanner == ',') {
		*token_end = *token_start + 1;
		return;
	}
	
	/* go until token end */
	saw_escape = FALSE;
	for (; *scanner != '\0'; scanner++) {
		if (saw_escape) {
			saw_escape = FALSE;
			continue;
		}		
		if (*scanner == '\\') {
			saw_escape = TRUE;
			continue;
		}
		if (isspace ((guchar) *scanner) || *scanner == ',') {
			break;
		}
	}	
	
	if (*token_start >= 0) {
		*token_end = scanner - buffer;
	}
}

#define READ_BUFFER_SIZE 2048

static gboolean
token_matches (const char *buffer, int start, int end, const char *pattern)
{
	if (start < 0) {
		return FALSE;
	}

	return strncmp (buffer + start, pattern, end - start) == 0;
}

typedef enum {
	EXPECT_CATALOGUE,
	EXPECT_ASSIGNMENT,
	EXPECT_DIRECTORY,
	EXPECT_COMMA
} FontConfigParseState;

static void
font_server_for_each_font_directory_internal (void (* callback) (const char *font_directory, gpointer callback_data),
					      gpointer callback_data,
					      char *buffer,
					      FILE *file)
{
	FontConfigParseState state;
	int token_start, token_end;
	char *font_directory;

	g_return_if_fail (callback != NULL);
	g_return_if_fail (buffer != NULL);
	g_return_if_fail (file != NULL);

	state = EXPECT_CATALOGUE;
	while (TRUE) {
		fgets (buffer, READ_BUFFER_SIZE, file);
		if (strlen (buffer) == 0) {
			if (state != EXPECT_COMMA) {
				g_warning ("unexpected file end.");
			}
			break;
		}
		
		chop_off_comments (buffer);
		
		token_start = 0;
		while (TRUE) {
			next_token (buffer, token_start, &token_start, &token_end);
	
			if (token_start < 0) {
				break;
			}

			switch (state) {
			case EXPECT_CATALOGUE:
				if (token_matches(buffer, token_start, token_end, "catalogue")) {
					state = EXPECT_ASSIGNMENT;
				}
				break;
				
			case EXPECT_ASSIGNMENT:
				if (!token_matches(buffer, token_start, token_end, "=")) {
					g_warning (" expected token \"=\" .");
					return;
				}
				state = EXPECT_DIRECTORY;
				break;
				
			case EXPECT_DIRECTORY:
				if (token_matches(buffer, token_start, token_end, ",")) {
					g_warning (" expected directory name.");
					return;
				}
				/* found a directory, call an each function on it */
				font_directory = g_strndup (buffer + token_start, token_end - token_start);
				(* callback) (font_directory, callback_data);
				g_free (font_directory);
				state = EXPECT_COMMA;
				break;

			case EXPECT_COMMA:
				if (!token_matches(buffer, token_start, token_end, ",")) {
					/* we are done, no more directories */
					return;
				}
				state = EXPECT_DIRECTORY;
				break;				
			}
			
			token_start = token_end;
		}
	}
}

static void
font_server_for_each_font_directory (const char *font_config_file_path,
				     void (* callback) (const char *font_directory, gpointer callback_data),
				     gpointer callback_data)
{
	/* scan the font config file, finding all the font directory paths */
	FILE *font_config_file;
	char *buffer;

	g_return_if_fail (string_is_valid (font_config_file_path));
	g_return_if_fail (callback != NULL);
	g_return_if_fail (callback_data != NULL);
	
	font_config_file = fopen (font_config_file_path, "r");

	if (font_config_file == NULL) {
		return;
	}
	
	buffer = g_malloc (READ_BUFFER_SIZE);
	font_server_for_each_font_directory_internal (callback,
						      callback_data,
						      buffer,
						      font_config_file);
	
	g_free (buffer);
	fclose (font_config_file);
}

static gboolean
directory_contains_file (const char *directory,
			 const char *file_name)
{
	gboolean result;
	char *path;

	g_return_val_if_fail (string_is_valid (directory), FALSE);
	g_return_val_if_fail (string_is_valid (file_name), FALSE);

	path = g_strdup_printf ("%s/%s", directory, file_name);
	result = g_file_exists (path);
	g_free (path);

	return result;
}

/* Iterating directories is slow cause of all the mime sniffing that
 * has to happen on each potential scalalble font.  By Ignoring
 * directories that arent interesting, we make things much faster.
 */
static gboolean
font_directory_is_ignored (const char *font_directory)
{
	guint i;

	g_return_val_if_fail (string_is_valid (font_directory), TRUE);

	for (i = 0; i < EEL_N_ELEMENTS (ignored_font_dir_suffices); i++) {
		if (eel_str_has_suffix (font_directory, ignored_font_dir_suffices[i])) {
			return TRUE;
		}
	}

	return FALSE;
}

static gboolean
font_foundry_is_ignored (const char *foundry) 
{
	guint i;
	
	g_return_val_if_fail (foundry != NULL, TRUE);
	
	for (i = 0; i < EEL_N_ELEMENTS (black_listed_font_foundries); i++) {
		if (eel_istr_is_equal (foundry, black_listed_font_foundries[i])) {
			return TRUE;
		}
	}
	
	return FALSE;
}

static gboolean
font_family_is_ignored (const char *family) 
{
	guint i;
	
	g_return_val_if_fail (family != NULL, TRUE);
	
	for (i = 0; i < EEL_N_ELEMENTS (black_listed_font_families); i++) {
		if (eel_istr_is_equal (family, black_listed_font_families[i])) {
			return TRUE;
		}
	}
	
	return FALSE;
}

static void
font_manager_collect_font_tables (const char *font_directory,
				  GList **collected_font_tables)
{
	GList *postscript_font_list = NULL;
	GList *true_type_font_list = NULL;
	FontDescriptionTable *table;

	g_return_if_fail (string_is_valid (font_directory));
	g_return_if_fail (collected_font_tables != NULL);

	if (font_directory_is_ignored (font_directory)) {
		return;
	}
	
	/* Collect postscript and true type font in this directory */
	collect_fonts_from_directory (font_directory, &postscript_font_list, &true_type_font_list);

	/* No scalable fonts found; we're done */
	if (g_list_length (postscript_font_list) == 0 
	    && g_list_length (true_type_font_list) == 0) {
		return;
	}

	/* If no "fonts.dir" exists, then the user has a missing description file (broken setup) */
	if (!directory_contains_file (font_directory, FONTS_DIR_FILE_NAME)) {
		eel_g_list_free_deep (postscript_font_list);
		eel_g_list_free_deep (true_type_font_list);
		g_warning ("Direcotry '%s' contains scalable fonts but no '%s' description file.",
			   font_directory,
			   FONTS_DIR_FILE_NAME);
		return;
	}
	
	table = font_description_table_new (font_directory, postscript_font_list, true_type_font_list);
	if (table == NULL) {
		eel_g_list_free_deep (postscript_font_list);
		eel_g_list_free_deep (true_type_font_list);
		g_warning ("Error trying to process font directory '%s'.", font_directory);
		return;
	}

	*collected_font_tables = g_list_append (*collected_font_tables, table);

	eel_g_list_free_deep (postscript_font_list);
	eel_g_list_free_deep (true_type_font_list);
}

static void
font_server_for_each_callback (const char *font_directory,
			       gpointer callback_data)
{
	g_return_if_fail (string_is_valid (font_directory));
	g_return_if_fail (callback_data != NULL);

	font_manager_collect_font_tables (font_directory, callback_data);
}

static GList *global_font_table = NULL;

static void
font_table_list_free (GList *font_table_list)
{
	GList *node;

	node = font_table_list;
	while (node != NULL) {
		g_assert (node->data != NULL);
		font_description_table_free (node->data);
		node = node->next;
	}
	g_list_free (font_table_list);
}

static const FontDescription *
font_table_list_find (const GList *font_table_list,
		      const char *file_name)
{
	const GList *node;
	const FontDescription *description;

	g_return_val_if_fail (file_name != NULL, NULL);

	for (node = font_table_list; node != NULL; node = node->next) {
		g_assert (node->data != NULL);
		description = font_description_table_find (node->data, file_name);
		if (description != NULL) {
			return description;
		}
	}

	return NULL;
}

static void
free_font_tables (void)
{
	font_table_list_free (global_font_table);
	global_font_table = NULL;
}

static gboolean
try_using_font_server (GList *font_table)
{
	EelStringList *possible_configs;
	guint i;
	char *nth_config_file;
	gboolean found_config;

	possible_configs = eel_string_list_new_from_tokens (POSSIBLE_FONT_SERVER_CONFIG_FILES,
							    ",",
							    TRUE);
	found_config = FALSE;
	for (i = 0; i < eel_string_list_get_length (possible_configs) && !found_config; i++) {
		nth_config_file = eel_string_list_nth (possible_configs, i);
		if (g_file_exists (nth_config_file)) {
			found_config = TRUE;
			font_server_for_each_font_directory (nth_config_file,
							     font_server_for_each_callback,
							     &font_table);
		}
		g_free (nth_config_file);
	}
	eel_string_list_free (possible_configs);

	return found_config;
}

static void
ensure_local_font_table (void)
{
	char *user_font_dir;

	if (global_font_table != NULL) {
		return;
	}

	/* Populate the default font table if needed.  Use the installed fonts
	 * if available.  Otherwise use the ones in the source tree itself, so that
	 * checks will work even if Eel has not undergone 'make install.'
	 */
	if (g_file_exists (DEFAULT_FONT_DIRECTORY "/" DEFAULT_FONT)) {
		font_manager_collect_font_tables (DEFAULT_FONT_DIRECTORY, &global_font_table);
	} else {
		font_manager_collect_font_tables (SOURCE_FONT_DIRECTORY, &global_font_table);
	}

	/* Populate the user font table if needed */
	user_font_dir = g_strdup_printf ("%s/.nautilus/%s", g_get_home_dir (), USER_FONT_DIRECTORY_NAME);
	if (g_file_test (user_font_dir, G_FILE_TEST_ISDIR)) {
		font_manager_collect_font_tables (user_font_dir, &global_font_table);
	}
	g_free (user_font_dir);

	/* Populate the system font table if needed - using the font server's configuration */
	if (!try_using_font_server (global_font_table)) {
		/* If that didnt work, then fallback to the directories known at configure time. */
#ifdef EEL_POSSIBLE_FONT_DIRECTORIES
		EelStringList *possible_dirs;
		guint i;
		char *nth_dir;
		
		possible_dirs = eel_string_list_new_from_tokens (EEL_POSSIBLE_FONT_DIRECTORIES,
								 ",",
								 TRUE);
		
		for (i = 0; i < eel_string_list_get_length (possible_dirs); i++) {
			nth_dir = eel_string_list_nth (possible_dirs, i);
			font_manager_collect_font_tables (nth_dir, &global_font_table);
			g_free (nth_dir);
		}

		eel_string_list_free (possible_dirs);
#endif
	}
	
	g_atexit (free_font_tables);
}

/* Public */

/**
 * eel_font_manager_for_each_font:
 * @callback: A callback to be called for each scalable font in the system.
 * @callback_data: User's data.
 *
 * Iterate all the scalable fonts available in the system.  The available
 * fonts are the sum of:
 *
 * 1) Fallback fonts installed by Eel
 * 2) User fonts found in ~/.eel/fonts
 * 3) Fonts listed in the font servers config file (/etc/X11/fs/config)
 *
 */
void
eel_font_manager_for_each_font (EelFontManagerCallback callback,
				     gpointer callback_data)
{
	GList *node;
	gboolean cont = TRUE;

 	g_return_if_fail (callback != NULL);

	/* We will be making GnomeVFS calls */
	gnome_vfs_init_if_needed ();
	
	/* Ensure that all the font tables exist */
	ensure_local_font_table ();

	for (node = global_font_table; node != NULL && cont; node = node->next) {
		g_assert (node->data != NULL);
		cont = font_description_table_for_each (node->data, callback, callback_data);
	}
}

/* Return the default font.  It will be found either in place where eel
 * gets installed (via 'make install') or the source directory if eel
 * has not undergone 'make install'
 */
char *
eel_font_manager_get_default_font (void)
{
	guint i;

	static const char *default_fonts[] = {
		DEFAULT_FONT_DIRECTORY "/" DEFAULT_FONT,
		SOURCE_FONT_DIRECTORY "/" DEFAULT_FONT
	};
	static const char *default_font = NULL;

	if (default_font)
		return g_strdup (default_font);
		
	for (i = 0; i < EEL_N_ELEMENTS (default_fonts); i++) {
		if (g_file_exists (default_fonts[i])) {
			default_font = default_fonts[i];
			return g_strdup (default_font);
		}
	}

	return NULL;
}

/* Return the default vold font.  It will be found either in place where eel
 * gets installed (via 'make install') or the source directory if eel
 * has not undergone 'make install'
 */
char *
eel_font_manager_get_default_bold_font (void)
{
	guint i;

	static const char *default_bold_fonts[] = {
		DEFAULT_FONT_DIRECTORY "/" DEFAULT_BOLD_FONT,
		SOURCE_FONT_DIRECTORY "/" DEFAULT_BOLD_FONT
	};
	static const char *default_bold_font = NULL;

	if (default_bold_font)
		return g_strdup (default_bold_font);
	
	for (i = 0; i < EEL_N_ELEMENTS (default_bold_fonts); i++) {
		if (g_file_exists (default_bold_fonts[i])) {
			default_bold_font = default_bold_fonts[i];
			return g_strdup (default_bold_font);
		}
	}

	return NULL;
}

gboolean
eel_font_manager_file_is_scalable_font (const char *file_name)
{
	gboolean is_scalable_font = FALSE;
	char *uri;
	GnomeVFSFileInfo *info;
	GnomeVFSResult result;
	
	g_return_val_if_fail (eel_strlen (file_name) > 0, FALSE);

	gnome_vfs_init_if_needed ();

	uri = gnome_vfs_get_uri_from_local_path (file_name);

	info = gnome_vfs_file_info_new ();
	result = gnome_vfs_get_file_info (uri, info,
					  GNOME_VFS_FILE_INFO_GET_MIME_TYPE
					  | GNOME_VFS_FILE_INFO_FOLLOW_LINKS);

	if (result == GNOME_VFS_OK) {
		is_scalable_font = 
			eel_istr_is_equal (info->mime_type, POSTSCRIPT_FONT_MIME_TYPE)
			|| eel_istr_is_equal (info->mime_type, TRUE_TYPE_FONT_MIME_TYPE);
	}

	gnome_vfs_file_info_unref (info);
	g_free (uri);

	return is_scalable_font;
}

typedef struct
{
	const FontDescription *description;
	char *found_file_name;
} FindData;

static gboolean
font_list_find_bold_callback (const char *font_file_name,
			      EelFontType font_type,
			      const char *foundry,
			      const char *family,
			      const char *weight,
			      const char *slant,
			      const char *set_width,
			      const char *char_set,
			      gpointer callback_data)
{
	FindData *data;

	g_return_val_if_fail (font_file_name != NULL, FALSE);
	g_return_val_if_fail (foundry != NULL, FALSE);
	g_return_val_if_fail (family != NULL, FALSE);
	g_return_val_if_fail (weight != NULL, FALSE);
	g_return_val_if_fail (slant != NULL, FALSE);
	g_return_val_if_fail (set_width != NULL, FALSE);
	g_return_val_if_fail (char_set != NULL, FALSE);
	g_return_val_if_fail (callback_data != NULL, FALSE);

	data = callback_data;

	g_return_val_if_fail (data->description != NULL, FALSE);
	g_return_val_if_fail (data->found_file_name == NULL, FALSE);
	
	if (eel_istr_is_equal (data->description->foundry, foundry)
	    && eel_istr_is_equal (data->description->family, family)
	    && eel_istr_is_equal (data->description->slant, slant)
	    && eel_istr_is_equal (data->description->set_width, set_width)
	    && eel_istr_is_equal (data->description->char_set, char_set)
	    && eel_font_manager_weight_is_bold (weight)) {
		data->found_file_name = g_strdup (font_file_name);
	}

	return (data->found_file_name == NULL);
}

char *
eel_font_manager_get_bold (const char *plain_font)
{
	FindData data;

	g_return_val_if_fail (eel_strlen (plain_font) > 0, NULL);

	ensure_local_font_table ();

 	data.description = font_table_list_find (global_font_table, plain_font);

 	if (data.description == NULL) {
 		return g_strdup (plain_font);
 	}
	
 	data.found_file_name = NULL;
 	eel_font_manager_for_each_font (font_list_find_bold_callback, &data);
	
 	if (data.found_file_name != NULL) {
 		return data.found_file_name;
 	}
	
	return g_strdup (plain_font);
}

gboolean
eel_font_manager_weight_is_bold (const char *weight)
{
	g_return_val_if_fail (weight != NULL, FALSE);

	return (eel_istr_is_equal (weight, "bold")
		|| eel_istr_is_equal (weight, "demibold")
		|| eel_istr_is_equal (weight, "black"));
}

#if !defined (EEL_OMIT_SELF_CHECK)

#define TEST_FONT1 SOURCE_FONT_DIRECTORY "/" "n019003l.pfb"
#define TEST_FONT2 SOURCE_FONT_DIRECTORY "/" "n019004l.pfb"
#define TEST_FONT3 SOURCE_FONT_DIRECTORY "/" "n019023l.pfb"
#define TEST_FONT4 SOURCE_FONT_DIRECTORY "/" "n019024l.pfb"

static char *
call_chop_off_comments (const char *input)
{
	char *test_copy;
	test_copy = g_strdup (input);
	chop_off_comments (test_copy);
	return test_copy;
}

void
eel_self_check_font_manager (void)
{
	FontDescriptionTable *table;
	const FontDescription *description;
	GList *font_table_list = NULL;

	/* chop_off_comments() */
	EEL_CHECK_STRING_RESULT (call_chop_off_comments ("foo bar"), "foo bar");
	EEL_CHECK_STRING_RESULT (call_chop_off_comments ("foo bar\n"), "foo bar\n");
	EEL_CHECK_STRING_RESULT (call_chop_off_comments ("#foo bar"), "");
	EEL_CHECK_STRING_RESULT (call_chop_off_comments ("foo bar#"), "foo bar");
	EEL_CHECK_STRING_RESULT (call_chop_off_comments ("\\foo bar"), "\\foo bar");
	EEL_CHECK_STRING_RESULT (call_chop_off_comments ("\\#foo bar"), "\\#foo bar");
	EEL_CHECK_STRING_RESULT (call_chop_off_comments ("\\##foo bar"), "\\#");

	g_return_if_fail (g_file_exists (SOURCE_FONT_DIRECTORY));

	g_return_if_fail (g_file_exists (TEST_FONT1));
	g_return_if_fail (g_file_exists (TEST_FONT2));
	g_return_if_fail (g_file_exists (TEST_FONT3));
	g_return_if_fail (g_file_exists (TEST_FONT4));

	font_manager_collect_font_tables (SOURCE_FONT_DIRECTORY, &font_table_list);
	g_return_if_fail (font_table_list != NULL);

	g_return_if_fail (g_list_nth_data (font_table_list, 0) != NULL);
	table = g_list_nth_data (font_table_list, 0);

 	EEL_CHECK_INTEGER_RESULT (font_description_table_get_length (table), 4);
 	EEL_CHECK_STRING_RESULT (font_description_table_get_nth_file_name (table, 0), TEST_FONT1);
 	EEL_CHECK_STRING_RESULT (font_description_table_get_nth_file_name (table, 1), TEST_FONT2);
 	EEL_CHECK_STRING_RESULT (font_description_table_get_nth_file_name (table, 2), TEST_FONT3);
 	EEL_CHECK_STRING_RESULT (font_description_table_get_nth_file_name (table, 3), TEST_FONT4);

	description = font_description_table_peek_nth (table, 0);
 	EEL_CHECK_STRING_RESULT (font_description_get_file_name (description), TEST_FONT1);
 	EEL_CHECK_STRING_RESULT (font_description_get_foundry (description), "URW");
 	EEL_CHECK_STRING_RESULT (font_description_get_family (description), "Helvetica Default");
 	EEL_CHECK_STRING_RESULT (font_description_get_weight (description), "medium");
 	EEL_CHECK_STRING_RESULT (font_description_get_slant (description), "r");
 	EEL_CHECK_STRING_RESULT (font_description_get_set_width (description), "normal");
 	EEL_CHECK_STRING_RESULT (font_description_get_char_set (description), "iso8859-1");

	description = font_description_table_peek_nth (table, 1);
 	EEL_CHECK_STRING_RESULT (font_description_get_file_name (description), TEST_FONT2);
 	EEL_CHECK_STRING_RESULT (font_description_get_foundry (description), "URW");
 	EEL_CHECK_STRING_RESULT (font_description_get_family (description), "Helvetica Default");
 	EEL_CHECK_STRING_RESULT (font_description_get_weight (description), "bold");
 	EEL_CHECK_STRING_RESULT (font_description_get_slant (description), "r");
 	EEL_CHECK_STRING_RESULT (font_description_get_set_width (description), "normal");
 	EEL_CHECK_STRING_RESULT (font_description_get_char_set (description), "iso8859-1");

	description = font_description_table_peek_nth (table, 2);
 	EEL_CHECK_STRING_RESULT (font_description_get_file_name (description), TEST_FONT3);
 	EEL_CHECK_STRING_RESULT (font_description_get_foundry (description), "URW");
 	EEL_CHECK_STRING_RESULT (font_description_get_family (description), "Helvetica Default");
 	EEL_CHECK_STRING_RESULT (font_description_get_weight (description), "medium");
 	EEL_CHECK_STRING_RESULT (font_description_get_slant (description), "o");
 	EEL_CHECK_STRING_RESULT (font_description_get_set_width (description), "normal");
 	EEL_CHECK_STRING_RESULT (font_description_get_char_set (description), "iso8859-1");

	description = font_description_table_peek_nth (table, 3);
 	EEL_CHECK_STRING_RESULT (font_description_get_file_name (description), TEST_FONT4);
 	EEL_CHECK_STRING_RESULT (font_description_get_foundry (description), "URW");
 	EEL_CHECK_STRING_RESULT (font_description_get_family (description), "Helvetica Default");
 	EEL_CHECK_STRING_RESULT (font_description_get_weight (description), "bold");
 	EEL_CHECK_STRING_RESULT (font_description_get_slant (description), "o");
 	EEL_CHECK_STRING_RESULT (font_description_get_set_width (description), "normal");
 	EEL_CHECK_STRING_RESULT (font_description_get_char_set (description), "iso8859-1");

	font_table_list_free (font_table_list);
}

#endif /* !EEL_OMIT_SELF_CHECK */
