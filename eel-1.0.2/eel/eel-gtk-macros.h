/* -*- Mode: C; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*-

   eel-gtk-macros.h: Macros to reduce boilerplate when using GTK.
 
   Copyright (C) 1999, 2000, 2001 Eazel, Inc.
  
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.
  
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
  
   You should have received a copy of the GNU General Public
   License along with this program; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
  
   Authors: Darin Adler <darin@eazel.com>
            Ramiro Estrugo <ramiro@eazel.com>
*/

#ifndef EEL_GTK_MACROS_H
#define EEL_GTK_MACROS_H

#include <glib.h>

/* Define a parent_class global and a get_type function for a GTK class.
   Since this is boilerplate, it's better not to repeat it over and over again.
   Called like this:

       EEL_DEFINE_CLASS_BOILERPLATE (EelBookmark, eel_bookmark, GTK_TYPE_OBJECT)

   The parent_class_type parameter is guaranteed to be evaluated only once
   so it can be an expression, even an expression that contains a function call.
*/
#define EEL_DEFINE_CLASS_BOILERPLATE(class_name, class_name_in_function_format, parent_class_type) \
                                                                                                        \
static gpointer parent_class;                                                                           \
                                                                                                        \
GtkType                                                                                                 \
class_name_in_function_format##_get_type (void)                                                         \
{                                                                                                       \
	GtkType parent_type;                                                                            \
	static GtkType type;                                                                            \
                                                                                                        \
	if (type == 0) {                                                                                \
		static GtkTypeInfo info = {                                                             \
			#class_name,                                                                    \
			sizeof (class_name),                                                            \
			sizeof (class_name##Class),                                                     \
			(GtkClassInitFunc)class_name_in_function_format##_initialize_class,             \
			(GtkObjectInitFunc)class_name_in_function_format##_initialize,                  \
			NULL,                                                                           \
			NULL,                                                                           \
			NULL                                                                            \
		};                                                                                      \
                                                                                           		\
		parent_type = (parent_class_type);                                                      \
		type = gtk_type_unique (parent_type, &info);                                            \
		parent_class = gtk_type_class (parent_type);                                            \
	}                                                                                               \
                                                                                                        \
	return type;                                                                                    \
}

/* Call a parent class version of a virtual function (or default
 * signal handler since that's the same thing). Nice because it
 * documents what it's doing and there is less chance for a
 * typo. Depends on the parent class pointer having the conventional
 * name "parent_class" as the boilerplate macro above does it.
 */
#define EEL_CALL_PARENT(parent_class_cast_macro, signal, parameters)                    \
                                                                                             \
G_STMT_START {                                                                               \
	if (parent_class_cast_macro (parent_class)->signal != NULL) {                        \
		(* parent_class_cast_macro (parent_class)->signal) parameters;               \
        }                                                                                    \
} G_STMT_END

/* Same thing, for functions with a return value. */
#define EEL_CALL_PARENT_WITH_RETURN_VALUE(parent_class_cast_macro, signal, parameters)  \
                                                                                             \
(parent_class_cast_macro (parent_class)->signal == NULL)                                     \
	? 0                                                                                  \
	: ((* parent_class_cast_macro (parent_class)->signal) parameters)

/* Call a virtual function. Useful when the virtual function is not a
 * signal, otherwise you want to gtk_signal emit. Nice because it
 * documents what it's doing and there is less chance for a typo.
 */
#define EEL_CALL_METHOD(class_cast_macro, object, signal, parameters)                   \
                                                                                             \
G_STMT_START {                                                                               \
	if (class_cast_macro (GTK_OBJECT (object)->klass)->signal != NULL) {                 \
		(* class_cast_macro (GTK_OBJECT (object)->klass)->signal) parameters;        \
	}                                                                                    \
} G_STMT_END

/* Same thing, for functions with a return value. */
#define EEL_CALL_METHOD_WITH_RETURN_VALUE(class_cast_macro, object, signal, parameters) \
                                                                                             \
(class_cast_macro (GTK_OBJECT (object)->klass)->signal == NULL)                              \
	? 0                                                                                  \
	: ((* class_cast_macro (GTK_OBJECT (object)->klass)->signal) parameters)

#ifndef G_DISABLE_ASSERT

/* Define a signal that is not implemented by this class but must be 
 * implemented by subclasses. This macro should be used inside the
 * class initialization function. The companion macro EEL_IMPLEMENT_MUST_OVERRIDE_SIGNAL
 * must be used earlier in the file. Called like this:
 * 
 * EEL_ASSIGN_MUST_OVERRIDE_SIGNAL (klass,
 *					 fm_directory_view,
 *					 clear); 
 */
#define EEL_ASSIGN_MUST_OVERRIDE_SIGNAL(class_pointer, class_name_in_function_format, signal)     \
                                                                                                       \
* (void (**)(void)) & (class_pointer)->signal = class_name_in_function_format##_unimplemented_##signal

/* Provide a debug-only implementation of a signal that must be implemented
 * by subclasses. The debug-only implementation fires a warning if it is called.
 * This macro should be placed as if it were a function, earlier in the file
 * than the class initialization function. Called like this:
 * 
 * EEL_IMPLEMENT_MUST_OVERRIDE_SIGNAL (fm_directory_view, clear);
 */
#define EEL_IMPLEMENT_MUST_OVERRIDE_SIGNAL(class_name_in_function_format, signal)        \
                                                                                              \
static void                                                                                   \
class_name_in_function_format##_unimplemented_##signal (void)                                 \
{                                                                                             \
	g_warning ("failed to override signal " #class_name_in_function_format "->" #signal); \
}

#else /* G_DISABLE_ASSERT */

#define EEL_DEFINE_MUST_OVERRIDE_SIGNAL(class_cast_macro, class_pointer, class_name_in_function_format, signal)
#define EEL_IMPLEMENT_MUST_OVERRIDE_SIGNAL(class_name_in_function_format, signal)
#define EEL_ASSIGN_MUST_OVERRIDE_SIGNAL(class_pointer, class_name_in_function_format, signal)

#endif /* G_DISABLE_ASSERT */

/* Access the class for a given object. */
#define EEL_CLASS(object) (GTK_OBJECT (object)->klass)

/* Access a method. */
#define EEL_ACCESS_METHOD(class_cast_macro, object, method)			\
(class_cast_macro (EEL_CLASS (object))->method)

/* Invoke a method for a given object. */
#define EEL_INVOKE_METHOD(class_cast_macro, object, method, parameters)		\
((* EEL_ACCESS_METHOD (class_cast_macro, object, method)) parameters)

/* Assert the non-nullness of a method for a given object. */
#define EEL_ASSERT_METHOD(class_cast_macro, object, method)			\
g_assert (EEL_ACCESS_METHOD (class_cast_macro, object, method) != NULL)

/* Invoke a method if it ain't null. */
#define EEL_INVOKE_METHOD_IF(class_cast_macro, object, method, parameters) 	\
(EEL_ACCESS_METHOD (class_cast_macro, object, method) ? 0 :			\
	EEL_INVOKE_METHOD (class_cast_macro, object, method, parameters))

#endif /* EEL_GTK_MACROS_H */
