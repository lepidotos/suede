#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

PKG_NAME="GNOME VFS"

(test -f $srcdir/configure.in \
  && test -f $srcdir/HACKING \
  && test -d $srcdir/libgnomevfs) || {
    echo -n "**Error**: Directory "\`$srcdir\'" does not look like the"
    echo " top-level $PKG_NAME directory"
    exit 1
}

. $srcdir/macros/autogen.sh
