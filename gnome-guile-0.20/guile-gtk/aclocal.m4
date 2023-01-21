dnl aclocal.m4 generated automatically by aclocal 1.4

dnl Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

dnl
dnl GNOME_CHECK_GUILE (failflag)
dnl
dnl if failflag is "fail" then GNOME_CHECK_GUILE will abort if guile is not found.
dnl

AC_DEFUN([GNOME_CHECK_GUILE],
[
	saved_ldflags="$LDFLAGS"
	saved_cppflags="$CPPFLAGS"
	LDFLAGS="$LDFLAGS $GNOME_LIBDIR"

	AC_CHECK_LIB(qthreads,main,[
		QTTHREADS_LIB="-lqthreads"
	],[
		AC_CHECK_LIB(qt, qt_null, QTTHREADS_LIB="-lqt")
	],$LIBS)
	AC_SUBST(QTTHREADS_LIB)

	AC_CHECK_LIB(termcap,main,TERMCAP_LIB="-ltermcap")
	AC_CHECK_LIB(readline,main,READLINE_LIB="-lreadline",,$TERMCAP_LIB)

	AC_SUBST(TERMCAP_LIB)
	AC_SUBST(READLINE_LIB)

	if test "x$cross_compiling" = "xyes" ; then
	  name_build_guile="$target_alias-guile-config"
	else
	  name_build_guile="guile-config"
	fi

	AC_CHECK_PROG(BUILD_GUILE, $name_build_guile, yes, no)

	if test "x$BUILD_GUILE" = "xyes"; then
	    AC_MSG_CHECKING(whether $name_build_guile works)
	    if test x`$name_build_guile --version >/dev/null 2>&1 || \
		echo no` = xno; then
		BUILD_GUILE=no
	    fi
	    AC_MSG_RESULT($BUILD_GUILE)
	else

	    if test "x$cross_compiling" = "xyes" ; then
		name_build_guile="$target_alias-build-guile"
	    else	
		name_build_guile="build-guile"
	    fi

	    AC_CHECK_PROG(BUILD_GUILE, $name_build_guile, yes, no)

	    if test "x$BUILD_GUILE" = "xyes"; then
		AC_MSG_CHECKING(whether $name_build_guile works)
		if test x`$name_build_guile --version >/dev/null 2>&1 || \
	 	    echo no` = xno; then
		    BUILD_GUILE=no
		fi
		AC_MSG_RESULT($BUILD_GUILE)
	    fi
	fi

	AC_CHECK_LIB(m, sin)

	if test "x$BUILD_GUILE" = "xyes"; then
		AC_MSG_CHECKING(for guile libraries)
		GUILE_LIBS="`$name_build_guile link`"
		AC_MSG_RESULT($GUILE_LIBS)
		AC_MSG_CHECKING(for guile headers)
		GUILE_INCS="`$name_build_guile compile`"
		AC_MSG_RESULT($GUILE_INCS)
	else
		GUILE_LIBS="$GNOME_LIBDIR"
		GUILE_INCS="$GNOME_INCLUDEDIR"
		AC_CHECK_LIB(rx, main, GUILE_LIBS="-lrx $GUILE_LIBS")
		AC_CHECK_LIB(qt, main, GUILE_LIBS="-lqt $GUILE_LIBS")
		AC_CHECK_LIB(dl, dlopen, GUILE_LIBS="-ldl $GUILE_LIBS")
		AC_CHECK_LIB(nsl, t_accept, GUILE_LIBS="$GUILE_LIBS -lnsl")
		AC_CHECK_LIB(socket, socket, GUILE_LIBS="$GUILE_LIBS -lsocket")
		GUILE_LIBS="-lguile $GUILE_LIBS $QTTHREADS_LIB $READLINE_LIB $TERMCAP_LIB"
	fi

	AC_SUBST(GUILE_LIBS)
	AC_SUBST(GUILE_INCS)

	saved_LIBS="$LIBS"
	LIBS="$LIBS $GUILE_LIBS"
	CPPFLAGS="$saved_cppflags $GUILE_INCS"

	AC_MSG_CHECKING(whether guile works)
	AC_TRY_LINK([
		#include <libguile.h>
		#include <guile/gh.h>
	],[
		gh_eval_str("(newline)");
		scm_boot_guile(0,NULL,NULL,NULL);
	],[
		ac_cv_guile_found=yes
		AC_DEFINE(HAVE_GUILE)
	],[
		ac_cv_guile_found=no
	])
	AC_MSG_RESULT($ac_cv_guile_found)

	if test x$ac_cv_guile_found = xno ; then
		if test x$1 = xfail ; then
		  AC_MSG_ERROR(Can not find Guile on this system)
		else
		  AC_MSG_WARN(Can not find Guile on this system)
		fi
		ac_cv_guile_found=no
	fi

	LIBS="$saved_LIBS"
	LDFLAGS="$saved_ldflags"
	CPPFLAGS="$saved_cppflags"

	AC_SUBST(GUILE_LIBS)
	AM_CONDITIONAL(GUILE, test x$ac_cv_guile_found = xyes)
])

# Define a conditional.

AC_DEFUN(AM_CONDITIONAL,
[AC_SUBST($1_TRUE)
AC_SUBST($1_FALSE)
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi])

# Like AC_CONFIG_HEADER, but automatically create stamp file.

AC_DEFUN(AM_CONFIG_HEADER,
[AC_PREREQ([2.12])
AC_CONFIG_HEADER([$1])
dnl When config.status generates a header, we must update the stamp-h file.
dnl This file resides in the same directory as the config header
dnl that is generated.  We must strip everything past the first ":",
dnl and everything past the last "/".
AC_OUTPUT_COMMANDS(changequote(<<,>>)dnl
ifelse(patsubst(<<$1>>, <<[^ ]>>, <<>>), <<>>,
<<test -z "<<$>>CONFIG_HEADERS" || echo timestamp > patsubst(<<$1>>, <<^\([^:]*/\)?.*>>, <<\1>>)stamp-h<<>>dnl>>,
<<am_indx=1
for am_file in <<$1>>; do
  case " <<$>>CONFIG_HEADERS " in
  *" <<$>>am_file "*<<)>>
    echo timestamp > `echo <<$>>am_file | sed -e 's%:.*%%' -e 's%[^/]*$%%'`stamp-h$am_indx
    ;;
  esac
  am_indx=`expr "<<$>>am_indx" + 1`
done<<>>dnl>>)
changequote([,]))])

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN(AM_INIT_AUTOMAKE,
[AC_REQUIRE([AC_PROG_INSTALL])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
VERSION=[$2]
AC_SUBST(VERSION)
dnl test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" && test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi
ifelse([$3],,
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package]))
AC_REQUIRE([AM_SANITY_CHECK])
AC_REQUIRE([AC_ARG_PROGRAM])
dnl FIXME This is truly gross.
missing_dir=`cd $ac_aux_dir && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_REQUIRE([AC_PROG_MAKE_SET])])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN(AM_SANITY_CHECK,
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftestfile
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftestfile 2> /dev/null`
   if test "[$]*" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftestfile`
   fi
   if test "[$]*" != "X $srcdir/configure conftestfile" \
      && test "[$]*" != "X conftestfile $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "[$]2" = conftestfile
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

dnl AM_MISSING_PROG(NAME, PROGRAM, DIRECTORY)
dnl The program must properly implement --version.
AC_DEFUN(AM_MISSING_PROG,
[AC_MSG_CHECKING(for working $2)
# Run test in a subshell; some versions of sh will print an error if
# an executable is not found, even if stderr is redirected.
# Redirect stdin to placate older versions of autoconf.  Sigh.
if ($2 --version) < /dev/null > /dev/null 2>&1; then
   $1=$2
   AC_MSG_RESULT(found)
else
   $1="$3/missing $2"
   AC_MSG_RESULT(missing)
fi
AC_SUBST($1)])


# serial 40 AC_PROG_LIBTOOL
AC_DEFUN(AC_PROG_LIBTOOL,
[AC_REQUIRE([AC_LIBTOOL_SETUP])dnl

# Save cache, so that ltconfig can load it
AC_CACHE_SAVE

# Actually configure libtool.  ac_aux_dir is where install-sh is found.
CC="$CC" CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" \
LD="$LD" LDFLAGS="$LDFLAGS" LIBS="$LIBS" \
LN_S="$LN_S" NM="$NM" RANLIB="$RANLIB" \
DLLTOOL="$DLLTOOL" AS="$AS" OBJDUMP="$OBJDUMP" \
${CONFIG_SHELL-/bin/sh} $ac_aux_dir/ltconfig --no-reexec \
$libtool_flags --no-verify $ac_aux_dir/ltmain.sh $lt_target \
|| AC_MSG_ERROR([libtool configure failed])

# Reload cache, that may have been modified by ltconfig
AC_CACHE_LOAD

# This can be used to rebuild libtool when needed
LIBTOOL_DEPS="$ac_aux_dir/ltconfig $ac_aux_dir/ltmain.sh"

# Always use our own libtool.
LIBTOOL='$(SHELL) $(top_builddir)/libtool'
AC_SUBST(LIBTOOL)dnl

# Redirect the config.log output again, so that the ltconfig log is not
# clobbered by the next message.
exec 5>>./config.log
])

AC_DEFUN(AC_LIBTOOL_SETUP,
[AC_PREREQ(2.13)dnl
AC_REQUIRE([AC_ENABLE_SHARED])dnl
AC_REQUIRE([AC_ENABLE_STATIC])dnl
AC_REQUIRE([AC_ENABLE_FAST_INSTALL])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
AC_REQUIRE([AC_PROG_RANLIB])dnl
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_PROG_LD])dnl
AC_REQUIRE([AC_PROG_NM])dnl
AC_REQUIRE([AC_PROG_LN_S])dnl
dnl

case "$target" in
NONE) lt_target="$host" ;;
*) lt_target="$target" ;;
esac

# Check for any special flags to pass to ltconfig.
libtool_flags="--cache-file=$cache_file"
test "$enable_shared" = no && libtool_flags="$libtool_flags --disable-shared"
test "$enable_static" = no && libtool_flags="$libtool_flags --disable-static"
test "$enable_fast_install" = no && libtool_flags="$libtool_flags --disable-fast-install"
test "$ac_cv_prog_gcc" = yes && libtool_flags="$libtool_flags --with-gcc"
test "$ac_cv_prog_gnu_ld" = yes && libtool_flags="$libtool_flags --with-gnu-ld"
ifdef([AC_PROVIDE_AC_LIBTOOL_DLOPEN],
[libtool_flags="$libtool_flags --enable-dlopen"])
ifdef([AC_PROVIDE_AC_LIBTOOL_WIN32_DLL],
[libtool_flags="$libtool_flags --enable-win32-dll"])
AC_ARG_ENABLE(libtool-lock,
  [  --disable-libtool-lock  avoid locking (might break parallel builds)])
test "x$enable_libtool_lock" = xno && libtool_flags="$libtool_flags --disable-lock"
test x"$silent" = xyes && libtool_flags="$libtool_flags --silent"

# Some flags need to be propagated to the compiler or linker for good
# libtool support.
case "$lt_target" in
*-*-irix6*)
  # Find out which ABI we are using.
  echo '[#]line __oline__ "configure"' > conftest.$ac_ext
  if AC_TRY_EVAL(ac_compile); then
    case "`/usr/bin/file conftest.o`" in
    *32-bit*)
      LD="${LD-ld} -32"
      ;;
    *N32*)
      LD="${LD-ld} -n32"
      ;;
    *64-bit*)
      LD="${LD-ld} -64"
      ;;
    esac
  fi
  rm -rf conftest*
  ;;

*-*-sco3.2v5*)
  # On SCO OpenServer 5, we need -belf to get full-featured binaries.
  SAVE_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -belf"
  AC_CACHE_CHECK([whether the C compiler needs -belf], lt_cv_cc_needs_belf,
    [AC_TRY_LINK([],[],[lt_cv_cc_needs_belf=yes],[lt_cv_cc_needs_belf=no])])
  if test x"$lt_cv_cc_needs_belf" != x"yes"; then
    # this is probably gcc 2.8.0, egcs 1.0 or newer; no need for -belf
    CFLAGS="$SAVE_CFLAGS"
  fi
  ;;

ifdef([AC_PROVIDE_AC_LIBTOOL_WIN32_DLL],
[*-*-cygwin* | *-*-mingw*)
  AC_CHECK_TOOL(DLLTOOL, dlltool, false)
  AC_CHECK_TOOL(AS, as, false)
  AC_CHECK_TOOL(OBJDUMP, objdump, false)
  ;;
])
esac
])

# AC_LIBTOOL_DLOPEN - enable checks for dlopen support
AC_DEFUN(AC_LIBTOOL_DLOPEN, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])])

# AC_LIBTOOL_WIN32_DLL - declare package support for building win32 dll's
AC_DEFUN(AC_LIBTOOL_WIN32_DLL, [AC_BEFORE([$0], [AC_LIBTOOL_SETUP])])

# AC_ENABLE_SHARED - implement the --enable-shared flag
# Usage: AC_ENABLE_SHARED[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AC_ENABLE_SHARED, [dnl
define([AC_ENABLE_SHARED_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(shared,
changequote(<<, >>)dnl
<<  --enable-shared[=PKGS]  build shared libraries [default=>>AC_ENABLE_SHARED_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case "$enableval" in
yes) enable_shared=yes ;;
no) enable_shared=no ;;
*)
  enable_shared=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_shared=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_shared=AC_ENABLE_SHARED_DEFAULT)dnl
])

# AC_DISABLE_SHARED - set the default shared flag to --disable-shared
AC_DEFUN(AC_DISABLE_SHARED, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_SHARED(no)])

# AC_ENABLE_STATIC - implement the --enable-static flag
# Usage: AC_ENABLE_STATIC[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AC_ENABLE_STATIC, [dnl
define([AC_ENABLE_STATIC_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(static,
changequote(<<, >>)dnl
<<  --enable-static[=PKGS]  build static libraries [default=>>AC_ENABLE_STATIC_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case "$enableval" in
yes) enable_static=yes ;;
no) enable_static=no ;;
*)
  enable_static=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_static=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_static=AC_ENABLE_STATIC_DEFAULT)dnl
])

# AC_DISABLE_STATIC - set the default static flag to --disable-static
AC_DEFUN(AC_DISABLE_STATIC, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_STATIC(no)])


# AC_ENABLE_FAST_INSTALL - implement the --enable-fast-install flag
# Usage: AC_ENABLE_FAST_INSTALL[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AC_ENABLE_FAST_INSTALL, [dnl
define([AC_ENABLE_FAST_INSTALL_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(fast-install,
changequote(<<, >>)dnl
<<  --enable-fast-install[=PKGS]  optimize for fast installation [default=>>AC_ENABLE_FAST_INSTALL_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case "$enableval" in
yes) enable_fast_install=yes ;;
no) enable_fast_install=no ;;
*)
  enable_fast_install=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_fast_install=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_fast_install=AC_ENABLE_FAST_INSTALL_DEFAULT)dnl
])

# AC_ENABLE_FAST_INSTALL - set the default to --disable-fast-install
AC_DEFUN(AC_DISABLE_FAST_INSTALL, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_FAST_INSTALL(no)])

# AC_PROG_LD - find the path to the GNU or non-GNU linker
AC_DEFUN(AC_PROG_LD,
[AC_ARG_WITH(gnu-ld,
[  --with-gnu-ld           assume the C compiler uses GNU ld [default=no]],
test "$withval" = no || with_gnu_ld=yes, with_gnu_ld=no)
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
ac_prog=ld
if test "$ac_cv_prog_gcc" = yes; then
  # Check if gcc -print-prog-name=ld gives a path.
  AC_MSG_CHECKING([for ld used by GCC])
  ac_prog=`($CC -print-prog-name=ld) 2>&5`
  case "$ac_prog" in
    # Accept absolute paths.
changequote(,)dnl
    [\\/]* | [A-Za-z]:[\\/]*)
      re_direlt='/[^/][^/]*/\.\./'
changequote([,])dnl
      # Canonicalize the path of ld
      ac_prog=`echo $ac_prog| sed 's%\\\\%/%g'`
      while echo $ac_prog | grep "$re_direlt" > /dev/null 2>&1; do
	ac_prog=`echo $ac_prog| sed "s%$re_direlt%/%"`
      done
      test -z "$LD" && LD="$ac_prog"
      ;;
  "")
    # If it fails, then pretend we aren't using GCC.
    ac_prog=ld
    ;;
  *)
    # If it is relative, then search for the first ld in PATH.
    with_gnu_ld=unknown
    ;;
  esac
elif test "$with_gnu_ld" = yes; then
  AC_MSG_CHECKING([for GNU ld])
else
  AC_MSG_CHECKING([for non-GNU ld])
fi
AC_CACHE_VAL(ac_cv_path_LD,
[if test -z "$LD"; then
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}${PATH_SEPARATOR-:}"
  for ac_dir in $PATH; do
    test -z "$ac_dir" && ac_dir=.
    if test -f "$ac_dir/$ac_prog" || test -f "$ac_dir/$ac_prog$ac_exeext"; then
      ac_cv_path_LD="$ac_dir/$ac_prog"
      # Check to see if the program is GNU ld.  I'd rather use --version,
      # but apparently some GNU ld's only accept -v.
      # Break only if it was the GNU/non-GNU ld that we prefer.
      if "$ac_cv_path_LD" -v 2>&1 < /dev/null | egrep '(GNU|with BFD)' > /dev/null; then
	test "$with_gnu_ld" != no && break
      else
	test "$with_gnu_ld" != yes && break
      fi
    fi
  done
  IFS="$ac_save_ifs"
else
  ac_cv_path_LD="$LD" # Let the user override the test with a path.
fi])
LD="$ac_cv_path_LD"
if test -n "$LD"; then
  AC_MSG_RESULT($LD)
else
  AC_MSG_RESULT(no)
fi
test -z "$LD" && AC_MSG_ERROR([no acceptable ld found in \$PATH])
AC_PROG_LD_GNU
])

AC_DEFUN(AC_PROG_LD_GNU,
[AC_CACHE_CHECK([if the linker ($LD) is GNU ld], ac_cv_prog_gnu_ld,
[# I'd rather use --version here, but apparently some GNU ld's only accept -v.
if $LD -v 2>&1 </dev/null | egrep '(GNU|with BFD)' 1>&5; then
  ac_cv_prog_gnu_ld=yes
else
  ac_cv_prog_gnu_ld=no
fi])
])

# AC_PROG_NM - find the path to a BSD-compatible name lister
AC_DEFUN(AC_PROG_NM,
[AC_MSG_CHECKING([for BSD-compatible nm])
AC_CACHE_VAL(ac_cv_path_NM,
[if test -n "$NM"; then
  # Let the user override the test.
  ac_cv_path_NM="$NM"
else
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}${PATH_SEPARATOR-:}"
  for ac_dir in $PATH /usr/ccs/bin /usr/ucb /bin; do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/nm || test -f $ac_dir/nm$ac_exeext ; then
      # Check to see if the nm accepts a BSD-compat flag.
      # Adding the `sed 1q' prevents false positives on HP-UX, which says:
      #   nm: unknown option "B" ignored
      if ($ac_dir/nm -B /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
	ac_cv_path_NM="$ac_dir/nm -B"
	break
      elif ($ac_dir/nm -p /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
	ac_cv_path_NM="$ac_dir/nm -p"
	break
      else
	ac_cv_path_NM=${ac_cv_path_NM="$ac_dir/nm"} # keep the first match, but
	continue # so that we can try to find one that supports BSD flags
      fi
    fi
  done
  IFS="$ac_save_ifs"
  test -z "$ac_cv_path_NM" && ac_cv_path_NM=nm
fi])
NM="$ac_cv_path_NM"
AC_MSG_RESULT([$NM])
])

# AC_CHECK_LIBM - check for math library
AC_DEFUN(AC_CHECK_LIBM,
[AC_REQUIRE([AC_CANONICAL_HOST])dnl
LIBM=
case "$lt_target" in
*-*-beos* | *-*-cygwin*)
  # These system don't have libm
  ;;
*-ncr-sysv4.3*)
  AC_CHECK_LIB(mw, _mwvalidcheckl, LIBM="-lmw")
  AC_CHECK_LIB(m, main, LIBM="$LIBM -lm")
  ;;
*)
  AC_CHECK_LIB(m, main, LIBM="-lm")
  ;;
esac
])

# AC_LIBLTDL_CONVENIENCE[(dir)] - sets LIBLTDL to the link flags for
# the libltdl convenience library and INCLTDL to the include flags for
# the libltdl header and adds --enable-ltdl-convenience to the
# configure arguments.  Note that LIBLTDL and INCLTDL are not
# AC_SUBSTed, nor is AC_CONFIG_SUBDIRS called.  If DIR is not
# provided, it is assumed to be `libltdl'.  LIBLTDL will be prefixed
# with '${top_builddir}/' and INCLTDL will be prefixed with
# '${top_srcdir}/' (note the single quotes!).  If your package is not
# flat and you're not using automake, define top_builddir and
# top_srcdir appropriately in the Makefiles.
AC_DEFUN(AC_LIBLTDL_CONVENIENCE, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
  case "$enable_ltdl_convenience" in
  no) AC_MSG_ERROR([this package needs a convenience libltdl]) ;;
  "") enable_ltdl_convenience=yes
      ac_configure_args="$ac_configure_args --enable-ltdl-convenience" ;;
  esac
  LIBLTDL='${top_builddir}/'ifelse($#,1,[$1],['libltdl'])/libltdlc.la
  INCLTDL='-I${top_srcdir}/'ifelse($#,1,[$1],['libltdl'])
])

# AC_LIBLTDL_INSTALLABLE[(dir)] - sets LIBLTDL to the link flags for
# the libltdl installable library and INCLTDL to the include flags for
# the libltdl header and adds --enable-ltdl-install to the configure
# arguments.  Note that LIBLTDL and INCLTDL are not AC_SUBSTed, nor is
# AC_CONFIG_SUBDIRS called.  If DIR is not provided and an installed
# libltdl is not found, it is assumed to be `libltdl'.  LIBLTDL will
# be prefixed with '${top_builddir}/' and INCLTDL will be prefixed
# with '${top_srcdir}/' (note the single quotes!).  If your package is
# not flat and you're not using automake, define top_builddir and
# top_srcdir appropriately in the Makefiles.
# In the future, this macro may have to be called after AC_PROG_LIBTOOL.
AC_DEFUN(AC_LIBLTDL_INSTALLABLE, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
  AC_CHECK_LIB(ltdl, main,
  [test x"$enable_ltdl_install" != xyes && enable_ltdl_install=no],
  [if test x"$enable_ltdl_install" = xno; then
     AC_MSG_WARN([libltdl not installed, but installation disabled])
   else
     enable_ltdl_install=yes
   fi
  ])
  if test x"$enable_ltdl_install" = x"yes"; then
    ac_configure_args="$ac_configure_args --enable-ltdl-install"
    LIBLTDL='${top_builddir}/'ifelse($#,1,[$1],['libltdl'])/libltdl.la
    INCLTDL='-I${top_srcdir}/'ifelse($#,1,[$1],['libltdl'])
  else
    ac_configure_args="$ac_configure_args --enable-ltdl-install=no"
    LIBLTDL="-lltdl"
    INCLTDL=
  fi
])

dnl old names
AC_DEFUN(AM_PROG_LIBTOOL, [indir([AC_PROG_LIBTOOL])])dnl
AC_DEFUN(AM_ENABLE_SHARED, [indir([AC_ENABLE_SHARED], $@)])dnl
AC_DEFUN(AM_ENABLE_STATIC, [indir([AC_ENABLE_STATIC], $@)])dnl
AC_DEFUN(AM_DISABLE_SHARED, [indir([AC_DISABLE_SHARED], $@)])dnl
AC_DEFUN(AM_DISABLE_STATIC, [indir([AC_DISABLE_STATIC], $@)])dnl
AC_DEFUN(AM_PROG_LD, [indir([AC_PROG_LD])])dnl
AC_DEFUN(AM_PROG_NM, [indir([AC_PROG_NM])])dnl

dnl This is just to silence aclocal about the macro not being used
ifelse([AC_DISABLE_FAST_INSTALL])dnl

# Configure paths for GTK+
# Owen Taylor     97-11-3

dnl AM_PATH_GTK([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, MODULES]]]])
dnl Test for GTK, and define GTK_CFLAGS and GTK_LIBS
dnl
AC_DEFUN(AM_PATH_GTK,
[dnl 
dnl Get the cflags and libraries from the gtk-config script
dnl
AC_ARG_WITH(gtk-prefix,[  --with-gtk-prefix=PFX   Prefix where GTK is installed (optional)],
            gtk_config_prefix="$withval", gtk_config_prefix="")
AC_ARG_WITH(gtk-exec-prefix,[  --with-gtk-exec-prefix=PFX Exec prefix where GTK is installed (optional)],
            gtk_config_exec_prefix="$withval", gtk_config_exec_prefix="")
AC_ARG_ENABLE(gtktest, [  --disable-gtktest       Do not try to compile and run a test GTK program],
		    , enable_gtktest=yes)

  for module in . $4
  do
      case "$module" in
         gthread) 
             gtk_config_args="$gtk_config_args gthread"
         ;;
      esac
  done

  if test x$gtk_config_exec_prefix != x ; then
     gtk_config_args="$gtk_config_args --exec-prefix=$gtk_config_exec_prefix"
     if test x${GTK_CONFIG+set} != xset ; then
        GTK_CONFIG=$gtk_config_exec_prefix/bin/gtk-config
     fi
  fi
  if test x$gtk_config_prefix != x ; then
     gtk_config_args="$gtk_config_args --prefix=$gtk_config_prefix"
     if test x${GTK_CONFIG+set} != xset ; then
        GTK_CONFIG=$gtk_config_prefix/bin/gtk-config
     fi
  fi

  AC_PATH_PROG(GTK_CONFIG, gtk-config, no)
  min_gtk_version=ifelse([$1], ,0.99.7,$1)
  AC_MSG_CHECKING(for GTK - version >= $min_gtk_version)
  no_gtk=""
  if test "$GTK_CONFIG" = "no" ; then
    no_gtk=yes
  else
    GTK_CFLAGS=`$GTK_CONFIG $gtk_config_args --cflags`
    GTK_LIBS=`$GTK_CONFIG $gtk_config_args --libs`
    gtk_config_major_version=`$GTK_CONFIG $gtk_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
    gtk_config_minor_version=`$GTK_CONFIG $gtk_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
    gtk_config_micro_version=`$GTK_CONFIG $gtk_config_args --version | \
           sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`
    if test "x$enable_gtktest" = "xyes" ; then
      ac_save_CFLAGS="$CFLAGS"
      ac_save_LIBS="$LIBS"
      CFLAGS="$CFLAGS $GTK_CFLAGS"
      LIBS="$GTK_LIBS $LIBS"
dnl
dnl Now check if the installed GTK is sufficiently new. (Also sanity
dnl checks the results of gtk-config to some extent
dnl
      rm -f conf.gtktest
      AC_TRY_RUN([
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>

int 
main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.gtktest");

  /* HP/UX 9 (%@#!) writes to sscanf strings */
  tmp_version = g_strdup("$min_gtk_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string\n", "$min_gtk_version");
     exit(1);
   }

  if ((gtk_major_version != $gtk_config_major_version) ||
      (gtk_minor_version != $gtk_config_minor_version) ||
      (gtk_micro_version != $gtk_config_micro_version))
    {
      printf("\n*** 'gtk-config --version' returned %d.%d.%d, but GTK+ (%d.%d.%d)\n", 
             $gtk_config_major_version, $gtk_config_minor_version, $gtk_config_micro_version,
             gtk_major_version, gtk_minor_version, gtk_micro_version);
      printf ("*** was found! If gtk-config was correct, then it is best\n");
      printf ("*** to remove the old version of GTK+. You may also be able to fix the error\n");
      printf("*** by modifying your LD_LIBRARY_PATH enviroment variable, or by editing\n");
      printf("*** /etc/ld.so.conf. Make sure you have run ldconfig if that is\n");
      printf("*** required on your system.\n");
      printf("*** If gtk-config was wrong, set the environment variable GTK_CONFIG\n");
      printf("*** to point to the correct copy of gtk-config, and remove the file config.cache\n");
      printf("*** before re-running configure\n");
    } 
#if defined (GTK_MAJOR_VERSION) && defined (GTK_MINOR_VERSION) && defined (GTK_MICRO_VERSION)
  else if ((gtk_major_version != GTK_MAJOR_VERSION) ||
	   (gtk_minor_version != GTK_MINOR_VERSION) ||
           (gtk_micro_version != GTK_MICRO_VERSION))
    {
      printf("*** GTK+ header files (version %d.%d.%d) do not match\n",
	     GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
      printf("*** library (version %d.%d.%d)\n",
	     gtk_major_version, gtk_minor_version, gtk_micro_version);
    }
#endif /* defined (GTK_MAJOR_VERSION) ... */
  else
    {
      if ((gtk_major_version > major) ||
        ((gtk_major_version == major) && (gtk_minor_version > minor)) ||
        ((gtk_major_version == major) && (gtk_minor_version == minor) && (gtk_micro_version >= micro)))
      {
        return 0;
       }
     else
      {
        printf("\n*** An old version of GTK+ (%d.%d.%d) was found.\n",
               gtk_major_version, gtk_minor_version, gtk_micro_version);
        printf("*** You need a version of GTK+ newer than %d.%d.%d. The latest version of\n",
	       major, minor, micro);
        printf("*** GTK+ is always available from ftp://ftp.gtk.org.\n");
        printf("***\n");
        printf("*** If you have already installed a sufficiently new version, this error\n");
        printf("*** probably means that the wrong copy of the gtk-config shell script is\n");
        printf("*** being found. The easiest way to fix this is to remove the old version\n");
        printf("*** of GTK+, but you can also set the GTK_CONFIG environment to point to the\n");
        printf("*** correct copy of gtk-config. (In this case, you will have to\n");
        printf("*** modify your LD_LIBRARY_PATH enviroment variable, or edit /etc/ld.so.conf\n");
        printf("*** so that the correct libraries are found at run-time))\n");
      }
    }
  return 1;
}
],, no_gtk=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
       CFLAGS="$ac_save_CFLAGS"
       LIBS="$ac_save_LIBS"
     fi
  fi
  if test "x$no_gtk" = x ; then
     AC_MSG_RESULT(yes)
     ifelse([$2], , :, [$2])     
  else
     AC_MSG_RESULT(no)
     if test "$GTK_CONFIG" = "no" ; then
       echo "*** The gtk-config script installed by GTK could not be found"
       echo "*** If GTK was installed in PREFIX, make sure PREFIX/bin is in"
       echo "*** your path, or set the GTK_CONFIG environment variable to the"
       echo "*** full path to gtk-config."
     else
       if test -f conf.gtktest ; then
        :
       else
          echo "*** Could not run GTK test program, checking why..."
          CFLAGS="$CFLAGS $GTK_CFLAGS"
          LIBS="$LIBS $GTK_LIBS"
          AC_TRY_LINK([
#include <gtk/gtk.h>
#include <stdio.h>
],      [ return ((gtk_major_version) || (gtk_minor_version) || (gtk_micro_version)); ],
        [ echo "*** The test program compiled, but did not run. This usually means"
          echo "*** that the run-time linker is not finding GTK or finding the wrong"
          echo "*** version of GTK. If it is not finding GTK, you'll need to set your"
          echo "*** LD_LIBRARY_PATH environment variable, or edit /etc/ld.so.conf to point"
          echo "*** to the installed location  Also, make sure you have run ldconfig if that"
          echo "*** is required on your system"
	  echo "***"
          echo "*** If you have an old version installed, it is best to remove it, although"
          echo "*** you may also be able to get things to work by modifying LD_LIBRARY_PATH"
          echo "***"
          echo "*** If you have a RedHat 5.0 system, you should remove the GTK package that"
          echo "*** came with the system with the command"
          echo "***"
          echo "***    rpm --erase --nodeps gtk gtk-devel" ],
        [ echo "*** The test program failed to compile or link. See the file config.log for the"
          echo "*** exact error that occured. This usually means GTK was incorrectly installed"
          echo "*** or that you have moved GTK since it was installed. In the latter case, you"
          echo "*** may want to edit the gtk-config script: $GTK_CONFIG" ])
          CFLAGS="$ac_save_CFLAGS"
          LIBS="$ac_save_LIBS"
       fi
     fi
     GTK_CFLAGS=""
     GTK_LIBS=""
     ifelse([$3], , :, [$3])
  fi
  AC_SUBST(GTK_CFLAGS)
  AC_SUBST(GTK_LIBS)
  rm -f conf.gtktest
])

# gnome-common.m4
# 

dnl GNOME_COMMON_INIT

AC_DEFUN([GNOME_COMMON_INIT],
[
	AC_CACHE_VAL(ac_cv_gnome_aclocal_dir,
	[ac_cv_gnome_aclocal_dir="$GNOME_COMMON_MACROS_DIR"])
	AC_CACHE_VAL(ac_cv_gnome_aclocal_flags,
	[ac_cv_gnome_aclocal_flags="$ACLOCAL_FLAGS"])
	GNOME_ACLOCAL_DIR="$ac_cv_gnome_aclocal_dir"
	GNOME_ACLOCAL_FLAGS="$ac_cv_gnome_aclocal_flags"
	AC_SUBST(GNOME_ACLOCAL_DIR)
	AC_SUBST(GNOME_ACLOCAL_FLAGS)

	ACLOCAL="$ACLOCAL $GNOME_ACLOCAL_FLAGS"

	AM_CONDITIONAL(INSIDE_GNOME_DOCU, false)
])

AC_DEFUN([GNOME_GTKDOC_CHECK],
[
	AC_CHECK_PROG(GTKDOC, gtkdoc-mkdb, true, false)
	AM_CONDITIONAL(HAVE_GTK_DOC, $GTKDOC)
	AC_SUBST(HAVE_GTK_DOC)

	dnl Let people disable the gtk-doc stuff.
	AC_ARG_ENABLE(gtk-doc, [  --enable-gtk-doc  Use gtk-doc to build documentation [default=auto]], enable_gtk_doc="$enableval", enable_gtk_doc=auto)

	if test x$enable_gtk_doc = xauto ; then
	  if test x$GTKDOC = xtrue ; then
	    enable_gtk_doc=yes
	  else
	    enable_gtk_doc=no
	  fi
	fi

	dnl NOTE: We need to use a separate automake conditional for this
	dnl       to make this work with the tarballs.
	AM_CONDITIONAL(ENABLE_GTK_DOC, test x$enable_gtk_doc = xyes)
])

AC_DEFUN([GNOME_DEBUG_CHECK],
[
	AC_ARG_ENABLE(debug, [  --enable-debug turn on debugging [default=no]], enable_debug="$enableval", enable_debug=no)

	if test x$enable_debug = xyes ; then
	  AC_DEFINE(GNOME_ENABLE_DEBUG)
	fi
])

dnl
dnl GNOME_PLATFORM_GNOME_2(default, [force])
dnl
dnl   If the first parameter is `yes', then the default is
dnl   the GNOME 2.x platform, otherwise the GNOME 1.x one.
dnl
dnl   If the optional second parameter is `force', then use
dnl   the default value without command line argument.
dnl

AC_DEFUN([GNOME_PLATFORM_GNOME_2],[
	AC_REQUIRE([GNOME_REQUIRE_PKGCONFIG])

	if test x$1 = xyes ; then
	    platform_gnome_2_default=yes
	else
	    platform_gnome_2_default=no
	fi
	if test x$2 = xforce ; then
	    platform_gnome_2="$platform_gnome_2_default";
	else
	    AC_ARG_ENABLE(platform-gnome-2, [  --enable-platform-gnome-2 enable GNOME 2.x platform [default=no]],[platform_gnome_2="$enableval"],[platform_gnome_2="$platform_gnome_2_default"])
	fi

	AM_CONDITIONAL(PLATFORM_GNOME_2, test $platform_gnome_2 = yes)

	AC_MSG_CHECKING(for GNOME Platform)
	if test $platform_gnome_2 = yes; then
	    AC_MSG_RESULT(GNOME 2.x)
	    GNOME_INTERFACE_VERSION=2
	else
	    AC_MSG_RESULT(GNOME 1.x)
	    GNOME_INTERFACE_VERSION=1
	fi
	AC_SUBST(GNOME_INTERFACE_VERSION)
])

dnl GNOME_PKGCONFIG_CHECK_VERSION() extracts up to 6 decimal numbers out of given-version
dnl and required-version, using any non-number letters as delimiters. it then
dnl compares each of those 6 numbers in order 1..6 to each other, requirering
dnl all of the 6 given-version numbers to be greater than, or at least equal
dnl to the corresponding number of required-version.
dnl GNOME_PKGCONFIG_CHECK_VERSION(given-version, required-version [, match-action] [, else-action])
AC_DEFUN([GNOME_PKGCONFIG_CHECK_VERSION],[
[eval `echo "$1:0:0:0:0:0:0" | sed -e 's/^[^0-9]*//' -e 's/[^0-9]\+/:/g' \
 -e 's/\([^:]*\):\([^:]*\):\([^:]*\):\([^:]*\):\([^:]*\):.*/ac_v1=\1 ac_v2=\2 ac_v3=\3 ac_v4=\4 ac_v5=\5 ac_v6=\6/' \
`]
[eval `echo "$2:0:0:0:0:0:0" | sed -e 's/^[^0-9]*//' -e 's/[^0-9]\+/:/g' \
 -e 's/\([^:]*\):\([^:]*\):\([^:]*\):\([^:]*\):\([^:]*\):.*/ac_r1=\1 ac_r2=\2 ac_r3=\3 ac_r4=\4 ac_r5=\5 ac_r6=\6/' \
`]
ac_vm=[`expr \( $ac_v1 \> $ac_r1 \) \| \( \( $ac_v1 \= $ac_r1 \) \& \(          \
              \( $ac_v2 \> $ac_r2 \) \| \( \( $ac_v2 \= $ac_r2 \) \& \(         \
               \( $ac_v3 \> $ac_r3 \) \| \( \( $ac_v3 \= $ac_r3 \) \& \(        \
                \( $ac_v4 \> $ac_r4 \) \| \( \( $ac_v4 \= $ac_r4 \) \& \(       \
                 \( $ac_v5 \> $ac_r5 \) \| \( \( $ac_v5 \= $ac_r5 \) \& \(      \
                  \( $ac_v6 \>= $ac_r6 \)                                       \
                 \) \)  \
                \) \)   \
               \) \)    \
              \) \)     \
             \) \)      `]
case $ac_vm in
[1)]
        [$3]
        ;;
*[)]
        [$4]
        ;;
esac
])

dnl
dnl GNOME_CHECK_PKGCONFIG (script-if-enabled, [failflag])
dnl
AC_DEFUN([GNOME_CHECK_PKGCONFIG],[
	AC_PATH_PROG(PKG_CONFIG, pkg-config)
	have_pkgconfig=no
	if test -x "$PKG_CONFIG" ; then
	    have_pkgconfig=yes
	else
	    PKG_CONFIG=
	fi
	AC_MSG_CHECKING(for pkg-config)
	if test x$have_pkgconfig = xyes ; then
	    pkgconfig_required_version=0.4.1
	    pkgconfig_version=`pkg-config --version`
	    GNOME_PKGCONFIG_CHECK_VERSION($pkgconfig_version, $pkgconfig_required_version, [have_pkgconfig=yes], [have_pkgconfig=no])
	fi
	if test x$have_pkgconfig = xyes ; then
	    AC_MSG_RESULT(yes)
	else
	    PKG_CONFIG=
	    AC_MSG_RESULT(not found)
 	    if test x$2 = xfail; then
		AC_MSG_ERROR([
*** You need the latest pkg-config (at least $pkgconfig_required_version).
*** Get the latest version of pkg-config from
*** http://pkgconfig.sourceforce.net.])
	    fi
	fi
	AC_SUBST(PKG_CONFIG)

	AC_PROVIDE([GNOME_REQUIRE_PKGCONFIG])
])

dnl
dnl GNOME_REQUIRE_PKGCONFIG
dnl
AC_DEFUN([GNOME_REQUIRE_PKGCONFIG],[
	GNOME_CHECK_PKGCONFIG([], fail)
])

dnl Check if the C compiler accepts a certain C flag, and if so adds it to
dnl CFLAGS
AC_DEFUN([GNOME_PKGCONFIG_CHECK_CFLAG], [
  AC_REQUIRE([GNOME_REQUIRE_PKGCONFIG])

  AC_MSG_CHECKING(if C compiler accepts $1)
  save_CFLAGS="$CFLAGS"

  dnl make sure we add it only once
  dnl this one doesn't seem to work: *[\ \	]$1[\ \ ]*) ;;
  case " $CFLAGS " in
  *\ $1\ *) echo $ac_n "(already in CFLAGS) ... " ;;
  *\ $1\	*) echo $ac_n "(already in CFLAGS) ... " ;;
  *\	$1\ *) echo $ac_n "(already in CFLAGS) ... " ;;
  *\	$1\	*) echo $ac_n "(already in CFLAGS) ... " ;;
  *) CFLAGS="$CFLAGS $1" ;;
  esac

  AC_TRY_COMPILE([#include <stdio.h>], [printf("hello");],
	         [ AC_MSG_RESULT(yes)],dnl
	         [ CFLAGS="$save_CFLAGS" AC_MSG_RESULT(no) ])
])

dnl add $ACLOCAL_FLAGS (and optionally more dirs) to the aclocal
dnl commandline, so make can work even if it needs to rerun aclocal
AC_DEFUN([GNOME_PKGCONFIG_ACLOCALFLAGS],
[
  AC_REQUIRE([GNOME_REQUIRE_PKGCONFIG])

  test -n "$ACLOCAL_FLAGS" && ACLOCAL="$ACLOCAL $ACLOCAL_FLAGS"

  for i in "$1"; do
    ACLOCAL="$ACLOCAL -I $i"
  done
])

AC_DEFUN([GNOME_PKGCONFIG_CHECK_OPTIONAL_MODULES],
[
    AC_REQUIRE([GNOME_REQUIRE_PKGCONFIG])

    name=$1
    depvar=$3

    AC_MSG_CHECKING(for libraries)
    pkg_list=""
    for module in $2 ""; do
	if test -n "$module"; then
	    if `echo $module |grep -q ":"`; then
		dnl has version requirement
		pkg_module_name=`echo $module |sed 's/\(.*\):.*/\1/'`
		test_version=`echo $module |sed 's/.*:\(.*\)/\1/'`

		msg=`$PKG_CONFIG $pkg_module_name 2>&1`
		if test -z "$msg"; then
		    dnl module exists
		    pkg_version=`$PKG_CONFIG --modversion $pkg_module_name`
		    GNOME_PKGCONFIG_CHECK_VERSION($pkg_version, $test_version,
			dnl has the right version
			echo $ac_n "$pkg_module_name "
			pkg_list="$pkg_list $pkg_module_name"
		    ,
			AC_MSG_RESULT([($pkg_module_name)])
			if test x$4 = xfail ; then
				AC_MSG_ERROR([An old version of $pkg_module_name (version $pkg_version) was found. You need at least version $test_version])
			else
				AC_MSG_WARN([An old version of $pkg_module_name (version $pkg_version) was found. You need at least version $test_version])
			fi
		    )
		else
		    dnl doesn't exist
		    AC_MSG_RESULT([($pkg_module_name)])
		    if test x$4 = xfail ; then
			AC_MSG_ERROR([$msg])
		    else
			AC_MSG_WARN([$msg])
		    fi
		fi
	    else
		msg=`$PKG_CONFIG $module 2>&1`
		if test -z "$msg"; then
		    echo $ac_n "$module "
		    pkg_list="$pkg_list $module"
		else
		    AC_MSG_RESULT([($module)])
		    if test x$4 = xfail ; then
			AC_MSG_ERROR([$msg])
		    else
			AC_MSG_WARN([$msg])
		    fi
		fi
	    fi
	fi
    done
    AC_MSG_RESULT([])
    if test -n "$pkg_list"; then
	eval $name'_CFLAGS'=\"`$PKG_CONFIG --cflags $pkg_list`\"
	eval $name'_LIBS'=\"`$PKG_CONFIG --libs $pkg_list`\"
	if test -n "$depvar"; then
	    eval $depvar'_DEPENDS'=\"\$$depname'_DEPENDS' $pkg_list\"
	else
	    eval $name'_DEPENDS'=\"$pkg_list\"
	fi
	if test -z "$4" ; then
	    eval 'HAVE_'$name=yes
	fi
    fi
])

AC_DEFUN([GNOME_PKGCONFIG_CHECK_MODULES],
[
	GNOME_PKGCONFIG_CHECK_OPTIONAL_MODULES($1,$2,$3,fail)
])

