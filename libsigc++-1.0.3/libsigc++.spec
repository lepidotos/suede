%define  RELEASE 1
%define  rel     %{?CUSTOM_RELEASE} %{!?CUSTOM_RELEASE:%RELEASE}

%define lib_name sigc++

Name: libsigc++
Version: 1.0.2
Release: %rel

Summary: The Typesafe Signal Framework for C++
Copyright: LGPL
Group: System Environment/Libraries
Vendor: Karl E. Nelson <kenelson@ece.ucdavis.edu>
Packager: Dmitry V. Levin <ldv@fandra.org>
Url: http://libsigc.sourceforge.net/

Source: ftp://download.sourceforge.net/pub/sourceforge/libsigc/%name-%version.tar.gz

Prefix: %_prefix
BuildRoot: %_tmppath/%name-%version-root

%description
This library implements a full callback system for use in widget libraries,
abstract interfaces, and general programming. Originally part of the Gtk--
widget set, %name is now a seperate library to provide for more general
use. It is the most complete library of its kind with the ablity to connect
an abstract callback to a class method, function, or function object. It
contains adaptor classes for connection of dissimilar callbacks and has an
ease of use unmatched by other C++ callback libraries.

Package GTK-- (gtkmm), which is a C++ binding to the GTK+ library,
starting with version 1.1.2, uses %name.

%package devel
Summary: development tools for the Typesafe Signal Framework for C++ 
Group: Development/Libraries
Requires: %name = %version

%description devel
The %name-devel package contains the static libraries and header files
needed for development with %name.

%package examples
Summary: examples and tests for the Typesafe Signal Framework for C++ 
Group: Development/Libraries
Requires: %name-devel = %version

%description examples
The %name-devel package contains source code of example and test
programs for %name.

%prep
%setup -q
perl -pi -e 's|\${prefix}|%prefix|' README
perl -pi -e 's|PREFIX|%prefix|' doc/FAQ

%build
%ifarch alpha
	ARCH_FLAGS="--host=alpha-redhat-linux"
%endif

%global optflags %(opt="%optflags"; for f in -fno-exceptions -fno-rtti; do opt="$(echo "$opt"|sed -e "s/ $f//g;s/$f //g")"; done; echo "$opt")

if [ ! -x configure ]; then
	CFLAGS="$RPM_OPT_FLAGS" CXXFLAGS="$RPM_OPT_FLAGS" ./autogen.sh $ARCH_FLAGS --prefix=%prefix
fi
CFLAGS="$RPM_OPT_FLAGS" CXXFLAGS="$RPM_OPT_FLAGS" ./configure $ARCH_FLAGS --prefix=%prefix

if [ -n "$SMP" ]; then
	make -j$SMP "MAKE=make -j$SMP"
else
	make
fi

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p -m 755 $RPM_BUILD_ROOT%prefix/{{include,lib}/%lib_name}
make install INSTALL="%(which install) -p" prefix=$RPM_BUILD_ROOT%prefix

cp -a examples tests $RPM_BUILD_ROOT%prefix/lib/%lib_name
find $RPM_BUILD_ROOT%prefix/lib/%lib_name -type d -name .libs |xargs -r rm -rf
find $RPM_BUILD_ROOT%prefix/lib/%lib_name -type f |xargs file |
	grep -E '(relocatable|executable|shell script)' |cut -d: -f1 |xargs -r rm -f

for i in $RPM_BUILD_ROOT%prefix/lib/%lib_name/{examples,tests/*}/Makefile; do
	rm -f $i.*
	cp -p scripts/examples.Makefile $i
done
for i in $RPM_BUILD_ROOT%prefix/lib/%lib_name/tests/Makefile; do
	rm -f $i.*
	cp -p scripts/tests.Makefile $i
done

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root,755)
%attr(755,root,root) %prefix/lib/lib*.so.*
%doc AUTHORS COPYING.LIB README IDEAS NEWS ChangeLog TODO

%files devel
%defattr(-,root,root,755)
%prefix/bin/*
%prefix/include/*
%prefix/lib/%lib_name/include
%prefix/share/aclocal/*
%attr(755,root,root) %prefix/lib/lib*.so
%attr(644,root,root) %prefix/lib/*.*a
%doc doc/*

%files examples
%defattr(-,root,root,755)
%prefix/lib/%lib_name/examples
%prefix/lib/%lib_name/tests

%changelog
* Sat Apr 15 2000 Dmitry V. Levin <ldv@fandra.org>
- updated Url and Source fileds
- 1.0.0 stable release

* Sat Jan 22 2000 Dmitry V. Levin <ldv@fandra.org>
- filtering out -fno-rtti and -fno-exceptions options from $RPM_OPT_FLAGS
- minor install section cleanup

* Wed Jan 19 2000 Allan Rae <rae@lyx.org>
- autogen just creates configure, not runs it, so cleaned that up too.

* Wed Jan 19 2000 Dmitry V. Levin <ldv@fandra.org>
- minor attr fix
- removed unnecessary curly braces
- fixed Herbert's adjustement

* Sat Jan 15 2000 Dmitry V. Levin <ldv@fandra.org>
- minor package dependence fix

* Sat Dec 25 1999 Herbert Valerio Riedel <hvr@gnu.org>
- fixed typo of mine
- added traditional CUSTOM_RELEASE stuff
- added SMP support

* Thu Dec 23 1999 Herbert Valerio Riedel <hvr@gnu.org>
- adjusted spec file to get tests.Makefile and examples.Makefile from scripts/

* Fri Oct 22 1999 Dmitry V. Levin <ldv@fandra.org>
- split into three packages: %name, %name-devel and %name-examples

* Thu Aug 12 1999 Karl Nelson <kenelson@ece.ucdavis.edu>
- updated source field and merged conflicts between revisions.

* Tue Aug 10 1999 Dmitry V. Levin <ldv@fandra.org>
- updated Prefix and BuildRoot fields

* Thu Aug  5 1999 Herbert Valerio Riedel <hvr@hvrlab.dhs.org>
- made sure configure works on all alphas

* Wed Jul  7 1999 Karl Nelson <kenelson@ece.ucdavis.edu>
- Added autoconf macro for sigc.

* Fri Jun 11 1999 Karl Nelson <kenelson@ece.ucdavis.edu>
- Made into a .in to keep version field up to date 
- Still need to do release by hand

* Mon Jun  7 1999 Dmitry V. Levin <ldv@fandra.org>
- added Vendor and Packager fields

* Sat Jun  5 1999 Dmitry V. Levin <ldv@fandra.org>
- updated to 0.8.0

* Tue Jun  1 1999 Dmitry V. Levin <ldv@fandra.org>
- initial revision
