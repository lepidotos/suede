# Note that this is NOT a relocatable package
%define ver      	1.4.0.2
%define RELEASE 	1
%define rel     	%{?CUSTOM_RELEASE} %{!?CUSTOM_RELEASE:%RELEASE}
%define localstatedir	/var/lib

Summary: 	GNOME games.
Name: 		gnome-games
Version: 	%ver
Release: 	%rel
License: 	GPL
Group: 		Amusements/Games
Source:		ftp://ftp.gnome.org/pub/GNOME/stable/sources/%{name}/%{name}-%{version}.tar.gz
BuildRoot: 	%{_tmppath}/%{name}-%{version}-root
URL: 		http://www.gnome.org
Requires:	gnome-libs >= 1.0.60
BuildRequires:  gnome-libs-devel >= 1.0.60
BuildRequires:  guile-devel

%description
GNOME (GNU Network Object Model Environment) is a user-friendly set of
applications and desktop tools to be used in conjunction with a window
manager for the X Window System.  GNOME is similar in purpose and scope
to CDE and KDE, but GNOME is based completely on free software.
The gnome-games package containes a collection of simple games for your
amusement.

You should install the gnome-games package if you would like to play the 
included games. You will also need to install the gnome-libs package.
If you would like to develop addtional games that utilize the GNOME
games libraries then you should install the gnome-games-devel package.

%package devel
Summary:	GNOME games development libraries.
Group: 		Development/Libraries
Requires:	%{name} = %{version}

%description devel
GNOME (GNU Network Object Model Environment) is a user-friendly set of
applications and desktop tools to be used in conjunction with a window
manager for the X Window System.  GNOME is similar in purpose and scope
to CDE and KDE, but GNOME is based completely on free software.
The gnome-games-devel package contains the libraries and include files
needed for development of GNOME games.

You should install the gnome-games package if you would like to play the
included games. You will also need to install the gnome-libs package.
If you would like to develop addtional games that utilize the GNOME
games libraries then you should install the gnome-games-devel package.

%prep
%setup -q

%build
%ifarch alpha
  MYARCH_FLAGS="--host=alpha-redhat-linux"
%endif

CFLAGS="$RPM_OPT_FLAGS" ./configure --quiet $MYARCH_FLAGS \
    --prefix=%{_prefix} --localstatedir=%{localstatedir} \
    --sysconfdir=%{_sysconfdir} --libdir=%{_libdir} \
    --bindir=%{_bindir} --datadir=%{_datadir} \
    --includedir=%{_includedir}

if [ "$SMP" != "" ]; then
  make -j$SMP MAKE="make -j$SMP"
else
  make
fi

%install
rm -rf $RPM_BUILD_ROOT

make scoredir=$RPM_BUILD_ROOT%{localstatedir}/games \
    prefix=$RPM_BUILD_ROOT%{_prefix} \
    localstatedir=$RPM_BUILD_ROOT%{localstatedir} \
    sysconfdir=$RPM_BUILD_ROOT%{_sysconfdir} \
    libdir=$RPM_BUILD_ROOT%{_libdir} bindir=$RPM_BUILD_ROOT%{_bindir} \
    datadir=$RPM_BUILD_ROOT%{_datadir} \
    includedir=$RPM_BUILD_ROOT%{_includedir} install >install.log 2>&1

%find_lang %{name}

%clean
rm -rf $RPM_BUILD_ROOT

%post 
if ! grep %{_libdir} /etc/ld.so.conf > /dev/null ; then
  echo "%{_libdir}" >> /etc/ld.so.conf
fi

/sbin/ldconfig

%postun -p /sbin/ldconfig



%files -f %{name}.lang
%defattr(-, root, root)

%doc AUTHORS COPYING ChangeLog NEWS README

%config(noreplace) %{_sysconfdir}/sound/events/*
%{_datadir}/gnome/apps/Games/*
%{_datadir}/gnibbles/*
%{_datadir}/gnobots2/*
%{_datadir}/gnome/help/*
%{_datadir}/gnome-stones/*
%{_datadir}/gturing/*
%{_datadir}/pixmaps/*
%{_datadir}/sol-games/*
%{_datadir}/sounds/*
%{_datadir}/xbill/*
%{_libdir}/lib*.so.*
%{_libdir}/gnome-stones/objects/lib*.so.*

%defattr(-, games, games)
%{localstatedir}/games/*

%defattr (-, root,games)
%{_bindir}/GnomeScott
%{_bindir}/ctali
%{_bindir}/freecell
%{_bindir}/gataxx
%{_bindir}/sol

%defattr (2555, root, games)
%{_bindir}/glines
%{_bindir}/gnibbles
%{_bindir}/gnobots2
%{_bindir}/gnome-stones
%{_bindir}/gnome-xbill
%{_bindir}/gnometris
%{_bindir}/gnomine
%{_bindir}/gnotravex  
%{_bindir}/gnotski
%{_bindir}/gtali
%{_bindir}/gturing
%{_bindir}/iagno
%{_bindir}/mahjongg
%{_bindir}/same-gnome

%files devel
%defattr(-, root, root)

%{_libdir}/lib*.so
%{_libdir}/*a
%{_libdir}/gnome-stones/objects/lib*.so
%{_libdir}/gnome-stones/objects/lib*a
%{_includedir}/*

%changelog
* Sun Jun 24 2001  <gleblanc@peecee.linuxweasel.com>
- removed unnecessary %defines

* Tue May 22 2001 Gregory Leblanc <gleblanc@localhost.localdomain>
- changed permissions on binaries so that they will actually run
- replaced copyright with License

* Thu Feb 22 2001 Gregory Leblanc <gleblanc@cu-portland.edu>
- fixed hard-coded paths, and improved macros

* Tue Sep 28 1999 Ian Peters <itp@gnu.org>
- Merged some changes from Dax Kelson <dax@gurulabs.com> to get this thing to
  work right.

* Sun Aug 01 1999 Gregory McLean <gregm@comstar.net>
- Updated with better descriptions and more consistant layout.

* Sat Nov 21 1998 Michael Fulbright <drmike@redhat.com>
- updated for 0.30 tree

* Fri Nov 20 1998 Pablo Saratxaga <srtxg@chanae.alphanet.ch>
- use --localstatedir=/var/lib in config state (score files for games
  for exemple will go there).

* Mon Mar 16 1998 Marc Ewing <marc@redhat.com>
- Integrate into gnome-games CVS source tree
