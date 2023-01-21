# Note that this is NOT a relocatable package
%define name     gnome-pim
%define ver      1.4.0
%define  RELEASE 1
%define  rel     %{?CUSTOM_RELEASE} %{!?CUSTOM_RELEASE:%RELEASE}
%define prefix   /usr

Summary: GNOME Personal Information Manager
Name: 		%name
Version: 	%ver
Release: 	%rel
Copyright: 	GPL
Group: 		Applications/Productivity
Source: ftp://ftp.gnome.org/pub/GNOME/unstable/sources/%{name}/%{name}-%{ver}.tar.gz
BuildRoot: 	/var/tmp/gnome-pim-%{PACKAGE_VERSION}
Obsoletes: 	%name
URL: 		http://www.gnome.org
Docdir: 	%{prefix}/doc
Requires: 	gnome-libs >= 1.0.0
Requires: 	ORBit >= 0.4.0

%description
GNOME (GNU Network Object Model Environment) is a user-friendly set of
applications and desktop tools to be used in conjunction with a window
manager for the X Window System.  GNOME is similar in purpose and scope
to CDE and KDE, but GNOME is based completely on free software.
The GNOME Personal Information Manager consists of applications to make
keeping up with your busy life easier.

Currently these apps are present:

 - gnomecal :  personal calendar and todo list
 - gnomecard:  contact list of friends and business associates




You should install the gnome-pim package if you would like to bring some
order to your life. You will also need to install the gnome-libs and ORBit
packages. If you would like to develop addtional applications for the 
Personal Information Manager suite you will need to install the 
gnome-pim-devel package.

%package devel
Summary: Libraries and include files for developing gnome-pim applications.
Group : 	Development/Libraries
Requires: 	%name = %{PACKAGE_VERSION}
Obsoletes: 	%name-devel

%description devel 
GNOME (GNU Network Object Model Environment) is a user-friendly set of
applications and desktop tools to be used in conjunction with a window
manager for the X Window System.  GNOME is similar in purpose and scope
to CDE and KDE, but GNOME is based completely on free software.
The gnome-pim-devel package includes the libraries and include files that
you will need to develop addtional gnome-pim applications.

Currently these apps are present:

 - gnomecal :  personal calendar and todo list
 - gnomecard:  contact list of friends and business associates

You should install the gnome-pim package if you would like to bring some
order to your life. You will also need to install the gnome-libs and ORBit
packages. If you would like to develop addtional applications for the 
Personal Information Manager suite you will need to install the 
gnome-pim-devel package.

%package conduits
Summary: Gnome Pilot conduits for GnomeCal and GnomeCard
Group : 	Applications/Productivity
Requires: 	%name = %{PACKAGE_VERSION}
Requires: 	gnome-pilot >= 0.1.50
Obsoletes: 	%name-conduits

%description conduits
GNOME (GNU Network Object Model Environment) is a user-friendly set of
applications and desktop tools to be used in conjunction with a window
manager for the X Window System.  GNOME is similar in purpose and scope
to CDE and KDE, but GNOME is based completely on free software.
The gnome-pim-conduits package includes the conduits needed to connect
your PalmPilot with gnome-pim applications.

Currently these conduits are present:

 - gnomecal :  synchronizes your GnomeCal calendar with your Palm's calendar
 - gnomecard:  synchronizes your contact list

%changelog
* Sun Dec 5 1999 Eskil Olsen <deity@eskil.dk>
- Updated to 1.1.2
- Created a subpackage for the conduits

* Fri Jul 30 1999 Gregory McLean <gregm@comstar.net>
- Updated to 1.0.9
- Updated the descriptions
- Cleaned up the spec.

* Fri Feb 26 1999 Gregory McLean <gregm@comstar.net>

- Fixed the devel section, updated to 1.0.0

* Mon Dec 14 1998 Michael Fulbright <drmike@redhat.com>

- first try at an RPM for the 0.99.0 release

%prep
%setup -q

%build
# Needed for snapshot releases.
%ifarch alpha
  ARCH_FLAGS="--host=alpha-redhat-linux"
%endif

if [ ! -f configure ]; then
  CFLAGS="$RPM_OPT_FLAGS" ./autogen.sh --quiet $ARCH_FLAGS --prefix=%prefix --sysconfdir=/etc
else
  CFLAGS="$RPM_OPT_FLAGS" ./configure --quiet $ARCH_FLAGS --prefix=%prefix --sysconfdir=/etc
fi

if [ "$SMP" != "" ]; then
  make -j$SMP "MAKE=make -j $SMP"
else
  make
fi

%install
rm -rf $RPM_BUILD_ROOT

make prefix=$RPM_BUILD_ROOT%{prefix} sysconfdir=$RPM_BUILD_ROOT/etc install > install.log 2>&1

%clean
rm -rf $RPM_BUILD_ROOT

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%defattr(-, root, root)

%doc AUTHORS COPYING ChangeLog NEWS README
%{prefix}/bin/gnomecal
%{prefix}/bin/gnomecard
%config /etc/CORBA/servers/*
%{prefix}/share/gnome/help/*
%{prefix}/share/gnome/apps/Applications/*
%{prefix}/share/pixmaps/gnome-gnomecard.png
%config %{prefix}/share/mime-info/*

%files devel
%defattr(-, root, root)

%{prefix}/share/idl/*.idl

%files conduits
%{prefix}/bin/calendar-pilot-sync
%{prefix}/bin/calendar-conduit-control-applet
%{prefix}/bin/address-conduit-capplet
%{prefix}/lib/*
%{prefix}/share/control-center/*
%{prefix}/share/gnome-pilot/conduits/*
%{prefix}/share/pixmaps/gnome-calendar-conduit.png
