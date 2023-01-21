# Note that this is NOT a relocateable package

%define name            gnome-user-docs
%define ver             1.4.1
%define RELEASE         1
%define rel             %{?CUSTOM_RELEASE} %{!?CUSTOM_RELEASE:%RELEASE}
%define localstatedir   /var/lib


Summary: 	General GNOME User Documentation
Name:		%name
Version: 	%ver
Release: 	%RELEASE
Copyright: 	FDL
Distribution: 	GNOME RPMS
Source: 	%{name}-%{version}.tar.gz
URL:		http://developer.gnome.org/projects/gdp/
Group: 		Documentation
BuildArch: 	noarch
BuildRoot: 	/var/tmp/%{name}-%{ver}-root
BuildRequires:  scrollkeeper
Obsoletes:	gnome-users-guide

%description
This package contains general GNOME user documentation which is not 
directly associated with any particular GNOME application or package.

%prep
%setup
%build
./configure --prefix %{_prefix}	--datadir=%{_datadir} \
    --mandir=$RPM_BUILD_ROOT%{_mandir} \
    --sysconfdir=$RPM_BUILD_ROOT%{_sysconfdir} \
    --localstatedir=$RPM_BUILD_ROOT%{localstatedir}
make 

%install
rm -rf $RPM_BUILD_ROOT
make prefix=$RPM_BUILD_ROOT%{_prefix} \
    datadir=$RPM_BUILD_ROOT%{_datadir} \
    mandir=$RPM_BUILD_ROOT%{_mandir} \
    sysconfdir=$RPM_BUILD_ROOT%{_sysconfdir} \
    localstatedir=$RPM_BUILD_ROOT%{localstatedir} install


%clean 
rm -rf $RPM_BUILD_ROOT

%post
if which scrollkeeper-update>/dev/null 2>&1; then scrollkeeper-update; fi

%postun
if which scrollkeeper-update>/dev/null 2>&1; then scrollkeeper-update; fi

%files
%defattr(-, root, root)
%doc COPYING COPYING-DOCS AUTHORS README ChangeLog NEWS INSTALL
%{_datadir}/gnome/help
%{_datadir}/omf/%{name}

%changelog
* Tue Mar 20 2001 Gregory Leblanc <gleblanc@cu-portland.edu>
- use RPM defined macros, remove hard-coded paths, add build
  requirement for scrollkeeper
 
* Sun Mar 11 2001 Dan Mueth <dan@eazel.com>
- Update as we move to its own module

* Mon Nov 27 2000 Kenny Graunke <kwg@teleport.com>
- Initial cut
