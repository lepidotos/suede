%define		localstatedir /var/lib

Name:		gnome_mime_data
Summary:	The GNOME virtual file-system libraries
Version:	1.0.1
Release:	1_cvs
License:	LGPL
Group:		System Environment/Libraries
Source:		ftp://ftp.gnome.org/pub/GNOME/stable/sources/%name/%name-%{version}.tar.gz
URL:		http://www.gnome.org/
BuildRoot:	/var/tmp/%{name}-root

%description
The GNOME MIME database contains a basic set of applications and MIME
types for a GNOME system.

%prep
%setup -q

%build

%ifarch alpha
    MYARCH_FLAGS="--host=alpha-redhat-linux"
%endif

# Needed for snapshot releases.
MYCFLAGS="$RPM_OPT_FLAGS"

if [ ! -f configure ]; then
    CFLAGS="$MYCFLAGS" ./autogen.sh $MYARCH_FLAGS \
	--enable-more-warnings --prefix=%{_prefix} \
	--localstatedir=%{localstatedir} --sysconfdir=%{_sysconfdir} \
	--mandir=%{_mandir} --libdir=%{_libdir} \
	--includedir=%{_includedir} --bindir=%{_bindir}
fi

CFLAGS="$MYCFLAGS" ./configure $MYARCH_FLAGS --enable-more-warnings \
    --prefix=%{_prefix} --localstatedir=%{localstatedir} \
    --sysconfdir=%{_sysconfdir} --mandir=%{_mandir} \
    --libdir=%{_libdir} --includedir=%{_includedir} \
    --bindir=%{_bindir} 

make

%install
rm -rf $RPM_BUILD_ROOT

make -k sysconfdir=$RPM_BUILD_ROOT%{_sysconfdir} \
    prefix=$RPM_BUILD_ROOT%{_prefix} mandir=$RPM_BUILD_ROOT%{_mandir} \
    libdir=$RPM_BUILD_ROOT%{_libdir} bindir=$RPM_BUILD_ROOT\%{_bindir} \
    includedir=$RPM_BUILD_ROOT%{_includedir} install

%find_lang %name

%clean
rm -rf $RPM_BUILD_ROOT

%files -f %{name}.lang
%defattr(-, root, root)

%doc AUTHORS COPYING ChangeLog NEWS README
%config %{_sysconfdir}/gnome-vfs-mime-magic
%dir %{_datadir}/application-registry
%{_datadir}/application-registry/gnome-vfs.applications
%{_datadir}/mime-info/*.keys
%{_datadir}/mime-info/*.mime
%{_libdir}/pkgconfig/*.pc

%changelog

* Sun Oct 21 2001 Gregory Leblanc <gleblanc@linuxweasel.com>
- some messing around with Requires: and BuildRequires
- cleaned up %files quite a bit (still not quite as good as it could be)
- removed a bunch of unnecessary %defines
