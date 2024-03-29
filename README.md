# Suede
Suede (SUEDE Unquestionably Existent Desktop Environment) is a desktop environment built around the Sawfish window manager, a fork of GNOME 1.4.1 bringing it up to modern standards. You can think of it as a complement to MATE, Trinity Desktop Environment, and KDE Restoration, or as a sort of GNOME 1.5. It will use some elements of GNOME 2.0/2.2 where appropriate, and a modern version of Sawfish, but it will try to keep faithful to the original GNOME codebase where it can get away with it.

The S has never meant anything in specific, but if you don't want to go the MIT route, it can stand for "Sawfish-based,".

# Building Suede
Suede is as of right now experimental, and the repo is a mess, so for now you may as well just build GNOME 1.4.1 -- you wouldn't be more than a few lines of code (mostly stuff like pointing imglib to specifically libpng12 rather than libpng) in the whole codebase off at this point, in any case. This repo should contain an unmodified copy of GNOME 1.4.1, unless I've replaced the config.sub in any of the components.

All repositories (ORBit is tricky as it itself contains two repositories) need to have their config.sub replaced, and many are out of date (well, obviously -- what I mean is that there are 2002 or 2003-era versions of many components that will work with GNOME 1.4.1), but it gets you to a level starting point.

# Planned (or not-so-planned) Features
Wallpaper options -- zoom-to-fit, manual positioning.

Fractional scaling isn't really on the radar yet, but integer scaling will eventually be an option for high DPI displays. It'll always default to 1x for as long as this sentence is in this file (all my displays are 1600x1200 or lower), but that's always subject to the whims of time.

New themes are going to happen eventually as a separate repository, but Suede itself will always default to Crux as the basic theme. On Fedora and Red Hat-like systems, it will default to Bluecurve if it's installed before Suede itself, and afterwards openSUSE, Slackware, Mageia/PCLinuxOS, and Debian/Ubuntu will all get themes of their own. There will eventually be Qt 5 and GTK3 themes replicating GTK+, as well.

Definitely on the radar but not the very first priority is to try to refactor it so that GCC doesn't scream at you.

Wayland, at some point. Once it's better supported on ppc64[le|el|be|eb].

Re-basing around GNOME 2.0 if it proves easier to work with. Not 2.x, 2.0 (and some elements of 2.2) specifically, as 2.0 was generally speaking a straightforward port of 1.4 to GTK+ 2, which modern Sawfish also uses.
