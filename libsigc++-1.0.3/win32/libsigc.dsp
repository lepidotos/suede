# Microsoft Developer Studio Project File - Name="libsigc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=libsigc - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libsigc.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libsigc.mak" CFG="libsigc - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libsigc - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libsigc - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=xicl6.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libsigc - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSIGC_EXPORTS" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MT /GR /GX /O2 /I "." /I ".." /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" /D "LIBSIGC_EXPORTS" /D "WIN32" /FD /TP /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=xilink6.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386

!ELSEIF  "$(CFG)" == "libsigc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBSIGC_EXPORTS" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /MTd /Gm /GR /GX /ZI /Od /I "." /I ".." /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" /D "LIBSIGC_EXPORTS" /D "WIN32" /FD /GZ /c /Tp
# SUBTRACT CPP /X /YX /Yc /Yu
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=xilink6.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "libsigc - Win32 Release"
# Name "libsigc - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE="..\sigc++\adaptor.cc"
# End Source File
# Begin Source File

SOURCE="..\sigc++\basic_signal.cc"
# End Source File
# Begin Source File

SOURCE="..\sigc++\marshal.cc"
# End Source File
# Begin Source File

SOURCE="..\sigc++\object.cc"
# End Source File
# Begin Source File

SOURCE="..\sigc++\scope.cc"
# End Source File
# Begin Source File

SOURCE="..\sigc++\slot.cc"
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE="..\sigc++\adaptor.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\basic_signal.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\bind.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\class_slot.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\convert.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\func_slot.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\generator.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\handle.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\handle_system.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\marshal.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\object.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\object_slot.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\rettype.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\scope.h"
# End Source File
# Begin Source File

SOURCE=".\sigc++config.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\signal_system.h"
# End Source File
# Begin Source File

SOURCE="..\sigc++\slot.h"
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
