#!/bin/bash

URL='http://litetabs.github.com/CL-CAD'
VERSION=`date +0.%-y.%-m.%-d`

PREFIX=C:\\projects\\cl-cad

cd windows
mingw32-make
cd ..

rm -rf win
mkdir -p win/cl-cad
cp cl-cad.exe win/cl-cad
cp windows/runner.exe win/cl-cad
cp -aR share win/cl-cad
cp -aR ../gtk-redist/* win/cl-cad

echo "[Setup]" > win/setup.iss
echo "AppName=cl-cad" >> win/setup.iss
echo "AppVerName=cl-cad $VERSION" >> win/setup.iss
echo "AppPublisher=Burdukov Denis" >> win/setup.iss
echo "AppPublisherURL=$URL" >> win/setup.iss
echo "AppSupportURL=$URL" >> win/setup.iss
echo "AppUpdatesURL=$URL" >> win/setup.iss
echo "DefaultDirName={pf}\cl-cad" >> win/setup.iss
echo "DisableDirPage=yes" >> win/setup.iss
echo "DisableProgramGroupPage=yes" >> win/setup.iss
echo "DisableReadyPage=yes" >> win/setup.iss
echo "DefaultGroupName=cl-cad" >> win/setup.iss
echo "OutputDir=$PREFIX" >> win/setup.iss
echo "OutputBaseFilename=cl-cad-$VERSION" >> win/setup.iss
echo "Compression=lzma" >> win/setup.iss
echo "SolidCompression=yes" >> win/setup.iss
echo "WizardImageFile=$PREFIX\windows\setup-bg.bmp" >> win/setup.iss
echo "[Languages]" >> win/setup.iss
echo "Name: \"english\"; MessagesFile: \"compiler:Default.isl\"" >> win/setup.iss
echo "Name: \"russian\"; MessagesFile: \"compiler:Languages\Russian.isl\"" >> win/setup.iss
echo "[Tasks]" >> win/setup.iss
echo "Name: \"desktopicon\"; Description: \"{cm:CreateDesktopIcon}\"; GroupDescription: \"{cm:AdditionalIcons}\"; Flags: unchecked" >> win/setup.iss
echo "Name: \"quicklaunchicon\"; Description: \"{cm:CreateQuickLaunchIcon}\"; GroupDescription: \"{cm:AdditionalIcons}\"; Flags: unchecked" >> win/setup.iss
echo "[Files]" >> win/setup.iss
echo "Source: \"$PREFIX\win\cl-cad\*\"; DestDir: \"{app}\"; Flags: ignoreversion recursesubdirs createallsubdirs" >> win/setup.iss
echo "[Icons]" >> win/setup.iss
echo "Name: \"{userprograms}\cl-cad\"; Filename: \"{app}\runner.exe\"; WorkingDir: \"{app}\"" >> win/setup.iss
echo "Name: \"{commondesktop}\cl-cad\"; Filename: \"{app}\runner.exe\"; Tasks: desktopicon; WorkingDir: \"{app}\"" >> win/setup.iss
echo "Name: \"{userappdata}\Microsoft\Internet Explorer\Quick Launch\cl-cad\"; Filename: \"{app}\runner.exe\"; Tasks: quicklaunchicon; WorkingDir: \"{app}\"" >> win/setup.iss
echo "[Run]" >> win/setup.iss
echo "Filename: \"{app}\runner.exe\"; Description: \"{cm:LaunchProgram,PassStorage}\"; Flags: nowait postinstall skipifsilent; WorkingDir: \"{app}\"" >> win/setup.iss

"/C/Program Files/Inno Setup 5/ISCC.exe" win/setup.iss
rm -rf win
