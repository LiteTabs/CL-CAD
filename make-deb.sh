#!/bin/bash

case `uname -m` in
    x86_64)
	ARCH=amd64
	;;
    i686)
	ARCH=i386
	;;
    *)
	echo 'Unknown architecture'
	exit 1
esac

while read line
do
    case "$line" in
	*\*ps-version\**)
	    VERSION=${line#*\"}
	    VERSION=${VERSION%%\"*}
	;;
    esac
done < cl-cad.version.lisp
if [ -z "$VERSION" ]
then
    echo "Can't parse version"
    exit
fi

DATE=`date -R`

PREFIX=deb

rm -rf $PREFIX
mkdir -p $PREFIX/usr/bin
cp cl-cad $PREFIX/usr/bin/
cp -aR share $PREFIX/usr/

mkdir -p $PREFIX/usr/share/applications/
echo "[Desktop Entry]"                       > $PREFIX/usr/share/applications/cl-cad.desktop
echo "Version=$VERSION"                     >> $PREFIX/usr/share/applications/cl-cad.desktop
echo "Name=cl-cad"                          >> $PREFIX/usr/share/applications/cl-cad.desktop
echo "GenericName=cl-cad"                   >> $PREFIX/usr/share/applications/cl-cad.desktop
echo "Comment=Simple CAD program"           >> $PREFIX/usr/share/applications/cl-cad.desktop
echo "Exec=cl-cad"                          >> $PREFIX/usr/share/applications/cl-cad.desktop
echo "Icon=cl-cad"                          >> $PREFIX/usr/share/applications/cl-cad.desktop
echo "Terminal=false"                       >> $PREFIX/usr/share/applications/cl-cad.desktop
echo "Type=Application"                     >> $PREFIX/usr/share/applications/cl-cad.desktop
echo "Categories=Utility;"                  >> $PREFIX/usr/share/applications/cl-cad.desktop
echo "StartupNotify=true"                   >> $PREFIX/usr/share/applications/cl-cad.desktop
echo "MimeType=application/x-revelation;"   >> $PREFIX/usr/share/applications/cl-cad.desktop

D=DEBIAN
mkdir -p $PREFIX/$D

echo "Package: cl-cad"                                 > $PREFIX/$D/control
echo "Version: $VERSION"                               >> $PREFIX/$D/control
echo "Maintainer: Burdukov Denis <litetabs@gmail.com>" >> $PREFIX/$D/control
echo "Architecture: $ARCH"                             >> $PREFIX/$D/control
echo "Section: misc"                                   >> $PREFIX/$D/control
echo "Priority: optional"                              >> $PREFIX/$D/control
echo "Description: Simple CAD program"                 >> $PREFIX/$D/control
echo "Depends: libc6, libgtk2.0-0 (>= 2.18)"           >> $PREFIX/$D/control

echo "cl-cad ($VERSION) stable; urgency=medium"     > $PREFIX/usr/share/doc/cl-cad/changelog
echo ""                                             >> $PREFIX/usr/share/doc/cl-cad/changelog
echo "* dummy."                                     >> $PREFIX/usr/share/doc/cl-cad/changelog
echo ""                                             >> $PREFIX/usr/share/doc/cl-cad/changelog
echo "-- Burdukov Denis <litetabs@gmail.com> $DATE" >> $PREFIX/usr/share/doc/cl-cad/changelog
echo ""                                             >> $PREFIX/usr/share/doc/cl-cad/changelog

gzip -9 $PREFIX/usr/share/doc/cl-cad/changelog

fakeroot dpkg-deb -Zbzip2 -z9 --build $PREFIX .
#  -z9 -Zxz
rm -rf $PREFIX

