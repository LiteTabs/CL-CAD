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

DATE=`date -R`
#VERSION=`date +0.%-y.%-m.%-d`
VERSION=`date +0.1`
PREFIX=deb/cl-cad

#rm -rf deb
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

mkdir -p $PREFIX/DEBIAN

echo "Package: cl-cad"                                 > $PREFIX/DEBIAN/control
echo "Version: $VERSION"                               >> $PREFIX/DEBIAN/control
echo "Maintainer: Burdukov Denis <litetabs@gmail.com>" >> $PREFIX/DEBIAN/control
echo "Architecture: $ARCH"                             >> $PREFIX/DEBIAN/control
echo "Description: Simple CAD program"                 >> $PREFIX/DEBIAN/control
echo "Depends: libgtk2.0-0 (>= 2.16)"                  >> $PREFIX/DEBIAN/control

echo "cl-cad ($VERSION) stable; urgency=medium"     > $PREFIX/DEBIAN/changelog
echo ""                                             >> $PREFIX/DEBIAN/changelog
echo "* dummy."                                     >> $PREFIX/DEBIAN/changelog
echo ""                                             >> $PREFIX/DEBIAN/changelog
echo "-- Burdukov Denis <litetabs@gmail.com> $DATE" >> $PREFIX/DEBIAN/changelog

chown -R root:root $PREFIX/

cd deb
dpkg-deb -z9 -Zlzma --build cl-cad
cd ..
cp deb/cl-cad.deb cl-cad_${VERSION}_${ARCH}.deb
rm -rf deb

