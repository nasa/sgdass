#!/bin/csh -f
setenv TLE_VERSION $1
setenv TLE_DISTRO_DIR  /astrogeo.org/tle
if ( -d $TLE_DISTRO_DIR ) then
     cp /tmp/tle-$TLE_VERSION.tar.bz2      $TLE_DISTRO_DIR
#
     cp -p doc/tle_configure.txt           $TLE_DISTRO_DIR/INSTALL.txt
     if ( -f README  ) cp -p README        $TLE_DISTRO_DIR/README.txt
     if ( -f CHANGES ) cp -p CHANGES       $TLE_DISTRO_DIR/Changes.txt
     cp -p /tmp/tle-$TLE_VERSION.tar.bz2   $TLE_DISTRO_DIR/
endif
