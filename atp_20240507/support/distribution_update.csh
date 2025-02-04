#!/bin/csh -f
setenv ATP_VERSION $1
setenv ATP_DISTRO_DIR  /astrogeo.org/atp
if ( -d $ATP_DISTRO_DIR ) then
     cp /tmp/atp-$ATP_VERSION.tar.bz2      $ATP_DISTRO_DIR
#
     cp -p doc/atp_configure.txt           $ATP_DISTRO_DIR/INSTALL.txt
     if ( -f README  ) cp -p README        $ATP_DISTRO_DIR/README.txt
     if ( -f CHANGES ) cp -p CHANGES       $ATP_DISTRO_DIR/Changes.txt
     cp -p /tmp/atp-$ATP_VERSION.tar.bz2   $ATP_DISTRO_DIR/
endif
