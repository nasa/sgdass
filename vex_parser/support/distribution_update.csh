#!/bin/csh -f
setenv VXP_VERSION $1
setenv VXP_DISTRO_DIR  /astrogeo.org/vex_parser
if ( -d $VXP_DISTRO_DIR ) then
     cp /tmp/vex_parser-$VXP_VERSION.tar.bz2   $VXP_DISTRO_DIR
#
     cp -p doc/vex_parser_configure.txt    $VXP_DISTRO_DIR/INSTALL.txt
     if ( -f README  ) cp -p README        $VXP_DISTRO_DIR/README.txt
     if ( -f CHANGES ) cp -p CHANGES       $VXP_DISTRO_DIR/Changes.txt
     cp -p /tmp/vex_parser-$VXP_VERSION.tar.bz2   $VXP_DISTRO_DIR/
endif
