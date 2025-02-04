#!/bin/csh -f
setenv FOURPACK_VERSION             $1
setenv FOURPACK_DISTRO_DIR          /astrogeo.org/fourpack
cp /tmp/fourpack-$FOURPACK_VERSION.tar.bz2  $FOURPACK_DISTRO_DIR/
cp ./INSTALL                        $FOURPACK_DISTRO_DIR/Install.txt
cp doc/fourpack_user_guide.txt      $FOURPACK_DISTRO_DIR/
cp doc/fourpack_timing_results.txt  $FOURPACK_DISTRO_DIR/
#
