#!/bin/csh -f
setenv VTD_VERSION $1
setenv VTD_DISTRO_DIR  /astrogeo.org/vtd
cp /tmp/vtd-$VTD_VERSION.tar.bz2   $VTD_DISTRO_DIR  
#
cp -p doc/vtd_intro.html                   $VTD_DISTRO_DIR/
cp -p doc/vtd_intro.pdf                    $VTD_DISTRO_DIR/
cp -p doc/vtd_apriori.html                 $VTD_DISTRO_DIR/
cp -p doc/vtd_apriori.pdf                  $VTD_DISTRO_DIR/
cp -p doc/vtd_keywords.syn                 $VTD_DISTRO_DIR/
cp -p doc/vtd_keywords.pdf                 $VTD_DISTRO_DIR/
cp -p doc/vtd_keywords.html                $VTD_DISTRO_DIR/
cp -p doc/vtd_apriori_files_management.txt $VTD_DISTRO_DIR/
cp -p doc/dop_alg.pdf                      $VTD_DISTRO_DIR/
cp -p doc/vtd_alg.pdf                      $VTD_DISTRO_DIR/
cp -p README                               $VTD_DISTRO_DIR/README.txt
cp -p INSTALL                              $VTD_DISTRO_DIR/INSTALL.txt
cp -p /tmp/vtd-$VTD_VERSION.tar.bz2        $VTD_DISTRO_DIR/
