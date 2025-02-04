#!/bin/csh -f
setenv PIMA_VERSION                 $1
setenv PIMA_DISTRO_DIR              /astrogeo.org/pima
cp /tmp/pima-$PIMA_VERSION.tar.bz2  $PIMA_DISTRO_DIR/
cp ./Changelog.txt                  $PIMA_DISTRO_DIR/Changelog_${PIMA_VERSION}.txt
cp ./INSTALL                        $PIMA_DISTRO_DIR/Install_${PIMA_VERSION}.txt
cp doc/pima_keywords.html           $PIMA_DISTRO_DIR/pima_keywords.html
cp doc/pima_keywords.pdf            $PIMA_DISTRO_DIR/pima_keywords.pdf
cp doc/pima_user_guide.html         $PIMA_DISTRO_DIR/pima_user_guide.html
cp doc/pima_user_guide.pdf          $PIMA_DISTRO_DIR/pima_user_guide.pdf
cp doc/pima_wrapper.html            $PIMA_DISTRO_DIR/pima_wrapper.html
cp doc/pima_wrapper.pdf             $PIMA_DISTRO_DIR/pima_wrapper.pdf
cp doc/pima_amp.pdf                 $PIMA_DISTRO_DIR/pima_amp.pdf
#
