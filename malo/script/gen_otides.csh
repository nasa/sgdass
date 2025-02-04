#!/bin/csh
# ************************************************************************
# *                                                                      *
# *   Program gen_eqt generates admittance for an equillibrim tide.      *
# *                                                                      *
# *  ### 15-JAN-2015    gen_eqt   2.0   (c)  L. Petrov  10-JUN-2017 ###  *
# *                                                                      *
# ************************************************************************
#
set res = 2699
#
#$MALO_DIR/bin_static/gen_otide got /imls/orig_data/got410c/grids_oceantide \
#                                   /imls/devel_model/mod44w_d${res}_dls_sea_6cells.heb \
#                                   /imls/oper_model/got410c_otide_d${res}_6cells.heb   \
#                                   4
#$MALO_DIR/bin_static/gen_otide fes /imls/orig_data/fes2014b_extr           \
#                                   /imls/devel_model/mod44w_d${res}_dls_sea_6cells.heb \
#                                   /imls/oper_model/fes2014b_otide_d${res}_6cells.heb  \
#                                   4
$MALO_DIR/bin_static/gen_otide fes /imls/orig_data/fes2012 \
                                   /imls/devel_model/mod44w_d${res}_dls_sea_6cells.heb \
                                   /imls/oper_model/fes2012_otide_d${res}_6cells.heb   \
                                   4
#$MALO_DIR/bin_static/gen_otide got /imls/orig_data/got48/grids_oceantide \
#                                   /imls/devel_model/mod44w_d${res}_dls_sea_6cells.heb \
#                                   /imls/oper_model/got48_otide_d${res}_6cells.heb   \
#                                   4
exit 0 # %%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
$MALO_DIR/bin_static/equilibrium_tide_loading load    0 \
                                              $MALO_DIR/share/toc_grid_d${res}.cnf \
                                              /imls/oper_model/eqt_d${res}.heb \
                                              6
$MALO_DIR/bin_static/equilibrium_tide_loading load    1 \
                                              $MALO_DIR/share/toc_grid_d${res}.cnf \
                                              /imls/oper_model/eqt_coslam_d${res}.heb
$MALO_DIR/bin_static/equilibrium_tide_loading load    2 \
                                              $MALO_DIR/share/toc_grid_d${res}.cnf \
                                              /imls/oper_model/eqt_sinlam_d${res}.heb
#
$MALO_DIR/bin_static/equilibrium_tide_loading load_d1 0 \
                                               $MALO_DIR/share/toc_grid_d${res}.cnf \
                                               /imls/oper_model/eqt_d1_d${res}.heb
$MALO_DIR/bin_static/equilibrium_tide_loading load_d1 1 \
                                               $MALO_DIR/share/toc_grid_d${res}.cnf \
                                               /imls/oper_model/eqt_d1_coslam_d${res}.heb
$MALO_DIR/bin_static/equilibrium_tide_loading load_d1 2 \
                                               $MALO_DIR/share/toc_grid_d${res}.cnf \
                                               /imls/oper_model/eqt_d1_sinlam_d${res}.heb

