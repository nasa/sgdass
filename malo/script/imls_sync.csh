#!/bin/csh -f
# ************************************************************************
# *                                                                      *
# *   Routine imsl_sync copies IMSL related files from Astrogeo Center   *
# *   to 
# *                                                                      *
# *  ### 24-JUN-2015               v1.0 (c)  L. Petrov  24-JUN-2015 ###  *
# *                                                                      *
# ************************************************************************
rsync -av /imls/love_sphe       ml:/imls/
rsync -av /imls/load_grid       ml:/imls/
rsync -av /imls/load_list       ml:/imls/
rsync -av /imls/load_hps        ml:/imls/
rsync -av /imls/load_bdsp       ml:/imls/
rsync -av /imls/spr             ml:/imls/
rsync -av /imls/load_d1_list    ml:/imls/
rsync -av /imls/load_d1_grid    ml:/imls/
rsync -av /imls/vgep            ml:/imls/
rsync -av /imls/orig_data       ml:/imls/
rsync -av /imls/load_tarbdsp    ml:/imls/
rsync -av /imls/load_d1_hps     ml:/imls/
rsync -av /imls/load_int        ml:/imls/
rsync -av /imls/load_d1_int     ml:/imls/
rsync -av /imls/otides          ml:/imls/
#
rsync -av /imls/heb/geosfp_507  ml:/imls/heb/
rsync -av /imls/heb/geosfp_511  ml:/imls/heb/
rsync -av /imls/heb/geosfp      ml:/imls/heb/
rsync -av /imls/heb/geosfpit    ml:/imls/heb/
rsync -av /imls/heb/noah025     ml:/imls/heb/
rsync -av /imls/heb/omct        ml:/imls/heb/
rsync -av /g7/heb/merra         ml:/e7/heb/
