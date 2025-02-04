#!/usr/bin/env python3
import sys, os, shutil, subprocess
import datetime
import optparse 

hostname = os.uname()[1]
if ( hostname ==  "astrogeo" ):
     MALO_BIN = "/opt64/bin"
     SPD_DIR  = "/opt64/spd"
elif ( hostname ==  "earthrotation" ):
     MALO_BIN = "/opt64/bin"
     SPD_DIR  = "/opt64/spd"
elif ( hostname ==  "gs61a-geodev-a" ):
     MALO_BIN = "/opt64/bin"
     SPD_DIR  = "/opt64/spd"
elif ( hostname ==  "gs698-geopod.gsfc.nasa.gov" ):
     MALO_BIN = "/Users/lpetrov/opt/bin"
     SPD_DIR  = "/opt64/spd"
elif ( hostname ==  "aws-astrogeo" ):
     MALO_BIN = "/opt64/bin"
     SPD_DIR  = "/opt64/spd"
else:
     print ( "malo_query.py: unknown home host" )
     exit ( 1 )

malo_query_label = "malo_query version 2.3  of 2016.07.12"

#
# ------------------------------------------------------------------------
#
def main():
    MALO_SHARE  = os.popen(MALO_BIN + "/malo_inq share").read().rstrip()
    MALO_SCRIPT = os.popen(MALO_BIN + "/malo_inq script").read().rstrip()
    SPD_SHARE   = SPD_DIR + "/share"
    SPD_SCRIPT  = SPD_DIR + "/script"

    opts = optparse.OptionParser( version=malo_query_label )

    opts.add_option ( "-s", "--service", action="store", \
                      dest="service", \
                      metavar="NAME", \
                      help="Service name" )

    opts.add_option ( "-m", "--model", action="store", \
                      dest="model", \
                      metavar="NAME", \
                      help="Model name" )

    opts.add_option ( "-a", "--action", action="store", \
                      dest="action", \
                      metavar="NAME", \
                      help="Action" )
#
# --- Get and parse options
#
    opts, args = opts.parse_args()

    if ( opts.service == "atm" ):
         if ( opts.model == "merra" or opts.model == "MERRA2" ):
              if ( opts.action.rfind("list") > -1 ):
                   config_file = MALO_SHARE + "/atm_merra2_list_service.conf"
              elif ( opts.action.rfind("grid") > -1 or opts.action.rfind("sphe") > -1 ):
                   config_file = MALO_SHARE + "/atm_merra2_grid_service.conf"
              elif ( opts.action.rfind("vgep") > -1 ):
                   config_file = MALO_SHARE + "/atm_merra2_vgep_service.conf"
              else:
                   config_file = MALO_SHARE + "/atm_merra2_grid_service.conf"
         elif ( opts.model == "geosfpit" or opts.model == "GEOSFPIT" ):
              if ( opts.action.rfind("list") > -1 ):
                   config_file = MALO_SHARE + "/atm_geosfpit_list_service.conf"
              elif ( opts.action.rfind("grid") > -1 or opts.action.rfind("sphe") > -1 ):
                   config_file = MALO_SHARE + "/atm_geosfpit_grid_service.conf"
              elif ( opts.action.rfind("vgep") > -1 ):
                   config_file = MALO_SHARE + "/atm_geosfpit_vgep_service.conf"
              else:
                   config_file = MALO_SHARE + "/atm_geosfpit_grid_service.conf"
         else:
              print ( "malo_query.py: unknown model ", opts.model )
              exit ( 1 )

    elif ( opts.service == "aam" ):
         if ( opts.model == "geosfcs" or opts.model == "GEOSFCS" ):
              config_file = MALO_SHARE + "/aam_geosfcs_service.conf"
         elif ( opts.model == "geosfp" or opts.model == "GEOSFP" ):
              config_file = MALO_SHARE + "/aam_geosfp_service.conf"
         elif ( opts.model == "geosfpit" or opts.model == "GEOSFPIT" ):
              config_file = MALO_SHARE + "/aam_geosfpit_service.conf"
         elif ( opts.model == "merra" or opts.model == "MERRA" ):
              config_file = MALO_SHARE + "/aam_merra2_service.conf"
         elif ( opts.model == "merra2" or opts.model == "MERRA2" ):
              config_file = MALO_SHARE + "/aam_merra2_service.conf"
         else:
              print ( "malo_query.py: unknown model ", opts.model, " for aam service" )
              exit ( 1 )

    elif ( opts.service == "spd" ):
         if ( opts.model == "geosfpit" or opts.model == "GEOSFPIT" ):
              config_file = SPD_SHARE + "/spd_geosfpit_service.conf"
         elif ( opts.model == "merra" or opts.model == "MERRA" ):
              config_file = SPD_SHARE + "/spd_merra_service.conf"
         elif ( opts.model == "merra2" or opts.model == "MERRA2" ):
              config_file = SPD_SHARE + "/spd_merra2_service.conf"
         else:
              print ( "malo_query.py: unknown model ", opts.model, " for spd service" )
              exit ( 1 )

    elif ( opts.service == "lws" ):
         if ( opts.model == "merra" or opts.model == "MERRA2" ):
              if ( opts.action.rfind("list") > -1 ):
                   config_file = MALO_SHARE + "/lws_merra2_list_service.conf"
              elif ( opts.action.rfind("grid") > -1 or opts.action.rfind("sphe") > -1 ):
                   config_file = MALO_SHARE + "/lws_merra2_grid_service.conf"
              elif ( opts.action.rfind("vgep") > -1 ):
                   config_file = MALO_SHARE + "/lws_merra2_vgep_service.conf"
              else:
                   config_file = MALO_SHARE + "/lws_merra2_grid_service.conf"
         elif ( opts.model == "geosfpit" or opts.model == "GEOSFPIT" ):
              if ( opts.action.rfind("list") > -1 ):
                   config_file = MALO_SHARE + "/lws_geosfpit_list_service.conf"
              elif ( opts.action.rfind("grid") > -1 or opts.action.rfind("sphe") > -1 ):
                   config_file = MALO_SHARE + "/lws_geosfpit_grid_service.conf"
              elif ( opts.action.rfind("vgep") > -1 ):
                   config_file = MALO_SHARE + "/lws_geosfpit_vgep_service.conf"
              else:
                   config_file = MALO_SHARE + "/lws_geosfpit_grid_service.conf"
         else:
              print ( "malo_query.py: unknown model ", opts.model, \
                      " for service ", opts.service )
              exit ( 1 )

    elif ( opts.service == "nto" ):
         if ( opts.model == "omct" or opts.model == "OMCT05" ):
              if ( opts.action.rfind("list") > -1 ):
                   config_file = MALO_SHARE + "/nto_omct05_list_service.conf"
              elif ( opts.action.rfind("grid") > -1 or opts.action.rfind("sphe") > -1 ):
                   config_file = MALO_SHARE + "/nto_omct05_grid_service.conf"
              elif ( opts.action.rfind("vgep") > -1 ):
                   config_file = MALO_SHARE + "/nto_omct05_vgep_service.conf"
              else:
                   config_file = MALO_SHARE + "/nto_omct05_grid_service.conf"
         elif ( opts.model == "mpiom06" or opts.model == "MPIOM06" ):
              if ( opts.action.rfind("list") > -1 ):
                   config_file = MALO_SHARE + "/nto_mpiom06_list_service.conf"
              elif ( opts.action.rfind("grid") > -1 or opts.action.rfind("sphe") > -1 ):
                   config_file = MALO_SHARE + "/nto_mpiom06_grid_service.conf"
              elif ( opts.action.rfind("vgep") > -1 ):
                   config_file = MALO_SHARE + "/nto_mpiom06_vgep_service.conf"
              else:
                   config_file = MALO_SHARE + "/nto_mpiom06_grid_service.conf"
         else:
              print ( "malo_query.py: unknown model ", opts.model, \
                      " for service ", opts.service )
              exit ( 1 )
    elif ( opts.service == "stat" ):
         if ( opts.model == "malo" ):
              config_file = MALO_SHARE + "/malo_stat_service.conf"
         elif ( opts.model == "spd" ):
              config_file = SPD_SHARE + "/spd_stat_service.conf"

    else:
         print ( "malo_query.py: unknown service ", opts.service )
         exit ( 1 )

    if ( opts.action == "get_loading_first_date" ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_loading_first_date" )
    elif ( opts.action == "get_loading_last_date" ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_loading_last_date" )
    elif ( opts.action == "get_loading_last_update" ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_loading_last_update" )

    elif ( opts.action == "get_vgep_first_date" ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_vgep_first_date" )
    elif ( opts.action == "get_vgep_last_date" ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_vgep_last_date" )
    elif ( opts.action == "get_vgep_last_update" ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_vgep_last_update" )

    elif ( opts.action == "get_aam_last_update" and os.path.isfile(config_file) ):
         com = "python3 " + MALO_SCRIPT + "/malo_service.py" \
               " -c " + config_file + \
               " -a get_aam_last_update"
         os.system ( com )
    elif ( opts.action == "get_aam_first_date" and os.path.isfile(config_file) ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_aam_first_date" )
    elif ( opts.action == "get_aam_last_date" and os.path.isfile(config_file) ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_aam_last_date" )

    elif ( opts.action == "get_spd_last_update" and os.path.isfile(config_file) ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_spd_last_update" )
    elif ( opts.action == "get_spd_first_date" and os.path.isfile(config_file) ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_spd_first_date" )
    elif ( opts.action == "get_spd_last_date" and os.path.isfile(config_file) ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_spd_last_date" )

    elif ( opts.action == "get_num_files" ):
         os.system ( "python3 " + MALO_SCRIPT + "/malo_service.py" \
                     " -c " + config_file + \
                     " -a get_num_files" )

    else:
         print ( "malo_query.py: unknown action ", opts.action )
         exit ( 1 )


if __name__ == "__main__":
#    if ( sys.version[:3] < "3.0" ): print ( "This script cannot run under Python-2" ); exit ( 1 )
    main()
