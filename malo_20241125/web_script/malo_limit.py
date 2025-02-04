#!/usr/bin/python3 
#
# ************************************************************************
# *                                                                      *
# *   Program malol_limit.py displays limits for ondemand loading        *
# *   computations.                                                      *
# *                                                                      *
# * ### 05-JUL-2015 check_st_list.py v2.3 (c) L. Petrov  29-SEP-2014 ### *
# *                                                                      *
# ************************************************************************
#
import os, sys, string, stat, datetime
from   malo_check_date   import *
from   malo_check_stafil import *
from   malo_subs         import *
from   malo_set_catcha   import *

#
# ------------------------------------------------------------------------
#

config = config_class()
ip_unlim = check_ip ( config, os.environ['REMOTE_ADDR']  )

if ( ip_unlim == 0 ):
     print ( "Limits: %2.1f years, %d stations per request." % \
              ( config.max_intrv/(366.0*86400.0), config.max_sta ) )
else:
     print ( "Limits: %2.1f years, %d stations per request." % \
              ( config.malo_max_intrv/(366.0*86400.0), config.malo_max_sta ) )
