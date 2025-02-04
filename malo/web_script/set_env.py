#!/usr/bin/python3 
# ************************************************************************
# *                                                                      *
# *   Routine set_env sets some environment variables.                   *
# *                                                                      *
# *  ### 11-NOV-2021     set_env   v1.0 (c)  L. Petrov  11-NOV-2021 ###  *
# *                                                                      *
# ************************************************************************
import os, sys
def set_env():
    if ( os.uname().nodename == "gs61a-geodev-a" ):
         os.environ["LD_LIBRARY_PATH"] = "/opt64/lib:" + os.environ["LD_LIBRARY_PATH"] 
    else:
         os.environ["LD_LIBRARY_PATH"] = "/opt64/lib"
