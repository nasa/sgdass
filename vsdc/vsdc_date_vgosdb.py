import sys, os, signal
import os
import re
import subprocess
import tarfile
import tempfile
import datetime
from   vsdc_misc          import *

def vsdc_date_vgosdb ( filename ):
    """
    Extracts the scan reference time of the first and the last
    observation from the VLBI database file in VGOSDB format.
    Author: S. Bolotin
    """
#
# --- Check whether the input file exists and whether it is a tar archive
#
    if ( len(filename) == 0):
            print("Error: no vgosDb archive name is provided")
            return (None, None)
    hfName = 'Head.nc'
#
    if not tarfile.is_tarfile(filename):
            print("Error: the file " + filename + " is not a tar archive")
            return (None, None)
#
    try:
        tar = tarfile.open(filename)
    except BaseException as e:
        print ( "Failure in opening input file %s for reading: %s " % (filename, str(e) ) )
        return (None, None)
#
    string = tar.getnames()[1]
    match = re.match(r'.*(\d{2}[A-Z]{3}\d{2}[A-Z]{2}).*', string)
    sessionName = match.group(1)
    match = re.match(r'(\w*)/?(.*)', string)
    tarRootName = match.group(1)
#
# --- Extract vgosdb header file
#
    tmpDirName = tempfile.mkdtemp(dir="./")
    tarHfName = tarRootName + '/' + hfName
    path2HfName = tmpDirName + '/' + tarHfName
#
    try:
        tar.extract(tarHfName, path=tmpDirName)
    except BaseException as e:
        print ( "Failure in extracting the header of VGOSDB file %s -- %s " % (filename, str(e) ) )
        return (None, None)
    tar.close()
#
# --- Run ncdump for vgosdb header file
#
    try:
        ncDumpRun = subprocess.run(['ncdump', '-v', 'iUTCInterval', path2HfName],
                                     stdout=subprocess.PIPE)
    except BaseException as e:
        print ( "Failure in running ncdump for the header file %s extracted from of VGOSDB file %s -- %s " % (path2HfName, filename, str(e) ) )
        return (None, None)
#
# --- clear the files/directories:
#
    os.remove(path2HfName)
    os.rmdir(tmpDirName + '/' + tarRootName)
    os.rmdir(tmpDirName)
#
# --- Extract the date from the header file
#
    ss = re.sub(r'\n', ' ', ncDumpRun.stdout.decode())
    ma = re.match(r'.*iUTCInterval\s+=\s+(\d{4}),\s+(\d+),\s+(\d+),\s+(\d+),'
        '\s+(\d+),\s+(\d{4}),\s+(\d+),\s+(\d+),\s+(\d+),\s+(\d+)\s+;\s+}.*', ss)
    if ma.lastindex == 10:
            StartYr = int(ma.group( 1))
            StartMo = int(ma.group( 2))
            StartDy = int(ma.group( 3))
            StartHr = int(ma.group( 4))
            StartMi = int(ma.group( 5))
            StartSc = 0
            StopYr  = int(ma.group( 6))
            StopMo  = int(ma.group( 7))
            StopDy  = int(ma.group( 8))
            StopHr  = int(ma.group( 9))
            StopMi  = int(ma.group(10))
            StopSc  = 0
    else:
            StartYr =0
            StartMo =0
            StartDy =0
            StartHr =0
            StartMi =0
            StartSc =0
            StopYr  =0
            StopMo  =0
            StopDy  =0
            StopHr  =0
            StopMi  =0
            StopSc  =0

    date_start = datetime.datetime ( StartYr, StartMo, StartDy, StartHr, StartMi, StartSc )
    date_stop  = datetime.datetime ( StopYr,  StopMo,  StopDy,  StopHr,  StopMi,  StopSc  )

    return ( date_start, date_stop )
