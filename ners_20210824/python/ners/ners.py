#!/usr/bin/env python3
"""
# ************************************************************************
# *                                                                      *
# *  Python interface to the Network Earth Rotation Service client       *
# *  interface.                                                          *
# *                                                                      *
# *  Contains the following functions:                                   *
# *                                                                      *
# *    get_eop       -- get Earth Orientation Parameter(s)               *
# *    get_azelha    -- compute azimuth, elevation, and houor angle      *
# *    utc_to_tai    -- convert UTC time tag to TAI                      *
# *    hlp_to_cart   -- convert ellipsoidal height, longitude, and       *
# *                     geodetic latitude to Cartesian coordinate vector.*
# *    cart_to_hlp   -- convert Cartesian coordinate vector to           *
# *                     ellipsoidal height, longitude, and geodetic      *
# *                     latitude.                                        *
# *    rad_to_dms    -- convert angle in radians to degree, minutes,     *
# *                     seconds and a fraction of a second.              *
# *    rad_to_hms    -- convert angle in radians to hours, minutes,      *
# *                     seconds and a fraction of a second.              *
# *    dms_to_rad    -- convert string with degrees, minutes, seconds    *
# *                     and a fraction of a second to radians.           *
# *    hms_to_rad    -- convert string with hours, minutes, seconds and  *
# *                      a fraction of a second to radians.              *
# *    ydhms_to_time -- converts a string with date like                 *
# *                     2017y256d18h01m02s.12 or 2017y256d18:01:02.12 to *
# *                     python time object.                              *
# *                                                                      *
# * ### 12-JUN-2018  ners.py vers 1.1 (c) Leonid Petrov  29-MAR-2022 ### *
# *                                                                      *
# ************************************************************************
"""
import sys, os, subprocess, signal
from   math     import trunc, sqrt, cos, sin, atan
from   datetime import datetime, timedelta, tzinfo
#

def init():
    global  NERS__UTC, NERS__TAI, NERS__PI, NERS__REA, NERS__FLAT, NERS__VERSION
    global ners_child_pid, ners_pref_dir 
    NERS__UTC     = 1294843592
    NERS__TAI     = 2049230389
    NERS__PI      = 3.141592653589793e0
    NERS__REA     = 6378137.0
    NERS__FLAT    = 1.0/298.257
    NERS__VERSION = "%%%%%%"

class NersException(Exception):
      def __init__(self, message ):
          super().__init__(message)
init()
#
# ------------------------------------------------------------------------
#
def get_eop ( time_tag, par_name, time_scale=NERS__UTC ):
    """
    get_eop computes the specified Earth Orientation Parameter(s) on the 
    moment of time defined in the python time object. Argument time_tag 
    can be either in UTC or in TAI. The third optional argument specifies
    the time argument: either NERS__UTC or NERS__TAI. If omitted, the
    time stamp is interpreted as UTC. The parameter that will be computed
    is defined by the 2nd argument. Look documentation to ners_eop program
    for the list of supported parameter names.

    Input:

         time_tag -- Python time tag. Timezone attribute should be set 
                     to None.

         par_name -- string with parameter name

     [time_scale] -- time scale for interpretation of the time argument.
                     Supported time scale:
                     ners.NERS__UTC
                     ners.NERS__TAI
    Output:

       list of requested parameter(s) as float numbers.

    Requires  sys, os, subprocess, signal, math, datetime
    """

#
# --- Check the timezone
#
    if ( time_tag.tzinfo != None ):
         if ( ret != 0 ):
              raise NersException ( "tzinfo attribute is not None. Please, replace it to None" )

#
# --- Convert UTC to TAI if needed
#
    if ( time_scale == NERS__UTC ):
         tai_time = utc_to_tai ( time_tag )
    else:
         tai_time = time_tag

#
# --- Check the EOP value(s)
#
    (ret, out) = ners_exe ( "ners_eop" + \
                            " -t " + datetime.strftime( tai_time, '%Y.%m.%d_%H:%M:%S.%f') + \
                            " -p " + par_name )
    if ( ret != 0 ):
         raise NersException ( "Error in computing EOP: \n %s" % out )
     
    eop = []
    for i in range(3,len(out[0].split())):
        word = out[-1].split()[i].replace("D","e")
        eop.append ( float(word) )

    return ( eop )

#
# ------------------------------------------------------------------------
#
def get_azelha ( time_tag, coo_trs, ra, dec, refr_mode, time_scale=NERS__UTC ):
    """
    get_azel computes azimuth and elevation of an object beyond the Solar
    system with right ascension ra and declination dec in the inertial 
    barycentric celestial coordinate system for a station with coordinates 
    coo_trs in the crust-fixed coordinate system on the moment of time 
    defined in the python time object. Argument time_tag can be either in 
    UTC or in TAI. The sixth optional argument specifies the time argument: 
    either NERS__UTC or NERS__TAI. If omitted, the time stamp is interpreted 
    as UTC. Reduction for Earth rotation, annual and diurnal aberration, and
    ( optionally ) for refraction is made. No reduction for relativistic 
    light deflection is made

    Input:

       time_tag   -- python time object with the time tag either in UTC 
                     or TAI. Attribute tzino should be NONE.

       coo_trs    -- a list of three real values of station coordinates in the 
                     crust fixed coordinate system (f.e. ITRF) in meters.

       ra         -- right ascension of the observed object that is beyond
                     the Solar system in the inertial celestial barycentric 
                     coordinate system in radians.

       dec        -- declination of the observed object that is beyond
                     the Solar system in the inertial celestial barycentric 
                     coordinate system in radians.

       refr_mode  -- refraction mode as a character string. Accepted modes:
                     none  -- no reduction for refraction 
                     radio -- refraction is computed for radio frequencies
                     optic -- refraction is computed for optic frequencies

       time_scale -- interpretation of the time argument. Supported scales:
                     NERS_UTC ( default ) -- the time argument is interpreted
                                             as UTC.
                     NERS_TAI -- the time argument is interpreted as TAI.

    Output: ( az, el, ha )

       az         -- azimuth in radians.
       el         -- elevation in radians.
       ha         -- bhour angle in radians.

    Requires  sys, os, subprocess, signal, math, datetime
    """
  
#
# -- Check validity of the refraction mode
#
    if ( refr_mode == None ):
         refr_mode = "none"
    elif ( refr_mode.lower() == "none" ):
         refr_mode = "none"
    elif ( refr_mode.lower() == "radio" ):
         refr_mode = "radio"
    elif ( refr_mode.lower() == "optic" ):
         refr_mode = "optic"
    else:
         raise NersException ( "Wrong 5th argument %s. Supported values are none, radio, and optic"  % refr_mode )

    if ( time_tag.tzinfo != None ):
         if ( ret != 0 ):
              raise NersException ( "tzinfo attribute is not None. Please, replace it to None" )

#
# --- Convert UTC to TAI if needed
#
    if ( time_scale == NERS__UTC ):
         tai_time = utc_to_tai ( time_tag )
    else:
         tai_time = time_tag

#
# --- Compute azimuth and elevation
#
    (ret, out) = ners_exe ( "ners_azelha" + \
                            " %s %g %g %g %g %g %s" % \
                            ( datetime.strftime( tai_time, '%Y.%m.%d_%H:%M:%S.%f'), \
                              coo_trs[0], coo_trs[1], coo_trs[2], ra, dec, refr_mode ) \
                          )
    if ( ret != 0 ):
         raise NersException ( "Error in computing EOP: \n %s" % out )
     
#
# --- Convert azimuth and elevation to float
#
    az = float(out[-1].split()[0])
    el = float(out[-1].split()[1])
    ha = float(out[-1].split()[2])

    return ( az, el, ha )
#
# ------------------------------------------------------------------------
#
def utc_to_tai ( utc_time_tag ):
    """
    utc_to_tai converts the python time object utc_time_tag from UTC to TAI
    and returns python time object. The time tag should be in a range from 
    1970.01.01 through 6 months in the future. Attribute tzinfo should be None.

    Requires  sys, os, subprocess, signal, math, datetime
    """

    if ( utc_time_tag.tzinfo != None ):
         if ( ret != 0 ):
              raise NersException ( "tzinfo attribute is not None. Please, replace it to None" )

#
# --- Convert Python object time into ascii string
#
    time_str = datetime.strftime( utc_time_tag, '%Y.%m.%d_%H:%M:%S.%f')
#
# --- Get the value of utc minus tai function on time tag utc_time_tag
#
    (ret, out) = ners_exe ( "utcmtai " + time_str )
    if ( ret != 0 ):
         raise NersException ( "Error in attemmpt to convert UTC time " + \
                               "tag to TAI: \n %s" % out )
#
# --- Transform utc minus tai to float
#
    utc_m_tai = int(float(out[0]))
#
# --- ... and then crate tai_time tag
#
    tai_time_tag = utc_time_tag - timedelta(seconds=utc_m_tai)
    return ( tai_time_tag )

#
# ------------------------------------------------------------------------
#
def hlp_to_cart ( hei, long, lat_gdt ):
    """
    Auxiliary routine hlp_to_cart transforms height above the reference
    ellipsod hei, longitude long, and geodetic latitude lat_gdt to
    Cartesian coordinates at the Earth's surface.
   
    Input:
  
           hei_ell -- height above the reference ellipsoid in meters, 
           long    -- east longitude in radians
           lat_gdt -- geodetic latitude in radians.

    Output:
 
           coo_trs -- real array of 3 elements of Cartesian coordinates
                      in a crust-fixed coordinate system.

    Requires math
    """
    exc_sq = 2.0*NERS__FLAT - NERS__FLAT**2 # Earth's shape eccentricity

    coo_trs = []
    coo_trs.append ( (                NERS__REA/sqrt(1.0 - exc_sq*sin(lat_gdt)**2) + hei ) * cos(long)*cos(lat_gdt) )
    coo_trs.append ( (                NERS__REA/sqrt(1.0 - exc_sq*sin(lat_gdt)**2) + hei ) * sin(long)*cos(lat_gdt) )
    coo_trs.append ( ( (1.0 - exc_sq)*NERS__REA/sqrt(1.0 - exc_sq*sin(lat_gdt)**2) + hei ) * sin(lat_gdt) )

    return ( coo_trs )
#
# ------------------------------------------------------------------------
#
def cart_to_hlp ( coo_trs ):
    """
    Auxiliary routine cart_to_hlp transforms vector of Cartesian 
    coordinates at the Earth's surface to height above the reference
    ellipsoid hei, longitude long, and geodetic latitude lat_gdt.
    
    Input:

           coo_trs -- real array of 3 elements of Cartesian coordinates
                      in a crust-fixed coordinate system.

    Output:
 
          ( hei_ell, long, lat_gdt ) -- tulip of the height above the 
                      reference ellipsoid in meters, east longitude
                      in radians in [-pi,pi] range, geodetic latitude 
                      in radians.

    Requires math
    """

    exc_sq = 2.0*NERS__FLAT - NERS__FLAT**2 # Earth's shape eccentricity
#
# ----- Computation of longitude
#
    if ( abs(coo_trs[0]) > 1.0e-8 ):
         long = atan ( coo_trs[1]/coo_trs[0] )
    else:
         long = NERS__PI/2.0

    if ( coo_trs[0] < 0.0 ): long = long +     NERS__PI
    if ( long       < 0.0 ): long = long + 2.0*NERS__PI

    p = sqrt ( coo_trs[0]**2 + coo_trs[1]**2 )
    if ( abs(p) < 1.0e-8 ): p = 1.0e-8
    rd = sqrt ( p**2 + coo_trs[2]**2 )
#
# --- Computation of geocentric latitude
#
    lat_gcn = atan( coo_trs[2]/p )
#
# ----- Computation of geodetic latitude
#
    mu = atan ( coo_trs[2]/p * ( (1.0 - NERS__FLAT) + exc_sq*NERS__REA/rd ) )
#
    lat_gdt = atan ( ( (1.0 - NERS__FLAT)* coo_trs[2] + exc_sq*NERS__REA*sin(mu)**3 )/ \
                     ( (1.0 - NERS__FLAT)*(p          - exc_sq*NERS__REA*cos(mu)**3 )) )
#
# --- Computation of height above hei_ell
#
    hei_ell = p*cos(lat_gdt) + coo_trs[2]*sin(lat_gdt)  - \
              NERS__REA*sqrt( 1.0 - exc_sq*sin(lat_gdt)**2 )

    return ( hei_ell, long, lat_gdt )
#
# ------------------------------------------------------------------------
#
def rad_to_dms ( ang, num_dig=6, plus_needed=None ):
    """
    Auxiliary routine rad_to_dms transforms angle from radians to 
    degree, minutes, seconds, and second fraction. The 2nd optional
    arguments controls how many digits of second fraction is 
    to keep. Default is 6. Degrees, minutes, and seconds are separated
    by colon character (:).

    By default, leading sygn '+' is not printed for positive angles.
    When the 3rd argument is '+', when it is printed

    Required: math
    """
    if ( ang < 0.0 ):
         sig = -1
    else:
         sig = 1
 
    deg  = (sig*ang)/NERS__PI*180.0
    ideg = trunc ( deg )
    mnt  = trunc ( (deg - ideg)*60.0 )
    imnt = trunc ( mnt ) 
    sec  = ((deg - ideg)*60.0 - imnt)*60.0

    if ( num_dig > 0 ):
         fmt = "%02d:%02d:%0" + "%d" % (num_dig + 3) + ".%d" % num_dig + "f"
    else:
         fmt = "%02d:%02d:%02d"

    dms_str = fmt % ( deg, mnt, sec )
    if ( sig == -1 ): dms_str = "-" + dms_str
    if ( sig ==  1 and plus_needed ): dms_str = "+" + dms_str
   
    return ( dms_str )
#
# ------------------------------------------------------------------------
#
def dms_to_rad ( dms ):
    """
    Auxiliary routine dms_to_rad transforms angle from degree, minutes, 
    seconds, and second fraction to radians. Degrees, minutes, and
    seconds are separated either by : or by _ or by d, m, and s.

    """
    dms_list=dms.replace("-","").replace("_",":").replace("d",":").replace("m",":").replace("'",":").replace("s","").replace('"','').split(":")
    if ( len(dms_list) == 2 ):
         rad = (float(dms_list[0]) + \
                float(dms_list[1])/60.0)/180.0*NERS__PI
    elif ( len(dms_list) == 3 ):
         rad = (float(dms_list[0]) + \
                float(dms_list[1])/60.0 + \
                float(dms_list[2])/3600.0 )/180.0*NERS__PI
    else: 
         raise NersException ( "String %s does not contain delimeter : or _" % dms )

    if ( dms[0:1] == "-" ): rad = -rad

    return ( rad )

#
# ------------------------------------------------------------------------
#
def rad_to_hms ( ang, num_dig=6 ):
    """
    Auxiliary routine rad_to_hms transforms angle from radians to 
    hours, minutes, seconds, and second fraction. The 2nd optional
    arguments controls how many digits of second fraction is 
    to keep. Default is 6. Hours, minutes, and seconds are separated
    by colon character (:).

    Required: math
    """
    if ( ang < 0.0 ):
         sig = -1
    else:
         sig = 1
    hou  = (sig*ang)/NERS__PI*12.0
    ihou = trunc ( hou )
    mnt  = trunc ( (hou - ihou)*60.0 )
    imnt = trunc ( mnt ) 
    sec  = ((hou - ihou)*60.0 - imnt)*60.0

    if ( num_dig > 0 ):
         fmt = "%02d:%02d:%0" + "%d" % (num_dig + 3) + ".%d" % num_dig + "f"
    else:
         fmt = "%02d:%02d:%02d"

    hms_str = fmt % ( hou, mnt, sec )
    if ( sig == -1 ): hms_str = "-" + hms_str
   
    return ( hms_str )

#
# ------------------------------------------------------------------------
#
def hms_to_rad ( hms ):
    """
    Auxiliary routine hms_to_rad transforms angle from hours, minutes, 
    seconds, and second fraction to radians. Degrees, minutes, and
    seconds are separated either by : or by _ or by h, m, and s.

    """
    hms_list=hms.replace("-","").replace("_",":").replace("h",":").replace("m",":").replace("s","").split(":")
    if ( len(hms_list) == 2 ):
         rad = (float(hms_list[0]) + \
                float(hms_list[1])/60.0)/12.0*NERS__PI
    elif ( len(hms_list) == 3 ):
         rad = (float(hms_list[0]) + \
                float(hms_list[1])/60.0 + \
                float(hms_list[2])/3600.0 )/12.0*NERS__PI
    else: 
         raise NersException ( "String %s does not contain delimeter : or _" % hms )

    if ( hms[0:1] == "-" ): rad = -rad

    return ( rad )

#
# ------------------------------------------------------------------------
#
def ydhms_to_time ( ydhms ):
    """
    Auxiliary routine ydhms_to_time transforms date in format like
    "2017y256d18h01m02s.123456", or "2017y256d18:01:02.123456"  or
    "2017y256d18h01m02",  "2017y256d" to python time.
    """
    if ( len(ydhms) > 5 ):
         if ( ydhms[4:5] == "." ): ydhms[0:4] + "y" + ydhms[5:] 

    if ( len(ydhms) > 9 ):
         if ( ydhms[8:9] == "." ): ydhms[0:8] + "d" + ydhms[9:] 

    ydhms_list = ydhms.replace("y",":").replace("d",":").replace("h",":").replace("m",":").replace("s","").split(":")

    if ( len(ydhms_list) == 5 ):
         if ( not "." in ydhms_list[4] ): ydhms_list[4] = ydhms_list[4] + ".0"
    elif ( len(ydhms_list) == 4 ):
         ydhms_list.append ( "0.0"  )
    elif ( len(ydhms_list) == 3 ):
         ydhms_list.append ( "00"   )
         ydhms_list.append ( "00.0" )
    elif ( len(ydhms_list) == 2 ):
         ydhms_list.append ( "00"   )
         ydhms_list.append ( "00"   )
         ydhms_list.append ( "00.0" )
    elif ( len(ydhms_list) == 1 ):
         ydhms_list.append ( "001" )
         ydhms_list.append ( "00"  )
         ydhms_list.append ( "00"  )
         ydhms_list.append ( "0.0" )

    try:
         tim = datetime.strptime( ":".join(ydhms_list), "%Y:%j:%H:%M:%S.%f").replace(tzinfo=None)
    except:
         pass
         raise NersException ( "String %s does not contain delimeter : or y d m h s " % ydhms )

    return ( tim )
#
# ------------------------------------------------------------------------
#
def ners_time_arg ( time_tag, time_scale=NERS__UTC ):
    """
    ners_time_arg converts the python time object to NERS time argument:
    time interval since epoch 2000.01.01_00:00:00.0 tai in seconds.
    time_tag can be either in UTC or in TAI. The second argument specifies
    the time argument: either NERS__UTC or NERS__TAI. If omitted, the
    time stamp is interpreted as UTC.

    Requires  sys, os, subprocess, signal, math, datetime
    """

    if ( time_tag.tzinfo != None ):
         if ( ret != 0 ):
              raise NersException ( "tzinfo attribute is not None. Please, replace it to None" )

    if   ( time_scale == NERS__UTC ):
         tai_time = utc_to_tai ( time_tag )
    elif ( time_scale == NERS__TAI ):
         tai_time = time_tag
    else: 
         raise NersException ( "Unsupported time scale %d. NERS__UTC or NERS__TAI were expected" % time_scale )

#
# --- Compute MJD for the python time object
#
    mjd = tai_time.day - 2432076 \
          + trunc( 1461* (        tai_time.year  + 4800 + trunc((tai_time.month-14)/12)   )/4   )    \
          + trunc(  367* (        tai_time.month     -2 - trunc((tai_time.month-14)/12)*12)/12  )    \
          - trunc(3*trunc((tai_time.year  + 4900 + trunc((tai_time.month-14)/12))/100)/4)  

#
# --- Finally, compuite tim argument.
#
    tim = (mjd - 51544)*86400.0 + \
          3600.0*tai_time.hour + \
            60.0*tai_time.minute + \
                 tai_time.second + \
                 tai_time.microsecond/1.0E6

    return ( tim )
#
# ------------------------------------------------------------------------
#
def ners_signal_handler_term ( signal, frame ):
    """
    Auxilliary routine ners_signal_handler_term is for handling singals. 
    IT is invoked by python signal handler. When it receives the signal, 
    it relays it to the NERS child process.
    """
    if ( ners_child_pid ):
         os.kill ( ners_child_pid, signal )
    print ( 'Terminated by TERM signal' )
    sys.exit(0)

#
# ------------------------------------------------------------------------
#
def ners_exe ( command, verb=0 ):
    global ners_child_pid
    """
    Auxilliary routine ners_exe spawns a supborcess, executes command in 
    the context of the suborpess, waits for its completion and return 
    the completion code and results as ( returncode, out ). out is a list 
    of strings without trailing "\n". If verb >= 2, the command to be 
    executed is also printed in stdout.

    Requires  sys, os, subprocess, signal, math, datetime
    """
    global ners_child_pid

    if ( verb >= 2 ): 
         date_str = datetime.now().strftime("%Y.%m.%d_%H:%M:%S.%f")[0:23]
         print ( date_str, "execute:", command )
         sys.stdout.flush()
         
#
# --- Launch the subprocess
#
    com = command.split()
    com[0] = ners_pref_dir + "/bin/" + com[0]
    proc = subprocess.Popen ( com, \
                              stdout=subprocess.PIPE, \
                              stderr=subprocess.STDOUT )
    ners_child_pid = proc.pid
#
# --- Catch the output in list out
#
    out = []
    for line in proc.stdout:
        out.append ( str(line, encoding="utf-8").split("\n")[0] )
#
# --- wait for completion of the child process
#
    proc.wait()
    ners_child_pid = None
#
# --- Return the runcode and the output that was caught
#
    return ( proc.returncode, out )


class ners:
      global ners_child_pid, ners_pref_dir
      vers = "%02d%02d%03d" % ( sys.version_info.major, sys.version_info.minor, sys.version_info.micro )
      if ( vers < "0302000" ): print ( "This script cannot run under Python older than 3.2. Please upgrade" ); exit ( 1 )
      signal.signal ( signal.SIGTERM, ners_signal_handler_term )
      signal.signal ( signal.SIGTTOU, signal.SIG_IGN )
      ners_child_pid = None
      ners_pref_dir = "@@@@@@"
