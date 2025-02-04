# -*- coding: utf-8 -*-
"""
Created on Mon 2022.03.07_16:21:29

@author: nhabana
"""

from sgp4.api import Satrec
from sgp4.api import jday
from skyfield.api import EarthSatellite
import datetime
import numpy as np
np.set_printoptions(precision = 5)
#
# Declare TLE data
#
###l0 = 'GPS-43'
###l1 = '1 24876U 97035A   22004.32945293  .00000038  00000-0  00000-0 0  9997'
###l2 = '2 24876  55.4921 162.0762 0056001  54.4997 305.9846  2.00563059179360'

l0 = 'NAVSTAR-81'
l1 = '1 48859U 21054A   22163.44225417 -.00000069  00000-0  00000-0 0  9995'
l2 = '2 48859  55.1820  31.2500 0004320 191.4916 350.0064  2.00573681  7353'
#
# Read TLE to SGP4
#
satellite = Satrec.twoline2rv(l1, l2)
sat = EarthSatellite(l1,l2, l0)
sat_info = str(sat)
#
# JD and FR arrays
#
#jd, fr =  jday(2022, 1, 5, 7, 54, 25)
jd, fr =  jday(2022, 6, 12, 18, 0, 0)
e, r, v = satellite.sgp4(jd, fr)
print("TLE Info: ", sat_info )
print("mjd, utc:", jd-2400000.5, fr*86400)
print("r [km]:  ", r)
print("v [km/s]:", v)
print ("--------------------------------------------------")

f = open("/progs/tle_20230331/share/"+l0+"_python.eph", "w")
f.write("# TLE Info" + sat_info + "\n")
f.write("# " + l0 + "\n")
f.write("# " + l1 + "\n")
f.write("# " + l2 + "\n")
f.write("# MJD   UTC   X[m]   Y[m]   Z[m]   XDOT[m/s]   YDOT[m/s]   ZDOT[m/s]"  + "\n")
f.write("# \n")
for i in range(1440):
    jd, fr =  jday(2022, 6, 12, 12, 0+i, 0)

    if fr > 1:
       jd = jd + 1
       fr = fr - 1

    e, r, v = satellite.sgp4(jd, fr)
    f.write("%6d %7.2f %14.5f %14.5f %14.5f %14.5f %14.5f %14.5f \n" % (jd-2400000.5, fr*86400, r[0]*1000, r[1]*1000, r[2]*1000, v[0]*1000, v[1]*1000, v[2]*1000))

f.close()
#----------------------------------------------------------------------------
l0 = 'ISS_ZARYA'
l1 = '1 25544U 98067A   22163.88779598  .00005586  00000-0  10589-3 0  9995'
l2 = '2 25544  51.6445 357.8820 0004341 229.5747 266.3508 15.49969331344515'

#
# Read TLE to SGP4
#
satellite = Satrec.twoline2rv(l1, l2)
sat = EarthSatellite(l1,l2, l0)
sat_info = str(sat)
#
# JD and FR arrays
#
jd, fr =  jday(2022, 6, 12, 18, 0, 0)
e, r, v = satellite.sgp4(jd, fr)
print("TLE Info: ", sat_info )
print("mjd, utc:", jd-2400000.5, fr*86400)
print("r [km]:  ", r)
print("v [km/s]:", v)
print ("--------------------------------------------------")

f = open("/progs/tle_20230331/share/"+l0+"_python.eph", "w")
f.write("# TLE Info" + sat_info + "\n")
f.write("# " + l0 + "\n")
f.write("# " + l1 + "\n")
f.write("# " + l2 + "\n")
f.write("# MJD   UTC   X[m]   Y[m]   Z[m]   XDOT[m/s]   YDOT[m/s]   ZDOT[m/s]"  + "\n")
f.write("# \n")
for i in range(1440):
    jd, fr =  jday(2022, 6, 12, 12, 0+i, 0)

    if fr > 1:
       jd = jd + 1
       fr = fr - 1

    e, r, v = satellite.sgp4(jd, fr)
    f.write("%6d %7.2f %14.5f %14.5f %14.5f %14.5f %14.5f %14.5f \n" % (jd-2400000.5, fr*86400, r[0]*1000, r[1]*1000, r[2]*1000, v[0]*1000, v[1]*1000, v[2]*1000))

f.close()
#----------------------------------------------------------------------------
l0 = 'TDRS-13'
l1 = '1 42915U 17047A   22163.33675887 -.00000088  00000-0  00000-0 0  9998'
l2 = '2 42915   4.4850 333.4417 0020029 112.5204 284.3828  1.00271738 17650'

#
# Read TLE to SGP4
#
satellite = Satrec.twoline2rv(l1, l2)
sat = EarthSatellite(l1,l2, l0)
sat_info = str(sat)
#
# JD and FR arrays
#
jd, fr =  jday(2022, 6, 12, 18, 0, 0)
e, r, v = satellite.sgp4(jd, fr)
print("TLE Info: ", sat_info )
print("mjd, utc:", jd-2400000.5, fr*86400)
print("r [km]:  ", r)
print("v [km/s]:", v)
print ("--------------------------------------------------")

f = open("/progs/tle_20230331/share/"+l0+"_python.eph", "w")
f.write("# TLE Info" + sat_info + "\n")
f.write("# " + l0 + "\n")
f.write("# " + l1 + "\n")
f.write("# " + l2 + "\n")
f.write("# MJD   UTC   X[m]   Y[m]   Z[m]   XDOT[m/s]   YDOT[m/s]   ZDOT[m/s]"  + "\n")
f.write("# \n")
for i in range(1440):
    jd, fr =  jday(2022, 6, 12, 12, 0+i, 0)

    if fr > 1:
       jd = jd + 1
       fr = fr - 1

    e, r, v = satellite.sgp4(jd, fr)
    f.write("%6d %7.2f %14.5f %14.5f %14.5f %14.5f %14.5f %14.5f \n" % (jd-2400000.5, fr*86400, r[0]*1000, r[1]*1000, r[2]*1000, v[0]*1000, v[1]*1000, v[2]*1000))

f.close()
