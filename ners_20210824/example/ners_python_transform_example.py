#!/usr/bin/env python3
import sys
sys.path.append("%%%%%%")
import ners
"""
Example of time tranformation
"""

print ( "==============================================================" )
print ( "Example of transforing dates and angles                       " )
print ( "NERS version: ", ners.NERS__VERSION                             )
print ( "==============================================================" )
print ( " " )

#
# --- Transform date in this format:
#
ydhms_date = "2017y256d18h01m02s.123456" 
tim     = ners.ydhms_to_time ( ydhms_date )
tim_str = tim.strftime("%Y.%m.%d_%H:%M:%S.%f") 

print ( "Converting time tag " + ydhms_date )
print ( 'tim     = ', tim )
print ( 'tim_str = ', tim_str )

#
# --- Transform date in this format:
#
ydhms_date = "2018y100d23:01:24"
tim     = ners.ydhms_to_time ( ydhms_date )
tim_str = tim.strftime("%Y.%m.%d_%H:%M:%S.%f") 

print ( " " )
print ( "Converting time tag " + ydhms_date )
print ( 'tim     = ', tim )
print ( 'tim_str = ', tim_str )

#
# --- Transform angle from DD:MM:SS.ff format to radians
#
angle_str = "-02:05:59.21" # "-02_05_59.21" or "-02d05m59s.21" "-02d05m59.21a" will work as well
angle_rad = ners.dms_to_rad ( angle_str )

print ( " " ) 
print ( "Angle in degrees, arcminutes, and arcseconds: ", angle_str, " the same angle in radians: ", angle_rad  )
angle_str = ners.rad_to_dms ( angle_rad, 4 )
#
# --- Transform angle from from radians to DD:MM:SS.ff 
#
print ( "Angle in radians: ", angle_rad, " the same aangle in degrees, arcminutes, and arcseconds: ", angle_str )

angle_rad = -angle_rad
angle_str = ners.rad_to_dms ( angle_rad, 4 )
print ( "Positive angle in radians: ", angle_rad, " (by default leading + is not pinted): ", angle_str )

angle_str = ners.rad_to_dms ( angle_rad, 4, '+' )
print ( "Positive angle in radians: ", angle_rad, " (the third arument is + ): ", angle_str )

#
# --- Transform angle from HH:MM:SS.ff format to radians
#

hour_str = "-18:34:02.96" # "18_34_02.96" or "18h34m02s.96" "18_34_02.96s" will work as well
angle_rad = ners.dms_to_rad ( hour_str )

print ( " " ) 
print ( "Angle in hours, minutes, and seconds: ", hour_str, " the same angle in radians: ", angle_rad )
#
# --- Transform angle from from radians to HH:MM:SS.ff 
#
hour_str = ners.rad_to_hms ( angle_rad, 5 )
print ( "Angle in radians: ", angle_rad, " the same angle in hours minutes and seconds: ", hour_str )

coo_trs = [1130794.763, -4831233.803, 3994217.042 ]
#
# --- Transform Carthesian coordintes to height above the reference ellipsoid, 
# --- east longitude, and geodetic latitude
#
( hei_ell, long, lat_gdt ) = ners.cart_to_hlp ( coo_trs )
print ( "Carthesian coordinates %13.4f, %13.4f, %13.4f meters" % (coo_trs[0], coo_trs[1], coo_trs[2]) )
print ( "Corresponding height above the reference ellipsoid: %8.3f %s " % ( hei_ell, "meters" ) )
print ( "Corresponding longitude: %s "         % ners.rad_to_dms ( long,    5 ) )
print ( "Corresponding geodetic latitude: %s " % ners.rad_to_dms ( lat_gdt, 4 ) )

#
# --- Transform height above the reference ellipsoid, east longitude, and geodetic latitude to
# --- Carthesian coordintes
#
coo_trs = ners.hlp_to_cart ( hei_ell, long, lat_gdt )

print ( " " )
print ( " Inverse transformation: " )
print ( "Carthesian coordinates %13.4f, %13.4f, %13.4f meters" % (coo_trs[0], coo_trs[1], coo_trs[2]) )
