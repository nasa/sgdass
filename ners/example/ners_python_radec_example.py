#!/usr/bin/env python3
import sys, datetime      
sys.path.append("~~~~~~")
import ners
"""
Example of computing azimuth and elevation using NERS
"""

print ( "=======================================================================" )
print ( "Example of comuting righrt ascension, declination and their rates using" )
print ( "NERS package of version ", ners.NERS__VERSION )
print ( "=======================================================================" )
print ( " " )

tai_time_tag_str = "2010.04.19_12:08:13.5"
tai_time_tag          = datetime.datetime.strptime( tai_time_tag_str, \
                          "%Y.%m.%d_%H:%M:%S.%f").replace(tzinfo=None)

coo_trs = [-1464001.8596, -5166632.9603, 3435015.2544] # Station Apollo
az  = 2.023812324
el  = ners.dms_to_rad ( "49d57m24.224"     )

(ra,dec,ra_rate,dec_rate)  = ners.get_radec ( tai_time_tag, coo_trs, az, el, "radio", ners.NERS__TAI )
( hei_ell, long, lat_gdt ) = ners.cart_to_hlp ( coo_trs )

print ( "Station with Carthesian coordinates %13.4f, %13.4f, %13.4f meters" % \
         (coo_trs[0], coo_trs[1], coo_trs[2]) )
print ( "with height above reference ellipsoid %8.3f meters, longitude %s, and geodetic latitide %s " % \
        ( hei_ell, ners.rad_to_dms ( long, 4 ), ners.rad_to_dms ( lat_gdt, 3 ) ) )
print ( "observes at azimuth    %12.7f deg  %13s,  elevation  %11.7f deg  %13s" % \
         ( az/ners.NERS__PI*180, ners.rad_to_dms ( az, 4 ), \
           el/ners.NERS__PI*180, ners.rad_to_dms ( el, 4 )  ) )
print ( "Its right ascension is %12.7f deg  %14s, declination %11.7f deg  %13s" % \
         ( ra/ners.NERS__PI*180,  ners.rad_to_hms ( ra,  4 ), \
           dec/ners.NERS__PI*180, ners.rad_to_dms ( dec, 4 )  ) )
print ( "Its right ascension rate is %12.7e rad/s, declination rate %12.7e rad/s" % \
        ( ra_rate, dec_rate ) )

print ( "at %s TAI" % tai_time_tag_str )
