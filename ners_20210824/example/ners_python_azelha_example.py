#!/usr/bin/env python3
import sys, datetime      
sys.path.append("~~~~~~")
import ners
"""
Example of computing azimuth and elevation using NERS
"""

print ( "==============================================================" )
print ( "Example of comuting azimuth, elevations and hour anbles using " )
print ( "NERS package of version ", ners.NERS__VERSION                   )
print ( "==============================================================" )
print ( " " )

tai_time_tag          = datetime.datetime.strptime( "2010.04.19_16:12:10.0", \
                          "%Y.%m.%d_%H:%M:%S.%f").replace(tzinfo=None)

coo_trs = [2730173.983, 1562442.587, 5529969.046] # Stations SVETLOE
ra      = ners.hms_to_rad ( "12:29:06.699732"  ) # 3C273B
dec     = ners.dms_to_rad ( "+02d03m08.59803s" )

(az,el,ha) = ners.get_azelha ( tai_time_tag, coo_trs, ra, dec, "radio", ners.NERS__TAI )
( hei_ell, long, lat_gdt ) = ners.cart_to_hlp ( coo_trs )

print ( "Station with Carthesian coordinates %13.4f, %13.4f, %13.4f meters" % \
         (coo_trs[0], coo_trs[1], coo_trs[2]) )
print ( "with height above reference ellipsoid %8.3f meters, longitude %s, and geodetic latitide %s " % \
        ( hei_ell, ners.rad_to_dms ( long, 4 ), ners.rad_to_dms ( lat_gdt, 3 ) ) )
print ( "observes a source with right ascension %s and declination %s" % \
        ( ners.rad_to_hms ( ra, 6 ), ners.rad_to_dms ( dec, 5 ) ) )
print ( "at azimuth %12.7f degrees, elevation %12.7f degrees, and hour angle %12.7f degrees" % \
         ( az/ners.NERS__PI*180, el/ners.NERS__PI*180, ha/ners.NERS__PI*180  ) )
        
