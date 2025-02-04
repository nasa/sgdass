#!/usr/bin/env python3
import sys, datetime      
sys.path.append("%%%%%%")
import ners
"""
Example of getting the Earth orientation parameters using NERS
"""

print ( "==============================================================" )
print ( "Example of getting the Earth orientation parameters using NERS" )
print ( "NERS version: ", ners.NERS__VERSION                             )
print ( "==============================================================" )
print ( " " )

now_utc_time_tag      = datetime.datetime.now()
earth_rotation_matrix = ners.get_eop ( now_utc_time_tag, "mat", ners.NERS__UTC )
print ( "Earth rotation matrix for now: (", now_utc_time_tag, "): \n", \
        "mat= ", earth_rotation_matrix )

tai_time_tag          = datetime.datetime.strptime( "2010.04.19_08:12:10.0", \
                          "%Y.%m.%d_%H:%M:%S.%f").replace(tzinfo=None)
earth_rotation_matrix = ners.get_eop ( tai_time_tag, "matall", ners.NERS__TAI )
print ( " " )
print ( "TAI time: ", tai_time_tag )
print ( "Earth rotation matrix        time derivative: \n ", earth_rotation_matrix[9:18] )
print ( " " )
print ( "Earth rotation matrix second time derivative: \n ", earth_rotation_matrix[18:27] )

ut1_minus_tai = ners.get_eop ( tai_time_tag, "ut1mtai", ners.NERS__TAI )[0]
lod           = ners.get_eop ( tai_time_tag, "lod",     ners.NERS__TAI )[0]

print ( " " )
print ( "TAI time: ", tai_time_tag )
print ( "Value of UT1 minus TAI function: ", ut1_minus_tai )
print ( "Value of LOD:                    ", lod )
print ( " " )

utc_time_tag          = datetime.datetime.strptime( "2016.10.23_00:00:00.0", \
                          "%Y.%m.%d_%H:%M:%S.%f").replace(tzinfo=None)

eopr = ners.get_eop ( tai_time_tag, "eop3r", ners.NERS__UTC )
nutr = ners.get_eop ( tai_time_tag, "nutr",  ners.NERS__UTC )

print ( " " )
print ( "UTC time: ", tai_time_tag )
print ( "X pole coordinate:         %13.7g %s " % ( eopr[0], " arcsec" ) )
print ( "Y pole coordinate:         %13.7g %s " % ( eopr[1], " arcsec" ) )
print ( "UT1 minus TAI:             %13.7g %s " % ( eopr[2], " s" ) )
print ( "Nutation in longitude      %13.7g %s " % ( nutr[0], " rad" ) )
print ( "Nutation in obliquity      %13.7g %s " % ( nutr[1], " rad" ) )
print ( "X pole rate                %13.7g %s " % ( eopr[3], " arcsec/day" ) )
print ( "Y pole rate:               %13.7g %s " % ( eopr[4], " arcsec/day" ) )
print ( "UT1 minus TAI rate:        %13.7g %s " % ( eopr[5], " s/day" ) )
print ( "Nutation in longitude rate %13.7g %s " % ( nutr[2], " rad/s" ) )
print ( "Nutation in obliquity rate %13.7g %s " % ( nutr[3], " rad/s" ) )
print ( " " )
