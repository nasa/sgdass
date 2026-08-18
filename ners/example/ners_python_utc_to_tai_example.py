#!/usr/bin/env python3
import sys, datetime      
sys.path.append("%%%%%%")
import ners
"""
Example of tranformation from UTC to TAI
"""

print ( "==============================================================" )
print ( "Example of transforming UTC time tag to TAI                   " )
print ( "NERS version: ", ners.NERS__VERSION                             )
print ( "==============================================================" )
print ( " " )

now_utc_time_tag = datetime.datetime.now()
now_tai_time_tag = ners.utc_to_tai ( now_utc_time_tag )
now_utcmtai      = ners.get_eop ( now_utc_time_tag, "utcmtai", ners.NERS__UTC )

print ( "Now utc_time_tag  = ", now_utc_time_tag )
print ( "Now tai_time_tag  = ", now_tai_time_tag )
print ( "NOW UTC minus TAI = ", now_utcmtai[0]   )

utc_time_tag = datetime.datetime.strptime( "1998.06.15_23:12:58.983256", \
               "%Y.%m.%d_%H:%M:%S.%f").replace(tzinfo=None)
tai_time_tag = ners.utc_to_tai ( utc_time_tag )
utcmtai      = ners.get_eop ( utc_time_tag, "utcmtai", ners.NERS__UTC )

print ( " " )
print ( "utc_time_tag= ", utc_time_tag )
print ( "tai_time_tag= ", tai_time_tag )
print ( "UTC minus TAI   = ", utcmtai[0]   )
