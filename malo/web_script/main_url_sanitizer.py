#!/usr/bin/env python3 
import  sys
from    url_sanitizer import  url_sanitizer

if ( len(sys.argv)-1 < 2 ):
     print ( "Usage: main_url_sanizer.py url_with_parameters typ" )
     exit  ( 1 )
else:
     url = sys.argv[1]
     typ = sys.argv[2]

res = url_sanitizer ( url.replace(" ","?"), typ )

if ( res ):
     print ( res )
     exit  ( 0 )
else:
     print ( res )
     exit  ( 1 )
