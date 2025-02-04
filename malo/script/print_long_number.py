#!/usr/bin/env python3
# ************************************************************************
# *                                                                      *
# *   Auxilliary routine print_long_number.py takes a long integer       *
# *   number (64 bit) from the pipe and prints it on the screen with     *
# *   adding comma separation between triplets of digits.                *
# *                                                                      *
# * # 29-JUN-2017 print_long_number.py v1.0 (c) L. Petrov 29-JUN-2017 ## *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, time, subprocess


def long_number_str ( num ):
    str = "%-18.0f" % num
    str = str.strip()
    il = len(str)
    if ( il > 15 ):
         str = str[0:il-15] + "," + str[il-15:il-12] + "," + str[il-12:il-9] + "," + str[il-9:il-6] + "," + str[il-6:il-3] + "," + str[il-3:il]
    elif ( il > 12 ):
         str = str[0:il-12] + "," + str[il-12:il-9] + "," + str[il-9:il-6] + "," + str[il-6:il-3] + "," + str[il-3:il]
    elif ( il > 9 ):
         str = str[0:il-9] + "," + str[il-9:il-6] + "," + str[il-6:il-3] + "," + str[il-3:il]
    elif ( il > 6 ):
         str = str[0:il-6] + "," + str[il-6:il-3] + "," + str[il-3:il]
    elif ( il > 4 ):
         str = str[0:il-3] + "," + str[il-3:il]
    return ( str )

#filin = sys.argv[1]
#with open ( filin ) as f:
#     buf = f.readlines()
#f.close ( )

buf=[]
for line in sys.stdin: 
    buf.append(line)

if ( len(buf[0].split()) > 0 ):
     num = float(buf[0].split()[0])
else:
     num = 0

num_str = long_number_str ( num )

print ( num_str )
