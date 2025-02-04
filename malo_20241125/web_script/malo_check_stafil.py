#!/usr/bin/python3 
#
#  Last update: 2016.11.09_11:42:02
# 
import os, sys, string, math

fil = "/tmp/fil.fil"

REA     = 6378136.3
FE      = 1.0/298.2570   # Earth's flattening
EXC_SQ  = 2.0*FE - FE**2 # Earth's eccentricity
PI      = 3.141592653589793
PI2     = 2.0*PI
P2I     = PI/2.0

def malo_check_sta_inp ( sta_buf ):
    n_sta = 0
    for i in range(0,len(sta_buf)):
        sta_buf[i].strip("\n")
        if ( len(sta_buf[i].split()) < 1 ): continue
        if ( sta_buf[i][0:1] == "#" ): continue
        if ( sta_buf[i].split()[0] == 'SITLIST' ): continue
        word = sta_buf[i].split()
        if ( len(word) < 4 ): 
             print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                     ' parsing the %d' % (i+1), "th line of the station file: " + \
                     sta_buf[i].replace("\n","") + \
                     " the number of words is not four as it was expected" )
             return ( "Error", n_sta )
        coo = []
        for j in range(0,3):
            try:
                coo.append ( float(word[j+1]) )
            except:
                print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                        ' parsing the %d' % (i+1), "th line of the station file: " + \
                        sta_buf[i].replace("\n","") + \
                        " the %d" % (j+1), " word should be a float number" )
                return ( "Error", n_sta )
         
#
# ----- Computation of longitude
#
        if ( coo[0] > 1.e-8 ):
             Lambda = math.atan ( coo[1]/coo[0] )
        else: 
             Lambda = P2I
        if ( coo[0] < 0.0 ): Lambda = PI  + Lambda
        if ( Lambda < 0.0 ): Lambda = PI2 + Lambda

        p = math.sqrt ( coo[0]**2 + coo[1]**2 )
        if ( abs(p) < 1.0e-8 ): p = 1.0e-8
        rd = math.sqrt ( p**2 + coo[2]**2 )
#
# ----- Computation of geocentric latitude
#
        Phi_gcn = math.atan ( coo[2]/p )
#
# ----- Comutation of geodetic latitude
#
        mu = math.atan ( coo[2]/p * ( (1.0 - FE) + EXC_SQ*REA/rd ) )

        Phi_gdt = math.atan( ( (1.0 - FE)*coo[2] + EXC_SQ*REA*math.sin(mu)**3 ) / \
                             ( (1.0 - FE)*(   p  - EXC_SQ*REA*math.cos(mu)**3 ) ) )
#
# ----- Computation of height above geoid
#
        H_ell = p*math.cos(Phi_gdt) + coo[2]*math.sin(Phi_gdt) -  \
                REA*math.sqrt( 1.0 -  EXC_SQ*math.sin(Phi_gdt)**2 )

        if ( H_ell > 7000.0 ):
             print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                     ' checking coordinates of station ', word[0], \
                     ' -- its coordinates ', word[1], word[1], word[2],
                     ' seemed to be more than 7000 meters above the sea level' )
             return ( "Error", n_sta )

        if ( H_ell < -1000.0 ):
             print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                     ' checking coordinates of station ', word[0], \
                     ' -- its coordinates ', word[1], word[1], word[2],
                     ' seemed to be more than 1000 meters below the sea level' )
             return ( "Error", n_sta )

#        print ( "coo: ", coo[0], coo[1], coo[2], ' H_ELL: ', H_ell )

    out_buf = []
    out_buf.append ( "SITLIST Format  Version 2003.07.31" )
    out_buf.append ( "#" )
    out_buf.append ( "# Name   X-coordinate Y-coordinate Z-coordinate Date_beg   Date_end   Tb" )
    out_buf.append ( "#" )

    for i in range(0,len(sta_buf)):
        if ( sta_buf[i][0:1] == "#" ): continue
        if ( len(sta_buf[i].split()) < 1 ): continue
        if ( sta_buf[i].split()[0] == 'SITLIST' ): continue
        word = sta_buf[i].split()
        coo = []
        n_sta = n_sta + 1
        for j in range(0,3):
            coo.append ( float(word[j+1]) )
            
        out_buf.append ( "%-8s %12.3f %12.3f %12.3f 1976.01.01 end        U" % \
                         (word[0], coo[0], coo[1], coo[2] ) )
    return ( out_buf, n_sta )

