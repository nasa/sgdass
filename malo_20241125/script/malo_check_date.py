#!/usr/bin/python3 
#
import os, sys, string, datetime

def malo_check_date ( date_str, date_type ):

    if ( len(date_str) < 10 ):
         print ( 'Error: ' + \
                 date_type + " date is too short. " + \
                 "Format: YYYY.MM.DD_hh:mm:ss.s;" + \
                 " Hours, minutes and/or seconds can be omitted" )
         return None

    year_str = date_str[0:4]
    mon_str  = date_str[5:7]
    day_str  = date_str[8:10]

    if ( len(date_str) >= 13 ): 
         hrs_str = date_str[11:13]
    else: 
         hrs_str = "00"

    if ( len(date_str) >= 16 ): 
         min_str = date_str[14:16]
    else: 
         min_str = "00"

    if ( len(date_str) >= 19 ): 
         sec_str = date_str[17:len(date_str)]
    else: 
         sec_str = "00"

    if ( len(date_str) >= 21 ): 
         fra_str = date_str[21:len(date_str)]
    else: 
         fra_str = ".0"
         sec_str = sec_str + fra_str

    try: 
         year = int(year_str)
    except:
         print ( 'Error: In ' + date_type + " field year " + year_str + \
                 " in not an integer" )
         return None

    if ( year_str < "1970" or year_str > "2040" ):
         print ( 'Error: In ' + date_type + " field year " + year_str + \
                 " in date is out of range " )
         return None

    try: 
         month = int(mon_str)
    except:
         print ( 'Error: In ' + date_type + " field month " + mon_str + \
                 " in not an integer" )
         return None

    if ( mon_str < "01" or mon_str > "12" ):
         print ( 'Error: In ' + date_type + " field month " + mon_str + \
                 " in date is out of range " )
         return None
        
    try: 
         day = int(day_str)
    except:
         print ( 'Error: In ' + date_type + " field day " + day_str + \
                 " in not an integer" )
         return None

    if ( day_str < "01" or day_str > "31" ):
         print ( 'Error: In ' + date_type + " field day " + day_str + \
                 " in date is out of range " )
         return None

    if ( hrs_str[0:1] < "0" or hrs_str[0:1] > "2" or \
         hrs_str[1:2] < "0" or hrs_str[1:2] > "9" or \
         hrs_str      > "23"                         ):
         print ( 'Error: In ' + date_type + " field hour " + hrs_str + \
                 " in date is out of range " )
         return None

    if ( min_str[0:1] < "0" or min_str[0:1] > "5" or \
         min_str[1:2] < "0" or min_str[1:2] > "9" or \
         min_str > "59"                              ):
         print ( 'Error: In ' + date_type + " field minute " + min_str + \
                 " in date is out of range " )
         return None

    if ( sec_str[0:1] < "0" or sec_str[0:1] > "5" or \
         sec_str[1:2] < "0" or sec_str[1:2] > "9" or \
         sec_str > "59.999999"                       ):
         print ( 'Error: In ' + date_type + " field second " + sec_str + \
                 " in date is out of range " )
         return None
        
    if ( len(sec_str) > 2 ):
         if ( sec_str[2:3] != "." ): 
              print ( 'Error: In ' + date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 3 ):
         if ( sec_str[3:4] < "0" or sec_str[3:4] > "9"  ): 
              print ( 'Error: In ' + date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 4 ):
         if ( sec_str[4:5] < "0" or sec_str[4:5] > "9"  ): 
              print ( 'Error: In ' + date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 5 ):
         if ( sec_str[5:6] < "0" or sec_str[5:6] > "9"  ): 
              print ( 'Error: In ' + date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 6 ):
         sec_str = sec_str[0:6]

    out_str = year_str + "." + mon_str + "." + day_str + \
              "_" + hrs_str + ":" + min_str + ":" + sec_str

    return out_str
#
# ==================================================================
#
def check_date_condense ( date_str, date_type ):

    if ( len(date_str) < 8 ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                 date_type + " date is too short. " + \
                 "Format: YYYYMMDD_hhmmss.s;" + \
                 " Hours, minutes and/or seconds can be omitted" )
         return None

    year_str = date_str[0:4]
    mon_str  = date_str[4:6]
    day_str  = date_str[6:8]

    if ( len(date_str) >= 11 ): 
         hrs_str = date_str[9:11]
    else: 
         hrs_str = "00"

    if ( len(date_str) >= 13 ): 
         min_str = date_str[11:13]
    else: 
         min_str = "00"

    if ( len(date_str) >= 15 ): 
         sec_str = date_str[13:len(date_str)]
    else: 
         sec_str = "00"

    try: 
         year = int(year_str)
    except:
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field year " + year_str + \
                 " in not an integer" )
         return None

    if ( year_str < "1979" or year_str > "2040" ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field year " + year_str + \
                 " in date is out of range " )
         return None

    try: 
         month = int(mon_str)
    except:
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field month " + mon_str + \
                 " in not an integer" )
         return None

    if ( mon_str < "01" or mon_str > "12" ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field month " + mon_str + \
                 " in date is out of range " )
         return None
        
    try: 
         day = int(day_str)
    except:
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field day " + day_str + \
                 " in not an integer" )
         return None

    if ( day_str < "01" or day_str > "31" ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field day " + day_str + \
                 " in date is out of range " )
         return None

    if ( hrs_str[0:1] < "0" or hrs_str[0:1] > "2" or \
         hrs_str[1:2] < "0" or hrs_str[1:2] > "9" or \
         hrs_str      > "23"                         ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field hour " + hrs_str + \
                 " in date is out of range " )
         return None

    if ( min_str[0:1] < "0" or min_str[0:1] > "5" or \
         min_str[1:2] < "0" or min_str[1:2] > "9" or \
         min_str > "59"                              ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field minute " + min_str + \
                 " in date is out of range " )
         return None

    if ( sec_str[0:1] < "0" or sec_str[0:1] > "5" or \
         sec_str[1:2] < "0" or sec_str[1:2] > "9" or \
         sec_str > "59.999999"                       ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field second " + sec_str + \
                 " in date is out of range " )
         return None
        
    if ( len(sec_str) > 2 ):
         if ( sec_str[2:3] != "." ): 
              print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                      date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 3 ):
         if ( sec_str[3:4] < "0" or sec_str[3:4] > "9"  ): 
              print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                      date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 4 ):
         if ( sec_str[4:5] < "0" or sec_str[4:5] > "9"  ): 
              print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                      date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 5 ):
         if ( sec_str[5:6] < "0" or sec_str[5:6] > "9"  ): 
              print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                      date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 6 ):
         sec_str = sec_str[0:6]

    out_str = year_str + "." + mon_str + "." + day_str + \
              "_" + hrs_str + ":" + min_str + ":" + sec_str

    return out_str
#
# ==================================================================
#

def parse_solve_date ( date_str, date_type ):

    if ( len(date_str) < 10 ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> ' + \
                 date_type + " date is too short. " + \
                 "Format: YYYY.MM.DD_hh:mm:ss.s;" + \
                 " Hours, minutes and/or seconds can be omitted" )
         return None

    year_str = date_str[0:4]
    mon_str  = date_str[5:7]
    day_str  = date_str[8:10]

    if ( len(date_str) >= 13 ): 
         hrs_str = date_str[11:13]
    else: 
         hrs_str = "00"

    if ( len(date_str) >= 16 ): 
         min_str = date_str[14:16]
    else: 
         min_str = "00"

    if ( len(date_str) >= 19 ): 
         sec_str = date_str[17:len(date_str)]
    else: 
         sec_str = "00"

    if ( year_str < "1979" or year_str > "2040" ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field year " + year_str + \
                 " in date is out of range " )
         return None

    if ( mon_str < "01" or mon_str > "12" ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field month " + mon_str + \
                 " in date is out of range " )
         return None
        
    if ( day_str < "01" or day_str > "31" ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field day " + day_str + \
                 " in date is out of range " )
         return None

    if ( hrs_str[0:1] < "0" or hrs_str[0:1] > "2" or \
         hrs_str[1:2] < "0" or hrs_str[1:2] > "9" or \
         hrs_str      > "23"                         ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field hour " + hrs_str + \
                 " in date is out of range " )
         return None

    if ( min_str[0:1] < "0" or min_str[0:1] > "5" or \
         min_str[1:2] < "0" or min_str[1:2] > "9" or \
         min_str > "59"                              ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field minute " + min_str + \
                 " in date is out of range " )
         return None

    if ( sec_str[0:1] < "0" or sec_str[0:1] > "5" or \
         sec_str[1:2] < "0" or sec_str[1:2] > "9" or \
         sec_str > "59.999999"                       ):
         print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                 date_type + " field second " + sec_str + \
                 " in date is out of range " )
         return None
        
    if ( len(sec_str) > 2 ):
         if ( sec_str[2:3] != "." ): 
              print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                      date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 3 ):
         if ( sec_str[3:4] < "0" or sec_str[3:4] > "9"  ): 
              print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                      date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 4 ):
         if ( sec_str[4:5] < "0" or sec_str[4:5] > "9"  ): 
              print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                      date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 5 ):
         if ( sec_str[5:6] < "0" or sec_str[5:6] > "9"  ): 
              print ( '<FONT COLOR="A04030"><B> Error: </B></FONT> In ' + \
                      date_type + " field second " + sec_str + \
                      " in date is out of range " )
              return None

    if ( len(sec_str) > 6 ):
         sec_str = sec_str[0:6]

    year = int(year_str)
    mon  = int(mon_str)
    day  = int(day_str)
    hrs  = int(hrs_str)
    min  = int(min_str)
    sec  = int(float(sec_str))

    return datetime.datetime( year, mon, day, hrs, min, sec )
