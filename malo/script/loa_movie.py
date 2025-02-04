#!/usr/bin/env python
# ************************************************************************
# *                                                                      *
# *   Program loa_movie.py generates a loading movie.                    *
# *                                                                      *
# * ### 24-APR-2015  load_movie.py  v1.0 (c)  L. Petrov  25-APR-2015 ### *
# *                                                                      *
# ************************************************************************
import sys, os, shutil, subprocess
import datetime
import optparse 
import malo_check_date

loa_movie__label = "loa_movie.py v 1.0 of 2015.04.24"
plot_gif_dir = "/s0/temp/plot_gif"
plot_jpg_dir = "/s0/temp/plot_jpg"
output_avi = "/s0/temp/temp.avi"
fs_rate    = "10"
num_core   = "16"
fil_temp_com = "/tmp/plot.com"

def exe ( command ):
#"""
#    Spawn a supborcess, execute command in the context of the suborpess,
#    wait for its completion and return completion code and results.
#"""
    words = command.split()
    (ret, out) = subprocess.getstatusoutput ( command )
    return ( ret, out.split ( "\n" ) )
#
# ------------------------------------------------------------------------
#
def load_movie ( indir, filout, date_beg, date_end, comp, qual, ivrb ):

    if ( comp == "u" ):
         map_par = "1 1 1 37 -2"
    elif ( comp == "e" ):
         map_par = "2 1 1 43 -2"
    elif ( comp == "n" ):
         map_par = "3 1 1 43 -2"

    filin_list = os.listdir( indir )
    filin_list = sorted ( filin_list )

#
# --- Generate the list of maps in netcdf format
#
    nc_list = []
    for file in filin_list:
        id = file.rfind(".nc")
        if ( id < 10 ): continue
        if ( file[id-13:id] < date_beg ): continue
        if ( file[id-13:id] > date_end ): continue
        if ( ivrb > 3 ):
             print ( "file: ", file ) 
        nc_list.append ( indir + "/" + file )

    if ( ivrb > 1 ):
         print ( "%d input nc-files have been found" % len(nc_list) )

#
# --- Generate the list of commands for genation of maps in gif format
#
    gif_list = []
    com_list = []
    ft = open ( fil_temp_com, "w" )
    for file in nc_list:
        id = file.rfind("/" )
        ie = file.rfind("." )
        filgif = plot_gif_dir + file[id:ie] + ".gif"
        gif_list.append ( filgif )
        com = "load_map_view" + " " + \
              file + " " + \
              map_par + " " + \
              filgif
        print ( com, file=ft )
    ft.close()

#
# --- Generate maps in parallel
#
    if ( ivrb > 1 ):
         print ( "Generate maps" )
    par_com = "cat " + fil_temp_com + " | parallel -P " + num_core

    (ret, out) = exe ( par_com )
    if ( ret != 0 ):
         print ( "Error in computing loading map" )
         print ( out )
         exit ( 1 )
    os.unlink ( fil_temp_com )

#
# --- Generate the list of commands for genation of maps in jpg format
#
    ft = open ( fil_temp_com, "w" )
    jpg_list = []
    for file in gif_list:
        id = file.rfind("/" )
        ie = file.rfind("." )
        filjpg = plot_jpg_dir + file[id:ie] + ".jpg"
        jpg_list.append ( filjpg )
        com = "convert -quality 100 " + file + " " + filjpg        
        print ( com, file=ft )
    ft.close()

#
# --- tranform maps in jpg format in parallel
#
    if ( ivrb > 1 ):
         print ( "Convert maps in jpg" )
    par_com = "cat " + fil_temp_com + " | parallel -P " + num_core
    (ret, out) = exe ( par_com )
    if ( ret != 0 ):
         print ( "Error in computing loading map" )
         print ( out )
         exit ( 1 )
    os.unlink ( fil_temp_com )

    (ret, out) = exe ( "identify " + jpg_list[1] )

    width  = out[0].split()[2].split("x")[0]
    height = out[0].split()[2].split("x")[1] 

    if ( ivrb > 2 ):
         print ( "width  = " + width )
         print ( "height = " + height )

    if ( qual == 1 ):
         mencode_opt = "mpeg4:vhq:threads=" + num_core
    elif ( qual == 2 ):
         mencode_opt = "msmpeg4v2:vbitrate=3200:threads=" + num_core

    com = "mencoder \"mf://" + \
          plot_jpg_dir + \
          "/*.jpg\" -mf type=jpg:w=" + width + ":h=" + height + \
          ":fps=" + fs_rate + \
          " -ovc lavc -lavcopts vcodec=" + mencode_opt + \
          " -nosound -of avi -ofps " + fs_rate + \
          " -o " + output_avi
#
# --- Generate the movie in avi format: the first pass
#
    if ( ivrb > 1 ):
         print ( "The first  pass to genreate movie" )
    (ret, out) = exe ( com )
    if ( ret != 0 ):
         print ( "com= " + com )
         print ( "Error in computing loading map" )
         print ( out )
         exit ( 1 )

    if ( qual == 1 ):
#
# ------ The second pass to encode in mpg
#
         mencode_opt = "msmpeg4v2:threads=" + num_core
         com = "mencoder -ovc lavc -lavcopts vcodec=" + mencode_opt + ":vpass=1" + \
              " -o " + filout + " " + output_avi

         if ( ivrb > 1 ):
              print ( "The second pass to genreate movie" )
         (ret, out) = exe ( com )
         if ( ret !=0 ):
              print ( "(E) ", ret, " Failure in executing ", com )
              print ( ret )
              exit ( 1 )

#
# ------ The third pass to encode in avi
#
         mencode_opt = "msmpeg4v2:threads=" + num_core
         com = "mencoder -ovc lavc -lavcopts vcodec=" + mencode_opt + ":vpass=2" + \
               " -o " + filout + " " + output_avi
         if ( ivrb > 1 ):
              print ( "The third  pass to genreate movie" )
         (ret, out) = exe ( com )
         if ( ret !=0 ):
              print ( "(E) ", ret, " Failure in executing ", com )
              print ( ret )
              exit ( 1 )
         os.unlink ( output_avi )
    else:
         shutil.copyfile ( output_avi, filout )
         os.unlink ( output_avi )

    for file in gif_list:
        os.unlink ( file )
    for file in jpg_list:
        os.unlink ( file )
        
    return 0
#
# ------------------------------------------------------------------------
#
#
# ------------------------------------------------------------------------
#
def main():

    opts = optparse.OptionParser( version=loa_movie__label )

    opts.add_option ( "-i", "--input_dir", action="store", \
                      dest="indir", \
                      metavar="NAME", \
                      help="input_directory" )


    opts.add_option ( "-o", "--output_file", action="store", \
                      dest="filout", \
                      metavar="NAME", \
                      help="output_file" )

    opts.add_option ( "-b", "--date_beg", action="store", \
                      dest="date_beg", \
                      metavar="NAME", \
                      help="start_date" )

    opts.add_option ( "-e", "--date_end", action="store", \
                      dest="date_end", \
                      metavar="NAME", \
                      help="stop_date" )

    opts.add_option ( "-c", "--component", action="store", \
                      dest="component", \
                      default="u", \
                      metavar="NAME", \
                      help="Component: u, e, or n" )

    opts.add_option ( "-q", "--quality", action="store", \
                      dest="quality", \
                      type="int", \
                      default=1, \
                      metavar="NAME", \
                      help="Quailty: 1(coarse) or 2(fine)" )

    opts.add_option ( "-v", "--verbosity", action="store", \
                      dest="ivrb", \
                      default=0, \
                      type="int", \
                      metavar="NAME", \
                      help="Verbosity level" )

#
# --- Get and parse options
#
    opts, args = opts.parse_args()
    
#
# --- Check option values
#
    if ( opts.ivrb == None ):
         opts.ivrb = 0

    if ( opts.indir == None ):
         print ( "Input directory is not specified" )
         exit ( 1 )

    if ( not os.path.isdir(opts.indir) ):
         print ( "Input directory ", opts.indir, " does not exist" )
         exit ( 1 )

    if ( opts.filout == None ):
         print ( "Output file is not specified" )
         exit ( 1 )

    if ( opts.date_beg == None ):
         print ( "Begin date is not specified" )
         exit ( 1 )

    if ( opts.date_end == None ):
         print ( "End date is not specified" )
         exit ( 1 )

    if ( not ( opts.component == "u" or opts.component == "e" or opts.component == "n" ) ):
         print ( "Component can be only u, e, or n, not " + opts.component  )
         exit ( 1 )

    date_beg = check_date ( opts.date_beg, "begin date" )
    if ( date_beg == None ):
         exit ( 1 )

    date_end = check_date ( opts.date_end, "end date" )
    if ( date_beg == None ):
         exit ( 1 )

    if ( not ( opts.quality == 1 or opts.quality == 2 ) ):
         print ( "Wrong quality %d -- supported quality is 1 or " % opts.quality )
         exit ( 1 )

    date_beg = date_beg[0:4] + date_beg[5:7] + date_beg[8:13] +date_beg[14:16] 
    date_end = date_end[0:4] + date_end[5:7] + date_end[8:13] +date_end[14:16] 

    ret = load_movie ( opts.indir, opts.filout, date_beg, date_end, \
                       opts.component, opts.quality, opts.ivrb )

if __name__ == "__main__":
    main()
