#!/usr/bin/python3 
import os, sys, string, subprocess, random, glob
from   malo_subs         import *

def malo_set_catcha ( date_req, mode, frame, model, service, \
                      n_sta, start_date, stop_date, remote_addr, \
                      malo_http, email ):

    config = config_class()
    random.seed ( 0.128732*random.random() + 0.871268*random.random() )

    pla_dir = glob.glob ( config.planet_dir + "/*.jpg" )
    pla_list = []

    com_str = "montage -border 0 -geometry 128x -tile 8x2 "

    len_pla_dir = len(pla_dir)
    for i in range(0,len_pla_dir):
        ind = int( random.random()*(len_pla_dir-i) )
        if ( pla_dir[ind].find("Jupiter") > 0 ):
             ind_jup = i
        com_str = com_str + " " + pla_dir[ind] 
        pla_dir.remove ( pla_dir[ind] )

    ani_dir = glob.glob ( config.animal_dir + "/*.jpg" )
    ani_list = []

    len_ani_dir = len(ani_dir)
    for i in range(0,len_ani_dir):
        ind = int( random.random()*(len_ani_dir-i) )
        if ( i == ind_jup ):
             ib = ani_dir[ind].rfind("/")+1
             ie = ani_dir[ind].rfind(".jpg")
             word = ani_dir[ind][ib:ie].lower()
        if ( i < config.num_pla ):
             com_str = com_str + " " + ani_dir[ind] 
        ani_dir.remove ( ani_dir[ind] )
    
    catcha_image = config.req_dir + "/" + date_req + "/catcha.jpg"
    com_str = com_str + " " + catcha_image

    # print ( "com_str = ", com_str  )

    os.system ( com_str )

    catcha_txt = config.catcha_dir + "/" + date_req + ".txt"
    f = open ( catcha_txt, 'w' )
    f.write ( word )
    f.close()

    com_str = com_str + " " + catcha_image

    catcha_html = config.req_dir + "/" + date_req + "/catcha.html"

    f = open ( catcha_html, 'w' )
    f.write ( '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">\n' )
    f.write ( '<HTML><HEAD> <META HTTP-EQUIV="Content-Type" content="text/html; charset=iso-8859-1">\n' )
    f.write ( '</HEAD><BODY>\n' )
    f.write ( 'In order to proceed, please idenfiy the largest planet of the Solar System \n' )
    f.write ( 'at the upper row and type what you see at the low row, just beneath that \n' )
    f.write ( 'planet.\n' )
    f.write ( '<P>\n' )
    f.write ( '<IMG WIDTH=90% SRC="' + config.req_html + '/' + date_req + '/' + 'catcha.jpg">\n' )
    f.write ( '<P>\n ')
    f.write ( '<FORM NAME="malo_catcha"\n' )
    f.write ( '      ACTION="/cgi-bin/malo_check_catcha.py" \n' )
    f.write ( '      METHOD="get">\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="date_req" VALUE="' + date_req + '" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="model" VALUE="' + model + '" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="mode" VALUE="' + mode + '" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="frame" VALUE="' + frame + '" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="service" VALUE="' + service + '" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="n_sta" VALUE="%d' % n_sta + '" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="start_date" VALUE="' + start_date + '" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="stop_date" VALUE="' + stop_date + '" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="remote_addr" VALUE="' + remote_addr + '" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="email" VALUE="' + email + '" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="malo_http" VALUE="' + malo_http + '" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="silent" VALUE="no" </INPUT>\n' )
    f.write ( '      <INPUT TYPE="hidden" NAME="ip_unlim" VALUE="no" </INPUT>\n' )
    f.write ( '      <P>&nbsp;\n' )
    f.write ( '      <INPUT TYPE="text"   NAME="answer" size="16" </INPUT> Your answer &nbsp; &nbsp; &nbsp; &nbsp; \n' )
    f.write ( '      <INPUT TYPE="SUBMIT" VALUE="Submit"> \n' )
    f.write ( '      <P>\n' )
    f.write ( '</FORM>\n' )
    f.write ( '<P>\n' )
    f.write ( '<HR size="1">\n' )
    f.write ( '</BODY></HTML>\n' )
    f.close()
