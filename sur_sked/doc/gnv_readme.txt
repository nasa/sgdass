User guide for scheduling GNSS/VLBA experiments.

sur_sked supports scheduling VLBA and collocated GNSS stations 
that are all considered as elements of a VLBI area. The output 
of the schedule is a directory with

a) Control file file for NRAO SCHED program, file  EEEEEE.key, 
   where EEEEE is experiment file

b) station definition file gnv_stations.txt

c) station location  file gnv_stations.txt

d) frequency definition  file gnv_freq.txt 

e) leapsecond file in SPICE format leapsec.tls        

f) Ephemeride files in two-line ephemeride format (TLE) with
   extension .tle for all observed GNSS satellites

These files are uploaded to NRAO.

I. Preparation files needed for scheduling
==========================================

 1) Install sched in spicy mode.

    1.1) Install naif-toolkit of 2022.01.03
         https://naif.jpl.nasa.gov/pub/naif/toolkit/FORTRAN/PC_Linux_gfortran_64bit/packages/toolkit.tar.Z
         mv      toolkit.tar.Z  /incoming/naif-toolkit-20220103.tar.Z
         mkdir /dist/naif-20220103
         cd    /dist/naif-20220103
         tar -zxvf /incoming/naif-toolkit-20220103.tar.Z
         cd toolkit
         setenv TKLINKOPTIONS "-m64"
         setenv F_COMPILEOPTIONS "-m64 -fPIC -c -std=legacy -Wno-character-truncation -fno-underscoring"
         ./makeall.csh >& make.log
         gfortran -shared -Wl,--whole-archive,-soname,libspice.so.1 -o /opt64/lib/libspice.so.1 lib/spicelib.a lib/support.a -Wl,-no-whole-archive
         ln -s /opt64/lib/libspice.so.1 /opt64/lib/libspice.so
         cp -pv exe/* /opt64/bin/


    1.2) Install sched-12.0
         cd /incoming
         wget ftp://ftp.aoc.nrao.edu/pub/sched/sched_12.1/sched_12.1.tar.gz
         wget http://astrogeo.org/patches/sched-11.8_longazel.patch
         wget http://astrogeo.org/patches/sched-11.8_satdebug.patch
         wget http://astrogeo.org/patches/sched-11.8_exit.patch
         cd /dist
         tar -xvf /incoming/sched_12.1.tar.gz
         setenv SCHED /dist/sched-12.1
         cd /dist/sched-12.1
         patch -Np0 -i /incoming/sched-11.8_longazel.patch
         patch -Np0 -i /incoming/sched-11.8_satdebug.patch
         patch -Np0 -i /incoming/sched-11.8_exit.patch
         cd src
         sed -i "s@-fno-backslash:@-fno-backslash -fno-underscoring -fallow-argument-mismatch:@g"   configureLinuxSpice64gfortran
         sed -i 's@-L\\$(LPGPLOT) -lpgplot@/opt64/lib/libpgplot.a@g'  configureLinuxSpice64gfortran
         sed -i 's@SATLD = ../lib/LINUX64GFORTRAN/spicelib.a ../lib/LINUX64GFORTRAN/support.a@SATLD = -L /opt64/lib -lspice -lpng@g' configureLinuxSpice64gfortran
         csh ./configureLinuxSpice64gfortran
         make clean
         make
         cp ../bin/linuxsp64gf/sched /opt64/bin/
         chmod o+rx /dist/sched-12.1
         chmod o+rx /dist/sched-12/1/catalogs
         chmod o+r /dist/sched-12.1/catalogs/*

 2) Copy file celestrak_url_list.txt from `tle_inq --root`/share to /cont/ :
    cp `tle_inq --root`/share/celestrak_url_list.txt /cont/
 
 3) Copy file tle_download.py and tle_auto_download.sh from 
    `tle_inq --root`/scripts/ to /auto/

    cp `tle_inq --root`/scripts/tle_download.py /auto/
    cp `tle_inq --root`/scripts/tle_auto_download.sh /auto/
 
 4) Copy directory tree from http://astrogeo.org/tle/ to /tle/

 5) Copy /tle/gnss from http://astrogeo.org with command
    wget -q -r -nH --cut-dirs=2 --reject='*.htm*' http://astrogeo.org/tle/gnss/ /tle/gnss/

 
 6) Execute /auto/tle_auto_download.sh
 
 7) Put execution of /auto/tle_auto_download.sh every night by cron
 
 8) Check the following files
 
    /apr/sta/glo.sit
    /apr/sta/glo.vel
    /apr/sta/station.desc
    /vlbi/vtd_data/gns_sat_table.txt
 
    If `vtd_inq --root`/share/glo.sit, `vtd_inq --root`/share/station.vel, 
    and `vtd_inq --root`/share/station.desc are new, copy them to 
    /apr/sta/glo.sit, /apr/sta/glo.vel, /apr/sta/station.desc, and
    /vlbi/vtd_data/gns_sat_table.txt
 
    NB: These files are not automatically updated when a new version of 
        VTD is installed.
 
 9) Check /apr/sta/glo.sit, /apr/sta/glo.vel, /apr/sta/station.desc
    They should have definitions of GNSS stations that are collocated with
    VLBI.

10) rsync -av `sur_sked_inq --root`/share/stp /cont/
 
11) Download http://astrogeo.org/vlbi/gnv  to /vlbi/gnv/

12) Run `sur_sked_inq --root`/auxil/gen_hrtr_files.py

13) Check the following files in /vlbi/gnv

    1) gnv_satellites.txt  # Are all satellites defined?
    2) gnv_stations.txt    # Are all VLBA stations and GNSS antennas defined?
    3) gnv_locations.txt   # Is position of all VLBA stations and GNSS antennas is correct?
    4) gnf_freq.txt        # Are all VLBA stations and GNSS antennas defined?

14) Add a record in gnv_master.txt.

    A record consists of five words separated by one or more blanks

    1st word: low case experiment date
    2nd word: UTC time tag for the experiment nominal start in YYYY.MM.DD_hh:mm:ss
    3rd word: UTC time tag for the experiment nominal stop  in YYYY.MM.DD_hh:mm:ss
    4rd word: A comma-separated list of station names or a special character @ followed
              by file name that has station names
    5rd word: A comma-separated list of satellite names in 3-letter long IGS
              notation or a special character @ followed by file name that has satellites 
              names

    Example:

     us008z  2025.12.24_22:00:00  2025.12.24_03:00:00  BRVB,FDV3,HNVB G02,G03,G04,G05,G06
     us008c  2025.10.27_17:00:00  2025.10.28_03:00:00  @/vlbi/us008c/us008c_station.names @/vlbi/us008c/us008c_source.names

   Station file format

# STATION-NAMES.  Format version of 2006.01.06
#
#  1-8   A2     Correlator station name
# 10-17  A8     IVS station name
# 21-32  F12.3  X-coordinate in meters 
# 37-48  F12.3  Y-coordinate in meters 
# 53-64  F12.3  Z-coordinate in meters 
#

    Both correlator station name and the IVS station names should be defined 
    in /vlbi/gnv/gnv_stations.txt

    Example:

    # STATION-NAMES.  Format version of 2006.01.06
    #
    #  1-8   A2     Correlator station name
    # 10-17  A8     IVS station name
    # 21-32  F12.3  X-coordinate in meters 
    # 37-48  F12.3  Y-coordinate in meters 
    # 53-64  F12.3  Z-coordinate in meters 
    #
    #  Characters in columns 65 and after are considered as comments
    #
    FD       FD-VLBA    -1324009.129    -5332181.970     3231962.472   00 00 00 itrf2000.sit
    BR       BR-VLBA    -2112065.021    -3705356.515     4726813.769   00 00 00 gsf_2025c
    HC       RTS2       -1330738.244    -5328056.522     3236477.559   00 00 00  # dummy value

   Satellite file format

# SOURCE-NAMES  v 3.0 2025.09.06
#
#      1:8   A8   object name 
#     11:15  I5   NORAD id
#     43:43  A1   class:
#                 D -- near-zone object with oribt found in a given directory
#                 F -- near-zone object with orbit found in a given file
#     46:173 A128 either a directory with orbits of a file with an orbit

   Example:

   # SAT     NORAD_id                      t d  File or directory
   #
   G01       37753                         E D  /vlbi/us008b/sp3  
   G02       28474                         E F  /vlbi/us008b/sp3

   NB: only the object name and NORAD ID are parsed for schedule preparation.

15) Examine file /vlbi/gnv/gnv_def.txt
   
    In particualar, you may need to upate sched_root keyword.

16) Generate the schedule with command

    gen_gnv_sched.py -c /vlbi/gnv/gnv_def.txt -e EEEEE -v 1
    where EEEEE is the experiment name defined in the gns_master.txt
   
17) Schedule files will be found in /vlbi/EEEEE directory, where EEEEE is the 
    directory name. Results of trial run of the schedule through sched will be
    found in /vlbi/EEEEE/sched. The export schedule files for NRAO will be found
    in /vlbi/EEEEE/export. NB: Program /vlbi/EEEE/EEEE_run_sched.csh runs sched.

18) Examine /vlbi/EEEEE/sched/EEEEE.sum file.

19) Submit contents of /vlbi/EEEEE/export to NRAO.

