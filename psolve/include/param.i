!
! This file is generated automatically by use_local.f from
!      template_file: /mk5/include/param.templ
!      local customization file: /mk5/local/solve.lcl
!
!      PLEASE DON'T EDIT THIS FILE!
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!! Param.templ
!
!     This include contains system dependent parameters for the VLBI
!     data reduction software preprocessing program.  All such
!     parameters from Dbedit program and the Apriori (including the
!     Apriori, Skeleton, and Ut1pm programs) library and Sscat library
!     (and program) have been placed here and the original routines
!     changed appropriately to use them.
!
!
! NOTE FOR PROGRAMMERS:
!
!  1)  Lines which starts from & are meta-definitions and they are tied with
!            local customization file.
!      The value of the parameter is an installation-specific.
!      Format of meta-commands:
!      a) &V&  <param_name> -- definition of the name of non-character type
!      b) &C0& <param_name> -- definition of the name of character type.
!                               No restrictions on the length of the line
!                               (but the line should not be empty)
!      c) &C<max_length>& <param_name> -- definition of the name of character
!                                         type. But the value should not have
!                                         length more than <max_length>
!                                         characters.
!      Actual values are are taken from the local customization file and
!      the output include file is generated.
!
!  2) Although each set of parameters is labeled, an expresion cannot
!     appear more than once in this file. That is, an expression in a
!     given set of parameter(s) cannot be used in another set, as that
!     would be a duplicate declaration of identifier or definition.
!
!  Programmer:
!
!    K. Wilcox		92.03.30  Creation.
!
!  Modifications:
!
!    B. Archinal	92.04.09  Comments improved.  Some important
!                                 additional parameters added.
!    B. Archinal        92.06.10  MXSSCE added for SSCAT program.
!    K. Wilcox          92.09.15  'KL' added to expression in FRRGEX.
!    B. Archinal        92.10.01  MAXNFR increased from 5000 to 20000.
!    B. Archinal        93.01.05  DFLSCR added for show_ut1pm.f.
!                                 MXDBSIT increased from 10 to 20.
!                                 Also noted that skeleton.f uses
!                                 DFSKULL, and getutpm_updt.f uses
!                                 DFLEAP.
!    D. Gordon          93.08.10  Parameters for Pwxcb added (default
!                                 cable and weather file path names and
!                                 number of characters in path).
!    M. White           94.01.13  Luout & Luerr standard outputs
!                                 variables added (for dbedit.f,
!                                 sumry.f).  FRREGX and RTREGX dropped
!                                 and moved to ../dbedit/open_disk.f for
!                                 SunOS compatibility.
!    B. Archinal        94.02.15  Note added that NGS_OPTION usage
!                                 has ceased.  Also added note of Sscat
!                                 usage of MXDBSIT and MXDBSTR.
!    B. Archinal        94.05.27  Calc 8 section added at end.
!    B. Archinal        95.02.21  "MAXNFR" changed to i*4, increased
!                                 from 20000 to 32768.  This is maximum
!                                 possible without extensive changes
!                                 to Dbedit (i*4 buffers, counters
!                                 needed).
!    B. Archinal        95.05.25  Default skeleton database name changed
!                                 from $$SKELETON to $SKULL1994 for
!                                 Calc 8.x use (at suggestion of DGG).
!                                 Comments updated for GSFC's "BLOKQ",
!                                 "DFLEAP", "DFUTNM", "DFSSCP", MXSSCD,
!                                 and "path_pwxcb".
!    "  "               98.03.24  Noted that "MAXNFR" had already been
!                                 increased at some point from 32768 to
!                                 100000.
!    K. Baver           98.10.27  Add new parameter, Y2K_START_YEAR.
!    L. Petrov          99.02.17  Added optional compilation feature for
!                                 support of analysis centers
!                                 GIUB GSFC IRACNR ITISCNR MATECGS USNO
!    B. Archinal      1999.05.20  Upped MXDBSTR from 300 to 1000 and
!                                 MXDBSIT from 20 to 32.
!    L. Petrov        1999.09.20  Added optional compilation for LEIPZIG
!    L. Petrov          1999.11.08  Updated the name of ephemerides file for
!                                   GIUB and GSFC
!    L. Petrov          1999.11.08  Added paramegter IRECL
!    L. Petrov          1999.12.14  Default skeleton database name changed
!                                   from $SKULL1994 to $SKULL2000 for
!                                   Calc 9.x use (at suggestion of DGG).
!    L. Petrov          2000.01.28  Added templates values for GCCFA, KASHIMA,
!                                   VALENCIA. Currently they DON'T correspond
!                                   to the actual values and are only for
!                                   smooth compling
!    L. Petrov          2000.02.01  Added preferences for MPIFR (Bonn).
!    B. Archinal        2000.02.02  Upped MAXNMLN from 32 to 40 and
!                                   MAX_STR from 80 to 132, for Mark IV.
!    Calvin             2000.04.19  Added preferences for NRCAN (GSD/Canada)
!    L. Petrov          2000.04.20  changed PATH_PWXCB for GSFC
!    L. Petrov          2000.05.24  Moved installation-specific customization
!                                   to local file
!    L. Petrov          2000.05.31  Removed MAINHIST and MNHSTLEN
!    L. Petrov          2000.06.05  Added parameter MAX_CHN, MAX_FRQ
!    L. Petrov          2000.06.14  Added parameter MIN_X_FREQ
!    L. Petrov          2000.07.17  Set DFEPHM to .FALSE.
!    L. Petrov          2000.10.31  Made parameter CALSTR customizable through
!                                   installation-dependent local file
!    L. Petrov          2001.04.16  Added parameters SESSION_ROOT, MASTER_DIR,
!                                   WGET_EXE
!    L. Petrov          2001.06.22  Renamed parameters MASTER_DIR and WGET_EXE
!                                   to MASTER_DIR_DEF, WGET_EXE_DEF
!    L. Petrov          2006.03.14  Added  JPL_405, A_TILTS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!     *****************************************************
!     *  $$$ $$$ $$$  $ $$$ $$$  $     $$$ $ $   $$$ $$$  *
!     *  $ $ $ $ $ $  $ $ $ $ $  $     $   $ $   $   $    *
!     *  $$$ $$$ $$$  $ $ $ $$$  $     $$$ $ $   $$$ $$$  *
!     *  $ $ $   $ $  $ $ $ $ $  $     $   $ $   $     $  *
!     *  $ $ $   $  $ $ $$$ $  $ $     $   $ $$$ $$$ $$$  *
!     *****************************************************
!
!     ******************************************************
!     *   THIS SET OF PARAMETERS IS  FOR APRIORI.F         *
!     ******************************************************
!
! === Parameters:
!
! --- MAXNDB is the number of databases that can be handled in one shot.
! --- DFVERB is the default verbosity.
! --- DFx    is the default value for each update activity when NO activities
! ---        are explicitly specified.
!
      INTEGER*2 MAXNDB, DFVERB
      LOGICAL*1 DFEPHM, DFUTPM, DFSKLL
      PARAMETER ( MAXNDB = 1000   , &
     &            DFVERB = 2      , &
     &            DFEPHM = .FALSE., &
     &            DFUTPM = .TRUE. , &
     &            DFSKLL = .TRUE.     )
!
!     ******************************************************
!     *  THIS PARAMETER IS FOR GET_BLOKQ.F                 *
!     ******************************************************
!
! === Parameters:
!
! --- BLOKQ specifies the default pathname for the blokq.dat file.
!
      CHARACTER BLOKQ*23
      PARAMETER ( BLOKQ = '/vlbi/apriori/blokq.dat' ) ! Local customization
!
!     ******************************************************
!     *  THESE PARAMETERS ARE SET FOR GET_EPHEM.F          *
!     ******************************************************
!
! === Parameters:
!
! --- These are the PEP and DExxx ephemeris database default postfix
! --- strings for this system.
!
      CHARACTER DFPEPEP*7
      PARAMETER ( DFPEPEP = 'JAN01PH' ) ! Local customization
      CHARACTER DFDE200*7
      PARAMETER ( DFDE200 = 'UNX2000' ) ! Local customization
!
!     ******************************************************
!     *  THIS PARAMETER IS FOR GET_LEAPSEC.F               *
!     ******************************************************
!
!     (Also used by getutpm_updt.f.)
!
! === Parameters:
!
! --- The default name of the leapsecond file.
!
      CHARACTER DFLEAP*23
      PARAMETER ( DFLEAP = '/vlbI/apriori/ut1ls.dat' ) ! Local customization
!
!     ******************************************************
!     *  THESE PARAMETERS ARE FOR GET_SKULL_DB.F           *
!     ******************************************************
!
! === PARAMETERS:
!
! --- The maximum number of sites and sources in a single database, as
! --- well as the larger of the two numbers.  Don't change these unless
! --- you really need to.
!
! --- Note: MXDBSIT and MXDBSTR are now also used by sscat.f
! ---      (B. Archinal and V. Sagar, 94.02.15).
!
      INTEGER*2   MXDBSIT, MXDBSTR, MAXINDX
      PARAMETER ( MXDBSIT = 20,   &
     &            MXDBSTR = 1000, &
     &            MAXINDX = ( MAX ( MXDBSIT, MXDBSTR) ) )
!
!     ******************************************************
!     *   THIS PARAMETER IS FOR GET_UT1PM.F                *
!     ******************************************************
!
! === PARAMETERS:
!
! --- NGS_OPTION sets the operation of this routine to comply with
! --- the SOLVE3 program at NGS, which can't handle a variable number
! --- of points.
!
! *** NOTE:
! --- This option is no longer used in versions of get_ut1pm.f
! --- dated 94.02.07 or later.  15 sets of points will now always be
! --- used.  It is still being set here for compatibility with older
! --- versions of Apriori and Dbedit. ***
!
      LOGICAL*1   NGS_OPTION
      PARAMETER ( NGS_OPTION = .FALSE. )
!
!     ******************************************************
!     *   THIS PARAMETER IS FOR GETUTPM_UPDT               *
!     ******************************************************
!
! === PARAMETERS:
!
! --- Default update file name...
!
      CHARACTER   DFUPNM*9
      PARAMETER ( DFUPNM = 'ut1up.dat' )
!
!     ********************************************************
!     * THESE PARAMETERS ARE FOR OPEN_SKULL.F AND SKELETON.F *
!     ********************************************************
!
! === PARAMETERS:
!
! --- DFSKULL is the default skeleton database on any data reduction
! --- system. Do not change it unless you know what you are doing.  Also,
! --- the skeleton database key description for new keys is given in
! --- DFKDESC.  (Previous values of DFSKULL were "$$SKELETON for USNO
! --- A900 and HP-UX system, and GSFC A900 system.  $SKULL2000 refers to a
! --- Calc 9.x compatible skeleton database.)
!
      CHARACTER   DFSKULL*10, DFKDESC*50
      PARAMETER ( DFSKULL = '$SKULL2000'                    )
      PARAMETER ( DFKDESC = &
     & 'User-defined Skeleton Database Chain for Calc 9.x ' )
!
!     ******************************************************
!     * THIS PARAMETER IS FOR  open_ut1pm.f and ut1pm.f    *
!     ******************************************************
!
! === PARAMETERS:
!
! --- DFUTNM is the default UT1PM filename.
!
      CHARACTER DFUTNM*23
      PARAMETER ( DFUTNM = '/vlbi/apriori/ut1pm.dat' ) ! Local customization
!
!     ******************************************************
!     *  THESE PARAMETERS ARE FOR PUT_UT1PM                *
!     ******************************************************
!
! === PARAMETERS:
!
! --- These parameters are the information fields for UT1 and wobble
! --- data.
!
      CHARACTER FVTXT*63, RSTXT*65, EXTXT*64
      PARAMETER ( &
     & FVTXT = 'Final Value wobble data generated by APRIORI library.'  , &
     & RSTXT = 'Rapid Service wobble data generated by APRIORI library.', &
     & EXTXT = 'Extrapolated wobble data generated by APRIORI library.'   )
!
!     ******************************************************
!     * THESE PARAMETERS ARE FOR SHOW_UT1PM.F              *
!     ******************************************************
!
! === PARAMETERS:
!
! --- DFFLBY is the default flyby file name.
! --- DFLSCR is the default scratch flyby file name.
! ---       (the process id is appended to it during execution.)
!
      CHARACTER   DFFLBY*9, DFLSCR*20
      PARAMETER ( DFFLBY = 'flyby.erp'            )
      PARAMETER ( DFLSCR = '/tmp/ut1pm_flyby.tmp' )
!
!     ******************************************************
!     *         PARAMETER FOR UT1PM.F                      *
!     ******************************************************
!
! === PARAMETERS:
!
! --- The size of the parameter buffer.  Change this if you are having
! --- troubles with the program accepting long parameter strings --
! --- but make sure it is at least 255.
!
      INTEGER*2   PARMSZ
      PARAMETER ( PARMSZ = 1024 )
!
! === PARAMETERS:
!
! --- ASECIR is the number of arcseconds in a circle (360 deg).
!
      REAL*8      SECCON, ASECIR
      PARAMETER ( SECCON = 206264.8062470964D0 )
      PARAMETER ( ASECIR = 1.296D6             )
!
!     ******************************************************
!     *      $$  $$  $$$ $$  $ $$$    $$$ $ $   $$$ $$$    *
!     *      $ $ $ $ $   $ $ $  $     $   $ $   $   $      *
!     *      $ $ $$  $$$ $ $ $  $     $$$ $ $   $$$ $$$    *
!     *      $ $ $ $ $   $ $ $  $     $   $ $   $     $    *
!     *      $$  $$  $$$ $$  $  $     $   $ $$$ $$$ $$$    *
!     ******************************************************
!
!     ******************************************************
!     *      THESE PARAMETERS ARE FOR DBEDIT.F             *
!     ******************************************************
!
! === PARAMETERS:
!
! --- These are the default file names for the control file, absolute
! --- scratch data file, default calc runstring, enviornment variable
! --- that indicates the user's home directory, and well as the default
! --- verbosity level.
!
! --- Note especially that the directory of the scratch datafile must be
! --- large enough to hold all the raw (database) data coming from the
! --- input source.  Also, the length of DFSCRF must be 5 more than the
! --- filename length given in order to accomdate the process id which
! --- will be tacked on the name to make it unique.
!
      CHARACTER   DFCFNM*5, HOME*4
      INTEGER*2   LUIN, LUOUT, LUERR
      INTEGER*4   DFVERD
      PARAMETER ( DFCFNM = 'simon'   )
      PARAMETER ( HOME   = 'HOME'    )
      PARAMETER ( DFVERD = 2         )
      CHARACTER DFSCRF*15
      PARAMETER ( DFSCRF = '/tmp/scrdatafil' ) ! Local customization
      CHARACTER CALSTR*7
      PARAMETER ( CALSTR = 'calc 0 ' ) ! Local customization
!
! --- Standard lu numbers are set for luout and luerr.
! --- (used by dbedit.f and sumry.f).
! --- Change Luerr = 0 for Sun OS use.
!
      PARAMETER ( LUIN  = 5 )     ! standard input lu number
      PARAMETER ( LUOUT = 6 )     ! standard output lu number
      PARAMETER ( LUERR = 7 )     ! standard error lu number
!
! --- Maximal allowed number of channels. (It was 14 for Mark-3,
! --- 16 for Mark-4, but may be much larger for S-2, S-3, S-4 )
!
      INTEGER*2    MAX_CHN, MAX_FRQ
      PARAMETER  ( MAX_CHN = 16        )
      PARAMETER  ( MAX_FRQ = 2*MAX_CHN )
      REAL*8       MIN_X_FREQ
      PARAMETER  ( MIN_X_FREQ = 4.8D9 ) ! If frequency is greater or equal than
!                                       ! MIN_X_FREQ then the frequency is
!                                       ! considered as of X-band, otherwise
!                                       ! it is considered as of S-band
!
!     ******************************************************
!     *  PARAMETER FOR GET_FILTER.F                        *
!     ******************************************************
!
! === PARAMETERS:
!
! --- DFRJTF is the default absolute file name for the "rejects" file.
! --- The length of DFRJT must be 5 more than the filename length given
! --- in order to accomdate the process id which will be tacked on the
! --- name to make it unique.
!
      CHARACTER DFRJTF*12
      PARAMETER ( DFRJTF = '/tmp/rejects' ) ! Local customization
!
!     ******************************************************
!     *  PARAMETERS FOR GET_INPUT.F                        *
!     ******************************************************
!
! === Parameters:
!
! --- This is the default specification for an input medium.
! --- DFINDB -- the default input database keyname, MUST be non-blank  --
! --- setting it blank will generate an error later on.
! --- Notice that DFINVR, the default input  database version number, is
! --- character, NOT integer.
! --- DFPATH and DFTAPE are the default directory name and tape device
! --- name.  Make sure the tape  device name is complete and correct.
!
      CHARACTER   DFINDB*10, DFINVR*1, DFTAPE*11
      PARAMETER ( DFINDB = '$80JAN01XS'  )
      PARAMETER ( DFINVR = '0'           )
      PARAMETER ( DFTAPE = '/dev/rmt/0h' )
!
      CHARACTER DFPATH*4
      PARAMETER ( DFPATH = '/tmp' ) ! Local customization
!
!     ******************************************************
!     *  PARAMETERS FOR GET_OUTDB.F                        *
!     ******************************************************
!
! === PARAMETERS:
!
! --- These parameters are used to set some of the values for an
! --- output database specification.  If improperly set, they could
! --- cause the program to crash, so be careful.  A few pointers:
! --- The value of DFSTAT is the status of the output database and
! --- can be [I]nput, [O]utput, [N]ew, or [G]enerated keyname.  If it is
! --- I, N, or O the database keyname must also be given as a default,
! --- which is generally unwise, so the only value can be G.  G generates
! --- a unique keyname for the database using the first date in the
! --- database, its band, and the first letter of the username.  The
! --- value of DFLETR is used to generate a database keyname, if
! --- needed.  If it is an asterisk a random letter is used.  DFHIST is
! --- the default output database history.  DFOWNB and DFOWNE are the
! --- default begining and ending times (in Julian dates) of the output
! --- database respectively.  DFBAND is the default wavelength band of
! --- the output database data.  DFSSCT indicates whether to update the
! --- sites and sources catalog (.FALSE. means no update).
!
      CHARACTER   DFSTAT*1, DFLETR*1, DFHIST*34
      CHARACTER   DFBAND_STR*2
      REAL*8      DFOWNB,   DFOWNE
      LOGICAL*1   DFSSCT
      PARAMETER ( DFSTAT = 'G', &
     &            DFLETR = '*', &
     &            DFHIST = 'Generic database created by dbedit.', &
     &            DFOWNB = -1.D99, &
     &            DFOWNE =  1.D99, &
     &            DFBAND_STR = 'A ',   &
     &            DFSSCT = .FALSE.  )
!
!     ******************************************************
!     *     THESE PARAMETERS ARE FOR OPEN_DISK.F           *
!     ******************************************************
!
! === PARAMETERS:
!
! --- Any of these parameters can be changed without affecting the rest
! --- of the program, but please don't change anything if you don't know
! --- what you're doing.
!
!     MAXNMLN  --  The maximum length of a relative root/FRNGE
!                  file name.  If changed, character type length
!                  must also be changed.  The maximum possible length
!                  of this sort of name should be 29 characters
!                  (e.g.: "../302-161850/0906+015.gzwlkq" (Mark-3) or
!                         "../363-1830_4C39.25/CU.S.11.ockofj (Mark-4)
!                         for a root file )
!     MAX_STR  --  Essentially, the longest expected simple filename or
!                  pathname length.
!     MAXNFR   --  Maximum number of ("FRNGE") data files that can be
!                  processed in a single run.
!     SCRPATH  --  Scratch file directory.  Three temporary files are
!                  created in this directory, but they are relatively
!                  small, containing lists of filenames of all files,
!                  root files, and fringe files.
!
      INTEGER*2   MAXNMLN, MAX_STR
      INTEGER*4   MAXNFR
      PARAMETER ( MAXNMLN = 40 )
      PARAMETER ( MAX_STR =128 )
      PARAMETER ( MAXNFR  = 100000 )
      CHARACTER   SCRPATH*(MAX_STR)
      PARAMETER ( SCRPATH = '/tmp' )
!
!     ******************************************************
!     *      SSCAT FILES                                   *
!     ******************************************************
!
!     ******************************************************
!     *  THESE PARAMETERS ARE FOR sscat.f, open_ssc.f, and *
!     *  search.f.                                         *
!     *  (Also see get_skull_db.f above.)                  *
!     ******************************************************
!
! === Parameters:
!
! --- DFSSCP defines the default path for the location of the SSCAT
! ---        catalog files.
!
! --- MXSSCS is the maximum number of sites
! --- MXSSCD is the maximum number of databases that the SSCAT catalogs
! ---        can handle.
! --- MXSSCE is the number of 32 bit words needed to store information on the
! ---        MXSSCD databases.
!
! *** NOTE: These values should never be changed unless new catalogs are
!           being generated! ***
!
      INTEGER*4 MXSSCS, MXSSCD, MXSSCE
      PARAMETER  ( MXSSCS = 200 )
      CHARACTER DFSSCP*9
      PARAMETER ( DFSSCP = '/dev/null' ) ! Local customization
      PARAMETER ( MXSSCD = 5000 ) ! Local customization
!
      PARAMETER  ( MXSSCE = MXSSCD/32 + 1 )
!
!     ********************************************************
!     *  $$$ $       $  $   $ $$$$ $$$    $$$ $ $   $$$ $$$  *
!     *  $ $ $       $   $ $  $    $  $   $   $ $   $   $    *
!     *  $$$  $  $  $     $   $    $$$    $$$ $ $   $$$ $$$  *
!     *  $    $ $ $ $    $ $  $    $  $   $   $ $   $     $  *
!     *  $     $   $    $   $ $$$$ $$$    $   $ $$$ $$$ $$$  *
!     ********************************************************
!
!     ******************************************************
!     *      PWXCB FILES                                   *
!     ******************************************************
!
! === PARAMETERS:
!
! --- PATH_PWXCB is the default path name for cable and weather files.
! --- NUM_PWXCB is the number of characters in the default file names
!
      INTEGER*2    NUM_PWXCB
      CHARACTER PATH_PWXCB*64
      PARAMETER ( PATH_PWXCB = '/vlbi/wxcb                                                      ' ) ! Local customization
      PARAMETER  ( NUM_PWXCB = LEN(PATH_PWXCB) )
!
!     ******************************************************************
!     *   $$$$     $     $      $$$$    $$$     $$$$ $ $    $$$$ $$$$  *
!     *  $        $ $    $     $       $   $    $    $ $    $    $     *
!     *  $       $$$$$   $     $        $$$$    $$$$ $ $    $$$$ $$$$  *
!     *  $      $     $  $     $           $    $    $ $    $       $  *
!     *   $$$$ $       $ $$$$$  $$$$    $$$     $    $ $$$$ $$$$ $$$$  *
!     ******************************************************************
!
!     ******************************************************************
!     *      CALC 9.x FILES                                            *
!     ******************************************************************
!
! === PARAMETERS:
!
! --- JPL_EPH 1s the path name for the JPL DE/LE ephemeris file
! ---         on your system, and IRECL is the record length of that file
!
      INTEGER*4   IRECL  ! Record length (in bytes) of ephemeride file
      PARAMETER ( IRECL = 8144 ) ! Local customization
      CHARACTER JPL_EPH*23
      PARAMETER ( JPL_EPH = '/vlbi/apriori/JPL.DE403' ) ! Local customization
      CHARACTER JPL_405*38
      PARAMETER ( JPL_405 = '/vlbi/solve/apriori_files/DE405_le.jpl' ) ! Local customization
      CHARACTER A_TILTS*4
      PARAMETER ( A_TILTS = 'None' ) ! Local customization
 
! --- Starting year in 1900 for vlbi.  E.g., 70 indicates that
! ---          70 to 99 corresponds to 1970 to 1999 and
! ---           0 to 69 corresponds to 2000 to 2069
!
      INTEGER*2   Y2K_START_YEAR
      PARAMETER ( Y2K_START_YEAR = 70 )
!
      INTEGER*4   I_RECL_JPL, N_RECL_JPL, K_SIZE_JPL
      PARAMETER ( N_RECL_JPL =    4 )
      PARAMETER ( K_SIZE_JPL = 2036 )
!
!  OTHER PROGRAMS
!  ~~~~~~~~~~~~~~
!
      CHARACTER SESSION_ROOT*16
      PARAMETER ( SESSION_ROOT = '/data10/sessions' ) ! Local customization
      CHARACTER MASTER_DIR_DEF*24
      PARAMETER ( MASTER_DIR_DEF = '/vlbi/solve/master_files' ) ! Local customization
      CHARACTER WGET_EXE_DEF*32
      PARAMETER ( WGET_EXE_DEF = '/dist/wget-pet-20041113/src/wget' ) ! Local customization
