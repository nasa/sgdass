!
! This file is generated automatically by use_local.f from
!      template_file: /mk5/include/catalog_parameters.templ
!      local customization file: /mk5/local/solve.lcl
!
!      PLEASE DON'T EDIT THIS FILE!
!      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Start of catalog_parameters.i
!
! This file specifies parameters of SOLVE catalog system.
!
! Last modified by L. Petrov 2000.05.26
!
! NOTE FOR PROGRAMMERS:
!
!      Lines which starts from & are meta-definitions and they are tied with
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
!
!     980105 kdb      Change stor_key to cddisa.
!                     Remove rem_key, subnet_id.
!     980108 kdb      New parameters export_key, export_areas
!     980120 kdb      New parameter exper_cat
!     980301 pet      Update for accommodation ITISCNR preferences
!     980310 kdb      Move parameter exper_cat to general.i.
!     980310 kdb      Move export_areas and export_key to general.i.
!     980402 Lanotte  Update for accommodation MATECGS preferences
!     980808 pet      Update for accommodation USNO preferences
!     981009 Tomasi   Update for IRACNR (Bologna) preferences
!     990504 kdb      Update for VALENCIA preferences
!     990615 kdb      Update for GCCFA preferences.  (GCCFA is Ben Redman,
!                     Harvard, Smithsonian Astronomical Observatory, gamma
!                     project.)
!                     Correct split_cat_2 value.
!                     Correct GIUB declaration for ERROR_MESSAGE_PATH.
!     990920 pet      Update for LEIPZIG preferences
!     991020 pet      Changes in paths for GIUB preferences
!     991129 kdb      Add TAR_DEVICE_MODE parameter.
!     991223 kdb      Update for KASHIMA preferences.
!     pet    2000.02.01  Added for preferences MPIFR (Bonn).
!     Calvin 2000.04.19  Added preferences for NRCAN (GSD, Ottawa, Canada)
!     pet    2000.05.24  Moved installation-specific customization to
!                        local file.
!     pet    2000.05.26  Added parameters CENTER_ABR, CENTER_FULL_NAME
!                        (NB: the same parameters are defined in
!                        ../src/solve/include/gsfcb.i )
!     kdb    2001.01.11  Add parameters for status messages from checks of
!                        the catalog's locking/existence status.
!
! --- Specifies the catalog type
!
      CHARACTER*8 CATALOG_TYPE
      PARAMETER ( CATALOG_TYPE = 'DATABASE' )
!
! --- Specifies the first two characters of the catalog name.
!
      CHARACTER CATALOG_CHAR*2
      PARAMETER ( CATALOG_CHAR = 'i4' ) ! Local customization
!
! --- Path to the first copy of the catalog.
!
      CHARACTER CATALOG_PATH1*15
      PARAMETER ( CATALOG_PATH1 = '/vlbi/mark3_cat' ) ! Local customization
!
      CHARACTER*20 SCR_PATH
      PARAMETER  ( SCR_PATH = '/tmp                ' )
!
! --- Number of bytes in a catalog record.
!
      INTEGER*2   BYTES_IN_CAT_REC
      PARAMETER ( BYTES_IN_CAT_REC = 400 )
!
! --- Number of bytes in a hashfile record.
!
      INTEGER*2   BYTES_IN_HASH_RECORD
      PARAMETER ( BYTES_IN_HASH_RECORD = 16 )
!
! --- Number of areas/lus permitted in the catalog system
! --- (Sets the size of various arrays related to the areas -
! --- exclusive areas, lus on other machines, etc.)
!
      INTEGER*2 MXAREA
      PARAMETER (MXAREA = 60)
!
!     Permissible ways to lock/open the catalog file.
!     First letter = 'X' means lock/open exclusively
!     First letter = 'S' means lock/open shared
!     Second letter = 'B' means permit user to break if he
!     can't lock the catalog right away
!     Second letter = 'N' means do not permit user to break - he
!     MUST!!!!! update the catalog
!
!     Note: SHOULD REMAIN THE SAME AT ALL INSTALLATIONS
!
      INTEGER*2 NUM_CMODES
      PARAMETER (NUM_CMODES = 3)
      CHARACTER*6 CMODES
      PARAMETER (CMODES = 'XBSBXN')
!
! --- This is the name and path to the catlg news file
!
      CHARACTER CATLG_NEWS*26
      PARAMETER ( CATLG_NEWS = '/vlbi/mark3_cat/catlg_news' ) ! Local customization
!
!     This file contains the installation specific information for
!     the database handles.
!
!     Directory where the data base handler error message file lives.
!     Moved here from dbase/dbcom.ftni on 1/6/94 by KDB to centralize the
!     parameters.  At the same time, this directory was changed from a relative
!     to an absolute (in GSFC: /data18/mk3/src/dbase96).
!     Subsequently on 11/10/97, KDB moved the file to /data1/cat_aux_files,
!     since the dbase96 and other source directories tend to move more
!     frequently than the data directories currently under /data1.
!
      CHARACTER ERROR_MESSAGE_PATH*10
      PARAMETER ( ERROR_MESSAGE_PATH = '/mk5/help/' ) ! Local customization
!
!     The first character in experiment names.
      CHARACTER*1 EXPPREF
      PARAMETER ( EXPPREF = '$' )
!
! --- Maximum number of data bases to restore in one pass.  The maximum
! --- number of tapes to list in one pass should also be set to this
! --- number.
!
      INTEGER*2   MAX_RESTORE
      PARAMETER ( MAX_RESTORE = 407 )
!
! --- Maximum number of data bases to save in one pass.
!
      INTEGER*2 MAX_SAVE
      PARAMETER ( MAX_SAVE = 1007 ) ! Local customization
!
! --- Maximum # of neighborhoods in the catalog system.
!
      INTEGER*2   MAX_NHOODS
      PARAMETER ( MAX_NHOODS=4 )
!
!     THE POSITIONS THESE NEIGHBORHOODS SHOULD TAKE ALONG THE
!     CATALOGINF CHAIN (THE ORDER IS "SH","F1","A9","NT")
!     (SHOULD REMAIN THE SAME AT ALL INSTALLATIONS)
!
      CHARACTER*8 CNPOS
      PARAMETER ( CNPOS = 'SHF1A9NT' )
!
!     As of 2/4/92, CATLG uses a single routine, blowr_buf, to write tapes on
!     both streaming and non-streaming tape drives.  Blowr_buf uses a strategy
!     which makes streaming drives run faster and has no effect on (i.e., is
!     safe for) non-streaming drives.  Blowr_buf saves up a group of
!     physical records, then
!     writes them to tape en masse without pausing.  Since streaming drives
!     reposition themselves after every pause, saving up the records will
!     reduce the number of pauses and therefore the amount of positioning
!     and the time it takes to write to tape.
!     The greater the number of records saved up, the faster streaming drives
!     will go.  However, increasing the number of records saved will require
!     an increase in the size of the buffer that holds them, which will make
!     CATLG bigger.  This can cause
!     another problem with speed.  On some machines, for some system
!     releases, when CATLG gets too
!     big, CATLG will take a while to start up.
!
!     The following parameters control the number of records blowr_buf saves
!     up and the size of the buffer that holds them.  This will allow
!     individual installations to strike a good
!     balance between the speeds of their streaming tape drives and the
!     start up speeds of CATLG, on each of their machines.  STRM_RECS sets the
!     maximum number of records blowr_buf saves, and STRM_WDS sets the
!     maximum number of words the buffer may hold.  Some values
!     which the GSFC VLBI group has found useful are:
!
!         Machine/          Tape drive       STRM_RECS       STRM_WDS
!          release          (including
!                              bpi)
!
!       9000/845 - 7.00     6250 9-track        100            500,000
!       9000/730 - 8.05    ~4 Million DAT       800          4,000,000
!                            (DDS-format)
!
!     (If you are not sure of your machine or release, run UNIX commands
!         uname -m for the machine and
!         uname -r (and look at last 4 characters) for the release.)
!
      INTEGER*2   STRM_RECS
      PARAMETER ( STRM_RECS = 800 )
      INTEGER*4   STRM_WDS
      PARAMETER ( STRM_WDS = 4000000 )
!
!     CARTLIST_NUM determines the maximum number of disk files which can
!     be checked on a cartridge by the CATLG LI command's CArtridge option.
!
      INTEGER*2 CARTLIST_NUM
      PARAMETER ( CARTLIST_NUM = 2500 ) ! Local customization
!
!     AUXDIR gives a place to put the auxilliary files
!     created by CATLG for various things like archiving.  This was created
!     for the benefit of the DT command because the file it wrote to /tmp
!     was being deleted during overnight runs.  Not all auxilliary files are
!     directed here.  Most are written to /tmp, but if a need develops to
!     put them somewhere more permanent, the code can be rewritten to direct
!     them here.
!
      CHARACTER AUXDIR*15
      PARAMETER ( AUXDIR = '/vlbi/mark3_cat' ) ! Local customization
!
!     FULL_TAPE_LOG specifies the file for recording tapes that have been
!     marked as full.  One file is used for all tape libraries.
!
      CHARACTER*16 FULL_TAPE_LOG
      PARAMETER  ( FULL_TAPE_LOG = 'full_tape_log_db' )
!
!     Parameters which tailor the database catalog code to handle the
!     catalog configuration at a specific installation.
!
!      WHETHER THE CATALOG LIVES ON A UNIX MACHINE OR NOT
       LOGICAL*2   CAT_ON_UNIX
       PARAMETER ( CAT_ON_UNIX = .TRUE. )
       LOGICAL     DUALPORT
       PARAMETER ( DUALPORT = .FALSE. )
!
!      SPECIFIES WHETHER OR NOT THE CATALOG SYSTEM HAS MULTIPLE
!      NEIGHBORHOODS
!      THE INTERACTION BETWEEN DUALPORT AND MULTI_NEIGH IS AS FOLLOWS:
!
!         NEIGHBORHOODS      DUALPORT        MULTI_NEIGH
!
!         NT                  .FALSE.          .FALSE.
!         A9,NT               .FALSE.          .TRUE.
!         SH,F1,A9,NT         .TRUE.           .TRUE.
!
       LOGICAL*2 MULTI_NEIGH
      PARAMETER ( MULTI_NEIGH = .TRUE. ) ! Local customization
!
! ---- Machine the user is on - UNIX, A900 or F-processor
!      (USE A9 = 16697, F1 = 17969, NT = 20052)
!
       INTEGER*2   IMACH
      PARAMETER ( IMACH = 21582 ) ! Local customization
!
! ---- Other machines
!
       INTEGER*2 JMACH1, JMACH2
      PARAMETER ( JMACH1 = 14657 ) ! Local customization
       PARAMETER ( JMACH2 = 0     )
!
!      6. DATA BASES LIVE ON LU'S (ON AN A900 OR F-PROCESSOR) OR "AREAS"
!         (ci-like subdirectories) ON A UNIX MACHINE.  LU'S AND AREAS
!         IN TURN EXIST IN ONE OF FOUR "NEIGHBORHOODS": AN F-PROCESSOR,
!         AN A900, A DISC SHARED BY AN A900 AND AN F-PROCESSOR, AND
!         A UNIX NETWORK.  NUM_NHOODS TELLS HOW MANY OF THESE NEIGHBORHOODS
!         EXIST IN YOUR CATALOG SYSTEM.
!
       INTEGER*2 NUM_NHOODS
      PARAMETER ( NUM_NHOODS = 1 ) ! Local customization
!
!         SOME OF THE NEIGHBORHOODS IN YOUR CATALOG SYSTEM WILL BE
!         ACCESSIBLE FROM THIS MACHINE THROUGH THE NORMAL CATALOG COMMANDS.
!         (I.E. THE CODE ON THIS MACHINE, (e.g. CATLG),
!         WILL BE ABLE TO ACCESS THE DATA BASES IN THESE NEIGHBORHOODS
!         AND MOVE THEM, PURGE THEM, ETC.)  THESE NEIGHBORHOODS ARE
!         "VALID LUS".
!           ONLY UNIX NETWORK DATA BASES ARE VALID FOR THE
!           UNIX MACHINE.  THE SERVER WILL NOT BE PERMITTED TO PURGE
!           A DATA BASE.
!
       INTEGER*2   NM_VAL_HOODS
       PARAMETER ( NM_VAL_HOODS = 1 )
!
       CHARACTER*2  VAL_HOODS
       PARAMETER  ( VAL_HOODS = 'NT' )
 
!
!         INACCESSIBLE NEIGHBORHOODS (WITH RESPECT TO NORMAL PROCESSING)
!         ARE "INVALID LUS". FOR THE UNIX
!         MACHINE, THIS IS EVERYTHING BUT ITSELF.
!
       INTEGER*2 NM_INV_HOODS
      PARAMETER ( NM_INV_HOODS = 0 ) ! Local customization
!
       CHARACTER*2 INV_HOODS
       PARAMETER ( INV_HOODS = 'A9' )
!
!      Some neighborhoods are valid part of the time, under special
!      circumstances--for example, via special commands.
!      Specifically, some special commands
!      can access neighborhoods
!      over the network between various machines.  (The only current
!      example as of 6/22/89 is a catlg command to move databases)
!      PVL_HOODS should list the neighborhoods accessible to
!      machines on the network at your installation, WITH THE
!      EXCEPTION OF THE 'NT' NEIGHBORHOOD, WHICH CAN BE ACCESSED
!      NORMALLY.
!      If your installation has an A900 on the network,
!      A9, the A900 neighborhood, should be included, AS LONG AS THIS
!      NEIGHBORHOOD IS PART OF YOUR CATALOG SYSTEM. SH, the shared disk
!      between an A900 and an F-processor, may also be included if
!      it meets the same two conditions. Otherwise, set nm_pvl_hoods
!      to zero.
!
       INTEGER*2 NM_PVL_HOODS
      PARAMETER ( NM_PVL_HOODS = 0 ) ! Local customization
!
       CHARACTER*2 PVL_HOODS
       PARAMETER ( PVL_HOODS = 'A9' )
!
!      Even if an area is in a "valid" neighborhood and code exists to access
!      data bases on it, your installation may
!      want to restrict access to that area for certain functions.
!      A specific example which has come up at the GSFC VLBI group is
!      that some of our new areas are slow to access, and we want to keep newer,
!      more frequently used data bases from being placed there automatically,
!      (e.g, by the data base handler's create/update mechanisms).
!      Users can still send the data bases there manually, through the CATLG
!      MV option, etc.
!      The following parameter controls whether or not all data bases may be
!      sent automatically to all areas in the "valid" neighborhoods.
!      (The specific algorithm for deciding which data base may go where is
!      being placed in area_rstrct, in the qcat library, and individual
!      installations can change it to suit their needs.)
!      TRUE means that the algorithm will be used.
!      FALSE means that any data base may be sent anywhere.
!
       LOGICAL*2 CURB_AUTO_PUTS
      PARAMETER ( CURB_AUTO_PUTS = .FALSE. ) ! Local customization
!
!     Some catalog systems may now include data base areas that are slow
!     to access.  (For example, at GSFC the jukeboxes mounted on the remote
!     unix machine are slow to access.)
!     Set SLOW_AREAS to .TRUE. if
!     your catalog system will include slow areas.
!
      LOGICAL*2 SLOW_AREAS
      PARAMETER ( SLOW_AREAS = .FALSE. ) ! Local customization
!
!     At the moment, slow unix areas are being incorporated into
!     catalog systems as an experiment.  So they will initially be
!     identified in an ad hoc way; they will be required to
!     contain a key word, specified by the following parameter.  If
!     your catalog system contains slow areas, please set the following
!     key word.  If your catalog will not contain slow areas, you can
!     set this parameter to blanks.
!
      CHARACTER*4 SLOW_KEY
      PARAMETER ( SLOW_KEY = 'juke')
 
!     The catalog system at GSFC has jukeboxes and only two of these can
!     be mounted at a time because there are only two jukebox drives.
!     Thus, whenever someone is using a drive, that user has a lock on
!     that drive.  This prevents the continual switching of jukeboxes
!     that would normally occur when more than two were accessed.
!     If your system has jukeboxes, set SWITCH_AREA to .TRUE.
!
      LOGICAL*2 SWITCH_AREAS
      PARAMETER ( SWITCH_AREAS = .FALSE. ) ! Local customization
!
!     Currently, the switchable data base areas
!     are identified in an ad hoc way; they will contain the key given below.
!     If your catalog system contains switchable areas,
!     please set the following
!     key word.  If your catalog system will not contain switchable areas,
!     you can set this parameter to blanks.
!
      CHARACTER*4 SWITCH_KEY
      PARAMETER ( SWITCH_KEY = 'juke' )
!
!     Some installations may now have areas which cannot be on disk at
!     the same time.  (For example, the
!     GSFC VLBI group's jukeboxes have two sides.  Only one side can
!     be on disk at a time.)  This can cause problems. For example,
!     the data base handler cannot update a data base on one of these
!     areas onto another.  If your catalog system contains areas which
!     cannot coexist on disk, set NOCOEX_AREAS to .TRUE.
!
      LOGICAL*2 NOCOEX_AREAS
      PARAMETER (NOCOEX_AREAS = .FALSE.)
!
!     Currently, the data base areas that cannot coexist with every other area
!     are identified in an ad hoc way; they will contain the key given below.
!     Code will determine which areas specifically cannot coexist.
!     If your catalog system contains this type of area,
!     please set the following
!     key word.  Otherwise, you can set this parameter to blanks.
!
      CHARACTER NOCOEX_KEY*4
      PARAMETER ( NOCOEX_KEY = 'juke' ) ! Local customization
!
!     Some catalog systems may now include UNIX data base areas that are
!     set aside for storing older data bases.  Set STOR_AREAS to .TRUE. if
!     your catalog system will include this type of area and you want to
!     take advantage of some helpful features.  (The first feature is
!     going to be that CATLG's LIst Active data base  option will start
!     asking users whether they want to skip listing older active data bases
!     that are mainly on disk for storage purposes.)
!
      LOGICAL*2 STOR_AREAS
      PARAMETER ( STOR_AREAS = .FALSE. ) ! Local customization
!
!     Currently, UNIX data base areas that are set aside for storing data bases
!     are identified in an ad hoc way; they will contain the key given below.
!     If your catalog system contains areas which are being reserved to
!     store older data bases, please set the following
!     key word.  If your catalog will not contain storage areas, you can
!     set this parameter to blanks.
!
      CHARACTER STOR_KEY*6
      PARAMETER ( STOR_KEY = '      ' ) ! Local customization
!
!      tells whether or not the catalog system has a non-unix component
!
       LOGICAL*2 NON_UNIX_PART
      PARAMETER ( NON_UNIX_PART = .FALSE. ) ! Local customization
!
!      tells whether or not there are two copies of the catalog in this
!      catalog system
!
       LOGICAL*2 TWO_CAT_COPIES
       PARAMETER (TWO_CAT_COPIES = .FALSE.)
!
!      tells whether or not certain keys used by only by a non-UNIX
!      catalog should exist in this catalog. Examples are:
!      the non-UNIX high density tape library keys
!      (HIDENSTAPE and HIGH0000##)
!      the key to track the last fmgr name assigned to a data base
!      A UNIX catalog may have these keys if
!      it was originally created on a non-UNIX machine
!
       LOGICAL*2 A900_KEYS
      PARAMETER ( A900_KEYS = .FALSE. ) ! Local customization
!
!      The GSFC VLBI group is in the process of splitting its catalog
!      system, which currently consists of a single catalog and programs
!      on two machines, into two separate catalog systems.  The catalog,
!      which currently lives on the a900, is being copied to the 845,
!      and the programs on each machine will now access the catalog on
!      their machine.  To get things running quickly, the
!      catalog systems will not be cleaned up so that they no longer know
!      about each other.  Instead, both systems will still have information
!      which indicates that they are part of the same system.  As problems
!      occur in using a piece of a once-unified system as an independent,
!      single machine system, work arounds will be coded.  These work arounds
!      will be put under the
!      control of a new cattail parameter, split_cat, so that other
!      installations can avoid them.  Your installation should set
!      split_cat to .FALSE. to turn off the work around code.
!
       LOGICAL*2 SPLIT_CAT
      PARAMETER ( SPLIT_CAT = .FALSE. ) ! Local customization
!
!      split_cat_2 controls :
!        whether or not non-unix cartridges are listed in CATLG's CC command
!         (false retains this ability so that the cartridges can be identified
!          easily, so that they can be checked via li ca, to see if any dbs
!          must be deactivated from them via po ("purge other machine").
!          true suppressed attempts to list non-unix cartridges once they are
!          cleared of data bases and possibly even removed from the catalog)
!
       LOGICAL*2   SPLIT_CAT_2
       PARAMETER ( SPLIT_CAT_2 = .TRUE. )
!
!      The following parameter sets special options put in for the GSFC VLBI
!      group.  Other installations should set this to false.
!
       LOGICAL*2 GV_MSG
      PARAMETER ( GV_MSG = .FALSE. ) ! Local customization
!
!      The GSFC VLBI group's UNIX catalog system is gradually evolving towards a
!      system containing multiple UNIX machines connected over a network.
!      Eventually this system MAY support a catalog on one of the machines,
!      and tape drives, software and data base areas on one or more machines,
!      with parameter controlled code that will make this system general
!      enough for any installation.
!      For now, we are adding features of this type of system piece by piece,
!      on a quick and dirty, experimental basis.  We will try to set the
!      parameters up so that the changing code will not cause problems at
!      other installations, but if if the new features cause problems,
!      PLEASE let us know.
!
!      The first feature, remote
!      data base areas (areas on a different machine than the software) have
!      already been put in on an experimental basis.  Now the handling of
!      remote areas is about to be changed to fit the latest stage of the
!      Goddard system, and other features are about to be added as follows:
!
!      If your catalog is on UNIX, CAT_REM will indicate whether your
!      catalog lives on this machine, or on a remote one on the network.
!      Set CAT_REM as follows:
!            If your catalog is on this machine,     .FALSE.
!            If your catalog is on a different UNIX machine,  .TRUE.
!            If your catalog is on the A900, doesn't matter
!
       LOGICAL*2 CAT_REM
      PARAMETER ( CAT_REM = .FALSE. ) ! Local customization
!
!     The way the catalog handles remote data bases is now changing.
!     (Remote data base areas are areas on machines other than this one.
!     If this machine will have remote areas, set REM_AREAS to
!     .TRUE. to do further checks as described below to identify the
!      remote areas. Otherwise, set it to .FALSE.,  to avoid unnecessary
!      checking.
!
      LOGICAL*2 REM_AREAS
      PARAMETER ( REM_AREAS = .FALSE. ) ! Local customization
!
!
! II.  Instructions
!
!      1. Catalog(s) which live on a UNIX machine can be set up
!         to support either advisory locking or enforcement
!         locking.  Enforcement
!         locking will refuse the user the ability to do contradictory
!         locking, reads or writes.  Advisory locking will only refuse
!         contradictory locks.  Because the catalog system has been
!         designed to funnel
!         all catalog requests through the CHAIN library, which will
!         lock the catalog(s) before any reads or writes, it should
!         be safe to run under either advisory or enforcement mode.
!         However, if the user has a preference, he should read about
!         the chmod command to learn how to specify these modes.
!
!     ARCH_LIB should be set to the two character code identifying your
!     main tape archive library.  This is the library in which data bases
!     will be archived.  The current choices are:
!       1.  HI - for UNIX high density library A
!       2.  LO - for UNIX high density library B
!
!        Affects:
!
!        1.  tape keys to be accessed
!             master library key will be
!                  HIUNIXTAPE = UNIX high density library A (LDEN = HI on UNIX)
!                  BINARYTAPE = UNIX high density lib B (LDEN = LO on UNIX)
!             tape keys will be
!                  HIUX000### = UNIX high density library A
!                  BINRYTP### = UNIX high density library B
!        2.  words to be accessed in a data base version catalog record to
!              get where the version is archived (tape number and position on
!              tape)
!                  74-75  = UNIX high density library A
!                  53-54  = unix high dens lib B
!        3.  labels used for archive tapes  (see tl for details)
!
      INTEGER*2 ARCH_LIB
      PARAMETER ( ARCH_LIB = 2HLO ) ! Local customization
!
!  The allowed data base disk file prefixes
!  Presently ( for GSFC, ) For Haystack
!  DBH_FILE_PREFIXN is the number of prefixes
!
      CHARACTER*6 DBH_FILE_PREFIXES
      INTEGER*2 DBH_FILE_PREFIXN
      CHARACTER*1 HOME_DBH_PREFIX
      PARAMETER ( DBH_FILE_PREFIXES = '()@/^>' )
      PARAMETER ( DBH_FILE_PREFIXN  = 6        )
      PARAMETER ( HOME_DBH_PREFIX   = '('      )
!
!     DOUBLE_TAPE_REC - DATs are now considered to have good enough
!         internal integrity checking that it is no longer considered
!         vital to write each tape record redundantly and consecutively,
!         in case the first record becomes corrupt.  For consistency with
!         installations that will not pick up the new code for a while
!         and therefore will not be able to handle non-redundant tapes,
!         this ability to turn off redundancy will initially be restricted
!         to archive tapes.   DOUBLE_TAPE_REC will therefore only affect
!         archive DAT tapes.  Also, for simplicity, all tapes in an
!         archive library are assumed to be either redundant or not-redundant,
!         so this parameter should only be set to true for new DAT libraries.
!         (and must be kept true until a new DAT library is started.)
!         Also, for simplicity, it's assumed that installations will
!         maintain a single DAT library -- or if they have an old backup
!         library as well as a main, active  DAT library, that both will
!         be redundant, or else neither will be.
!
      LOGICAL*2 DOUBLE_TAPE_REC
      PARAMETER ( DOUBLE_TAPE_REC = .FALSE. ) ! Local customization
!
!     SETMARKS and SETMARK_FREQ:
!                       DAT (DDS-format, 4 millimeter) archive tapes
!                       can have setmarks, special tape marks that
!                       can be found quickly to speed up access.  In solution
!                       and data base
!                       catalog systems, these marks will be written once for
!                       every n tape files, at the start of each set of n files.
!                       Please note that this only applies to archive tapes.
!                       Temporary tapes will not use setmarks at this point.
!
!                       If your installation does not have a DAT archive
!                       library, or if no tape in your DAT archive library
!                       uses setmarks, set SETMARKS to NONE and SETMARK_FREQ
!                       to 0.
!
!                       Otherwise, if one or more tapes in your DAT library
!                       uses setmarks, set SETMARKS and SETMARK_FREQ as follows:
!
!                       SETMARK_FREQ will be used to determine the value of n
!                       given above.  That is, on each new archive tape
!                       started, SOLOP will write a setmark before each set of
!                       SETMARK_FREQ files.  SETMARK_FREQ can be changed to
!                       change the spacing of the setmarks on future tapes.
!                       SETMARKS itself must be set to either SAME or DIFF.
!                       If different tapes in your library will have different
!                       setmark spacings, or some tapes
!                       will have setmarks and some won't, set SETMARKS to
!                       DIFF.  Then the code will check the catalog
!                       every time it tries to archive to, restore from, or
!                       list a specific archive tape, in order to determine
!                       the spacing on that tape.  Otherwise,
!                       if every tape in your library will have setmarks, and
!                       each tape will have the same spacing,
!                       set SETMARKS to SAME.  In this case, the code
!                       will assume that every tape has the setmark spacing
!                       given by SETMARK_FREQ, which will eliminate a lot of
!                       catalog accesses.  However, if you ever change the
!                       spacing, or stop or start writing setmarks, you must
!                       change SETMARKS to DIFF and leave it as DIFF
!                       permaently.  Otherwise,  your catalog system
!                       will make incorrect
!                       assumptions about some tapes and be unable to read
!                       from them or write to them properly.
!
      CHARACTER*4 SETMARKS
      PARAMETER ( SETMARKS = 'SAME' )
      INTEGER*2   SETMARK_FREQ
      PARAMETER ( SETMARK_FREQ = 1 )
!
!     CATLG now writes archive tapes via internal calls to the UNIX tar
!     commands, as an
!     optional alternative to the standard method of buffering individual
!     logical records into a physical record and writing it to tape.
!     The following five parameters control the use of tar format archive
!     tapes in a catalog system.   Temporary tar tapes are not supported.
!
!     TAR_NEW controls whether or not subsequent new archive tapes will be
!     created as UNIX tar tapes.  Any tape created (labelled via TL AND
!     written to via DT) while TAR_NEW is set to .TRUE. will be a tar tape.
!     Information to this effect will be stored in the catalog, so that
!     setting TAR_NEW back to .FALSE. will not harm previous tapes.
!     If TAR_NEW is .TRUE., the values of SETMARKS and SETMARK_FREQ are
!     ignored, because all CATLG tar tapes use setmarks at a specific spacing.
!
!     However, different tar tapes may have different blocking factors
!     (number of 512 byte tar logical records per physical tar record).
!     TAR_BLOCK_FACTOR sets the factor to be used for future tapes.  It too
!     may be changed without affecting past tapes.  A default value of 20
!     is recommended, and it is not anticipated that any installation should
!     actually need to change from this value.   If TAR_NEW is .FALSE.,
!     TAR_BLOCK_FACTOR is ignored.
!
!     Once tar tapes are used,
!     MAX_TAR_BLOCK_FACTOR should be set to the maximum tar blocking factor
!     ever used at this installation for the current archive library,
!     regardless of the current values
!     of TAR_NEW and TAR_BLOCK_FACTOR.  Until tar tapes are used, this
!     should be set to 0.
!
!     MAX_FROM_SETGRP sets the size of an array used to restore from a tar
!     tape.  On a tar tape, all files archived in a single DT call are
!     tarred together and followed by a setmark.  So the files between two
!     setmarks represent a single DT call and are referred to as a setmark
!     group.  Because of the way tar operates, all files restored from the
!     same setmark group are handled through an array.  MAX_FROM_SETGRP
!     sets this array size.    This value is independent of the other
!     parameters.
!
!     Finally, TAR_DEVICE_MODE was added to assist in the transition to HP-UX
!     10.20 operating systems.  In operating systems before 10.20, most HP-UX
!     installations used the default DAT tape devices, 0m and 0mn, which were
!     generally Berkeley-mode.  This mode is essential for the mt command,
!     which is used for catalog system tar tapes, and it also worked with
!     non-tar tapes.  On 10.20, however, 0m and 0mn are AT&T mode, so
!     installations that use tar tapes must now access new Berkeley-mode
!     device names, 0mb and 0mnb.  The behavior of 0mb and 0mnb on HP operating
!     systems before 10.20 is not verified, so installations with HP operating
!     systems before 10.20 should access the original device names, 0m and 0mn.
!     For tar tape handling only,
!     catlg appends the value of TAR_DEVICE_MODE to the tape device name catlg
!     has constructed from preset catalog information (usually /dev/rmt/0m or
!     /dev/rmt/0mn).   So TAR_DEVICE_MODE should be set to 'b' (10.20 or later
!     systems) or ' ' (pre-10.20) to form 0mb and 0mnb or to retain 0m and 0mn.
!     Any installation using a non-standard device name (including 0h) may wish
!     to set TAR_DEVICE_MODE to ' ' or another value.
!     Catlg ignores TAR_DEVICE_MODE when it handles non-tar tapes.
!
!     Suggested values:
!
!                          non-tar system           tar system
!
!     TAR_NEW                 .false.                 .true.
!     TAR_BLOCK_FACTOR           0                      20
!     MAX_TAR_BLOCK_FACTOR       0                      20
!     MAX_FROM_SETGRP            2                 depends on needs of
!                                                   installation
!     TAR_DEVICE_MODE          ' '               HP-UX 10.20 (or greater)   'b'
!                                                earlier HP-UX versions     ' '
!                                                non-standard HP-UX
!                                                  device names (e.g., 0h)  ' '
!                                                non-HP-UX                  ' '
!
!
!               summary of interactions between values of TAR_NEW and
!               use of other parameters
!
!                                                 TAR_NEW
!                                       true              false
!
!     SETMARKS                          ignore             use
!     SETMARK_FREQ                      ignore             use
!     TAR_BLOCK_FACTOR                   use               ignore
!     MAX_TAR_BLOCK_FACTOR               independent - historical value
!     MAX_FROM_SETGRP                    independent - use depends on
!                                            catalog values for individual
!                                            archive tapes
!     TAR_DEVICE_MODE                   use                ignore
!
!
      LOGICAL*2 TAR_NEW
      INTEGER*2 TAR_BLOCK_FACTOR
      INTEGER*2 MAX_TAR_BLOCK_FACTOR
      INTEGER*2 MAX_FROM_SETGRP
      CHARACTER*1 TAR_DEVICE_MODE
!
      PARAMETER ( TAR_NEW = .TRUE. ) ! Local customization
      PARAMETER ( TAR_BLOCK_FACTOR = 20 ) ! Local customization
      PARAMETER ( MAX_TAR_BLOCK_FACTOR = 20 ) ! Local customization
      PARAMETER ( MAX_FROM_SETGRP = 500 ) ! Local customization
      PARAMETER ( TAR_DEVICE_MODE = 'b' ) ! Local customization
!
! --- Center abbreviation and center full name
!
      CHARACTER CENTER_ABR*3
      PARAMETER ( CENTER_ABR = 'AG ' ) ! Local customization
      CHARACTER CENTER_FULL_NAME*12
      PARAMETER ( CENTER_FULL_NAME = 'astrogeo.org' ) ! Local customization
!
! --- Parameters for statuses from checks of the catalog's existence/
! --- fcntl locking status.
!
      INTEGER*4 DB_CAT_LOCK_NONE
      PARAMETER ( DB_CAT_LOCK_NONE = 1901 )     !file exists, no lock
      INTEGER*4 DB_CAT_LOCK_READ
      PARAMETER ( DB_CAT_LOCK_READ = 1902 )     !file exists, read lock(s)
      INTEGER*4 DB_CAT_LOCK_WRITE
      PARAMETER ( DB_CAT_LOCK_WRITE = 1903 )    !file exists, write lock
      INTEGER*4 DB_CAT_LOCK_MISSING
      PARAMETER ( DB_CAT_LOCK_MISSING = 1904 )  !file does not exist
      INTEGER*4 DB_CAT_LOCK_OPEN_ERROR
      PARAMETER ( DB_CAT_LOCK_OPEN_ERROR = 1905 )    !error during open for check
      INTEGER*4 DB_CAT_LOCK_CHECK_ERROR
      PARAMETER ( DB_CAT_LOCK_CHECK_ERROR = 1906 )    !error during lock check
