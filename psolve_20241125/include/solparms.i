!
! modfications
!
! kdb 961018 Increase allocation for reserve files for
!            spool and progress files to 50 MB and
!            1 MB, respectively.
! kdb 971217 Reinstating cpyct_sl, so initializing the value of catalog_pathcp.
! pet 2000.04.25 Corrected ugly syntax in the specifications of constant
!                NAME_DIR_TIES
!
!     solparms.i - parameter file for solarch
!
!     I. Old catparms.ftni
!
!
!      1. SPECIFIES THE FIRST TWO CHARACTERS OF THE CATALOG NAME.
      CHARACTER*2 CATALOG_CHAR
      PARAMETER (CATALOG_CHAR='sl')
!
!      2. cartridge of the production catalog, copy 1, (for an a900 catalog)
!         (only used to print a message: catalog_path1 does the real work
!         of telling the server where to find the catalog
      INTEGER*2 CATCART_PROD
      PARAMETER (CATCART_PROD=0)
!
!      2.a. PATH TO THE FIRST COPY OF THE CATALOG. IF THERE
!         IS ONLY ONE COPY PUT IT HERE. THE CHOICES ARE:
!            FOR A UNIX CATALOG - DIRECTORY WHERE THE CATALOG LIVES
!            FOR A NON-UNIX CAT -
!                 TEST#1 FOR A TEST CATALOG
!              OR PROD#1 FOR A PRODUCTION CATALOG (THE A900 SERVER WILL FIND
!                   THE APPROPRIATE CATALOG.)  THE SERVER IS ONLY BUILT TO
!                   ACCESS UP TO TWO SETS OF CATALOGS, THE PRODUCTION CAT
!                   (UP TO TWO COPIES) AND THE TEST CAT (UP TO TWO COPIES)
      CHARACTER*140 CATALOG_PATH1
      PARAMETER (CATALOG_PATH1='/data1/cat_files          ')
!
!      2.b. PATH TO THE SECOND CATALOG COPY. IF THERE IS
!         IS NONE PUT A 0 HERE.
!           THE CHOICES ARE:
!                   CATALOG ON UNIX          CATALOG NOT ON UNIX
!
!      ONE COPY         ::                           ::
!
!      TWO COPIES    directory where             PROD#2 or
!                    copy 2 lives                TEST#2, as appropriate
!
!
      CHARACTER*140 CATALOG_PATH2
      PARAMETER (CATALOG_PATH2='::                        ')
! PARAMETER (CATALOG_PATH2='/users/lef/anals/test/datb')
!
!      2.c. PATH TO THE BACKUP COPY OF THE CATALOG.
!           THE ONLY PROGRAM WHICH HANDLES THE BACKUP COPY OF THE CATALOG
!             IS CPYCT, THE PROGRAM WHICH RECREATES IT AT EACH RUN.
!           CURRENTLY, EACH CPYCT ONLY DOES A BACKUP WITHIN THE MACHINE IT'S ON.
!           SO, IF YOUR SYSTEM KEEPS ITS CATALOG ON AN F-PROCESSOR OR A900,
!             YOU SHOULD RUN THE F-PROCESSOR/A900 CPYCT AND CAN IGNORE THIS
!             PARAMETER.
!           IF YOUR SYSTEM KEEPS ITS CATALOG ON THE UNIX MACHINE:
!             FILL IN THE ACTUAL UNIX DIRECTORY WHERE YOU WOULD LIKE CPYCT
!             TO CREATE THE BACKUP COPY.
!             DON'T SPECIFY THE ACTUAL FILE NAME.  CPYCT WILL FILL THAT IN.
!        (EX. /data/catfiles)
!
      CHARACTER*140 CATALOG_PATHCP
      PARAMETER (CATALOG_PATHCP='/data10/backups/solcat ')
!
!     2.d.  PATH TO THE SCRATCH DIRECTORY
!
      CHARACTER*20 SCR_PATH
      PARAMETER (SCR_PATH='/tmp                ')
      INTEGER*2 SCR_LEN
      PARAMETER (SCR_LEN = 4)
!
!     3. NUMBER OF WORDS THE SOFTWARE EXPECTS A
!        CATALOG RECORD TO HAVE
!
      INTEGER*2 WORDS_IN_CAT_REC
      PARAMETER (WORDS_IN_CAT_REC = 140)
!
!      6.  Number of areas/lus permitted in the catalog system
!          (Sets the size of various arrays related to the areas -
!            exclusive areas, lus on other machines, etc.)
!
      INTEGER*2 MXAREA
      PARAMETER (MXAREA = 60)
!
!      9. Permissible ways to lock/open the catalog file.
!           First letter = 'X' means lock/open exclusively
!           First letter = 'S' means lock/open shared
!           Second letter = 'B' means permit user to break if he
!              can't lock the catalog right away
!           Second letter = 'N' means do not permit user to break - he
!              MUST!!!!! update the catalog
!
!    Note: SHOULD REMAIN THE SAME AT ALL INSTALLATIONS
!
      INTEGER*2 NUM_CMODES
      PARAMETER (NUM_CMODES = 3)
      CHARACTER*6 CMODES
      PARAMETER (CMODES = 'XBSBXN')
!
!     This is the name and path to the solop news file
      Character*31 solop_news
!
      Parameter(solop_news = '/data1/cat_aux_files/solop_news')
!
!
!     Maximum number of data bases to restore in one pass.  The maximum
!       number of tapes to list in one pass should also be set to this
!       number.
!
      INTEGER*2 MAX_RESTORE
      PARAMETER (MAX_RESTORE = 407)
!
!     Maximum number of data bases to save in one pass.
!
      INTEGER*2 MAX_SAVE
      PARAMETER (MAX_SAVE = 407)
!
!     Maximum number of items that SOLOP MO (clear, or "move off" lu)
!     command can handle.
!
      INTEGER*2 MAX_MO
      PARAMETER (MAX_MO = 200)
!
!
!     MAXIMUM # OF NEIGHBORHOODS IN THE CATALOG SYSTEM (THESE INCLUDE
!      THE SHARED DISK BETWEEN AN F-PROCESSOR AND AN A900 ("SH"),
!      THE F-PROCESSOR ("F1"), THE A900 ("A9") AND THE UNIX NETWORK
!      ("NT") ).  THIS PARAMETER SHOULD REMAIN THE SAME AT ALL INSTALLATIONS
!
      INTEGER*2 MAX_NHOODS
      PARAMETER (MAX_NHOODS=4)
!
!     THE POSITIONS THESE NEIGHBORHOODS SHOULD TAKE ALONG THE
!     CATALOGINF CHAIN (THE ORDER IS "SH","F1","A9","NT")
!     (SHOULD REMAIN THE SAME AT ALL INSTALLATIONS)
!
      CHARACTER*8 CNPOS
      PARAMETER (CNPOS = 'SHF1A9NT')
!
!     ARCH_LIB should be set to the two character code identifying your
!     tape archive library.  The current choices are:
!       1.  HI - for 6250 libraries
!       2.  LO - for 800 and 1600 libraries
!
!     Currently, ARCH_LIB only affects LI.  (It determines the tape library
!     about which LI prints information, in LI Tape and others.)   CATLG
!     still determines which library to archive to/restore from via its code.
!
      INTEGER*2 ARCH_LIB
      PARAMETER (ARCH_LIB = 2HHI)
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
      INTEGER*2 STRM_RECS
      PARAMETER (STRM_RECS = 800)
      INTEGER*4 STRM_WDS
      PARAMETER (STRM_WDS = 4000000)
!
!     CARTLIST_NUM determines the maximum number of disk item files which can be
!     checked on a cartridge by the SOLOP OR command.
!
      INTEGER*2 CARTLIST_NUM
      PARAMETER (CARTLIST_NUM = 3000)
!
!     DOUBLE_TAPE_REC - if true, writes tape records to tape twice.
!                       if false, writes them once.
!
      LOGICAL*2 DOUBLE_TAPE_REC
      PARAMETER (DOUBLE_TAPE_REC = .FALSE.)
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
      PARAMETER (SETMARKS = 'DIFF')
      INTEGER*2 SETMARK_FREQ
      PARAMETER (SETMARK_FREQ = 1)
!
!
!
!     II. Old cattail.ftni
!
!        file to tailor a catalog system to a specific site's needs
!
!        consists of fortran parameters and user instructions
!
! I.   PARAMETERS WHICH TAILOR THE DATA BASE CATALOG CODE TO HANDLE THE
!      CATALOG CONFIGURATION AT A SPECIFIC INSTALLATION
!
!      1. WHETHER THE CATALOG LIVES ON A UNIX MACHINE OR NOT
!
       LOGICAL*2 CAT_ON_UNIX
       PARAMETER (CAT_ON_UNIX = .TRUE.)
!
!      2.  NAME OF MACHINE ON WHICH THE CATALOG LIVES
!          (REALLY ONLY NECESSARY IF THE CATALOG LIVES ON THE A900)
!
       CHARACTER*6 CAT_HOME
       PARAMETER (CAT_HOME = '      ')
!
!      Number of characters in CAT_HOME
!
       INTEGER*2 CAT_HOME_NUM
       PARAMETER (CAT_HOME_NUM = 6)
!
!      1. SPECIFIES WHETHER PORT CONFIGURATION ON THE NON-UNIX PORTION
!         OF THE CATALOG SYSTEM IS DUAL OR SINGLE.
!           IF THE NON-UNIX PORTION IS A SINGLE MACHINE, USE .FALSE.
!           IF THE NON-UNIX PORTION IS TWO MACHINES WHICH SHARE A
!             DISC, USE .TRUE.
!           IF YOUR CATALOG SYSTEM WILL ONLY INCLUDE A UNIX MACHINE, USE
!             .FALSE.
!
       LOGICAL*2 DUALPORT
       PARAMETER (DUALPORT = .FALSE.)
!
!        SPECIFIES WHETHER OR NOT THE CATALOG SYSTEM HAS MULTIPLE
!        NEIGHBORHOODS
!        THE INTERACTION BETWEEN DUALPORT AND MULTI_NEIGH IS AS FOLLOWS:
!
!         NEIGHBORHOODS      DUALPORT        MULTI_NEIGH
!
!         NT                  .FALSE.          .FALSE.
!         A9,NT               .FALSE.          .TRUE.
!         SH,F1,A9,NT         .TRUE.           .TRUE.
!
      LOGICAL*2 MULTI_NEIGH
      PARAMETER (MULTI_NEIGH = .FALSE.)
!
!
!      2. MACHINE THE USER IS ON - UNIX, A900 OR F-PROCESSOR
!           (USE A9 = 16697, F1 = 17969, NT = 20052)
!
       INTEGER*2 IMACH
       PARAMETER (IMACH = 20052)
!
!      3. OTHER MACHINES
!
       INTEGER*2 JMACH1, JMACH2
       PARAMETER (JMACH1 = 0)
       PARAMETER (JMACH2 = 0)
!
!
!      6. DATA BASES LIVE ON LU'S (ON AN A900 OR F-PROCESSOR) OR "AREAS"
!         (ci-like subdirectories) ON A UNIX MACHINE.  LU'S AND AREAS
!         IN TURN EXIST IN ONE OF FOUR "NEIGHBORHOODS": AN F-PROCESSOR,
!         AN A900, A DISC SHARED BY AN A900 AND AN F-PROCESSOR, AND
!         A UNIX NETWORK.  NUM_NHOODS TELLS HOW MANY OF THESE NEIGHBORHOODS
!         EXIST IN YOUR CATALOG SYSTEM.
!
       INTEGER*2 NUM_NHOODS
       PARAMETER (NUM_NHOODS = 1)
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
       INTEGER*2 NM_VAL_HOODS
       PARAMETER (NM_VAL_HOODS = 1)
!
       CHARACTER*2 VAL_HOODS
       PARAMETER (VAL_HOODS = 'NT')
!
!
!         INACCESSIBLE NEIGHBORHOODS (WITH RESPECT TO NORMAL PROCESSING)
!         ARE "INVALID LUS". FOR THE UNIX
!         MACHINE, THIS IS EVERYTHING BUT ITSELF.
!
       INTEGER*2 NM_INV_HOODS
       PARAMETER (NM_INV_HOODS = 0)
!
       CHARACTER*2 INV_HOODS
       PARAMETER (INV_HOODS = '  ')
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
       PARAMETER (NM_PVL_HOODS = 0)
!
       CHARACTER*2 PVL_HOODS
       PARAMETER (PVL_HOODS = 'A9')
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
       PARAMETER (CURB_AUTO_PUTS = .TRUE.)
!
!     Some catalog systems may now include data base areas that are slow
!     to access.  (For example, at GSFC the jukeboxes mounted on the remote
!     unix machine are slow to access.)
!     Set SLOW_AREAS to .TRUE. if
!     your catalog system will include slow areas.
!
      LOGICAL*2 SLOW_AREAS
      PARAMETER (SLOW_AREAS = .FALSE.)
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
      PARAMETER (SLOW_KEY = 'juke')
!
!     The catalog system at GSFC has jukeboxes and only two of these can
!     be mounted at a time because there are only two jukebox drives.
!     Thus, whenever someone is using a drive, that user has a lock on
!     that drive.  This prevents the continual switching of jukeboxes
!     that would normally occur when more than two were accessed.
!     If your system has jukeboxes, set SWITCH_AREA to .TRUE.
!
      LOGICAL*2 SWITCH_AREAS
      PARAMETER (SWITCH_AREAS = .FALSE.)
!
!     Currently, the switchable data base areas
!     are identified in an ad hoc way; they will contain the key given below.
!     If your catalog system contains switchable areas,
!     please set the following
!     key word.  If your catalog system will not contain switchable areas,
!     you can set this parameter to blanks.
!
      CHARACTER*4 SWITCH_KEY
      PARAMETER (SWITCH_KEY = 'juke')
!
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
      CHARACTER*4 NOCOEX_KEY
      PARAMETER (NOCOEX_KEY = 'juke')
!
!
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
      PARAMETER (STOR_AREAS = .TRUE.)
!
!     Currently, UNIX data base areas that are set aside for storing data bases
!     are identified in an ad hoc way; they will contain the key given below.
!     If your catalog system contains areas which are being reserved to
!     store older data bases, please set the following
!     key word.  If your catalog will not contain storage areas, you can
!     set this parameter to blanks.
!
      CHARACTER*4 STOR_KEY
      PARAMETER (STOR_KEY = 'juke')
!
!      tells whether or not the catalog system has a non-unix component
!
       LOGICAL*2 NON_UNIX_PART
       PARAMETER (NON_UNIX_PART = .FALSE.)
!
!      tells whether or not there are two copies of the catalog in this
!      catalog system
!
       LOGICAL*2 TWO_CAT_COPIES
       PARAMETER (TWO_CAT_COPIES = .FALSE.)
!
!      tells whether or not certain keys used by only by a non-UNIX
!      catalog should exist in this catalog. Examples are:
!        the non-UNIX high density tape library keys
!           (HIDENSTAPE and HIGH0000##)
!        the key to track the last fmgr name assigned to a data base
!      A UNIX catalog may have these keys if
!        it was originally created on a non-UNIX machine
!
       LOGICAL*2 A900_KEYS
       PARAMETER (A900_KEYS = .FALSE.)
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
       PARAMETER (SPLIT_CAT = .FALSE.)
!
!      When the GSFC VLBI group brought up its catalog on the 845, it was
!      helpful to us to have a second split cat parameter to keep some code
!      active until we could clear up some catalog conditions, then deactivate
!      the code since it no longer applies to the split catalog in its final
!      form.   Other installations
!      should set this to .FALSE., since the code being deactivated here will
!      still apply at other installations.
!
!      Specifically, split_cat_2 controls :
!
!       1. whether or not non-unix cartridges are listed in CATLG's CC command
!         (false retains this ability so that the cartridges can be identified
!          easily, so that they can be checked via li ca, to see if any dbs
!          must be deactivated from them via po ("purge other machine").
!          true suppressed attempts to list non-unix cartridges once they are
!          cleared of data bases and possibly even removed from the catalog)
!
       LOGICAL*2 SPLIT_CAT_2
       PARAMETER (SPLIT_CAT_2 = .FALSE.)
!
!      The following parameter can be set to false, to turn off messages
!      to users in the GSFC VLBI group.
!
       LOGICAL*2 GV_MSG
       PARAMETER (GV_MSG = .TRUE.)
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
       PARAMETER (CAT_REM = .TRUE.)
!
!     The way the catalog handles remote data bases is now changing.
!     (Remote data base areas are areas on machines other than this one.
!     If this machine will have remote areas, set REM_AREAS to
!     .TRUE. to do further checks as described below to identify the
!      remote areas. Otherwise, set it to .FALSE.,  to avoid unnecessary
!      checking.
!
      LOGICAL*2 REM_AREAS
      PARAMETER (REM_AREAS = .TRUE.)
!
!     Remote data base areas are identified in the following ways:
!
!        If REM_AREAS is set to .TRUE.,
!
!            the subroutine which identifies remote areas checks SUBNET_ID
!            to see what machine is running this software.  Each machine
!            will have a specific algorithm.
!
!            Currently AQ (aquila at the GSFC VLBI group)
!                 considers every data base area remote
!            Every other id will use the old algorithm, where the
!            subroutine searches each data base name for a key, and
!            considers those containing the key to be remote to this
!            machine.   It is recommended that other installations set
!            SUBNET_ID to "OT" for other machine.
!
!            Note: AQ should be used for all 700 series machines.
!
      CHARACTER*2 SUBNET_ID
      PARAMETER (SUBNET_ID = 'AQ')
!
      CHARACTER*4 REM_KEY
      PARAMETER (REM_KEY = 'juke')
!
!
!     Some instructions:
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
!
!     III.  Parameters created for the solution archiving system.
!
!         A. Values which are applicable to all installations and
!            should not be changed.
!
!     STANDARD KEY NAMES AND MARKERS
!
      INTEGER*2 NUM_MARKERS
      PARAMETER (NUM_MARKERS = 10)
      CHARACTER*2 MARK_SYMBOL
      PARAMETER (MARK_SYMBOL = '**')
      CHARACTER*2 SOL_MARK
      PARAMETER (SOL_MARK = 'US')
      CHARACTER*2 PRI_CODE
      PARAMETER (PRI_CODE = 'PR')
      CHARACTER*2 SEC_CODE
      PARAMETER (SEC_CODE = 'SC')
!
!     Number of items currently associated with a solution.  Currently 7
!     (control file, arc list, spool file, progress file,
!     output cgm, covariance file and correlation file).  The input cgm
!     is associated with the solution that made it.
!
!     Also various forms for identifying the type of item.
!
      INTEGER*2 NUM_SOLITEMS
      PARAMETER (NUM_SOLITEMS = 7)
      INTEGER*2 NUM_SPCL_ITEMS
      PARAMETER (NUM_SPCL_ITEMS = 3)
!
      CHARACTER*7 SN_ITEM_NAMES
      PARAMETER (SN_ITEM_NAMES = 'CASPOVR')
      CHARACTER*3 SN_SPCL_ITEMS
      PARAMETER (SN_SPCL_ITEMS = 'XIB')
!
      CHARACTER*14 DB_ITEM_NAMES
!
!      Note: choice of codes for db_item_names was driven by the existence
!            of pre-SOLARCH files starting with these letters
!                                 | | | | | | |
      PARAMETER (DB_ITEM_NAMES = 'glarspprcgcvcr')
      CHARACTER*6 DB_SPCL_ITEMS
      PARAMETER (DB_SPCL_ITEMS = 'ngicba')
!
      CHARACTER*21 TR_ITEM_NAMES
!
!      Note: choice of codes for tr_item_names was driven by the
!            potential existence
!            of pre-SOLARCH files starting with these letters
!            at non-GSFC installations
!                                 |  |  |  |  |  |  |
      PARAMETER (TR_ITEM_NAMES = 'glbarcsplprgcgmcvfcrf')
!
!     Note: choice of bak code is driven by the potential existence
!           of pre-SOLARCH files starting with bak
!           at non-GSFC installations
!
      CHARACTER*9 TR_SPCL_ITEMS
      PARAMETER (TR_SPCL_ITEMS = 'nglicgbak')
!
      CHARACTER*70 KY_ITEM_NAMES
      PARAMETER ( KY_ITEM_NAMES = &
     & 'CNTRLFILESARCLISTS  SPOOLFILESPROGFILES CGMFILES  COVFILES  CORRFILES ' )
!
      CHARACTER*30 KY_SPCL_ITEMS
      PARAMETER (KY_SPCL_ITEMS = 'NGCGMFILES                    ')
!
!     Initially just used for display
!
      CHARACTER*112 FL_ITEM_NAMES
      PARAMETER ( FL_ITEM_NAMES = &
     & 'control file    arc list(file)  spoolfile      progress file   cgm file        covariance file correlation file' )
      CHARACTER*48 FL_SPCL_ITEMS
      PARAMETER ( FL_SPCL_ITEMS = &
     & 'non-globl cgm   input cgm       back control fil')
!
      INTEGER*2 IPT_CONTROL
      PARAMETER (IPT_CONTROL = 1)
!
      INTEGER*2 IPT_NONGL,IPT_INPUT_CGM,IPT_BACK_CNT
      PARAMETER (IPT_NONGL = 1)
      PARAMETER (IPT_INPUT_CGM = 2)
      PARAMETER (IPT_BACK_CNT = 3)
!
!
      INTEGER*2 NUM_SOL_TYPES
      PARAMETER (NUM_SOL_TYPES = 5)
!
      INTEGER*2 MAX_SOL_TYPES
      PARAMETER (MAX_SOL_TYPES = 10)
!
      CHARACTER*5 SN_SOL_TYPES
      PARAMETER (SN_SOL_TYPES = 'FBCIS')
!
      CHARACTER*55 FL_SOL_TYPES
      PARAMETER (FL_SOL_TYPES = &
     & 'FORWARD    BACK       COMPLETE   INDEPENDENTSUPPRESS   ' )
!
!     Maximum number of users (owners) who may maintain solutions in the
!     catalog.
!
      INTEGER*2 MAX_USERS
      PARAMETER (MAX_USERS =  100)
!
!     Projected sizes of the various items (best guess, based on the average
!     sizes of previous items of each type)  (in bytes)
!
      INTEGER*4 PROJ_SZ_GL, PROJ_SZ_AR, PROJ_SZ_SP, PROJ_SZ_PR, &
     &          PROJ_SZ_CG, PROJ_SZ_CV, PROJ_SZ_CR
!
      PARAMETER (PROJ_SZ_GL =        100000)
      PARAMETER (PROJ_SZ_AR =        150000)
      PARAMETER (PROJ_SZ_SP =      50000000)
      PARAMETER (PROJ_SZ_PR =       1000000)
      PARAMETER (PROJ_SZ_CG =       8500000)
      PARAMETER (PROJ_SZ_CV =       7000000)
      PARAMETER (PROJ_SZ_CR =       2000000)
!
!     Flags which indicate the status of the previous GLOBL run attempted with
!     a given set of run initials:
!          NO_PEND - previous run successfully completed - no leftover pending
!                    info
!        PART_PEND - previous run aborted while still setting up pending info.
!                    Partial pending info -- must abort and clean up the
!                    erroneous state.
!        FULL_PEND - previous run aborted after managing to set up full set of
!                    pending info.  Safe to use the pending info to resume the
!                    previous run or change to a new run.
!
      CHARACTER*2 FULL_PEND, PART_PEND, NO_PEND
      PARAMETER (FULL_PEND = 'FP')
      PARAMETER (PART_PEND = 'PP')
      PARAMETER (  NO_PEND = '  ')
!
!     Code for building placeholder files (files that reserve space for a
!     GLOBL run's future primary storage items, by occupying space for the
!     future items)
!
      CHARACTER*2 PLHOLD_CODE
      PARAMETER (PLHOLD_CODE = 'rs')
!
!     At its most basic level, SOLARCH allows any combination of directories
!     to hold any combination of items.  However, in practice, some
!     installations may decide to keep certain types of items together in
!     their own directories.
!     For example, at the GSFC VLBI group, all directories are reserved
!     for one of three sorts of items: control-type (including control and arc
!     files), solution output files (including spool files, progress files,
!     covariance and
!     correlation files) and cgms (including globl and non-globl cgms).
!     The following parameters tell SOLARCH code to take these implementations
!     into account.   If you do not want to take advantage of this feature,
!     set NUM_DIR_TIES to 0.
!
!     To use this feature, decide how many sets of item directories should
!     be related, or "tied together" and set NUM_DIR_TIES to this.  Then
!     set DIR_TIES, which is essentially a set of bit variables, as follows:
!     DIR_TIES must have one bit variable per set of related directories.
!     In each set, for every item included in the set, turn the corresponding
!     bit on.  (The correspondence is:
!     bit 1 = non-globl cgm (X) and bits 2-8 = items CASPOVR.  (These letters
!     are defined earlier in this file.) )
!     So to say that
!     the directories which contain non-globl (X) and normal cgms (O)
!     are all cgm directories, put 33 (= 1 +32 or bits 1 and 6 on), into
!     DIR_TIES.  To say that control files and arc lists (C and A)
!     are always kept together on disk, put another bit variable in DIR_TIES
!     equal to 6 (= 2 + 4 or bits 2 and 3 on).   Each bit
!     variable must be right adjusted in a field 6 characters wide.
!     Then set NAME_DIR_TIES to some descriptive name to be displayed when
!     needed to describe the relationship within each set of items
!     (e.g., CGM DIRS, CONTRL&ARC).
!     The names must be placed in fields 20 characters wide.
!
!     Note: the size of a local variable in solop subroutine fs_s limits
!     the number of directory tie groups (NUM_DIR_TIES) to 12.  This number
!     can be increased, but at the expense of either decreasing the width
!     of the NAME_DIR_TIES fields or recoding.
!
!     Note: this feature assumes that if a set of items is "tied together",
!        then the entire set of items is either permitted or prohibited on
!        a given directory d.  The feature will probably malfunction if
!        there is a directory which may hold some of the "tied" items, but
!        not others.
!
      INTEGER*2 NUM_DIR_TIES
      PARAMETER (NUM_DIR_TIES = 3)
      CHARACTER*18 DIR_TIES
      PARAMETER (DIR_TIES = '     6   216    33')
      CHARACTER*60 NAME_DIR_TIES
!      PARAMETER (NAME_DIR_TIES = 'CONTROL & ARC FILES OUTPUT FILES      '
!     .  CGMS                ')
      PARAMETER  ( NAME_DIR_TIES = &
     & 'CONTROL & ARC FILES OUTPUT FILES        CGMS                ' )
!
!     Output file for listing unarchived items
!
      CHARACTER*18 UNARCHS
      PARAMETER (UNARCHS = '/tmp/sl_unarchived')
!
!     Number of copies to be made of each archive tape.
!     If multiple copies are requested, each copy will be assigned the
!     same tape number N, and the catalog system will label the
!     requested number of copies and do each archive onto the set of
!     copies.  Restoration may be made from any copy.
!     The catalog itself will be unaware of the extra copies  -- it will
!     only know that a given item is stored on a tape with a given number.
!     Each copy's label will contain the tape number, for restoration
!     verification, and the copy number, to keep the tapes straight during
!     archiving.
!
      INTEGER*2 NUM_TAPE_COPIES
      PARAMETER (NUM_TAPE_COPIES = 2)
!
      CHARACTER*8 CATALOG_TYPE
      PARAMETER (CATALOG_TYPE = 'SOLUTION')
