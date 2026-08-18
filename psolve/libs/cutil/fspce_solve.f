      SUBROUTINE FSPCE_SOLVE ( NPATHS, PATHS, PATHLENS, ISPACES, KERR )
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
!     FSPCE finds the space left on the given directories
!
!     RESTRICTIONS - Assumes no more than 75 block devices will be on
!     the system, and that their names will be no longer
!     than 60 characters (easily changed)
!     System-dependent (tailored for machines with
!     df commands with the following format:
!
!        field 1 - directory name
!        field 2 - (xxx...):, where xxx... represents
!                             variable contents
!        field 3 - number of blocks available
!                  field 4 - "blocks"
!                  other fields - may be anything
!
!        (there must be one or more spaces between
!        fields 1 and 2, or else fields 2 and 3.
!        there must be one or more spaces between
!        fields 3 and 4.)                           )
!
!        No block device name may contain the word "blocks"
!        (or else the df_com command must be rewritten.)
!
!     INPUT VARIABLES:
!
!      NPATHS - the number of directories to be checked
!      PATHS - the directories for which the amount of remaining space
!              is to be found
!      PATHLENS - the lengths of the directory names
!
!     OUTPUT VARIABLES:
!
!        ISPACES - the amount of space left on the directories according to
!                  FSPCE;
!        KERR - error return - 0 or -1
!
         INTEGER*2 NPATHS,KERR
         INTEGER*2 PATHLENS(*)
         INTEGER*4 ISPACES(*)
         CHARACTER*(*) PATHS(*)
!
!     LOCAL VARIABLES
      INTEGER*2 MAX_BDS, unit
      PARAMETER (MAX_BDS = 300)
      CHARACTER*20 SPACEFILE
      INTEGER*2 IERR,SYSTEM2_SH,TRIMLEN,ICT,IBD,NBD,NCHARS,LBD,IP
      INTEGER*4 IERR4
!
!     Note: if the df command changes, change the size of df_com, as needed,
!       (and also change initialization loop for blanking out command,
!         if df_com size changes)
!
      CHARACTER*155 DF_COM
      INTEGER*4 IPID,GETPID
      CHARACTER*5 CPID
      LOGICAL*2 BDFOUND,SLFOUND
      CHARACTER*180 SPACEBUF,RESTBUF,FIRSTBUF
      INTEGER*2 BD_LENS(MAX_BDS),CURLEN
      CHARACTER*60 BLOCK_DEVS(MAX_BDS)
      INTEGER*4 IFREES(MAX_BDS)
      CHARACTER*140 CURPATH
      LOGICAL*2 GETUN
      INTEGER*2 INLU,MAX_EXPECTED,NUMBER_BDS,LERR
      INTEGER*2 INM,LEN_MNTS(MAX_BDS),LEN_TRUES(MAX_BDS)
      CHARACTER*140 NAME_MNTS(MAX_BDS),NAME_TRUES(MAX_BDS)
      CHARACTER*1 LORRS(MAX_BDS)
      CHARACTER*140 CDUMS1(MAX_BDS),CDUMS2(MAX_BDS),CDUMS3(MAX_BDS)
      INTEGER*2 IDUMS1(MAX_BDS),IDUMS2(MAX_BDS),IDUMS3(MAX_BDS)
      CHARACTER*1 CDUMS4(MAX_BDS)
      LOGICAL*2 LOCALHOST
!
! 6.  PROGRAMMER: K. Baver 2/8/96 (from fspce.f)
!
!     LAST MODIFIED:
!
!     970311 kdb Raise max_bds from 150 to 300.
!     970724 kdb Change to get the true space on the fake jukebox, /juke,
!                on bootes.
!     kdb 970801 New mnt_names arguments (passing up more information)
!     kdb 970804 Optionally pass up true local host name
!     pet 990409 Replaced call of system with call of system2_sh in order to
!                fix SYSTEM-APR99 bug
!     pet 2000.06.15  Fixed this bug once more.
!
!     PROGRAM STRUCTURE
!
!CCCCC
!
! --- Form file that receives results of df command. Clear arrays.
!
      KERR = 0
      IPID = GETPID()
      WRITE(CPID,"(I5.5)") IPID
      SPACEFILE = '/tmp/fspcesolve'//CPID
      DO NBD = 1,MAX_BDS
        DO NCHARS = 1,60
          BLOCK_DEVS(NBD)(NCHARS:NCHARS) = ' '
        END DO
        IFREES(NBD) = 0
      END DO
!
!     Each directory resides on a block device.  Every directory
!     on a given block device can use all the space available to the block
!     device, so we want to find the block device on which a given
!     directory lives, then find out how much space the block device has.
!
!     The first step will be finding how much space each block device has.
!     FSPCE will generate this information by
!     a call to df and store it in a file, then read it into arrays.
!
!     The other step will be determining the block device where each
!     directory lives, then looking up the corresponding amount of space in
!     the appropriate array.
!
!
!     Step 1:  use df to generate a list of the block devices and
!     the space left on them; put this into a file, then read info into arrays.
!
      DO ICT = 1,155
         DF_COM(ICT:ICT) = ' '
      END DO
!
!out    DF_COM = '/etc/mount | cut -d" " -f1 | grep -v juke'//
!out &    ' | xargs df '
!out &     //'| sed "s#(.*):##" | sed "s# blocks.*\$##" > ' //SPACEFILE
!
        DF_COM = &
     &     " df "//"| sed 's#(.*):##' | sed 's# blocks.*\$##' > " //SPACEFILE
!
      IERR = SYSTEM2_SH ( DF_COM )
      IF ( IERR .NE. 0 ) THEN
           CALL FERR ( INT2(281), &
     &         "FSPCE_SOLVE: Error executing df command. Continue.", INT2(0), &
     &          INT2(0) )
           KERR = -1
           RETURN
      END IF
!
!     Read results of the df and put the block devices and their free
!       space amounts in arrays
!
      unit = 305
      OPEN (unit,FILE=SPACEFILE,IOSTAT=IERR4,ERR=101,STATUS='OLD', &
     &  ACCESS='SEQUENTIAL',FORM='FORMATTED')
 101  CONTINUE
      IF (IERR4.NE.0) THEN
        CALL FERR( INT2(282), "FSPCE_SOLVE: Error opening space file ", &
     &             INT2(0), INT2(0) )
        KERR = -1
        RETURN
      END IF
      NBD = 0
      DO WHILE (NBD .GE. 0)
        READ(unit,"(A180)",IOSTAT=IERR4,ERR=102,END=103) SPACEBUF
 102    CONTINUE
        IF (IERR4.NE.0) THEN
          CALL FERR( INT2(283), "FSPCE_SOLVE: Error reading space file ", &
     &         INT2(0), INT2(0) )
          CLOSE(unit)
          KERR = -1
          RETURN
        END IF
        NBD = NBD + 1
        CALL SPLITSTRING(SPACEBUF,FIRSTBUF,RESTBUF )
        LBD = TRIMLEN(FIRSTBUF)
        IF (NBD.GT.MAX_BDS .OR. LBD.GT.60) THEN
          IF (NBD.GT.MAX_BDS)CALL FERR( INT2(284), &
     &     "FSPCE_SOLVE: too many block devices ", INT2(0), INT2(0) )
          IF (LBD.GT.60)CALL FERR( INT2(285), &
     &     "FSPCE_SOLVE: block device names too long ", INT2(0), INT2(0) )
          CLOSE(unit)
          KERR = -1
          RETURN
        END IF
        BLOCK_DEVS(NBD)(1:LBD) = FIRSTBUF(1:LBD)
        BD_LENS(NBD) = LBD
!
        READ(RESTBUF,*,IOSTAT=IERR4,ERR=105) IFREES(NBD)
        IF ( IFREES(NBD) .LT. 0 ) IERR4=1
 105    CONTINUE
        IF (IERR4.NE.0) THEN
          CALL FERR( INT2(286), "FSPCE_SOLVE: internal read error ", INT2(0), &
     &         INT2(0) )
          CLOSE(unit)
          KERR = -1
          RETURN
        END IF
      END DO
 103  CLOSE (unit,STATUS='DELETE')
!
!     In some systems, the block devices may be mounted locally under names
!     that differ from the true block device names on the remote machines
!     where the block devices live.  In this case, the df
!     command will have picked up the mount name which cannot be matched
!     directly with the catalog paths.  Convert the mount names to the true
!     block device names.
!
!     Local block devices will be mounted under the true block device names
!     expected by the catalog system.
!
      GETUN = .FALSE.
      INLU = 305
      MAX_EXPECTED = MAX_BDS
!     Don't need to know actual local host name
      LOCALHOST = .FALSE.
      CALL MNT_NAMES(LOCALHOST,GETUN,INLU,MAX_EXPECTED, &
     &               NUMBER_BDS,NAME_MNTS,LEN_MNTS, &
     &               NAME_TRUES,LEN_TRUES, &
     &               CDUMS1,IDUMS1, &
     &               CDUMS2,IDUMS2, &
     &               CDUMS3,IDUMS3, &
     &               LORRS,CDUMS4,LERR )
      IF (LERR.NE.0) THEN
        KERR = -1
        RETURN
      ENDIF
      DO IBD = 1,NBD
        DO INM = 1,NUMBER_BDS
          IF (LORRS(INM).EQ.'R') THEN
            IF (LEN_MNTS(INM).EQ.BD_LENS(IBD) .AND. &
     &          NAME_MNTS(INM)(1:LEN_MNTS(INM)).EQ. &
     &          BLOCK_DEVS(IBD)(1:BD_LENS(IBD))) THEN
              BD_LENS(IBD) = LEN_TRUES(INM)
              BLOCK_DEVS(IBD) = NAME_TRUES(INM)(1:LEN_TRUES(INM))
            END IF
          END IF
        END DO
      END DO
!
!     Step 2:
!
!     Now we have list(s) of the block devices and the amount of free space
!       on each one.   Each directory on a block device has access to all
!       the free space on that block device.  So find the block device each
!       directory is on and just read the amount of free space the device has.
!
!       The search strategy will be to look for each directory in the
!       list of block devices generated by the df command.  If a directory
!       isn't there, then it must be a
!       subdirectory of one of the block devices, so keep trying the next
!       directory up until one of the block devices is reached.
!
      DO IP = 1,NPATHS
!
!       Try the target directory first.
!
        DO ICT = 1,140
          CURPATH(ICT:ICT) = ' '
        END DO
        CURLEN = PATHLENS(IP)
        CURPATH(1:CURLEN) = PATHS(IP)(1:CURLEN)
!
          BDFOUND = .FALSE.
          DO WHILE (.NOT.BDFOUND)
!
!           Search for the current directory (either the target directory or
!             one of the directories it's on) in the list of block devices.
!
            DO IBD = 1,NBD
              IF (  (CURLEN.EQ. BD_LENS(IBD))         .AND. &
     &             (CURPATH(1:CURLEN).EQ.BLOCK_DEVS(IBD)(1:BD_LENS(IBD)) )) &
     &             THEN !found it!
                BDFOUND = .TRUE.
                ISPACES(IP) = IFREES(IBD)
              END IF
            END DO
            IF (BDFOUND) GO TO 104
!
! ---------- The current directory wasn't a block device, so prepare to check
! ---------- the next directory up.
!
            SLFOUND = .FALSE.
            DO WHILE (CURLEN.GT.1 .AND. (.NOT.SLFOUND))
              IF (CURPATH(CURLEN:CURLEN) .EQ.'/') SLFOUND = .TRUE.
              CURPATH(CURLEN:CURLEN) = ' '
              CURLEN = CURLEN - 1
            END DO !finding the next directory up
 104        CONTINUE
          END DO !checking the block device list for the block device the
!target directory's on
      END DO !running over target directories
!
      RETURN
      END  !#!  FSPCE_SOLVE  #!#
