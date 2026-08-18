      SUBROUTINE SEL_BDS(LOCALHOST,GETUN,INLU,PRINT_ERR, &
     &               NPATHS,PATHS,PATHLENS, &
     &               MOUNT_NAMES,MOUNT_LENGTHS, &
     &               BD_NAMES,BD_LENGTHS, &
     &               PARENT_NAMES,PARENT_LENGTHS, &
     &               HOST_NAMES,HOST_LENGTHS, &
     &               BDF_NAMES,BDF_LENGTHS, &
     &               LORR_ARRAY,MSTATUS_ARRAY, &
     &               ERRMSG,KERR)
!
      implicit none
!
      INCLUDE "general.i"
!
!     SEL_BDS: Given an input list of directories which may be block devices
!              or just subdirectories of block devices, SEL_BDS finds the
!              information for the block devices on which the input directories
!              live.
!
!     RESTRICTIONS - Assumes no more than a maximum, parameterized number of
!     block devices will be on the system.
!
!     INPUT VARIABLES:
!
!     LOCALHOST - TRUE TO GET LOCAL HOST NAMES.
!                 FALSE TO PASS UP AN ARBITRARY
!                   CODE SIGNIFYING THAT A DIRECTORY'S  HOST IS LOCAL
!                   WITHOUT IDENTIFYING THE HOST
!     GETUN - true to assign unit numbers via getunit,
!             false to use hard coded lus.
!     INLU - if getun is true, -1.
!            if getun is false, lu to use.
!     PRINT_ERR - true to print error messages to the terminal,
!                 false to suppress them
!     NPATHS - the number of input directories
!     PATHS - the names of the input directories
!     PATHLENS - the lengths of the directory names
!
      LOGICAL*2 GETUN,LOCALHOST,PRINT_ERR
      INTEGER*2 INLU,NPATHS
      INTEGER*2 PATHLENS(*)
      CHARACTER*140 PATHS(*)
!
!      OUTPUT VARIABLES:
!
!      MOUNT_NAMES - mount point names
!      MOUNT_LENGTHS - name lengths
!      BD_NAMES - block device names ("True" local names)
!                      (the in-some-cases user-hostile form)
!      BD_LENGTHS - name lengths
!      PARENT_NAMES - user friendly top level names
!      PARENT_LENGTHS - name lengths
!      HOST_NAMES  names of machines where directories live
!      HOST_LENGTHS - name lengths
!      BDF_NAMES - the block device names are, for remote machines,
!                a concatentation of the machine and block device name.
!                This field returns the full form.
!      BDF_LENGTHS - name lengths
!      LORR_ARRAY L for local, R for remote
!      MSTATUS_ARRAY I for included for mounting, E for excluded
!      ERRMSG - error message
!      KERR - error return
!
      CHARACTER*140 MOUNT_NAMES(*),BD_NAMES(*),PARENT_NAMES(*), &
     &              HOST_NAMES(*),BDF_NAMES(*)
      INTEGER*2 MOUNT_LENGTHS(*),BD_LENGTHS(*),PARENT_LENGTHS(*), &
     &              HOST_LENGTHS(*),BDF_LENGTHS(*)
      CHARACTER*1 LORR_ARRAY(*),MSTATUS_ARRAY(*)
      INTEGER*2 KERR
      CHARACTER*(*) ERRMSG
!
!     LOCAL VARIABLES
!
      INTEGER*2 TRIMLEN,ICT,IBD,IBD_PT,IP,LERR
      LOGICAL*2 BDFOUND,SLFOUND
      INTEGER*2 CURLEN
      CHARACTER*140 CURPATH
      INTEGER*2 MAX_EXPECTED,NUMBER_BDS
      INTEGER*2 LEN_MOUNTS(MAX_BDS),LEN_BDS(MAX_BDS), &
     &          LEN_PARS(MAX_BDS),LEN_HOSTS(MAX_BDS), &
     &          LEN_BDFS(MAX_BDS)
      CHARACTER*140 NAME_MOUNTS(MAX_BDS),NAME_BDS(MAX_BDS), &
     &          NAME_PARS(MAX_BDS),NAME_HOSTS(MAX_BDS), &
     &          NAME_BDFS(MAX_BDS)
      CHARACTER*1 LORRS(MAX_BDS),MSTATUSES(MAX_BDS)
!
! 6.  PROGRAMMER: K. Baver 8/4/97
!
!     LAST MODIFIED:
!
!     kdb 970807 Consolidate max_bds parameter in new general.i include file.
!
!     PROGRAM STRUCTURE
!
!
      IF (NPATHS.GT.MAX_BDS) THEN
        WRITE(ERRMSG,"('SEL_BDS HAD TOO MANY (',I5, &
     &     ') INPUT DIRECTORIES - RAISE LIMIT AND RECOMPILE ')") &
     &     NPATHS
        IF (PRINT_ERR) &
     &    WRITE(6,"(A)") ERRMSG(1:TRIMLEN(ERRMSG))
        KERR = -1
        RETURN
      ENDIF
!
!     Get the block device information for all block devices.
!
      MAX_EXPECTED = MAX_BDS
      CALL MNT_NAMES(LOCALHOST,GETUN,INLU,MAX_EXPECTED, &
     &               NUMBER_BDS,NAME_MOUNTS,LEN_MOUNTS, &
     &               NAME_BDS,LEN_BDS, &
     &               NAME_PARS,LEN_PARS, &
     &               NAME_HOSTS,LEN_HOSTS, &
     &               NAME_BDFS,LEN_BDFS, &
     &               LORRS,MSTATUSES,LERR)
      IF (LERR.NE.0) THEN
        WRITE(ERRMSG,"('SEL_BDS HAD MNT_NAMES ERROR ',I5)") LERR
        IF (PRINT_ERR) &
     &    WRITE(6,"(A)") ERRMSG(1:TRIMLEN(ERRMSG))
        KERR = -2
        RETURN
      ENDIF
!
!     Each input directory is a subdirectory of a block device (or possibly
!     the device itself).  Locate each input directory's block device in the
!     list just generated and put the block device's information into the
!     output array.
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
!         Search for the current directory (either the target directory or
!           one of the directories it's on) in the list of block devices.
!
          DO IBD = 1,NUMBER_BDS
            IF (  (CURLEN.EQ. LEN_PARS(IBD))         .AND. &
     &       (CURPATH(1:CURLEN).EQ.NAME_PARS(IBD)(1:LEN_PARS(IBD)) ) ) &
     &         THEN !found it!
              BDFOUND = .TRUE.
              IBD_PT = IBD
            END IF
          END DO
          IF (BDFOUND) THEN
!           Load output arrays
            MOUNT_NAMES(IP) =         NAME_MOUNTS(IBD_PT)
            MOUNT_LENGTHS(IP) =       LEN_MOUNTS(IBD_PT)
            BD_NAMES(IP) =            NAME_BDS(IBD_PT)
            BD_LENGTHS(IP) =          LEN_BDS(IBD_PT)
            PARENT_NAMES(IP) =        NAME_PARS(IBD_PT)
            PARENT_LENGTHS(IP) =      LEN_PARS(IBD_PT)
            HOST_NAMES(IP) =          NAME_HOSTS(IBD_PT)
            HOST_LENGTHS(IP) =        LEN_HOSTS(IBD_PT)
            BDF_NAMES(IP) =           NAME_BDFS(IBD_PT)
            BDF_LENGTHS(IP) =         LEN_BDFS(IBD_PT)
            LORR_ARRAY(IP) =          LORRS(IBD_PT)
            MSTATUS_ARRAY(IP) =       MSTATUSES(IBD_PT)
          ELSE
!
!           The current directory wasn't a block device, so prepare to check
!             the next directory up.
!
            SLFOUND = .FALSE.
            DO WHILE (CURLEN.GT.1 .AND. (.NOT.SLFOUND))
              IF (CURPATH(CURLEN:CURLEN) .EQ.'/') SLFOUND = .TRUE.
              CURPATH(CURLEN:CURLEN) = ' '
              CURLEN = CURLEN - 1
            END DO !finding the next directory up
          ENDIF !block device found vs. not found
        END DO !checking the block device list for the block device the
!target directory's on
      END DO !running over target directories
!
      ERRMSG = ' '
      KERR = 0
!
      RETURN
      END
