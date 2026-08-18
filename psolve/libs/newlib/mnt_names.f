      SUBROUTINE MNT_NAMES(LOCALHOST,GETUN,INLU,MAX_EXPECTED, &
     &                     NUM_NAMES,MOUNT_NAMES,MOUNT_LENS, &
     &                     BD_NAMES,BD_LENS, &
     &                     PAR_NAMES,PAR_LENS, &
     &                     HOST_NAMES,HOST_LENS, &
     &                     BDF_NAMES,BDF_LENS, &
     &                     LORRS,MSTATUSES,KERR)
!
!     MNT_NAMES
!
      implicit none
!
! 1.  MNT_NAMES PROGRAM SPECIFICATION
!
! 1.1.   MNT_NAMES looks in /etc/checklist and returns several pieces of
!                  information about the block devices listed there.
!                  (See the output argument list below for the specific
!                   information returned.)
!
!
!
! 1.2.   RESTRICTIONS -
!
! 1.3.   REFERENCES - none
!
! 2.  MNT_NAMES INTERFACE
!
! 2.1.   CALLING SEQUENCE:
!
!
!     INPUT VARIABLES:
!
!     GETUN - CALL GETUNIT IF TRUE.
!             USE INLU IF FALSE.
!     INLU - suggested lu number if getun is false.
!     MAX_EXPECTED - MAXIMUM NUMBER OF BLOCK DEVICES THAT CAN BE HANDLED
!     LOCALHOST - true to get actual local host name,
!                false to pass up arbitrary code indicating host is local
!                    without identifying actual host
!
      LOGICAL*2 GETUN,LOCALHOST
      INTEGER*2 MAX_EXPECTED,INLU
!
!     OUTPUT VARIABLES:
!
!        NUM_NAMES - number of block devices found in /etc/checklist
!        MOUNT_NAMES - the names under which block devices are mounted
!                      on the local machine
!             e.g., /data16, /MOUNTS/aquila/data10
!             comes from: entire field 2 of /etc/checklist
!        MOUNT_LENS - name lengths
!        BD_NAMES - true block device names (the names by which the block
!                     devices are known on the machines where they live)
!             e.g., /dev/dsk/c201d3s0, /data10
!             comes from: last half of field 1 of /etc/checklist for remote
!                            block devices
!                         all of field 1 for local block devices
!        BD_LENS - name lengths
!        PAR_NAMES -  more user friendly forms of the block device names.
!             e.g., /data16, /data10
!             comes from: the last third of field 2 for remote block devices
!                         all of field 2 for local block devices
!        PAR_LENS - name lengths
!        HOST_NAMES - names of the machines on which the block devices live
!                     (for local block devices, returns ---)
!             comes from: first half of field 1 for remote block devices
!                         --- may be arbitrarily set for local block devices
!                           OR the actual local host may be passed back,
!                              depending on an input flag.
!                            In the latter case, the name comes from a
!                            system call.
!
!        HOST_LENS - name lengths
!        BDF_NAMES - full block device fields
!             block_device for local block devices or
!               machine:block_device for remote ones
!                e.g., /dev/dsk/c201d3s0 or aquila:/data10
!             comes from:  entire field 1
!        BDF_LENS - name lengths
!        LORRS - identifies each block device as local (L) or remote (R)
!        MSTATUSES - identifies each block device as included for mounting (I)
!                  or temporarily excluded, but still part of the file system
!                       (commented out with an initial field of ##) (E)
!                  (Any other line starting with # is a comment and is ignored)
!
!        KERR - error return - 0 if ok
!
         INTEGER*2 NUM_NAMES,MOUNT_LENS(*),BD_LENS(*),KERR
         INTEGER*2  PAR_LENS(*), HOST_LENS(*), BDF_LENS(*)
         CHARACTER*1 LORRS(*), MSTATUSES(*)
         CHARACTER*140 MOUNT_NAMES(*),BD_NAMES(*)
         CHARACTER*140 PAR_NAMES(*),HOST_NAMES(*), BDF_NAMES(*)
!
! 2.2.   COMMON BLOCKS USED: none
!
! 2.3.   DATA BASE ACCESSES: none
!
! 2.4.   EXTERNAL INPUT/OUTPUT: none
!
! 2.5.   SUBROUTINE INTERFACE:
!
!     CALLING SUBROUTINES: utility
!
!     CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IUNIT,GETUNIT,ILU,IERR,COLON_PT,FULL_LEN, &
     &          TRIMLEN
      CHARACTER*180 LINEBUF,RESTBUF,CURFIELD
      CHARACTER*1 LINE_TYPE
      INTEGER*2 ILN,ICT,ISLASH_PT,ISLASH_CT
      INTEGER*4 IERR4
      CHARACTER*64 CHOST_LOCAL
      INTEGER*2 IHOST_LOCAL(32),J,CHOST_LEN
      EQUIVALENCE (IHOST_LOCAL(1),CHOST_LOCAL)
      CHARACTER*2 CNULL,CCHAR
      INTEGER*2 INULL,ICHAR,ICT2
      EQUIVALENCE (INULL,CNULL)
      INTEGER*4 J64, FC_GETHOSTNAME
      CHARACTER  FILE_CHECK*16
#ifdef LINUX
      DATA  FILE_CHECK / '/etc/fstab      ' /
#else
      DATA  FILE_CHECK / '/etc/checklist  ' /
#endif
      DATA J64 /64/
      SAVE IUNIT
      INTEGER*4, EXTERNAL :: I_LEN
!
!
! 4.  CONSTANTS USED:
!
! 5.  INITIALIZED VARIABLES:
!
      DATA IUNIT /0/
!
! 6.  PROGRAMMER: K. Baver 7/24/95
!
!     LAST MODIFIED:
!
!     kdb 970801 Fix error: access /etc/checklist, not /etc/mnttab, since we
!                want to know general information about the directories,
!                not their mount statuses.
!                (Note: this involves more than just changing the name of a
!                 file.  The code must now also ignore comment lines,
!                 but include lines starting with "## ", lines representing
!                 block devices that are temporarily unmounted, but
!                 nevertheless are part of the file system and have valid
!                 information to be reported.)
!     kdb 970801 Pass up more information from /etc/checklist
!                 (host names, user friendly forms of block device names,
!                  full block device field names,  status for mounting).
!     kdb 970804 Optionally pass up true local host name
!     kdb 971126 Correct the declarations of argument arrays from (1)
!                (which causes a range error at element 2 in solve/fspce_solve)
!                to (*).
!     BA  990406 Fixed to allow for blank lines in /etc/checklist file
!                (as on CasA at USNO).  Also greatly improved error
!                handling.
!     pet 2000.05.26 Fixed a bug: th previous version erroneously computed
!                    ISLASH_PT and may set it to zero.
!     kdb 2003.07.25 For lahey linux: change gethostname call to fc_gethostname 
!                    call.
!     pet 2007.12.06 Transformed hard-corded name /etc/checklist into
!                    variavle FILE_CHECK and added optional compilation.
!                    This variable is /etc/fstab under linus
!
!     PROGRAM STRUCTURE
!
      KERR = -999
!
!     if the actual local host name is desired, get it now so that
!       it can be inserted into the output array at the proper time
!
      IF (LOCALHOST) THEN
        IERR4 = FC_GETHOSTNAME(CHOST_LOCAL, J64)
        IF (IERR4.LT.0) THEN
          WRITE(6,"('MNT_NAMES ERROR ',I10,' USING FC_GETHOSTNAME')") &
     &      IERR4
          KERR = -4
          RETURN
        END IF
!
!       Gethostname will fill the unused part of the host name variable
!       with nulls.  Convert to blanks for trimlen.
!       (Note the latter is actually now unnecessary, as trimlen now
!       checks for nulls.  BA 99.04.06.)
!
        DO ICT2 = 1,32
          WRITE(CCHAR,"(A2)") IHOST_LOCAL(ICT2)
          DO J = 1,2
            CNULL(2:2) = CCHAR(J:J)
            READ(CNULL,"(A2)") ICHAR
            IF (ICHAR .EQ. 0)  CCHAR(J:J) = ' '
          END DO
          READ(CCHAR,"(A2)") IHOST_LOCAL(ICT2)
        END DO
        CHOST_LEN = TRIMLEN(CHOST_LOCAL)
        if(chost_len.le.0) then
          write(6,"('ERROR in ../newlib/mnt_names.f.  Blank' &
     &      ,' host name read via fc_gethostname.')")
          stop
        endif
      ENDIF
!
      IF (GETUN) THEN
        IF (IUNIT.EQ.0) IUNIT = GETUNIT()
        ILU = IUNIT
      ELSE
        ILU = INLU
      ENDIF
      OPEN ( ILU, FILE=FILE_CHECK, IOSTAT=IERR4, ERR=101, STATUS='OLD', &
     &       ACCESS='SEQUENTIAL', FORM='FORMATTED')
 101  IF (IERR4.NE.0) THEN
        WRITE(6,"('ERROR in ../newlib/mnt_names.f:',I5 &
     &    ,' OPENING ',A)") FILE_CHECK(1:I_LEN(FILE_CHECK)), IERR4
        KERR = -1
        RETURN
      END IF
      NUM_NAMES = 0
      DO WHILE (NUM_NAMES .GE. 0)
        READ(ILU,"(A180)",IOSTAT=IERR4,ERR=102,END=103) LINEBUF
 102    IF (IERR4.NE.0) THEN
          WRITE(6,"('ERROR in ../newlib/mnt_names.f:',I5 &
     &      ,' READING ',A)") FILE_CHECK(1:I_LEN(FILE_CHECK)), IERR4
          CLOSE(ILU)
          KERR = -2
          RETURN
        END IF
        IF ((LINEBUF(1:3).EQ.'## '.OR.LINEBUF(1:1).NE.'#').and. &
     &      trimlen(linebuf).ne.0) then
!         line other than a comment or blank
          NUM_NAMES = NUM_NAMES + 1
          IF (NUM_NAMES .GT. MAX_EXPECTED) THEN
            WRITE(6,"('ERROR in ../newlib/mnt_names.f: too many block' &
     &        ,' devices.  Maximum allowed is ',I4)") MAX_EXPECTED
            CLOSE(ILU)
            KERR = -3
            RETURN
          END IF
!         Strip off the initial ## field for a temporarily unusable
!         block device.  At the same time, note whether this block device
!         is currently mountable or not.
          IF (LINEBUF(1:3).EQ.'## ') THEN
            CALL SPLITSTRING(LINEBUF,CURFIELD,LINEBUF)
            MSTATUSES(NUM_NAMES) = 'E'
          ELSE
            MSTATUSES(NUM_NAMES) = 'I'
          ENDIF
          CALL SPLITSTRING(LINEBUF,CURFIELD,RESTBUF)
!
!         Extract all relevant information from field 1.
!         (Field 1 sets the host name, block device name and full block device
!                  field.)
!
!         Field 1 takes one of two forms depending on whether the block
!           device is local or remote:
!
!          local:
!             block_device_name, e.g., /dev/dsk/0s0
!          remove:
!             host:block_device_name e.g., aquila:/data10
!
!          Take the entire field for the full block device field.
!          Then see if it can be broken down into two parts separated
!            by a colon.
!          If so, it's
!            remote and split it into the host and block device name.
!          Otherwise, it's local, so take the full field for the block device
!             name and return the special symbol for local machine.
!             (Let the caller determine the actual machine if necessary,
!              to cut down on extra processing.)
!          At the same time, note whether this block device is local or remote.
!
          COLON_PT = INDEX(CURFIELD,":")
          IF (COLON_PT.NE.0) THEN
!           Found a colon - remote
           LINE_TYPE = 'R'
          ELSE
            LINE_TYPE = 'L'
          ENDIF
!
          LORRS(NUM_NAMES) = LINE_TYPE
!
!         Load the full field into the full block device field.
!
          ILN = TRIMLEN(CURFIELD)
          if(ILN.le.0) then
            write(6,"('ERROR in ../newlib/mnt_names.f.  Blank' &
     &        ,' full block device name read from ',A/ &
     &        ,' offending line is =>',A,'<=')") FILE_CHECK(1:I_LEN(FILE_CHECK)), &
     &           LINEBUF
            stop
          endif
          BDF_NAMES(NUM_NAMES) = CURFIELD(1:ILN)
          BDF_LENS(NUM_NAMES) = ILN
!
!         Get the host.
!
          IF (LINE_TYPE.EQ.'L') THEN
            IF (LOCALHOST) THEN
              HOST_NAMES(NUM_NAMES) = CHOST_LOCAL(1:CHOST_LEN)
              HOST_LENS(NUM_NAMES) =  CHOST_LEN
            ELSE
              HOST_NAMES(NUM_NAMES) = '---'
              HOST_LENS(NUM_NAMES) =  3
            ENDIF
          ELSE
            ILN = COLON_PT - 1
            if(ILN.le.0) then
              write(6,"('ERROR in ../newlib/mnt_names.f.  Blank' &
     &          ,' host name read from ',A/ &
     &          ,' offending line is =>',A,'<=')") FILE_CHECK(1:I_LEN(FILE_CHECK)), LINEBUF
              stop
            endif
            HOST_NAMES(NUM_NAMES) = CURFIELD(1:ILN)
            HOST_LENS(NUM_NAMES) =  ILN
          ENDIF
!
!         Get the block device name.
!
          FULL_LEN = TRIMLEN(CURFIELD)
          IF (LINE_TYPE.EQ.'L') THEN
            BD_NAMES(NUM_NAMES) = CURFIELD(1:FULL_LEN)
            BD_LENS(NUM_NAMES) = FULL_LEN
          ELSE
            BD_NAMES(NUM_NAMES) = CURFIELD(COLON_PT+1:FULL_LEN)
            BD_LENS(NUM_NAMES) = FULL_LEN - COLON_PT
          END IF
          if(bd_lens(num_names).le.0) then
            write(6,"('ERROR in ../newlib/mnt_names.f.  Blank' &
     &        ,' block device name read from ',A/ &
     &        ,' offending line is =>',A,'<=')") FILE_CHECK(1:I_LEN(FILE_CHECK)), LINEBUF
            stop
          endif
!
!         Now extract all relevant information from field 2.
!         (Field 2 sets the mount name and the parent block device name
!            (the "user friendly" form of the block device name).)
!
!         Field 2 takes one of two forms depending on whether the block
!           device is local or remote:
!
!          local:
!             "parent_name"  e.g., /data15
!          remote:
!             /MOUNTS/host/parent_name e.g., /MOUNTS/aquila/data10
!
!          Take the entire field for the mount name.
!          Then, if it's remote, take the last part as the parent name.
!          Otherwise, it's local, and take the whole field.
!
!         Get the mount name.
!
          CALL SPLITSTRING(RESTBUF,CURFIELD,RESTBUF)
          FULL_LEN = TRIMLEN(CURFIELD)
          if(full_len.le.0) then
            write(6,"('ERROR in ../newlib/mnt_names.f.  Blank' &
     &        ,' mount name read from ',A/ &
     &        ,' offending line is =>',A,'<=')") FILE_CHECK(1:I_LEN(FILE_CHECK)), LINEBUF
            stop
          endif
          MOUNT_NAMES(NUM_NAMES) = CURFIELD(1:FULL_LEN)
          MOUNT_LENS(NUM_NAMES) = FULL_LEN
!
!         Get the "parent" block device name.
!
          IF (LINE_TYPE.EQ.'L') THEN
            PAR_NAMES(NUM_NAMES) = CURFIELD(1:FULL_LEN)
            PAR_LENS(NUM_NAMES) = FULL_LEN
          ELSE
!           Find third slash, which marks the start of the "parent" name
            ISLASH_CT = 0
            ISLASH_PT = 0
            DO ICT = 1,FULL_LEN
              IF (CURFIELD(ICT:ICT).EQ.'/') THEN
                ISLASH_CT = ISLASH_CT + 1
                IF (ISLASH_CT.EQ.3) ISLASH_PT = ICT
              ENDIF
            ENDDO
            IF ( ISLASH_PT .LE. 0 ) ISLASH_PT = 1
            PAR_NAMES(NUM_NAMES) = CURFIELD(ISLASH_PT:FULL_LEN)
            PAR_LENS(NUM_NAMES) = FULL_LEN - ISLASH_PT + 1
          ENDIF
          if(par_lens(num_names).le.0) then
            write(6,"('ERROR in ../newlib/mnt_names.f.  Blank' &
     &        ,' parent block device name read from ',A/ &
     &        ,' offending line is =>',A,'<=')") FILE_CHECK(1:I_LEN(FILE_CHECK)), LINEBUF
            stop
          endif
        ENDIF
      END DO
!
 103  CLOSE (ILU)
!
      KERR = 0
!
      RETURN
      END
