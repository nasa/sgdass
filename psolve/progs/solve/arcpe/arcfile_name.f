      SUBROUTINE ARCFILE_NAME ( FINAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  ARCFILE_NAME  makes filenhame for arc-file. It scans   *
! *   consecuently 3 directories ARCDIR ( from glbcm.i ), test whether   *
! *   the arcfile exist (if yes it removes), test disk space (if less    *
! *   then 64 Mb remains on disk in the case where proposed arc-file     *
! *   should be located on the same disk where SPOOL-file is and 16Mb in *
! *   other case -- it goes to the next directory) and generate file     *
! *   name for arc-file. If there is no room on all specified            *
! *   directories then it assigns filename on null-device. That mean     *
! *   that in that case SOLVE will not write down arcfiles (and will not *
! *   let it know to user).                                              *
! *                                                                      *
! *  ###  19-MAY-1997  ARCFILE_NAME  v2.0  (c) L. Petrov 11-JUN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'precm.i'
      INCLUDE    'glbc4.i'
      CHARACTER  FINAM*(*), STR*32, STR_ARC*5, STR_SPO*80, ARCDIR_ZERO*128, &
     &           DEBUG_ARCF*8
!
      INTEGER*8  MIN_FREE_NSP, MIN_FREE_SPL, MIN_FREE, FREE_SIZE, &
     &           FREE_SIZE_BLK, BLK_SIZE
      INTEGER*4  IUER
      INTEGER*4  MIN_FREE__NONSPOOL, MIN_FREE__SPOOL
      PARAMETER  ( MIN_FREE__NONSPOOL =  64*1024*2024  ) !  64Mb -- min free disk space for a sub-matrix
      PARAMETER  ( MIN_FREE__SPOOL    = 216*1024*2024  ) ! 216Mb -- min free disk space fo spool
      INTEGER*4  J1, IL4, ISTRU(128), IP_SPO, IP_ARC
      INTEGER*8  DIR_SIZE_TOT, DIR_SIZE_FREE, VAL_BLK
!
      INTEGER*2  ITR
      LOGICAL*4  LEX
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*2, EXTERNAL :: TRIMLEN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, STATFS
!
      CALL CLRCH    (  DEBUG_ARCF )  
      CALL GETENVAR ( 'DEBUG_ARCF', DEBUG_ARCF )
!
! --- Some decodings
!
      CALL CLRCH  (               STR_ARC ) ! Put blanks to STR_ARC
      CALL INCH   ( IARCNM,       STR_ARC ) ! INTEGER*4 --> CHARACTER
      CALL CHASHR (               STR_ARC ) ! Shift to right
      CALL BLANK_TO_ZERO (        STR_ARC ) ! Replace blanks with zeroes
      CALL CLRCH ( FINAM )  ! Fill FINAM by blanks
!
! --- Determine amount of free space which should be left on disks.
! --- If environament variables are not specified, then use default
!
      CALL GETENVAR ( 'MIN_FREE_NSP_MB', STR )
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL CHIN ( STR, MIN_FREE_NSP )
           MIN_FREE_NSP = MIN_FREE_NSP*1024*1024  ! Trasnformation from MB to blocks
           IF ( MIN_FREE_NSP .LT. 0 ) MIN_FREE_NSP = MIN_FREE__NONSPOOL
         ELSE
           MIN_FREE_NSP = MIN_FREE__NONSPOOL ! To use defaults
      END IF
!
! --- The same for the case when arc-files are located on spooling device
!
      CALL GETENVAR ( 'MIN_FREE_SPL_MB', STR )
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL CHIN ( STR, MIN_FREE_SPL )
           MIN_FREE_SPL = MIN_FREE_SPL*2*1024 ! Trasnform from MB to blocks
           IF ( MIN_FREE_SPL .LT. 0 ) MIN_FREE_SPL = MIN_FREE__SPOOL
         ELSE
           MIN_FREE_SPL = MIN_FREE__SPOOL ! To use defaults
      END IF
!
      DO 410 J1=1,3 ! Scan all three directories
         ITR = TRIMLEN ( ARCDIR(J1) ) ! The number non-blanks symbols in ARCDIR
         IF ( ISOLU .NE. 0 .OR. ( ITR .EQ. 0   .AND.  J1 .EQ. 1 ) ) THEN
!
! ----------- not-forward solution   .OR.
! ----------- Empty directory was the first in the list of arc-directories.
! ----------- In this case we'll write arc-related stuff in NRMF-file
!
              FINAM = PRE_SCR_DIR(1:PRE_SD_LEN)//'NRMF'//PRE_LETRS
!
! ----------- Write down SAVAF in disk in order to allow BACK to read correct
! ----------- name for ARC-file
!
              SAVAF = FINAM
              CALL USE_GLBFIL ( 'OWC' )
              CALL ERR_LOG ( 0, IUER )
              RETURN
            ELSE IF ( ITR .EQ. 0   .AND.  J1 .GT. 1 ) THEN
              GOTO 410
            ELSE
!
! --------- Form ARC-file name
!
            IF ( ARCDIR(J1)(ITR:ITR) .EQ. '/' ) THEN
                 FINAM = ARCDIR(J1)(1:ITR)//'['//STR_ARC//PRE_LETRS
              ELSE IF ( ARCDIR(J1)(ITR:ITR) .NE. ' ' ) THEN
                 FINAM = ARCDIR(J1)(1:ITR)//'/['//STR_ARC//PRE_LETRS
            END IF
!
! --------- Does file exist?
!
            INQUIRE ( FILE=FINAM, EXIST=LEX )
!
! --------- If Yes -- remove it
!
            IF ( LEX ) THEN
!@                 CALL UNLINK ( FINAM(1:I_LEN(FINAM))//CHAR(0) )
!
! -------------- Added this stupod line in order to circrumvent a bug in 
! -------------- HP FORTRAN90 compiler
!
                 FINAM(I_LEN(FINAM)+1:I_LEN(FINAM)+1) = CHAR(0)
                 CALL UNLINK ( FINAM )
            END IF
!
! --------- Call system subroutine for inquring disk space. Free (for ordinary
! --------- user) disk space (in blocks) will be written in the 5-th long-long word
! --------- of ISTRU (If ARCDIR(J1) doesn't exist we will have IL4=-1 and we
! --------- will consider this case as lack of free space)
!
!@            IL4 = STATFS ( ARCDIR(J1)(1:ITR)//CHAR(0), ISTRU )
!
! --------- Added these stupid lines in order to curcumvent bug in HP FORTRAN90
! --------- 2.5.1
!
            ARCDIR_ZERO = ARCDIR(J1)(1:ITR)
            ARCDIR_ZERO(ITR+1:ITR+1) = CHAR(0)
            IL4 = STATFS ( ARCDIR_ZERO, ISTRU )
#ifdef ADR_32BIT             
            FREE_SIZE = ISTRU(5)
#else
            BLK_SIZE = ISTRU(3)
            CALL MEMCPY ( FREE_SIZE_BLK, ISTRU(9), %VAL(8) )
            FREE_SIZE = FREE_SIZE_BLK*BLK_SIZE
#endif
!
! --------- Form the name of the spool-file
!
            CALL CLRCH ( STR_SPO )
            CALL GETENVAR ( 'PSOLVE_SPOOL_DIR', STR_SPO )
            IF ( STR_SPO(1:1) .EQ. ' ' ) STR_SPO = SPOOL_DIR ! From solve.i
            IF ( STR_SPO(I_LEN(STR_SPO):I_LEN(STR_SPO)) .NE. '/') &
     &           STR_SPO(I_LEN(STR_SPO)+1:) = '/'
            STR_SPO(I_LEN(STR_SPO)+1:) = 'SPLF'//PRE_LETRS
!
! --------- Extract the disk name for arcfile and spoolfile
!
            IP_ARC = INDEX ( ARCDIR(J1)(2:), '/' ) + 1
            IP_SPO = INDEX ( STR_SPO(2:),    '/' ) + 1
!
! --------- Comparing disk names
!
            IF ( ARCDIR(J1)(1:IP_ARC) .EQ. STR_SPO(1:IP_SPO) ) THEN
                 MIN_FREE = MIN_FREE_SPL
              ELSE
                 MIN_FREE = MIN_FREE_NSP
            END IF
!
            IF ( DEBUG_ARCF(1:3) == 'YES' ) THEN
                 WRITE ( 6, * ) 'ARCFILE_NAME-155: ARCDIR_ZERO = ', TRIM(ARCDIR_ZERO), ' ISTRU(5)= ', ISTRU(5), ' MIN_FREE = ', MIN_FREE
                 WRITE ( 6, * ) 'ISTRU = ', ISTRU 
                 WRITE ( 6, * ) 'FREE_SIZE= ', FREE_SIZE, ' MIN_FREE= ', MIN_FREE
            END IF
            IF ( IL4 .EQ. 0  .AND.  FREE_SIZE .GT. MIN_FREE ) THEN
!
! -------------- Write down SAVAF in disk in order to allow BACK to read
! -------------- correct name for ARC-file
!
                 SAVAF = FINAM
                 CALL USE_GLBFIL ( 'OWC' )
                 CALL ERR_LOG ( 0, IUER )
                 RETURN
            END IF
         END IF
 410  CONTINUE
!
! --- Alas! We didn't find free space!
!
!      CALL CLRCH ( STR )
!      CALL INCH  ( INT4(MIN_FREE), STR )
!      CALL ERR_LOG ( INT4(8591), IUER, 'ARCFILE_NAME', 'There are no space '//
!     #    'for arc files: less then '//STR(1:I_LEN(STR))//' free blocks are'//
!     #    ' in directories '//ARCDIR(1)(1:I_LEN(ARCDIR(1)))//
!     #    ' '//ARCDIR(2)(1:I_LEN(ARCDIR(2)))//
!     #    ' '//ARCDIR(3)(1:I_LEN(ARCDIR(3)))//' (or some of this '//
!     #    'directories are not exist)'  )
!
      FINAM = '/dev/null/'
      CALL CLRCH ( SAVAF )
      CALL USE_GLBFIL ( 'OWC' )
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  ARCFILE_NAME  #!#
