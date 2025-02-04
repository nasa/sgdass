      PROGRAM    HAR_POS_LAUNCH
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL HAR_POS()
      END  PROGRAM  HAR_POS_LAUNCH
!
! ------------------------------------------------------------------------
!
      SUBROUTINE HAR_POS()
! ************************************************************************
! *                                                                      *
! *   Program HAR_POS analyzes a Solve solution listing in spool-format, *
! *   extracts the part related to estimates of site positions modeled   *
! *   with a sum of harmonics for each station, transforms these         *
! *   estimates  from crust-fixed coordinate system to local topocentric *
! *   coordinate system for each site: Up, East, North and writes        *
! *   results in output file in HARPOS format.                           *
! *                                                                      *
! *  ### 22-MAR-2005    HAR_POS    v1.1 (c)  L. Petrov  23-JUN-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'hsp.i'
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 512*1024 )
      TYPE     ( HSP__TYPE ), ALLOCATABLE :: HSP(:)
      CHARACTER  SPOOL_FIL*128, FILOUT*128, BUF(MBUF)*160, STR*160, STR1*32
      REAL*8     AREA_RADIUS
      PARAMETER  ( AREA_RADIUS = 10.0 )
      LOGICAL*4  LEX, FL_HSP
      INTEGER*4  L_HSP, NBUF, I_STA, IND_SEC, LUN, J1, IOS, IUER
      CHARACTER  HARPOS_VERS*54
      CHARACTER, EXTERNAL :: GET_VERSION*54
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT 
!
      IF ( IARGC() .LT. 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: har_pos  <spool_file>  <output_file>'
           CALL EXIT ( 1 ) 
         ELSE
           CALL GETARG ( 1, SPOOL_FIL )
           CALL GETARG ( 2, FILOUT    )
      END IF
!
      INCLUDE  'har_pos_version.i'
      HARPOS_VERS = GET_VERSION()
!
! --- Check whether the spool file exists
!
      INQUIRE ( FILE=SPOOL_FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 701, -1, 'HAR_POS', 'Spool file '// &
     &                    SPOOL_FIL(1:I_LEN(SPOOL_FIL))//' was not found' )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Open the spool file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=SPOOL_FIL, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 702, -1, 'HAR_POS', 'Spool file '// &
     &                    SPOOL_FIL(1:I_LEN(SPOOL_FIL))//' was not found' )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Infinite cycle of reading the spool file, We searc hfor the
! --- so-called HPE-section. Its contsnts will be put into the buffer array
! --- BUF
!
      FL_HSP = .FALSE.
      L_HSP = 0
      NBUF = 0
      DO 410 J1=1,1024*1024*1024
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .EQ. -1 ) GOTO 810
         IF ( IOS .NE.  0 ) THEN
              CALL CLRCH ( STR ) 
              CALL INCH  ( J1, STR ) 
              CALL CLRCH ( STR1 ) 
              CALL INCH  ( IOS, STR1 ) 
              CALL ERR_LOG ( 703, -1, 'HAR_POS', 'Error '// &
     &             STR1(1:I_LEN(STR1))//' in reading line '// &
     &             STR(1:I_LEN(STR))//' of spool file '//SPOOL_FIL )
              CALL EXIT ( 1 ) 
         END IF
         IF ( STR(1:6) == '1  HPE' ) THEN
!
! ----------- Aga! The HPE-section started
!
              FL_HSP = .TRUE.
              GOTO 410
         END IF
         IF ( STR(1:6) == '2  HPE' ) THEN
!
! ----------- Ogo! The HPE-section ended
!
              FL_HSP = .FALSE.
              GOTO 810
         END IF
!
         IF ( FL_HSP ) THEN
!
! ----------- Copy the line into the buffer
!
              NBUF = NBUF + 1
              IF ( NBUF == 1 ) IND_SEC = J1
              IF ( NBUF > MBUF ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( MBUF, STR )
                   CALL ERR_LOG ( 704, -1, 'HAR_POS', 'Parameter MBUF '// &
     &                 'is too small: '//STR )
                   CALL EXIT ( 1 )
              END IF
              BUF(NBUF) = STR
!
              IF ( STR(1:5) == 'L_HPE' ) THEN
!
! ---------------- Learn the nunber of stations whose coordinates were modeled
! ---------------- as harmonic variations
!
                   CALL CHIN ( STR(6:15), L_HSP )
                   IF ( L_HSP < 1  .OR.  L_HSP > M__HPE ) THEN
                        CALL ERR_LOG ( 705, -1, 'HAR_POS', 'Erro in '// &
     &                      'line '//STR(1:I_LEN(STR))//' of spool file '// &
     &                      SPOOL_FIL(1:I_LEN(SPOOL_FIL))//' -- parameter '// &
     &                     'L_HPE is out of range' )
                        CALL EXIT  ( 1 )
                   END IF
              END IF
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( NBUF == 0 ) THEN
           WRITE ( 6, '(A)' ) 'No harmonic site position variations were found'
           CALL EXIT ( 0 ) 
      END IF
      IF ( NBUF > 0  .AND. FL_HSP ) THEN
           WRITE ( 6, '(A)' ) 'No end of HPE-section was found in the '// &
     &                        'spool file'
           CALL EXIT ( 0 ) 
      END IF
      IF ( L_HSP .LE. 0  ) THEN
           WRITE ( 6, '(A)' ) 'No harmonics were found'
           CALL EXIT ( 0 ) 
      END IF
!
! --- Allocate memory for HSP-objects
!
      ALLOCATE ( HSP(L_HSP), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 707, -1, 'HAR_POS', 'Error in allocating '// &
     &         'memory for HSP array'  )
           CALL EXIT ( 1 ) 
      END IF
      WRITE ( 6, *) ' NBUF=',NBUF, ' L_HSP= ',L_HSP
!
! --- Parse the listing and pur results in array HSP
!
      IUER = -1
      CALL PARSE_HAR_LISTING ( NBUF, BUF, IND_SEC, L_HSP, HSP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 708, -1, 'HAR_POS', 'Error in an attempt to '// &
     &         'parse the input spool-file '//SPOOL_FIL )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Transform the estimates from XYZ crust-fixeed coordinate system to
! --- local topocentric system
!
      IUER = -1
      CALL TRANSFORM_HARPOS ( 1, L_HSP, HSP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 709, -1, 'HAR_POS', 'Error in an attempt to '// &
     &         'transform input set of estimates of harmonic site position '// &
     &         'variations '//SPOOL_FIL )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Write the estimates in the output file
!
      IUER = -1
      CALL WRI_HARPOS ( L_HSP, HSP, HARPOS_VERS, AREA_RADIUS, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 710, -1, 'HAR_POS', 'Error in an attempt to '// &
     &         'write the output HARPOS-file ' )
           CALL EXIT ( 1 ) 
      END IF
      WRITE ( 6, '(A)' ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
!
      END  SUBROUTINE  HAR_POS  !#!#
