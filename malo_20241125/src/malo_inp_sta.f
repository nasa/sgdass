      SUBROUTINE MALO_INP_STA ( MALO, FIL_STA, EDGE_SEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_INP_STA  reads and parses the file with information  *
! *   about stations for AMD_CREATE program. It fills object STA with    *
! *   results of parsing.                                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FIL_STA ( CHARACTER ) -- The name of the input file in            *
! *                             AMD-STA-INPUT format.                    *
! *   EDGE_SEC ( REAL*8    ) -- This adjustment will be subtracted from  *
! *                             the requested date of beginning of the   *
! *                             date range for each station and added to *
! *                             the requested date of the end of the     *
! *                             date range. So, it makes the date range  *
! *                             wider at EDGE_SEC*2 than the requested.  *
! *                             The purpose of this option is to expand  *
! *                             in interval in order to allow better     *
! *                             interpolation close the ends of the      *
! *                             date range. Units: sec.                  *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! * MALO ( MALO__TYPE     ) -- Object that holds internal datastructure  *
! *                            of package malo.                          *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                            Input: switch IUER=0 -- no error messages *
! *                                   will be generated even in the case *
! *                                   of error. IUER=-1 -- in the case   *
! *                                   of error the message will be put   *
! *                                   on stdout.                         *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 21-DEC-2012  MALO_INP_STA  v1.0 (c) L. Petrov 21-DEC-2012  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO
      CHARACTER  FIL_STA*(*)
      REAL*8     EDGE_SEC
      INTEGER*4  IUER
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      CHARACTER    REG*3, DATE_DEF*21
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//',|' )
      DATA        DATE_DEF / '0000.01.01_00:00:00.0' /
      CHARACTER,  ALLOCATABLE :: BUF(:)*80
      REAL*8      PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 )
      CHARACTER  STR*32, STR1*32, DATE_CHR*21
      LOGICAL*4  LEX
      INTEGER*8  SIZE_I8 
      INTEGER*4  IOS1, IOS2, IOS3, NBUF, LIND, IND(2,MIND), IP, IDAY, &
     &           INP_TYP, UNIX_DATE, IS, J1, J2, MBUF, IER
      REAL*8     PHI_GDT, RD, G_ACC, LAT_GCN, DAY_FRAC, UTC_BEG, UTC_END
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO
!
! --- Check whether the file really exists
!
      INQUIRE ( FILE=FIL_STA, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 8511, IUER, 'MALO_INP_STA', 'File '// &
     &                    FIL_STA(1:I_LEN(FIL_STA))//' was not found' )
           RETURN
      END IF
      IS = FILE_INFO ( FIL_STA(1:I_LEN(FIL_STA))//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 8512, IUER, 'MALO_INP_STA', 'File '// &
     &                    FIL_STA(1:I_LEN(FIL_STA))//' was not found' )
           RETURN
      END IF
      MBUF = SIZE_I8/54 + 32
!
      ALLOCATE ( BUF(MBUF), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8513, IUER, 'MALO_INP_STA', 'Failure to allocate '// &
     &                    STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &                   'array BUF' )
           RETURN
      END IF
!
! --- Read the input file into the buffer
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FIL_STA, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8514, IUER, 'MALO_INP_STA', 'Error in reading the '// &
     &         'input file '//FIL_STA(1:I_LEN(FIL_STA))// &
     &         ' was not found' )
           DEALLOCATE ( BUF ) 
           RETURN
      END IF
!
! --- Check the first line. It should contain magic label
!
      IF ( BUF(1)(1:LEN(MALO_STA__LABEL)) .EQ. MALO_STA__LABEL ) THEN 
           CONTINUE
         ELSE 
           CALL TRAN ( 13, BUF(1), BUF(1) )
           CALL ERR_LOG ( 8515, IUER, 'MALO_INP_STA', 'Wrong format of the '// &
     &         'input file '//FIL_STA(1:I_LEN(FIL_STA))// &
     &         ' -- the first line '//MALO_STA__LABEL//' was expected, but '// &
     &         'the file contains line: '//BUF(1) )
           DEALLOCATE ( BUF ) 
           RETURN
      END IF
!
      MALO%NSTA = 0
      DO 410 J1=2,NBUF
         IF ( ILEN(BUF(J1)) .LT.  1  ) GOTO 410 ! skip empty line
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410 ! skip comment line
         MALO%NSTA = MALO%NSTA + 1
 410  CONTINUE 
!
      ALLOCATE ( MALO%STA(MALO%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR ) 
           CALL IINCH  ( MALO%NSTA*SIZEOF(MALO%STA(1)), STR )
           CALL ERR_LOG ( 8516, IUER, 'MALO_INP_STA', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'MALO%STA' )
           MALO%STA_STATUS = MALO__UNDF
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
      MALO%STA_STATUS = MALO__ALLO 
!
      MALO%NSTA = 0
      DO 420 J2=2,NBUF
         IF ( ILEN(BUF(J2)) .LT.  1  ) GOTO 420 ! skip empty line
         IF ( BUF(J2)(1:1)  .EQ. '#' ) GOTO 420 ! skip comment line
!
! ------ Split the line onto words
!
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, REG, -3 )
         IF ( LIND .LT. 6 ) THEN
              CALL CLRCH ( STR )
              CALL INCH   ( J2, STR )
              CALL ERR_LOG ( 8517, IUER, 'MALO_INP_STA', 'Error in parsing '// &
     &            ' the input file: '//FIL_STA(1:I_LEN(FIL_STA))// &
     &            ' -- too few words on line '//STR )
              DEALLOCATE ( BUF ) 
              DEALLOCATE ( MALO%STA ) 
              MALO%STA_STATUS = MALO__UNDF 
              RETURN
         END IF
         MALO%NSTA = MALO%NSTA + 1
!
! ------ Extract the station name
!
         MALO%STA(MALO%NSTA)%NAME = BUF(J2)(IND(1,1):IND(1,2)+7)
!
! ------ Read station coordinates
!
         READ ( UNIT=BUF(J2)(IND(1,2):IND(2,2)), FMT='(F12.3)', IOSTAT=IOS1 ) &
     &          MALO%STA(MALO%NSTA)%COO(1)
         READ ( UNIT=BUF(J2)(IND(1,3):IND(2,3)), FMT='(F12.3)', IOSTAT=IOS2 ) &
     &          MALO%STA(MALO%NSTA)%COO(2)
         READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F12.3)', IOSTAT=IOS3 ) &
     &          MALO%STA(MALO%NSTA)%COO(3)
         IF ( IOS1 .NE. 0  .OR.  IOS2 .NE.0  .OR.  IOS3 .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 8518, IUER, 'MALO_INP_STA', 'Error during '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the input '// &
     &            'file '//FIL_STA(1:I_LEN(FIL_STA))//' -- wrong format '// &
     &            'of station coordinates' )
              DEALLOCATE ( BUF ) 
              DEALLOCATE ( MALO%STA ) 
              MALO%STA_STATUS = MALO__UNDF 
              RETURN
         END IF
!
! ------ Get the start date for the L_STA-th station
!
         CALL CLRCH ( DATE_CHR )
         IF ( BUF(J2)(IND(1,5):IND(2,5)) .EQ. 'start' .OR. &
     &        BUF(J2)(IND(1,5):IND(2,5)) .EQ. 'START' .OR. &
     &        BUF(J2)(IND(1,5):IND(2,5)) .EQ. 'begin' .OR. &
     &        BUF(J2)(IND(1,5):IND(2,5)) .EQ. 'BEGIN'      ) THEN
              DATE_CHR = '1976.05'
            ELSE
              DATE_CHR = BUF(J2)(IND(1,5):IND(2,5))
         END IF
         DATE_CHR = DATE_CHR(1:ILEN(DATE_CHR))//DATE_DEF(ILEN(DATE_CHR)+1:)
!
! ------ Date transformation
!
         CALL ERR_PASS ( IUER, IER )
         CALL DATE_TO_TIME ( DATE_CHR, MALO%STA(MALO%NSTA)%MJD_BEG, UTC_BEG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 8519, IUER, 'MALO_INP_STA', 'Error during '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the input '// &
     &            'file '//FIL_STA(1:I_LEN(FIL_STA))//' -- wrong format '// &
     &            'of start date' )
              DEALLOCATE ( BUF ) 
              DEALLOCATE ( MALO%STA ) 
              MALO%STA_STATUS = MALO__UNDF 
              RETURN
         END IF
         MALO%STA(MALO%NSTA)%TAI_BEG = UTC_BEG
!
! ------ Adjustment of the end of the range
!
         DAY_FRAC = (MALO%STA(MALO%NSTA)%TAI_BEG - EDGE_SEC)/86400.D0
         IDAY = IDINT ( DAY_FRAC )
         MALO%STA(MALO%NSTA)%MJD_BEG = MALO%STA(MALO%NSTA)%MJD_BEG + IDAY
         MALO%STA(MALO%NSTA)%TAI_BEG = MALO%STA(MALO%NSTA)%TAI_BEG - EDGE_SEC &
     &                                 - IDAY*86400.0D0
         IDAY = MALO%STA(MALO%NSTA)%TAI_BEG/86400.0D0
         MALO%STA(MALO%NSTA)%TAI_BEG = MALO%STA(MALO%NSTA)%TAI_BEG - IDAY*86400.0D0
         MALO%STA(MALO%NSTA)%MJD_BEG = MALO%STA(MALO%NSTA)%MJD_BEG + IDAY
!
! ------ Get the end date for the L_STA-th station
!
         CALL CLRCH ( DATE_CHR )
         IF ( BUF(J2)(IND(1,6):IND(2,6)) .EQ. 'end' .OR. &
     &        BUF(J2)(IND(1,6):IND(2,6)) .EQ. 'now' .OR. &
     &        BUF(J2)(IND(1,6):IND(2,6)) .EQ. 'END' .OR. &
     &        BUF(J2)(IND(1,6):IND(2,6)) .EQ. 'NOW'      ) THEN
              DATE_CHR = '2049.12.01'
            ELSE
              DATE_CHR = BUF(J2)(IND(1,6):IND(2,6))
         END IF
         DATE_CHR = DATE_CHR(1:ILEN(DATE_CHR))//DATE_DEF(ILEN(DATE_CHR)+1:)
!
! ------ Date transformation
!
         CALL ERR_PASS ( IUER, IER )
         CALL DATE_TO_TIME ( DATE_CHR, MALO%STA(MALO%NSTA)%MJD_END, UTC_END, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 8521, IUER, 'MALO_INP_STA', 'Error during '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the input '// &
     &            'file '//FIL_STA(1:I_LEN(FIL_STA))//' -- wrong '// &
     &            'format of start date' )
              DEALLOCATE ( BUF ) 
              DEALLOCATE ( MALO%STA ) 
              MALO%STA_STATUS = MALO__UNDF 
              RETURN
         END IF
!
! ------ Set TAI equal UTC for the end of the interval
!
         MALO%STA(MALO%NSTA)%TAI_END = UTC_END
!
         MALO%STA(MALO%NSTA)%TAI_END = MALO%STA(MALO%NSTA)%TAI_END + EDGE_SEC
         IDAY = MALO%STA(MALO%NSTA)%TAI_END/86400.D0
         MALO%STA(MALO%NSTA)%MJD_END = MALO%STA(MALO%NSTA)%MJD_END + IDAY
         MALO%STA(MALO%NSTA)%TAI_END = MALO%STA(MALO%NSTA)%TAI_END - IDAY*86400.0D0
!
! ------ Computation of station longitude, geocentric latitude, height above
! ------ ellipsoid
!
         CALL REF_ELL ( 0, MALO%STA(MALO%NSTA)%COO(1), LAT_GCN, &
     &                  MALO%STA(MALO%NSTA)%LAT_GDT, &
     &                  MALO%STA(MALO%NSTA)%LON, MALO%STA(MALO%NSTA)%HEI_ELL, &
     &                  RD, G_ACC )
         CALL GETENVAR ( 'APLO_HEIGHT_CHECK_DISABLE', STR )
         CALL TRAN ( 11, STR, STR )
         IF ( STR .NE. 'YES' ) THEN
              IF ( MALO%STA(MALO%NSTA)%HEI_ELL .LT. MALO__HEIGHT_MIN .OR. &
     &             MALO%STA(MALO%NSTA)%HEI_ELL .GT. MALO__HEIGHT_MAX      ) THEN
!
                   CALL CLRCH ( STR )
                   WRITE ( UNIT=STR, FMT='(1PE12.5)' ) MALO%STA(MALO%NSTA)%HEI_ELL
                   CALL ERR_LOG ( 8523, IUER, 'MALO_INP_STA', 'Station '// &
     &                  MALO%STA(MALO%NSTA)%NAME//' did not pass sanity '// &
     &                 'check: its height above the referenced ellipsoid is '// &
     &                  STR(1:I_LEN(STR))//' -- a little bit strange, '// &
     &                 'isn''t it? Please, check once more file '//FIL_STA  )
                   RETURN
              END IF
         END IF
 420  CONTINUE
      MALO%STA_STATUS = MALO__LOAD
      DEALLOCATE ( BUF ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_INP_STA  !#!#
