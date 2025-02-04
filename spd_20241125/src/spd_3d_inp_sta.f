      SUBROUTINE SPD_3D_INP_STA ( SPD, EDGE_SEC, HEB_GEOID_BSPL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_INP_STA  reads and parses the file with            *
! *   information  about stations for SPD_3D_COMP and THREM_DEF          *
! *   programs. It fills sub-object SPD%STA with  results of parsing.    *
! *   It also makes auxilliary computations: latitude, longitude,        *
! *   height above the reference ellipsoid, height above the geoid, etc. *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   EDGE_SEC ( REAL*8    ) -- This adjustment will be subtracted from  *
! *                             the requested date of beginning of the   *
! *                             date range for each station and added to *
! *                             the requested date of the end of the     *
! *                             date range. So, it makes the date range  *
! *                             wider at EDGE_SEC*2 than the requested.  *
! *                             The purpose of this option is to expand  *
! *                             in interval in order to allow better     *
! *                             interpolation close the ends of the      *
! *                             date dange. Units: sec.                  *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *  SPD ( SPD_GRID__TYPE ) --  Record with configuration parameters,    *
! *                             and data for the meteoroloigcal grid.    *
! *                             Information about stations is put in     *
! *                             this record.                             *
! * HEB_GEOID_BSPL ( HEB__TYPE ) -- HEB data strcuture that keeps        *
! *                             the array   of coefficients of the geoid *
! *                             heights expansion into the 2D B-spline   *
! *                             basis.                                   *
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
! * ### 20-AUG-2002  SPD_3D_INP_STA  v2.4 (c) L. Petrov 21-NOV-2013  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INCLUDE   'heb.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      TYPE     ( HEB__TYPE    ) :: HEB_GEOID_BSPL
      REAL*8     EDGE_SEC
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 2*SPD__M_STA )
      PARAMETER  ( MIND = 32 )
      CHARACTER  REG*3, DATE_DEF*21
      CHARACTER, ALLOCATABLE :: BUF(:)*80
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//',|' )
      DATA       DATE_DEF / '0000.01.01_00:00:00.0' /
      CHARACTER  STR*32, STR1*32, DATE_CHR*21
      LOGICAL*4  LEX
      INTEGER*4  IOS1, IOS2, IOS3, NBUF, LIND, IND(2,MIND), IP, IDAY, &
     &           INP_TYP, J1, J2, IND_STA, IER
      REAL*8     PHI_GDT, RD, G_ACC, DAY_FRAC
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REAL*8,    EXTERNAL :: GET_GEOID 
!
! --- Check whether the file really exists
!
      INQUIRE ( FILE=SPD%CONF%FIL_STA, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5251, IUER, 'SPD_3D_INP_STA', 'File '// &
     &              SPD%CONF%FIL_STA(1:I_LEN(SPD%CONF%FIL_STA))// &
     &              ' was not found' )
           RETURN
      END IF
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LEN(BUF(1))*MBUF, STR )
           CALL ERR_LOG ( 5252, IUER, 'SPD_3D_INP_STA', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the station buffer' )
           RETURN
      END IF
!
      CALL FREE_HEB ( HEB_GEOID_BSPL )
!
! --- Real the input file into the buffer
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SPD%CONF%FIL_STA, MBUF, BUF, NBUF, IER )
      IF ( IER .Ne. 0 ) THEN
           CALL ERR_LOG ( 5252, IUER, 'SPD_3D_INP_STA', 'Error in reading the '// &
     &         'input file '//SPD%CONF%FIL_STA )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
! --- Check the first line. It should contain magic label
!
      IF ( BUF(1)(1:LEN(AMD_INP__LABEL)) .EQ. AMD_INP__LABEL ) THEN 
           INP_TYP = TYP__AMD
         ELSE IF ( BUF(1)(1:LEN(SIT_INP__LABEL)) .EQ. SIT_INP__LABEL ) THEN 
           INP_TYP = TYP__SIT
         ELSE 
           CALL ERR_LOG ( 5254, IUER, 'SPD_3D_INP_STA', 'Wrong format of the '// &
     &         'input file '//SPD%CONF%FIL_STA(1:I_LEN(SPD%CONF%FIL_STA))// &
     &         ' -- the first line '//AMD_INP__LABEL//' or '// &
     &         SIT_INP__LABEL//' was expected, but '// &
     &         'the file contains line: '//BUF(1) )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      IF ( SPD%NSTA > 0 ) THEN
           IF ( ASSOCIATED ( SPD%STA ) ) THEN
                DEALLOCATE ( SPD%STA )
           END IF
      END IF
!
      SPD%NSTA = 0
      DO 410 J1=2,NBUF
         IF ( ILEN(BUF(J1)) .LT.  1  ) GOTO 410 ! skip empty line
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410 ! skip comment line
         SPD%NSTA = SPD%NSTA + 1
 410  CONTINUE 
!
      ALLOCATE ( SPD%STA(SPD%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( SPD%NSTA*SIZEOF(SPD%STA), STR )
           CALL ERR_LOG ( 5255, IUER, 'SPD_3D_INP_STA', 'Failure to'// &
     &         ' allocate '//STR(1:I_LEN(STR))//' bytes of dynamic'// &
     &         ' memory for array SPD%STA' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      CALL NOUT ( SIZEOF(SPD%STA), SPD%STA )
!
      IND_STA = 0
      DO 420 J2=2,NBUF
         IF ( ILEN(BUF(J2)) .LT.  1  ) GOTO 420 ! skip empty line
         IF ( BUF(J2)(1:1)  .EQ. '#' ) GOTO 420 ! skip comment line
!
! ------ Split the line onto words. We make a special trick
! ------ to handle a case when station name has blanks
! ------ (a terrible idea, I should say!)
!
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, REG, -3 )
         IF ( LIND .LT. 6 ) THEN
              CALL CLRCH ( STR )
              CALL INCH   ( J2, STR )
              CALL ERR_LOG ( 5256, IUER, 'SPD_3D_INP_STA', 'Error in parsing '// &
     &            ' the input file: '//SPD%CONF%FIL_STA(1:I_LEN(SPD%CONF%FIL_STA))// &
     &            ' -- too few words on line '//STR )
              DEALLOCATE ( BUF )
              RETURN
         END IF
!
         IND_STA = IND_STA + 1
!
! ------ Get the station name
!
         SPD%STA(IND_STA)%NAME = BUF(J2)(IND(1,1):IND(1,2)+7)
!
! ------ Read station coordinates
!
         READ ( UNIT=BUF(J2)(IND(1,2):IND(2,2)), FMT='(F12.3)', IOSTAT=IOS1 ) &
     &          SPD%STA(IND_STA)%COO_CFS(1)
         READ ( UNIT=BUF(J2)(IND(1,3):IND(2,3)), FMT='(F12.3)', IOSTAT=IOS2 ) &
     &          SPD%STA(IND_STA)%COO_CFS(2)
         READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F12.3)', IOSTAT=IOS3 ) &
     &          SPD%STA(IND_STA)%COO_CFS(3)
         IF ( IOS1 .NE. 0  .OR.  IOS2 .NE.0  .OR.  IOS3 .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 5257, IUER, 'SPD_3D_INP_STA', 'Error during '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the input '// &
     &            'file '//SPD%CONF%FIL_STA(1:I_LEN(SPD%CONF%FIL_STA))//' -- wrong format '// &
     &            'of station coordinates' )
              DEALLOCATE ( BUF )
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
              DATE_CHR = '1979.01.01'
            ELSE
              DATE_CHR = BUF(J2)(IND(1,5):IND(2,5))
         END IF
         DATE_CHR = DATE_CHR(1:ILEN(DATE_CHR))//DATE_DEF(ILEN(DATE_CHR)+1:)
!
! ------ Date transformation
!
         CALL ERR_PASS ( IUER, IER )
         CALL DATE_TO_TIME ( DATE_CHR, SPD%STA(IND_STA)%MJD_BEG, &
     &                                 SPD%STA(IND_STA)%SEC_BEG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 5258, IUER, 'SPD_3D_INP_STA', 'Error during '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the input '// &
     &            'file '//SPD%CONF%FIL_STA(1:I_LEN(SPD%CONF%FIL_STA))//' -- wrong format '// &
     &            'of start date' )
              DEALLOCATE ( BUF )
              RETURN
         END IF
!
! ------ Adjustment of the end of the range
!
         DAY_FRAC = ( SPD%STA(IND_STA)%SEC_BEG - EDGE_SEC)/86400.D0
         IDAY = IDINT ( DAY_FRAC )
         SPD%STA(IND_STA)%MJD_BEG = SPD%STA(IND_STA)%MJD_BEG + IDAY
         SPD%STA(IND_STA)%SEC_BEG = SPD%STA(IND_STA)%SEC_BEG - &
     &                                   EDGE_SEC - IDAY*86400.0D0
         IF ( SPD%STA(IND_STA)%SEC_BEG .GE. 86400.0D0 ) THEN
              SPD%STA(IND_STA)%SEC_BEG = SPD%STA(IND_STA)%SEC_BEG - 86400.0D0
              SPD%STA(IND_STA)%MJD_BEG = SPD%STA(IND_STA)%MJD_BEG + 1
         END IF
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
         CALL DATE_TO_TIME ( DATE_CHR, SPD%STA(IND_STA)%MJD_END, &
     &                                 SPD%STA(IND_STA)%SEC_END, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 5259, IUER, 'SPD_3D_INP_STA', 'Error during '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the input '// &
     &            'file '//SPD%CONF%FIL_STA(1:I_LEN(SPD%CONF%FIL_STA))//' -- wrong '// &
     &            'format of start date' )
              DEALLOCATE ( BUF )
              RETURN
         END IF
!
! ------ Adjustment of the end of the range
!
         SPD%STA(IND_STA)%SEC_END = SPD%STA(IND_STA)%SEC_END + EDGE_SEC
         IP = SPD%STA(IND_STA)%SEC_END/86400.0D0
         IF ( SPD%STA(IND_STA)%SEC_END .LT. 0 ) IP = IP-1
         SPD%STA(IND_STA)%MJD_END = SPD%STA(IND_STA)%MJD_END - IP
         SPD%STA(IND_STA)%SEC_END = SPD%STA(IND_STA)%SEC_END + IP*86400.0D0
!
! ------ Computation of station longitude, geocentric latitude, height above
! ------ ellipsoid
!
         CALL REF_ELL ( 0, SPD%STA(IND_STA)%COO_CFS, &
     &                 SPD%STA(IND_STA)%LAT_GCN, &
     &                 SPD%STA(IND_STA)%LAT_GDT, &
     &                 SPD%STA(IND_STA)%LON, &
     &                 SPD%STA(IND_STA)%HEI_ELL, &
     &                 SPD%STA(IND_STA)%RD, &
     &                 SPD%STA(IND_STA)%G_ACC )
         CALL GETENVAR ( 'SPD_HEIGHT_CHECK_DISABLE', STR )
         CALL TRAN ( 11, STR, STR )
         IF ( STR .NE. 'YES' ) THEN
              IF ( SPD%STA(IND_STA)%HEI_ELL .LT. HEIGHT_MIN .OR. &
     &             SPD%STA(IND_STA)%HEI_ELL.GT. HEIGHT_MAX      ) THEN
!
                   CALL CLRCH ( STR )
                   WRITE ( UNIT=STR, FMT='(1PE12.5)' ) SPD%STA(IND_STA)%HEI_ELL
!                   CALL ERR_LOG ( 5260, IUER, 'SPD_3D_INP_STA', 'Station '// &
!     &                  SPD%STA(IND_STA)%NAME//' did not pass sanity '// &
!     &                 'check: its height above the referenced ellipsoid is '// &
!     &                  STR(1:I_LEN(STR))//' -- a little bit strange, '// &
!     &                 'isn''t it? Please, check once more file '// &
!     &                  SPD%CONF%FIL_STA  )
!                   DEALLOCATE ( BUF )
!                   RETURN
              END IF
         END IF
!
         CALL ERR_PASS ( IUER, IER  )
         SPD%STA(IND_STA)%HEI_GEOID = SPD%STA(IND_STA)%HEI_ELL - &
     &                                GET_GEOID ( SPD%STA(IND_STA)%LAT_GDT, &
     &                                            SPD%STA(IND_STA)%LON, &
     &                                            SPD%CONF%FIL_GEOID, &
     &                                            HEB_GEOID_BSPL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL ERR_LOG ( 5261, IUER, 'SPD_3D_INP_STA', 'Error during '// &
     &            'an attept to compute the height above the geoid for '// &
     &            'station '//SPD%STA(IND_STA)%NAME )
              DEALLOCATE ( BUF )
              RETURN
         END IF
!
         ALLOCATE ( SPD%STA(IND_STA)%DEL(SPD%CONF%N_EL,SPD%CONF%N_AZ,SPD__MTYP), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 4*SPD%CONF%N_EL*SPD%CONF%N_AZ*SPD%NSTA*SPD__MTYP, STR )
              CALL ERR_LOG ( 5262, IUER, 'SPD_3D_INP_STA', 'Failure in '// &
     &            'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &            ' bytes of dynamic memory for array SPD%STA()%DEL' )
             RETURN 
         END IF
         IF ( SPD%CONF%N_FRQ > 0 ) THEN
              ALLOCATE ( SPD%STA(IND_STA)%OPA(SPD%CONF%N_FRQ,SPD%CONF%N_EL,SPD%CONF%N_AZ), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*SPD%CONF%N_EL*SPD%CONF%N_AZ*SPD%NSTA*SPD%CONF%N_FRQ, STR )
                   CALL ERR_LOG ( 5262, IUER, 'SPD_3D_INP_STA', 'Failure in '// &
     &                  'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                  ' bytes of dynamic memory for array SPD%STA()%OPA' )
                  RETURN 
              END IF
!
              ALLOCATE ( SPD%STA(IND_STA)%TAT(SPD%CONF%N_FRQ,SPD%CONF%N_EL,SPD%CONF%N_AZ), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*SPD%CONF%N_EL*SPD%CONF%N_AZ*SPD%NSTA*SPD%CONF%N_FRQ, STR )
                   CALL ERR_LOG ( 5262, IUER, 'SPD_3D_INP_STA', 'Failure in '// &
     &                  'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                  ' bytes of dynamic memory for array SPD%STA()%TAT' )
                  RETURN 
              END IF
         END IF
 420  CONTINUE
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_INP_STA  !#!#
