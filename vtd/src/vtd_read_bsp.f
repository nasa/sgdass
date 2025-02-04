      SUBROUTINE VTD_READ_BSP ( FILBSP, FL_LOAD_COV, M_BSP, L_BSP, BSP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_READ_BSP reads the file with coefficients of site      *
! *   displacements development with the expansion with B-spline         *
! *   basis and puts its contents into the fields of the BSP array.      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     FILBSP ( CHARACTER  ) -- The name of the file with the           *
! *                              coefficients of site position           *
! *                              variations  expansion into the          *
! *                              B-spline basis.                         *
! * FL_LOAD_COV ( LOGICAL*4 ) -- Flag. If TRUE, covariance is loaded     *
! *                              from the file, if the file has them.    *
! *       M_BSP ( REAL*8    ) -- The maximum number of stations for      *
! *                              which coefficients of station           *
! *                              displacements can be defined.           *
! *       L_BSP ( REAL*8    ) -- The maximum number of stations for      *
! *                              which coefficients of station           *
! *                              displacements can be defined.           *
! *        BSP ( REAL*8     ) -- The object which for a given station    *
! *                              keeps parameters related to the         *
! *                              expansion of residual site              *
! *                              displacements with B-spline basis: the  *
! *                              number of knots, spline degree, epochs  *
! *                              of knots, values of spline              *
! *                              coefficients, station name, apriori     *
! *                              coordinates.                            *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                            Input: switch IUER=0 -- no error messages *
! *                                   will be generated even in the case *
! *                                   of error. IUER=-1 -- in the case   *
! *                                   of error the message will be put   *
! *                                   on stdout.                         *
! *                            Output: 0 in the case of successful       *
! *                                    completion and non-zero in the    *
! *                                    case of error.                    *
! *                                                                      *
! * ### 15-MAR-2005  VTD_READ_BSP  v3.2 (c)  L. Petrov  16-NOV-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INTEGER*4  M_BSP, L_BSP
      TYPE      ( BSPPOS__TYPE ) :: BSP(M_BSP)
      CHARACTER  FILBSP*(*)
      LOGICAL*4  FL_LOAD_COV
      INTEGER*4  IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 32*1024 )
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER  SOL_ID*64, SOL_DATE*32, STR*128
      LOGICAL*4  LEX
      REAL*8     TAI
      INTEGER*4  J1, J2, J3, IND, MJD, NBUF, IOS, I_STA, I_NOD, &
     &           ICMP1, ICMP2, INOD1, INOD2, IER
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( ILEN(FILBSP) == 0 ) THEN
           CALL ERR_LOG ( 3411, IUER, 'VTD_READ_BSP', 'Empty name of the '// &
     &         'parametr FILBSP' )
           RETURN 
      END IF
!
! --- Check: wether the file with BSP exists
!
      INQUIRE ( FILE=FILBSP, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 3412, IUER, 'VTD_READ_BSP', 'File with B-spline '// &
     &         'coefficients '//FILBSP(1:I_LEN(FILBSP))// &
     &         ' was not found' )
           RETURN 
      END IF
!
! --- Alocate memory for the buffer where the file will be read
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( MBUF*128, STR )
           CALL ERR_LOG ( 3413, IUER, 'VTD_READ_BSP', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes for an internal '// &
     &         'buffer' ) 
           RETURN 
      END IF
!
! --- Read the file into the buffer
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILBSP, MBUF, BUF, NBUF, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3414, IUER, 'VTD_READ_BSP', 'Error in reading '// &
     &         'the file with coefficients of site displacements expansion '// &
     &         'with B-spline basis '//FILBSP )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(BSPPOS__LABEL)) .NE. BSPPOS__LABEL ) THEN
           CALL ERR_LOG ( 3415, IUER, 'VTD_READ_BSP', 'Input file '// &
     &          FILBSP(1:I_LEN(FILBSP))//' is not in BSP '// &
     &         'format. The first line is "'//BUF(1)(1:I_LEN(BUF(1)))// &
     &         '"  file the BSP '//'format label "'//BSPPOS__LABEL// &
     &         '" was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(NBUF)(1:LEN(BSPPOS__LABEL)) .NE. BSPPOS__LABEL ) THEN
           CALL ERR_LOG ( 3416, IUER, 'VTD_READ_BSP', 'Input file '// &
     &          FILBSP(1:I_LEN(FILBSP))//' was not read to '// &
     &         'the end. The last line is "'//BUF(NBUF)(1:I_LEN(BUF(NBUF)))// &
     &         '"  file the BSP '//'format label "'//BSPPOS__LABEL// &
     &         '" was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Initialzation
!
      DO 410 J1=1,M_BSP
         CALL NOUT  ( SIZEOF(BSP(J1)), BSP(J1) )
         CALL CLRCH ( BSP(J1)%STATION )
	 CALL CLRCH ( BSP(J1)%SOLUTION_ID )
	 CALL CLRCH ( BSP(J1)%SOLUTION_DATE )
         BSP(J1)%FILE_NAME = FILBSP
         BSP(J1)%FL_EST_COO = .FALSE.
         BSP(J1)%FL_EST_VEL = .FALSE.
         BSP(J1)%FL_EST_SPL = .FALSE.
         BSP(J1)%FL_COV_AVL = .FALSE.
 410  CONTINUE 
!
! --- The first run of buffer parsing
!
      DO 420 J1=2,NBUF-1
!
! ------ Bypass comments
!
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 420
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 420
         IF ( BUF(J1)(1:LEN('SOL_ID: ')) == 'SOL_ID: ' ) THEN
!
! =========== SOL_ID-record
!
              SOL_ID = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('SOL_DATE: ')) == 'SOL_DATE: ' ) THEN
!
! =========== SOL_DATE-record
!
              SOL_DATE = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('N_STA: ')) == 'N_STA: ' ) THEN
!
! ----------- N_STA record
!
              READ ( UNIT=BUF(J1)(8:11), FMT=*, IOSTAT=IOS ) L_BSP
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3417, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the N_STA-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              IF ( L_BSP > M_BSP ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_BSP, STR )
                   CALL ERR_LOG ( 3418, IUER, 'VTD_READ_BSP', 'File '// &
     &                  FILBSP(1:I_LEN(FILBSP))//' contains too many '// &
     &                 'stations: '//BUF(J1)(8:11)//' -- more than '// &
     &                  STR )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
         END IF
 420  CONTINUE 
!
! --- The second run of buffer parsing
!
      DO 430 J3=2,NBUF-1
         IF ( BUF(J3)(1:1)  .EQ. '#' ) GOTO 430
         IF ( ILEN(BUF(J3)) .EQ.  0  ) GOTO 430
         IF ( BUF(J3)(1:LEN('L_DEG: ')) == 'L_DEG: ' .OR. &
     &        BUF(J3)(1:LEN('N_NOD: ')) == 'N_NOD: ' .OR. &
     &        BUF(J3)(1:LEN('R_EPC: ')) == 'P_EST: ' .OR. &
     &        BUF(J3)(1:LEN('P_EST: ')) == 'R_EPC: ' .OR. &
     &        BUF(J3)(1:LEN('P_VEL: ')) == 'P_VEL: ' .OR. &
     &        BUF(J3)(1:LEN('EPOCH: ')) == 'EPOCH: ' .OR. &
     &        BUF(J3)(1:LEN('B_SPL: ')) == 'B_SPL: ' .OR. &
     &        BUF(J3)(1:LEN('B_COV: ')) == 'B_COV: '      ) THEN
!
! ----------- Get station index
!
              READ ( UNIT=BUF(J3)(19:22), FMT=*, IOSTAT=IOS ) I_STA
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3419, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the spline degree at line '// &
     &                  STR(1:I_LEN(STR))//' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3420, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record, line '// &
     &                 STR(1:I_LEN(STR))//' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              IF ( I_STA > L_BSP .OR. I_STA .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3421, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'station index, '// &
     &                 'line '//STR(1:I_LEN(STR))//' file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
         END IF
!
         IF ( BUF(J3)(1:LEN('L_DEG: ')) == 'L_DEG: ' ) THEN
!
! =========== Get the spline degree
!
              READ ( UNIT=BUF(J3)(8:11), FMT=*, IOSTAT=IOS ) BSP(I_STA)%L_DEG
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3422, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the spline degree at line '// &
     &                  STR(1:I_LEN(STR))//' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              IF ( BSP(I_STA)%L_DEG > VTD__M_SPD ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( VTD__M_SPD, STR )
                   CALL ERR_LOG ( 3423, IUER, 'VTD_READ_BSP', 'Too big '// &
     &                 'spline degree in file '//FILBSP(1:I_LEN(FILBSP))// &
     &                 ' -- '//BUF(J3)(8:11)//' -- greater than '//STR )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
! ----------- Store important parameters
!
              BSP(I_STA)%STATION = BUF(J3)(25:32)
              BSP(I_STA)%SOLUTION_ID   = SOL_ID
              BSP(I_STA)%SOLUTION_DATE = SOL_DATE
              BSP(I_STA)%FILE_NAME     = FILBSP
           ELSE IF ( BUF(J3)(1:LEN('N_NOD:')) == 'N_NOD:' ) THEN
!
! =========== Get the number of knots
!
              READ ( UNIT=BUF(J3)(8:11), FMT=*, IOSTAT=IOS ) BSP(I_STA)%L_NOD
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3424, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the number of spline nodes at line '// &
     &                  STR(1:I_LEN(STR))//' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              IF ( BSP(I_STA)%L_NOD > VTD__M_NOD ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( VTD__M_NOD, STR )
                   CALL ERR_LOG ( 3425, IUER, 'VTD_READ_BSP', 'Too many '// &
     &                 'spline knots in file '//FILBSP(1:I_LEN(FILBSP))// &
     &                 ' -- '//BUF(J3)(8:11)//' -- greater than '//STR )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
         END IF
!
         IF ( BUF(J3)(1:LEN('EPOCH: ')) == 'EPOCH: ' .OR. &
     &        BUF(J3)(1:LEN('B_SPL: ')) == 'B_SPL: '      ) THEN
!
! ----------- Get knot index
!
              READ ( UNIT=BUF(J3)(8:11), FMT=*, IOSTAT=IOS ) I_NOD
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3426, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the spline degree at line '// &
     &                  STR(1:I_LEN(STR))//' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3427, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record, line '// &
     &                  STR(1:I_LEN(STR))//' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              IF ( I_NOD <  1-BSP(I_STA)%L_DEG  .OR. &
     &             I_NOD >    BSP(I_STA)%L_NOD       ) THEN
!
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   WRITE ( 6, * ) ' BSP(I_STA)%L_DEG = ', BSP(I_STA)%L_DEG, ' I_NOD = ', I_NOD
                   CALL ERR_LOG ( 3428, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong knot '// &
     &                 'index, line '//STR(1:I_LEN(STR))//' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
         END IF
!
         IF ( BUF(J3)(1:LEN('L_DEG: ')) == 'L_DEG: ' ) THEN
              CONTINUE 
           ELSE IF ( BUF(J3)(1:LEN('N_NOD:')) == 'N_NOD:' ) THEN
              CONTINUE 
           ELSE IF ( BUF(J3)(1:LEN('P_EST: ')) == 'R_EPC: ' ) THEN
!
! =========== Get the reference epoch
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J3)(35:57), MJD, TAI, IER ) 
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3429, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'reference date at line '//STR(1:I_LEN(STR))// &
     &                 ' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              BSP(I_STA)%TIM_REF = (MJD - J2000__MJD)*86400.0D0 + &
     &                             (TAI - 43200.0D0)
           ELSE IF ( BUF(J3)(1:LEN('R_EPC: ')) == 'P_EST: ' ) THEN
!
! =========== Get the global site position estimate
!
              READ ( UNIT=BUF(J3)(35:48), FMT=*, IOSTAT=IER ) BSP(I_STA)%COO(1)
              IF ( IOS .NE. 0 .OR. IS_R8_NAN(BSP(I_STA)%COO(1)) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3430, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'float format line '//STR(1:I_LEN(STR))// &
     &                 ' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J3)(50:63), FMT=*, IOSTAT=IER ) BSP(I_STA)%COO(2)
              IF ( IOS .NE. 0 .OR. IS_R8_NAN(BSP(I_STA)%COO(2)) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3431, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'float format line '//STR(1:I_LEN(STR))// &
     &                 ' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J3)(65:78), FMT=*, IOSTAT=IER ) BSP(I_STA)%COO(3)
              IF ( IOS .NE. 0 .OR. IS_R8_NAN(BSP(I_STA)%COO(3)) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3432, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'float format line '//STR(1:I_LEN(STR))// &
     &                 ' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              BSP(I_STA)%FL_EST_COO = .TRUE.
           ELSE IF ( BUF(J3)(1:LEN('P_VEL: ')) == 'P_VEL: ' ) THEN
!
! =========== Get the global site velocity estimate
!
              READ ( UNIT=BUF(J3)(35:48), FMT='(D15.6)', IOSTAT=IER ) BSP(I_STA)%VEL(1)
              IF ( IOS .NE. 0 .OR. IS_R8_NAN(BSP(I_STA)%VEL(1)) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3433, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'float format line '//STR(1:I_LEN(STR))// &
     &                 ' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J3)(50:63), FMT='(D15.6)', IOSTAT=IER ) BSP(I_STA)%VEL(2)
              IF ( IOS .NE. 0 .OR. IS_R8_NAN(BSP(I_STA)%VEL(2)) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3434, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'float format line '//STR(1:I_LEN(STR))// &
     &                 ' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J3)(65:78), FMT='(D15.6)', IOSTAT=IER ) BSP(I_STA)%VEL(3)
              IF ( IOS .NE. 0 .OR. IS_R8_NAN(BSP(I_STA)%VEL(3)) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3435, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'float format line '//STR(1:I_LEN(STR))// &
     &                 ' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              BSP(I_STA)%FL_EST_VEL = .TRUE.
           ELSE IF ( BUF(J3)(1:LEN('EPOCH: ')) == 'EPOCH: ' ) THEN
!
! =========== Get the knot epoch
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J3)(35:57), MJD, TAI, IER ) 
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3436, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'knot date at line '//STR(1:I_LEN(STR))// &
     &                 ' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              BSP(I_STA)%TIM(I_NOD)= (MJD - J2000__MJD)*86400.0D0 + &
     &                               (TAI - 43200.0D0)
           ELSE IF ( BUF(J3)(1:LEN('B_SPL: ')) == 'B_SPL: ' ) THEN
!
! =========== Get coefficients of B-spline
!
              READ ( UNIT=BUF(J3)(36:48), FMT='(F13.6)', IOSTAT=IER ) &
     &               BSP(I_STA)%SPL(I_NOD,1)
              IF ( IOS .NE. 0 .OR. IS_R8_NAN (BSP(I_STA)%SPL(I_NOD,1)) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3437, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'float format line '//STR(1:I_LEN(STR))// &
     &                 ' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J3)(50:62), FMT='(F13.6)', IOSTAT=IER ) &
     &               BSP(I_STA)%SPL(I_NOD,2)
              IF ( IOS .NE. 0 .OR. IS_R8_NAN (BSP(I_STA)%SPL(I_NOD,2)) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3438, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'float format line '//STR(1:I_LEN(STR))// &
     &                 ' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J3)(64:76), FMT='(F13.6)', IOSTAT=IER ) &
     &               BSP(I_STA)%SPL(I_NOD,3)
              IF ( IOS .NE. 0 .OR. IS_R8_NAN (BSP(I_STA)%SPL(I_NOD,3)) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J3, STR )
                   CALL ERR_LOG ( 3439, IUER, 'VTD_READ_BSP', 'Error in '// &
     &                 'decoding the '//BUF(J3)(1:6)//'-record: wrong '// &
     &                 'float format line '//STR(1:I_LEN(STR))// &
     &                 ' of file '//FILBSP )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              BSP(I_STA)%FL_EST_SPL = .TRUE.
            ELSE IF ( BUF(J3)(1:LEN('SOL_ID: '))   == 'SOL_ID: '   ) THEN
              CONTINUE 
            ELSE IF ( BUF(J3)(1:LEN('SOL_DATE: ')) == 'SOL_DATE: ' ) THEN
              CONTINUE 
            ELSE IF ( BUF(J3)(1:LEN('N_STA: '))    == 'N_STA: '    ) THEN
              CONTINUE 
            ELSE IF ( BUF(J3)(1:LEN('S: '))        == 'S: '        ) THEN
              CONTINUE 
            ELSE IF ( BUF(J3)(1:LEN('B_COV: '))    == 'B_COV: '    ) THEN
              IF ( FL_LOAD_COV ) THEN
                   IF ( .NOT. BSP(I_STA)%FL_COV_AVL ) THEN
                        ALLOCATE ( BSP(I_STA)%COV(1-BSP(I_STA)%L_DEG:BSP(I_STA)%L_NOD+1, &
     &                                         1-BSP(I_STA)%L_DEG:BSP(I_STA)%L_NOD+1, &
     &                                         3,3), STAT=IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( 8*(BSP(I_STA)%L_DEG+BSP(I_STA)%L_NOD+1)**2*9, &
     &                                    STR )
                             CALL ERR_LOG ( 3440, IUER, 'VTD_READ_BSP', &
     &                           'Failure to allocate '//STR(1:I_LEN(STR))// &
     &                           ' bytes of memory for covariance matrix' )
                             DEALLOCATE ( BUF )
                             RETURN 
                        END IF
                   END IF
!
                   BSP(I_STA)%FL_COV_AVL = .TRUE.
                   CALL CHIN ( BUF(J3)(43:43), ICMP1 )
                   CALL CHIN ( BUF(J3)(66:66), ICMP2 )
                   CALL CHIN ( BUF(J3)(53:56), INOD1 )
                   CALL CHIN ( BUF(J3)(76:79), INOD2 )
                   IF ( ICMP1 < 1  .OR.  &
     &                  ICMP1 > 3  .OR.  &
     &                  ICMP2 < 1  .OR.  &
     &                  ICMP2 > 3        ) THEN
!
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 3441, IUER, 'VTD_READ_BSP', 'Wrong '// &
     &                      'component index: '//BUF(J3)(1:79)// &
     &                      'at the line '//STR(1:I_LEN(STR))// &
     &                      ' of file '//FILBSP )
                        DEALLOCATE ( BSP(I_STA)%COV )
                        DEALLOCATE ( BUF )
                        RETURN 
                   END IF
                   IF ( INOD1 < 1-BSP(I_STA)%L_DEG  .OR.  &
     &                  INOD1 > 1+BSP(I_STA)%L_NOD  .OR.  &
     &                  INOD2 < 1-BSP(I_STA)%L_DEG  .OR.  &
     &                  INOD2 > 1+BSP(I_STA)%L_NOD        ) THEN
!
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 3442, IUER, 'VTD_READ_BSP', 'Wrong '// &
     &                      'component index: '//BUF(J3)(1:79)// &
     &                      'at the line '//STR(1:I_LEN(STR))// &
     &                      ' of file '//FILBSP )
                        DEALLOCATE ( BSP(I_STA)%COV )
                        DEALLOCATE ( BUF )
                        RETURN 
                   END IF
                   READ ( UNIT=BUF(J3)(87:99), FMT=*, IOSTAT=IER ) &
     &                    BSP(I_STA)%COV(INOD1,INOD2,ICMP1,ICMP2)
                   IF ( IER .NE. 0 .OR. IS_R8_NAN (BSP(I_STA)%COV(INOD1,INOD2,ICMP1,ICMP2)) ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J3, STR )
                        CALL ERR_LOG ( 3443, IUER, 'VTD_READ_BSP', 'Error '// &
     &                      'on reading covariance value '//BUF(J3)(87:99)// &
     &                      'at the line '//STR(1:I_LEN(STR))// &
     &                      ' of file '//FILBSP )
                        DEALLOCATE ( BSP(I_STA)%COV )
                        DEALLOCATE ( BUF )
                        RETURN 
                   END IF
                   BSP(I_STA)%COV(INOD2,INOD1,ICMP1,ICMP2) = &
     &                 BSP(I_STA)%COV(INOD1,INOD2,ICMP1,ICMP2)
              END IF
            ELSE 
              CALL CLRCH ( STR )
              CALL INCH  ( J3, STR )
              CALL ERR_LOG ( 3444, IUER, 'VTD_READ_BSP', 'Unrecognized '// &
     &            'keyword '//BUF(J3)(1:6)//' at the line '// &
     &             STR(1:I_LEN(STR))//' of file '//FILBSP )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
 430  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_READ_BSP  !#!#
