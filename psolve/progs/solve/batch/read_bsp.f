      SUBROUTINE READ_BSP ( BSP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_BSP reads the file with coefficients of site          *
! *   displacements development with the expansion with B-spline         *
! *   basis and puts its contents into the fields of the BSP object.     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  BSP ( REAL*8         ) -- The object which for a given station      *
! *                            keeps parameters related to the expansion *
! *                            of residual site diplsacements with       *
! *                            B-spline basis: the number of knots,      *
! *                            spline degree, epochs of knots, values    *
! *                            of spline coefficients, station name,     *
! *                            apriori coordinates.                      *
! *                            In input BSP should have the name of the  *
! *                            input file in the FILE_NAME field.        *
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
! *  ### 15-MAR-2005    READ_BSP   v1.0 (c)  L. Petrov  15-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'bsp.i'
      TYPE      ( BSPSTA__TYPE ) :: BSP
      INTEGER*4  IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 4096 )
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER  STR*32
      LOGICAL*4  LEX
      REAL*8     TAI
      INTEGER*4  J1, J2, IND, MJD, NBUF, IOS, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( ILEN(BSP%FILE_NAME) == 0 ) THEN
           CALL ERR_LOG ( 3771, IUER, 'READ_BSP', 'The field FILE_NAME '// &
     &         'in the object BSP is empty' )
           RETURN 
      END IF
!
! --- Check: wether the file with BSP exists
!
      INQUIRE ( FILE=BSP%FILE_NAME, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 3712, IUER, 'READ_BSP', 'File with B-spline '// &
     &         'coefficients '//BSP%FILE_NAME(1:I_LEN(BSP%FILE_NAME))// &
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
           CALL ERR_LOG ( 3713, IUER, 'READ_BSP', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes for an internal '// &
     &         'buffer' ) 
           RETURN 
      END IF
!
! --- Read the file into the buffer
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( BSP%FILE_NAME, MBUF, BUF, NBUF, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3714, IUER, 'READ_BSP', 'Error in reading '// &
     &         'the file with coefficients of site displacements expansion '// &
     &         'with B-spline basis '//BSP%FILE_NAME )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(BSP__LABEL)) .NE. BSP__LABEL ) THEN
           CALL ERR_LOG ( 3715, IUER, 'READ_BSP', 'Input file '// &
     &          BSP%FILE_NAME(1:I_LEN(BSP%FILE_NAME))//' is not in BSP '// &
     &         'format. The first line is "'//BUF(1)(1:I_LEN(BUF(1)))// &
     &         '"  file the BSP '//'format label "'//BSP__LABEL// &
     &         '" was expected' )
           RETURN 
      END IF
!
      IF ( BUF(NBUF)(1:LEN(BSP__LABEL)) .NE. BSP__LABEL ) THEN
           CALL ERR_LOG ( 3715, IUER, 'READ_BSP', 'Input file '// &
     &          BSP%FILE_NAME(1:I_LEN(BSP%FILE_NAME))//' was not read to '// &
     &         'the end. The last line is "'//BUF(NBUF)(1:I_LEN(BUF(NBUF)))// &
     &         '"  file the BSP '//'format label "'//BSP__LABEL// &
     &         '" was expected' )
           RETURN 
      END IF
!
! --- Parse the buffer
!
      DO 410 J1=2,NBUF-1
!
! ------ Bypass comments
!
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410
         IF ( BUF(J1)(1:1) .EQ. 'K' ) THEN
!
! =========== K-record
!
              READ ( UNIT=BUF(J1)(3:6), FMT='(I9)', IOSTAT=IOS ) BSP%L_NOD
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3716, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the K-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//BSP%FILE_NAME )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(1:1) .EQ. 'D' ) THEN
!
! =========== D-record
!
              READ ( UNIT=BUF(J1)(3:6), FMT='(I4)', IOSTAT=IOS ) BSP%DEGREE
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3717, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the D-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//BSP%FILE_NAME )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(1:1) .EQ. 'S' ) THEN
!
! =========== S-record
!
              BSP%STATION = BUF(J1)(4:11) 
              READ ( UNIT=BUF(J1)(14:26), FMT='(F13.4)', IOSTAT=IOS ) BSP%APR_COO(1)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3718, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the S-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//BSP%FILE_NAME )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J1)(28:40), FMT='(F13.4)', IOSTAT=IOS ) BSP%APR_COO(1)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3719, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the S-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//BSP%FILE_NAME )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J1)(42:54), FMT='(F13.4)', IOSTAT=IOS ) BSP%APR_COO(1)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3720, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the S-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//BSP%FILE_NAME )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(1:1) .EQ. 'E' ) THEN
!
! =========== S-record
!
              READ ( UNIT=BUF(J1)(3:6), FMT='(I4)', IOSTAT=IOS ) IND
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3721, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the S-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//BSP%FILE_NAME )
                   RETURN 
              END IF
!
              IF ( IND < 1-BSP%DEGREE  .OR.  IND > BSP%L_NOD ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3722, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the E-record while parsing line '// &
     &                 STR(1:I_LEN(STR))//' of file '// &
     &                 BSP%FILE_NAME(1:I_LEN(BSP%FILE_NAME))//' index '// &
     &                 BUF(J1)(3:6)//' is out of range'  )
                   RETURN 
              END IF
!
              CALL DATE_TO_TIME ( BUF(J1)(9:31), MJD, TAI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3723, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the E-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//BSP%FILE_NAME )
                   RETURN 
              END IF
              BSP%TIM(IND) = (MJD - J2000__MJD)*86400.0D0 + (TAI - 43200.0D0)
            ELSE IF ( BUF(J1)(1:1) .EQ. 'B' ) THEN
!
! =========== B-record
!
              READ ( UNIT=BUF(J1)(3:6), FMT='(I4)', IOSTAT=IOS ) IND
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3724, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the B-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//BSP%FILE_NAME )
                   RETURN 
              END IF
!
              IF ( IND < 1-BSP%DEGREE  .OR.  IND > BSP%L_NOD-1 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3725, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the B-record while parsing line '// &
     &                  STR(1:I_LEN(STR))//' of file '// &
     &                  BSP%FILE_NAME(1:I_LEN(BSP%FILE_NAME))//' index '// &
     &                  BUF(J1)(3:6)//' is out of range'  )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J1)(9:18),  FMT='(F10.5)', IOSTAT=IOS ) BSP%POS(1,IND)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3726, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the B-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//BSP%FILE_NAME )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J1)(21:30), FMT='(F10.5)', IOSTAT=IOS ) BSP%POS(2,IND)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3727, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the B-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//BSP%FILE_NAME )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J1)(33:42), FMT='(F10.5)', IOSTAT=IOS ) BSP%POS(3,IND)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 3728, IUER, 'READ_BSP', 'Error in '// &
     &                 'decoding the B-record, line '//STR(1:I_LEN(STR))// &
     &                 ' file '//BSP%FILE_NAME )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(1:1) .EQ. 'P' ) THEN
!
! =========== P-record
!
              CONTINUE 
            ELSE IF ( BUF(J1)(1:1) .EQ. 'V' ) THEN
!
! =========== V-record
!
              CONTINUE 
            ELSE IF ( BUF(J1)(1:1) .EQ. 'S' ) THEN
!
! =========== S-record
!
              CONTINUE 
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_BSP 
