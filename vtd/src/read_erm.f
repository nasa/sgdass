      SUBROUTINE READ_ERM ( FILIN, M_DEG, M_ERM, L_DEG, L_ERM, MJD_BEG,  &
     &                      TAI_BEG, TIM_ARR, SPL_ARR, ERR_ARR, COV_ARR, &
     &                      IUER )
! ************************************************************************
! *                                                                      *
! *     Routine READ_ERM  reads the file with the coefficients of        *
! *   B-spline which describes the slow variations of the small          *
! *   perturbation vector of the Earth rotation, so-called empirical     *
! *   Earth rotation model. The input file should conform specifications *
! *   of ERM format.                                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    FILIN ( CHARACTER ) -- Input file in ERM format.                  *
! *    M_DEG ( INTEGER*4 ) -- Maximum expected degree of the B-spline.   *
! *    M_ERM ( INTEGER*4 ) -- Maximum expected number of knots of        *
! *                           the B-spline for the Earth rotation model. *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    L_DEG ( INTEGER*4 ) -- The degree of the B-spline for each        *
! *                           component in the Earth rotation.           *
! *                           Dimension: 3.                              *
! *    L_ERM ( INTEGER*4 ) -- The number of knots of the B-spline for    *
! *                           each component in the Earth rotation.      *
! *                           The knots are numbered from 1 to L_ERM.    *
! *                           Dimension: 3.                              *
! *  MJD_BEG ( INTEGER*4 ) -- Modified Julian date of the epoch of the   *
! *                           first knot.                                *
! *  TAI_BEG ( REAL*8    ) -- Time in TAI of the epoch of the first knot.*
! *  TIM_ARR ( REAL*8    ) -- Array of time tags of the B-spline:        *
! *                           time in seconds elapsed from the first     *
! *                           knot. Dimension: (1-M_DEG:M_ERM,3)         *
! *                           The first index runs over the extended     *
! *                           knots sequence, the second runs over       *
! *                           components in the Earth's rotation.        *
! *  SPL_ARR ( REAL*8    ) -- Array of coefficients of B-spline for      *
! *                           the small perturbation vector of the       *
! *                           Earth rotation. Dimension:                 *
! *                           (1-M_DEG:M_ERM,3). The last dimension runs *
! *                           over the 1st, 2nd and 3rd component of     *
! *                           the small rotation vector.                 *
! *  ERR_ARR ( REAL*8    ) -- Array of formal uncertainties of the       *
! *                           estimates of coefficients of B-spline for  *
! *                           the small perturbation vector of the       *
! *                           Earth rotation. Dimension:                 *
! *                           (1-M_DEG:M_ERM,3). The last dimension runs *
! *                           over the 1st, 2nd and 3rd component of     *
! *                           the small rotation vector.                 *
! *  COV_ARR ( REAL*8    ) -- Covariance matrix of the estimates of      *
! *                           coefficients of B-spline for the small     *
! *                           perturbation vector of the Earth rotation. *
! *                           Dimension: (1-M_DEG:M_ERM,M_DEG,3).        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ###  20-JAN-2006    READ_ERM    v2.0 (c)  L. Petrov  10-MAY-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FILIN*(*)
      INTEGER*4  M_ERM, M_DEG, L_ERM(3), L_DEG(3), MJD_BEG, IUER
      REAL*8     TAI_BEG, TIM_ARR(1-M_DEG:M_ERM,3), SPL_ARR(1-M_DEG:M_ERM,3), &
     &           ERR_ARR(1-M_DEG:M_ERM,3), COV_ARR(1-M_DEG:M_ERM,M_DEG,3)
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 128*1024 )
      CHARACTER  STR*128, ERM__LABEL1*42, ERM__LABEL2*44
      PARAMETER  ( ERM__LABEL1 = '# ERM v  2.2  Format version of 2021.10.30' )
      PARAMETER  ( ERM__LABEL2 = '# ERM   v 1.0  Format version of 2006.01.19 ' )
      CHARACTER*128, ALLOCATABLE :: BUF(:)
      LOGICAL*4  FL_ERM, FL_TIM
      INTEGER*4  J1, J2, J3, MJD, ICMP, ICMP1, ICMP2, KNOT, KNOT2, NBUF, IER
      REAL*8     SEC, E3, E3_DOT 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Initialization
!
      L_DEG   = 0
      L_ERM   = 0
      MJD_BEG = 0
      TAI_BEG = 0.0D0
#ifdef SUN
      CALL NOUT_R8 ( 3*(M_ERM+M_DEG+1), TIM_ARR )
      CALL NOUT_R8 ( 3*(M_ERM+M_DEG+1), SPL_ARR )
      CALL NOUT_R8 ( 3*(M_ERM+M_DEG+1), ERR_ARR )
      CALL NOUT_R8 ( 3*(M_ERM+M_DEG+1), COV_ARR )
#else
      CALL NOUT_R8 ( SIZEOF(TIM_ARR)/8, TIM_ARR )
      CALL NOUT_R8 ( SIZEOF(SPL_ARR)/8, SPL_ARR )
      CALL NOUT_R8 ( SIZEOF(ERR_ARR)/8, ERR_ARR )
      CALL NOUT_R8 ( SIZEOF(COV_ARR)/8, COV_ARR )
#endif
!
! --- Allocate memory for the buffer which with the input file
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL IINCH   ( MBUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 6311, IUER, 'READ_ERM', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
! --- Read the input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILIN, MBUF, BUF, NBUF, IER )
      IF ( IER > 0 ) THEN
           CALL ERR_LOG ( 6312, IUER, 'READ_ERM', 'Error in an attempt to '// &
     &         'read the file with the Earth Rotation Model parameters '// &
     &          FILIN )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      IF ( BUF(1)(1:LEN(ERM__LABEL1)) == ERM__LABEL1 ) THEN
           CONTINUE
         ELSE IF ( BUF(1)(1:LEN(ERM__LABEL1)) == ERM__LABEL2 ) THEN
           CONTINUE
         ELSE
           CALL TRAN ( 13, BUF(1), STR )
           CALL ERR_LOG ( 6313, IUER, 'READ_ERM', 'The first line of the '// &
     &         'input file '//FILIN(1:I_LEN(FILIN))//' is not the lable of '// &
     &         'a valid file in ERM format: '//STR(1:I_LEN(STR))// &
     &         ' while '//ERM__LABEL1//' was expected' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      IF ( BUF(NBUF) .NE. BUF(1) ) THEN
           CALL ERR_LOG ( 6314, IUER, 'READ_ERM', 'Trap of internal '// &
     &         'control: the last line of the input file '// &
     &          FILIN(1:I_LEN(FILIN))//' is not the same as the first '// &
     &         'line. Presumably, the file has not been read up to the end' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      FL_ERM = .FALSE.
      DO 410 J1=2,NBUF-1
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
!
         IF ( BUF(J1)(1:7)  == 'ERM DIM' ) THEN
!
! ----------- Parsing the line which defines the degere of the B-spline
! ----------- and the number of components
!
              READ ( UNIT=BUF(J1)(16:16), FMT='(I1)', IOSTAT=IER ) ICMP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6315, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the EOP component in the '// &
     &                  STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( ICMP < 1  .OR.  ICMP > 3 ) THEN
                   CALL ERR_LOG ( 6316, IUER, 'READ_ERM', 'Wrong '// &
     &                 'component EOP index in the '//STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' -- a value in '// &
     &                 'the range 1-3 was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              L_DEG(ICMP) = 3
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6317, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding B-spline degree in the '//STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( L_DEG(ICMP) < 0 .OR. L_DEG(ICMP) > M_DEG ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_DEG, STR )
                   CALL ERR_LOG ( 6318, IUER, 'READ_ERM', 'Wrong value of '// &
     &                 'B-spline degree found in parsing the '// &
     &                  STR(1:I_LEN(STR))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' . The value in '// &
     &                 'the range [0, '//STR(1:I_LEN(STR))//' was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(34:38), FMT='(I6)', IOSTAT=IER ) L_ERM(ICMP)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6319, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding B-spline degree in the '//STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( L_ERM(ICMP) < L_DEG(ICMP) + 1 .OR. L_ERM(ICMP) > M_ERM ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,            STR(1:10)  )
                   CALL INCH  ( L_DEG(ICMP)+1, STR(11:20) )
                   CALL INCH  ( M_ERM,         STR(21:30) )
                   CALL ERR_LOG ( 6320, IUER, 'READ_ERM', 'Wrong value of '// &
     &                 'knots of B-spline found in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' . The value in '// &
     &                 'the range ['//STR(11:I_LEN(STR(11:20))+10)//', '// &
     &                  STR(21:I_LEN(STR))//'] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              FL_ERM = .TRUE.
            ELSE IF ( BUF(J1)(1:7) == 'ERM EST' ) THEN
!
! ----------- Parsing the line which defines the estimate of the ERM component
!
              READ ( UNIT=BUF(J1)(20:20), FMT='(I1)', IOSTAT=IER ) ICMP
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 6321, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( ICMP < 1  .OR.  ICMP > 3 ) THEN
                   CALL CLRCH (     STR       )
                   CALL INCH  ( J1, STR(1:10) )
                   CALL ERR_LOG ( 6322, IUER, 'READ_ERM', 'Wrong '// &
     &                 'ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' -- an integer '// &
     &                 'value the range [1,3] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(22:29), FMT='(I8)', IOSTAT=IER ) KNOT
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 6323, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( KNOT < 1-L_DEG(ICMP)  .OR. KNOT > L_ERM(ICMP) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL INCH  ( 1-L_DEG, STR(11:20) )
                   CALL INCH  ( L_ERM,   STR(21:30) )
                   CALL ERR_LOG ( 6324, IUER, 'READ_ERM', 'Wrong value of '// &
     &                 'knot of B-spline found in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' . The value in '// &
     &                 'the range ['//STR(11:I_LEN(STR(11:20))+10)//', '// &
     &                  STR(21:I_LEN(STR))//'] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(37:57), MJD, SEC, IER )
              IF ( IER > 0 ) THEN
                   CALL CLRCH (     STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6325, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the ERM date in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              IF ( .NOT. FL_TIM ) THEN
                   MJD_BEG = MJD
                   TAI_BEG = SEC
                   FL_TIM = .TRUE.
              END IF
!
              TIM_ARR(KNOT,ICMP) = (MJD - MJD_BEG)*86400.0D0 + (SEC - TAI_BEG)
!
              READ ( UNIT=BUF(J1)(86:101), FMT=*, IOSTAT=IER ) &
     &               SPL_ARR(KNOT,ICMP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (     STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6326, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the estimate of the ERM component in '// &
     &                 'parsing the '//STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(108:118), FMT=*, IOSTAT=IER ) &
     &               ERR_ARR(KNOT,ICMP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (     STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6327, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the estimate of the error of the ERM '// &
     &                 'component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
! ==========  Rest of the parsing is related to the old format
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
            ELSE IF ( BUF(J1)(1:5) == 'L_ERM' ) THEN
!
! ----------- Parsing teh line which defines the degere of the B-spline
! ----------- and the number of components
!
              READ ( UNIT=BUF(J1)(8:8), FMT='(I1)', IOSTAT=IER ) ICMP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6328, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the EOP component in the '// &
     &                  STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( ICMP < 1  .OR.  ICMP > 3 ) THEN
                   CALL ERR_LOG ( 6329, IUER, 'READ_ERM', 'Wrong '// &
     &                 'component EOP index in the '//STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' -- a value in '// &
     &                 'the range 1-3 was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(11:11), FMT='(I1)', IOSTAT=IER ) L_DEG(ICMP)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6330, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding B-spline degree in the '//STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( L_DEG(ICMP) < 0 .OR. L_DEG(ICMP) > M_DEG ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_DEG, STR )
                   CALL ERR_LOG ( 6331, IUER, 'READ_ERM', 'Wrong value of '// &
     &                 'B-spline degree found in parsing the '// &
     &                  STR(1:I_LEN(STR))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' . The value in '// &
     &                 'the range [0, '//STR(1:I_LEN(STR))//' was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(15:18), FMT='(I5)', IOSTAT=IER ) L_ERM(ICMP)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6332, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding B-spline degree in the '//STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( L_ERM(ICMP) < L_DEG(ICMP) + 1 .OR. L_ERM(ICMP) > M_ERM ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,            STR(1:10)  )
                   CALL INCH  ( L_DEG(ICMP)+1, STR(11:20) )
                   CALL INCH  ( M_ERM,         STR(21:30) )
                   CALL ERR_LOG ( 6333, IUER, 'READ_ERM', 'Wrong value of '// &
     &                 'knots of B-spline found in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' . The value in '// &
     &                 'the range ['//STR(11:I_LEN(STR(11:20))+10)//', '// &
     &                  STR(21:I_LEN(STR))//'] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              FL_ERM = .TRUE.
            ELSE IF ( BUF(J1)(4:11)  == 'Par: ERM'  .AND.  &
     &                BUF(J1)(25:28) == 'Epc:'             ) THEN
!
! ----------- Parsing the line which defines the estimate of the ERM component
!
              READ ( UNIT=BUF(J1)(13:13), FMT='(I1)', IOSTAT=IER ) ICMP
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 6334, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( ICMP < 1  .OR.  ICMP > 3 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 6335, IUER, 'READ_ERM', 'Wrong '// &
     &                 'ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' -- an integer '// &
     &                 'value the range [1,3] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(18:22), FMT='(I5)', IOSTAT=IER ) KNOT
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 6336, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( KNOT < 1-L_DEG(ICMP)  .OR. KNOT > L_ERM(ICMP) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL INCH  ( 1-L_DEG, STR(11:20) )
                   CALL INCH  ( L_ERM,   STR(21:30) )
                   CALL ERR_LOG ( 6337, IUER, 'READ_ERM', 'Wrong value of '// &
     &                 'knot of B-spline found in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' . The value in '// &
     &                 'the range ['//STR(11:I_LEN(STR(11:20))+10)//', '// &
     &                  STR(21:I_LEN(STR))//'] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(30:50), MJD, SEC, IER )
              IF ( IER > 0 ) THEN
                   CALL CLRCH (     STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6338, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the ERM date in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              IF ( .NOT. FL_TIM ) THEN
                   MJD_BEG = MJD
                   TAI_BEG = SEC
                   FL_TIM = .TRUE.
              END IF
!
              TIM_ARR(KNOT,ICMP) = (MJD - MJD_BEG)*86400.0D0 + (SEC - TAI_BEG)
!
              READ ( UNIT=BUF(J1)(59:77), FMT=*, IOSTAT=IER ) &
     &               SPL_ARR(KNOT,ICMP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (     STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6339, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the estimate of the ERM component in '// &
     &                 'parsing the '//STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(85:95), FMT=*, IOSTAT=IER ) &
     &               ERR_ARR(KNOT,ICMP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (     STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6340, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the estimate of the error of the ERM '// &
     &                 'component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(4:11)  == 'Par: ERM'  .AND.  &
     &                BUF(J1)(25:28) == 'Par:'             ) THEN
!
! ----------- Parsing the line which defines the correlations between the
! ----------- estimates
!
              READ ( UNIT=BUF(J1)(13:13), FMT='(I1)', IOSTAT=IER ) ICMP1
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 6341, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( ICMP1 < 1  .OR.  ICMP1 > 3 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 6342, IUER, 'READ_ERM', 'Wrong '// &
     &                 'ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' -- an integer '// &
     &                 'value the range [1,3] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(18:22), FMT='(I5)', IOSTAT=IER ) KNOT
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 6343, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( KNOT < 1-L_DEG(ICMP1)  .OR.  KNOT > L_ERM(ICMP1) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,            STR(1:10)  )
                   CALL INCH  ( 1-L_DEG(ICMP), STR(11:20) )
                   CALL INCH  ( L_ERM(ICMP),   STR(21:30) )
                   CALL ERR_LOG ( 6344, IUER, 'READ_ERM', 'Wrong value of '// &
     &                 'knot of B-spline found in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN(1:I_LEN(FILIN))//' . The value in '// &
     &                 'the range ['//STR(11:I_LEN(STR(11:20))+10)//', '// &
     &                  STR(21:I_LEN(STR))//'] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(34:34), FMT='(I1)', IOSTAT=IER ) ICMP2
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 6345, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the 2nd ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
! ----------- If components are different skip them
!
              IF ( ICMP2 .NE. ICMP1 ) GOTO  410
!
              READ ( UNIT=BUF(J1)(39:43), FMT='(I5)', IOSTAT=IER ) KNOT2
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 6346, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the 2nd ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
! ----------- If components have too big stride, skip it
!
              IF ( KNOT2 - KNOT < 0  .OR.  KNOT2 - KNOT > L_DEG(ICMP1) ) GOTO 410
!
              READ ( UNIT=BUF(J1)(51:58), FMT=*, IOSTAT=IER ) &
     &               COV_ARR(KNOT,KNOT2-KNOT,ICMP1)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (     STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6347, IUER, 'READ_ERM', 'Failure in '// &
     &                 'decoding the estimate of the correlation betwen '// &
     &                 'the ERM components in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FILIN )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              COV_ARR(KNOT,KNOT2-KNOT,ICMP1) = COV_ARR(KNOT,KNOT2-KNOT,ICMP1)* &
     &                     ERR_ARR(KNOT,ICMP1)*ERR_ARR(KNOT2,ICMP1)
            ELSE IF ( BUF(J1)(1:7)  == 'ERM SOL' ) THEN
              CONTINUE 
            ELSE IF ( BUF(J1)(1:7)  == 'ERM APR' ) THEN
              CONTINUE 
            ELSE IF ( BUF(J1)(1:7)  == 'ERM UZM' ) THEN
              CONTINUE 
            ELSE IF ( BUF(J1)(1:7)  == 'ERM UZU' ) THEN
              CONTINUE 
            ELSE IF ( BUF(J1)(1:7)  == 'ERM TIM' ) THEN
              CONTINUE 
            ELSE IF ( BUF(J1)(1:7)  == 'ERM COV' ) THEN
              CONTINUE 
            ELSE IF ( BUF(J1)(1:7)  == 'ERM CNS' ) THEN
              CONTINUE 
            ELSE
              CALL CLRCH (     STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 6348, IUER, 'READ_ERM', 'Inrecognizable '// &
     &            'format of the '//STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &            BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &            'of the input file with the Earth rotation '// &
     &            'model '//FILIN )
              DEALLOCATE ( BUF )
              RETURN
         END IF
 410  CONTINUE
!
      DO 420 J2=1,3
         DO 430 J3=1-L_DEG(J2),0
            TIM_ARR(J3,J2) = TIM_ARR(1,J2)
 430     CONTINUE
 420  CONTINUE
!
      DEALLOCATE ( BUF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_ERM  !#!#
