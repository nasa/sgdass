      SUBROUTINE GERM ( FINAM_MERM, L_MERM, ADR_MERM, IUER )
! ************************************************************************
! *                                                                      *
! *    Routine  GERM  contary to its name is not contageous.             *
! *   It allocates dynamic memory for HEO data structures, reads input   *
! *   file with the Earth Rotation Model and loads parrameters of the    *
! *   model into the the data structure with address ADR_MERM.           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FINAME_MERM ( CHARACTER ) -- Name of the input file with Earth       *
! *                              Rotation MDOEL in ERM format.           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     L_MERM ( INTEGER*4 ) -- The number size of the object with ERM   *
! *                             in bytes.                                *
! *   ADR_MERM ( INTEGER*8 ) -- Address of the data structure which      *
! *                             holds parameteres associated with the    *
! *                             Earth rotation model.                    *
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
! *  ### 27-JAN-2006      GERM     v1.1 (c)  L. Petrov  27-OCT-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      TYPE     ( ERM__TYPE ), POINTER :: ERM
      SAVE       ERM
      INTEGER*4  L_MERM, IUER
      ADDRESS__TYPE :: ADR_MERM
      CHARACTER  FINAM_MERM*(*)
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 12*ERM__MKNOT )
      CHARACTER    ERM__LABEL1*44
      PARAMETER  ( ERM__LABEL1 = '# ERM   v 1.0  Format version of 2006.01.19 ' )
      CHARACTER  STR*128
      CHARACTER, ALLOCATABLE :: BUF(:)*256
      INTEGER*4  J1, J2, J3, J4, J5, ICMP, ICMP1, ICMP2, IOS, MJD, KNOT, &
     &           KNOT2, NBUF, IER
      INTEGER*8        MEM_LEN, SIZE_ERM
      ADDRESS__TYPE :: MEM_ADR
      LOGICAL*4  FL_TIM, FL_ERM
      REAL*8     SEC
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL IINCH   ( MBUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 8211, IUER, 'GERM', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM_MERM, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8212, IUER, 'GERM', 'Error in an attempt to '// &
     &         'read the file with the Earth Rotation Model parameters '// &
     &          FINAM_MERM )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
! --- Allocate ERM object
!
      ALLOCATE ( ERM, STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 8213, IUER, 'GERM', 'Failure to '// &
     &         'allocate in memory object ERM' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
! --- Initialization
!
      CALL NOUT ( SIZEOF(ERM), ERM )
      ERM%FL_EST = .FALSE.
      ERM%FL_APL = .TRUE.
!
      IF ( BUF(1)(1:LEN(ERM__LABEL1)) == ERM__LABEL1 ) THEN
           CONTINUE
         ELSE
           CALL TRAN ( 13, BUF(1), STR )
           CALL ERR_LOG ( 8214, IUER, 'GERM', 'The first line of the '// &
     &         'input file '//FINAM_MERM(1:I_LEN(FINAM_MERM))//' is not the lable of '// &
     &         'a valid file in ERM format: '//STR(1:I_LEN(STR))// &
     &         ' while '//ERM__LABEL1//' was expected' )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      IF ( BUF(NBUF) .NE. BUF(1) ) THEN
           CALL ERR_LOG ( 8215, IUER, 'GERM', 'Trap of internal '// &
     &         'control: the last line of the input file '// &
     &          FINAM_MERM(1:I_LEN(FINAM_MERM))//' is not the same as the first '// &
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
         IF ( BUF(J1)(1:5) == 'L_ERM' ) THEN
!
! ----------- Parseing teh line which defines the degere of the B-spline
! ----------- and the number of components
!
              READ ( UNIT=BUF(J1)(8:8), FMT='(I1)', IOSTAT=IER ) ICMP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8216, IUER, 'GERM', 'Failure in '// &
     &                 'decoding the EOP component in the '// &
     &                  STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( ICMP < 1  .OR.  ICMP > 3 ) THEN
                   CALL ERR_LOG ( 8217, IUER, 'GERM', 'Wrong '// &
     &                 'component EOP index in the '//STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM(1:I_LEN(FINAM_MERM))//' -- a value in '// &
     &                 'the range 1-3 was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(11:11), FMT='(I1)', IOSTAT=IER ) ERM%DEGREE(ICMP)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8218, IUER, 'GERM', 'Failure in '// &
     &                 'decoding B-spline degree in the '//STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( ERM%DEGREE(ICMP) < 0 .OR. ERM%DEGREE(ICMP) > ERM__MSPL ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( ERM__MSPL, STR )
                   CALL ERR_LOG ( 8219, IUER, 'GERM', 'Wrong value of '// &
     &                 'B-spline degree found in parsing the '// &
     &                  STR(1:I_LEN(STR))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM(1:I_LEN(FINAM_MERM))//' . The value in '// &
     &                 'the range [0, '//STR(1:I_LEN(STR))//' was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(15:18), FMT='(I5)', IOSTAT=IER ) ERM%NKNOTS(ICMP)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8220, IUER, 'GERM', 'Failure in '// &
     &                 'decoding B-spline degree in the '//STR(1:I_LEN(STR))// &
     &                 'th line "'//BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'line of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( ERM%NKNOTS(ICMP) < ERM%DEGREE(ICMP) + 1 .OR. &
     &             ERM%NKNOTS(ICMP) > ERM__MKNOT                ) THEN
!
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,            STR(1:10)  )
                   CALL INCH  ( ERM%DEGREE(ICMP)+1, STR(11:20) )
                   CALL INCH  ( ERM__MKNOT,    STR(21:30) )
                   CALL ERR_LOG ( 8221, IUER, 'GERM', 'Wrong value of '// &
     &                 'knots of B-spline found in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM(1:I_LEN(FINAM_MERM))//' . The value in '// &
     &                 'the range ['//STR(11:I_LEN(STR(11:20))+10)//', '// &
     &                  STR(21:I_LEN(STR))//'] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              FL_ERM = .TRUE.
            ELSE IF ( BUF(J1)(4:11)  == 'Par: ERM'  .AND.  &
     &                BUF(J1)(25:28) == 'Epc:'             ) THEN
!
! ----------- Parsing hte line which defines the estimate of the ERM component
!
              READ ( UNIT=BUF(J1)(13:13), FMT='(I1)', IOSTAT=IER ) ICMP
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 8222, IUER, 'GERM', 'Failure in '// &
     &                 'decoding the ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( ICMP < 1  .OR.  ICMP > 3 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 8223, IUER, 'GERM', 'Wrong '// &
     &                 'ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM(1:I_LEN(FINAM_MERM))//' -- an integer '// &
     &                 'value the range [1,3] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(18:22), FMT='(I5)', IOSTAT=IER ) KNOT
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 8224, IUER, 'GERM', 'Failure in '// &
     &                 'decoding the ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( KNOT < 1-ERM%DEGREE(ICMP)  .OR. KNOT > ERM%NKNOTS(ICMP) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,               STR(1:10)  )
                   CALL INCH  ( 1-ERM%DEGREE,     STR(11:20) )
                   CALL INCH  ( ERM%NKNOTS(ICMP), STR(21:30) )
                   CALL ERR_LOG ( 8225, IUER, 'GERM', 'Wrong value of '// &
     &                 'knot of B-spline found in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM(1:I_LEN(FINAM_MERM))//' . The value in '// &
     &                 'the range ['//STR(11:I_LEN(STR(11:20))+10)//', '// &
     &                  STR(21:I_LEN(STR))//'] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(30:50), MJD, SEC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (     STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8226, IUER, 'GERM', 'Failure in '// &
     &                 'decoding the ERM date in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              IF ( .NOT. FL_TIM ) THEN
                   ERM%MJD_BEG = MJD
                   ERM%TAI_BEG = SEC
                   FL_TIM = .TRUE.
              END IF
!
!!              ERM%TIM(KNOT,ICMP) = (MJD-J2000__MJD)*86400.0D0 - 43200.0D0 + SEC
              ERM%TIM(KNOT,ICMP) = (MJD - ERM%MJD_BEG)*86400.0D0 + &
     &                             (SEC - ERM%TAI_BEG)
!
              READ ( UNIT=BUF(J1)(59:77), FMT='(F19.12)', IOSTAT=IER ) &
     &               ERM%VAL(KNOT,ICMP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (     STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8227, IUER, 'GERM', 'Failure in '// &
     &                 'decoding the estimate of the ERM component in '// &
     &                 'parsing the '//STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(85:95), FMT='(F11.6)', IOSTAT=IER ) &
     &               ERM%ERR(KNOT,ICMP)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (     STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8228, IUER, 'GERM', 'Failure in '// &
     &                 'decoding the estimate of the error of the ERM '// &
     &                 'component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
            ELSE IF ( BUF(J1)(4:11)  == 'Par: ERM'  .AND.  &
     &                BUF(J1)(25:28) == 'Par:'             ) THEN
!
! ----------- Parsing hte line which defines the correlations between the
! ----------- estimates
!
              READ ( UNIT=BUF(J1)(13:13), FMT='(I1)', IOSTAT=IER ) ICMP1
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 8229, IUER, 'GERM', 'Failure in '// &
     &                 'decoding the ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( ICMP1 < 1  .OR.  ICMP1 > 3 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 8230, IUER, 'GERM', 'Wrong '// &
     &                 'ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM(1:I_LEN(FINAM_MERM))//' -- an integer '// &
     &                 'value the range [1,3] was expected' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              READ ( UNIT=BUF(J1)(18:22), FMT='(I6)', IOSTAT=IER ) KNOT
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,      STR(1:10)  )
                   CALL ERR_LOG ( 8231, IUER, 'GERM', 'Failure in '// &
     &                 'decoding the ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
              IF ( KNOT < 1-ERM%DEGREE(ICMP1)  .OR.  KNOT > ERM%NKNOTS(ICMP1) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1,                 STR(1:10)  )
                   CALL INCH  ( 1-ERM%DEGREE(ICMP), STR(11:20) )
                   CALL INCH  ( ERM%NKNOTS(ICMP1),  STR(21:30) )
                   CALL ERR_LOG ( 8232, IUER, 'GERM', 'Wrong value of '// &
     &                 'knot of B-spline found in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM(1:I_LEN(FINAM_MERM))//' . The value in '// &
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
                   CALL ERR_LOG ( 8233, IUER, 'GERM', 'Failure in '// &
     &                 'decoding the 2nd ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
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
                   CALL ERR_LOG ( 8234, IUER, 'GERM', 'Failure in '// &
     &                 'decoding the 2nd ERM component in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
! ----------- If components have too big stride, skip it
!
              IF ( KNOT2 - KNOT < 0  .OR.  KNOT2 - KNOT > ERM%DEGREE(ICMP1) ) GOTO 410
!
              READ ( UNIT=BUF(J1)(51:58), FMT='(F8.4)', IOSTAT=IER ) &
     &               ERM%COV(KNOT,KNOT2-KNOT,ICMP1)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH (     STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 8235, IUER, 'GERM', 'Failure in '// &
     &                 'decoding the estimate of the correlation betwen '// &
     &                 'the ERM components in parsing the '// &
     &                  STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &                  BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &                 'of the input file with the Earth rotation '// &
     &                 'model '//FINAM_MERM )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
              ERM%COV(KNOT,KNOT2-KNOT,ICMP) = ERM%COV(KNOT,KNOT2-KNOT,ICMP1)* &
     &                     ERM%ERR(KNOT,ICMP1)*ERM%ERR(KNOT2,ICMP1)
            ELSE
              CALL CLRCH (     STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 8236, IUER, 'GERM', 'Inrecognizable '// &
     &            'format of the '//STR(1:I_LEN(STR(1:10)))//'th line "'// &
     &            BUF(J1)(1:I_LEN(BUF(J1)))//'" of the '// &
     &            'of the input file with the Earth rotation '// &
     &            'model '//FINAM_MERM )
              DEALLOCATE ( BUF )
              RETURN
         END IF
 410  CONTINUE
!
      DO 420 J2=1,3
         DO 430 J3=1-ERM%DEGREE(J2),0
            ERM%TIM(J3,J2) = ERM%TIM(1,J2) - (J3-1)*ERM__TIM_EPS
 430     CONTINUE
         DO 440 J4=1,ERM%DEGREE(J2)
            ERM%TIM(ERM%NKNOTS(J3)+J4,J2) = ERM%TIM(ERM%NKNOTS(J3),J2) + ERM__TIM_EPS*J4
 440     CONTINUE
 420  CONTINUE
!
      DEALLOCATE ( BUF )
!
#ifdef HPUX
!
! --- Buggy compiler HPUX ignores operator SAVE and deallocates MERM object
! --- upon return. Damn it!
!
      CALL ERR_PASS  ( IUER, IER )
      CALL GRAB_MEM  ( IER, MEM_LEN, MEM_ADR, 1, SIZE_ERM, ADR_MERM )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' SIZE_ERM=',SIZE_ERM
           CALL ERR_LOG ( 8237, IUER, 'GERM', 'Error in an '// &
     &         'an attempt to allocate dynamic memory' )
           DEALLOCATE ( BUF )
           DEALLOCATE ( ERM )
           RETURN
      END IF
      CALL LIB$MOVC3 ( SIZE_ERM, ERM, %VAL(ADR_MERM) )
#else
      ADR_MERM  = LOC(ERM)
#endif
      L_MERM = SIZEOF(ERM)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GERM  !#!#
