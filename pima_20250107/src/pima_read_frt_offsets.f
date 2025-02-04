      SUBROUTINE PIMA_READ_FRT_OFFSETS ( FRT_FILE, NOBS, &
     &                                   TIME_FRT_OFFSET_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_READ_FRT_OFFSETS reads and parses file with fringe    *
! *   reference time offsets.                                            *
! *                                                                      *
! * # 11-MAR-2010 PIMA_READ_FRT_OFFSETS v1.1 (c) L. Petrov 13-APR-2020 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      INTEGER*4  NOBS, IUER
      CHARACTER  FRT_FILE*(*)
      REAL*8     TIME_FRT_OFFSET_ARR(NOBS)
      CHARACTER, ALLOCATABLE :: BUF(:)*1024
      REAL*8     VAL
      CHARACTER  STR*128, STR1*128
      INTEGER*4  NP, J1, J2, IOS, IND, IFMT, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      ALLOCATE ( BUF(2*NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*2*NOBS, STR )
           CALL ERR_LOG ( 7491, IUER, 'PIMA_READ_FRT_OFFSETS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for BUF array' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL RD_TEXT ( FRT_FILE, 2*NOBS, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7492, IUER, 'PIMA_READ_FRT_OFFSETS', 'Failure in '// &
     &         'an attempt to read external FRT offset file '//FRT_FILE )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      DO 410 J1=1,NOBS
         TIME_FRT_OFFSET_ARR(J1) = PIMA__FRT_UNDF 
 410  CONTINUE 
!
      IF ( BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) == PIMA__FRIRES_LABEL ) THEN
           IFMT = 1
        ELSE IF ( BUF(1)(1:LEN(PIMA__FRIRES_LABEL)) == '# PIMA Fringe results  v  1.02  Format version of 2014.12.24' ) THEN
           IFMT = 2
        ELSE
           CALL ERR_LOG ( 7493, IUER, 'PIMA_READ_FRT_OFFSETS', &
     &         'Unsupported FRT offset file. Expected to see the label '// &
     &         'of the fringe results in the first line of file '// &
     &         TRIM(FRT_FILE)//' but got '//BUF(1) )
           RETURN 
      END IF
!
      DO 420 J2=1,NP
         IF ( BUF(J2)(1:1) == '#' ) GOTO 420
         IF ( BUF(J2)(1:6) .NE. '      ' ) THEN
              READ ( UNIT=BUF(J2)(1:6), FMT='(I6)', IOSTAT=IOS ) IND
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 7494, IUER, 'PIMA_READ_FRT_OFFSETS', &
     &                 'Failure to decode observation index '// &
     &                 BUF(J2)(10:14)//' at line '//STR(1:I_LEN(STR))// &
     &                 ' of the external FRT offset file '//FRT_FILE )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              IF ( IND < 1  .OR.  IND > NOBS  ) THEN
                   CALL CLRCH ( STR  )
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( J2,   STR  )
                   CALL INCH  ( NOBS, STR1 )
                   CALL ERR_LOG ( 7495, IUER, 'PIMA_READ_FRT_OFFSETS', &
     &                 'Failure to decode observation index '// &
     &                  BUF(J2)(10:14)//' at line '//STR(1:I_LEN(STR))// &
     &                 ' of the external FRT offset file '// &
     &                 FRT_FILE(1:I_LEN(FRT_FILE))//' -- it should be '// &
     &                 'in range [1, '//STR1(1:I_LEN(STR1))//']' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              IF ( IFMT == 1 ) THEN
                   READ ( UNIT=BUF(J2)(180:195), FMT='(F16.9)', IOSTAT=IOS ) VAL
                 ELSE IF ( IFMT == 2 ) THEN
                   READ ( UNIT=BUF(J2)(177:192), FMT='(F16.9)', IOSTAT=IOS ) VAL
              END IF
              IF ( IOS == 0 ) THEN
                   TIME_FRT_OFFSET_ARR(IND) = VAL
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 7496, IUER, 'PIMA_READ_FRT_OFFSETS', &
     &                 'Failure to decode The FRT offset '// &
     &                  BUF(J2)(180:195)//' at line '//STR(1:I_LEN(STR))// &
     &                 ' of the external FRT offset file '//FRT_FILE )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
         END IF
 420  CONTINUE 
!
      DEALLOCATE ( BUF )
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_READ_FRT_OFFSETS  !#!#
