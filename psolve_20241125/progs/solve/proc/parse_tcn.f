      SUBROUTINE PARSE_TCN ( TEC_NOISE_FILE, M_TCN, TCN, L_TCN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_TCN 
! *                                                                      *
! *  ### 21-OCT-2010   PARSE_TCN   v1.0 (c)  L. Petrov  21-OCT-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INTEGER*4  M_TCN, L_TCN, IUER
      CHARACTER  TEC_NOISE_FILE*(*)
      TYPE      ( TCN__TYPE ) TCN(M_TCN)
      CHARACTER  TEC_LABEL*38
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      PARAMETER  ( TEC_LABEL = '# GPS_TEC_NOISE   Format of 2010.10.21' )
      LOGICAL*4  LEX
      CHARACTER  STR*128
      INTEGER*4  NB, J1, J2, J3, J4, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      INQUIRE ( FILE=TEC_NOISE_FILE, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
            CALL ERR_LOG ( 8461, IUER, 'PARSE_TCN', 'Cannot find file '// &
     &           TEC_NOISE_FILE )
            RETURN 
      END IF
!
      ALLOCATE ( BUF(M_TCN), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( M_TCN*SIZEOF(BUF(1)), STR )
           CALL ERR_LOG ( 8462, IUER, 'PARSE_TCN', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &          'array BUF' )
           RETURN 
      END IF 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( TEC_NOISE_FILE, M_TCN, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8463, IUER, 'PARSE_TCN', 'Failure in an attempt '// &
     &         ' to read file '//TEC_NOISE_FILE )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(TEC_LABEL)) .NE. TEC_LABEL ) THEN
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 8464, IUER, 'PARSE_TCN', 'Unrecognized format '// &
     &         'of the TEC noise file '// &
     &          TEC_NOISE_FILE(1:I_LEN(TEC_NOISE_FILE))//' -- the first '// &
     &         'line '//STR(1:I_LEN(STR))//' while the valid label '// &
     &          TEC_NOISE_FILE(1:I_LEN(TEC_NOISE_FILE))//' was expected' )
           RETURN 
      END IF
!
      L_TCN = 0
      DO 410 J1=2,NB
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
!
         L_TCN = L_TCN + 1
         IF ( L_TCN > M_TCN ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_TCN, STR )
              CALL ERR_LOG ( 8465, IUER, 'PARSE_TCN', 'Too many TCN records: '// &
     &            'more than '//STR(1:I_LEN(STR))//' -- you need to raise '// &
     &            'parameter M_TCN' )
              RETURN 
         END IF
         TCN(L_TCN)%MODE   = BUF(J1)(1:8)
         IF ( TCN(L_TCN)%MODE == TCN__REGR ) THEN
              CONTINUE 
            ELSE IF ( TCN(L_TCN)%MODE == TCN__DEL ) THEN
              CONTINUE 
            ELSE 
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 8466, IUER, 'PARSE_TCN', 'Error in parsing '// &
     &            'line '//STR(1:I_LEN(STR))// ' of the TEC noise file '// &
     &             TEC_NOISE_FILE(1:I_LEN(TEC_NOISE_FILE))//' field '// &
     &            'MODE: '//BUF(J1)(1:8)//' is not supported. Suppored '// &
     &            'modes: RMS_REGR, RMS_TEC' )
              RETURN 
         END IF
         TCN(L_TCN)%STA_NAM(1) = BUF(J1)(11:18)
         TCN(L_TCN)%STA_NAM(2) = BUF(J1)(22:29)
         READ ( UNIT=BUF(J1)(31:38), FMT='(D8.3)', IOSTAT=IER ) TCN(L_TCN)%FLOOR
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 8467, IUER, 'PARSE_TCN', 'Error in parsing '// &
     &            'line '//STR(1:I_LEN(STR))// ' of the TEC noise file '// &
     &             TEC_NOISE_FILE(1:I_LEN(TEC_NOISE_FILE))//' field '// &
     &            'FLOOR: '//BUF(J1)(31:38) )
              RETURN 
         END IF
         READ ( UNIT= BUF(J1)(41:46), FMT='(F6.4)', IOSTAT=IER ) TCN(L_TCN)%SLOPE
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 8468, IUER, 'PARSE_TCN', 'Error in parsing '// &
     &            'line '//STR(1:I_LEN(STR))// ' of the TEC noise file '// &
     &             TEC_NOISE_FILE(1:I_LEN(TEC_NOISE_FILE))//' field '// &
     &            'SLOPE: '//BUF(J1)(41:46) )
              RETURN 
         END IF
!
         READ ( UNIT= BUF(J1)(49:55), FMT='(D7.3)', IOSTAT=IER ) TCN(L_TCN)%FREQ
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 8469, IUER, 'PARSE_TCN', 'Error in parsing '// &
     &            'line '//STR(1:I_LEN(STR))// ' of the TEC noise file '// &
     &             TEC_NOISE_FILE(1:I_LEN(TEC_NOISE_FILE))//' field '// &
     &            'SLOPE: '//BUF(J1)(49:55) )
              RETURN 
         END IF
 410  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PARSE_TCN  !#!  
