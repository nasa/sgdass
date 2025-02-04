      SUBROUTINE ANTI_PARSE ( ANTI_FILE, ANTI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine ANTI_PARSE pareses the file with antenna information and   *
! *   puts the extracted information into the ANTI data structure.       *
! *                                                                      *
! *  ### 22-APR-2008   ANTI_PARSE  v1.1 (c)  L. Petrov  21-NOV-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'anti.i'
      CHARACTER  ANTI_FILE*(*)
      TYPE     ( ANTENNA_DATA__TYPE ) :: ANTI
      INTEGER*4  IUER
      INTEGER*4  MBUF
      CHARACTER, ALLOCATABLE :: BUF(:)*256
      CHARACTER  STR*128, STR1*128
      LOGICAL*4  LEX
      REAL*8     VAL
      INTEGER*4  J1, J2, J3, NBUF, K_ANT, IER
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN
!
      CALL NOUT ( SIZEOF(ANTI), ANTI ) 
!
      INQUIRE ( FILE=ANTI_FILE, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 3811, IUER, 'ANTI_PARSE', 'Cannot find file '// &
     &         'with antenna information '//ANTI_FILE )
           RETURN 
      END IF
!
      MBUF = 1024
      ALLOCATE ( BUF(MBUF), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 256*MBUF, STR )
           CALL ERR_LOG ( 3812, IUER, 'ANTI_PARSE', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &         'a temporary buffer' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( ANTI_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL ERR_LOG ( 3813, IUER, 'ANTI_PARSE', 'Failure in reading '// &
     &         'the file with antenna information '//ANTI_FILE )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(ANTI__LABEL)) == ANTI__LABEL ) THEN
           CONTINUE 
         ELSE
           CALL CLRCH ( STR )
           CALL TRAN ( 13, BUF(1), STR )
           CALL ERR_LOG ( 3814, IUER, 'ANTI_PARSE', 'Input file '// &
     &          ANTI_FILE(1:I_LEN(ANTI_FILE))//' has wrong format: '// &
     &         'its first line is '//STR(1:LEN(ANTI__LABEL))// &
     &         ' while '//ANTI__LABEL//' was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
      IF ( BUF(NBUF) .NE. BUF(1) ) THEN
           CALL ERR_LOG ( 3815, IUER, 'ANTI_PARSE', 'File with antenna '// &
     &         'information '//ANTI_FILE(1:I_LEN(ANTI_FILE))//' is damaged: '// &
     &         'the last line should have a footer, but it contains '// &
     &         'something different' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      DO 410 J1=2,NBUF-1
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( BUF(J1)(1:1)  == '!' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         IF ( BUF(J1)(1:12) == 'ANTENNA_INFO' ) THEN
              ANTI%N_ANT = ANTI%N_ANT + 1
         END IF
 410  CONTINUE 
!
      ALLOCATE ( ANTI%STA_NAM(ANTI%N_ANT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*ANTI%N_ANT, STR )
           CALL ERR_LOG ( 3816, IUER, 'ANTI_PARSE', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &         'ANTI%STA_NAM' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      ALLOCATE ( ANTI%INFO(ANTI%N_ANT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZEOF(ANTI%INFO(1))*ANTI%N_ANT, STR )
           CALL ERR_LOG ( 3817, IUER, 'ANTI_PARSE', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &         'ANTI%INFO' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      ANTI%N_ANT = 0
      DO 420 J2=2,NBUF-1
         IF ( BUF(J2)(1:1)  == '#' ) GOTO 420
         IF ( BUF(J2)(1:1)  == '!' ) GOTO 420
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 420
         IF ( BUF(J2)(1:12) == 'ANTENNA_INFO' ) THEN
              K_ANT = ADD_CLIST ( MBUF, ANTI%N_ANT, ANTI%STA_NAM, &
     &                    BUF(J2)(ANTI_V1__FLD(1,2):ANTI_V1__FLD(2,2)), IER )
              IF ( K_ANT .NE. ANTI%N_ANT ) THEN
                   CALL CLRCH ( STR )
                   CALL ERR_LOG ( 3818, IUER, 'ANTI_PARSE', 'Station '// &
     &                  BUF(J2)(ANTI_V1__FLD(1,2):ANTI_V1__FLD(2,2))// &
     &                  ' is defined in the antenna infromation file '// &
     &                  ANTI_FILE(1:I_LEN(ANTI_FILE))//' more than once' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              ANTI%INFO(K_ANT)%NAME = ANTI%STA_NAM(K_ANT)
              IF ( BUF(J2)(ANTI_V1__FLD(1,3):ANTI_V1__FLD(2,3)) == ANTI__FO_PRIM_CH ) THEN
                   ANTI%INFO(K_ANT)%FOCUS_TYPE = ANTI__FO_PRIM 
                 ELSE IF ( BUF(J2)(ANTI_V1__FLD(1,3):ANTI_V1__FLD(2,3)) == ANTI__FO_SECN_CH ) THEN
                   ANTI%INFO(K_ANT)%FOCUS_TYPE = ANTI__FO_SECN 
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 3819, IUER, 'ANTI_PARSE', 'Failure in '// &
     &                 'an attempt to parse antenna information file '// &
     &                  ANTI_FILE(1:I_LEN(ANTI_FILE))// &
     &                 ' line '//STR(1:I_LEN(STR))//' -- unsupported '// &
     &                 ' focus type: '// &
     &                 BUF(J2)(ANTI_V1__FLD(1,3):ANTI_V1__FLD(2,3)) ) 
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              IF ( BUF(J2)(ANTI_V1__FLD(1,4):ANTI_V1__FLD(2,4)) == ANTI__MO_AZEL_CH ) THEN
                   ANTI%INFO(K_ANT)%MOUNTING_TYPE = ANTI__MO_AZEL 
                 ELSE IF ( BUF(J2)(ANTI_V1__FLD(1,4):ANTI_V1__FLD(2,4)) == ANTI__MO_EQUA_CH ) THEN
                   ANTI%INFO(K_ANT)%MOUNTING_TYPE = ANTI__MO_EQUA
                 ELSE IF ( BUF(J2)(ANTI_V1__FLD(1,4):ANTI_V1__FLD(2,4)) == ANTI__MO_XYNO_CH ) THEN
                   ANTI%INFO(K_ANT)%MOUNTING_TYPE = ANTI__MO_XYNO
                 ELSE IF ( BUF(J2)(ANTI_V1__FLD(1,4):ANTI_V1__FLD(2,4)) == ANTI__MO_XYEA_CH ) THEN
                   ANTI%INFO(K_ANT)%MOUNTING_TYPE = ANTI__MO_XYEA
                 ELSE IF ( BUF(J2)(ANTI_V1__FLD(1,4):ANTI_V1__FLD(2,4)) == ANTI__MO_RICH_CH ) THEN
                   ANTI%INFO(K_ANT)%MOUNTING_TYPE = ANTI__MO_RICH
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 3820, IUER, 'ANTI_PARSE', 'Failure in '// &
     &                 'an attempt to parse antenna information file '// &
     &                  ANTI_FILE(1:I_LEN(ANTI_FILE))// &
     &                 ' line '//STR(1:I_LEN(STR))//' -- unsupported '// &
     &                 'mounting type '// &
     &                 BUF(J2)(ANTI_V1__FLD(1,4):ANTI_V1__FLD(2,4)) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              IF ( BUF(J2)(ANTI_V1__FLD(1,5):ANTI_V1__FLD(2,5)) == ANTI__RA_NO_CH ) THEN
                   ANTI%INFO(K_ANT)%RADOME_HAS = ANTI__RA_NO
                 ELSE IF ( BUF(J2)(ANTI_V1__FLD(1,5):ANTI_V1__FLD(2,5)) == ANTI__RA_YES_CH ) THEN
                   ANTI%INFO(K_ANT)%RADOME_HAS = ANTI__RA_YES
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 3821, IUER, 'ANTI_PARSE', 'Failure in '// &
     &                 'an attempt to parse antenna information file '// &
     &                  ANTI_FILE(1:I_LEN(ANTI_FILE))// &
     &                 ' line '//STR(1:I_LEN(STR))//' -- unsupported '// &
     &                 'radome type '// &
     &                 BUF(J2)(ANTI_V1__FLD(1,5):ANTI_V1__FLD(2,5)) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              IF ( BUF(J2)(ANTI_V1__FLD(1,6):ANTI_V1__FLD(2,6)) == ANTI__ME_COMP_CH ) THEN
                   ANTI%INFO(K_ANT)%MEAS_TYPE = ANTI__ME_COMP
                 ELSE IF ( BUF(J2)(ANTI_V1__FLD(1,6):ANTI_V1__FLD(2,6)) == ANTI__ME_INCM_CH ) THEN
                   ANTI%INFO(K_ANT)%MEAS_TYPE = ANTI__ME_INCM
                 ELSE IF ( BUF(J2)(ANTI_V1__FLD(1,6):ANTI_V1__FLD(2,6)) == ANTI__ME_ROUG_CH ) THEN
                   ANTI%INFO(K_ANT)%MEAS_TYPE = ANTI__ME_ROUG
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 3822, IUER, 'ANTI_PARSE', 'Failure in '// &
     &                 'an attempt to parse antenna information file '// &
     &                  ANTI_FILE(1:I_LEN(ANTI_FILE))// &
     &                 ' line, '//STR(1:I_LEN(STR))//' -- unsupported '// &
     &                 'measurement type '// &
     &                 BUF(J2)(ANTI_V1__FLD(1,6):ANTI_V1__FLD(2,6)) )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
!
              DO 430 J3=7,ANTI_V1__NFL 
                 READ ( UNIT=BUF(J2)(ANTI_V1__FLD(1,J3):ANTI_V1__FLD(2,J3)), &
     &                  FMT=*, IOSTAT=IER ) VAL
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J2, STR )
                      CALL CLRCH ( STR1 )
                      CALL INCH  ( J3, STR1 )
                      CALL ERR_LOG ( 3823, IUER, 'ANTI_PARSE', 'Failure in '// &
     &                    'an attempt to parse antenna information file '// &
     &                     ANTI_FILE(1:I_LEN(ANTI_FILE))// &
     &                    ' line '//STR(1:I_LEN(STR))//', word '// &
     &                    STR1(1:I_LEN(STR1))//' -- failure to decode '// &
     &                    ' word : '// &
     &                    BUF(J2)(ANTI_V1__FLD(1,J3):ANTI_V1__FLD(2,J3)) )
                      DEALLOCATE ( BUF )
                      RETURN 
                 END IF
!
                 IF ( J3 == 7 ) THEN
                      ANTI%INFO(K_ANT)%REF_TEMP = VAL + 273.15D0
                    ELSE IF ( J3 == 8 ) THEN
                      ANTI%INFO(K_ANT)%ANN_SIN_TEMP = VAL 
	            ELSE IF ( J3 == 9 ) THEN
                      ANTI%INFO(K_ANT)%ANN_COS_TEMP = VAL
	            ELSE IF ( J3 == 10 ) THEN
                      ANTI%INFO(K_ANT)%REF_PRES = VAL*1.D2
	            ELSE IF ( J3 == 11 ) THEN
                      ANTI%INFO(K_ANT)%DIAMETER = VAL
	            ELSE IF ( J3 == 12 ) THEN
                      ANTI%INFO(K_ANT)%FOUNDATION_HEIGHT = VAL
	            ELSE IF ( J3 == 13 ) THEN
                      ANTI%INFO(K_ANT)%FOUNDATION_DEPTH = VAL
	            ELSE IF ( J3 == 14 ) THEN
                      ANTI%INFO(K_ANT)%FOUNDATION_TE = VAL
	            ELSE IF ( J3 == 15 ) THEN
                      ANTI%INFO(K_ANT)%PILLAR_HEIGHT = VAL
	            ELSE IF ( J3 == 16 ) THEN
                      ANTI%INFO(K_ANT)%PILLAR_TE  = VAL
	            ELSE IF ( J3 == 17 ) THEN
                      ANTI%INFO(K_ANT)%AXOF_LEN = VAL
	            ELSE IF ( J3 == 18 ) THEN
                      ANTI%INFO(K_ANT)%AXOF_TE = VAL
	            ELSE IF ( J3 == 19 ) THEN
                      ANTI%INFO(K_ANT)%VERTEX_LEN = VAL
	            ELSE IF ( J3 == 20 ) THEN
                      ANTI%INFO(K_ANT)%VERTEX_TE = VAL
	            ELSE IF ( J3 == 21 ) THEN
                      ANTI%INFO(K_ANT)%SUBREFL_HEIGHT = VAL
	            ELSE IF ( J3 == 22 ) THEN
                      ANTI%INFO(K_ANT)%SUBREFL_TE = VAL
                 END IF
 430          CONTINUE 
         END IF
 420  CONTINUE 
!
      ANTI%FILENAME = ANTI_FILE
      ANTI%STATUS = ANTI__LOAD 
      DEALLOCATE ( BUF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ANTI_PARSE  !#!  
