      SUBROUTINE GPOSVAR ( STRING, TOKEN, N_POSVAR, POSVAR_FIL, POSVAR_MOD, &
     &                     POSVAR_INT, POSVAR_USE, G_WARNING, FL_WARN_POSVAR, &
     &                     IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GPOSVAR  parses POSITION_VARIATIONS keyword.              *
! *                                                                      *
! *  ### 03-OCT-2002    GPOSVAR    v2.4 (c)  L. Petrov  03-SEP-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'bindisp.i'
      INCLUDE   'precm.i'
      CHARACTER  STRING*(*), TOKEN*(*)
      INTEGER*4  N_POSVAR, POSVAR_MOD(M__POSVAR), POSVAR_INT(M__POSVAR), &
     &           POSVAR_USE(M__POSVAR), IUER
      LOGICAL*2  G_WARNING
      LOGICAL*4  FL_WARN_POSVAR(M__POSVAR)
      CHARACTER  POSVAR_FIL(M__POSVAR)*(*)
      LOGICAL*4  LEX
      INTEGER*4  IP, J1, J2, IER
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LIB$SKPC
      ADDRESS__TYPE       :: OPENDIR 
!
      DO 410 J1=1,M__POSVAR
         N_POSVAR = J1
         POSVAR_FIL(J1) = TOKEN
         IF ( POSVAR_FIL(J1)(1:1) .NE. '/' ) THEN
              POSVAR_FIL(J1) = PRE_SAV_DIR(1:PRE_SV_LEN)//POSVAR_FIL(J1)
         END IF
!
         CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
         IF ( ILEN(TOKEN) .EQ. 0 ) THEN
              CALL ERR_LOG ( 6311, IUER, 'GPOSVAR', 'Error in parsing '// &
     &            'POSITION_VARIATIONS was found: no mode quailifier is '// &
     &            'supplied after the filename '// &
     &             POSVAR_FIL(J1)(1:I_LEN(POSVAR_FIL(J1))) )
              RETURN
         END IF
!
         IF ( TOKEN .EQ. 'HARMONIC_MODEL' ) THEN
              POSVAR_MOD(J1) = PSV__HMD
            ELSE IF ( TOKEN .EQ. 'HMD' ) THEN
              POSVAR_MOD(J1) = PSV__HMD
            ELSE IF ( TOKEN .EQ. 'TIME_SERIES' ) THEN
              POSVAR_MOD(J1) = PSV__TSR
            ELSE IF ( TOKEN .EQ. 'TSR' ) THEN
              POSVAR_MOD(J1) = PSV__TSR
            ELSE
              CALL ERR_LOG ( 6312, IUER, 'GPOSVAR', 'Wrong mode qualifer in '// &
     &            'parsing the keyword POSITION_VARIATIONS was found: '// &
     &             TOKEN(1:I_LEN(TOKEN))//' -- only HARMONIC_MODEL and '// &
     &            'TIME_SERIES are supported' )
              RETURN
         END IF
!
         IF ( POSVAR_MOD(J1) .EQ. PSV__HMD ) THEN
              INQUIRE ( FILE=POSVAR_FIL(J1), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL ERR_LOG ( 6313, IUER, 'GPOSVAR', 'Position '// &
     &                 'variation file '// &
     &                  POSVAR_FIL(J1)(1:I_LEN(POSVAR_FIL(J1)))// &
     &                 ' was not found' )
                   RETURN
              END IF
            ELSE IF ( POSVAR_MOD(J1) .EQ. PSV__TSR ) THEN
              DIR_DESC = OPENDIR ( POSVAR_FIL(J1)(1:I_LEN(POSVAR_FIL(J1)))//CHAR(0) )
              IF ( DIR_DESC .EQ. 0 ) THEN
                   CALL ERR_LOG ( 6314, IUER, 'GPOSVAR', 'Directory with '// &
     &                 'position variation files '// &
     &                  POSVAR_FIL(J1)(1:I_LEN(POSVAR_FIL(J1)))// &
     &                 ' was not found' )
                   RETURN
                ELSE
                 CALL CLOSEDIR ( %VAL(DIR_DESC) )
            END IF
         END IF
!
         CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
         IF ( ILEN(TOKEN) .EQ. 0 ) THEN
              CALL ERR_LOG ( 6315, IUER, 'GPOSVAR', 'Error in parsing '// &
     &            'POSITION_VARIATIONS was found: no mode quailifier is '// &
     &            'supplied after the mode, after filename '// &
     &             POSVAR_FIL(J1)(1:I_LEN(POSVAR_FIL(J1))) )
              RETURN
         END IF
!
         IF ( TOKEN .EQ. 'CLOSE_POINT' ) THEN
              POSVAR_INT(J1) = PSV__CLS
            ELSE IF ( TOKEN .EQ. 'CLS' ) THEN
              POSVAR_INT(J1) = PSV__CLS
            ELSE IF ( TOKEN .EQ. 'LINEAR' ) THEN
              POSVAR_INT(J1) = PSV__LIN
            ELSE IF ( TOKEN .EQ. 'LIN' ) THEN
              POSVAR_INT(J1) = PSV__LIN
            ELSE IF ( TOKEN .EQ. 'SPLINE' ) THEN
              POSVAR_INT(J1) = PSV__SPL
            ELSE IF ( TOKEN .EQ. 'SPL' ) THEN
              POSVAR_INT(J1) = PSV__SPL
            ELSE
              CALL ERR_LOG ( 6316, IUER, 'GPOSVAR', 'Wrong interpolation '// &
     &            'qualifer in parsing the keyword POSITION_VARIATIONS was '// &
     &            'found: '//TOKEN(1:I_LEN(TOKEN))//' -- only CLOSE_POINT, '// &
     &            'LINEAR and SPLINE are supported' )
              RETURN
         END IF
!
         CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
         IF ( ILEN(TOKEN) .EQ. 0 ) THEN
              CALL ERR_LOG ( 6317, IUER, 'GPOSVAR', 'Error in parsing '// &
     &            'POSITION_VARIATIONS was found: no mode quailifier is '// &
     &            'supplied after the interpolation mode, after filename '// &
     &             POSVAR_FIL(J1)(1:I_LEN(POSVAR_FIL(J1))) )
              RETURN
         END IF
         IF ( TOKEN .EQ. 'AVL' ) THEN
              POSVAR_USE(J1) = PSV__AVL
            ELSE IF ( TOKEN .EQ. 'IF_AVAILABLE' ) THEN
              POSVAR_USE(J1) = PSV__AVL
            ELSE IF ( TOKEN .EQ. 'REQUIRED' ) THEN
              POSVAR_USE(J1) = PSV__REQ
            ELSE IF ( TOKEN .EQ. 'REQ' ) THEN
              POSVAR_USE(J1) = PSV__REQ
            ELSE
              CALL ERR_LOG ( 6318, IUER, 'GPOSVAR', 'Wrong usage '// &
     &            'qualifer in parsing the keyword POSITION_VARIATIONS was '// &
     &            'found: '//TOKEN(1:I_LEN(TOKEN))//' -- only IF_AVAILABLE '// &
     &            'and REQUIRED are supported' )
              RETURN
         END IF
!
         FL_WARN_POSVAR(J1) = G_WARNING
         IP = LIB$SKPC ( ' ', STRING )
         IF ( IP .LE. 0 ) GOTO 810
!
         IF ( STRING(IP:IP+7) .EQ. 'WARNING ' ) THEN
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( TOKEN .EQ. 'WARNING' ) THEN
                   CONTINUE 
                 ELSE
                   CALL ERR_LOG ( 6319, IUER, 'GPOSVAR', 'Error in parsing '// &
     &                 'POSITION_VARIATIONS was found: qualifier '// &
     &                  TOKEN(1:I_LEN(TOKEN))//' is not supported or '// &
     &                 'mispositioned' )
                   RETURN
              END IF
!
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
              IF ( TOKEN .EQ. 'ON' ) THEN
                   FL_WARN_POSVAR(J1) = .TRUE.
                 ELSE IF ( TOKEN .EQ. 'OFF' ) THEN
                   FL_WARN_POSVAR(J1) = .FALSE.
                 ELSE
                   CALL ERR_LOG ( 6320, IUER, 'GPOSVAR', 'Error in parsing '// &
     &                 'POSITION_VARIATIONS was found: wrong value of '// &
     &                 'WARNING qualifier: '//TOKEN(1:I_LEN(TOKEN))// &
     &                 ' only ON or OFF are supported' )
                   RETURN
              END IF              
         END IF
!
         IF ( STRING(IP:IP) .NE. '\' ) GOTO 810
         CALL SPLIT_STRING ( STRING, TOKEN, STRING )
 410  CONTINUE
 810  CONTINUE
!
! --- Now, let's load all position variation files
!
      DO 420 J2=1,N_POSVAR
         IF ( POSVAR_MOD(J2) .EQ. PSV__HMD ) THEN
!
! ----------- Let's load position variations file in HARPOS format
!
              CALL ERR_PASS    ( IUER, IER )
              CALL LOAD_HARPOS ( J2, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6319, IUER, 'GPOSVAR', 'Error in an '// &
     &                 'attempt to load the file with harmonic site position '// &
     &                 'variations in HARPOS format '//POSVAR_FIL(J2) )
                   RETURN
              END IF
            ELSE
!
! ----------- Let's load position variations file in BINDISP format
!
              CALL ERR_PASS     ( IUER, IER )
              CALL LOAD_BINDISP ( J2, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6320, IUER, 'GPOSVAR', 'Error in an '// &
     &                 'attempt to load the file with time series of the '// &
     &                 'site position variations in BINDISP format '// &
     &                  POSVAR_FIL(J2) )
                   RETURN
              END IF
         END IF
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GPOSVAR  #!#
