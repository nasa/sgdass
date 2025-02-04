      SUBROUTINE PIMA_LOAD_STACAT ( PIM, L_CST, STA_CAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_LOAD_STACAT 
! *                                                                      *
! * ## 07-JAN-2006  PIMA_LOAD_STACAT  v1.0 (c)  L. Petrov 07-JAN-2006 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      TYPE     ( PIM_STA__TYPE ) :: STA_CAT(PIM__MCST)
      INTEGER*4  L_CST, IUER
!
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 2*PIM__MCST )
      CHARACTER, ALLOCATABLE :: BUF(:)*256
      CHARACTER  LABEL__STANAM1*46, STR*128
      PARAMETER  ( LABEL__STANAM1 = &
     &             '# STATION-NAMES.  Format version of 2006.01.06' )
      LOGICAL*4  LEX
      INTEGER*4  J1, J2, NBUF, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      INQUIRE ( FILE=PIM%CONF%STA_NAMES_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7181, IUER, 'PIMA_LOAD_STACAT', 'Cannot find '// &
     &         'station names file '//PIM%CONF%STA_NAMES_FILE )
           RETURN 
      END IF
!
      ALLOCATE ( BUF(MBUF) )
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL RD_TEXT  ( PIM%CONF%STA_NAMES_FILE, MBUF, BUF, NBUF, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7182, IUER, 'PIMA_LOAD_STACAT', 'Error in an '// &
     &         'attempt to read station names file '// &
     &          PIM%CONF%STA_NAMES_FILE )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(LABEL__STANAM1)) == LABEL__STANAM1 ) THEN
           CONTINUE 
         ELSE 
           CALL ERR_LOG ( 7183, IUER, 'PIMA_LOAD_STACAT', 'Error in an '// &
     &         'attempt to parse station names file '// &
     &          PIM%CONF%STA_NAMES_FILE(1:I_LEN(PIM%CONF%STA_NAMES_FILE))// &
     &         ' -- the first line does not contain a format label. Is '// &
     &         'this file in STATION-NAMES format?' ) 
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(NBUF) .NE. BUF(1) ) THEN
           CALL ERR_LOG ( 7184, IUER, 'PIMA_LOAD_STACAT', 'Error in an '// &
     &         'attempt to parse station names file '// &
     &          PIM%CONF%STA_NAMES_FILE(1:I_LEN(PIM%CONF%STA_NAMES_FILE))// &
     &         ' -- the last line does not the format label. Was '// &
     &         'this file accidentally truncated?' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      L_CST = 0
      DO 410 J1=2,NBUF-1
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         L_CST = L_CST + 1
         IF ( L_CST > PIM__MCST ) THEN
              CALL CLRCH   ( STR ) 
              CALL INCH    ( PIM__MCST, STR )
              CALL ERR_LOG ( 7185, IUER, 'POLA_LOAD_STACAT', 'Station names '// &
     &            'file '// &
     &             PIM%CONF%STA_NAMES_FILE(1:I_LEN(PIM%CONF%STA_NAMES_FILE))// &
     &            ' containts too many stations, more than PIM__MCST: '//STR )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
         STA_CAT(L_CST)%NAME     = BUF(J1)(1:8)
         STA_CAT(L_CST)%IVS_NAME = BUF(J1)(10:17)
!
         READ ( UNIT=BUF(J1)(21:32), FMT='(F12.5)', IOSTAT=IER ) STA_CAT(L_CST)%COO(1)
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH   ( STR ) 
              CALL INCH    ( J1, STR )
              CALL ERR_LOG ( 7186, IUER, 'POLA_LOAD_STACAT', 'Failure to '// &
     &            'decode station position at line '//STR(1:I_LEN(STR))// &
     &            ' of file '// &
     &             PIM%CONF%STA_NAMES_FILE(1:I_LEN(PIM%CONF%STA_NAMES_FILE))// &
     &            ' -- '//BUF(J1)(21:32) )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
         READ ( UNIT=BUF(J1)(37:48), FMT='(F12.5)', IOSTAT=IER ) STA_CAT(L_CST)%COO(2)
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH   ( STR ) 
              CALL INCH    ( J1, STR )
              CALL ERR_LOG ( 7187, IUER, 'POLA_LOAD_STACAT', 'Failure to '// &
     &            'decode station position at line '//STR(1:I_LEN(STR))// &
     &            ' of file '// &
     &             PIM%CONF%STA_NAMES_FILE(1:I_LEN(PIM%CONF%STA_NAMES_FILE))// &
     &            ' -- '//BUF(J1)(37:48) )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
         READ ( UNIT=BUF(J1)(53:64), FMT='(F12.5)', IOSTAT=IER ) STA_CAT(L_CST)%COO(3)
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH   ( STR ) 
              CALL INCH    ( J1, STR )
              CALL ERR_LOG ( 7188, IUER, 'POLA_LOAD_STACAT', 'Failure to '// &
     &            'decode station position at line '//STR(1:I_LEN(STR))// &
     &            ' of file '// &
     &             PIM%CONF%STA_NAMES_FILE(1:I_LEN(PIM%CONF%STA_NAMES_FILE))// &
     &            ' -- '//BUF(J1)(53:64) )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
 410  CONTINUE 
!
      DEALLOCATE ( BUF )
      IF ( L_CST == 0 ) THEN
           CALL ERR_LOG ( 7189, IUER, 'POLA_LOAD_STACAT', 'No stations '// &
     &         'was found in the station position file '// &
     &          PIM%CONF%STA_NAMES_FILE(1:I_LEN(PIM%CONF%STA_NAMES_FILE))// &
     &         ' -- only comments.' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_LOAD_STACAT  !#!#

