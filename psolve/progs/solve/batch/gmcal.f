      SUBROUTINE GMCAL()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GMCAL PROGRAM SPECIFICATION
!
! 1.1 Parse MODE_CALIBRATIONS section of the control file.
!
! 1.2 REFERENCES:
!
! 2.  GMCAL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'solve.i'
      INCLUDE 'dcont.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrlfl
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER STRING*256, TOKEN*256
      INTEGER*2 LENGTH, CFREAD, TRIMLEN, IDUM
      LOGICAL*2 CFEOF, KNON, KIN
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   pet  1999.11.18  Created by using gcont as a template
!   pet  2000.05.11  Added support of a keyword SET
!
! 5.  GMCAL PROGRAM STRUCTURE
!
      L_CLM = 0
      KNON = .FALSE.
      KIN  = .FALSE.
!
! --- Read first record of the MODE_CALIBRATIONS section
!
      LENGTH = CFREAD(STRING)
      DO WHILE ( STRING(1:1) .EQ. ' ' .AND. .NOT.CFEOF(IDUM) )
          DO WHILE ( TRIMLEN(STRING).GT.0 )
             CALL SPLITSTRING ( STRING, TOKEN, STRING )
             IF ( TOKEN .EQ. 'SET' .OR. TOKEN .EQ. 'set' ) THEN
!
! --------------- Skip token SET
!
                  CALL SPLITSTRING ( STRING, TOKEN, STRING )
                ELSE
                  CALL FERR ( INT2(4806), &
     &                'BATCH(gmcal) Wrong keyword was found '// &
     &                ' in MODE_CALIOBRATION section: '//TOKEN(:16)// &
     &                ' only SET is supported', INT2(0), INT2(0) )
                  STOP 'BATCH: Abnormal termination'
             END IF
!
             L_CLM=L_CLM+1
             IF ( L_CLM .GT. MAX_CONT ) THEN
                  WRITE ( 6, * ) ' MAC_CONT=',MAX_CONT
                  CALL FERR ( INT2(4810), &
     &                'BATCH(gmcal) Too many mode calibrations '// &
     &                'in the $MODE_CALIBRATIONS section', INT2(0), INT2(0) )
                  STOP 'BATCH: Abnormal termination'
             ENDIF
!
             MCAL(L_CLM) = TOKEN
             CALL UNDSCR ( MCAL(L_CLM) )
!
             IF ( MCAL(L_CLM)(1:2) .EQ. 'NO      '  .OR. &
     &            MCAL(L_CLM)(1:2) .EQ. 'NONE    '       ) THEN
!
! --------------- SET NO or NONE keyword
!
                  IF ( KNON ) CALL FERR ( INT2(4820), &
     &                'BATCH(gmcal) qualifier NO '//'or NONE used twice', &
     &                 INT2(0), INT2(0) )
                  KNON = .TRUE.
             ENDIF
!
             IF ( MCAL(L_CLM)(1:8) .EQ. 'IN      ' ) THEN
!
! --------------- IN keyword
!
                  IF ( KNON ) CALL FERR ( INT2(4830), &
     &                'BATCH(gmcal) Keyword IN '//'used twice', INT2(0), INT2(0) )
                  KNON = .TRUE.
             ENDIF
          ENDDO
          LENGTH = CFREAD ( STRING )
      ENDDO
!
! --- Now that this section is finished, what now?
!
      IF ( KNON .AND. L_CLM .GT. 1 ) THEN
           CALL FERR ( INT2(4840), &
     &         'BATCH(gmcal) Keyword SET NO or SET NONE must '// &
     &         'appear by itself only once', INT2(0), INT2(0) )
         ELSE IF ( KIN .AND. L_CLM .GT. 1 ) THEN
           CALL FERR ( INT2(4850), 'BATCH(gmcal) Keyword IN must appear '// &
     &         'by itself only once', INT2(0), INT2(0) )
         ELSE IF ( L_CLM .EQ. 0 ) THEN
           CALL FERR ( INT2(4860), 'BATCH(gmcal) No keyword was found in '// &
     &         'MODE_CALIBRATIOBNS section', INT2(0), INT2(0) )
         ELSE
           CALL CFUNRD ( LENGTH, STRING )
      ENDIF
      RETURN
      END  !#!  GMCAL  #!#
