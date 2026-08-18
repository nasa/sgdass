      SUBROUTINE GCONT()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GCONT PROGRAM SPECIFICATION
!
! 1.1 Parse CONTRIBUTIONS section of the control file.
!
! 1.2 REFERENCES:
!
! 2.  GCONT INTERFACE
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
      LOGICAL*2 KDEF, CFEOF, KNON
!
      DATA KDEF / .FALSE. /, &
     &     KNON / .FALSE. /
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   pet  1999.11.18  Corrected a bug: the previous version allowed no more
!                    then MAX_CONT-1 instead of MAX_CONT contributions.
!                    Improved comments.
!   pet  2000.05.11  Added support of a keyword SET
!
! 5.  GCONT PROGRAM STRUCTURE
!
      ICONTR=0
!
! --- Read first record of the CONTRIBUTIONS section
!
      LENGTH = CFREAD(STRING)
      DO WHILE ( STRING(1:1) .EQ. ' ' .AND. .NOT.CFEOF(IDUM) )
          DO WHILE ( TRIMLEN(STRING).GT.0 )
             CALL SPLITSTRING ( STRING, TOKEN, STRING )
             IF ( TOKEN .EQ. 'SET'  .OR.  TOKEN .EQ. 'set' ) THEN
!
! --------------- Skip SET keyword
!
                  CALL SPLITSTRING ( STRING, TOKEN, STRING )
             END IF
             ICONTR=ICONTR+1
             IF ( ICONTR .GT. MAX_CONT ) THEN
                  CALL FERR ( INT2(10010), &
     &                'BATCH(gcont) Too many contributions', INT2(0), INT2(0) )
             ENDIF
!
             CONTRB(ICONTR) = TOKEN
             CALL UNDSCR ( CONTRB(ICONTR) )
!
             IF ( CONTRB(ICONTR) .EQ. 'IN'  .OR. &
     &            CONTRB(ICONTR) .EQ. 'DEFAULT'  ) THEN
!
! --------------- IN (DEFAULT) keyword
!
                  IF ( KDEF ) CALL FERR ( INT2(10020), &
     &                'BATCH(gcont) keyword '//CONTRB(ICONTR)//' used twice', &
     &                 INT2(0), INT2(0) )
                  KDEF = .TRUE.
                ELSE IF ( CONTRB(ICONTR) .EQ. 'NONE' .OR. &
     &                    CONTRB(ICONTR) .EQ. 'NO'        ) THEN
!
! --------------- NONE keyword
!
                  IF ( KNON ) CALL FERR ( INT2(10030), &
     &                'BATCH(gcont) keyword '//CONTRB(ICONTR)//' used twice', &
     &                 INT2(0), INT2(0) )
                  KNON = .TRUE.
             ENDIF
          ENDDO
          LENGTH = CFREAD ( STRING )
      ENDDO
!
! --- Now that this section is finished, what now?
!
      IF ( KDEF .AND. ICONTR.GT.1 ) THEN
           CALL FERR ( INT2(10040), &
     &         'BATCH(gcont) Keyword SET IN or DEFAULT must '// &
     &         'appear by itself only once', INT2(0), INT2(0) )
         ELSE IF ( KNON .AND. ICONTR.GT.1 ) THEN
           CALL FERR ( INT2(10050), &
     &         'BATCH(gcont) Keyword SET NO or NONE must '// &
     &         'appear by itself only once', INT2(0), INT2(0) )
         ELSE IF ( ICONTR .EQ. 0 ) THEN
           CALL FERR ( INT2(10060), 'BATCH(gcont) No keyword was found in '// &
     &         'CONTRIBUTIONS section', INT2(0), INT2(0) )
         ELSE
           CALL CFUNRD ( LENGTH, STRING )
      ENDIF
      RETURN
      END  !#!  GCONT  #!#
