      SUBROUTINE GBASE ( TOKEN, STRNG )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GBASE PROGRAM SPECIFICATION
!
! 1.1 Process EXCEPT clause of BASELINE line of $OUTPUT section.
!
! 1.2 REFERENCES:
!
! 2.  GBASE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER TOKEN*(*), STRNG*(*)
!
! TOKEN - Token picked up from STRNG
! STRNG - String to be parsed
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'ba2cm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gflags
!       CALLED SUBROUTINES: cfread, splitstring
!
! 3.  LOCAL VARIABLES
!
      CHARACTER STDUM(16)*1, BSLASH*1
      INTEGER*2 I, ISTDUM(8), LENGTH, CFREAD
      EQUIVALENCE (STDUM(1),ISTDUM(1))
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!  WHO  WHEN        WHAT
!  JLR  921215      replaced '\' with BSLASH
!  pet  2000.05.08  Added support of construction BASELINE NO
!
! 5.  GBASE PROGRAM STRUCTURE
!
      NEXCBL = 0
      CALL SPLITSTRING ( STRNG, TOKEN, STRNG )
!
      IF ( TOKEN .EQ. 'EXCEPT' ) THEN
           CALL SPLITSTRING ( STRNG, TOKEN, STRNG )
           DO WHILE  ( TOKEN .NE. ' ' )
              IF ( TOKEN .EQ. BSLASH ) THEN
                   LENGTH = CFREAD(STRNG)
                 ELSE
                   IF ( NEXCBL .EQ. MAX_STA ) THEN
                        WRITE ( 6, * ) ' NEXCBL=',NEXCBL,' MAX_STA=',MAX_STA
                        CALL FERR ( INT2(9030), &
     &                      'GBASE(BATCH) Too many baselines '// &
     &                      'in keyword BASELINES in DATA section', INT2(0), &
     &                       INT2(0) )
                   END IF
                   DO I=1,8
                      STDUM(I)=TOKEN(I:I)
                      IF ( STDUM(I) .EQ. '_' ) STDUM(I)=' '
                   ENDDO
!
                   DO I=9,16
                      STDUM(I)=TOKEN(I+1:I+1)
                      IF ( STDUM(I) .EQ. '_' ) STDUM(I)=' '
                   ENDDO
                   NEXCBL=NEXCBL+1
                   DO I=1,8
                      IBLNM(I,NEXCBL) = ISTDUM(I)
                   ENDDO
              ENDIF
              CALL SPLITSTRING ( STRNG, TOKEN, STRNG )
           ENDDO
         ELSE
           BASDF = 'Y'
      ENDIF
!
      RETURN
      END  !#!  GBASE  #!#
