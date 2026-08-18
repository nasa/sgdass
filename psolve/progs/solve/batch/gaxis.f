      SUBROUTINE GAXIS ( TOKEN, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GAXIS PROGRAM SPECIFICATION
!
! 1.1 Process EXCEPT clause of AXIS line of $FLAGS section.
!
! 1.2 REFERENCES:
!
! 2.  GAXIS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER  TOKEN*(*), STRING*(*)
!
! TOKEN - Token picked up from STRING
! STRING - String to be parsed
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'axscm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gflags
!       CALLED SUBROUTINES: cfread, splitstring
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   LENGTH, CFREAD
      CHARACTER*1 BSLASH
      PARAMETER  ( BSLASH = CHAR(92) )
!
! 4.  HISTORY
!  WHO  WHEN    WHAT
!   JLR  921215      replace '\' with BSLASH
!   PET  2001.09.26  Improved comments
!
! 5.  GAXIS PROGRAM STRUCTURE
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
!
      IF ( TOKEN .EQ. 'EXCEPT' ) THEN
           CALL SPLITSTRING(STRING,TOKEN,STRING )
           DO WHILE ( TOKEN .NE. ' ' )
              IF ( TOKEN .EQ. BSLASH ) THEN
                   LENGTH = CFREAD(STRING)
                ELSE
                  IF ( NUM_AXIS .EQ. MAX_STA ) THEN
                       CALL FERR ( INT2(9030), 'BATCH(gaxis) '// &
     &                     'Exception list for axis offsets is too long', INT2(0), &
     &                      INT2(0) )
                       STOP 'BATCH Abnornal termination'
                  END IF
!
!@U                  CALL UNDSCR ( TOKEN )
                  NUM_AXIS=NUM_AXIS + 1
                  AXNM_CHR(NUM_AXIS) = TOKEN
              ENDIF
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
          ENDDO
      ENDIF
!
      RETURN
      END  !#!  GAXIS  #!#
