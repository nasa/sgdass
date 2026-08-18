      SUBROUTINE GTLST ( TOKEN, STRING, KCAR, ISTRT, NOBJCT, SELAR, NXSEL, &
     &                   IMAX, SPACE )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GTLST PROGRAM SPECIFICATION
!
! 1.1 Get list of sources in CARRY section of the control file.
!
! 1.2 REFERENCES:
!
! 2.  GTLST INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER     SPACE*1, STR*16
      CHARACTER*(*) STRING
      INTEGER*4     NOBJCT, NXSEL, IMAX
!
! IMAX - Maximum carry
! NOBJCT -
! NXSEL - Current carry
! SPACE - Single blank character
! STRING - String to be parsed
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) TOKEN
      LOGICAL*2  KCAR
      INTEGER*4  ISTRT
      INTEGER*2  SELAR(IMAX)
      INTEGER*4, EXTERNAL :: I_LEN
!
! ISTRT -
! KCAR - Carry flag
! SELAR -
! TOKEN - Individual token pulled from STRING
! NOBJCT -
! NXSEL - Current carry
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gcarry
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 CFEOF
      CHARACTER*1 STDUM(8)
      INTEGER*2 I,ISTDUM(4),LENGTH,CFREAD,IDUM
      EQUIVALENCE (STDUM(1),ISTDUM(1))
      CHARACTER*1  BSLASH
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215      replace '\' with BSLASH
!   PET   2004.06.17  Improved error messages
!   PET   2022.12.19  Fixed INTEGER*2/INTEGER*4 bug
!
! 5.  GTLST PROGRAM STRUCTURE
!
! Parse the string:
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
!
!
      IF ( TOKEN .EQ. 'YES' ) THEN
           KCAR = .TRUE.
         ELSE IF(TOKEN.EQ.'NO') THEN
           KCAR = .FALSE.
         ELSE
           CALL FERR( INT2(9010), 'ILLEGAL CARRY DEFAULT '//TOKEN(1:16), &
     &                INT2(0), INT2(0) )
      ENDIF
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      IF ( TOKEN .EQ. ' ' ) THEN
           RETURN
         ELSE IF(TOKEN.EQ.'EXCEPT') THEN
           CALL SPLITSTRING(STRING,TOKEN,STRING )
           DO WHILE ( TOKEN .NE. ' ' )
              IF ( TOKEN .EQ. BSLASH ) THEN
                   LENGTH = CFREAD(STRING)
                   IF ( STRING(1:1) .EQ. '$' .OR. CFEOF(IDUM) ) THEN
                        CALL FERR( INT2(9020), 'ILLEGAL CONTINUATION LINE '// &
     &                             STRING(1:10), INT2(0), INT2(0) )
                   ENDIF
                 ELSE
                   IF ( NXSEL+3 .GT. IMAX ) THEN
                        CALL CLRCH  ( STR ) 
                        CALL INCH   ( IMAX, STR )
                        CALL CHASHL ( STR )
                        CALL FERR( INT2(9030), 'Run out of carry room '// &
     &                             TOKEN(1:10)//' the number of tokens '// &
     &                            'exceeded the limit '//STR(1:I_LEN(STR)), &
     &                             INT2(0), INT2(0) )
                   END IF
                   IF ( NOBJCT .EQ. 0 ) ISTRT=NXSEL
                   DO I=1,8
                      STDUM(I)=TOKEN(I:I)
                      IF ( STDUM(I) .EQ. SPACE ) STDUM(I)=' '
                   ENDDO
                   DO I=1,4
                      SELAR(NXSEL)=ISTDUM(I)
                      NXSEL=NXSEL+1
                   ENDDO
                   NOBJCT=NOBJCT+1
              ENDIF
              CALL SPLITSTRING(STRING,TOKEN,STRING )
           ENDDO
      ENDIF
!
      RETURN
      END  !#! GTLST  #!#
