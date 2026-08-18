      SUBROUTINE GVELCNST ( DEFVELCNST, ALTVELCNST, VELCON, STASUP, ISTASP, &
     &                      TOKEN, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GVELCNST PROGRAM SPECIFICATION
!
! 1.1 Get VELOCITY info from CONSTRAINTS section of control file.
!
! 1.2 REFERENCES:
!
! 2.  GVELCNST INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 STASUP(4,MAX_STA)
      CHARACTER*(*) STRING
!
! STASUP - Station components for stations in exception list
! STRING - String to be parsed
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 VELCON(STA_BIT_WORDS)
      real*8 DEFVELcnst(3),altvelcnst(3)
      INTEGER*2 ISTASP
      CHARACTER*(*) TOKEN
!
! ISTASP - Number of stations in exception list
! TOKEN - Individual token from STRING
! VELCON - Velocity constraint flag
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gconst
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2  CFEOF
      CHARACTER  TEMP*8, BSLASH*1, COMP_STR(3)*11
      DATA       &
     &         COMP_STR/ 'u-compoment', 'e-compoment', 'n-compoment' /
      INTEGER*2  I, LENGTH, CFREAD, IDUM, IERR, IPOS
      INTEGER*4  IOS
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN    WHAT
!   JLR   921215  replace '\' with BSLASH
!   PET   990312  updated comments. Improved error messages
!
! 5.  GVELCNST PROGRAM STRUCTURE
!
! Get first token from input string; must be YES, NO or UEN
!
      DO I=1,STA_BIT_WORDS
         VELCON(I) = 0
      ENDDO
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN .EQ. 'NO' ) THEN
           DO I=1,3
              DEFVELCNST(I) = -1.D0
           ENDDO
        ELSE IF(TOKEN.EQ.'YES') THEN
           DO I=1,3
              ALTVELCNST(I) = -1.D0
           ENDDO
!
           DO I=1,3
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( TOKEN(1:5) .EQ. 'SIGMA' ) THEN
!
! ---------------- Skip a word SIGMA
!
                   CALL SPLITSTRING ( STRING, TOKEN, STRING )
              END IF
!
              IF ( TOKEN .NE. ' ' ) THEN
                   READ ( TOKEN, *, IOSTAT=IOS ) DEFVELCNST(I)
                   IERR = IOS
                   IF ( IERR .NE. 0 ) THEN
                        CALL FERR ( INT2(6411), &
     &                      'GVELCNST Error in decoding the '//COMP_STR(I)// &
     &                      ' velocity constraint: '//TOKEN(1:16), INT2(0), &
     &                       INT2(0) )
                   END IF
                 ELSE
                   CALL FERR ( INT2(6412), 'GVELCNST No sigma for the '// &
     &                  COMP_STR(I)//' velocity constraint was supplied', INT2(0), &
     &                  INT2(0) )
              END IF
           ENDDO
         ELSE
           CALL FERR ( INT2(9010), &
     &         'GVELCONST: illegal velocities parameter '//TOKEN(1:16), INT2(0), &
     &          INT2(0) )
      ENDIF
!
! --- Get next token; if not EXCEPT then we're finished
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN .EQ. 'EXCEPT' ) THEN
           DO I=1,3
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              IF ( TOKEN(1:5) .EQ. 'SIGMA' ) THEN
!
! ---------------- Skip a word SIGMA
!
                   CALL SPLITSTRING ( STRING, TOKEN, STRING )
              END IF
!
              IF ( TOKEN .NE. ' ' ) THEN
                   READ ( TOKEN, *, IOSTAT=IOS ) ALTVELCNST(I)
                   IERR = IOS
                   IF ( IERR .NE. 0 ) THEN
                        CALL FERR ( INT2(6421), &
     &                      'GVELCNST Error in decoding the '//COMP_STR(I)// &
     &                      ' velocity constraint: '//TOKEN(1:16), INT2(0), &
     &                       INT2(0) )
                   END IF
                 ELSE
                   CALL FERR ( INT2(6422), 'GVELCNST No sigma for the '// &
     &                  COMP_STR(I)//' velocity constraint was supplied', INT2(0), &
     &                  INT2(0) )
              END IF
!
!
           ENDDO
!
! -------- Get next token and then loop until no more tokens
!
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
           DO WHILE ( TOKEN .NE. ' ' )
!
! ----------- Handle continuation line by reading next line of control file
!
              IF ( TOKEN .EQ. BSLASH ) THEN
                   LENGTH = CFREAD(STRING)
                   IF ( STRING(1:1).EQ.'$' .OR. CFEOF(IDUM) ) THEN
                        CALL FERR ( INT2(9020), &
     &                      'GVELCONST: illegal continuation '//'LINE '// &
     &                       STRING(1:10), INT2(0), INT2(0) )
                   END IF
                 ELSE
                   CALL UNDSCR ( TOKEN )
                   DO I=1,ISTASP
                      CALL HOL2CHAR( STASUP(1,I), INT2(1), INT2(8), TEMP )
                      IPOS=I
                      IF ( TOKEN .EQ. TEMP ) GOTO 90
                   ENDDO
!
                   ISTASP=ISTASP+1
                   IF ( ISTASP .GT. MAX_STA ) THEN
                        CALL FERR ( INT2(9030), &
     &                      'GVELCONST: Too many velocities '//TOKEN(1:10), &
     &                       INT2(0), INT2(0) )
                   END IF
!
                   CALL CHAR2HOL( TOKEN, STASUP(1,ISTASP), INT2(1), INT2(8) )
                   IPOS=ISTASP
!
90                 CONTINUE
                   CALL KSBIT ( VELCON(1), IPOS, 1 )
              ENDIF
!
              CALL SPLITSTRING(STRING,TOKEN,STRING )
           ENDDO
        ELSE IF ( TOKEN .NE. ' ' ) THEN
           CALL FERR ( INT2(9040), 'GVELCONST: Incorrect except clause: '// &
     &          TOKEN(1:10), INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END  !#!  GVELCNST  #!#
