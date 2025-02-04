      SUBROUTINE GNUVCOV ( DEFNUVCOV, NUVCOVFLG, NUVEL_WT, STASUP, ISTASP, &
     &                     FIXED_PLATE, TOKEN, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      LOGICAL*2 L4TOL2
! 1.  GNUVCOV PROGRAM SPECIFICATION
!
! 1.1 Get NUVEL_COVAR info from CONSTRAINTS section of control file.
!
! 1.2 REFERENCES:
!
! 2.  GNUVCOV INTERFACE
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
      INTEGER*2 NUVCOVFLG(STA_BIT_WORDS), DEFNUVCOV
      REAL*8    NUVEL_WT
      INTEGER*2 ISTASP
      CHARACTER TOKEN*(*), FIXED_PLATE*(*)
!
! ISTASP - Number of stations in exception list
! TOKEN - Individual token from STRING
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
      LOGICAL*2   CFEOF
      CHARACTER*8 TEMP, BSLASH*1
      PARAMETER  ( BSLASH = CHAR(92) )
      INTEGER*2   I, LENGTH, CFREAD, IDUM, IPOS
      INTEGER*4   IOS
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215   replace '\' with BSLASH
!   pet   2000.06.02  Added support of a qualifier NO
!   pet   2000.11.22  Fixed a bug: variable DEFNUVCOV was defined as
!                     REAL*8. Improved comments
!
! 5.  GNUVCOV PROGRAM STRUCTURE
!
! Get first token from input string; must be YES, NO or UEN
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN .EQ. 'NO' ) THEN
           DEFNUVCOV = 0
           RETURN
         ELSE IF ( TOKEN .EQ. 'YES' ) THEN
           DEFNUVCOV = 1
         ELSE
           CALL FERR ( INT2(9010), 'GNUVCOV: illegal nuvel_covar parameter '// &
     &          TOKEN(1:16), INT2(0), INT2(0) )
      ENDIF
!
! --- Initialize bit pointer array to default value
!
      DO I=1,STA_BIT_WORDS
         NUVCOVFLG(I) = DEFNUVCOV
      ENDDO
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN .EQ. ' ' ) THEN
           CALL FERR ( INT2(456), 'NUVEL_COVAR must include fixed_plate', &
     &          INT2(0), INT2(0) )
      ENDIF
      FIXED_PLATE = TOKEN(1:4)
      CALL SPLITSTRING(STRING,TOKEN,STRING )
!
      IF ( TOKEN .NE. 'EXCEPT' .AND. TOKEN .NE. ' ' ) THEN
           READ ( TOKEN, *, IOSTAT=IOS ) NUVEL_WT
           CALL FERR ( INT2(IOS), "Reading NUVEL weighting factor", &
     &                 INT2(0), INT2(0) )
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
      ENDIF
!
! Get next token; if not EXCEPT then we're finished
!
      IF(TOKEN.EQ.'EXCEPT') THEN
!
! Get next token and then loop until no more tokens
!
        CALL SPLITSTRING(STRING,TOKEN,STRING )
        DO WHILE (TOKEN.NE.' ')
!
! Handle continuation line by reading next line of control file
!
          IF(TOKEN.EQ.BSLASH) THEN
            LENGTH=CFREAD(STRING)
            IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM))CALL FERR( INT2(9020), &
     &        'ILLEGAL CONTINUATION LINE '//STRING(1:10), INT2(0), INT2(0) )
          ELSE
            CALL UNDSCR(TOKEN )
            DO I=1,ISTASP
              CALL HOL2CHAR( STASUP(1,I), INT2(1), INT2(8), TEMP )
              IPOS=I
              IF(TOKEN.EQ.TEMP) GO TO 90
            ENDDO
            ISTASP=ISTASP+1
            IF(ISTASP.GT.MAX_STA)CALL FERR( INT2(9030), &
     &        'TOO MANY VELOCITIES '//TOKEN(1:10), INT2(0), INT2(0) )
            CALL CHAR2HOL( TOKEN, STASUP(1,ISTASP), INT2(1), INT2(8) )
            IPOS=ISTASP
90          CONTINUE
            CALL CHAR2HOL( TOKEN, STASUP(1,IPOS), INT2(1), INT2(8) )
            CALL KSBIT( nuvcovflg, IPOS, l4tol2(1-defnuvcov) )
          ENDIF
          CALL SPLITSTRING(STRING,TOKEN,STRING )
        ENDDO
      ELSE IF (TOKEN.NE.' ') THEN
        CALL FERR( INT2(9040), 'INCORRECT EXCEPT CLAUSE: '//TOKEN(1:10), &
     &       INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END  !#!  GNUVCOV  #!#
