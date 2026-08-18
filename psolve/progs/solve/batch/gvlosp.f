      SUBROUTINE GVLOSP(DEFVEL,VELSUP,STASUP,ISTASP,DEFCMP,CMPSUP, &
     &                  TOKEN,STRING,velohoriz)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      LOGICAL*2 L4TOL2
! 1.  GVLOSP PROGRAM SPECIFICATION
!
! 1.1 Parse VELOCITY_ORIGIN line of $SUPPRESSION section.
!
! 1.2 REFERENCES:
!
! 2.  GVLOSP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 DEFVEL(2),STASUP(4,MAX_STA)
      INTEGER*2 ISTASP,DEFCMP,CMPSUP(STA_BIT_WORDS,*)
      CHARACTER*(*) STRING
!
! DEFVEL - Default station componenets for velocities
! DEFCMP - Default station componenets for positions
! ISTASP - Number of stations in the exception list
! STASUP - Station components for stations in exception list
! STRING - Remainder of STATION_ORIGIN line
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 VELSUP(STA_BIT_WORDS,*)
      CHARACTER*(*) TOKEN
      integer*2 velohoriz
!
! CMPSUP - Station suppression flag
! TOKEN - Single token from STRING
! VELSUP - Velocity suppression flag
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gsuprs
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2   CFEOF,CHKCMP,OKAY,KBIT
      CHARACTER*1 VEL
      CHARACTER   TEMP*8, FIRST_TOKEN*32
      INTEGER*2   I, LENGTH, CFREAD, IDUM, J, IPOS, KBITN, IVAL
      CHARACTER*1 BSLASH
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  910225  created, based on GORISP
!   JLR  921215  replaced '\' with BSLASH
!   KDB  950720  Add the ability to have horizontal-only or vertical-only
!                velocity_origin constraints.  Also add the "both" token
!                as an alternative to the more esoteric "horizontal" token.
!   kdb  961125  declare cmpsup,velsup with *
!   pet  2000.05.10  Added support of a qualifier XYZ
!   pet  2000.07.19  Added support of a qualifier NO
!
! 5.  GVLOSP PROGRAM STRUCTURE
!
      velohoriz = 0  !neither horizontal nor vertical
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      FIRST_TOKEN = TOKEN
      IF ( TOKEN .EQ. 'NO' ) THEN
           CONTINUE
         ELSE IF ( TOKEN .EQ. 'YES' ) THEN
           CALL SBIT( DEFVEL, INT2(4), INT2(1) )
         ELSE
           CALL FERR ( INT2(9010), 'ILLEGAL VELOCITY ORIGIN PARAMETER '// &
     &          TOKEN(1:16), INT2(0), INT2(0) )
      ENDIF
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( FIRST_TOKEN .EQ. 'NO' .AND. TOKEN(1:1) .EQ. ' ' ) THEN
           RETURN
      END IF
!
!
      IVAL=KBITN( DEFVEL, INT2(4) )
!
!***** Initialize whole velsup array, not just first istasp elements
!        mwh  930830
!
      DO I=1,MAX_STA
         CALL SBIT ( VELSUP(1,4), I, IVAL )
      ENDDO
!
      IF ( TOKEN .EQ. 'HORIZ' .OR. TOKEN .EQ. 'BOTH' ) THEN
!
! -------- Old horiz token retained for backwards compatibility
!
         CALL SBIT ( VELOHORIZ, INT2(1), INT2(1) ) ! horizontal constraint
         CALL SBIT ( VELOHORIZ, INT2(2), INT2(1) ) ! vertical constraint
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
        ELSE IF ( TOKEN .EQ. 'HORIZ_ONLY' ) THEN
         CALL SBIT ( VELOHORIZ, INT2(1), INT2(1) ) ! horizontal constraint
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
        ELSE IF ( TOKEN .EQ. 'VERT_ONLY' ) THEN
           CALL SBIT ( VELOHORIZ, INT2(2), INT2(1) ) ! vertical constraint
           CALL SPLITSTRING(STRING,TOKEN,STRING )
        ELSE IF ( TOKEN .EQ. 'XYZ' ) THEN
           CONTINUE
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
        ELSE
           CALL FERR ( INT2(9086), 'gvlosp(BATCH) Unsopported qualifier '// &
     &          TOKEN//' occurred after the keyword VELOSITY_ORIGIN '// &
     &         'in suppression section of your control file. '// &
     &         'One of HOIRZ_ONLY, VERT_ONLY, BORH or XYZ was expected', INT2(0), &
     &          INT2(0) )
           STOP 'BATCH Abnormal termination'
      ENDIF
!
      IF ( TOKEN .EQ. 'EXCEPT' ) THEN
           IF ( KBIT ( DEFVEL, INT2(4) ) ) THEN
                VEL='N'
             ELSE
                VEL='Y'
           ENDIF
!
        CALL SPLITSTRING(STRING,TOKEN,STRING )
        DO WHILE (TOKEN.NE.' ')
          IF(TOKEN.EQ.BSLASH) THEN
            LENGTH=CFREAD(STRING)
            IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM))CALL FERR( INT2(9020), &
     &        'ILLEGAL CONTINUATION LINE '//STRING(1:10), INT2(0), INT2(0) )
          ELSE
!@U            CALL UNDSCR(TOKEN )
            DO I=1,ISTASP
              CALL HOL2CHAR( STASUP(1,I), INT2(1), INT2(8), TEMP )
              IPOS=I
              IF(TOKEN.EQ.TEMP) GO TO 90
            ENDDO
            ISTASP=ISTASP+1
            IF(ISTASP.GT.MAX_STA)CALL FERR( INT2(9030), &
     &        'TOO MANY VELOCITIES '//TOKEN(1:10), INT2(0), INT2(0) )
            CALL CHAR2HOL( TOKEN, STASUP(1,ISTASP), INT2(1), INT2(8) )
!
!***** Remove following lines because this initialization will now be
!       done elsewhere  mwh  930830
!
!            CALL KSBIT(cmpSUP(1,1),(ISTASP),KBIT(DEFcmp,1))
!            CALL KSBIT(cmpSUP(1,2),(ISTASP),KBIT(DEFcmp,2))
!            CALL KSBIT(cmpSUP(1,3),(ISTASP),KBIT(DEFcmp,3))
!            CALL KSBIT(cmpSUP(1,4),(ISTASP),KBIT(DEFcmp,4))
!            CALL KSBIT(velSUP(1,1),(ISTASP),KBIT(DEFvel,1))
!            CALL KSBIT(velSUP(1,2),(ISTASP),KBIT(DEFvel,2))
!            CALL KSBIT(velSUP(1,3),(ISTASP),KBIT(DEFvel,3))
            IPOS=ISTASP
90          CONTINUE
            CALL KSBIT ( VELSUP(1,4), IPOS, l4tol2(VEL(1:1) .EQ. 'Y') )
          ENDIF
          CALL SPLITSTRING(STRING,TOKEN,STRING )
        ENDDO
      ELSE IF (TOKEN.NE.' ') THEN
        CALL FERR( INT2(9040), 'INCORECT EXCEPT CLAUSE: '//TOKEN(1:10), &
     &       INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END
