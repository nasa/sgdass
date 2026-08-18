      SUBROUTINE GCARRY(STACRY,SRCCRY,STACMP)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GCARRY PROGRAM SPECIFICATION
!
! 1.1 Parse the CARRY section of the control file.
!
! 1.2 REFERENCES:
!
! 2.  GCARRY INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STACMP
!
! STACMP - Station component flag (XYZ, X-Z,--Z, etc.)
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) STACRY,SRCCRY
!
! SRCCRY - Source carry flag
! STACRY - Station carry flag
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc2.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrlfl
!       CALLED SUBROUTINES: gtlst,cfread,gtlst_st_ca,cfunrd
!
! 3.  LOCAL VARIABLES
!
      CHARACTER STRING*256, TOKEN*256
      INTEGER*2  LENGTH, IDUM, I
      INTEGER*4  NXSEL
      LOGICAL*2  KSTA, KSRC, KNUT, KAXIS, KTIDES
!
!  EXTERNAL FUNCTIONS
!
      INTEGER*2 CFREAD,TRIMLEN
      LOGICAL*2 CFEOF
!
!  INITIALIZATION
!
      DATA KSTA/.FALSE./,KSRC/.FALSE./,kaxis/.FALSE./
      data ktides/.FALSE./
!
! 4.  HISTORY
!
!     modifications
!
!     jmg 960610 Set max selar to gsfcb parameter.
!
! 5.  GCARRY PROGRAM STRUCTURE
!
! Initialize some things
!
      IACSRC=0
      IACSTA=0
      NACSRC=0
      NACSTA=0
      NXSEL=1
      kcaxis = .TRUE.
      kctide = .TRUE.
!
! Read first record of CARRY section
!
      LENGTH=CFREAD(STRING)
      DO WHILE(STRING(1:1).EQ.' '.AND..NOT.CFEOF(IDUM))
        DO WHILE(TRIMLEN(STRING).GT.0)
          CALL SPLITSTRING(STRING,TOKEN,STRING )
!
! 'SOURCES' KEYWORD
!
          IF(TOKEN.EQ.'SOURCES') THEN
            IF(KSRC) CALL FERR( INT2(5010), 'SOURCES USED TWICE', INT2(0), &
     &         INT2(0) )
            CALL GTLST ( TOKEN, STRING, KCSRC, IACSRC, NACSRC, ISELAR, NXSEL, &
     &                   MAX4_SELAR, ' ' )
            SRCCRY='N'
            IF(KCSRC) SRCCRY='Y'
            KSRC=.TRUE.
!
!  'STATIONS' KEYWORD
!
          ELSE IF(TOKEN.EQ.'STATIONS') THEN
            IF(KSTA) CALL FERR( INT2(5020), 'STATIONS USED TWICE', INT2(0), &
     &         INT2(0) )
            CALL GTLST_ST_CA ( TOKEN, STRING, KCSTA, IACSTA, NACSTA, ISELAR, &
     &                         NXSEL, MAX_SELAR, '_', STACMP, CARCMP )
            STACRY='N'
            IF(KCSTA.AND.(NACSTA .EQ. 0)) STACRY='Y'
            KSTA=.TRUE.
!
!  'AXIS' KEYWORD
!
          ELSE IF(TOKEN.EQ.'AXIS') THEN
            IF(kaxis) CALL FERR( INT2(5030), 'AXIS USED TWICE', INT2(0), &
     &         INT2(0) )
            call splitstring(string,token,string )
            if(token.eq.'YES') then
                kcaxis = .TRUE.
            else if(token.eq.'NO') then
                kcaxis = .FALSE.
            else
                call ferr( INT2(9010), 'ILLEGAL CARRY DEFAULT '//token(1:16), &
     &               INT2(0), INT2(0) )
            endif
            kaxis=.TRUE.
!
!  'TIDES' KEYWORD
!
          ELSE IF(TOKEN.EQ.'TIDES') THEN
             CALL FERR ( INT2(5002), 'BATCH(gcarry): Keyword TIDES is not '// &
     &           'supported any more. You can estimate Love '// &
     &           'numbers in user partials mode', INT2(0), INT2(0) )
             STOP 'BATCH(gcarry)'
!
!  SOMETHING THAT ISN'T SUPPOSE TO BE THERE
!
          ELSE
            CALL FERR( INT2(5090), 'UNKNOWN KEYWORD '//TOKEN(1:16), INT2(0), &
     &           INT2(0) )
          ENDIF
        ENDDO
        LENGTH=CFREAD(STRING)
      ENDDO
!
! NOW THAT THIS SECTION IS FINISHED, WHAT NOW?
!
      IF(.NOT.(KSTA.AND.KSRC)) THEN
        CALL FERR( INT2(5095), ' MISSING KEYWORDS FROM $CARRY', INT2(0), &
     &       INT2(0) )
      ELSE
        CALL CFUNRD(LENGTH,STRING )
      ENDIF
      RETURN
      END
