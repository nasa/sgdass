      SUBROUTINE GSUPRS ( STACMP, FIXSTA_CHR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GSUPRS PROGRAM SPECIFICATION
!
! 1.1 Parse SUPPRESSION section of the control file.
!
! 1.2 REFERENCES:
!
! 2.  GSUPRS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER STACMP*(*), FIXSTA_CHR*8
!
! STACMP - Station component flag
!
! 2.3 OUTPUT Variables: None
!
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrlfl
!       CALLED SUBROUTINES: cfread,gvelsp,gnut,gsrcsp,graosp,gorisp,
!                           gstasp,cfunrd
!
! 3.  LOCAL VARIABLES
!
      CHARACTER STRING*256, TOKEN*256, STR*256
      INTEGER*2 LENGTH, IDUM, ITOKEN(40), BLANK, I, J
      LOGICAL*2 KVEL, KNON, KNUT, KPRE, KTID, KREL, KDIR, KSR, KST, KOR, KRA
      LOGICAL*2 KVO, KVT, KSTT, KPROP, KDC
      LOGICAL*4 LEX
      INTEGER*2 DUMB(4)
      CHARACTER NUTLCL(116)*2
      INTEGER*4, EXTERNAL :: I_LEN
!
!  EXTERNAL FUNCTIONS
!
      INTEGER*2 CFREAD,TRIMLEN,ichmv
      LOGICAL*2 CFEOF
!
      EQUIVALENCE (TOKEN,ITOKEN(1))
!
      DATA KVEL/.FALSE./,KNON/.FALSE./,KNUT/.FALSE./,KDIR/.FALSE./
      DATA KTID/.FALSE./,KREL/.FALSE./,KPRE/.FALSE./,KSR/.FALSE./
      DATA KST /.FALSE./,KOR /.FALSE./,KRA /.FALSE./,kvo/.FALSE./
      DATA kvt /.FALSE./,kprop /.FALSE./,kstt /.FALSE./,kdc/.FALSE./
      DATA BLANK/2H  /
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  910318  Add VELOCITY_ORIGIN option to minimize adjustments among
!                    a group of stations
!   mwh  910318  Add VELOCITY_TIE option to allow tying together the
!                  velocities of groups of stations
!   kdb  950720  Disable the ability to apply the velocity_origin
!                constraint if UEN sites are being estimated.
!   kdb  961125  Fix errors: initialize velsup(*,4) and sousup(*,4-6) to
!                0 (off).
!   kdb  960115  Activate corrected no net rotation for sources constraint.
!                (Don't reinitialize isrcsp here or the constraint will be
!                not be applied.)
!                Also fix error in which this sub zeroes defcmp and defvel,
!                which would erase bits set by the $CONSTRAINTS section.
!   pet  2000.05.09  Deactivated support of the keyword NUTATION since CALC 9.1
!                    doesn't support any more partils wrt specific terms of
!                    nutation expansion
!   pet  2000.07.19  Added support of qualifier NO for all keywords.
!
!   pet  2001.08.10  converted type of FIXSTA ( INTEGER*2 ) to FIXSTA_CHR
!                    ( CHARACTER )
!   pet  2004.07.01  Fixed bug related to processing STATION_ORIGIN and
!                    VELOCITY_ORIGIN keywords: if station coordinates were 
!                    parameterized as UEN, a spurious error message emerged 
!                    and solve stopped
!
!   pet  2007.10.01  Added support of keyword SUPPRESS_FILE
!
!
! 5.  GSUPRS PROGRAM STRUCTURE
!
      CALL USE_GLBFIL_4 ( 'OR' )
      PRESUP=.FALSE.
      RELSUP=.FALSE.
      TIDSUP=.FALSE.
      VELOHORIZ = 0
!
      numvelgrp=0
      do i=1,MAX_STA
        velties(i)=0
      enddo
!     Note: some cmpsup, velsup and sousup slots are used in the $constraints
!           section, and those slots are initialized there.
!           Do not re-initialize them (and overwrite part of the batch setup)
!           here!!
      DO J=1,4
        DO I=1,STA_BIT_WORDS
          CMPSUP(I,J)=0
          VELSUP(I,J)=0
        ENDDO
      ENDDO
!
      DO J=1,6
        DO I=1,SRC_BIT_WORDS
           SOUSUP(I,J)=0
        ENDDO
      ENDDO
!
      DO I=1,116
        NUTLCL(I)='NN'
        CALL CHAR2HOL( NUTLCL(I), NUTSUP(I), INT2(1), INT2(2) )
      ENDDO
!
      DO I=1,4
        DATSTA(I)=BLANK
        DTOSTA(I)=BLANK
      ENDDO
      CALL CLRCH ( SUPPRESS_FILE )
!
      LENGTH=CFREAD(STRING)
      DO WHILE(STRING(1:1).EQ.' '.AND..NOT.CFEOF(IDUM))
        DO WHILE(TRIMLEN(STRING).GT.0)
          CALL SPLITSTRING(STRING,TOKEN,STRING )
!
! 'VELOCITIES' KEYWORD
!
          IF(TOKEN.EQ.'VELOCITIES') THEN
            IF(KVEL) CALL FERR( INT2(5010), 'VELOCITIES USED TWICE', INT2(0), &
     &         INT2(0) )
            CALL GVELSP(DEFVEL,VELSUP,STASUP,ISTASP,DEFCMP,CMPSUP, &
     &                  TOKEN,STRING )
            KVEL=.TRUE.
          ELSE IF(TOKEN.EQ.'PROPER_MOTIONS') THEN
            IF(kprop) CALL FERR( INT2(5010), 'PROPER_MOTIONS USED TWICE', &
     &         INT2(0), INT2(0) )
            CALL gpropsp(defsrc,sousup,srcsup,isrcsp,token,string )
            kprop=.TRUE.
          ELSE IF(TOKEN.EQ.'VELOCITY_ORIGIN') THEN
            STR = STRING
            CALL CHASHL ( STR  )
            IF ( STR(1:3) .NE. 'NO '  .AND.  STACMP .EQ. 'UEN' ) THEN 
                 CALL FERR( INT2(5010), 'VELOCITY_ORIGIN FORBIDDEN UNDER '// &
     &                'UEN ESTIMATION', INT2(0), INT2(0) )
            END IF
            IF(kvo) CALL FERR( INT2(5010), 'VELOCITY_ORIGIN USED TWICE', &
     &         INT2(0), INT2(0) )
            CALL gvlosp(DEFVEL,VELSUP,STASUP,ISTASP,DEFCMP,CMPSUP, &
     &                  TOKEN,STRING,velohoriz )
            kvo=.TRUE.
          ELSE IF(TOKEN.EQ.'VELOCITY_TIE') THEN
            IF(kvt) CALL FERR( INT2(5010), 'VELOCITY_TIE USED TWICE', INT2(0), &
     &         INT2(0) )
            CALL GVELTIE ( NUMVELGRP, VELTIES, STASUP, ISTASP, TOKEN, &
     &                     STRING, DEFVEL, VELSUP, DEFCMP, CMPSUP )
            kvt=.TRUE.
          ELSE IF(TOKEN.EQ.'STATION_TIE') THEN
            IF(kstt) CALL FERR( INT2(5010), 'STATION_TIE USED TWICE', INT2(0), &
     &         INT2(0) )
            CALL gstatie(numstagrp,staties,stasup,istasp,token,string, &
     &           defvel,velsup,defcmp,cmpsup )
            kstt=.TRUE.
          ELSE IF(TOKEN.EQ.'NONE') THEN
            IF(KNON) CALL FERR( INT2(5020), 'NONE USED TWICE', INT2(0), &
     &         INT2(0) )
            KNON=.TRUE.
          ELSE IF(TOKEN.EQ.'NUTATION') THEN
             CALL FERR ( INT2(2832), 'GSUPRS(BATCH) Keyword NUTATION in '// &
     &           '$SUPPRESSION section is not supported any more. Please '// &
     &           'remove this keyword from your control file', INT2(0), INT2(0) )
             STOP 'BATCH(gsuprs) Abonrmal termination'
!!            IF(KNUT) CALL FERR(5030,'NUTATION USED TWICE',0,0)
!!            CALL GNUT(NUTLCL,TOKEN,STRING,FCNPR,'SUPP')
!!            DO I=1,116
!!              CALL CHAR2HOL(NUTLCL(I),NUTSUP(I),1,2)
!!            ENDDO
!!            KNUT=.TRUE.
          ELSE IF ( TOKEN .EQ. 'PRECESSION' ) THEN
            IF(KPRE) CALL FERR( INT2(5030), 'PRECESSION USED TWICE', INT2(0), &
     &         INT2(0) )
            CALL SPLITSTRING(STRING,TOKEN,STRING )
            IF ( TOKEN .NE. 'YES' ) THEN
                 PRESUP = .TRUE.
              ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                 PRESUP = .FALSE.
              ELSE
                 CALL FERR ( INT2(5035), &
     &               'GSUPS: Illegal precession parameter '//TOKEN(1:16)// &
     &               'YES or NO were expected', INT2(0), INT2(0) )
            ENDIF
            KPRE=.TRUE.
          ELSE IF ( TOKEN .EQ. 'RELATIVITY' ) THEN
            IF(KREL) CALL FERR( INT2(5040), 'RELATIVITY USED TWICE', INT2(0), &
     &         INT2(0) )
            CALL SPLITSTRING(STRING,TOKEN,STRING )
            IF ( TOKEN .EQ. 'YES' ) THEN
                 RELSUP = .TRUE.
               ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                 RELSUP = .FALSE.
               ELSE
                 CALL FERR ( INT2(5045), &
     &               'GSUPRS: Illegal relativity parameter '//TOKEN(1:16), &
     &                INT2(0), INT2(0) )
            ENDIF
            KREL=.TRUE.
          ELSE IF(TOKEN.EQ.'TIDES') THEN
!            IF(KTID) CALL FERR(5050,'TIDES USED TWICE',0,0)
!            CALL SPLITSTRING(STRING,TOKEN,STRING)
!            IF(TOKEN.NE.'YES'.AND.TOKEN.NE.'NO') THEN
!              CALL FERR(5055,'ILLEGAL TIDES PARAMETER '//TOKEN,0,0)
!            ENDIF
!            TIDSUP=TOKEN(1:1).EQ.'Y'
!            KTID=.TRUE.
             CALL FERR ( INT2(2834), 'GSUPRS(BATCH) Keyword TIDES in the '// &
     &           '$SUPPRESSION section is not supported any more. Please '// &
     &           'remove this keyword from your control file', INT2(0), INT2(0) )
             STOP 'BATCH(suprs) Abonrmal termination'
          ELSE IF ( TOKEN .EQ. 'DIRECTION' ) THEN
            IF ( KDIR ) CALL FERR ( INT2(5060), &
     &          'GSUPRS(batch) Keyword DIRECTIONS '//'used twice', INT2(0), &
     &           INT2(0))
            CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
            IF ( TOKEN .EQ. 'YES' .OR. TOKEN .EQ. 'yes' ) THEN
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
            END IF
            IF ( TOKEN .NE. 'NO'  .AND.  TOKEN .NE. 'no' ) THEN
!@U                 CALL UNDSCR   ( TOKEN )
                 CALL CHAR2HOL ( TOKEN, DATSTA, INT2(1), INT2(8) )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .NE. 'TO'  .AND.  TOKEN .NE. 'to' ) THEN
                      CALL FERR ( INT2(5065), 'GSUPRS(batch) Qualifier TO '// &
     &                    'missing from in the keyword DIRECTION of '// &
     &                    '$SUPPRESSION section', INT2(0), INT2(0) )
                 END IF
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!@U                 CALL UNDSCR   ( TOKEN )
                 CALL CHAR2HOL ( TOKEN, DTOSTA, INT2(1), INT2(8) )
!
! -------------- Make sure that fixed station (if mentioned) is treated as the
! -------------- origin for direction suppression
!
                 IF ( TOKEN .EQ. FIXSTA_CHR ) THEN
!@                      IDUM = ICHMV ( DUMB, INT2(1), DTOSTA, INT2(1), INT2(8) )
!@                      IDUM = ICHMV ( DTOSTA, INT2(1), DATSTA, INT2(1), INT2(8))
!@                      IDUM = ICHMV ( DATSTA, INT2(1), DUMB, INT2(1), INT2(8) )
                      CALL LIB$MOVC3 ( 8, DTOSTA, DUMB   )
                      CALL LIB$MOVC3 ( 8, DATSTA, DTOSTA )
                      CALL LIB$MOVC3 ( 8, DUMB,   DTOSTA )
                 ENDIF
                 KDIR=.TRUE.
            ENDIF
          ELSE IF(TOKEN.EQ.'SOURCES') THEN
            IF(KSR) CALL FERR( INT2(5010), 'SOURCES USED TWICE', INT2(0), &
     &         INT2(0) )
            CALL GSRCSP(DEFSRC,SOUSUP,SRCSUP,ISRCSP,TOKEN,STRING )
            KSR=.TRUE.
          ELSE IF(TOKEN.EQ.'RIGHT_ASCENSION') THEN
            IF(KRA) CALL FERR( INT2(5010), 'RIGHT_ASCENSION USED TWICE', &
     &         INT2(0), INT2(0) )
            CALL GRAOSP(DEFSRC,SOUSUP,SRCSUP,ISRCSP,TOKEN,STRING )
            KRA=.TRUE.
          ELSE IF(TOKEN.EQ.'DECLINATION') THEN
            IF(KDC) CALL FERR( INT2(5010), 'DECLINATION USED TWICE', INT2(0), &
     &                          INT2(0) )
            CALL GDCOSP(DEFSRC,SOUSUP,SRCSUP,ISRCSP,TOKEN,STRING )
            KDC=.TRUE.
          ELSE IF(TOKEN.EQ.'STATIONS') THEN
            IF(KST) CALL FERR( INT2(5010), 'STATIONS USED TWICE', INT2(0), &
     &         INT2(0) )
            CALL GSTASP ( DEFVEL, VELSUP, STASUP, ISTASP, DEFCMP, CMPSUP, &
     &                    STACMP, TOKEN, STRING )
            KST=.TRUE.
          ELSE IF ( TOKEN .EQ. 'STATION_ORIGIN' ) THEN
            IF(KOR) CALL FERR( INT2(5010), 'STATION_ORIGIN USED TWICE', &
     &         INT2(0), INT2(0) )
            STR = STRING
            CALL CHASHL ( STR  )
            IF ( STR(1:3) .NE. 'NO '  .AND.  STACMP .NE. 'XYZ' ) THEN
                 CALL FERR( INT2(5011), 'CANNOT SUPPRESS UEN ORIGIN', INT2(0), &
     &               INT2(0) )
            ENDIF
            CALL GORISP ( DEFVEL, VELSUP, STASUP, ISTASP, DEFCMP, CMPSUP, &
     &                    TOKEN, STRING )
            KOR=.TRUE.
          ELSE IF ( TOKEN .EQ. 'SUPPRESS_FILE' ) THEN
            CALL SPLITSTRING ( STRING, TOKEN, STRING )
            SUPPRESS_FILE = TOKEN
            IF ( SUPPRESS_FILE == 'NONE' ) THEN
                 CALL CLRCH ( SUPPRESS_FILE )
               ELSE
                 INQUIRE ( FILE=SUPPRESS_FILE, EXIST=LEX ) 
                 IF ( .NOT. LEX ) THEN
                      CALL ERR_LOG ( 1273, -2, 'GSUPRS', 'Cannot find file '// &
     &                     SUPPRESS_FILE(1:I_LEN(SUPPRESS_FILE))// &
     &                     ' specified in the $SUPPRESS section of the '// &
     &                     'control file' )
                      CALL EXIT ( 1 )
                 END IF
            END IF
          ELSE
!
! --------- Something that isn't suppose to be there
!
            CALL FERR( INT2(5090), 'UNKNOWN KEYWORD '//TOKEN(1:16), INT2(0), &
     &           INT2(0) )
          ENDIF
        ENDDO
        LENGTH=CFREAD(STRING)
      ENDDO
!
! NOW THAT THIS SECTION IS FINISHED, WHAT NOW?
!
      IF(KNON.AND. &
     &   (KVEL.OR.KNUT.OR.KPRE.OR.KREL.OR.KTID.OR.KDIR.OR.KSR.OR.KST.OR. &
     &   KOR.OR.KRA.or.kvo.or.kvt.or.kstt.or. &
     &   kdc)) THEN
        CALL FERR( INT2(5090), ' NONE MUST APPEAR BY ITSELF', INT2(0), &
     &       INT2(0) )
      ELSE IF(.NOT.(KNON.OR. &
     &  KVEL.OR.KNUT.OR.KPRE.OR.KREL.OR.KTID.OR.KDIR.OR.KSR.OR.KST.OR. &
     &  KOR.OR.KRA.or.kvo.or.kvt.or.kstt.or. &
     &  kdc)) THEN
        CALL FERR( INT2(5095), ' MISSING KEYWORDS FROM $SUPPRESSION', INT2(0), &
     &       INT2(0) )
      ELSE
        CALL CFUNRD(LENGTH,STRING )
      ENDIF
      CALL USE_GLBFIL_4 ( 'WC' )
      RETURN
      END  SUBROUTINE  GSUPRS  !#!#   
