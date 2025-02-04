      SUBROUTINE GDATA ( BASDF )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GDATA PROGRAM SPECIFICATION
!
! 1.1 Parse DATA section of the control file.
!
! 1.2 REFERENCES:
!
! 2.  GDATA INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
      character*(*) basdf
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'bdata.i'
      INCLUDE 'belev.i'
      INCLUDE 'bwvrm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'glbc2.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrlfl
!       CALLED SUBROUTINES: cfread,gtlst,gtelev,gtwvm,cfunrd
!
! 3.  LOCAL VARIABLES
!
      CHARACTER STRING*256, TOKEN*256
      INTEGER*4 NXSEL
      INTEGER*2 LENGTH, IDUM, ERR, DECIMALTOINT, I, ICT
      LOGICAL*2 KSTA,KSRC,KTYP,KNRT,KELC,KWVM,KEV,KBAS
      CHARACTER DATYP__ABR__ARR(FIRST__DTP:LAST__DTP)*6
!!                   CHARACTER  STR*2048
!
!  EXTERNAL FUNCTIONS
!
      INTEGER*2  CFREAD,TRIMLEN
      LOGICAL*2  CFEOF
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  IL, LTM_DIF
!
!  INITIALIZATION
!
      DATA KSTA/.FALSE./,KSRC/.FALSE./,KTYP/.FALSE./,KELC/.FALSE./
      DATA KEV/.FALSE./,KBAS/.FALSE./,KNRT/.FALSE./, KWVM /.FALSE./
!CCCCC
!
! 4.  HISTORY
!   WHO  WHEN       WHAT
!
!   pet  1998.04.01 Added support of extended list of solution type
!                   abbreviations.
!   pet  2000.01.25 Added support of a keyword NORATE_FLAG
!   pet  2000.05.08 Added support of a qualifier NO for the keyword EVERY.
!                   Fixed a bug: variable KWVM was not initialized
!   pet  2000.11.22 Replaced DEFVELCNST with STAXYZ_CNST, STAUEN_CNST,
!                                            VELXYZ_CNST, VELUEN_CNST
!   pet  2001.01.29 Added check whether parameter TYPE is specified
!
!CCCCC
!
! 5.  GDATA PROGRAM STRUCTURE
!
! Initialize some things
!
      IDSRC=0
      IDSTA=0
      NDSRC=0
      NDSTA=0
      NXSEL=1
      ELDEF = 0.0
      NEL   = 0
      NEL_ARC = 0
      WVMDEF  = 0
      NWVM    =0
      CALL USE_GLBFIL_4 ( 'OR' )
!
! --- Turn off constraints on station positions and velocitites
!
      DO I = 1,3
         STAXYZ_CNST(I) = 0.0D0
         STAUEN_CNST(I) = 0.0D0
         VELXYZ_CNST(I) = 0.0D0
         VELUEN_CNST(I) = 0.0D0
      ENDDO
      DO ICT = 1,STA_BIT_WORDS
         STAXYZ_CNSB(ICT) = 0
         STAUEN_CNSB(ICT) = 0
         VELXYZ_CNSB(ICT) = 0
         VELUEN_CNSB(ICT) = 0
      END DO
      STAXYZ_CNFL = .FALSE.
      STAUEN_CNFL = .FALSE.
      VELXYZ_CNFL = .FALSE.
      VELUEN_CNFL = .FALSE.
!
      NORATE_FLAG = NORATE__DEF
      EVINT = 1
      EVSTART = 1
!
! --- Transforming DATYP__ABR from one condensed string to arrray of 6-symbols
! --- strings
!
      CALL LIB$MOVC3 ( DATYP__LEN, DATYP__ABR, DATYP__ABR__ARR )
!
! --- Read the first DATA record
!
      LENGTH=CFREAD(STRING)
      DO WHILE(STRING(1:1).EQ.' '.AND..NOT.CFEOF(IDUM))
         DO WHILE(TRIMLEN(STRING).GT.0)
            CALL SPLITSTRING(STRING,TOKEN,STRING )
!
            IF ( TOKEN .EQ. 'SOURCES' ) THEN
!
! ------------- 'SOURCES' Keyword
!
                 IF ( KSRC ) CALL FERR ( INT2(2010), &
     &               'Keyword SOURCES used twice', INT2(0), INT2(0) )
                 CALL GTLST ( TOKEN, STRING, KDSRC, IDSRC, NDSRC, IDELAR, NXSEL, &
     &                        MAX4_SELAR, ' ' )
                 KSRC=.TRUE.
          ELSE IF(TOKEN.EQ.'STATIONS') THEN
!
! ----------- 'STATIONS' Keyword
!
               IF ( KSTA ) CALL FERR ( INT2(2020), &
     &             'Keyword STATIONS used twice', INT2(0), INT2(0) )
               CALL GTLST ( TOKEN, STRING, KDSTA, IDSTA, NDSTA, IDELAR, NXSEL, &
     &                      MAX4_SELAR, ' ' )
               KSTA=.TRUE.
          ELSE IF ( TOKEN .EQ. 'TYPE' ) THEN
!
! ------------ 'TYPE' Keyword
!
               IF ( KTYP ) CALL FERR ( INT2(2030), 'Keyword TYPE used twice', &
     &              INT2(0), INT2(0) )
               CALL SPLITSTRING(STRING,TOKEN,STRING )
               IF ( TOKEN .EQ. 'GROUP_DELAYS_AND_RATES' ) THEN
                    DTYPE='GDR   '
                 ELSE IF ( TOKEN .EQ. 'GROUP_DELAYS_ONLY' ) THEN
                    DTYPE='GD    '
                 ELSE IF ( TOKEN .EQ. 'PHASE_DELAYS_AND_RATES' ) THEN
                    DTYPE='PDR   '
                 ELSE IF ( TOKEN .EQ. 'PHASE_DELAYS_ONLY' ) THEN
                    DTYPE='PD    '
                 ELSE
!
! ----------------- Search for the token in the table of supported
! ----------------- abbreviations of solution types to decide: does the token
! ----------------- legitimate here or not
!
                    IL = LTM_DIF ( 0, LAST__DTP-FIRST__DTP+1, DATYP__ABR__ARR, TOKEN(1:6) )
                    IF ( IL .LT. 1 ) THEN
                         CALL FERR ( INT2(2032), &
     &                       'BATCH(gdata): Illegal type '// &
     &                       'parameter for TYPE "'//TOKEN(1:22)//'" ', INT2(0), &
     &                        INT2(0) )
                    END IF
                    DTYPE = TOKEN
               END IF
               KTYP=.TRUE.
          ELSE IF ( TOKEN .EQ. 'NORATE_FLAG' ) THEN
!
! --------- NORATE_FLAG keyword
!
            IF ( KNRT ) CALL FERR ( INT2(2080), &
     &          'Keyword NORATE_FLAG used twice', INT2(0), INT2(0) )
            CALL SPLITSTRING(STRING,TOKEN,STRING )
            CALL TRAN ( 11, TOKEN, TOKEN )
            IF ( TOKEN .EQ. 'YES' ) THEN
                 NORATE_FLAG = .TRUE.
               ELSE IF ( TOKEN .EQ. 'ON' ) THEN
                 NORATE_FLAG = .TRUE.
               ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                 NORATE_FLAG = .FALSE.
               ELSE IF ( TOKEN .EQ. 'OFF' ) THEN
                 NORATE_FLAG = .FALSE.
               ELSE
                 CALL FERR ( INT2(2082), &
     &               'BATCH(gdata) Unrecognised value of the '// &
     &               'keyword NORATE_FLAG: '//TOKEN(1:16)// &
     &               ' YES or NO was expcted', INT2(0), INT2(0) )
                 STOP 'Abnormal termination'
            END IF
            KNRT=.TRUE.
          ELSE IF(TOKEN.EQ.'ELEVATION') THEN
!
! --------- 'ELEVATION' keyword
!
            IF(KELC) CALL FERR( INT2(2040), 'ELEVATION USED TWICE', INT2(0), &
     &         INT2(0) )
            CALL GTELEV(TOKEN,STRING )
            KELC=.TRUE.
          ELSE IF(TOKEN.EQ.'WVR_MASK') THEN
!
! --------- 'WVR_MASK' keyword
!
            IF ( KWVM ) CALL FERR ( INT2(2050), &
     &          'GDATA(BATCH) Keyword WVR_MASK '//'used twice', INT2(0), INT2(0) )
            CALL GTWVM ( TOKEN, STRING )
            KWVM = .TRUE.
          ELSE IF ( TOKEN .EQ. 'EVERY' ) THEN
!
! --------- 'EVERY' KEYWORD
!
            IF ( KEV ) CALL FERR ( INT2(2060), &
     &          'GDATA(BATCH): Keyword EVERY used '//'twice', INT2(0), INT2(0) )
            CALL SPLITSTRING ( STRING, TOKEN, STRING )
            IF ( TOKEN .EQ. 'NO'  .OR.  TOKEN .EQ. 'no' ) THEN
                 CONTINUE
               ELSE
                 EVINT = DECIMALTOINT ( TOKEN, ERR )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .NE. 'START' ) THEN
                      CALL FERR ( INT2(2070), &
     &                    '(GDATA) EVERY must be followed by '//'START', INT2(0), &
     &                     INT2(0) )
                 ENDIF
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 EVSTART = DECIMALTOINT ( TOKEN, ERR )
                 EVSTART = MOD ( EVSTART, EVINT )
            END IF
            KEV = .TRUE.
          ELSE IF ( TOKEN .EQ. 'BASELINES' ) THEN
!
! -------- 'BASELINES' KEYWORD
!
            IF ( KBAS ) CALL FERR ( INT2(6030), &
     &          'Keyword BASELINES used twice', INT2(0), INT2(0) )
            CALL SPLITSTRING(STRING,TOKEN,STRING )
            IF ( TOKEN .EQ. 'YES' ) THEN
                 BASDF='Y'
               ELSE IF(TOKEN.EQ.'NO') THEN
                 BASDF='N'
               ELSE
                 CALL FERR ( INT2(6032), 'Illegal baselines parameter '// &
     &                TOKEN(1:16), INT2(0), INT2(0) )
            ENDIF
            CALL GBASE ( TOKEN, STRING )
            KBAS=.TRUE.
          ELSE
!
! --------- Something that isn't suppose to be there
!
            CALL FERR ( INT2(2090), '(GDATA) Unknown keyword '//TOKEN(1:16), &
     &           INT2(0), INT2(0) )
          ENDIF
        ENDDO
        LENGTH=CFREAD(STRING)
      ENDDO
!
! --- Now that this section is finished, what now?
!
      IF ( .NOT. KSTA ) THEN
           CALL FERR ( INT2(2091), &
     &         '(Gdata) missing keyword STATIONS from $DATA', INT2(0), INT2(0) )
         ELSE IF ( .NOT. KSRC ) THEN
           CALL FERR ( INT2(2092), &
     &         '(Gdata) missing keyword SOURCES from $DATA', INT2(0), INT2(0) )
         ELSE IF ( .NOT. KTYP ) THEN
           CALL FERR ( INT2(2093), '(Gdata) missing keyword TYPE from $DATA', &
     &          INT2(0), INT2(0) )
         ELSE
           CALL CFUNRD(LENGTH,STRING )
      END IF
!
      CALL USE_GLBFIL_4 ( 'WC' )
      RETURN
      END  !#! GDATA  #!#
