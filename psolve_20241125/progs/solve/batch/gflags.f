      SUBROUTINE GFLAGS ( SOLTYP, STAFLG, FIXSTA_CHR, STADIU, VELFLG, SRCFLG, &
     &           FIXSRC_CHR, PROFLG, NUTFLG, UT1FLG, PRCFLG, RELFLG, ATMFLG, &
     &           INTRVL, CLKPOL_FLG, CLKPOL_DEG, CLKFLG, CKNTRVL, &
     &           FCNPR,  AXSFLG, OFFLG, RATFLG, ACCEOP_FLG, &
     &           IEOP_FLG, REOP_FLG, IEOPLL, &
     &           BLCFLG, IOS_EST_BATCH, EOPMID, KHFEOPEST, GRADFLG, GRINTRVL, &
     &           SIT_EST_EPOCH_VAL, SOU_EST_EPOCH_VAL, EOP_EPOCH_MJD, &
     &           EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI  )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GFLAGS PROGRAM SPECIFICATION
!
! 1.1 Parse FLAGS section of the control file.
!
! 1.2 REFERENCES:
!
! 2.  GFLAGS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'glbc3.i'
!
! 2.2 INPUT Variables: None
!
      CHARACTER  SOLTYP*1
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) STAFLG, STADIU, VELFLG, SRCFLG, PROFLG, NUTFLG(116), &
     &              UT1FLG, PRCFLG, RELFLG
      CHARACTER*(*) ATMFLG, CLKPOL_FLG, CLKFLG, AXSFLG
      CHARACTER*(*) BLCFLG, GRADFLG, FIXSTA_CHR*8, FIXSRC_CHR*8
      INTEGER*2 INTRVL, CLKPOL_DEG, CKNTRVL, &
     &          OFFLG, RATFLG, ACCEOP_FLG
      INTEGER*2 IEOP_FLG(6), IEOPLL, GRINTRVL, EOPMID, IOS_EST_BATCH
      REAL*8    REOP_FLG(4)
      REAL*8    FCNPR, SIT_EST_EPOCH_VAL, SOU_EST_EPOCH_VAL, &
     &          EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI  
      LOGICAL*2 KHFEOPEST
!
! ATMFLG     - Atmosphere flag
! AXSFLG     - Axis offset flag
! CLKPOL_FLG - Global polinomial clock flag
! CLKFLG     - Segmented clock flag
! CLKPOL_DEG - Maximim degree of global polinomial clocks
! CKNTRVL    - clock interval, minutes
! FIXSRC     - Reference source name
! FIXSTA   - Reference station name
! FCNPR    - Free core nutation period
! INTRVL   - Atmosphere interval, minutes
! NUTFLG   - Nutation flag
! PRCFLG   - Precession flag
! RELFLG   - Relativity flag
! SRCFLG   - Source coordinates flag
! PROFLG   - Source proper motion flag
! STAFLG   - Station flag
! VELFLG   - Station velosity flag
! STADIU   - Station diurnal flag
! UT1FLG   - Earth orientation flag
! offlg    - Earth orientation offset flag
! ratflg   - Earth orientation rate flag
! ACCEOP_FLG - Earth orientation acceleration flag.
! ieop_flg - Local earth orientation flags
! reop_flg - Earth orientation intervals, constraints
! ieopll   - Earth orientation plot flag
! SIT_EST_EPOCH_VAL -- reference epoch for site position estimation
! SOU_EST_EPOCH_VAL -- reference epoch for source position estimation
! EOP_EPOCH_MJD     -- reference epoch for EOP estimation (MJD part)
! EOP_EPOCH_SEC     -- reference epoch for EOP estimation (SEC part)
! IOS_EST_BATCH     -- flags of estimation of ioniosphere path delay scale
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'axscm.i'
      INCLUDE 'batme.i'
      INCLUDE 'dmapp.i'
      INCLUDE 'precm.i'
      INCLUDE 'exceptions.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrlfl
!       CALLED  SUBROUTINES: cfread, splitstring, gnut, gut1, gaxis,cfunrd,gex
! 3.  LOCAL VARIABLES
!
      CHARACTER CFIX*8, STRING*256, TOKEN*256, STR*32, DEFAULT_DATE*19
      CHARACTER  FNAME*128
      DATA DEFAULT_DATE / '2000.01.01_00:00:00' /
      INTEGER*4  SIZE_HPE, SIZE_SPE
      INTEGER*2 LENGTH, IDUM, IFX(4), IERR
      LOGICAL*2 KSTA, KSRC, KNUT, KUT1, KPRC, KREL, KTID, KVEL, KATM, KCLK, &
     &          KAXS, KPROP, KBLC, KHF, KGRD, KIOS, FL_EXCEPT, FL_GETNEXT, &
     &          FL_PRP_SEL, FL_VEL_SEL, FL_SIT_EST_EPOCH, FL_SOU_EST_EPOCH, &
     &          FL_SPE, FL_HPE, FL_ERM, FL_HEO, FL_SOUADM
      LOGICAL*4 LEX
      INTEGER*4 MJD, EOP_EPOCH_MJD, IUER
      REAL*8    SEC, EOP_EPOCH_SEC 
!
      REAL*8     MJD_SEC_TO_JD
      CHARACTER, EXTERNAL :: JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
!  EXTERNAL FUNCTIONS
!
      INTEGER*2 CFREAD, TRIMLEN, DECIMALTOINT
      LOGICAL*2 CFEOF
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910419 Added proper_motions stuff.
!   PET   970515 Improved comments and error mesasges. Changed syntax for CLOCK
!   PET   970902 Added ACCEOP_FLG support
!   jwr   971018 Old code for suppressing bl-dep clocks turned back on.
!   pet   1999.11.19  Added a stop for the case when a user specifed keyword
!                     TIDES which is not supported any more.
!   pet   2000.05.03  Added support of qualifiers IN in the keywrds ATMOSPHERE,
!                     CLOCKS. Corrected a bug related with parsing CLOCKS
!                     keyword.
!   pet   2000.10.05  Corrected a bug: EOPMID should be of INTEGER*2 type!
!
!   pet   2001.08.10  substantially schanged the logic of parsing keywords
!                     SOURCES, PROPER_MOTIONS, STATIONS and VELOCITIES
!   pet   2002.12.24  Added support of qualisifer REF_EPOCH for keywods
!                     STATIONS and SOURCES
!   pet   2006.06.25  Added support of keywords EHEO, ERM
!   pet   2007.05.14  Fixed a bug: the previous version strored the reference epoch
!                     as TAI, but the new version stores as TDT
!   pet   2007.08.09  Added support of flags for source structure admittance
!   pet   2008.04.09  Added support of NUT_USE_CODE  flag
!   pet   2022.08.22  Added support of IOS_EST_BATCH flag
!
! 5.  GFLAGS PROGRAM STRUCTURE
!
! MEMORY MANIPULATION
!
      EQUIVALENCE (CFIX,IFX)
!
!  INITIALIZE
!
      DATA KSTA/.FALSE./, KSRC /.FALSE./, KNUT/.FALSE./
      DATA KUT1/.FALSE./, KPRC/.FALSE./,KREL/.FALSE./,KAXS/.FALSE./
      DATA KTID/.FALSE./, KVEL/.FALSE./,KATM/.FALSE./,KCLK/.FALSE./
      DATA KPROP/.FALSE./,kblc/.FALSE./,khf/.FALSE./, kgrd/.FALSE./, &
     &     KIOS / .FALSE. /
!
      CALL USE_GLBFIL_4 ( 'OR' )
      CALL USE_GLBFIL_3 ( 'OR' )
!
! --- Initialization
!
      HFEOPEST  = ' '
      KHFEOPEST = .FALSE.
      AXSFLG    = 'N'
      NATMEX    = 0
      STAFLG    = ' '
      STADIU    = ' '
      VELFLG    = ' '
      SRCFLG    = ' '
      PROFLG    = ' '
!
      FL_SIT_EST_EPOCH = .FALSE.
      FL_SOU_EST_EPOCH = .FALSE.
      FL_PRP_SEL = .FALSE.
      FL_VEL_SEL = .FALSE.
      FL_HPE = .FALSE.
      FL_SPE = .FALSE.
      FL_ERM = .FALSE.
      FL_HEO = .FALSE.
      FL_SOUADM = .FALSE.
      L_HPE    = 0
      L_SPE    = 0
      L_EERM   = 0
      L_EHEO   = 0
      L_EHEC   = 0
      L_AEM    = 0
      ADR_HPE  = 0
      ADR_SPE  = 0
      ADR_EERM = 0
      ADR_EHEO = 0
      ADR_EHEC = 0
      ADR_EHES = 0
      ADR_AEM  = 0
      SIZE_HPE = 0
      SIZE_SPE = 0
      MJD_EHEO_REF = 0
      TAI_EHEO_REF = 0.0D0
      SOU_ADM_FLAG = SOUADM__NO
!
      SIT_EST_EPOCH_VAL = SOLVE_REF_EPOCH__JD
      SOU_EST_EPOCH_VAL = J2000__JD
      NUT_USE_CODE = 0
!
      NUMEXC_STA = 0
      NUMEXC_VEL = 0
      NUMEXC_SOU = 0
      NUMEXC_VEL = 0
      NUM_AXIS   = 0
      IND_SOU_ADM(1) = 0
      IND_SOU_ADM(2) = 0
      IOS_EST_BATCH  = IOS__UNDF
!
      CALL CLRCH ( STA_CMP_ALL )
      CALL CLRCH ( VEL_CMP_ALL )
      CALL CLRCH ( SOU_CMP_ALL )
      CALL CLRCH ( PRO_CMP_ALL )
      CALL CLRCH ( FIXSTA_CHR  )
      CALL CLRCH ( FIXSRC_CHR  )
!
! --- Remove stale estimation files, if they exist
!
      FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'EHPE'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST=LEX )
      IF ( LEX ) THEN
           CALL UNLINK ( FNAME(1:I_LEN(FNAME))//CHAR(0) )
      END IF
!
      FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'ESPE'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST=LEX )
      IF ( LEX ) THEN
           CALL UNLINK ( FNAME(1:I_LEN(FNAME))//CHAR(0) )
      END IF
!
      FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'EERM'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST=LEX )
      IF ( LEX ) THEN
           CALL UNLINK ( FNAME(1:I_LEN(FNAME))//CHAR(0) )
      END IF
!
      FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'EHEO'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST=LEX )
      IF ( LEX ) THEN
           CALL UNLINK ( FNAME(1:I_LEN(FNAME))//CHAR(0) )
      END IF
!
      FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'EHEC'//PRE_LETRS
      INQUIRE ( FILE=FNAME, EXIST=LEX )
      IF ( LEX ) THEN
           CALL UNLINK ( FNAME(1:I_LEN(FNAME))//CHAR(0) )
      END IF
!
! --- Nor read the line and ...
!
      LENGTH=CFREAD(STRING)
!
! --- Parse control file line by line
!
      DO WHILE ( STRING(1:1) .EQ. ' '   .AND. .NOT. CFEOF(IDUM) )
         DO WHILE ( TRIMLEN(STRING) .GT. 0 )
            CALL SPLITSTRING ( STRING, TOKEN, STRING )
            IF ( TOKEN .EQ. 'SOURCES' ) THEN
!
! -------------- 'SOURCES' keyword
!
                 IF ( KSRC ) CALL FERR ( INT2(4010), &
     &               'BATCH(gflags) Keyword '//'SOURCES used twice', INT2(0), &
     &                INT2(0) )
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 CALL TRAN ( 11, TOKEN, TOKEN ) ! Translation to capital lett.
                 IF ( TOKEN .EQ. 'YES' ) THEN
                      SRCFLG = 'Y'
                   ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      SRCFLG = 'N'
                   ELSE IF ( TOKEN .EQ. 'IN' ) THEN
                      SRCFLG = 'I'
                   ELSE
                      CALL FERR ( INT2(4012), &
     &                    'BATCH(gflags) Illegal SOURCES '//'qualifier '// &
     &                     TOKEN(1:16), INT2(0), INT2(0) )
                 ENDIF
!
! -------------- Get the next token
!
                 CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
                 IF ( TOKEN .EQ. 'REF_EPOCH' ) THEN
!
! ------------------- Parse the qualifier REF_EPOCH. First get the value
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( ILEN(TOKEN) .LT. 9 ) THEN
                           CALL ERR_LOG ( 8621, -2, 'GFLAGS', &
     &                         'Error in parsing keyword SOURCES in $FLAGS '// &
     &                         'section: too short station position '// &
     &                         'reference epoch qualifier: '// &
     &                          TOKEN(1:ILEN(TOKEN))//' -- the date in format YYYY.MM.DD_hh:mm:ss '// &
     &                         'was expected' )
                           STOP 'BATCH: Abnormal termination'
                        ELSE IF ( ILEN(TOKEN) .LT. LEN(DEFAULT_DATE) ) THEN
                           TOKEN = TOKEN(1:ILEN(TOKEN))// &
     &                             DEFAULT_DATE(ILEN(TOKEN)+1:)
                      END IF
!
! ------------------- Then parse the date
!
                      IUER = -1
                      CALL DATE_TO_TIME ( TOKEN, MJD, SEC, IUER )
                      IF ( IUER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8622, -2, 'GFLAGS', &
     &                         'Error in parsing keyword SOURCES in $FLAGS '// &
     &                         'section: wrong station position reference '// &
     &                         'epoch qualifier: '//TOKEN(1:ILEN(TOKEN))// &
     &                         ' -- the date in format YYYY.MM.DD_hh:mm:ss '// &
     &                         'was expected' )
                           STOP 'BATCH: Abnormal termination'
                      END IF
                      SOU_EST_EPOCH_VAL = MJD_SEC_TO_JD ( MJD, SEC + 32.184D0 )
!
! ------------------- Get the next token
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
                      FL_SOU_EST_EPOCH = .TRUE.
                 END IF
!
                 FIXSRC_CHR = '        '
                 FL_EXCEPT  = .FALSE.
                 FL_GETNEXT = .FALSE.
!
                 IF ( TOKEN .EQ. 'RD' ) THEN
                      SOU_CMP_ALL = TOKEN
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                    ELSE IF ( TOKEN .EQ. 'R-' ) THEN
                      SOU_CMP_ALL = TOKEN
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                    ELSE IF ( TOKEN .EQ. '-D' ) THEN
                      SOU_CMP_ALL = TOKEN
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                    ELSE IF ( TOKEN .EQ. '--' ) THEN
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      SOU_CMP_ALL = TOKEN
                 END IF
!
                 IF ( TOKEN .EQ. 'PICK' ) THEN
                      FIXSRC_CHR = 'PICK    '
                    ELSE IF ( TOKEN .EQ. 'NONE' ) THEN
                      CONTINUE
                    ELSE IF ( TOKEN .EQ. 'EXCEPT' ) THEN
                      SOU_CMP_ALL = 'RD'
                      FL_EXCEPT  = .TRUE.
                    ELSE IF ( TOKEN(1:1) .EQ. ' ' ) THEN
                      CONTINUE
                    ELSE
                      FIXSRC_CHR = TOKEN
                 END IF
!
                 IF ( FL_GETNEXT ) THEN
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN .NE. 'EXCEPT' ) THEN
                           CALL FERR ( INT2(4014), 'BATCH(gflags) Illegal '// &
     &                         'qualifier after keyword SOURCE:  '//TOKEN(1:16)// &
     &                         '  EXCEPT '//'was expected', INT2(0), INT2(0) )
                           STOP 'BATCH Abnormal termination'
                      END IF
                      FL_EXCEPT = .TRUE.
                 END IF
!
                 IF ( SOU_CMP_ALL .EQ. '  ' ) SOU_CMP_ALL = 'RD'
                 IF ( FL_EXCEPT ) THEN
                      IUER = -1
                      CALL GEXP ( 'SOURCES', SOU_CMP_ALL, STRING, IUER )
                      IF ( IUER .NE. 0 ) THEN
                           CALL FERR ( INT2(4016), &
     &                         'BATCH(gflags) Error in parsing '// &
     &                         'exception list of the keyword SOURCE', INT2(0), &
     &                          INT2(0) )
                           STOP 'BATCH Abnormal termination'
                      END IF
                 END IF
                 KSRC=.TRUE.
             ELSE IF ( TOKEN .EQ. 'PROPER_MOTIONS' ) THEN
!
! ------------- 'PROPER_MOTIONS' keyword
!
               IF ( KPROP ) CALL FERR ( INT2(4100), 'BATCH(gflags) Keyword '// &
     &             'PROPER_MOTIONS used twice', INT2(0), INT2(0) )
               IF ( .NOT. KSRC ) THEN
                    CALL FERR ( INT2(4102), 'BATCH(gflags) Keyword SOURCES '// &
     &                  'must preceed keyword PROPER_MOTIONS', INT2(0), INT2(0) )
               ENDIF
               CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
               IF ( TOKEN .EQ. 'YES' ) THEN
                    PROFLG = 'Y'
                 ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                    PROFLG = 'N'
                 ELSE
                    CALL FERR ( INT2(4104), 'BATCH(gflags) qualifier '// &
     &                   TOKEN(1:16)//' after keyword PROPER_MOTIONS: one of '// &
     &                  'YES or NO were expceted', INT2(0), INT2(0) )
                    STOP 'BATCH Abnormal termination'
               ENDIF
!
! ------------ Get the new token
!
               CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
               FL_EXCEPT  = .FALSE.
               FL_GETNEXT = .FALSE.
               IF ( TOKEN .EQ. 'RD' ) THEN
                    PRO_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. 'R-' ) THEN
                    PRO_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. '-D' ) THEN
                    PRO_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. '--' ) THEN
                    PRO_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. 'EXCEPT' ) THEN
                    PRO_CMP_ALL = 'RD'
                    FL_EXCEPT = .TRUE.
                  ELSE IF ( TOKEN(1:1) .EQ. ' ' ) THEN
                    CONTINUE
                  ELSE
                    CALL FERR ( INT2(4106), 'BATCH(gflags) Wrong qualifier '// &
     &                   TOKEN(1:16)//' after keyword PROPER_MOTIONS: '// &
     &                  'one of EXCEPT RD R- -D -- were expected', INT2(0), &
     &                   INT2(0) )
                    STOP 'BATCH Abnormal termination'
               END IF
!
               IF ( FL_GETNEXT ) THEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                    IF ( TOKEN(1:1) .NE. ' '  .AND.  TOKEN .NE. 'EXCEPT' ) THEN
                         CALL FERR ( INT2(4108), &
     &                       'BATCH(gflags) Illegal qualifier '// &
     &                       'after keyword PROPER_MOTIONS:  '//TOKEN(1:16)// &
     &                       '  EXCEPT was expected', INT2(0), INT2(0) )
                         STOP 'BATCH Abnormal termination'
                    END IF
                    FL_EXCEPT = .TRUE.
               END IF
!
               IF ( PRO_CMP_ALL .EQ. '  ' ) PRO_CMP_ALL = 'RD'
               IF ( FL_EXCEPT ) THEN
                    IUER = -1
                    CALL GEXP ( 'PROPER_MOTIONS', PRO_CMP_ALL, STRING, IUER )
                    IF ( IUER .NE. 0 ) THEN
                         CALL FERR ( INT2(4109), &
     &                       'BATCH(gflags) Error in parsing '// &
     &                       'exception list of the keyword PROPER_MOTIONS', &
     &                        INT2(0), INT2(0) )
                         STOP 'BATCH Abnormal termination'
                    END IF
               END IF
!
               IF ( PROFLG .EQ. 'N'  .AND.  .NOT. FL_EXCEPT ) THEN
                    FL_PRP_SEL = .FALSE.
                  ELSE
                    FL_PRP_SEL = .TRUE.
               END IF
!
               KPROP=.TRUE.
            ELSE IF ( TOKEN .EQ. 'STATIONS' ) THEN
!
! ------------ 'STATIONS' keyword
!
               IF ( KSTA ) CALL FERR ( INT2(4020), &
     &             'BATCH(gflags) Stations used '//'twice', INT2(0), INT2(0) )
               CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
               IF ( TOKEN .EQ. 'YES' ) THEN
                    STAFLG = 'Y'
                  ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                    STAFLG = 'N'
                  ELSE
                    CALL FERR ( INT2(4022), &
     &                  'BATCH(gflags) Illegal qualifier '// &
     &                  'after keyword STATIONS '//TOKEN(1:16)// &
     &                  ' one or YES or NO was expected', INT2(0), INT2(0) )
                    STOP 'BATCH Abnormal termination'
               ENDIF
!
               CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
               IF ( TOKEN .EQ. 'REF_EPOCH' ) THEN
!
! ----------------- Parse the qualifier REF_EPOCH. First get the value
!
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                    IF ( ILEN(TOKEN) .LT. 4 ) THEN
                         CALL ERR_LOG ( 8623, -2, 'GFLAGS', &
     &                       'Error in parsing keyword STATIONS in $FLAGS '// &
     &                       'section: too short station position reference '// &
     &                       'epoch qualifier: '//TOKEN(1:ILEN(TOKEN))// &
     &                       ' -- the date in format YYYY.MM.DD_hh:mm:ss '// &
     &                       'was expected' )
                         STOP 'BATCH: Abnormal termination'
                       ELSE IF ( ILEN(TOKEN) .LT. LEN(DEFAULT_DATE) ) THEN
                         TOKEN = TOKEN(1:ILEN(TOKEN))// &
     &                           DEFAULT_DATE(ILEN(TOKEN)+1:)
                    END IF
!
! ----------------- Then parse the date
!
                    IUER = -1
                    CALL DATE_TO_TIME ( TOKEN, MJD, SEC, IUER )
                    IF ( IUER .NE. 0 ) THEN
                         CALL ERR_LOG ( 8624, -2, 'GFLAGS', &
     &                       'Error in parsing keyword STATIONS in $FLAGS '// &
     &                       'section: wrong station position reference '// &
     &                       'epoch qualifier: '//TOKEN(1:ILEN(TOKEN))// &
     &                       ' -- the date in format YYYY.MM.DD_hh:mm:ss '// &
     &                       'was expected' )
                         STOP 'BATCH: Abnormal termination'
                    END IF
                    SIT_EST_EPOCH_VAL = MJD_SEC_TO_JD ( MJD, SEC + 32.184D0 )
!
! ----------------- Get the next token
!
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
                    FL_SIT_EST_EPOCH = .TRUE.
               END IF
!
               IF ( TOKEN .EQ. 'XYZ' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. '-YZ' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. 'X-Z' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. 'XY-' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. '--Z' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. '-Y-' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. 'X--' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. '---' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. 'UEN' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. '-EN' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. 'U-N' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. 'UE-' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. '--N' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. '-E-' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. 'U--' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN .EQ. '---' ) THEN
                    STA_CMP_ALL = TOKEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
               END IF
!
               IF ( TOKEN .EQ. 'D' ) THEN
                    STADIU = 'D'
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
               END IF
!
               FL_EXCEPT  = .FALSE.
               IF ( TOKEN .EQ. 'PICK' ) THEN
                    FIXSTA_CHR = 'PICK    '
                  ELSE IF ( TOKEN .EQ. 'NONE' ) THEN
                    CONTINUE
                  ELSE IF ( TOKEN .EQ. 'EXCEPT' ) THEN
                    FL_EXCEPT = .TRUE.
                  ELSE
                    FIXSTA_CHR = TOKEN
!@U                    CALL UNDSCR ( FIXSTA_CHR  )
               END IF
!
               IF ( STA_CMP_ALL .EQ. '   ' ) STA_CMP_ALL = 'XYZ'
               IF ( FL_EXCEPT ) THEN
                    IUER = -1
                    CALL GEXP ( 'STATIONS', STA_CMP_ALL, STRING, IUER )
                    IF ( IUER .NE. 0 ) THEN
                         CALL FERR ( INT2(4024), &
     &                       'BATCH(gflags) Error in parsing '// &
     &                       'exception list of the keyword STATIONS', INT2(0), &
     &                        INT2(0) )
                         STOP 'BATCH Abnormal termination'
                    END IF
                END IF
                KSTA=.TRUE.
             ELSE IF ( TOKEN .EQ. 'VELOCITIES' ) THEN
!
! ----------- 'VELOCITIES' KEYWORD
!
               IF ( KVEL ) CALL FERR ( INT2(4080), 'BATCH(gflags) Keyword '// &
     &             'VELOCITIES used twice', INT2(0), INT2(0) )
               IF ( .NOT. KSTA ) THEN
                    CALL FERR ( INT2(4082), &
     &                  'BATCH(gflags) Keyword STATIONS must '// &
     &                  'preceed keyword VELOCITIES', INT2(0), INT2(0) )
               ENDIF
               CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
               IF ( TOKEN .EQ. 'YES' ) THEN
                    VELFLG = 'Y'
                 ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                    VELFLG = 'N'
                 ELSE
                    CALL FERR ( INT2(4084), 'BATCH(gflags) qualifier '// &
     &                   TOKEN(1:16)//' after keyword VELOCITIES: one of '// &
     &                  'YES or NO were expected', INT2(0), INT2(0) )
                    STOP 'BATCH Abnormal termination'
               ENDIF
!
! ------------ Get the new token
!
               CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
!
               FL_EXCEPT  = .FALSE.
               FL_GETNEXT = .FALSE.
               IF ( TOKEN .EQ. 'XYZ' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. '-YZ' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. 'X-Z' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. 'XY-' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. '--Z' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. '-Y-' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. 'X--' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. '---' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. 'UEN' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. '-EN' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. 'U-N' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. 'UE-' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. '--N' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. '-E-' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. 'U--' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. '---' ) THEN
                    VEL_CMP_ALL = TOKEN
                    FL_GETNEXT = .TRUE.
                  ELSE IF ( TOKEN .EQ. 'EXCEPT' ) THEN
                    FL_EXCEPT = .TRUE.
                  ELSE IF ( TOKEN(1:1) .EQ. ' ' ) THEN
                    CONTINUE
                  ELSE
                    CALL FERR ( INT2(4086), 'BATCH(gflags) wrong qualifier '// &
     &                   TOKEN(1:16)//' after keyword VELOCITIES: '// &
     &                  'station component or EXCEPT were expected', INT2(0), &
     &                   INT2(0) )
                    STOP 'BATCH Abnormal termination'
               END IF
!
               IF ( FL_GETNEXT ) THEN
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                    IF ( TOKEN(1:1) .NE. ' '  .AND.  TOKEN .NE. 'EXCEPT' ) THEN
                         CALL FERR ( INT2(4088), &
     &                       'BATCH(gflags) Illegal qualifier '// &
     &                       'after keyword VELOCITIES:  '//TOKEN(1:16)// &
     &                       '  EXCEPT was expected', INT2(0), INT2(0) )
                         STOP 'BATCH Abnormal termination'
                    END IF
                    FL_EXCEPT = .TRUE.
               END IF
!
               IF ( VEL_CMP_ALL .EQ. '   ' ) VEL_CMP_ALL = 'XYZ'
               IF ( FL_EXCEPT ) THEN
                    IUER = -1
                    CALL GEXP ( 'VELOCITIES', VEL_CMP_ALL, STRING, IUER )
                    IF ( IUER .NE. 0 ) THEN
                         CALL FERR ( INT2(4089), &
     &                       'BATCH(gflags) Error in parsing '// &
     &                       'exception list of the keyword VELOCITIES', INT2(0), &
     &                        INT2(0) )
                         STOP 'BATCH Abnormal termination'
                    END IF
               END IF
!
               IF ( VELFLG .EQ. 'N'  .AND.  .NOT. FL_EXCEPT ) THEN
                    FL_VEL_SEL = .FALSE.
                  ELSE
                    FL_VEL_SEL = .TRUE.
               END IF
               KVEL=.TRUE.
             ELSE IF ( TOKEN .EQ. 'HARMONIC_POS' ) THEN
                IF ( FL_HPE ) THEN
                     CALL FERR ( INT2(4040), 'BATCH(gflags) Keyword '// &
     &                   'HARMONIC_POS used twice', INT2(0), INT2(0) )
                END IF                
!
                IUER = -1 
                CALL PARSE_HPE ( STRING, L_HPE, ADR_HPE, SIZE_HPE, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8625, -2, 'GFLAGS', 'Error in parsing '// &
     &                   'the keyword HARMONIC_POS' )
                     WRITE ( 6, * ) 'BATCH: abnormal termination'
                     CALL EXIT ( 1 ) 
                END IF
                FL_HPE = .TRUE.
             ELSE IF ( TOKEN .EQ. 'SPLINE_POS' ) THEN
                IF ( FL_SPE ) THEN
                     CALL FERR ( INT2(4040), 'BATCH(gflags) Keyword '// &
     &                   'SPLINE_POS used twice', INT2(0), INT2(0) )
                END IF                
!
                IUER = -1 
                CALL PARSE_SPE ( STRING, L_SPE, ADR_SPE, SIZE_SPE, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8626, -2, 'GFLAGS', 'Error in parsing '// &
     &                   'the keyword SPLINE_POS' )
                     WRITE ( 6, * ) 'BATCH: abnormal termination'
                     CALL EXIT ( 1 ) 
                END IF
!
                FL_SPE = .TRUE.
             ELSE IF ( TOKEN .EQ. 'NUTATION' ) THEN
!
! ------------- 'NUTATION' keyword
!
                 IF ( KNUT ) CALL FERR ( INT2(4030), &
     &               'BATCH(gflags) keyword NUTATION used twice', INT2(0), &
     &                INT2(0) )
                 CALL GNUT ( NUTFLG, TOKEN, STRING, FCNPR, 'FLAG' )
                 KNUT=.TRUE.
              ELSE IF ( TOKEN .EQ. 'UT1/PM' ) THEN
!
! -------------- 'UT1/PM' KEYWORD
!
                IF ( KUT1 ) CALL FERR ( INT2(4040), &
     &              'BATCH(gflags) Keyword UT1/PM used twice', INT2(0), &
     &               INT2(0))
                CALL GUT1 ( UT1FLG, OFFLG, RATFLG, ACCEOP_FLG, TOKEN, &
     &                      STRING, IEOP_FLG, REOP_FLG, IEOPLL, EOPMID, &
     &                      EOP_EPOCH_MJD, EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, &
     &                      EOP_AFTER_SEC_TAI )
                KUT1=.TRUE.
              ELSE IF ( TOKEN.EQ.'PRECESSION' ) THEN
!
! -------------- 'PRECESSION' keyword
!
                IF ( KPRC ) CALL FERR ( INT2(4050), 'BATCH(gflags) Keyword '// &
     &              'PRECESSION used twice', INT2(0), INT2(0) )
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                CALL TRAN ( 11, TOKEN, TOKEN ) ! Translation to capital lett.
                IF ( TOKEN .EQ. 'YES' ) THEN
                     PRCFLG = 'Y'
                   ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                     PRCFLG = 'N'
                   ELSE
                     CALL FERR ( INT2(4052), &
     &                   'BATCH(gflags) Illegal qualifier '// &
     &                   'after keyword  PRECESSION '//TOKEN(1:16), INT2(0), &
     &                    INT2(0) )
                ENDIF
                KPRC=.TRUE.
              ELSE IF ( TOKEN .EQ. 'RELATIVITY' ) THEN
!
! ----------- 'RELATIVITY' keyword
!
                IF ( KREL ) CALL FERR ( INT2(4060), 'BATCH(gflags) Keyword '// &
     &              'RELATIVITY used twice', INT2(0), INT2(0) )
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                CALL TRAN ( 11, TOKEN, TOKEN ) ! Translation to capital lett.
                IF ( TOKEN .EQ. 'YES' ) THEN
                     RELFLG = 'Y'
                  ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                     RELFLG = 'N'
                  ELSE
                     CALL FERR ( INT2(4062), &
     &                   'BATCH(gflags) illegal RELATIVITY '//'parameter '// &
     &                    TOKEN(1:16), INT2(0), INT2(0) )
                ENDIF
                KREL=.TRUE.
              ELSE IF ( TOKEN .EQ. 'TIDES' ) THEN
!
! ------------ 'TIDES' keyword -- already obsolete
!
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                IF ( TOKEN(1:2) .NE. 'NO' ) THEN
                     CALL FERR ( INT2(4070), &
     &                   'BATCH(gflags): Keyword TIDES is not '// &
     &                   'supported any more. You can estimate Love '// &
     &                   'numbers in user partials mode', INT2(0), INT2(0) )
                     STOP 'BATCH(gflags)'
                END IF
!                IF ( KTID ) CALL FERR ( 4070, 'Tides used twice', 0, 0 )
!                CALL SPLITSTRING ( STRING, TOKEN, STRING )
!                CALL TRAN ( INT4(11), TOKEN, TOKEN ) ! Translation to capital lett.
!                IF ( TOKEN .EQ. 'GLOBAL' ) THEN
!                     TIDFLG='G'
!                  ELSE IF(TOKEN.EQ.'LOCAL') THEN
!                     TIDFLG='L'
!                  ELSE IF(TOKEN.EQ.'NO') THEN
!                     TIDFLG='N'
!                  ELSE
!                     CALL FERR ( 4072, 'Illegal TIDES parameter '//
!     #               TOKEN(1:16), 0, 0 )
!                ENDIF
!                KTID=.TRUE.
              ELSE IF ( TOKEN .EQ. 'HI_FREQ_EOP' ) THEN
!
! ------------- 'HI_FREQ_EOP' keyword
!
                IF ( KHF ) CALL FERR ( INT2(4090), 'BATCH(gflags) keyword '// &
     &              'HI_FREQ_EOP used twice', INT2(0), INT2(0) )
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                CALL TRAN ( 11, TOKEN, TOKEN ) ! Translation to capital lett.
                IF ( TOKEN(1:3) .EQ. 'YES' ) THEN
                     CALL SPLITSTRING ( STRING, TOKEN, STRING )
                     IF ( TOKEN .EQ. ' ' ) THEN
                          CALL FERR ( INT2(4092), 'BATCH(gflags) missing '// &
     &                        'HI_FREQ_EOP file name', INT2(0), INT2(0) )
                     ENDIF
                     KHFEOPEST = .TRUE.
                ENDIF
!
! ------------- Set name of this mapping file
!
                HFEOPEST = TOKEN
                KHF = .TRUE.
            ELSE IF ( TOKEN .EQ. 'ATMOSPHERES' ) THEN
!
! ------------ 'ATMOSPHERES' keyword
!
               INTRVL = 0
               IF ( KATM ) CALL FERR ( INT2(4110), 'BATCH(gflags) Keyword '// &
     &             'ATMOSPHERES used twice', INT2(0), INT2(0) )
               CALL SPLITSTRING ( STRING, TOKEN, STRING )
               CALL TRAN ( 11, TOKEN, TOKEN ) ! Translation to capital lett.
               IF ( TOKEN .EQ. 'IN' ) THEN
                    ATMFLG = 'D'
                 ELSE IF ( TOKEN .EQ. 'DEFAULT' ) THEN
                    ATMFLG = 'D'
                 ELSE IF ( TOKEN .EQ. 'FORCE' ) THEN
                    ATMFLG='F'
                 ELSE IF ( TOKEN .EQ. 'NO'    ) THEN
                    ATMFLG='N'
                 ELSE IF ( TOKEN .EQ. 'AUTO' .OR. TOKEN .EQ. 'MOST' ) THEN
                    ATMFLG=TOKEN(1:1)
                    CALL SPLITSTRING ( STRING, TOKEN, STRING )
                    INTRVL = DECIMALTOINT ( TOKEN, IERR )
                    CALL FERR ( IERR, 'BATCH(gflags) Invalid atmosphere '// &
     &                  'interval specification: '//TOKEN(1:16), INT2(0), INT2(0) )
                    IF ( ATMFLG .EQ. 'A' ) CALL ATMEXC ( TOKEN, STRING )
                 ELSE
                    CALL FERR ( INT2(4112), &
     &                  'GFLAGS(BATCH) Unknown qualifiers '// &
     &                  'of the keyword ATMOSPHERE '//TOKEN(1:16)// &
     &                  ' one of IN, FORCE, NO, AUTO, MOST or DEFAULT were '// &
     &                  'expected', INT2(0), INT2(0) )
               ENDIF
               KATM=.TRUE.
             ELSE IF ( TOKEN .EQ. 'GRADIENTS' ) THEN
!
! ------------ 'GRADIENTS' keyword
!
               GRINTRVL = 0
               IF ( KGRD ) CALL FERR ( INT2(4120), 'BATCH(gflags) Keyword '// &
     &             'GRADIENTS used twice', INT2(0), INT2(0) )
               CALL SPLITSTRING ( STRING, TOKEN, STRING )
               CALL TRAN ( 11, TOKEN, TOKEN ) ! Translation to capital lett.
               IF ( TOKEN .EQ. 'YES' ) THEN
                    GRADFLG = 'Y'
                    CALL SPLITSTRING ( STRING, TOKEN, STRING )
                    GRINTRVL = DECIMALTOINT ( TOKEN, IERR )*60
                    CALL FERR ( IERR, 'Invalid gradient interval '// &
     &                  'specification: '//TOKEN(1:16), INT2(0), INT2(0) )
                 ELSE IF (TOKEN .EQ. 'NO') THEN
                    GRADFLG = 'N'
                    CALL SPLITSTRING ( STRING, TOKEN, STRING )
                    IF ( ILEN(TOKEN) > 0 ) THEN
                         GRINTRVL = DECIMALTOINT ( TOKEN, IERR )*60
                         CALL FERR ( IERR, 'Invalid gradient interval '// &
     &                       'specification: '//TOKEN(1:16), INT2(0), INT2(0) )
                    END IF
                 ELSE
                    CALL FERR ( INT2(4122), &
     &                  'BATCH(gflags) Unknown GRADIENT '//'parameter '// &
     &                   TOKEN(1:16), INT2(0), INT2(0) )
               ENDIF
               CALL GRADEXC ( TOKEN, STRING, GRINTRVL, GRADFLG )
               KGRD=.TRUE.
             ELSE IF ( TOKEN .EQ. 'CLOCKS' ) THEN
!
! ------------ 'CLOCKS' keyword
!
               CKNTRVL=0
               IF ( KCLK ) CALL FERR ( INT2(4130), &
     &             'BATCH(gflags) Keyword CLOCKS '//'used twice', INT2(0), &
     &              INT2(0))
               CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
 910           CONTINUE
               IF ( TOKEN .EQ. 'DEFAULT' ) THEN
!
! ----------------- Obsolete syntax
!
                    CLKFLG = 'D'
                    CLKPOL_FLG  = 'D'
                    CLKPOL_DEG = 2
                 ELSE IF ( TOKEN .EQ. 'FORCE'  .OR.  TOKEN .EQ. 'AUTO'  .OR. &
     &                     TOKEN .EQ. 'PICK'   .OR.  TOKEN .EQ. 'MOST'  ) THEN
!
! ----------------- Obsolete syntax
!
                    CLKFLG = TOKEN(1:1)
                    CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                    CKNTRVL = DECIMALTOINT ( TOKEN, IERR )
                    CALL FERR ( IERR, &
     &                  'BATCH(gflags) Invalid clock interval '// &
     &                  'specification: '//TOKEN(1:16), INT2(0), INT2(0) )
                    CLKPOL_DEG =  2
                    CLKPOL_FLG  = 'D'
                 ELSE IF ( TOKEN(1:3) .EQ. 'MAX' ) THEN
!
! ================ Modern (15-MAY-97) syntax. First word is MAX_DEGREE
!
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                   IF ( TOKEN .EQ. 'AUTO'  .OR.  TOKEN .EQ. 'IN' ) THEN
                        CLKPOL_FLG = TOKEN(1:1)
                        IF ( CLKPOL_FLG .EQ. 'I' ) CLKPOL_FLG = 'D'
                      ELSE
                        CALL FERR ( INT2(4132), &
     &                      'BATCH(gflags) Error in parsing '// &
     &                      'keyword CLOCKS. Qualifiers IN or AUTO  '// &
     &                      'after the qualier MAX_DEGREE were expected, but '// &
     &                      'not '//TOKEN(1:16), INT2(0), INT2(0) )
                   END IF
!
                   IF ( TOKEN(1:1) .EQ. 'I' ) THEN
                        CLKPOL_DEG = 99
                     ELSE
!
! ------------------- Parsing token with max degree
!
                      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                      IF ( TOKEN(1:1) .EQ. '1' ) THEN
                           CLKPOL_DEG = 1
                        ELSE IF ( TOKEN(1:1) .EQ. '2' ) THEN
                           CLKPOL_DEG = 2
                        ELSE
                           CALL FERR ( INT2(4134), &
     &                         'BATCH(gflags) Error in parsing '// &
     &                         'keyword CLOCK. Only 1 or 2 are allowed for '// &
     &                         'max degree specification, but not '//TOKEN(1:16), &
     &                          INT2(0), INT2(0) )
                      END IF
                   END IF
!
! ---------------- Reading INTERVALS qualifier
!
                   CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                   IF ( TOKEN(1:3) .NE. 'INT' ) THEN
                        CALL FERR ( INT2(4136), &
     &                      'BATCH(gflags) Error in parsing '// &
     &                      'keyword CLOCKS. Word INTERVALS was expected, '// &
     &                      'but not '//TOKEN(1:16), INT2(0), INT2(0) )
                        STOP 'BATCH: Abnormal termination'
                   END IF
!
! ---------------- Well, We have avidence that the next qualifier is good,
! ---------------- let's parse it.
!
                   GOTO 910 ! backward
                 ELSE IF ( TOKEN(1:3) .EQ. 'INT' ) THEN
!
! ================ Modern (15-MAY-97) syntax. First word is INTERVALS
!
!
! ---------------- Reading further qualifier
!
                   CALL SPLITSTRING ( STRING, TOKEN, STRING )
                   CALL TRAN ( 11, TOKEN, TOKEN ) ! Translation to capital lett.
                   IF ( TOKEN .EQ. 'FORCE'  .OR.  TOKEN .EQ. 'AUTO'  .OR. &
     &                  TOKEN .EQ. 'MOST'   .OR.  TOKEN .EQ. 'IN'    .OR. &
     &                  TOKEN .EQ. 'NO'                                   ) THEN
!
                        CLKFLG = TOKEN(1:1)
                        IF ( CLKFLG .EQ. 'I' ) CLKFLG = 'D'
                      ELSE
                        CALL FERR ( INT2(4138), &
     &                      'BATCH(gflags) Error in parsing '// &
     &                      'keyword CLOCKS. After INTERVALS qualifiers '// &
     &                      'FORCE, AUTO, PICK, MOST, IN or NO are expected, '// &
     &                      'but not '//TOKEN(1:16), INT2(0), INT2(0) )
                   END IF
                   IF ( CLKFLG .EQ. 'I' ) THEN
                        CKNTRVL = 10000
                     ELSE IF ( CLKFLG .EQ. 'N' ) THEN
                        CKNTRVL = 0
                     ELSE
!
! --------------------- Reading values of the intervals (in miuntes)
!
                        CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
                        CKNTRVL = DECIMALTOINT ( TOKEN, IERR )
                        CALL FERR ( IERR, 'BATCH(gflags) Invalid clock '// &
     &                      'interval specification: '//TOKEN(1:16), INT2(0), &
     &                       INT2(0) )
                   END IF
                 ELSE
                   CALL FERR ( INT2(4139), 'BATCH(glfags) Unknown CLOCKS '// &
     &                 'qualifiers: '//TOKEN(1:16), INT2(0), INT2(0) )
               ENDIF
               KCLK=.TRUE.
             ELSE IF ( TOKEN .EQ. 'AXIS' ) THEN
!
! ------------ 'AXIS' keyword
!
              IF ( KAXS ) CALL FERR ( INT2(4140), &
     &            'BATCH(gflags) Keyword AXIS '//'used twice', INT2(0), INT2(0) )
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              CALL TRAN ( 11, TOKEN, TOKEN ) ! Translation to capital lett.
              IF ( TOKEN .EQ. 'YES' ) THEN
                   AXSFLG='Y'
                ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                   AXSFLG='N'
                ELSE
                   CALL FERR ( INT2(4142), &
     &                 'BATCH(gflags) Illegal AXIS parameter '//TOKEN(1:16), &
     &                  INT2(0), INT2(0) )
              ENDIF
              CALL GAXIS ( TOKEN, STRING )
              KAXS=.TRUE.
            ELSE IF ( TOKEN .EQ. 'BASELINE_CLOCKS' ) THEN
!
! ----------- 'BASELINE_CLOCKS' keyword
!
              IF ( KBLC ) CALL FERR ( INT2(4150), &
     &            'BATCH(gflags) Baseline_clocks '//'used twice', INT2(0), &
     &             INT2(0))
              CALL SPLITSTRING ( STRING, TOKEN, STRING )
              CALL TRAN ( 11, TOKEN, TOKEN ) ! Translation to capital lett.
              IF ( TOKEN .EQ. 'YES' ) THEN
                   BLCFLG='Y'
                ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                   BLCFLG='N'
                ELSE IF ( TOKEN .EQ. 'IN' ) THEN
                   BLCFLG='I'
                ELSE
                   CALL FERR ( INT2(4152), &
     &                 'BATCH(gflags) Illegal baseline_clocks '//'parameter '// &
     &                  TOKEN(1:16), INT2(0), INT2(0) )
              ENDIF
              KBLC = .TRUE.
            ELSE IF ( TOKEN .EQ. 'ERM' ) THEN
!
! ------------- 'ERM' keyword
!
                IF ( FL_ERM ) THEN
                     CALL FERR ( INT2(4040), 'BATCH(gflags) Keyword '// &
     &                   'ERM used twice', INT2(0), INT2(0) )
                END IF                
!
                IUER = -1 
                CALL PARSE_ERM_CNT ( STRING, L_EERM, ADR_EERM, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8627, -2, 'GFLAGS', 'Error in parsing '// &
     &                   'the keyword ERM' )
                     WRITE ( 6, * ) 'BATCH: abnormal termination'
                     CALL EXIT ( 1 ) 
                END IF
!
                FL_ERM = .TRUE.
            ELSE IF ( TOKEN .EQ. 'HEO' ) THEN
!
! ------------- 'HEO' keyword
!
                IF ( FL_HEO ) THEN
                     CALL FERR ( INT2(4040), 'BATCH(gflags) Keyword '// &
     &                   'HEO used twice', INT2(0), INT2(0) )
                END IF                
!
                IUER = -1 
                CALL PARSE_EHEO ( STRING, L_EHEO, ADR_EHEO, L_EHEC, ADR_EHEC, &
     &                            MJD_EHEO_REF, TAI_EHEO_REF, ADR_EHES, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8628, -2, 'GFLAGS', 'Error in parsing '// &
     &                   'the keyword HEO' )
                     WRITE ( 6, * ) 'BATCH: abnormal termination'
                     CALL EXIT ( 1 ) 
                END IF
                FL_HEO = .TRUE. 
            ELSE IF ( TOKEN .EQ. 'STRUCTURE_ADMITTANCE' ) THEN
!
! ------------- 'STRUCTURE_ADMITTANCE' keyword
!
                IF ( FL_SOUADM ) THEN
                     CALL FERR ( INT2(4050), 'BATCH(gflags) Keyword '// &
     &                   'STRUCTURE_ADMITTANCE used twice', INT2(0), INT2(0) )
                END IF                
!
                IUER = -1 
                CALL PARSE_SOUADM ( STRING, SOU_ADM_FLAG, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8630, -2, 'GFLAGS', 'Error in parsing '// &
     &                   'the keyword STRUCTURE_ADMITTANCE' )
                     WRITE ( 6, * ) 'BATCH: abnormal termination'
                     CALL EXIT ( 1 ) 
                END IF
                FL_SOUADM = .TRUE. 
            ELSE IF ( TOKEN .EQ. 'IONOSPHERE_SCALE' ) THEN
               IF ( KIOS ) CALL FERR ( INT2(4090), 'BATCH(gflags) Keyword '// &
     &             'IONOSPHERE_SCALE used twice', INT2(0), INT2(0) )
!
! ------------ Get the next token
!
               CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
               IF ( TOKEN(1:2) == 'NO' ) THEN
                    IOS_EST_BATCH = IOS__UNDF
                 ELSE IF ( TOKEN(1:3) == 'SES' ) THEN
                    IOS_EST_BATCH = IOS__SES
                 ELSE IF ( TOKEN(1:3) == 'STA' ) THEN
                    IOS_EST_BATCH = IOS__STA
                 ELSE IF ( TOKEN(1:3) == 'BAS' ) THEN
                    IOS_EST_BATCH = IOS__BAS
                 ELSE
                    CALL ERR_LOG ( 8629, -2, 'GFLAGS', 'Error in parsing '// &
     &                  'of the keyword IONOSPHERE_SCALE value: NO, SES, STA, '// &
     &                  'or BAS were expected, but got '//TRIM(TOKEN) )
                    WRITE ( 6, * ) 'BATCH: abnormal termination'
                    CALL EXIT ( 1 ) 
               END IF
               KIOS = .TRUE.
            ELSE
!
! ----------- Something that isn't suppose to be there
!
              CALL FERR ( INT2(4160), &
     &            'BATCH(gflags) Unknown KEYWORD in $FLAGS '//'section: '// &
     &             TOKEN(1:16), INT2(0), INT2(0) )
          ENDIF
        ENDDO
        LENGTH = CFREAD ( STRING )
      ENDDO
!
! === Now that this section is finished, what now?
!
      IF ( .NOT. KSTA ) THEN
           CALL FERR ( INT2(4201), &
     &         'BATCH(gflags) Keyword STATIONS is missing in '//'$FLAGS section', &
     &          INT2(0), INT2(0) )
        ELSE IF ( .NOT. KSRC ) THEN
           CALL FERR ( INT2(4202), &
     &         'BATCH(gflags) Keyword SOURCES is missing in '//'$FLAGS section', &
     &          INT2(0), INT2(0) )
        ELSE IF ( .NOT. KNUT ) THEN
           CALL FERR ( INT2(4203), &
     &         'BATCH(gflags) Keyword NUTATION is missing in '//'$FLAGS section', &
     &          INT2(0), INT2(0) )
        ELSE IF ( .NOT. KUT1 ) THEN
           CALL FERR ( INT2(4204), &
     &         'BATCH(gflags) Keyword UT1/PM is missing in '//'$FLAGS section', &
     &          INT2(0), INT2(0) )
        ELSE IF ( .NOT. KREL ) THEN
           CALL FERR ( INT2(4205), &
     &         'BATCH(gflags) Keyword RELATIVITY is missing in '// &
     &         '$FLAGS section', INT2(0), INT2(0) )
        ELSE IF ( .NOT. KPRC ) THEN
           CALL FERR ( INT2(4206), &
     &         'BATCH(gflags) Keyword PRECESSION is missing in '// &
     &         '$FLAGS section', INT2(0), INT2(0) )
        ELSE IF ( .NOT. KVEL ) THEN
           CALL FERR ( INT2(4208), &
     &         'BATCH(gflags) Keyword VELOCITIES is missing in '// &
     &         '$FLAGS section', INT2(0), INT2(0) )
        ELSE IF ( .NOT. KATM ) THEN
           CALL FERR ( INT2(4209), &
     &         'BATCH(gflags) Keyword ATMOSPHERES is missing '// &
     &         'in $FLAGS section', INT2(0), INT2(0) )
        ELSE IF ( .NOT. KCLK ) THEN
           CALL FERR ( INT2(4210), &
     &         'BATCH(gflags) Keyword CLOCKS is missing in '//'$FLAGS section', &
     &          INT2(0), INT2(0) )
        ELSE IF ( .NOT. KBLC ) THEN
           CALL FERR ( INT2(4211), &
     &         'BATCH(gflags) Keyword BASELINE_CLOCKS is '// &
     &         'missing in $FLAGS section', INT2(0), INT2(0) )
        ELSE
           CALL CFUNRD ( LENGTH, STRING )
      ENDIF
      KGRAD = KGRD
!
! --- Warnings:
!
      IF ( KSTA  .AND.  FL_VEL_SEL  .AND.  .NOT. FL_SIT_EST_EPOCH ) THEN
           STR = JD_TO_DATE ( SIT_EST_EPOCH_VAL, -3 )
           WRITE ( 6, '(A)' ) 'WARNING: Reference epoch for estimates of '// &
     &                        'site positions was not specified. '// &
     &                        'Set to '//STR(1:19)
      END IF
      IF ( KSRC  .AND.  FL_PRP_SEL  .AND.  .NOT. FL_SOU_EST_EPOCH ) THEN
           STR = JD_TO_DATE ( SOU_EST_EPOCH_VAL, -3 )
           WRITE ( 6, '(A)' ) 'WARNING: Reference epoch for estimates of '// &
     &                        'source coordinates was not specified. '// &
     &                        'Set to '//STR(1:19)
      END IF
!
      CALL USE_GLBFIL_4 ( 'WC' )
!
      IF ( ADR_HPE .NE. 0 ) THEN
           IF ( SOLTYP .EQ. 'I' ) THEN
                CALL FERR ( INT2(4221), 'BATCH(gflags) Keyword HARMONIC_POS '// &
     &              'is not supported in the independent mode. Please '// &
     &              'change it to HARMONIC_POS NONE', INT2(0), INT2(0) )
                CALL EXIT ( 1 )
           END IF
!@           CALL PRINT_HPE ( 6, L_HPE, %VAL(ADR_HPE) ) ! %%%%%%%%%%%%%%%%%%%%%%%%
!@           CALL PAUSE ( 'GFLAGS after PRINT_HPE' ) 
      END IF
      IF ( ADR_SPE .NE. 0 ) THEN
           IF ( SOLTYP .EQ. 'I' ) THEN
                CALL FERR ( INT2(4222), 'BATCH(gflags) Keyword SPLINE_POS '// &
     &              'is not supported in the independent mode. Please '// &
     &              'change it to SPLINE_POS NONE', INT2(0), INT2(0) )
                CALL EXIT ( 1 )
           END IF
!@           CALL PRINT_SPE ( 6, L_SPE, %VAL(ADR_SPE) )
!@           CALL PAUSE ( 'GFLAGS after PRINT_SPE' ) 
      END IF
!
      IF ( ADR_EHEO .NE. 0 ) THEN
           IF ( SOLTYP .EQ. 'I' ) THEN
                CALL FERR ( INT2(4223), 'BATCH(gflags) Keyword HEO '// &
     &              'is not supported in the independent mode. Please '// &
     &              'change it to HEO NONE', INT2(0), INT2(0) )
                CALL EXIT ( 1 )
           END IF
!@           CALL PRINT_EHEO ( 6, L_EHEO, %VAL(ADR_EHEO), L_EHEC, &
!@     &                          %VAL(ADR_EHEC), %VAL(ADR_EHES) )
!@           CALL PAUSE ( 'GFLAGS after PRINT_EHEO' ) 
      END IF
      IF ( ADR_EERM .NE. 0 ) THEN
           IF ( SOLTYP .EQ. 'I' ) THEN
                CALL FERR ( INT2(4224), 'BATCH(gflags) Keyword ERM '// &
     &              'is not supported in the independent mode. Please '// &
     &              'change it to ERM NONE', INT2(0), INT2(0) )
                CALL EXIT ( 1 )
           END IF
      END IF
      CALL USE_GLBFIL_3 ( 'OWC' )
!
      RETURN
      END   !#!  GFLAGS  #!#
