      SUBROUTINE CTRLFL ( RUN_INITS, ARCREC, ITARCS, WEIGHTS, LF_WEI, &
     &           WEIGHT_FILE, SOLTYP, CGMNMR, INCGM_TYPE, INCGM_USER, &
     &           INCGM_SOL, B_ARCDIR, ID, USER_PROG, B_KPERMARC, &
     &           USER_BUFF, STAFLG, FIXSTA_CHR, STADIU, VELFLG, SRCFLG, &
     &           FIXSRC_CHR, PROFLG, NUTFLG, UT1FLG, PRCFLG, RELFLG, IONCTL, &
     &           STACRY, SRCCRY, RSTOUT, MINOUT, BASOUT, FWDOUT, SCNOUT, &
     &           ATMFLG, INTRVL, CLKPOL_FLG, CLKPOL_DEG, CLKFLG, CKNTRVL, &
     &           FCNPR, TBLOUT, AXSFLG, &
     &           OFFLG, RATFLG, ACCEOP_FLG, IEOP_FLG, REOP_FLG, &
     &           IEOPLL, OUTCGM, POSELL, &
     &           BLCFLG, IOS_EST_BATCH, BASDF, USER_TAG, SOL_TAG, SOLARCH_SOL, &
     &           KUSER_PART, USER_PART_PROG, KGLOBONLY, EOPMID, KHFEOPEST, &
     &           IONFLG, POSEPOCH, POSNUM, MODOUTFLG, RESFILE, KMIN_SIG, &
     &           GRADFLG, GRINTRVL, KUSER_CONST, USER_CONST_PROG, &
     &           CMERG, KOUTNRM, KZERONRM, WEIGHT_TYPE_MA, WEIGHT_ALGORITHM, &
     &           SIT_EST_EPOCH_VAL, SOU_EST_EPOCH_VAL, EOP_EPOCH_MJD, &
     &           EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI, &
     &           PARU_FILE, MF_WEI )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CTRLFL PROGRAM SPECIFICATION
!
! 1.1 Parse the batch control file.
!
! 1.2 REFERENCES:
!
! 2.  CTRLFL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*2   RUN_INITS
      CHARACTER*(*) WEIGHTS, WEIGHT_FILE(*), USER_PART_PROG
      CHARACTER*(*) WEIGHT_TYPE_MA
      CHARACTER*(*) SOLTYP, STAFLG, VELFLG, SRCFLG, PROFLG, NUTFLG(116), &
     &              STACRY, SRCCRY
      CHARACTER*(*) UT1FLG, RSTOUT, MINOUT, BASOUT, SCNOUT, RELFLG, PRCFLG
      CHARACTER*(*) IONCTL, STADIU, TBLOUT, ID(10)
      CHARACTER*(*) CGMNMR, ATMFLG, CLKPOL_FLG, CLKFLG, USER_PROG, USER_BUFF, &
     &              AXSFLG
      CHARACTER*(*) B_ARCDIR(3), OUTCGM, POSELL, BLCFLG, BASDF, IONFLG
      CHARACTER*(*) RESFILE, GRADFLG, USER_CONST_PROG, CMERG(10)
      CHARACTER*4   INCGM_TYPE
      CHARACTER*2   INCGM_USER,USER_TAG
      CHARACTER     INCGM_SOL*8, SOL_TAG*8, FIXSTA_CHR*8, FIXSRC_CHR*8, &
     &              PARU_FILE*(*)
      INTEGER*4     LF_WEI
      INTEGER*2     FWDOUT,ITARCS,INTRVL, OFFLG, RATFLG, &
     &              ACCEOP_FLG
      INTEGER*2  CLKPOL_DEG, CKNTRVL, IEOP_FLG(6), IEOPLL, POSNUM, GRINTRVL, &
     &           EOPMID, IOS_EST_BATCH
      REAL*8     REOP_FLG(4), POSEPOCH, EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI
      INTEGER*4  ARCREC
      LOGICAL*2  B_KPERMARC, SOLARCH_SOL, KUSER_PART, KGLOBONLY
      LOGICAL*2  KHFEOPEST, MODOUTFLG, KMIN_SIG, KUSER_CONST
      LOGICAL*2  KOUTNRM, KZERONRM
      INTEGER*4  WEIGHT_ALGORITHM
      REAL*8     FCNPR, SIT_EST_EPOCH_VAL, SOU_EST_EPOCH_VAL, EOP_EPOCH_SEC
      INTEGER*4  MF_WEI, EOP_EPOCH_MJD, IUER
!
! ARCREC - file position of first arc
! ATMFLG - atmosphere flag
! AXSFLG - axis offset flag
! B_ARCDIR - directories in which to write arc files
! B_KPERMARC - TRUE if arc files to be saved permanently
! BASOUT - Baseline output flag
! CLKPOL_FLG - Global polinomial clock flag
! CLKFLG     - Segmented clock flag
! CLKPOL_DEG - Maximim degree of global polinomial clocks
! CKNTRVL - clock interval, minutes
! FCNPR - free core nutation period
! FIXSRC_CHR - reference source name
! FIXSTA_CHR - reference station name
! FWDOUT - flag for output for forward solution
! INTRVL - atmosphere interval, minutes
! ID - String identifying this batch run
! IONCTL - ionosphere control flag
! ITARCS - number of arcs in control file
! MINOUT - flag for minimum output to spool file
! NUTFLG - nutation flag
! PRCFLG - precession flag
! RELFLG - relativity flag
! RSTOUT - TRUE if spool file is to be reset
! SCNOUT - screen output flag
! SOLTYP - solution type (Complete, Back, etc.)
! SRCCRY - source carry flag
! SRCFLG - source flag
! STACMP - station component flag
! STADIU - station diurnal flag
! STACRY - station carry flag
! STAFLG - station flag
! TBLOUT - station table flag
! USER_BUFF - user buffer
! USER_PROG - user program
! UT1FLG    - earth orientation flag
! LF_WEI    - the number of weight files
! WEIGHTS   - weights flag
! WEIGHT_FILE    - Name of file containing weights
! WEIGHT_TYPE_MA - Type of weights (A = by_arc, S = site, B = baseline)
! OFFLG  - Earth orientation offset flag
! RATFLG - Earth orientation rate flag
! ACCEOP_FLG - Earth orientation acceleration flag.
! IEOP_FLG - Local earth orientation flags
! REOP_FLG - Earth orientation intervals and constraints
! IEOPLL   - Earth orientation plot flag
! WEIGHT_ALGORITHM  -- identifier of the algorithm used for making weights
! SIT_EST_EPOCH_VAL -- reference epoch for site position estimation
! SOU_EST_EPOCH_VAL -- reference epoch for source position estimation
! PARU_FILE         -- name of the PARU-file needed for ELIM algorithm
! MF_WEI            -- the maximum number of weights files
! IOS_EST_BATCH     -- estimation of ionosphere path delay scale
!
! 2.4 COMMON BLOCKS USED
!
      INCLUDE 'solve.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'exceptions.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrls
!       CALLED SUBROUTINES: goutpt,gsetup,gcarry,gflags,gdata,gcalib,
!                           gcont,gsuprs,gmap,gconst,kpart
!
! 3.  LOCAL VARIABLES
!
      CHARACTER TOKEN*256, STRING*8192, BATSIL*3
      INTEGER*2 LENGTH,IDUM,CFREAD
      LOGICAL*2 KSETUP,KFLAGS,KARCS,KCARRY,KOUTPT,KDATA,KCALIB,KCONT
      LOGICAL*2 CFEOF,KMAP,KSUPRS,KCNSTR,KPART, KMCAL
!
! INITIALIZE
!
      DATA KSETUP/.FALSE./,KFLAGS/.FALSE./,KCARRY/.FALSE./
      DATA KOUTPT/.FALSE./,KARCS /.FALSE./,KDATA /.FALSE./
      DATA KCONT /.FALSE./,KCALIB/.FALSE./,KMAP  /.FALSE./
      DATA KSUPRS/.FALSE./,KCNSTR/.FALSE./,KPART /.FALSE./
      DATA KMCAL /.FALSE./
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  901128  Enabled earth orientation parameterization scheme
!   AEE  910419  Added SRCCMP for proper_motions
!   AEE  920522  Added PARTIAL section.
!   KDB  970204  New site weighting feature.  Pass up new variable (for
!                the type of weights).
!   KDB  980223  Batch interface for sinex output.
!   pet  1999.11.18  Added mode calibration section
!   pet  2000.03.29  Added variable WEIGHT_ALGORITM
!   pet  2000.09.07  Added support of environment variable BATCH_SILENT which
!                    suppresses information messages while control
!                    file is read.
!   pet  2000.10.05  Corrected a bug: EOPMID should be of INTEGER*2 type!
!
!   pet  2001.08.10  converted type of FIXSTA, FIXSRC ( INTEGER*2 ) to
!                    FIXSTA_CHR, FIXSRC_CHR ( CHARACTER ). Removed STACMP,
!                    SRCCMP, TIDFLG. Added VELFLG, PROFLG
!   pet  2002.03.27  Removed variables LSINEX, LLSNXDIR
!   pet  2007.08.10  Added initialization of GLBC3
!
!
! 5.  CTRLFL PROGRAM STRUCTURE
!
!
! --- Initialize some things
!
      USER_PROG      = ' '
      USER_PART_PROG = ' '
      KUSER_PART     = .FALSE.
      KGLOBONLY      = .FALSE.
      USER_BUFF      = 'N'
      EOPMID         = 0
!
! --- Initialization of GLBC3
!
      CALL USE_GLBFIL_3 ( 'OR' )
      CALL NOUT ( JGLBC3_BLOCKS*INT2(BLOCK_BYTES), BEGMARK_GLBC3_I2 )
      NNT_POS_GLO = .FALSE.
      NNT_POS_LOC = .FALSE.
      NNR_POS_GLO = .FALSE.
      NNR_POS_LOC = .FALSE.
      NNR_SRC_GLO = .FALSE.
      NNR_SRC_LOC = .FALSE.
      ISRCSP      = 0
      ISTASP      = 0
      IND_NNT_POS = 0
      IND_NNR_POS = 0
      IND_NNT_VEL = 0
      IND_NNR_VEL = 0
      IND_NNR_SRC = 0
      IND_NNR_PRP = 0
      CALL USE_GLBFIL_3 ( 'WC' )
!
      BATSIL = '   '
      CALL GETENVAR ( "BATCH_SILENT", BATSIL )
      IF ( BATSIL(1:1) .EQ. 'y' ) BATSIL(1:1) = 'Y'
!
! --- Read first record, and then loop until end of file
!
      LENGTH=CFREAD(STRING)
      DO WHILE (.NOT. CFEOF(IDUM))
        IF(STRING(1:1).NE.'$') THEN
          CALL FERR( INT2(1000), '$KEYWORD EXPECTED, NOT: '//STRING(1:16), &
     &         INT2(0), INT2(0) )
        ENDIF
!
        CALL SPLITSTRING(STRING,TOKEN,STRING )
        IF(TOKEN.EQ.'$SETUP') THEN
!
! ------- Handle $SETUP section
!
          IF ( KSETUP ) CALL FERR ( INT2(1010), '$SETUP USED TWICE', INT2(0), &
     &         INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$SETUP'
          IUER = -1
          CALL GSETUP ( RUN_INITS, SOLTYP, CGMNMR, INCGM_TYPE, &
     &                  INCGM_USER, INCGM_SOL, &
     &                  B_ARCDIR, ID, USER_PROG, USER_BUFF, &
     &                  B_KPERMARC, WEIGHTS, LF_WEI, WEIGHT_FILE, OUTCGM, &
     &                  USER_TAG, SOL_TAG, SOLARCH_SOL, KUSER_PART, &
     &                  USER_PART_PROG, KUSER_CONST, USER_CONST_PROG, &
     &                  CMERG, WEIGHT_TYPE_MA, WEIGHT_ALGORITHM, PARU_FILE, &
                        MF_WEI, IUER )
          IF ( IUER .NE. 0 ) THEN
               CALL ERR_LOG ( 8561, -2, 'CTRLFL', 'Failure to parse $SETUP '// &
     &             'section of the batch control file' )
               CALL EXIT ( 1 )
          END IF
          KSETUP=.TRUE.
        ELSE IF ( TOKEN .EQ. '$FLAGS' ) THEN
!
! ------- Handle $FLAGS section
!
          IF(KFLAGS) CALL FERR( INT2(1020), '$FLAGS USED TWICE', INT2(0), &
     &       INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$FLAGS'
          CALL GFLAGS ( SOLTYP, STAFLG, FIXSTA_CHR, STADIU, VELFLG, SRCFLG,    &
     &         FIXSRC_CHR, PROFLG, NUTFLG, UT1FLG, PRCFLG, RELFLG, ATMFLG,     &
     &         INTRVL, CLKPOL_FLG, CLKPOL_DEG, CLKFLG, CKNTRVL, FCNPR, AXSFLG, &
     &         OFFLG, RATFLG, ACCEOP_FLG, IEOP_FLG, REOP_FLG,        &
     &         IEOPLL, BLCFLG, IOS_EST_BATCH, EOPMID, KHFEOPEST, GRADFLG, GRINTRVL, &
     &         SIT_EST_EPOCH_VAL, SOU_EST_EPOCH_VAL, EOP_EPOCH_MJD,  &
     &         EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI  )
          KFLAGS = .TRUE.
        ELSE IF ( TOKEN .EQ. '$ARCS' ) THEN
!
! ------- Count the arcs in the $ARCS section and save posoition of first one
!
          IF(KARCS) CALL FERR( INT2(1030), '$ARCS USED TWICE', INT2(0), &
     &       INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$ARCS'
          CALL CFPOS(ARCREC )
          ITARCS=0
          LENGTH=CFREAD(STRING)
          DO WHILE (STRING(1:1).NE.'$'.AND..NOT.CFEOF(IDUM))
             ITARCS=ITARCS+1
             LENGTH=CFREAD(STRING)
          ENDDO
          CALL CFUNRD(LENGTH,STRING )
          KARCS=.TRUE.
        ELSE IF(TOKEN.EQ.'$CARRY') THEN
!
! ------- Handle $CARRY section
!
          IF(KCARRY) CALL FERR( INT2(1040), '$CARRY USED TWICE', INT2(0), &
     &       INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$CARRY'
          IF ( .NOT. KFLAGS ) CALL FERR ( INT2(1045), &
     &        '$FLAGS MUST PRECEED $CARRY', INT2(0), INT2(0) )
          CALL GCARRY ( STACRY, SRCCRY, STA_CMP_ALL )
          KCARRY=.TRUE.
        ELSE IF(TOKEN.EQ.'$OUTPUT') THEN
!
! ------- Handle $OUTPUT section
!
          IF(KOUTPT) CALL FERR( INT2(1050), '$OUTPUT USED TWICE', INT2(0), &
     &       INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$OUTPUT'
          CALL GOUTPT ( SOLTYP, RSTOUT, MINOUT, BASOUT, FWDOUT, &
     &                  SCNOUT, TBLOUT, POSELL, POSEPOCH, POSNUM, &
     &                  MODOUTFLG, RESFILE, KMIN_SIG, KOUTNRM, KZERONRM )
          KOUTPT=.TRUE.
        ELSE IF(TOKEN.EQ.'$DATA') THEN
!
! ------- Handle $DATA section
!
          IF(KDATA) CALL FERR( INT2(1060), '$DATA USED TWICE', INT2(0), &
     &       INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$DATA'
          CALL GDATA(basdf )
          KDATA=.TRUE.
        ELSE IF(TOKEN.EQ.'$CONTRIBUTIONS') THEN
!
! ------- Handle $CONTRIBUTIONS section
!
          IF(KCONT) CALL FERR( INT2(1070), '$CONTRIBUTIONS USED TWICE', &
     &       INT2(0), INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$CONTRIBUTIONS'
          CALL GCONT()
          KCONT=.TRUE.
        ELSE IF ( TOKEN .EQ. '$MODE_CALIBRATIONS' ) THEN
!
! ------- Handle $MODE_CALIBRATIONS section
!
          IF(KMCAL) CALL FERR ( INT2(1080), '$MODE_CALIBRATIONS USED TWICE', &
     &       INT2(0), INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$MODE_CALIBRATIONS'
          CALL GMCAL()
          KMCAL = .TRUE.
        ELSE IF(TOKEN.EQ.'$CALIBRATIONS') THEN
!
! ------- Handle $CALIBRATIONS section
!
          IF(KCALIB) CALL FERR( INT2(1090), '$CALIBRATIONS USED TWICE', &
     &       INT2(0), INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$CALIBRATIONS'
          CALL GCALIB(IONCTL,ionflg )
          KCALIB=.TRUE.
        ELSE IF(TOKEN.EQ.'$PARTIALS') THEN
!
! ------- Handle $PARTIALS section
!
          IF(KPART) CALL FERR( INT2(1100), '$PARTIALS USED TWICE', INT2(0), &
     &       INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$PARTIALS'
          KPART=.TRUE.
          CALL GPART(kpart )
        ELSE IF(TOKEN.EQ.'$MAPPING') THEN
!
! ------- Handle $MAPPING section
!
          IF(KMAP) CALL FERR( INT2(1110), '$MAPPING USED TWICE', INT2(0), &
     &       INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$MAPPING'
          CALL GMAP()
          KMAP=.TRUE.
        ELSE IF(TOKEN.EQ.'$SUPPRESSION') THEN
!
! ------- Handle $SUPPRESSION section
!
          IF ( .NOT. KCNSTR) THEN
               CALL FERR ( INT2(1120), &
     &             '$CONSTRAINTS must precede $SUPPRESSION', INT2(0), INT2(0) )
          END IF
          IF ( KSUPRS ) THEN
               CALL FERR( INT2(1120), '$SUPPRESSION USED TWICE', INT2(0), &
     &              INT2(0) )
          END IF
!
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$SUPPRESSION'
          IF ( .NOT. KFLAGS ) CALL FERR ( INT2(1105), '$FLAGS MUST PRECEED '// &
     &        '$SUPPRESION', INT2(0), INT2(0) )
          CALL GSUPRS ( STA_CMP_ALL, FIXSTA_CHR )
          KSUPRS = .TRUE.
        ELSE IF(TOKEN.EQ.'$CONSTRAINTS') THEN
!
! ------- Handle $CONSTRAINTS section
!
          IF(KCNSTR)CALL FERR( INT2(1130), '$CONSTRAINTS USED TWICE', INT2(0), &
     &       INT2(0) )
          IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( *, * ) '$CONSTRAINTS'
          CALL GCONST()
          KCNSTR=.TRUE.
        ELSE
!
! ------- Error for unknown keyword
!
          CALL FERR( INT2(1995), 'UNKNOWN KEYWORD: '//TOKEN(1:16), INT2(0), &
     &         INT2(0) )
        ENDIF
!
        LENGTH=CFREAD(STRING)
      ENDDO
!
! --- Get number of last arc if we want it output on forward solution
!
      IF(FWDOUT.LT.0) FWDOUT=ITARCS
!
! --- Make sure all required sections have been found and processed
!
      IF ( .NOT. KPART) CALL GPART (KPART )
      IF(.NOT. &
     &   (KSETUP.AND.KFLAGS.AND.KARCS.AND.KCARRY.AND.KOUTPT.AND. &
     &         KDATA.AND.KCALIB.AND.KCONT.AND.KMAP.AND.KSUPRS.AND. &
     &   KCNSTR)) THEN
        CALL FERR( INT2(1999), '$KEYWORDS MISSING FROM COMMAND FILE', INT2(0), &
     &       INT2(0) )
      ENDIF
!
      RETURN
      END  !#!  CTRLFL  #!#
