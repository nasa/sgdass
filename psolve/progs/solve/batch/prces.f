      SUBROUTINE PRCES ( IONCTL, IWARNING, BUF_WARNING, GLBMEM, SUPNAM_WEI, &
     &                   SUPVER_WEI, BASELINE_WEI, ARR_WEI, VCAT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PRCES PROGRAM SPECIFICATION
!
! 1.1 Process batch run according to control file.
!
! 1.2 REFERENCES:
!
! 2.  PRCES INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!     GLBMEM -- a data structure for keeping CGM
!
      CHARACTER IONCTL*(*)
!
! IONCTL - Ionosphere control (ON, OFF or DEFAULT)
!
! 2.3 OUTPUT Variables:
!     IWARNING, BUF_WARNING - error code and message for errors which warrant
!     flagging, but not until the actual processing is complete
!
       INTEGER*2  IWARNING, SUPVER_WEI(MAX4_WEIREC)
       CHARACTER  BUF_WARNING*248
       REAL*8     ARR_WEI(4,MAX4_WEIREC)
       CHARACTER  SUPNAM_WEI(MAX4_WEIREC)*10, BASELINE_WEI(MAX4_WEIREC)*16
!
!
! 2.4 COMMON BLOCKS USED
      INCLUDE  'precm.i'
      INCLUDE  'erm.i'
      INCLUDE  'socom.i'
      INCLUDE  'socom_plus.i'
      INCLUDE  'batcm.i'
      INCLUDE  'ba2cm.i'
      INCLUDE  'glbcm.i'
      INCLUDE  'glbc3.i'
      INCLUDE  'glbc4.i'
      INCLUDE  'dmapp.i'
      INCLUDE  'prfil.i'
      INCLUDE  'glbp.i'
      INCLUDE  'cnstr.i'
      INCLUDE  'fast.i'
      INCLUDE  'gvh.i'
      INCLUDE  'trp.i'
      INCLUDE  'vcat.i'
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM   ! defined in glbp.i
      TYPE ( B3D__STRU     ) ::  B3DOBJ
      TYPE ( B1B3D__STRU   ) ::  B1B3DOBJ
      TYPE ( CNSTR__STRU   ) ::  CNSTROBJ ! constraints data structures
      TYPE ( GVH__STRU     ) ::  GVH
      TYPE ( VCAT__TYPE    ) ::  VCAT
      INTEGER*4  MEMORY_DEBUG_FLAG, MEMORY_DEBUG__YES, I88
      PARAMETER  ( MEMORY_DEBUG__YES = 1020304908 )
      COMMON   / MEMORY_DEBUG  / MEMORY_DEBUG_FLAG
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: batch
!       CALLED SUBROUTINES: cfspos, save_weights, prgres, saves, arcset,
!                           get_weights, apnd_2_spool
!
! 3.  LOCAL VARIABLES
!
      CHARACTER DBNAME*10, TOKEN*16, APPEND_FLAG*1, CDUM*80, STRING1*256, &
     &          ARCNAME*10, RESP*1
      CHARACTER STOPFILE*(NAME_SIZE), FNAME*(NAME_SIZE)
      INTEGER*2 TRIMLEN, IERR, DECIMALTOINT
      INTEGER*2 VER, IBUFF(64), I, VERSION
      CHARACTER      CBUFF*128
      EQUIVALENCE  ( IBUFF, CBUFF )
      LOGICAL*2 DONE, KMORE, KLAST, KFAIL, FL_FOUND
      LOGICAL*4 KEX, LEX, FL_OPEN, OLD_LOGWEI
      INTEGER*4 NBAD_ARC, IUER, J1, IOS, IB, L_WEI, INT_DAT, SAVE_CNTREC, &
     &          IM, IND_SES, INC_VERS
      INTEGER*8 VTD_ADR_SAVE
      REAL*8    CONSTANTS ( 4, MAX_ARC_BSL )
      CHARACTER GA_CHAR*1, STR*128, CGMZ_FIL*128, PARU_FILE_ARC*128, &
     &          PARU_FILE_USE*128, WEI_FILE_LOCK*128
      REAL*8    WEI_POLL_INT, WEI_MAX_INT
      PARAMETER  ( WEI_POLL_INT = 0.005D0 ) ! Poll intercal for weights lock file
      PARAMETER  ( WEI_MAX_INT  = 8.000D0 ) ! maximal age of the weights lock file
      INTEGER*8  MEM_RSS 
      INTEGER*2  NBLINE, LENGTH, IDUM
      LOGICAL*4  FL_GVF, FL_SKIP
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*2, EXTERNAL :: CFREAD
      LOGICAL*2, EXTERNAL :: KBIT, CFEOF
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      INTEGER*8, EXTERNAL :: GET_MEMRSS 
!
      DATA      KMORE / .TRUE. /,  KFAIL / .FALSE. /
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE  910419  Added SRCCMP for proper_motions
!   mwh  910813  Implement eop rate constraints
!   AEE  910913  Changed call to runprog for slend from PASS to WAIT.
!   AEE  920204  Removed hard coded path for fclib.i
!   KDB  950802  In user partial, check for failure to specify global or arc.
!   KDB  970204  New site weighting feature.
!   PET  970516  Time profiling feature added.
!   PET  970522  Bug fixed: SOCOM_EXT and FAST_BYPASS added in order to change
!                FAST mode if it is necssary.
!   PET  971006  Bug fixed: SOCOM_EXT and FAST_BYPASS were moved from prces to
!                ARCSET
!   KDB  980223  Batch interface for sinex output feature.
!   pet  990118  Made a set of changes to support NO TRAIN mode.
!                Improved comments
!   pet  990403  Add call of routines from hausr library for gathering
!                statistics in NO TRAIN mode
!   pet  990404  Corrected logic with openning and closing spool file
!   pet  990405  Added call of STATUS_SET. Removed unused variables
!   pet  990420  Minor change: replaced the second call use_spoll ( 'OS' ) with
!                use_spoll ( 'O' )
!   pet  1999.05.31  Added CNSTROBJ to call ot GLO_PROC, GLO_NORML, INDP
!                    in order to reduce usage of operative memory
!   pet  1999.06.11  socom is re-read after user_partials and/or user_program
!                    since user_program and/or user_partials may change it.
!   pet  2000.01.18  Added removing ULC file before execution user-program
!                    or user partials
!   pet  2000.03.29  Added support of UPWEI algorithm in computation of
!                    quadratci correction to formal uncertainties
!   pet  2000.09.28  Small change in logic: SOLVE writes down the name of
!                    the output CGM file in the file CGMZxx. This trick
!                    allows other programs to bypass the logic of creation
!                    the name for permanent CGM and to get this name easy.
!   pet  2000.10.30  Moved setting the final batch status from prcess to batch
!                    in order to prevent appearence of messages about
!                    successfull solve finish before all thigs are really done.
!   pet  2000.11.24  Added support of KVELCONST flag.
!   pet  2001.07.17  Improved error messages related to the errors in
!                    user partial file
!   pet  2001.08.10  Slightly changed interface to arcset
!   pet  2001.09.05  Added varaibles ARR_WEI, SUPNAM_WEI, SUPVER_WEI,
!                    BASELINE_WEI. Implemented new logic for reading weights
!                    file and checking whether all supefiles have records in
!                    weights file. In according new logic, the weihghts file
!                    is parsed and saved in memery at the versy beginning,
!                    before processing the first session
!   pet  2001.12.13  Added support of a new type of solutions: GLOBAL_ONLY
!                    ( SOLTYP = 'G' )
!   pet  2002.03.18  Added call of a subroutine user_restore which fixed the
!                    the bug of the previous version: the previous version
!                    lost accumulated USRGxx file. The new version restores
!                    this file back. It may be necessary in the beginning
!                    back run or in restoreation mode in the case when user
!                    global parameters were adjusted.
!   pet  2002.10.15  Fixed a typo: the previous version erroneosly labeled
!                    the file for user local constraints as ULC instead of
!                    ULCC
!   pet  2007.07.05  Added support of PARU
!   pet  2008.02.07  Added support of using files with external 
!                    troposheric path delay
!   pet  2013.06.24  Forced to save the name of the control file at 
!                    a start of processing the the first experiment
!
! 5.  PRCES PROGRAM STRUCTURE
!CCCCC
!
! --- Setting status: memory is free
!
      B3DOBJ%MEM_STAT   = F__MFR
      B1B3DOBJ%MEM_STAT = F__MFR
!
      IF ( KUSER_PART ) THEN
!
! -------- Restore contetns of USRGxx file if ncessary
!
           CALL USER_RESTORE ( )
      END IF
!
      FL_GVF = .FALSE.
      DO WHILE ( KMORE )
!
! ------ This is which branch?
!
         IF ( .NOT. RESTRT ) THEN
!
! --------- If starting from beginning, set solution type and position to
! --------- first arc line in control file
!
            SOLTY2 = SOLTYP
            IF ( SOLTYP .EQ. 'S'                  ) SOLTY2='B'
            IF ( SOLTYP .EQ. 'G'                  ) SOLTY2='F'
            IF ( SOLTYP .EQ. 'C' .AND. IPASS.EQ.1 ) SOLTY2='F'
            IF ( SOLTYP .EQ. 'C' .AND. IPASS.EQ.2 ) SOLTY2='B'
            ARCNUM = 0
            DONE   = .FALSE.
         ENDIF ! restrt
!
! ------ This is a temporary logic for a kludge environment variable
! ------ 2001.09.05
!
         CALL GETENVAR ( 'OLD_LOGWEI', STR )
         IF ( STR .EQ. 'YES' .OR. STR .EQ. 'yes' ) THEN
              OLD_LOGWEI = .TRUE.
            ELSE
              OLD_LOGWEI = .FALSE.
         END IF
!
! ------ Save position in the control file
!
         SAVE_CNTREC = SAVREC
!
! ------ Set internal position in control file.
!
         CALL CFSPOS ( ARCREC )
         IF ( WEIGHTS .EQ. 'R' .OR. WEIGHTS .EQ. 'U' ) THEN
              WRITE ( 6, '(A$)' ) '  Weights file is being read...'//CHAR(13)
!
! ---------- Read weights file
!
             IUER = -1
             CALL READ_WEIGHTS ( MAX4_WEIREC, L_WEI, WEIGHT_TYPE_GEN, &
     &                           LF_WEI, WEIGHT_FILE, SUPNAM_WEI, SUPVER_WEI, &
     &                           BASELINE_WEI, ARR_WEI, IUER )
             IF ( IUER .NE. 0 ) THEN
                  CALL ERR_LOG ( 4101, -2, 'PRCES', &
     &                'Error in attempt to read weights file '//WEIGHT_FILE(1) )
                  WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                  CALL EXIT ( 1 )
             END IF
!
             IF ( OLD_LOGWEI ) THEN
                  CALL CHWGHT ( ' ', FL_FOUND, INT2(0) )
             END IF
             READ ( 92, "(A)", END=60, IOSTAT=IOS ) STRING1
             CALL FERR ( INT2(IOS), "Reading control file", INT2(0), INT2(0) )
!
! ---------- Now check all superfiles: whether there are records in weights
! ---------- file for each superfile listed in the control file
!
             NBAD_ARC = 0
             DO WHILE ( STRING1(1:1) .NE. '$' )
                IF ( STRING1 .EQ. ' ' ) GOTO 60
                IF ( OLD_LOGWEI ) THEN
                     IF ( STRING1(2:2).EQ.'$' ) THEN
                          CALL CHWGHT ( STRING1, FL_FOUND, INT2(1) )
                          IF ( .NOT. FL_FOUND)  NBAD_ARC = NBAD_ARC + 1
                     ENDIF
                END IF
                IF ( STRING1(1:1) .EQ. '*' ) GOTO 810
!
! ------------- Get superfile name and its version from control file
!
                CALL SPLITSTRING ( STRING1, ARCNAME, STRING1 )
                IF ( ARCNAME == 'GVF' ) THEN
                     CALL SPLITSTRING ( STRING1, CDUM, STRING1 )
                     IB = LINDEX ( CDUM, '/' )
                     CALL CHIN ( CDUM(IB+1:IB+8), INT_DAT )
                     IF ( INT_DAT < 19700101 .OR. INT_DAT > 20700101 ) THEN
                          CALL SPLITSTRING ( STRING1, CDUM, STRING1 )
                          IB = LINDEX ( CDUM, '/' )
                     END IF 
                     ARCNAME = CDUM(IB+1:IB+10)
                     VERSION = DECIMALTOINT ( CDUM(IB+13:IB+15), IERR )
                     FL_GVF = .TRUE.
                   ELSE 
                     CALL SPLITSTRING ( STRING1, TOKEN, STRING1 )
                     VERSION = DECIMALTOINT ( TOKEN, IERR )
                END IF
!
                FL_FOUND = .FALSE.
!
! ------------- Now scan all records in weights file
!
                DO 410 J1=1,L_WEI
                   IF ( .NOT. FL_FOUND ) THEN
                        IF ( SUPNAM_WEI(J1) .EQ. ARCNAME  .AND. &
     &                       SUPVER_WEI(J1) .EQ. VERSION        ) THEN
                             FL_FOUND = .TRUE.
                        END IF
                        IF ( SUPNAM_WEI(J1) .EQ. ARCNAME  .AND. &
     &                       SUPVER_WEI(J1) .EQ. 0              ) THEN
                             FL_FOUND = .TRUE.
                        END IF
                   END IF
 410            CONTINUE
!
                IF ( .NOT. FL_FOUND ) THEN
                     NBAD_ARC = NBAD_ARC + 1
                     IF ( G_WARNING ) THEN
                          WRITE ( 6, 110 ) ARCNAME, VERSION, &
     &                            WEIGHT_FILE(1)(1:I_LEN(WEIGHT_FILE(1)))
 110                      FORMAT ( "BATCH(prces) Warning: Database ",A10," vers ",I2, &
     &                             " was not found in weight file ",A )
                          IF ( ILEN(ARCNAME) .EQ. 0 ) THEN
                               WRITE ( 6, 110 ) STRING(1:I_LEN(STRING))
                          END IF
!
                          INQUIRE ( UNIT=23, OPENED=FL_OPEN )
                          IF ( FL_OPEN ) THEN
                               WRITE ( 23, 110 ) ARCNAME, VERSION
                               IF ( ILEN(ARCNAME) .EQ. 0 ) THEN
                                    WRITE ( 23, 110 ) STRING(1:I_LEN(STRING))
                               END IF
                          END IF
                      END IF
                END IF
!
 810            CONTINUE
                READ ( 92, "(A)", END=60, IOSTAT=IOS ) STRING1
                CALL FERR ( INT2(IOS), "Reading control file", INT2(0), &
     &               INT2(0) )
             ENDDO ! reading weight file
!
 60          CONTINUE
             IF ( NBAD_ARC .GT. 0 ) THEN
                  IF ( WEIGHTS .EQ. 'R' ) then
                       WRITE ( *, * ) NBAD_ARC,' superfiles are without '// &
     &                                         'weights'
                       CALL FERR ( INT2(111), 'Superfile(s) missing from '// &
     &                     'weight file '//WEIGHT_FILE(1), INT2(0), INT2(0) )
                    ELSE IF ( WEIGHTS .EQ. 'U' ) THEN
                       WRITE ( *, * ) NBAD_ARC,' superfiles are without '// &
     &                                         'weights'
                    ELSE IF ( WEIGHTS .EQ. 'R' ) THEN
                       WRITE ( *, "('Continue despite missing weights ', &
     &                              '(Y or N)? [Y] ')" )
                       READ ( *, "(A)" ) RESP
                       IF ( RESP .EQ. 'N' .OR. RESP .EQ. 'n' ) THEN
                            CALL FERR ( INT2(112), &
     &                          'Superfile(s) missing from '//'weight file '// &
     &                           WEIGHT_FILE(1), INT2(0), INT2(0) )
                       ENDIF
                  ENDIF
             ENDIF ! NBAD_ARC > 0
!
! ---------- Positioning at the control file
!
             CALL CFSPOS ( ARCREC )
             WRITE ( 6, '(A)' ) '  Weights file has been read              '
           ELSE 
             DO WHILE ( STRING1(1:1) .NE. '$' )
                IF ( STRING1 .EQ. ' ' ) GOTO 66
!
! ------------- Get superfile name and its version from control file
!
                CALL SPLITSTRING ( STRING1, ARCNAME, STRING1 )
                IF ( ARCNAME == 'GVF' ) THEN
                     FL_GVF = .TRUE.
                END IF
                READ ( 92, "(A)", END=66, IOSTAT=IOS ) STRING1
                CALL FERR ( INT2(IOS), "Reading control file", INT2(0), &
     &               INT2(0) )
             END DO
 66          CONTINUE 
             CALL CFSPOS ( ARCREC )
         ENDIF ! weights
!
         IF ( STS_TRP_FIL == UNDF__TRP         .AND. &
     &        ( TRP_USE == USE__TRP  .OR.            &
     &          TRP_USE == REQ__TRP           )      ) THEN
!
              IUER = -1
              CALL TRP_INIT ( FL_GVF, TRP_DIR, N_FIL_TRP, &
     &                        ADR_TRP_FIL_BUF, STS_TRP_FIL, &
     &                        ADR_TRP_SES_BUF, STS_TRP_SES, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4102, -2, 'PRCES', 'Failure to read '// &
     &                 'directory with external atmosphere path delay '// &
     &                 'files '//TRP_DIR )
                   CALL EXIT ( 1 )
              END IF
              CALL USE_GLBFIL_4 ( 'OWC' )
         END IF
         IF ( RESTRT ) THEN
!
! ----------- Restore positoin in the control file
!
              CALL CFSPOS ( SAVE_CNTREC )
         END IF
!
! ------ Update the progress file
!
         CALL PRGRES ( ARCNUM, IPASS, SOLTY2, DBNAME, VER, DONE, &
     &                 ID, RESTRT, ETIME0, ETIMP0, KFAIL, SOLTYP )
         RESTRT=.FALSE.
!
         DONE=.FALSE.
!
! ------ Open and close the stop file to time-tag it with current time
!
         STOPFILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'stop'//PRE_LETRS
         OPEN ( UNIT=65, FILE=STOPFILE, IOSTAT=IOS )
         CALL FERR ( INT2(IOS), "Opening stop file", INT2(0), INT2(0) )
         CLOSE ( UNIT=65 )
!
! ------ Open spool file. We seek <EOF> in the spool file and position at this
! ------ place. It has sense in restaring mode (<EOF> is written by SAVES
! ------ and keep the last position in spool file before saving) since it
! ------ allows to strip away messages there added after the last solution
! ------ saving
!
         IF ( KSPOOL ) CALL USE_SPOOL ( 'OS' )
         IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! ----------- Timing printout
!
              CALL TIM_GET ( '-BATCH-01' )
         END IF
!
! ------ Loop over all arc lines
!
         IF ( SOLTY2 == 'B' ) THEN
              WRITE ( 6, '(A)' ) '# Solve started the backward run'
            ELSE 
              WRITE ( 6, '(A)' ) '# Solve started processing sessions'
         END IF
         IF ( SOLTYP .EQ. 'G' .AND.      &
     &        CGMNMR .NE. 'NONE'         ) THEN
!
! ----------- A special case: GLOBAL_ONLY solution with the input CGM
!
              DONE = .TRUE.
         END IF 
!
         DO WHILE (.NOT. DONE )

            IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------------- Set timer
!
                 CALL TIM_INIT()
            END IF
            IND_SES = IARCNM + 1
            IF ( IND_SES == -1 ) IND_SES = 1
            IF ( IND_SES == 0  ) IND_SES = 1
            IM = MOD( IND_SES, NUM_PROC )
            IF ( IM == 0 ) IM = NUM_PROC
            IF ( IM .EQ. IND_PROC ) THEN
                 FL_SKIP = .FALSE.
               ELSE 
                 FL_SKIP = .TRUE.
            END IF
!
! --------- Set up the next arc for processing
!
            IF ( .NOT. FL_SKIP ) THEN
                 VTD_ADR_SAVE = VTD_ADR
                 CALL ARCSET ( DONE, ARCNUM, IPASS, SOLTY2, CGMNMR, B_ARCDIR, &
     &                ID, USER_PROG, B_KPERMARC, WEIGHTS, LF_WEI, WEIGHT_FILE, &
     &                STAFLG, FIXSTA_CHR, STADIU, VELFLG, SRCFLG, FIXSRC_CHR, &
     &                PROFLG, NUTFLG, UT1FLG, PRCFLG, RELFLG, IONCTL, &
     &                MINOUT, BASOUT, FWDOUT, SCNOUT, STACRY, &
     &                DBNAME, VER, KLAST, KFAIL, KCORL, IDBNAME, &
     &                QICOV, QJCOV, ATMFLG, INTRVL, FCNPR, QATMCNST, &
     &                CLKPOL_FLG,  CLKPOL_DEG,  CLKFLG, CKNTRVL, QCLKCNST, &
     &                ITARCS, SOLTYP, QCLKEXCPT, &
     &                QATMEXCPT, TBLOUT, AXSFLG, CLKCNS, ATMCNS, EOPSIG, &
     &                EOPCNS, OFFLG, RATFLG, ACCEOP_FLG, IEOP_FLG, &
     &                REOP_FLG, IEOPL_BA, NESM, ESMSITES, ESMDATES, EOPRSIG, EOPRCNS, &
     &                EOPFACT, POSELL, REFREQ, PWCCNS, QPWCCNST, BLCFLG, IOS_EST_BATCH, &
     &                IOS_SIG_BATCH, NEXCBL, IBLNM, BASDF, &
     &                EOPMID, IONFLG, NO_SUPERFILE, USER_BUFF, SOLTYP, RESFILE, &
     &                KMIN_SIG, NUTSIG, NUTCNS, GRADCNS, QGRADCNST, GRADFLG, GRINTRVL, &
     &                KOUTNRM, KZERONRM, KSTACONST, KVELCONST, DBNAME_MES, &
     &                WEIGHT_TYPE_GEN, L_WEI, SUPNAM_WEI, SUPVER_WEI, BASELINE_WEI, &
     &                ARR_WEI, EOP_EPOCH_MJD, EOP_EPOCH_SEC, &
     &                EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI, FL_GVF, GVH, &
     &                PARU_FILE_ARC, WEIGHT_ALGORITHM, VCAT )
                 VTD_ADR = VTD_ADR_SAVE 
            END IF
!
! ------------- Write in the spool file name of user programs, 
! ------------- user_partial programs.
! ------------- This nformation mey be necessary for software which 
! ------------- will be parsing spool files
!
            IF ( .NOT. DONE ) THEN
!%               CALL STATUS_SET ( 'BATCH', STA__BEG )
               IF ( FL_SKIP ) GOTO 820
               IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! ----------------- Timing printout
!
                    CALL TIM_GET ( 'BATCH-02' )
                    CALL TIM_INIT()
               END IF
               IF ( .NOT. KFAIL ) THEN
!
! --------------- Apply pressure-loading correction if requested
!
                  RCOND = 0.D0
                  KPLODCAL = .FALSE.
                  CALL CHAR2HOL ( 'NONE', PLCALF, INT2(1), NAME_SIZE )
                  CALL PLOD_CORR ( PLODCALF )
                  CALL USE_GLBFIL_4 ( 'OWC' )
!
! --------------- We check whether ULCC (User local constraint file) exist
!
                  FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'ULCC'//PRE_LETRS
                  INQUIRE ( FILE=FNAME, EXIST=KEX )
                  IF ( KEX ) THEN
!
! -------------------- If file exist we remove it. The true is that PROC checks
! -------------------- whether this file exist and applies these constraints.
! -------------------- We remove the old version of this file. USER-PROGRAM of
! -------------------- USER-PARTIALS will create it if necessary. We should
! -------------------- avoid the situatiuon when user local constraints are not
! -------------------- applied but the file exists
!
                       CALL BIN_UNLINK ( FNAME, IERR )
                  END IF
!
! --------------- Run user-specified program, if any
!
                  IF ( USER_PROG(1:1) .NE. ' '  .AND.  &
     &                 USER_PROG(1:4) .NE. 'NONE' ) THEN
                     IF ( USER_BUFF(1:1) .EQ. 'Y' ) THEN
                          CALL CHAR2HOL ( USER_BUFF(2:), IBUFF, INT2(1), &
     &                         INT2(80) )
                          CALL USE_BUFFER ( IBUFF, INT2(40), 'OWC' )
                     ENDIF
!
! ------------------ Add '1' to argument list to indicate that program name
! ------------------ includes entire path  (MWH - 910418)
!
                     CALL RUN_PROG ( USER_PROG, 'WAIT', INT2(1) )
!
! ------------------ Re-read socom and parfil since user program may change it
!
                     CALL USE_COMMON ( 'ORC' )
                     SOCOM_PLUS_FIRST = SPL__UNDF
                     CALL SOCOM_EXT()
                     CALL USE_PARFIL ( 'ORC' )
                     CALL PARCN()
!
                     IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! ----------------------- Timing printout
!
                          CALL TIM_GET ( 'BATCH-03' )
                          CALL TIM_INIT()
                     END IF
                  END IF
!
! --------------- Run user_specified partial program, if any
!
                  IF ( KUSER_PART ) THEN
                       CALL RUN_PROG   ( USER_PART_PROG, 'WAIT', INT2(1) )
!
! -------------------- Re-read socom, since user partials program might
! -------------------- change it
!
                       CALL USE_COMMON ( 'ORC' )
                       SOCOM_PLUS_FIRST = SPL__UNDF
                       CALL SOCOM_EXT()
                       CALL USE_GLBFIL ( 'ORC' )
!
                       NUM_USER_GLOB = 0
                       UPT_FLAG = UPT__UND
                       FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRP'//PRE_LETRS
                       OPEN ( 66, FILE=FNAME, IOSTAT=IOS )
                       IF ( IOS .NE. 0 ) THEN
                            CALL FERR ( INT2(IOS), " PRCES Opening user "// &
     &                          "partial file "//FNAME, INT2(0), INT2(0) )
                       END IF
                       CDUM = '*'
                       DO WHILE ( CDUM(1:1) .EQ. '*' )
                          READ ( 66, '(A)', IOSTAT=IOS ) CDUM
                          IF ( IOS .NE. 0 ) THEN
                               CALL FERR ( INT2(IOS), " PRCES (1) Reading "// &
     &                             "user partial file "//FNAME, INT2(0), &
     &                              INT2(0) )
                          END IF
                       ENDDO
!
                       CALL SPLITSTRING ( CDUM, TOKEN, CDUM )
                       NUM_USER_PART = DECIMALTOINT ( TOKEN, IERR )
                       ARC_USER_PART = DECIMALTOINT ( TOKEN, IERR )
                       CALL SPLITSTRING ( CDUM, TOKEN, CDUM )
                       IF ( TOKEN(1:1) .NE. ' ' ) THEN
                            READ ( UNIT=TOKEN, FMT='(I9)', IOSTAT=IOS ) UPT_FLAG
                            IF ( IOS .NE. 0 ) THEN
                                 CALL FERR ( INT2(680), "PRCES(batch) "// &
     &                               "unsupported second word "//TOKEN(1:16)// &
     &                               " in the header of the user partial file", &
     &                                INT2(0), INT2(0) )
                                 WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                                 CALL EXIT ( 1 )
                            END IF
                            IF ( UPT_FLAG .NE. UPT__FUL  .AND. &
     &                           UPT_FLAG .NE. UPT__DEL  .AND. &
     &                           UPT_FLAG .NE. UPT__CMP        ) THEN
                                 CALL FERR ( INT2(690), &
     &                               "PRCES(batch) unsupported "// &
     &                               "second word "//TOKEN(1:16)// &
     &                               " in the header of the user partial file", &
     &                                INT2(0), INT2(0) )
                                 WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                                 CALL EXIT ( 1 )
                            END IF
                       END IF
!
                       DO I=1,ARC_USER_PART
                          CDUM = '*'
                          DO WHILE ( CDUM(1:1) .EQ. '*' )
                             READ ( 66, '(A)', IOSTAT = IOS ) CDUM
                             IF ( IOS .EQ. -1 ) THEN
                                  CALL FERR ( INT2(2783), &
     &                                " PRCES user partials "//"file "// &
     &                                 FNAME(1:I_LEN(FNAME))// &
     &                                " is too short. Please count lines "// &
     &                                "and compare this number with the "// &
     &                                "first line. You see the "// &
     &                                "difference, right?", INT2(0), &
     &                                INT2(0) )
                                  CALL EXIT ( 1 )
                             END IF
                             IF ( IOS .NE. 0 ) THEN
                                  CALL FERR ( IOS, " PRCES (2) Reading user "// &
     &                               "partials file "//FNAME, INT2(0), INT2(0) )
                             END IF
                          ENDDO
                          GA_CHAR = CDUM(22:22)
                          IF ( TRIMLEN(CDUM) .LT. 22 ) GA_CHAR = ' '
                          CALL CASEFOLD ( GA_CHAR )
                          IF ( GA_CHAR .EQ. 'G' ) then
                               NUM_USER_GLOB = NUM_USER_GLOB + 1
                            ELSE IF ( GA_CHAR .NE. 'A' ) THEN
                               CALL FERR ( INT2(7734), &
     &                             'BATCH(prces) Type of user '// &
     &                             'parameter (G/A) not in column 22 of user '// &
     &                             'partials file '//FNAME, INT2(0), INT2(0) )
                          ENDIF
                       ENDDO
                       CLOSE ( UNIT=66 )
!
                       CALL USE_GLBFIL ( 'OWC' )
!
! -------------------- Recompute the total number of parameters
!
                       CALL PARCN()
                       IF ( NPARAM .GT. NRMFL_PARMS ) THEN
                            WRITE (  6, * ) 'ERROR:  BATCH(prces) NPARAM = ', &
     &                                      NPARAM, &
     &                                      ' NUM_USER_PART = ',NUM_USER_PART, &
     &                                      ' ARC_USER_PART = ',ARC_USER_PART
                            WRITE (  6, * ) 'The total number of parameters, '// &
     &                                      'global and local in '//DBNAME_MES// &
     &                                      ' exceeded the limit'
                            WRITE (  6, * ) 'Recommendation: run solve_reset '// &
     &                                      'and increase the "maximum '// &
     &                                      'number of parameters"'
                            WRITE ( 23, * ) 'ERROR:  BATCH(prces) NPARAM = ', &
     &                                      NPARAM, &
     &                                      ' NUM_USER_PART = ',NUM_USER_PART, &
     &                                      ' ARC_USER_PART = ',ARC_USER_PART
                            WRITE ( 23, * ) 'The total number of parameters, '// &
     &                                      'global and local in '//DBNAME_MES// &
     &                                      ' exceeded the limit'
                            WRITE ( 23, * ) 'Recommendation: run solve_reset '// &
     &                                      'and increase the "maximum '// &
     &                                      'number of parameters"'
                            CALL FERR ( INT2(7735), &
     &                          'BATCH(prces) -- total number '// &
     &                          'of parameters exceeded the limit', INT2(0), &
     &                           INT2(0) )
                            WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                            CALL EXIT ( 1 )
                       ENDIF
!
                       CALL USE_COMMON ( 'OWC' )
                       IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! ------------------------- Timing printout
!
                            CALL TIM_GET ( 'BATCH-04' )
                            CALL TIM_INIT()
                       END IF
                  ENDIF ! kuser part
!
! --------------- Determine and save new weights, if requested
!
                  IF ( WEIGHTS .EQ. 'M'  .OR.  &
     &                 WEIGHTS .EQ. 'A'  .OR.  &
     &                 WEIGHTS .EQ. 'T'        ) THEN
                     IF ( WEIGHT_ALGORITHM .EQ. WEIGHT__MYWAY ) THEN
!
! --------------------- Make specified weights by using MYWAY algorithm
! --------------------- First set a glbfil flag which will tell reway
! --------------------- to use the specified weighting type
! --------------------- (by arc, by site) when it iterates
!
                        CALL USE_GLBFIL ( 'OR' )
                        IF ( WEIGHT_TYPE_GEN .EQ. 'S' ) THEN
                             WEIGHTING_TYPE = 'S!'
                          ELSE IF ( WEIGHT_TYPE_GEN .EQ. 'A' ) THEN
                             WEIGHTING_TYPE = 'D!'
                          ELSE
                             WEIGHTING_TYPE = 'B!'
                        ENDIF
                        CALL USE_GLBFIL ( 'WC' )
                        VTD_ADR_SAVE = VTD_ADR
                        CALL RUN_PROG ( 'REWAY', 'WAIT', INT2(0) )
                        VTD_ADR = VTD_ADR_SAVE 
                        INC_VERS = 0
!
! --------------------- Modify the glbfil flag to later tell CRES the weighting
! --------------------- type.
!
                        CALL USE_GLBFIL ( 'OR' )
                        IF ( WEIGHT_TYPE_GEN .EQ. 'S' ) THEN
                             WEIGHTING_TYPE = 'ST'
                          ELSE IF ( WEIGHT_TYPE_GEN .EQ. 'A' ) THEN
                             WEIGHTING_TYPE = 'DB'
                          ELSE
                             WEIGHTING_TYPE = 'BL'
                        ENDIF
                        CALL USE_GLBFIL ( 'WC' )
                      ELSE IF ( WEIGHT_ALGORITHM .EQ. WEIGHT__UPWEI      .OR. &
     &                          WEIGHT_ALGORITHM .EQ. WEIGHT__UPWEI_OPT  .OR. &
     &                          WEIGHT_ALGORITHM .EQ. WEIGHT__ELIM    ) THEN
!
! --------------------- Compute quadratic correction to formal uncertainties
! --------------------- by using UPWEI or UPWEI_OPT algorithms
!
                        CALL USE_GLBFIL ( 'OR' )
                        IF ( WEIGHT_ALGORITHM .EQ. WEIGHT__UPWEI ) THEN
                             EQUMEM_FLAG = .FALSE.
                           ELSE IF ( WEIGHT_ALGORITHM .EQ. WEIGHT__UPWEI_OPT) &
     &                     THEN
                             EQUMEM_FLAG = .TRUE.
                           ELSE IF ( WEIGHT_ALGORITHM .EQ. WEIGHT__ELIM ) THEN
                             EQUMEM_FLAG = .TRUE.
                        END IF
                        CALL USE_GLBFIL_4 ( 'R' )
                        ELIM_AMB = .FALSE.
                        ELIM_ION = .FALSE.
                        CALL USE_GLBFIL_4 ( 'W' )
                        CALL USE_GLBFIL ( 'WC' )
!
                        IF ( WEIGHT_ALGORITHM .EQ. WEIGHT__ELIM ) THEN
                             IUER = -1
                             IF ( ILEN(PARU_FILE_ARC) > 0 ) THEN
                                  PARU_FILE_USE = PARU_FILE_ARC
                                ELSE 
                                  PARU_FILE_USE = PARU_FILE
                             END IF
!
! -------------------------- Call routine for outliers elimination, 
! -------------------------- reweigting and phase delay ambiguity resolution
!
                             CALL STATUS_SET ( 'ELIM', STA__BEG )
                             CALL PARU_DO ( PARU_FILE_USE, GVH, 3, INC_VERS, &
     &                                      IUER )
                             IF ( IUER .NE. 0 ) THEN
                                  CALL CLRCH ( STR )
                                  CALL INCH  ( INT4(ARCNUM), STR )
                                  IUER = -1
                                  CALL ERR_LOG ( 4103, IUER, 'PRCES', &
     &                                'Error in attempt to run PARU for '// &
     &                                'the '//STR(1:I_LEN(STR))// &
     &                                '-th database '//DBNAME_CH )
                                  CALL EXIT ( 1 )
                             END IF
                           ELSE 
!
! -------------------------- Invoke program ELIM which would call UPWEI_DO
! -------------------------- for making actual job
!
                             CALL STATUS_SET ( 'ELIM', STA__BEG )
                             CALL NOUT ( 128, IBUFF )
                             IBUFF(1) = INT2(1)
                             CALL USE_BUFFER ( IBUFF, INT2(64), 'OWC' )
                             CALL RUN_PROG   ( 'ELIM', 'WAIT', INT2(0) )
                             INC_VERS = 0
                        END IF
                     END IF
!
! ------------------ Get weights
!
                     CALL GET_WEIGHTS ( CONSTANTS, NBLINE )
                     APPEND_FLAG = 'A'
                     IF ( ARCNUM.EQ.1 .AND. WEIGHTS.EQ.'M' ) APPEND_FLAG=' '
!
! ------------------ Get the weights from the namfil and save them in the weight
! ------------------ file.
!
                     WEI_FILE_LOCK = PRE_SCR_DIR(1:PRE_SD_LEN)// &
     &                               'LOWE'//PRE_LETRS
!
! ------------------ Set lock to weights file
!
                     IUER = -1
                     CALL SET_FILE_LOCK ( WEI_FILE_LOCK, WEI_POLL_INT, &
     &                                    WEI_MAX_INT, IUER )
                     IF ( IUER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4104, -2, 'PRCES', &
     &                                'Error in waiting for weights file lock' )
                          CALL EXIT ( 1 ) 
                     END IF
!
                     CALL SAVE_WEIGHTS ( DBNAME_CH, DBNAME_VER, WEIGHT_FILE, &
     &                                   CONSTANTS, APPEND_FLAG, &
     &                                   WEIGHT_TYPE_GEN, NBLINE, NUMSTA, &
     &                                   ISITN, INC_VERS )
                     IF ( WEIGHTS == 'T' ) THEN
                          IUER = -1
                          CALL SORT_WEIGHTS ( WEIGHT_FILE, WEIGHT_TYPE_GEN, &
     &                                        IUER )
                          IF ( IUER .NE. 0 ) THEN
                               CALL UNLINK ( WEI_FILE_LOCK(1:I_LEN(WEI_FILE_LOCK))//CHAR(0) )
                               CALL ERR_LOG ( 4105, -2, 'PRCES', &
     &                             'Error in attempt to sort weights '// &
     &                             'file '//WEIGHT_FILE(1) )
                               WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                               CALL EXIT ( 1 )
                          END IF
                     END IF
!
! ------------------ Remove the weight lock file
!
                     CALL UNLINK ( WEI_FILE_LOCK(1:I_LEN(WEI_FILE_LOCK))//CHAR(0) )
                  ELSE
!
! ------------------ Do least squares solution for this arc
!
                     IF ( TRAIN ) THEN
!
! --------------------- Call old-fashioned scheduler which in turn will call
! --------------------- a train of standalone programms for making some steps
! --------------------- for least square solution
!
                        VTD_ADR_SAVE = VTD_ADR
                        CALL RUN_PROG ( 'GLOBL', 'WAIT', INT2(0) )
                        VTD_ADR = VTD_ADR_SAVE 
                      ELSE
                        IF ( ISLTY2 .EQ. 'F' ) THEN
#ifdef DEBUG
   write ( 6, * ) 'prces-815    GLBMEM%L_GPA= ', GLBMEM%L_GPA, ' db= ', DBNAME_MES, ' L_GPA= ', GLBMEM%L_GPA, ' ARCNUM= ', ARCNUM ! %%%%
#endif
!
! ------------------------- Forward run
!
                            CALL STATUS_SET ( 'FORW', STA__BEG )
                            IUER = -1
                            CALL GLO_FORW ( INT4(ARCNUM), DBNAME_MES, GLBMEM, &
     &                                      B3DOBJ, B1B3DOBJ, CNSTROBJ, IUER )
                            IF ( IUER .NE. 0 ) THEN
                                 CALL CLRCH ( STR )
                                 CALL INCH  ( INT4(ARCNUM), STR )
                                 CALL ERR_LOG ( 4106, -2, 'PRCES', &
     &                               'Error in processing the '// &
     &                                STR(1:I_LEN(STR))//'-th database '//DBNAME_MES// &
     &                               ' in forward run of '//'global solution' )
                                 WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                                 CALL EXIT ( 1 )
                            END IF
#ifdef DEBUG
   write ( 6, * ) 'prces-832    GLBMEM%L_GPA= ', GLBMEM%L_GPA ! %%%%
#endif
                          ELSE IF ( ISLTY2 .EQ. 'B' ) THEN
!
! ------------------------- Backward run
!
                            IF ( (ISOLU .EQ. 1  .AND.  IARCNM .EQ. 1 ) .OR. &
     &                           (ISOLU .EQ. 0  .AND.  ICONT  .EQ. 0 ) ) THEN
                                 CALL STATUS_SET ( '-NORML', STA__BEG )
!
! ------------------------------ CGM inverse
!
                                 IUER = -1
                                 CALL GLO_NORM ( DBNAME_MES, GLBMEM, B3DOBJ, &
     &                                           CNSTROBJ, IUER )
                                 IF ( IUER .NE. 0 ) THEN
                                      CALL ERR_LOG ( 4107, -2,'PRCES', &
     &                                    'Error in inversion of CGM' )
                                      WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                                      CALL EXIT ( 1 )
                                 END IF
                            END IF
!
! ------------------------- Compute local parameters for the ARCNUM session
! ------------------------- in backward run
!
                            CALL STATUS_SET ( 'BACK', STA__BEG )
                            IUER = -1
                            CALL GLO_BACK ( INT4(ARCNUM), DBNAME_MES, GLBMEM, &
     &                                      B3DOBJ, B1B3DOBJ, CNSTROBJ, IUER )
                            IF ( IUER .NE. 0 ) THEN
                                 CALL CLRCH ( STR )
                                 CALL INCH  ( INT4(ARCNUM), STR )
                                 CALL ERR_LOG ( 4108, -2, 'PRCES', &
     &                               'Error in processing the '// &
     &                                STR(1:I_LEN(STR))//'-th database '//DBNAME_MES// &
     &                               ' in backward run of '//'global solution' )
                                 WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                                 CALL EXIT ( 1 )
                            END IF
                          ELSE IF ( ISLTY2 .EQ. 'I' ) THEN
!
! ------------------------- Independnent run
!
                            CALL STATUS_SET ( 'INDP', STA__BEG )
                            IUER = -1
                            CALL INDP ( INT4(ARCNUM), DBNAME_MES, GLBMEM, &
     &                                  B3DOBJ, B1B3DOBJ, CNSTROBJ, IUER )
                            IF ( IUER .NE. 0 ) THEN
                                 CALL CLRCH ( STR )
                                 CALL INCH  ( INT4(ARCNUM), STR )
                                 IUER = -1
                                 CALL ERR_LOG ( 4109, IUER, 'PRCES', &
     &                               'Error in processing the '// &
     &                                STR(1:I_LEN(STR))//'-th database '//DBNAME_MES// &
     &                               ' in independent solution' )
                                 WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                                 CALL EXIT ( 1 )
                            END IF
                        END IF ! islty2
                     END IF ! train
                 ENDIF  ! weights
!
                 IF ( FL_GVF ) THEN
                      IUER = -1
                      CALL GVH_RELEASE ( GVH, IUER )
                      IF ( IUER .NE. 0 ) THEN
                           CALL ERR_LOG ( 4110, -2, 'PRCES', &
     &                         'Failure in an attempt to release '// &
     &                         'dynamic memory grabbed by GVH' )
                           WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                           CALL EXIT ( 1 )
                      END IF
                      IF ( VTD_ADR .NE. 0 ) THEN
                           IUER = 0
                           CALL VTD_QUIT ( %VAL(VTD_ADR), IUER )
                      END IF
                 END IF
             ENDIF
             IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! --------------- Set timer
!
                  CALL TIM_INIT()
             END IF
 820         CONTINUE 
!
! ---------- Update progress file
!
             IF ( FL_SKIP ) THEN
                  IF ( IARCNM == -2 ) IARCNM = 0
                  IF ( ARCNUM == -2 ) ARCNUM = 0
                  ARCNUM = ARCNUM + 1
                  IARCNM = IARCNM + 1
                  CALL CLRCH  ( STRING )
                  LENGTH=CFREAD(STRING)
                  DONE = STRING(1:1).EQ.'$' .OR. CFEOF(IDUM)
                  CALL USE_GLBFIL ( 'OWC' ) 
                ELSE 
                  VTD_ADR_SAVE = VTD_ADR 
                  CALL PRGRES ( ARCNUM, IPASS, SOLTY2, DBNAME, VER, KLAST, &
     &                          ID, RESTRT, ETIME0, ETIMP0, KFAIL, SOLTYP )
                  VTD_ADR = VTD_ADR_SAVE
             END IF
!
             IF ( MOD ( IARCNM, SAVING_RATE ) .EQ. 0  .OR. &
     &            ISLTY2 .NE. 'F' ) THEN
!
! --------------- Save the state of the program in order to recover lately,
! --------------- if the index of the arc is a multiple of SAVING_RATE
!
                  IF ( .NOT. TRAIN     .AND. &   ! NO TRAIN mode
     &                 ISLTY2 .EQ. 'F' .AND. &   ! forward run
     &                 .NOT. SLAST              ) THEN ! not the last session
!
! -------------------- Special saving solution in NO TRAIN mode of forward
! -------------------- run.
! -------------------- Saving CGM and the list of parameters in CGMBxx file
! -------------------- in NO TRAIN more
!
                       IUER = -1
                       CALL GLO_SAVE ( 1, GLBMEM, DBNAME_MES, IUER )
                       IF ( IUER .NE. 0 ) THEN
                            CALL ERR_LOG ( 4111, -2, 'PRCES', &
     &                          'Error during attempt to save temporary CGM '// &
     &                          'on disk after processing database '//DBNAME_MES )
                            RETURN
                       END IF
                     ELSE
!
! -------------------- Usual saving solution. Setting variables in
! -------------------- glbcm.i block indicating that CGM is saved. Then
! -------------------- temporary CGM is copied from CGMFxx to CGMBxx flag.
!
                       CALL SAVES ( KMORE, ETIME0, ETIMP0, SCNOUT, KCORL, &
     &                              IEOPL_BA, DBNAME_MES, LENCNT, LENARC )
                  END IF
!
                  INQUIRE ( FILE=STOPFILE, EXIST=KEX )
                  IF ( .NOT. KEX ) THEN
                       CALL FATAL ( STOPFILE(1:TRIMLEN(STOPFILE))//' missing' )
                  ENDIF
                  CALL SAVE_CFNAME ( CFNAME )
!
! --------------- Reopenning spool file anew since SAVE closes it
!
                  CALL USE_SPOOL ( 'O' )
                ELSE IF ( IARCNM .EQ. 1 ) THEN
!
! --------------- Save the name of control file if we process the first session
!
                  CALL SAVE_CFNAME ( CFNAME )
             END IF
             IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! --------------- Timing printout
!
                  CALL TIM_GET ( 'BATCH-05' )
             END IF
!
             IF ( ISLTY2 .EQ. 'F' .AND. SLAST  ) THEN
!
! --------------- The last session is processed -- then write down CGM filename
!
                  CGMZ_FIL = PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMZ'//PRE_LETRS
!
! --------------- Put the name of the output CGM in STR
!
                  CALL CLRCH ( STR )
                  STR = ONAMCG
!
! --------------- If file exists remove it
!
                  INQUIRE ( FILE=CGMZ_FIL, EXIST=LEX )
                  IF ( LEX ) THEN
                       CALL BIN_UNLINK ( CGMZ_FIL, IERR )
                  END IF
!
! --------------- ... and write down CGM filename
!
                  IUER = -1
                  CALL WR_TEXT ( 1, STR, CGMZ_FIL, IUER )
                  IF ( IUER .NE. 0 ) THEN
                       CALL ERR_LOG ( 4112, -2, 'PRCES', &
     &                     'Error during attempt to write the name of '// &
     &                     'the CGM into file '//CGMZ_FIL )
                       RETURN
                  END IF
             END IF
!
             IF ( MEMORY_DEBUG_FLAG  .EQ.  MEMORY_DEBUG__YES ) THEN
!
! --------------- Special debugging printout. Activated when environment
! --------------- variable MEMORY_DEBUG is "YES"
!
                  OPEN ( UNIT=88, FILE='/tmp/mem.mem', STATUS='UNKNOWN', &
     &                   ACCESS='APPEND', IOSTAT=I88 )
#ifdef LINUX
                  MEM_RSS = GET_MEMRSS()
                  WRITE ( 88, 210 ) DBNAME_MES, MEM_RSS
 210              FORMAT ( ' BATCH ended: session: ',a, ' rss_mem: ', I18 )
#else
                  WRITE ( 88, 220 ) DBNAME_MES
 220              FORMAT ( ' BATCH ended: session: ',a )
#endif
                  CLOSE ( UNIT=88 )
                  WRITE ( 6, * ) ' '
                  WRITE ( 6, * ) ' dbname_mes = ',dbname_mes
#ifdef HPUX
                  CALL MEMORYMAP ( %VAL(1) )
#endif
             END IF
           ENDIF ! Done
         ENDDO  ! end of loop on arcs
!
! ------ Set KMORE so we won't recover
!
         IPASS = IPASS+1
         KMORE = ( IPASS.LE.2 .AND. SOLTYP.EQ.'C' ) .OR. &
     &           ( IPASS.EQ.1 .AND. SOLTYP.NE.'C' )
      ENDDO  ! Kmore -- loop over sessions
!
      IF ( SOLTYP .EQ. 'G'  .AND.  .NOT. TRAIN ) THEN
           CALL STATUS_SET ( 'NORML', STA__BEG )
!
! -------- Set flags stating that now we will deal with CGM
!
           CALL USE_GLBFIL ( 'OR' )
           ICONT = 0
           ISOLU = 1
           CALL USE_GLBFIL ( 'WC' )
!
! -------- Open spool file if needed
!
           IF ( KSPOOL ) CALL USE_SPOOL ( 'OS' )
!
! -------- CGM inverse
!
           IF ( SOLTYP .EQ. 'G' .AND.  CGMNMR .NE. 'NONE' ) THEN
                DBNAME_MES = 'CGM'
              ELSE
                DBNAME_MES = 'cgm'
           END IF
!
           IUER = -1
           CALL GLO_NORM ( DBNAME_MES, GLBMEM, B3DOBJ, CNSTROBJ, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4113, -2, 'PRCES', 'Error in '// &
     &              'inversion of CGM' )
                WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                CALL EXIT ( 1 )
           END IF
!
! -------- Compute global parameters
!
           IF ( SOLTYP .EQ. 'G' .AND.  CGMNMR .NE. 'NONE' ) THEN
                DBNAME_MES = 'CGM'
              ELSE
                DBNAME_MES = 'cgm'
           END IF
           IUER = -1
           CALL GLO_GLOBAL ( DBNAME_MES, GLBMEM, B3DOBJ, B1B3DOBJ, CNSTROBJ, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4114, -2, 'PRCES', 'Error in '// &
     &              'computation of global parameters just after '// &
     &              'inversion of CGM' )
                WRITE ( 6, '(A)' ) 'BATCH(prces): abnormal termination'
                CALL EXIT ( 1 )
           END IF
           CALL STATUS_SET ( 'NORML', STA__END )
      END IF
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_INIT()
      END IF
!
! --- Saving the status of the solution once more. We have to write a flag of
! --- that the soliution is over and no recovering is needed.
!
      CALL SAVES ( KMORE, ETIME0, ETIMP0, SCNOUT, KCORL, IEOPL_BA, DBNAME_MES, &
     &             LENCNT, LENARC )
!
      IF ( SOLTYP .EQ. 'C' .OR.  SOLTYP .EQ. 'B'  .OR. &
     &     SOLTYP .EQ. 'S' .OR.  SOLTYP .EQ. 'G'       ) THEN
!
! -------- Run the program which computes statistics
!
           IF ( TRAIN ) THEN
                CALL RUN_PROG ( 'HAUSR', 'WAIT', INT2(0) )
             ELSE
!
! ------------- Call routines which control processing of SARFxx information
!
                CALL OUTFL_HAUSR ( 'O' )
!
                CALL USE_GLBFIL   ( 'OR' )
                CALL USE_GLBFIL_2 ( 'R'  )
                CALL USE_GLBFIL_4 ( 'RC' )
!
                CALL CUMULOOP ( INT2(1) )
                CALL OUTWITHITALL()
                CALL OUTFL_HAUSR ( 'C' )
           END IF
      ENDIF
      CALL APND_2_SPOOL ( IWARNING, BUF_WARNING )
!
! --- Close spool file
!
      IF ( KSPOOL ) CALL USE_SPOOL ( 'C' )
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Timing printout
!
           CALL TIM_GET ( 'BATCH-06' )
      END IF
!
      RETURN
      END  !#!  PRCES  #!#
