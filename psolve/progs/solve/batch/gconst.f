      SUBROUTINE GCONST()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GCONST PROGRAM SPECIFICATION
!
! 1.1 Parse the CONSTRAINTS section of the control file
!
! 1.2 REFERENCES:
!
! 2.  GCONST INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
!
! 2.3 OUTPUT Variables:
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'ba2cm.i'
      INCLUDE 'dmapp.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrlfl
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER STRINH*256, TOKEN*80, STR*128
      INTEGER*2 LENGTH, IDUM, I, IERR, J, ICT
      LOGICAL*2 KNON, KVAL, KEY_OK, WAS_SIGMA
      LOGICAL*2 KNNTPS, KNNRPS, KNNTVS, KNNRVS, KNNRSS, KNNRPP, KSOUADM, KIOS, KPRP
      INTEGER*2 IBITD, IWORDE, IEXCEPTSZ, M_KEYS
      PARAMETER  ( M_KEYS = 10 )
      CHARACTER OPT_ID*128, SECT_ID*11, KEYS_ARRAY(M_KEYS)*40, ERRBUF*255
      INTEGER*2 NN, NUM_KEYS, IKEYS, IND_RTP, ISTASP_BEG, ISRCSP_BEG
      LOGICAL*2 FL_RTP(3)
!
!  EXTERNAL FUNCTIONS
!
      LOGICAL*2 KBIT
      INTEGER*2 CFREAD, TRIMLEN
      LOGICAL*2 CFEOF
      INTEGER*4 IOS, IUER
      INTEGER*2 INT2_ARG
      INTEGER*4  I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE  910809  Fixed eopmod file test. It didn't correctly test for having
!        eopmod file and earth orientation constraints.
!   mwh  910813  Added eop rate constraints
!   kdb  961125  New suppression options,
!                no net rotation and translation positions and velocities.
!   kdb  970115  Fifth new constraint (no net rotation sources)
!                Also correct two errors in the 11/25/96 implementation of
!                the no net rot/trans positions/velocities (defvel and defcmp
!                should be initialized here, not later after this subroutine
!                has set information in them.)
!  kdb 970312 Implement the ability to use weighted or uniform no net
!               constraints via the batopt file.
!             Also implement in batopt the ability to use the horizontal
!               projection matrix vs. just the identity matrix for the no
!               net constraints.
!  kdb 980213 Set interlock: flag attempts to turn on the
!             no_net_rotation_source constraint when sources aren't being
!             estimated.
!  pet 980330 Made specification of type of NNT_POS or NNT_VEL constraint:
!             HORIS or ALL mandatory
!  pet 980722 Substantial changes. Changed comments throught.
!             Added qialifier BASELINE_CLOCK
!             Added capacity to specify values of sigmas:
!             bas_clk_sigma, src_coo_sigma, sta_wea_sigma, nnt_pos_sigma
!             nnr_pos_sigma, nnt_vel_sigma, nnr_vel_sigma, nnr_src_sigma
!  pet 980723 Corrected a bug: error message should not been issued when
!             BASELINE_CLOCK, SOURCE or STATION have only qualifier YES
!             without SIGMA
!  pet 990103 Changed a bit logic of parsing NO_NET_xxxx keywords to avoid
!             "Segmentation fault" in some cases when control file had
!             wrong syntax. Constructions like NO_NET_xxx NO become to be
!             supported.
!
!  pet 2000.07.25  Added support of qualifier IN in EARTH_ORIENTATION section
!
!  pet 2000.07.25  Added support of qualifier SIGMA in EARTH_ORIENTATION section
!
!  pet 2000.09.26  Added support of qualifier SIGMA for keyword NUTATION,
!                  improved error messages
!  pet 2000.11.21  Improved comments.
!  pet 2002.03.13  Added support of qualiferis GLO, LOC in keywords
!                  NO_NET_TRANSLATION_POSITION, NO_NET_ROTATION_POSITION,
!                  NO_NET_ROTATION_SOURCE
!  pet 2002.04.03  Fixed a typo related to NNR_POS_LOC constraits
!  pet 2002.05.10  Added support of qualifiers RIGHT_PART in keywords
!                  NO_NET_TRANSLATION_POSITION, NO_NET_ROTATION_POSITION,
!                  NO_NET_ROTATION_SOURCE
!  pet 2005.08.12  Fixed a bug: the previous version did not initialized &
!                  sigmas of constraints on source coordinates and/or &
!                  station positions
!  pet 2007.08.09  Added support of constraints on source structure admittance
!  pet 2022.08.22  Added support of constraints on ionospheric path delay
!  pet 2023.02.26  Added support of right hand side for no net rottion for
!                  source positions and proper motions
!  pet 2023.03.12  Added support of settings reciprocal weights constraints on proper motions
!
!CCCC
      DATA KNNTPS  / .FALSE. /, KNNRPS / .FALSE. /, KNNTVS / .FALSE. /, &
     &     KNNRVS  / .FALSE. /, KNNRSS / .FALSE. /, KNNRPP / .FALSE. /, &
     &     KSOUADM / .FALSE. /, KIOS   / .FALSE. /, KPRP   / .FALSE. /
!
! 5.  GCONST PROGRAM STRUCTURE
!
! Initialize some things
!
      SECT_ID = 'CONSTRAINTS'
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_4 (  'R' )
!
! --- Default constraints
!
      KSRC_CONST = .FALSE. ! Weak constraints on source positions
      KPRP_CONST = .FALSE. ! Weak constraints on source proper motions
      KBSL_CONST = .TRUE.  ! Baseline dependent clocks
!
      CALL USE_GLBFIL   ( 'WC' )
!
      DO J=5,6
         DO I=1,STA_BIT_WORDS
            CMPSUP(I,J)=0
            VELSUP(I,J)=0
         ENDDO
      ENDDO
      DO I=1,SRC_BIT_WORDS
         SOUSUP(I,7)=0
         SOUSUP(I,8)=0
      ENDDO
!
      CLKCNS  = 'N'
      ATMCNS  = 'N'
      EOPCNS  = 'N'
      NUTCNS  = 'N'
      EOPRCNS = 'N'
      PWCCNS  = 'N'
!
! --- Set default forms of no net translation and no net rotation position,
! ---     velocity and source constraints to be used.
! ---     Kmatrix... = "HR" uses the horizontal projection matrix,
! ---                  not the identity matrix.
! ---     Ksig_scale... = "UN"  uses uniform constraints, not weighted ones
!
      KMATRIX_NNTP = "HR"
      KMATRIX_NNTV = "HR"
      KSIG_SCALE_NNTP = "UN"
      KSIG_SCALE_NNTV = "UN"
      KSIG_SCALE_NNRP = "UN"
      KSIG_SCALE_NNRV = "UN"
      KSIG_SCALE_NNRS = "UN"
!
! --- Initialize variables used by both the $CONSTRAINTS and $SUPPRESSION
! --- sections.
!
      KCENTERMASS = .FALSE.
      KSTACONST   = .FALSE.
      KVELCONST   = .FALSE.
      STA_WT_FIL  = STATION_WEIGHT_FILE
      DO I=1,3
         EOPSIG(I) =0.0
         EOPRSIG(I)=0.0
      ENDDO
      NUTSIG(1) = 0.D0
      NUTSIG(2) = 0.D0
      REFREQ    = 'Y'
      KNON      = .FALSE.
      EOPFACT   = 0.0D0
      IOS_SIG_BATCH = 0.0D0
!
      DO I=1,3
         STAXYZ_CNST(I) = 0
         STAUEN_CNST(I) = 0
         VELXYZ_CNST(I) = 0
         VELUEN_CNST(I) = 0
      ENDDO
      DO ICT=1,STA_BIT_WORDS
         STAXYZ_CNSB(ICT) = 0
         STAUEN_CNSB(ICT) = 0
         VELXYZ_CNSB(ICT) = 0
         VELUEN_CNSB(ICT) = 0
      ENDDO
      STAXYZ_CNFL = .FALSE.
      STAUEN_CNFL = .FALSE.
      VELXYZ_CNFL = .FALSE.
      VELUEN_CNFL = .FALSE.
!
      SRC_COO_SIGMA  = 0.0D0
      SRC_PRP_SIGMA  = 0.0D0
      STAXYZ_CNST(1) = 0.0D0
      STAXYZ_CNST(2) = 0.0D0
      STAXYZ_CNST(3) = 0.0D0
      STAUEN_CNST(1) = 0.0D0
      STAUEN_CNST(2) = 0.0D0
      STAUEN_CNST(3) = 0.0D0
!
      CALL NOUT_R8 ( 3, NNT_POS_RTP )
      CALL NOUT_R8 ( 3, NNR_POS_RTP )
      CALL NOUT_R8 ( 3, NNT_VEL_RTP )
      CALL NOUT_R8 ( 3, NNR_VEL_RTP )
      CALL NOUT_R8 ( 3, NNR_SRC_RTP )
      CALL NOUT_R8 ( 3, NNR_PRP_RTP )
!
      DEFNUVCOV   = 0
      NUVEL_WT    = 1.0D0
      SOU_ADM_CNS = 0.0D0
!
      CALL USE_GLBFIL_4 ( 'OWC' )
!
! --- Read first line of the CONSTRAINTS section
!
      LENGTH=CFREAD(STRINH)
      DO WHILE ( STRINH(1:1) .EQ. ' '  .AND. .NOT. CFEOF(IDUM) )
         DO WHILE ( TRIMLEN(STRINH) .GT. 0 )
            CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
            IF (TOKEN .NE. 'NONE') THEN
            KVAL = .TRUE.
!
            IF ( TOKEN .EQ. 'EARTH_ORIENTATION' ) THEN
!
! -------------- EARTH ORIENTATION keyword
!
                 EOPFACT = 0
                 CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                 IF ( TOKEN .EQ. 'YES') THEN
                      EOPCNS='Y'
                      IF ( (EOPMAP.EQ.' ' .OR. EOPMAP .EQ. 'NONE' ) .AND. &
     &                     (STRINH.EQ.' ' .OR. STRINH(1:6).EQ.'FACTOR' ) ) THEN
                         CALL FERR ( INT2(8201), 'No mapping file for eop '// &
     &                       'constraints', INT2(0), INT2(0) )
                      END IF
!
                      IF ( STRINH(1:6) .EQ. 'FACTOR' ) THEN
                           CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                           CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                           READ ( TOKEN, *, IOSTAT=IOS ) EOPFACT
                           IF ( IOS .NE. 0 ) THEN
                                CALL FERR ( INT2(8202), "Decoding eop "// &
     &                              "FACTOR: "//TOKEN(1:16), INT2(0), INT2(0) )
                           END IF
                        ELSE
                           IF ( STRINH .NE. ' ' ) THEN
                              CALL CLRCH ( STR )
                              STR = STRINH
                              CALL CHASHL ( STR )
                              IF ( STR(1:5) .EQ. 'SIGMA' ) THEN
                                   CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                              END IF
                              DO I=1,3
                                 CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                                 READ ( TOKEN, *, IOSTAT=IOS ) EOPSIG(I)
                                 IF ( IOS .NE. 0 ) THEN
                                      CALL FERR ( INT2(8203), 'Decoding EOP '// &
     &                                    'constraint: '//TOKEN(1:16), &
     &                                     INT2(0), INT2(0) )
                                 ENDIF
                              ENDDO
!
! --------------------------- EOPSIG: masec, masec, msec
!
                              EOPSIG(3) = EOPSIG(3)*1000.
                           ENDIF
                      ENDIF
                   ELSE IF ( TOKEN .EQ. 'IN' ) THEN
                      EOPCNS='Y'
                      IF ( EOPMAP.EQ.' ' .OR. EOPMAP .EQ. 'NONE' ) THEN
                         CALL FERR ( INT2(8204), &
     &                       'GCONST (EARTH_CONSTAINT IN): '// &
     &                       'No mapping file for eop constraints', INT2(0), &
     &                        INT2(0) )
                      END IF
                   ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                      EOPCNS='N'
                   ELSE
                      CALL FERR ( INT2(8205), &
     &                    'Unknown eop constraint keyword: '//TOKEN(1:16), &
     &                     INT2(0), INT2(0) )
                 END IF ! TOKEN
!
                 IF ( STRINH(1:6) .EQ. 'FACTOR' ) THEN
                      CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                      CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                      READ ( TOKEN, *, IOSTAT=IOS ) EOPFACT
                      IF ( IOS .NE. 0 ) THEN
                           CALL FERR ( INT2(8206), "Decoding eop FACTOR "// &
     &                          TOKEN(1:16), INT2(0), INT2(0) )
                      END IF
                 ENDIF
!
                 IF ( STRINH .NE. ' ' ) THEN
                      CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                      IF ( TOKEN .EQ. 'RATES' ) THEN
                           EOPRCNS = 'Y'
                           IF ( STRINH .NE. ' ' ) THEN
                                CALL CLRCH ( STR )
                                STR = STRINH
                                CALL CHASHL ( STR )
                                IF ( STR(1:5) .EQ. 'SIGMA' ) THEN
                                     CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                                END IF
                                DO I=1,3
                                   CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                                   READ ( TOKEN, *, IOSTAT=IOS ) EOPRSIG(I)
!
! -------------------------------- EOPRSIG: masec/day, masec/day, msec/day
!
                                   IF ( IOS .NE. 0 ) THEN
                                        CALL FERR ( INT2(8207), 'Decoding '// &
     &                                      'EOP constraint: '//TOKEN(1:16), &
     &                                       INT2(0), INT2(0) )
                                   ENDIF
                                ENDDO
                                EOPRSIG(3) = EOPRSIG(3)*1000.0
                           ENDIF
                        ELSE IF  ( TOKEN .EQ. 'NO' ) THEN
                          EOPRCNS = 'N'
                        ELSE
                          CALL FERR ( INT2(8208), 'Unknown eop constraint '// &
     &                        'keyword: '//TOKEN(1:16), INT2(0), INT2(0) )
                      ENDIF ! token
                 ENDIF ! strin
            ELSE IF ( TOKEN .EQ. 'NUTATION' ) THEN
!
! ----------- NUTATION keyword
!
              CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
              IF ( TOKEN .EQ. 'YES' ) THEN
                   NUTCNS = 'Y'
                   IF ( ( NTSMAP .EQ. ' ' .OR. NTSMAP .EQ. 'NONE' ).AND. &
     &                  ( STRINH .EQ. ' ' .OR. STRINH(1:6).EQ.'FACTOR') ) THEN
                        CALL FERR ( INT2(8209), &
     &                      'No mapping file for nutation '//'constraints '// &
     &                       TOKEN(1:16), INT2(0), INT2(0) )
                   END IF
!
                   IF ( STRINH .NE. ' ' ) THEN
                        DO I=1,2
                           CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                           IF ( TOKEN .EQ. 'SIGMA' .OR. TOKEN.EQ. 'sigma' ) THEN
                                CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                           END IF
!
                           READ ( TOKEN, *, IOSTAT=IOS ) NUTSIG(I)
!
! ------------------------ NUTSIG: masec, masec
!
                           IF ( IOS .NE. 0 ) THEN
                                CALL FERR ( INT2(8210), 'Decoding NUTATION '// &
     &                              'constraint: '//TOKEN(1:16), INT2(0), &
     &                               INT2(0) )
                           ENDIF
                        ENDDO
                   ENDIF
                ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                   NUTCNS='N'
                ELSE
                   CALL FERR ( INT2(8211), &
     &                 'Unknown nutatin constraint keyword: '//TOKEN(1:16), &
     &                  INT2(0), INT2(0) )
              END IF  ! TOKEN
            ELSE IF ( TOKEN .EQ. 'ATMOSPHERES' ) THEN
!
! ----------- ATMOSPHERES keyword
!
              CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
              IF ( TOKEN .EQ. 'YES'  .OR.  &
     &             TOKEN .EQ. 'MOST' .OR.  &
     &             TOKEN .EQ. 'WEAKEST'    ) THEN
                   ATMCNS=TOKEN(1:1)
                   IF ( ATMCNS(1:1) .EQ. 'W' ) ATMCNS(1:1) = 'M'
                   CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                   IF ( TOKEN .NE. ' ' ) THEN
                        READ ( TOKEN, *, IOSTAT=IOS ) QATMCNST(1)
                        IF ( IOS .NE. 0 ) THEN
                             CALL FERR ( INT2(8212), '(1) Error decoding '// &
     &                           'atmosphere constraint '//TOKEN(1:16), INT2(0), &
     &                            INT2(0) )
                        END IF
                        CALL CHAR2HOL ( '        ', QATMEXCPT, INT2(1), &
     &                                  INT2(8) )
                        CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                        IF ( ATMCNS .EQ. 'Y'  .AND. TOKEN .EQ. 'EXCEPT' ) THEN
                             CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                             READ ( TOKEN, *, IOSTAT=IOS ) QATMCNST(2)
                             IF ( IOS .NE. 0 ) THEN
                                  CALL FERR ( INT2(8213), '(2) Error '// &
     &                                'decoding atmosphere constraint '// &
     &                                 TOKEN(1:16), INT2(0), INT2(0) )
                             END IF
                             CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                             CALL CHAR2HOL ( TOKEN, QATMEXCPT, INT2(1), &
     &                                       INT2(8) )
                          ELSE IF ( ATMCNS .EQ. 'Y'  .AND. TOKEN .NE. ' ' ) THEN
                             CALL FERR ( INT2(8214), 'Error atm cnstr EXCEPT', &
     &                                   INT2(0), INT2(0) )
                       ENDIF
                    ELSE
                       CALL FERR ( INT2(8215), 'No sigma for atmosphere '// &
     &                     'constraints', INT2(0), INT2(0) )
                  END IF
                ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                  ATMCNS = 'N'
                ELSE IF ( TOKEN .EQ. 'IN'  .OR.  TOKEN .EQ. 'DEFAULT' ) THEN
                  ATMCNS='D'
                ELSE
                  CALL FERR ( INT2(8216), &
     &                'Unknown atmosphere constraint keyword: '//TOKEN(1:16), &
     &                 INT2(0), INT2(0) )
              END IF  ! TOKEN
            ELSE IF ( TOKEN .EQ. 'GRADIENTS' ) then
!
! ----------- GRADIENTS keyword
!
              CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
              IF ( TOKEN .EQ. 'YES' ) THEN
                   GRADCNS = 'Y'
                   CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                   IF ( TOKEN .NE. ' ' ) THEN
                        READ ( TOKEN, *, IOSTAT=IOS ) QGRADCNST(1)
                        IF ( IOS .NE. 0 ) THEN
                             CALL FERR ( INT2(8217), '(1) Error decoding '// &
     &                           'gradient constraint '//TOKEN(1:16), &
     &                            INT2(0), INT2(0) )
                        END IF
                        CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                        IF ( TOKEN .NE. ' ' ) THEN
                             READ ( TOKEN, *, IOSTAT=IOS ) QGRADCNST(2)
                             IF ( IOS .NE. 0 ) THEN
                                  CALL FERR ( INT2(8218), '(2) Error '// &
     &                                'decoding gradient constraint '// &
     &                                 TOKEN(1:16), INT2(0), INT2(0) )
                             END IF
                          ELSE
                             CALL FERR ( INT2(8219), 'No sigma for gradient '// &
     &                           'offset constraints', INT2(0), INT2(0) )
                        ENDIF
                     ELSE
                        CALL FERR ( INT2(8220), 'No sigma for gradient '// &
     &                             'rate constraints', INT2(0), INT2(0) )
                   END IF
                ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                   GRADCNS = 'N'
                ELSE
                   Call FERR ( INT2(8221), 'Unknown gradient constraint '// &
     &                        'keyword: '//TOKEN(1:16), INT2(0), INT2(0) )
              END IF
            ELSE IF ( TOKEN .EQ. 'CLOCKS' ) THEN
!
! ----------- CLOCKS keyword
!
              Call SPLITSTRING ( STRINH, TOKEN, STRINH )
              IF ( TOKEN .EQ. 'YES'  .OR.  &
     &             TOKEN .EQ. 'MOST' .OR.  &
     &             TOKEN .EQ. 'WEAKEST'    ) THEN
                   CLKCNS=TOKEN(1:1)
                   IF ( CLKCNS(1:1) .EQ. 'W' ) CLKCNS(1:1) = 'M'
                   CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                   IF ( TOKEN .NE. ' ' ) THEN
                        READ ( TOKEN, *, IOSTAT=IOS ) QCLKCNST(1)
                        IF ( IOS .NE. 0 ) THEN
                             CALL FERR ( INT2(8222), 'ERR decoding clk '// &
     &                           'cnstr '//TOKEN(1:16), INT2(0), INT2(0) )
                        END IF
                        CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                        CALL CHAR2HOL ( '        ', QCLKEXCPT, INT2(1), &
     &                                  INT2(8) )
                        IF ( CLKCNS .EQ. 'Y'  .AND. TOKEN .EQ. 'EXCEPT' ) THEN
                             CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                             READ ( TOKEN, *, IOSTAT=IOS ) QCLKCNST(2)
                             IF ( IOS .NE. 0 ) THEN
                                  CALL FERR ( INT2(8223), 'ERR decoding clk '// &
     &                                'cnstr 2 '//TOKEN(1:16), INT2(0), &
     &                                 INT2(0) )
                             END IF
                             CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                             CALL CHAR2HOL ( TOKEN, QCLKEXCPT, INT2(1), &
     &                                       INT2(8) )
                          ELSE IF ( CLKCNS .EQ. 'Y'  .AND. TOKEN .NE. ' ' ) THEN
                             CALL FERR ( INT2(8224), 'ERR clk cnstr except', &
     &                                   INT2(0), INT2(0) )
                       ENDIF
                    ELSE
                       CALL FERR ( INT2(8225), 'No sigma for clock constraits', &
     &                             INT2(0), INT2(0) )
                  END IF
                ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                  CLKCNS = 'N'
                ELSE IF ( TOKEN .EQ. 'IN'  .OR.  TOKEN .EQ. 'DEFAULT' ) THEN
                  CLKCNS = 'D'
              ELSE
                CALL FERR ( INT2(8226), 'Unknown clock constraint keyword: '// &
     &               TOKEN(1:16), INT2(0), INT2(0) )
              END IF
            ELSE IF ( TOKEN .EQ. 'CLOCK_REF_REQ' ) THEN
!
! ------------ CLOCK_REF_REQ keyword
!
!              CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
!              IF ( TOKEN .EQ. 'YES' ) THEN
!                   REFREQ = 'Y'
!               ELSE
!                  REFREQ = 'N'
!             ENDIF
             CALL FERR ( INT2(8227), &
     &           'GCONST(BATCH) Keyword CLOCK_REQ_REQ in '// &
     &           '$CONSTRAINTS section is not supported any more. Please '// &
     &           'remove this keyword from your control file', INT2(0), INT2(0) )
             STOP 'BATCH(gconst) Abonrmal termination'
            ELSE IF ( TOKEN .EQ. 'BASELINE_CLOCKS' ) THEN
!
! ----------- BASELINE_CLOCKS keyword
!
              CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
              IF ( TOKEN .EQ. 'YES' ) THEN
                   KBSL_CONST = .TRUE.  ! Baseline dependent clocks
                   CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                   WAS_SIGMA = .FALSE.
                   IF ( TOKEN .EQ. 'SIGMA' ) THEN
!
! --------------------- Skip qulifier SIGMA
!
                        CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                        WAS_SIGMA = .TRUE.
                   END IF
!
                   IF ( TOKEN(1:1) .NE. ' ' ) THEN
                        READ ( TOKEN, *, IOSTAT=IOS ) BAS_CLK_SIGMA
                        IF ( IOS .NE. 0 ) THEN
                             CALL FERR ( INT2(8228), 'Error decoding '// &
     &                           'baseline clock constraint sigma: '// &
     &                            TOKEN(1:16), INT2(0), INT2(0) )
                        END IF
                      ELSE IF ( TOKEN(1:1) .EQ. ' ' .AND. WAS_SIGMA ) THEN
                        CALL FERR ( INT2(8229), &
     &                      '(GCONST) Value of baseline clock '// &
     &                      'constraint sigma is not supplied', INT2(0), INT2(0) )
                   END IF
                ELSE IF ( TOKEN(1:2) .EQ. 'NO' ) THEN
                   KBSL_CONST = .FALSE.  ! Baseline dependent clocks
                ELSE
                   CALL FERR ( INT2(8230), &
     &                 'Unrecoginzed qualifier for baseline '// &
     &                 'clock constraint: '//TOKEN(1:16), INT2(0), INT2(0) )
              ENDIF
!
              CALL USE_GLBFIL   ( 'OW' )
              CALL USE_GLBFIL_4 ( 'WC' )
            ELSE IF ( TOKEN .EQ. 'NO_NET_TRANSLATION' ) THEN
!
! ----------- NO_NET_TRANSLATION keyword
!
              CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
              IF ( TOKEN .EQ. 'YES' ) THEN
                   KCENTERMASS = .TRUE.
                   IF ( STRINH .NE. ' ' ) THEN
                        CALL SPLITSTRING ( STRINH, STA_WT_FIL, STRINH )
                   ENDIF
                ELSE
                   KCENTERMASS = .FALSE.
              ENDIF
              CALL USE_GLBFIL_4 ( 'OWC' )
            ELSE IF ( TOKEN .EQ. 'SOURCES' ) THEN
!
! ----------- SOURCES keyword
!
              CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
              IF ( TOKEN .EQ. 'YES' ) THEN
                   KSRC_CONST = .TRUE.
!
! ---------------- If YES then SIGMA is allowed
!
                   CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                   WAS_SIGMA = .FALSE.
                   IF ( TOKEN .EQ. 'SIGMA' ) THEN
!
! --------------------- Skip qualifier SIGMA
!
                        CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                        WAS_SIGMA = .TRUE.
                   END IF
!
                   IF ( TOKEN(1:1) .NE. ' ' ) THEN
                        READ ( TOKEN, *, IOSTAT=IOS ) SRC_COO_SIGMA
                        IF ( IOS .NE. 0 ) THEN
                             CALL FERR ( INT2(8231), 'Error decoding source '// &
     &                           'constraint sigma: '//TOKEN(1:16), INT2(0), &
     &                            INT2(0) )
                        END IF
                      ELSE IF ( TOKEN(1:1) .EQ. ' ' .AND. WAS_SIGMA ) THEN
                        CALL FERR ( INT2(8232), '(GCONST) Value of source '// &
     &                      'constraint sigma is not supplied', INT2(0), &
     &                       INT2(0) )
                   END IF
                ELSE IF ( TOKEN(1:2) .EQ. 'NO' ) THEN
                   KSRC_CONST = .FALSE.
                ELSE
                   CALL FERR ( INT2(8233), 'GCONST: Unrecognized qualifier '// &
     &                 'in SOURCES: '//TOKEN(1:16)//' One of "YES" or. "NO" '// &
     &                 'was expected', INT2(0), INT2(0) )
              ENDIF
              CALL USE_GLBFIL   ( 'OW' )
              CALL USE_GLBFIL_4 ( 'WC' )
            ELSE IF ( TOKEN .EQ. 'PROPER_MOTIONS' ) THEN
!
! ----------- PROPER_MOTIONS keyword
!
              CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
              IF ( TOKEN .EQ. 'YES' ) THEN
                   KPRP_CONST = .TRUE.
!
! ---------------- If YES then SIGMA is allowed
!
                   CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                   WAS_SIGMA = .FALSE.
                   IF ( TOKEN .EQ. 'SIGMA' ) THEN
!
! --------------------- Skip qualifier SIGMA
!
                        CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                        WAS_SIGMA = .TRUE.
                   END IF
!
                   IF ( TOKEN(1:1) .NE. ' ' ) THEN
                        READ ( TOKEN, *, IOSTAT=IOS ) SRC_PRP_SIGMA
                        IF ( IOS .NE. 0 ) THEN
                             CALL FERR ( INT2(8234), 'Error decoding proper '// &
     &                           'motion constraint sigma: '//TOKEN(1:16), INT2(0), &
     &                            INT2(0) )
                        END IF
                      ELSE IF ( TOKEN(1:1) .EQ. ' ' .AND. WAS_SIGMA ) THEN
                        CALL FERR ( INT2(8235), '(GCONST) Value of source '// &
     &                      'proper motion constraint sigma is not supplied', INT2(0), &
     &                       INT2(0) )
                   END IF
                ELSE IF ( TOKEN(1:2) .EQ. 'NO' ) THEN
                   KPRP_CONST = .FALSE.
                ELSE
                   CALL FERR ( INT2(8236), 'GCONST: Unrecognized qualifier '// &
     &                 'in PROPER_MOTIONS: '//TOKEN(1:16)//' One of "YES" or. "NO" '// &
     &                 'was expected', INT2(0), INT2(0) )
              ENDIF
              CALL USE_GLBFIL   ( 'OW' )
              CALL USE_GLBFIL_4 ( 'WC' )
            ELSE IF ( TOKEN .EQ. 'STATIONS' ) THEN
!
! ----------- STATIONS keyword
!
              IUER = -1
              CALL GSTAVELCNST ( 'STATIONS', KSTACONST, &
     &                            STAXYZ_CNFL, STAXYZ_CNST, STAXYZ_CNSB, &
     &                            STAUEN_CNFL, STAUEN_CNST, STAUEN_CNSB, &
     &                            STASUP, ISTASP, TOKEN, STRINH, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL FERR ( INT2(8237), 'GCONST(BATCH) Error in parsing '// &
     &                 'keyword STATIONS in $CONTRAINTS section', INT2(0), &
     &                  INT2(0) )
                   STOP 'BATCH Abnormal termination'
              ENDIF
              CALL USE_GLBFIL_4 ( 'OWC' )
            ELSE IF ( TOKEN .EQ. 'PIECE_WISE_STA' ) THEN
!
! ----------- PIECE_WISE_STA keyword
!
              CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
              IF ( TOKEN .EQ. 'YES' ) then
                   PWCCNS=TOKEN(1:1)
                   CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                   IF ( TOKEN .EQ. 'SIGMA' .OR. TOKEN .EQ. 'sigma' ) THEN
                        CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                   END IF
                   IF ( TOKEN .NE. ' ' ) THEN
                        READ ( TOKEN, *, IOSTAT=IOS ) QPWCCNST
                        IF ( IOS .NE. 0 ) THEN
                             CALL FERR ( INT2(8238), 'Error decoding pwc '// &
     &                           'cnstr '//TOKEN(1:16), INT2(0), INT2(0) )
                        END IF
                    ELSE
                        CALL FERR ( INT2(8239), 'No sigma for constraint on '// &
     &                      'rate of linear spline for station position '// &
     &                      'parameterization', INT2(0), INT2(0) )
                   END IF
                ELSE IF ( TOKEN .EQ. 'NO' ) THEN
                   PWCCNS='N'
                ELSE
                   CALL FERR ( INT2(8240), &
     &                 'Unknown piece wise station constraint '//'keyword: '// &
     &                  TOKEN(1:16), INT2(0), INT2(0) )
              END IF
              CALL USE_GLBFIL_4 ( 'OWC' )
            ELSE IF ( TOKEN .EQ. 'VELOCITIES' ) THEN
!
! ----------- VELOCITIES keyword
!
              IUER = -1
              CALL GSTAVELCNST ( 'VELOCITIES', KVELCONST, &
     &                            VELXYZ_CNFL, VELXYZ_CNST, VELXYZ_CNSB, &
     &                            VELUEN_CNFL, VELUEN_CNST, VELUEN_CNSB, &
     &                            STASUP, ISTASP, TOKEN, STRINH, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL FERR ( INT2(8241), 'GCONST(BATCH) Error in parsing '// &
     &                 'keyword VELOCITIES in $CONTRAINTS section', INT2(0), &
     &                  INT2(0) )
                   STOP 'BATCH Abnormal termination'
              ENDIF
              CALL USE_GLBFIL_4 ( 'OWC' )
            ELSE IF ( TOKEN .EQ. 'NUVEL_COVAR' ) THEN
!
! ----------- NUVEL_COVAR keyword
!
              CALL GNUVCOV ( DEFNUVCOV, NUVCOVFLG, NUVEL_WT, STASUP, &
     &                       ISTASP, FIXED_PLATE, TOKEN, STRINH )
              CALL USE_GLBFIL_4 ( 'OWC' )
            ELSE IF( TOKEN .EQ. 'NO_NET_TRANSLATION_POSITION' ) THEN
!
! ----------- NO_NET_TRANSLATION_POSITION keyword
!
              IF ( KNNTPS ) THEN
                   CALL FERR ( INT2(8242), &
     &                 'NO_NET_TRANSLATION_POSITION keyword '//'used twice', &
     &                  INT2(0), INT2(0) )
              END IF
              IEXCEPTSZ = STA_BIT_WORDS
              IBITD  = 5
              IWORDE = 5
              OPT_ID = TOKEN
!
! ----------- Parsing remaining part of the string(s): we split the line(s) on
! ----------- keywords array KEYS_ARRAY and "EXCEPT" list
!
              ISTASP_BEG     = ISTASP + 1
              IND_NNT_POS(1) = ISTASP_BEG
              CALL GYNEXL ( DEFCMP, CMPSUP, IEXCEPTSZ, STASUP_I2, ISTASP, &
     &                      ISTASP_BEG, M_KEYS, NUM_KEYS, KEYS_ARRAY, MAX_STA, &
     &                      IBITD, IWORDE, SECT_ID, OPT_ID, TOKEN, STRINH )
              IND_NNT_POS(2) = ISTASP
!
              IF ( NUM_KEYS .EQ. -1 ) THEN
                   CALL FERR ( INT2(8243), &
     &                 'Missed qualifiers after keyword '// &
     &                 'NO_NET_TRANSLATION_POSITION. Please, check syntax', &
     &                  INT2(0), INT2(0) )
                   STOP 'BATCH(GCONST): Abnormal termination'
                 ELSE IF ( NUM_KEYS .EQ. 0 ) THEN
                   IF ( ISTASP .GT. 0  .OR. KBIT(DEFCMP,IBITD) ) THEN
!
! --------------------- We will apply NNT_POS constraint
!
                        KMATRIX_NNTP = '??'
                   END IF
                 ELSE IF ( NUM_KEYS .GE. 1 ) THEN
                   KMATRIX_NNTP = '??'
                   IND_RTP = 0
                   FL_RTP(1) = .FALSE.
                   FL_RTP(2) = .FALSE.
                   FL_RTP(3) = .FALSE.
!
                   DO IKEYS = 1,NUM_KEYS
                      KEY_OK = .FALSE.
                      IF ( KEYS_ARRAY(IKEYS) .EQ. 'ALL' ) THEN
                           KMATRIX_NNTP = 'AL'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'HORIZ'    ) THEN
                           KMATRIX_NNTP = 'HR'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'UNIFORM'  ) THEN
                           KSIG_SCALE_NNTP = 'UN'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'WEIGHTED' ) THEN
                           KSIG_SCALE_NNTP = 'WG'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'GLOBAL'   ) THEN
                           NNT_POS_GLO = .TRUE.
                           KEY_OK      = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'LOCAL'    ) THEN
                           NNT_POS_LOC = .TRUE.
                           KEY_OK      = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'SIGMA'    ) THEN
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'RIGHT_PART' ) THEN
                           IND_RTP = IKEYS
                           KEY_OK = .TRUE.
                        ELSE
!
! ------------------------ Check: maybe it is the value of right hand part?
!
                           IF ( IND_RTP .GT. 0          .AND. &
     &                          (IKEYS-IND_RTP) .GE. 1  .AND. &
     &                          (IKEYS-IND_RTP) .LE. 3        ) THEN
!
                                READ ( UNIT=KEYS_ARRAY(IKEYS), FMT='(F12.6)', &
     &                                 IOSTAT=IOS ) NNT_POS_RTP(IKEYS-IND_RTP)
                                IF ( IOS .NE. 0 ) THEN
                                     CALL FERR ( INT2(8244), 'Error decoding '// &
     &                                    'value '// &
     &                                     KEYS_ARRAY(IKEYS)(1:I_LEN(KEYS_ARRAY(IKEYS)))// &
     &                                    ' of right part of '// &
     &                                    'NO_NET_TRANSLATION_POSITION '// &
     &                                    'constraint ', INT2(0), INT2(0) )
                                     STOP 'BATCH(GCONST): Abnormal termination'
                                END IF
                                FL_RTP(IKEYS-IND_RTP) = .TRUE.
                                KEY_OK = .TRUE.
                           END IF
!
! ------------------------ Check: may be it is the value of sigma?
!
                           IF ( IKEYS .GT. 1 ) THEN
                              IF ( KEYS_ARRAY(IKEYS-1) .EQ. 'SIGMA' ) THEN
!
! ------------------------------ Yes, it looks like...
!
                                 READ ( KEYS_ARRAY(IKEYS), *, IOSTAT=IOS) &
     &                                  NNT_POS_SIGMA
                                 IF ( IOS .NE. 0 ) THEN
                                      CALL FERR ( INT2(8245), &
     &                                    'Error decoding '// &
     &                                    'NO_NET_TRANSLATION_POSITION '// &
     &                                    'constraint sigma: '// &
     &                                     KEYS_ARRAY(IKEYS), INT2(0), INT2(0) )
                                      STOP 'BATCH(GCONST): Abnormal termination'
                                    ELSE
                                      KEY_OK = .TRUE.
                                 END IF
                              END IF
                           END IF
                      ENDIF
!
                      IF ( .NOT. KEY_OK ) THEN
                           ERRBUF = 'Unrecognized qualifier for '// &
     &                              'NO_NET_TRANSLATION_POSITION keyword: '// &
     &                              KEYS_ARRAY(IKEYS)
                           CALL FERR ( INT2(8246), ERRBUF(1:TRIMLEN(ERRBUF)), &
     &                          INT2(0), INT2(0) )
                           STOP 'BATCH(GCONST): Abnormal termination'
                      END IF
                   ENDDO
!
                   IF ( KMATRIX_NNTP .EQ. '??' ) THEN
                        CALL FERR ( INT2(8247), 'Please supply type of '// &
     &                      'NO_NET_TRANSLATION_POSITION constraint: '// &
     &                      'HORIZ or ALL', INT2(0), INT2(0) )
                        STOP 'BATCH(GCONST): Abnormal termination'
                   END IF
!
                   IF ( IND_RTP .GT. 0  .AND. ( .NOT. FL_RTP(1) .OR. &
     &                  .NOT. FL_RTP(2) .OR. .NOT. FL_RTP(3)         ) ) THEN
!
                        CALL FERR ( INT2(8248), &
     &                      'Not all values of right parts '// &
     &                      'of NO_NET_TRANSLATION_POSITION constraint '// &
     &                      'were supplied', INT2(0), INT2(0) )
                        STOP 'BATCH(GCONST): Abnormal termination'
                   END IF
              END IF  ! num_key
!
              IF ( .NOT. NNT_POS_GLO  .AND.  .NOT. NNT_POS_LOC ) THEN
                   NNT_POS_GLO = .TRUE.
              END IF
              KNNTPS=.TRUE.
              CALL USE_GLBFIL_3 ( 'OWC' )
              CALL USE_GLBFIL_4 ( 'OWC' )
            ELSE IF ( TOKEN .EQ. 'NO_NET_ROTATION_POSITION' ) THEN
!
! ----------- NO_NET_ROTATION_POSITION keyword
!
              IF ( KNNRPS ) THEN
                   CALL FERR ( INT2(8249), &
     &                 'Keyword NO_NET_ROTATION_POSITION used '//'twice', &
     &                  INT2(0), INT2(0) )
                   STOP 'BATCH(GCONST): Abnormal termination'
              END IF
              IEXCEPTSZ = STA_BIT_WORDS
              IBITD     = 6
              IWORDE    = 6
              OPT_ID = TOKEN
!
! ----------- Parsing remaining part of the string(s): we split the line(s) on
! ----------- keywords array KEYS_ARRAY and "EXCEPT" list
!
              ISTASP_BEG     = ISTASP + 1
              IND_NNR_POS(1) = ISTASP_BEG
              CALL GYNEXL ( DEFCMP, CMPSUP, IEXCEPTSZ, STASUP_I2, ISTASP, &
     &                      ISTASP_BEG, M_KEYS, NUM_KEYS, KEYS_ARRAY, MAX_STA, &
     &                      IBITD, IWORDE, SECT_ID, OPT_ID, TOKEN, STRINH )
              IND_NNR_POS(2) = ISTASP
              IF ( NUM_KEYS .EQ. -1 ) THEN
                   CALL FERR ( INT2(8250), &
     &                 'Missed qualifiers after keyword '// &
     &                 'NO_NET_ROTATIUON_POSITION. Please, check syntax', INT2(0), &
     &                  INT2(0) )
                   STOP 'BATCH(GCONST): Abnormal termination'
                 ELSE IF ( NUM_KEYS .GE. 1 ) THEN
                   IND_RTP = 0
                   FL_RTP(1) = .FALSE.
                   FL_RTP(2) = .FALSE.
                   FL_RTP(3) = .FALSE.
!
                   DO IKEYS = 1, NUM_KEYS
                      KEY_OK = .FALSE.
                      IF ( KEYS_ARRAY(IKEYS) .EQ. 'UNIFORM' ) THEN
                           KSIG_SCALE_NNRP = 'UN'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'WEIGHTED' ) THEN
                           KSIG_SCALE_NNRP = 'WG'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'GLOBAL'   ) THEN
                           NNR_POS_GLO = .TRUE.
                           KEY_OK      = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'LOCAL'    ) THEN
                           NNR_POS_LOC = .TRUE.
                           KEY_OK      = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'SIGMA'    ) THEN
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'RIGHT_PART' ) THEN
                           IND_RTP = IKEYS
                           KEY_OK  = .TRUE.
                        ELSE
!
! ------------------------ Check: maybe it is te value of right hand part?
!
                           IF ( IND_RTP .GT. 0          .AND. &
     &                          (IKEYS-IND_RTP) .GE. 1  .AND. &
     &                          (IKEYS-IND_RTP) .LE. 3        ) THEN
!
                                READ ( UNIT=KEYS_ARRAY(IKEYS), FMT='(F12.6)', &
     &                                 IOSTAT=IOS ) NNR_POS_RTP(IKEYS-IND_RTP)
                                IF ( IOS .NE. 0 ) THEN
                                     CALL FERR ( INT2(8251), 'Error decoding '// &
     &                                    'value '// &
     &                                    KEYS_ARRAY(IKEYS)(1:I_LEN(KEYS_ARRAY(IKEYS)))// &
     &                                    ' of right part of '// &
     &                                    'NO_NET_ROTATION_POSITION '// &
     &                                    'constraint ', INT2(0), INT2(0) )
                                     STOP 'BATCH(GCONST): Abnormal termination'
                                END IF
                                FL_RTP(IKEYS-IND_RTP) = .TRUE.
                                KEY_OK = .TRUE.
                           END IF
!
! ----------------------- Check: may be it is the value of sigma?
!
                           IF ( IKEYS .GT. 1 ) THEN
                              IF ( KEYS_ARRAY(IKEYS-1) .EQ. 'SIGMA' ) THEN
!
! -------------------------------- Yes, it looks like...
!
                                 READ ( KEYS_ARRAY(IKEYS), *, IOSTAT=IOS) &
     &                                  NNR_POS_SIGMA
                                 IF ( IOS .NE. 0 ) THEN
                                      CALL FERR ( INT2(8252), 'Error '// &
     &                                    'decoding NO_NET_ROTATION_POSITION '// &
     &                                    'constraint sigma: '// &
     &                                     KEYS_ARRAY(IKEYS), INT2(0), INT2(0) )
                                      STOP 'BATCH(GCONST): Abnormal termination'
                                   ELSE
                                      KEY_OK = .TRUE.
                                 END IF
                              END IF
                           END IF
                      ENDIF
!
                      IF ( .NOT. KEY_OK ) THEN
                           ERRBUF = 'Unrecognized qualifier for '// &
     &                              'NO_NET_ROTATION_POSITION keyword: '// &
     &                               KEYS_ARRAY(IKEYS)
                           CALL FERR ( INT2(8253), ERRBUF(1:TRIMLEN(ERRBUF)), &
     &                          INT2(0), INT2(0) )
                           STOP 'BATCH(GCONST): Abnormal termination'
                      ENDIF
                   ENDDO
!
                   IF ( IND_RTP .GT. 0  .AND. ( .NOT. FL_RTP(1) .OR. &
     &                  .NOT. FL_RTP(2) .OR. .NOT. FL_RTP(3)         ) ) THEN
!
                        CALL FERR ( INT2(8254), &
     &                      'Not all values of right parts '// &
     &                      'of NO_NET_ROTATION_POSITION constraint were '// &
     &                      'supplied', INT2(0), INT2(0) )
                        STOP 'BATCH(GCONST): Abnormal termination'
                   END IF
              END IF  ! num_keys
!
              IF ( .NOT. NNR_POS_GLO  .AND.  .NOT. NNR_POS_LOC ) THEN
                   NNR_POS_GLO = .TRUE.
              END IF
!
              KNNRPS=.TRUE.
              CALL USE_GLBFIL_3 ( 'OWC' )
              CALL USE_GLBFIL_4 ( 'OWC' )
            ELSE IF ( TOKEN .EQ. 'NO_NET_TRANSLATION_VELOCITY' ) THEN
!
! ----------- NO_NET_TRANSLATION_VELOCITY keyword
!
              IF ( KNNTVS ) THEN
                   CALL FERR ( INT2(8255), &
     &                 'Keyword NO_NET_TRANSLATION_VELOCITY '//'used twice', &
     &                  INT2(0), INT2(0) )
                   STOP 'BATCH(GCONST): Abnormal termination'
              END IF
!
              IEXCEPTSZ = STA_BIT_WORDS
              IBITD     = 5
              IWORDE    = 5
              OPT_ID    = TOKEN
!
! ----------- Parsing remaining part of the string(s): we split the line(s) on
! ----------- keywords array KEYS_ARRAY and "EXCEPT" list
!
              ISTASP_BEG     = ISTASP + 1
              IND_NNT_VEL(1) = ISTASP_BEG
              CALL GYNEXL ( DEFVEL, VELSUP, IEXCEPTSZ, STASUP_I2, ISTASP, &
     &                      ISTASP_BEG, M_KEYS, NUM_KEYS, KEYS_ARRAY, MAX_STA, &
     &                      IBITD, IWORDE, SECT_ID, OPT_ID, TOKEN, STRINH )
              IND_NNT_VEL(2) = ISTASP
              IF ( NUM_KEYS .EQ. -1 ) THEN
                   CALL FERR ( INT2(8256), &
     &                 'Missed qualifiers after keyword '// &
     &                 'NO_NET_TRANSLATION_VELOCITY. Please, check syntax', &
     &                  INT2(0), INT2(0) )
                   STOP 'BATCH(GCONST): Abnormal termination'
                 ELSE IF ( NUM_KEYS .EQ. 0 ) THEN
                   IF ( ISTASP .GT. 0  .OR. KBIT(DEFVEL,IBITD) ) THEN
!
! --------------------- We will apply NNT_VEL constraint
!
                        KMATRIX_NNTV = '??'
                   END IF
                 ELSE IF ( NUM_KEYS .GE. 1 ) THEN
                   KMATRIX_NNTV = '??'
                   IND_RTP = 0
                   FL_RTP(1) = .FALSE.
                   FL_RTP(2) = .FALSE.
                   FL_RTP(3) = .FALSE.
!
                   DO IKEYS = 1, NUM_KEYS
                      KEY_OK = .FALSE.
                      IF ( KEYS_ARRAY(IKEYS)         .EQ. 'ALL'      ) THEN
                           KMATRIX_NNTV = 'AL'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'HORIZ'    ) THEN
                           KMATRIX_NNTV = 'HR'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'UNIFORM'  ) THEN
                           KSIG_SCALE_NNTV = 'UN'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'WEIGHTED' ) THEN
                           KSIG_SCALE_NNTV = 'WG'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'GLOBAL'   ) THEN
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'SIGMA'    ) THEN
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'RIGHT_PART' ) THEN
                           IND_RTP = IKEYS
                           KEY_OK  = .TRUE.
                        ELSE
!
! ------------------------ Check: maybe it is te value of right hand part?
!
                           IF ( IND_RTP .GT. 0          .AND. &
     &                          (IKEYS-IND_RTP) .GE. 1  .AND. &
     &                          (IKEYS-IND_RTP) .LE. 3        ) THEN
!
                                READ ( UNIT=KEYS_ARRAY(IKEYS), FMT='(F12.6)', &
     &                                 IOSTAT=IOS ) NNT_VEL_RTP(IKEYS-IND_RTP)
                                IF ( IOS .NE. 0 ) THEN
                                     CALL FERR ( INT2(8257), 'Error decoding '// &
     &                                   'value '// &
     &                                    KEYS_ARRAY(IKEYS)(1:I_LEN(KEYS_ARRAY(IKEYS)))// &
     &                                   ' of right part of '// &
     &                                   'NO_NET_TRANSLATION_VELOCITY '// &
     &                                   'constraint ', INT2(0), INT2(0) )
                                     STOP 'BATCH(GCONST): Abnormal termination'
                                END IF
                                FL_RTP(IKEYS-IND_RTP) = .TRUE.
                                KEY_OK = .TRUE.
                           END IF
!
! ------------------------ Check: may be it is the value of sigma?
!
                           IF ( IKEYS .GT. 1 ) THEN
                              IF ( KEYS_ARRAY(IKEYS-1) .EQ. 'SIGMA' ) THEN
!
! ------------------------------ Yes, it looks like...
!
                                 READ ( KEYS_ARRAY(IKEYS), *, IOSTAT=IOS ) &
     &                                  NNT_VEL_SIGMA
                                 IF ( IOS .NE. 0 ) THEN
                                      CALL FERR ( INT2(8258), 'Error '// &
     &                                    'decoding '// &
     &                                    'NO_NET_TRANSLATION_VELOCITY '// &
     &                                    'constraint sigma: '// &
     &                                     KEYS_ARRAY(IKEYS), INT2(0), INT2(0) )
                                      STOP 'BATCH(GCONST): Abnormal termination'
                                    ELSE
                                      KEY_OK = .TRUE.
                                 END IF
                              END IF
                           END IF
                      ENDIF
!
                      IF ( .NOT. KEY_OK ) THEN
                           ERRBUF = 'Unrecognized qualifier for '// &
     &                              'NO_NET_TRANSLATION_VELOCITY keyword: '// &
     &                               KEYS_ARRAY(IKEYS)
                           CALL FERR ( INT2(8259), ERRBUF(1:TRIMLEN(ERRBUF)), &
     &                          INT2(0), INT2(0) )
                           STOP 'BATCH(GCONST): Abnormal termination'
                      ENDIF
                   ENDDO
!
                   IF ( KMATRIX_NNTV .EQ. '??' ) THEN
                        CALL FERR ( INT2(8260), 'Please supply type of '// &
     &                      'NO_NET_TRANSLATION_VELOCITY constraint: '// &
     &                      'HORIZ or ALL', INT2(0), INT2(0) )
                        STOP 'BATCH(GCONST): Abnormal termination'
                   END IF
!
                   IF ( IND_RTP .GT. 0  .AND. ( .NOT. FL_RTP(1) .OR. &
     &                  .NOT. FL_RTP(2) .OR. .NOT. FL_RTP(3)         ) ) THEN
!
                        CALL FERR ( INT2(8261), &
     &                      'Not all values of right parts '// &
     &                      'of NO_NET_TRANSLATION_VELOCITY constraint were '// &
     &                      'supplied', INT2(0), INT2(0) )
                        STOP 'BATCH(GCONST): Abnormal termination'
                   END IF
              END IF
!
              KNNTVS=.TRUE.
              CALL USE_GLBFIL_3 ( 'OWC' )
              CALL USE_GLBFIL_4 ( 'OWC' )
            ELSE IF ( TOKEN .EQ. 'NO_NET_ROTATION_VELOCITY' ) THEN
!
! ----------- NO_NET_ROTATION_VELOCITY keyword
!
              IF ( KNNRVS ) THEN
                   CALL FERR ( INT2(8262), &
     &                 'Keyword NO_NET_ROTATION_VELOCITY '//'used twice', &
     &                  INT2(0), INT2(0) )
                   STOP 'BATCH(GCONST): Abnormal termination'
              END IF
!
              IEXCEPTSZ = STA_BIT_WORDS
              IBITD     = 6
              IWORDE    = 6
              OPT_ID    = TOKEN
!
! ----------- Parsing remaining part of the string(s): we split the line(s) on
! ----------- keywords array KEYS_ARRAY and "EXCEPT" list
!
              ISTASP_BEG     = ISTASP + 1
              IND_NNR_VEL(1) = ISTASP_BEG
              CALL GYNEXL ( DEFVEL, VELSUP, IEXCEPTSZ, STASUP_I2, ISTASP, &
     &                      ISTASP_BEG, M_KEYS, NUM_KEYS, KEYS_ARRAY, MAX_STA, &
     &                      IBITD, IWORDE, SECT_ID, OPT_ID, TOKEN, STRINH )
              IND_NNR_VEL(2) = ISTASP
              IF ( NUM_KEYS .EQ. -1 ) THEN
                   CALL FERR ( INT2(8263), &
     &                 'Missed qualifiers after keyword '// &
     &                 'NO_NET_ROTATION_VELOCITY. Please, check syntax', INT2(0), &
     &                  INT2(0) )
                   STOP 'BATCH(GCONST): Abnormal termination'
                 ELSE IF ( NUM_KEYS .GE. 1 ) THEN
                   IND_RTP = 0
                   FL_RTP(1) = .FALSE.
                   FL_RTP(2) = .FALSE.
                   FL_RTP(3) = .FALSE.
!
                   DO IKEYS=1,NUM_KEYS
                      KEY_OK = .FALSE.
                      IF (         KEYS_ARRAY(IKEYS) .EQ. 'UNIFORM'  ) THEN
                           KSIG_SCALE_NNRV = 'UN'
                           KEY_OK = .TRUE.
                         ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'WEIGHTED' ) THEN
                           KSIG_SCALE_NNRV = 'WG'
                           KEY_OK = .TRUE.
                         ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'GLOBAL'   ) THEN
                           KEY_OK      = .TRUE.
                         ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'SIGMA'    ) THEN
                           KEY_OK = .TRUE.
                         ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'RIGHT_PART' ) THEN
                           IND_RTP = IKEYS
                           KEY_OK  = .TRUE.
                         ELSE
!
! ------------------------ Check: maybe it is te value of right hand part?
!
                           IF ( IND_RTP .GT. 0          .AND. &
     &                          (IKEYS-IND_RTP) .GE. 1  .AND. &
     &                          (IKEYS-IND_RTP) .LE. 3        ) THEN
!
                                READ ( UNIT=KEYS_ARRAY(IKEYS), FMT='(F12.6)', &
     &                                 IOSTAT=IOS ) NNR_VEL_RTP(IKEYS-IND_RTP)
                                IF ( IOS .NE. 0 ) THEN
                                     CALL FERR ( INT2(8264), 'Error decoding '// &
     &                                   'value '// &
     &                                    KEYS_ARRAY(IKEYS)(1:I_LEN(KEYS_ARRAY(IKEYS)))// &
     &                                   ' of right part of '// &
     &                                   'NO_NET_ROTATION_VELOCITY '// &
     &                                    'constraint ', INT2(0), INT2(0) )
                                     STOP 'BATCH(GCONST): Abnormal termination'
                                END IF
                                FL_RTP(IKEYS-IND_RTP) = .TRUE.
                                KEY_OK = .TRUE.
                           END IF
!
! ------------------------ Check: may be it is the value of sigma?
!
                           IF ( IKEYS .GT. 1 ) THEN
                              IF ( KEYS_ARRAY(IKEYS-1) .EQ. 'SIGMA' ) THEN
!
! ------------------------------ Yes, it looks like...
!
                                 READ ( KEYS_ARRAY(IKEYS), *, IOSTAT=IOS) &
     &                                  NNR_VEL_SIGMA
                                 IF ( IOS .NE. 0 ) THEN
                                      CALL FERR ( INT2(8265), 'Error '// &
     &                                    'decoding '// &
     &                                    'NO_NET_ROTATION_VELOCITY constraint '// &
     &                                    'sigma: '//KEYS_ARRAY(IKEYS), INT2(0), &
     &                                     INT2(0) )
                                      STOP 'BATCH(GCONST): Abnormal termination'
                                    ELSE
                                      KEY_OK = .TRUE.
                                 END IF
                              END IF
                           END IF
                      ENDIF
!
                      IF ( .NOT. KEY_OK ) THEN
                            ERRBUF = 'Unrecognized qualifier for '// &
     &                               'NO_NET_ROTATION_VELOCITY keyword: '// &
     &                      KEYS_ARRAY(IKEYS)
                            CALL FERR ( INT2(8266), ERRBUF(1:TRIMLEN(ERRBUF)), &
     &                           INT2(0), INT2(0) )
                            STOP 'BATCH(GCONST): Abnormal termination'
                      ENDIF
                   ENDDO
!
                   IF ( IND_RTP .GT. 0  .AND. ( .NOT. FL_RTP(1) .OR. &
     &                  .NOT. FL_RTP(2) .OR. .NOT. FL_RTP(3)         ) ) THEN
!
                        CALL FERR ( INT2(8267), &
     &                      'Not all values of right parts '// &
     &                      'of NO_NET_ROTATION_VELOCITY constraint were '// &
     &                      'supplied', INT2(0), INT2(0) )
                        STOP 'BATCH(GCONST): Abnormal termination'
                   END IF
              END IF
!
              KNNRVS=.TRUE.
              CALL USE_GLBFIL_3 ( 'OWC' )
              CALL USE_GLBFIL_4 ( 'OWC' )
            ELSE IF ( TOKEN .EQ. 'NO_NET_ROTATION_SOURCE' ) THEN
!
! ----------- NO_NET_ROTATION_SOURCE keyword
!
              IF ( KNNRSS ) THEN
                   CALL FERR ( INT2(8268), 'Keyword NO_NET_ROTATION_SOURCE '// &
     &                 'used twice', INT2(0), INT2(0) )
                   STOP 'BATCH(GCONST): Abnormal termination'
              END IF
!
              IEXCEPTSZ   = SRC_BIT_WORDS
              IBITD       = 7
              IWORDE      = 7
              OPT_ID      = TOKEN
!
! ----------- Parsing remaining part of the string(s): we split the line(s) on
! ----------- keywords array KEYS_ARRAY and "EXCEPT" list
!
              ISRCSP_BEG     = ISRCSP + 1
              IND_NNR_SRC(1) = ISRCSP_BEG
              CALL GYNEXL ( DEFSRC, SOUSUP, IEXCEPTSZ, SRCSUP_I2, ISRCSP, &
     &                      ISRCSP_BEG, M_KEYS, NUM_KEYS, KEYS_ARRAY, MAX_SRC, &
     &                      IBITD, IWORDE, SECT_ID, OPT_ID, TOKEN, STRINH )
              IND_NNR_SRC(2) = ISRCSP
!
              IF ( NUM_KEYS .EQ. -1 ) THEN
                   CALL FERR ( INT2(8269), &
     &                 'Missed qualifiers after keyword '// &
     &                 'NO_NET_ROTATION_SOURCE. Please, check syntax', INT2(0), &
     &                  INT2(0) )
                   STOP 'BATCH(GCONST): Abnormal termination'
                 ELSE IF ( NUM_KEYS .GE. 1 ) THEN
                   IND_RTP = 0
                   FL_RTP(1) = .FALSE.
                   FL_RTP(2) = .FALSE.
                   FL_RTP(3) = .FALSE.
                   DO IKEYS = 1, NUM_KEYS
                      KEY_OK = .FALSE.
                      IF (        KEYS_ARRAY(IKEYS) .EQ. 'UNIFORM'  ) THEN
                           KSIG_SCALE_NNRS = 'UN'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'WEIGHTED' ) THEN
                           KSIG_SCALE_NNRS = 'WG'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'GLOBAL'   ) THEN
                           NNR_SRC_GLO = .TRUE.
                           KEY_OK      = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'LOCAL'    ) THEN
                           NNR_SRC_LOC = .TRUE.
                           KEY_OK      = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'SIGMA'    ) THEN
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'RIGHT_PART'    ) THEN
                           IND_RTP = IKEYS
                           KEY_OK = .TRUE.
                        ELSE
!
! ------------------------ Check: maybe it is te value of right hand part?
!
                           IF ( IND_RTP .GT. 0          .AND. &
     &                          (IKEYS-IND_RTP) .GE. 1  .AND. &
     &                          (IKEYS-IND_RTP) .LE. 3        ) THEN
!
                                READ ( UNIT=KEYS_ARRAY(IKEYS), FMT='(F13.6)', &
     &                                 IOSTAT=IOS ) NNR_SRC_RTP(IKEYS-IND_RTP)
                                IF ( IOS .NE. 0 ) THEN
                                     CALL FERR ( INT2(8270), 'Error decoding '// &
     &                                   'value '// &
     &                                    KEYS_ARRAY(IKEYS)(1:I_LEN(KEYS_ARRAY(IKEYS)))// &
     &                                   ' of right part of '// &
     &                                   'NO_NET_ROTATION_SOURCE '// &
     &                                    'constraint ', INT2(0), INT2(0) )
                                     STOP 'BATCH(GCONST): Abnormal termination'
                                END IF
                                FL_RTP(IKEYS-IND_RTP) = .TRUE.
                                KEY_OK = .TRUE.
                           END IF
!
! ------------------------ Check: may be it is the value of sigma?
!
                           IF ( IKEYS .GT. 1 ) THEN
                                IF ( KEYS_ARRAY(IKEYS-1) .EQ. 'SIGMA' ) THEN
!
! ---------------------------------- Yes, it looks like...
!
                                     READ ( KEYS_ARRAY(IKEYS), *, IOSTAT=IOS ) &
     &                                      NNR_SRC_SIGMA
                                     IF ( IOS .NE. 0 ) THEN
                                          CALL FERR ( INT2(8271), 'Error '// &
     &                                        'decoding NO_NET_ROTATION_SOURCE '// &
     &                                        'constraint sigma: '// &
     &                                         KEYS_ARRAY(IKEYS), INT2(0), INT2(0) )
                                          STOP 'BATCH(GCONST): Abnormal termination'
                                       ELSE
                                          KEY_OK = .TRUE.
                                     END IF
                                END IF
                          END IF
                      ENDIF
!
                      IF ( .NOT. KEY_OK ) THEN
                           ERRBUF = 'GCONST Unrecognized qualifier for '// &
     &                              'NO_NET_ROTATION_SOURCE keyword: '// &
     &                     KEYS_ARRAY(IKEYS)
                           CALL FERR ( INT2(8272), ERRBUF(1:TRIMLEN(ERRBUF)), &
     &                          INT2(0), INT2(0) )
                           STOP 'BATCH(GCONST): Abnormal termination'
                      ENDIF
                   ENDDO
                END IF ! Num_key for NO_NET_ROTATION_SOURCE
!
                IF ( .NOT. NNR_SRC_GLO  .AND.  .NOT. NNR_SRC_LOC ) THEN
                     NNR_SRC_GLO = .TRUE.
                END IF
!
                KNNRSS=.TRUE.
                CALL USE_GLBFIL_3 ( 'OWC' )
                CALL USE_GLBFIL_4 ( 'OWC' )
            ELSE IF ( TOKEN .EQ. 'NO_NET_ROTATION_PROPER_MOTION' ) THEN
!
! ----------- NO_NET_ROTATION_PROPER_MOTION keyword
!

              IEXCEPTSZ   = SRC_BIT_WORDS
              IBITD       = 8
              IWORDE      = 8
              OPT_ID      = TOKEN
!
! ----------- Parsing remaining part of the string(s): we split the line(s) on
! ----------- keywords array KEYS_ARRAY and "EXCEPT" list
!
              ISRCSP_BEG = ISRCSP+1
              IND_NNR_PRP(1) = ISRCSP_BEG 
              CALL GYNEXL ( DEFSRC, SOUSUP, IEXCEPTSZ, SRCSUP_I2, ISRCSP, &
     &                      ISRCSP_BEG, M_KEYS, NUM_KEYS, KEYS_ARRAY, MAX_SRC, &
     &                      IBITD, IWORDE, SECT_ID, OPT_ID, TOKEN, STRINH )
              IND_NNR_PRP(2) = ISRCSP
!
              IF ( NUM_KEYS .EQ. -1 ) THEN
                   CALL FERR ( INT2(8273), 'Missed qualifiers after '// &
     &                 'keyword NO_NET_ROTATION_PROPER_MOTION. Please, '// &
     &                 'check syntax', INT2(0), INT2(0) )
                   STOP 'BATCH(GCONST): Abnormal termination'
                 ELSE IF ( NUM_KEYS .GE. 1 ) THEN
                   IND_RTP = 0
                   FL_RTP(1) = .FALSE.
                   FL_RTP(2) = .FALSE.
                   FL_RTP(3) = .FALSE.
                   DO IKEYS = 1, NUM_KEYS
                      KEY_OK = .FALSE.
                      IF (        KEYS_ARRAY(IKEYS) .EQ. 'UNIFORM'  ) THEN
                           KSIG_SCALE_NNRQ = 'UN'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'WEIGHTED' ) THEN
                           KSIG_SCALE_NNRQ = 'WG'
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'SIGMA'    ) THEN
                           KEY_OK = .TRUE.
                        ELSE IF ( KEYS_ARRAY(IKEYS) .EQ. 'RIGHT_PART' ) THEN
                           IND_RTP = IKEYS
                           KEY_OK  = .TRUE.
                        ELSE
!
! ------------------------ Check: maybe it is te value of right hand part?
!
                           IF ( IND_RTP .GT. 0          .AND. &
     &                          (IKEYS-IND_RTP) .GE. 1  .AND. &
     &                          (IKEYS-IND_RTP) .LE. 3        ) THEN
!
                                READ ( UNIT=KEYS_ARRAY(IKEYS), FMT='(F13.6)', &
     &                                 IOSTAT=IOS ) NNR_PRP_RTP(IKEYS-IND_RTP)
                                IF ( IOS .NE. 0 ) THEN
                                     CALL FERR ( INT2(8274), 'Error decoding '// &
     &                                   'value '// &
     &                                    KEYS_ARRAY(IKEYS)(1:I_LEN(KEYS_ARRAY(IKEYS)))// &
     &                                   ' of right part of '// &
     &                                   'NO_NET_ROTATION_PROPER_MOTION '// &
     &                                    'constraint ', INT2(0), INT2(0) )
                                     STOP 'BATCH(GCONST): Abnormal termination'
                                END IF
                                FL_RTP(IKEYS-IND_RTP) = .TRUE.
                                KEY_OK = .TRUE.
                           END IF
!
! ------------------------ Check: may be it is the value of sigma?
!
                           IF ( IKEYS .GT. 1 ) THEN
                              IF ( KEYS_ARRAY(IKEYS-1) .EQ. 'SIGMA' ) THEN
!
! ------------------------------ Yes, it looks like...
!
                                 READ ( KEYS_ARRAY(IKEYS), *, IOSTAT=IOS ) &
     &                                  NNR_PRP_SIGMA
                                 NNR_PRP_SIGMA = NNR_PRP_SIGMA*YEAR__TO__SEC 
                                 IF ( IOS .NE. 0 ) THEN
                                      CALL FERR ( INT2(8275), 'Error '// &
     &                                    'decoding '// & 
     &                                    'NO_NET_ROTATION_PROPER_MOTION '// &
     &                                    'constraint sigma: '// &
     &                                     KEYS_ARRAY(IKEYS), INT2(0), INT2(0) )
                                      STOP 'BATCH(GCONST): Abnormal termination'
                                   ELSE
                                      KEY_OK = .TRUE.
                                 END IF
                              END IF
                           END IF
                      ENDIF
!
                      IF ( .NOT. KEY_OK ) THEN
                           ERRBUF = 'GCONST Unrecognized qualifier for '// &
     &                            'NO_NET_ROTATION_PROPER_MOTION keyword: '// &
     &                     KEYS_ARRAY(IKEYS)
                           CALL FERR ( INT2(8276), ERRBUF(1:TRIMLEN(ERRBUF)), &
     &                          INT2(0), INT2(0) )
                           STOP 'BATCH(GCONST): Abnormal termination'
                      ENDIF
                   ENDDO
                END IF ! Num_key for NO_NET_ROTATION_PROPER_MOTION
!
                KNNRPP=.TRUE.
                CALL USE_GLBFIL_3 ( 'OWC' )
                CALL USE_GLBFIL_4 ( 'OWC' )
            ELSE IF ( TOKEN .EQ. 'STRUCTURE_ADMITTANCE' ) THEN
                IF ( KSOUADM ) THEN
                     CALL FERR ( INT2(8277), 'Keyword '// &
     &                   'STRUCTURE_ADMITTANCE used twice', INT2(0), INT2(0) )
                     STOP 'BATCH(GCONST): Abnormal termination'
                END IF
!
! ------------- Source structrue admittanvce constraint
!
                CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                CALL TRAN ( 11, TOKEN, TOKEN )
                IF ( TOKEN == 'NO' ) THEN
                   ELSE IF ( TOKEN == 'YES' ) THEN
                     CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                     IF ( TOKEN == '' ) THEN
                          CALL FERR ( INT2(8278), 'GCONST: value SIGMA '// &
     &                                'after keyword STRUCTURE_ADMITTANCE '// &
     &                                ' was epxected, but nothing was found' )
                          STOP 'BATCH(GCONST): Abnormal termination'
                        ELSE IF ( TOKEN == 'SIGMA' ) THEN
                          CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                          IF ( TOKEN == "" ) THEN
                               CALL FERR ( INT2(8279), 'GCONST: Value '// &
     &                                'of reciprocatl weight after '// &
     &                                'STRUCTURE_ADMITTANCE YES SIGMA '// &
     &                                'was expected, but nothing was found' )
                               STOP 'BATCH(GCONST): Abnormal termination'
                             ELSE 
                               IF ( INDEX ( TOKEN, '.' ) == 0 ) THEN
                                    TOKEN = TOKEN(1:I_LEN(TOKEN))//'.0' 
                               END IF
                               READ ( UNIT=TOKEN, FMT='(F12.6)', IOSTAT=IOS ) SOU_ADM_CNS
                               IF ( IOS .NE. 0 ) THEN
                                    IF ( IOS .NE. 0 ) THEN
                                         CALL FERR ( INT2(8280), 'GCONST: '// &
     &                                       'error in decoding the '// &
     &                                       'reciprocal weight of '// &
     &                                       'STRUCTURE_ADMITTANCE: '// &
     &                                       TOKEN(1:I_LEN(TOKEN))//' is '// &
     &                                       'not a REAL*8 number' )
                                         STOP 'BATCH(GCONST): Abnormal termination'
                                    END IF
                               END IF
                          END IF
                     END IF
                     CALL USE_GLBFIL_4 ( 'OWC' )
                   ELSE 
                     CALL FERR ( INT2(8281), 'GCONST: No value found '// &
     &                          'after keyword STRUCTURE_ADMITTANCE '// &
     &                          ' NO or YES SIGMA {value} were expected' )
                     STOP 'BATCH(GCONST): Abnormal termination'
                END IF
            ELSE IF ( TOKEN .EQ. 'IONOSPHERE_SCALE' ) THEN
                IF ( KIOS ) THEN
                     CALL FERR ( INT2(8282), 'Keyword '// &
     &                   'IONIOSPHERE_SCALE used twice', INT2(0), INT2(0) )
                     STOP 'BATCH(GCONST): Abnormal termination'
                END IF
                CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                CALL TRAN ( 11, TOKEN, TOKEN )
                IF ( TOKEN == 'NO' ) THEN
                     IOS_SIG_BATCH = 0.0D0
                   ELSE IF ( TOKEN == 'YES' ) THEN
                     CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                     IF ( TOKEN == '' ) THEN
                          CALL FERR ( INT2(8283), 'GCONST: value SIGMA '// &
     &                                'after keyword STRUCTURE_ADMITTANCE '// &
     &                                ' was epxected, but nothing was found' )
                          STOP 'BATCH(GCONST): Abnormal termination'
                        ELSE IF ( TOKEN == 'SIGMA' ) THEN
!
! ----------------------- ionospheric scale constraint
!
                          CALL SPLITSTRING ( STRINH, TOKEN, STRINH )
                          IF ( INDEX ( TOKEN, '.' ) == 0 ) THEN
                               TOKEN = TOKEN(1:I_LEN(TOKEN))//'.0' 
                          END IF
                          READ ( UNIT=TOKEN, FMT=*, IOSTAT=IOS ) IOS_SIG_BATCH
                          IF ( IOS .NE. 0 ) THEN
                               IF ( IOS .NE. 0 ) THEN
                                    CALL FERR ( INT2(8284), 'GCONST: error in decoding '// &
     &                                  'the reciprocal weight of IONOSPHERE_SCALE '// &
     &                                  TOKEN(1:I_LEN(TOKEN))//' is '// &
     &                                  'not a REAL*8 number' )
                                    STOP 'BATCH(GCONST): Abnormal termination'
                               END IF
                          END IF
                     END IF
                END IF
            ELSE
!
! ------------- Unknown keyword
!
                CALL FERR ( INT2(8285), '(GCONST) Unknown constraint '// &
     &                     'keyword: '//TOKEN(1:16), INT2(0), INT2(0) )
                STOP 'BATCH(GCONST): Abnormal termination'
            ENDIF
          ELSE IF ( TOKEN .EQ. 'NONE') THEN
!
! --------- NONE keyword
!
            IF ( KNON ) THEN
                 CALL FERR ( INT2(8286), 'Qualifier NONE used twice', INT2(0), &
     &                INT2(0) )
            END IF
            KNON = .TRUE.
            KVAL = .FALSE.
          ELSE
            CALL FERR ( INT2(8287), '(GCONST) unknown keyword: '//TOKEN(1:16), &
     &                  INT2(0), INT2(0) )
          ENDIF
        ENDDO
!
! ----- Read next record
!
        LENGTH = CFREAD ( STRINH )
      ENDDO
!
! --- deal with possible case of too many things there
!
      IF ( KNON .AND. KVAL ) THEN
           CALL FERR ( INT2(8288), '(GCONST) none must appear by itself', &
     &          INT2(0), INT2(0) )
        ELSE IF ( .NOT.(KVAL.OR.KNON) ) THEN
           CALL FERR ( INT2(8289), 'Missing keywords from $constraints', &
     &          INT2(0), INT2(0) )
        ELSE
           CALL CFUNRD ( LENGTH, STRINH )
      ENDIF
!
      RETURN
      END  !#! GCONST  #!#
