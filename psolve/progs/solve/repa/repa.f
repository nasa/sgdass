      PROGRAM REPA
! ************************************************************************
! *                                                                      *
! *   Program REPA ( REsidual Plots and Ambiguities) is an interactive   *
! *   tool which plots residuals and allows the users                    *
! *                                                                      *
! *     --  shift ambiguity of a single point (for delays only);         *
! *                                                                      *
! *     --  toggle suppression status of a single point;                 *
! *                                                                      *
! *     --  shift ambiguities for all points at the baseline             *
! *         (for delays only);                                           *
! *                                                                      *
! *     --  suppress all observations with residuals at the baseline     *
! *         greater than the value which the cursor is pointing at;      *
! *                                                                      *
! *     --  restore all observations with residuals at the baseline      *
! *         greater than the value which the cursor is pointing at;      *
! *                                                                      *
! *     --  plot delays, delay rates, their supported linear             *
! *         combinations, meteorological parameters, station and         *
! *         baseline calibrations;                                       *
! *                                                                      *
! *  ### 01-DEC-2004      REPA     v2.0 (c)  L. Petrov  06-JAN-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'prfil.i'
      INCLUDE   'socom.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'diagi.i'
      INCLUDE   'repa.i'
      TYPE     ( REP__TYPE ) :: REP
      CHARACTER  STR*128, STR1*80, REPA_CONFIL*128
      LOGICAL*4  LEX
      CHARACTER  GET_VERSION*54
      INTEGER*4  NBOX_BEG, MBOX, NC_BOX, NR_BOX, NPAGE, NBUT
      INTEGER*4  J1, J2, ICODE, IUER
      LOGICAL*4  FL_PAGE_KEY
      INTEGER*2  NUMDB, LDBNAM(5,15), IDBV(15)
      INTEGER*4, EXTERNAL :: GET_DBVERS, ILEN, I_LEN, LTM_DIF
      CHARACTER, EXTERNAL :: GET_DBNAME*10
!
      INCLUDE   'repa_version.i' ! Set revision date of the current version
      STR(1:54) = GET_VERSION()
      CALL PRE_PROG()
!
! --- This is to circumvent a bug in HP-UX Fortran compiler
!
#ifdef CHAR_BUG ! It is a bug in HP-UX Fortran compiler
      REPA__BUTLET(10)(3:3) = 220 ! NB: Dangerours place!!!
      REPA__BUTLET(11)(3:3) = 221 ! Do not forget to update if
!                                 ! the button list in repa.i is changed
#else
      REPA__BUTLET(10)(3:3) = CHAR(220) ! NB: Dangerours place!!!
      REPA__BUTLET(11)(3:3) = CHAR(221) ! Do not forget to update if
!                                       ! the button list in repa.i is changed
#endif
!
! --- Initialize object REP
!
      CALL NOUT ( SIZEOF(REP), REP )
      REP%STATUS = REPA__INIT
!
! --- Build the name of the configuration file for repa
!
      CALL CLRCH ( REPA_CONFIL )
      REPA_CONFIL = PRE_SCR_DIR(1:PRE_SD_LEN)//'REPA'//PRE_LETRS
!
! --- Check whether this file exists
!
      CALL CHASHL ( REPA_CONFIL )
      INQUIRE ( FILE=REPA_CONFIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 7701, IUER, 'REPA', 'Configuration file for '// &
     &         'REPA: '//REPA_CONFIL(1:I_LEN(REPA_CONFIL))//' was not '// &
     &         'found. If you do not know where it is gone, you may need '// &
     &         'to create it by running solve_reset' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read commong blocks associated with experiments
!
      CALL OPENNAMFIL()
      CALL USE_COMMON   ( 'ORC' )
      CALL SOCOM_EXT()
      CALL USE_PARFIL   ( 'ORC' )
      CALL USE_GLBFIL   ( 'OR'  )
      CALL USE_GLBFIL_4 ( 'RC'  )
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBEND )
      IF ( IDBEND(1) .EQ. 0 ) THEN
           WRITE ( 6, * ) 'No observations were processed. Nothing to do'
           CALL HIT_CONT ( %VAL(0), %VAL(0) )
           CALL END_PROG()
      END IF
      CALL SET_SIGNAL_CTRLC ( 3 )
      REP%BAND_NAM(1) = BAND_NAM(1)
      REP%BAND_NAM(2) = BAND_NAM(2)
!
      IF ( ILEN(PIMA_CNT) > 0 ) THEN
           CALL GETENVAR ( PIMA_CNT, STR )
           IF ( ILEN(STR) == 0 ) THEN
                STR = PIMA_CNT
           END IF
           IUER = -1
           CALL PIMA_PARSE ( STR, REP%FPL_DIR, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, * ) 'PIMA_CNT= ', PIMA_CNT
                IUER = -1
                CALL ERR_LOG ( 7702, IUER, 'REPA', 'Error in an attempt '// &
     &              'to parse PIMA configuration file '//STR )
                CONTINUE 
           END IF
         ELSE 
           CALL CLRCH ( REP%FPL_DIR )
      END IF
      CALL GETENVAR ( 'SOLVE_GIF_VIEWER', STR )
      IF ( ILEN(STR) > 0 ) THEN
           REP%GIF_VIEWER = STR
         ELSE 
           REP%GIF_VIEWER = SOLVE_GIF_VIEWER
      END IF
!
! --- Fill the REP%DBNAME_STR field
!
      DBNAME_CH = GET_DBNAME()
      DBNAME_VER = GET_DBVERS()
      CALL CLRCH ( REP%DBNAME_STR )
      REP%DBNAME_STR = DBNAME_CH
      REP%DBNAME_STR(12:12) = '<'
      CALL INCH ( DBNAME_VER, REP%DBNAME_STR(13:15) )
      CALL CHASHL ( REP%DBNAME_STR(13:15) )
      REP%DBNAME_STR(ILEN(REP%DBNAME_STR)+1:) = '>'
!
! --- Parse configuration file
!
      IUER = -1
      CALL REPA_PARSE_CNF ( REP, REPA_CONFIL, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7703, -2, 'REPA', 'Error in processing REPA '// &
     &         'configuration file '//REPA_CONFIL )
           CALL EXIT ( 1 )
      END IF
!
! --- Store observable type
!
      REP%DATYP_I2 = IDATYP
!
! --- Write down REPA status
!
      IUER = -1
      CALL REPA_WRISTAT ( REP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7704, -2, 'REPA', 'Error in attempt to write '// &
     &         'status file '//REP%CNF%STAT_FILE )
           CALL EXIT ( 1 )
      END IF
!
! --- Load observations
!
      IUER = -1
      CALL REPA_LOAD_OBS ( REP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7705, -2, 'REPA', 'Error in an attempt to load '// &
     &         'the observations in REPA internal data structures' )
           CALL EXIT ( 1 )
      END IF
!
! --- Load residuals
!
      IUER = -1
      CALL REPA_LOAD_RES ( REP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7706, -2, 'REPA', 'Error in an attempt to load '// &
     &         'the residuals in REPA internal data structures' )
           CALL EXIT ( 1 )
      END IF
!
! --- Set internal parameters for plotting specific values as a function of
! --- a specific argument
!
      IUER = -1
      CALL REPA_SETFUNC ( REP, SUPMET, REP%CNF%ARG_IND, REP%CNF%VAL_IND, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7707, -2, 'REPA', 'Error in an attempt to set '// &
     &         'the arguments and values for plotting' )
           CALL EXIT ( 1 )
      END IF
!
! --- Set internal parameters for plotting boxes
!
      IUER = -1
      CALL REPA_SETDIAGI ( REP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7708, -2, 'REPA', 'Error in an attempt to set '// &
     &         'plotting parameters' )
           CALL EXIT ( 1 )
      END IF
!
! --- Select the number of boxes per page and compute the total number of pages
!
      MBOX = MIN ( REP%N_BAS, 25 )
      IF ( MBOX .LE. 4 ) THEN
           NC_BOX = 2
           NR_BOX = 2
         ELSE IF ( MBOX .LE. 9 ) THEN
           NC_BOX = 3
           NR_BOX = 3
         ELSE IF ( MBOX .LE. 16 ) THEN
           NC_BOX = 4
           NR_BOX = 4
         ELSE
           NC_BOX = 5
           NR_BOX = 5
      END IF
      NPAGE = REP%N_BAS/(NC_BOX*NR_BOX)
      IF ( MBOX .LT. REP%N_BAS ) THEN
           IF ( NPAGE*(NC_BOX*NR_BOX) .LT. REP%N_BAS ) NPAGE = NPAGE + 1
           NBUT = REPA__M_BUT
         ELSE
           NBUT = REPA__M_BUT - 2
      END IF
      IF ( NPAGE < 1 ) NPAGE = 1
!
! --- Set parameter ICODE -- the box which should be displayed first.
! --- ICODE = 0 means that all boxes at the specified page should be shown
!
      ICODE = LTM_DIF ( 1, REP%N_BAS, REP%LIS%C_BAS, REP%CNF%BASELINE )
      IF ( ICODE .LE. 0 ) ICODE = 0
      IF ( ICODE .GT. NC_BOX*NR_BOX ) THEN
           ICODE = ICODE - (REP%CNF%PAGE-1)*NC_BOX*NR_BOX
      END IF
!
      WRITE ( 6, '(A)' ) ' '
      IF ( REP%CNF%MARKED_SOURCE .NE. 'NO      ' ) THEN
           CALL CLRCH ( REPA__BUTNAM(8) )
           REPA__BUTNAM(8) = ' Mark a source|'//REP%CNF%MARKED_SOURCE
      END IF
!
! --- Inifinite loop
!
      DO 410 J1=1,1024*1024*1024
!
! ------ Compute the total number of boxes at the current page and the
! ------ index of the first displayed box
!
         NBOX_BEG = 1 + (REP%CNF%PAGE-1)*NC_BOX*NR_BOX
         MBOX = REP%N_BAS - (REP%CNF%PAGE-1)*NC_BOX*NR_BOX
         IF ( REP%N_BAS .GT. NC_BOX*NR_BOX ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( REP%CNF%PAGE, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( NPAGE, STR1 )
              REP%TITLE = REP%TITLE(1:I_LEN(REP%TITLE))//' page '// &
     &                    STR(1:I_LEN(STR))//'('//STR1(1:I_LEN(STR1))//')'
         END IF
         IF ( MBOX .GT. NC_BOX*NR_BOX ) MBOX = NC_BOX*NR_BOX
         IF ( NBOX_BEG > REP%N_BAS ) THEN
              write ( 6, * ) ' NBOX_BEG = ', NBOX_BEG, ' REP%N_BAS = ', REP%N_BAS 
              write ( 6, * ) ' REP%CNF%PAGE = ', REP%CNF%PAGE
              write ( 6, * ) ' NC_BOX = ', NC_BOX, ' NR_BOX = ', NR_BOX
              CALL ERR_LOG ( 7709, -2, 'REPA', 'Parameter REP%CNF%PAGE '// &
     &            'is wrong. You may have stale file '// &
     &             REPA_CONFIL(1:ILEN(REPA_CONFIL)-6)//'RPST'// &
     &             REPA_CONFIL(ILEN(REPA_CONFIL)-1:ILEN(REPA_CONFIL))// &
     &             ' Try to remove it. This should help.' )
              CALL EXIT ( 1 )
         END IF
!
! ------ Call the utility which will make the plots
!
         IUER = -1
         CALL MULTI_DIAGI ( REP%TITLE, MBOX, NC_BOX, NR_BOX, &
     &                      REP%TITS(NBOX_BEG), NBUT, REPA__BUTNAM, &
     &                      REPA__BUTLET, REP%PREF(NBOX_BEG), &
     &                      REP%DIAGI(NBOX_BEG), ICODE, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 7710, -2, 'REPA', 'Error in an attempt '// &
     &                       'to make a plot ' )
              CALL EXIT ( 1 )
         END IF
!
! ------ Analyze completion code
!
         FL_PAGE_KEY = .FALSE.
         IF ( ICODE .EQ. -3 ) THEN
              FL_PAGE_KEY = .TRUE.
!
! ----------- A user hit PageUp when Multi_DiaGi showed the last plot
! ----------- Show the first plot at the next page
!
              REP%CNF%PAGE = REP%CNF%PAGE + 1
              IF ( REP%CNF%PAGE .GT. NPAGE ) REP%CNF%PAGE = 1
              ICODE = 1
           ELSE IF ( ICODE .EQ. -2 ) THEN
!
! ----------- A user hit PageDn when Multi_DiaGi showed the first plot.
! ----------- Show the last plot at the prior page
!
              FL_PAGE_KEY = .TRUE.
              REP%CNF%PAGE = REP%CNF%PAGE - 1
              IF ( REP%CNF%PAGE .LT. 1 ) REP%CNF%PAGE = NPAGE
              ICODE = NC_BOX*NR_BOX
              IF ( ICODE .GT. REP%N_BAS - (REP%CNF%PAGE-1)*NC_BOX*NR_BOX ) THEN
                   ICODE = REP%N_BAS - (REP%CNF%PAGE-1)*NC_BOX*NR_BOX
              END IF
          ELSE IF ( ICODE .LE. 0  .OR.  ICODE .GT. REPA__M_BUT ) THEN
!
! ----------- Unsupported exit code. Treat it as exit
!
              GOTO  810
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'X' ) THEN
!
! ----------- Apparently, user bored to play with REPA. Let's exit
!
              GOTO  810
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'Q' ) THEN
!
! ----------- Apparently, user bored to play with REPA. Let's exit
!
              GOTO  810
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'T' ) THEN
!
! ----------- Call the interactive utility for change plot argument
!
              CALL REPA_CHANGE_PAR ( REP%DIAGI(1)%IDEV, &
     &                               "Change plot's argument", REP__M_ARG, &
     &                               REP%CH_ARG, REP%CNF%ARG_IND )
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'V' ) THEN
!
! ----------- Call the interactive utility for change plot value
!
              CALL REPA_CHANGE_PAR ( REP%DIAGI(1)%IDEV, &
     &                               "Change plot's value", REP__M_VAL, &
     &                               REP%CH_VAL, REP%CNF%VAL_IND )
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'G' ) THEN
!
! ----------- Set the status of REP%CNF%BOU_IND "Show only good observations"
!
              REP%CNF%BOU_IND = REPA__I_GOO
              REP%CNF%BOU_BOX = REPA__LET_BOX(REP%CNF%BOU_IND )
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'B' ) THEN
!
! ----------- Set the status of REP%CNF%BOU_IND "Show good and bad observations"
!
              REP%CNF%BOU_IND = REPA__I_BAD
              REP%CNF%BOU_BOX = REPA__LET_BOX(REP%CNF%BOU_IND )
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'L' ) THEN
!
! ----------- Set the status of REP%CNF%BOU_IND "Show good, bad and
! ----------- unrecoverable observations"
!
              REP%CNF%BOU_IND = REPA__I_UNR
              REP%CNF%BOU_BOX = REPA__LET_BOX(REP%CNF%BOU_IND )
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'S' ) THEN
!
! ----------- Set the status of REP%CNF%BOX_SYMMETRIC = Yes  --
! ----------- to plot points around [ -|max(value)}, +|max(value)]
!
              REP%CNF%BOX_SYMMETRIC = 'YES '
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'R' ) THEN
!
! ----------- Set the status of REP%CNF%BOX_SYMMETRIC = No  --
! ----------- to plot points around min(value)}, +(value)
!
              REP%CNF%BOX_SYMMETRIC = 'NO  '
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'N' ) THEN
!
! ----------- Go to the next page
!
              REP%CNF%PAGE = REP%CNF%PAGE + 1
              IF ( REP%CNF%PAGE .GT. NPAGE ) REP%CNF%PAGE = 1
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'P' ) THEN
!
! ----------- Go to the prior page
!
              REP%CNF%PAGE = REP%CNF%PAGE - 1
              IF ( REP%CNF%PAGE .LT. 1 ) REP%CNF%PAGE = NPAGE
           ELSE IF ( REPA__BUTLET(ICODE)(1:1) .EQ. 'M' ) THEN
!
! ----------- Mark a source
!
              IUER = -1
              CALL REPA_SELSOU ( 'Source mark', REPA__M_SOU, REP%LIS%L_SOU,  &
     &                           REP%LIS%C_SOU, REP%LIS%KG_SOU, REP%LIS%KB_SOU, &
     &                           REP%LIS%EF_SOU, REP%LSEL_SOU, REP%IND_SOU_SEL_LAST, &
     &                           REP%IND_SOU_SEL, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7711, -2, 'REPA', 'Error in an attempt to '// &
     &                 'mark a source' )
                   CALL EXIT ( 1 )
              END IF
              IF ( REP%IND_SOU_SEL > 0 ) THEN
                   REP%CNF%MARKED_SOURCE  = REP%LIS%C_SOU(REP%IND_SOU_SEL)
                   CALL CLRCH ( REPA__BUTNAM(8) )
                   REPA__BUTNAM(8) = ' Mark a source|'//REP%CNF%MARKED_SOURCE
                 ELSE
                   REP%CNF%MARKED_SOURCE  = 'NO      '
                   REPA__BUTNAM(8) = 'Mark |a source                  '
              END IF
!
              IUER = -1
              CALL REPA_WRISTAT ( REP, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7712, -2, 'REPA', 'Error in an attempt '// &
     &                 'to write REPA status into the file' )
                   CALL EXIT ( 1 )
              END IF
!
              REP%IND_SOU_SEL_LAST = REP%IND_SOU_SEL
         END IF
!
! ------ We still here? Hmm. Let's re-set internal structure for plotting
! ------ specific arguments and values
!
         IUER = -1
         CALL REPA_SETFUNC ( REP, SUPMET, REP%CNF%ARG_IND, REP%CNF%VAL_IND, &
     &                       IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 7713, -2, 'REPA', 'Error in an attempt to set '// &
     &            'the arguments and values for plotting' )
              CALL EXIT ( 1 )
         END IF
!
! ------ Set internal data structure for plotting
!
         IUER = -1
         CALL REPA_SETDIAGI ( REP, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 7714, -2, 'REPA', 'Error in an attempt to set '// &
     &            'plotting parameters' )
              CALL EXIT ( 1 )
         END IF
!
         IF ( .NOT. FL_PAGE_KEY ) THEN
              ICODE = 0
         END IF
 410  CONTINUE
 810  CONTINUE
!
! --- Write down the current REPA status
!
      IUER = -1
      CALL REPA_WRISTAT ( REP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7715, -2, 'REPA', 'Error in an attempt to write '// &
     &                    'REPA status into the file' )
           CALL EXIT ( 1 )
      END IF
!
! --- Update source estimation flags
!
      CALL USE_COMMON   ( 'OR' )
      CALL SOCOM_EXT()
      DO 420 J2=1,REP%LIS%L_SOU
         IF ( REP%LIS%EF_SOU(J2) ) THEN
              CALL SBIT ( LSTAR(1,1), INT2(J2), INT2(1) )
              CALL SBIT ( LSTAR(1,2), INT2(J2), INT2(1) )
            ELSE
              CALL SBIT ( LSTAR(1,1), INT2(J2), INT2(0) )
              CALL SBIT ( LSTAR(1,2), INT2(J2), INT2(0) )
         END IF
 420  CONTINUE 
      CALL USE_COMMON ( 'WC' )
!
! --- Deallocate dynamic memory used by REPA
!
      IUER = -1
      CALL REPA_QUIT ( REP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7716, -2, 'REPA', 'Error in an attempt to '// &
     &                    'deallocate dynamic memory grabbed by REPA' )
           CALL EXIT ( 1 )
      END IF
!
! --- We worked well, let's take a cup of hot tee and relax
!
      CALL END_PROG()
      END  PROGRAM  REPA  !#!#
