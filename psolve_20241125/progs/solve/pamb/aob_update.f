      SUBROUTINE AOB_UPDATE ( IPAR, IDB2, DBNAME, DBOBJ, OBSAOB, OBSBAS, &
     &                        PAMBI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  AOB_UPDATE  updates (or don't updates) some observables   *
! *   in oborg-area and/or OBSBAS for information which contains in      *
! *   AOB-file. It first checks bits of the status oborg-update and      *
! *   obsbas update in opp_status. If both them are set it doesn't do    *
! *   anything. Else it reads  AOB-file. Then it scans all observations  *
! *   and update fields of oborg-area and/or OBSBAS data structure if    *
! *   they have not previously updated. It adds there information which  *
! *   was missed in database/superfiles but which is needed for phase    *
! *   delay ambiguity resolution. Information only for X-band, only for  *
! *   S-band or for both bands maybe updated in according with the value *
! *   IPAR.                                                              *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *    IPAR ( INTEGER*4 ) -- Switch of working mode:                     *
! *                          IPAR = 1 -- update information for X-band   *
! *                                      only;                           *
! *                          IPAR = 2 -- update information for S-band   *
! *                                      only;                           *
! *                          IPAR = 3 -- update information for both     *
! *                                      X-band and S-band.              *
! *    IDB2 ( INTEGER*2 ) -- Index of the considered database in the     *
! *                          scratch file.                               *
! *  DBNAME ( CHARACTER ) -- Database name.                              *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *  OBSAOB ( RECORD    ) -- Array of data structures which keeps        *
! *                          additional information about observables    *
! *                          and some quantities to be missed in         *
! *                          database/superfiles and which are           *
! *                          essential for making phase delay ambiguity  *
! *                          resolving.                                  *
! *  OBSBAS ( RECORD    ) -- Array of data structures which keeps        *
! *                          baseline dependent information about the    *
! *                          session.                                    *
! *   PAMBI ( RECORD    ) -- Array of data structures keeping            *
! *                          information about phase delays, their       *
! *                          errors, ambiguities and etc.                *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  16-FEB-98   AOB_UPDATE   v1.5  (c)  L. Petrov  08-JUN-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'oborg.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'pamb.i'
      INTEGER*2  IDB2
      CHARACTER  DBNAME*(*)
      INTEGER*4  IPAR, IUER
!
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(*)
      TYPE ( AOB__STRU ) ::  OBSAOB(*)
      TYPE ( PAMBI__STRU ) ::  PAMBI(*)
      CHARACTER  FINAM*255, AOB_DIR*80
      LOGICAL*4  BAD_OBS, FREAD_OBORG
      INTEGER*4  NBT, IER, LUN, J1, NOBS
      INTEGER*2    INT2_ARG
      INTEGER*4    INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! --- Get name of AOB directory
!
      CALL CLRCH     (  AOB_DIR )
      CALL GETENVAR  ( 'AOB_DIR',  AOB_DIR )
!
! --- Create the name of AOB file
!
      CALL CATLG_FNAME ( DBOBJ%NAME(1:10), 0, AOB_DIR, '.AOB', FINAM )
!
! --- Openning AOB file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6251, IUER, 'AOB_UPDATE', 'Error during attempt '// &
     &                   ' to open AOB file '//FINAM )
           RETURN
      END IF
!
! --- Reading AOB file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, 4, NOBS, NBT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6252, IUER, 'AOB_UPDATE', 'Error during '// &
     &         'reading the first record of AOB file '//FINAM )
           RETURN
      END IF
!
      IF ( NOBS .NE. DBOBJ%L_OBS ) THEN
           WRITE ( 6, * ) 'Number of observations in the database '// &
     &            'and AOB file is not the same:'
           WRITE ( 6, * ) ' AOB-file: ',NOBS
           WRITE ( 6, * ) ' Database  ',DBOBJ%L_OBS
           CALL HIT_CONT ( %VAL(0), %VAL(0) )
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, LEN_AOB*NOBS, OBSAOB, NBT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6253, IUER, 'AOB_UPDATE', 'Error during '// &
     &         'reading the last record of AOB file '//FINAM )
          RETURN
      END IF
!
! --- Closing AOB file
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      FREAD_OBORG = .TRUE.
!
      IF ( FREAD_OBORG ) THEN
!
! -------- Openning file with oborg area
!
           CALL ACS_OBSFIL ( 'O' )
      END IF
!
      DO 410 J1=1,DBOBJ%L_OBS
         IF ( FREAD_OBORG ) THEN
              CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
         END IF
!
! ------ Always put in oborg unique information from AOB file about missed
! ------ S-band observables
!
         TOTPH_S     = OBSAOB(J1)%PHAS_S*180.0/PI__NUM
         PHAMI8_S    = 1.D6 * ( 1.D0/OBSAOB(J1)%FREQPHA_S )
         SNR_S       = OBSAOB(J1)%SNR_S
         AMPL_S      = OBSAOB(J1)%AMPL_S
         FEED_HORN   = OBSAOB(J1)%HORN_COR
         NPHAM4_S    = OBSAOB(J1)%NPHAMB_S
         DOBS_ORIG_S = OBSAOB(J1)%TAU_GR_ERR_S *1.D6
         FAMB_S      = OBSAOB(J1)%GRAMB_SP_S
         NUMAMB_S    = OBSAOB(J1)%NGRAMB_S
         ROBSXS      = OBSAOB(J1)%ROBS_S
         RERRXS      = OBSAOB(J1)%RERR_S
         LQUALXS     = OBSAOB(J1)%LQUAL_S
         DNB_S       = OBSAOB(J1)%DNB_S
         DNBER_S     = OBSAOB(J1)%DNBER_S
         FRINGE_S_FINAM = OBSAOB(J1)%FRINGE_S_FINAM
!
! ------ Special trick to bypass erroneous iunwp=8 flag
!
         IF ( .NOT. BAD_OBS ( LQUAL )    .AND. &
     &        .NOT. BAD_OBS ( LQUALXS )  .AND. &
     &        IUNWP .EQ. INT2(8)                ) THEN
!
              IUNWP = IUNW
         END IF
!
         IF ( IPAR .EQ. 1  .OR.  IPAR .EQ. 3 ) THEN
!
! ----------- Put in oborg.i area information about X-band observables
!
              FREQ_SKY = OBSAOB(J1)%FREQPHA_X*1.D-6
              EFFREQ   = OBSAOB(J1)%GRFREQ_X*1.D-6
              PHEFFREQ = OBSAOB(J1)%PHFREQ_X*1.D-6
              SNR      = OBSAOB(J1)%SNR_X
              FAMB     = OBSAOB(J1)%GRAMB_SP_X
              NUMAMB   = OBSAOB(J1)%NGRAMB_X
              NPHAM4   = OBSAOB(J1)%NPHAMB_X
              DOBS     = ( OBSAOB(J1)%TAU_GR_X + &
     &                     OBSAOB(J1)%NGRAMB_X*OBSAOB(J1)%GRAMB_SP_X )*1.D6
              TOTPH    = OBSAOB(J1)%PHAS_X*180.0/PI__NUM
              DERR     = OBSAOB(J1)%TAU_GR_ERR_X
              DPHER    = OBSAOB(J1)%TAU_PH_ERR_X
              PHAMI8   = 1.D6 * ( 1.D0/OBSAOB(J1)%FREQPHA_X )
!
              DOBS_ORIG_S = OBSAOB(J1)%TAU_GR_ERR_X*1.D6
              FAMB        = OBSAOB(J1)%GRAMB_SP_X
              NUMAMB      = OBSAOB(J1)%NGRAMB_X
              DNB         = OBSAOB(J1)%DNB_X
              DNBER       = OBSAOB(J1)%DNBER_X
              FRINGE_X_FINAM = OBSAOB(J1)%FRINGE_X_FINAM
!
! ----------- Tricky place: CALC-convention about sign of feed horn
!
              DPH      = ( ( OBSAOB(J1)%PHAS_X + OBSAOB(J1)%HORN_COR )/PI2 + &
     &                       OBSAOB(J1)%NPHAMB_X ) /OBSAOB(J1)%FREQPHA_X *1.D6
              PAMBI(J1)%NPHAMB_X = 0
              PAMBI(J1)%STATUS_X = 0
!CC
!
! ----------- Putting information about X-band observables in OBSBAS
! ----------- data structure
!
              OBSBAS(J1)%TAUGR_OBS = &
     &                 ( OBSAOB(J1)%TAU_GR_X + &
     &                   OBSAOB(J1)%NGRAMB_X*OBSAOB(J1)%GRAMB_SP_X )
              OBSBAS(J1)%TAUGR_ERR = OBSAOB(J1)%TAU_GR_ERR_X
              OBSBAS(J1)%TAUPH_OBS = &
     &                 ( ( OBSAOB(J1)%PHAS_X + OBSAOB(J1)%HORN_COR )/PI2 + &
     &                     OBSAOB(J1)%NPHAMB_X ) /OBSAOB(J1)%FREQPHA_X
              OBSBAS(J1)%TAUPH_ERR    = OBSAOB(J1)%TAU_PH_ERR_X
              OBSBAS(J1)%FREQ_IONO_GR = OBSAOB(J1)%GRFREQ_X
              OBSBAS(J1)%FREQ_IONO_PH = OBSAOB(J1)%PHFREQ_X
              OBSBAS(J1)%FREQ_OBSV_PH = OBSAOB(J1)%FREQPHA_X
         END IF
!
         IF ( IPAR .EQ. 2  .OR.  IPAR .EQ. 3 ) THEN
!
! ----------- Putting information into oborg area about S-band
!
              EFFREQ_XS   = OBSAOB(J1)%GRFREQ_S*1.D-6
              PHEFFREQ_XS = OBSAOB(J1)%PHFREQ_S*1.D-6
              DOBSXS      =   ( OBSAOB(J1)%TAU_GR_S + &
     &                          OBSAOB(J1)%NGRAMB_S*OBSAOB(J1)%GRAMB_SP_S )*1.D6
              DERRXS      = OBSAOB(J1)%TAU_GR_ERR_S
              DPHERXS     = OBSAOB(J1)%TAU_PH_ERR_S
              DPHXS       = ( ( OBSAOB(J1)%PHAS_S + OBSAOB(J1)%HORN_COR )/PI2 + &
     &                        OBSAOB(J1)%NPHAMB_S ) /OBSAOB(J1)%FREQPHA_S *1.D6
              DNB_S       = OBSAOB(J1)%DNB_S *1.D6
              DNBER_S     = OBSAOB(J1)%DNBER_S
              FRINGE_S_FINAM = OBSAOB(J1)%FRINGE_S_FINAM
              PAMBI(J1)%NPHAMB_S = 0
              PAMBI(J1)%STATUS_S = 0
!CC
!
! ----------- Putting information about S-band observables in OBSBAS
! ----------- data structure
!
              OBSBAS(J1)%TAUGR_OBS_OPP = &
     &          ( OBSAOB(J1)%TAU_GR_S + &
     &            OBSAOB(J1)%NGRAMB_S*OBSAOB(J1)%GRAMB_SP_S )
              OBSBAS(J1)%TAUGR_ERR_OPP = OBSAOB(J1)%TAU_GR_ERR_S
              OBSBAS(J1)%TAUPH_OBS_OPP = &
     &           ( ( OBSAOB(J1)%PHAS_S + OBSAOB(J1)%HORN_COR )/PI2 + &
     &               OBSAOB(J1)%NPHAMB_S ) /OBSAOB(J1)%FREQPHA_S
              OBSBAS(J1)%TAUPH_ERR_OPP    = OBSAOB(J1)%TAU_PH_ERR_S
              OBSBAS(J1)%FREQ_IONO_GR_OPP = OBSAOB(J1)%GRFREQ_S
              OBSBAS(J1)%FREQ_IONO_PH_OPP = OBSAOB(J1)%PHFREQ_S
              OBSBAS(J1)%FREQ_OBSV_PH_OPP = OBSAOB(J1)%FREQPHA_S
         END IF
!
         CALL SBIT ( OBSBAS(J1)%SUPSTAT(1), WPAS__SPS, INT2(0) ) ! Lift bit
         IF ( IUNWP .EQ. INT2(98) ) IUNWP = INT2(0)              ! of wrong spac
         OBSBAS(J1)%AUTO_SUP = IBCLR ( OBSBAS(J1)%AUTO_SUP, INT4(WPAS__SPS) )
!
         IF ( IUNWP .EQ. INT2(0)  ) THEN
!
! ------------ Setting the same downweight flags for phase delay observables
! ------------ as for group delay observables IN THE CASE WHEN PHASE DELAY
! ------------ OBSERVABLES HAS NOT BEEN DOWNWEIGHTED
!
               IUNWP = IUNW
         END IF
!
! ------ Writing updated record back to oborg
!
         IF ( FREAD_OBORG ) THEN
              CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
         END IF
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!          type *,'j1=',j1,'  freq_s=',1.d0/phami8_s   ! %%%%%%%%
!          type *,'          reffreq_xs=',reffreq_xs   ! %%%%%%%%
!          type *,'           effreq_xs=',effreq_xs    ! %%%%%%%%
!          type *,'         pheffreq_xs=',pheffreq_xs  ! %%%%%%%%
!       type *,'j1=',j1,'--  FREQ = ',    FREQ   ,' <-- ',
!     #                                OBSAOB(J1).FREQPHA_X*1.D-6
!       type *,'   EFFREQ = ',EFFREQ     ,' <-- ',OBSAOB(J1).GRFREQ_X*1.D-6
!       type *,' PHEFFREQ = ',PHEFFREQ   ,' <-- ',OBSAOB(J1).PHFREQ_X*1.D-6
!       type *,'      SNR = ',SNR        ,' <-- ',OBSAOB(J1).SNR_X
!       write ( 6, * ) '     FAMB = ',FAMB       ,' <-- ',OBSAOB(J1)%GRAMB_SP_X
!       type *,'   NUMAMB = ',NUMAMB     ,' <-- ',OBSAOB(J1).NGRAMB_X
!       type *,'   NPHAM4 = ',NPHAM4     ,' <-- ',OBSAOB(J1).NPHAMB_X
!       type *,'     DOBS = ',DOBS       ,' <-- ',
!     #        (OBSAOB(J1).TAU_GR_X + OBSAOB(J1).NGRAMB_X*OBSAOB(J1).GRAMB_SP_X)*
!     #        1.D6
!                       OBSAOB(J1).NPHAMB_X = NPHAM4
!       type *,'      DPH = ',DPH        ,' <-- ',
!     #  ( ( OBSAOB(J1).PHAS_X + OBSAOB(J1).HORN_COR)/PI2 +
!     #               OBSAOB(J1).NPHAMB_X) /OBSAOB(J1).FREQPHA_X *1.D6
!       type *,'     DERR = ',DERR       ,' <-- ',OBSAOB(J1).TAU_GR_ERR_X
!       type *,'    DPHER = ',DPHER      ,' <-- ',OBSAOB(J1).TAU_PH_ERR_X
!       type *,'   PHAMI8 = ',PHAMI8     ,' <-- ',1.D6/OBSAOB(J1).FREQPHA_X
!!
!       type *,'j1=',j1,'--  '
!       type *,'   EFFREQ_XS = ',EFFREQ_XS    ,' <-- ',OBSAOB(J1).GRFREQ_S*1.D-6
!       type *,' PHEFFREQ_XS = ',PHEFFREQ_XS  ,' <-- ',OBSAOB(J1).PHFREQ_S*1.D-6
!       type *,'     DOBS_XS = ',DOBSXS       ,' <-- ',
!     #        (OBSAOB(J1).TAU_GR_S + OBSAOB(J1).NGRAMB_S*OBSAOB(J1).GRAMB_SP_S)*
!     #        1.D6
!       type *,'     DERR_XS = ',DERRXS       ,' <-- ',OBSAOB(J1).TAU_GR_ERR_S
!       type *,'    DPHER_XS = ',DPHERXS      ,' <-- ',OBSAOB(J1).TAU_PH_ERR_S
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
 410  CONTINUE
      IF ( FREAD_OBORG ) THEN
           CALL ACS_OBSFIL ( 'C' )
      END IF
!
! --- Storying the status of the opposite band bask to socom
!
      IF ( IPAR .EQ. 2  .OR.  IPAR .EQ. 3 ) THEN
           CALL SBIT  ( OPP_STATUS, OPP_SET1__BIT, INT2(1) )
           CALL SBIT  ( OPP_STATUS, OPP_SET2__BIT, INT2(1) )
      END IF
      CALL USE_COMMON ( 'OWC' )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  AOB_UPDATE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CATLG_FNAME ( DBNAME, IVER, PATH, EXT, FINAM )
! ************************************************************************
! *                                                                      *
! *   Routine  CATLG_FNAME  creates file name for AOB-file using CATLG   *
! *   database name.                                                     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * DBNAME ( CHARACTER ) -- Database name (10 first symbols are examined)*
! *   IVER ( INTEGER*4 ) -- Version number of the database.              *
! *   PATH ( CHARACTER ) -- Line with directory name used for creation   *
! *                         full file name.                              *
! *    EXT ( CHARACTER ) -- Extension (f.e. '.mk3' )                     *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  FINAM ( CHARACTER ) -- Full name (including path) for AOB file.     *
! *                                                                      *
! *  ###  20-NOV-97   CATLG_FNAME  v1.2  (c)  L. Petrov  12-MAY-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IVER
      CHARACTER  DBNAME*(*), PATH*(*), FINAM*(*), EXT*(*)
      CHARACTER  STR*9, STV*4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL CLRCH ( FINAM )
      CALL CLRCH ( STR   )
!
      IF ( DBNAME(1:1) .EQ. '$' ) THEN
!
! -------- Biting away a dollar sign form the beginning if necessary
!
           STR = DBNAME(2:)
         ELSE
           STR = DBNAME
      END IF
!
! --- Setting the last symbol
!
      IF ( STR(LEN(STR):LEN(STR)) .EQ. ' ' ) THEN
           STR(LEN(STR):LEN(STR)) = '_'
        ELSE IF ( STR(LEN(STR):LEN(STR)) .EQ. '*' ) THEN
           STR(LEN(STR):LEN(STR)) = '$'
      END IF
!
! --- Setting version number. But it is not used nowadays
!
      CALL CLRCH  ( STV )
      CALL INCH   ( IVER, STV )
      CALL CHASHR        ( STV )
      CALL BLANK_TO_ZERO ( STV )
!
! --- ... and at least creation of the file name
!
      IF ( ILEN(PATH) .GT. 0 ) THEN
!
! -------- Adding path
!
            IF ( PATH(ILEN(PATH):ILEN(PATH)) .EQ. '/' ) THEN
                 FINAM = PATH(1:ILEN(PATH))//STR//EXT(1:I_LEN(EXT))
               ELSE
                 FINAM = PATH(1:ILEN(PATH))//'/'//STR//EXT(1:I_LEN(EXT))
            ENDIF
         ELSE
!
! --------- Path was not available
!
            FINAM = STR//EXT(1:I_LEN(EXT))
      END IF
!
      RETURN
      END  !#!  CATLG_FNAME  #!#
