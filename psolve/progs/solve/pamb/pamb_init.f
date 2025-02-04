      SUBROUTINE PAMB_INIT ( IDB2, DBNAME, DBOBJ, OBSHLD, OBSBAS, CHIOBJ, &
     &           PAMBI, F_AMB, F_SUP, F_NWT, INIT_WEI, IOBS_USED, IOBS_SPAC, &
     &           IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PAMB_INIT  calculates phase delay observables using       *
! *   primary observables (phase and frequencies) written in oborg       *
! *   area. It updates phase delay observables in oborg area and in      *
! *   OBSBAS data structures.                                            *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *     IDB2 ( INTEGER*2 ) -- Index of the considered database in the    *
! *                           scratch file.                              *
! *   DBNAME ( CHARACTER ) -- Database name.                             *
! *    DBOBJ ( RECORD    ) -- Data structure which keeps general         *
! *                           information about the database such as     *
! *                           lists of the objects.                      *
! *    F_AMB ( LOGICAL*4 ) -- Flag: if .TRUE. then ambiguities will be   *
! *                           initialized.                               *
! *    F_SUP ( LOGICAL*4 ) -- Flag: if .TRUE. then suppression status of *
! *                           phase delay observables will be            *
! *                           initialized.                               *
! *    F_NWT ( LOGICAL*4 ) -- Flag: if .TRUE. then new phase delay       *
! *                           baseline-dependent reweight constants will *
! *                           be set up.                                 *
! * INIT_WEI ( LOGICAL*4 ) -- Values of baseline reweight constant       *
! *                           (in seconds). INIT_WEI is reweight         *
! *                           constant common for all baselines. It      *
! *                           modifies weights as                        *
! *                           NEW_WEI = 1./SQRT ( SIGMA_OBS**2 +         *
! *                                               INIT_WEI**2    )       *
! *                           where SIGMA_OBS -- formal error of used    *
! *                           observable.                                *
! *                           NB: baseline dependent constant us set up  *
! *                           ONLY IF F_NWT is .TRUE. Otherwise INIT_WEI *
! *                           is ignored and old value of reweight       *
! *                           constant is in use.                        *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! * IOBS_USED ( INTEGER*4 ) -- Number of used phase delay observations.  *
! * IOBS_SPAC ( INTEGER*4 ) -- Number of observations which were flagged *
! *                            as unrecoverable for phase delay          *
! *                            solutions since they have
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *   OBSHLD ( RECORD    ) -- Data structure which keeps the current     *
! *                           status of the database and some            *
! *                           session-dependent information.             *
! *   OBSBAS ( RECORD    ) -- Array of data structures which keeps       *
! *                           baseline dependent information about the   *
! *                           session.                                   *
! *   CHIOBJ ( RECORD    ) -- Object with data structure for keeping     *
! *                           accumulators of the chi-squares and        *
! *                           their mathematical expectations.           *
! *    PAMBI ( RECORD    ) -- Array of data structures keeping           *
! *                           information about phase delays, their      *
! *                           errors, ambiguities and etc.               *
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
! *  ###  06-MAR-98    PAMB_INIT   v2.2  (c)  L. Petrov  09-NOV-98  ###  *
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
      LOGICAL*4  F_AMB, F_SUP, F_NWT
      INTEGER*4  IOBS_USED, IOBS_SPAC, IUER
      REAL*8     INIT_WEI
!
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( HLD_O__STRU ) ::  OBSHLD
      TYPE ( BAS_O__STRU ) ::  OBSBAS(*)
      TYPE ( CHIACC__STRU ) ::  CHIOBJ
      TYPE ( PAMBI__STRU ) ::  PAMBI(*)
      REAL*8     INIT_WEI_USED, SFREQ_DEF
      PARAMETER  ( SFREQ_DEF = 2.21299D9 ) ! Default frequency for S-band
      INTEGER*4    J1, J2, ITEST, IER
      PARAMETER  ( ITEST = 0 ) ! How many first records to print (for test)
!
      CALL ACS_OBSFIL ( 'O' )
      IOBS_USED = 0
      DO 410 J1=1,DBOBJ%L_OBS
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
!
! ------ Tricky place: CALC-convention about sign of feed horn
!
         IF ( F_AMB ) THEN
              DPH = ( ( TOTPH*PI__NUM/180.0 + FEED_HORN )/PI2 + NPHAM4 ) *PHAMI8
              OBSBAS(J1)%TAUPH_OBS = DPH*1.D-6
              PAMBI(J1)%NPHAMB_X = 0
              PAMBI(J1)%STATUS_X = 0
!
              DPHXS = ( ( TOTPH_S*PI__NUM/180.0 + FEED_HORN )/PI2 + NPHAM4_S ) * &
     &                PHAMI8_S
!
! ----------- We calculate "original phase delay" for the S-band.
! ----------- In fact it doesn't have sense, except for CNPLT which
! ----------- requires it when it is going to 'resolve ambiguities to 0'
!
              DPH_ORIG_S = DPHXS
              OBSBAS(J1)%TAUPH_OBS_OPP = DPHXS*1.D-6
              PAMBI(J1)%NPHAMB_S = 0
              PAMBI(J1)%STATUS_S = 0
         END IF
!
         IF ( PHAMI8_S .LE. 0.0D0 ) IUNWP = INT2(8)
         IF ( IUNWP .EQ. INT2(0)  ) IOBS_USED = IOBS_USED + 1
!
         IF ( IUNWP .EQ. INT2(8) ) THEN
!
! ----------- Special trick for the case when there is no matching observation.
! ----------- Of couse, S-band phase delay observable is senseless for such
! ----------- a case, but it is more convenient to keep dummy observable to be
! ----------- near to real X-band observable, otherwise CNPLT may try to show
! ----------- such fantom observable at ugle space and spoil the plot of real
! ----------- data
!
              DPHXS    = DPH
              TOTPH_S  = TOTPH
              PHAMI8_S = 1.D0/(SFREQ_DEF*1.D-6)
              NPHAM4_S = DPHXS/PHAMI8_S
!
              OBSBAS(J1)%TAUPH_OBS_OPP = DPHXS*1.D-6
              OBSBAS(J1)%TAUGR_OBS_OPP = OBSBAS(J1)%TAUGR_OBS
              OBSBAS(J1)%FREQ_OBSV_PH_OPP = SFREQ_DEF
              OBSBAS(J1)%FREQ_IONO_GR_OPP = OBSBAS(J1)%FREQ_OBSV_PH_OPP
              OBSBAS(J1)%FREQ_IONO_PH_OPP = OBSBAS(J1)%FREQ_OBSV_PH_OPP
              OBSBAS(J1)%LQUAL_CHR_OPP = ' 0'
              OBSBAS(J1)%TAUGR_ERR_OPP = OBSBAS(J1)%TAUGR_ERR
              OBSBAS(J1)%TAUPH_ERR_OPP = OBSBAS(J1)%TAUPH_ERR
         ENDIF
         IF ( F_SUP ) THEN
!
! ----------- Setting the same downweight flags for phase delay observables
! ----------- as for group delay observables
!
              IUNWP = INT2(0)
              IF ( IUNW  .EQ. INT2(1) ) IUNWP = IUNW
!
              IF ( IUNWP .EQ. INT2(0) ) THEN
                   CALL SBIT ( OBSBAS(J1)%UACSUP, PSUP__UAS, 0 )
                 ELSE
                   CALL SBIT ( OBSBAS(J1)%UACSUP, PSUP__UAS, 1 )
              END IF
         END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         if ( j1 .le. itest ) then                                      ! %%%%
            WRITE ( 6, * ) '=== j1=',j1,' totph=',totph,' totph_s = ',totph_s   ! %%%%
            WRITE ( 6, * ) ' npham4 = ', npham4,' npham4_s = ', npham4_s        ! %%%%
            WRITE ( 6, * ) ' feed_horn = ',feed_horn,' opp_status = ',opp_status  ! %%
            WRITE ( 6, * ) ' iunw = ',iunw,' iunwp=',iunwp                      ! %%%%
            WRITE ( 6, * ) ' TAU_C     = ',OBSBAS(J1)%TAU_C                     ! %%%%
            WRITE ( 6, * ) ' TAUGR_OBS = ',OBSBAS(J1)%TAUGR_OBS                 ! %%%%
            WRITE ( 6, * ) ' TAUGR_ERR = ',OBSBAS(J1)%TAUGR_ERR                 ! %%%%
            WRITE ( 6, * ) ' TAUPH_OBS = ',OBSBAS(J1)%TAUPH_OBS                 ! %%%%
            WRITE ( 6, * ) ' TAUPH_ERR = ',OBSBAS(J1)%TAUPH_ERR                 ! %%%%
            WRITE ( 6, * ) ' FREQ_IONO_GR = ',OBSBAS(J1)%FREQ_IONO_GR           ! %%%%
            WRITE ( 6, * ) ' FREQ_IONO_PH = ',OBSBAS(J1)%FREQ_IONO_PH           ! %%%%
            WRITE ( 6, * ) ' FREQ_OBSV_PH = ',OBSBAS(J1)%FREQ_OBSV_PH           ! %%%%
!
            WRITE ( 6, * ) ' TAUGR_OBS_OPP = ',OBSBAS(J1)%TAUGR_OBS_OPP         ! %%%%
            WRITE ( 6, * ) ' TAUGR_ERR_OPP = ',OBSBAS(J1)%TAUGR_ERR_OPP         ! %%%%
            WRITE ( 6, * ) ' TAUPH_OBS_OPP = ',OBSBAS(J1)%TAUPH_OBS_OPP         ! %%%%
            WRITE ( 6, * ) ' TAUPH_ERR_OPP = ',OBSBAS(J1)%TAUPH_ERR_OPP         ! %%%%
            WRITE ( 6, * ) ' FREQ_IONO_GR_OPP = ',OBSBAS(J1)%FREQ_IONO_GR_OPP   ! %%%%
            WRITE ( 6, * ) ' FREQ_IONO_PH_OPP = ',OBSBAS(J1)%FREQ_IONO_PH_OPP   ! %%%%
            WRITE ( 6, * ) ' FREQ_OBSV_PH_OPP = ',OBSBAS(J1)%FREQ_OBSV_PH_OPP   ! %%%%
!
            WRITE ( 6, * ) ' TAU_COR = ',OBSBAS(J1)%TAU_COR                     ! %%%%
            WRITE ( 6, * ) ' TAUGR_ERR_COR = ',OBSBAS(J1)%TAUGR_ERR_COR         ! %%%%
            WRITE ( 6, * ) ' TAUPH_ERR_COR = ',OBSBAS(J1)%TAUPH_ERR_COR         ! %%%%
!
            WRITE ( 6, * ) ' LQUAL_CHR = ',OBSBAS(J1)%LQUAL_CHR                 ! %%%%
            WRITE ( 6, * ) ' LQUAL_CHR_OPP = ',OBSBAS(J1)%LQUAL_CHR_OPP         ! %%%%
         end if                                                         ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! ------ Writing updated record back to oborg
!
         CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
 410  CONTINUE
      CALL ACS_OBSFIL ( 'C' )
      IF ( F_AMB ) THEN
!
! -------- Storing the status of the opposite band bask to socom
!
           CALL SBIT  ( OPP_STATUS, OPP_SET1__BIT, INT2(1) )
           CALL SBIT  ( OPP_STATUS, OPP_SET2__BIT, INT2(1) )
           PAMB_STATUS = INT2(1)
           CALL USE_COMMON ( 'OWC' )
      END IF
!
      IF ( F_NWT ) THEN
!
! -------- Substitution to the initial value of baseline dependent correction
! -------- to weights
!
           INIT_WEI_USED = INIT_WEI
           IF ( INIT_WEI_USED .LT. 1.D-13 ) INIT_WEI_USED = 1.D-13
!
           DO 420 J2=1,DBOBJ%L_BAS
              CHIOBJ%WEIPH_BAS(J2) = 1.D0/INIT_WEI_USED
 420       CONTINUE
!
! -------- Write weights to NAMFIL
!
           CALL ERR_PASS ( IUER, IER )
           CALL IO_WGT   ( 2, IDB2, DBOBJ, CHIOBJ%WEIGR_BAS, CHIOBJ%WEIPH_BAS, &
     &                     IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6371, IUER, 'PAMB_INIT', 'Error duiring '// &
     &              'putting weights to NAMFIL while database '//DBOBJ%NAME// &
     &              ' was processing' )
                RETURN
           END IF
!
! -------- Setting global weights
!
           CHIOBJ%WEIPH_GLO  = CHIOBJ%WEIPH_BAS(1)
           CHIOBJ%LAST_FIELD = 1
!
! -------- Refresh the weights in the data structure OBSBAS
!
           CALL ERR_PASS    ( IUER, IER )
           CALL REFRESH_WEI ( OBSHLD, DBOBJ, OBSBAS, CHIOBJ%WEIGR_BAS, &
     &                        CHIOBJ%WEIPH_BAS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6372, IUER, 'PAMB_INIT', 'Error during '// &
     &              'refreshing weights in data structure OBSBAS while '// &
     &              'database '//DBOBJ%NAME//' was processing' )
                RETURN
           END IF
!
! -------- Refreshing weights in the data structure OBSHLD
!
           CALL COPY_V ( MO_BAS, CHIOBJ%WEIPH_BAS, OBSHLD%WEIPH_BAS )
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL PAS_CHECK ( DBOBJ, OBSBAS, IOBS_SPAC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6373, IUER, 'PAMB_INIT', 'Error during '// &
     &         'check of consistency of phase delay ambiguity spacings '// &
     &         'detected while database '//DBOBJ%NAME//' was processing' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PAMB_INIT  #!#
