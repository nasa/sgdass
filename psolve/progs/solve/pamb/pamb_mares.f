      SUBROUTINE PAMB_MARES ( IPAR, NOBS, DBOBJ, OBSSCA, OBSBAS, RES, &
     &                        OBSAOB, PAMBI, KAMB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PAMB_MARES  reads array of residuals made in group-delay  *
! *   solution. Then it produces mixed phase delay residuals: PX_GXS,    *
! *   PS_GXS, resolve phase delay ambiguities for X- and S-band on the   *
! *   basis PG_GXS and PS_GXS residuals. It updates ambiguities and      *
! *   phase delay observables in the arrays of data astructures OBSBAS,  *
! *   OBSAOB and writes phase delay-related stuff in fields of the array *
! *   PAMBI.                                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    IPAR ( INTEGER*4 ) -- Switch of the mode of work:                 *
! *                          IPAR = 0 -- not to do phase delay ambiguity *
! *                                      resolution;                     *
! *                          IPAR = 1 -- make phase delay ambiguity      *
! *                                      resolution to the interval      *
! *                                      [-0.5, 0.5] phase turn.         *
! *    NOBS ( INTEGER*4 ) -- Total number of observations in the session *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *  OBSSCA ( RECORD    ) -- Array of data structures which keeps        *
! *                          scan-dependent information about the        *
! *                          session.                                    *
! *  OBSBAS ( RECORD    ) -- Array of data structures which keeps        *
! *                          baseline dependent information about the    *
! *                          session.                                    *
! *     RES ( RECORD    ) -- Array of data structures keeping            *
! *                          information about residuals.                *
! *  OBSAOB ( RECORD    ) -- Array of data structures which keeps        *
! *                          additional information about observables    *
! *                          some quantities to be missed in database/   *
! *                          superfiles and which is essential for       *
! *                          making phase delay ambiguity resolving.     *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   PAMBI ( RECORD    ) -- Array of data structures keeping            *
! *                          information about phase delays, their       *
! *                          errors, ambiguities and etc.                *
! *    KAMB ( INTEGER*4 ) -- Number of ambiguities which were changed.   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  12-NOV-1997  PAMB_MARES  v3.10 (c)  L. Petrov 02-MAR-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'obser.i'
      INCLUDE   'vtd.i'
      INCLUDE   'pamb.i'
      INTEGER*4  IPAR, NOBS, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(NOBS)
      TYPE ( RES_O__STRU ) ::  RES(NOBS)
      TYPE ( AOB__STRU   ) ::  OBSAOB(NOBS)
      TYPE ( PAMBI__STRU ) ::  PAMBI(NOBS)
      REAL*8      PI2, P2I
      PARAMETER ( PI2=2.D0*3.141592653589793D0, P2I=PI2/4.D0 )
      REAL*8      RATE_CALC, RATE_OBS_X, RATE_OBS_S, RATE_ERR_X, RATE_ERR_S, &
     &            COR_RATE, ADDERR_RATE, TAUSB_OBS_X, TAUSB_OBS_S, &
     &            TAUSB_ERR_X, TAUSB_ERR_S, FREQ_RATE_X, FREQ_RATE_S, &
     &            RATE_OC, TAU_E, RATE_E, TAU_M
      REAL*8      TAU_GR_OC, TAU_PX_OC, TAU_PS_OC, Q_GR, Q_GR_ERR, ADDW_FRQ, ADDW_USED
      REAL*8      ADDW_EXT(MAX_OBS), ADDERR_GR_TAU
      LOGICAL*4   F_ONEBAND, DATYP_INQ
      CHARACTER   BUF_EDIT(MAX_OBS)*16, BUF_ADDW(MAX_OBS)*64
      LOGICAL*1   FL_EDIT, FL_ADDW, FL_ADDW_IONO, FL_ADDW_BW, FL_SUP(MAX_OBS)
      INTEGER*4   J1, J2, J3, NE, N_WEI, IAMB_X, IAMB_S, IND_OBS, KAMB, IER
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN
!
      KAMB = 0
!
! --- Determine: whether we should apply ionosphere free linear combination
! --- of group/phase delay or we should apply one-band difference phase-group.
! --- It depends on what is the solution type.
!
      IF ( DATYP_INQ ( DBOBJ%IDATYP, GX__DTP) .OR. &
     &     DATYP_INQ ( DBOBJ%IDATYP, GS__DTP) .OR. &
     &     DATYP_INQ ( DBOBJ%IDATYP, PX__DTP) .OR. &
     &     DATYP_INQ ( DBOBJ%IDATYP, PS__DTP) .OR. &
     &     DATYP_INQ ( DBOBJ%IDATYP, SNG_X__DTP  ) .OR. &
     &     DATYP_INQ ( DBOBJ%IDATYP, SNG_S__DTP  ) .OR. &
     &     DATYP_INQ ( DBOBJ%IDATYP, GRPONL__DTP ) .OR. &
     &     DATYP_INQ ( DBOBJ%IDATYP, GRPRAT__DTP ) .OR. &
     &     DATYP_INQ ( DBOBJ%IDATYP, SNBONL__DTP )      ) THEN
!
           F_ONEBAND = .TRUE.
        ELSE
           F_ONEBAND = .FALSE.
      END IF
!
      IF ( ILEN(EDIT_FIL) > 0 ) THEN
           FL_EDIT = .TRUE.
           FL_SUP  = .FALSE.
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT ( EDIT_FIL, MAX_OBS, BUF_EDIT, NE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7371, IUER, 'PAMB_MARES', 'Error in an attempt '// &
     &              'to read external edit flag file '//EDIT_FIL )
                RETURN
           END IF
           DO 410 J1=1,NE
              IF ( BUF_EDIT(J1)(1:1) == '#' ) GOTO 410
              IF ( ILEN(BUF_EDIT(J1)) == 0  ) GOTO 410
              CALL CHIN ( BUF_EDIT(J1), IND_OBS )
              IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                   FL_SUP(IND_OBS) = .TRUE.
              END IF
   410     CONTINUE 
        ELSE 
           FL_EDIT = .FALSE.
           FL_SUP  = .FALSE.
      END IF
!
      IF ( ILEN(ADDW_FIL) > 0 ) THEN
           FL_ADDW = .TRUE.
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( ADDW_FIL, MAX_OBS, BUF_ADDW, N_WEI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7372, IUER, 'PAMB_MARES', 'Error in an attempt '// &
     &              'to read external additive flag file '//ADDW_FIL )
                RETURN
           END IF
!
           IF ( BUF_ADDW(1)(1:LEN(LABEL__ADDW)) .NE. LABEL__ADDW       .AND. &
     &          BUF_ADDW(1)(1:LEN(LABEL__ADDW_V1)) .NE. LABEL__ADDW_V1       ) THEN
                CALL ERR_LOG ( 7373, IUER, 'PAMB_MARES', 'Wrong 1st line of the '// &
     &              'external additive flag file '//TRIM(ADDW_FIL)// &
     &              ' -- '//TRIM(BUF_ADDW(1))//' while '//LABEL__ADDW// &
     &              ' was expected'  )
                RETURN
           END IF
           IF ( BUF_ADDW(3)(15:24) .NE. DBOBJ%NAME(1:10) ) THEN
                CALL ERR_LOG ( 7374, IUER, 'PAMB_MARES', 'Experiment name '// &
     &              'defined in external additive flag file '//TRIM(ADDW_FIL)// &
     &              ' -- '//BUF_ADDW(3)(15:24)//' does not match to the '// &
     &              'experiment being processed '//DBOBJ%NAME )
                RETURN
           END IF
           ADDW_EXT = 0.0D0
           FL_ADDW_IONO = .FALSE.
           FL_ADDW_BW   = .FALSE.
           DO 420 J2=1,N_WEI
              IF ( BUF_ADDW(J2)(1:29) == '# Baseline weights: included'    ) FL_ADDW_BW = .TRUE.
              IF ( BUF_ADDW(J2)(1:31) == '# Baseline weights: independent' ) FL_ADDW_BW = .FALSE.
              IF ( BUF_ADDW(J2)(1:1) == '#' ) GOTO 420
              IF ( ILEN(BUF_ADDW(J2)) == 0  ) GOTO 420
              CALL CHIN ( BUF_ADDW(J2)(10:15), IND_OBS )
              IF ( BUF_ADDW(J2)(1:32) == '# Average Ionospheric frequency:' ) THEN
                   READ ( UNIT=BUF_ADDW(J2)(34:46), FMT='(F13.5)' ) ADDW_FRQ
                   FL_ADDW_IONO = .TRUE.
              END IF
              IF ( IND_OBS > 0 .AND. IND_OBS .LE. MAX_OBS ) THEN
                   READ ( UNIT=BUF_ADDW(J2)(27:38), FMT='(D12.5)' ) ADDW_EXT(IND_OBS)
              END IF
   420     CONTINUE 
        ELSE 
           FL_ADDW_IONO = .FALSE.
           FL_ADDW_BW   = .FALSE.
           FL_ADDW      = .FALSE.
           ADDW_EXT     = 0.0D0
      END IF
!
      DO 430 J3=1,NOBS
!
! ------ Clear the J3-th PAMBI record
!
         CALL NOUT ( ML_PAM, PAMBI(J3) )
!
! ------ 1. Make O-C for group delay solution currently applied.
!
         RATE_CALC   = 0.0D0
         COR_RATE    = 0.0D0
         ADDERR_RATE = 0.0
         TAUSB_OBS_X = 0.0D0
         TAUSB_OBS_S = 0.0D0
         TAUSB_ERR_X = 0.0D0
         TAUSB_ERR_S = 0.0D0
         RATE_OBS_X  = 0.0D0
         RATE_OBS_S  = 0.0D0
         RATE_ERR_X  = 0.0D0
         RATE_ERR_S  = 0.0D0
         FREQ_RATE_X = 1.D4
         FREQ_RATE_S = 2.D4
!
         IF ( OBSBAS(J3)%FREQ_IONO_GR_OPP .LT. MIN__FRQ*1.D6 ) THEN
              OBSBAS(J3)%FREQ_IONO_GR_OPP = -1.0
         END IF
         IF ( OBSBAS(J3)%FREQ_IONO_PH_OPP .LT. MIN__FRQ*1.D6 ) THEN
              OBSBAS(J3)%FREQ_IONO_PH_OPP = -1.0
         END IF
!
         IF ( FL_ADDW ) THEN
              IF ( FL_ADDW_IONO ) THEN
!
! ---------------- Case when we have additive weigth corrections due to ionospheric 
! ---------------- contribution. These additive weigtht corrections are compuited 
! ---------------- at frequency ADDW_FRQ which in general not the same as 
! ---------------- the effecive ionospheric frequency of the obsevation.
! ---------------- Therefore, we scale that additive weight correction by 
! ---------------- the ratio of frequencies
!
                   IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) ) THEN
                        ADDW_USED = ADDW_EXT(J3) * (ADDW_FRQ/OBSBAS(J3)%FREQ_IONO_GR)**2
                      ELSE IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
                        ADDW_USED = ADDW_EXT(J3) * (ADDW_FRQ/OBSBAS(J3)%FREQ_IONO_GR_OPP)**2
                   END IF
                 ELSE
                   ADDW_USED = ADDW_EXT(J3)
              END IF
!
! ----------- Apply external additive in quadrature reciprocal weight correction
!
              ADDERR_GR_TAU = DSQRT ( OBSBAS(J3)%TAUGR_ERR_COR**2 + ADDW_USED**2 )
            ELSE
              ADDERR_GR_TAU = OBSBAS(J3)%TAUGR_ERR_COR
         END IF
!
         CALL MAKE_OC ( OBSBAS(J3)%TAU_C, RATE_CALC, OBSBAS(J3)%TAU_COR, &
     &          COR_RATE, ADDERR_GR_TAU, OBSBAS(J3)%TAUPH_ERR_COR, &
     &          ADDERR_RATE, OBSBAS(J3)%TAUGR_OBS, OBSBAS(J3)%TAUGR_OBS_OPP, &
     &          OBSBAS(J3)%TAUPH_OBS, OBSBAS(J3)%TAUPH_OBS_OPP, &
     &          TAUSB_OBS_X, TAUSB_OBS_S, &
     &          OBSBAS(J3)%TAUGR_ERR, OBSBAS(J3)%TAUGR_ERR_OPP, &
     &          OBSBAS(J3)%TAUPH_ERR, OBSBAS(J3)%TAUPH_ERR_OPP, &
     &          TAUSB_ERR_X, TAUSB_ERR_S, &
     &          RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &          OBSBAS(J3)%FREQ_IONO_GR, OBSBAS(J3)%FREQ_IONO_GR_OPP, &
     &          OBSBAS(J3)%FREQ_IONO_PH, OBSBAS(J3)%FREQ_IONO_PH_OPP, &
     &          FREQ_RATE_X, FREQ_RATE_S, OBSBAS(J3)%AUTO_SUP, &
     &          OBSBAS(J3)%USER_SUP, OBSBAS(J3)%USER_REC, OBSBAS(J3)%DTEC_FLG, &
     &          IDATYP, OPP_STATUS, PAMB_STATUS, &
     &          TAU_GR_OC, RATE_OC, TAU_E, RATE_E )
!
! ------ TAU_M -- contribution of estimation
!
         TAU_M = TAU_GR_OC - RES(J3)%PSF_DEL
!
! ------ Calculate group delay ionosphere correction and its formal error.
! ------ It may need for some archaic SOLVE programs.
!
         IF ( OBSBAS(J3)%FREQ_IONO_GR > MIN__FRQ*1.D6  .AND. &
     &        DATYP_INQ ( DBOBJ%IDATYP, FUSED__DTP )     .AND. &
     &        ( DABS(OBSBAS(J3)%FREQ_IONO_GR - OBSBAS(J3)%FREQ_IONO_GR_OPP) < MIN__FRQ*1.D6 ) ) THEN
              WRITE ( 6, * ) 'J3= ', J3
              WRITE ( 6, * ) 'OBSBAS(J3)%FREQ_IONO_GR     = ', OBSBAS(J3)%FREQ_IONO_GR 
              WRITE ( 6, * ) 'OBSBAS(J3)%FREQ_IONO_GR_OPP = ', OBSBAS(J3)%FREQ_IONO_GR_OPP
              CALL ERR_LOG ( 7375, IUER, 'PAMB_MARES', 'Ionospheric effective '//&
     &            'frequencies are too close to each others' )
              RETURN 
         END IF
         IF ( DABS(OBSBAS(J3)%FREQ_IONO_GR - OBSBAS(J3)%FREQ_IONO_GR_OPP) < 1.D0 ) THEN
              OBSBAS(J3)%FREQ_IONO_GR = OBSBAS(J3)%FREQ_IONO_GR_OPP + 1.D0
         ENDIF
         IF ( DABS(OBSBAS(J3)%FREQ_IONO_PH - OBSBAS(J3)%FREQ_IONO_PH_OPP) < 1.D0 ) THEN
              OBSBAS(J3)%FREQ_IONO_PH = OBSBAS(J3)%FREQ_IONO_PH_OPP + 1.D0
         ENDIF
         Q_GR = -( OBSBAS(J3)%TAUGR_OBS - OBSBAS(J3)%TAUGR_OBS_OPP )* &
     &             OBSBAS(J3)%FREQ_IONO_GR**2 * OBSBAS(J3)%FREQ_IONO_GR_OPP**2/ &
     &           ( OBSBAS(J3)%FREQ_IONO_GR**2 - OBSBAS(J3)%FREQ_IONO_GR_OPP**2 )
         Q_GR_ERR = DSQRT ( OBSBAS(J3)%TAUGR_ERR**2 + &
     &                      OBSBAS(J3)%TAUGR_ERR_OPP**2 )* &
     &             OBSBAS(J3)%FREQ_IONO_GR**2 * OBSBAS(J3)%FREQ_IONO_GR_OPP**2/ &
     &           ( OBSBAS(J3)%FREQ_IONO_GR**2 - OBSBAS(J3)%FREQ_IONO_GR_OPP**2 )
         IF ( IDATYP .EQ. GRPONL__DTP .OR. IDATYP .EQ. GRPRAT__DTP ) THEN
!
! ----------- Substract group delay ionosphere correction when solution
! ----------- type was set in compatibility mode with archaic style
!
              TAU_M = TAU_M - Q_GR/OBSBAS(J3)%FREQ_IONO_GR**2
         END IF
!
! ------ PAMBI(J3).TAU_CA is theoretical delay plus contribution from estimation
!
         PAMBI(J3)%TAU_CA  = OBSBAS(J3)%TAU_C + OBSBAS(J3)%TAU_COR + TAU_M
         PAMBI(J3)%PSF_DEL = RES(J3)%PSF_DEL
         IF ( F_ONEBAND ) THEN
!
! ----------- 2a. Produce one-band X-band phase delay observable from group
! ----------- delay observables and post-fit residuals
!
              TAU_PX_OC = ( OBSBAS(J3)%TAUPH_OBS - OBSBAS(J3)%TAUGR_OBS ) + &
     &                      RES(J3)%PSF_DEL
              TAU_E     = DSQRT ( OBSBAS(J3)%TAUPH_ERR**2 + &
     &                            OBSBAS(J3)%TAUGR_ERR**2 + &
     &                            OBSBAS(J3)%TAUGR_ERR_COR**2 + &
     &                            ADDW_EXT(J3)**2 )
           ELSE
!
! ----------- 2b. Make o-c for PX_GXS ionosphere free linear combination
!
              CALL MAKE_OC ( OBSBAS(J3)%TAU_C, RATE_CALC, OBSBAS(J3)%TAU_COR + &
     &             TAU_M, COR_RATE, OBSBAS(J3)%TAUGR_ERR_COR, &
     &             OBSBAS(J3)%TAUPH_ERR_COR, ADDERR_RATE, OBSBAS(J3)%TAUGR_OBS, &
     &             OBSBAS(J3)%TAUGR_OBS_OPP, OBSBAS(J3)%TAUPH_OBS, &
     &             OBSBAS(J3)%TAUPH_OBS_OPP, TAUSB_OBS_X, TAUSB_OBS_S, &
     &             OBSBAS(J3)%TAUGR_ERR, OBSBAS(J3)%TAUGR_ERR_OPP, &
     &             OBSBAS(J3)%TAUPH_ERR, OBSBAS(J3)%TAUPH_ERR_OPP, &
     &             TAUSB_ERR_X, TAUSB_ERR_S, &
     &             RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &             OBSBAS(J3)%FREQ_IONO_GR, OBSBAS(J3)%FREQ_IONO_GR_OPP, &
     &             OBSBAS(J3)%FREQ_IONO_PH, OBSBAS(J3)%FREQ_IONO_PH_OPP, &
     &             FREQ_RATE_X, FREQ_RATE_S, OBSBAS(J3)%AUTO_SUP, &
     &             OBSBAS(J3)%USER_SUP, OBSBAS(J3)%USER_REC, OBSBAS(J3)%DTEC_FLG, &
     &             PX_GXS__DTP, OPP_STATUS, PAMB_STATUS, &
     &             TAU_PX_OC, RATE_OC, TAU_E, RATE_E )
          END IF
!
          PAMBI(J3)%RES_PX_GXS = TAU_PX_OC
          PAMBI(J3)%ERR_PX_GXS = TAU_E
!
! ------- Make X-band phase delay ambiguity resolution for the interval
! ------- [-0.5, 0.5] phase turns. Updating phase delays in OBSBAS
! ------- array also.
!
          IF ( IPAR .EQ. 1 ) THEN
               IF ( DABS(TAU_PX_OC * OBSBAS(J3)%FREQ_OBSV_PH) < 2.0D9 ) THEN
                    IAMB_X    = - NINT( TAU_PX_OC * OBSBAS(J3)%FREQ_OBSV_PH )
                  ELSE 
                    IAMB_X    = 0
               END IF
               TAU_PX_OC =  TAU_PX_OC + IAMB_X * (1.D0/OBSBAS(J3)%FREQ_OBSV_PH)
               IF ( TAU_PX_OC .LT. -0.5D0/OBSBAS(J3)%FREQ_OBSV_PH  ) THEN
                    IAMB_X    = IAMB_X    + 1
                    TAU_PX_OC = TAU_PX_OC + 1.0D0/OBSBAS(J3)%FREQ_OBSV_PH
               END IF
!
               IF ( TAU_PX_OC .GT.  0.5D0/OBSBAS(J3)%FREQ_OBSV_PH  ) THEN
                    IAMB_X    = IAMB_X    - 1
                    TAU_PX_OC = TAU_PX_OC - 1.0D0/OBSBAS(J3)%FREQ_OBSV_PH
               END IF
!
! ------------ Update observatbles and residuals for changes in ambiguities
!
               CALL ERR_PASS ( IUER, IER )
               CALL AMB_UPDATE ( PAMB__XBAND, IAMB_X, OBSBAS(J3), PAMBI(J3), &
     &                           IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 7376, IUER, 'PAMB_MARES', 'Error in '// &
     &                  'AMB_UPDATE' )
                    RETURN 
               END IF
               IF ( IAMB_X .NE. 0 ) KAMB = KAMB + 1
          END IF
          PAMBI(J3)%STATUS_X   = 1
!
          IF ( F_ONEBAND ) THEN
!
! ----------- 3a. Producre one-band X-band phase delay observable from group
! ----------- delay observables and post-fit residuals
!
              TAU_PS_OC = ( OBSBAS(J3)%TAUPH_OBS_OPP - OBSBAS(J3)%TAUGR_OBS ) + &
     &                    RES(J3)%PSF_DEL
              TAU_E     = DSQRT ( OBSBAS(J3)%TAUPH_ERR_OPP**2 + &
     &                            OBSBAS(J3)%TAUGR_ERR**2     + &
     &                            OBSBAS(J3)%TAUGR_ERR_COR**2 + &
     &                            ADDW_EXT(J3)**2 )
           ELSE
!
! ----------- 3b. Make o-c for PS_GXS ionosphere free linear combination
!
              CALL MAKE_OC ( OBSBAS(J3)%TAU_C, RATE_CALC, OBSBAS(J3)%TAU_COR + &
     &             TAU_M, COR_RATE, OBSBAS(J3)%TAUGR_ERR_COR, &
     &             OBSBAS(J3)%TAUPH_ERR_COR, &
     &             ADDERR_RATE, OBSBAS(J3)%TAUGR_OBS, OBSBAS(J3)%TAUGR_OBS_OPP, &
     &             OBSBAS(J3)%TAUPH_OBS, OBSBAS(J3)%TAUPH_OBS_OPP, &
     &             TAUSB_OBS_X, TAUSB_OBS_S, &
     &             OBSBAS(J3)%TAUGR_ERR, OBSBAS(J3)%TAUGR_ERR_OPP, &
     &             OBSBAS(J3)%TAUPH_ERR, OBSBAS(J3)%TAUPH_ERR_OPP, &
     &             TAUSB_ERR_X, TAUSB_ERR_S, &
     &             RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &             OBSBAS(J3)%FREQ_IONO_GR, OBSBAS(J3)%FREQ_IONO_GR_OPP, &
     &             OBSBAS(J3)%FREQ_IONO_PH, OBSBAS(J3)%FREQ_IONO_PH_OPP, &
     &             FREQ_RATE_X, FREQ_RATE_S, &
     &             OBSBAS(J3)%AUTO_SUP, OBSBAS(J3)%USER_SUP, OBSBAS(J3)%USER_REC,  &
     &             OBSBAS(J3)%DTEC_FLG, PS_GXS__DTP, OPP_STATUS, &
     &             PAMB_STATUS, TAU_PS_OC, RATE_OC, TAU_E, RATE_E )
         END IF
!
         PAMBI(J3)%RES_PS_GXS = TAU_PS_OC
         PAMBI(J3)%ERR_PS_GXS = TAU_E
!
! ------ Make S-band phase delay ambiguity resolution for the interval
! ------ [-0.5, 0.5] phase turns. Updating phase delays in OBSBAS
! ------ array also.
!
         IF ( IPAR .EQ. 1 ) THEN
              IAMB_S    = -NINT ( TAU_PS_OC * OBSBAS(J3)%FREQ_OBSV_PH_OPP )
              TAU_PS_OC =  TAU_PS_OC + IAMB_S* &
     &                               ( 1.D0/OBSBAS(J3)%FREQ_OBSV_PH_OPP )
              IF ( TAU_PS_OC .LT. -0.5D0/OBSBAS(J3)%FREQ_OBSV_PH_OPP )THEN
                   IAMB_S    = IAMB_S    + 1
                   TAU_PS_OC = TAU_PS_OC + 1.0D0/OBSBAS(J3)%FREQ_OBSV_PH_OPP
              END IF
!
              IF ( TAU_PS_OC .GT. 0.5D0/OBSBAS(J3)%FREQ_OBSV_PH_OPP ) THEN
                   IAMB_S    = IAMB_S    - 1
                   TAU_PS_OC = TAU_PS_OC - 1.0D0/OBSBAS(J3)%FREQ_OBSV_PH_OPP
              END IF
!
! ----------- Update observables and residuals for changes in ambiguities
!
              CALL ERR_PASS ( IUER, IER ) 
              CALL AMB_UPDATE ( PAMB__SBAND, IAMB_S, OBSBAS(J3), PAMBI(J3), &
     &                          IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7377, IUER, 'PAMB_MARES', 'Error in '// &
     &                 'AMB_UPDATE' )
                   RETURN 
              END IF
              IF ( IAMB_S .NE. 0 ) KAMB = KAMB + 1
         END IF
!
         PAMBI(J3)%STATUS_S = 1
!
! ------ 4. Make o-c for P_PXS ionosphere free linear combination.
! ------ Storing it and its formal error in record of PAMBI data structure
!
         CALL MAKE_OC ( OBSBAS(J3)%TAU_C, RATE_CALC, OBSBAS(J3)%TAU_COR + TAU_M, &
     &        COR_RATE, OBSBAS(J3)%TAUGR_ERR_COR, OBSBAS(J3)%TAUPH_ERR_COR, &
     &        ADDERR_RATE, OBSBAS(J3)%TAUGR_OBS, OBSBAS(J3)%TAUGR_OBS_OPP, &
     &        OBSBAS(J3)%TAUPH_OBS, OBSBAS(J3)%TAUPH_OBS_OPP, &
     &        TAUSB_OBS_X, TAUSB_OBS_S, &
     &        OBSBAS(J3)%TAUGR_ERR, OBSBAS(J3)%TAUGR_ERR_OPP, &
     &        OBSBAS(J3)%TAUPH_ERR, OBSBAS(J3)%TAUPH_ERR_OPP, &
     &        TAUSB_ERR_X, TAUSB_ERR_S, &
     &        RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &        OBSBAS(J3)%FREQ_IONO_GR, OBSBAS(J3)%FREQ_IONO_GR_OPP, &
     &        OBSBAS(J3)%FREQ_IONO_PH, OBSBAS(J3)%FREQ_IONO_PH_OPP, &
     &        FREQ_RATE_X, FREQ_RATE_S, OBSBAS(J3)%AUTO_SUP, &
     &        OBSBAS(J3)%USER_SUP, OBSBAS(J3)%USER_REC, &
     &        OBSBAS(J3)%DTEC_FLG, P_PXS__DTP, OPP_STATUS, PAMB_STATUS, &
     &        PAMBI(J3)%RES_P_PXS,  RATE_OC, PAMBI(J3)%ERR_P_PXS, RATE_E )
 430  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PAMB_MARES  !#!#
