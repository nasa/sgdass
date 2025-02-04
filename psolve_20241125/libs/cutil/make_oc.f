      SUBROUTINE MAKE_OC ( TAU_CALC, RATE_CALC, COR_TAU, &
     &           COR_RATE, ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &           TAUGR_OBS_X, TAUGR_OBS_S, TAUPH_OBS_X, TAUPH_OBS_S, &
     &           TAUSB_OBS_X, TAUSB_OBS_S, TAUGR_ERR_X, TAUGR_ERR_S, &
     &           TAUPH_ERR_X, TAUPH_ERR_S, TAUSB_ERR_X, TAUSB_ERR_S, &
     &           RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &           FREQ_GR_X,   FREQ_GR_S,   FREQ_PH_X,   FREQ_PH_S, &
     &           FREQ_RATE_X, FREQ_RATE_S, &
     &           AUTO_SUP, USER_SUP, USER_REC, DTEC_FLG, IDATYP, &
     &           OPP_STATUS, PAMB_STATUS, TAU_OC, RATE_OC, TAU_E, RATE_E )
! ************************************************************************
! *                                                                      *
! *     Routine  MAKE_OC  applies calibrations for the certain           *
! *   observation and produces O-C which  will be used as a right part   *
! *   of equations of conditions in according with the type of solution  *
! *   kept in the variable IDATYP. It is assumed that all calibrations   *
! *   were calculated beforehand and gathered together.                  *
! *                                                                      *
! *  ###  12-FEB-98     MAKE_OC    v2.9  (c)  L. Petrov 14-FEB-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      REAL*8     TAU_CALC,    RATE_CALC,     COR_TAU, &
     &           COR_RATE,    ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &           TAUGR_OBS_X, TAUGR_OBS_S,   TAUPH_OBS_X,   TAUPH_OBS_S, &
     &           TAUSB_OBS_X, TAUSB_OBS_S,   TAUGR_ERR_X,   TAUGR_ERR_S, &
     &           TAUPH_ERR_X, TAUPH_ERR_S,   TAUSB_ERR_X,   TAUSB_ERR_S, &
     &           RATE_OBS_X,  RATE_OBS_S,    RATE_ERR_X,    RATE_ERR_S, &
     &           FREQ_GR_X,   FREQ_GR_S,     FREQ_PH_X,     FREQ_PH_S, &
     &           FREQ_RATE_X, FREQ_RATE_S,   &
     &           TAU_OC,        RATE_OC,     TAU_E,       RATE_E
      INTEGER*2  AUTO_SUP, USER_SUP, USER_REC, DTEC_FLG, IDATYP, &
     &           OPP_STATUS, PAMB_STATUS
      INTEGER*2  IDATYP_SAVE 
      REAL*8     TAU_O, TAU_C, RATE_O, RATE_C, FREQ_LIM
      LOGICAL*1  FL_USED_GX, FL_USED_GS, FL_USED_GXS
      PARAMETER  ( FREQ_LIM = 1.0D4 ) ! Minimal frequency
      LOGICAL*4  DATYP_INQ, IS_R8_NAN, META_SUPR_INQ
!
      IF (         DATYP_INQ ( IDATYP, GRPRAT__DTP ) ) THEN
!
! -------- Solution type: "Group delay & rate"
!
           TAU_O   = TAUGR_OBS_X
           TAU_E   = DSQRT ( TAUGR_ERR_X**2 + ADDERR_GR_TAU**2 )
!
           RATE_O  = RATE_OBS_X
           RATE_C  = RATE_CALC + COR_RATE
           RATE_OC = RATE_O - RATE_C
           RATE_E  = DSQRT ( RATE_ERR_X**2  + ADDERR_RATE**2   )
         ELSE IF ( DATYP_INQ ( IDATYP, PHSRAT__DTP ) ) THEN
!
! -------- Solution type: "Phase delay & rate"
!
           TAU_O   = TAUPH_OBS_X
           TAU_E   = DSQRT ( TAUPH_ERR_X**2 + ADDERR_PH_TAU**2 )
!
           RATE_O  = RATE_OBS_X
           RATE_C  = RATE_CALC + COR_RATE
           RATE_OC = RATE_O - RATE_C
           RATE_E  = DSQRT ( RATE_ERR_X**2  + ADDERR_RATE**2   )
         ELSE IF ( DATYP_INQ ( IDATYP, SNBRAT__DTP ) ) THEN
!
! -------- Solution type: "N.Band delay & rate"
!
           TAU_O   = TAUSB_OBS_X
           TAU_E   = DSQRT ( TAUSB_ERR_X**2 + ADDERR_GR_TAU**2 )
!
           RATE_O  = RATE_OBS_X
           RATE_C  = RATE_CALC + COR_RATE
           RATE_OC = RATE_O - RATE_C
           RATE_E  = DSQRT ( RATE_ERR_X**2  + ADDERR_RATE**2   )
         ELSE IF ( DATYP_INQ ( IDATYP, GRPONL__DTP ) ) THEN
!
! -------- Solution type: "Group delay only"
!
           TAU_O   = TAUGR_OBS_X
           TAU_E   = DSQRT ( TAUGR_ERR_X**2 + ADDERR_GR_TAU**2 )
!
           RATE_O  = RATE_OBS_X
           RATE_C  = RATE_CALC + COR_RATE
           RATE_OC = RATE_O - RATE_C
           RATE_E  = DSQRT ( RATE_ERR_X**2  + ADDERR_RATE**2   )
         ELSE IF ( DATYP_INQ ( IDATYP, PHSONL__DTP ) ) THEN
!
! -------- Solution type: "Phase delay only"
!
           TAU_O   = TAUPH_OBS_X
           TAU_E   = DSQRT ( TAUPH_ERR_X**2 + ADDERR_PH_TAU**2 )
         ELSE IF ( DATYP_INQ ( IDATYP, SNBONL__DTP ) ) THEN
!
! -------- Solution type: "N.Band delay only"
!
!
           TAU_O   = TAUSB_OBS_X
           TAU_E   = DSQRT ( TAUSB_ERR_X**2 + ADDERR_GR_TAU**2 )
           TAU_E = 2.D-9
!
           RATE_O  = RATE_OBS_X
           RATE_C  = RATE_CALC + COR_RATE
           RATE_OC = RATE_O - RATE_C
           RATE_E  = DSQRT ( RATE_ERR_X**2  + ADDERR_RATE**2   )
         ELSE IF ( DATYP_INQ ( IDATYP, RATONL__DTP ) ) THEN
!
! -------- Solution type: "Rate only"
!
           RATE_O  = RATE_OBS_X
           RATE_C  = RATE_CALC + COR_RATE
           RATE_OC = RATE_O - RATE_C
           RATE_E  = DSQRT ( RATE_ERR_X**2  + ADDERR_RATE**2   )
         ELSE IF ( DATYP_INQ ( IDATYP,  G_GXS__DTP ) ) THEN
!
! -------- Solution type: "G-Gxs combination" -- ionosphere free linear
! -------- combination of group delays at X- and S- bands.
!
           IF ( FREQ_GR_X .GT. FREQ_LIM                   .AND. &
     &          FREQ_GR_S .GT. FREQ_LIM                   .AND. &
     &          DABS(FREQ_GR_X - FREQ_GR_S) .GT. FREQ_LIM       ) THEN
!
                TAU_O   = ( TAUGR_OBS_X*FREQ_GR_X**2 - TAUGR_OBS_S*FREQ_GR_S**2 )/ &
     &                    (             FREQ_GR_X**2 -             FREQ_GR_S**2 )
                TAU_E   = DSQRT ( &
     &                       ( TAUGR_ERR_X**2*FREQ_GR_X**4 + TAUGR_ERR_S**2*FREQ_GR_S**4 )/ &
     &                         ( FREQ_GR_X**2 - FREQ_GR_S**2 )**2 + ADDERR_GR_TAU**2 )
                IF ( DABS(TAUGR_ERR_S) > MAX__DELAY ) THEN
                     TAU_E   = DSQRT ( TAUGR_ERR_X**2 + ADDERR_GR_TAU**2 )
                END IF
              ELSE
                TAU_O = TAUGR_OBS_X
                TAU_E = TAU_ERR__BAD
           END IF
         ELSE IF ( DATYP_INQ ( IDATYP, PX_GXS__DTP ) ) THEN
!
! -------- Solution type: "Px-Gxs combination" -- phase delay for X-band with
! -------- ionosphere calibrations obtained from group delays at X- and S-
! -------- bands.
!
           IF ( FREQ_PH_X .GT. FREQ_LIM  .AND.             &
     &          FREQ_GR_X .GT. FREQ_LIM  .AND.             &
     &          FREQ_GR_S .GT. FREQ_LIM  .AND.             &
     &          DABS(FREQ_GR_X - FREQ_GR_S) .GT. FREQ_LIM  ) THEN
!
                TAU_O   = TAUPH_OBS_X - (TAUGR_OBS_X  - TAUGR_OBS_S)* &
     &                                  (FREQ_GR_X**2 * FREQ_GR_S**2)/ &
     &                    (FREQ_PH_X**2* (FREQ_GR_X**2 - FREQ_GR_S**2))
                TAU_E   = DSQRT ( TAUPH_ERR_X**2 + &
     &                  ( (FREQ_GR_X**2 * FREQ_GR_S**2)/ &
     &                  (FREQ_PH_X**2* (FREQ_GR_X**2 - FREQ_GR_S**2)) )**2 * &
     &                  (TAUGR_ERR_X**2 + TAUGR_ERR_S**2) + &
     &                       ADDERR_PH_TAU**2 )
              ELSE
                TAU_O = TAUPH_OBS_X 
                TAU_E = TAU_ERR__BAD
           END IF
         ELSE IF ( DATYP_INQ ( IDATYP, PS_GXS__DTP ) ) THEN
!
! -------- Solution type: "Ps-Gxs combination" -- phase delay for S-band with
! -------- ionosphere calibrations obtained from group delays at X- and S-
! -------- bands.
!
           IF ( FREQ_PH_S .GT. FREQ_LIM                   .AND. &
     &          FREQ_GR_X .GT. FREQ_LIM                   .AND. &
     &          FREQ_GR_S .GT. FREQ_LIM                   .AND. &
     &          DABS(FREQ_GR_X - FREQ_GR_S) .GT. FREQ_LIM       ) THEN
!
                TAU_O   = TAUPH_OBS_S - (TAUGR_OBS_X  - TAUGR_OBS_S)* &
     &                                  (FREQ_GR_X**2 * FREQ_GR_S**2)/ &
     &                            (FREQ_PH_S**2* (FREQ_GR_X**2 - FREQ_GR_S**2))
                TAU_E   = DSQRT ( TAUPH_ERR_S**2 + &
     &                  ( (FREQ_GR_X**2 * FREQ_GR_S**2)/ &
     &                  (FREQ_PH_S**2* (FREQ_GR_X**2 - FREQ_GR_S**2)) )**2 * &
     &                  (TAUGR_ERR_X**2 + TAUGR_ERR_S**2) + &
     &                  ADDERR_PH_TAU**2 )
              ELSE
                TAU_O = TAU_E
                TAU_E = TAU_ERR__BAD
           END IF
         ELSE IF ( DATYP_INQ ( IDATYP, PX_GX__DTP  ) ) THEN
!
! -------- Ionosphere free linear combination of X-band phase delay and
! -------- X-band group delay
!
           IF ( FREQ_PH_X .GT. FREQ_LIM  .AND. &
     &          FREQ_GR_X .GT. FREQ_LIM        ) THEN
                TAU_O = TAUPH_OBS_X*FREQ_PH_X**2/(FREQ_GR_X**2 + FREQ_PH_X**2) + &
     &                  TAUGR_OBS_X*FREQ_GR_X**2/(FREQ_GR_X**2 + FREQ_PH_X**2)
                TAU_E   = DSQRT ( &
     &                       ( TAUPH_ERR_X* &
     &                         FREQ_PH_X**2/(FREQ_GR_X**2 + FREQ_PH_X**2) )**2 + &
     &                       ( TAUGR_ERR_X* &
     &                         FREQ_GR_X**2/(FREQ_GR_X**2 + FREQ_PH_X**2) )**2 + &
     &                       ADDERR_PH_TAU**2 )
              ELSE
                TAU_O = TAU_ERR__BAD
                TAU_E = TAU_ERR__BAD
           END IF
         ELSE IF ( DATYP_INQ ( IDATYP, PX_GS__DTP  ) ) THEN
!
! -------- Ionosphere free linear combination of X-band phase delay and
! -------- S-band group delay
!
           TAU_O   = TAUPH_OBS_X*FREQ_PH_X**2/(FREQ_GR_S**2 + FREQ_PH_X**2) + &
     &               TAUGR_OBS_S*FREQ_GR_S**2/(FREQ_GR_S**2 + FREQ_PH_X**2)
           IF ( FREQ_PH_X .GT. FREQ_LIM  .AND. &
     &          FREQ_GR_S .GT. FREQ_LIM         ) THEN
                TAU_E   = DSQRT ( &
     &                       ( TAUPH_ERR_X* &
     &                         FREQ_PH_X**2/(FREQ_GR_S**2 + FREQ_PH_X**2) )**2 + &
     &                       ( TAUGR_ERR_S* &
     &                         FREQ_GR_S**2/(FREQ_GR_S**2 + FREQ_PH_X**2) )**2 + &
     &                       ADDERR_PH_TAU**2 )
              ELSE
                TAU_E = TAU_ERR__BAD
           END IF
         ELSE IF ( DATYP_INQ ( IDATYP, PS_GX__DTP  ) ) THEN
!
! -------- Ionosphere free linear combination of S-band phase delay and
! -------- X-band group delay
!
           TAU_O   = TAUPH_OBS_S*FREQ_PH_S**2/(FREQ_GR_X**2 + FREQ_PH_S**2) + &
     &               TAUGR_OBS_X*FREQ_GR_X**2/(FREQ_GR_X**2 + FREQ_PH_S**2)
           IF ( FREQ_PH_S .GT. FREQ_LIM  .AND. &
     &          FREQ_GR_X .GT. FREQ_LIM         ) THEN
                TAU_E   = DSQRT ( &
     &                       ( TAUPH_ERR_S* &
     &                         FREQ_PH_S**2/(FREQ_GR_X**2 + FREQ_PH_S**2) )**2 + &
     &                       ( TAUGR_ERR_X* &
     &                         FREQ_GR_X**2/(FREQ_GR_X**2 + FREQ_PH_S**2) )**2 + &
     &                       ADDERR_PH_TAU**2 )
              ELSE
                TAU_E = TAU_ERR__BAD
           END IF
         ELSE IF ( DATYP_INQ ( IDATYP, PS_GS__DTP  ) ) THEN
!
! -------- Ionosphere free linear combination of S-band phase delay and
! -------- S-band group delay
!
           IF ( FREQ_PH_S .GT. FREQ_LIM  .AND. &
     &          FREQ_GR_S .GT. FREQ_LIM         ) THEN
!
                TAU_O = TAUPH_OBS_S*FREQ_PH_S**2/(FREQ_GR_S**2 + FREQ_PH_S**2) + &
     &                  TAUGR_OBS_S*FREQ_GR_S**2/(FREQ_GR_S**2 + FREQ_PH_S**2)
                TAU_E   = DSQRT ( &
     &                       ( TAUPH_ERR_S* &
     &                         FREQ_PH_S**2/(FREQ_GR_S**2 + FREQ_PH_S**2) )**2 + &
     &                       ( TAUGR_ERR_S* &
     &                         FREQ_GR_S**2/(FREQ_GR_S**2 + FREQ_PH_S**2) )**2 + &
     &                       ADDERR_PH_TAU**2 )
              ELSE
                TAU_O = TAU_ERR__BAD
                TAU_E = TAU_ERR__BAD
           END IF
         ELSE IF ( DATYP_INQ ( IDATYP, P_PXS__DTP  ) ) THEN
!
! -------- Solution type: "P-Pxs combination" -- ionosphere free linear
! -------- compbination of phase delays at X- and S- bands.
!
           TAU_O   = ( TAUPH_OBS_X*FREQ_PH_X**2 - TAUPH_OBS_S*FREQ_PH_S**2 )/ &
     &               (             FREQ_PH_X**2 -             FREQ_PH_S**2 )
           IF ( FREQ_PH_S .GT. FREQ_LIM  .AND. &
     &          FREQ_GR_S .GT. FREQ_LIM         ) THEN
                TAU_E   = DSQRT ( &
     &        ( TAUPH_ERR_X**2*FREQ_PH_X**4 + TAUPH_ERR_S**2*FREQ_PH_S**4 )/ &
     &        (                FREQ_PH_X**2 -                FREQ_PH_S**2 )**2 + &
     &          ADDERR_PH_TAU**2    )
              ELSE
                TAU_E = TAU_ERR__BAD
           END IF
         ELSE IF ( DATYP_INQ ( IDATYP, GX__DTP  ) ) THEN
!
! -------- Solution type "X-band group delay without ionpsphere correction"
!
           TAU_O = TAUGR_OBS_X
           TAU_E = DSQRT ( TAUGR_ERR_X**2 + ADDERR_GR_TAU**2   )
           RATE_O  = RATE_OBS_X
           RATE_C  = RATE_CALC + COR_RATE
           RATE_OC = RATE_O - RATE_C
           RATE_E  = DSQRT ( RATE_ERR_X**2  + ADDERR_RATE**2   )
         ELSE IF ( DATYP_INQ ( IDATYP, GS__DTP  ) ) THEN
!
! -------- Solution type "S-band group delay without ionpsphere correction"
!
           TAU_O = TAUGR_OBS_S
           TAU_E = DSQRT ( TAUGR_ERR_S**2 + ADDERR_GR_TAU**2 )
           RATE_O  = RATE_OBS_S
           RATE_C  = RATE_CALC + COR_RATE
           RATE_OC = RATE_O - RATE_C
         ELSE IF ( DATYP_INQ ( IDATYP, PX__DTP  ) ) THEN
!
! -------- Solution type "X-band phase delay without ionpsphere correction"
!
           TAU_O = TAUPH_OBS_X
           TAU_E = DSQRT ( TAUPH_ERR_X**2 + ADDERR_PH_TAU**2 )
         ELSE IF ( DATYP_INQ ( IDATYP, PS__DTP  ) ) THEN
!
! -------- Solution type "S-band phase delay without ionpsphere correction"
!
           TAU_O = TAUPH_OBS_S
           TAU_E = DSQRT ( TAUPH_ERR_S**2 + ADDERR_PH_TAU**2 )
         ELSE IF ( DATYP_INQ ( IDATYP, SNG_X__DTP  ) ) THEN
           TAU_O = TAUSB_OBS_X
           TAU_E = DSQRT ( TAUSB_ERR_X**2 + ADDERR_GR_TAU**2 )
           RATE_O  = RATE_OBS_X
           RATE_C  = RATE_CALC + COR_RATE
           RATE_OC = RATE_O - RATE_C
         ELSE IF ( DATYP_INQ ( IDATYP, SNG_S__DTP  ) ) THEN
           TAU_O = TAUSB_OBS_S
           TAU_E = DSQRT ( TAUSB_ERR_S**2 + ADDERR_GR_TAU**2 )
           RATE_O  = RATE_OBS_S
           RATE_C  = RATE_CALC + COR_RATE
           RATE_OC = RATE_O - RATE_C
         ELSE IF ( DATYP_INQ ( IDATYP, FUSED__DTP  ) ) THEN
           IF ( BTEST ( DTEC_FLG, DTHL__STS ) ) THEN
!
! ------------- Proceed as if solution type: "G-Gxs combination" -- ionosphere free linear
! ------------- compination of group delays at X- and S- bands.
!
                TAU_O   = ( TAUGR_OBS_X*FREQ_GR_X**2 - TAUGR_OBS_S*FREQ_GR_S**2 )/ &
     &                    (             FREQ_GR_X**2 -             FREQ_GR_S**2 )
                IF ( FREQ_GR_X .GT. FREQ_LIM  .AND. &
     &               FREQ_GR_S .GT. FREQ_LIM         ) THEN
                     TAU_E   = DSQRT ( &
     &             ( TAUGR_ERR_X**2*FREQ_GR_X**4 + TAUGR_ERR_S**2*FREQ_GR_S**4 )/ &
     &             ( FREQ_GR_X**2 - FREQ_GR_S**2 )**2 + &
     &               ADDERR_GR_TAU**2 )
                     IF ( DABS(TAUGR_ERR_S) > MAX__DELAY ) THEN
                          TAU_E   = DSQRT ( TAUGR_ERR_X**2 + ADDERR_GR_TAU**2 )
                     END IF
                   ELSE
                     TAU_E = TAU_ERR__BAD
                END IF
              ELSE IF ( BTEST ( DTEC_FLG, DTH__STS ) ) THEN
                TAU_O = TAUGR_OBS_X
                TAU_E = DSQRT ( TAUGR_ERR_X**2 + ADDERR_GR_TAU**2 )
                RATE_O  = RATE_OBS_X
                RATE_C  = RATE_CALC + COR_RATE
                RATE_OC = RATE_O - RATE_C
                RATE_E  = DSQRT ( RATE_ERR_X**2  + ADDERR_RATE**2   )
              ELSE IF ( BTEST ( DTEC_FLG, DTL__STS ) ) THEN
                TAU_O = TAUGR_OBS_S
                TAU_E = DSQRT ( TAUGR_ERR_S**2 + ADDERR_GR_TAU**2   )
                RATE_O  = RATE_OBS_S
                RATE_C  = RATE_CALC + COR_RATE
                RATE_OC = RATE_O - RATE_C
                RATE_E  = DSQRT ( RATE_ERR_X**2  + ADDERR_RATE**2   )
              ELSE
!
! ------------- No single-band delay preference. Use the low band
!
                TAU_O = TAUGR_OBS_S
                TAU_E = DSQRT ( TAUGR_ERR_S**2 + ADDERR_GR_TAU**2   )
                RATE_O  = RATE_OBS_S
                RATE_C  = RATE_CALC + COR_RATE
                RATE_OC = RATE_O - RATE_C
                RATE_E  = DSQRT ( RATE_ERR_X**2  + ADDERR_RATE**2   )
           END IF
      END IF
!
! -------- Check: is COR_TAU valid number
!
      IF ( IS_R8_NAN ( COR_TAU ) ) THEN
!
! -------- Not a Number. Set bogus values of COR_TAU and "bad" value for error
!
           COR_TAU = 0.0D0
           TAU_E   = TAU_ERR__BAD
      END IF
!
      TAU_C  = TAU_CALC + COR_TAU
      TAU_OC = TAU_O    - TAU_C
      IF ( IS_R8_NAN(TAU_E) ) TAU_E = 1.D-6
      IF ( TAU_E .LT. TAU_ERR__TINY ) TAU_E = TAU_ERR__TINY
!
      RETURN
      END  !#!  MAKE_OC  #!#
