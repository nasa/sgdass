      SUBROUTINE REPA_COM ( REP )
! ************************************************************************
! *                                                                      *
! *   Routine REP_COM modifies oborg and residual files in accordance    *
! *   with the command stack. At the end it empties the stack by setting *
! *   REP%N_COM = 0                                                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *                                                                      *
! *  ### 08-DEC-2004    REPA_COM   v1.4 (c)  L. Petrov  06-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_QUIT
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'oborg.i'
      INCLUDE   'resfl.i'
      INCLUDE   'diagi.i'
      INCLUDE   'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  J1, IND_OBS
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: DATYP_INQ, SUPR_INQ, META_SUPR_INQ 
      REAL*8,    EXTERNAL :: REPA_GET_AMBSP
!
      IF ( REP%N_COM .EQ. 0 ) RETURN
!
      CALL ACS_OBSFIL   ( 'O' )
      CALL ACS_RESFIL   ( 'O' )
!
! --- Cycle over command in the REPA command stack
!
      DO 410 J1=1,REP%N_COM
         IF ( REP%COM(J1)%IND_SBC == REP__COM_SNGTGL ) THEN
!
! ----------- Command: toggle suppression status
!
              IND_OBS = REP%COM(J1)%IND_OBS
!
! ----------- Read the IND_OBS -th record of oborg into the buffer
!
              CALL USE_OBSFIL ( IOBSFIL, IND_OBS , 'R' )
              IF ( REP%COM(J1)%IVAL == 1 ) THEN
!
! ---------------- Suppress
!
                   CALL SUPR_OBS ( IDATYP, REP%OBS(IND_OBS)%SUPSTAT, &
     &                                     REP%OBS(IND_OBS)%UACSUP   )
                   IF ( SUPMET == SUPMET__META ) THEN
                        USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
                   END IF
                 ELSE IF ( REP%COM(J1)%IVAL == 2 ) THEN
!
! ---------------- Resurrect
!
                   CALL RECV_OBS ( IDATYP, REP%OBS(IND_OBS)%SUPSTAT, &
     &                                     REP%OBS(IND_OBS)%UACSUP   )
                   IF ( SUPMET == SUPMET__META ) THEN
                        IF ( META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, INT2(RECO__SPS) ) ) THEN
                             USER_SUP = IBCLR ( USER_SUP, INT4(IDATYP) )
                        END IF
                   END IF
!                   write ( 6, 210 ) 'After ', ind_obs, auto_sup, user_sup, user_rec, supmet, idatyp, meta_supr_inq ( auto_sup, user_sup, user_rec, int2(reco__sps) ), kbit ( auto_sup, unrc__sps ), kbit ( user_sup, unrc__sps ) ! %%%%%
! 210               format ( A, ' ind_obs= ', i6, ' aupt_sup =', b32, ' user_sup = ', b32, ' user_rec = ', b32, ' supmet= ', i6, ' datyp= ', i6, ' fl= ', l1, 1x, l1, 1x, l1 )  ! %%%%%%%%%%%%%%%
              END IF
!
! ----------- Resetting IUNW, IUNWP flags for backward compatibility
! ----------- with PRE-APR98 versions of SOLVE
!
              IF ( SUPMET .NE. SUPMET__META ) THEN
                   CALL SUPSTAT_UNW ( REP%OBS(IND_OBS)%SUPSTAT, &
     &                                REP%OBS(IND_OBS)%UACSUP, IUNW, IUNWP )
              END IF
              SUPSTAT(1) = REP%OBS(IND_OBS)%SUPSTAT(1)
              SUPSTAT(2) = REP%OBS(IND_OBS)%SUPSTAT(2)
              UACSUP     = REP%OBS(IND_OBS)%UACSUP
!
! ----------- Write down the observation
!
              CALL USE_OBSFIL ( IOBSFIL, REP%COM(J1)%IND_OBS , 'W' )
           ELSE IF ( REP%COM(J1)%IND_SBC == REP__COM_SNGAMB ) THEN
!
! ----------- Command: shift ambiguity for a single point
!
              IND_OBS = REP%COM(J1)%IND_OBS
!
! ----------- Read the IND_OBS -th record of oborg into the buffer
!
              CALL USE_OBSFIL ( IOBSFIL, IND_OBS , 'R' )
!
! ----------- Read the IND_OBS -th record of resud into the buffer
!
              CALL USE_RESFIL ( IND_OBS , 'R' )
!
! ----------- Ambiguity was changed.
! ----------- Remind once more:
! -----------        FAMB is in seconds,
! -----------        DOBS, DPH, GION(1), PHION are in microseconds
! -----------        RDOC is in nanoseconds
!
! ----------- 1) Update of ambiguity counter
!
              IF ( DATYP_INQ ( REP%DATYP_I2, GROUP__DTP ) ) THEN
!
! ---------------- Group delay
!
                   IF ( DATYP_INQ ( REP%DATYP_I2, FUSED__DTP ) ) THEN
                        NUMAMB = 0
                        REP%OBS(IND_OBS)%NAMB_GR_X = 0
                        REP%OBS(IND_OBS)%NAMB_GR_S = 0
                     ELSE IF ( DATYP_INQ ( REP%DATYP_I2, XBAND__DTP ) .OR. &
     &                  DATYP_INQ ( REP%DATYP_I2, COMB__DTP  )      ) THEN
                        NUMAMB = NUMAMB + REP%COM(J1)%IVAL
                        REP%OBS(IND_OBS)%NAMB_GR_X = NUMAMB
                        REP%OBS(IND_OBS)%TAU_GR_X  = REP%OBS(IND_OBS)%TAU_GR_X + &
     &                          REP%COM(J1)%IVAL*REP%OBS(IND_OBS)%SPAMB_GR_X
                      ELSE IF ( DATYP_INQ ( REP%DATYP_I2, SBAND__DTP ) ) THEN
                        NUMAMB_S = NUMAMB_S + REP%COM(J1)%IVAL
                        REP%OBS(IND_OBS)%NAMB_GR_S = NUMAMB_S
                        REP%OBS(IND_OBS)%TAU_GR_S  = REP%OBS(IND_OBS)%TAU_GR_S + &
     &                          REP%COM(J1)%IVAL*REP%OBS(IND_OBS)%SPAMB_GR_S
                   END IF
                 ELSE IF ( DATYP_INQ ( REP%DATYP_I2, PHASE__DTP ) ) THEN
!
! ---------------- Phase delay
!
                   IF ( DATYP_INQ ( REP%DATYP_I2, XBAND__DTP ) .OR. &
     &                  DATYP_INQ ( REP%DATYP_I2, COMB__DTP  )      ) THEN
                        NPHAM4 = NPHAM4 + REP%COM(J1)%IVAL
                        REP%OBS(IND_OBS)%NAMB_PH_X = NPHAM4
                        REP%OBS(IND_OBS)%TAU_PH_X = REP%OBS(IND_OBS)%TAU_PH_X + &
     &                          REP%COM(J1)%IVAL*REP%OBS(IND_OBS)%SPAMB_PH_X
                      ELSE IF ( DATYP_INQ ( REP%DATYP_I2, SBAND__DTP ) ) THEN
                        NPHAM4_S = NPHAM4_S + REP%COM(J1)%IVAL
                        REP%OBS(IND_OBS)%NAMB_PH_S = NPHAM4_S
                        REP%OBS(IND_OBS)%TAU_PH_S = REP%OBS(IND_OBS)%TAU_PH_S + &
     &                          REP%COM(J1)%IVAL*REP%OBS(IND_OBS)%SPAMB_PH_S
                   END IF
              END IF
!
! ----------- 2) Update of observable
!
              IF ( DATYP_INQ ( REP%DATYP_I2, GROUP__DTP ) ) THEN
!
! ---------------- Group delay
!
                   IF ( DATYP_INQ ( REP%DATYP_I2, XBAND__DTP ) .OR. &
     &                  DATYP_INQ ( REP%DATYP_I2, COMB__DTP  )      ) THEN
                        DOBS = DOBS + REP%COM(J1)%IVAL*FAMB*1.D6
                      ELSE IF ( DATYP_INQ ( REP%DATYP_I2, SBAND__DTP ) ) THEN
                        DOBS_S = DOBS_S + REP%COM(J1)%IVAL*FAMB_S*1.D6
                   END IF
                 ELSE IF ( DATYP_INQ ( REP%DATYP_I2, PHASE__DTP ) ) THEN
!
! ---------------- Phase delay
!
                   IF ( DATYP_INQ ( REP%DATYP_I2, XBAND__DTP ) .OR. &
     &                  DATYP_INQ ( REP%DATYP_I2, COMB__DTP  )      ) THEN
                        DPH  = DPH  + REP%COM(J1)%IVAL*PHAMI8
                      ELSE IF ( DATYP_INQ ( REP%DATYP_I2, SBAND__DTP ) ) THEN
                        DPHXS = DPHXS + REP%COM(J1)%IVAL*PHAMI8_S
                   END IF
              END IF
!
! ----------- Update residuals
!
              REP%RES(IND_OBS)%RES_DEL = REP%RES(IND_OBS)%RES_DEL + &
     &                         REP%COM(J1)%IVAL*REPA_GET_AMBSP( REP, IND_OBS )
              RDOC = RDOC + REP%COM(J1)%IVAL*REPA_GET_AMBSP( REP, IND_OBS )*1.D9
!
! ----------- 3) Update of ionosphere calibration
!
              IF ( DATYP_INQ ( REP%DATYP_I2, GROUP__DTP ) ) THEN
                   IF ( KBIT ( OPP_STATUS, OPP_SET1__BIT ) .AND. &
     &                  KBIT ( OPP_STATUS, OPP_SET2__BIT )       ) THEN
!
! --------------------- We have information about both bands. 
! --------------------- Let's compute GION(1) anew.
! --------------------- NB: sign!!
!
                        GION(1) = - (DOBS - DOBS_S)* EFFREQ_XS**2/ &
     &                                             ( EFFREQ**2 - EFFREQ_XS**2 )
                      ELSE
!
! --------------------- We do not have information about both bands. 
! --------------------- Let's update GION(1)
! --------------------- NB: sign!!
!
                        IF ( DATYP_INQ ( REP%DATYP_I2, XBAND__DTP ) .OR. &
     &                       DATYP_INQ ( REP%DATYP_I2, COMB__DTP  )      ) THEN
                             GION(1) = GION(1) - REP%COM(J1)%IVAL*FAMB*1.D6* &
     &                                 EFFREQ_XS**2/ ( EFFREQ**2 - EFFREQ_XS**2 )
                           ELSE 
                             GION(1) = GION(1) - REP%COM(J1)%IVAL*FAMB_S*1.D6* &
     &                                 EFFREQ_XS**2/ ( EFFREQ**2 - EFFREQ_XS**2 )
                        END IF
                   END IF
!
! ---------------- Recalculate formal error. We do it in order to prevent
! ---------------- suppression of this observation only because it had
! ---------------- zero formal error of group delay ionosphere calibration.
!
                   GIONSG(1) = EFFREQ_XS**2 / ( EFFREQ**2 - EFFREQ_XS**2 )* &
     &                         SQRT(DERR**2 + DERRXS**2)
                   GIONSG(2) = EFFREQ_XS**2 / ( EFFREQ**2 - EFFREQ_XS**2 )* &
     &                         SQRT(RERR**2 + RERRXS**2)
!
! ---------------- Setting bit status (for the case)
!
                   CALL SBIT ( ICORR,   INT2(4),   INT2(1) )
                   CALL SBIT ( ICORR,   INT2(5),   INT2(0) )
                 ELSE IF ( DATYP_INQ ( REP%DATYP_I2, PHASE__DTP ) ) THEN
!
! ---------------- NB: sign!! It uses old conventions
!
                   IF ( KBIT ( OPP_STATUS, OPP_SET1__BIT ) .AND. &
     &                  KBIT ( OPP_STATUS, OPP_SET2__BIT )       ) THEN
!
! --------------------- NB: Sign
!
                        PHION = - (DPH_S - DPH)* PHEFFREQ_XS**2/ &
     &                              ( PHEFFREQ**2 - PHEFFREQ_XS**2 )
                      ELSE 
                        IF ( DATYP_INQ ( REP%DATYP_I2, XBAND__DTP ) .OR. &
     &                       DATYP_INQ ( REP%DATYP_I2, COMB__DTP  )      ) THEN
                             PHION = PHION + REP%COM(J1)%IVAL*PHAMI8* &
     &                           PHEFFREQ_XS**2/ ( PHEFFREQ**2 - PHEFFREQ_XS**2 )
                          ELSE IF ( DATYP_INQ ( REP%DATYP_I2, SBAND__DTP ) ) THEN
                             PHION = PHION - REP%COM(J1)%IVAL*PHAMI8_S* &
     &                           PHEFFREQ_XS**2/ ( PHEFFREQ**2 - PHEFFREQ_XS**2 )
                        END IF
                   END IF
              END IF
!
! ----------- Write down the observation
!
              CALL USE_OBSFIL ( IOBSFIL, IND_OBS, 'W' )
              CALL USE_RESFIL (          IND_OBS, 'W' )
         END IF
 410  CONTINUE
      CALL ACS_OBSFIL   ( 'C' )
      CALL ACS_RESFIL   ( 'C' )
!
! --- Empty the stack
!
      REP%N_COM = 0
      RETURN
      END  SUBROUTINE  REPA_COM
