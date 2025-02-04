      SUBROUTINE REPA_UPDAMB ( IND_OBS, IAMB ) 
! ************************************************************************
! *                                                                      *
! *   Routine  REPA_UPDAMB  updates parameters in the oborg.i which      *
! *   are needed to update when the observaion with the index IND_OBS    *
! *   in the experiment have the ambiguity change IAMB.                  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  IND_OBS ( INTEGER*4 ) -- Index of the observation in the            *
! *                           experiment.                                *
! *     IAMB ( INTEGER*4 ) -- Amount of integer ambiguity change.        *
! *                                                                      *
! *  ### 10-DEC-2004  REPA_UPDAMB  v1.1 (c)  L. Petrov  14-JAN-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'oborg.i'
      INCLUDE   'resfl.i' 
      INTEGER*4  IND_OBS, IAMB
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: DATYP_INQ
!
! --- Read the IND_OBS -th record of oborg into the buffer
!
      CALL USE_OBSFIL ( IOBSFIL, IND_OBS , 'R' )
!
! --- Read the IND_OBS -th record of resud into the buffer
!
      CALL USE_RESFIL ( IND_OBS , 'R' )
!
! --- Ambiguity was changed.
! --- Remind once more:
! ---        FAMB is in seconds,
! ----       DOBS, DPH, GION(1), PHION are in microseconds
!
! ---- 1) Update of ambiguity counter
!
      IF ( DATYP_INQ ( IDATYP, GROUP__DTP ) ) THEN
!
! -------- Group delay
!
           IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) .OR. &
     &          DATYP_INQ ( IDATYP, COMB__DTP  )      ) THEN
                NUMAMB   = NUMAMB + IAMB
             ELSE IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
                NUMAMB_S = NUMAMB_S + IAMB
           END IF
        ELSE IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- Phase delay
!
           IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) .OR. &
     &          DATYP_INQ ( IDATYP, COMB__DTP  )      ) THEN
                NPHAM4 = NPHAM4 + IAMB    
             ELSE IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
                NPHAM4_S = NPHAM4_S + IAMB    
           END IF
      END IF
!
! --- 2) Update of observable
!
      IF ( DATYP_INQ ( IDATYP, GROUP__DTP ) ) THEN
!
! -------- Group delay
!
           IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) .OR. &
     &          DATYP_INQ ( IDATYP, COMB__DTP  )      ) THEN
                DOBS = DOBS + IAMB*FAMB*1.D6
                RDOC = RDOC + IAMB*FAMB*1.D9
             ELSE IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
                DOBS_S = DOBS_S + IAMB*FAMB_S*1.D6
                RDOC = RDOC + IAMB*FAMB_S*1.D9
           END IF
         ELSE IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- Phase delay
!
           IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) .OR. &
     &          DATYP_INQ ( IDATYP, COMB__DTP  )      ) THEN
                DPH  = DPH  + IAMB*PHAMI8
                RDOC = RDOC + IAMB*PHAMI8*1.D9
              ELSE IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
                DPHXS = DPHXS + IAMB*PHAMI8_S
                RDOC  = RDOC  + IAMB*PHAMI8*1.D9
           END IF
      END IF
!
! --- 3) Update of ionosphere calibration
!
      IF ( DATYP_INQ ( IDATYP, GROUP__DTP ) ) THEN
!
! -------- NB: sign!!
!
           IF ( KBIT ( OPP_STATUS, OPP_SET1__BIT ) .AND. &
     &          KBIT ( OPP_STATUS, OPP_SET2__BIT )       ) THEN
!
! ------------- Database contains information about both bands: compute 
! ------------- GION(1) anew
!
                GION(1) = (DOBS - DOBS_S)* EFFREQ_XS**2/ &
     &                                   ( EFFREQ**2 - EFFREQ_XS**2 )
             ELSE 
!
! ------------- Database does not have information abotu S-band: update GION(1)
!
                GION(1) = GION(1) - IAMB*FAMB*1.D6* EFFREQ_XS**2/ &
     &                            ( EFFREQ**2 - EFFREQ_XS**2 )
           END IF
!
! -------- Recalculate formal error. We do it in order to prevent
! -------- suppression of this observation only because it had
! -------- zero formal error of group delay ionosphere calibration.
!
           GIONSG(1) = EFFREQ_XS**2 / ( EFFREQ**2 - EFFREQ_XS**2 )* &
     &                 DSQRT(DERR**2 + DERRXS**2)
           GIONSG(2) = EFFREQ_XS**2 / ( EFFREQ**2 - EFFREQ_XS**2 )* &
     &                 DSQRT(RERR**2 + RERRXS**2)
!
! -------- Setting bit status (for the case)
!
           CALL SBIT ( ICORR, INT2(4), INT2(1) )
           CALL SBIT ( ICORR, INT2(5), INT2(0) )
         ELSE IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- NB: sign!! It uses old conventions
!
           IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) .OR. &
     &          DATYP_INQ ( IDATYP, COMB__DTP  )      ) THEN
                PHION = PHION + IAMB*PHAMI8* PHEFFREQ_XS**2/ &
     &                          ( PHEFFREQ**2 - PHEFFREQ_XS**2 )
              ELSE IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
                PHION = PHION - IAMB*PHAMI8_S* PHEFFREQ_XS**2/ &
     &                          ( PHEFFREQ**2 - PHEFFREQ_XS**2 )
            END IF
      END IF
!
! --- Write down the observation
!
      CALL USE_OBSFIL ( IOBSFIL, IND_OBS, 'W' )
      CALL USE_RESFIL (          IND_OBS, 'W' )
!
      RETURN
      END  SUBROUTINE  REPA_UPDAMB 
