      FUNCTION   GET_APR_SEFD_BAS ( PIM, IND_OBS )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_APR_SEFD_BAS_FRQ
! *                                                                      *
! * ### 01-FEB-2009  GET_APR_SEFD_BAS v1.0 (c) L. Petrov 01-FEB-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE     ) :: PIM
      INTEGER*4  IND_OBS, IUER
      REAL*8     GET_APR_SEFD_BAS
      INTEGER*4  J1
      REAL*8,    EXTERNAL :: GET_APR_SEFD_BAS_FRQ
!
      GET_APR_SEFD_BAS = 1.0
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         GET_APR_SEFD_BAS = GET_APR_SEFD_BAS * &
     &                      GET_APR_SEFD_BAS_FRQ ( PIM, IND_OBS, J1 )
 410  CONTINUE
      RETURN
      END  FUNCTION   GET_APR_SEFD_BAS  !#!
!
! ------------------------------------------------------------------------
!
      FUNCTION   GET_APR_SEFD_BAS_FRQ ( PIM, IND_OBS, IND_FRQ )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_APR_SEFD_BAS_FRQ
! *                                                                      *
! * ## 01-FEB-2009 GET_APR_SEFD_BAS_FRQ v5.1 (c) L. Petrov 03-JUN-2018 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE     ) :: PIM
      INTEGER*4  IND_OBS, IND_FRQ, IUER
      REAL*8     GET_APR_SEFD_BAS_FRQ
      REAL*8     TSYS_VAL(2,2), AGAIN_VAL(2,2), SEFD_VAL(2,2), POLY_VAL
      REAL*8     AGAIN_MIN, AGAIN_MAX, TSYS_MIN, TSYS_MAX, ANG_DEG(2)
      PARAMETER  ( AGAIN_MIN = 1.D-5 )
      PARAMETER  ( AGAIN_MAX = 1.D5  )
      PARAMETER  ( TSYS_MIN  = 2.0   )
      PARAMETER  ( TSYS_MAX  = 1.D5  )
      INTEGER*4  J1, J2, J3, IND_STA(2), IND_POL(2), N_POL, U_FRQ, U_FRG
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
!
! --- Get polarization index
!
      IND_POL = 0
      IF ( PIM%NPOL == 1 ) THEN
           IND_POL(1) = 1
           N_POL = 1
         ELSE
           IF ( PIM%CONF%POLAR == PIMA__POLAR_RR ) THEN
                IND_POL(1) = 1
                N_POL = 1
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LL ) THEN
                IND_POL(1) = 2
                N_POL = 1
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_RL ) THEN
                IND_POL(1) = 1
                IND_POL(2) = 2
                N_POL = 2
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_LR ) THEN
                IND_POL(1) = 2
                IND_POL(2) = 1
                N_POL = 2
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_HH ) THEN
                IND_POL(1) = 1
                N_POL = 1
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_VV ) THEN
                IND_POL(1) = 2
                IND_POL(2) = 2
                N_POL = 1
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_HV ) THEN
                IND_POL(1) = 1
                IND_POL(2) = 2
                N_POL = 2
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_VH ) THEN
                IND_POL(1) = 2
                IND_POL(2) = 1
                N_POL = 2
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_I ) THEN
                IND_POL(1) = 1
                IND_POL(2) = 2
                N_POL = 2
           END IF
      END IF
!
      IND_STA(1) = PIM%OBS(IND_OBS)%STA_IND(1)
      IND_STA(2) = PIM%OBS(IND_OBS)%STA_IND(2)
!
      IF ( PIM%FRG_USE == PIMA__SINGLE ) THEN
           U_FRG = PIM%CONF%FRQ_GRP
           U_FRQ = IND_FRQ
         ELSE IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
           U_FRG = PIM%REV_FRG(IND_FRQ)
           U_FRQ = PIM%REV_FRQ(IND_FRQ)
         ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
           U_FRG = PIM%REV_FRG(IND_FRQ)
           U_FRQ = PIM%REV_FRQ(IND_FRQ)
      END IF
!
      DO 410 J1=1,N_POL
         DO 420 J2=1,2 ! over stations of a baseline
            IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MEASURED ) THEN
!
! -------------- Case of measured Tsys
!
                 IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J2))%TSYS(U_FRG)%AVAIL .AND. &
     &                PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J2))%GAIN(U_FRG)%AVAIL       ) THEN
!
! ------------------- Well, Tsys and Gain are available for both stations of a baseline
!
                      IF ( PIM%OBS(IND_OBS)%TSYS_IND(J1,U_FRG) > 0 ) THEN
                           TSYS_VAL(J2,J1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J2))%TSYS(U_FRG)%TSYS(U_FRQ,PIM%OBS(IND_OBS)%TSYS_IND(J2,U_FRG),IND_POL(J1))
                           AGAIN_VAL(J2,J1)= PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J2))%GAIN(U_FRG)%SENS(U_FRQ,IND_POL(J1))
                        ELSE
                           GET_APR_SEFD_BAS_FRQ = 0.0
                           RETURN
                      END IF
                    ELSE
                      GET_APR_SEFD_BAS_FRQ = 0.0
                      RETURN
                 END IF
               ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED .OR. &
     &                   PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED      ) THEN
!
! -------------- Case of modeled or cleaned Tsys
!
                 IF ( PIM%OBS(IND_OBS)%STMO_IND(J2) > 0                            .AND. &
     &                PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J2))%STMO(U_FRG)%TSYS_AVAIL .AND. &
     &                PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J2))%TSYS(U_FRG)%AVAIL            ) THEN
                      IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_CLEANED ) THEN
!
! ------------------------ Cleaned Tsys
!
                           TSYS_VAL(J2,J1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J2))%STMO(U_FRG)%TSYS_CLN(PIM%OBS(IND_OBS)%STMO_IND(J2),IND_FRQ,IND_POL(J1))
                         ELSE IF ( PIM%CONF%TSYS_CAL_CODE == PIMA__TSYS_MODELED ) THEN
!
! ------------------------ Modeled Tsys
!
                           TSYS_VAL(J2,J1) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J2))%STMO(U_FRG)%TSYS_MOD(PIM%OBS(IND_OBS)%STMO_IND(J2),IND_FRQ,IND_POL(J1))
                      END IF
                      AGAIN_VAL(J2,J1)= PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J2))%GAIN(U_FRG)%SENS(U_FRQ,IND_POL(J1))
                    ELSE
                      GET_APR_SEFD_BAS_FRQ = 0.0
                      RETURN
                 END IF
            END IF
!
! --------- Check, whether Tsys and Gain are in a range
!
            IF ( IS_R8_NAN ( TSYS_VAL(J2,J1)  ) ) TSYS_VAL(J2,J1)  = TSYS_MIN/10.0D0
            IF ( IS_R8_NAN ( AGAIN_VAL(J2,J1) ) ) AGAIN_VAL(J2,J1) = AGAIN_MIN/10.0D0
            IF ( TSYS_VAL(J2,J1)  < TSYS_MIN  .OR. &
     &           TSYS_VAL(J2,J1)  > TSYS_MAX  .OR. &
     &           AGAIN_VAL(J2,J1) < AGAIN_MIN .OR. &
     &           AGAIN_VAL(J2,J1) > AGAIN_MAX      ) THEN
!
                 IF ( PIM%CONF%DEBUG_LEVEL == 14 ) THEN
                      WRITE ( 6, 210 ) IND_OBS, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J2)), &
     &                                 IND_POL(J1), TSYS_VAL(J2,J1), AGAIN_VAL(J2,J1)
 210                  FORMAT ( 'Wrong Tsys/Again Ind_obs: ', I5, &
     &                         ' Sta: ', A, ' Ind_pol= ', I1, ' Tsys= ', 1PD12.5, &
     &                         ' Again= ', 1PD12.5 )
                 END IF
                 GET_APR_SEFD_BAS_FRQ = 0.0
                 RETURN
            END IF
!
            IF ( ( PIM%STA(IND_STA(J2))%GAIN(U_FRG)%TYP(J2,IND_POL(J1)) == PIMA__GA_ELEV .OR. &
     &             PIM%STA(IND_STA(J2))%GAIN(U_FRG)%TYP(J2,IND_POL(J1)) == PIMA__GA_ZEN  ) .AND. &
     &           PIM%STA(IND_STA(J2))%GAIN(U_FRG)%NTAB > 0                                       ) THEN
!
! -------------- Compute the value of the polynomial that models
! -------------- elevation depedence of the gain
!
! -------------- The caveat: indexing of array GAIN%Y_VAL starts from 1
!
                 POLY_VAL = 0.0D0
                 IF ( PIM%STA(IND_STA(J2))%GAIN(U_FRG)%TYP(J2,IND_POL(J1)) == PIMA__GA_ELEV ) THEN
!
! ------------------- Gain is a function of elevation in degrees
!
                      ANG_DEG(J2) = PIM%STA(IND_STA(J2))%TSYS(U_FRG)%ELEV_R4(PIM%OBS(IND_OBS)%TSYS_IND(J2,U_FRG))/DEG__TO__RAD
                   ELSE IF ( PIM%STA(IND_STA(J2))%GAIN(U_FRG)%TYP(J2,IND_POL(J1)) == PIMA__GA_ZEN ) THEN
!
! ------------------- Gain is a function of zenith angle in degrees
!
                      ANG_DEG(J2) = 90.0D0 - PIM%STA(IND_STA(J2))%TSYS(U_FRG)%ELEV_R4(PIM%OBS(IND_OBS)%TSYS_IND(J2,U_FRG))/DEG__TO__RAD
                 END IF 
!
                 DO 430 J3=0,PIM%STA(IND_STA(J2))%GAIN(U_FRG)%NTAB
                    IF ( IS_R4_NAN ( PIM%STA(IND_STA(J2))%GAIN(U_FRG)%Y_VAL(J3,U_FRQ,IND_POL(J1)) ) ) THEN
                         POLY_VAL = POLY_VAL + &
     &                      PIM%STA(IND_STA(J2))%GAIN(U_FRG)%GAIN(J3,U_FRQ,IND_POL(J1))* &
     &                      ANG_DEG(J2)**J3
                       ELSE
                         POLY_VAL = POLY_VAL + &
     &                     PIM%STA(IND_STA(J2))%GAIN(U_FRG)%Y_VAL(J3,U_FRQ,IND_POL(J1))* &
     &                     ANG_DEG(J2)**J3
                    END IF
 430             CONTINUE
!
                 IF ( POLY_VAL .NE. 0.0D0 ) THEN
                      AGAIN_VAL(J2,J1) = AGAIN_VAL(J2,J1)*POLY_VAL
                 END IF
            END IF
!
            IF ( AGAIN_VAL(J2,J1) < AGAIN_MIN .OR. &
     &           AGAIN_VAL(J2,J1) > AGAIN_MAX .OR. &
     &           AGAIN_VAL(J2,J1) < AGAIN_MIN .OR. &
     &           AGAIN_VAL(J2,J1) > AGAIN_MAX      ) THEN
!
                 GET_APR_SEFD_BAS_FRQ = 0.0
                 RETURN
            END IF
 420     CONTINUE 
!
         SEFD_VAL(1,J1) = TSYS_VAL(1,J1)/AGAIN_VAL(1,J1)
         SEFD_VAL(2,J1) = TSYS_VAL(2,J1)/AGAIN_VAL(2,J1)
 410  CONTINUE 
!
      IF ( N_POL == 1 ) THEN
           GET_APR_SEFD_BAS_FRQ = DSQRT ( SEFD_VAL(1,1)*SEFD_VAL(2,1) )
         ELSE IF ( N_POL == 2 ) THEN
           IF ( PIM%CONF%POLAR == PIMA__POLAR_RL .OR. &
     &          PIM%CONF%POLAR == PIMA__POLAR_LR      ) THEN
                GET_APR_SEFD_BAS_FRQ = DSQRT ( SEFD_VAL(1,1)*SEFD_VAL(2,1) * SEFD_VAL(1,2)*SEFD_VAL(2,2) )
              ELSE IF ( PIM%CONF%POLAR == PIMA__POLAR_I    ) THEN
                GET_APR_SEFD_BAS_FRQ = ( DSQRT ( SEFD_VAL(1,1)*SEFD_VAL(2,1) ) + &
     &                                   DSQRT ( SEFD_VAL(1,2)*SEFD_VAL(2,2) )   )/2.0D0
           END IF
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL == 14 ) THEN
           WRITE ( 6, 220 ) IND_OBS, U_FRQ, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                      PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), IND_POL(1), &
     &                      AGAIN_VAL, TSYS_VAL, GET_APR_SEFD_BAS_FRQ, &
     &                      PIM%OBS(IND_OBS)%TSYS_IND(1,U_FRG), &
     &                      PIM%OBS(IND_OBS)%TSYS_IND(2,U_FRG)
 220      FORMAT ( 'GET_APR_SEFD_BAS: IND_OBS: ', I6,' FRQ: ', I2, &
     &         ' Sta: ', A, ' / ', A, ' I_pol: ', I1, &
     &         ' Gain_pol1: ', F7.5, 1X, F7.5, &
     &         ' Gain_pol2: ', F7.5, 1X, F7.5, &
     &         ' Tsys_pol1: ', F7.1, 1X, F7.1, &
     &         ' Tsys_pol2: ', F7.1, 1X, F7.1, &
     &         ' SEFD: ', F7.1, ' Tsys_ind: ', I6, 1X, I6 )
      END IF
!
      RETURN
      END  FUNCTION  GET_APR_SEFD_BAS_FRQ  !#!#
