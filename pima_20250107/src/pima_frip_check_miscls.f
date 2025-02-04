      SUBROUTINE PIMA_FRIP_CHECK_MISCLS ( PIM, SCA_TYP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FRIP_CHECK_MISCLS 
! *                                                                      *
! * # 09-MAR-2012 PIMA_FRIP_CHECK_MISCLS v1.0 (c) L. Petrov 09-MAR-2012 #*
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE   ) :: PIM
      INTEGER*4  SCA_TYP, IUER
      INTEGER*4  J1, J2, J3, J4, J5, IND_STA(2,3), N_CLS(PIM__MFRQ), LFRQ
      REAL*4     PHS(3), PHS_CLS, &
     &           PHS_CLS_MAX(PIM__MFRQ), PHS_CLS_AVR(PIM__MFRQ), &
     &           PHS_CLS_RMS(PIM__MFRQ)
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
!
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
!
      PHS_CLS_MAX = 0.0
      PHS_CLS_AVR = 0.0
      PHS_CLS_RMS = 0.0
      N_CLS = 0
      DO 410 J1=1,LFRQ
         DO 420 J2=1,PIM%FRIP(SCA_TYP)%NOBS-2
            IF ( .NOT. PIM%FRIP(SCA_TYP)%USED(J2) ) GOTO 420
            IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J1,J2) < PIMA__WEI_MIN ) GOTO 420
!
            PHS(1) = PHAS_CMPL_R4 ( PIM%FRIP(SCA_TYP)%VIS_AF(J1,J2) ) &
     &               + PI2*PIM%FRIP(SCA_TYP)%IAMB_AF(J1,J2)
            IND_STA(1,1) = PIM%FRIP(SCA_TYP)%IND_STA(1,J2) 
            IND_STA(2,1) = PIM%FRIP(SCA_TYP)%IND_STA(2,J2) 
            DO 430 J3=J2+1,PIM%FRIP(SCA_TYP)%NOBS-1
               IF ( .NOT. PIM%FRIP(SCA_TYP)%USED(J3) ) GOTO 430
               IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J1,J3) < PIMA__WEI_MIN ) GOTO 430
               PHS(2) = PHAS_CMPL_R4 ( PIM%FRIP(SCA_TYP)%VIS_AF(J1,J3) ) &
     &                  + PI2*PIM%FRIP(SCA_TYP)%IAMB_AF(J1,J3)
               IND_STA(1,2) = PIM%FRIP(SCA_TYP)%IND_STA(1,J3) 
               IND_STA(2,2) = PIM%FRIP(SCA_TYP)%IND_STA(2,J3) 
               DO 440 J4=J3+1,PIM%FRIP(SCA_TYP)%NOBS
                  IF ( .NOT. PIM%FRIP(SCA_TYP)%USED(J4) ) GOTO 440
                  IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J1,J4) < PIMA__WEI_MIN ) GOTO 440
                  PHS(3) = PHAS_CMPL_R4 ( PIM%FRIP(SCA_TYP)%VIS_AF(J1,J4) ) &
     &                     + PI2*PIM%FRIP(SCA_TYP)%IAMB_AF(J1,J4)
                  IND_STA(1,3) = PIM%FRIP(SCA_TYP)%IND_STA(1,J4) 
                  IND_STA(2,3) = PIM%FRIP(SCA_TYP)%IND_STA(2,J4) 
                  PHS_CLS = 1.D9
                  IF ( IND_STA(1,2) == IND_STA(1,1) ) THEN
                       IF ( IND_STA(1,3) == IND_STA(2,1) .AND. &
     &                      IND_STA(2,3) == IND_STA(2,2)       ) THEN
!
! ------------------------- A-B  -  A-C  +  B-C
!
                            PHS_CLS = PHS(1) - PHS(2) + PHS(3)
                       END IF
                       IF ( IND_STA(1,3) == IND_STA(2,2) .AND. &
     &                      IND_STA(2,3) == IND_STA(2,1)       ) THEN
!
! ------------------------- A-B  -  A-C  -  C-B
!
                            PHS_CLS = PHS(1) - PHS(2) - PHS(3)
                       END IF
                     ELSE IF ( IND_STA(2,2) == IND_STA(1,1) ) THEN
                       IF ( IND_STA(1,3) == IND_STA(2,1) .AND. &
     &                      IND_STA(2,3) == IND_STA(2,2) ) THEN
!
! ------------------------- A-B  +  C-A  +  B-C
!
                            PHS_CLS = PHS(1) + PHS(2) + PHS(3)
                          ELSE IF ( IND_STA(1,3) == IND_STA(1,2) .AND. &
     &                              IND_STA(2,3) == IND_STA(2,1)       ) THEN
!
! ------------------------- A-B  +  C-A  -  C-B
!
                            PHS_CLS = PHS(1) + PHS(2) - PHS(3)
                       END IF
                  END IF
                  IF ( PHS_CLS < 1.D8 ) THEN
                       N_CLS(J1) = N_CLS(J1) + 1
                       PHS_CLS_MAX(J1) = MAX ( PHS_CLS_MAX(J1), ABS(PHS_CLS) )
                       PHS_CLS_AVR(J1) = PHS_CLS_AVR(J1) + PHS_CLS
                       PHS_CLS_RMS(J1) = PHS_CLS_RMS(J1) + PHS_CLS**2
                       IF ( ( PIM%CONF%WARNING  .AND.  ABS(PHS_CLS) > P2I ) .OR. &
     &                      ( ABS(PHS_CLS) > PI__NUM .AND. PIM%CONF%CHECK_SEVERITY .GE. 2 ) .OR. &
     &                      PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
!
                            WRITE ( 6, 210 ) &
     &                               PIM%FRIP(SCA_TYP)%IND_SCA, &
     &                               PIM%SOU(PIM%SCA(PIM%FRIP(SCA_TYP)%IND_SCA)%SOU_IND)%IVS_NAME,   &
     &                               PIM%SOU(PIM%SCA(PIM%FRIP(SCA_TYP)%IND_SCA)%SOU_IND)%J2000_NAME, &
     &                               PHS_CLS, &
     &                               PHS(1), PHS(2), PHS(3), &
     &                               PIM%FRIP(SCA_TYP)%IAMB_AF(J1,J2),  &
     &                               PIM%FRIP(SCA_TYP)%IAMB_AF(J1,J3),  &
     &                               PIM%FRIP(SCA_TYP)%IAMB_AF(J1,J4),  &
     &                               J1, &
     &                               PIM%FRIP(SCA_TYP)%OBS(J2)%IND_OBS, &
     &                               PIM%FRIP(SCA_TYP)%OBS(J3)%IND_OBS, &
     &                               PIM%FRIP(SCA_TYP)%OBS(J4)%IND_OBS, &
     &                               PIM%C_STA(PIM%FRIP(SCA_TYP)%IND_STA(1,J2)), &
     &                               PIM%C_STA(PIM%FRIP(SCA_TYP)%IND_STA(2,J2)), &
     &                               PIM%C_STA(PIM%FRIP(SCA_TYP)%IND_STA(1,J3)), &
     &                               PIM%C_STA(PIM%FRIP(SCA_TYP)%IND_STA(2,J3)), &
     &                               PIM%C_STA(PIM%FRIP(SCA_TYP)%IND_STA(1,J4)), &
     &                               PIM%C_STA(PIM%FRIP(SCA_TYP)%IND_STA(2,J4))
 210                        FORMAT ( 'Misclosure: Ind_sca: ', I3, &
     &                               ' Sou: ', A, 2X, A, 2X, F5.2, &
     &                               ' rad  Phases: ', 3(F5.2,1X), &
     &                               ' Iamb: ', 3(I2,1X), &
     &                               ' Ifrq: ', I2, &
     &                               ' Ind_obs: ', 3(I5,1X), &
     &                               3(' Sta: ',A, ' / ', A, ' || ') )
                       END IF
                       IF ( ABS(PHS_CLS) > PI__NUM .AND. &
     &                      PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                            CALL ERR_LOG ( 6591, IUER, 'PIMA_FRIP_CHECK_MISCLS', &
     &                          'Phase misclosure is too big. Quitting' )
                            RETURN 
                       END IF
                  END IF
 440           CONTINUE 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           DO 450 J5=1,LFRQ
              IF ( N_CLS(J5) > 1 ) THEN
                   PHS_CLS_RMS(J5) = SQRT ( MAX( 0.0, &
     &                 PHS_CLS_RMS(J5)/N_CLS(J5) - PHS_CLS_AVR(J5)**2/N_CLS(J5)**2 ) )
                   WRITE ( 6, 220 ) J5, PIM%FRIP(SCA_TYP)%IND_SCA, &
     &                     PIM%SOU(PIM%SCA(PIM%FRIP(SCA_TYP)%IND_SCA)%SOU_IND)%IVS_NAME,   &
     &                     PIM%SOU(PIM%SCA(PIM%FRIP(SCA_TYP)%IND_SCA)%SOU_IND)%J2000_NAME, &
     &                     N_CLS(J5), PHS_CLS_AVR(J5)/N_CLS(J5), PHS_CLS_RMS(J5), PHS_CLS_MAX(J5)
 220               FORMAT ( 'Misclosure statitics: Ind_frq: ', I3, &
     &                      ' Ind_sca: ', I3, &
     &                      ' Sou: ', A, 2X, A / &
     &                      ' N_cls: ', I4,' Mscl_avr: ', F6.2, &
     &                      ' Mscl_rms: ', F6.2, ' Mscl_max: ', F6.2 )
              END IF
 450       CONTINUE 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRIP_CHECK_MISCLS  !#!  
