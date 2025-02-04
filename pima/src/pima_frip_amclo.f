      SUBROUTINE PIMA_FRIP_AMCLO ( PIM, SCA_TYP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_FRIP_AMCLO
! *                                                                      *
! * ### 08-JAN-2012  PIMA_FRIP_AMCLO  v1.0 (c) L. Petrov 08-JAN-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  SCA_TYP, IUER
      REAL*4     PHS_STA(PIM__MSTA), PHS_BAS(PIM__MBAS), CLS
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, LFRQ, UIS_STA(PIM__MSTA), &
     &           IND_SCN_STA(PIM__MSTA), JMP_BAS(PIM__MBAS), IST1, IST2, &
     &           LSTA, L_TRI, IND_OBS, IND_REF, IER
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: ADD_LIS, ILEN, I_LEN, NSTBA, IFIND_PL, LTM_DIF
!
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      PIM%FRIP(SCA_TYP)%IAMB_AF = 0
!
! --- Get the station indewx list LSTA, UIS_STA
!
      DO 410 J1=1,LFRQ
         LSTA = 0
         DO 420 J2=1,PIM%FRIP(SCA_TYP)%NOBS
            IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J1,J2) < PIMA__WEI_MIN ) GOTO 420
            IF ( .NOT. PIM%FRIP(SCA_TYP)%USED(J2) ) GOTO 420
            IND_OBS = PIM%FRIP(SCA_TYP)%OBS(J2)%IND_OBS
!
            IST1 = ADD_LIS ( PIM__MSTA, LSTA, UIS_STA, &
     &                       INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4 ), IER )
            IST2 = ADD_LIS ( PIM__MSTA, LSTA, UIS_STA, &
     &                       INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4 ), IER )
  420    CONTINUE 
!
! ------ Sort the station index list 
!
         CALL SORT_I ( LSTA, UIS_STA )
!
! ------ Find the index of the reference station in the station index list
!
         IND_REF = IFIND_PL ( LSTA, UIS_STA, PIM%FRIP(SCA_TYP)%IND_STA_REF )
!
!%  write ( 6, * ) 'PFA  ind_sta_ref = ', PIM%FRIP(SCA_TYP)%IND_STA_REF, &
!%     &       ' IND_REF = ', IND_REF
!
!
! ------ Get "station" phases for baselines with the reference station
!
         PHS_STA = 0.0
         DO 430 J3=1,PIM%FRIP(SCA_TYP)%NOBS
            IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J1,J3) < PIMA__WEI_MIN ) GOTO 430
            IF ( .NOT. PIM%FRIP(SCA_TYP)%USED(J3) ) GOTO 430
            IND_OBS = PIM%FRIP(SCA_TYP)%OBS(J3)%IND_OBS
!
            IST1 = IFIND_PL ( LSTA, UIS_STA, &
     &                        INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4 ) )
            IST2 = IFIND_PL ( LSTA, UIS_STA, &
     &                        INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4 ) )
            PHS_BAS(J3) = PHAS_CMPL_R4( PIM%FRIP(SCA_TYP)%VIS_AF(J1,J3) )
            IF ( IST1 == IND_REF ) THEN
                 PHS_STA(IST2) =  PHS_BAS(J3)
            END IF
            IF ( IST2 == IND_REF ) THEN
                 PHS_STA(IST1) = -PHS_BAS(J3)
            END IF
  430    CONTINUE 
!
! ------ Correct ambiguities for observations with baselines that do not
! ------ have a reference station
!
         DO 440 J4=1,PIM%FRIP(SCA_TYP)%NOBS
            IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J1,J4) < PIMA__WEI_MIN ) GOTO 440
            IF ( .NOT. PIM%FRIP(SCA_TYP)%USED(J4) ) GOTO 440
            IND_OBS = PIM%FRIP(SCA_TYP)%OBS(J4)%IND_OBS
!
            PHS_BAS(J4) = PHAS_CMPL_R4( PIM%FRIP(SCA_TYP)%VIS_AF(J1,J4) )
!
! --------- Find IST1, IST2 -- station indices of J4-th baseline
! --------- in the list of stations
!
            IST1 = IFIND_PL ( LSTA, UIS_STA, &
     &                        INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4 ) )
            IST2 = IFIND_PL ( LSTA, UIS_STA, &
     &                        INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4 ) )
            IF ( IST1 .NE. IND_REF  .AND.  IST2 .NE. IND_REF ) THEN
                 PIM%FRIP(SCA_TYP)%IAMB_AF(J1,J4) = &
     &              -NINT( (PHS_BAS(J4) - ( PHS_STA(IST2) - PHS_STA(IST1) ) )/ &
     &                     PI2 )
            END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! IF ( J1 == 1 ) THEN
!     WRITE ( 6, 128 ) IND_OBS, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
!     &                         PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2)), &
!     &                PHS_BAS(J4), PHS_STA(IST2), PHS_STA(IST1), &
!     &                PHS_BAS(J4) - ( PHS_STA(IST2) - PHS_STA(IST1) ), &
!     &                PIM%FRIP(SCA_TYP)%IAMB_AF(J1,J4)
! 128  FORMAT ( 'PFA  Ind_obs: ', I5, ' C_sta: ', A, ' / ', A, &
!     &         ' Phs_bas: ', F5.2, ' Phs_sta: ', F5.2, 1X, F5.2, &
!     &         ' Msc: ', F5.2, ' Jmp: ', I2 )
! END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  440    CONTINUE 
  410 CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRIP_AMCLO  !#!  
