      SUBROUTINE POST_VTD ( IND_OBS, VTD, TRP_USE, TRP, STS_TRP, &
     &                      DELAY, RATE, DER_DEL, DER_RAT, &
     &                      FL_NO_IONO_DEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine POST_VTD 
! *                                                                      *
! *  ### 19-APR-2006    POST_VTD   v2.3 (c)  L. Petrov  15-JUN-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'vtd.i'
      INCLUDE   'socom.i'
      INCLUDE   'oborg.i'
      INCLUDE   'prfil.i'
      INTEGER*4  IND_OBS, TRP_USE, STS_TRP, IUER 
      TYPE      ( VTD__TYPE ) :: VTD
      TYPE      ( TRP__TYPE ) :: TRP
      REAL*8     DELAY, TAU_PH, RATE
      REAL*8     DER_DEL(VTD__NDER), DER_RAT(VTD__NDER)
      LOGICAL*2  FL_NO_IONO_DEL
      CHARACTER  STA1*8, STA2*8, STR*32
      REAL*8       EPSILON_0 
      PARAMETER  ( EPSILON_0 = 0.4090928041D0 )  ! rad
      INTEGER*4  ISTA(2), IND_TRP(2), IND_SCA, J1, LQX, LQS, IER
      REAL*8     NMF_H1, NMF_H2, ZEN_H1, ZEN_H2, FJDOBS, LJDOBS
      INTEGER*4, EXTERNAL :: I_LEN, LTM_DIF, VTD_STA_INDEX
      REAL*8,    EXTERNAL :: ZENDEL_SAA, NMF_H 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      STA1 = ISITN_CHR(ISITE(1)) 
      STA2 = ISITN_CHR(ISITE(2)) 
      CALL VTD_NAME_REPAIR ( STA1 )
      CALL VTD_NAME_REPAIR ( STA2 )
      ISTA(1) = VTD_STA_INDEX ( VTD, STA1 )
      ISTA(2) = VTD_STA_INDEX ( VTD, STA2 )
      IF ( ISTA(1) .LE. 0 ) THEN
           CALL ERR_LOG ( 8671, IUER, 'POST_VTD', 'Trap of internal '// &
     &         'control: cannot station find index for '//ISITN_CHR(ISITE(1)) )
           RETURN 
      END IF
!
      IF ( ISTA(2) .LE. 0 ) THEN
           CALL ERR_LOG ( 8672, IUER, 'POST_VTD', 'Trap of internal '// &
     &         'control: cannot station find index for '//ISITN_CHR(ISITE(2)) )
           RETURN 
      END IF
!
      IF ( STS_TRP == LOAD__TRP                            .AND. &
     &   ( TRP_USE == REQ__TRP  .OR.  TRP_USE == USE__TRP )      ) THEN
           IND_SCA = LTM_DIF ( 1, TRP%N_SCA, TRP%C_SCA, SCAN_NAME )
           IF ( IND_SCA .LE. 0 ) THEN
                CALL ERR_LOG ( 8673, IUER, 'POST_VTD', 'Cannot find '// &
     &              'scan name '//SCAN_NAME//' in the external slanted '// &
     &              'neutral atmosphere path delay '//TRP%FILE_NAME )
                RETURN 
           END IF
           IND_TRP(1) = 0
           IND_TRP(2) = 0
           DO 410 J1=1,TRP%SCA(IND_SCA)%L_STA
              IF ( TRP%STA(TRP%SCA(IND_SCA)%IND_STA(J1))%NAME == ISITN_CHR(ISITE(1)) ) THEN
                   IND_TRP(1) = J1
              END IF
              IF ( TRP%STA(TRP%SCA(IND_SCA)%IND_STA(J1))%NAME == ISITN_CHR(ISITE(2)) ) THEN
                   IND_TRP(2) = J1
              END IF
 410       CONTINUE
           IF ( IND_TRP(1) == 0 ) THEN
                CALL CLRCH ( STR )
                STR = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI, -2 )
                CALL ERR_LOG ( 8674, IUER, 'POST_VTD', 'Cannot find '// &
     &              'a value of the external atmospheric path delay '// &
     &              'in file '//TRP%FILE_NAME(1:I_LEN(TRP%FILE_NAME))// &
     &              ' for observation at '//STR(1:23)//' TAI at station '// &
     &              ISITN_CHR(ISITE(1))//' for experiment '//DBNAME_CH )
                RETURN 
           END IF
           IF ( IND_TRP(2) == 0 ) THEN
                CALL CLRCH ( STR )
                STR = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI, -2 )
                CALL ERR_LOG ( 8675, IUER, 'POST_VTD', 'Cannot find '// &
     &              'a value of the external slanted neutral atmosphere '// &
     &              'path delay in file '// &
     &               TRP%FILE_NAME(1:I_LEN(TRP%FILE_NAME))// &
     &              ' for observation at '//STR(1:21)//' TAI at station '// &
     &              ISITN_CHR(ISITE(2))//' for experiment '//DBNAME_CH )
                RETURN 
           END IF
!           
           DELAY =   DELAY &
     &             + TRP%SCA(IND_SCA)%DAT(IND_TRP(2))%DEL_TOT_SLANT &
     &             - TRP%SCA(IND_SCA)%DAT(IND_TRP(1))%DEL_TOT_SLANT 
!           
           DER_DEL(VTD__DER_AT1) = -TRP%SCA(IND_SCA)%DAT(IND_TRP(1))%DER_ZEN
           DER_DEL(VTD__DER_AT2) =  TRP%SCA(IND_SCA)%DAT(IND_TRP(2))%DER_ZEN
      END IF
!
      IF ( FL_NO_IONO_DEL ) THEN
           DELAY = DELAY - ( DER_DEL(VTD__IONO2) - DER_DEL(VTD__IONO1) )
           RATE  = RATE  - ( DER_RAT(VTD__IONO2) - DER_RAT(VTD__IONO1) )
      END IF
!
!!      IF ( .NOT. SIMULATION_TEST ) THEN
           DT = DELAY*1.D6
           RT = RATE
!!         ELSE
!!           DT = 0.0D0
!!           RT = 0.0D0
!!      END IF
!
! --- Delay derivative
!
      BP(1,1,1) =  DER_DEL(VTD__DER_ST1X)
      BP(2,1,1) =  DER_DEL(VTD__DER_ST1Y)
      BP(3,1,1) =  DER_DEL(VTD__DER_ST1Z)
      BP(1,2,1) =  DER_DEL(VTD__DER_ST2X)
      BP(2,2,1) =  DER_DEL(VTD__DER_ST2Y)
      BP(3,2,1) =  DER_DEL(VTD__DER_ST2Z)
!
      SP(1,1)   =  DER_DEL(VTD__DER_RA)
      SP(2,1)   =  DER_DEL(VTD__DER_DL)
!
      ROTP(1,1) =  DER_DEL(VTD__DER_E2)
      ROTP(2,1) =  DER_DEL(VTD__DER_E1)
      ROTP(3,1) = -DER_DEL(VTD__DER_E3)*UT1__TO__E3
!
      NUTP(1,1) = -( DER_DEL(VTD__DER_E1)*DSIN(VTD%MOM%S_ANG) + &
     &               DER_DEL(VTD__DER_E2)*DCOS(VTD%MOM%S_ANG)   )* &
     &               DSIN(EPSILON_0)
      NUTP(2,1) =   DER_DEL(VTD__DER_E1)*DCOS(VTD%MOM%S_ANG) - &
     &              DER_DEL(VTD__DER_E2)*DSIN(VTD%MOM%S_ANG)
!
      AP(1,1)   =  DER_DEL(VTD__DER_AT1)
      AP(2,1)   =  DER_DEL(VTD__DER_AT2)
      AGRAD_PART(1,1,1) = -DER_DEL(VTD__DER_ATN1)
      AGRAD_PART(2,1,1) =  DER_DEL(VTD__DER_ATN2)
      AGRAD_PART(1,2,1) = -DER_DEL(VTD__DER_ATE1)
      AGRAD_PART(2,2,1) =  DER_DEL(VTD__DER_ATE2)
      AXOFP(1,1) = DER_DEL(VTD__DER_AXF1)
      AXOFP(2,1) = DER_DEL(VTD__DER_AXF2)
      RELP(1)    = DER_DEL(VTD__DER_GAMMA)
!
! --- Delay rate derivative
!
      BP(1,1,2) =  DER_RAT(VTD__DER_ST1X)
      BP(2,1,2) =  DER_RAT(VTD__DER_ST1Y)
      BP(3,1,2) =  DER_RAT(VTD__DER_ST1Z)
      BP(1,2,2) =  DER_RAT(VTD__DER_ST2X)
      BP(2,2,2) =  DER_RAT(VTD__DER_ST2Y)
      BP(3,2,2) =  DER_RAT(VTD__DER_ST2Z)
!
      SP(1,2)   =  DER_RAT(VTD__DER_RA)
      SP(2,2)   =  DER_RAT(VTD__DER_DL)
!
      ROTP(1,2) =  DER_RAT(VTD__DER_E2)
      ROTP(2,2) =  DER_RAT(VTD__DER_E1)
      ROTP(3,2) = -DER_RAT(VTD__DER_E3)*UT1__TO__E3
!
      NUTP(1,2) = -( DER_RAT(VTD__DER_E1)*DSIN(VTD%MOM%S_ANG) + &
     &               DER_RAT(VTD__DER_E2)*DCOS(VTD%MOM%S_ANG)   )* &
     &               DSIN(EPSILON_0)
      NUTP(2,2) =   DER_RAT(VTD__DER_E1)*DCOS(VTD%MOM%S_ANG) - &
     &              DER_RAT(VTD__DER_E2)*DSIN(VTD%MOM%S_ANG)
!
      AP(1,2)   =  DER_RAT(VTD__DER_AT1)
      AP(2,2)   =  DER_RAT(VTD__DER_AT2)
      AGRAD_PART(1,1,2) = -DER_RAT(VTD__DER_ATN1)
      AGRAD_PART(2,1,2) =  DER_RAT(VTD__DER_ATN2)
      AGRAD_PART(1,2,2) = -DER_RAT(VTD__DER_ATE1)
      AGRAD_PART(2,2,2) =  DER_RAT(VTD__DER_ATE2)
      AXOFP(1,2) = DER_RAT(VTD__DER_AXF1)
      AXOFP(2,2) = DER_RAT(VTD__DER_AXF2)
!
      AZ(1)   = VTD%STA(ISTA(1))%AZ
      AZ(2)   = VTD%STA(ISTA(2))%AZ
      ELEV(1) = VTD%STA(ISTA(1))%ELEV
      ELEV(2) = VTD%STA(ISTA(2))%ELEV
!
      UT1_M_TAI = VTD%MOM%UT1_M_TAI
      X_POLE    = VTD%MOM%XPL/MAS__TO__RAD
      Y_POLE    = VTD%MOM%YPL/MAS__TO__RAD
      UT1_RATE  = VTD%MOM%UT1_RATE
      XP_RATE   = VTD%MOM%XPL_RATE
      YP_RATE   = VTD%MOM%YPL_RATE
!
      UV_COOR(1) = VTD%UV_COOR(1)
      UV_COOR(2) = VTD%UV_COOR(2)
!
      STRUC_DEL = DER_DEL(VTD__STRUC)
!
      TROP_WZD(1)   = DER_DEL(VTD__TRP_WZD1)
      TROP_WZD(2)   = DER_DEL(VTD__TRP_WZD2)
      ATM_ZENDEL(1) = DER_DEL(VTD__TRP_HZD1) + DER_DEL(VTD__TRP_WZD1)
      ATM_ZENDEL(2) = DER_DEL(VTD__TRP_HZD2) + DER_DEL(VTD__TRP_WZD2)
!
!!    write ( 6, * ) 'POST-VTD-199  NUTP= ', NUTP(1:2,1) ; call flush ( 6 ) ! %%%%%%%
!!    write ( 6, * ) 'POST-VTD-200  ind_obs= ', int2(ind_obs), ' sta= ', sta1, ' ', sta2, ' sou= ', istrn_chr(istar), ' elev= ', sngl(elev) ! %%
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  POST_VTD !#!#
