      SUBROUTINE ZPD_TO_ASCII ( SPD, HEB_ZPD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine ZPD_TO_ASCII
! *                                                                      *
! * ### 10-OCT-2021   ZPD_TO_ASCII   v1.0 (c) L. Petrov  10-OCT-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INCLUDE   'heb.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      TYPE     ( HEB__TYPE    ) :: HEB_ZPD
      INTEGER*4  IUER
      INTEGER*4  J1, J2, I_LAT, I_LON, I_LATN, I_LONN
      REAL*8     D_LON, D_LAT, LAT_STEP, LON_STEP
!
      SPD%CONF%N_EL  = 1
      SPD%CONF%N_AZ  = 1
      SPD%CONF%N_FRQ = 0
      SPD%MJD = HEB_ZPD%MJD
      SPD%TAI = HEB_ZPD%UTC
      SPD%UTC = HEB_ZPD%UTC
!
      ALLOCATE ( SPD%AZM%AZIM(SPD%CONF%N_AZ), SPD%ELV%ELEV(SPD%CONF%N_EL) )
      SPD%AZM%AZIM(SPD%CONF%N_AZ) = 0.0D0
      SPD%ELV%ELEV(SPD%CONF%N_EL) = P2I
      LAT_STEP = PI__NUM/(HEB_ZPD%DIMS(2)-1) 
      LON_STEP = PI2/HEB_ZPD%DIMS(1)
      DO 410 J1=1,SPD%NSTA
         I_LAT = INT( (SPD%STA(J1)%LAT_GDT + P2I)/LAT_STEP ) + 1
         I_LON = INT(  SPD%STA(J1)%LON/LON_STEP ) + 1
         D_LAT = SPD%STA(J1)%LAT_GDT + P2I - (I_LAT-1)*LAT_STEP 
         D_LON = SPD%STA(J1)%LON           - (I_LON-1)*LON_STEP
!
! ------ I_LON,I_LAT   is the south-wet corener of the cell
! ------ I_LONN,I_LATN is the north-east corener of the cell
!
         IF ( I_LON > HEB_ZPD%DIMS(1) ) I_LON = 1
         I_LATN = I_LAT + 1
         IF ( I_LAT> HEB_ZPD%DIMS(2) ) I_LATN = I_LAT - 1
         I_LONN = I_LON + 1
         IF ( I_LONN > HEB_ZPD%DIMS(1) ) I_LONN = 1
!
! ------ Bi-linear interpolation
!
         SPD%STA(J1)%DEL(1,1,SPD__TOT) = &
     &           ( ( HEB_ZPD%VAL(I_LON,I_LAT,1,1)*(1.D0 - D_LAT/LAT_STEP) + &
     &               HEB_ZPD%VAL(I_LON,I_LATN,1,1)*D_LAT/LAT_STEP)*(1.D0 - D_LON/LON_STEP) + &
     &             ( HEB_ZPD%VAL(I_LONN,I_LAT,1,1)*(1.D0 - D_LAT/LAT_STEP) + &
     &               HEB_ZPD%VAL(I_LONN,I_LATN,1,1)*D_LAT/LAT_STEP)*D_LON/LON_STEP ) &
     &           /SPD__C
         SPD%STA(J1)%DEL(1,1,SPD__WAT) = &
     &           ( ( HEB_ZPD%VAL(I_LON,I_LAT,2,1)*(1.D0 - D_LAT/LAT_STEP) + &
     &               HEB_ZPD%VAL(I_LON,I_LATN,2,1)*D_LAT/LAT_STEP)*(1.D0 - D_LON/LON_STEP) + &
     &             ( HEB_ZPD%VAL(I_LONN,I_LAT,2,1)*(1.D0 - D_LAT/LAT_STEP) + &
     &               HEB_ZPD%VAL(I_LONN,I_LATN,2,1)*D_LAT/LAT_STEP)*D_LON/LON_STEP ) &
     &           /SPD__C
!
         SPD%STA(J1)%SUR_PRS = -1.0D0
         SPD%STA(J1)%SUR_PWP = -1.0D0
         SPD%STA(J1)%SUR_TEM = -1.0D0
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ZPD_TO_ASCII  !#!#
