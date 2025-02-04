      SUBROUTINE APPLY_ATD ( ANTI, ISTA_BAS, ATD_USE, TEM_AVR, DEL )
! ************************************************************************
! *                                                                      *
! *   Routine  APPLY_ATD 
! *                                                                      *
! *  ### 28-APR-2008   APPLY_ATD   v1.1 (c)  L. Petrov  19-NOV-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'prfil.i'
      INCLUDE   'oborg.i'
      INCLUDE   'anti.i'
      TYPE      ( ANTENNA_DATA__TYPE ) :: ANTI
      INTEGER*4  ISTA_BAS, ATD_USE, IUER
      REAL*8     TEM_AVR, DEL
      INTEGER*4  IND_STA, IND_AHM_STA, SIGN
      REAL*8     VTD__C
      PARAMETER  ( VTD__C = 299792458.0D0 )
      REAL*8       TEM_LOW, TEM_HIGH
      PARAMETER  ( TEM_LOW  =   213.0D0 ) ! -60C
      PARAMETER  ( TEM_HIGH =   333.0D0 ) ! +60C
      REAL*8     TEM_EFF, TEM_USE, HEI_VAR, OFF_VAR, LOS_VAR, BP_UEN(3,2,2)
      REAL*8     PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC 
      INTEGER*4, EXTERNAL :: LTM_DIF
!
      DEL = 0.0D0
      IND_STA = 0
      IND_AHM_STA = 0
      IF ( ANTI%N_ANT > 0 ) THEN
           IND_STA = LTM_DIF ( 1, ANTI%N_ANT, ANTI%STA_NAM, &
     &                         ISITN_CHR(ISITE(ISTA_BAS)) )
      END IF
      IF ( ANTI%N_AHM > 0 ) THEN
           IND_AHM_STA = LTM_DIF ( 1, ANTI%N_AHM, ANTI%AHM_STA_NAM, & 
                                   ISITN_CHR(ISITE(ISTA_BAS)) )
      END IF
      IF ( IND_AHM_STA > 0 ) IND_STA = 0
      IF ( IND_STA == 0 ) THEN
!
! -------- Nothing to do
!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Compute the height variation
!
      IF ( ATD_USE == ATD__AVERAGE ) THEN
           TEM_EFF = TEM_AVR
        ELSE IF ( ATD_USE == ATD__INSTANT ) THEN
           TEM_EFF = TEMPC(ISTA_BAS) + 273.15D0
      END IF
!
      IF ( TEM_EFF < TEM_LOW  .OR.  TEM_EFF > TEM_HIGH ) THEN
!
! -------- Temperature data are unsane. Use IMA standard atmosphere instead of that.
!
           CALL REF_ELL ( 0, VSITEC(1,IND_STA), PHI_GCN, PHI_GDT, LAMBDA, &
     &                    H_ELL, RD, G_ACC )
           TEM_EFF = 288.15D0 - 0.0065D0*H_ELL
      END IF
!
      HEI_VAR = ANTI%INFO(IND_STA)%FOUNDATION_HEIGHT* &
     &          ANTI%INFO(IND_STA)%FOUNDATION_TE* &
     &          (TEM_EFF - ANTI%INFO(IND_STA)%REF_TEMP) + &
!
     &          ANTI%INFO(IND_STA)%PILLAR_HEIGHT* &
     &          ANTI%INFO(IND_STA)%PILLAR_TE* &
     &          (TEM_EFF - ANTI%INFO(IND_STA)%REF_TEMP)
!
! --- Compute the axis offset variation
!
      OFF_VAR = ANTI%INFO(IND_STA)%AXOF_LEN* &
     &          ANTI%INFO(IND_STA)%AXOF_TE*  &
     &          (TEM_EFF - ANTI%INFO(IND_STA)%REF_TEMP)
!
! --- Compute the line-of-sight variation
!
      IF ( ANTI%INFO(IND_STA)%FOCUS_TYPE == ANTI__FO_PRIM ) THEN
           LOS_VAR = ANTI%INFO(IND_STA)%VERTEX_LEN* &
     &               ANTI%INFO(IND_STA)%VERTEX_TE + &
!
     &               ANTI%INFO(IND_STA)%SUBREFL_HEIGHT*ANTI__FO_ADM* &
     &               ANTI%INFO(IND_STA)%SUBREFL_TE* &
     &              (TEM_EFF - ANTI%INFO(IND_STA)%REF_TEMP)
         ELSE IF ( ANTI%INFO(IND_STA)%FOCUS_TYPE == ANTI__FO_SECN ) THEN
           LOS_VAR = ANTI%INFO(IND_STA)%VERTEX_LEN* &
     &               ANTI%INFO(IND_STA)%VERTEX_TE + &
!
     &               ANTI%INFO(IND_STA)%SUBREFL_HEIGHT*2.0D0*ANTI__FO_ADM* &
     &               ANTI%INFO(IND_STA)%SUBREFL_TE* &
     &              (TEM_EFF - ANTI%INFO(IND_STA)%REF_TEMP)
      END IF
!
! --- Compute partial derivatives wrt Up, East, North site coordinates
!
      CALL ROTATE_PARTS_SAVE ( ISITE(ISTA_BAS), INT2(ISTA_BAS), BP_UEN )
      IF ( ISTA_BAS == 1 ) THEN
           SIGN =  1
         ELSE 
           SIGN = -1
      END IF
!
! --- Finally compute the thermal displacement contribution
!
      DEL = (                                     &
     &         SIGN*HEI_VAR*BP_UEN(1,ISTA_BAS,1)  &
     &       + SIGN*OFF_VAR*AXOFP(ISTA_BAS,1)     &
     &       + LOS_VAR/VTD__C                     &
     &      )
!      
      RETURN
      END  !#!#  SUBROUTINE APPLY_ATD  !#!#
