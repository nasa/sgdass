      SUBROUTINE EQU_TO_GAL ( ALP, DEL, GAL_L, GAL_B )
! ************************************************************************
! *                                                                      *
! *   Program EQU_TO_GAL converts coordinates from equatorial J2000.0    *
! *   system ALP, DEL to IAU1958 galactic longitude and latitude GAL_L   *
! *   and GAL_B.                                                         *
! *                                                                      *
! *  ### 23-MAY-2013   EQU_TO_GAL  v1.0 (c)  L. Petrov  23-MAY-2013 ###  *
! *                                                                      *
! ************************************************************************
      REAL*8     ALP, DEL, GAL_L, GAL_B
      REAL*8     S_GAL(3), S_J2000(3), RD
      REAL*8     EQU_TO_GAL_MAT(3,3)
      INTEGER*4  IUER
!
      EQU_TO_GAL_MAT(1,1) = -0.054875539726D0
      EQU_TO_GAL_MAT(2,1) =  0.494109453312D0
      EQU_TO_GAL_MAT(3,1) = -0.867666135858D0 
      EQU_TO_GAL_MAT(1,2) = -0.873437108010D0 
      EQU_TO_GAL_MAT(2,2) = -0.444829589425D0
      EQU_TO_GAL_MAT(3,2) = -0.198076386122D0 
      EQU_TO_GAL_MAT(1,3) = -0.483834985808D0
      EQU_TO_GAL_MAT(2,3) =  0.746982251810D0
      EQU_TO_GAL_MAT(3,3) =  0.455983795705D0
!
      S_J2000(1) = DCOS(DEL)*DCOS(ALP)
      S_J2000(2) = DCOS(DEL)*DSIN(ALP)
      S_J2000(3) = DSIN(DEL)
!
      IUER = 0
      CALL MUL_MV_IV_V ( 3, 3, EQU_TO_GAL_MAT, 3, S_J2000, 3, S_GAL, IUER )
      CALL DECPOL ( 3, S_GAL, RD, GAL_L, GAL_B, IUER )
!
      RETURN
      END  SUBROUTINE  EQU_TO_GAL  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAL_TO_EQU ( GAL_L, GAL_B, ALP, DEL )
! ************************************************************************
! *                                                                      *
! *   Program GAL_TO_EQU converts coordinates from IAU1958 galactic      *
! *   longitude and latitude GAL_L and GAL_B to equatorial J2000.0       *
! *   system ALP, DEL.                                                   *
! *                                                                      *
! *  ### 23-MAY-2013   GAL_TO_EQU  v1.0 (c)  L. Petrov  23-MAY-2013 ###  *
! *                                                                      *
! ************************************************************************
      REAL*8     GAL_L, GAL_B, ALP, DEL
      REAL*8     S_GAL(3), S_J2000(3), RD
      REAL*8     EQU_TO_GAL_MAT(3,3)
      INTEGER*4  IUER
!
      EQU_TO_GAL_MAT(1,1) = -0.054875539726D0
      EQU_TO_GAL_MAT(2,1) =  0.494109453312D0
      EQU_TO_GAL_MAT(3,1) = -0.867666135858D0 
      EQU_TO_GAL_MAT(1,2) = -0.873437108010D0 
      EQU_TO_GAL_MAT(2,2) = -0.444829589425D0
      EQU_TO_GAL_MAT(3,2) = -0.198076386122D0 
      EQU_TO_GAL_MAT(1,3) = -0.483834985808D0
      EQU_TO_GAL_MAT(2,3) =  0.746982251810D0
      EQU_TO_GAL_MAT(3,3) =  0.455983795705D0
!
      S_GAL(1) = DCOS(GAL_B)*DCOS(GAL_L)
      S_GAL(2) = DCOS(GAL_B)*DSIN(GAL_L)
      S_GAL(3) = DSIN(GAL_B)
!
      IUER = 0
      CALL MUL_MV_TV_V ( 3, 3, EQU_TO_GAL_MAT, 3, S_GAL, 3, S_J2000, IUER )
      CALL DECPOL ( 3, S_J2000, RD, ALP, DEL, IUER )
!
      RETURN
      END  SUBROUTINE  GAL_TO_EQU   !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_GALCOO ( ALPHA, DELTA, GAL_B, GAL_L )
! ************************************************************************
! *                                                                      *
! *   Rouine GET_GALCOO computes galactic latitude GAL_B and galactic    *
! *   longitude CAL_L for given right ascension ALPHA and declination    *
! *   DELTA.                                                             *
! *                                                                      *
! *  ### 16-SEP-2003   GET_GALCOO  v1.0 (c)  L. Petrov  16-SEP-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     ALPHA, DELTA, GAL_B, GAL_L
      REAL*8      PI, PI2, P2I
      PARAMETER ( PI=3.141592653589793D0, PI2=2.D0*PI, P2I=PI/2D0 ) ! Pi number
      REAL*8     L0, ALPHA0, DELTA0
      PARAMETER  (     L0 =  33.00D0/180.0D0*PI )
      PARAMETER  ( ALPHA0 = 282.25D0/180.0D0*PI )
      PARAMETER  ( DELTA0 =  62.60D0/180.0D0*PI )
      REAL*8, EXTERNAL :: ATAN_CS
!
      GAL_B = DASIN ( DSIN(DELTA)*DCOS(DELTA0) - &
     &                DCOS(DELTA)*DSIN(ALPHA-ALPHA0)*DSIN(DELTA0) )
      GAL_L = L0 + ATAN_CS ( DCOS(DELTA)*DCOS(ALPHA-ALPHA0), &
     &                       DSIN(DELTA)*DSIN(DELTA0) + &
     &                       DCOS(DELTA)*DSIN(ALPHA-ALPHA0)*DCOS(DELTA0) )
      IF ( GAL_L .GE. PI2 ) GAL_L = GAL_L - PI2
!
      RETURN
      END  !#!  GET_GALCOO  #!#
