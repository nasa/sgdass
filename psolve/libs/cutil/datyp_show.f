      SUBROUTINE DATYP_SHOW ( IDATYP_I2, OUT )
! ************************************************************************
! *                                                                      *
! *   Routine  DATYP_SHOW  generates output line with short description  *
! *   of solution type. Output line left-adjusted may have up to 21      *
! *   symbols.                                                           *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   IDATYP_I2 ( INTEGER*2 ) -- Code of solution type. Supported codes  *
! *                              are defined in                          *
! *                              $PSOLVE_ROOT/include/solve.i            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *         OUT ( CHARACTER ) -- Output line.                            *
! *                                                                      *
! *  ###  03-FEB-98   DATYP_SHOW   v1.2  (c)  L. Petrov  16-DEC-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INTEGER*2  IDATYP_I2
      CHARACTER  OUT*(*)
!
      IF ( IDATYP_I2         .EQ. GRPRAT__DTP ) THEN
           OUT = GRPRAT__DTC
         ELSE IF ( IDATYP_I2 .EQ. PHSRAT__DTP ) THEN
           OUT = PHSRAT__DTC
         ELSE IF ( IDATYP_I2 .EQ. SNBRAT__DTP ) THEN
           OUT = SNBRAT__DTC
         ELSE IF ( IDATYP_I2 .EQ. GRPONL__DTP ) THEN
           OUT = GRPONL__DTC
         ELSE IF ( IDATYP_I2 .EQ. PHSONL__DTP ) THEN
           OUT = PHSONL__DTC
         ELSE IF ( IDATYP_I2 .EQ. SNBONL__DTP ) THEN
           OUT = SNBONL__DTC
         ELSE IF ( IDATYP_I2 .EQ. RATONL__DTP ) THEN
           OUT = RATONL__DTC
         ELSE IF ( IDATYP_I2 .EQ. G_GXS__DTP  ) THEN
           OUT = G_GXS__DTC
         ELSE IF ( IDATYP_I2 .EQ. PX_GXS__DTP ) THEN
           OUT = PX_GXS__DTC
         ELSE IF ( IDATYP_I2 .EQ. PS_GXS__DTP ) THEN
           OUT = PS_GXS__DTC
         ELSE IF ( IDATYP_I2 .EQ. PX_GX__DTP  ) THEN
           OUT = PX_GX__DTC
         ELSE IF ( IDATYP_I2 .EQ. PX_GS__DTP  ) THEN
           OUT = PX_GS__DTC
         ELSE IF ( IDATYP_I2 .EQ. PS_GX__DTP  ) THEN
           OUT = PS_GX__DTC
         ELSE IF ( IDATYP_I2 .EQ. PS_GS__DTP  ) THEN
           OUT = PS_GS__DTC
         ELSE IF ( IDATYP_I2 .EQ. P_PXS__DTP  ) THEN
           OUT = P_PXS__DTC
         ELSE IF ( IDATYP_I2 .EQ. GX__DTP ) THEN
           OUT = GX__DTC
         ELSE IF ( IDATYP_I2 .EQ. GS__DTP ) THEN
           OUT = GS__DTC
         ELSE IF ( IDATYP_I2 .EQ. PX__DTP ) THEN
           OUT = PX__DTC
         ELSE IF ( IDATYP_I2 .EQ. PS__DTP ) THEN
           OUT = PS__DTC
         ELSE IF ( IDATYP_I2 .EQ. SNG_X__DTP ) THEN
           OUT = SNG_X__DTC
         ELSE IF ( IDATYP_I2 .EQ. SNG_S__DTP ) THEN
           OUT = SNG_S__DTC
         ELSE IF ( IDATYP_I2 .EQ. FUSED__DTP ) THEN
           OUT = FUSED__DTC
        ELSE
           OUT = 'UNKNOWN SOLUTION TYPE'
      END IF
!
      RETURN
      END  !#!  DATYP_SHOW  #!#
