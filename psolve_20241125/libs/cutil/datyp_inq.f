      FUNCTION   DATYP_INQ ( IDATYP_I2, CODE_I2 )
! ************************************************************************
! *                                                                      *
! *   Logical*4 function DATYP_INQ inquires: does solution type code     *
! *   IDATYP_I2 corresponds to inquire code CODE_I2. Two modes of        *
! *   inquiry are supporterd: exact mode, when exact correspondence      *
! *   of the solution type and inquiry code is tested and class inquire  *
! *   when solution type is tested: does it belong to the class of       *
! *   solution type, such as DELAY, RATE, GROUP, SINGL or MIXED.         *
! *   Inquiry codes are defined in  ../include/solve.i                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   IDATYP_I2 ( INTEGER*2 ) -- Code of solution type.                  *
! *     CODE_I2 ( INTEGER*2 ) -- Inquire code.                           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <DATYP_INQ> ( LOGICAL*4 ) -- Result of inquiry.                      *
! *                                                                      *
! *  ###  03-FEB-98    DATYP_INQ   v1.2  (c)  L. Petrov 16-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      LOGICAL*4  DATYP_INQ
      INTEGER*2  IDATYP_I2, CODE_I2
      CHARACTER  STR*12
!
      DATYP_INQ = .FALSE.
      IF ( IDATYP_I2 .EQ. GRPRAT__DTP ) THEN
           IF ( CODE_I2 .EQ. GRPRAT__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.   RATE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  GROUP__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.   COMB__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  IOCAL__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. PHSRAT__DTP ) THEN
           IF ( CODE_I2 .EQ. PHSRAT__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.   RATE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  PHASE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.   COMB__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  IOCAL__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. SNBRAT__DTP ) THEN
           IF ( CODE_I2 .EQ. SNBRAT__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.   RATE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  SINGL__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  IOCAL__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. GRPONL__DTP ) THEN
           IF ( CODE_I2 .EQ. GRPONL__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  GROUP__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.   COMB__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  IOCAL__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. PHSONL__DTP ) THEN
           IF ( CODE_I2 .EQ. PHSONL__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  PHASE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.   COMB__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  IOCAL__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. SNBONL__DTP ) THEN
           IF ( CODE_I2 .EQ. SNBONL__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  SINGL__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  IOCAL__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. RATONL__DTP ) THEN
           IF ( CODE_I2 .EQ. RATONL__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.   RATE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  IOCAL__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. G_GXS__DTP ) THEN
           IF ( CODE_I2 .EQ.  G_GXS__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  GROUP__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.   COMB__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. PX_GXS__DTP ) THEN
           IF ( CODE_I2 .EQ. PX_GXS__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  PHASE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  MIXED__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  XBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. PS_GXS__DTP ) THEN
           IF ( CODE_I2 .EQ. PS_GXS__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  PHASE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  MIXED__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  SBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. PX_GX__DTP ) THEN
           IF ( CODE_I2 .EQ.  PX_GX__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  PHASE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  MIXED__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  XBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. PX_GS__DTP ) THEN
           IF ( CODE_I2 .EQ.  PX_GS__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  PHASE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  MIXED__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  XBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. PS_GX__DTP ) THEN
           IF ( CODE_I2 .EQ.  PS_GX__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  PHASE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  MIXED__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  SBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. PS_GS__DTP ) THEN
           IF ( CODE_I2 .EQ.  PS_GS__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  PHASE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  MIXED__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  SBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. P_PXS__DTP ) THEN
           IF ( CODE_I2 .EQ.  P_PXS__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  PHASE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.   COMB__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. GX__DTP ) THEN
           IF ( CODE_I2 .EQ.  GX__DTP    ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  GROUP__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  XBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. GS__DTP ) THEN
           IF ( CODE_I2 .EQ.  GS__DTP    ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  GROUP__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  SBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. PX__DTP ) THEN
           IF ( CODE_I2 .EQ.  PX__DTP    ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  PHASE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  XBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. PS__DTP ) THEN
           IF ( CODE_I2 .EQ.  PS__DTP    ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  PHASE__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  SBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. SNG_X__DTP ) THEN
           IF ( CODE_I2 .EQ.  SNG_X__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  SINGL__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  XBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. SNG_S__DTP ) THEN
           IF ( CODE_I2 .EQ.  SNG_S__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  SINGL__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  SBAND__DTP ) DATYP_INQ = .TRUE.
         ELSE IF ( IDATYP_I2 .EQ. FUSED__DTP ) THEN
           IF ( CODE_I2 .EQ.  FUSED__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  GROUP__DTP ) DATYP_INQ = .TRUE.
           IF ( CODE_I2 .EQ.  DELAY__DTP ) DATYP_INQ = .TRUE.
        ELSE
           WRITE ( UNIT=STR, FMT='(I12)' ) IDATYP_I2
           CALL FERR ( INT2(1102), 'DATYP_INQ: unsupported code of IDATYP: '// &
     &          STR, INT2(0), INT2(0) )
      END IF
!
      RETURN
      END  !#!  DATYP_INQ  #!#
