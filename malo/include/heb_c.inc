!
! === heb_c.inc  Version 1.0 of 2013.02.28_12:47:40
!
      INTEGER*4  HEB__MATT, HEB__LATT
      PARAMETER  ( HEB__MATT =  64 ) 
      PARAMETER  ( HEB__LATT = 128 ) 
      TYPE     HEB_C__TYPE
           CHARACTER  ATT_NAME(HEB__MATT)*(HEB__LATT)
           CHARACTER  ATT_VAL(HEB__MATT)*(HEB__LATT)
           INTEGER*8  DIMS(4)
           INTEGER*4  NATTS
!
           TYPE(C_PTR) :: R8_PTR
           TYPE(C_PTR) :: I8_PTR
           TYPE(C_PTR) :: R4_PTR
           TYPE(C_PTR) :: I4_PTR
           TYPE(C_PTR) :: I2_PTR
           TYPE(C_PTR) :: I1_PTR
!
           INTEGER*8  R8_FILL_VALUE
           REAL*8     I8_FILL_VALUE
           REAL*4     R4_FILL_VALUE
           INTEGER*4  I4_FILL_VALUE
           INTEGER*2  I2_FILL_VALUE
           INTEGER*1  I1_FILL_VALUE
!
           REAL*4, POINTER :: ARR_1D_R4(:)       => NULL()
           REAL*4, POINTER :: ARR_2D_R4(:,:)     => NULL()
           REAL*4, POINTER :: ARR_3D_R4(:,:,:)   => NULL()
           REAL*4, POINTER :: ARR_4D_R4(:,:,:,:) => NULL()
!
           REAL*8, POINTER :: ARR_1D_R8(:)       => NULL()
           REAL*8, POINTER :: ARR_2D_R8(:,:)     => NULL()
           REAL*8, POINTER :: ARR_3D_R8(:,:,:)   => NULL()
           REAL*8, POINTER :: ARR_4D_R8(:,:,:,:) => NULL()
!
           INTEGER*1,  POINTER :: ARR_1D_I1(:,:)     => NULL()
           INTEGER*1,  POINTER :: ARR_2D_I1(:,:)     => NULL()
           INTEGER*1,  POINTER :: ARR_3D_I1(:,:,:)   => NULL()
           INTEGER*1,  POINTER :: ARR_4D_I1(:,:,:,:) => NULL()
!
           INTEGER*2,  POINTER :: ARR_1D_I2(:)       => NULL()
           INTEGER*2,  POINTER :: ARR_2D_I2(:,:)     => NULL()
           INTEGER*2,  POINTER :: ARR_3D_I2(:,:,:)   => NULL()
           INTEGER*2,  POINTER :: ARR_4D_I2(:,:,:,:) => NULL()
!
           INTEGER*4,  POINTER :: ARR_1D_I4(:)       => NULL()
           INTEGER*4,  POINTER :: ARR_2D_I4(:,:)     => NULL()
           INTEGER*4,  POINTER :: ARR_3D_I4(:,:,:)   => NULL()
           INTEGER*4,  POINTER :: ARR_4D_I4(:,:,:,:) => NULL()
      END TYPE HEB_C__TYPE

