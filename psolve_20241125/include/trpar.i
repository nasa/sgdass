!@This is the start of file &TRPAR
!
!   parameters:
!
!   Determined by SOLVE, but derived:
!
      INTEGER*2 A_MATRIX, B_VECTOR, MATRIX_SIZE
      INTEGER*2 SCALE_V,MAX_STA_PAR
      INTEGER*4 MAX_BSL
      INTEGER*4 LONG_MAX_PAR, ELEMENTS_LONG, L_MATRIX_SIZE
!
!   Just used here:
!
      INTEGER*2 PARMS_WORDS,NUM_TRANSFORM,TRANSFORM_SIZE
!
      PARAMETER ( &
     &  MAX_BSL          = (MAX_STA*(MAX_STA-1))/2, &
     &  PARMS_WORDS      = 10, &
     &  NUM_TRANSFORM    = 7, &
!
     &  LONG_MAX_PAR     = MAX_PAR, &
     &  A_MATRIX         = 3*MAX_PAR, &
     &  B_VECTOR         = 2*MAX_PAR, &
     &  MAX_STA_PAR      = 3*MAX_STA, &
     &  SCALE_V          = 0, &
     &  L_MATRIX_SIZE    = (LONG_MAX_PAR*(LONG_MAX_PAR+1))/2, &
     &  ELEMENTS_LONG    = L_MATRIX_SIZE+3*LONG_MAX_PAR, &
!
     &  TRANSFORM_SIZE   = (NUM_TRANSFORM*(NUM_TRANSFORM+1))/2)
