!@This is the start of file &RTPAR
!
!   parameters:
!
      INTEGER*2 A_MATRIX, B_VECTOR
      INTEGER*2 SCALE_V,MAX_SRC_RTSRC,MAX_SRC_PAR
      INTEGER*4 LONG_MAX_PAR, ELEMENTS_LONG,L_MATRIX_SIZE
!
      INTEGER*2 PARMS_WORDS,NUM_TRANSFORM,TRANSFORM_SIZE
!
      PARAMETER ( &
!    &  MAX_SRC_RTSRC    = MIN(MAX_PAR/2,MAX_SRC),
     &  MAX_SRC_RTSRC    = MAX_SRC, &
     &  PARMS_WORDS      = 10, &
     &  NUM_TRANSFORM    = 5, &
!
     &  LONG_MAX_PAR     = MAX_PAR, &
     &  A_MATRIX         = 3*MAX_PAR, &
     &  B_VECTOR         = 2*MAX_PAR, &
     &  MAX_SRC_PAR      = 2*MAX_SRC_RTSRC, &
     &  SCALE_V          = 0, &
     &  L_MATRIX_SIZE    = (LONG_MAX_PAR*(LONG_MAX_PAR+1))/2, &
     &  ELEMENTS_LONG    = L_MATRIX_SIZE+3*LONG_MAX_PAR, &
!
     &  TRANSFORM_SIZE   = (NUM_TRANSFORM*(NUM_TRANSFORM+1))/2)
