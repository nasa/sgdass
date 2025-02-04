! >>>>> Include block for Equation of observations in pSolve
! >>>>> 2021.05.18 (c)  L. Petrov  v 0.55  2021.05.18_15:16:25
!
!     Needs constant  MAX_OBS that is found in solve.i
!
      TYPE PSOLVE__EQUOB_TYPE
             INTEGER*4  IND_OBS
             INTEGER*4  NPAR
             INTEGER*4, POINTER :: IND_PAR(:) => NULL()
             REAL*8,    POINTER :: DER_DEL(:) => NULL()
             REAL*8,    POINTER :: DER_RAT(:) => NULL()
             REAL*8     DEL_OC
             REAL*8     DEL_ERR
             REAL*8     RAT_OC
             REAL*8     RAT_ERR
      END TYPE PSOLVE__EQUOB_TYPE
!
      TYPE PSOLVE__EQUOBS_TYPE
          TYPE ( PSOLVE__EQUOB_TYPE ) :: OBS(MAX_OBS)
          INTEGER*4  NOBS
          INTEGER*4  NTOT_PAR
          INTEGER*4  USE_RAT ! 0 -- not to use delay rates; 1 -- to use delay rates
          INTEGER*4  STATUS
      END TYPE PSOLVE__EQUOBS_TYPE
!
      INTEGER*4    EQU__UNDF, EQU__INIT, EQU__FILLED
      PARAMETER  ( EQU__UNDF   = 0 )
      PARAMETER  ( EQU__INIT   = 1924784504 )
      PARAMETER  ( EQU__FILLED = 1274035923 )
