!
! >>>>> INCLUDE-BLOCK with description of data structures used by
!       programs dclient
!
!       sima.i  2000.08.02 v 1.0  Leonid Petrov  02-AUG-2000 12:09:52
!
        INTEGER*4  M_PAR, M_BUF
        PARAMETER  ( M_PAR =   3 )
        PARAMETER  ( M_BUF = 128 )
!
        TYPE      SIMA__STRU
           INTEGER*4      FIRST_FIELD
!
           CHARACTER*256  BATCH_CONTROL_FILE
           CHARACTER*256  TEMP_CONTROL_FILE
           CHARACTER*256  SIMA_OUTPUT_FILE
           CHARACTER*2    SOLVE_INITIALS
!
           CHARACTER*256  CONFIG_FILE
           INTEGER*4      LAST_FIELD
        END TYPE  SIMA__STRU  !  SIMA__STRU  !
!
! <<<<< end of INCLUDE-BLOCK  sima.i
!
