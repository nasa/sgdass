!
! >>>>> INCLUDE-BLOCK with description of data structures used for saving
!       equations of conditions in memory.
!
! Automatically generated on 2024.11.30_21:43:54 from equmem_i.templ
!
!       equmem.i 27-MAR-2000 09:48:16  --  2017.10.17_12:05:17
!
        TYPE      EQUMEM__STRU
            INTEGER*4  FIRST_FIELD
            INTEGER*4  NOBS
            INTEGER*8  LEN_TOTAL
            LOGICAL*4  USE_FLAG
!
            INTEGER*8 :: ADR_MEMOBS(MAX_OBS)
            INTEGER*8        LEN_MEMOBS(MAX_OBS)
!
            INTEGER*4  N_GLO(MAX_OBS)
            INTEGER*4  N_LOC(MAX_OBS)
            INTEGER*4  N_SG1(MAX_OBS)
            INTEGER*4  N_SG2(MAX_OBS)
!
            INTEGER*4  CLO_SEG(MAX_OBS)
            INTEGER*4  ATM_SEG(MAX_OBS)
            INTEGER*4  EOP_SEG(MAX_OBS)
!
            INTEGER*4  STATUS
            INTEGER*4  LAST_FIELD
        END TYPE  EQUMEM__STRU  ! EQUMEM_STRU  !
!
! <<<<< end of INCLUDE-BLOCK  equmem.i
!
