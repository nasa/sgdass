      MODULE PET_UTIL
      INTERFACE
!
      SUBROUTINE ERR_LOG ( NERR, IUER, PROG, MES )
         IMPLICIT   NONE 
         INTEGER*4  NERR, IUER
         CHARACTER, OPTIONAL :: PROG*(*), MES*(*)
      END SUBROUTINE ERR_LOG
!
      SUBROUTINE ERR_PASS ( NERR, IUER )
         IMPLICIT   NONE 
         INTEGER*4  NERR, IUER
      END SUBROUTINE ERR_PASS
!
      SUBROUTINE TIM_TP ( IPRN, TCPU, TELP, OUT )
         IMPLICIT   NONE 
         INTEGER*4, OPTIONAL :: IPRN
         REAL*8,    OPTIONAL :: TELP, TCPU
         CHARACTER, OPTIONAL :: OUT*(*)
      END SUBROUTINE TIM_TP
!
      SUBROUTINE GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, NPAR, &
     &                            LEN_01,  ADR_01, &
     &                            LEN_02,  ADR_02, &
     &                            LEN_03,  ADR_03, &
     &                            LEN_04,  ADR_04, &
     &                            LEN_05,  ADR_05, &
     &                            LEN_06,  ADR_06, &
     &                            LEN_07,  ADR_07, &
     &                            LEN_08,  ADR_08, &
     &                            LEN_09,  ADR_09, &
     &                            LEN_10,  ADR_10, &
     &                            LEN_11,  ADR_11, &
     &                            LEN_12,  ADR_12, &
     &                            LEN_13,  ADR_13, &
     &                            LEN_14,  ADR_14, &
     &                            LEN_15,  ADR_15, &
     &                            LEN_16,  ADR_16, &
     &                            LEN_17,  ADR_17, &
     &                            LEN_18,  ADR_18, &
     &                            LEN_19,  ADR_19, &
     &                            LEN_20,  ADR_20, &
     &                            LEN_21,  ADR_21, &
     &                            LEN_22,  ADR_22, &
     &                            LEN_23,  ADR_23, &
     &                            LEN_24,  ADR_24, &
     &                            LEN_25,  ADR_25, &
     &                            LEN_26,  ADR_26, &
     &                            LEN_27,  ADR_27, &
     &                            LEN_28,  ADR_28, &
     &                            LEN_29,  ADR_29, &
     &                            LEN_30,  ADR_30, &
     &                            LEN_31,  ADR_31, &
     &                            LEN_32,  ADR_32  )
      IMPLICIT   NONE
      INTEGER*4  IUER, MEM_LEN, MEM_ADR, NPAR
      INTEGER*4  LEN_01
      INTEGER*4, OPTIONAL ::     LEN_02, LEN_03, LEN_04, LEN_05, &
     &           LEN_06, LEN_07, LEN_08, LEN_09, LEN_10, &
     &           LEN_11, LEN_12, LEN_13, LEN_14, LEN_15, &
     &           LEN_16, LEN_17, LEN_18, LEN_19, LEN_20, &
     &           LEN_21, LEN_22, LEN_23, LEN_24, LEN_25, &
     &           LEN_26, LEN_27, LEN_28, LEN_29, LEN_30, &
     &           LEN_31, LEN_32
      INTEGER*4  ADR_01
      INTEGER*4, OPTIONAL ::     ADR_02, ADR_03, ADR_04, ADR_05, &
     &           ADR_06, ADR_07, ADR_08, ADR_09, ADR_10, &
     &           ADR_11, ADR_12, ADR_13, ADR_14, ADR_15, &
     &           ADR_16, ADR_17, ADR_18, ADR_19, ADR_20, &
     &           ADR_21, ADR_22, ADR_23, ADR_24, ADR_25, &
     &           ADR_26, ADR_27, ADR_28, ADR_29, ADR_30, &
     &           ADR_31, ADR_32
      END SUBROUTINE GRAB_MEM 
      END INTERFACE
      END MODULE PET_UTIL
