      SUBROUTINE META_VAR_EXCHANGE ( IACT, N_BAS, N_STA, N_SOU, &
     &                               CLO_INTR, ATM_INTR, TIL_INTR, &
     &                               EOP_CNS, CLO_CNS, ATM_CNS,  &
     &                               TLOF_CNS, TLRT_CNS, STPS_CNS, &
     &                               SOCO_CNS, DGCL_EST, BSCL_CNS, &
     &                               RW_DEL, RW_RAT, RW_BAS )
! ************************************************************************
! *                                                                      *
! *   Routine META_VAR_EXCHANGE is for
! *                                                                      *
! * ### 26-JUL-2007 META_VAR_EXCHANGE v1.1 (c) L. Petrov 02-AUG-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'glbc3.i'
      INTEGER*4  IACT, N_BAS, N_STA, N_SOU
      REAL*8   CLO_INTR(0:SLV__MAX_SOLTYP-1),             & 
     &         ATM_INTR(0:SLV__MAX_SOLTYP-1),             &
     &         TIL_INTR(0:SLV__MAX_SOLTYP-1),             &
     &         EOP_CNS(11,0:SLV__MAX_SOLTYP-1),           &
     &         CLO_CNS(N_STA,0:SLV__MAX_SOLTYP-1),  &
     &         ATM_CNS(N_STA,0:SLV__MAX_SOLTYP-1),  &
     &         TLOF_CNS(N_STA,0:SLV__MAX_SOLTYP-1), &
     &         TLRT_CNS(N_STA,0:SLV__MAX_SOLTYP-1), &
     &         STPS_CNS(N_STA,0:SLV__MAX_SOLTYP-1), &
     &         SOCO_CNS(N_SOU,0:SLV__MAX_SOLTYP-1), &
     &         BSCL_CNS(N_BAS,0:SLV__MAX_SOLTYP-1), &
     &         RW_DEL(0:SLV__MAX_SOLTYP-1,N_BAS),   &
     &         RW_RAT(0:SLV__MAX_SOLTYP-1,N_BAS)
      INTEGER*2  DGCL_EST(N_STA,0:SLV__MAX_SOLTYP-1)
      CHARACTER  RW_BAS(2,N_BAS)*(*)
      INTEGER*4  J1, J2, J3, J4, J5
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      CALL USE_GLBFIL_3 ( 'ORC' )
      IF ( IACT == 0  .OR.  IACT == 1 ) THEN
           META_N_STA = N_STA
           META_N_SOU = N_SOU
           META_N_BAS = N_BAS
!
           CALL NOUT_R8 ( SLV__MAX_SOLTYP,    META_CLO_INTR )
           CALL NOUT_R8 ( SLV__MAX_SOLTYP,    META_ATM_INTR )
           CALL NOUT_R8 ( SLV__MAX_SOLTYP,    META_TIL_INTR )
           CALL NOUT_R8 ( SLV__MAX_SOLTYP*11, META_EOP_CNS  )
!
           CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_STA), META_CLO_CNS  )
           CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_STA), META_ATM_CNS  )
           CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_STA), META_TLOF_CNS )
           CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_STA), META_TLRT_CNS )
           CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_STA), META_STPS_CNS )
           CALL NOUT_I2 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_STA), META_DGCL_EST )
           CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_SRC), META_SOCO_CNS )
           CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_BSL), META_BSCL_CNS )
           CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_BSL), META_RW_DEL   )
           CALL NOUT_R8 ( SLV__MAX_SOLTYP*INT4(MAX_ARC_BSL), META_RW_RAT   )
           CALL NOUT    ( 2*8*INT4(MAX_ARC_BSL),             META_RW_BAS   )
      END IF
!
      DO 410 J1=0,SLV__MAX_SOLTYP-1
         IF ( IACT == 1 ) THEN
              META_CLO_INTR(J1) = CLO_INTR(J1) 
              META_ATM_INTR(J1) = ATM_INTR(J1) 
              META_TIL_INTR(J1) = TIL_INTR(J1) 
            ELSE IF ( IACT == 2 ) THEN
              CLO_INTR(J1) = META_CLO_INTR(J1) 
              ATM_INTR(J1) = META_ATM_INTR(J1) 
              TIL_INTR(J1) = META_TIL_INTR(J1) 
         END IF
!
         DO 420 J2=1,11
            IF ( IACT == 1 ) THEN
                 META_EOP_CNS(J2,J1) = EOP_CNS(J2,J1) 
              ELSE IF ( IACT == 2 ) THEN
                 EOP_CNS(J2,J1) = META_EOP_CNS(J2,J1) 
            END IF
 420     CONTINUE 
!
         DO 430 J3=1,N_STA
            IF ( IACT == 1 ) THEN
                 META_CLO_CNS(J1,J3)  = CLO_CNS(J3,J1)
                 META_ATM_CNS(J1,J3)  = ATM_CNS(J3,J1) 
                 META_TLOF_CNS(J1,J3) = TLOF_CNS(J3,J1) 
                 META_TLRT_CNS(J1,J3) = TLRT_CNS(J3,J1) 
                 META_STPS_CNS(J1,J3) = STPS_CNS(J3,J1) 
                 META_DGCL_EST(J1,J3) = DGCL_EST(J3,J1) 
               ELSE IF ( IACT == 2 ) THEN
                 CLO_CNS(J3,J1)  = META_CLO_CNS(J1,J3)  
                 ATM_CNS(J3,J1)  = META_ATM_CNS(J1,J3)  
                 TLOF_CNS(J3,J1) = META_TLOF_CNS(J1,J3) 
                 TLRT_CNS(J3,J1) = META_TLRT_CNS(J1,J3) 
                 STPS_CNS(J3,J1) = META_STPS_CNS(J1,J3) 
                 DGCL_EST(J3,J1) = META_DGCL_EST(J1,J3) 
            END IF
 430     CONTINUE 
!
         DO 440 J4=1,N_SOU
            IF ( IACT == 1 ) THEN
                 META_SOCO_CNS(J1,J4) = SOCO_CNS(J4,J1) 
               ELSE IF ( IACT == 2 ) THEN
                 SOCO_CNS(J4,J1) = META_SOCO_CNS(J1,J4) 
            END IF
 440     CONTINUE 
!
         DO 450 J5=1,N_BAS
            IF ( IACT == 1 ) THEN
                 META_RW_BAS(1,J5)  = RW_BAS(1,J5)
                 META_RW_BAS(2,J5)  = RW_BAS(2,J5)
                 META_RW_DEL(J1,J5) = RW_DEL(J1,J5) 
                 META_RW_RAT(J1,J5) = RW_RAT(J1,J5) 
               ELSE IF ( IACT == 2 ) THEN
                 RW_BAS(1,J5)  = META_RW_BAS(1,J5) 
                 RW_BAS(2,J5)  = META_RW_BAS(2,J5) 
                 RW_DEL(J1,J5) = META_RW_DEL(J1,J5) 
                 RW_RAT(J1,J5) = META_RW_RAT(J1,J5) 
            END IF
 450     CONTINUE 
 410  CONTINUE 
!
      IF ( IACT == 1 ) THEN
           CALL USE_GLBFIL_3 ( 'OWC' )
      END IF
!
      RETURN
      END  SUBROUTINE  META_VAR_EXCHANGE  !#!#
