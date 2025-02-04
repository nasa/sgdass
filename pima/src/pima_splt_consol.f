      SUBROUTINE PIMA_SPLT_CONSOL ( PIM, L_UVO, UVO, SOU_NAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_SPLT_CONSOL
! *                                                                      *
! * ### 27-FEB-2016  PIMA_SPLT_CONSOL v1.0 (c) L. Petrov 28-FEB-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      CHARACTER  SOU_NAM*(*)
      INTEGER*4  L_UVO, IUER 
      TYPE     ( PIMA__TYPE    ) :: PIM
      TYPE     ( PIM_SUB__TYPE ) :: KSUB
      TYPE     ( UVO__TYPE     ) :: UVO(L_UVO)
      LOGICAL*1  FL_CON
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, L_CON, K_SUB, ISA, L_SUB, &
     &           ISA_REF(PIM__MSUB), KSA_REF(PIM__MSUB), KU_SUB(PIM__MSUB), &
     &           KN_STA, KSTA, IND_SUB, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IFIND_PL
!
      L_SUB = PIM%L_SUB
      L_CON = 0
      ISA_REF = 0
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
           WRITE ( 6, 210 ) SOU_NAM, 'BEFORE', PIM%L_SUB
           DO 410 J1=1,PIM%L_SUB
 210          FORMAT ( ' PIMA_SPLT_CONSOL: Sou: ', A, 2X, A, '  Number of subarrays: ', I2 )
              CALL CLRCH ( STR )
              DO 420 J2=1,PIM%SUB%L_STA(J1)
                 CALL INCH   ( PIM%SUB%LIS_STA(J2,J1), STR(1+(J2-1)*3:2+(J2-1)*3) )
                 CALL CHASHR ( STR(1+(J2-1)*3:2+(J2-1)*3) )
 420          CONTINUE 
              WRITE ( 6, 220 ) J1, STR(1:I_LEN(STR))
 220          FORMAT ( 'PIMA_SPLT_CONSOL   Subarray: ', I2, '  Stations:  ', A )
 410      CONTINUE 
      END IF
!
      IF ( PIM%CONF%SPLT_SUBARRAY_CONSOLIDATION == PIMA__SA_MIN ) THEN
           DO 430 J3=1,PIM%L_SUB
              DO 440 J4=1,PIM%L_SUB
                 IF ( J4 == J3  ) GOTO 440
                 IF ( ISA_REF(J4) .NE. 0 ) GOTO 440
                 IF ( PIM%SUB%L_STA(J4) < PIM%SUB%L_STA(J3) ) GOTO 440
                 DO 450 J5=1,PIM%SUB%L_STA(J3)
                    IF ( IFIND_PL ( PIM%SUB%L_STA(J4), PIM%SUB%LIS_STA(1,J4), PIM%SUB%LIS_STA(J5,J3) ) < 1 ) THEN
!
! ---------------------- We found a station in the J3-th subarray that is not
! ---------------------- in the J5-th subarray. Do not consider the J3-th
! ---------------------- subarray any more
!
                         GOTO 440
                    END IF
 450             CONTINUE 
!
! -------------- Oh! All stations of the J3-the subarray are present in the 
! -------------- J4th subarray. We can eliminate this subarray
!
                 ISA_REF(J3) = J4
                 L_CON = L_CON + 1
 440          CONTINUE 
 430       CONTINUE 
        ELSE IF ( PIM%CONF%SPLT_SUBARRAY_CONSOLIDATION == PIMA__SA_MAX ) THEN
           ISA_REF = 0
           KU_SUB  = 0
           DO 460 J6=1,PIM%L_SUB
              KSTA = 0
              DO 470 J7=1,PIM%L_SUB
                 IF ( ISA_REF(J7) > 0 ) GOTO 470
                 IF ( KU_SUB(J7)  > 0 ) GOTO 470
                 IF ( PIM%SUB%L_STA(J7) > KSTA ) THEN
                      KSTA = PIM%SUB%L_STA(J7) 
                      IND_SUB = J7
                 END IF
 470          CONTINUE 
              IF ( KSTA == 0 ) GOTO 860
              DO 480 J8=1,PIM%L_SUB
                 IF ( ISA_REF(J8) .NE. 0 ) GOTO 480
                 IF ( J8 == IND_SUB      ) GOTO 480
                 KN_STA = 0
                 DO 490 J9=1,PIM%SUB%L_STA(J8)
                    IF ( IFIND_PL ( PIM%SUB%L_STA(IND_SUB), PIM%SUB%LIS_STA(1,IND_SUB), &
     &                              PIM%SUB%LIS_STA(J9,J8) ) > 0 ) THEN
                         KN_STA = KN_STA + 1
                    END IF
 490             CONTINUE 
                 IF ( KN_STA > 1 ) THEN
                     ISA_REF(J8) = IND_SUB
                     L_CON = L_CON + 1
                 END IF
 480          CONTINUE 
              KU_SUB(IND_SUB) = 1
 460       CONTINUE 
 860       CONTINUE 
      END IF
      IF ( L_CON > 0 ) THEN
           K_SUB = 0
           KSUB%IND_SOU = PIM%SUB%IND_SOU
           ALLOCATE ( KSUB%OBS_IND_SUB(PIM%NOBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH ( 4*PIM%NOBS, STR )
                CALL ERR_LOG ( 8721, IUER, 'PIMA_SPLT_CONSOL', 'Failure '// &
          &         'to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
          &         'dynamic memory for array PIM%SUB%OBS_IND_SUB' )
                RETURN 
           END IF
!
           ALLOCATE  ( KSUB%TIM_SRT(PIM__MSUB,PIM__MSCA), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( 8*PIM__MSUB*PIM__MSCA, STR )
                CALL ERR_LOG ( 8722, IUER, 'PIMA_SPLT_CONSOL', 'Failure '// &
          &         'to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
          &         'dynamic memory for array PIM%SUB%TIM_SRT' )
                RETURN 
           END IF
!
           DO 4130 J13=1,PIM%L_SUB
              IF ( ISA_REF(J13) == 0 ) THEN
                   K_SUB = K_SUB + 1
                   KSUB%L_STA(K_SUB) = PIM%SUB%L_STA(J13)
                   KSUB%LIS_STA(1:KSUB%L_STA(K_SUB),K_SUB) = PIM%SUB%LIS_STA(1:PIM%SUB%L_STA(J13),J13)
                   KSUB%TIM_SRT(K_SUB,1:PIM__MSCA) = PIM%SUB%TIM_SRT(J13,1:PIM__MSCA)
                   KSA_REF(J13) = K_SUB
              END IF
 4130      CONTINUE 
           DO 4140 J14=1,PIM%L_SUB
              IF ( ISA_REF(J14) > 0 ) THEN
!
! ---------------- Check nested consolidation
!
                   IF ( ISA_REF(ISA_REF(J14)) > 0 ) THEN
                        ISA_REF(J14) = ISA_REF(ISA_REF(J14)) 
                   END IF
                   IF ( ISA_REF(ISA_REF(J14)) > 0 ) THEN
                        ISA_REF(J14) = ISA_REF(ISA_REF(J14)) 
                   END IF
                   IF ( ISA_REF(ISA_REF(J14)) > 0 ) THEN
                        ISA_REF(J14) = ISA_REF(ISA_REF(J14)) 
                   END IF
!
                   KSA_REF(J14) = KSA_REF(ISA_REF(J14))
                   ISA          = KSA_REF(J14)        ! Index of the subarray in the new list
                   DO 4150 J15=1,PIM%SUB%L_STA(J14)
                      IF ( IFIND_PL ( KSUB%L_STA(ISA),     &
     &                                KSUB%LIS_STA(1,ISA), &
     &                                PIM%SUB%LIS_STA(J15,J14) ) < 1   ) THEN
                           KSUB%L_STA(ISA) = KSUB%L_STA(ISA) + 1
                           KSUB%LIS_STA(KSUB%L_STA(ISA),ISA) = PIM%SUB%LIS_STA(J15,J14) 
                      END IF                      
 4150              CONTINUE 
                   CALL SORT_I ( KSUB%L_STA(ISA), KSUB%LIS_STA(1,ISA) )
              END IF
 4140      CONTINUE 
           KSUB%OBS_IND_SUB = PIM%SUB%OBS_IND_SUB
           DO 4160 J16=1,PIM%NOBS
              IF ( PIM%OBS(J16)%SOU_IND == PIM%SUB%IND_SOU ) THEN
                   IF ( KSUB%OBS_IND_SUB(J16) > 0 ) THEN
                        PIM%SUB%OBS_IND_SUB(J16) = KSA_REF(KSUB%OBS_IND_SUB(J16))
                   END IF
              END IF
 4160      CONTINUE 
           DO 4170 J17=1,L_UVO
!
! ----------- Change the subarray index
!
              UVO(J17)%IND_SUB = KSA_REF(UVO(J17)%IND_SUB)
 4170      CONTINUE 
           PIM%L_SUB = K_SUB
           PIM%SUB%L_STA = KSUB%L_STA
           PIM%SUB%LIS_STA = KSUB%LIS_STA
!
           DEALLOCATE ( KSUB%OBS_IND_SUB )
           DEALLOCATE ( KSUB%TIM_SRT )
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, 230 ) L_CON, L_SUB, PIM%L_SUB, SOU_NAM 
 230       FORMAT ( ' PIMA_SPLT_CONSOL: ', I2, ' subarrays out of ', &
     &              I2, ' were consolidated into ', I2, ' for source ', A )
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
           WRITE ( 6, 210 ) SOU_NAM, 'AFTER ', PIM%L_SUB
           DO 4180 J18=1,PIM%L_SUB
              CALL CLRCH ( STR )
              DO 4190 J19=1,PIM%SUB%L_STA(J18)
                 CALL INCH   ( PIM%SUB%LIS_STA(J19,J18), STR(1+(J19-1)*3:2+(J19-1)*3) )
                 CALL CHASHR ( STR(1+(J19-1)*3:2+(J19-1)*3) )
 4190         CONTINUE 
              WRITE ( 6, 220 ) J18, STR(1:I_LEN(STR))
 4180      CONTINUE 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_SPLT_CONSOL  !#!  
