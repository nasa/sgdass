      SUBROUTINE DCM_PERFORM ( DCM, EDC, COUNTER, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine DCM_PERFORM  actually performs decimatin: it sets the      *
! *   field DCM_STS in the exterinal decimation file using criteria      *
! *   formalized in the DCM object.                                      *
! *                                                                      *
! *  ### 26-OCT-2007  DCM_PERFORM   v1.0 (c) L. Petrov  26-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'edc.i'
      TYPE     ( DCM__TYPE     ) :: DCM
      TYPE     ( EDC__TYPE     ) :: EDC
      INTEGER*4  COUNTER(DCM__M_OBJ), IUER
      LOGICAL*4  FL_INC, FL_EXC, FL_IN(2), FL_EX(2)
      CHARACTER  BAS_NAM(2)*16, BAS_NA(2)*16
      INTEGER*4  J1, J2, J3, IND_OBJ, IND_OB(2), IER
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF
!
      EDC%HEA%PRC_NAME = DCM%PRC_NAME
      DO 410 J1=1,EDC%HEA%N_OBS
         IF ( EDC%OBS(J1)%SUP_STS .NE. 0 ) GOTO 410
         IF ( DCM%OBJECT == DCM__ALL ) THEN
!
! =========== All type
!
              IND_OBJ = 1
              COUNTER(IND_OBJ) = COUNTER(IND_OBJ) + 1
              IF ( MOD(COUNTER(IND_OBJ),DCM%TOTAL_DCM) == DCM%SELECT_DCM ) THEN
                   EDC%OBS(J1)%DCM_STS = 0 
                 ELSE 
                   EDC%OBS(J1)%DCM_STS = DCM%EDC_PAR
              END IF
            ELSE IF ( DCM%OBJECT == DCM__SOU ) THEN
!
! =========== Source type
!
!
! ----------- Check include list
!
              IF ( LTM_DIF ( 3, DCM%L_INC, DCM%NAM_INC, &
     &             EDC%C_SOU(EDC%OBS(J1)%IND_SOU) ) > 0 ) THEN
                   FL_INC = .TRUE.
                 ELSE 
                   FL_INC = .FALSE.
              END IF
!
! ----------- Check exclude list
!
              IF ( LTM_DIF ( 3, DCM%L_EXC, DCM%NAM_EXC, &
     &             EDC%C_SOU(EDC%OBS(J1)%IND_SOU) ) > 0 ) THEN
                   FL_EXC = .TRUE.
                 ELSE 
                   FL_EXC = .FALSE.
              END IF
!
! ----------- Find the the object in the list (or adding it there)
!
              CALL ERR_PASS ( IUER, IER )
              IND_OBJ = ADD_CLIST ( DCM__M_OBJ, DCM%L_OBJ, DCM%NAM_OBJ, &
     &                              EDC%C_SOU(EDC%OBS(J1)%IND_SOU), IER )
              IF ( FL_INC .AND. .NOT. FL_EXC ) THEN
!
! ---------------- Perform decimation
!
                   COUNTER(IND_OBJ) = COUNTER(IND_OBJ) + 1
                   IF ( MOD(COUNTER(IND_OBJ),DCM%TOTAL_DCM) == &
     &                  DCM%SELECT_DCM ) THEN
                        EDC%OBS(J1)%DCM_STS = 0 
                     ELSE 
                       EDC%OBS(J1)%DCM_STS = DCM%EDC_PAR
                   END IF
              END IF
            ELSE IF ( DCM%OBJECT == DCM__STA ) THEN
!
! =========== Station type
!
!
! ----------- Check both stations in the include list
!
              IF ( LTM_DIF ( 3, DCM%L_INC, DCM%NAM_INC, &
     &             EDC%C_STA(EDC%OBS(J1)%IND_STA(1)) ) > 0 ) THEN
                   FL_IN(1) = .TRUE.
                 ELSE 
                   FL_IN(1) = .FALSE.
              END IF
              IF ( LTM_DIF ( 3, DCM%L_INC, DCM%NAM_INC, &
     &             EDC%C_STA(EDC%OBS(J1)%IND_STA(2)) ) > 0 ) THEN
                   FL_IN(2) = .TRUE.
                 ELSE 
                   FL_IN(2) = .FALSE.
              END IF
!
! ----------- Check both stations in the exclude list
!
              FL_EXC = .FALSE.
              IF ( LTM_DIF ( 3, DCM%L_EXC, DCM%NAM_EXC, &
     &             EDC%C_STA(EDC%OBS(J1)%IND_STA(1)) ) > 0 ) THEN
                   FL_EX(1) = .TRUE.
                 ELSE 
                   FL_EX(1) = .FALSE.
              END IF
              IF ( LTM_DIF ( 3, DCM%L_EXC, DCM%NAM_EXC, &
     &             EDC%C_STA(EDC%OBS(J1)%IND_STA(2)) ) > 0 ) THEN
                   FL_EX(2) = .TRUE.
                 ELSE
                   FL_EX(2) = .FALSE.
              END IF
!
! ----------- Find the object in the list (or adding it there)
!
              CALL ERR_PASS ( IUER, IER )
              IND_OB(1) = ADD_CLIST ( DCM__M_OBJ, DCM%L_OBJ, DCM%NAM_OBJ, &
     &                                EDC%C_STA(EDC%OBS(J1)%IND_STA(1)), IER )
              IND_OB(2) = ADD_CLIST ( DCM__M_OBJ, DCM%L_OBJ, DCM%NAM_OBJ, &
     &                                EDC%C_STA(EDC%OBS(J1)%IND_STA(2)), IER )
              IF ( ( FL_IN(1) .OR. FL_IN(2) ) .AND. &
     &             .NOT. FL_EX(1)             .AND. &
     &             .NOT. FL_EX(2)                   ) THEN
!
! ---------------- Perform decimation
!
                   IF ( FL_IN(1) ) THEN
!
! --------------------- First, check the first station
!
                        COUNTER(IND_OB(1)) = COUNTER(IND_OB(1)) + 1
                        IF ( MOD(COUNTER(IND_OB(1)),DCM%TOTAL_DCM) == &
     &                       DCM%SELECT_DCM                           ) THEN
!
                             EDC%OBS(J1)%DCM_STS = 0 
                          ELSE 
                             EDC%OBS(J1)%DCM_STS = DCM%EDC_PAR
                        END IF
                      ELSE IF ( FL_IN(2) ) THEN
!
! --------------------- If the first station is not in the include list, 
! --------------------- check the second station
!
                        COUNTER(IND_OB(2)) = COUNTER(IND_OB(2)) + 1
                        IF ( MOD(COUNTER(IND_OB(2)),DCM%TOTAL_DCM) == &
     &                       DCM%SELECT_DCM                           ) THEN
!
                             EDC%OBS(J1)%DCM_STS = 0 
                          ELSE 
                             EDC%OBS(J1)%DCM_STS = DCM%EDC_PAR
                        END IF
                   END IF
              END IF
            ELSE IF ( DCM%OBJECT == DCM__BAS ) THEN
!
! =========== Baseline type
!
! ----------- Check include list
!
              BAS_NA(1) = EDC%C_STA(EDC%OBS(J1)%IND_STA(1))//EDC%C_STA(EDC%OBS(J1)%IND_STA(2)) 
              BAS_NA(2) = EDC%C_STA(EDC%OBS(J1)%IND_STA(2))//EDC%C_STA(EDC%OBS(J1)%IND_STA(1)) 
              IF ( BAS_NA(1) < BAS_NA(2) ) THEN
                   BAS_NAM = BAS_NA(1)
                 ELSE 
                   BAS_NAM = BAS_NA(2)
              END IF
              IF ( LTM_DIF ( 3, DCM%L_INC, DCM%NAM_INC, BAS_NA(1) ) > 0 .OR. &
     &             LTM_DIF ( 3, DCM%L_INC, DCM%NAM_INC, BAS_NA(2) ) > 0 ) THEN
                   FL_INC = .TRUE.
                 ELSE 
                   FL_INC = .FALSE.
              END IF
!
! ----------- Check exclude list
!
              IF ( LTM_DIF ( 3, DCM%L_EXC, DCM%NAM_EXC, BAS_NA(1) ) > 0 .OR. &
     &             LTM_DIF ( 3, DCM%L_EXC, DCM%NAM_EXC, BAS_NA(2) ) > 0 ) THEN
                   FL_EXC = .TRUE.
                 ELSE 
                   FL_EXC = .FALSE.
              END IF
!
! ----------- Find the the object in the list (or adding it there)
!
              CALL ERR_PASS ( IUER, IER )
              IND_OBJ = ADD_CLIST ( DCM__M_OBJ, DCM%L_OBJ, DCM%NAM_OBJ, &
     &                              BAS_NAM, IER )
              IF ( FL_INC .AND. .NOT. FL_EXC ) THEN
!
! ---------------- Perform decimation
!
                   COUNTER(IND_OBJ) = COUNTER(IND_OBJ) + 1
                   IF ( MOD(COUNTER(IND_OBJ),DCM%TOTAL_DCM) == &
     &                  DCM%SELECT_DCM ) THEN
                        EDC%OBS(J1)%DCM_STS = 0 
                     ELSE 
                       EDC%OBS(J1)%DCM_STS = DCM%EDC_PAR
                   END IF
              END IF
            ELSE 
              CALL ERR_LOG ( 6391, IUER, 'DCM_PERFORM', 'Trap of internal '// &
     &            'control: unsuppored parameter DCM%OBJECT: '//DCM%OBJECT )
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  DCM_PERFORM  !#!#
