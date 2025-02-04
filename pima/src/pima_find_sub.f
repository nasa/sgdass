      SUBROUTINE PIMA_FIND_SUB ( PIM, IND_SCA, FL_USE_OBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FIND_SUB finds all subarrays that corresponds to      *
! *   observations marked as .TRUE. in FL_USE_OBS. Subarray in this      *
! *   context is a set of observations that form a connected graph.      *
! *                                                                      *
! *  ### 06-AUG-2013  PIMA_FIND_SUB  v3.2 (c) L. Petrov 28-JUN-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  IND_SCA, IUER 
      TYPE     ( PIMA__TYPE    ) :: PIM
      TYPE     ( PIM_SUB__TYPE ) :: SUB
      LOGICAL*1  FL_USE_OBS(PIM%NOBS)
      LOGICAL*1  FL_DIFF
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, J18, J19, J20, J21, J22, &
     &           L_SUB, IP, N_USED, N_FOUND, IND_OLD_SUB, OLD_L_SUB, IER 
      LOGICAL*1  FL_REDUCE_L_SUB 
      INTEGER*4, EXTERNAL :: ADD_LIS, IFIND_PL, ILEN, I_LEN
!
      L_SUB   = 1
      CALL NOUT ( SIZEOF(SUB), SUB )
      ALLOCATE  ( SUB%OBS_IND_SUB(PIM%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*PIM%NOBS, STR )
           CALL ERR_LOG ( 7821, IUER, 'PIMA_FIND_SUB', 'Failure in an '// &
     &         'attempt to '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array SUB%OBS_IND_SUB' )
           RETURN 
      END IF
      SUB%OBS_IND_SUB = 0
      N_USED  = 0
      N_FOUND = 0
      DO 410 J1=1,PIM%NSTA ! The maximum number of subarrays
         DO 420 J2=1,2
            DO 430 J3=1,PIM%NOBS
               IF ( FL_USE_OBS(J3) .AND. SUB%OBS_IND_SUB(J3) == 0 ) THEN
                    IF ( J1 == 1 .AND. J2 == 1 ) N_USED = N_USED + 1 ! Increment of used points 
                    IF ( SUB%L_STA(L_SUB) == 0 ) THEN
!
! ---------------------- Well, we start the new sub-array. Add the station
! ---------------------- indexes of the J3-th observation to the list of
! ---------------------- station indexes
!
                         IP = ADD_LIS ( PIM__MSTA, SUB%L_STA(L_SUB), &
     &                                  SUB%LIS_STA(1,L_SUB), &
     &                                  INT(PIM%OBS(J3)%STA_IND(1),KIND=4), IER )
                         IP = ADD_LIS ( PIM__MSTA, SUB%L_STA(L_SUB), &
     &                                  SUB%LIS_STA(1,L_SUB), &
     &                                  INT(PIM%OBS(J3)%STA_IND(2),KIND=4), IER )
!
! ---------------------- Set the sub-array index
!
                         SUB%OBS_IND_SUB(J3) = L_SUB 
                         N_FOUND = N_FOUND + 1 ! Count the observation that is associated to
!                                              ! a known subarray
                       ELSE
!
! ---------------------- Check the observation among new (SIC!) subarrays, whether it is 
! ---------------------- eligible to join
!
                         DO 440 J4=1,L_SUB
                            DO 450 J5=1,SUB%L_STA(J4)
                               IF ( PIM%OBS(J3)%STA_IND(1) == SUB%LIS_STA(J5,J4) .OR. &
     &                              PIM%OBS(J3)%STA_IND(2) == SUB%LIS_STA(J5,J4)      ) THEN
!
! --------------------------------- Yes! At least one station of a baseline is already in the list.
! --------------------------------- Add both stations to the list...
!
                                    IP = ADD_LIS ( PIM__MSTA, SUB%L_STA(J4), &
     &                                             SUB%LIS_STA(1,J4), &
     &                                             INT(PIM%OBS(J3)%STA_IND(1),KIND=4), IER )
                                    IP = ADD_LIS ( PIM__MSTA, SUB%L_STA(J4), &
     &                                             SUB%LIS_STA(1,J4), &
     &                                             INT(PIM%OBS(J3)%STA_IND(2),KIND=4), IER )
!
! --------------------------------- ... and assign the observation to a given subarry
!
                                    SUB%OBS_IND_SUB(J3) = J4
                                    N_FOUND = N_FOUND + 1
                                    GOTO 840
                               END IF
 450                        CONTINUE 
 440                     CONTINUE 
 840                     CONTINUE 
                    END IF
               END IF
 430        CONTINUE 
 420     CONTINUE
         IF ( N_FOUND == N_USED ) GOTO 810
         L_SUB = L_SUB + 1
         IF ( L_SUB > PIM__MSUB ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%L_SUB, STR )
              CALL ERR_LOG ( 7822, IUER, 'PIMA_FUND_SUB', 'Trap of internal '// &
        &         'control: too many subarrays: more than '//STR )
              RETURN 
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      IF ( SUB%L_STA(L_SUB) == 0 ) THEN
           L_SUB = L_SUB - 1
      END IF
      IF ( PIM%L_SUB == 0 ) THEN
!
! -------- There were no session-wide subarray. Copy subarrays for this scan
! -------- to the session-wide subarray structure 
!
           PIM%L_SUB = L_SUB
           DO 460 J6=1,PIM%L_SUB 
              PIM%SUB%L_STA(J6) = SUB%L_STA(J6)
              PIM%SUB%LIS_STA(1:PIM__MSTA,J6) = SUB%LIS_STA(1:PIM__MSTA,J6)
              IF ( PIM%SUB%L_STA(J6) > 0 ) THEN
                   CALL SORT_I ( PIM%SUB%L_STA(J6), PIM%SUB%LIS_STA(1:PIM__MSTA,J6) )
              END IF
 460       CONTINUE 
           PIM%SUB%STATUS  = PIMA__ALLOCATED
           DO 470 J7=1,PIM%NOBS
              PIM%SUB%OBS_IND_SUB(J7) = SUB%OBS_IND_SUB(J7)
 470       CONTINUE 
         ELSE
           DO 480 J8=1,L_SUB
              IND_OLD_SUB = 0
              DO 490 J9=1,PIM%L_SUB
!
! -------------- First check the number of stations
!
                 IF ( SUB%L_STA(J8) .EQ. PIM%SUB%L_STA(J9) ) THEN 
!
! ------------------- New J8-th local subarray has no less station than 
! ------------------- the global J9-th array
! ------------------- Then check, whether all stations of new local J8th subarray
! ------------------- are present in the global subarray J9
!
                      FL_DIFF = .FALSE.
                      DO 4100 J10=1,PIM%SUB%L_STA(J9)
                         IF ( IFIND_PL ( PIM%SUB%L_STA(J9), PIM%SUB%LIS_STA(1,J9), &
     &                                                      SUB%LIS_STA(J10,J8) ) .LE. 0 ) THEN
                              FL_DIFF = .TRUE.
                         END IF
 4100                 CONTINUE 
                      IF ( .NOT. FL_DIFF ) THEN
                           IND_OLD_SUB = J9
                      END IF
                 END IF
 490          CONTINUE 
!
              IF ( IND_OLD_SUB == 0 ) THEN
                   PIM%L_SUB = PIM%L_SUB + 1
                   PIM%SUB%L_STA(PIM%L_SUB) = SUB%L_STA(J8)
                   PIM%SUB%LIS_STA(1:PIM__MSTA,PIM%L_SUB) = SUB%LIS_STA(1:PIM__MSTA,J8)
                   CALL SORT_I ( PIM%SUB%L_STA, PIM%SUB%LIS_STA )
                   DO 4110 J11=1,PIM%NOBS
                      IF ( SUB%OBS_IND_SUB(J11) == J8 ) THEN
                           PIM%SUB%OBS_IND_SUB(J11) = PIM%L_SUB 
                      END IF
 4110              CONTINUE 
                 ELSE
!
! ---------------- New local subarray J8 is either identical or a subset of 
! ---------------- the global J8-th subarray. 
!
                   DO 4120 J12=1,PIM%NOBS
                      IF ( SUB%OBS_IND_SUB(J12) == J8 ) THEN
                           PIM%SUB%OBS_IND_SUB(J12) = IND_OLD_SUB 
                      END IF
 4120              CONTINUE 
              END IF
 480       CONTINUE 
      END IF
!
      DO 4130 J13=1,PIM%L_SUB
         IF ( PIM%SUB%L_STA(J13) == 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J13, STR )
              CALL ERR_LOG ( 7823, IUER, 'PIMA_FUND_SUB', 'Trap of internal '// &
        &         'control: subarray '//STR(1:I_LEN(STR))//' has no '// &
     &            'stations' )
              RETURN 
         END IF
!
! ------ Again sort. This is very important! Otherwise difmap will compute
! ------ incorrect phase closure
!
         CALL SORT_I ( PIM%SUB%L_STA(J13), PIM%SUB%LIS_STA(1,J13) )
 4130 CONTINUE
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 7 ) THEN
           DO 4140 J14=1,PIM%NOBS
              IF ( FL_USE_OBS(J14) ) THEN
                   WRITE ( 6, 220 ) J14, PIM%SUB%OBS_IND_SUB(J14), PIM%OBS(J14)%STA_IND 
  220              FORMAT ( 'PIMA_FIND_SUB Final  Ind_obs: ', I5, ' Ind_sub: ', I2, &
     &                      ' Ind_sta: ', I2, 1X, I2 )
              END IF
 4140      CONTINUE 
!
           DO 4150 J15=1,PIM%L_SUB
              WRITE ( 6, 230 ) J15, PIM%SUB%L_STA(J15), &
     &                         PIM%SUB%LIS_STA(1:PIM%SUB%L_STA(J15),J15)
  230         FORMAT ( 'Sub: ', I3, ' L_Sta: ', I2, ' Sta: ', 12(I2,1X) )
 4150      CONTINUE
      END IF
!
      DEALLOCATE  ( SUB%OBS_IND_SUB )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FIND_SUB  !#!#
