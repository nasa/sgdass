      SUBROUTINE PIMA_FRG_COMBINE ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FRG_COMBINE updates the data structure for creating   *
! *   a virtual combined frequency group. The combined frequency group   *
! *   is the last frequency group in the list. It has sorted             *
! *   intermediate frequency that is a combination of other groups.      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     PIM ( PIMA__TYPE ) -- Object with information related to package *
! *                           PIMA.                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! * ### 18-DEC-2014 PIMA_FRG_COMBINE  v1.1 (c) L. Petrov 23-JUL-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE    ) :: PIM
      INTEGER*4  IUER
      TYPE     ( PIM_FRQ__TYPE ) :: NEW_FRQ(PIM__MFRQ,PIM__MFRG)
      REAL*8,    POINTER         :: FREQ_ARR(:,:,:) => NULL()
      INTEGER*4  NEW_UV_IND(PIM__MUV,PIM__MFRG), SAVED_CORR_FLAG(PIM__MUV,PIM__MFRG)
      LOGICAL*1  FL_INIT
      CHARACTER  STR*128, STR1*32, STR2*32
      REAL*8     TIM_FIRST
      INTEGER*4  NFRQ, NFRG, NFRG_OLD, IFRQ, MAX_NUM_OBS, IND_AP, UV_IND, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, MAX_I4
#ifdef GNU
      INTEGER*4, EXTERNAL :: PIMA_COMPAR_FRQ
#else
      INTEGER*2, EXTERNAL :: PIMA_COMPAR_FRQ
#endif
!
      NFRG_OLD = PIM%NFRG  ! Keep the old frequency group counter
!
! --- Compute mnew frequency and frequency group counters
!
      NFRG = PIM%VIRT_NFRG + 1
      NFRQ = PIM%NFRQ*PIM%NFRG
!
! --- Allocate memory for the new frequency table and initialize it
!
      ALLOCATE ( FREQ_ARR(PIM%NCHN,NFRQ,NFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*PIM%NCHN*NFRQ*NFRG, STR )
           CALL ERR_LOG ( 8171, IUER, 'PIMA_FRG_COMBINE', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for the array FREQ_ARR' )
           RETURN
      END IF
      FREQ_ARR = 0.0D0
      CALL NOUT ( PIM__MFRQ*PIM__MFRG*SIZEOF(NEW_FRQ(1,1)), NEW_FRQ )
!
! --- Copy the old frequency table into the new table
!
      IFRQ = 0
      DO 410 J1=1,PIM%NFRG
         DO 420 J2=1,PIM%NFRQ
            FREQ_ARR(1:PIM%NCHN,J2,J1) = PIM%FREQ_ARR(1:PIM%NCHN,J2,J1)
            NEW_FRQ(J2,J1) = PIM%FRQ(J2,J1)
!
! --------- Combine the new frequency table
!
            IFRQ = IFRQ + 1
            NEW_FRQ(IFRQ,NFRG) = PIM%FRQ(J2,J1)
 420     CONTINUE 
 410  CONTINUE 
!
! --- Sort FRQ array for the new frequency group
!
      CALL FOR_QSORT ( NEW_FRQ(1,NFRG), NFRQ, SIZEOF(NEW_FRQ(1,1)), PIMA_COMPAR_FRQ )
!
      DO 430 J3=1,NFRQ
         DO 440 J4=1,PIM%NCHN
            FREQ_ARR(J4,J3,NFRG) =        NEW_FRQ(J3,NFRG)%FREQ + &
     &                             (J4-1)*NEW_FRQ(J3,NFRG)%CHAN_WIDTH
 440     CONTINUE 
!
! ------ Search for indexes of the frequencies on the new frequency group
! ------ in previous groups
!
         DO 450 J5=1,PIM%NFRG
            DO 460 J6=1,PIM%NFRQ
               IF ( NEW_FRQ(J3,NFRG)%FREQ_I8 == PIM%FRQ(J6,J5)%FREQ_I8 ) THEN
                    PIM%REF_FRQ(J6,NFRG) = J3
                    PIM%REV_FRQ(J3) = J6
                    PIM%REV_FRG(J3) = J5
               END IF
 460        CONTINUE 
 450     CONTINUE 
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
              WRITE ( 6, * ) 'PIMA_FRQ_COMBINE: IND_FRQ: ', INT2(J3), &
     &                       ' REV_FRQ= ', PIM%REV_FRQ(J3), ' REV_FRG= ', PIM%REV_FRG(J3)
         END IF
 430  CONTINUE 
!      
      PIM%NFRG = NFRG
      PIM%NFRQ = NFRQ
!
! --- Re-allocate memory for PIM%FREQ_ARR 
!
      DEALLOCATE ( PIM%FREQ_ARR )
      ALLOCATE ( PIM%FREQ_ARR(PIM%NCHN,PIM%NFRQ,PIM%NFRG), STAT=IER )
!
! --- Reassign PIM%FREQ_ARR
!
      PIM%FREQ_ARR = 0.0D0
      PIM%FREQ_ARR(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NFRG) = FREQ_ARR(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NFRG) 
!
! --- Reassign PIM%FRQ
!
      PIM%FRQ = NEW_FRQ
      DEALLOCATE ( FREQ_ARR )
!
! --- Cycle over observations
!
      DO 470 J7=1,PIM%NOBS
         DO 480 J8=1,PIM%OBS(J7)%NUM_EPC(1)
            IF ( PIM%NFRG > 2 ) THEN
                 DO 490 J9=2,PIM%NFRG-1
                    IF ( PIM%UV_IND(PIM%OBS(J7)%UV_IND(J8,J9))%TIM_IND == &
     &                   PIM%UV_IND(PIM%OBS(J7)%UV_IND(J8,1))%TIM_IND     ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( J7, STR )
                         CALL CLRCH ( STR1 )
                         CALL INCH  ( J8, STR1 )
                         CALL CLRCH ( STR2 )
                         CALL INCH  ( J9, STR2 )
                         CALL ERR_LOG ( 8172, IUER, 'PIMA_FRG_COMBINE', 'Observation '// &
     &                        TRIM(STR)//', AP index '//TRIM(STR1)//' frequency groups '// &
     &                        TRIM(STR2)//' and 1 have the same time tag while '// &
     &                       'combined frequency group setup was requested. Such case '// &
     &                       'is not supported. Please use frequnecy group merging instead' )
                         RETURN 
                    END IF
 490             CONTINUE 
            END IF
 480     CONTINUE 
!
! ------ Collect the array of indexes for the combined frequency group
!
         PIM%OBS(J7)%NUM_EPC(PIM%NFRG) = 0
         DO 4100 J10=1,PIM%NFRG-1
            DO 4110 J11=1,PIM%OBS(J7)%NUM_EPC(J10)
               PIM%OBS(J7)%NUM_EPC(PIM%NFRG) = PIM%OBS(J7)%NUM_EPC(PIM%NFRG) + 1
               NEW_UV_IND(J11,J10) = PIM%OBS(J7)%UV_IND(J11,J10)
               NEW_UV_IND(PIM%OBS(J7)%NUM_EPC(PIM%NFRG),PIM%NFRG) = PIM%OBS(J7)%UV_IND(J11,J10)
               SAVED_CORR_FLAG(J11,J10) = PIM%OBS(J7)%CORR_FLAG(J11,J10)
 4110        CONTINUE 
 4100    CONTINUE 
!
! ------ Sort UV-indexes
!
         CALL SORT_FAST_I4 ( PIM%OBS(J7)%NUM_EPC(PIM%NFRG), NEW_UV_IND(1,PIM%NFRG) )
!
! ------ Reallocate memory for the new UV index table
!
         DEALLOCATE ( PIM%OBS(J7)%UV_IND )
         DEALLOCATE ( PIM%OBS(J7)%CORR_FLAG )
         PIM%OBS(J7)%NUVS = PIM%NFRG
         MAX_NUM_OBS = MAX_I4 ( PIM%OBS(J7)%NUVS, PIM%OBS(J7)%NUM_EPC )
         ALLOCATE ( PIM%OBS(J7)%UV_IND(MAX_NUM_OBS,PIM%OBS(J7)%NUVS), &
     &              STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 4*MAX_NUM_OBS*PIM%OBS(J7)%NUVS, STR )
              CALL ERR_LOG ( 8173, IUER, 'PIMA_FRG_COMBINE', 'Failure to '// &
     &            'allocate dynamic memory for UV_IND data' )
              RETURN
         END IF
!
! ------ Re-allocate memory for the correlator flags
!
         ALLOCATE ( PIM%OBS(J7)%CORR_FLAG(MAX_NUM_OBS,PIM%OBS(J7)%NUVS), &
     &              STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 4*MAX_NUM_OBS*PIM%OBS(J7)%NUVS, STR )
              CALL ERR_LOG ( 8174, IUER, 'PIMA_FRG_COMBINE', 'Failure to '// &
     &            'allocate dynamic memory for COFF_FLAG data' )
              RETURN
         END IF
         PIM%OBS(J7)%UV_IND = 0
         PIM%OBS(J7)%CORR_FLAG = 0
!
! ------ Fill the new table of correlator flags
!
         FL_INIT = .FALSE.
         DO 4120 J12=1,PIM%OBS(J7)%NUVS
            DO 4130 J13=1,PIM%OBS(J7)%NUM_EPC(J12)
               PIM%OBS(J7)%UV_IND(J13,J12) = NEW_UV_IND(J13,J12)  
               IF ( J12 < PIM%OBS(J7)%NUVS ) THEN
                    PIM%OBS(J7)%CORR_FLAG(J13,J12) = SAVED_CORR_FLAG(J13,J12)
                  ELSE 
                    DO 4140 J14=1,PIM%OBS(J7)%NUVS-1
                       DO 4150 J15=1,PIM%OBS(J7)%NUM_EPC(J14)
                          IF ( PIM%OBS(J7)%UV_IND(J13,J12) == PIM%OBS(J7)%UV_IND(J15,J14)  ) THEN
                               PIM%OBS(J7)%CORR_FLAG(J13,J12) = PIM%OBS(J7)%CORR_FLAG(J15,J14)
                          END IF 
 4150                  CONTINUE 
 4140               CONTINUE 
               END IF
!
               IF ( J12 == PIM%OBS(J7)%NUVS ) THEN
                    UV_IND = PIM%OBS(J7)%UV_IND(J13,J12)  
                    IF ( UV_IND > 0 ) THEN
                         IF ( .NOT. FL_INIT ) THEN
                              TIM_FIRST = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND)
                              FL_INIT = .TRUE.
                         END IF
                         IND_AP = IDNINT ( (PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - TIM_FIRST)/ &
     &                                     PIM%OBS(J7)%AP_LEN )
!
! ---------------------- Update variable PIM%OBS(J7)%NUM_AP_SPAN -- the span
! ---------------------- of accumulation periods
!
                         PIM%OBS(J7)%NUM_AP_SPAN(J12) = IND_AP + 1
                    END IF
               END IF
 4130       CONTINUE 
 4120    CONTINUE 
!
! ------ Store the cross-reference index
!
         PIM%OBS(J7)%REF_FRG_INDS(PIM%NFRG) = PIM%NFRG
 470  CONTINUE    
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, 110 ) PIM%CONF%FRG_LIST(1), PIM%CONF%FRG_LIST(NFRG_OLD)
 110       FORMAT ( 'PIMA_FRG_COMBINE combined frequency groups ', I1, &
     &              ' through ', I1 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRG_COMBINE  !#!  
