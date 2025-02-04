      SUBROUTINE PIMA_DRF_MAX_ARR ( LCHN, LFRQ, LTIM, FREQ_ARR, GRAMBSP, &
     &                              TIM_ARR, WEI_THR, TIME_FRT, PH_RAT, &
     &                              GR_DEL, GR_RAT, FREQ_REF, &
     &                              CFRQ_REF, WEI, UV, M_SCM, &
     &                              GRD_SEC_MAX_ARR, AMP_SEC_MAX_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_DRF_MAX_ARR 
! *                                                                      *
! * ### 02-MAR-2010  PIMA_DRF_MAX_ARR v1.0 (c) L. Petrov 12-MAR-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  LCHN, LFRQ, LTIM, M_SCM, IUER
      REAL*8     FREQ_ARR(LCHN,LFRQ), GRAMBSP, TIM_ARR(LTIM), WEI_THR, &
     &           TIME_FRT, PH_RAT, GR_DEL, GR_RAT, FREQ_REF, &
     &           CFRQ_REF(LCHN,LFRQ), GRD_SEC_MAX_ARR(M_SCM), &
     &           AMP_SEC_MAX_ARR(M_SCM)
      REAL*4     WEI(LTIM)
      COMPLEX*8  UV(LCHN,LFRQ,LTIM)
      REAL*8     GRD_MAR, GRD_MIN_STP
      PARAMETER  ( GRD_MAR     = 1.4D0  )
      PARAMETER  ( GRD_MIN_STP = 0.3D-9 )
      COMPLEX*8  DRF, DRF_IF(PIM__MFRQ)
      REAL*8     GRD_MIN, GRD_MAX, GRD_STP, AMP_MAX
      REAL*8,    ALLOCATABLE :: GRD_ARR(:), AMP_ARR(:), &
     &                          AMP_ARR_MAX(:), GRD_ARR_MAX(:)
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, LARR, KARR, LOG_LARR, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Compute the number of points in the DRF: LARR
!
      IF ( M_SCM .LE. 8 ) THEN
           LOG_LARR = IDINT(DLOG(2*GRD_MAR*GRAMBSP/GRD_MIN_STP)/DLOG(2.0D0)) + 1
         ELSE 
           LOG_LARR = IDINT(DLOG(4*GRD_MAR*GRAMBSP/GRD_MIN_STP)/DLOG(2.0D0)) + 1
      END IF
      LARR     = IDNINT ( 2.0D0**LOG_LARR )
!
      ALLOCATE ( GRD_ARR(LARR),     STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LFRQ, STR )
           CALL ERR_LOG ( 7692, IUER, 'PIMA_DRF_MAX_ARR', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for temporary array GRD_ARR' )
           RETURN 
      END IF
      ALLOCATE ( AMP_ARR(LARR),     STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LFRQ, STR )
           CALL ERR_LOG ( 7693, IUER, 'PIMA_DRF_MAX_ARR', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for temporary array AMP_ARR' )
           RETURN 
      END IF
      ALLOCATE ( AMP_ARR_MAX(LARR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LFRQ, STR )
           CALL ERR_LOG ( 7694, IUER, 'PIMA_DRF_MAX_ARR', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for temporary array AMP_ARR_MAX' )
           RETURN 
      END IF
      ALLOCATE ( GRD_ARR_MAX(LARR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*LFRQ, STR )
           CALL ERR_LOG ( 7695, IUER, 'PIMA_DRF_MAX_ARR', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory '// &
     &         'for temporary array GRD_ARR_MAX' )
           RETURN 

      END IF
!
      IF ( M_SCM .LE. 8 ) THEN
           GRD_MIN = -GRAMBSP*GRD_MAR + GR_DEL 
           GRD_MAX =  GRAMBSP*GRD_MAR + GR_DEL 
         ELSE 
           GRD_MIN = -2.0*GRAMBSP*GRD_MAR + GR_DEL 
           GRD_MAX =  2.0*GRAMBSP*GRD_MAR + GR_DEL 
      END IF
      GRD_STP = (GRD_MAX - GRD_MIN)/(LARR-1)
      KARR = 0
!
! --- Compute the DRF at LARR points
!
!$OMP PARALLEL
!$OMP DO PRIVATE ( J1, DRF, DRF_IF )
      DO 410 J1=1,LARR
         GRD_ARR(J1) = (GRD_MIN + (J1-1)*GRD_STP)
!
! ------ Compute the DRF for group delay GRD_ARR(J1)
!
         CALL PIMA_UV_DRF3 ( PIMA__GRAT, .TRUE., LTIM, LCHN, LFRQ, WEI, TIM_ARR, &
     &                       WEI_THR, TIME_FRT, PH_RAT, GRD_ARR(J1), &
     &                       GR_RAT, 0.0D0, FREQ_ARR, FREQ_REF, &
     &                       CFRQ_REF, UV, DRF, DRF_IF )
         AMP_ARR(J1) = ABS(DRF)
 410  CONTINUE 
!$OMP END DO
!$OMP END PARALLEL
!
! --- Search for local extrema
!
      DO 420 J2=1,LARR
         IF ( J2 > 2 ) THEN
!
! ----------- Check, whether the first derivative (in the form of the 
! ----------- first difference) crossed the zero. If yes, this the 
! ----------- local extremim
!
              IF ( ( AMP_ARR(J2)   - AMP_ARR(J2-1) .LE. 0.0D0 .AND. &
     &               AMP_ARR(J2-1) - AMP_ARR(J2-2) .GE. 0.0D0       )    ) THEN
!
! ---------------- Yes, this is the local extremum. Add it to the array of 
! ---------------- extrema. NB: we put the amplitude with reversed sign.
! ---------------- This is done for consecutive sorting
!
                   KARR = KARR + 1
                   IF ( AMP_ARR(J2) .GE. AMP_ARR(J2-1) .AND. &
     &                  AMP_ARR(J2) .GE. AMP_ARR(J2-2)       ) THEN
!
! --------------------- The current point has the largest amplitude
!
                        AMP_ARR_MAX(KARR) = -AMP_ARR(J2)
                        GRD_ARR_MAX(KARR) =  GRD_ARR(J2)
                      ELSE IF ( AMP_ARR(J2-1) .GE. AMP_ARR(J2)   .AND. &
     &                          AMP_ARR(J2-1) .GE. AMP_ARR(J2-2)       ) THEN
!
! --------------------- The previous point has the largest amplitude
!
                        AMP_ARR_MAX(KARR) = -AMP_ARR(J2-1)
                        GRD_ARR_MAX(KARR) =  GRD_ARR(J2-1)
                      ELSE 
!
! --------------------- The previous-previous point has the largest
! --------------------- amplitude
!
                        AMP_ARR_MAX(KARR) = -AMP_ARR(J2-2)
                        GRD_ARR_MAX(KARR) =  GRD_ARR(J2-2)
                   END IF
              END IF
         END IF
 420  CONTINUE 
!
! --- Sort arrays of amplitude maxima and associated group delays 
!
      CALL SORT8 ( KARR, AMP_ARR_MAX, GRD_ARR_MAX )
      AMP_MAX = -AMP_ARR_MAX(1)
      IF ( AMP_MAX > -PIMA__AMP_MIN ) THEN
           AMP_MAX = -PIMA__AMP_MIN 
      END IF
!
! --- Normalize the amplitude to the amplitude of the global maximum
!
      DO 430 J3=1,M_SCM
         IF ( J3 .LE. KARR ) THEN
              AMP_SEC_MAX_ARR(J3) = -AMP_ARR_MAX(J3)/AMP_MAX 
              GRD_SEC_MAX_ARR(J3) =  GRD_ARR_MAX(J3)
            ELSE 
              AMP_SEC_MAX_ARR(J3) = -AMP_ARR_MAX(KARR)/AMP_MAX 
              GRD_SEC_MAX_ARR(J3) =  GRD_ARR_MAX(KARR)
         END IF
 430  CONTINUE 
      DEALLOCATE ( GRD_ARR )
      DEALLOCATE ( AMP_ARR )
      DEALLOCATE ( AMP_ARR_MAX )
      DEALLOCATE ( GRD_ARR_MAX )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_DRF_MAX_ARR  !#!  
