      SUBROUTINE PIMA_ACCOR_MEAN ( PIM, IND_OBS, LTIM, ISTA, AC, WEI_1D, &
     &                             AC_MEAN, FL_MASK, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_ACCOR_MEAN computes the mean value of the             *
! *   autocorrleation. The averaging is done over time and over          *
! *   spectral channel for each IF frequency, each station of the        *
! *   baseline. Optionally autcorrlection mask can be applied.           *
! *                                                                      *
! * ### 28-SEP-2009 PIMA_ACCOR_MEAN  v5.0 (c) L. Petrov  15-NOV-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_OBS, LTIM, ISTA, IUER
      INTEGER*4  IND_STA
      CHARACTER  STR*128
      COMPLEX*8  AC(PIM%NCHN,PIM%CONF%END_FRQ-PIM%CONF%BEG_FRQ+1,LTIM)
      COMPLEX*8, ALLOCATABLE :: AC_TAV(:,:)
      INTEGER*4, ALLOCATABLE :: NCC(:,:)
      REAL*4     WEI_1D(PIM__MUV), AC_MEAN(PIM%CONF%END_FRQ-PIM%CONF%BEG_FRQ+1)
      REAL*4     ACC_AVR
      LOGICAL*1  FL_MASK, FL_ACCOR_PRINT
      INTEGER*4  J1, J2, J3, J4, J5, LFRQ, IFRQ, KTIM, NC(PIM__MCHN), IER
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL, IS_R4_NAN
!
      IND_STA = PIM%OBS(IND_OBS)%STA_IND(ISTA)
!
      FL_ACCOR_PRINT = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_ACCOR_PRINT', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_ACCOR_PRINT = .TRUE. 
      END IF
!
! --- Re-normalization of the autocorrelation function to a constant
!
      IF ( PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_VLBA_CNST1 ) THEN
           AC_MEAN = PIMA__ACCR_CNST1
           CALL ERR_LOG ( 0, IUER )
           RETURN 
         ELSE IF ( PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_VLBA_CNST2 ) THEN
           AC_MEAN = PIMA__ACCR_CNST2
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
      AC_MEAN = 0.0D0
!
      IFRQ = 0
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
      ALLOCATE ( AC_TAV(PIM%NCHN,LFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_OBS, STR )
           CALL ERR_LOG ( 8472, IUER, 'PIMA_ACCOR_MEAN', 'Failure '// &
     &         'in attempt to allocate dynamic memory for array AC_TAV' )
           RETURN 
      END IF 
!
      ALLOCATE ( NCC(PIM%NCHN,LFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_OBS, STR )
           CALL ERR_LOG ( 8472, IUER, 'PIMA_ACCOR_MEAN', 'Failure '// &
     &         'in attempt to allocate dynamic memory for array NCC' )
           RETURN 
      END IF 
!
      NCC    = 0
      AC_TAV = (0.0, 0.0)
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         NC   = 0
         IFRQ = IFRQ + 1
         KTIM = 0
!
! ------ Average autocorrelation over time
!
         DO 420 J2=1,LTIM
!
! --------- Check whether weights are OK
!
            IF ( WEI_1D(J2) < PIMA__AMP_MIN ) GOTO 420
            IF ( WEI_1D(J2) < PIM%CONF%FRIB_AUTOCORR_THRESHOLD ) GOTO 420
!
            KTIM = KTIM + 1
            DO 430 J3=1,PIM%NCHN
               IF ( ABS(AC(J3,IFRQ,J2)) > PIM%CONF%FRIB_AUTOCORR_THRESHOLD .AND. &
     &              ABS(AC(J3,IFRQ,J2)) < PIMA__ACC_MAX                          ) THEN
                    AC_TAV(J3,IFRQ) = AC_TAV(J3,IFRQ) + AC(J3,IFRQ,J2)
                    NCC(J3,IFRQ) = NCC(J3,IFRQ) + 1
               END IF
 430        CONTINUE 
 420     CONTINUE 
!
         DO 440 J4=1,PIM%NCHN
            IF ( NCC(J4,IFRQ) > 0 ) THEN
                 AC_TAV(J4,IFRQ) = AC_TAV(J4,IFRQ)/NCC(J4,IFRQ) 
            END IF
 440     CONTINUE 
!
         IF ( FL_MASK .AND. PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
!
! ----------- Apply AUTC bandpass mask if available
!
              CALL PIMA_ACCOR_APPLY_MASK ( PIM%NCHN, AC_TAV(1,IFRQ), &
     &                                     PIM%BANDPASS_MASK(1:PIM%NCHN,J1,PIM%OBS(IND_OBS)%STA_IND(ISTA),PIMA__MASK_AUTC) )
         END IF
!
         IF ( PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_MEA ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_ACCNRM_TAPER ( PIM%NCHN, PIM%NLEV(IND_STA), &
     &                                 PIM%NLEV(IND_STA), AC_TAV(1,IFRQ), &
     &                                 ACC_AVR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IND_OBS, STR )
                   CALL ERR_LOG ( 8471, IUER, 'PIMA_ACCOR_MEAN', 'Failure '// &
     &                 'in attempt to re-normalize autocorrelation for '// &
     &                 'the first station for observation '//STR )
                   RETURN 
              END IF 
            ELSE IF ( PIM%CONF%FRIB_AUTOCORR_CALIB == PIMA__ACCR_SQRT_KOG ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_ACCNRM_KOGAN ( PIM%NCHN, PIM%NLEV(IND_STA), &
     &                                 PIM%NLEV(IND_STA), AC_TAV(1,IFRQ), &
     &                                 ACC_AVR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IND_OBS, STR )
                   CALL ERR_LOG ( 8473, IUER, 'PIMA_ACCOR_MEAN', 'Failure '// &
     &                 'in attempt to re-normalize autocorrelation for '// &
     &                 'the first station for observation '//STR )
                   RETURN 
              END IF 
         END IF
!
! ------ Update AC_MEAN -- average over time and spectral channels
!
         DO 450 J5=1,PIM%NCHN
            IF ( ABS(AC_TAV(J5,IFRQ)) > PIM%CONF%FRIB_AUTOCORR_THRESHOLD .AND. &
     &           ABS(AC_TAV(J5,IFRQ)) < PIMA__ACC_MAX                          ) THEN
                 AC_MEAN(IFRQ) = AC_MEAN(IFRQ) + ABS(AC_TAV(J5,IFRQ))
                 NC(IFRQ) = NC(IFRQ) + 1
            END IF
 450     CONTINUE
!
         IF ( NC(IFRQ) > 0 ) THEN
              AC_MEAN(IFRQ) = AC_MEAN(IFRQ)/NC(IFRQ)
            ELSE 
              AC_MEAN(IFRQ) = 1.0
         END IF
         IF ( FL_ACCOR_PRINT ) THEN
              WRITE  ( 6, 210 ) PIM%C_STA(ISTA), J1, AC_MEAN(IFRQ)
 210          FORMAT ( 'PIMA_ACCOR_MEAN: Sta: ', A, ' J1= ', I3, ' AC_MEAN= ', 1PD12.5 )
         END IF
 410  CONTINUE
      DEALLOCATE ( NCC    ) 
      DEALLOCATE ( AC_TAV ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_ACCOR_MEAN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_ACCOR_APPLY_MASK ( L_CHN, AC, AC_ARR_I1 )
! ************************************************************************
! *                                                                      *
! *   Roitine  PIMA_ACCOR_APPLY_MASK 
! *                                                                      *
! * # 10-AUG-2013 PIMA_ACCOR_APPLY_MASK v1.0 (c) L. Petrov 10-AUG-2013 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  L_CHN
      COMPLEX*8  AC(L_CHN)
      INTEGER*1  AC_ARR_I1(L_CHN)
      LOGICAL*1  FL_HOLE
      INTEGER*4  J1, J2, J3, J4, IND_LAST
!
      IND_LAST = 0 
      FL_HOLE   = .FALSE.
      DO 410 J1=1,L_CHN
         IF ( AC_ARR_I1(J1) == 0 ) THEN
              FL_HOLE = .TRUE.
            ELSE
              IF ( FL_HOLE ) THEN
                   IF ( IND_LAST == 0 ) THEN
!
! --------------------- The hole is at the left edge
!
                        DO 420 J2=1,J1-1
                           AC(J2) = AC(J1)
 420                    CONTINUE 
                      ELSE
!
! --------------------- The hole is at the middle
!
                        DO 430 J3=IND_LAST+1,J1-1
                           AC(J3) = (AC(IND_LAST) + AC(J1))/2.0
 430                    CONTINUE 
                   END IF 
              END IF
              IND_LAST = J1
              FL_HOLE = .FALSE.
         END IF
 410  CONTINUE 
      IF ( FL_HOLE .AND. IND_LAST > 0 ) THEN
!
! -------- The hole is at the end
!
           DO 440 J4=IND_LAST+1,L_CHN
              AC(J4) = AC(IND_LAST)
 440       CONTINUE 
      END IF
!
      RETURN
      END  SUBROUTINE PIMA_ACCOR_APPLY_MASK   !#!#
