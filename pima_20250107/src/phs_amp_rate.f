      FUNCTION   PHS_AMP_RATE_R4 ( N, FRQ_R4, PHS_R4, AMP_R4, MASK_I1, IUER )
! ************************************************************************
! *                                                                      *
! *   Function PHS_AMP_RATE_R4 finds the rate of change of phase         *
! *   in the complext function represented via arrays of phases and      *
! *   amplitudes PHS, AMP using the FFT method and searching the         *
! *   maximum. Optional mask MASK_I1 array is supported.                 *
! *                                                                      *
! * ### 15-JAN-2016  PHS_AMP_RATE_R4  v1.0 (c) L. Petrov 15-JAN-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  N, IUER
      REAL*4     PHS_AMP_RATE_R4
      REAL*4     FRQ_R4(N), PHS_R4(N), AMP_R4(N)
      INTEGER*1, OPTIONAL ::  MASK_I1(N)
      REAL*8       TOL_MP, PC_OVS
      PARAMETER  ( PC_OVS = 16.0D0 )  ! Oversampling factor
      PARAMETER  ( TOL_MP = 0.9 )     ! Tolerance factor for ??
      CHARACTER  STR*128
      REAL*8     FREQ_STEP, FREQ_DIF, MD_STEP, AMP_MAX
      INTEGER*4  L_MD_RAW, L_MD_DEG, L_MD, J1, J2, J3, J4, J5, J6, &
     &           IND, IND_MAX, IER 
      COMPLEX*8, ALLOCATABLE :: SIG_CMPL(:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Get frequency step
!
      FREQ_STEP = FRQ_R4(2) - FRQ_R4(1)
      IF ( FREQ_STEP < PIMA__MIN_FRQ ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:15), FMT='(1PD15.7)' ) FREQ_STEP
           CALL ERR_LOG ( 4711, IUER, 'PHS_AMP_RATE_R4', 'Trap of internal '// &
     &         'control: too small frequency step: '//STR(1:I_LEN(STR))// &
     &         ' Hz' )
           RETURN 
      END IF
!
! --- Find the size of the FFT transform, taking into account the oversampling
! --- factor. The size should be the power of 2
!
      L_MD_RAW = (FRQ_R4(N) - FRQ_R4(1))/FREQ_STEP*PC_OVS
      IF ( L_MD_RAW < 1 ) L_MD_RAW = 1
      L_MD_DEG = DLOG(1.0D0*L_MD_RAW*TOL_MP)/DLOG(2.0D0)+1
      L_MD = NINT ( 2.0D0** DFLOAT(L_MD_DEG) )
      ALLOCATE ( SIG_CMPL(L_MD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 16*L_MD, STR )
           CALL ERR_LOG ( 4712, IUER, 'PHS_AMP_RATE_R4', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'SIG_CMPL' )
           RETURN 
      END IF
!
      SIG_CMPL = (0.0, 0.0)
      DO 410 J1=1,N
!
! ------ Fill the complex array of pcal
!
         IND = IDNINT( (FRQ_R4(J1) - FRQ_R4(1))/FREQ_STEP ) + 1
         IF ( LOC(MASK_I1) .NE. 0 ) THEN
              IF ( MASK_I1(J1) == 0 ) GOTO 410
         END IF
         SIG_CMPL(IND) = CMPLX ( AMP_R4(J1)*COS(PHS_R4(J1)), &
     &                           AMP_R4(J1)*SIN(PHS_R4(J1))  )
 410  CONTINUE 
!
! --- Perform direct FFT of the pcal data
!
      CALL ERR_PASS   ( IUER, IER )
      CALL FFT_1D_C8 ( L_MD, 1, SIG_CMPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4713, IUER, 'PHS_AMP_RATE_R4', 'Error in FFT' )
           RETURN 
      END IF
!
! --- Search the maximum of amplitude
!
      MD_STEP = 1.0D0/(FREQ_STEP*L_MD)
      AMP_MAX = -1.0
      IND_MAX = -1
      DO 420 J2=1,L_MD
         IF ( ABS(SIG_CMPL(J2)) > AMP_MAX ) THEN
              AMP_MAX = ABS(SIG_CMPL(J2)) 
              IND_MAX = J2
         END IF
 420  CONTINUE 
      IF ( IND_MAX .LE. L_MD/2 ) THEN
           PHS_AMP_RATE_R4 = MD_STEP*(IND_MAX-1)*PI2
         ELSE 
           PHS_AMP_RATE_R4 = MD_STEP*(IND_MAX-1-L_MD)*PI2
      END IF
!
! --- Deallocate memory and good bye
!
      DEALLOCATE ( SIG_CMPL )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  PHS_AMP_RATE_R4  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   PHS_AMP_RATE_R8 ( N, FRQ_R8, PHS_R8, AMP_R8, MASK_I1, IUER )
! ************************************************************************
! *                                                                      *
! *   Function PHS_AMP_RATE_R8 finds the rate of change of phase         *
! *   in the complext function represented via arrays of phases and      *
! *   amplitudes PHS, AMP using the FFT method and searching the         *
! *   maximum. Optional mask MASK_I1 array is supported.                 *
! *                                                                      *
! * ### 15-JAN-2016  PHS_AMP_RATE_R8  v1.0 (c) L. Petrov 15-JAN-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  N, IUER
      REAL*8     PHS_AMP_RATE_R8
      REAL*8     FRQ_R8(N), PHS_R8(N), AMP_R8(N)
      INTEGER*1, OPTIONAL ::  MASK_I1(N)
      REAL*8       TOL_MP, PC_OVS
      PARAMETER  ( PC_OVS = 16.0D0 )  ! Oversampling factor
      PARAMETER  ( TOL_MP = 0.9 )     ! Tolerance factor for ??
      CHARACTER  STR*128
      REAL*8     FREQ_STEP, FREQ_DIF, MD_STEP, AMP_MAX
      INTEGER*4  L_MD_RAW, L_MD_DEG, L_MD, J1, J2, J3, J4, J5, J6, &
     &           IND, IND_MAX, IER 
      COMPLEX*16, ALLOCATABLE :: SIG_CMPL(:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Get frequency step
!
      FREQ_STEP = FRQ_R8(2) - FRQ_R8(1)
      IF ( FREQ_STEP < PIMA__MIN_FRQ ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:15), FMT='(1PD15.7)' ) FREQ_STEP
           CALL ERR_LOG ( 4721, IUER, 'PHS_AMP_RATE_R8', 'Trap of internal '// &
     &         'control: too small frequency step: '//STR(1:I_LEN(STR))// &
     &         ' Hz' )
           RETURN 
      END IF
!
! --- Find the size of the FFT transform, taking into account the oversampling
! --- factor. The size should be the power of 2
!
      L_MD_RAW = (FRQ_R8(N) - FRQ_R8(1))/FREQ_STEP*PC_OVS
      IF ( L_MD_RAW < 1 ) L_MD_RAW = 1
      L_MD_DEG = DLOG(1.0D0*L_MD_RAW*TOL_MP)/DLOG(2.0D0)+1
      L_MD = NINT ( 2.0D0** DFLOAT(L_MD_DEG) )
      ALLOCATE ( SIG_CMPL(L_MD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 16*L_MD, STR )
           CALL ERR_LOG ( 4722, IUER, 'PHS_AMP_RATE_R8', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'SIG_CMPL' )
           RETURN 
      END IF
!
      SIG_CMPL = (0.0D0, 0.0D0)
      DO 410 J1=1,N
!
! ------ Fill the complex array of pcal
!
         IND = IDNINT( (FRQ_R8(J1) - FRQ_R8(1))/FREQ_STEP ) + 1
         IF ( LOC(MASK_I1) .NE. 0 ) THEN
              IF ( MASK_I1(J1) == 0 ) GOTO 410
         END IF
         SIG_CMPL(IND) = DCMPLX ( AMP_R8(J1)*DCOS(PHS_R8(J1)), &
     &                            AMP_R8(J1)*DSIN(PHS_R8(J1))  )
 410  CONTINUE 
!
! --- Perform direct FFT of the pcal data
!
      CALL ERR_PASS   ( IUER, IER )
      CALL FFT_1D_C16 ( L_MD, 1, SIG_CMPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4723, IUER, 'PHS_AMP_RATE_R8', 'Error in FFT' )
           RETURN 
      END IF
!
! --- Search the maximum of amplitude
!
      MD_STEP = 1.0D0/(FREQ_STEP*L_MD)
      AMP_MAX = -1.0
      IND_MAX = -1
      DO 420 J2=1,L_MD
         IF ( ABS(SIG_CMPL(J2)) > AMP_MAX ) THEN
              AMP_MAX = ABS(SIG_CMPL(J2)) 
              IND_MAX = J2
         END IF
 420  CONTINUE 
      IF ( IND_MAX .LE. L_MD/2 ) THEN
           PHS_AMP_RATE_R8 = MD_STEP*(IND_MAX-1)*PI2
         ELSE 
           PHS_AMP_RATE_R8 = MD_STEP*(IND_MAX-1-L_MD)*PI2
      END IF
!
! --- Deallocate memory and good bye
!
      DEALLOCATE ( SIG_CMPL )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  PHS_AMP_RATE_R8  !#
