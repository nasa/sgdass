      SUBROUTINE PIMA_PC_MOD ( PIM, NPC, LCHN, IFRQ, ISTA, FREQ_PC, PHAS_PC, &
     &                         AMPL_PC, PC_GDEL, FREQ_MOD, PHAS_MOD, &
     &                         AMPL_MOD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PC_MOD  interpolate/extrapolate phase-cal phases      *
! *   to the spectral channel frequencies of uv-data.                    *
! *                                                                      *
! *  ### 12-MAY-2015  PIMA_PC_MOD  v2.2 (c)  L. Petrov  18-SEP-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IFRQ, LCHN, NPC, ISTA, IUER
      REAL*8     FREQ_PC(NPC), PHAS_PC(NPC), AMPL_PC(NPC), &
     &           FREQ_MOD(LCHN), PHAS_MOD(LCHN), AMPL_MOD(LCHN)
      REAL*4     PC_GDEL, PC_SDEL, PC_SDEL_ARR(PIM__MFRQ)
      REAL*8,    ALLOCATABLE :: FREQ_USE(:), PHAS_USE(:), AMPL_USE(:)
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-5 )
!     real*8     t8(32768), x8(32768)
      CHARACTER  STR*128, STR1*128
      LOGICAL*1  FL_1ST
      REAL*8     PHS_DIF, PH_RAT(2)
      INTEGER*4  J1, J2, J3, J4, IFIRST, ILAST, KP, ITURN, IND, LUSE, IP, &
     &           NPOLY, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8 
!
      ALLOCATE ( FREQ_USE(NPC) ) 
      ALLOCATE ( PHAS_USE(NPC) ) 
      ALLOCATE ( AMPL_USE(NPC) ) 
!
      NPOLY = 1
!
! --- Compute group delay in pcal for each individual IF separately
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_PC_SDEL ( NPC, 1, FREQ_PC, PHAS_PC, AMPL_PC, &
     &                    PC_GDEL, PC_SDEL, PC_SDEL_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3311, IUER, 'PIMA_PC_MOD', 'Failure to compute '// &
     &         'single-band group delay over phase calibration signal' )
           DEALLOCATE ( FREQ_USE ) 
           DEALLOCATE ( PHAS_USE ) 
           DEALLOCATE ( AMPL_USE ) 
           RETURN 
      END IF
!
      ILAST  = 0
      IFIRST = 0
      IP = 0
      PH_RAT = 0.0D0
!
      IF ( PIM%CONF%DEBUG_LEVEL == 10 ) THEN
           WRITE ( 6, * ) 'PIMA_PC_MODE  Sta: ', PIM%C_STA(ISTA), ' IFRQ= ', INT2(IFRQ), &
     &                     ' PC_SDEL_INP= ', SNGL(PC_GDEL), &
     &                     ' PC_SDEL_OUT= ', SNGL(PC_SDEL), ' PC_SDEL_ARR_1: ', PC_SDEL_ARR(1)
      END IF
!
      DO 410 J1=1,NPC
         IF ( AMPL_PC(J1) < PIMA__PCAL_AMP_MIN .OR. &
     &        FREQ_PC(J1) < PIMA__MIN_FRQ           ) GOTO 410
         IP = IP + 1
         FREQ_USE(IP) = FREQ_PC(J1)
         AMPL_USE(IP) = AMPL_PC(J1)
         PHAS_USE(IP) = PHAS_PC(J1) - PI2*(FREQ_USE(IP) - FREQ_PC(1))*PC_SDEL
         IF ( ILAST > 0 ) THEN
               PHAS_USE(IP) = PHAS_USE(IP) - PI2*IDNINT((PHAS_USE(IP) - PHAS_USE(ILAST))/PI2)
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL == 10 ) THEN
              WRITE ( 6, * ) 'PIMA_PC_MODE      TONE: ', INT2(J1), ' PHS_USE: ', SNGL(PHAS_USE(IP) + PI2*(FREQ_USE(IP) - FREQ_PC(1))*PC_SDEL )
              WRITE ( 6, * ) 'PIMA_PC_MODE      tone: ', INT2(J1), ' PHS_USE: ', SNGL(PHAS_USE(IP))
         END IF
         IF ( ILAST == 1 .AND. IP > ILAST ) THEN
!
! ----------- Phase rate at the left end
!
              IF ( (FREQ_USE(IP) - FREQ_USE(ILAST)) < PIMA__PCAL_AMP_MIN ) THEN
                   WRITE ( 6, * ) 'NPC = ', NPC
                   WRITE ( 6, * ) 'FREQ_PC = ', FREQ_PC(1:NPC)
                   WRITE ( 6, * ) 'AMPL_PC = ', AMPL_PC(1:NPC)
                   CALL CLRCH ( STR )
                   CALL INCH  ( IFRQ, STR )
                   CALL ERR_LOG ( 3312, IUER, 'PIMA_PC_MOD', 'Trap of internal '// &
     &                 'control: there are repeating frequencies in phase '// &
     &                 'calibration singal at station '//PIM%C_STA(ISTA)//' IF '// &
     &                 TRIM(STR)//'. This may happend either due to a bug in '// &
     &                 'difx2fits or in pima' )
                   DEALLOCATE ( FREQ_USE ) 
                   DEALLOCATE ( PHAS_USE ) 
                   DEALLOCATE ( AMPL_USE ) 
                   RETURN 
              END IF
              PH_RAT(1) = (PHAS_USE(IP) - PHAS_USE(ILAST))/(FREQ_USE(IP) - FREQ_USE(ILAST))
         END IF 
!
         IF ( ILAST > 0 .AND. IP > ILAST ) THEN
!
! ----------- Phase rate the right end
!
              PH_RAT(2) = (PHAS_USE(IP) - PHAS_USE(ILAST))/(FREQ_USE(IP) - FREQ_USE(ILAST))
         END IF
         IF ( IFIRST == 0 ) IFIRST = IP
         ILAST = IP
 410  CONTINUE 
      LUSE = IP
      IF ( NPOLY == 1 ) THEN
!
! -------- Linear extrapolation and linear interpolation 
!
           DO 420 J2=1,LCHN
              IF ( LUSE > 0 ) THEN
                   IND = IXMN8 ( LUSE, FREQ_USE, FREQ_MOD(J2) )
                ELSE
                   IND = -1
              END IF
              IF ( IND == -1 ) THEN
!
! ---------------- Linear extrapolation at the left part of the interval
!
                   IF ( IFIRST > 0 ) THEN
                        PHAS_MOD(J2) = PHAS_USE(IFIRST) + PH_RAT(1)*(FREQ_MOD(J2) - FREQ_USE(IFIRST))
                      ELSE 
!
! --------------------- This may happen if all phase cal amplitudes are too low
!
                        PHAS_MOD(J2) = PHAS_PC(1)
                   END IF
                 ELSE IF ( IND == -2 .OR. IND == LUSE ) THEN
!
! ---------------- Linear extrapolation at the right part of the interval
!
                   IF ( ILAST > 0 ) THEN
                        PHAS_MOD(J2) = PHAS_USE(ILAST)  + PH_RAT(2)*(FREQ_MOD(J2) - FREQ_USE(ILAST))
                      ELSE 
!
! --------------------- This may happen if all phase cal amplitudes are too low
!
                        PHAS_MOD(J2) = PHAS_PC(NPC)
                   END IF
                 ELSE 
!
! ---------------- Linear intrerpolation between two adjacent usable phase-cal phases
!
                   PHAS_MOD(J2) = PHAS_USE(IND) + (PHAS_USE(IND+1) - PHAS_USE(IND))/ &
          &                                       (FREQ_USE(IND+1) - FREQ_USE(IND))* &
          &                                       (FREQ_MOD(J2)    - FREQ_USE(IND))
              END IF 
              PHAS_MOD(J2) = PHAS_MOD(J2) + PI2*(FREQ_MOD(J2) - FREQ_PC(1))*PC_SDEL
 420       CONTINUE 
      END IF
!
      DEALLOCATE ( FREQ_USE ) 
      DEALLOCATE ( PHAS_USE ) 
      DEALLOCATE ( AMPL_USE ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PC_MOD  !#!#  
!
! ------------------------------------------------------------------------
!
      FUNCTION   PIMA_PC_GDEL ( KP, FREQ, PHAS, AMPL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PC_GDEL computes the group delay over all selected    *
! *   phase-cal tones in the band. The frequencies of pcal are assumed   *
! *   to be equidistant.                                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        KP ( INTEGER*4 ) -- Number of pcal tones over the band.       *
! *      FREQ ( REAL*8    ) -- Frequency array. Dimension: KP. Units: Hz.*
! *      PHAS ( REAL*8    ) -- Phase array. Dimension: KP. Units: rad.   *
! *      AMPL ( REAL*8    ) -- Amplitude array. Dimension: KP. Units: d/l*
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <PIMA_PC_GDEL> ( REAL*8    ) -- group delay. Units: s.               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  TIME_FRT ( REAL*8    ) -- Fringe reference time from the nominal    *
! *                            start of the observation.                 *
! *                            If the input value if PIMA__FRT_UNDF,     *
! *                            then the TIME_FRT is computed as the      *
! *                            weighted mean epoch counted from the      *
! *                            nominal start of the observation.         *
! *                            Otherwise, the input value is used.       *
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
! * ### 09-MAY-2015   PIMA_PC_GDEL   v3.2 (c) L. Petrov 07-DEC-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      REAL*4     PIMA_PC_GDEL
      INTEGER*4  KP, IUER
      REAL*8     FREQ(KP), PHAS(KP), AMPL(KP)
      CHARACTER  STR*128
      REAL*8       TOL_MP, PC_OVS, PCF_TOL
      PARAMETER  ( PC_OVS  =  16.0D0 )  ! Oversampling factor
      PARAMETER  ( PCF_TOL =  1.D-3 )  ! Tolerance to non-equidistant frequency sequence
      PARAMETER  ( TOL_MP  =  0.9   )  ! Tolerance factor for ??
      REAL*8     FREQ_LAST, FREQ_STEP, FREQ_DIF, MD_STEP, AMP_MAX
      COMPLEX*16, ALLOCATABLE :: PC_CMPL(:)
      INTEGER*4  L_MD_RAW, L_MD_DEG, L_MD, J1, J2, J3, J4, LP, &
     &           IND, IND_MAX, IAMB, IER 
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      PIMA_PC_GDEL = 0.0
      IF ( KP < 2 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
      LP = 0
      FREQ_LAST = 0.0
      FREQ_STEP = 0.0
!
      DO 410 J1=1,KP 
         IF ( IS_R8_NAN( AMPL(J1) ) ) AMPL(J1) = 0.0D0
         IF ( IS_R8_NAN( FREQ(J1) ) ) FREQ(J1) = 0.0D0
         IF ( FREQ(J1) > PIMA__MIN_FRQ      .AND. &
     &        AMPL(J1) > PIMA__PCAL_AMP_MIN       ) THEN
              IF ( FREQ_LAST > PIMA__MIN_FRQ ) THEN
                   IF ( FREQ_STEP > PIMA__MIN_FRQ ) THEN
                        FREQ_STEP = MIN ( FREQ(J1) - FREQ_LAST, FREQ_STEP )
                      ELSE
                        FREQ_STEP = FREQ(J1) - FREQ_LAST
                   END IF
              END IF
              FREQ_LAST = FREQ(J1) 
              LP = LP + 1
         END IF
 410  CONTINUE 
      IF ( LP < 2 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Check, whether the frequency sequence equi-distant
!
      DO 420 J2=2,LP-1
         IF ( AMPL(J2+1) > PIMA__PCAL_AMP_MIN  .AND.  FREQ(J2+1) > PIMA__MIN_FRQ .AND. &
     &        AMPL(J2)   > PIMA__PCAL_AMP_MIN  .AND.  FREQ(J2)   > PIMA__MIN_FRQ       ) THEN
              FREQ_DIF = (FREQ(J2+1) - FREQ(J2))/FREQ_STEP
              IF ( DABS(FREQ_DIF - IDNINT(FREQ_DIF)) > PCF_TOL ) THEN
                   WRITE ( 6, * ) 'LP  = ', LP
                   WRITE ( 6, * ) 'J2  = ', J2,   ' FREQ(J2)   = ', FREQ(J2)
                   WRITE ( 6, * ) 'J2+1= ', J2+1, ' FREQ(J2+1) = ', FREQ(J2+1)
                   WRITE ( 6, * ) 'FREQ_STEP = ', FREQ_STEP
                   WRITE ( 6, * ) 'FREQ_DIF  = ', FREQ_DIF
                   CALL ERR_LOG ( 7412, IUER, 'PIMA_PC_GDEL', 'Trap of internal '// &
     &                 'control: a non-eqidistant frequency step has been detected' )
                   RETURN 
              END IF
         END IF
 420  CONTINUE 
!
! --- Find the size of the FFT transform, taking into account the oversampling
! --- factor. The size should be the power of 2
!
      L_MD_RAW = (FREQ_LAST - FREQ(1))/FREQ_STEP*PC_OVS
      IF ( L_MD_RAW < 1 ) L_MD_RAW = 1
      L_MD_DEG = DLOG(1.0D0*L_MD_RAW*TOL_MP)/DLOG(2.0D0)+1
      L_MD = NINT ( 2.0D0** DFLOAT(L_MD_DEG) )
      ALLOCATE ( PC_CMPL(L_MD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 16*L_MD, STR )
           CALL ERR_LOG ( 7413, IUER, 'PIMA_PC_GDEL', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &         'PC_CMPL' )
           RETURN 
      END IF
!
! --- Fill the complex array of pcal
!
      PC_CMPL = 0.0D0
      DO 430 J3=1,LP
         IF ( AMPL(J3) > PIMA__PCAL_AMP_MIN  .AND. FREQ(J3) > PIMA__MIN_FRQ ) THEN
              IND = IDNINT( (FREQ(J3) - FREQ(1))/FREQ_STEP ) + 1
              IF ( IS_R8_NAN(PHAS(J3)) ) PHAS(J3) = 0.0D0
              IF ( IND > 0 .AND. IND .LE. L_MD ) THEN 
                   PC_CMPL(IND) = DCMPLX ( DCOS(PHAS(J3)), DSIN(PHAS(J3)) )
              END IF
         END IF
 430  CONTINUE 
!
! --- Perform direct FFT of the pcal data
!
      CALL ERR_PASS   ( IUER, IER )
      CALL FFT_1D_C16 ( L_MD, 1, PC_CMPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7413, IUER, 'PIMA_PC_GDEL', 'Error in FFT' )
           DEALLOCATE ( PC_CMPL )
           RETURN 
      END IF
!
! --- Search the maximum of amplitude
!
      MD_STEP = 1.0D0/(FREQ_STEP*L_MD)
      AMP_MAX = -1.0
      IND_MAX = -1
      DO 440 J4=1,L_MD
         IF ( ABS(PC_CMPL(J4)) > AMP_MAX ) THEN
              AMP_MAX = ABS(PC_CMPL(J4)) 
              IND_MAX = J4
         END IF
 440  CONTINUE !
!%%   write ( 6, * ) 'PIMA_PC_GDEL-326  ind_max= ', ind_max, ' l_md = ', l_md ! %%%
!
! --- Find the group delay
!
      IF ( IND_MAX .LE. L_MD/2 ) THEN
           PIMA_PC_GDEL = MD_STEP*(IND_MAX-1)
         ELSE 
           PIMA_PC_GDEL = MD_STEP*(IND_MAX-1-L_MD)
      END IF
      IAMB = IDNINT(PIMA_PC_GDEL/FREQ_STEP)
      PIMA_PC_GDEL = PIMA_PC_GDEL - IAMB*FREQ_STEP
!
! --- Deallocate memory and good bye
!
      DEALLOCATE ( PC_CMPL )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  PIMA_PC_GDEL  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_PC_SDEL ( NTON, NFRQ, FREQ, PHAS, AMPL, &
     &                          PC_INP_SDEL, PC_OUT_SDEL, PC_SDEL_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PC_SDEL computes the single-band delay over all       *
! *   selected phase-cal tones in the band. The frequencies of pcal      *
! *   are assumed to be equidistant.                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        NTON ( INTEGER*4 ) -- Number of pcal tones over the band.     *
! *        NFRQ ( INTEGER*4 ) -- Number of intermediate frequencies.     *
! *        FREQ ( REAL*8    ) -- Frequency array. Dimension: (NTON,NFRQ) *
! *                              Units: Hz.                              *
! *        PHAS ( REAL*8    ) -- Array of phase calibration phases.      *
! *                              Dimension: (NTON,NFRQ). Units: rad.     *
! *        AMPL ( REAL*8    ) -- Phase array. Dimension: (NTON,NFRQ).    *
! *                              Units: dimensionless.                   *
! * PC_INP_SDEL ( REAL*4    ) -- Input group delay in phase calibration  *
! *                              computed over each IF. It is used to    *
! *                              resolve amiguitities in group delay in  *
! *                              such a way that the output single band  *
! *                              delay in phase calibration differs with *
! *                              respect to the input group delay by     *
! *                              no more than 1/2 of the group delay     *
! *                              spacing. Dimension: NFRQ. Units s.      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * PC_OUT_SDEL ( REAL*4    ) -- Output group delay in phase calibration *
! *                              computed over each IF separately and    *
! *                              then averaged. Dimension: NFRQ.         *
! *                              Units: s.                               *
! * PC_SDEL_ARR ( REAL*4    ) -- Array of single-band delays for each    *
! *                              intermediate frequency.                 *
! *                              Dimension: NFRQ. Units: s.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  TIME_FRT ( REAL*8    ) -- Fringe reference time from the nominal    *
! *                            start of the observation.                 *
! *                            If the input value if PIMA__FRT_UNDF,     *
! *                            then the TIME_FRT is computed as the      *
! *                            weighted mean epoch counted from the      *
! *                            nominal start of the observation.         *
! *                            Otherwise, the input value is used.       *
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
! * ### 14-MAY-2015   PIMA_PC_SDEL   v3.3 (c) L. Petrov 21-SEP-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  NTON, NFRQ, IUER
      REAL*4     PC_INP_SDEL(NFRQ), PC_OUT_SDEL, PC_SDEL_ARR(NFRQ)
      REAL*8     FREQ(NTON,NFRQ), PHAS(NTON,NFRQ), AMPL(NTON,NFRQ)
      CHARACTER  STR*128
      REAL*8       TOL_MP, PC_OVS, PCF_TOL
      PARAMETER  ( PC_OVS  = 16.0D0 ) ! Oversamling factor
      PARAMETER  ( PCF_TOL = 1.D-3  ) ! Tolerance to non-equidistant frequency sequence
      PARAMETER  ( TOL_MP  = 0.9    ) ! Tolerance factor for ??
      REAL*8     PCAL_FREQ_LAST, PCAL_FREQ_STEP(PIM__MFRQ), PCAL_FREQ_STEP_ALL, &
     &           FREQ_DIF, FREQ_LOW, FREQ_HIGH, MD_STEP, AMP_MAX, PCAL_USE
      COMPLEX*16, ALLOCATABLE :: PC_CMPL(:)
      INTEGER*4  L_MD_RAW, L_MD_DEG, L_MD, J1, J2, J3, J4, J5, J6, IAMB, &
     &           IND, IND_MAX, LT(PIM__MFRQ), IND_TON(NTON,NFRQ), KFRQ, IER 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
!
      PC_OUT_SDEL = 0.0
      PC_SDEL_ARR = 0.0
      IF ( NTON < 4 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Check, whether the frequency sequence equidistant.
!
      FREQ_LOW = 0.0D0
      DO 410 J1=1,NFRQ
         PCAL_FREQ_LAST     = 0.0
         PCAL_FREQ_STEP(J1) = 0.0
         LT(J1) = 0
         DO 420 J2=1,NTON
            IF ( FREQ(J2,J1) > PIMA__MIN_FRQ  .AND.  AMPL(J2,J1) > PIMA__PCAL_AMP_MIN ) THEN
                 LT(J1) = LT(J1) + 1
                 IND_TON(LT(J1),J1) = J2
            END IF
            IF ( FREQ(J2,J1) > PIMA__MIN_FRQ  .AND.  AMPL(J2,J1) > PIMA__PCAL_AMP_MIN ) THEN
                 IF ( PCAL_FREQ_LAST > PIMA__MIN_FRQ ) THEN
                      IF ( PCAL_FREQ_STEP(J1) > PIMA__MIN_FRQ ) THEN
                           PCAL_FREQ_STEP(J1) = MIN ( FREQ(J2,J1) - PCAL_FREQ_LAST, PCAL_FREQ_STEP(J1) )
                        ELSE
                           PCAL_FREQ_STEP(J1) = FREQ(J2,J1) - PCAL_FREQ_LAST
                      END IF
                 END IF
                 PCAL_FREQ_LAST = FREQ(J2,J1) 
            END IF
 420     CONTINUE 
         DO 430 J3=NTON-1,1,-1
            IF ( FREQ(J3+1,J1) < PIMA__MIN_FRQ ) GOTO 430
            IF ( PCAL_FREQ_STEP(J1) > PIMA__MIN_FRQ .AND.  &
     &           AMPL(J3,J1)   > PIMA__PCAL_AMP_MIN .AND.  &
     &           AMPL(J3+1,J1) > PIMA__PCAL_AMP_MIN .AND.  &
     &           FREQ(J3,J1)   > PIMA__MIN_FRQ      .AND.  &
     &           FREQ(J3+1,J1) > PIMA__MIN_FRQ             ) THEN
!
                 FREQ_DIF = (FREQ(J3+1,J1) - FREQ(J3,J1))/PCAL_FREQ_STEP(J1)
                 IF ( DABS(FREQ_DIF - IDNINT(FREQ_DIF)) > PCF_TOL ) THEN
                      WRITE ( 6, * ) 'J1= ', J1, ' J3  = ', J3,   ' FREQ(J3,J1)   = ', FREQ(J3,J1)
                      WRITE ( 6, * ) 'J1= ', J1, ' J3+1= ', J3+1, ' FREQ(J3+1,J1) = ', FREQ(J3+1,J1)
                      WRITE ( 6, * ) 'FREQ(1:NTON)   = ', FREQ(1:NTON,J1) 
                      WRITE ( 6, * ) 'PCAL_FREQ_STEP = ', PCAL_FREQ_STEP(J1)
                      CALL ERR_LOG ( 7432, IUER, 'PIMA_PC_SDEL', 'Trap of internal '// &
     &                    'control: a non-eqidistant frequency step has been detected' )
                      RETURN 
                 END IF
            END IF
 430     CONTINUE 
 410  CONTINUE 
!
      KFRQ = 0
      DO 440 J4=1,NFRQ
         IF ( LT(J4) < 2  .OR.  PCAL_FREQ_STEP(J4) < PIMA__MIN_FRQ ) THEN
              PC_SDEL_ARR(J4) = 0.0
              GOTO 440
         END IF
         FREQ_LOW  = FREQ(IND_TON(     1,J4),J4)
         FREQ_HIGH = FREQ(IND_TON(LT(J4),J4),J4)
!
! ------ Find the size of the FFT transform, taking into account the oversampling
! ------ factor. The size should be the power of 2
!
         L_MD_RAW = (FREQ_HIGH - FREQ_LOW)/PCAL_FREQ_STEP(J4)*PC_OVS
         IF ( L_MD_RAW < 1 ) L_MD_RAW = 1
         L_MD_DEG = DLOG(1.0D0*L_MD_RAW*TOL_MP)/DLOG(2.0D0)+1
         L_MD = NINT ( 2.0D0** DFLOAT(L_MD_DEG) )
         ALLOCATE ( PC_CMPL(L_MD), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( 16*L_MD, STR )
              CALL ERR_LOG ( 7433, IUER, 'PIMA_PC_SDEL', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' bytes of dynamic memory for array '// &
     &            'PC_CMPL' )
              DEALLOCATE ( PC_CMPL )
              RETURN 
         END IF
!
! ------ Fill the complex array of pcal
!
         PC_CMPL = (0.0, 0.0)
         DO 450 J5=1,LT(J4)
            IND = IDNINT( (FREQ(IND_TON(J5,J4),J4) - FREQ(IND_TON(1,J4),J4))/PCAL_FREQ_STEP(J4) ) + 1
            IF ( AMPL(IND_TON(J5,J4),J4) > PIMA__PCAL_AMP_MIN .AND. &
     &           FREQ(IND_TON(J5,J4),J4) > PIMA__MIN_FRQ      .AND. &
     &           IND .GE. 1                                   .AND. &
     &           IND .LE. L_MD                                      ) THEN 
!
                 IF ( IS_R8_NAN(PHAS(J5,J4)) ) PHAS(J5,J4) = 0.0
                 IF ( IS_R8_NAN(AMPL(J5,J4)) ) AMPL(J5,J4) = 0.0
                 PCAL_USE = PHAS(IND_TON(J5,J4),J4) - PI2*PC_INP_SDEL(J4)* &
     &                                                   (FREQ(IND_TON(J5,J4),J4) - FREQ_LOW)
                 PC_CMPL(IND) = DCMPLX ( AMPL(IND_TON(J5,J4),J4)*DCOS(PCAL_USE), &
     &                                   AMPL(IND_TON(J5,J4),J4)*DSIN(PCAL_USE) )
            END IF
 450     CONTINUE 
!
! ------ Perform direct FFT of the pcal data
!
         CALL ERR_PASS   ( IUER, IER )
         CALL FFT_1D_C16 ( L_MD, 1, PC_CMPL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7413, IUER, 'PIMA_PC_SDEL', 'Error in FFT' )
              DEALLOCATE ( PC_CMPL )
              RETURN 
         END IF
!
! ------ Search the maximum of amplitude
!
         MD_STEP = 1.0D0/(PCAL_FREQ_STEP(J4)*L_MD)
         AMP_MAX = -1.0
         IND_MAX = -1
         DO 460 J6=1,L_MD
            IF ( ABS(PC_CMPL(J6)) > AMP_MAX ) THEN
                 AMP_MAX = ABS(PC_CMPL(J6)) 
                 IND_MAX = J6
            END IF
 460     CONTINUE 
!
! ------ Deallocate memory
!
         DEALLOCATE ( PC_CMPL )
!
         IF ( IND_MAX .LE. L_MD/2 ) THEN
              PC_SDEL_ARR(J4) = MD_STEP*(IND_MAX-1)
            ELSE 
              PC_SDEL_ARR(J4) = MD_STEP*(IND_MAX-1-L_MD)
         END IF
         PC_SDEL_ARR(J4) = PC_INP_SDEL(J4) + PC_SDEL_ARR(J4)
         IF ( PCAL_FREQ_STEP(J4) > PIMA__MIN_FRQ ) THEN 
              IAMB = IDNINT ( (PC_SDEL_ARR(J4) - PC_INP_SDEL(J4))/PCAL_FREQ_STEP(J4) )
              PC_SDEL_ARR(J4) = PC_SDEL_ARR(J4) - IAMB*PCAL_FREQ_STEP(J4) 
         END IF
!
         PC_OUT_SDEL = PC_OUT_SDEL + PC_SDEL_ARR(J4) 
         KFRQ = KFRQ + 1
 440  CONTINUE 
      IF ( KFRQ > 0 ) THEN
           PC_OUT_SDEL = PC_OUT_SDEL/KFRQ
         ELSE 
           PC_OUT_SDEL = 0.0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   PIMA_PC_SDEL  !#!#
