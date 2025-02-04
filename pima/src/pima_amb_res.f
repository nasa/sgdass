      SUBROUTINE PIMA_AMB_RES_R8 ( NF, FREQ, PHAS, AMPL, PHS_AVR, GR_DEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_AMB_RES_R8
! *                                                                      *
! * ### 07-OCT-2020  PIMA_AMB_RES_R8  v2.1 (c) L. Petrov 24-NOV-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  NF
      INTEGER*4    IUER 
      REAL*8     FREQ(NF), PHAS(NF), AMPL(NF), PHS_AVR, GR_DEL
      REAL*8       PC_OVS
      PARAMETER  ( PC_OVS  = 128.0D0 ) ! Oversamling factor
      REAL*8      FRQ_STEP, FREQ_1ST, MD_STEP, AMP_MAX, PHS_MOD
      CHARACTER   STR*128
      COMPLEX*16, ALLOCATABLE :: VIS_CMPL(:)
      COMPLEX*16  VIS_AVR
      INTEGER*4  J1, J2, J3, J4, J5, L_MD, IND_FRQ, IND_MAX, IAMB, NVIS, IER
      REAL*8,    EXTERNAL :: PHAS_CMPL_R8
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      PHS_AVR = 0.0D0
      GR_DEL  = 0.0D0
      FRQ_STEP = 0
      FREQ_1ST = 0
      DO 410 J1=2,NF
         IF ( IS_R8_NAN( FREQ(J1) ) .OR. IS_R8_NAN( FREQ(J1-1) ) ) GOTO 410
         IF ( FREQ(J1) > PIMA__MIN_FRQ .AND. FREQ(J1-1) > PIMA__MIN_FRQ ) THEN
              FRQ_STEP = FREQ(J1) - FREQ(J1-1) 
              IF ( FREQ_1ST < PIMA__MIN_FRQ ) THEN
                   FREQ_1ST = FREQ(J1) - (J1-1)*FRQ_STEP
              END IF
         END IF
 410  CONTINUE 
      IF ( FRQ_STEP < PIMA__MIN_FRQ/PIM__MCHN  ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      L_MD = NF*PC_OVS
      ALLOCATE ( VIS_CMPL(L_MD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 16*L_MD, STR )
           CALL ERR_LOG ( 7711, IUER, 'PIMA_AMB_RES_R8', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array VIS_CMPL' )
           RETURN 
      END IF
!
      VIS_CMPL = 0.0D0
      DO 420 J2=1,NF
         IF ( IS_R8_NAN( FREQ(J2) ) .OR. &
     &        IS_R8_NAN( AMPL(J2) ) .OR. &
     &        IS_R8_NAN( PHAS(J2) ) ) GOTO 420
!
! ------ Use Hann window
!
         VIS_CMPL(J2) = AMPL(J2)*DCMPLX ( DCOS(PHAS(J2)), DSIN(PHAS(J2)) ) * &
     &                           DSIN(PI2*(J2-1)/NF/PC_OVS)**2
 420  CONTINUE 
!
! --- Perform direct FFT of the pcal data
!
      CALL ERR_PASS   ( IUER, IER )
      CALL FFT_1D_C16 ( L_MD, 1, VIS_CMPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7712, IUER, 'PIMA_AMB_RES_R8', 'Error in FFT' )
           DEALLOCATE ( VIS_CMPL )
           RETURN 
      END IF
!
! --- Search the maximum of amplitude
!
      MD_STEP = 1.0D0/(FRQ_STEP*L_MD)
      AMP_MAX = -1.0
      IND_MAX = -1
      DO 430 J3=1,L_MD
         IF ( ABS(VIS_CMPL(J3)) > AMP_MAX ) THEN
              AMP_MAX = ABS(VIS_CMPL(J3)) 
              IND_MAX = J3
         END IF
 430  CONTINUE !
!
! --- Find the group delay
!
      IF ( IND_MAX .LE. L_MD/2 ) THEN
           GR_DEL = MD_STEP*(IND_MAX-1)
         ELSE 
           GR_DEL = MD_STEP*(IND_MAX-1-L_MD)
      END IF
!
      VIS_AVR = 0.0D0
      NVIS    = 0
      DO 440 J4=1,NF
         IF ( IS_R8_NAN( AMPL(J4) ) .OR. &
     &        IS_R8_NAN( FREQ(J4) ) .OR. &
     &        IS_R8_NAN( PHAS(J4) ) ) GOTO 440
!
         IF ( FREQ(J4) > PIMA__MIN_FRQ  .AND. &
     &        AMPL(J4) > PIMA__AMP_MIN        ) THEN
              PHS_MOD = PI2 * GR_DEL * (FREQ(J4) - FREQ_1ST)
              IND_FRQ  = IDNINT( PC_OVS*(FREQ(J4) - FREQ_1ST)/FRQ_STEP ) + 1
              IF ( IND_FRQ > 0 .AND. IND_FRQ .LE. L_MD ) THEN 
                   VIS_AVR = VIS_AVR + CMPLX( DCOS(PHAS(J4)-PHS_MOD), DSIN(PHAS(J4)-PHS_MOD) )
                   NVIS = NVIS + 1
              END IF
         END IF
 440  CONTINUE 
      IF ( NVIS > 0 ) THEN
           PHS_AVR = PHAS_CMPL_R8 ( VIS_AVR )
         ELSE
           PHS_AVR = 0.0D0
      END IF
!
      DO 450 J5=1,NF
         IF ( IS_R8_NAN( AMPL(J5) ) .OR. &
     &        IS_R8_NAN( FREQ(J5) ) .OR. &
     &        IS_R8_NAN( PHAS(J5) )      ) GOTO 450
!
         IF ( FREQ(J5) > PIMA__MIN_FRQ  .AND. &
     &        AMPL(J5) > PIMA__AMP_MIN        ) THEN
              PHS_MOD  = PI2 * GR_DEL * (FREQ(J5) - FREQ_1ST) + PHS_AVR
              IAMB     = IDNINT( (PHAS(J5) - PHS_MOD)/PI2 )
              PHAS(J5) = PHAS(J5) - PI2*IAMB
         END IF
 450  CONTINUE 
!
      DEALLOCATE ( VIS_CMPL )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_AMB_RES_R8  !#!#  
