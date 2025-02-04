      SUBROUTINE SPD_LOAD_MF ( SPD, L_STA, I_STA, APD_NAME, &
     &                         LAYER_HEIGHT, LAYER_FWHM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_LOAD_MF
! *                                                                      *
! *  ### 19-AUG-2014   SPD_LOAD_MF  1.0 (c)  L. Petrov  15-SEP-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  L_STA, I_STA, IUER 
      CHARACTER  APD_NAME*(*)
      TYPE     ( SPD_DEL__TYPE ) :: SPD(L_STA)
      REAL*8     LAYER_HEIGHT, LAYER_FWHM
      CHARACTER  STR*128
      REAL*8     EL_MIN, EL_MAX
      INTEGER*4  L_NOD
      PARAMETER  ( EL_MIN = 2.0D0*DEG__TO__RAD )
      PARAMETER  ( EL_MAX = P2I )
      PARAMETER  ( L_NOD  = 24  )
      REAL*8     TAI_MID
      INTEGER*4  MJD_MID, IDAY, J1, J2, J3, IER
      REAL*8,    EXTERNAL :: NMF_H, NMF_W
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      SPD(I_STA)%MF%STATUS  = SPD__UNDF
      SPD(I_STA)%MF%L_NOD   = L_NOD
      SPD(I_STA)%MF%MF_NAME = APD_NAME
!
      IF ( ASSOCIATED ( SPD(I_STA)%MF%EL_ARG ) ) THEN
           DEALLOCATE ( SPD(I_STA)%MF%EL_ARG )
      END IF
      ALLOCATE ( SPD(I_STA)%MF%EL_ARG(1:SPD(I_STA)%MF%L_NOD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG( 2441, IUER, 'SPD_LOAD_MF', 'Error in an '// &
     &         'attempt to allocate memory for array SPD(I_STA)%MF%EL_ARG' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( SPD(I_STA)%MF%MF_SPL ) ) THEN
           DEALLOCATE ( SPD(I_STA)%MF%MF_SPL )
      END IF
      ALLOCATE ( SPD(I_STA)%MF%MF_SPL(1-SPD__MDEG:SPD(I_STA)%MF%L_NOD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG( 2442, IUER, 'SPD_LOAD_MF', 'Error in an '// &
     &         'attempt to allocate memory for array SPD(I_STA)%MF%MF_SPL' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( SPD(I_STA)%MF%MF_ARG ) ) THEN
           DEALLOCATE ( SPD(I_STA)%MF%MF_ARG )
      END IF
!
      ALLOCATE ( SPD(I_STA)%MF%MF_ARG(1:SPD(I_STA)%MF%L_NOD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG( 2443, IUER, 'SPD_LOAD_MF', 'Error in an '// &
     &         'attempt to allocate memory for array SPD(I_STA)%MF%MF_ARG' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( SPD(I_STA)%MF%EL_SPL ) ) THEN
           DEALLOCATE ( SPD(I_STA)%MF%EL_SPL )
      END IF
      ALLOCATE ( SPD(I_STA)%MF%EL_SPL(1-SPD__MDEG:SPD(I_STA)%MF%L_NOD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG( 2444, IUER, 'SPD_LOAD_MF', 'Error in an '// &
     &         'attempt to allocate memory for array SPD(I_STA)%MF%EL_SPL' )
           RETURN 
      END IF
      SPD(I_STA)%MF%STATUS = SPD__ALLO
!
      IF ( APD_NAME == SPD__GL_STR  ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL SPD_GAUSSIAN_LAYER_MF ( LAYER_HEIGHT, LAYER_FWHM,  &
     &                                  EL_MIN, EL_MAX, SPD__MDEG, &
     &                                  SPD(I_STA)%MF%L_NOD,  &
     &                                  SPD(I_STA)%MF%EL_ARG, &
     &                                  SPD(I_STA)%MF%MF_SPL, &
     &                                  SPD(I_STA)%MF%MF_ARG, &
     &                                  SPD(I_STA)%MF%EL_SPL, &
     &                                  IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG( 2445, IUER, 'SPD_LOAD_MF', 'Error in '// &
     &              'an attempt to compute the coefficients of the '// &
     &              'B-spline expansion of mapping function for '// &
     &              'the Gaussian layer model' )
                RETURN 
           END IF
           SPD(I_STA)%MF%STATUS = SPD__INTR
         ELSE IF ( APD_NAME == SPD__NMFW_STR ) THEN
           DO 410 J1=1,SPD(I_STA)%MF%L_NOD
              IF ( J1 == 1 ) THEN
                   SPD(I_STA)%MF%EL_ARG(J1) = EL_MIN
                 ELSE IF ( J1 == SPD(I_STA)%MF%L_NOD ) THEN
                   SPD(I_STA)%MF%EL_ARG(J1) = EL_MAX
                 ELSE 
!
! ---------------- We use Chebyshev alternance for defining the knot sequence
!
                   SPD(I_STA)%MF%EL_ARG(J1) = EL_MAX - (1.0D0 + DCOS( (P2I*(2*J1-3))/ &
     &                                                 (SPD(I_STA)%MF%L_NOD-2) ))* &
     &                                                 (EL_MAX - EL_MIN)/2.0D0
                   SPD(I_STA)%MF%EL_SPL(J1) = SPD(I_STA)%MF%EL_ARG(J1) 
              END IF
              SPD(I_STA)%MF%MF_SPL(J1) = NMF_W ( SPD(I_STA)%STA%PHI_GCN, &
     &                                           SPD(I_STA)%MF%EL_ARG(J1) )
              SPD(I_STA)%MF%MF_ARG(J1) = SPD(I_STA)%MF%MF_SPL(J1) 
 410       CONTINUE 
!
! -------- Expand mapping function into B-spline basis
!
           CALL ERR_PASS    ( IUER, IER )
           CALL BSPL_1D_CMP ( SPD__MDEG, 0, SPD(I_STA)%MF%L_NOD, &
     &                        SPD(I_STA)%MF%EL_ARG, SPD(I_STA)%MF%MF_SPL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG( 2446, IUER, 'SPD_LOAD_MF', 'Error in '// &
     &              'an attempt to compute the coefficients of the '// &
     &              'B-spline expansion of mapping function for '// &
     &              'the NMF_W mapping function' )
                RETURN 
           END IF
!
! -------- Expand inverted mapping function into B-spline basis
!
           CALL ERR_PASS    ( IUER, IER )
           CALL BSPL_1D_CMP ( SPD__MDEG, 0, SPD(I_STA)%MF%L_NOD, &
     &                        SPD(I_STA)%MF%MF_ARG, SPD(I_STA)%MF%EL_SPL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG( 2447, IUER, 'SPD_LOAD_MF', 'Error in '// &
     &              'an attempt to compute the coefficients of the '// &
     &              'B-spline expansion of inverted mapping function '// &
     &              'for the NMF_W mapping function' )
                RETURN 
           END IF
         ELSE IF ( APD_NAME == SPD__NMFH_STR ) THEN
           MJD_MID = SPD(I_STA)%TIM%MJD_BEG
           TAI_MID = ( (SPD(I_STA)%TIM%MJD_END + SPD(I_STA)%TIM%MJD_BEG)*86400.0D0 + &
     &                 (SPD(I_STA)%TIM%TAI_END + SPD(I_STA)%TIM%TAI_BEG) )/2.0D0
           IDAY = INT(TAI_MID/86400.D0)
           MJD_MID = MJD_MID + IDAY
           TAI_MID = TAI_MID - IDAY*86400.0D0
           DO 420 J2=1,SPD(I_STA)%MF%L_NOD
              IF ( J2 == 1 ) THEN
                   SPD(I_STA)%MF%EL_ARG(J2) = EL_MIN
                 ELSE IF ( J2 == SPD(I_STA)%MF%L_NOD ) THEN
                   SPD(I_STA)%MF%EL_ARG(J2) = EL_MAX
                 ELSE 
!
! ---------------- We use Chebyshev alternance for defining the knot sequence
!
                   SPD(I_STA)%MF%EL_ARG(J2) = EL_MAX - (1.0D0 + DCOS( (P2I*(2*J2-3))/ &
     &                                                 (SPD(I_STA)%MF%L_NOD-2) ))* &
     &                                                 (EL_MAX - EL_MIN)/2.0D0
                   SPD(I_STA)%MF%EL_SPL(J2) = SPD(I_STA)%MF%EL_ARG(J2) 
              END IF
              SPD(I_STA)%MF%MF_SPL(J2) = NMF_H ( MJD_MID, TAI_MID, &
     &                                           SPD(I_STA)%STA%PHI_GCN, &
     &                                           SPD(I_STA)%STA%HEI_ELL, &
     &                                           SPD(I_STA)%MF%EL_ARG(J2) )
              SPD(I_STA)%MF%MF_ARG(J2) = SPD(I_STA)%MF%MF_SPL(J2) 
 420       CONTINUE 
!
! -------- Expand mapping function into B-spline basis
!
           CALL ERR_PASS    ( IUER, IER )
           CALL BSPL_1D_CMP ( SPD__MDEG, 0, SPD(I_STA)%MF%L_NOD, &
     &                        SPD(I_STA)%MF%EL_ARG, SPD(I_STA)%MF%MF_SPL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG( 2448, IUER, 'SPD_LOAD_MF', 'Error in '// &
     &              'an attempt to compute the coefficients of the '// &
     &              'B-spline expansion of mapping function for '// &
     &              'the NMF_H mapping function' )
                RETURN 
           END IF
!
! -------- Expand inverted mapping function into B-spline basis
!
           CALL ERR_PASS    ( IUER, IER )
           CALL BSPL_1D_CMP ( SPD__MDEG, 0, SPD(I_STA)%MF%L_NOD, &
     &                        SPD(I_STA)%MF%MF_ARG, SPD(I_STA)%MF%EL_SPL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG( 2449, IUER, 'SPD_LOAD_MF', 'Error in '// &
     &              'an attempt to compute the coefficients of the '// &
     &              'B-spline expansion of inverted mapping function '// &
     &              'for the NMF_W mapping function' )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_LOAD_MF  !#!#
