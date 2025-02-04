#define BALF_SQ_COMP
#define BPRINT_PMN
      FUNCTION SPHE_INV_2NN_X ( FSH, MD, DEG, NORM, IPHS, SPH, N, FUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPHE_INV_2NN_X  given the spherical harmonic coefficients *
! *   SPH od degree/order MD, will evalate the function on a grid with   *
! *   an equal number of samples N latitude and 2*N in longitude.        *
! *   This is the inverse the routine SPHE_DIR_2NN, both of which are    *
! *   done quickly using FFTs for each degree of each latitude band.     *
! *   The number of samples is determined by the spherical harmonic      *
! *   bandwidth DEG may be equal or less than MD.                        *
! *                                                                      *
! *   Note that N is always EVEN for this routine.                       *
! *                                                                      *
! *   The Legendre functions are computed on the fly using the scaling   *
! *   methodolgy presented in Holmes and Featherston (2002).             *
! *   When NORM = 1, 2 or 4, these are accurate to about degree 2800.    *
! *   When NORM = 3, the routine is only stable to about degree 15!      *
! *                                                                      *
! *   The output grid contains N samples in latitude from -90 to         *
! *   90-interval, and in longitude from 0 to 360-2*interval             *
! *   (or 2N x N), where interval is the sampling interval, and          *
! *   n=2*(deg+1). 
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FSH ( SPHE_TYPE ) -- Internal data structure that keeps internal    *
! *                       arrays with intermediate results and their     *
! *                       status for possible re-use.                    *
! *   MD ( INTEGER*4 ) -- Dimension of the array for spherical function. *
! *  DEG ( INTEGER*4 ) -- Maximum degree of the transform. Should not    *
! *                       exceed MD.                                     *
! * NORM ( INTEGER*4 ) -- Normalization to be used when calculating      *
! *                       Legendre functions                             *
! *                       1 -- "geodesy";                                *
! *                       2 -- Schmidt;                                  *
! *                       3 -- unnormalized;                             *
! *                       4 -- orthonormalized;                          *
! * IPHS ( INTEGER*4 ) -- Phase flag.                                    *
! *                        1: Do not include the Condon-Shortley phase   *
! *                           factor of (-1)^m.                          *
! *                       -1: Apply the Condon-Shortley phase factor     *
! *                           of (-1)^m.                                 *
! *  SPH ( REAL*8    ) -- Array with spherical transform coefficients.   *
! *                       Dimension: (2,0:MD,0:MD). The first dimesion   *
! *                       runs over cosine/sine compoenent, the second   *
! *                       dimension runs over order l, the third         *
! *                       dimension runs over degre.                     *
! *                       NB: only coefficients l =< m are filled!       *
! *                       The part of array SPH l > m is filled with     *
! *                       zeroes.                                        *
! *    N ( INTEGER*4 ) -- Dimension of the output function along         *
! *                       latitude.                                      *
! *                                     
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  FUN ( REAL*8    ) -- The function that is to be expanded.           *
! *                       Dimension 2N*N (longitude,latitude).           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Copyright (c) 2004-2012, Mark A. Wieczorek                         *
! *   All rights reserved.                                               *
! *                                                                      *
! * ## 17-AUG-2012 SPHE_INV_2NN_X v3.0 modified by L. Petrov 15-OCT-2015 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      REAL*8     SPHE_INV_2NN_X 
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  N, MD, NORM, IPHS, DEG, IUER
      REAL*8     FUN(2*N,N), SPH(2,0:MD,0:MD)
      CHARACTER  STR*128, STR1*128
      INTEGER*4  DEG_MAX
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, I_EQ, I_S, KD, NUM_THR_SAVED, NTHR, IER
      COMPLEX*16 COEF(2*DEG+3), COEFS(2*DEG+3), TEMPC
      REAL*8     THETA, Z, U, PM1, PM2, P, PMM, PROD, &
     &           COEF0, COEF0S, TEMPR, PMM_ARR(0:DEG), ALF_SQ_R8, PUM
      INTEGER*8  PLAN
      TYPE ( X__TYPE ) :: UM_X(DEG), P_X, PM1_X, PM2_X, ALF_SQ_X
      LOGICAL*1   FL_ERROR 
      LOGICAL*4,  EXTERNAL :: PROBE_READ_ADDRESS, OMP_IN_PARALLEL
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN, &
     &                       OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!
      FUN = 0.0D0
      IF ( DEG == 0 ) THEN
           IF ( NORM == 1 .OR. NORM == 2 .OR. NORM == 3 ) THEN
                PM2 = 1.0D0
              ELSE 
                PM2 = 1.0D0/ SQRT(4.0D0*PI)
           END IF
           FUN = SPH(1,0,0)* PM2
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Compute normalization coefficients
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL SPHE_FF ( FSH, DEG, NORM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6529, IUER, 'SPHE_INV_2NN_X', 'Error in '// &
     &         'an attempt to precompute multiplicative factors used '// &
     &         ' in recursion relationships F1 and F2' )
           RETURN 
      END IF
!
! --- Evaluate the number of threads
!
      IF ( .NOT. OMP_IN_PARALLEL() ) THEN
           NUM_THR_SAVED = OMP_GET_NUM_THREADS()
           CALL OMP_SET_NUM_THREADS ( %VAL(FSH%NUM_THR) )
           NTHR = FSH%NUM_THR
         ELSE 
!
! -------- Do serial if we are already in the parallel region
!
           NTHR = 1
      END IF
!
! --- Create FFT plan
!
      CALL DFFTW_PLAN_DFT_C2R_1D ( PLAN, 2*N, COEF(1:N+1), &
     &                             FUN(1:2*N,1), FSH%FF_MODE )
      I_EQ = N/2 + 1	! Index corresponding to zero latitude
!      
!$OMP PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&         PRIVATE ( J3, J4, J5, J6, KD, THETA, Z, U, TEMPR, TEMPC, &
!$OMP&             I_S, P, PM1, PM2, COEF, COEFS, COEF0, COEF0S, &
!$OMP&             PMM_ARR, UM_X, P_X, PM1_X, PM2_X, PUM, ALF_SQ_X, ALF_SQ_R8 ), &
!$OMP&             SCHEDULE ( STATIC, NTHR )
      DO 430 J3=1,I_EQ-1 
!
! ------ J3  -- latitudinal index in the southern hemisphere, 
! ------ I_S -- the index in the northern hemisphere
!
         I_S = 2*I_EQ - J3 
	 THETA = PI* DBLE(J3-1)/DBLE(N)
         Z = COS(THETA)
         U = SIN(THETA)
!
         COEF(1:DEG+1)  = DCMPLX ( 0.0D0, 0.0D0 )
         COEFS(1:DEG+1) = DCMPLX ( 0.0D0, 0.0D0 )
         COEF0  = 0.0D0
         COEF0S = 0.0D0
         IF ( NORM == 1 .OR. NORM == 2 .OR. NORM == 3 ) THEN
              PM2 = 1.0D0
           ELSE IF ( NORM == 4 ) THEN
              PM2 = 1.0D0/ DSQRT(4.0D0*PI)
         END IF
#ifdef ALF_SQ_COMP
         CALL FP_ZERO_X ( ALF_SQ_X )
         CALL FP_XFF    ( ALF_SQ_X, PM2 )
#endif
!
         TEMPR  = PM2 * SPH(1,0,0)
	 COEF0  = COEF0  + TEMPR
	 COEF0S = COEF0S + TEMPR
!				
         PM1    = FSH%F1(1,0)* Z* PM2
         TEMPR  = PM1 * SPH(1,1,0)
         COEF0  = COEF0  + TEMPR
         COEF0S = COEF0S - TEMPR
#ifdef ALF_SQ_COMP
         CALL FP_XFF ( ALF_SQ_X, PM1 )
#endif
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) J3, 0, 0, PM2
         WRITE ( 6, 210 ) J3, 1, 0, PM1
 210     FORMAT ( 'Lat: ', I5, ' Deg: ', I5, ' Ord: ', I5, ' Pmn= ', 1PD22.15 )
 220     FORMAT ( 'Lat: ', I5, ' Deg: ', I5, ' Ord: ', I5, ' Pmn= ', 1PD22.15, 1X, I6 )
#endif
!
! ------ Calculate P(0,0) and P(1,1)
!
         IF ( NORM == 1 .OR. NORM == 2 ) THEN
              PMM_ARR(0) = SQRI(2)
           ELSE IF ( NORM == 3 ) THEN
              PMM_ARR(0) = 1.0D0
           ELSE IF ( NORM == 4 ) THEN
              PMM_ARR(0) = SQRI(2)/ SQRT(4.0D0*PI)
         END IF
         IF ( NORM == 1 .OR. NORM == 2 .OR. NORM == 4 ) THEN
              PMM_ARR(1) = IPHS* PMM_ARR(0)* SQRI(2*1+1)/ SQRI(2*1)
            ELSE IF ( NORM == 3 ) THEN
              PMM_ARR(1) = IPHS* PMM_ARR(0)* DBLE(2*1-1)
         END IF
         CALL FP_F_TO_X ( U, UM_X(1) )
!
! ------ Zonal terms
!
         CALL FP_F_TO_X ( PM1, PM1_X )
         CALL FP_F_TO_X ( PM2, PM2_X )
!
! ------ First, calculate P(m,0) terms
!
         DO 440 J4=2,DEG ! over degree
!
! --------- P: p(j4,0)
!
            CALL FP_XPA  ( UM_X(J4), U, UM_X(J4-1) )
            CALL FP_AYBZ ( FSH%F1(J4,0)*Z, PM1_X, -FSH%F2(J4,0), PM2_X, P_X )
            PM2_X = PM1_X
            PM1_X = P_X
!
            IF ( P_X%E == 0 ) THEN 
                 TEMPR = P_X%F * SPH(1,J4,0)
                 COEF0 = COEF0 + TEMPR
                 IF ( MOD(J4,2) == 0 ) THEN
                      COEF0S = COEF0S + TEMPR
                   ELSE 
                      COEF0S = COEF0S - TEMPR
                 END IF
            END IF
            IF ( NORM == 1 .OR. NORM == 4 ) THEN
                 PMM_ARR(J4) = IPHS* PMM_ARR(J4-1)* SQRI(2*J4+1)/ SQRI(2*J4)
              ELSE IF ( NORM == 2 ) THEN
                 PMM_ARR(J4) = IPHS* PMM_ARR(J4-1)* SQRI(2*J4+1)/ SQRI(2*J4)
              ELSE IF ( NORM == 3 ) THEN
                 PMM_ARR(J4) = IPHS* PMM_ARR(J4-1)* DBLE(2*J4-1)
            END IF
#ifdef ALF_SQ_COMP
            CALL FP_XZZ ( ALF_SQ_X, P_X )
#endif
#ifdef PRINT_PMN
            WRITE ( 6, 210 ) J3, J4, 0, P_X%F, P_X%E
#endif
 440     CONTINUE 
!
         DO 450 J5=1,DEG-1 ! over order
!
! --------- Sectorial (m,m) element
! --------- PM2: p(j5,j5)
!
            IF ( NORM == 1 .OR. NORM == 3 .OR. NORM == 4  ) THEN
                 PM2 = PMM_ARR(J5)
              ELSE 
                 PM2 = PMM_ARR(J5) / SQRI(2*J5+1)
            END IF
            CALL FP_F_TO_X ( PM2, PM2_X )
            CALL FP_YZ     ( PM2_X, UM_X(J5), PM2_X, PUM )
            IF ( PM2_X%E == 0 ) THEN
                 TEMPC = PM2_X%F* DCMPLX( SPH(1,J5,J5), -SPH(2,J5,J5) )
                 COEF(J5+1)  = COEF(J5+1)  + TEMPC
                 COEFS(J5+1) = COEFS(J5+1) + TEMPC
            END IF
!
! --------- Semi-sectorial (m+1,m) element
! --------- PM1: p(j5+1,j5)
!
            PM1 = Z* FSH%F1(J5+1,J5)* PM2
            CALL FP_F_TO_X ( PM1, PM1_X )
            CALL FP_YZ     ( PM1_X, UM_X(J5), PM1_X, PUM )
            IF ( PM1_X%E == 0 ) THEN
                 TEMPC = PM1_X%F* DCMPLX( SPH(1,J5+1,J5), -SPH(2,J5+1,J5))
                 COEF(J5+1)  = COEF(J5+1)  + TEMPC	
                 COEFS(J5+1) = COEFS(J5+1) - TEMPC
            END IF
#ifdef ALF_SQ_COMP
            CALL FP_XZZ    ( ALF_SQ_X, PM2_X )
            CALL FP_XZZ    ( ALF_SQ_X, PM1_X )
#endif
#ifdef PRINT_PMN
            WRITE ( 6, 210 ) J3, J5, J5,   PM2
            WRITE ( 6, 210 ) J3, J5, J5+1, PM1
#endif
!
            IF ( MOD(DEG,2) == MOD(J5,2) ) THEN
                 KD = DEG-2
               ELSE 
                 KD = DEG-1
            END IF
!
            DO 460 J6=J5+2,KD,2
!
! ------------ Compute associated Legendre function p(j6,j5)
!
               CALL FP_AYBZ ( FSH%F1(J6,J5)*Z, PM1_X, -FSH%F2(J6,J5), PM2_X, P_X )
               PM2_X = PM1_X
               PM1_X = P_X
               IF ( P_X%E == 0 ) THEN
                    TEMPC       = P_X%F* DCMPLX( SPH(1,J6,J5), -SPH(2,J6,J5) )
                    COEF(J5+1)  = COEF(J5+1)  + TEMPC
                    COEFS(J5+1) = COEFS(J5+1) + TEMPC
               END IF
#ifdef ALF_SQ_COMP
               CALL FP_XZZ    ( ALF_SQ_X, P_X )
#endif
#ifdef PRINT_PMN
               WRITE ( 6, 220 ) J3, J6,   J5, P_X%F, P_X%E
#endif
!
! ------------ Compute associated Legendre function p(j6+1,j5)
!
               CALL FP_AYBZ ( FSH%F1(J6+1,J5)*Z, PM1_X, -FSH%F2(J6+1,J5), PM2_X, P_X )
               PM2_X = PM1_X
               PM1_X = P_X
               IF ( P_X%E == 0 ) THEN
                    TEMPC       = P_X%F* DCMPLX( SPH(1,J6+1,J5), -SPH(2,J6+1,J5) )
                    COEF(J5+1)  = COEF(J5+1)  + TEMPC
                    COEFS(J5+1) = COEFS(J5+1) - TEMPC
               END IF
#ifdef ALF_SQ_COMP
               CALL FP_XZZ    ( ALF_SQ_X, P_X )
#endif
#ifdef PRINT_PMN
               WRITE ( 6, 220 ) J3, J6+1, J5, P_X%F, P_X%E
#endif
 460        CONTINUE 
            IF ( KD == DEG-2 ) THEN
!
! -------------- If j5 is odd, then we need to compute the last term
!
! -------------- Compute p: associated Legendre function p(deg,j5)
!
                 CALL FP_AYBZ ( FSH%F1(DEG,J5)*Z, PM1_X, -FSH%F2(DEG,J5), PM2_X, P_X )
                 PM2_X = PM1_X
                 PM1_X = P_X
                 IF ( P_X%E == 0 ) THEN
                      TEMPC = P_X%F* DCMPLX( SPH(1,DEG,J5), -SPH(2,DEG,J5) )
                      COEF(J5+1)  = COEF(J5+1)  + TEMPC
                      COEFS(J5+1) = COEFS(J5+1) + TEMPC
                 END IF
#ifdef ALF_SQ_COMP
                 CALL FP_XZZ    ( ALF_SQ_X, P_X )
#endif
#ifdef PRINT_PMN
                 WRITE ( 6, 220 ) J3, DEG, J5, P_X%F, P_X%E
#endif
            END IF 
 450     CONTINUE 
!
! ------ P: p(deg,deg)
!
         P = PMM_ARR(DEG) 
         IF ( UM_X(DEG)%E == 0 ) THEN
              TEMPC = P* UM_X(DEG)%F * &
     &                DCMPLX( SPH(1,DEG,DEG), -SPH(2,DEG,DEG))
              COEF(DEG+1)  = COEF(DEG+1)  + TEMPC
              COEFS(DEG+1) = COEFS(DEG+1) + TEMPC
         END IF
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) J3, DEG, DEG, P
#endif
#ifdef ALF_SQ_COMP
         CALL FP_XFF    ( ALF_SQ_X, P*FP_X_TO_F ( UM_X(DEG) ) )
!
! ------ Compute the sum of squares of associated Legendra function.
! ------ In order to prevent accuracy loss, we use the Horner scheme
! ------ for summation
!
         ALF_SQ_R8 = FP_X_TO_F ( ALF_SQ_X )
!$OMP CRITICAL
         WRITE ( 6, 110 ) N/2+1-J3, (ALF_SQ_R8 - (DEG+1)**2)/ALF_SQ_R8
!$OMP END CRITICAL
 110     FORMAT ( 'SPH_INV_2NN_X  Lat: ', I5, ' Alf_sq_err= ', 1PD25.12 )
#endif
         COEF(1) = DCMPLX ( COEF0, 0.0D0 )
         COEF(2:DEG+1) = COEF(2:DEG+1)/2.0D0
	 COEF(DEG+2:2*DEG+3) = DCMPLX( 0.0D0, 0.0D0 )	
         IF ( J3 == 1 ) THEN
!
! ----------- Special case: north pole
!
              SPHE_INV_2NN_X  = REAL(COEF(1))
            ELSE
              CALL DFFTW_EXECUTE_DFT_C2R ( PLAN, COEF, FUN(1:2*N,N+2-J3) ) ! Take Fourier transform
         END IF
         COEF(1) = DCMPLX ( COEF0S, 0.0D0 )
         COEF(2:DEG+1) = COEFS(2:DEG+1)/2.0D0
         COEF(DEG+2:2*DEG+3) = DCMPLX ( 0.0D0, 0.0D0 )
         CALL DFFTW_EXECUTE_DFT_C2R ( PLAN, COEF, FUN(1:2*N,N+2-I_S)  ) ! Take Fourier Transform
 430  CONTINUE 
!$OMP END PARALLEL DO
      IF ( .NOT. OMP_IN_PARALLEL() ) THEN
           CALL OMP_SET_NUM_THREADS ( %VAL(NUM_THR_SAVED) )
      END IF
!
! --- Finally, do equator
!
      Z = 0.0D0
      U = 1.0D0
      COEF(1:DEG+1) = DCMPLX ( 0.0D0, 0.0D0 )
      COEF0 = 0.0D0
      IF ( NORM == 1 .OR. NORM == 2 .OR. NORM == 3) THEN
           PM2 = 1.0D0
          ELSE IF ( NORM == 4 ) THEN
           PM2 = 1.0D0/ SQRT(4.0D0*PI)
      END IF
      COEF0 = COEF0 + SPH(1,0,0) * PM2
#ifdef ALF_SQ_COMP
      CALL FP_ZERO_X ( ALF_SQ_X )
      CALL FP_XFF    ( ALF_SQ_X, PM2 )
#endif
#ifdef PRINT_PMN
      WRITE ( 6, 210 ) 0, 1, 0, PM2
#endif
!
      IF ( DEG > 0 ) THEN
           DO 480 J8=2,DEG,2
              P = -FSH%F2(J8,0) * PM2
              PM2 = P
              COEF0 = COEF0 + SPH(1,J8,0)*P
#ifdef ALF_SQ_COMP
              CALL FP_XFF    ( ALF_SQ_X, P )
#endif
#ifdef PRINT_PMN
      WRITE ( 6, 210 ) 0, J8, 0, P
#endif
 480       CONTINUE 
           IF ( NORM == 1 .OR. NORM == 2 ) THEN
                PMM = SQRI(2)
             ELSE IF ( NORM == 3 ) THEN
                PMM = 1.0D0
             ELSE IF ( NORM == 4 ) THEN
                PMM = SQRI(2)/ DSQRT(4.0D0*PI)
           END IF
!
           DO 490 J9=1,DEG-1
              IF ( NORM == 1 .OR. NORM == 4 ) THEN
                   PMM = IPHS* PMM* SQRI(2*J9+1)/ SQRI(2*J9)
                   PM2 = PMM
                ELSE IF ( NORM == 2 ) THEN
                   PMM = IPHS* PMM* SQRI(2*J9+1)/ SQRI(2*J9)
                   PM2 = PMM/ SQRI(2*J9+1)
                ELSE IF ( NORM == 3 ) THEN
                   PMM = IPHS* PMM* DBLE(2*J9-1)
                   PM2 = PMM
              END IF
              COEF(J9+1) = COEF(J9+1) + PM2* DCMPLX( SPH(1,J9,J9), -SPH(2,J9,J9))
#ifdef ALF_SQ_COMP
              CALL FP_XFF  ( ALF_SQ_X, PM2 )
#endif
#ifdef PRINT_PMN
              WRITE ( 6, 210 ) 0, J9, 1, PM2
#endif
              DO 4100 J10=J9+2,DEG,2 
                 P = -FSH%F2(J10,J9)* PM2
                 PM2 = P
                 COEF(J9+1) = COEF(J9+1) + P* DCMPLX( SPH(1,J10,J9), -SPH(2,J10,J9))
#ifdef ALF_SQ_COMP
               CALL FP_XFF  ( ALF_SQ_X, P )
#endif
#ifdef PRINT_PMN
              WRITE ( 6, 210 ) 0, J9, J10, P
#endif
 4100         CONTINUE 
 490       CONTINUE 
           IF ( NORM == 1 .OR. NORM == 4 ) THEN
                PMM = IPHS* PMM* SQRI(2*DEG+1)/ SQRI(2*DEG)
              ELSE IF ( NORM == 2 ) THEN
                PMM = IPHS* PMM/ SQRI(2*DEG)
              ELSE IF ( NORM == 3 ) THEN
                PMM = IPHS* PMM* DBLE(2*DEG-1)
           END IF
!         			
           COEF(DEG+1) = COEF(DEG+1) + DCMPLX( SPH(1,DEG,DEG), -SPH(2,DEG,DEG))* PMM
           COEF(1) = DCMPLX ( COEF0, 0.0D0 )
           COEF(2:DEG+1) = COEF(2:DEG+1)/ 2.0D0
           COEF(DEG+2:2*DEG+3) = DCMPLX( 0.0D0, 0.0D0 )	
#ifdef ALF_SQ_COMP
           CALL FP_XFF  ( ALF_SQ_X, PMM )
           ALF_SQ_R8 = FP_X_TO_F ( ALF_SQ_X )
           WRITE ( 6, 110 ) 0, (ALF_SQ_R8 - (DEG+1)**2)/ALF_SQ_R8
#endif
#ifdef PRINT_PMN
           WRITE ( 6, 210 ) 0, DEG, DEG, PMM
#endif
      END IF
      CALL DFFTW_EXECUTE_DFT_C2R ( PLAN, COEF, FUN(1:2*N,I_EQ) ) ! Take Fourier transform
!
!$OMP CRITICAL
      CALL DFFTW_DESTROY_PLAN ( PLAN )
!$OMP END CRITICAL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      CONTAINS 
        INCLUDE 'x_ariphmetic.f'
      END  FUNCTION  SPHE_INV_2NN_X  !#!#
