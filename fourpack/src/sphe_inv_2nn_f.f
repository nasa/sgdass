#define BALF_SQ_COMP
#define BPRINT_PMN
      FUNCTION SPHE_INV_2NN_F ( FSH, MD, DEG, NORM, IPHS, SPH, N, FUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPHE_INV_2NN_F  given the spherical harmonic coefficients *
! *   SPH of degree/order MD, will evalate the function on a grid with   *
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
! * ## 17-AUG-2012 SPHE_INV_2NN_F v3.0 modified by L. Petrov 15-OCT-2015 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      REAL*8     SPHE_INV_2NN_F 
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  N, MD, NORM, IPHS, DEG, IUER
      REAL*8     FUN(2*N,N), SPH(2,0:MD,0:MD)
      CHARACTER  STR*128, STR1*128
      REAL*8     SCALEF 
      INTEGER*4  DEG_MAX
      REAL*16    ALF_SQ, ALF_ORD_SQ(0:FSH__MAX_DEG)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, I_EQ, I_S, KD, NUM_THR_SAVED, NTHR, IER
      COMPLEX*16 COEF(2*DEG+3), COEFS(2*DEG+3), TEMPC
      REAL*8     THETA, Z, U, PM1, PM2, P, PMM, RESCALEM, PROD, &
     &           COEF0, COEF0S, TEMPR, PMM_ARR(0:DEG), SCL_ARR(DEG)
      INTEGER*8  PLAN
      LOGICAL*1   FL_ERROR 
      LOGICAL*4,  EXTERNAL :: PROBE_READ_ADDRESS, OMP_IN_PARALLEL
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN, &
     &                       OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!
      DEG_MAX = N/2
      SCALEF = 1.0D-280
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
      CALL ERR_PASS ( IUER, IER ) 
      CALL SPHE_FF ( FSH, DEG, NORM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6519, IUER, 'SPHE_INV_2NN_F', 'Error in '// &
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
!$OMP&         PRIVATE ( J3, J4, J5, J6, J7, KD, THETA, Z, U, TEMPR, TEMPC, &
!$OMP&             I_S, P, PM1, PM2, PMM, COEF, COEFS, COEF0, COEF0S, &
!$OMP&             SCL_ARR, PMM_ARR, ALF_SQ, ALF_ORD_SQ ), &
!$OMP&             SCHEDULE ( STATIC, NTHR )
      DO 430 J3=1,I_EQ-1 
#ifdef ALF_SQ_COMP
         ALF_ORD_SQ(0:DEG) = REAL(0.0,KIND=16)
#endif
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
!
         TEMPR  = SPH(1,0,0)* PM2
	 COEF0  = COEF0  + TEMPR
	 COEF0S = COEF0S + TEMPR
!				
         PM1    = FSH%F1(1,0)* Z* PM2
         TEMPR  = SPH(1,1,0)* PM1
         COEF0  = COEF0  + TEMPR
         COEF0S = COEF0S - TEMPR
#ifdef ALF_SQ_COMP
         ALF_ORD_SQ(0) = ALF_ORD_SQ(0) + REAL(PM2,KIND=16)**2
         ALF_ORD_SQ(0) = ALF_ORD_SQ(0) + REAL(PM1,KIND=16)**2
#endif
#ifdef PRINT_PMN
!A         WRITE ( 6, 210 ) J3, 0, 0, PM2
!A         WRITE ( 6, 210 ) J3, 1, 0, PM1
 210     FORMAT ( 'Lat: ', I5, ' Deg: ', I5, ' Ord: ', I5, ' Pmn= ', 1PD22.15 )
#endif
!
         IF ( NORM == 1 .OR. NORM == 2 ) THEN
              PMM_ARR(0) = SQRI(2)* SCALEF
           ELSE IF ( NORM == 3 ) THEN
              PMM_ARR(0) = SCALEF
           ELSE IF ( NORM == 4 ) THEN
              PMM_ARR(0) = SQRI(2)* SCALEF/ SQRT(4.0D0*PI)
         END IF
         IF ( NORM == 1 .OR. NORM == 2 .OR. NORM == 4 ) THEN
              PMM_ARR(1) = IPHS* PMM_ARR(0)* SQRI(2*1+1)/ SQRI(2*1)
            ELSE IF ( NORM == 3 ) THEN
              PMM_ARR(1) = IPHS* PMM_ARR(0)* DBLE(2*1-1)
         END IF
!				
         SCL_ARR(1) = U/ SCALEF
!
! ------ Zonal terms
!
         DO 440 J4=2,DEG
!
! --------- P: p(j4,0)
!
            P = FSH%F1(J4,0)* Z* PM1 - FSH%F2(J4,0)* PM2
            TEMPR = SPH(1,J4,0)* P
            COEF0 = COEF0 + TEMPR
            IF ( MOD(J4,2)== 0 ) THEN
                 COEF0S = COEF0S + TEMPR
               ELSE 
                 COEF0S = COEF0S - TEMPR
            END IF
            PM2 = PM1
            PM1 = P
            SCL_ARR(J4) = U* SCL_ARR(J4-1)
            IF ( NORM == 1 .OR. NORM == 4 ) THEN
                 PMM_ARR(J4) = IPHS* PMM_ARR(J4-1)* SQRI(2*J4+1)/ SQRI(2*J4)
              ELSE IF ( NORM == 2 ) THEN
                 PMM_ARR(J4) = IPHS* PMM_ARR(J4-1)* SQRI(2*J4+1)/ SQRI(2*J4)
              ELSE IF ( NORM == 3 ) THEN
                 PMM_ARR(J4) = IPHS* PMM_ARR(J4-1)* DBLE(2*J4-1)
            END IF
#ifdef ALF_SQ_COMP
            ALF_ORD_SQ(0) = ALF_ORD_SQ(0) + REAL(P,KIND=16)**2
#endif
#ifdef PRINT_PMN
!A            WRITE ( 6, 210 ) J3, J4, 0, P
#endif
 440     CONTINUE 
!
         DO 450 J5=1,DEG-1
            IF ( NORM == 1 .OR. NORM == 3 .OR. NORM == 4  ) THEN
                 PM2 = PMM_ARR(J5)
              ELSE 
                 PM2 = PMM_ARR(J5) / SQRI(2*J5+1)
            END IF
!
            TEMPC = PM2* DCMPLX( SPH(1,J5,J5), -SPH(2,J5,J5) )
!
            COEF(J5+1)  = COEF(J5+1)  + TEMPC
            COEFS(J5+1) = COEFS(J5+1) + TEMPC
!
            PM1 = Z* FSH%F1(J5+1,J5)* PM2
!	   				
            TEMPC = PM1* DCMPLX( SPH(1,J5+1,J5), -SPH(2,J5+1,J5))
            COEF(J5+1)  = COEF(J5+1)  + TEMPC	
            COEFS(J5+1) = COEFS(J5+1) - TEMPC
!
            IF ( MOD(DEG,2) == MOD(J5,2) ) THEN
                 KD = DEG-2
               ELSE 
                 KD = DEG-1
            END IF
#ifdef ALF_SQ_COMP
            ALF_ORD_SQ(J5) = ALF_ORD_SQ(J5) + REAL(PM2,KIND=16)**2
            ALF_ORD_SQ(J5) = ALF_ORD_SQ(J5) + REAL(PM1,KIND=16)**2
#endif
#ifdef PRINT_PMN
!A            WRITE ( 6, 210 ) J3, J5, J5,   PM2*SCL_ARR(J5)
!A            WRITE ( 6, 210 ) J3, J5, J5+1, PM1*SCL_ARR(J5+1)
#endif
!
            DO 460 J6=J5+2,KD,2
               P = Z* FSH%F1(J6,J5)* PM1 - FSH%F2(J6,J5)* PM2
               PM2 = PM1
               PM1 = P
               TEMPC = P* DCMPLX( SPH(1,J6,J5), -SPH(2,J6,J5) )
               COEF(J5+1)  = COEF(J5+1)  + TEMPC
               COEFS(J5+1) = COEFS(J5+1) + TEMPC
#ifdef ALF_SQ_COMP
               ALF_ORD_SQ(J5) = ALF_ORD_SQ(J5) + REAL(P,KIND=16)**2
#endif
#ifdef PRINT_PMN
!A               WRITE ( 6, 210 ) J3, J6, J5, PM1*SCL_ARR(J5)
#endif
!
               P = Z* FSH%F1(J6+1,J5)* PM1 - FSH%F2(J6+1,J5)* PM2
               PM2 = PM1
               PM1 = P
               TEMPC = P* DCMPLX( SPH(1,J6+1,J5), -SPH(2,J6+1,J5) )
               COEF(J5+1)  = COEF(J5+1)  + TEMPC
               COEFS(J5+1) = COEFS(J5+1) - TEMPC
#ifdef ALF_SQ_COMP
               ALF_ORD_SQ(J5) = ALF_ORD_SQ(J5) + REAL(P,KIND=16)**2
#endif
#ifdef PRINT_PMN
!A               WRITE ( 6, 210 ) J3, J6+1, J5, PM1*SCL_ARR(J5)
#endif
 460        CONTINUE 
            IF ( KD == DEG-2 ) THEN
                 P = Z* FSH%F1(DEG,J5)* PM1 - FSH%F2(DEG,J5)* PM2
                 PM2 = PM1
                 PM1 = P
                 TEMPC = P* DCMPLX( SPH(1,DEG,J5), -SPH(2,DEG,J5) )
                 COEF(J5+1)  = COEF(J5+1)  + TEMPC
                 COEFS(J5+1) = COEFS(J5+1) + TEMPC
#ifdef ALF_SQ_COMP
                 ALF_ORD_SQ(J5) = ALF_ORD_SQ(J5) + REAL(P,KIND=16)**2
#endif
#ifdef PRINT_PMN
!A                 WRITE ( 6, 210 ) J3, DEG, J5, P*SCL_ARR(J5)
#endif
            END IF 
!					
            COEF(J5+1)  = SCL_ARR(J5)* COEF(J5+1)
            COEFS(J5+1) = SCL_ARR(J5)* COEFS(J5+1)
 450     CONTINUE 
!
! ------ P: p(deg,deg)
!
         TEMPC = SCL_ARR(DEG)* PMM_ARR(DEG)* &
     &           DCMPLX( SPH(1,DEG,DEG), -SPH(2,DEG,DEG))
         COEF(DEG+1)  = COEF(DEG+1)  + TEMPC
         COEFS(DEG+1) = COEFS(DEG+1) + TEMPC
#ifdef PRINT_PMN
!A         WRITE ( 6, 210 ) J3, DEG, DEG, P*SCL_ARR(DEG)
#endif
#ifdef ALF_SQ_COMP
         ALF_ORD_SQ(DEG) = REAL(PMM_ARR(DEG),KIND=16)**2
!
! ------ Compute the sum of squares of associated Legendre function.
! ------ In order to prevent accuracy loss, we use the Horner scheme
! ------ for summation
!
         ALF_SQ = 0.0
         DO 470 J7=DEG,1,-1
            ALF_SQ = (ALF_SQ + ALF_ORD_SQ(J7))*(REAL(U,KIND=16))**2
 470     CONTINUE
         ALF_SQ = ALF_ORD_SQ(0) + ALF_SQ/REAL(SCALEF,KIND=16)**2
!$OMP CRITICAL
         WRITE ( 6, 110 ) N/2+1-J3, (ALF_SQ - (DEG+1)**2)/ALF_SQ
!$OMP END CRITICAL
 110     FORMAT ( 'SPH_INV_2NN_F  Lat: ', I5, ' Alf_sq_err= ', 1PD25.12 )
#endif
         COEF(1) = DCMPLX ( COEF0, 0.0D0 )
         COEF(2:DEG+1) = COEF(2:DEG+1)/2.0D0
	 COEF(DEG+2:2*DEG+3) = DCMPLX( 0.0D0, 0.0D0 )	
         IF ( J3 == 1 ) THEN
!
! ----------- Special case: north pole
!
              SPHE_INV_2NN_F  = REAL(COEF(1))
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
      ALF_SQ            = 0.0
      ALF_ORD_SQ(0:DEG) = REAL(0.0,KIND=16)
      ALF_ORD_SQ(0) = REAL(PM2,KIND=16)**2
#endif
#ifdef PRINT_PMN
      WRITE ( 6, 210 ) 0, 0, 0, PM2
#endif
!
      DO 480 J8=2,DEG,2
         P = -FSH%F2(J8,0) * PM2
         PM2 = P
         COEF0 = COEF0 + SPH(1,J8,0)*P
#ifdef ALF_SQ_COMP
         ALF_ORD_SQ(0) = ALF_ORD_SQ(0) + REAL(P,KIND=16)**2
#endif
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) 0, 0, J8, P
#endif
 480  CONTINUE 
!
      IF ( NORM == 1 .OR. NORM == 2 ) THEN
           PMM = SQRI(2)* SCALEF
        ELSE IF ( NORM == 3 ) THEN
           PMM = SCALEF
        ELSE IF ( NORM == 4 ) THEN
           PMM = SQRI(2)* SCALEF/ DSQRT(4.0D0*PI)
      END IF
      RESCALEM = 1.0D0/SCALEF
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
         ALF_SQ = ALF_SQ + REAL(PM2,KIND=16)**2
#endif
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) 0, J9, J9, PM2
#endif
         DO 4100 J10=J9+2,DEG,2 
            P = -FSH%F2(J10,J9)* PM2
            PM2 = P
            COEF(J9+1) = COEF(J9+1) + P* DCMPLX( SPH(1,J10,J9), -SPH(2,J10,J9))
#ifdef ALF_SQ_COMP
            ALF_SQ = ALF_SQ + REAL(P,KIND=16)**2
#endif
#ifdef PRINT_PMN
            WRITE ( 6, 210 ) 0, J10, J9, P
#endif
 4100    CONTINUE 
 490  CONTINUE 
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
      COEF(2:DEG+1) = COEF(2:DEG+1)* RESCALEM/ 2.0D0
      COEF(DEG+2:2*DEG+3) = DCMPLX( 0.0D0, 0.0D0 )	
#ifdef ALF_SQ_COMP
      ALF_SQ = ALF_SQ + REAL(PMM,KIND=16)**2
      ALF_SQ = ALF_ORD_SQ(0) + ALF_SQ/REAL(SCALEF,KIND=16)**2
      WRITE ( 6, 110 ) 0, (ALF_SQ - (DEG+1)**2)/ALF_SQ
#endif
#ifdef PRINT_PMN
      WRITE ( 6, 210 ) 0, DEG, DEG, PMM
#endif
!
      CALL DFFTW_EXECUTE_DFT_C2R ( PLAN, COEF, FUN(1:2*N,I_EQ) ) ! Take Fourier transform
!
!$OMP CRITICAL
      CALL DFFTW_DESTROY_PLAN ( PLAN )
!$OMP END CRITICAL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  FUNCTION  SPHE_INV_2NN_F  !#!#
