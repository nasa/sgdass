#define BALF_SQ_COMP
#define BPRINT_PMN
      SUBROUTINE SPHE_INV_2NN_VEC_F ( FSH, MD, DEG, SPH, N, FUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPHE_INV_2NN_VEC  evaluates function FUN and its partial  *
! *   derivatives over longitude and latitude on a regular lon/lat       *
! *   grid 2N*N given its spherical harmonics transform and its          *
! *   tangential derivative.                                             *
! *                                                                      *
! *   Four-dimensional array SPH has the first dimension 1:2 that        *
! *   runs over cosine and sine components, second dimension 0:DEG       *
! *   runs over degree, third dimension 0:DEG runs over order, and       *
! *   the fourth dimension 1:2 runs over spherical harmonics and its     *
! *   tangential constituent. SPH contains components nm and mn of       *
! *   degree/order: SPH(a,c,b,d) = SPH(a,b,c,d). The so-called           *
! *   tangential constituent is the Spherical Harmonics Transform of     *
! *   a function that is in general different than FUN.                  *
! *                                                                      *
! *   The output is placed in three-dimensional array FUN defined on     *
! *   a regular longitude/latitude grid at the unit sphere.              *
! *   Its first slide is the value of the function restored from the     *
! *   spherical harmonics defined in the 1st slice of SPH. The second    *
! *   slice of FUN is the partial derivative of function defined by its  *
! *   spherical harmonics stored in the 2nd slice of SPH over the        *
! *   longitudinal direction. The third slice of FUN is the partial      *
! *   derivative of the function defined by its spherical harmonics      *
! *   stored in the 2nd slice of SPH over the latitudinal direction.     *
! *                                                                      *
! *   FUN(i,j,1) = \sum_n \sum_m SPH(b,c,1) * Y_mn                       *
! *   FUN(i,j,2) = \sum_n \sum_m SPH(b,c,2) * \d Y_mn \d long / cos\phi  *
! *   FUN(i,j,3) = \sum_n \sum_m SPH(b,c,2) * \d Y_mn \d latitude        *
! *                                                                      *
! *   This is the inverse the routine SPHE_DIR_2NN, both of which are    *
! *   done quickly using FFTs for each degree of each latitude band.     *
! *   The number of samples is determined by the spherical harmonic      *
! *   bandwidth DEG may be equal or less than MD.                        *
! *                                                                      *
! *   Note that N is always EVEN for this routine.                       *
! *                                                                      *
! *   The Legendre functions are computed on the fly using the scaling   *
! *   methodology presented in Holmes and Featherston (2002).            *
! *   Geodesy normalization for spherical harmonics us used.             *
! *                                                                      *
! *   The output grid contains N samples in latitude from -90 to         *
! *   90-interval, and in longitude from 0 to 360-1*interval             *
! *   (or 2N x N), where interval is the sampling interval, and          *
! *   n=2*(deg+1).                                                       *
! *                                                                      *
! *   Timing using E5-2660-v3 (in seconds):                              *
! *                                                                      *
! *    Lat    Deg     1-thr  16-thr                                      *
! *                                                                      *
! *    181     89     0.010   0.004                                      *
! *    257    127     0.019   0.007                                      *
! *    359    179     0.055   0.014                                      *
! *    513    255     0.120   0.017                                      *
! *    721    359     0.333   0.064                                      *
! *   1025    511     0.94    0.113                                      *
! *   1441    719     2.70    0.32                                       *
! *   2049   1023     8.13    0.89                                       *
! *   4097   2047    62.0     6.82                                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FSH ( SPHE_TYPE ) -- Internal data structure that keeps internal    *
! *                       arrays with intermediate results and their     *
! *                       status for possible re-use.                    *
! *   MD ( INTEGER*4 ) -- Dimension of the array for spherical function. *
! *  DEG ( INTEGER*4 ) -- Maximum degree of the transform. Should not    *
! *                       exceed MD.                                     *
! *  SPH ( REAL*8    ) -- Array with spherical transform coefficients.   *
! *                       Dimension: (2,0:MD,0:MD,2). The first dimesion *
! *                       runs over cosine/sine component, the second    *
! *                       dimension runs degree m, the third dimension   *
! *                       runs over order l. The fourth dimension runs   *
! *                       over 1 and 2.                                  *
! *    N ( INTEGER*4 ) -- Dimension of the output function along         *
! *                       latitude.                                      *
! *                                                                      *
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
! *   This code is based in SHTOOLS software by Mark A. Wieczorek.       *
! *                                                                      *
! * ### 11-FEB-2015 SPHE_INV_2NN_VEC_F v3.0 (c) L. Petrov 16-OCT-2015 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      REAL*8     SPHE_INV_2NN_VEC
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  N, MD, DEG, IUER
      REAL*8     FUN(2*(N-1),N,3), SPH(2,0:MD,0:MD,2)
      CHARACTER  STR*128, STR1*128
      REAL*8     SCALEF, EPS
      PARAMETER  ( EPS = 1.D-12 )
      REAL*16    ALF_SQ, ALF_ORD_SQ(0:FSH__MAX_DEG)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, I_EQ, I_S, KD, NUM_THR_SAVED, NTHR, SGN, IER
      TYPE CAR__TYPE
           COMPLEX*16 C(0:FSH__M-1,3)
           COMPLEX*16 S(0:FSH__M-1,3)
      END TYPE CAR__TYPE
      TYPE ( CAR__TYPE ) :: CAR
      COMPLEX*16 TEMPC(4)
      REAL*8     THETA, Z, U, PM1, PM2, P, PV(4), DP, &
     &           PMM, RESCALEM, PROD, COEF0C(3), COEF0S(3), &
     &           PMM_ARR(0:DEG), SCL_ARR(DEG)
      LOGICAL*1   FL_ERROR 
      LOGICAL*4,  EXTERNAL :: PROBE_READ_ADDRESS, OMP_IN_PARALLEL
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN, &
     &                        OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!
!           write ( 6, * ) 'sphe_inv_2nn_vec_f shape=', shape(fun), shape(sph) ! %%%
!           write ( 6, * ) 'sphe_inv_2nn_vec_f n = ', n, ' md= ', md, ' deg = ', deg ! %%%%
      FUN = 0.0D0
      IF ( DEG == 0 ) THEN
!
! -------- A special case of degree/order 0
!
           PM2 = 1.0D0
           FUN(1,1,1) = SPH(1,0,0,1)* PM2
           FUN(1,1,2) = 0.0D0
           FUN(1,1,3) = 0.0D0
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Store the scaling coefficient
!
      SCALEF = 1.0D-280
      RESCALEM = 1.0D0/SCALEF
!
! --- Compute normalization coefficients
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL SPHE_FF  ( FSH, DEG, 1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6569, IUER, 'SPHE_INV_2NN_VEC_F', 'Error in '// &
     &         'an attempt to precompute multiplicative factors used '// &
     &         ' in recursion relationships F1, F2, and F3' )
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
      I_EQ = (N-1)/2 + 1  ! Index corresponding to zero latitude
!      
!$OMP PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&         PRIVATE  ( J3, J4, J5, J6, J7, IER, KD, THETA, Z, U, TEMPC, &
!$OMP&                    I_S, P, PM1, PM2, PMM, PV, &
!$OMP&                    CAR, SCL_ARR, PMM_ARR, ALF_SQ, ALF_ORD_SQ ), &
!$OMP&         SCHEDULE ( STATIC )
      DO 430 J3=1,I_EQ-1 
#ifdef ALF_SQ_COMP
         ALF_ORD_SQ(0:DEG) = REAL(0.0,KIND=16)
#endif
!
! ------ J3  -- latitudinal index in the southern hemisphere, 
! ------ I_S -- the index in the northern hemisphere
!
         I_S = 2*I_EQ - J3
	 THETA = PI* DBLE(J3-1)/DBLE(N-1)
         Z = COS(THETA)
         U = SIN(THETA)
         SCL_ARR(1) = U/ SCALEF
         CALL NOUT ( SIZEOF(CAR), CAR )
!
! ------ Contribution of the (0,0) term
!
         PM2 = 1.0D0
         TEMPC(1)   = PM2 * DCMPLX ( SPH(1,0,0,1), 0.0D0 )
         CAR%C(0,1) = CAR%C(0,1) + TEMPC(1)
         CAR%S(0,1) = CAR%S(0,1) + TEMPC(1)
!
! ------ Contribution of the (1,0) term
!
         PM1         =  FSH%F1(1,0)* Z* PM2
         TEMPC(1)    =  PM1 * DCMPLX ( SPH(1,1,0,1), 0.0D0 )
         TEMPC(3)    = -FSH%F3(1,1) * PM1 * DCMPLX( SPH(1,1,1,2), -SPH(2,1,1,2) )
!
         CAR%C(0,1)  = CAR%C(0,1) + TEMPC(1)
         CAR%C(1,3)  = CAR%C(1,3) + TEMPC(3)
         CAR%S(0,1)  = CAR%S(0,1) - TEMPC(1)
         CAR%S(1,3)  = CAR%S(1,3) - TEMPC(3)
#ifdef ALF_SQ_COMP
         ALF_ORD_SQ(0) = ALF_ORD_SQ(0) + REAL(PM2,KIND=16)**2
         ALF_ORD_SQ(0) = ALF_ORD_SQ(0) + REAL(PM1,KIND=16)**2
#endif
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) J3, 0, 0, PM2
         WRITE ( 6, 210 ) J3, 1, 0, PM1
 210     FORMAT ( 'Lat: ', I5, ' Deg: ', I5, ' Ord: ', I5, ' Pmn= ', 1PD22.15 )
#endif
!
! ------ Calculate P(0,0) and P(1,1)
!
         PMM_ARR(0) = SQRI(2)* SCALEF
         PMM_ARR(1) = PMM_ARR(0)* SQRI(2*1+1)/ SQRI(2*1)
!
! ------ First, calculate P(m,m) and P(m,0) terms
!
         DO 440 J4=2,DEG ! over degree
            SCL_ARR(J4) = U* SCL_ARR(J4-1)
!
! --------- Calculate P(m,0)
!
            P = FSH%F1(J4,0)* Z* PM1 - FSH%F2(J4,0)* PM2
            TEMPC(1)   =                         P * DCMPLX ( SPH(1,J4,0,1), 0.0D0 )
            TEMPC(3)   = - FSH%F3(J4,1)/SQRI(2)* P * DCMPLX ( SPH(1,J4,1,2), -SPH(2,J4,1,2) )
!
            CAR%C(0,1) = CAR%C(0,1) + TEMPC(1)
            CAR%C(1,3) = CAR%C(1,3) + TEMPC(3)
            IF ( MOD(J4,2) == 1 ) THEN
                 TEMPC(1) = -TEMPC(1)
                 TEMPC(3) = -TEMPC(3)
            END IF
            CAR%S(0,1) = CAR%S(0,1) + TEMPC(1)
            CAR%S(1,3) = CAR%S(1,3) + TEMPC(3)
!
            PM2 = PM1
            PM1 = P
!
! --------- Calculate P(m,m)
!
            PMM_ARR(J4) = PMM_ARR(J4-1)* SQRI(2*J4+1)/ SQRI(2*J4)
#ifdef ALF_SQ_COMP
            ALF_ORD_SQ(0) = ALF_ORD_SQ(0) + REAL(P,KIND=16)**2
#endif
#ifdef PRINT_PMN
            WRITE ( 6, 210 ) J3, J4, 0, P
#endif
 440     CONTINUE 
!
         DO 450 J5=1,DEG-1 ! over order
!
! --------- Sectorial (m,m) element
! --------- PM2: p(j5,j5)
!
            PM2 = PMM_ARR(J5)
            TEMPC(1) =                             PM2 * DCMPLX( SPH(1,J5,J5,1),   -SPH(2,J5,J5,1) )
            TEMPC(2) =                        J5 * PM2 * DCMPLX( SPH(2,J5,J5,2),    SPH(1,J5,J5,2) )
            TEMPC(3) = SCL_ARR(J5)*FSH%F3(J5,J5) * PM2 * DCMPLX( SPH(1,J5,J5-1,2), -SPH(2,J5,J5-1,2) )
!
            CAR%C(J5,1)   = CAR%C(J5,1)   + TEMPC(1)
            CAR%C(J5,2)   = CAR%C(J5,2)   + TEMPC(2)
            CAR%C(J5-1,3) = CAR%C(J5-1,3) + TEMPC(3)
!
            CAR%S(J5,1)   = CAR%S(J5,1)   + TEMPC(1)
            CAR%S(J5,2)   = CAR%S(J5,2)   + TEMPC(2)
            CAR%S(J5-1,3) = CAR%S(J5-1,3) + TEMPC(3)
!
! --------- Calculate P(m+1,m)
!
            PM1 = Z* FSH%F1(J5+1,J5)* PM2
!
! --------- Contribution of (m+1,m) term
!
            TEMPC(1) =                                    PM1 * DCMPLX( SPH(1,J5+1,J5,1),   -SPH(2,J5+1,J5,1))
            TEMPC(2) =                              J5  * PM1 * DCMPLX( SPH(2,J5+1,J5,2),    SPH(1,J5+1,J5,2))
            TEMPC(3) =  FSH%F3(J5+1,J5)   * SCL_ARR(J5) * PM1 * DCMPLX( SPH(1,J5+1,J5-1,2), -SPH(2,J5+1,J5-1,2))
            TEMPC(4) = -FSH%F3(J5+1,J5+1) * SCL_ARR(J5) * PM1 * DCMPLX( SPH(1,J5+1,J5+1,2), -SPH(2,J5+1,J5+1,2))
!
            CAR%C(J5,1)   = CAR%C(J5,1)   + TEMPC(1)
            CAR%C(J5,2)   = CAR%C(J5,2)   + TEMPC(2)
            CAR%C(J5-1,3) = CAR%C(J5-1,3) + TEMPC(3)
            CAR%C(J5+1,3) = CAR%C(J5+1,3) + TEMPC(4)
!
            CAR%S(J5,1)   = CAR%S(J5,1)   - TEMPC(1)
            CAR%S(J5,2)   = CAR%S(J5,2)   - TEMPC(2)
            CAR%S(J5-1,3) = CAR%S(J5-1,3) - TEMPC(3)
            CAR%S(J5+1,3) = CAR%S(J5+1,3) - TEMPC(4)
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
            WRITE ( 6, 210 ) J3, J5, J5,   PM2*SCL_ARR(J5)
            WRITE ( 6, 210 ) J3, J5, J5+1, PM1*SCL_ARR(J5+1)
#endif
!
            IF ( DEG > 1 ) THEN
                 DO 460 J6=J5+2,KD,2 ! over degree
!
! ----------------- Contribution of (l,m) term
!
                    PV(1) = Z* FSH%F1(J6,J5)* PM1   - FSH%F2(J6,J5)* PM2
                    PM2 = PM1
                    PM1 = PV(1)
!
                    PV(2) = Z* FSH%F1(J6+1,J5)* PM1 - FSH%F2(J6+1,J5)* PM2
                    PM2 = PM1
                    PM1 = PV(2)
#ifdef ALF_SQ_COMP
                    ALF_ORD_SQ(J5) = ALF_ORD_SQ(J5) + REAL(PV(1),KIND=16)**2
                    ALF_ORD_SQ(J5) = ALF_ORD_SQ(J5) + REAL(PV(2),KIND=16)**2
#endif
#ifdef PRINT_PMN
                    WRITE ( 6, 210 ) J3, J6,   J5, PM1*SCL_ARR(J5)
                    WRITE ( 6, 210 ) J3, J6+1, J5, PM1*SCL_ARR(J5)
#endif
!
                    TEMPC(1) =    PV(1)* DCMPLX( SPH(1,J6,J5,1), -SPH(2,J6,J5,1) )
                    TEMPC(2) = J5*PV(1)* DCMPLX( SPH(2,J6,J5,2),  SPH(1,J6,J5,2) )
                    TEMPC(3) =    PV(1)* FSH%F3(J6,J5)   * SCL_ARR(J5) * DCMPLX( SPH(1,J6,J5-1,2), -SPH(2,J6,J5-1,2) )
                    TEMPC(4) =   -PV(1)* FSH%F3(J6,J5+1) * SCL_ARR(J5) * DCMPLX( SPH(1,J6,J5+1,2), -SPH(2,J6,J5+1,2) )
!
                    CAR%C(J5,1)   = CAR%C(J5,1)   + TEMPC(1)
                    CAR%C(J5,2)   = CAR%C(J5,2)   + TEMPC(2)
                    CAR%C(J5-1,3) = CAR%C(J5-1,3) + TEMPC(3)
                    CAR%C(J5+1,3) = CAR%C(J5+1,3) + TEMPC(4)
!
                    CAR%S(J5,1)   = CAR%S(J5,1)   + TEMPC(1)
                    CAR%S(J5,2)   = CAR%S(J5,2)   + TEMPC(2)
                    CAR%S(J5-1,3) = CAR%S(J5-1,3) + TEMPC(3)
                    CAR%S(J5+1,3) = CAR%S(J5+1,3) + TEMPC(4)
!
                    TEMPC(1) =     PV(2)* DCMPLX( SPH(1,J6+1,J5,1), -SPH(2,J6+1,J5,1) )
                    TEMPC(2) =  J5*PV(2)* DCMPLX( SPH(2,J6+1,J5,2),  SPH(1,J6+1,J5,2) )
                    TEMPC(3) =     PV(2)* FSH%F3(J6+1,J5)   * SCL_ARR(J5) * DCMPLX( SPH(1,J6+1,J5-1,2), -SPH(2,J6+1,J5-1,2) )
                    TEMPC(4) =    -PV(2)* FSH%F3(J6+1,J5+1) * SCL_ARR(J5) * DCMPLX( SPH(1,J6+1,J5+1,2), -SPH(2,J6+1,J5+1,2) )
!
                    CAR%C(J5,1)   = CAR%C(J5,1)   + TEMPC(1)
                    CAR%C(J5,2)   = CAR%C(J5,2)   + TEMPC(2)
                    CAR%C(J5-1,3) = CAR%C(J5-1,3) + TEMPC(3)
                    CAR%C(J5+1,3) = CAR%C(J5+1,3) + TEMPC(4)
!
                    CAR%S(J5,1)   = CAR%S(J5,1)   - TEMPC(1)
                    CAR%S(J5,2)   = CAR%S(J5,2)   - TEMPC(2)
                    CAR%S(J5-1,3) = CAR%S(J5-1,3) - TEMPC(3)
                    CAR%S(J5+1,3) = CAR%S(J5+1,3) - TEMPC(4)
 460             CONTINUE 
                 IF ( KD == DEG-2 ) THEN
                      P = Z* FSH%F1(DEG,J5)* PM1 - FSH%F2(DEG,J5)* PM2
                      PM2 = PM1
                      PM1 = P
#ifdef ALF_SQ_COMP
                      ALF_ORD_SQ(J5) = ALF_ORD_SQ(J5) + REAL(P,KIND=16)**2
#endif
#ifdef PRINT_PMN
                      WRITE ( 6, 210 ) J3, DEG, J5, P*SCL_ARR(J5)
#endif
                      TEMPC(1) =     P* DCMPLX( SPH(1,DEG,J5,1), -SPH(2,DEG,J5,1) )
                      TEMPC(2) =  J5*P* DCMPLX( SPH(2,DEG,J5,2),  SPH(1,DEG,J5,2) )
                      TEMPC(3) =     P* FSH%F3(DEG,J5)   * SCL_ARR(J5) * DCMPLX( SPH(1,DEG,J5-1,2), -SPH(2,DEG,J5-1,2) )
                      TEMPC(4) =    -P* FSH%F3(DEG,J5+1) * SCL_ARR(J5) * DCMPLX( SPH(1,DEG,J5+1,2), -SPH(2,DEG,J5+1,2) )
!
                      CAR%C(J5,1)   = CAR%C(J5,1)   + TEMPC(1)
                      CAR%S(J5,1)   = CAR%S(J5,1)   + TEMPC(1)
!
                      CAR%C(J5,2)   = CAR%C(J5,2)   + TEMPC(2)
                      CAR%S(J5,2)   = CAR%S(J5,2)   + TEMPC(2)
!
                      CAR%C(J5-1,3) = CAR%C(J5-1,3) + TEMPC(3)
                      CAR%S(J5-1,3) = CAR%S(J5-1,3) + TEMPC(3)
!
                      CAR%C(J5+1,3) = CAR%C(J5+1,3) + TEMPC(4)
                      CAR%S(J5+1,3) = CAR%S(J5+1,3) + TEMPC(4)
                 END IF 
            END IF
!					
            CAR%C(J5,1:2) = SCL_ARR(J5)* CAR%C(J5,1:2) 
            CAR%S(J5,1:2) = SCL_ARR(J5)* CAR%S(J5,1:2) 
 450     CONTINUE 
!
! ------ Contribution of (deg,deg) term
!
         TEMPC(1) =     SCL_ARR(DEG)* PMM_ARR(DEG)* &
     &                  DCMPLX( SPH(1,DEG,DEG,1),   -SPH(2,DEG,DEG,1) )
         TEMPC(2) = DEG*SCL_ARR(DEG)* PMM_ARR(DEG)* &
     &                  DCMPLX( SPH(2,DEG,DEG,2),    SPH(1,DEG,DEG,2) )
         TEMPC(3) =     SCL_ARR(DEG)* PMM_ARR(DEG)* FSH%F3(DEG,DEG) * &
     &                  DCMPLX( SPH(1,DEG,DEG-1,2), -SPH(2,DEG,DEG-1,2) )
!
         CAR%C(DEG,1)   = CAR%C(DEG,1)   + TEMPC(1)
         CAR%C(DEG,2)   = CAR%C(DEG,2)   + TEMPC(2)
         CAR%C(DEG-1,3) = CAR%C(DEG-1,3) + TEMPC(3)
!
         CAR%S(DEG,1)   = CAR%S(DEG,1)   + TEMPC(1)
         CAR%S(DEG,2)   = CAR%S(DEG,2)   + TEMPC(2)
         CAR%S(DEG-1,3) = CAR%S(DEG-1,3) + TEMPC(3)
!
         CAR%C(1:DEG,1:3) = CAR%C(1:DEG,1:3)/2.0D0
         CAR%S(1:DEG,1:3) = CAR%S(1:DEG,1:3)/2.0D0
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) J3, DEG, DEG, P*SCL_ARR(DEG)
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
!
! ------ Now we run Fourier transform
!
         IER =0 
         CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%C(0,1), FUN(1:2*(N-1),N+1-J3,1), IER )
         CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%C(0,2), FUN(1:2*(N-1),N+1-J3,2), IER )
         CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%C(0,3), FUN(1:2*(N-1),N+1-J3,3), IER )
!
         CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%S(0,1), FUN(1:2*(N-1),J3,1), IER )
         CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%S(0,2), FUN(1:2*(N-1),J3,2), IER )
         CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%S(0,3), FUN(1:2*(N-1),J3,3), IER )
!
         IF ( DABS(U) > EPS ) THEN
              FUN(1:2*(N-1),N+1-J3,2) = FUN(1:2*(N-1),N+1-J3,2) /U
              FUN(1:2*(N-1),J3,2)     = FUN(1:2*(N-1),J3,2)     /U
         END IF
 430  CONTINUE 
!$OMP END PARALLEL DO
      IF ( .NOT. OMP_IN_PARALLEL() ) THEN
           CALL OMP_SET_NUM_THREADS ( %VAL(NUM_THR_SAVED) )
      END IF
!
! --- Finally, do equator
!     ===================
!
      Z = 0.0D0
      U = 1.0D0
!
      CALL NOUT ( SIZEOF(CAR), CAR )
      PM2 = 1.0D0
!
! --- Contribution of (0,0) term
!
      CAR%C(0,1) = CAR%C(0,1) + SPH(1,0,0,1) * PM2
#ifdef ALF_SQ_COMP
      ALF_SQ            = 0.0
      ALF_ORD_SQ(0:DEG) = REAL(0.0,KIND=16)
      ALF_ORD_SQ(0) = REAL(PM2,KIND=16)**2
#endif
#ifdef PRINT_PMN
      WRITE ( 6, 210 ) 0, 1, 0, PM2
#endif
!
      DO 480 J8=2,DEG,2 ! degree
         P = -FSH%F2(J8,0) * PM2
         PM2 = P
!
! ------ Contribution of (0,j8) term
!
         CAR%C(0,1) = CAR%C(0,1) + SPH(1,J8,0,1)*P
         CAR%C(1,3) = CAR%C(1,3) - FSH%F3(J8,1)/SQRI(2)*P*SCALEF * &
     &                             DCMPLX( SPH(1,J8,1,2), -SPH(2,J8,1,2) )
#ifdef ALF_SQ_COMP
         ALF_ORD_SQ(0) = ALF_ORD_SQ(0) + REAL(P,KIND=16)**2
#endif
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) 0, 0, J8, P
#endif
 480  CONTINUE 
!
      PMM = SQRI(2)* SCALEF
!
      DO 490 J9=1,DEG-1  ! order
         PMM = PMM* SQRI(2*J9+1)/ SQRI(2*J9)
         PM2 = PMM
!
! ------ Contribution of (m,m) term
!
         TEMPC(1) =    PM2* DCMPLX( SPH(1,J9,J9,1), -SPH(2,J9,J9,1) )
         TEMPC(2) = J9*PM2* DCMPLX( SPH(2,J9,J9,2),  SPH(1,J9,J9,2) )
         TEMPC(3) = FSH%F3(J9,J9)* PM2* DCMPLX( SPH(1,J9,J9-1,2), -SPH(2,J9,J9-1,2) )
!
         CAR%C(J9,1)   = CAR%C(J9,1)   + TEMPC(1)
         CAR%C(J9,2)   = CAR%C(J9,2)   + TEMPC(2)
         CAR%C(J9-1,3) = CAR%C(J9-1,3) + TEMPC(3)
!
#ifdef ALF_SQ_COMP
         ALF_SQ = ALF_SQ + REAL(PM2,KIND=16)**2
#endif
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) 0, J9, J9, PM2
#endif
         IF ( DEG > 1 ) THEN
              DO 4100 J10=J9+2,DEG,2 ! J10 -- degree; J9 -- order
                 P = -FSH%F2(J10,J9)* PM2
                 PM2 = P
!
! -------------- Contribution of (l,m) term
!
                 TEMPC(1) =                      P * DCMPLX( SPH(1,J10,J9,1),   -SPH(2,J10,J9,1)   )
                 TEMPC(2) =                  J9* P * DCMPLX( SPH(2,J10,J9,2),    SPH(1,J10,J9,2)   )
                 TEMPC(3) =   FSH%F3(J10,J9)   * P * DCMPLX( SPH(1,J10,J9-1,2), -SPH(2,J10,J9-1,2) )
                 TEMPC(4) = - FSH%F3(J10,J9+1) * P * DCMPLX( SPH(1,J10,J9+1,2), -SPH(2,J10,J9+1,2) )
!
                 CAR%C(J9,1)   = CAR%C(J9,1)   + TEMPC(1)
                 CAR%C(J9,2)   = CAR%C(J9,2)   + TEMPC(2)
                 CAR%C(J9-1,3) = CAR%C(J9-1,3) + TEMPC(3)
                 CAR%C(J9+1,3) = CAR%C(J9+1,3) + TEMPC(4)
#ifdef ALF_SQ_COMP
                 ALF_SQ = ALF_SQ + REAL(P,KIND=16)**2
#endif
#ifdef PRINT_PMN
                 WRITE ( 6, 210 ) 0, J10, J9, P
#endif
 4100         CONTINUE 
         END IF
 490  CONTINUE 
      PMM = PMM* SQRI(2*DEG+1)/ SQRI(2*DEG)
#ifdef ALF_SQ_COMP
      ALF_SQ = ALF_SQ + REAL(PMM,KIND=16)**2
      ALF_SQ = ALF_ORD_SQ(0) + ALF_SQ/REAL(SCALEF,KIND=16)**2
      WRITE ( 6, 110 ) 0, (ALF_SQ - (DEG+1)**2)/ALF_SQ
#endif
#ifdef PRINT_PMN
      WRITE ( 6, 210 ) 0, DEG, DEG, PMM
#endif
!
! --- Contribution of (deg,deg) term
!
      CAR%C(DEG,1)   = CAR%C(DEG,1)   +                   PMM * DCMPLX( SPH(1,DEG,DEG,1),   -SPH(2,DEG,DEG,1) )
      CAR%C(DEG,2)   = CAR%C(DEG,2)   +               DEG*PMM * DCMPLX( SPH(2,DEG,DEG,2),    SPH(1,DEG,DEG,2) )
      CAR%C(DEG-1,3) = CAR%C(DEG-1,3) + FSH%F3(DEG,DEG) * PMM * DCMPLX( SPH(1,DEG,DEG-1,2), -SPH(2,DEG,DEG-1,2) )
!
      CAR%C(1:DEG,1:2) = CAR%C(1:DEG,1:2) * RESCALEM/2.0D0
      CAR%C(0:DEG,3:3) = CAR%C(0:DEG,3:3) * RESCALEM
      CAR%C(1:DEG,3:3) = CAR%C(1:DEG,3:3)/2.0D0
      CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%C(0,1), FUN(1:2*(N-1),I_EQ,1), IER )
      CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%C(0,2), FUN(1:2*(N-1),I_EQ,2), IER )
      CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%C(0,3), FUN(1:2*(N-1),I_EQ,3), IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  SPHE_INV_2NN_VEC_F  !#!#
