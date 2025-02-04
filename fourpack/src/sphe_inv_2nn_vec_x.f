#define BALF_SQ_COMP
#define BPRINT_PMN
      SUBROUTINE SPHE_INV_2NN_VEC_X ( FSH, MD, DEG, SPH, N, FUN, IUER )
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
! * ### 11-FEB-2015 SPHE_INV_2NN_VEC_X v3.1 (c) L. Petrov 22-APR-2016 ## *
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
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-12 )
      REAL*16    ALF_SQ, ALF_ORD_SQ(0:FSH__MAX_DEG)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, I_EQ, I_S, KD, NUM_THR_SAVED, NTHR, SGN, IER
      TYPE CAR__TYPE
           COMPLEX*16 C(0:FSH__M-1,3)
           COMPLEX*16 S(0:FSH__M-1,3)
      END TYPE CAR__TYPE
      TYPE ( CAR__TYPE ) :: CAR
      TYPE ( X__TYPE   ) :: UM_X(FSH__MAX_DEG), P_X, PM1_X, PM2_X, PV_X(2), ALF_SQ_X
      COMPLEX*16 TEMPC(4)
      REAL*8     THETA, Z, U, PM1, PM2, P, PV(4), DP, &
     &           PMM, PROD, PMM_ARR(0:DEG), ALF_SQ_R8, PUM
      LOGICAL*1   FL_ERROR 
      LOGICAL*4,  EXTERNAL :: PROBE_READ_ADDRESS, OMP_IN_PARALLEL
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN, &
     &                        OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!
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
! --- Compute normalization coefficients
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL SPHE_FF  ( FSH, DEG, 1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6579, IUER, 'SPHE_INV_2NN_VEC_X', 'Error in '// &
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
!$OMP&                    I_S, P, PM1, PM2, PMM, CAR, PMM_ARR, &
!$OMP&                    PV_X, UM_X, P_X, PM1_X, PM2_X, &
!$OMP&                    PUM, ALF_SQ_X, ALF_SQ_R8 ), &
!$OMP&         SCHEDULE ( STATIC )
      DO 430 J3=1,I_EQ-1 
!
! ------ J3  -- latitudinal index in the southern hemisphere, 
! ------ I_S -- the index in the northern hemisphere
!
         I_S = 2*I_EQ - J3
	 THETA = PI* DBLE(J3-1)/DBLE(N-1)
         Z = COS(THETA)
         U = SIN(THETA)
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
         CALL FP_ZERO_X ( ALF_SQ_X )
         CALL FP_XFF    ( ALF_SQ_X, PM2 )
         CALL FP_XFF    ( ALF_SQ_X, PM1 )
#endif
#ifdef PRINT_PMN
         WRITE ( 6, 210 ) J3, 0, 0, PM2
         WRITE ( 6, 210 ) J3, 1, 0, PM1
 210     FORMAT ( 'Lat: ', I5, ' Deg: ', I5, ' Ord: ', I5, ' Pmn= ', 1PD22.15 )
 220     FORMAT ( 'Lat: ', I5, ' Deg: ', I5, ' Ord: ', I5, ' Pmn= ', 1PD22.15, 1X, I6 )
#endif
!
! ------ Calculate Pmm(0,0) and Pmm(1,1)
!
         PMM_ARR(0) = SQRI(2)
         PMM_ARR(1) = PMM_ARR(0)* SQRI(2*1+1)/ SQRI(2*1)
!
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
! --------- Calculate P(m,0)
!
            IF ( P_X%E == 0 ) THEN 
                 TEMPC(1)   =                         P_X%F * DCMPLX ( SPH(1,J4,0,1), 0.0D0 )
                 TEMPC(3)   = - FSH%F3(J4,1)/SQRI(2)* P_X%F * DCMPLX ( SPH(1,J4,1,2), -SPH(2,J4,1,2) )
!
                 CAR%C(0,1) = CAR%C(0,1) + TEMPC(1)
                 CAR%C(1,3) = CAR%C(1,3) + TEMPC(3)
                 IF ( MOD(J4,2) == 1 ) THEN
                      TEMPC(1) = -TEMPC(1)
                      TEMPC(3) = -TEMPC(3)
                 END IF
                 CAR%S(0,1) = CAR%S(0,1) + TEMPC(1)
                 CAR%S(1,3) = CAR%S(1,3) + TEMPC(3)
            END IF
!
            PMM_ARR(J4) = PMM_ARR(J4-1)* SQRI(2*J4+1)/ SQRI(2*J4)
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
            PM2 = PMM_ARR(J5)
            CALL FP_F_TO_X ( PM2, PM2_X )
            CALL FP_YZ     ( PM2_X, UM_X(J5), PM2_X, PUM )
            IF ( PM2_X%E == 0 ) THEN
                 TEMPC(1) =                 PM2_X%F * DCMPLX( SPH(1,J5,J5,1),   -SPH(2,J5,J5,1) )
                 TEMPC(2) =            J5 * PM2_X%F * DCMPLX( SPH(2,J5,J5,2),    SPH(1,J5,J5,2) )
                 TEMPC(3) = FSH%F3(J5,J5) * PM2_X%F * DCMPLX( SPH(1,J5,J5-1,2), -SPH(2,J5,J5-1,2) )
!
                 CAR%C(J5,1)   = CAR%C(J5,1)   + TEMPC(1)
                 CAR%C(J5,2)   = CAR%C(J5,2)   + TEMPC(2)
                 CAR%C(J5-1,3) = CAR%C(J5-1,3) + TEMPC(3)
!
                 CAR%S(J5,1)   = CAR%S(J5,1)   + TEMPC(1)
                 CAR%S(J5,2)   = CAR%S(J5,2)   + TEMPC(2)
                 CAR%S(J5-1,3) = CAR%S(J5-1,3) + TEMPC(3)
            END IF
!
! --------- Semi-sectorial (m+1,m) element
! --------- PM1: p(j5+1,j5)
!
            PM1 = Z* FSH%F1(J5+1,J5)* PM2
            CALL FP_F_TO_X ( PM1, PM1_X )
            CALL FP_YZ     ( PM1_X, UM_X(J5), PM1_X, PUM )
            IF ( PM1_X%E == 0 ) THEN
                 TEMPC(1) =                      PM1_X%F * DCMPLX( SPH(1,J5+1,J5,1),   -SPH(2,J5+1,J5,1))
                 TEMPC(2) =                J5  * PM1_X%F * DCMPLX( SPH(2,J5+1,J5,2),    SPH(1,J5+1,J5,2))
                 TEMPC(3) =  FSH%F3(J5+1,J5)   * PM1_X%F * DCMPLX( SPH(1,J5+1,J5-1,2), -SPH(2,J5+1,J5-1,2))
                 TEMPC(4) = -FSH%F3(J5+1,J5+1) * PM1_X%F * DCMPLX( SPH(1,J5+1,J5+1,2), -SPH(2,J5+1,J5+1,2))
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
            IF ( DEG > 1 ) THEN
                 DO 460 J6=J5+2,KD,2 ! over degree
!
! ----------------- Compute associated Legendre function p(j6,j5)
!
                    CALL FP_AYBZ ( FSH%F1(J6,J5)*Z, PM1_X, -FSH%F2(J6,J5), PM2_X, PV_X(1) )
                    PM2_X = PM1_X
                    PM1_X = PV_X(1)
!
! ----------------- Compute associated Legendre function p(j6+1,j5)
!
                    CALL FP_AYBZ ( FSH%F1(J6+1,J5)*Z, PM1_X, -FSH%F2(J6+1,J5), PM2_X, PV_X(2) )
                    PM2_X = PM1_X
                    PM1_X = PV_X(2)
#ifdef ALF_SQ_COMP
                    CALL FP_XZZ    ( ALF_SQ_X, PV_X(1) )
                    CALL FP_XZZ    ( ALF_SQ_X, PV_X(2) )
#endif
#ifdef PRINT_PMN
                    WRITE ( 6, 220 ) J3, J6,   J5, PV_X(1)%F, PV_X(1)%E
                    WRITE ( 6, 220 ) J3, J6+1, J5, PV_X(2)%F, PV_X(2)%E
#endif
!
                    IF ( PV_X(1)%E == 0 ) THEN
                         TEMPC(1) =    PV_X(1)%F* DCMPLX( SPH(1,J6,J5,1), -SPH(2,J6,J5,1) )
                         TEMPC(2) = J5*PV_X(1)%F* DCMPLX( SPH(2,J6,J5,2),  SPH(1,J6,J5,2) )
                         TEMPC(3) =    PV_X(1)%F* FSH%F3(J6,J5)   * DCMPLX( SPH(1,J6,J5-1,2), -SPH(2,J6,J5-1,2) )
                         TEMPC(4) =   -PV_X(1)%F* FSH%F3(J6,J5+1) * DCMPLX( SPH(1,J6,J5+1,2), -SPH(2,J6,J5+1,2) )
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
                    END IF
!
                    IF ( PV_X(2)%E == 0 ) THEN
                         TEMPC(1) =     PV_X(2)%F* DCMPLX( SPH(1,J6+1,J5,1), -SPH(2,J6+1,J5,1) )
                         TEMPC(2) =  J5*PV_X(2)%F* DCMPLX( SPH(2,J6+1,J5,2),  SPH(1,J6+1,J5,2) )
                         TEMPC(3) =     PV_X(2)%F* FSH%F3(J6+1,J5)   * DCMPLX( SPH(1,J6+1,J5-1,2), -SPH(2,J6+1,J5-1,2) )
                         TEMPC(4) =    -PV_X(2)%F* FSH%F3(J6+1,J5+1) * DCMPLX( SPH(1,J6+1,J5+1,2), -SPH(2,J6+1,J5+1,2) )
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
                    END IF
 460             CONTINUE 
!
                 IF ( KD == DEG-2 ) THEN
!
! ------------------- If j5 is odd, then we need to compute the last term
! ------------------- Compute p: associated Legendre function p(deg,j5)
!
                      CALL FP_AYBZ ( FSH%F1(DEG,J5)*Z, PM1_X, -FSH%F2(DEG,J5), PM2_X, P_X )
                      PM2_X = PM1_X
                      PM1_X = P_X
#ifdef ALF_SQ_COMP
                      CALL FP_XZZ    ( ALF_SQ_X, P_X )
#endif
#ifdef PRINT_PMN
                      WRITE ( 6, 220 ) J3, DEG, J5, P_X%F, P_X%E
#endif
                      IF ( P_X%E == 0 ) THEN
                           TEMPC(1) =     P_X%F* DCMPLX( SPH(1,DEG,J5,1), -SPH(2,DEG,J5,1) )
                           TEMPC(2) =  J5*P_X%F* DCMPLX( SPH(2,DEG,J5,2),  SPH(1,DEG,J5,2) )
                           TEMPC(3) =     P_X%F* FSH%F3(DEG,J5)   * DCMPLX( SPH(1,DEG,J5-1,2), -SPH(2,DEG,J5-1,2) )
                           TEMPC(4) =    -P_X%F* FSH%F3(DEG,J5+1) * DCMPLX( SPH(1,DEG,J5+1,2), -SPH(2,DEG,J5+1,2) )
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
            END IF
 450     CONTINUE 
!
! ------ Contribution of (deg,deg) term
!
         P = PMM_ARR(DEG) 
         IF ( UM_X(DEG)%E == 0 ) THEN
              TEMPC(1) =     P * UM_X(DEG)%F * &
     &                       DCMPLX( SPH(1,DEG,DEG,1),   -SPH(2,DEG,DEG,1) )
              TEMPC(2) = DEG* P * UM_X(DEG)%F * &
     &                       DCMPLX( SPH(2,DEG,DEG,2),    SPH(1,DEG,DEG,2) )
              TEMPC(3) =     P * UM_X(DEG)%F * FSH%F3(DEG,DEG) * &
     &                       DCMPLX( SPH(1,DEG,DEG-1,2), -SPH(2,DEG,DEG-1,2) )
!
              CAR%C(DEG,1)   = CAR%C(DEG,1)   + TEMPC(1)
              CAR%C(DEG,2)   = CAR%C(DEG,2)   + TEMPC(2)
              CAR%C(DEG-1,3) = CAR%C(DEG-1,3) + TEMPC(3)
!
              CAR%S(DEG,1)   = CAR%S(DEG,1)   + TEMPC(1)
              CAR%S(DEG,2)   = CAR%S(DEG,2)   + TEMPC(2)
              CAR%S(DEG-1,3) = CAR%S(DEG-1,3) + TEMPC(3)
         END IF 
         CAR%C(1:DEG,1:3) = CAR%C(1:DEG,1:3)/2.0D0
         CAR%S(1:DEG,1:3) = CAR%S(1:DEG,1:3)/2.0D0
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
      CALL FP_ZERO_X ( ALF_SQ_X )
      CALL FP_XFF    ( ALF_SQ_X, PM2 )
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
         CAR%C(1,3) = CAR%C(1,3) - FSH%F3(J8,1)/SQRI(2) * P * &
     &                             DCMPLX( SPH(1,J8,1,2), -SPH(2,J8,1,2) )
#ifdef ALF_SQ_COMP
              CALL FP_XFF    ( ALF_SQ_X, P )
#endif
#ifdef PRINT_PMN
      WRITE ( 6, 210 ) 0, J8, 0, P
#endif
 480  CONTINUE 
!
      PMM = SQRI(2)
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
              CALL FP_XFF  ( ALF_SQ_X, PM2 )
#endif
#ifdef PRINT_PMN
              WRITE ( 6, 210 ) 0, J9, 1, PM2
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
               CALL FP_XFF  ( ALF_SQ_X, P )
#endif
#ifdef PRINT_PMN
              WRITE ( 6, 210 ) 0, J9, J10, P
#endif
 4100         CONTINUE 
         END IF
 490  CONTINUE 
      PMM = PMM* SQRI(2*DEG+1)/ SQRI(2*DEG)
#ifdef ALF_SQ_COMP
           CALL FP_XFF  ( ALF_SQ_X, PMM )
           ALF_SQ_R8 = FP_X_TO_F ( ALF_SQ_X )
           WRITE ( 6, 110 ) 0, (ALF_SQ_R8 - (DEG+1)**2)/ALF_SQ_R8
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
      CAR%C(1:DEG,1:2) = CAR%C(1:DEG,1:2)/2.0D0
      CAR%C(0:DEG,3:3) = CAR%C(0:DEG,3:3)
      CAR%C(1:DEG,3:3) = CAR%C(1:DEG,3:3)/2.0D0
      CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%C(0,1), FUN(1:2*(N-1),I_EQ,1), IER )
      CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%C(0,2), FUN(1:2*(N-1),I_EQ,2), IER )
      CALL FFT_1D_C2R_C16 ( 4*(MD+1), CAR%C(0,3), FUN(1:2*(N-1),I_EQ,3), IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      CONTAINS 
        INCLUDE 'x_ariphmetic.f'
      END  SUBROUTINE  SPHE_INV_2NN_VEC_X  !#!#
