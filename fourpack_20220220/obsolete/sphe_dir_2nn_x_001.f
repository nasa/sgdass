#define BLF_SQ_COMP
      SUBROUTINE SPHE_DIR_2NN_X ( FSH, N, FUN, MD, DEG, NORM, IPHS, SPH, IUER )
! ************************************************************************
! *                                                                      *
! *     This routine will expand a grid containing 2*N samples           *
! *   in longitude and N samples in latitude into spherical harmonics.   *
! *   This routine makes use of the sampling theorem presented in        *
! *   Driscoll and Healy (1994) and employs FFTs when calculating        *
! *   the sin and cos terms. The number of samples, N, must be EVEN      *
! *   for this routine to work, and the spherical harmonic expansion     *
! *   is exact if the function is bandlimited to degree N/2-1.           *
! *                                                                      *
! *   X-arithmetic is used. This maximum dimension of the transform is   *
! *   theoretically unlimited. In practice it is restricted by           *
! *   parameter FSH__MAX_DEG.                                            *
! *                                                                      *
! *   Legendre functions are computed on the fly using the scaling       *
! *   methodolgy presented in Holmes and Featherston (2002). When NORM   *
! *   is 1,2 or 4, these are accurate to about degree 2800.              *
! *   When NORM is 3, the routine is only stable to about degree 15.     *
! *                                                                      *
! *   The input grid contains N samples in latitude from -90 to          *
! *   90-interval, and 2*N samples in longitude from 0 to                *
! *   360-interval, where interval is the latitudinal sampling           *
! *   interval 180/N.                                                    *
! *                                                                      *
! *   The input grid must contain N samples in latitude and 2N samples   *
! *   in longitude. The sampling intervals in latitude and longitude     *
! *   are 180/N and 360/N respectively. When performing the FFTs in      *
! *   longitude, the frequencies greater than N/2-1 are simply discarded *
! *   to prevent aliasing.                                               *
! *                                                                      *
! *   Notes:                                                             *
! *   1. This routine does not use the fast legendre transforms that     *
! *      are presented in Driscoll and Heally (1994).                    *
! *   2. Use of a N by 2N grid is implemented because many geographic    *
! *      grids are sampled this way. When taking the Fourier transforms  *
! *      in longitude, all of the higher frequencies are ultimately      *
! *      discarded. If, instead, every other column of the grid were     *
! *      discarded to form a NxN grid, higher frequencies could be       *
! *      aliased into lower frequencies.                                 *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FSH ( SPHE_TYPE ) -- Internal data structure that keeps internal    *
! *                       arrays with intermediate results and their     *
! *                       status for possible re-use.                    *
! *    N ( INTEGER*4 ) -- Dimension of the input function along          *
! *                       latitude.                                      *
! *  FUN ( REAL*8    ) -- The function that is to be expanded.           *
! *                       Dimension 2N*N (longitude,latitude).           *
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
! *                                     
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  SPH ( REAL*8    ) -- Array with spherical transform coefficients.   *
! *                       Dimension: (2,0:MD,0:MD). The first dimesion   *
! *                       runs over cosine/sine compoenent, the second   *
! *                       dimension runs over order l, the third         *
! *                       dimension runs over degre.                     *
! *                       NB: only coefficients l =< m are filled!       *
! *                       The part of array SPH l > m is filled with     *
! *                       zeroes.                                        *
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
! * ## 08-AUG-2012 SPHE_DIR_2NN v1.3 modified by L. Petrov 10-FEB-2015 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  N, MD, NORM, IPHS, DEG, IUER
      REAL*8     FUN(2*N,N), SPH(2,0:MD,0:MD)
      CHARACTER  STR*128, STR1*128
      INTEGER*4  DEG_MAX
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, I_EQ, I_S, KD, NUM_THR_SAVED, NTHR, IER
      COMPLEX*16 CC(N+1)
      INTEGER*8  PLAN
      TYPE ( X__TYPE ) :: UM_X(DEG), P_X, PM1_X, PM2_X, PUM_X
      TYPE ( X__TYPE ) :: ALF_SQ_X, ALF_ORD_SQ_X(0:FSH__MAX_DEG)
      REAL*8     GRIDL(2*N), THETA, Z, U, FCOEF1(2,N/2+1), FCOEF2(2,N/2+1), &
     &           PM1, PM2, P, PMM, PROD, FFC_ODD(2), FFC_EVEN(2), &
     &           UM(DEG), PMM_ARR(0:DEG), ALF_SQ_R8, PUM
      LOGICAL*1  FL_ERROR, FL_PARAL
      CHARACTER, EXTERNAL :: GET_CDATE_MS*23
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS, OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL SPHE_FF_X ( FSH, DEG, NORM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6431, IUER, 'SPHE_DIR_2NN_X', 'Error in '// &
     &         'an attempt to precompute multiplicative factors used '// &
     &         ' in recursion relationships F1 and F2' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL SPHE_AJ_X ( FSH, N, NORM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6432, IUER, 'SPHE_DIR_2NN_X', 'Error in '// &
     &         'an attempt to precompute coefficients A_j for the '// &
     &         'Driscoll and Healy (1994) spherical harmonics '// &
     &         'quadrature' )
           RETURN 
      END IF
      SPH = 0.0D0
!
      CC = 0.0D0
!$OMP CRITICAL
      CALL DFFTW_PLAN_DFT_R2C_1D ( PLAN, 2*N, GRIDL, CC, FFTW_MEASURE )
!$OMP END CRITICAL
!
      I_EQ = N/2 + 1	! Index corresponding to the equator
!
! --- Save the current number of threads and set up the new number of threads
! --- that was supplied during SPHE initialization
!
      IF ( .NOT. OMP_IN_PARALLEL() ) THEN
           NUM_THR_SAVED = OMP_GET_NUM_THREADS()
           CALL OMP_SET_NUM_THREADS ( %VAL(FSH%NUM_THR) )
           NTHR = FSH%NUM_THR
         ELSE 
           NTHR = 1
      END IF
!  call cpu_timer ( %val(1) ) ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, * ) 'sphe_dir_2nn_x 164 ' ; call flush ( 6 ) ! %%%%%%%%%%
!  call cpu_timer ( %val(0) )  ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!$OMP PARALLEL DO IF ( NTHR > 1 ), &
!$OMP    PRIVATE ( J3, J4, J5, J6, J7, I_S, KD, THETA, Z, U, CC, &
!$OMP&             FCOEF1, FCOEF2, FFC_EVEN, FFC_ODD, UM,  &
!$OMP&             P, PM1, PM2, PMM_ARR, ALF_SQ_X, ALF_ORD_SQ_X, ALF_SQ_R8 ),  &
!$OMP&             REDUCTION (+: SPH),  &
!$OMP&             SCHEDULE ( STATIC )
!
! --- Cycle from the zone one cell near the pole to the zone one cell near equator
!
      DO 430 J3=2,I_EQ-1
         THETA = PI* DBLE(J3-1)/DBLE(N)
         Z = COS(THETA)
         U = SIN(THETA)
         I_S = 2*I_EQ - J3
!
         CALL DFFTW_EXECUTE_DFT_R2C ( PLAN, FUN(1:2*N,I_S), CC ) ! Take fourier transform
         FCOEF1(1,1:N/2) =  SQRT(2.0D0*PI)* FSH%AJ(I_S)* DREAL(CC(1:N/2))/ DBLE(2*N)
         FCOEF1(2,1:N/2) = -SQRT(2.0D0*PI)* FSH%AJ(I_S)* DIMAG(CC(1:N/2))/ DBLE(2*N)
!		
         CALL DFFTW_EXECUTE_DFT_R2C ( PLAN, FUN(1:2*N,J3), CC ) ! Take fourier transform
	 FCOEF2(1,1:N/2) =  SQRT(2.0D0*PI)* FSH%AJ(J3)* DREAL(CC(1:N/2))/ DBLE(2*N)
	 FCOEF2(2,1:N/2) = -SQRT(2.0D0*PI)* FSH%AJ(J3)* DIMAG(CC(1:N/2))/ DBLE(2*N)
!
! ------ PM2: p(0,0)
!
         IF ( NORM == 1 .OR. NORM == 2 .OR. NORM == 3 ) THEN
              PM2 = 1.0D0
           ELSE IF ( NORM == 4 ) THEN
              PM2 = 1.0D0/ DSQRT(4.0D0*PI)
         END IF
#ifdef ALF_SQ_COMP
         CALL ZERO_X ( ALF_ORD_SQ_X(0) )
         CALL XFF ( ALF_ORD_SQ_X(0), PM2 )
#endif
!
         SPH(1,0,0) = SPH(1,0,0) + PM2* ( FCOEF1(1,1) + FCOEF2(1,1) )
         IF ( DEG == 0 ) GOTO 430
!
! ------ PM1: p(1,0)
!
         PM1 = FSH%F1(1,0)* Z* PM2  !@@@
         SPH(1,1,0) = SPH(1,1,0) + PM1* ( FCOEF1(1,1) - FCOEF2(1,1) )
#ifdef ALF_SQ_COMP
         CALL XFF ( ALF_ORD_SQ_X(0), PM1 )
#endif
!
         FFC_EVEN(1) = FCOEF1(1,1) + FCOEF2(1,1)
         FFC_ODD(1)  = FCOEF1(1,1) - FCOEF2(1,1)
         IF ( MOD(DEG,2) == 0 ) THEN
              KD = DEG-2
            ELSE 
              KD = DEG-1
         END IF
         IF ( NORM == 1 .OR. NORM == 2 ) THEN
              PMM_ARR(0) = SQRI(2)
           ELSE IF ( NORM == 3 ) THEN
              PMM_ARR(0) = 1.0D0
           ELSE IF ( NORM == 4 ) THEN
              PMM_ARR(0) = SQRI(2)/ SQRT(4.0D0*PI)
         END IF
         IF ( NORM == 1 .OR. NORM == 4 ) THEN
              PMM_ARR(1) = IPHS* PMM_ARR(0)* SQRI(2*1+1)/ SQRI(2*1)
            ELSE IF ( NORM == 2 ) THEN
              PMM_ARR(1) = IPHS* PMM_ARR(0)* SQRI(2*1+1)/ SQRI(2*1)
            ELSE IF ( NORM == 3 ) THEN
              PMM_ARR(1) = IPHS* PMM_ARR(0)* DBLE(2*1-1)
         END IF
         UM(1) = U
         CALL F_TO_X ( U, UM_X(1) )
!
! ------ Zonal terms
!
         CALL F_TO_X ( PM1, PM1_X )
         CALL F_TO_X ( PM2, PM2_X )
         DO 440 J4=2,KD,2
!
! --------- P: p(j4,0)
!
            CALL AYBZ ( FSH%F1(J4,0)*Z, PM1_X, -FSH%F2(J4,0), PM2_X, P_X, P )
            PM2_X = PM1_X
            PM1_X = P_X
            SPH(1,J4,0) = SPH(1,J4,0) + P* FFC_EVEN(1)
            UM(J4) = U* UM(J4-1)
            CALL XPA ( UM_X(J4), U, UM_X(J4-1) )
            IF ( NORM == 1 .OR. NORM == 4 ) THEN
                 PMM_ARR(J4) = IPHS* PMM_ARR(J4-1)* SQRI(2*J4+1)/ SQRI(2*J4)
              ELSE IF ( NORM == 2 ) THEN
                 PMM_ARR(J4) = IPHS* PMM_ARR(J4-1)* SQRI(2*J4+1)/ SQRI(2*J4)
              ELSE IF ( NORM == 3 ) THEN
                 PMM_ARR(J4) = IPHS* PMM_ARR(J4-1)* DBLE(2*J4-1)
            END IF
#ifdef ALF_SQ_COMP
            CALL XZZ ( ALF_ORD_SQ_X(0), P_X )
#endif
!
! --------- P: p(j4+1,0)
!
            CALL AYBZ ( FSH%F1(J4+1,0)*Z, PM1_X, -FSH%F2(J4+1,0), PM2_X, P_X, P )
            PM2_X = PM1_X
            PM1_X = P_X
            SPH(1,J4+1,0) = SPH(1,J4+1,0) + P* FFC_ODD(1)
            UM(J4+1) = U* UM(J4)
            CALL XPA ( UM_X(J4+1), U, UM_X(J4) )
            IF ( NORM == 1 .OR. NORM == 4 ) THEN
                 PMM_ARR(J4+1) = IPHS* PMM_ARR(J4)* SQRI(2*J4+3)/ SQRI(2*J4+2)
              ELSE IF ( NORM == 2 ) THEN
                 PMM_ARR(J4+1) = IPHS* PMM_ARR(J4)* SQRI(2*J4+3)/ SQRI(2*J4+2)
              ELSE IF ( NORM == 3 ) THEN
                 PMM_ARR(J4+1) = IPHS* PMM_ARR(J4)* DBLE(2*J4+1)
            END IF
#ifdef ALF_SQ_COMP
            CALL XZZ ( ALF_ORD_SQ_X(0), P_X )
#endif
 440     CONTINUE 
         IF ( KD == DEG - 2  ) THEN
!
! ----------- Remaining zonal terms. P: p(deg,0)
!
              CALL AYBZ ( FSH%F1(DEG,0)*Z, PM1_X, -FSH%F2(DEG,0), PM2_X, P_X, P )
              PM2_X = PM1_X
              PM1_X = P_X
              SPH(1,DEG,0) = SPH(1,DEG,0) + P* FFC_EVEN(1)
              UM(DEG) = U* UM(DEG-1)
              CALL XPA ( UM_X(DEG), U, UM_X(DEG-1) )
              IF ( NORM == 1 .OR. NORM == 4 ) THEN
                   PMM_ARR(DEG) = IPHS* PMM_ARR(DEG-1)* SQRI(2*DEG+1)/ SQRI(2*DEG)
                 ELSE IF ( NORM == 2 ) THEN
                   PMM_ARR(DEG) = IPHS* PMM_ARR(DEG-1)/ SQRI(2*DEG)
                 ELSE IF ( NORM == 3 ) THEN
                   PMM_ARR(DEG) = IPHS* PMM_ARR(DEG-1)/ DBLE(2*DEG-1)
              END IF
#ifdef ALF_SQ_COMP
              CALL XZZ ( ALF_ORD_SQ_X(0), P_X )
#endif
         END IF
         DO 450 J5=1,DEG-1 ! J5: order
            IF ( NORM == 1 .OR. NORM == 4 ) THEN
                 PM2 = PMM_ARR(J5)
              ELSE IF ( NORM == 2 ) THEN
                 PM2 = PMM_ARR(J5) / SQRI(2*J5+1)
              ELSE IF ( NORM == 3 ) THEN
                 PM2 = PMM_ARR(J5)
            END IF
!
! --------- Sectorial (m,m) element
! --------- PM2: p(j5,j5)
!
            FFC_ODD(1:2)  = FCOEF1(1:2,J5+1) + FCOEF2(1:2,J5+1)
            FFC_EVEN(1:2) = FCOEF1(1:2,J5+1) - FCOEF2(1:2,J5+1)
            CALL F_TO_X ( PM2, PM2_X )
            CALL YZ ( PM2_X, UM_X(J5), PUM_X, PUM )
            IF ( PUM .NE. 0 ) SPH(1:2,J5,J5) = SPH(1:2,J5,J5) + PUM * FFC_ODD(1:2)
#ifdef ALF_SQ_COMP
            CALL ZERO_X ( ALF_ORD_SQ_X(J5)      )
            CALL XZZ    ( ALF_ORD_SQ_X(J5), PM2_X )
#endif
!
! --------- Semi-sectorial (m+1,m) element
! --------- PM1: p(j5+1,j5)
!
            PM1 = Z* FSH%F1(J5+1,J5)* PM2
            CALL F_TO_X ( PM1, PM1_X )
            CALL YZ ( PM1_X, UM_X(J5), PUM_X, PUM )
            IF ( PUM .NE. 0 ) SPH(1:2,J5+1,J5) = SPH(1:2,J5+1,J5) + PUM * FFC_EVEN(1:2)
#ifdef ALF_SQ_COMP
            CALL XZZ    ( ALF_ORD_SQ_X(J5), PM1_X )
#endif
            IF ( MOD(DEG,2) == MOD(J5,2) ) THEN
                 KD = DEG-2
               ELSE 
                 KD = DEG-1
            END IF
!
! --------- Tesseral terms
!
	    DO 460 J6=J5+2,KD,2 ! J6: degree
!
! ------------ P: p(j6,j5)
!
               CALL AYBZ ( FSH%F1(J6,J5)*Z, PM1_X, -FSH%F2(J6,J5), PM2_X, P_X, P )
               PM2_X = PM1_X
               PM1_X = P_X
               CALL YZ ( P_X, UM_X(J5), PUM_X, PUM )
               IF ( PUM .NE. 0.0D0 ) SPH(1:2,J6,J5) = SPH(1:2,J6,J5) + PUM* FFC_ODD
#ifdef ALF_SQ_COMP
               CALL XZZ    ( ALF_ORD_SQ_X(J5), P_X )
#endif
!
! ------------ P: p(j6+1,j5)
!
               CALL AYBZ ( FSH%F1(J6+1,J5)*Z, PM1_X, -FSH%F2(J6+1,J5), PM2_X, P_X, P )
               PM2_X = PM1_X
               PM1_X = P_X
               CALL YZ ( P_X, UM_X(J5), PUM_X, PUM )
               IF ( PUM .NE. 0.0D0 ) SPH(1:2,J6+1,J5) = SPH(1:2,J6+1,J5) + PUM* FFC_EVEN
#ifdef ALF_SQ_COMP
               CALL XZZ    ( ALF_ORD_SQ_X(J5), P_X )
#endif
 460        CONTINUE 
            IF ( KD == DEG-2 ) THEN
                 CALL AYBZ ( FSH%F1(DEG,J5)*Z, PM1_X, -FSH%F2(DEG,J5), PM2_X, P_X, P )
                 PM2_X = PM1_X
                 PM1_X = P_X
                 CALL YZ ( P_X, UM_X(J5), PUM_X, PUM )
                 IF ( PUM .NE. 0.0D0 ) SPH(1:2,DEG,J5) = SPH(1:2,DEG,J5) + PUM* FFC_ODD
#ifdef ALF_SQ_COMP
                 CALL XZZ    ( ALF_ORD_SQ_X(J5), P_X )
#endif
            END IF
 450     CONTINUE 
!
! ------ P: p(deg,deg)
!
         P = PMM_ARR(DEG) 
         SPH(1:2,DEG,DEG) = SPH(1:2,DEG,DEG) + P * X_TO_F ( UM_X(DEG) )* &
     &                         ( FCOEF1(1:2,DEG+1) + FCOEF2(1:2,DEG+1) )
#ifdef ALF_SQ_COMP
         CALL ZERO_X ( ALF_ORD_SQ_X(DEG) )
         CALL XFF    ( ALF_ORD_SQ_X(DEG), P )
!
! ------ Compute the sum of squares of associated Legendra function.
! ------ In order to prevent accuracy loss, we use the Horner scheme
! ------ for summation
!
         CALL ZERO_X ( ALF_SQ_X )
         DO 470 J7=DEG,1,-1
            CALL AYBZ ( U**2, ALF_SQ_X, U**2, ALF_ORD_SQ_X(J7), ALF_SQ_X, P )
 470     CONTINUE
         CALL AYBZ ( 1.0D0, ALF_ORD_SQ_X(0), 1.D0, ALF_SQ_X, ALF_SQ_X, ALF_SQ_R8 )
         WRITE ( 6, 110 ) N/2+1-J3, (ALF_SQ_R8 - (DEG+1)**2)/ALF_SQ_R8
 110     FORMAT ( 'SPH_DIR_2NN_X  Lat: ', I5, ' Alf_sq_err= ', 1PD25.12 )
#endif
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
!	
      CALL DFFTW_EXECUTE_DFT_R2C ( PLAN, FUN(1:2*N,I_EQ), CC ) ! Take fourier transform
      FCOEF1(1,1:N/2) =  DSQRT(2.0D0*PI)* FSH%AJ(I_EQ)* DBLE (CC(1:N/2))/ DBLE(2*N)
      FCOEF1(2,1:N/2) = -DSQRT(2.0D0*PI)* FSH%AJ(I_EQ)* DIMAG(CC(1:N/2))/ DBLE(2*N)
!
      IF ( NORM == 1 .OR. NORM == 2 .OR. NORM == 3 ) THEN
           PM2 = 1.0D0
        ELSE IF ( NORM == 4 ) THEN
           PM2 = 1.0D0/ DSQRT(4.0D0*PI)
      END IF
      SPH(1,0,0) = SPH(1,0,0) + PM2 * FCOEF1(1,1)
#ifdef ALF_SQ_COMP
         CALL ZERO_X ( ALF_SQ_X )
         CALL XFF    ( ALF_SQ_X, PM2 )
#endif
      IF ( DEG > 0 ) THEN
           DO 480 J8=2,DEG,2
              P = -FSH%F2(J8,0) * PM2
              PM2 = P
              SPH(1,J8,0) = SPH(1,J8,0) + P * FCOEF1(1,1)
#ifdef ALF_SQ_COMP
              CALL XFF    ( ALF_SQ_X, P )
#endif
 480      CONTINUE 
          IF ( NORM == 1 .OR. NORM == 2 ) THEN
               PMM = SQRI(2)
            ELSE IF ( NORM == 3 ) THEN
               PMM = 1.0D0
            ELSE IF ( NORM == 4 ) THEN
               PMM = SQRI(2)/ DSQRT(4.0D0*PI)
          END IF
          DO 490 J9=1,DEG-1
             IF ( NORM == 1 .OR. NORM == 4 ) THEN
                  PMM = IPHS * PMM * SQRI(2*J9+1) / SQRI(2*J9)
                  PM2 = PMM
               ELSE IF ( NORM == 2 ) THEN
                  PMM = IPHS * PMM * SQRI(2*J9+1) / SQRI(2*J9)
                  PM2 = PMM/ SQRI(2*J9+1)
               ELSE IF ( NORM == 3 ) THEN
                  PMM = IPHS * PMM * DBLE(2*J9-1)
                  PM2 = PMM
             END IF
             FCOEF1(1:2,J9+1) = FCOEF1(1:2,J9+1)
             SPH(1:2,J9,J9) = SPH(1:2,J9,J9) + PM2 * FCOEF1(1:2,J9+1)
#ifdef ALF_SQ_COMP
             CALL XFF  ( ALF_SQ_X, PM2 )
#endif
             DO 4100 J10=J9+2,DEG,2 
                P = -FSH%F2(J10,J9)* PM2
                PM2 = P
                SPH(1:2,J10,J9) = SPH(1:2,J10,J9) + P * FCOEF1(1:2,J9+1) 
#ifdef ALF_SQ_COMP
              CALL XFF  ( ALF_SQ_X, P )
#endif
 4100        CONTINUE 
 490      CONTINUE 
!
          IF ( NORM == 1 .OR. NORM == 4 ) THEN
               PMM = IPHS* PMM* SQRI(2*DEG+1)/ SQRI(2*DEG)
             ELSE IF ( NORM == 2 ) THEN
               PMM = IPHS* PMM/ SQRI(2*DEG)
             ELSE IF ( NORM == 3 ) THEN
               PMM = IPHS* PMM* DBLE(2*DEG-1)
          END IF
          SPH(1:2,DEG,DEG) = SPH(1:2,DEG,DEG) + PMM * FCOEF1(1:2,DEG+1) 
#ifdef ALF_SQ_COMP
         CALL XFF  ( ALF_SQ_X, PMM )
         ALF_SQ_R8 = X_TO_F ( ALF_SQ_X )
         WRITE ( 6, 110 ) 0, (ALF_SQ_R8 - (DEG+1)**2)/ALF_SQ_R8
#endif
      END IF 
!$OMP CRITICAL
      CALL DFFTW_DESTROY_PLAN ( PLAN ) 
!$OMP END CRITICAL
!
! --- Normailzation
!
      IF ( NORM == 1 ) THEN
           SPH = SPH/ (4.0D0*PI)
         ELSE IF ( NORM == 2 ) THEN
           DO 4110 J11=0,DEG
              SPH(1:2,J11,0:J11) = SPH(1:2,J11,0:J11)* DBLE(2*J11+1)/ (4.0D0*PI)
 4110      CONTINUE 
         ELSE IF ( NORM == 3 ) THEN
           DO 4120 J12=0,DEG ! L
              PROD = 4.0D0*PI/DBLE(2*J12+1)
	      SPH(1,J12,0) = SPH(1,J12,0) / PROD
              PROD = PROD / 2.0D0
              DO 4130 J13=1,J12-1 ! M
                 PROD = PROD * DBLE(J12+J13) * DBLE(J12-J13+1)
                 SPH(1:2,J12,J13) = SPH(1:2,J12,J13)/PROD
 4130         CONTINUE 
              IF ( J12 .NE. 0 ) THEN
                   SPH(1:2,J12,J12) = SPH(1:2,J12,J12)/(PROD*DBLE(2*J12))
              END IF
 4120      CONTINUE 
      END IF		
!      
      CALL ERR_LOG ( 0, IUER )
      RETURN
      CONTAINS 
          SUBROUTINE ZERO_X ( X )
          REAL*8     F
          TYPE ( X__TYPE ) :: X
          X%F = 0.0D0
          X%E = 0
          END SUBROUTINE ZERO_X
!
          SUBROUTINE F_TO_X ( F, X )
          REAL*8     F
          TYPE ( X__TYPE ) :: X
          X%F = F
          X%E = 0
          END SUBROUTINE F_TO_X
!
          FUNCTION X_TO_F ( X )
          REAL*8   X_TO_F
          TYPE ( X__TYPE ) :: X
          IF ( X%E == 0 ) THEN
               X_TO_F = X%F
            ELSE IF ( X%E < 0 ) THEN
               X_TO_F = X%F*BIGU_FF
            ELSE
               X_TO_F = X%F*BIGO_FF
          END IF
          END FUNCTION X_TO_F
!
          SUBROUTINE NRML ( X )
          TYPE ( X__TYPE ) :: X
          REAL*8     W
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          END SUBROUTINE NRML
!
          SUBROUTINE AX ( A, X )
!
! ------- X := A*X
!
          REAL*8     A, W
          TYPE ( X__TYPE ) :: X
          X%F = A*X%F
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          END SUBROUTINE AX
!
          SUBROUTINE AYBZ ( A, Y, B, Z, X, F )
!
! ------- X := A*Y + B*Z
!
          REAL*8     A, B, F, W
          TYPE ( X__TYPE ) :: Y, Z, X
          INTEGER*4   ID
          ID = Y%E - Z%E
          IF ( ID == 0 ) THEN
               X%F = A*Y%F + B*Z%F
               X%E = Y%E
            ELSE IF ( ID ==  1 ) THEN
               X%F = A*Y%F + B*(Z%F*BIGU_FF)
               X%E = Y%E
            ELSE IF ( ID == -1 ) THEN
               X%F = A*(Y%F*BIGU_FF) + B*Z%F
               X%E = Z%E
            ELSE IF ( ID  >  1 ) THEN
               X%F = A*Y%F
               X%E = Y%E
            ELSE
               X%F = B*Z%F
               X%E = Z%E
          END IF
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          IF ( X%E == 0 ) THEN
               F = X%F
            ELSE IF ( X%E < 0 ) THEN
               F = X%F*BIGU_FF
            ELSE
               F = X%F*BIGO_FF
          END IF
          END SUBROUTINE AYBZ
!
          SUBROUTINE YZ ( Y, Z, X, F )
!
! ------- X := Y*Z
!
          REAL*8     F, W
          TYPE ( X__TYPE ) :: Y, Z, X
          INTEGER*4   ID
!
          X%F = Y%F * Z%F
          X%E = Y%E + Z%E
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          IF ( X%E == 0 ) THEN
               F = X%F
            ELSE IF ( X%E < 0 ) THEN
               F = 0.0D0
            ELSE
               F = X%F*BIGO_FF
          END IF
          END SUBROUTINE YZ
!
          SUBROUTINE XAZ ( X, A, Z )
!
! ------- X := X + A*Z
!
          REAL*8     A, W
          TYPE ( X__TYPE ) :: X, Z
          INTEGER*4   ID
          ID = X%E - Z%E
          IF ( ID == 0 ) THEN
               X%F = X%F + A*Z%F
               X%E = X%E
            ELSE IF ( ID ==  1 ) THEN
               X%F = X%F + A*(Z%F*BIGU_FF)
               X%E = X%E
            ELSE IF ( ID == -1 ) THEN
               X%F = X%F*BIGU_FF + A*Z%F
               X%E = Z%E
            ELSE IF ( ID  >  1 ) THEN
               CONTINUE 
            ELSE
               X%F = A*Z%F
               X%E = Z%E
          END IF
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          END SUBROUTINE XAZ
!
          SUBROUTINE XPA ( X, A, Y )
!
! ------- X := A*Y
!
          REAL*8     A, W
          TYPE ( X__TYPE ) :: X, Y
          INTEGER*4  ID
          X%F = A*Y%F
          X%E = Y%E
          W = ABS(X%F)
          IF ( W .GE. BIGHO_FF ) THEN
               X%F = X%F*BIGU_FF
               X%E = X%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               X%F = X%F*BIGO_FF
               X%E = X%E - 1
          END IF
          END SUBROUTINE XPA
!
          SUBROUTINE XFF ( X, F )
!
! ------- X := X + Z*Z
!
          REAL*8     W, F
          TYPE ( X__TYPE ) :: X, R
          INTEGER*4  ID
          R%F = F * F
          R%E = 0
          W = ABS(R%F)
          IF ( W .GE. BIGHO_FF ) THEN
               R%F = R%F*BIGU_FF
               R%E = R%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               R%F = R%F*BIGO_FF
               R%E = R%E - 1
          END IF
!
          ID = X%E - R%E
          IF ( ID == 0 ) THEN
               X%F = X%F + R%F
               X%E = X%E
            ELSE IF ( ID ==  1 ) THEN
               X%F = X%F + R%F*BIGU_FF
               X%E = X%E
            ELSE IF ( ID == -1 ) THEN
               X%F = X%F*BIGU_FF + R%F
               X%E = R%E
            ELSE IF ( ID  >  1 ) THEN
               CONTINUE 
            ELSE
               X%F = R%F
               X%E = R%E
          END IF
          END SUBROUTINE XFF
!
          SUBROUTINE XZZ ( X, Z )
!
! ------- X := X + Z*Z
!
          REAL*8     W, F
          TYPE ( X__TYPE ) :: X, Z, R
          INTEGER*4  ID
          R%F = Z%F * Z%F
          R%E = 2*Z%E
          W = ABS(R%F)
          IF ( W .GE. BIGHO_FF ) THEN
               R%F = R%F*BIGU_FF
               R%E = R%E + 1
            ELSE IF ( W .LT. BIGHU_FF ) THEN
               R%F = R%F*BIGO_FF
               R%E = R%E - 1
          END IF
!
          ID = X%E - R%E
          IF ( ID == 0 ) THEN
               X%F = X%F + R%F
               X%E = X%E
            ELSE IF ( ID ==  1 ) THEN
               X%F = X%F + R%F*BIGU_FF
               X%E = X%E
            ELSE IF ( ID == -1 ) THEN
               X%F = X%F*BIGU_FF + R%F
               X%E = R%E
            ELSE IF ( ID  >  1 ) THEN
               CONTINUE 
            ELSE
               X%F = R%F
               X%E = R%E
          END IF
          END SUBROUTINE XZZ
!
      END  SUBROUTINE  SPHE_DIR_2NN_X  !#!#
