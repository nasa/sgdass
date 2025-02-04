      FUNCTION SPHE_INV_2NN ( FSH, MD, DEG, NORM, IPHS, SPH, N, FUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPHE_INV_2NN  given the spherical harmonic coefficients   *
! *   SPH of degree/order MD, will evalate the function on a grid with   *
! *   an equal number of samples N latitude and 2*N in longitude.        *
! *   This is the inverse the routine SPHE_DIR_2NN, both of which are    *
! *   done quickly using FFTs for each degree of each latitude band.     *
! *   The number of samples specified by the spherical harmonic          *
! *   bandwidth DEG may be equal or less than MD.                        *
! *                                                                      *
! *   Note that N is always EVEN for this routine.                       *
! *                                                                      *
! *   In a case if the dimension of transform is less or equal           *
! *   FSH__MAX_SCL, then the Legendre associated functions are computed  *
! *   on the fly  using the scaling methodology presented in Holmes and  *
! *   Featherston (2002). When NORM is 1,2 or 4, these are accurate to   *
! *   about degree 2800. When NORM is 3, the routine is only stable to   *
! *   about degree 15. In a case if the dimension of transform is        *
! *   greater than FSH__MAX_SCL, then the Legendre functions are         *
! *   computed on the fly using the X-number algebra introduced by       *
! *   Fukushima (2011). The essence of that approach is that the         *
! *   intermediate variables needed for computation of Legendre          *
! *   associated functions are kept in two numbers: the double precision *
! *   float number and integer number for additional exponent.           *
! *                                                                      *
! *   The output grid contains N samples in latitude from -90 to         *
! *   +90 deg interval, including south pole, but excluding north pole   *
! *   and in longitude from 0 to 360-1*interval (or 2(N-1) x N), where   *
! *   interval is the sampling interval, and n=2*(deg+1)+1.              *
! *                                                                      *
! *   NB: the grid excluides the northern pole.                          *
! *                                                                      *
! *   The output grid contains N samples in latitude from -90 to         *
! *   90-interval, and in longitude from 0 to 360-2*interval             *
! *   (or 2N x N), where interval is the sampling interval, and          *
! *   n=2*(deg+1). 
! *                                                                      *
! *   Timing using E5-2660-v3 (in seconds):                              *
! *                                                                      *
! *    New:           F-version          X-version                       *
! *                                                                      *
! *    Lat    Deg     1-thr  16-thr                          wall 16-th  *
! *                                                           f       x  *
! *                                                                      *
! *   1025    511     0.30    0.34        0.73    0.51     0.035   0.057 *
! *   2049   1023     2.38    0.21        5.54    0.35     0.22    0.42  *
! *   4097   2047    19.34    1.65       44.85    5.35     1.70    5.60  *
! *   5401   2699    43.23    3.45      100.89    6.53     3.33    6.50  *
! *   8193   4095      -       -        334.33   22.65      -     28.75  *
! *  10801   5399      -       -                 65.99      -     66.02  *
! *  16385   8191      -       -                176.36      -    245.6   *
! *  18001   8999      -       -                330.86      -    488.6   *                     
! *  21601  10799      -       -                661.63      -    680.5   *
! *  43201  21599      -       -               4307.0       -   4331.5   *
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
! *                       runs over cosine/sine component, the second    *
! *                       dimension runs degree m, the third dimension   *
! *                       runs over order l.                             *
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
! *   References:                                                        *
! *                                                                      *
! *   1) J.R. Driscoll  and D.M. Healy, "Computing Fourier Transforms    *
! *      and Convolutions on the 2-Sphere", (1994), Adv. Applied. Math., *
! *      15, 202-250, doi:10.1006/aama.1994.1008.                        *
! *                                                                      *
! *   2) S.A. Holmes, W.E. Featherstone, "A unified approach to the      *
! *      Clenshaw summation and the recursive computation of very high   *
! *      degree and order normalised associated Legendre functions,      *
! *      (2002), J. Geodesy, 76, 279-299, 10.1007/s00190-002-0216-2      *
! *                                                                      *
! *   3) T. Fukushima, "Numerical computation of spherical harmonics     *
! *      of arbitrary degree and order by extending exponent of          *
! *      floating point numbers", (2012), J. Geodesy, 86, 271-285,       *
! *      10.1007/s00190-011-0519-2.                                      *
! *                                                                      *
! *   4) M.A. Wieczorek, " SHTOOLS -- Tools for working with spherical   *
! *      harmonics", http://shtools.ipgp.fr/                             *
! *                                                                      *
! *   Copyright (c) 2004-2012, Mark A. Wieczorek                         *
! *   All rights reserved.                                               *
! *                                                                      *
! * ## 17-AUG-2012 SPHE_INV_2NN v3.0 modified by L. Petrov 19-OCT-2015 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      REAL*8     SPHE_INV_2NN 
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  N, MD, NORM, IPHS, DEG, IUER
      REAL*8     FUN(2*N,N), SPH(2,0:MD,0:MD)
      CHARACTER  STR*128, STR1*128
      REAL*8     SCALEF 
      INTEGER*4  DEG_MAX
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
      IF ( .NOT. PROBE_READ_ADDRESS(FSH) ) THEN
           CALL ERR_LOG ( 6501, IUER, 'SPHE_INV_2NN', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6502, IUER, 'SPHE_INV_2NN', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
      IF ( DEG < 1 .OR. DEG > FSH__MAX_DEG ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG, STR )
           CALL INCH  ( FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6503, IUER, 'SPHE_INV_2NN', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
      IF ( N < 2 .OR. MD > 2*FSH__MAX_DEG ) THEN
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 6504, IUER, 'SPHE_INV_2NN', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, 2880] was expected' )
           RETURN 
      END IF
      IF ( DEG > DEG_MAX ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( N,   STR )
           CALL INCH  ( DEG, STR1 )
           CALL ERR_LOG ( 6505, IUER, 'SPHE_INV_2NN', 'Either N is '// &
     &          'too big or DEG is too small. It should be DEG .LE. N/2 - 1'// &
     &          ' N= '//STR(1:I_LEN(STR))//' DEG= '//STR1 )
           RETURN 
      END IF 
      IF ( N .NE. 2*(DEG+1) ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( N,   STR )
           CALL INCH  ( DEG, STR1 )
           CALL ERR_LOG ( 6506, IUER, 'SPHE_INV_2NN', 'Either N is '// &
     &          'too big or DEG is too small. It should be N == 2*(DEG+1) '// &
     &          'N= '//STR(1:I_LEN(STR))//' DEG= '//STR1 )
           RETURN 
      END IF 
!
      IF ( NORM < 1 .OR. NORM > 4 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NORM, STR )
           CALL ERR_LOG ( 6507, IUER, 'SPHE_INV_2NN', 'Wrong value '// &
     &         'of NORM: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, 4] was expected' )
           RETURN 
      END IF
!
      IF ( IPHS .NE. 1 .AND. IPHS .NE. -1 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( IPHS, STR )
           CALL ERR_LOG ( 6508, IUER, 'SPHE_INV_2NN', 'Wrong value '// &
     &         'of IPHS: '//STR(1:I_LEN(STR))//' -- either 1 or -1 '// &
     &         'was expected' )
           RETURN 
      END IF
      CALL ERR_PASS ( IUER, IER  )
!
! --- First check debugging variables and decide whether we have to force
! --- F-version of X-versions. This is only for debugging purposes!
!
      IF ( FSH%FF_FORCE_F == FSH__ENFORCE ) THEN
           WRITE ( 6, '(A)' ) 'SPHE_INV_2NN: F-version'
           CALL SPHE_INV_2NN_F ( FSH, MD, DEG, NORM, IPHS, SPH, N, FUN, IER )
           CALL ERR_LOG ( IER, IUER )
           RETURN
         ELSE IF ( FSH%FF_FORCE_X == FSH__ENFORCE ) THEN
           WRITE ( 6, '(A)' ) 'SPHE_INV_2NN: X-version'
           CALL SPHE_INV_2NN_X ( FSH, MD, DEG, NORM, IPHS, SPH, N, FUN, IER )
           CALL ERR_LOG ( IER, IUER )
           RETURN
      END IF

      IF ( DEG .LE. FSH__MAX_SCL ) THEN
           CALL SPHE_INV_2NN_F ( FSH, MD, DEG, NORM, IPHS, SPH, N, FUN, IER )
         ELSE
           CALL SPHE_INV_2NN_X ( FSH, MD, DEG, NORM, IPHS, SPH, N, FUN, IER )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  FUNCTION  SPHE_INV_2NN  !#!#
