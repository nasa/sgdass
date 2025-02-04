      SUBROUTINE SPHE_DIR_2NN ( FSH, N, FUN, MD, DEG, NORM, IPHS, SPH, IUER )
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
! *   In a case if the dimension of transform is less or equal           *
! *   FSH__MAX_SCL, then the Legendre associated functions are computed  *
! *   on the fly  using the scaling methodology presented in Holmes and  *
! *   Featherston (2002). When NORM is 1,2 or 4, these are accurate to   *
! *   about degree 2800. When NORM is 3, the routine is only stable to   *
! *   about degree 15. In a case if the dimension of transform is        *
! *   greater than FSH__MAX_SCL=2700, then the Legendre functions are    *
! *   computed on the fly using the X-number algebra introduced by       *
! *   Fukushima (2011). The essence of that approach is that the         *
! *   intermediate variables needed for computation of Legendre          *
! *   associated functions are kept in two numbers: the double precision *
! *   float number and integer number for additional exponent.           *
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
! *   1. This routine does not use the fast Legendre transforms that     *
! *      are presented in Driscoll and Heally (1994).                    *
! *   2. Use of a N by 2N grid is implemented because many geographic    *
! *      grids are sampled this way. When taking the Fourier transforms  *
! *      in longitude, all of the higher frequencies are ultimately      *
! *      discarded. If, instead, every other column of the grid were     *
! *      discarded to form a NxN grid, higher frequencies could be       *
! *      aliased into lower frequencies.                                 *
! *                                                                      *
! *   Timing using E5-2660-v3 (in seconds):                              *
! *                                                                      *
! *    New:           F-version          X-version                       *
! *                                                                      *
! *    Lat    Deg     1-thr  16-thr                          wall 16-th  *
! *                                                           f       x  *
! *                                                                      *
! *   1025    511     0.32    0.036       0.60    0.048    0.328   0.059 *
! *   2049   1023     2.88    0.25        4.37    0.39     0.49    0.54  *  
! *   4097   2047    21.31    2.19       34.74    3.02     4.54    3.48  *
! *   5401   2699    48.15    5.32       81.91    9.20     9.11    9.09  *
! *   8193   4095      -       -        278.78   23.14      -     29.5   *
! *  10801   5399      -       -        716.03   58.68      -     66.8   *      
! *  16385   8191      -       -       2188.0   182.78      -    229.4   *
! *  18001   8999      -       -                262.69      -    330.4   *
! * (21601  10799      -       -                430.74      -    563.6)  *
! *  21601  10799      -       -                493.0       -    522.8   *
! *  43201  21599      -       -               4105.6           3812.6   *
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
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  SPH ( REAL*8    ) -- Array with spherical transform coefficients.   *
! *                       Dimension: (2,0:MD,0:MD). The first dimesion   *
! *                       runs over cosine/sine component, the second    *
! *                       dimension runs over order degree m, the third  *
! *                       dimension runs over order l.                   *
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
! * ## 08-AUG-2012 SPHE_DIR_2NN v2.0 modified by L. Petrov 19-OCT-2015 # *
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
      REAL*8     SCALEF 
      INTEGER*4  DEG_MAX
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, I_EQ, I_S, KD, NUM_THR_SAVED, NTHR, IER
      COMPLEX*16 CC(N+1)
      INTEGER*8  PLAN
      REAL*16    ALF_16, UM_16, ALF_SQ, ALF_ORD_SQ(0:FSH__MAX_DEG)
      REAL*8     GRIDL(2*N), THETA, Z, U, FCOEF1(2,N/2+1), FCOEF2(2,N/2+1), &
     &           PM1, PM2, P, PMM, RESCALEM, PROD, FFC_ODD(2), FFC_EVEN(2), &
     &           SCL_ARR(DEG), PMM_ARR(0:DEG)
      LOGICAL*1  FL_ERROR, FL_PARAL
      CHARACTER, EXTERNAL :: GET_CDATE_MS*23
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS, OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!
      DEG_MAX = N/2
      IF ( .NOT. PROBE_READ_ADDRESS(FSH) ) THEN
           CALL ERR_LOG ( 6411, IUER, 'SPHE_DIR_2NN', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6412, IUER, 'SPHE_DIR_2NN', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
      IF ( DEG < 1 .OR. DEG > FSH__MAX_DEG ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG, STR )
           CALL INCH  ( FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6413, IUER, 'SPHE_DIR_2NN', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
      IF ( N < 2 .OR. MD > 2*FSH__MAX_DEG ) THEN
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 6414, IUER, 'SPHE_DIR_2NN', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, 2880] was expected' )
           RETURN 
      END IF
      IF ( DEG > DEG_MAX ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( N,   STR )
           CALL INCH  ( DEG, STR1 )
           CALL ERR_LOG ( 6415, IUER, 'SPHE_DIR_2NN', 'Either N is '// &
     &          'too big or DEG is too small. It should be DEG .LE. N/2 - 1'// &
     &          ' N= '//STR(1:I_LEN(STR))//' DEG= '//STR1 )
           RETURN 
      END IF 
!
      IF ( NORM < 1 .OR. NORM > 4 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NORM, STR )
           CALL ERR_LOG ( 6416, IUER, 'SPHE_DIR_2NN', 'Wrong value '// &
     &         'of NORM: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, 4] was expected' )
           RETURN 
      END IF
!
      IF ( IPHS .NE. 1 .AND. IPHS .NE. -1 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( IPHS, STR )
           CALL ERR_LOG ( 6417, IUER, 'SPHE_DIR_2NN', 'Wrong value '// &
     &         'of IPHS: '//STR(1:I_LEN(STR))//' -- either 1 or -1 '// &
     &         'was expected' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER  )
!
! --- First check debugging variables and decide whether we have to force
! --- F-version of X-versions. This is only for debugging purposes!
!
      IF ( FSH%FF_FORCE_F == FSH__ENFORCE ) THEN
           WRITE ( 6, '(A)' ) 'SPHE_DIR_2NN: F-version'
           CALL SPHE_DIR_2NN_F ( FSH, N, FUN, MD, DEG, NORM, IPHS, SPH, IER )
           CALL ERR_LOG ( IER, IUER )
           RETURN
         ELSE IF ( FSH%FF_FORCE_X == FSH__ENFORCE ) THEN
           WRITE ( 6, '(A)' ) 'SPHE_DIR_2NN: X-version'
           CALL SPHE_DIR_2NN_X ( FSH, N, FUN, MD, DEG, NORM, IPHS, SPH, IER )
           CALL ERR_LOG ( IER, IUER )
           RETURN
      END IF
!
! --- If the dimension of the transform is below the threshold,
! --- use the F-version. Otherwise, use the X-version of the transform
!
      IF ( DEG .LE. FSH__MAX_SCL ) THEN
           CALL SPHE_DIR_2NN_F  ( FSH, N, FUN, MD, DEG, NORM, IPHS, SPH, IER )
         ELSE 
           CALL SPHE_DIR_2NN_X ( FSH, N, FUN, MD, DEG, NORM, IPHS, SPH, IER )
      END IF
!      
      CALL ERR_LOG ( IER, IUER )
      RETURN
      END  SUBROUTINE  SPHE_DIR_2NN  !#!#
