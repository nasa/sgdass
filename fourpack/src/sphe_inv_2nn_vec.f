      SUBROUTINE SPHE_INV_2NN_VEC ( FSH, MD, DEG, SPH, N, FUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPHE_INV_2NN_VEC  evaluates function FUN and its partial  *
! *   derivatives over longitude and latitude on a regular lon/lat       *
! *   grid 2(N-1)*N given its spherical harmonics transform and its      *
! *   tangential derivative.                                             *
! *                                                                      *
! *   Four-dimensional array SPH has the first dimension 1:2 that        *
! *   runs over cosine and sine components, second dimension 0:DEG       *
! *   runs over degree, third dimension 0:DEG runs over order, and       *
! *   the fourth dimension 1:2 runs over spherical harmonics and its     *
! *   tangential constituent. SPH contains components nm and mn of       *
! *   degree/order: SPH(a,c,b,d) = SPH(a,b,c,d). The so-called           *
! *   tangential constituent is the Spherical Harmonics Transform of     *
! *   a function that may be different than FUN.                         *
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
! *   This is the inverse of the routine SPHE_DIR_2NN, both of which are *
! *   done quickly using FFTs for each degree of each latitude band.     *
! *   The number of samples is determined by the spherical harmonic      *
! *   bandwidth DEG may be equal or less than MD.                        *
! *                                                                      *
! *   Note that N is always ODD for this routine.                        *
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
! *   Geodesy normalization for spherical harmonics us used.             *
! *                                                                      *
! *   The output grid contains N samples in latitude from -90 to         *
! *   +90 deg interval, including both poles, and in longitude from      *
! *   0 to 360-1*interval (or 2(N-1) x N), where interval is the         *
! *   sampling interval, and n=2*(deg+1)+1.                              *
! *                                                                      *
! *   NB: the grid includes both poles.                                  *
! *                                                                      *
! *   Timing using E5-2660-v3 (in seconds):                              *
! *                                                                      *
! *    New:           F-version          X-version                       *
! *                                                                      *
! *    Lat    Deg     1-thr  16-thr                          wall 16-th  *
! *                                                           f       x  *
! *                                                                      *
! *   1025    511     1.35    0.29        1.82    0.27      0.30    0.35 *
! *   2049   1023     8.23    0.94       11.74    0.77      1.08    1.4  *
! *   4097   2047    60.00    5.40       81.80    4.43      7.0     7.5  *        
! *   5401   2699   129.52   11.08      178.12    9.08     14.7    16.9  *
! *   8193   4095                                30.25       -     59.0  *
! *  10801   5399                      1338.4    65.01       -    119.5  *
! *  16385   8191                               239.53       -    450.0  *
! *  21601  10799                               518.43            879.6  *
! *  43201  21599                              8853.4            8853.4  *
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
! *   This code is based in SHTOOLS software by Mark A. Wieczorek.       *
! *                                                                      *
! * ### 11-FEB-2015  SPHE_INV_2NN_VEC v3.0 (c) L. Petrov 16-OCT-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INCLUDE   'fourpack_constants.i'
      INCLUDE   'fftw3.f'
      TYPE     ( SPHE_TYPE ) :: FSH
      INTEGER*4  N, MD, DEG, IUER
      REAL*8     FUN(2*(N-1),N,3), SPH(2,0:MD,0:MD,2)
      CHARACTER  STR*128, STR1*128
      REAL*8     SCALEF, EPS
      PARAMETER  ( EPS = 1.D-12 )
      INTEGER*4  DEG_MAX
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
      DEG_MAX = N/2
      IF ( .NOT. PROBE_READ_ADDRESS(FSH) ) THEN
           CALL ERR_LOG ( 6541, IUER, 'SPHE_INV_2NN_VEC', 'Adddress for '// &
     &         'data structure FSH is not readable' )
           RETURN 
      END IF
      IF ( FSH%STATUS .NE. FSH__INIT .AND. FSH%STATUS .NE. FSH__ALLO ) THEN
           CALL ERR_LOG ( 6542, IUER, 'SPHE_INV_2NN_VEC', 'Data structure '// &
     &         'FSH has not been initialized' )
           RETURN 
      END IF
      IF ( DEG < 1 .OR. DEG > FSH__MAX_DEG ) THEN
           CALL CLRCH ( STR ) 
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG, STR )
           CALL INCH  ( FSH__MAX_DEG, STR1 )
           CALL ERR_LOG ( 6543, IUER, 'SPHE_INV_2NN_VEC', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//'] was expected' )
           RETURN 
      END IF
      IF ( N < 2 .OR. MD > 2*FSH__MAX_DEG ) THEN
           CALL INCH  ( DEG, STR )
           CALL ERR_LOG ( 6544, IUER, 'SPHE_INV_2NN_VEC', 'Wrong value '// &
     &         'of DEG: '//STR(1:I_LEN(STR))//' -- a value in range '// &
     &         '[1, 2880] was expected' )
           RETURN 
      END IF
      IF ( DEG > DEG_MAX ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( N,   STR )
           CALL INCH  ( DEG, STR1 )
           CALL ERR_LOG ( 6545, IUER, 'SPHE_INV_2NN_VEC', 'Either N is '// &
     &          'too big or DEG is too small. It should be DEG .LE. N/2 - 1'// &
     &          ' N= '//STR(1:I_LEN(STR))//' DEG= '//STR1 )
           RETURN 
      END IF 
!
      IF ( DEG > MD ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( DEG,   STR )
           CALL INCH  ( MD, STR1 )
           CALL ERR_LOG ( 6546, IUER, 'SPHE_INV_2NN_VEC', 'Either DEG is '// &
     &          'too big or MD is too small. It should be MD =< DEG '// &
     &          'DEG= '//STR(1:I_LEN(STR))//' MD= '//STR1 )
           RETURN 
      END IF 
!
      IF ( N-1 > 2*(MD+1) ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( N,   STR )
           CALL INCH  ( MD, STR1 )
           CALL ERR_LOG ( 6547, IUER, 'SPHE_INV_2NN_VEC', 'Either N is '// &
     &          'too big or MD is too small. It should be N =< 2*(MD+1) '// &
     &          'N= '//STR(1:I_LEN(STR))//' MD= '//STR1 )
           RETURN 
      END IF 
!
!           write ( 6, * ) 'sphe_inv_2nn_vec shape=', shape(fun), shape(sph) ! %%%
!           write ( 6, * ) 'sphe_inv_2nn_vec n = ', n, ' md= ', md, ' deg = ', deg, FSH__MAX_SCL ! %%%%
      CALL ERR_PASS ( IUER, IER  )
      IF ( FSH%FF_FORCE_F == FSH__ENFORCE ) THEN
           WRITE ( 6, '(A)' ) 'SPHE_INV_2NN_VEC: F-version'
           CALL SPHE_INV_2NN_VEC_F ( FSH, MD, DEG, SPH, N, FUN, IER )
           CALL ERR_LOG ( IER, IUER )
           RETURN
         ELSE IF ( FSH%FF_FORCE_X == FSH__ENFORCE ) THEN
           WRITE ( 6, '(A)' ) 'SPHE_INV_2NN_VEC: X-version'
           CALL SPHE_INV_2NN_VEC_X ( FSH, MD, DEG, SPH, N, FUN, IER )
           CALL ERR_LOG ( IER, IUER )
           RETURN
      END IF
!
      IF ( DEG .LE. FSH__MAX_SCL ) THEN
           CALL SPHE_INV_2NN_VEC_F ( FSH, MD, DEG, SPH, N, FUN, IER )
         ELSE
           CALL SPHE_INV_2NN_VEC_X ( FSH, MD, DEG, SPH, N, FUN, IER )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  SPHE_INV_2NN_VEC !#!#
