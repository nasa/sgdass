      FUNCTION   SPHE_INIT ( NUM_THR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPHE_INIT initializes internal data structure for          *
! *   consecutive computation of direct or inverse spherical transform.  *
! *   It returns the address of the internal data strcuture and performs *
! *   some initializations.                                              *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * NUM_THR ( INTEGER*4 ) -- the number of threads used in subsequent    *
! *                          computations. If NUN_THR == -1, then        *
! *                          the number of threads is read from the      *
! *                          enviroment variable OMP_NUM_THREADS.        *
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
! * ###   08-AUG-2012   SPHE_INIT  v1.0  (c)  L. Petrov 14-AUG-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      ADDRESS__TYPE :: SPHE_INIT 
      INTEGER*4  NUM_THR, IUER, NUM_THR_USED
      CHARACTER  STR*32, NUM_THR_STR*32
      TYPE     ( SPHE_TYPE ) :: DUMMY_FSH
      ADDRESS__TYPE :: MEM_LEN
      INTEGER*4  IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      MEM_LEN = LOC(DUMMY_FSH%STATUS) - LOC(DUMMY_FSH%FIRST) + &
     &          SIZEOF(DUMMY_FSH%STATUS) 
      CALL GET_MEM   ( MEM_LEN+1024*1024, SPHE_INIT )
      IF ( SPHE_INIT  == 0 ) THEN
           CALL ERR_LOG ( 6111, IUER, 'SPHE_INIT', 'Failure '// &
     &         'in an attempt to allocate dynanic memory for '// &
     &         'F_SHTOOLS' )
           RETURN 
      END IF
      IF ( NUM_THR .LE. 0 ) THEN
           CALL GETENVAR ( 'OMP_NUM_THREADS', NUM_THR_STR )
           IF ( ILEN(NUM_THR_STR) > 0 ) THEN
!
! ------------- Well, OMP_NUM_THREADS was setup. Read it.
!
                CALL CHIN ( NUM_THR_STR, NUM_THR_USED )
                IF ( NUM_THR_USED < 1 ) NUM_THR_USED = 1
              ELSE 
                NUM_THR_USED = 1
           END IF
         ELSE 
           NUM_THR_USED = NUM_THR
      END IF
!
      CALL SPHE_INIT2 ( %VAL(SPHE_INIT), 0, NUM_THR_USED )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  SPHE_INIT  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPHE_INIT_PLAN ( WISDOM_FILE, FFTW_MODE, FFTW_TIMEOUT, &
     &                           NTHREADS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPHE_INIT_PLAN initialize internal data structure for      *
! *   consecutive computation of a fast Fourier transform or direct or   *
! *   inverse spherical transform. It returns the address of the         *
! *   internal data strcuture and performs some initializations.         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * WISDOM_FILE ( CHARACTER ) -- Name of the so-called wisdom file that  *
! *                              contains tune-up parameters of FFTW     *
! *                              library. The file name may be blank --  *
! *                              in that case no wisdom is acquiured.    *
! *   FFTW_MODE ( INTEGER*4 ) -- Used mode of FFTW. The following modes  *
! *                              are supported:                          *
! *                              0 -- ESTIMATE -- provides a reasonable  *
! *                                   speed for execution and very fast  *
! *                                   plan evaluation. Recommended for   *
! *                                   using unless, an evidence exists   *
! *                                   that other modes will be faster.   *
! *                              1 -- MEASURE -- may faster than         *
! *                                   ESTIMATE, but may be 100 times     *
! *                                   slower for startup. Recommended    *
! *                                   only with a wisdom file.           *
! *                              2 -- PATIENT -- may faster than         *
! *                                   ESTIMATE, nut may be 1000 times    *
! *                                   slower for startup. Recommended    *
! *                                   only with a wisdom file.           *
! *                              3 -- EXHAUSTIVE -- may faster than      *
! *                                   ESTIMATE, but it may take 10000    *
! *                                   times slower for a startup.        *
! *                                   You should be crazy if you try it  *
! *                                   without a wisdom file.             *
! * FFTW_TIMEOUT ( REAL*8    ) -- Maximum amount of time in seconds for  *
! *                               creation of plan. FFTW may be so slow  *
! *                               in desifing which plan to use that you *
! *                               will certainly want to limit this time.*
! *                               0.01 is good value for dimensions      *
! *                               less than 65536.                       *
! *     NTHREADS ( INTEGER*4 ) -- The number of threads to be used.      *
! *                               Value 0 means no threads. Value -1     *
! *                               means to get the number of threads     *
! *                               from the environment vairable          *
! *                               OMP_NUM_THREADS. NB: FFTW wisdom       *
! *                               created for a non-thread enviroment    *
! *                               will be discarded if the number of     *
! *                               threads > 1.                           *
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
! * ###  12-FEB-2015  SPHE_INIT_PLAN v2.0 (c) L. Petrov 15-FEB-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      ADDRESS__TYPE :: SPHE_INIT_PLAN
      CHARACTER  WISDOM_FILE*(*)
      INTEGER*4  FFTW_MODE, NTHREADS, IUER
      REAL*8     FFTW_TIMEOUT
      CHARACTER  STR*32, NUM_THR_STR*32
      TYPE     ( SPHE_TYPE ) :: DUMMY_FSH
      ADDRESS__TYPE :: MEM_LEN
      INTEGER*4  NUM_THR_USED, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, INIT_FFTW 
!
      MEM_LEN = LOC(DUMMY_FSH%STATUS) - LOC(DUMMY_FSH%FIRST) + &
     &          SIZEOF(DUMMY_FSH%STATUS) 
      CALL GET_MEM   ( MEM_LEN+1024*1024, SPHE_INIT_PLAN )
      IF ( SPHE_INIT_PLAN  == 0 ) THEN
           CALL ERR_LOG ( 6151, IUER, 'SPHE_INIT_PLAN', 'Failure '// &
     &         'in an attempt to allocate dynanic memory for '// &
     &         'F_SHTOOLS' )
           RETURN 
      END IF
!
      NUM_THR_USED = INIT_FFTW ( WISDOM_FILE, FFTW_MODE, FFTW_TIMEOUT, &
     &                           NTHREADS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6152, IUER, 'SPHE_INIT_PLAN', 'Failure '// &
     &         'in an attempt initialize FFTW' )
!!           DEALLOCATE ( SPHE_INIT_NEW )
           RETURN 
      END IF
!
      CALL SPHE_INIT2 ( %VAL(SPHE_INIT_PLAN), FFTW_MODE, NUM_THR_USED )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  SPHE_INIT_PLAN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPHE_INIT2 ( FSH, FFTW_MODE, NUM_THR )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program SPHE_INIT2 perfoms intialization of the fields  *
! *   of the interal data structure FSH.                                 *
! *                                                                      *
! *  ### 22-AUG-2012   SPHE_INIT2  v1.2 (c)  L. Petrov  19-OCT-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'fourpack.i'
      INTEGER*4  FFTW_MODE, NUM_THR, IUER
      TYPE     ( SPHE_TYPE ) :: FSH
!
      FSH%NUM_THR = NUM_THR
      FSH%DEG  = 0
      FSH%NORM = 0
      FSH%AJ   => NULL()
      FSH%F1   => NULL()
      FSH%F2   => NULL()
      FSH%F3   => NULL()
      FSH%PL   => NULL()
      FSH%PLT  => NULL()
      FSH%DPLT => NULL()
      FSH%MSIN => NULL()
      FSH%MCOS => NULL()
!
      FSH%AJ_DIM     = FSH__INIT
      FSH%AJ_NORM    = FSH__INIT
      FSH%AJ_STATUS  = FSH__INIT
!
      FSH%FF_DEG     = FSH__INIT
      FSH%FF_NORM    = FSH__INIT
      FSH%FF_MODE    = FFTW_MODE
      FSH%FF_FORCE_F = FSH__UNDF
      FSH%FF_FORCE_X = FSH__UNDF
      FSH%FF_STATUS  = FSH__INIT
!
      FSH%PL_DEG     = FSH__INIT
      FSH%PL_NORM    = FSH__INIT
      FSH%PL_STATUS  = FSH__INIT
!
      FSH%MS_DEG     = FSH__INIT
      FSH%MS_STATUS  = FSH__INIT
!
      FSH%LAT        = 1.0D20
      FSH%LON        = 1.0D20
      FSH%STATUS     = FSH__INIT
!
      RETURN
      END  SUBROUTINE  SPHE_INIT2  !#!#
