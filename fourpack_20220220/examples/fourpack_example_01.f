      PROGRAM    FOURPACK_EXAMPLE_01
! ************************************************************************
! *                                                                      *
! *   Progrma FOURPACK_EXAMPLE_01 provides an example of direct and      *
! *   inverse spherical harmonics transform.                             *
! *                                                                      *
! * ## 27-APR-2016 FOURPACK_EXAMPLE_01 v1.0 (c) L. Petrov 27-APR-2016 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*8  FSH
      CHARACTER  FFTW_PLAN_FILE*128, STR*128
      INTEGER*4  NLAT, DEG
      REAL*8,    ALLOCATABLE :: FUN(:,:), INV_FUN(:,:), SPH(:,:,:)
      INTEGER*4  J1, J2, IS, IND, INDS(2), IPHS, MODE, NTHR, IUER
      LOGICAL*4  IS_R8_NAN
      INTEGER*8, EXTERNAL :: SPHE_INIT_PLAN 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Define the grid (NLAT), number of threads (NTHR),
! --- the phase convention (IPHS), and the FFTW plan file FFTW_PLAN_FILE
! --- NB: the plan file depends on the number of threads.
!
      NLAT =  2000
      NTHR = 16
      IPHS =  1
!
      CALL CLRCH ( STR )
      CALL INCH  ( NTHR, STR )
      FFTW_PLAN_FILE = '/progs/malo_20151228/share/malo_fftw_plan_'//STR(1:I_LEN(STR))//'thr.wis'
!
! --- Derive the resolution over longitude (twice greater than over latitude)
! --- and the degree of the transform.
!
      NLON = 2*NLAT
      DEG  = NLAT/2 -1
!
! --- Initialization for spherical harmonics package for NTHR threads
!
      IUER = -1
      FSH = SPHE_INIT_PLAN ( FFTW_PLAN_FILE, 1, 2.0D0, NTHR, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6001, IUER, 'FOURPACK_EXAMPLE_01', 'Error in an '// &
     &         'attempt to initialize FSH object for the spherical '// &
     &         'harmonics transform'  )
           CALL EXIT ( 1 )
      END IF   
!
! --- Allocate memory
!
      ALLOCATE ( FUN(NLON,NLAT) )
      ALLOCATE ( INV_FUN(NLON,NLAT) )
      ALLOCATE ( SPH(2,0:DEG,0:DEG) )
!
! --- Fill the array of the function to be transform
!
      DO 410 J1=1,N
         DO 420 J2=1,2*N
            FUN(J2,J1) = (J1-J2) + J2/100.0
 420     CONTINUE 
 410  CONTINUE 
!
! --- Perform direct spherical harmonics transform of a function defined on
! --- a uniform longitude/latitude grid. Result is put in array SPH.
! --- The first dimesion of SPH runs over cosine/sine component, the second
! --- dimension runs over order degree m, the third dimension runs over order l.
! --- NB: only coefficients l =< m are filled! part of array SPH l > m is 
! --- filled with zeroes.
!
      IUER = -1
      CALL SPHE_DIR_2NN ( %VAL(FSH), NLAT, FUN, DEG, DEG, 1, IPHS, SPH, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6002, IUER, 'FOURPACK_EXAMPLE_01', 'Error in an '// &
     &         'attempt to compute direct spherical harmonics transform'
     &         harmonics transform'  )
           CALL EXIT ( 1 )
      END IF   
!
! --- Perform inverse spherical harmonics transform of degree DEG. The result
! --- is written in array INV_FUN defined on a uniform longitude/latitude grid.
!
      IUER = -1
      CALL SPHE_INV_2NN ( %VAL(FSH), DEG, DEG, 1, IPHS, SPH, N, INV_FUN, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6003, IUER, 'FOURPACK_EXAMPLE_01', 'Error in an '// &
     &         'attempt to compute inverse spherical harmonics transform'
     &         harmonics transform'  )
           CALL EXIT ( 1 )
      END IF   
!
! --- Release memory used for spherical harmonics transform
!
      CALL SPHE_QUIT ( %VAL(FSH) )
      END  PROGRAM    FOURPACK_EXAMPLE_01  !#!#
