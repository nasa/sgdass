      PROGRAM    FOURPACK_EXAMPLE_03
! ************************************************************************
! *                                                                      *
! *   Progrma FOURPACK_EXAMPLE_03 provides an example of inverse vector  *
! *   spherical harmonics transform.                                     *
! *                                                                      *
! * ## 27-APR-2016 FOURPACK_EXAMPLE_03 v1.0 (c) L. Petrov 27-APR-2016 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*8  FSH
      CHARACTER  FFTW_PLAN_FILE*128, STR*128
      INTEGER*4  NLAT, DEG
      REAL*8,    ALLOCATABLE :: FUN(:,:,:), INV_FUN(:,:), SPH(:,:,:.:)
      INTEGER*4  J1, J2, IS, IND, INDS(2), IPHS, MODE, NTHR, IUER
      LOGICAL*4  IS_R8_NAN
      INTEGER*8, EXTERNAL :: SPHE_INIT_PLAN 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Define the grid (NLAT), number of threads (NTHR),
! --- the phase convention (IPHS), and the FFTW plan file FFTW_PLAN_FILE
! --- NB: the plan file depends on the number of threads.
!
      NLAT =  2001
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
! --- NB: SPHE_INV_2NN_VEC uses the grid that includes both southern and northern
! --- poles. Therefore, NLON = (NLAT-1)*2
!
      NLON = 2*(NLAT-1)
      DEG  = NLAT/2 -1
!
! --- Initialization for spherical harmonics package for NTHR threads
!
      IUER = -1
      FSH = SPHE_INIT_PLAN ( FFTW_PLAN_FILE, 1, 2.0D0, NTHR, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6001, IUER, 'FOURPACK_EXAMPLE_03', 'Error in an '// &
     &         'attempt to initialize FSH object for the spherical '// &
     &         'harmonics transform'  )
           CALL EXIT ( 1 )
      END IF   
!
! --- Allocate memory
!
      ALLOCATE ( INV_FUN(NLON,NLAT,3) )
      ALLOCATE ( SPH(2,0:DEG,0:DEG,2) )
!
! --- Fill the array of the spherical harmonics.
! --- The first dimesion of SPH runs over cosine/sine component, the second
! --- dimension runs over order degree m, the third dimension runs over order l, 
! --- and the fourth dimension runs over 1 and 2. In general, the contents of 
! --- a section over first three dimensions can be different.
! --- That means that the inverse harmonics transform is done over one set of harmonics
! --- but partial derivatives over longitude and latitude are computed for another 
! --- set of harmonics. If it is undersirable, you can put the same set of harmonics
! --- into sections over the fourth dimensions 1 and 2
! --- 
! --- NB: Only coefficients l =< m are filled! part of array SPH l > m is 
! --- filled with zeroes.
!
      DO 410 J1=0,DEG
         DO 420 J2=J1,DEG
            SPH(1,J2,J1,1) =   J1 + J2/10.0
            SPH(2,J2,J1,1) =   J1 + J2/20.0
            SPH(1,J2,J1,2) = 2*(J1 + J2/10.0)
            SPH(2,J2,J1,2) = 2*(J1 + J2/20.0)
 420     CONTINUE 
 410  CONTINUE 
!
! --- Perform inverse spherical harmonics transform of degree DEG. The result
! --- is written in array INV_FUN defined on a uniform longitude/latitude grid.
!
      IUER = -1
      CALL SPHE_INV_2NN_VEC ( %VAL(FSH), DEG, DEG, 1, IPHS, SPH, N, INV_FUN, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6003, IUER, 'FOURPACK_EXAMPLE_03', 'Error in an '// &
     &         'attempt to compute inverse spherical harmonics transform'
     &         harmonics transform'  )
           CALL EXIT ( 1 )
      END IF   
!
! --- Release memory used for spherical harmonics transform
!
      CALL SPHE_QUIT ( %VAL(FSH) )
      END  PROGRAM    FOURPACK_EXAMPLE_03  !#!#
