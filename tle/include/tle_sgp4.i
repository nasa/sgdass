! >>>>> Include block for tle
! >>>>> 2023.03.30 (c)  N. Habana  v 2.2 2023.03.30_14:47:17
!
      INTEGER*4  EPH__MFIL
      PARAMETER  ( EPH__MFIL = 32*1024 )
!
      TYPE     EPH__TLE__TYPE 
           CHARACTER  SAT_NAM*24
           INTEGER*4  SAT_CAT
	   CHARACTER  SAT_CLASS
           INTEGER*4  LY		! Year of launch          
           INTEGER*4  LNY		! Launch number of the year
           CHARACTER  INT_DES*8       ! Committee of Space Research (COSPAR) Identifier aka International Designator
           INTEGER*4  MJD
           REAL*8     UTC
           REAL*8     UTC_MTAI          ! 
           CHARACTER  C_EPOCH*14	! Epoch in TLE format as a character
           REAL*8     EPOCH		! Epoch in TLE format
!@@!	   REAL*8     SEMI_MAJ		! Semi-major axis [er]. Not given in file
           REAL*8     ECC		! eccentricity    []
	   REAL*8     INC		! Inclination	  [rad]
	   REAL*8     AOP		! Arg. of perigee [rad]
	   REAL*8     RAN              	! right asc.	  [rad]
	   REAL*8     MA		! Mean Anomaly	  [rad]
	   REAL*8     MM		! Mean Motion	  [rad]
           REAL*8     MM_DOT		! 1st der. of Mean Motion	 [rad/min^2]
           REAL*8     MM_DOTDOT		! 2nd der. of Mean Motion == 0	 [] 
           REAL*8     BSTAR		! radiation pressure coefficient [1/er]
           INTEGER*4  NTLE              ! Number of TLE's for this object
           INTEGER*4  ET		! Ephemeris Type
	   INTEGER*4  NREV		! Number of full revolutions at epoch
      END      TYPE  EPH__TLE__TYPE
!
! ---------------------------------------------------------------------
!
      TYPE      EPH__TYPE
           CHARACTER  STN_NAM*8
           REAL*8     STN_COO(3)
           TYPE ( EPH__TLE__TYPE ), POINTER :: TLE(:) => NULL()          
           INTEGER*4  LSTATUS
      END       TYPE EPH__TYPE
!
! ---------------------------------------------------------------------
!
      INTEGER*4  EPH__LUNDF, EPH__INIT, EPH__LOAD
      PARAMETER  ( EPH__LUNDF = 0 )
      PARAMETER  ( EPH__LOAD = 1028931 )
      PARAMETER  ( EPH__INIT = 22831221 )

