      FUNCTION   GET_GEOID ( LAT_GDT, LON, FIL_BSPL_HEB, GEOID_BSPL_HEB, &
     &                       IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_GEOID  returns the height of geoid with respect to    *
! *   the reference WGS84 ellipsoid. If the function is called the       *
! *   firmst time, it reads the file with coefficients of B-spline       *
! *   coefficients GEOID_BSPL_HEB. When it is called the next time,      *
! *   it re-uses the coefficients stored in object GEOID_BSPL_HEB.       *
! *   Program GEN_EGM2008_GEOID from package MALO gerenates such file    *
! *   with coefficients using EGM2008 1'x1' undulations.                 *
! *                                                                      *
! * ___________________________ Input parameters: ______________________ *
! *                                                                      *
! *        LAT_GDT ( REAL*8    ) -- Geodetic latitude in rad.            *
! *            LON ( REAL*8    ) -- Longitude in rad.                    *
! *   FIL_BSPL_HEB ( CHARACTER ) -- File that contains coefficients of   *
! *                                 B-spline expansion of geoid          *
! *                                 undulations.                         *
! * GEOID_BSPL_HEB ( HEB__TYPE ) -- Heb-object that keeps the expansion  *
! *                                 coefficients.                        *
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
! *  ### 20-NOV-2013   GET_GEOID   v1.0 (c)  L. Petrov  20-NOV-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: GEOID_BSPL_HEB
      REAL*8     LAT_GDT, LON
      CHARACTER  FIL_BSPL_HEB*(*) 
      REAL*8     GET_GEOID
      INTEGER*4  IUER
      INTEGER*4  MDEG
      PARAMETER  ( MDEG = 3 )
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-6 )
      CHARACTER  STR*128
      REAL*4     LON_R4
      INTEGER*4  NLON, NLAT, IND_LON, IND_LAT, J1, J2, IT, IER
      REAL*4,    EXTERNAL :: VAL_2D_BSPL4 
      INTEGER*4, EXTERNAL :: IXMN4 
!
      IF ( .NOT. ASSOCIATED ( GEOID_BSPL_HEB%VAL )   ) THEN
!
! -------- Read the HEB file with exapnsion coefficients
!
           CALL ERR_PASS ( IUER, IER )
           CALL READ_HEB ( FIL_BSPL_HEB, GEOID_BSPL_HEB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7241, IUER, 'GET_GEOID', 'Error in an '// &
     &              'attempt to read input file with B-spline '// &
     &              'coefficients of geoid undulations '//FIL_BSPL_HEB )
                RETURN 
           END IF
!
! -------- Extract the dimensions
!
           NLON = GEOID_BSPL_HEB%DIMS(1) - MDEG - 2
!
! -------- Allocate arrays with axes along longitude and latitude
!
           NLAT = GEOID_BSPL_HEB%DIMS(2) - MDEG
           ALLOCATE ( GEOID_BSPL_HEB%ARR_LON(NLON+2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7242, IUER, 'GET_GEOID', 'Error in an '// &
     &              'attempt to allocate dynamic memory for array '// &
     &              'ARR_LON' )
                RETURN 
           END IF
           ALLOCATE ( GEOID_BSPL_HEB%ARR_LAT(NLAT),   STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7243, IUER, 'GET_GEOID', 'Error in an '// &
     &              'attempt to allocate dynamic memory for array '// &
     &              'ARR_LAT' )
                RETURN 
           END IF
!
! -------- Fill arrays with axes along longitude and latitude
!
           DO 410 J1=1,NLON+2
              GEOID_BSPL_HEB%ARR_LON(J1) = (J1-1)*PI2/(NLON-1)
 410       CONTINUE 
!
           DO 420 J2=1,NLAT
              GEOID_BSPL_HEB%ARR_LAT(J2) = -P2I + (J2-1)*PI__NUM/(NLAT-1)
 420       CONTINUE 
         ELSE 
           NLON = GEOID_BSPL_HEB%DIMS(1) - MDEG - 2
           NLAT = GEOID_BSPL_HEB%DIMS(2) - MDEG
      END IF
!
! --- Resolve 2pi ambiguite in longitude an map it to [0, 2pi] range
!
      IT = IDINT ( LON/PI2 )
      LON_R4 = LON - IT*PI2
!
! --- Get pivotal indices along longitude and latitude
!
      IND_LON = IXMN4 ( NLON, GEOID_BSPL_HEB%ARR_LON, LON_R4 )
      IF ( LAT_GDT > PI2 + EPS ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:14), FMT='(1PD14.7)' ) LAT_GDT
           CALL ERR_LOG ( 7244, IUER, 'GET_GEOID', 'Wrong latitude '//STR )
           RETURN
        ELSE IF ( LAT_GDT < -PI2 - EPS ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:14), FMT='(1PD14.7)' ) LAT_GDT
           CALL ERR_LOG ( 7245, IUER, 'GET_GEOID', 'Wrong latitude '//STR )
           RETURN
        ELSE IF ( LAT_GDT > PI2 - EPS ) THEN
           IND_LAT = 1
        ELSE IF ( LAT_GDT < -PI2 + EPS ) THEN
           IND_LAT = NLAT - 1
        ELSE
           IND_LAT = IXMN4 ( NLAT, GEOID_BSPL_HEB%ARR_LAT, SNGL(LAT_GDT) )
      END IF
!
! --- Finally compute geoid height using coefficients of B-spline expansion
!
      GET_GEOID = VAL_2D_BSPL4 ( LON_R4, SNGL(LAT_GDT), NLON+2, NLAT, MDEG, &
     &                           IND_LON, IND_LAT, &
     &                           GEOID_BSPL_HEB%ARR_LON, &
     &                           GEOID_BSPL_HEB%ARR_LAT, &
     &                           GEOID_BSPL_HEB%VAL )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  GET_GEOID  !#!  
