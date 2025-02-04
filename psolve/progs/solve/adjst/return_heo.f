      SUBROUTINE RETURN_HEO ( L_HEO, HEO, TIME_JD, HEO_EPOCH_SEC, UT1_M_TDB, &
     &                        DUT, DXWOB, DYWOB, DUTD, DXWOBD, DYWOBD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  RETURN_HEO  returns harmonic EOP variations and their     *
! *   rates and transforms them into units which ADJUST expects to see.  *
! *                                                                      *
! *  ### 02-OCT-2003   RETURN_HEO  v1.1 (c)  L. Petrov  12-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'heo.i'
      INTEGER*4  L_HEO, IUER
      TYPE     ( HEO__STRUC ) HEO(L_HEO)
      REAL*8     TIME_JD, HEO_EPOCH_SEC, DUT, DXWOB, DYWOB, DUTD, DXWOBD, &
     &           DYWOBD, UT1_M_TDB
      REAL*8     T_SEC, VEC_HEO(3), VEC_HEO_RATE(3) 
      INTEGER*4  IER
!
      T_SEC = ( TIME_JD - J2000__JD)*86400.0d0
!
! --- Compute harmonic EOP variations
!
      CALL ERR_PASS ( IUER, IER )
      CALL GET_HEO  ( T_SEC, HEO_EPOCH_SEC, UT1_M_TDB, L_HEO, HEO, VEC_HEO, &
     &                IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8331, IUER, 'RETURN_HEO', 'Error in computing '// &
     &         'harmonic EOP variations' )
           RETURN 
      END IF
!
! --- Compute rate of harmonic EOP variations
!
      CALL ERR_PASS     ( IUER, IER )
      CALL GET_HEO_RATE ( T_SEC, HEO_EPOCH_SEC, UT1_M_TDB, L_HEO, HEO, &
     &                    VEC_HEO_RATE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8332, IUER, 'RETURN_HEO', 'Error in computing '// &
     &         'rates of harmonic EOP variations' )
           RETURN 
      END IF
!
! --- Polar motion is in radians
!
      DXWOB  = VEC_HEO(2)
      DYWOB  = VEC_HEO(1)
!
! --- UT in seconds
!
      DUT    = -VEC_HEO(3)/(1000.0D0*MSEC__TO__RAD)/1.002737D0
!
! --- Polar motion rate is in radian/days
!
      DXWOBD = VEC_HEO_RATE(2)*86400.D0
      DYWOBD = VEC_HEO_RATE(1)*86400.D0
!
! --- Ut1 rate is in sec/day
!
      DUTD   = -VEC_HEO_RATE(3)/(1000.0D0*MSEC__TO__RAD)*86400.D0/1.002737D0
!
      RETURN
      END  !#!  RETURN_HEO  #!#
