      FUNCTION MEDIAN_R8 ( N, X )
!
! ***************************************************************************
! *                                                                         *
! *   Function MEDIAN_R8 outputs the median of a real*8 type array, X, with *
! *   N entries.                                                            *
! *                                                                         *
! *  INPUT:                                                                 *
! *              N      =  Array length                 { INT }             *
! *                                                                         *
! *              X      =  Array to get median of.      { REAL } [N-by-1]   *
! *                                                                         *
! *  OUTPUT:                                                                *
! *         MEDIAN_R8   =   median                      { REAL }            *
! *                                                                         *
! * ###   18-AUG-2020   MEDIAN_R8   v1.0 (c)  N. Habana   18-AUG-2020   ### *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT    NONE
      INTEGER*4   N, J1
      REAL*8      MEDIAN_R8, X(N), XTEMP(N)
!
      IF ( N .LT. 1 ) RETURN
!
      IF ( N .EQ. 1 ) THEN
         MEDIAN_R8 = X(1)       
         RETURN
      END IF
!     
! --- Copy the array X to a temporary array
!
      DO 110 J1 = 1, N
         XTEMP(J1) = X(J1)
 110  CONTINUE
!
! --- Sort the element of TEMP in ascending order
!
      CALL SORT_R8 ( N, XTEMP )
!
! --- Compute the median
!
      IF ( MOD(N,2) .EQ. 0 ) THEN       ! N is even
         MEDIAN_R8  =  ( XTEMP(N/2) + XTEMP((N/2)+1) )/2.D0
      ELSE                              ! N is odd
         MEDIAN_R8  =  XTEMP((N/2)+1)
      END IF
!
      RETURN
      END FUNCTION !#!#!#!#!

      
