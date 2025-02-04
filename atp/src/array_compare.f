      SUBROUTINE ARRAY_COMPARE ( N1, N2, A1, A2, A3, IUER )
!
! ************************************************************************
! *                                                                      *
! *   Routine  A_COM                                                   *
! *   Compares two integer arrays, by checking the elements of one       *
! *   array in the other.                                                *
! *   N.B: We assume the following:                                      *
! *        1. Elements of both arrays are sorted in ascending order.     *
! *        2. Each array has unique elements.                            *
! *        3. N1 .GE. N2                                                 *
! *                                                                      *
! *   INPUT:                                                             *
! *          N1      = Number of elements in A1      { INT*4 }         *
! *                                                                      *
! *          N2      = Number of elements in A2      { INT*4 }         *
! *                                                                      *
! *          A1      = Array 1                         { INT*4 } (N1x1)  *
! *                                                                      *
! *          A2      = Array 2                         { INT*4 } (N2x1)  *
! *                                                                      *
! *   OUTPUT:                                                            *
! *          A3     = Output array.                   { INT*4 } (N1x1)   *
! *                    if an element of A1 is also available in A2,      *
! *                    then the corresponding index in A3 will be        *
! *                    equal to 1, else it is equal to zero              *
! *                    e.g., given A1 = [ 1 2 3 4 5 6 ]                  *
! *                                A2 = [ 2 4 6 ]                        *
! *                          then, A3 = [ 0 1 0 1 0 1 ]                  *
! *                                                                      *
! * ###  23-OCT-2023   ARR_COM    v1.0 (c)  N. Habana  23-OCT-2023   ### *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
      INTEGER*4  N1, N2, A1(N1), A2(N2), A3(N1)
      INTEGER*4  IUER, J1, IFND
      INTEGER*4, EXTERNAL :: IFIND_PL
!     
! --- Make sure elements of both arrays are sorted
! 
      CALL SORT_I ( N1, A1 )
      CALL SORT_I ( N2, A2 )
!
! --- Make sure that N1 .GE. N2
!
      IUER = 0
      IF ( N1 .LT. N2 ) THEN
         IUER = -1
         CALL ERR_LOG ( 1, IUER, 'ARR_COM',                             &
     &                  'A1 is meant to be .GE. A2' )
         RETURN
      END IF
!     
! ------ Check if the J1 element of A1 is in A2
!
      DO 410 J1 = 1, N1
         IFND = -1               ! default
         IFND = IFIND_PL (N2, A2, A1(J1) )
!
! ------ If it's there then IFND will be the index, else IFND == -1
!
         IF ( IFND == -1 ) THEN
            A3(J1) = 0
         ELSE
            A3(J1) = 1
         END IF
 410  CONTINUE
! ---
      RETURN
      END SUBROUTINE !#! 1
