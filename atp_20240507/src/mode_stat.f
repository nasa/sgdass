      FUNCTION MODE_R8 ( N, X )
      
!
! *****************************************************************************
! *                                                                           *
! *   Function MODE_R8 outputs the mode of a real*8 type array, X, with N     *
! *   entries.                                                                *
! *   N.B: If more than one mode, pick the lowest value as mode.              *
! *                                                                           *
! *  INPUT:                                                                   *
! *              N      =  Array length                 { INT*4 }             *
! *                                                                           *
! *              X      =  Sorted Array to get mode of  { REAL*8 } [N-by-1]   *
! *                                                                           *
! *  OUTPUT:                                                                  *
! *           MODE_R8   =   mode                        { REAL*8 }            *
! *                                                                           *
! * ###   02-JAN-2024   MODE_R8       v1.0 (c)  N. Habana   02-JAN-2024   ### *
! *                                                                           *
! *****************************************************************************
!
      IMPLICIT    NONE
      INTEGER*4   N, J1, ICNT, ICURRENTCNT
      REAL*8      MODE_R8, X(N)
!
! --- Initialize
!
      MODE_R8 = X(1) 
      ICNT = 1
      ICURRENTCNT = 1
! ---
      DO 110 J1 = 2, N
!
! ------ We are going through the loop looking for values == X(J1-1)...
!
         IF ( X(J1) == X(J1-1) ) THEN
!
! --------- We spotted another X(J1-1), so increment the count.
!
            ICURRENTCNT = ICURRENTCNT + 1
         ELSE
!
! --------- There are no more X(J1-1)
            IF ( ICURRENTCNT > ICNT ) THEN
!
! ------------- There were more elements of value X(J1-1) than of
!               value MODE_R8
               ICNT = ICURRENTCNT
               MODE_R8 = X(J1-1)
            END IF
!
! ------------ Next we are looking for values == X(J1), so far we have spotted one...
!
            ICURRENTCNT = 1
         END IF
 110  CONTINUE
! ---      
      IF ( ICURRENTCNT > ICNT ) THEN
!
! ------ This means there are more elements of value X(N) than of value MODE_R8.
!
         MODE_R8 = X(N)
      END IF
! ---
      RETURN
      END FUNCTION !#!1
!
! ------------------------------------------------------------------------------------------
!
      FUNCTION MODE_R4 ( N, X )
      
!
! *****************************************************************************
! *                                                                           *
! *   Function MODE_R4 outputs the mode of a real*4 type array, X, with N     *
! *   entries.                                                                *
! *   N.B: If more than one mode, pick the lowest value as mode.              *
! *                                                                           *
! *  INPUT:                                                                   *
! *              N      =  Array length                 { INT*4 }             *
! *                                                                           *
! *              X      =  Sorted Array to get mode of  { REAL*4 } [N-by-1]   *
! *                                                                           *
! *  OUTPUT:                                                                  *
! *           MODE_R4   =   mode                        { REAL*4 }            *
! *                                                                           *
! * ###   02-JAN-2024   MODE_R4       v1.0 (c)  N. Habana   02-JAN-2024   ### *
! *                                                                           *
! *****************************************************************************
!
      IMPLICIT    NONE
      INTEGER*4   N, J1, ICNT, ICURRENTCNT
      REAL*4      MODE_R4, X(N)
!
! --- Initialize
!
      MODE_R4 = X(1) 
      ICNT = 1
      ICURRENTCNT = 1
! ---
      DO 110 J1 = 2, N
!
! ------ We are going through the loop looking for values == X(J1-1)...
!
         IF ( X(J1) == X(J1-1) ) THEN
!
! --------- We spotted another X(J1-1), so increment the count.
!
            ICURRENTCNT = ICURRENTCNT + 1
         ELSE
!
! --------- There are no more X(J1-1)
            IF ( ICURRENTCNT > ICNT ) THEN
!
! ------------- There were more elements of value X(J1-1) than of
!               value MODE_R4
               ICNT = ICURRENTCNT
               MODE_R4 = X(J1-1)
            END IF
!
! ------------ Next we are looking for values == X(J1), so far we have spotted one...
!
            ICURRENTCNT = 1
         END IF
 110  CONTINUE
! ---      
      IF ( ICURRENTCNT > ICNT ) THEN
!
! ------ This means there are more elements of value X(N) than of value MODE_R4.
!
         MODE_R4 = X(N)
      END IF
! ---
      RETURN
      END FUNCTION !#!2
      
