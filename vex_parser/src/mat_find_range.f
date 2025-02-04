      SUBROUTINE  RNG_IDX ( IDX, CNT, A, LN, RNG, IFLAG, IUER )
!     
! ***************************************************************************
! *                                                                         *
! *   This function records the indices of the occurrences within a given   *
! *   range.                                                                *
! *                                                                         *
! *   N.B: This routine does not assume that the elements of A are sorted   *
! *        any way, therefore it will check for instances of RNG, given     *
! *        IFLAG, through the whole array.                                  *
! *                                                                         *
! *   INPUT:                                                                *
! *            CNT    =  Size of IDX              { INT }                   *
! *                                                                         *
! *            A      =  Array to be analysed     { REAL }                  *
! *                                                                         *
! *            LN     =  Length of A              { INT }                   *
! *                                                                         *
! *            RNG    =  boundary range           { REAL } [2x1]            *
! *                                                                         *
! *            IFLAG  =  Boundary flag            { INT }                   *
! *                      IFLAG = 0 -- Do not include any of the boundaries  *
! *                                       ( RNG(1), RNG(2) )                *
! *                            = 1 -- Only include the lower boundary       *
! *                                       [ RNG(1), RNG(2) )                *
! *                            = 2 -- Only include the upper boundary       *
! *                                       ( RNG(1), RNG(2) ]                *
! *                            = 3 -- Include both boundaries               *
! *                                       [ RNG(1), RNG(2) ]                *
! *                                                                         *
! *            IUER   =  Error flag               { INT }                   *
! *                                                                         *
! *   OUTPUT:                                                               *
! *            IDX    =  Indices array            { INT } [CNT-by-1]        *
! *                                                                         *
! * ### 19-AUG-2020    RNG_IDX   v1.0  (c)    N. Habana    19-AUG-2020 ###  *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT    NONE
      INTEGER*4   CNT, LN, IFLAG, IUER
      REAL*8      A(LN), RNG(2), RTEMP(2)
      INTEGER*4   IDX(CNT), J1, J2  
!
! --- Ensure that there is at least one variable that meets the threshold
!
      IF ( CNT .LT. 1 ) RETURN
!     
! --- Make sure that RNG(1) is always smaller that RNG(2)
!
      IF ( RNG(1) .GT. RNG(2) ) THEN
         RTEMP(1)  =  RNG(2)
         RTEMP(2)  =  RNG(1)
!
! ------ Swap the values
!
         RNG(1)   =  RTEMP(1)
         RNG(2)   =  RTEMP(2)
      END IF
!
      J2  =  0
      IF ( IFLAG == 0 )  THEN
!
! ------ Index the occurences excluding both boundaries 
!
         DO 100 J1 = 1, LN
            IF ( (A(J1) .GT. RNG(1)) .AND. (A(J1) .LT. RNG(2)) ) THEN
               J2       =  J2 + 1
               IDX(J2)  =  J1
            END IF               
 100     CONTINUE
      ELSE IF ( IFLAG == 1 ) THEN
!
! ------ Count the occurences excluding the end boundary 
!
         DO 101 J1 = 1, LN
            IF ( (A(J1) .GE. RNG(1)) .AND. (A(J1) .LT. RNG(2)) ) THEN
               J2       =  J2 + 1
               IDX(J2)  =  J1
            END IF               
 101     CONTINUE
      ELSE IF ( IFLAG == 2 ) THEN
!
! ------ Count the occurences excluding the initial boundary 
!
         DO 102 J1 = 1, LN
            IF ( (A(J1) .GT. RNG(1)) .AND. (A(J1) .LE. RNG(2)) ) THEN
               J2       =  J2 + 1
               IDX(J2)  =  J1
            END IF               
 102        CONTINUE
      ELSE IF ( IFLAG == 3 ) THEN
!
! ------ Count the occurences including both boundaries
!
         DO 103 J1 = 1, LN
            IF ( (A(J1) .GE. RNG(1)) .AND. (A(J1) .LE. RNG(2)) ) THEN
               J2       =  J2 + 1
               IDX(J2)  =  J1
            END IF               
 103     CONTINUE
      ELSE
         CALL ERR_LOG ( 3501, IUER, 'RNG_IDX',                          &
     &        'IFLAG value not one of permissable values:'//   &
     &        ' 0, 1, 2, and 3 ' )
         RETURN
      END IF
! 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE !#!#!
!
! ---------------------------------------------------------------------------
!
      SUBROUTINE  RNG_CNT ( CNT, A, LN, RNG, IFLAG, IUER )
!
! ***************************************************************************
! *                                                                         *
! *   This function computes the number of occurrences within a given range *
! *   as well as 
! *                                                                         *
! *   N.B: This routine does not assume that the elements of A are sorted   *
! *        any way, therefore it will check for instances of RNG (given     *
! *        IFLAG) through the whole array.                                  *
! *                                                                         *
! *   INPUT:                                                                *
! *            A      =  Array to be analysed     { REAL } [LN-by-1]        *
! *                                                                         *
! *            LN     =  Length of A              { INT }                   *
! *                                                                         *
! *            RNG    =  boundary range           { REAL } [2x1]            *
! *                                                                         *
! *            IFLAG  =  Boundary flag            { INT }                   *
! *                      IFLAG = 0 -- Do not include any of the boundaries  *
! *                                       ( RNG(1), RNG(2) )                *
! *                            = 1 -- Only include the lower boundary       *
! *                                       [ RNG(1), RNG(2) )                *
! *                            = 2 -- Only include the upper boundary       *
! *                                       ( RNG(1), RNG(2) ]                *
! *                            = 3 -- Include both boundaries               *
! *                                       [ RNG(1), RNG(2) ]                *
! *                                                                         *
! *            IUER   =  Error flag               { INT }                   *
! *                                                                         *
! *   OUTPUT:                                                               *
! *            CNT    =  No. of occurences        { INT }                   *
! *                                                                         *
! * ### 19-AUG-2020    RNG_CNT   v1.0  (c)    N. Habana    19-AUG-2020 ###  *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT    NONE
      INTEGER*4   CNT, LN, IFLAG, IUER
      REAL*8      A(LN), RNG(2), RTEMP(2)
      INTEGER*4   J1
!     
! --- Make sure that RNG(1) is always smaller that RNG(2)
!
      IF ( RNG(1) .GT. RNG(2) ) THEN
         RTEMP(1)  =  RNG(2)
         RTEMP(2)  =  RNG(1)
!
! ------ Swap the values
!
         RNG(1)   =  RTEMP(1) 
         RNG(2)   =  RTEMP(2)
      END IF
!
      CNT = 0
      IF ( IFLAG == 0 )  THEN
!
! ------ Count the occurences excluding both boundaries 
!
         DO 100 J1 = 1, LN
            IF ( (A(J1) .GT. RNG(1)) .AND. (A(J1) .LT. RNG(2)) ) THEN
               CNT  =  CNT + 1
            END IF               
 100     CONTINUE
      ELSE IF ( IFLAG == 1 ) THEN
!
! ------ Count the occurences excluding the end boundary 
!
         DO 101 J1 = 1, LN
            IF ( (A(J1) .GE. RNG(1)) .AND. (A(J1) .LT. RNG(2)) ) THEN
               CNT  =  CNT + 1
            END IF               
 101     CONTINUE
      ELSE IF ( IFLAG == 2 ) THEN
!
! ------ Count the occurences excluding the initial boundary 
!
         DO 102 J1 = 1, LN
            IF ( (A(J1) .GT. RNG(1)) .AND. (A(J1) .LE. RNG(2)) ) THEN
               CNT  =  CNT + 1
            END IF               
 102        CONTINUE
      ELSE IF ( IFLAG == 3 ) THEN
!
! ------ Count the occurences including both boundaries
!
         DO 103 J1 = 1, LN
            IF ( (A(J1) .GE. RNG(1)) .AND. (A(J1) .LE. RNG(2)) ) THEN
               CNT  =  CNT + 1
            END IF               
 103     CONTINUE
      ELSE
         CALL ERR_LOG ( 3601, IUER, 'RNG_CNT',                          &
     &        'IFLAG value not one of permissable values:'//   &
     &        ' 0, 1, 2, and 3 ' )
         RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE !#!#!#!
