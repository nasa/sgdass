      SUBROUTINE ADD_TRG ( OC, SIGMA, &
     &                     N,  IND,     EQCON, &
     &                     M,  NOR_VEC, NOR_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_TRG  updates normal matrix and normal vector for the  *
! *   next observation. Vector of equation of conditions assumed to be   *
! *   sparse, but only non-zero elements are being passed in routine.    *
! *   Indices of non-zero elements are in the vector IND. Vector IND can *
! *   be unsorted, although if it is sorted the faster algorithm is used *
! *   what reduces processing time twice.                                *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      OC ( REAL*8     ) -- Right part of the equation of condition.   *
! *   SIGMA ( REAL*8     ) -- A priori uncertainty of the observations.  *
! *       N ( INTEGER*4  ) -- Number of non-zero elements in an equation *
! *                           of condition.                              *
! *     IND ( INTEGER*4  ) -- Vector of indices of non-zero elements.    *
! *   EQCON ( INTEGER*4  ) -- Squeezed vector of non-zero elements of an *
! *                           equation of condition. It contains only    *
! *                           non-zero elements. Dimension: N .          *
! *       M ( INTEGER*4  ) -- Total number of elements in an equation    *
! *                           of condition (including zero elements).    *
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! * NOR_VEC ( REAL*8     ) -- Normal vector. Dimension: M .              *
! * NOR_MAT ( REAL*8     ) -- Normal matrix. Dimension: M*(M+1)/2 .      *
! *                                                                      *
! *   IMPORTANT: Should be compiled with switch +O3 and without -C -g    *
! *                                                                      *
! *  ###   31-DEC-1996   ADD_TRG   v3.4 (c)  L. Petrov  10-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, IND(N), M
      REAL*8     OC, SIGMA, EQCON(N), NOR_VEC(M), NOR_MAT(*)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, MS
      PARAMETER  ( MS = 12        )
      INTEGER*8  LAI(MS)
      REAL*8     MAT_TMP((MS*(MS+1))/2)
      INTEGER*8, ALLOCATABLE :: LAJ(:)
      INTEGER*8  LA
      LOGICAL*4  FL_ORDER, ORDER_DENY
      PARAMETER  ( ORDER_DENY = .FALSE. ) ! .TRUE. used for debugging only
      REAL*8     WEI
#ifdef GNU
      INTRINSIC FLUSH
#endif
!
      FL_ORDER = .TRUE.
!
      IF ( N .LE. 0 ) THEN
           WRITE ( 6, * ) ' ADD_TRG: N= ', N 
           CALL FLUSH ( 6 )
           CALL MEMCPY ( %VAL(0), N, %VAL(4) )
      END IF
!
! --- First check are the indices in increasing order or not. We have two
! --- algorithms: one (fast) for the case when indices are sorted and another
! --- for the case when they are not sorted
!
      IF ( N .GT. 1 .AND. N .LE. MS ) THEN
           DO 410 J1=1,N-1
              IF ( IND(J1+1) .LE. IND(J1) ) FL_ORDER = .FALSE.
              LAI(J1) = (INT8(IND(J1))*INT8(IND(J1)-1))/2
 410       CONTINUE
           LAI(N) = (INT8(IND(N))*INT8(IND(N)-1))/2
        ELSE IF ( N ==  1 ) THEN
           LAI(1) = (INT8(IND(N))*INT8(IND(N)-1))/2
           IF ( .NOT. ORDER_DENY ) THEN
                ALLOCATE ( LAJ(1) )
                LAJ(1) = LAI(1)
           END IF
        ELSE IF ( N > MS ) THEN
           ALLOCATE ( LAJ(N) )
           DO 510 J1=1,N-1
              IF ( IND(J1+1) .LE. IND(J1) ) FL_ORDER = .FALSE.
              LAJ(J1) = (INT8(IND(J1))*INT8(IND(J1)-1))/2
 510       CONTINUE
           LAJ(N) = (INT8(IND(N))*INT8(IND(N)-1))/2
           LAI(1:MS) = LAJ(1:MS)
      END IF
      IF ( ORDER_DENY ) FL_ORDER = .FALSE. ! debugging feature
!
! --- Calculation of the weight of the observation
!
      WEI=1.D0/SIGMA**2
      IF ( FL_ORDER  ) THEN
!
! -------- The indices were in order. We don't need to make checks
!          ~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! -------- Cases of dimension  1-12
!
           IF ( N .EQ. 1 ) THEN
                NOR_MAT(LAI(1)+IND(1)) = NOR_MAT(LAI(1)+IND(1)) + &
     &                                   WEI*EQCON(1)*EQCON(1)
                NOR_VEC(IND(1)) = NOR_VEC(IND(1)) + WEI*EQCON(1)*OC
                RETURN
             ELSE IF ( N .EQ. 2 ) THEN
                LA = LAI(1) + IND(1)
                NOR_MAT(LA) = NOR_MAT(LA) + WEI*EQCON(1)*EQCON(1)
                LA = LAI(2) + IND(1)
                NOR_MAT(LA) = NOR_MAT(LA) + WEI*EQCON(2)*EQCON(1)
                LA = LAI(2) + IND(2)
                NOR_MAT(LA) = NOR_MAT(LA) + WEI*EQCON(2)*EQCON(2)
!
                NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
                NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
                RETURN
              ELSE IF ( N .EQ. 3 ) THEN
                MAT_TMP(1) = WEI*EQCON(1)*EQCON(1)
                MAT_TMP(2) = WEI*EQCON(2)*EQCON(1)
                MAT_TMP(3) = WEI*EQCON(2)*EQCON(2)
                MAT_TMP(4) = WEI*EQCON(3)*EQCON(1)
                MAT_TMP(5) = WEI*EQCON(3)*EQCON(2)
                MAT_TMP(6) = WEI*EQCON(3)*EQCON(3)
!
                NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
                NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
                NOR_VEC( IND(3) ) = NOR_VEC( IND(3) ) + WEI*EQCON(3)*OC
!
                NOR_MAT(LAI(1)+IND(1)) = NOR_MAT(LAI(1)+IND(1)) + MAT_TMP(1)
                NOR_MAT(LAI(2)+IND(1)) = NOR_MAT(LAI(2)+IND(1)) + MAT_TMP(2)
                NOR_MAT(LAI(2)+IND(2)) = NOR_MAT(LAI(2)+IND(2)) + MAT_TMP(3)
                NOR_MAT(LAI(3)+IND(1)) = NOR_MAT(LAI(3)+IND(1)) + MAT_TMP(4)
                NOR_MAT(LAI(3)+IND(2)) = NOR_MAT(LAI(3)+IND(2)) + MAT_TMP(5)
                NOR_MAT(LAI(3)+IND(3)) = NOR_MAT(LAI(3)+IND(3)) + MAT_TMP(6)
                RETURN
              ELSE IF ( N .EQ. 4 ) THEN
                MAT_TMP(1)  = WEI*EQCON(1)*EQCON(1)
                MAT_TMP(2)  = WEI*EQCON(2)*EQCON(1)
                MAT_TMP(3)  = WEI*EQCON(2)*EQCON(2)
                MAT_TMP(4)  = WEI*EQCON(3)*EQCON(1)
                MAT_TMP(5)  = WEI*EQCON(3)*EQCON(2)
                MAT_TMP(6)  = WEI*EQCON(3)*EQCON(3)
                MAT_TMP(7)  = WEI*EQCON(4)*EQCON(1)
                MAT_TMP(8)  = WEI*EQCON(4)*EQCON(2)
                MAT_TMP(9)  = WEI*EQCON(4)*EQCON(3)
                MAT_TMP(10) = WEI*EQCON(4)*EQCON(4)
!
                NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
                NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
                NOR_VEC( IND(3) ) = NOR_VEC( IND(3) ) + WEI*EQCON(3)*OC
                NOR_VEC( IND(4) ) = NOR_VEC( IND(4) ) + WEI*EQCON(4)*OC
!
                NOR_MAT(LAI(1)+IND(1)) = NOR_MAT(LAI(1)+IND(1)) + MAT_TMP(1)
                NOR_MAT(LAI(2)+IND(1)) = NOR_MAT(LAI(2)+IND(1)) + MAT_TMP(2)
                NOR_MAT(LAI(2)+IND(2)) = NOR_MAT(LAI(2)+IND(2)) + MAT_TMP(3)
                NOR_MAT(LAI(3)+IND(1)) = NOR_MAT(LAI(3)+IND(1)) + MAT_TMP(4)
                NOR_MAT(LAI(3)+IND(2)) = NOR_MAT(LAI(3)+IND(2)) + MAT_TMP(5)
                NOR_MAT(LAI(3)+IND(3)) = NOR_MAT(LAI(3)+IND(3)) + MAT_TMP(6)
                NOR_MAT(LAI(4)+IND(1)) = NOR_MAT(LAI(4)+IND(1)) + MAT_TMP(7)
                NOR_MAT(LAI(4)+IND(2)) = NOR_MAT(LAI(4)+IND(2)) + MAT_TMP(8)
                NOR_MAT(LAI(4)+IND(3)) = NOR_MAT(LAI(4)+IND(3)) + MAT_TMP(9)
                NOR_MAT(LAI(4)+IND(4)) = NOR_MAT(LAI(4)+IND(4)) + MAT_TMP(10)
                RETURN
              ELSE IF ( N .EQ. 5 ) THEN
                MAT_TMP(1)  = WEI*EQCON(1)*EQCON(1)
                MAT_TMP(2)  = WEI*EQCON(2)*EQCON(1)
                MAT_TMP(3)  = WEI*EQCON(2)*EQCON(2)
                MAT_TMP(4)  = WEI*EQCON(3)*EQCON(1)
                MAT_TMP(5)  = WEI*EQCON(3)*EQCON(2)
                MAT_TMP(6)  = WEI*EQCON(3)*EQCON(3)
                MAT_TMP(7)  = WEI*EQCON(4)*EQCON(1)
                MAT_TMP(8)  = WEI*EQCON(4)*EQCON(2)
                MAT_TMP(9)  = WEI*EQCON(4)*EQCON(3)
                MAT_TMP(10) = WEI*EQCON(4)*EQCON(4)
                MAT_TMP(11) = WEI*EQCON(5)*EQCON(1)
                MAT_TMP(12) = WEI*EQCON(5)*EQCON(2)
                MAT_TMP(13) = WEI*EQCON(5)*EQCON(3)
                MAT_TMP(14) = WEI*EQCON(5)*EQCON(4)
                MAT_TMP(15) = WEI*EQCON(5)*EQCON(5)
!
                NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
                NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
                NOR_VEC( IND(3) ) = NOR_VEC( IND(3) ) + WEI*EQCON(3)*OC
                NOR_VEC( IND(4) ) = NOR_VEC( IND(4) ) + WEI*EQCON(4)*OC
                NOR_VEC( IND(5) ) = NOR_VEC( IND(5) ) + WEI*EQCON(5)*OC
!
                NOR_MAT(LAI(1)+IND(1)) = NOR_MAT(LAI(1)+IND(1)) + MAT_TMP(1)
                NOR_MAT(LAI(2)+IND(1)) = NOR_MAT(LAI(2)+IND(1)) + MAT_TMP(2)
                NOR_MAT(LAI(2)+IND(2)) = NOR_MAT(LAI(2)+IND(2)) + MAT_TMP(3)
                NOR_MAT(LAI(3)+IND(1)) = NOR_MAT(LAI(3)+IND(1)) + MAT_TMP(4)
                NOR_MAT(LAI(3)+IND(2)) = NOR_MAT(LAI(3)+IND(2)) + MAT_TMP(5)
                NOR_MAT(LAI(3)+IND(3)) = NOR_MAT(LAI(3)+IND(3)) + MAT_TMP(6)
                NOR_MAT(LAI(4)+IND(1)) = NOR_MAT(LAI(4)+IND(1)) + MAT_TMP(7)
                NOR_MAT(LAI(4)+IND(2)) = NOR_MAT(LAI(4)+IND(2)) + MAT_TMP(8)
                NOR_MAT(LAI(4)+IND(3)) = NOR_MAT(LAI(4)+IND(3)) + MAT_TMP(9)
                NOR_MAT(LAI(4)+IND(4)) = NOR_MAT(LAI(4)+IND(4)) + MAT_TMP(10)
                NOR_MAT(LAI(5)+IND(1)) = NOR_MAT(LAI(5)+IND(1)) + MAT_TMP(11)
                NOR_MAT(LAI(5)+IND(2)) = NOR_MAT(LAI(5)+IND(2)) + MAT_TMP(12)
                NOR_MAT(LAI(5)+IND(3)) = NOR_MAT(LAI(5)+IND(3)) + MAT_TMP(13)
                NOR_MAT(LAI(5)+IND(4)) = NOR_MAT(LAI(5)+IND(4)) + MAT_TMP(14)
                NOR_MAT(LAI(5)+IND(5)) = NOR_MAT(LAI(5)+IND(5)) + MAT_TMP(15)
                RETURN
              ELSE IF ( N .EQ. 6 ) THEN
                MAT_TMP(1)  = WEI*EQCON(1)*EQCON(1)
                MAT_TMP(2)  = WEI*EQCON(2)*EQCON(1)
                MAT_TMP(3)  = WEI*EQCON(2)*EQCON(2)
                MAT_TMP(4)  = WEI*EQCON(3)*EQCON(1)
                MAT_TMP(5)  = WEI*EQCON(3)*EQCON(2)
                MAT_TMP(6)  = WEI*EQCON(3)*EQCON(3)
                MAT_TMP(7)  = WEI*EQCON(4)*EQCON(1)
                MAT_TMP(8)  = WEI*EQCON(4)*EQCON(2)
                MAT_TMP(9)  = WEI*EQCON(4)*EQCON(3)
                MAT_TMP(10) = WEI*EQCON(4)*EQCON(4)
                MAT_TMP(11) = WEI*EQCON(5)*EQCON(1)
                MAT_TMP(12) = WEI*EQCON(5)*EQCON(2)
                MAT_TMP(13) = WEI*EQCON(5)*EQCON(3)
                MAT_TMP(14) = WEI*EQCON(5)*EQCON(4)
                MAT_TMP(15) = WEI*EQCON(5)*EQCON(5)
                MAT_TMP(16) = WEI*EQCON(6)*EQCON(1)
                MAT_TMP(17) = WEI*EQCON(6)*EQCON(2)
                MAT_TMP(18) = WEI*EQCON(6)*EQCON(3)
                MAT_TMP(19) = WEI*EQCON(6)*EQCON(4)
                MAT_TMP(20) = WEI*EQCON(6)*EQCON(5)
                MAT_TMP(21) = WEI*EQCON(6)*EQCON(6)
!
                NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
                NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
                NOR_VEC( IND(3) ) = NOR_VEC( IND(3) ) + WEI*EQCON(3)*OC
                NOR_VEC( IND(4) ) = NOR_VEC( IND(4) ) + WEI*EQCON(4)*OC
                NOR_VEC( IND(5) ) = NOR_VEC( IND(5) ) + WEI*EQCON(5)*OC
                NOR_VEC( IND(6) ) = NOR_VEC( IND(6) ) + WEI*EQCON(6)*OC
!
                NOR_MAT(LAI(1)+IND(1)) = NOR_MAT(LAI(1)+IND(1)) + MAT_TMP(1)
                NOR_MAT(LAI(2)+IND(1)) = NOR_MAT(LAI(2)+IND(1)) + MAT_TMP(2)
                NOR_MAT(LAI(2)+IND(2)) = NOR_MAT(LAI(2)+IND(2)) + MAT_TMP(3)
                NOR_MAT(LAI(3)+IND(1)) = NOR_MAT(LAI(3)+IND(1)) + MAT_TMP(4)
                NOR_MAT(LAI(3)+IND(2)) = NOR_MAT(LAI(3)+IND(2)) + MAT_TMP(5)
                NOR_MAT(LAI(3)+IND(3)) = NOR_MAT(LAI(3)+IND(3)) + MAT_TMP(6)
                NOR_MAT(LAI(4)+IND(1)) = NOR_MAT(LAI(4)+IND(1)) + MAT_TMP(7)
                NOR_MAT(LAI(4)+IND(2)) = NOR_MAT(LAI(4)+IND(2)) + MAT_TMP(8)
                NOR_MAT(LAI(4)+IND(3)) = NOR_MAT(LAI(4)+IND(3)) + MAT_TMP(9)
                NOR_MAT(LAI(4)+IND(4)) = NOR_MAT(LAI(4)+IND(4)) + MAT_TMP(10)
                NOR_MAT(LAI(5)+IND(1)) = NOR_MAT(LAI(5)+IND(1)) + MAT_TMP(11)
                NOR_MAT(LAI(5)+IND(2)) = NOR_MAT(LAI(5)+IND(2)) + MAT_TMP(12)
                NOR_MAT(LAI(5)+IND(3)) = NOR_MAT(LAI(5)+IND(3)) + MAT_TMP(13)
                NOR_MAT(LAI(5)+IND(4)) = NOR_MAT(LAI(5)+IND(4)) + MAT_TMP(14)
                NOR_MAT(LAI(5)+IND(5)) = NOR_MAT(LAI(5)+IND(5)) + MAT_TMP(15)
                NOR_MAT(LAI(6)+IND(1)) = NOR_MAT(LAI(6)+IND(1)) + MAT_TMP(16)
                NOR_MAT(LAI(6)+IND(2)) = NOR_MAT(LAI(6)+IND(2)) + MAT_TMP(17)
                NOR_MAT(LAI(6)+IND(3)) = NOR_MAT(LAI(6)+IND(3)) + MAT_TMP(18)
                NOR_MAT(LAI(6)+IND(4)) = NOR_MAT(LAI(6)+IND(4)) + MAT_TMP(19)
                NOR_MAT(LAI(6)+IND(5)) = NOR_MAT(LAI(6)+IND(5)) + MAT_TMP(20)
                NOR_MAT(LAI(6)+IND(6)) = NOR_MAT(LAI(6)+IND(6)) + MAT_TMP(21)
                RETURN
              ELSE IF ( N .EQ. 7 ) THEN
                MAT_TMP(1)  = WEI*EQCON(1)*EQCON(1)
                MAT_TMP(2)  = WEI*EQCON(2)*EQCON(1)
                MAT_TMP(3)  = WEI*EQCON(2)*EQCON(2)
                MAT_TMP(4)  = WEI*EQCON(3)*EQCON(1)
                MAT_TMP(5)  = WEI*EQCON(3)*EQCON(2)
                MAT_TMP(6)  = WEI*EQCON(3)*EQCON(3)
                MAT_TMP(7)  = WEI*EQCON(4)*EQCON(1)
                MAT_TMP(8)  = WEI*EQCON(4)*EQCON(2)
                MAT_TMP(9)  = WEI*EQCON(4)*EQCON(3)
                MAT_TMP(10) = WEI*EQCON(4)*EQCON(4)
                MAT_TMP(11) = WEI*EQCON(5)*EQCON(1)
                MAT_TMP(12) = WEI*EQCON(5)*EQCON(2)
                MAT_TMP(13) = WEI*EQCON(5)*EQCON(3)
                MAT_TMP(14) = WEI*EQCON(5)*EQCON(4)
                MAT_TMP(15) = WEI*EQCON(5)*EQCON(5)
                MAT_TMP(16) = WEI*EQCON(6)*EQCON(1)
                MAT_TMP(17) = WEI*EQCON(6)*EQCON(2)
                MAT_TMP(18) = WEI*EQCON(6)*EQCON(3)
                MAT_TMP(19) = WEI*EQCON(6)*EQCON(4)
                MAT_TMP(20) = WEI*EQCON(6)*EQCON(5)
                MAT_TMP(21) = WEI*EQCON(6)*EQCON(6)
                MAT_TMP(22) = WEI*EQCON(7)*EQCON(1)
                MAT_TMP(23) = WEI*EQCON(7)*EQCON(2)
                MAT_TMP(24) = WEI*EQCON(7)*EQCON(3)
                MAT_TMP(25) = WEI*EQCON(7)*EQCON(4)
                MAT_TMP(26) = WEI*EQCON(7)*EQCON(5)
                MAT_TMP(27) = WEI*EQCON(7)*EQCON(6)
                MAT_TMP(28) = WEI*EQCON(7)*EQCON(7)
!
                NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
                NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
                NOR_VEC( IND(3) ) = NOR_VEC( IND(3) ) + WEI*EQCON(3)*OC
                NOR_VEC( IND(4) ) = NOR_VEC( IND(4) ) + WEI*EQCON(4)*OC
                NOR_VEC( IND(5) ) = NOR_VEC( IND(5) ) + WEI*EQCON(5)*OC
                NOR_VEC( IND(6) ) = NOR_VEC( IND(6) ) + WEI*EQCON(6)*OC
                NOR_VEC( IND(7) ) = NOR_VEC( IND(7) ) + WEI*EQCON(7)*OC
!
                NOR_MAT(LAI(1)+IND(1)) = NOR_MAT(LAI(1)+IND(1)) + MAT_TMP(1)
                NOR_MAT(LAI(2)+IND(1)) = NOR_MAT(LAI(2)+IND(1)) + MAT_TMP(2)
                NOR_MAT(LAI(2)+IND(2)) = NOR_MAT(LAI(2)+IND(2)) + MAT_TMP(3)
                NOR_MAT(LAI(3)+IND(1)) = NOR_MAT(LAI(3)+IND(1)) + MAT_TMP(4)
                NOR_MAT(LAI(3)+IND(2)) = NOR_MAT(LAI(3)+IND(2)) + MAT_TMP(5)
                NOR_MAT(LAI(3)+IND(3)) = NOR_MAT(LAI(3)+IND(3)) + MAT_TMP(6)
                NOR_MAT(LAI(4)+IND(1)) = NOR_MAT(LAI(4)+IND(1)) + MAT_TMP(7)
                NOR_MAT(LAI(4)+IND(2)) = NOR_MAT(LAI(4)+IND(2)) + MAT_TMP(8)
                NOR_MAT(LAI(4)+IND(3)) = NOR_MAT(LAI(4)+IND(3)) + MAT_TMP(9)
                NOR_MAT(LAI(4)+IND(4)) = NOR_MAT(LAI(4)+IND(4)) + MAT_TMP(10)
                NOR_MAT(LAI(5)+IND(1)) = NOR_MAT(LAI(5)+IND(1)) + MAT_TMP(11)
                NOR_MAT(LAI(5)+IND(2)) = NOR_MAT(LAI(5)+IND(2)) + MAT_TMP(12)
                NOR_MAT(LAI(5)+IND(3)) = NOR_MAT(LAI(5)+IND(3)) + MAT_TMP(13)
                NOR_MAT(LAI(5)+IND(4)) = NOR_MAT(LAI(5)+IND(4)) + MAT_TMP(14)
                NOR_MAT(LAI(5)+IND(5)) = NOR_MAT(LAI(5)+IND(5)) + MAT_TMP(15)
                NOR_MAT(LAI(6)+IND(1)) = NOR_MAT(LAI(6)+IND(1)) + MAT_TMP(16)
                NOR_MAT(LAI(6)+IND(2)) = NOR_MAT(LAI(6)+IND(2)) + MAT_TMP(17)
                NOR_MAT(LAI(6)+IND(3)) = NOR_MAT(LAI(6)+IND(3)) + MAT_TMP(18)
                NOR_MAT(LAI(6)+IND(4)) = NOR_MAT(LAI(6)+IND(4)) + MAT_TMP(19)
                NOR_MAT(LAI(6)+IND(5)) = NOR_MAT(LAI(6)+IND(5)) + MAT_TMP(20)
                NOR_MAT(LAI(6)+IND(6)) = NOR_MAT(LAI(6)+IND(6)) + MAT_TMP(21)
                NOR_MAT(LAI(7)+IND(1)) = NOR_MAT(LAI(7)+IND(1)) + MAT_TMP(22)
                NOR_MAT(LAI(7)+IND(2)) = NOR_MAT(LAI(7)+IND(2)) + MAT_TMP(23)
                NOR_MAT(LAI(7)+IND(3)) = NOR_MAT(LAI(7)+IND(3)) + MAT_TMP(24)
                NOR_MAT(LAI(7)+IND(4)) = NOR_MAT(LAI(7)+IND(4)) + MAT_TMP(25)
                NOR_MAT(LAI(7)+IND(5)) = NOR_MAT(LAI(7)+IND(5)) + MAT_TMP(26)
                NOR_MAT(LAI(7)+IND(6)) = NOR_MAT(LAI(7)+IND(6)) + MAT_TMP(27)
                NOR_MAT(LAI(7)+IND(7)) = NOR_MAT(LAI(7)+IND(7)) + MAT_TMP(28)
                RETURN
              ELSE IF ( N .EQ. 8 ) THEN
                MAT_TMP(1)  = WEI*EQCON(1)*EQCON(1)
                MAT_TMP(2)  = WEI*EQCON(2)*EQCON(1)
                MAT_TMP(3)  = WEI*EQCON(2)*EQCON(2)
                MAT_TMP(4)  = WEI*EQCON(3)*EQCON(1)
                MAT_TMP(5)  = WEI*EQCON(3)*EQCON(2)
                MAT_TMP(6)  = WEI*EQCON(3)*EQCON(3)
                MAT_TMP(7)  = WEI*EQCON(4)*EQCON(1)
                MAT_TMP(8)  = WEI*EQCON(4)*EQCON(2)
                MAT_TMP(9)  = WEI*EQCON(4)*EQCON(3)
                MAT_TMP(10) = WEI*EQCON(4)*EQCON(4)
                MAT_TMP(11) = WEI*EQCON(5)*EQCON(1)
                MAT_TMP(12) = WEI*EQCON(5)*EQCON(2)
                MAT_TMP(13) = WEI*EQCON(5)*EQCON(3)
                MAT_TMP(14) = WEI*EQCON(5)*EQCON(4)
                MAT_TMP(15) = WEI*EQCON(5)*EQCON(5)
                MAT_TMP(16) = WEI*EQCON(6)*EQCON(1)
                MAT_TMP(17) = WEI*EQCON(6)*EQCON(2)
                MAT_TMP(18) = WEI*EQCON(6)*EQCON(3)
                MAT_TMP(19) = WEI*EQCON(6)*EQCON(4)
                MAT_TMP(20) = WEI*EQCON(6)*EQCON(5)
                MAT_TMP(21) = WEI*EQCON(6)*EQCON(6)
                MAT_TMP(22) = WEI*EQCON(7)*EQCON(1)
                MAT_TMP(23) = WEI*EQCON(7)*EQCON(2)
                MAT_TMP(24) = WEI*EQCON(7)*EQCON(3)
                MAT_TMP(25) = WEI*EQCON(7)*EQCON(4)
                MAT_TMP(26) = WEI*EQCON(7)*EQCON(5)
                MAT_TMP(27) = WEI*EQCON(7)*EQCON(6)
                MAT_TMP(28) = WEI*EQCON(7)*EQCON(7)
                MAT_TMP(29) = WEI*EQCON(8)*EQCON(1)
                MAT_TMP(30) = WEI*EQCON(8)*EQCON(2)
                MAT_TMP(31) = WEI*EQCON(8)*EQCON(3)
                MAT_TMP(32) = WEI*EQCON(8)*EQCON(4)
                MAT_TMP(33) = WEI*EQCON(8)*EQCON(5)
                MAT_TMP(34) = WEI*EQCON(8)*EQCON(6)
                MAT_TMP(35) = WEI*EQCON(8)*EQCON(7)
                MAT_TMP(36) = WEI*EQCON(8)*EQCON(8)
!
                NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
                NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
                NOR_VEC( IND(3) ) = NOR_VEC( IND(3) ) + WEI*EQCON(3)*OC
                NOR_VEC( IND(4) ) = NOR_VEC( IND(4) ) + WEI*EQCON(4)*OC
                NOR_VEC( IND(5) ) = NOR_VEC( IND(5) ) + WEI*EQCON(5)*OC
                NOR_VEC( IND(6) ) = NOR_VEC( IND(6) ) + WEI*EQCON(6)*OC
                NOR_VEC( IND(7) ) = NOR_VEC( IND(7) ) + WEI*EQCON(7)*OC
                NOR_VEC( IND(8) ) = NOR_VEC( IND(8) ) + WEI*EQCON(8)*OC
!
                NOR_MAT(LAI(1)+IND(1)) = NOR_MAT(LAI(1)+IND(1)) + MAT_TMP(1)
                NOR_MAT(LAI(2)+IND(1)) = NOR_MAT(LAI(2)+IND(1)) + MAT_TMP(2)
                NOR_MAT(LAI(2)+IND(2)) = NOR_MAT(LAI(2)+IND(2)) + MAT_TMP(3)
                NOR_MAT(LAI(3)+IND(1)) = NOR_MAT(LAI(3)+IND(1)) + MAT_TMP(4)
                NOR_MAT(LAI(3)+IND(2)) = NOR_MAT(LAI(3)+IND(2)) + MAT_TMP(5)
                NOR_MAT(LAI(3)+IND(3)) = NOR_MAT(LAI(3)+IND(3)) + MAT_TMP(6)
                NOR_MAT(LAI(4)+IND(1)) = NOR_MAT(LAI(4)+IND(1)) + MAT_TMP(7)
                NOR_MAT(LAI(4)+IND(2)) = NOR_MAT(LAI(4)+IND(2)) + MAT_TMP(8)
                NOR_MAT(LAI(4)+IND(3)) = NOR_MAT(LAI(4)+IND(3)) + MAT_TMP(9)
                NOR_MAT(LAI(4)+IND(4)) = NOR_MAT(LAI(4)+IND(4)) + MAT_TMP(10)
                NOR_MAT(LAI(5)+IND(1)) = NOR_MAT(LAI(5)+IND(1)) + MAT_TMP(11)
                NOR_MAT(LAI(5)+IND(2)) = NOR_MAT(LAI(5)+IND(2)) + MAT_TMP(12)
                NOR_MAT(LAI(5)+IND(3)) = NOR_MAT(LAI(5)+IND(3)) + MAT_TMP(13)
                NOR_MAT(LAI(5)+IND(4)) = NOR_MAT(LAI(5)+IND(4)) + MAT_TMP(14)
                NOR_MAT(LAI(5)+IND(5)) = NOR_MAT(LAI(5)+IND(5)) + MAT_TMP(15)
                NOR_MAT(LAI(6)+IND(1)) = NOR_MAT(LAI(6)+IND(1)) + MAT_TMP(16)
                NOR_MAT(LAI(6)+IND(2)) = NOR_MAT(LAI(6)+IND(2)) + MAT_TMP(17)
                NOR_MAT(LAI(6)+IND(3)) = NOR_MAT(LAI(6)+IND(3)) + MAT_TMP(18)
                NOR_MAT(LAI(6)+IND(4)) = NOR_MAT(LAI(6)+IND(4)) + MAT_TMP(19)
                NOR_MAT(LAI(6)+IND(5)) = NOR_MAT(LAI(6)+IND(5)) + MAT_TMP(20)
                NOR_MAT(LAI(6)+IND(6)) = NOR_MAT(LAI(6)+IND(6)) + MAT_TMP(21)
                NOR_MAT(LAI(7)+IND(1)) = NOR_MAT(LAI(7)+IND(1)) + MAT_TMP(22)
                NOR_MAT(LAI(7)+IND(2)) = NOR_MAT(LAI(7)+IND(2)) + MAT_TMP(23)
                NOR_MAT(LAI(7)+IND(3)) = NOR_MAT(LAI(7)+IND(3)) + MAT_TMP(24)
                NOR_MAT(LAI(7)+IND(4)) = NOR_MAT(LAI(7)+IND(4)) + MAT_TMP(25)
                NOR_MAT(LAI(7)+IND(5)) = NOR_MAT(LAI(7)+IND(5)) + MAT_TMP(26)
                NOR_MAT(LAI(7)+IND(6)) = NOR_MAT(LAI(7)+IND(6)) + MAT_TMP(27)
                NOR_MAT(LAI(7)+IND(7)) = NOR_MAT(LAI(7)+IND(7)) + MAT_TMP(28)
                NOR_MAT(LAI(8)+IND(1)) = NOR_MAT(LAI(8)+IND(1)) + MAT_TMP(29)
                NOR_MAT(LAI(8)+IND(2)) = NOR_MAT(LAI(8)+IND(2)) + MAT_TMP(30)
                NOR_MAT(LAI(8)+IND(3)) = NOR_MAT(LAI(8)+IND(3)) + MAT_TMP(31)
                NOR_MAT(LAI(8)+IND(4)) = NOR_MAT(LAI(8)+IND(4)) + MAT_TMP(32)
                NOR_MAT(LAI(8)+IND(5)) = NOR_MAT(LAI(8)+IND(5)) + MAT_TMP(33)
                NOR_MAT(LAI(8)+IND(6)) = NOR_MAT(LAI(8)+IND(6)) + MAT_TMP(34)
                NOR_MAT(LAI(8)+IND(7)) = NOR_MAT(LAI(8)+IND(7)) + MAT_TMP(35)
                NOR_MAT(LAI(8)+IND(8)) = NOR_MAT(LAI(8)+IND(8)) + MAT_TMP(36)
                RETURN
             ELSE IF ( N .EQ. 9 ) THEN
                MAT_TMP(1)  = WEI*EQCON(1)*EQCON(1)
                MAT_TMP(2)  = WEI*EQCON(2)*EQCON(1)
                MAT_TMP(3)  = WEI*EQCON(2)*EQCON(2)
                MAT_TMP(4)  = WEI*EQCON(3)*EQCON(1)
                MAT_TMP(5)  = WEI*EQCON(3)*EQCON(2)
                MAT_TMP(6)  = WEI*EQCON(3)*EQCON(3)
                MAT_TMP(7)  = WEI*EQCON(4)*EQCON(1)
                MAT_TMP(8)  = WEI*EQCON(4)*EQCON(2)
                MAT_TMP(9)  = WEI*EQCON(4)*EQCON(3)
                MAT_TMP(10) = WEI*EQCON(4)*EQCON(4)
                MAT_TMP(11) = WEI*EQCON(5)*EQCON(1)
                MAT_TMP(12) = WEI*EQCON(5)*EQCON(2)
                MAT_TMP(13) = WEI*EQCON(5)*EQCON(3)
                MAT_TMP(14) = WEI*EQCON(5)*EQCON(4)
                MAT_TMP(15) = WEI*EQCON(5)*EQCON(5)
                MAT_TMP(16) = WEI*EQCON(6)*EQCON(1)
                MAT_TMP(17) = WEI*EQCON(6)*EQCON(2)
                MAT_TMP(18) = WEI*EQCON(6)*EQCON(3)
                MAT_TMP(19) = WEI*EQCON(6)*EQCON(4)
                MAT_TMP(20) = WEI*EQCON(6)*EQCON(5)
                MAT_TMP(21) = WEI*EQCON(6)*EQCON(6)
                MAT_TMP(22) = WEI*EQCON(7)*EQCON(1)
                MAT_TMP(23) = WEI*EQCON(7)*EQCON(2)
                MAT_TMP(24) = WEI*EQCON(7)*EQCON(3)
                MAT_TMP(25) = WEI*EQCON(7)*EQCON(4)
                MAT_TMP(26) = WEI*EQCON(7)*EQCON(5)
                MAT_TMP(27) = WEI*EQCON(7)*EQCON(6)
                MAT_TMP(28) = WEI*EQCON(7)*EQCON(7)
                MAT_TMP(29) = WEI*EQCON(8)*EQCON(1)
                MAT_TMP(30) = WEI*EQCON(8)*EQCON(2)
                MAT_TMP(31) = WEI*EQCON(8)*EQCON(3)
                MAT_TMP(32) = WEI*EQCON(8)*EQCON(4)
                MAT_TMP(33) = WEI*EQCON(8)*EQCON(5)
                MAT_TMP(34) = WEI*EQCON(8)*EQCON(6)
                MAT_TMP(35) = WEI*EQCON(8)*EQCON(7)
                MAT_TMP(36) = WEI*EQCON(8)*EQCON(8)
                MAT_TMP(37) = WEI*EQCON(9)*EQCON(1)
                MAT_TMP(38) = WEI*EQCON(9)*EQCON(2)
                MAT_TMP(39) = WEI*EQCON(9)*EQCON(3)
                MAT_TMP(40) = WEI*EQCON(9)*EQCON(4)
                MAT_TMP(41) = WEI*EQCON(9)*EQCON(5)
                MAT_TMP(42) = WEI*EQCON(9)*EQCON(6)
                MAT_TMP(43) = WEI*EQCON(9)*EQCON(7)
                MAT_TMP(44) = WEI*EQCON(9)*EQCON(8)
                MAT_TMP(45) = WEI*EQCON(9)*EQCON(9)
!
                NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
                NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
                NOR_VEC( IND(3) ) = NOR_VEC( IND(3) ) + WEI*EQCON(3)*OC
                NOR_VEC( IND(4) ) = NOR_VEC( IND(4) ) + WEI*EQCON(4)*OC
                NOR_VEC( IND(5) ) = NOR_VEC( IND(5) ) + WEI*EQCON(5)*OC
                NOR_VEC( IND(6) ) = NOR_VEC( IND(6) ) + WEI*EQCON(6)*OC
                NOR_VEC( IND(7) ) = NOR_VEC( IND(7) ) + WEI*EQCON(7)*OC
                NOR_VEC( IND(8) ) = NOR_VEC( IND(8) ) + WEI*EQCON(8)*OC
                NOR_VEC( IND(9) ) = NOR_VEC( IND(9) ) + WEI*EQCON(9)*OC
!
                NOR_MAT(LAI(1)+IND(1)) = NOR_MAT(LAI(1)+IND(1)) + MAT_TMP(1)
                NOR_MAT(LAI(2)+IND(1)) = NOR_MAT(LAI(2)+IND(1)) + MAT_TMP(2)
                NOR_MAT(LAI(2)+IND(2)) = NOR_MAT(LAI(2)+IND(2)) + MAT_TMP(3)
                NOR_MAT(LAI(3)+IND(1)) = NOR_MAT(LAI(3)+IND(1)) + MAT_TMP(4)
                NOR_MAT(LAI(3)+IND(2)) = NOR_MAT(LAI(3)+IND(2)) + MAT_TMP(5)
                NOR_MAT(LAI(3)+IND(3)) = NOR_MAT(LAI(3)+IND(3)) + MAT_TMP(6)
                NOR_MAT(LAI(4)+IND(1)) = NOR_MAT(LAI(4)+IND(1)) + MAT_TMP(7)
                NOR_MAT(LAI(4)+IND(2)) = NOR_MAT(LAI(4)+IND(2)) + MAT_TMP(8)
                NOR_MAT(LAI(4)+IND(3)) = NOR_MAT(LAI(4)+IND(3)) + MAT_TMP(9)
                NOR_MAT(LAI(4)+IND(4)) = NOR_MAT(LAI(4)+IND(4)) + MAT_TMP(10)
                NOR_MAT(LAI(5)+IND(1)) = NOR_MAT(LAI(5)+IND(1)) + MAT_TMP(11)
                NOR_MAT(LAI(5)+IND(2)) = NOR_MAT(LAI(5)+IND(2)) + MAT_TMP(12)
                NOR_MAT(LAI(5)+IND(3)) = NOR_MAT(LAI(5)+IND(3)) + MAT_TMP(13)
                NOR_MAT(LAI(5)+IND(4)) = NOR_MAT(LAI(5)+IND(4)) + MAT_TMP(14)
                NOR_MAT(LAI(5)+IND(5)) = NOR_MAT(LAI(5)+IND(5)) + MAT_TMP(15)
                NOR_MAT(LAI(6)+IND(1)) = NOR_MAT(LAI(6)+IND(1)) + MAT_TMP(16)
                NOR_MAT(LAI(6)+IND(2)) = NOR_MAT(LAI(6)+IND(2)) + MAT_TMP(17)
                NOR_MAT(LAI(6)+IND(3)) = NOR_MAT(LAI(6)+IND(3)) + MAT_TMP(18)
                NOR_MAT(LAI(6)+IND(4)) = NOR_MAT(LAI(6)+IND(4)) + MAT_TMP(19)
                NOR_MAT(LAI(6)+IND(5)) = NOR_MAT(LAI(6)+IND(5)) + MAT_TMP(20)
                NOR_MAT(LAI(6)+IND(6)) = NOR_MAT(LAI(6)+IND(6)) + MAT_TMP(21)
                NOR_MAT(LAI(7)+IND(1)) = NOR_MAT(LAI(7)+IND(1)) + MAT_TMP(22)
                NOR_MAT(LAI(7)+IND(2)) = NOR_MAT(LAI(7)+IND(2)) + MAT_TMP(23)
                NOR_MAT(LAI(7)+IND(3)) = NOR_MAT(LAI(7)+IND(3)) + MAT_TMP(24)
                NOR_MAT(LAI(7)+IND(4)) = NOR_MAT(LAI(7)+IND(4)) + MAT_TMP(25)
                NOR_MAT(LAI(7)+IND(5)) = NOR_MAT(LAI(7)+IND(5)) + MAT_TMP(26)
                NOR_MAT(LAI(7)+IND(6)) = NOR_MAT(LAI(7)+IND(6)) + MAT_TMP(27)
                NOR_MAT(LAI(7)+IND(7)) = NOR_MAT(LAI(7)+IND(7)) + MAT_TMP(28)
                NOR_MAT(LAI(8)+IND(1)) = NOR_MAT(LAI(8)+IND(1)) + MAT_TMP(29)
                NOR_MAT(LAI(8)+IND(2)) = NOR_MAT(LAI(8)+IND(2)) + MAT_TMP(30)
                NOR_MAT(LAI(8)+IND(3)) = NOR_MAT(LAI(8)+IND(3)) + MAT_TMP(31)
                NOR_MAT(LAI(8)+IND(4)) = NOR_MAT(LAI(8)+IND(4)) + MAT_TMP(32)
                NOR_MAT(LAI(8)+IND(5)) = NOR_MAT(LAI(8)+IND(5)) + MAT_TMP(33)
                NOR_MAT(LAI(8)+IND(6)) = NOR_MAT(LAI(8)+IND(6)) + MAT_TMP(34)
                NOR_MAT(LAI(8)+IND(7)) = NOR_MAT(LAI(8)+IND(7)) + MAT_TMP(35)
                NOR_MAT(LAI(8)+IND(8)) = NOR_MAT(LAI(8)+IND(8)) + MAT_TMP(36)
                NOR_MAT(LAI(9)+IND(1)) = NOR_MAT(LAI(9)+IND(1)) + MAT_TMP(37)
                NOR_MAT(LAI(9)+IND(2)) = NOR_MAT(LAI(9)+IND(2)) + MAT_TMP(38)
                NOR_MAT(LAI(9)+IND(3)) = NOR_MAT(LAI(9)+IND(3)) + MAT_TMP(39)
                NOR_MAT(LAI(9)+IND(4)) = NOR_MAT(LAI(9)+IND(4)) + MAT_TMP(40)
                NOR_MAT(LAI(9)+IND(5)) = NOR_MAT(LAI(9)+IND(5)) + MAT_TMP(41)
                NOR_MAT(LAI(9)+IND(6)) = NOR_MAT(LAI(9)+IND(6)) + MAT_TMP(42)
                NOR_MAT(LAI(9)+IND(7)) = NOR_MAT(LAI(9)+IND(7)) + MAT_TMP(43)
                NOR_MAT(LAI(9)+IND(8)) = NOR_MAT(LAI(9)+IND(8)) + MAT_TMP(44)
                NOR_MAT(LAI(9)+IND(9)) = NOR_MAT(LAI(9)+IND(9)) + MAT_TMP(45)
                RETURN
             ELSE IF ( N .EQ. 10 ) THEN
                MAT_TMP(1)  = WEI*EQCON(1)*EQCON(1)
                MAT_TMP(2)  = WEI*EQCON(2)*EQCON(1)
                MAT_TMP(3)  = WEI*EQCON(2)*EQCON(2)
                MAT_TMP(4)  = WEI*EQCON(3)*EQCON(1)
                MAT_TMP(5)  = WEI*EQCON(3)*EQCON(2)
                MAT_TMP(6)  = WEI*EQCON(3)*EQCON(3)
                MAT_TMP(7)  = WEI*EQCON(4)*EQCON(1)
                MAT_TMP(8)  = WEI*EQCON(4)*EQCON(2)
                MAT_TMP(9)  = WEI*EQCON(4)*EQCON(3)
                MAT_TMP(10) = WEI*EQCON(4)*EQCON(4)
                MAT_TMP(11) = WEI*EQCON(5)*EQCON(1)
                MAT_TMP(12) = WEI*EQCON(5)*EQCON(2)
                MAT_TMP(13) = WEI*EQCON(5)*EQCON(3)
                MAT_TMP(14) = WEI*EQCON(5)*EQCON(4)
                MAT_TMP(15) = WEI*EQCON(5)*EQCON(5)
                MAT_TMP(16) = WEI*EQCON(6)*EQCON(1)
                MAT_TMP(17) = WEI*EQCON(6)*EQCON(2)
                MAT_TMP(18) = WEI*EQCON(6)*EQCON(3)
                MAT_TMP(19) = WEI*EQCON(6)*EQCON(4)
                MAT_TMP(20) = WEI*EQCON(6)*EQCON(5)
                MAT_TMP(21) = WEI*EQCON(6)*EQCON(6)
                MAT_TMP(22) = WEI*EQCON(7)*EQCON(1)
                MAT_TMP(23) = WEI*EQCON(7)*EQCON(2)
                MAT_TMP(24) = WEI*EQCON(7)*EQCON(3)
                MAT_TMP(25) = WEI*EQCON(7)*EQCON(4)
                MAT_TMP(26) = WEI*EQCON(7)*EQCON(5)
                MAT_TMP(27) = WEI*EQCON(7)*EQCON(6)
                MAT_TMP(28) = WEI*EQCON(7)*EQCON(7)
                MAT_TMP(29) = WEI*EQCON(8)*EQCON(1)
                MAT_TMP(30) = WEI*EQCON(8)*EQCON(2)
                MAT_TMP(31) = WEI*EQCON(8)*EQCON(3)
                MAT_TMP(32) = WEI*EQCON(8)*EQCON(4)
                MAT_TMP(33) = WEI*EQCON(8)*EQCON(5)
                MAT_TMP(34) = WEI*EQCON(8)*EQCON(6)
                MAT_TMP(35) = WEI*EQCON(8)*EQCON(7)
                MAT_TMP(36) = WEI*EQCON(8)*EQCON(8)
                MAT_TMP(37) = WEI*EQCON(9)*EQCON(1)
                MAT_TMP(38) = WEI*EQCON(9)*EQCON(2)
                MAT_TMP(39) = WEI*EQCON(9)*EQCON(3)
                MAT_TMP(40) = WEI*EQCON(9)*EQCON(4)
                MAT_TMP(41) = WEI*EQCON(9)*EQCON(5)
                MAT_TMP(42) = WEI*EQCON(9)*EQCON(6)
                MAT_TMP(43) = WEI*EQCON(9)*EQCON(7)
                MAT_TMP(44) = WEI*EQCON(9)*EQCON(8)
                MAT_TMP(45) = WEI*EQCON(9)*EQCON(9)
                MAT_TMP(46) = WEI*EQCON(10)*EQCON(1)
                MAT_TMP(47) = WEI*EQCON(10)*EQCON(2)
                MAT_TMP(48) = WEI*EQCON(10)*EQCON(3)
                MAT_TMP(49) = WEI*EQCON(10)*EQCON(4)
                MAT_TMP(50) = WEI*EQCON(10)*EQCON(5)
                MAT_TMP(51) = WEI*EQCON(10)*EQCON(6)
                MAT_TMP(52) = WEI*EQCON(10)*EQCON(7)
                MAT_TMP(53) = WEI*EQCON(10)*EQCON(8)
                MAT_TMP(54) = WEI*EQCON(10)*EQCON(9)
                MAT_TMP(55) = WEI*EQCON(10)*EQCON(10)
!
                NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
                NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
                NOR_VEC( IND(3) ) = NOR_VEC( IND(3) ) + WEI*EQCON(3)*OC
                NOR_VEC( IND(4) ) = NOR_VEC( IND(4) ) + WEI*EQCON(4)*OC
                NOR_VEC( IND(5) ) = NOR_VEC( IND(5) ) + WEI*EQCON(5)*OC
                NOR_VEC( IND(6) ) = NOR_VEC( IND(6) ) + WEI*EQCON(6)*OC
                NOR_VEC( IND(7) ) = NOR_VEC( IND(7) ) + WEI*EQCON(7)*OC
                NOR_VEC( IND(8) ) = NOR_VEC( IND(8) ) + WEI*EQCON(8)*OC
                NOR_VEC( IND(9) ) = NOR_VEC( IND(9) ) + WEI*EQCON(9)*OC
                NOR_VEC( IND(10) ) = NOR_VEC( IND(10) ) + WEI*EQCON(10)*OC
!
                NOR_MAT(LAI(1)+IND(1)) = NOR_MAT(LAI(1)+IND(1)) + MAT_TMP(1)
                NOR_MAT(LAI(2)+IND(1)) = NOR_MAT(LAI(2)+IND(1)) + MAT_TMP(2)
                NOR_MAT(LAI(2)+IND(2)) = NOR_MAT(LAI(2)+IND(2)) + MAT_TMP(3)
                NOR_MAT(LAI(3)+IND(1)) = NOR_MAT(LAI(3)+IND(1)) + MAT_TMP(4)
                NOR_MAT(LAI(3)+IND(2)) = NOR_MAT(LAI(3)+IND(2)) + MAT_TMP(5)
                NOR_MAT(LAI(3)+IND(3)) = NOR_MAT(LAI(3)+IND(3)) + MAT_TMP(6)
                NOR_MAT(LAI(4)+IND(1)) = NOR_MAT(LAI(4)+IND(1)) + MAT_TMP(7)
                NOR_MAT(LAI(4)+IND(2)) = NOR_MAT(LAI(4)+IND(2)) + MAT_TMP(8)
                NOR_MAT(LAI(4)+IND(3)) = NOR_MAT(LAI(4)+IND(3)) + MAT_TMP(9)
                NOR_MAT(LAI(4)+IND(4)) = NOR_MAT(LAI(4)+IND(4)) + MAT_TMP(10)
                NOR_MAT(LAI(5)+IND(1)) = NOR_MAT(LAI(5)+IND(1)) + MAT_TMP(11)
                NOR_MAT(LAI(5)+IND(2)) = NOR_MAT(LAI(5)+IND(2)) + MAT_TMP(12)
                NOR_MAT(LAI(5)+IND(3)) = NOR_MAT(LAI(5)+IND(3)) + MAT_TMP(13)
                NOR_MAT(LAI(5)+IND(4)) = NOR_MAT(LAI(5)+IND(4)) + MAT_TMP(14)
                NOR_MAT(LAI(5)+IND(5)) = NOR_MAT(LAI(5)+IND(5)) + MAT_TMP(15)
                NOR_MAT(LAI(6)+IND(1)) = NOR_MAT(LAI(6)+IND(1)) + MAT_TMP(16)
                NOR_MAT(LAI(6)+IND(2)) = NOR_MAT(LAI(6)+IND(2)) + MAT_TMP(17)
                NOR_MAT(LAI(6)+IND(3)) = NOR_MAT(LAI(6)+IND(3)) + MAT_TMP(18)
                NOR_MAT(LAI(6)+IND(4)) = NOR_MAT(LAI(6)+IND(4)) + MAT_TMP(19)
                NOR_MAT(LAI(6)+IND(5)) = NOR_MAT(LAI(6)+IND(5)) + MAT_TMP(20)
                NOR_MAT(LAI(6)+IND(6)) = NOR_MAT(LAI(6)+IND(6)) + MAT_TMP(21)
                NOR_MAT(LAI(7)+IND(1)) = NOR_MAT(LAI(7)+IND(1)) + MAT_TMP(22)
                NOR_MAT(LAI(7)+IND(2)) = NOR_MAT(LAI(7)+IND(2)) + MAT_TMP(23)
                NOR_MAT(LAI(7)+IND(3)) = NOR_MAT(LAI(7)+IND(3)) + MAT_TMP(24)
                NOR_MAT(LAI(7)+IND(4)) = NOR_MAT(LAI(7)+IND(4)) + MAT_TMP(25)
                NOR_MAT(LAI(7)+IND(5)) = NOR_MAT(LAI(7)+IND(5)) + MAT_TMP(26)
                NOR_MAT(LAI(7)+IND(6)) = NOR_MAT(LAI(7)+IND(6)) + MAT_TMP(27)
                NOR_MAT(LAI(7)+IND(7)) = NOR_MAT(LAI(7)+IND(7)) + MAT_TMP(28)
                NOR_MAT(LAI(8)+IND(1)) = NOR_MAT(LAI(8)+IND(1)) + MAT_TMP(29)
                NOR_MAT(LAI(8)+IND(2)) = NOR_MAT(LAI(8)+IND(2)) + MAT_TMP(30)
                NOR_MAT(LAI(8)+IND(3)) = NOR_MAT(LAI(8)+IND(3)) + MAT_TMP(31)
                NOR_MAT(LAI(8)+IND(4)) = NOR_MAT(LAI(8)+IND(4)) + MAT_TMP(32)
                NOR_MAT(LAI(8)+IND(5)) = NOR_MAT(LAI(8)+IND(5)) + MAT_TMP(33)
                NOR_MAT(LAI(8)+IND(6)) = NOR_MAT(LAI(8)+IND(6)) + MAT_TMP(34)
                NOR_MAT(LAI(8)+IND(7)) = NOR_MAT(LAI(8)+IND(7)) + MAT_TMP(35)
                NOR_MAT(LAI(8)+IND(8)) = NOR_MAT(LAI(8)+IND(8)) + MAT_TMP(36)
                NOR_MAT(LAI(9)+IND(1)) = NOR_MAT(LAI(9)+IND(1)) + MAT_TMP(37)
                NOR_MAT(LAI(9)+IND(2)) = NOR_MAT(LAI(9)+IND(2)) + MAT_TMP(38)
                NOR_MAT(LAI(9)+IND(3)) = NOR_MAT(LAI(9)+IND(3)) + MAT_TMP(39)
                NOR_MAT(LAI(9)+IND(4)) = NOR_MAT(LAI(9)+IND(4)) + MAT_TMP(40)
                NOR_MAT(LAI(9)+IND(5)) = NOR_MAT(LAI(9)+IND(5)) + MAT_TMP(41)
                NOR_MAT(LAI(9)+IND(6)) = NOR_MAT(LAI(9)+IND(6)) + MAT_TMP(42)
                NOR_MAT(LAI(9)+IND(7)) = NOR_MAT(LAI(9)+IND(7)) + MAT_TMP(43)
                NOR_MAT(LAI(9)+IND(8)) = NOR_MAT(LAI(9)+IND(8)) + MAT_TMP(44)
                NOR_MAT(LAI(9)+IND(9)) = NOR_MAT(LAI(9)+IND(9)) + MAT_TMP(45)
                NOR_MAT(LAI(10)+IND(1)) = NOR_MAT(LAI(10)+IND(1)) + MAT_TMP(46)
                NOR_MAT(LAI(10)+IND(2)) = NOR_MAT(LAI(10)+IND(2)) + MAT_TMP(47)
                NOR_MAT(LAI(10)+IND(3)) = NOR_MAT(LAI(10)+IND(3)) + MAT_TMP(48)
                NOR_MAT(LAI(10)+IND(4)) = NOR_MAT(LAI(10)+IND(4)) + MAT_TMP(49)
                NOR_MAT(LAI(10)+IND(5)) = NOR_MAT(LAI(10)+IND(5)) + MAT_TMP(50)
                NOR_MAT(LAI(10)+IND(6)) = NOR_MAT(LAI(10)+IND(6)) + MAT_TMP(51)
                NOR_MAT(LAI(10)+IND(7)) = NOR_MAT(LAI(10)+IND(7)) + MAT_TMP(52)
                NOR_MAT(LAI(10)+IND(8)) = NOR_MAT(LAI(10)+IND(8)) + MAT_TMP(53)
                NOR_MAT(LAI(10)+IND(9)) = NOR_MAT(LAI(10)+IND(9)) + MAT_TMP(54)
                NOR_MAT(LAI(10)+IND(10)) = NOR_MAT(LAI(10)+IND(10)) + MAT_TMP(55)
                RETURN
             ELSE IF ( N .EQ. 11 ) THEN
                MAT_TMP(1)  = WEI*EQCON(1)*EQCON(1)
                MAT_TMP(2)  = WEI*EQCON(2)*EQCON(1)
                MAT_TMP(3)  = WEI*EQCON(2)*EQCON(2)
                MAT_TMP(4)  = WEI*EQCON(3)*EQCON(1)
                MAT_TMP(5)  = WEI*EQCON(3)*EQCON(2)
                MAT_TMP(6)  = WEI*EQCON(3)*EQCON(3)
                MAT_TMP(7)  = WEI*EQCON(4)*EQCON(1)
                MAT_TMP(8)  = WEI*EQCON(4)*EQCON(2)
                MAT_TMP(9)  = WEI*EQCON(4)*EQCON(3)
                MAT_TMP(10) = WEI*EQCON(4)*EQCON(4)
                MAT_TMP(11) = WEI*EQCON(5)*EQCON(1)
                MAT_TMP(12) = WEI*EQCON(5)*EQCON(2)
                MAT_TMP(13) = WEI*EQCON(5)*EQCON(3)
                MAT_TMP(14) = WEI*EQCON(5)*EQCON(4)
                MAT_TMP(15) = WEI*EQCON(5)*EQCON(5)
                MAT_TMP(16) = WEI*EQCON(6)*EQCON(1)
                MAT_TMP(17) = WEI*EQCON(6)*EQCON(2)
                MAT_TMP(18) = WEI*EQCON(6)*EQCON(3)
                MAT_TMP(19) = WEI*EQCON(6)*EQCON(4)
                MAT_TMP(20) = WEI*EQCON(6)*EQCON(5)
                MAT_TMP(21) = WEI*EQCON(6)*EQCON(6)
                MAT_TMP(22) = WEI*EQCON(7)*EQCON(1)
                MAT_TMP(23) = WEI*EQCON(7)*EQCON(2)
                MAT_TMP(24) = WEI*EQCON(7)*EQCON(3)
                MAT_TMP(25) = WEI*EQCON(7)*EQCON(4)
                MAT_TMP(26) = WEI*EQCON(7)*EQCON(5)
                MAT_TMP(27) = WEI*EQCON(7)*EQCON(6)
                MAT_TMP(28) = WEI*EQCON(7)*EQCON(7)
                MAT_TMP(29) = WEI*EQCON(8)*EQCON(1)
                MAT_TMP(30) = WEI*EQCON(8)*EQCON(2)
                MAT_TMP(31) = WEI*EQCON(8)*EQCON(3)
                MAT_TMP(32) = WEI*EQCON(8)*EQCON(4)
                MAT_TMP(33) = WEI*EQCON(8)*EQCON(5)
                MAT_TMP(34) = WEI*EQCON(8)*EQCON(6)
                MAT_TMP(35) = WEI*EQCON(8)*EQCON(7)
                MAT_TMP(36) = WEI*EQCON(8)*EQCON(8)
                MAT_TMP(37) = WEI*EQCON(9)*EQCON(1)
                MAT_TMP(38) = WEI*EQCON(9)*EQCON(2)
                MAT_TMP(39) = WEI*EQCON(9)*EQCON(3)
                MAT_TMP(40) = WEI*EQCON(9)*EQCON(4)
                MAT_TMP(41) = WEI*EQCON(9)*EQCON(5)
                MAT_TMP(42) = WEI*EQCON(9)*EQCON(6)
                MAT_TMP(43) = WEI*EQCON(9)*EQCON(7)
                MAT_TMP(44) = WEI*EQCON(9)*EQCON(8)
                MAT_TMP(45) = WEI*EQCON(9)*EQCON(9)
                MAT_TMP(46) = WEI*EQCON(10)*EQCON(1)
                MAT_TMP(47) = WEI*EQCON(10)*EQCON(2)
                MAT_TMP(48) = WEI*EQCON(10)*EQCON(3)
                MAT_TMP(49) = WEI*EQCON(10)*EQCON(4)
                MAT_TMP(50) = WEI*EQCON(10)*EQCON(5)
                MAT_TMP(51) = WEI*EQCON(10)*EQCON(6)
                MAT_TMP(52) = WEI*EQCON(10)*EQCON(7)
                MAT_TMP(53) = WEI*EQCON(10)*EQCON(8)
                MAT_TMP(54) = WEI*EQCON(10)*EQCON(9)
                MAT_TMP(55) = WEI*EQCON(10)*EQCON(10)
                MAT_TMP(56) = WEI*EQCON(11)*EQCON(1)
                MAT_TMP(57) = WEI*EQCON(11)*EQCON(2)
                MAT_TMP(58) = WEI*EQCON(11)*EQCON(3)
                MAT_TMP(59) = WEI*EQCON(11)*EQCON(4)
                MAT_TMP(60) = WEI*EQCON(11)*EQCON(5)
                MAT_TMP(61) = WEI*EQCON(11)*EQCON(6)
                MAT_TMP(62) = WEI*EQCON(11)*EQCON(7)
                MAT_TMP(63) = WEI*EQCON(11)*EQCON(8)
                MAT_TMP(64) = WEI*EQCON(11)*EQCON(9)
                MAT_TMP(65) = WEI*EQCON(11)*EQCON(10)
                MAT_TMP(66) = WEI*EQCON(11)*EQCON(11)
!
                NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
                NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
                NOR_VEC( IND(3) ) = NOR_VEC( IND(3) ) + WEI*EQCON(3)*OC
                NOR_VEC( IND(4) ) = NOR_VEC( IND(4) ) + WEI*EQCON(4)*OC
                NOR_VEC( IND(5) ) = NOR_VEC( IND(5) ) + WEI*EQCON(5)*OC
                NOR_VEC( IND(6) ) = NOR_VEC( IND(6) ) + WEI*EQCON(6)*OC
                NOR_VEC( IND(7) ) = NOR_VEC( IND(7) ) + WEI*EQCON(7)*OC
                NOR_VEC( IND(8) ) = NOR_VEC( IND(8) ) + WEI*EQCON(8)*OC
                NOR_VEC( IND(9) ) = NOR_VEC( IND(9) ) + WEI*EQCON(9)*OC
                NOR_VEC( IND(10) ) = NOR_VEC( IND(10) ) + WEI*EQCON(10)*OC
                NOR_VEC( IND(11) ) = NOR_VEC( IND(11) ) + WEI*EQCON(11)*OC
!
                NOR_MAT(LAI(1)+IND(1)) = NOR_MAT(LAI(1)+IND(1)) + MAT_TMP(1)
                NOR_MAT(LAI(2)+IND(1)) = NOR_MAT(LAI(2)+IND(1)) + MAT_TMP(2)
                NOR_MAT(LAI(2)+IND(2)) = NOR_MAT(LAI(2)+IND(2)) + MAT_TMP(3)
                NOR_MAT(LAI(3)+IND(1)) = NOR_MAT(LAI(3)+IND(1)) + MAT_TMP(4)
                NOR_MAT(LAI(3)+IND(2)) = NOR_MAT(LAI(3)+IND(2)) + MAT_TMP(5)
                NOR_MAT(LAI(3)+IND(3)) = NOR_MAT(LAI(3)+IND(3)) + MAT_TMP(6)
                NOR_MAT(LAI(4)+IND(1)) = NOR_MAT(LAI(4)+IND(1)) + MAT_TMP(7)
                NOR_MAT(LAI(4)+IND(2)) = NOR_MAT(LAI(4)+IND(2)) + MAT_TMP(8)
                NOR_MAT(LAI(4)+IND(3)) = NOR_MAT(LAI(4)+IND(3)) + MAT_TMP(9)
                NOR_MAT(LAI(4)+IND(4)) = NOR_MAT(LAI(4)+IND(4)) + MAT_TMP(10)
                NOR_MAT(LAI(5)+IND(1)) = NOR_MAT(LAI(5)+IND(1)) + MAT_TMP(11)
                NOR_MAT(LAI(5)+IND(2)) = NOR_MAT(LAI(5)+IND(2)) + MAT_TMP(12)
                NOR_MAT(LAI(5)+IND(3)) = NOR_MAT(LAI(5)+IND(3)) + MAT_TMP(13)
                NOR_MAT(LAI(5)+IND(4)) = NOR_MAT(LAI(5)+IND(4)) + MAT_TMP(14)
                NOR_MAT(LAI(5)+IND(5)) = NOR_MAT(LAI(5)+IND(5)) + MAT_TMP(15)
                NOR_MAT(LAI(6)+IND(1)) = NOR_MAT(LAI(6)+IND(1)) + MAT_TMP(16)
                NOR_MAT(LAI(6)+IND(2)) = NOR_MAT(LAI(6)+IND(2)) + MAT_TMP(17)
                NOR_MAT(LAI(6)+IND(3)) = NOR_MAT(LAI(6)+IND(3)) + MAT_TMP(18)
                NOR_MAT(LAI(6)+IND(4)) = NOR_MAT(LAI(6)+IND(4)) + MAT_TMP(19)
                NOR_MAT(LAI(6)+IND(5)) = NOR_MAT(LAI(6)+IND(5)) + MAT_TMP(20)
                NOR_MAT(LAI(6)+IND(6)) = NOR_MAT(LAI(6)+IND(6)) + MAT_TMP(21)
                NOR_MAT(LAI(7)+IND(1)) = NOR_MAT(LAI(7)+IND(1)) + MAT_TMP(22)
                NOR_MAT(LAI(7)+IND(2)) = NOR_MAT(LAI(7)+IND(2)) + MAT_TMP(23)
                NOR_MAT(LAI(7)+IND(3)) = NOR_MAT(LAI(7)+IND(3)) + MAT_TMP(24)
                NOR_MAT(LAI(7)+IND(4)) = NOR_MAT(LAI(7)+IND(4)) + MAT_TMP(25)
                NOR_MAT(LAI(7)+IND(5)) = NOR_MAT(LAI(7)+IND(5)) + MAT_TMP(26)
                NOR_MAT(LAI(7)+IND(6)) = NOR_MAT(LAI(7)+IND(6)) + MAT_TMP(27)
                NOR_MAT(LAI(7)+IND(7)) = NOR_MAT(LAI(7)+IND(7)) + MAT_TMP(28)
                NOR_MAT(LAI(8)+IND(1)) = NOR_MAT(LAI(8)+IND(1)) + MAT_TMP(29)
                NOR_MAT(LAI(8)+IND(2)) = NOR_MAT(LAI(8)+IND(2)) + MAT_TMP(30)
                NOR_MAT(LAI(8)+IND(3)) = NOR_MAT(LAI(8)+IND(3)) + MAT_TMP(31)
                NOR_MAT(LAI(8)+IND(4)) = NOR_MAT(LAI(8)+IND(4)) + MAT_TMP(32)
                NOR_MAT(LAI(8)+IND(5)) = NOR_MAT(LAI(8)+IND(5)) + MAT_TMP(33)
                NOR_MAT(LAI(8)+IND(6)) = NOR_MAT(LAI(8)+IND(6)) + MAT_TMP(34)
                NOR_MAT(LAI(8)+IND(7)) = NOR_MAT(LAI(8)+IND(7)) + MAT_TMP(35)
                NOR_MAT(LAI(8)+IND(8)) = NOR_MAT(LAI(8)+IND(8)) + MAT_TMP(36)
                NOR_MAT(LAI(9)+IND(1)) = NOR_MAT(LAI(9)+IND(1)) + MAT_TMP(37)
                NOR_MAT(LAI(9)+IND(2)) = NOR_MAT(LAI(9)+IND(2)) + MAT_TMP(38)
                NOR_MAT(LAI(9)+IND(3)) = NOR_MAT(LAI(9)+IND(3)) + MAT_TMP(39)
                NOR_MAT(LAI(9)+IND(4)) = NOR_MAT(LAI(9)+IND(4)) + MAT_TMP(40)
                NOR_MAT(LAI(9)+IND(5)) = NOR_MAT(LAI(9)+IND(5)) + MAT_TMP(41)
                NOR_MAT(LAI(9)+IND(6)) = NOR_MAT(LAI(9)+IND(6)) + MAT_TMP(42)
                NOR_MAT(LAI(9)+IND(7)) = NOR_MAT(LAI(9)+IND(7)) + MAT_TMP(43)
                NOR_MAT(LAI(9)+IND(8)) = NOR_MAT(LAI(9)+IND(8)) + MAT_TMP(44)
                NOR_MAT(LAI(9)+IND(9)) = NOR_MAT(LAI(9)+IND(9)) + MAT_TMP(45)
                NOR_MAT(LAI(10)+IND(1)) = NOR_MAT(LAI(10)+IND(1)) + MAT_TMP(46)
                NOR_MAT(LAI(10)+IND(2)) = NOR_MAT(LAI(10)+IND(2)) + MAT_TMP(47)
                NOR_MAT(LAI(10)+IND(3)) = NOR_MAT(LAI(10)+IND(3)) + MAT_TMP(48)
                NOR_MAT(LAI(10)+IND(4)) = NOR_MAT(LAI(10)+IND(4)) + MAT_TMP(49)
                NOR_MAT(LAI(10)+IND(5)) = NOR_MAT(LAI(10)+IND(5)) + MAT_TMP(50)
                NOR_MAT(LAI(10)+IND(6)) = NOR_MAT(LAI(10)+IND(6)) + MAT_TMP(51)
                NOR_MAT(LAI(10)+IND(7)) = NOR_MAT(LAI(10)+IND(7)) + MAT_TMP(52)
                NOR_MAT(LAI(10)+IND(8)) = NOR_MAT(LAI(10)+IND(8)) + MAT_TMP(53)
                NOR_MAT(LAI(10)+IND(9)) = NOR_MAT(LAI(10)+IND(9)) + MAT_TMP(54)
                NOR_MAT(LAI(10)+IND(10)) = NOR_MAT(LAI(10)+IND(10)) + MAT_TMP(55)
                NOR_MAT(LAI(11)+IND(1)) = NOR_MAT(LAI(11)+IND(1)) + MAT_TMP(56)
                NOR_MAT(LAI(11)+IND(2)) = NOR_MAT(LAI(11)+IND(2)) + MAT_TMP(57)
                NOR_MAT(LAI(11)+IND(3)) = NOR_MAT(LAI(11)+IND(3)) + MAT_TMP(58)
                NOR_MAT(LAI(11)+IND(4)) = NOR_MAT(LAI(11)+IND(4)) + MAT_TMP(59)
                NOR_MAT(LAI(11)+IND(5)) = NOR_MAT(LAI(11)+IND(5)) + MAT_TMP(60)
                NOR_MAT(LAI(11)+IND(6)) = NOR_MAT(LAI(11)+IND(6)) + MAT_TMP(61)
                NOR_MAT(LAI(11)+IND(7)) = NOR_MAT(LAI(11)+IND(7)) + MAT_TMP(62)
                NOR_MAT(LAI(11)+IND(8)) = NOR_MAT(LAI(11)+IND(8)) + MAT_TMP(63)
                NOR_MAT(LAI(11)+IND(9)) = NOR_MAT(LAI(11)+IND(9)) + MAT_TMP(64)
                NOR_MAT(LAI(11)+IND(10)) = NOR_MAT(LAI(11)+IND(10)) + MAT_TMP(65)
                NOR_MAT(LAI(11)+IND(11)) = NOR_MAT(LAI(11)+IND(11)) + MAT_TMP(66)
                RETURN
             ELSE IF ( N .EQ. 12 ) THEN
                MAT_TMP(1)  = WEI*EQCON(1)*EQCON(1)
                MAT_TMP(2)  = WEI*EQCON(2)*EQCON(1)
                MAT_TMP(3)  = WEI*EQCON(2)*EQCON(2)
                MAT_TMP(4)  = WEI*EQCON(3)*EQCON(1)
                MAT_TMP(5)  = WEI*EQCON(3)*EQCON(2)
                MAT_TMP(6)  = WEI*EQCON(3)*EQCON(3)
                MAT_TMP(7)  = WEI*EQCON(4)*EQCON(1)
                MAT_TMP(8)  = WEI*EQCON(4)*EQCON(2)
                MAT_TMP(9)  = WEI*EQCON(4)*EQCON(3)
                MAT_TMP(10) = WEI*EQCON(4)*EQCON(4)
                MAT_TMP(11) = WEI*EQCON(5)*EQCON(1)
                MAT_TMP(12) = WEI*EQCON(5)*EQCON(2)
                MAT_TMP(13) = WEI*EQCON(5)*EQCON(3)
                MAT_TMP(14) = WEI*EQCON(5)*EQCON(4)
                MAT_TMP(15) = WEI*EQCON(5)*EQCON(5)
                MAT_TMP(16) = WEI*EQCON(6)*EQCON(1)
                MAT_TMP(17) = WEI*EQCON(6)*EQCON(2)
                MAT_TMP(18) = WEI*EQCON(6)*EQCON(3)
                MAT_TMP(19) = WEI*EQCON(6)*EQCON(4)
                MAT_TMP(20) = WEI*EQCON(6)*EQCON(5)
                MAT_TMP(21) = WEI*EQCON(6)*EQCON(6)
                MAT_TMP(22) = WEI*EQCON(7)*EQCON(1)
                MAT_TMP(23) = WEI*EQCON(7)*EQCON(2)
                MAT_TMP(24) = WEI*EQCON(7)*EQCON(3)
                MAT_TMP(25) = WEI*EQCON(7)*EQCON(4)
                MAT_TMP(26) = WEI*EQCON(7)*EQCON(5)
                MAT_TMP(27) = WEI*EQCON(7)*EQCON(6)
                MAT_TMP(28) = WEI*EQCON(7)*EQCON(7)
                MAT_TMP(29) = WEI*EQCON(8)*EQCON(1)
                MAT_TMP(30) = WEI*EQCON(8)*EQCON(2)
                MAT_TMP(31) = WEI*EQCON(8)*EQCON(3)
                MAT_TMP(32) = WEI*EQCON(8)*EQCON(4)
                MAT_TMP(33) = WEI*EQCON(8)*EQCON(5)
                MAT_TMP(34) = WEI*EQCON(8)*EQCON(6)
                MAT_TMP(35) = WEI*EQCON(8)*EQCON(7)
                MAT_TMP(36) = WEI*EQCON(8)*EQCON(8)
                MAT_TMP(37) = WEI*EQCON(9)*EQCON(1)
                MAT_TMP(38) = WEI*EQCON(9)*EQCON(2)
                MAT_TMP(39) = WEI*EQCON(9)*EQCON(3)
                MAT_TMP(40) = WEI*EQCON(9)*EQCON(4)
                MAT_TMP(41) = WEI*EQCON(9)*EQCON(5)
                MAT_TMP(42) = WEI*EQCON(9)*EQCON(6)
                MAT_TMP(43) = WEI*EQCON(9)*EQCON(7)
                MAT_TMP(44) = WEI*EQCON(9)*EQCON(8)
                MAT_TMP(45) = WEI*EQCON(9)*EQCON(9)
                MAT_TMP(46) = WEI*EQCON(10)*EQCON(1)
                MAT_TMP(47) = WEI*EQCON(10)*EQCON(2)
                MAT_TMP(48) = WEI*EQCON(10)*EQCON(3)
                MAT_TMP(49) = WEI*EQCON(10)*EQCON(4)
                MAT_TMP(50) = WEI*EQCON(10)*EQCON(5)
                MAT_TMP(51) = WEI*EQCON(10)*EQCON(6)
                MAT_TMP(52) = WEI*EQCON(10)*EQCON(7)
                MAT_TMP(53) = WEI*EQCON(10)*EQCON(8)
                MAT_TMP(54) = WEI*EQCON(10)*EQCON(9)
                MAT_TMP(55) = WEI*EQCON(10)*EQCON(10)
                MAT_TMP(56) = WEI*EQCON(11)*EQCON(1)
                MAT_TMP(57) = WEI*EQCON(11)*EQCON(2)
                MAT_TMP(58) = WEI*EQCON(11)*EQCON(3)
                MAT_TMP(59) = WEI*EQCON(11)*EQCON(4)
                MAT_TMP(60) = WEI*EQCON(11)*EQCON(5)
                MAT_TMP(61) = WEI*EQCON(11)*EQCON(6)
                MAT_TMP(62) = WEI*EQCON(11)*EQCON(7)
                MAT_TMP(63) = WEI*EQCON(11)*EQCON(8)
                MAT_TMP(64) = WEI*EQCON(11)*EQCON(9)
                MAT_TMP(65) = WEI*EQCON(11)*EQCON(10)
                MAT_TMP(66) = WEI*EQCON(11)*EQCON(11)
                MAT_TMP(67) = WEI*EQCON(12)*EQCON(1)
                MAT_TMP(68) = WEI*EQCON(12)*EQCON(2)
                MAT_TMP(69) = WEI*EQCON(12)*EQCON(3)
                MAT_TMP(70) = WEI*EQCON(12)*EQCON(4)
                MAT_TMP(71) = WEI*EQCON(12)*EQCON(5)
                MAT_TMP(72) = WEI*EQCON(12)*EQCON(6)
                MAT_TMP(73) = WEI*EQCON(12)*EQCON(7)
                MAT_TMP(74) = WEI*EQCON(12)*EQCON(8)
                MAT_TMP(75) = WEI*EQCON(12)*EQCON(9)
                MAT_TMP(76) = WEI*EQCON(12)*EQCON(10)
                MAT_TMP(77) = WEI*EQCON(12)*EQCON(11)
                MAT_TMP(78) = WEI*EQCON(12)*EQCON(12)
!
                NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
                NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
                NOR_VEC( IND(3) ) = NOR_VEC( IND(3) ) + WEI*EQCON(3)*OC
                NOR_VEC( IND(4) ) = NOR_VEC( IND(4) ) + WEI*EQCON(4)*OC
                NOR_VEC( IND(5) ) = NOR_VEC( IND(5) ) + WEI*EQCON(5)*OC
                NOR_VEC( IND(6) ) = NOR_VEC( IND(6) ) + WEI*EQCON(6)*OC
                NOR_VEC( IND(7) ) = NOR_VEC( IND(7) ) + WEI*EQCON(7)*OC
                NOR_VEC( IND(8) ) = NOR_VEC( IND(8) ) + WEI*EQCON(8)*OC
                NOR_VEC( IND(9) ) = NOR_VEC( IND(9) ) + WEI*EQCON(9)*OC
                NOR_VEC( IND(10) ) = NOR_VEC( IND(10) ) + WEI*EQCON(10)*OC
                NOR_VEC( IND(11) ) = NOR_VEC( IND(11) ) + WEI*EQCON(11)*OC
                NOR_VEC( IND(12) ) = NOR_VEC( IND(12) ) + WEI*EQCON(12)*OC
!
                NOR_MAT(LAI(1)+IND(1)) = NOR_MAT(LAI(1)+IND(1)) + MAT_TMP(1)
                NOR_MAT(LAI(2)+IND(1)) = NOR_MAT(LAI(2)+IND(1)) + MAT_TMP(2)
                NOR_MAT(LAI(2)+IND(2)) = NOR_MAT(LAI(2)+IND(2)) + MAT_TMP(3)
                NOR_MAT(LAI(3)+IND(1)) = NOR_MAT(LAI(3)+IND(1)) + MAT_TMP(4)
                NOR_MAT(LAI(3)+IND(2)) = NOR_MAT(LAI(3)+IND(2)) + MAT_TMP(5)
                NOR_MAT(LAI(3)+IND(3)) = NOR_MAT(LAI(3)+IND(3)) + MAT_TMP(6)
                NOR_MAT(LAI(4)+IND(1)) = NOR_MAT(LAI(4)+IND(1)) + MAT_TMP(7)
                NOR_MAT(LAI(4)+IND(2)) = NOR_MAT(LAI(4)+IND(2)) + MAT_TMP(8)
                NOR_MAT(LAI(4)+IND(3)) = NOR_MAT(LAI(4)+IND(3)) + MAT_TMP(9)
                NOR_MAT(LAI(4)+IND(4)) = NOR_MAT(LAI(4)+IND(4)) + MAT_TMP(10)
                NOR_MAT(LAI(5)+IND(1)) = NOR_MAT(LAI(5)+IND(1)) + MAT_TMP(11)
                NOR_MAT(LAI(5)+IND(2)) = NOR_MAT(LAI(5)+IND(2)) + MAT_TMP(12)
                NOR_MAT(LAI(5)+IND(3)) = NOR_MAT(LAI(5)+IND(3)) + MAT_TMP(13)
                NOR_MAT(LAI(5)+IND(4)) = NOR_MAT(LAI(5)+IND(4)) + MAT_TMP(14)
                NOR_MAT(LAI(5)+IND(5)) = NOR_MAT(LAI(5)+IND(5)) + MAT_TMP(15)
                NOR_MAT(LAI(6)+IND(1)) = NOR_MAT(LAI(6)+IND(1)) + MAT_TMP(16)
                NOR_MAT(LAI(6)+IND(2)) = NOR_MAT(LAI(6)+IND(2)) + MAT_TMP(17)
                NOR_MAT(LAI(6)+IND(3)) = NOR_MAT(LAI(6)+IND(3)) + MAT_TMP(18)
                NOR_MAT(LAI(6)+IND(4)) = NOR_MAT(LAI(6)+IND(4)) + MAT_TMP(19)
                NOR_MAT(LAI(6)+IND(5)) = NOR_MAT(LAI(6)+IND(5)) + MAT_TMP(20)
                NOR_MAT(LAI(6)+IND(6)) = NOR_MAT(LAI(6)+IND(6)) + MAT_TMP(21)
                NOR_MAT(LAI(7)+IND(1)) = NOR_MAT(LAI(7)+IND(1)) + MAT_TMP(22)
                NOR_MAT(LAI(7)+IND(2)) = NOR_MAT(LAI(7)+IND(2)) + MAT_TMP(23)
                NOR_MAT(LAI(7)+IND(3)) = NOR_MAT(LAI(7)+IND(3)) + MAT_TMP(24)
                NOR_MAT(LAI(7)+IND(4)) = NOR_MAT(LAI(7)+IND(4)) + MAT_TMP(25)
                NOR_MAT(LAI(7)+IND(5)) = NOR_MAT(LAI(7)+IND(5)) + MAT_TMP(26)
                NOR_MAT(LAI(7)+IND(6)) = NOR_MAT(LAI(7)+IND(6)) + MAT_TMP(27)
                NOR_MAT(LAI(7)+IND(7)) = NOR_MAT(LAI(7)+IND(7)) + MAT_TMP(28)
                NOR_MAT(LAI(8)+IND(1)) = NOR_MAT(LAI(8)+IND(1)) + MAT_TMP(29)
                NOR_MAT(LAI(8)+IND(2)) = NOR_MAT(LAI(8)+IND(2)) + MAT_TMP(30)
                NOR_MAT(LAI(8)+IND(3)) = NOR_MAT(LAI(8)+IND(3)) + MAT_TMP(31)
                NOR_MAT(LAI(8)+IND(4)) = NOR_MAT(LAI(8)+IND(4)) + MAT_TMP(32)
                NOR_MAT(LAI(8)+IND(5)) = NOR_MAT(LAI(8)+IND(5)) + MAT_TMP(33)
                NOR_MAT(LAI(8)+IND(6)) = NOR_MAT(LAI(8)+IND(6)) + MAT_TMP(34)
                NOR_MAT(LAI(8)+IND(7)) = NOR_MAT(LAI(8)+IND(7)) + MAT_TMP(35)
                NOR_MAT(LAI(8)+IND(8)) = NOR_MAT(LAI(8)+IND(8)) + MAT_TMP(36)
                NOR_MAT(LAI(9)+IND(1)) = NOR_MAT(LAI(9)+IND(1)) + MAT_TMP(37)
                NOR_MAT(LAI(9)+IND(2)) = NOR_MAT(LAI(9)+IND(2)) + MAT_TMP(38)
                NOR_MAT(LAI(9)+IND(3)) = NOR_MAT(LAI(9)+IND(3)) + MAT_TMP(39)
                NOR_MAT(LAI(9)+IND(4)) = NOR_MAT(LAI(9)+IND(4)) + MAT_TMP(40)
                NOR_MAT(LAI(9)+IND(5)) = NOR_MAT(LAI(9)+IND(5)) + MAT_TMP(41)
                NOR_MAT(LAI(9)+IND(6)) = NOR_MAT(LAI(9)+IND(6)) + MAT_TMP(42)
                NOR_MAT(LAI(9)+IND(7)) = NOR_MAT(LAI(9)+IND(7)) + MAT_TMP(43)
                NOR_MAT(LAI(9)+IND(8)) = NOR_MAT(LAI(9)+IND(8)) + MAT_TMP(44)
                NOR_MAT(LAI(9)+IND(9)) = NOR_MAT(LAI(9)+IND(9)) + MAT_TMP(45)
                NOR_MAT(LAI(10)+IND(1)) = NOR_MAT(LAI(10)+IND(1)) + MAT_TMP(46)
                NOR_MAT(LAI(10)+IND(2)) = NOR_MAT(LAI(10)+IND(2)) + MAT_TMP(47)
                NOR_MAT(LAI(10)+IND(3)) = NOR_MAT(LAI(10)+IND(3)) + MAT_TMP(48)
                NOR_MAT(LAI(10)+IND(4)) = NOR_MAT(LAI(10)+IND(4)) + MAT_TMP(49)
                NOR_MAT(LAI(10)+IND(5)) = NOR_MAT(LAI(10)+IND(5)) + MAT_TMP(50)
                NOR_MAT(LAI(10)+IND(6)) = NOR_MAT(LAI(10)+IND(6)) + MAT_TMP(51)
                NOR_MAT(LAI(10)+IND(7)) = NOR_MAT(LAI(10)+IND(7)) + MAT_TMP(52)
                NOR_MAT(LAI(10)+IND(8)) = NOR_MAT(LAI(10)+IND(8)) + MAT_TMP(53)
                NOR_MAT(LAI(10)+IND(9)) = NOR_MAT(LAI(10)+IND(9)) + MAT_TMP(54)
                NOR_MAT(LAI(10)+IND(10)) = NOR_MAT(LAI(10)+IND(10)) + MAT_TMP(55)
                NOR_MAT(LAI(11)+IND(1)) = NOR_MAT(LAI(11)+IND(1)) + MAT_TMP(56)
                NOR_MAT(LAI(11)+IND(2)) = NOR_MAT(LAI(11)+IND(2)) + MAT_TMP(57)
                NOR_MAT(LAI(11)+IND(3)) = NOR_MAT(LAI(11)+IND(3)) + MAT_TMP(58)
                NOR_MAT(LAI(11)+IND(4)) = NOR_MAT(LAI(11)+IND(4)) + MAT_TMP(59)
                NOR_MAT(LAI(11)+IND(5)) = NOR_MAT(LAI(11)+IND(5)) + MAT_TMP(60)
                NOR_MAT(LAI(11)+IND(6)) = NOR_MAT(LAI(11)+IND(6)) + MAT_TMP(61)
                NOR_MAT(LAI(11)+IND(7)) = NOR_MAT(LAI(11)+IND(7)) + MAT_TMP(62)
                NOR_MAT(LAI(11)+IND(8)) = NOR_MAT(LAI(11)+IND(8)) + MAT_TMP(63)
                NOR_MAT(LAI(11)+IND(9)) = NOR_MAT(LAI(11)+IND(9)) + MAT_TMP(64)
                NOR_MAT(LAI(11)+IND(10)) = NOR_MAT(LAI(11)+IND(10)) + MAT_TMP(65)
                NOR_MAT(LAI(11)+IND(11)) = NOR_MAT(LAI(11)+IND(11)) + MAT_TMP(66)
                NOR_MAT(LAI(12)+IND(1)) = NOR_MAT(LAI(12)+IND(1)) + MAT_TMP(67)
                NOR_MAT(LAI(12)+IND(2)) = NOR_MAT(LAI(12)+IND(2)) + MAT_TMP(68)
                NOR_MAT(LAI(12)+IND(3)) = NOR_MAT(LAI(12)+IND(3)) + MAT_TMP(69)
                NOR_MAT(LAI(12)+IND(4)) = NOR_MAT(LAI(12)+IND(4)) + MAT_TMP(70)
                NOR_MAT(LAI(12)+IND(5)) = NOR_MAT(LAI(12)+IND(5)) + MAT_TMP(71)
                NOR_MAT(LAI(12)+IND(6)) = NOR_MAT(LAI(12)+IND(6)) + MAT_TMP(72)
                NOR_MAT(LAI(12)+IND(7)) = NOR_MAT(LAI(12)+IND(7)) + MAT_TMP(73)
                NOR_MAT(LAI(12)+IND(8)) = NOR_MAT(LAI(12)+IND(8)) + MAT_TMP(74)
                NOR_MAT(LAI(12)+IND(9)) = NOR_MAT(LAI(12)+IND(9)) + MAT_TMP(75)
                NOR_MAT(LAI(12)+IND(10)) = NOR_MAT(LAI(12)+IND(10)) + MAT_TMP(76)
                NOR_MAT(LAI(12)+IND(11)) = NOR_MAT(LAI(12)+IND(11)) + MAT_TMP(77)
                NOR_MAT(LAI(12)+IND(12)) = NOR_MAT(LAI(12)+IND(12)) + MAT_TMP(78)
                RETURN
           END IF
!
! -------- Case of big dimensions and incices in order
!
           DO 430 J3=1,N
!
! ----------- Updating normal matrix
!
              DO 440 J4=1,J3
                 LA = LAJ(J3) + IND(J4)
                 NOR_MAT(LA) = NOR_MAT(LA) + WEI*EQCON(J3)*EQCON(J4)
 440          CONTINUE
!
! ----------- Updating normal vector (NB: brackets in (WEI*OC) are necessary to
!------------ keep succession with teh old version of SOLVE)
!
              NOR_VEC( IND(J3) ) = NOR_VEC( IND(J3) ) + EQCON(J3)*(WEI*OC)
 430       CONTINUE
           DEALLOCATE ( LAJ ) 
         ELSE IF ( N == 2 ) THEN
!
! -------- The indices were NOT in increasing order.
!          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! -------- We have to use slow algorithm which checks indices
!
           LA = LAI(1) + IND(1)
           NOR_MAT(LA) = NOR_MAT(LA) + WEI*EQCON(1)*EQCON(1)
           IF ( IND(2) .LE. IND(1) ) THEN
                LA = LAI(1) + IND(2)
                NOR_MAT(LA) = NOR_MAT(LA) + WEI*EQCON(1)*EQCON(2)
             ELSE 
                LA = LAI(2) + IND(1)
                NOR_MAT(LA) = NOR_MAT(LA) + WEI*EQCON(1)*EQCON(2)
           END IF
           LA = LAI(2) + IND(2)
           NOR_MAT(LA) = NOR_MAT(LA) + WEI*EQCON(2)*EQCON(2)
           NOR_VEC( IND(1) ) = NOR_VEC( IND(1) ) + WEI*EQCON(1)*OC
           NOR_VEC( IND(2) ) = NOR_VEC( IND(2) ) + WEI*EQCON(2)*OC
           RETURN
         ELSE IF ( N .LE. MS ) THEN
           DO 450 J5=1,N
!
! ----------- Updating normal matrix
!
              DO 460 J6=1,N
                 IF ( IND(J6) .LE. IND(J5) ) THEN ! this condition bypass lower
                      LA = LAI(J5) + IND(J6)
                      NOR_MAT(LA) = NOR_MAT(LA) + WEI*EQCON(J5)*EQCON(J6)
                 END IF
 460          CONTINUE
!
! ----------- Updating normal vector (NB: brackets in (WEI*OC) are necessary to
! ----------- keep succession with old version of SOLVE)
!
              NOR_VEC( IND(J5) ) = NOR_VEC( IND(J5) ) + EQCON(J5)*(WEI*OC)
 450       CONTINUE
         ELSE
           DO 470 J7=1,N
!
! ----------- Updating normal matrix
!
              DO 480 J8=1,N
                 IF ( IND(J8) .LE. IND(J7) ) THEN ! this condition bypass lower
!                                                 ! diagonal elements-phantom
                      LA = LAJ(J7) + IND(J8)
                      NOR_MAT(LA) = NOR_MAT(LA) + WEI*EQCON(J7)*EQCON(J8)
                 END IF
 480         CONTINUE
!
! ----------- Updating normal vector (NB: brackets in (WEI*OC) are necessary to
! ----------- keep succession with old version of SOLVE)
!
              NOR_VEC( IND(J7) ) = NOR_VEC( IND(J7) ) + EQCON(J7)*(WEI*OC)
 470       CONTINUE
           DEALLOCATE ( LAJ ) 
      END IF
!
      RETURN
      END  !#!  ADD_TRG  #!#
