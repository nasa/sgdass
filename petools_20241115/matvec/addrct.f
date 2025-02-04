      SUBROUTINE ADD_RCT ( OC, SIGMA, &
     &                     N1, IND1, EQ1, &
     &                     N2, IND2, EQ2, &
     &                     M1, M2,   BND_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_RCT  updates rectangular part of the normal matrix    *
! *   for the next observation. Vector of equation of conditions assumed *
! *   to be sparse, but only non-zero elements are being bassed in       *
! *   routine. Indices of non-zero elements are in the vectors IND1 and  *
! *   IND2. Vectors IND1, IND2 can be unsorted.                          *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      OC ( REAL*8     ) -- Right part of the equation of condition.   *
! *   SIGMA ( REAL*8     ) -- A priori uncertainty of the observations.  *
! *      N1 ( INTEGER*4  ) -- Number of non-zero elements in the first   *
! *                           equation of condition.                     *
! *    IND1 ( INTEGER*4  ) -- Vector of indices of non-zero elements in  *
! *                           the 1-st equation of condition.            *
! *     EQ1 ( REAL*8     ) -- Squeezed vector of non-zero elements of    *
! *                           the 1-st equation of condition. It         *
! *                           contains only non-zero elements.           *
! *                           Dimension: N1.                             *
! *      N2 ( INTEGER*4  ) -- Number of non-zero elements in the second  *
! *                           equation of condition.                     *
! *    IND2 ( INTEGER*4  ) -- Vector of indices of non-zero elements in  *
! *                           the 2-nd equation of condition.            *
! *     EQ2 ( REAL*8     ) -- Squeezed vector of non-zero elements of    *
! *                           the 2-nd equation of condition. It         *
! *                           contains only non-zero elements.           *
! *                           Dimension: N2.                             *
! *      M1 ( INTEGER*4  ) -- Total number of elements in the 1-st       *
! *                           equation of cond. (including zero elements)*
! *      M2 ( INTEGER*4  ) -- Total number of elements in the 2-nd       *
! *                           equation of cond. (including zero elements)*
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! * BND_MAT ( REAL*8     ) -- Segment of normal matrix. Dimension: M1*M2 *
! *                                                                      *
! *   IMPORTANT: Should be compiled with switch +O2 and without -C -g    *
! *                                                                      *
! *  ### 10-APR-2001   ADD_RCT     v1.0 (c)  L. Petrov  10-APR-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Defintion of DB__ADD_RCT
      INTEGER*4  N1, N2, IND1(N1), IND2(N2), M1, M2
      REAL*8     OC, SIGMA, EQ1(N1), EQ2(N2), WEI, BND_MAT(M1,M2)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IN2, IP, IR
!
      WEI=1.D0/SIGMA**2
!
! --- First solve cases N1 = 1, 2, 3, 4, 5, 6, 7
!
      IF ( N1 .EQ. 1 ) THEN
           DO 410 J1=1,N2
              BND_MAT(IND1(1),IND2(J1)) = BND_MAT(IND1(1),IND2(J1)) + &
     &                                    WEI*EQ1(1)*EQ2(J1)
 410       CONTINUE
           RETURN
        ELSE IF ( N1 .EQ. 2 ) THEN
           DO 420 J2=1,N2
              IN2 = IND2(J2)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J2)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J2)
 420       CONTINUE
           RETURN
        ELSE IF ( N1 .EQ. 3 ) THEN
           DO 430 J3=1,N2
              IN2 = IND2(J3)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J3)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J3)
              BND_MAT(IND1(3),IN2)=BND_MAT(IND1(3),IN2) + WEI*EQ1(3)*EQ2(J3)
 430       CONTINUE
           RETURN
        ELSE IF ( N1 .EQ. 4 ) THEN
           DO 440 J4=1,N2
              IN2 = IND2(J4)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J4)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J4)
              BND_MAT(IND1(3),IN2)=BND_MAT(IND1(3),IN2) + WEI*EQ1(3)*EQ2(J4)
              BND_MAT(IND1(4),IN2)=BND_MAT(IND1(4),IN2) + WEI*EQ1(4)*EQ2(J4)
 440       CONTINUE
           RETURN
        ELSE IF ( N1 .EQ. 5 ) THEN
           DO 450 J5=1,N2
              IN2 = IND2(J5)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J5)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J5)
              BND_MAT(IND1(3),IN2)=BND_MAT(IND1(3),IN2) + WEI*EQ1(3)*EQ2(J5)
              BND_MAT(IND1(4),IN2)=BND_MAT(IND1(4),IN2) + WEI*EQ1(4)*EQ2(J5)
              BND_MAT(IND1(5),IN2)=BND_MAT(IND1(5),IN2) + WEI*EQ1(5)*EQ2(J5)
 450       CONTINUE
           RETURN
        ELSE IF ( N1 .EQ. 6 ) THEN
           DO 460 J6=1,N2
              IN2 = IND2(J6)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J6)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J6)
              BND_MAT(IND1(3),IN2)=BND_MAT(IND1(3),IN2) + WEI*EQ1(3)*EQ2(J6)
              BND_MAT(IND1(4),IN2)=BND_MAT(IND1(4),IN2) + WEI*EQ1(4)*EQ2(J6)
              BND_MAT(IND1(5),IN2)=BND_MAT(IND1(5),IN2) + WEI*EQ1(5)*EQ2(J6)
              BND_MAT(IND1(6),IN2)=BND_MAT(IND1(6),IN2) + WEI*EQ1(6)*EQ2(J6)
 460       CONTINUE
           RETURN
        ELSE IF ( N1 .EQ. 7 ) THEN
           DO 470 J7=1,N2
              IN2 = IND2(J7)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J7)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J7)
              BND_MAT(IND1(3),IN2)=BND_MAT(IND1(3),IN2) + WEI*EQ1(3)*EQ2(J7)
              BND_MAT(IND1(4),IN2)=BND_MAT(IND1(4),IN2) + WEI*EQ1(4)*EQ2(J7)
              BND_MAT(IND1(5),IN2)=BND_MAT(IND1(5),IN2) + WEI*EQ1(5)*EQ2(J7)
              BND_MAT(IND1(6),IN2)=BND_MAT(IND1(6),IN2) + WEI*EQ1(6)*EQ2(J7)
              BND_MAT(IND1(7),IN2)=BND_MAT(IND1(7),IN2) + WEI*EQ1(7)*EQ2(J7)
 470       CONTINUE
           RETURN
      END IF
!
! --- In the case if N1 > 7, we update BND_MAT by parts with DB__ADD_RCT
! --- indeces in each part
!
! --- compute IP -- number of full parts with DB__ADD_RCT indeces in each,
! --- IR -- number of indeces in the last,  IP+1 -th portion
!
      IP = N1/DB__ADD_RCT
      IR = N1 - IP*DB__ADD_RCT
      IF ( IR .EQ. 0 ) THEN
           IR = DB__ADD_RCT
           IP = IP - 1
      END IF
!
      IF ( IP .EQ. 0 ) THEN
!
! -------- No parts ( N1 < DB__ADD_RCT   )
!
           CALL ADDRCT_UPD ( OC, SIGMA, &
     &                       N1, IND1, EQ1, &
     &                       N2, IND2, EQ2, &
     &                       M1, M2,   BND_MAT )
         ELSE
!
! -------- First update matrix in each parts
!
           DO 480 J8=1,IP
              IP = 1 + (J8-1)*DB__ADD_RCT
              CALL ADDRCT_UPD ( OC, SIGMA, &
     &                          DB__ADD_RCT, IND1(IP), EQ1(IP), &
     &                          N2, IND2, EQ2, &
     &                          M1, M2,   BND_MAT )
 480       CONTINUE
           IP = IP + DB__ADD_RCT
           IF ( IR .GT. 0 ) THEN
!
! ------------- ... then update in the last IP+1 -th part
!
                CALL ADDRCT_UPD ( OC, SIGMA, &
     &                            IR, IND1(IP), EQ1(IP), &
     &                            N2, IND2, EQ2, &
     &                            M1, M2,   BND_MAT )
           END IF
      END IF
!
      RETURN
      END  !#!  ADD_RCT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADDRCT_UPD ( OC, SIGMA, &
     &                        N1, IND1, EQ1, &
     &                        N2, IND2, EQ2, &
     &                        M1, M2,   BND_MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_RCT  updates rectangular part of the normal matrix    *
! *   for the next observation. Vector of equation of conditions assumed *
! *   to be sparse, but only non-zero elements are being bassed in       *
! *   routine. Indices of non-zero elements are in the vectors IND1 and  *
! *   IND2. Vectors IND1, IND2 can be unsorted.                          *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      OC ( REAL*8     ) -- Right part of the equation of condition.   *
! *   SIGMA ( REAL*8     ) -- A priori uncertainty of the observations.  *
! *      N1 ( INTEGER*4  ) -- Number of non-zero elements in the first   *
! *                           equation of condition.                     *
! *    IND1 ( INTEGER*4  ) -- Vector of indices of non-zero elements in  *
! *                           the 1-st equation of condition.            *
! *     EQ1 ( REAL*8     ) -- Squeezed vector of non-zero elements of    *
! *                           the 1-st equation of condition. It         *
! *                           contains only non-zero elements.           *
! *                           Dimension: N1.                             *
! *      N2 ( INTEGER*4  ) -- Number of non-zero elements in the second  *
! *                           equation of condition.                     *
! *    IND2 ( INTEGER*4  ) -- Vector of indices of non-zero elements in  *
! *                           the 2-nd equation of condition.            *
! *     EQ2 ( REAL*8     ) -- Squeezed vector of non-zero elements of    *
! *                           the 2-nd equation of condition. It         *
! *                           contains only non-zero elements.           *
! *                           Dimension: N2.                             *
! *      M1 ( INTEGER*4  ) -- Total number of elements in the 1-st       *
! *                           equation of cond. (including zero elements)*
! *      M2 ( INTEGER*4  ) -- Total number of elements in the 2-nd       *
! *                           equation of cond. (including zero elements)*
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! * BND_MAT ( REAL*8     ) -- Segment of normal matrix. Dimension: M1*M2 *
! *                                                                      *
! *   IMPORTANT: Should be compiled with switch +O2 and without -C -g    *
! *                                                                      *
! *  ###  11-APR-2001  ADDRCT_UPD  v1.0 (c)  L. Petrov  11-APR-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Defintion of DB__ADD_RCT
      INTEGER*4  N1, N2, IND1(N1), IND2(N2), M1, M2
      REAL*8     OC, SIGMA, EQ1(N1), EQ2(N2), WEI, BND_MAT(M1,M2)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, IN1, IN2
!
      WEI=1.D0/SIGMA**2
!
! --- special tric for optimization
!
      IF ( N1 .EQ. 1 ) THEN
           DO 410 J1=1,N2
              BND_MAT(IND1(1),IND2(J1)) = BND_MAT(IND1(1),IND2(J1)) + &
     &                                    WEI*EQ1(1)*EQ2(J1)
 410       CONTINUE
        ELSE IF ( N1 .EQ. 2 ) THEN
           DO 420 J2=1,N2
              IN2 = IND2(J2)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J2)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J2)
 420       CONTINUE
        ELSE IF ( N1 .EQ. 3 ) THEN
           DO 430 J3=1,N2
              IN2 = IND2(J3)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J3)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J3)
              BND_MAT(IND1(3),IN2)=BND_MAT(IND1(3),IN2) + WEI*EQ1(3)*EQ2(J3)
 430       CONTINUE
        ELSE IF ( N1 .EQ. 4 ) THEN
           DO 440 J4=1,N2
              IN2 = IND2(J4)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J4)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J4)
              BND_MAT(IND1(3),IN2)=BND_MAT(IND1(3),IN2) + WEI*EQ1(3)*EQ2(J4)
              BND_MAT(IND1(4),IN2)=BND_MAT(IND1(4),IN2) + WEI*EQ1(4)*EQ2(J4)
 440       CONTINUE
        ELSE IF ( N1 .EQ. 5 ) THEN
           DO 450 J5=1,N2
              IN2 = IND2(J5)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J5)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J5)
              BND_MAT(IND1(3),IN2)=BND_MAT(IND1(3),IN2) + WEI*EQ1(3)*EQ2(J5)
              BND_MAT(IND1(4),IN2)=BND_MAT(IND1(4),IN2) + WEI*EQ1(4)*EQ2(J5)
              BND_MAT(IND1(5),IN2)=BND_MAT(IND1(5),IN2) + WEI*EQ1(5)*EQ2(J5)
 450       CONTINUE
        ELSE IF ( N1 .EQ. 6 ) THEN
           DO 460 J6=1,N2
              IN2 = IND2(J6)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J6)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J6)
              BND_MAT(IND1(3),IN2)=BND_MAT(IND1(3),IN2) + WEI*EQ1(3)*EQ2(J6)
              BND_MAT(IND1(4),IN2)=BND_MAT(IND1(4),IN2) + WEI*EQ1(4)*EQ2(J6)
              BND_MAT(IND1(5),IN2)=BND_MAT(IND1(5),IN2) + WEI*EQ1(5)*EQ2(J6)
              BND_MAT(IND1(6),IN2)=BND_MAT(IND1(6),IN2) + WEI*EQ1(6)*EQ2(J6)
 460       CONTINUE
        ELSE IF ( N1 .LE. 64 ) THEN
           DO 470 J7=1,N2
              IN2 = IND2(J7)
              BND_MAT(IND1(1),IN2)=BND_MAT(IND1(1),IN2) + WEI*EQ1(1)*EQ2(J7)
              BND_MAT(IND1(2),IN2)=BND_MAT(IND1(2),IN2) + WEI*EQ1(2)*EQ2(J7)
              BND_MAT(IND1(3),IN2)=BND_MAT(IND1(3),IN2) + WEI*EQ1(3)*EQ2(J7)
              BND_MAT(IND1(4),IN2)=BND_MAT(IND1(4),IN2) + WEI*EQ1(4)*EQ2(J7)
              BND_MAT(IND1(5),IN2)=BND_MAT(IND1(5),IN2) + WEI*EQ1(5)*EQ2(J7)
              BND_MAT(IND1(6),IN2)=BND_MAT(IND1(6),IN2) + WEI*EQ1(6)*EQ2(J7)
              IF ( N1 .EQ. 6 ) GOTO 470
              BND_MAT(IND1(7),IN2)=BND_MAT(IND1(7),IN2) + WEI*EQ1(7)*EQ2(J7)
              IF ( N1 .EQ. 7 ) GOTO 470
              BND_MAT(IND1(8),IN2)=BND_MAT(IND1(8),IN2) + WEI*EQ1(8)*EQ2(J7)
              IF ( N1 .EQ. 8 ) GOTO 470
              BND_MAT(IND1(9),IN2)=BND_MAT(IND1(9),IN2) + WEI*EQ1(9)*EQ2(J7)
              IF ( N1 .EQ. 9 ) GOTO 470
              BND_MAT(IND1(10),IN2)=BND_MAT(IND1(10),IN2) + WEI*EQ1(10)*EQ2(J7)
              IF ( N1 .EQ. 10 ) GOTO 470
              BND_MAT(IND1(11),IN2)=BND_MAT(IND1(11),IN2) + WEI*EQ1(11)*EQ2(J7)
              IF ( N1 .EQ. 11 ) GOTO 470
              BND_MAT(IND1(12),IN2)=BND_MAT(IND1(12),IN2) + WEI*EQ1(12)*EQ2(J7)
              IF ( N1 .EQ. 12 ) GOTO 470
              BND_MAT(IND1(13),IN2)=BND_MAT(IND1(13),IN2) + WEI*EQ1(13)*EQ2(J7)
              IF ( N1 .EQ. 13 ) GOTO 470
              BND_MAT(IND1(14),IN2)=BND_MAT(IND1(14),IN2) + WEI*EQ1(14)*EQ2(J7)
              IF ( N1 .EQ. 14 ) GOTO 470
              BND_MAT(IND1(15),IN2)=BND_MAT(IND1(15),IN2) + WEI*EQ1(15)*EQ2(J7)
              IF ( N1 .EQ. 15 ) GOTO 470
              BND_MAT(IND1(16),IN2)=BND_MAT(IND1(16),IN2) + WEI*EQ1(16)*EQ2(J7)
              IF ( N1 .EQ. 16 ) GOTO 470
              BND_MAT(IND1(17),IN2)=BND_MAT(IND1(17),IN2) + WEI*EQ1(17)*EQ2(J7)
              IF ( N1 .EQ. 17 ) GOTO 470
              BND_MAT(IND1(18),IN2)=BND_MAT(IND1(18),IN2) + WEI*EQ1(18)*EQ2(J7)
              IF ( N1 .EQ. 18 ) GOTO 470
              BND_MAT(IND1(19),IN2)=BND_MAT(IND1(19),IN2) + WEI*EQ1(19)*EQ2(J7)
              IF ( N1 .EQ. 19 ) GOTO 470
              BND_MAT(IND1(20),IN2)=BND_MAT(IND1(20),IN2) + WEI*EQ1(20)*EQ2(J7)
              IF ( N1 .EQ. 20 ) GOTO 470
              BND_MAT(IND1(21),IN2)=BND_MAT(IND1(21),IN2) + WEI*EQ1(21)*EQ2(J7)
              IF ( N1 .EQ. 21 ) GOTO 470
              BND_MAT(IND1(22),IN2)=BND_MAT(IND1(22),IN2) + WEI*EQ1(22)*EQ2(J7)
              IF ( N1 .EQ. 22 ) GOTO 470
              BND_MAT(IND1(23),IN2)=BND_MAT(IND1(23),IN2) + WEI*EQ1(23)*EQ2(J7)
              IF ( N1 .EQ. 23 ) GOTO 470
              BND_MAT(IND1(24),IN2)=BND_MAT(IND1(24),IN2) + WEI*EQ1(24)*EQ2(J7)
              IF ( N1 .EQ. 24 ) GOTO 470
              BND_MAT(IND1(25),IN2)=BND_MAT(IND1(25),IN2) + WEI*EQ1(25)*EQ2(J7)
              IF ( N1 .EQ. 25 ) GOTO 470
              BND_MAT(IND1(26),IN2)=BND_MAT(IND1(26),IN2) + WEI*EQ1(26)*EQ2(J7)
              IF ( N1 .EQ. 26 ) GOTO 470
              BND_MAT(IND1(27),IN2)=BND_MAT(IND1(27),IN2) + WEI*EQ1(27)*EQ2(J7)
              IF ( N1 .EQ. 27 ) GOTO 470
              BND_MAT(IND1(28),IN2)=BND_MAT(IND1(28),IN2) + WEI*EQ1(28)*EQ2(J7)
              IF ( N1 .EQ. 28 ) GOTO 470
              BND_MAT(IND1(29),IN2)=BND_MAT(IND1(29),IN2) + WEI*EQ1(29)*EQ2(J7)
              IF ( N1 .EQ. 29 ) GOTO 470
              BND_MAT(IND1(30),IN2)=BND_MAT(IND1(30),IN2) + WEI*EQ1(30)*EQ2(J7)
              IF ( N1 .EQ. 30 ) GOTO 470
              BND_MAT(IND1(31),IN2)=BND_MAT(IND1(31),IN2) + WEI*EQ1(31)*EQ2(J7)
              IF ( N1 .EQ. 31 ) GOTO 470
              BND_MAT(IND1(32),IN2)=BND_MAT(IND1(32),IN2) + WEI*EQ1(32)*EQ2(J7)
              IF ( N1 .EQ. 32 ) GOTO 470
              BND_MAT(IND1(33),IN2)=BND_MAT(IND1(33),IN2) + WEI*EQ1(33)*EQ2(J7)
              IF ( N1 .EQ. 33 ) GOTO 470
              BND_MAT(IND1(34),IN2)=BND_MAT(IND1(34),IN2) + WEI*EQ1(34)*EQ2(J7)
              IF ( N1 .EQ. 34 ) GOTO 470
              BND_MAT(IND1(35),IN2)=BND_MAT(IND1(35),IN2) + WEI*EQ1(35)*EQ2(J7)
              IF ( N1 .EQ. 35 ) GOTO 470
              BND_MAT(IND1(36),IN2)=BND_MAT(IND1(36),IN2) + WEI*EQ1(36)*EQ2(J7)
              IF ( N1 .EQ. 36 ) GOTO 470
              BND_MAT(IND1(37),IN2)=BND_MAT(IND1(37),IN2) + WEI*EQ1(37)*EQ2(J7)
              IF ( N1 .EQ. 37 ) GOTO 470
              BND_MAT(IND1(38),IN2)=BND_MAT(IND1(38),IN2) + WEI*EQ1(38)*EQ2(J7)
              IF ( N1 .EQ. 38 ) GOTO 470
              BND_MAT(IND1(39),IN2)=BND_MAT(IND1(39),IN2) + WEI*EQ1(39)*EQ2(J7)
              IF ( N1 .EQ. 39 ) GOTO 470
              BND_MAT(IND1(40),IN2)=BND_MAT(IND1(40),IN2) + WEI*EQ1(40)*EQ2(J7)
              IF ( N1 .EQ. 40 ) GOTO 470
              BND_MAT(IND1(41),IN2)=BND_MAT(IND1(41),IN2) + WEI*EQ1(41)*EQ2(J7)
              IF ( N1 .EQ. 41 ) GOTO 470
              BND_MAT(IND1(42),IN2)=BND_MAT(IND1(42),IN2) + WEI*EQ1(42)*EQ2(J7)
              IF ( N1 .EQ. 42 ) GOTO 470
              BND_MAT(IND1(43),IN2)=BND_MAT(IND1(43),IN2) + WEI*EQ1(43)*EQ2(J7)
              IF ( N1 .EQ. 43 ) GOTO 470
              BND_MAT(IND1(44),IN2)=BND_MAT(IND1(44),IN2) + WEI*EQ1(44)*EQ2(J7)
              IF ( N1 .EQ. 44 ) GOTO 470
              BND_MAT(IND1(45),IN2)=BND_MAT(IND1(45),IN2) + WEI*EQ1(45)*EQ2(J7)
              IF ( N1 .EQ. 45 ) GOTO 470
              BND_MAT(IND1(46),IN2)=BND_MAT(IND1(46),IN2) + WEI*EQ1(46)*EQ2(J7)
              IF ( N1 .EQ. 46 ) GOTO 470
              BND_MAT(IND1(47),IN2)=BND_MAT(IND1(47),IN2) + WEI*EQ1(47)*EQ2(J7)
              IF ( N1 .EQ. 47 ) GOTO 470
              BND_MAT(IND1(48),IN2)=BND_MAT(IND1(48),IN2) + WEI*EQ1(48)*EQ2(J7)
              IF ( N1 .EQ. 48 ) GOTO 470
              BND_MAT(IND1(49),IN2)=BND_MAT(IND1(49),IN2) + WEI*EQ1(49)*EQ2(J7)
              IF ( N1 .EQ. 49 ) GOTO 470
              BND_MAT(IND1(50),IN2)=BND_MAT(IND1(50),IN2) + WEI*EQ1(50)*EQ2(J7)
              IF ( N1 .EQ. 50 ) GOTO 470
              BND_MAT(IND1(51),IN2)=BND_MAT(IND1(51),IN2) + WEI*EQ1(51)*EQ2(J7)
              IF ( N1 .EQ. 51 ) GOTO 470
              BND_MAT(IND1(52),IN2)=BND_MAT(IND1(52),IN2) + WEI*EQ1(52)*EQ2(J7)
              IF ( N1 .EQ. 52 ) GOTO 470
              BND_MAT(IND1(53),IN2)=BND_MAT(IND1(53),IN2) + WEI*EQ1(53)*EQ2(J7)
              IF ( N1 .EQ. 53 ) GOTO 470
              BND_MAT(IND1(54),IN2)=BND_MAT(IND1(54),IN2) + WEI*EQ1(54)*EQ2(J7)
              IF ( N1 .EQ. 54 ) GOTO 470
              BND_MAT(IND1(55),IN2)=BND_MAT(IND1(55),IN2) + WEI*EQ1(55)*EQ2(J7)
              IF ( N1 .EQ. 55 ) GOTO 470
              BND_MAT(IND1(56),IN2)=BND_MAT(IND1(56),IN2) + WEI*EQ1(56)*EQ2(J7)
              IF ( N1 .EQ. 56 ) GOTO 470
              BND_MAT(IND1(57),IN2)=BND_MAT(IND1(57),IN2) + WEI*EQ1(57)*EQ2(J7)
              IF ( N1 .EQ. 57 ) GOTO 470
              BND_MAT(IND1(58),IN2)=BND_MAT(IND1(58),IN2) + WEI*EQ1(58)*EQ2(J7)
              IF ( N1 .EQ. 58 ) GOTO 470
              BND_MAT(IND1(59),IN2)=BND_MAT(IND1(59),IN2) + WEI*EQ1(59)*EQ2(J7)
              IF ( N1 .EQ. 59 ) GOTO 470
              BND_MAT(IND1(60),IN2)=BND_MAT(IND1(60),IN2) + WEI*EQ1(60)*EQ2(J7)
              IF ( N1 .EQ. 60 ) GOTO 470
              BND_MAT(IND1(61),IN2)=BND_MAT(IND1(61),IN2) + WEI*EQ1(61)*EQ2(J7)
              IF ( N1 .EQ. 61 ) GOTO 470
              BND_MAT(IND1(62),IN2)=BND_MAT(IND1(62),IN2) + WEI*EQ1(62)*EQ2(J7)
              IF ( N1 .EQ. 62 ) GOTO 470
              BND_MAT(IND1(63),IN2)=BND_MAT(IND1(63),IN2) + WEI*EQ1(63)*EQ2(J7)
              IF ( N1 .EQ. 63 ) GOTO 470
              BND_MAT(IND1(64),IN2)=BND_MAT(IND1(64),IN2) + WEI*EQ1(64)*EQ2(J7)
  470      CONTINUE
        ELSE IF ( N1 .GT. DB__ADD_RCT ) THEN
           DO 480 J8=1,N1
              IN1 = IND1(J8)
              DO 490 J9=1,N2
                 IN2 = IND2(J9)
                 BND_MAT(IN1,IN2)=BND_MAT(IN1,IN2) + WEI*EQ1(J8)*EQ2(J9)
  490         CONTINUE
  480      CONTINUE
      END IF
!
      RETURN
      END  !#!  ADD_RCT  #!#
