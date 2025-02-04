        FUNCTION IXMN8 ( N, X, X0 )
! ************************************************************************
! *                                                                      *
! *     Finction  IXMN8  finds the index of the element X in the array   *
! *   X of N elements, which is minimal among those elements which       *
! *   exceed X0. Binary search method is used.                           *
! *   If X(I) > X for every I then IXMN8_S = -1                          *
! *   If X(I) < X for every I then IXMN8_S = -2                          *
! *   If X0 = X(1) then IXMN8 = 1                                        *
! *   If X0 = X(N) then IXMN8 = N                                        *
! *                                                                      *
! *   NB: array X should be ordered in the order of increasing its       *
! *   elements.                                                          *
! *                        ______  Example: ______                       *
! *                                                                      *
! *       X(1)  X(2)  X(3)  X(4)   ...        X(N-1)  X(N)               *
! *        !-----!-----!-----!------!---- ...  --!-----!                 *
! *      !    !           !                        !   !        !        *
! *     C1   C2          C3                       C4   C5      C6        *
! *                                                                      *
! *     Value of IXMN8_S ( 1, N, X, CK ):                                *
! *                                                                      *
! *         for C1  -1                                                   *
! *         for C2   2                                                   *
! *         for C3   4                                                   *
! *         for C4   N                                                   *
! *         for C5   N                                                   *
! *         for C6  -2                                                   *
! *                                                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       N ( INTEGER*4 ) -- The number of elements in array X.          *
! *       X ( REAL*8    ) -- Array ordered in increasing the elements.   *
! *                          dimension: N.                               *
! *      X0 ( REAL*8    ) -- The point under investigation.              *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <IXMN8> ( INTEGER*4 ) -- Index of the pivot element.                 *
! *                                                                      *
! * ###  20-OCT-1989     IXMN8    v1.2 (c) L. Petrov  19-JUL-2007   ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  N, IXMN8
        REAL*8     X(N), X0
        INTEGER    IL, IR, IM
!
! ----- Check of the number of actual arguments
!
!!        CALL VER$ARG ( 3 )
!
        IF ( X0.LT.X(1) ) THEN
             IXMN8 = -1
             RETURN
        END IF
!
        IF ( X0.GT.X(N) ) THEN
             IXMN8 = -2
             RETURN
        END IF
!
        IF ( X0.EQ.X(N) ) THEN
             IXMN8=N
             RETURN
        END IF
!
        IL=1
        IR=N
!
! ----- Binary search
!
  910   CONTINUE
           IF ( IL > IR ) THEN
                IM = IR
                GOTO 810
           END IF
           IM = IL + (IR - IL)/2
           IF ( X0 < X(IM)  ) THEN 
                IR = IM - 1
                GOTO 910
              ELSE IF ( X0 >  X(IM) ) THEN 
                IL = IM + 1
                GOTO 910
              ELSE IF ( X0 == X(IM) ) THEN
                GOTO 810
           END IF
   810  CONTINUE
!
        IXMN8 = IM
        RETURN
        END  !#!  IXMN8  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION IXMN8_S ( IX_LAST, N, X, X0 )
! ************************************************************************
! *                                                                      *
! *     Finction  IXMN8_S  find the index of the element X in the array  *
! *   X of N elements, which is minimal among those elements which       *
! *   exceed X0. Sequential search method is used. The serach starts     *
! *   from the element IX_LAST.                                          *
! *   If X(I) > X for every I then IXMN8_S = -1                          *
! *   If X(I) < X for every I then IXMN8_S = -2                          *
! *   If X0 = X(IX_LAST) then IXMN8_S = N                                *
! *   If X0 = X(N) then IXMN8_S = N                                      *
! *                                                                      *
! *   NB: array X should be ordered in the order of increasing its       *
! *   elements.                                                          *
! *                        ______  Example: ______                       *
! *                                                                      *
! *       X(1)  X(2)  X(3)  X(4)   ...        X(N-1)  X(N)               *
! *        !-----!-----!-----!------!---- ...  --!-----!                 *
! *      !    !           !                        !   !        !        *
! *     C1   C2          C3                       C4   C5      C6        *
! *                                                                      *
! *     Value of IXMN8_S ( 1, N, X, CK ):                                *
! *                                                                      *
! *         for C1  -1                                                   *
! *         for C2   2                                                   *
! *         for C3   4                                                   *
! *         for C4   N                                                   *
! *         for C5   N                                                   *
! *         for C6  -2                                                   *
! *                                                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * IX_LAST ( INTEGER*4 ) -- The element from whcih the search starts.   *
! *       N ( INTEGER*4 ) -- The number of elements in array X.          *
! *       X ( REAL*8    ) -- Array ordered in increasing the elements.   *
! *                          dimension: N.                               *
! *      X0 ( REAL*8    ) -- The poinst under investigation.             *
! *                                                                      *
! * _______________________ קשטןהמשו נבעבםופעש: ________________________ *
! *                                                                      *
! *   <IXMN8_S> ( INTEGER*4 ) -- Index of the pivot element.             *
! *                                                                      *
! *  ###  20-OCT-1989    IXMN8_S    v1.0 (c) L. Petrov 04-JUL-1995  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 IXMN8_S, IX_LAST, N
        REAL*8    X(N), X0
        INTEGER   J1, IXB, IXMN8
!
! ----- Check of the number of actual arguments
!
!!        CALL VER$ARG ( 4 )
!
        IXB=IX_LAST-1
        IF ( IXB.LE.0 ) IXB=1
!
        IF ( IXB.GE.N ) THEN
             IXMN8_S = -1
             IF ( IXB.EQ.N .AND. X(N).EQ.X0 ) IXMN8_S=N
             RETURN
        END IF
!
        IF ( X(IXB).GE.X0 ) THEN
             IF ( IXB.EQ.1 ) IXMN8_S=-2
             IF ( IXB.EQ.1 .AND. X(1).EQ.X0 ) IXMN8_S=1
             IF ( IXB.NE.1 ) IXMN8_S=IXMN8 ( N, X, X0 )
             RETURN
        END IF
!
        DO 410 J1=IXB+1,N
           IF ( X(J1).GT.X0 ) GOTO 810
  410   CONTINUE
        IXMN8_S=-1
        IF ( X(N).EQ.X0 ) IXMN8_S=N
        RETURN
!
  810   CONTINUE
        IXMN8_S=J1-1
        RETURN
        END  !#!  IXMN8_S  #!#
