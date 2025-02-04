        FUNCTION IXMN4 ( N, X, X0 )
! ************************************************************************
! *                                                                      *
! *     Finction  IXMN4  finds the index of the element X in the array   *
! *   X of N elements, which is minimal among those elements which       *
! *   exceed X0. Binary search method is used.                           *
! *   If X(I) > X for every I then IXMN4_S = -1                          *
! *   If X(I) < X for every I then IXMN4_S = -2                          *
! *   If X0 = X(1) then IXMN4 = 1                                        *
! *   If X0 = X(N) then IXMN4 = N                                        *
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
! *     Value of IXMN4_S ( 1, N, X, CK ):                                *
! *                                                                      *
! *         for C1  -1                                                   *
! *         for C2   1                                                   *
! *         for C3   3                                                   *
! *         for C4   N-1                                                 *
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
! * <IXMN4> ( INTEGER*4 ) -- Index of the pivot element.                 *
! *                                                                      *
! * ###  20-OCT-1989     IXMN4    v1.2 (c) L. Petrov  19-JUL-2007   ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  N, IXMN4
        REAL*4     X(N), X0
        INTEGER    IL, IR, IM
!
! ----- Check of the number of actual arguments
!
!!        CALL VER$ARG ( 3 )
!
        IF ( X0.LT.X(1) ) THEN
             IXMN4 = -1
             RETURN
        END IF
!
        IF ( X0.GT.X(N) ) THEN
             IXMN4 = -2
             RETURN
        END IF
!
        IF ( X0.EQ.X(N) ) THEN
             IXMN4=N
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
        IXMN4 = IM
        RETURN
        END  !#!  IXMN4  #!#
!
! ------------------------------------------------------------------------
!
        FUNCTION IXMN4_S ( IX_LAST, N, X, X0 )
! ************************************************************************
! *                                                                      *
! *     Finction  IXMN4_S  find the index of the element X in the array  *
! *   X of N elements, which is minimal among those elements which       *
! *   exceed X0. Sequential search method is used. The serach starts     *
! *   from the element IX_LAST.                                          *
! *   If X(I) > X for every I then IXMN4_S = -1                          *
! *   If X(I) < X for every I then IXMN4_S = -2                          *
! *   If X0 = X(IX_LAST) then IXMN4_S = N                                *
! *   If X0 = X(N) then IXMN4_S = N                                      *
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
! *     Value of IXMN4_S ( 1, N, X, CK ):                                *
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
! *   <IXMN4_S> ( INTEGER*4 ) -- Index of the pivot element.             *
! *                                                                      *
! *  ###  20-OCT-1989    IXMN4_S    v1.0 (c) L. Petrov 04-JUL-1995  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 IXMN4_S, IX_LAST, N
        REAL*4    X(N), X0
        INTEGER   J1, IXB, IXMN4
!
! ----- Check of the number of actual arguments
!
!!        CALL VER$ARG ( 4 )
!
        IXB=IX_LAST-1
        IF ( IXB.LE.0 ) IXB=1
!
        IF ( IXB.GE.N ) THEN
             IXMN4_S = -1
             IF ( IXB.EQ.N .AND. X(N).EQ.X0 ) IXMN4_S=N
             RETURN
        END IF
!
        IF ( X(IXB).GE.X0 ) THEN
             IF ( IXB.EQ.1 ) IXMN4_S=-2
             IF ( IXB.EQ.1 .AND. X(1).EQ.X0 ) IXMN4_S=1
             IF ( IXB.NE.1 ) IXMN4_S=IXMN4 ( N, X, X0 )
             RETURN
        END IF
!
        DO 410 J1=IXB+1,N
           IF ( X(J1).GT.X0 ) GOTO 810
  410   CONTINUE
        IXMN4_S=-1
        IF ( X(N).EQ.X0 ) IXMN4_S=N
        RETURN
!
  810   CONTINUE
        IXMN4_S=J1-1
        RETURN
        END  !#!  IXMN4_S  #!#
