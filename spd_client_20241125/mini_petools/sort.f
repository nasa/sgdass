        SUBROUTINE SORT_I ( L, LIS )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_I  sorts the integer array LIS of length L in      *
! *   the increasing order.                                              *
! *   NB: Slow algorithm O(N**2) is used. This routine should be         *
! *   replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large  *
! *   arrays.                                                            *
! *                                                                      *
! *  ###  31-JUL-1989  SORT_I    v1.0 (c) L. Petrov  24-MAY-1990    ###  *
! *                                                                      *
! ************************************************************************
        INTEGER*4 LIS(L)
        IF ( L .LE. 1 ) RETURN 
        DO 410 J1=1,L-1
           LI=LIS(J1)
           IN=J1
           DO 420 J2=J1+1,L
              IF( LIS(J2).LT.LI ) THEN
                  LI=LIS(J2)
                  IN=J2
              END IF
  420      CONTINUE
           IF( IN.EQ.J1 ) GOTO 410
           LR=LIS(J1)
           LIS(J1)=LI
           LIS(IN)=LR
  410   CONTINUE
        RETURN
        END  !#!  SORT_I  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT_I8 ( L, LIS )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_I8 sorts the integer array LIS of length L in      *
! *   the increasing order.                                              *
! *   NB: Slow algorithm O(N**2) is used. This routine should be         *
! *   replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large  *
! *   arrays.                                                            *
! *                                                                      *
! *  ###  31-JUL-1989  SORT_I8   v1.0 (c) L. Petrov  31-OCT-2005    ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*8 L
        INTEGER*8 LIS(L)
        INTEGER*8 J1, J2, IN, LI, LR
        IF ( L .LE. 1 ) RETURN 
        DO 410 J1=1,L-1
           LI=LIS(J1)
           IN=J1
           DO 420 J2=J1+1,L
              IF( LIS(J2).LT.LI ) THEN
                  LI=LIS(J2)
                  IN=J2
              END IF
  420      CONTINUE
           IF( IN.EQ.J1 ) GOTO 410
           LR=LIS(J1)
           LIS(J1)=LI
           LIS(IN)=LR
  410   CONTINUE
        RETURN
        END  SUBROUTINE  SORT_I8  !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT_R4 ( L, LIS )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_R4 sorts the real*4  array LIS of length L in      *
! *   the increasing order.                                              *
! *   NB: Slow algorithm O(N**2) is used. This routine should be         *
! *   replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large  *
! *   arrays.                                                            *
! *                                                                      *
! *  ###  31-JUL-1989   SORT_R4   v1.0  (c) L. Petrov  01-DEC-2013  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4  L
        REAL*4     LIS(L)
        REAL*4     LI, LR
        INTEGER*4  J1, J2, IN
        IF ( L .LE. 1 ) RETURN 
        DO 410 J1=1,L-1
           LI=LIS(J1)
           IN=J1
           DO 420 J2=J1+1,L
              IF( LIS(J2).LT.LI ) THEN
                  LI=LIS(J2)
                  IN=J2
              END IF
  420      CONTINUE
           IF( IN.EQ.J1 ) GOTO 410
           LR=LIS(J1)
           LIS(J1)=LI
           LIS(IN)=LR
  410   CONTINUE
        RETURN
        END  SUBROUTINE  SORT_R4  !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT_IA ( L, LIS )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_I  sorts the integer array LIS of length L in      *
! *   the order of increasing modules of its values.
! *   NB: Slow algorithm O(N**2) is used. This routine should be         *
! *   replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large  *
! *   arrays.                                                            *
! *                                                                      *
! *  ###  28-SEP-98    SORT_IA     v1.0  (c)  L. Petrov  28-SEP-98  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  L, LIS(L)
        INTEGER*4  J1, J2, LI, LR, IN
        IF ( L .LE. 1 ) RETURN 
        DO 410 J1=1,L-1
           LI=LIS(J1)
           IN=J1
           DO 420 J2=J1+1,L
              IF( IABS(LIS(J2)) .LT. IABS(LI) ) THEN
                  LI=LIS(J2)
                  IN=J2
              END IF
  420      CONTINUE
           IF( IN.EQ.J1 ) GOTO 410
           LR=LIS(J1)
           LIS(J1)=LI
           LIS(IN)=LR
  410   CONTINUE
        RETURN
        END  !#!  SORT_IA  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT_I2 ( L, LIS, MIS )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_I2  sorts both integer arrays  LIS  and MIS        *
! *   of length L in the increasing order of the elements of the array   *
! *   LIS. One can consider the lements LIS(I), MIS(I)  as connected     *
! *   pairs. Thus, these connected paris are sorted.                     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       L ( INTEGER*4 ) --  Number of elements in arrays LIS, MIS.     *
! *                                                                      *
! * ___________________ Modified parametersL: __________________________ *
! *                                                                      *
! *     LIS ( INTEGER*4 ) --  The first sorted array. It is sorted in    *
! *                           increasing its elements.                   *
! *     MIS ( INTEGER*4 ) --  The second sorted array. It is sorted in   *
! *                           the order of increasing elemts of the      *
! *                           array LIS.                                 *
! *                                                                      *
! *   NB: Slow algorithm O(N**2) is used. This routine should be         *
! *   replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large  *
! *   arrays.                                                            *
! *                                                                      *
! *  ###  31-JUL-1989    SORT_I2   v1.0 (c) L. Petrov  30-MAR-1992  ###  *
! *                                                                      *
! ************************************************************************
        INTEGER*4 LIS(L), MIS(L)
!!        CALL VER$ARG ( 3 )
!
        IF ( L .LE. 1 ) RETURN 
        DO 410 J1=1,L-1
           LI=LIS(J1)
           MI=MIS(J1)
           IN=J1
           DO 420 J2=J1+1,L
              IF( LIS(J2).LT.LI ) THEN
                  LI=LIS(J2)
                  MI=MIS(J2)
                  IN=J2
              END IF
  420      CONTINUE
           IF( IN.EQ.J1 ) GOTO 410
           LR=LIS(J1)
           MR=MIS(J1)
           LIS(J1)=LI
           MIS(J1)=MI
           LIS(IN)=LR
           MIS(IN)=MR
  410   CONTINUE
        RETURN
        END  !#!  SORT_I2  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT8 ( N, X, Y )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT8  sorts both real*8 arrays  X  and Y of length L   *
! *   in the increasing order of the elements of the array X using the   *
! *   the elements swap method. One can consider the elments X(I), Y(I)  *
! *   as connected pairs. Thus, these connected paris are sorted.        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       L ( INTEGER*4 ) --  Number of elements in arrays X, Y.         *
! *                                                                      *
! * ___________________ Modified parametersL: __________________________ *
! *                                                                      *
! *     X ( REAL*8    ) --  The first sorted array. It is sorted in      *
! *                         increasing its elements.                     *
! *     Y ( REAL*8    ) --  The second sorted array. It is sorted in the *
! *                         order of increasing elements of the array X. *
! *                                                                      *
! *   NB: Slow algorithm O(N**2) is used. This routine should be         *
! *   replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large  *
! *   arrays.                                                            *
! *                                                                      *
! *  ###  31-JUL-1989    SORT8    v 1.0  (c) L. Petrov 12-JUL-1995  ###  *
! *                                                                      *
! ************************************************************************
        INTEGER*4 N, J1, NL
        REAL*8    X(N), Y(N), XTEMP, YTEMP
!
        IF ( N .LE. 1 ) RETURN 
        DO 410 J1=1,N-1
!
! -------- Look for the mininla element of the array X among [J1,N-1]
! -------- elements of the array X. This element is denoted as XTEMP
!
           NL=J1
           XTEMP=X(J1)
           DO 420 J2=J1+1,N
              IF( X(J2).GE.XTEMP ) GOTO 420
              XTEMP=X(J2)
              NL=J2
  420      CONTINUE
           IF( NL.EQ.J1 ) GOTO 410
!
! -------- The element whcih has been found and the X(J1) are swapped
! -------- as well as YTEMP and Y(J1)
!
           YTEMP=Y(NL)
!
           X(NL)=X(J1)
           Y(NL)=Y(J1)
!
           X(J1)=XTEMP
           Y(J1)=YTEMP
!
  410   CONTINUE
        RETURN
        END  !#!  SORT8  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT83 ( N, X, Y, Z )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT83  sorts both real*8 arrays  X, Y and Z of length  *
! *   L in the increasing order of the elements of the array X using the *
! *   the elements swap method. One can consider the elments X(I), Y(I)  *
! *   Z(I) as connected triplets. Thus, these connected triplts are      *
! *   sorted.                                                            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     N ( INTEGER*4 ) --  Number of elements in arrays X, Y, Z.        *
! *                                                                      *
! * ___________________ Modified parametersL: __________________________ *
! *                                                                      *
! *     X ( REAL*8    ) --  The first sorted array. It is sorted in      *
! *                         increasing its elements.                     *
! *     Y ( REAL*8    ) --  The second sorted array. It is sorted in the *
! *                         order of increasing elements of the array X. *
! *     Z ( REAL*8    ) --  The third sorted array. It is sorted in the  *
! *                         order of increasing elements of the array X. *
! *                                                                      *
! *   NB: Slow algorithm O(N**2) is used. This routine should be         *
! *   replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large  *
! *   arrays.                                                            *
! *                                                                      *
! *  ###  31-JUL-89     SORT83    V1.0  (c) L. Petrov  03-NOV-1998  ###  *
! *                                                                      *
! ************************************************************************
        INTEGER*4 N, J1, NL
        REAL*8    X(N), Y(N), Z(N), XTEMP, YTEMP, ZTEMP
!
        IF ( N .LE. 1 ) RETURN 
        DO 410 J1=1,N-1
!
! -------- Look for the mininla element of the array X among [J1,N-1]
! -------- elements of the array X. This element is denoted as XTEMP
!
           NL=J1
           XTEMP=X(J1)
           DO 420 J2=J1+1,N
              IF( X(J2).GE.XTEMP ) GOTO 420
              XTEMP=X(J2)
              NL=J2
  420      CONTINUE
           IF( NL.EQ.J1 ) GOTO 410
!
! -------- The element which has been found and the X(J1) are swapped
! -------- as well as YTEMP and Y(J1); ZTEMP and Z(J1).
!
           YTEMP=Y(NL)
           ZTEMP=Z(NL)
!
           X(NL)=X(J1)
           Y(NL)=Y(J1)
           Z(NL)=Z(J1)
!
           X(J1)=XTEMP
           Y(J1)=YTEMP
           Z(J1)=ZTEMP
!
  410   CONTINUE
        RETURN
        END  !#!  SORT83  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT84 ( N, X, Y, Z, A )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT84  sorts four real*8 arrays  X, Y and Z of length  *
! *   L in the increasing order of the elements of the array X using the *
! *   the elements swap method. One can consider the elments X(I), Y(I)  *
! *   Z(I), A(I) as connected quadruplets. Thus, these connected         *
! *   quadruplets are sorted.                                            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     N ( INTEGER*4 ) --  Number of elements in arrays X, Y, Z.        *
! *                                                                      *
! * ___________________ Modified parametersL: __________________________ *
! *                                                                      *
! *     X ( REAL*8    ) --  The first sorted array. It is sorted in      *
! *                         increasing its elements.                     *
! *     Y ( REAL*8    ) --  The second sorted array. It is sorted in the *
! *                         order of increasing elements of the array X. *
! *     Z ( REAL*8    ) --  The third sorted array. It is sorted in the  *
! *                         order of increasing elements of the array X. *
! *     A ( REAL*8    ) --  The fourth sorted array. It is sorted in the *
! *                         order of increasing elements of the array X. *
! *                                                                      *
! *   NB: Slow algorithm O(N**2) is used. This routine should be         *
! *   replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large  *
! *   arrays.                                                            *
! *                                                                      *
! *  ###  31-JUL-89     SORT84    V1.0  (c) L. Petrov  25-AUG-2009  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4  N, J1, J2, NL
        REAL*8     X(N), Y(N), Z(N), A(N), XTEMP, YTEMP, ZTEMP, ATEMP
!
        IF ( N .LE. 1 ) RETURN 
        DO 410 J1=1,N-1
!
! -------- Look for the mininla element of the array X among [J1,N-1]
! -------- elements of the array X. This element is denoted as XTEMP
!
           NL=J1
           XTEMP=X(J1)
           DO 420 J2=J1+1,N
              IF( X(J2).GE.XTEMP ) GOTO 420
              XTEMP=X(J2)
              NL=J2
  420      CONTINUE
           IF( NL.EQ.J1 ) GOTO 410
!
! -------- The element which has been found and the X(J1) are swapped
! -------- as well as YTEMP and Y(J1); ZTEMP and Z(J1); ATEMP and A(J1)
!
           YTEMP=Y(NL)
           ZTEMP=Z(NL)
           ATEMP=A(NL)
!
           X(NL)=X(J1)
           Y(NL)=Y(J1)
           Z(NL)=Z(J1)
           A(NL)=A(J1)
!
           X(J1)=XTEMP
           Y(J1)=YTEMP
           Z(J1)=ZTEMP
           A(J1)=ATEMP
!
  410   CONTINUE
        RETURN
        END  SUBROUTINE  SORT84  !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT83I ( N, X, Y, Z, I )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT83  sorts both real*8 arrays  X, Y, Z and I of      *
! *   length L in the increasing order of the elements of the array X    *
! *   using the the elements swap method. One can consider the           *
! *   elments X(k), Y(k), Z(k), I(k) as connected four-element subarrys. *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     N ( INTEGER*4 ) --  Number of elements in arrays X, Y, Z, I.     *
! *                                                                      *
! * ____________________ Modified parameters: __________________________ *
! *                                                                      *
! *     X ( REAL*8    ) --  The first sorted array. It is sorted in      *
! *                         increasing its elements.                     *
! *     Y ( REAL*8    ) --  The second sorted array. It is sorted in the *
! *                         order of increasing elements of the array X. *
! *     Z ( REAL*8    ) --  The third sorted array. It is sorted in the  *
! *                         order of increasing elements of the array X. *
! *     I ( INTEGER*4 ) --  The fourth sorted array. It is sorted in the *
! *                         order of increasing elements of the array X. *
! *                                                                      *
! *   NB: Slow algorithm O(N**2) is used. This routine should be         *
! *   replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large  *
! *   arrays.                                                            *
! *                                                                      *
! *  ###  03-DEC-2004  SORT83I    V1.0  (c) L. Petrov  03-DEC-2004  ###  *
! *                                                                      *
! ************************************************************************
        INTEGER*4 N, J1, NL
        REAL*8    X(N), Y(N), Z(N), XTEMP, YTEMP, ZTEMP
        INTEGER*4 I(N), ITEMP
!
        IF ( N .LE. 1 ) RETURN 
!
        DO 410 J1=1,N-1
!
! -------- Look for the minimal element of the array X among [J1,N-1]
! -------- elements of the array X. This element is denoted as XTEMP
!
           NL = J1
           XTEMP = X(J1)
           DO 420 J2=J1+1,N
              IF ( X(J2) .GE. XTEMP ) GOTO 420
              XTEMP = X(J2)
              NL = J2
  420      CONTINUE
           IF ( NL .EQ. J1 ) GOTO 410
!
! -------- The element which has been found and the X(J1) are swapped
! -------- as well as YTEMP and Y(J1); ZTEMP and Z(J1).
!
           YTEMP = Y(NL)
           ZTEMP = Z(NL)
           ITEMP = I(NL)
!
           X(NL) = X(J1)
           Y(NL) = Y(J1)
           Z(NL) = Z(J1)
           I(NL) = I(J1)
!
           X(J1) = XTEMP
           Y(J1) = YTEMP
           Z(J1) = ZTEMP
           I(J1) = ITEMP
  410   CONTINUE
        RETURN
        END  SUBROUTINE  SORT83I
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT_R8 ( N, X )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_R8  sorts the integer array X of length L in       *
! *   increasing order.                                                  *
! *                                                                      *
! *   NB: Slow algorithm O(N**2) is used. This routine should be         *
! *   replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large  *
! *   arrays.                                                            *
! *                                                                      *
! *  ###  31-JUL-1989   SORT_R8   v 1.0  (c) L. Petrov 12-JUL-1995  ###  *
! *                                                                      *
! ************************************************************************
        INTEGER*4 N, J1, NL
        REAL*8    X(N), XTEMP, YTEMP
!
        IF ( N .LE. 1 ) RETURN 
        DO 410 J1=1,N-1
!
! -------- Look for the mininla element of the array X among [J1,N-1]
! -------- elements of the array X. This element is denoted as XTEMP
!
           NL=J1
           XTEMP=X(J1)
           DO 420 J2=J1+1,N
              IF( X(J2).GE.XTEMP ) GOTO 420
              XTEMP=X(J2)
              NL=J2
  420      CONTINUE
           IF( NL.EQ.J1 ) GOTO 410
!
! -------- The element whcih has been found and the X(J1) are swapped
!
           X(NL)=X(J1)
!
           X(J1)=XTEMP
  410   CONTINUE
        RETURN
        END  !#!  SORT_R8  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT_CH ( L, LIS_CH )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_CH  sorts the character array LIS_CH of length L   *
! *   in increasing order.                                               *
! *                                                                      *
! *   NB: Slow algorithm O(N**2) is used. This routine should be         *
! *   replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large  *
! *   arrays.                                                            *
! *                                                                      *
! *  ###  14-MAY-1998    SORT_CH  v1.0  (c)  L. Petrov  14-MAY-1998 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  L, LN, IN, J1, J2
        CHARACTER  LIS_CH(L)*(*), ELI_CH*16384, ELR_CH*16384
        IF ( L .LE. 1 ) RETURN 
        LN = LEN(LIS_CH(1))
!
        DO 410 J1=1,L-1
           ELI_CH(1:LN)=LIS_CH(J1)(1:LN)
           IN=J1
           DO 420 J2=J1+1,L
              IF ( LIS_CH(J2)(1:LN) .LT. ELI_CH(1:LN) ) THEN
                   ELI_CH(1:LN) = LIS_CH(J2)(1:LN)
                   IN=J2
              END IF
  420      CONTINUE
           IF ( IN.EQ.J1 ) GOTO 410
           ELR_CH(1:LN)     = LIS_CH(J1)(1:LN)
           LIS_CH(J1)(1:LN) = ELI_CH(1:LN)
           LIS_CH(IN)(1:LN) = ELR_CH(1:LN)
  410   CONTINUE
        RETURN
        END  !#!  SORT_CH  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT_FAST_I4 ( L, LIS )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_FAST_I4  sorts the integer array LIS of length L   *
! *   in the order of increasing their values using Hoar quick sort      *
! *   algorithm.                                                         *
! *                                                                      *
! *  ###  07-JAN-2006 SORT_FAST_I4  v1.0  (c) L. Petrov  07-JAN-2006 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  L, LIS(L)
#if defined (INTEL) || defined (SUN)
        INTEGER*4, EXTERNAL :: I4_COMPAR
#else
        INTEGER*2, EXTERNAL :: I4_COMPAR
#endif      
        IF ( L .LE. 1 ) RETURN 
#if defined (INTEL) || defined (ADR_64BIT)
        CALL FOR_QSORT ( LIS, INT8(L), 4, I4_COMPAR )
#else
        CALL FOR_QSORT ( LIS, L, 4, I4_COMPAR )
#endif
        RETURN
        END  SUBROUTINE  SORT_FAST_I4  !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT_FAST_I8 ( L, LIS )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_FAST_I8  sorts the integer array LIS of length L   *
! *   in the order of increasing their values using Hoar quick sort      *
! *   algorithm.                                                         *
! *                                                                      *
! *  ### 07-JAN-2006  SORT_FAST_I8  v1.0  (c) L. Petrov  07-JAN-2006 ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*8  L
        INTEGER*8  LIS(L)
        INTEGER*4, EXTERNAL :: I8_COMPAR
        IF ( L .LE. 1 ) RETURN 
#ifdef GNU
#ifdef ADR_32BIT
        CALL QSORT ( LIS, %VAL(L), %VAL(8), I8_COMPAR )
#else
        CALL QSORT ( LIS, %VAL(L), %VAL(8), I8_COMPAR )
#endif
#else
#ifdef ADR_32BIT
        CALL QSORT ( LIS, INT4(L), 8, I8_COMPAR )
#else
        CALL QSORT ( LIS, L,       8, I8_COMPAR )
#endif
#endif
        RETURN
        END  SUBROUTINE  SORT_FAST_I8  !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT_FAST_R4 ( L4, LIS )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_FAST_R4  sorts the integer array LIS of length L   *
! *   in the order of increasing their values using Hoar quick sort      *
! *   algorithm.                                                         *
! *                                                                      *
! *  ### 07-JAN-2006  SORT_FAST_R4  v1.0  (c) L. Petrov  07-JAN-2006 ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  L4
        REAL*4     LIS(L4)
        INTEGER*4, EXTERNAL :: R4_COMPAR
        IF ( L4 .LE. 1 ) RETURN 
#ifdef GNU
        CALL QSORT ( LIS, %VAL(L4), %VAL(4), R4_COMPAR )
#else
#ifdef ADR_32BIT
        CALL QSORT ( LIS,      L4, 4, R4_COMPAR )
#else
        CALL QSORT ( LIS, INT8(L4), 4, R4_COMPAR )
#endif
#endif
        RETURN
        END  SUBROUTINE  SORT_FAST_R4  !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT_FAST_R8 ( L8, LIS )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_FAST_R8  sorts the integer array LIS of length L   *
! *   in the order of increasing their values using Hoar quick sort      *
! *   algorithm.                                                         *
! *                                                                      *
! *  ### 07-JAN-2006  SORT_FAST_R8  v1.0  (c) L. Petrov  07-JAN-2006 ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*8  L8
        REAL*8     LIS(L8)
        INTEGER*4, EXTERNAL :: R8_COMPAR
        IF ( L8 .LE. 1 ) RETURN 
#ifdef GNU
        CALL QSORT ( LIS, %VAL(L8), %VAL(8), R8_COMPAR )
#else
#ifdef ADR_32BIT
        CALL QSORT ( LIS, INT4(L8), 8, R8_COMPAR )
#else
        CALL QSORT ( LIS, L8, 8, R8_COMPAR )
#endif
#endif
        RETURN
        END  SUBROUTINE  SORT_FAST_R8  !#!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE SORT_FAST_CH ( L, LIS_CH )
! ************************************************************************
! *                                                                      *
! *     Routine  SORT_FAST_CH  sorts the character array LIS of length L *
! *   in the order of increasing their values using Hoar quick sort      *
! *   algorithm.                                                         *
! *                                                                      *
! *  ### 07-FEB-2008  SORT_FAST_CH  v1.0  (c) L. Petrov  07-FEB-2008 ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  L
        CHARACTER  LIS_CH(L)*(*)
        INTEGER*4  LEN_CH_COMPAR
        COMMON  / CH_COMPAR_BLOCK / LEN_CH_COMPAR
        INTEGER*4  LOC__SUN$$_STR
#ifdef GNU
        INTEGER*4, EXTERNAL :: CH_COMPAR
#else
        INTEGER*2, EXTERNAL :: CH_COMPAR
#endif
        IF ( L .LE. 1 ) RETURN 
        LEN_CH_COMPAR = LEN(LIS_CH(1))
#ifdef SUN
        CALL QSORT ( %VAL(LOC__SUN$$_STR(LIS_CH)), L, LEN(LIS_CH), CH_COMPAR )
#else
#if defined (INTEL) || defined (ADR_64BIT)
        CALL FOR_QSORT ( %REF(LIS_CH), INT8(L), INT8(LEN(LIS_CH(1))), CH_COMPAR )
#else
        CALL FOR_QSORT ( %REF(LIS_CH),       L, LEN(LIS_CH(1)), CH_COMPAR )
#endif
#endif
        RETURN
        END  SUBROUTINE  SORT_FAST_CH  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION  R8_COMPAR ( VAL1_R8, VAL2_R8 )
      IMPLICIT   NONE 
      INTEGER*4 R8_COMPAR
      REAL*8     VAL1_R8, VAL2_R8
      IF ( VAL1_R8 > VAL2_R8 ) THEN
            R8_COMPAR = 1
         ELSE IF ( VAL1_R8 < VAL2_R8 ) THEN
            R8_COMPAR = -1
         ELSE 
            R8_COMPAR = 0
      END IF
      RETURN
      END  !#!  R8_COMPAR  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION  R4_COMPAR ( VAL1_R4, VAL2_R4 )
      IMPLICIT   NONE 
      INTEGER*4 R4_COMPAR
      REAL*4    VAL1_R4, VAL2_R4
      IF ( VAL1_R4 > VAL2_R4 ) THEN
            R4_COMPAR = 1
         ELSE IF ( VAL1_R4 < VAL2_R4 ) THEN
            R4_COMPAR = -1
         ELSE 
            R4_COMPAR = 0
      END IF
      RETURN
      END  !#!  R4_COMPAR  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION  I8_COMPAR ( VAL1_I8, VAL2_I8 )
      IMPLICIT   NONE 
      INTEGER*4 I8_COMPAR
      INTEGER*8 VAL1_I8, VAL2_I8
      IF ( VAL1_I8 > VAL2_I8 ) THEN
            I8_COMPAR = 1
         ELSE IF ( VAL1_I8 < VAL2_I8 ) THEN
            I8_COMPAR = -1
         ELSE 
            I8_COMPAR = 0
      END IF
      RETURN
      END  !#!  I8_COMPAR  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION  I4_COMPAR ( VAL1_I4, VAL2_I4 )
      IMPLICIT   NONE 
      INTEGER*4 I4_COMPAR
      INTEGER*4 VAL1_I4, VAL2_I4
      IF ( VAL1_I4 > VAL2_I4 ) THEN
            I4_COMPAR = 1
         ELSE IF ( VAL1_I4 < VAL2_I4 ) THEN
            I4_COMPAR = -1
         ELSE 
            I4_COMPAR = 0
      END IF
      RETURN
      END  !#!  I4_COMPAR  #!#
!
! ------------------------------------------------------------------------
!
#ifdef INTEL
      FUNCTION   CH_COMPAR ( STR1, STR2 )
      IMPLICIT   NONE 
      INTEGER*2  CH_COMPAR
      INTEGER*2, EXTERNAL :: CH_CH_COMPAR
      COMMON  / CH_COMPAR_BLOCK / LEN_CH_COMPAR
      INTEGER*4  LEN_CH_COMPAR
      CHARACTER  STR1*(LEN_CH_COMPAR), STR2*(LEN_CH_COMPAR)
      IF ( STR1 > STR2 ) THEN
           CH_COMPAR = 1
         ELSE IF ( STR1 < STR2 ) THEN
           CH_COMPAR = -1
         ELSE 
           CH_COMPAR = 0
      END IF
      RETURN
      END  !#!  CH_COMPAR  #!#
#else
      FUNCTION  CH_COMPAR ( ISTR1, ISTR2 )
      IMPLICIT   NONE 
      INTEGER*4  CH_COMPAR
      INTEGER*4, EXTERNAL :: CH_CH_COMPAR
      COMMON  / CH_COMPAR_BLOCK / LEN_CH_COMPAR
      INTEGER*4  LEN_CH_COMPAR
      INTEGER*1  ISTR1, ISTR2
      CH_COMPAR = CH_CH_COMPAR ( ISTR1, ISTR2, %VAL(LEN_CH_COMPAR), %VAL(LEN_CH_COMPAR) )
      RETURN
      END  !#!  CH_COMPAR  #!#
#endif
!
! ------------------------------------------------------------------------
!
      FUNCTION  CH_CH_COMPAR ( STR1, STR2 )
      IMPLICIT   NONE 
#ifdef GNU
      INTEGER*4 CH_CH_COMPAR
#else
      INTEGER*2 CH_CH_COMPAR
#endif
      CHARACTER STR1*(*), STR2*(*)
      IF ( STR1 > STR2 ) THEN
            CH_CH_COMPAR = 1
         ELSE IF ( STR1 < STR2 ) THEN
            CH_CH_COMPAR = -1
         ELSE 
            CH_CH_COMPAR = 0
      END IF
      RETURN
      END  !#!  CH_CH_COMPAR  #!#
