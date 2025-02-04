      SUBROUTINE COPY_V ( N, VEC1, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  COPY_V  copies N elements of the vector VEC1 to the    *
! *   vector  VEC2                                                       *
! *                                                                      *
! *  ###  12-Dec-96     COPY_V     v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*8     VEC1(N), VEC2(N)
      INTEGER*4  J1
      IF ( N .GT. 0 ) THEN
#ifdef HPUX
           CALL VEC_$DCOPY ( VEC1, VEC2, N )
#else
!!           DO 410 J1=1,N
!!              VEC2(J1) = VEC1(J1)
!! 410       CONTINUE 
           CALL MEMCPY ( VEC2, VEC1, %VAL(8*N) )
#endif
      END IF
      RETURN
      END  !#!  COPY_V  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE COPY_R8 ( N, VEC1, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  COPY_R8  copies N elements of the vector VEC1 to the   *
! *   vector  VEC2. Type of vector: REAL*8.                              *
! *                                                                      *
! *  ###  12-Dec-96    COPY_R8     v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*8     VEC1(N), VEC2(N)
      INTEGER*4  J1
      IF ( N .GT. 0 ) THEN
#ifdef HPUX
           CALL VEC_$DCOPY ( VEC1, VEC2, N )
#else
!!           DO 410 J1=1,N
!!              VEC2(J1) = VEC1(J1)
!! 410       CONTINUE 
           CALL MEMCPY ( VEC2, VEC1, %VAL(8*N) )
#endif
      END IF
      RETURN
      END  !#!  COPY_R8  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE COPY_R4 ( N, VEC1, VEC2 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  COPY_R4  copies N elements of the vector VEC1 to the   *
! *   vector  VEC2. Type of vector: REAL*4.                              *
! *                                                                      *
! *  ###  12-Dec-96    COPY_R4     v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*4     VEC1(N), VEC2(N)
      INTEGER*4  J1
      IF ( N .GT. 0 ) THEN
!!           DO 410 J1=1,N
!!              VEC2(J1) = VEC1(J1)
!! 410       CONTINUE 
           CALL MEMCPY ( VEC2, VEC1, %VAL(4*N) )
      END IF
      RETURN
      END  !#!  COPY_R4  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE COPY_I4 ( N, IVEC1, IVEC2 )
! ************************************************************************
! *                                                                      *
! *   Subroutine  COPY_I4  copies N elements of the INTEGER*4 vector     *
! *   IVEC1 to the vector  IVEC2.                                        *
! *                                                                      *
! *  ###  02-AUG-97     COPY_I4    v1.0  (c)  L. Petrov  02-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      INTEGER*4  IVEC1(N), IVEC2(N)
      INTEGER*4  J1
      IF ( N .GT. 0 ) THEN
#ifdef HPUX
           CALL VEC_$ICOPY ( IVEC1, IVEC2, N )
#else
!!           DO 410 J1=1,N
!!              IVEC2(J1) = IVEC1(J1)
!! 410       CONTINUE 
           CALL MEMCPY ( IVEC2, IVEC1, %VAL(4*N) )
#endif
      END IF
      RETURN
      END  !#!  COPY_I4  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NOUT_I8 ( N, VEC )
! ************************************************************************
! *                                                                      *
! *   Subroutine  NOUT_I8  fill N elements of the vector  VEC by zeroes. *
! *                                                                      *
! *  ###  10-OCT-2017   NOUT_I8    v1.0  (c)  L. Petrov 10-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*8  N, VEC(N)
      CALL BZERO ( VEC, %VAL(8*N) )
      RETURN
      END  !#!  NOUT_I8  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NOUT_I4 ( N, VEC )
! ************************************************************************
! *                                                                      *
! *   Subroutine  NOUT_I4  fill N elements of the vector  VEC by zeroes. *
! *                                                                      *
! *  ###  12-Dec-96     NOUT_I4    v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N, VEC(N)
      INTEGER*4  J1
      IF ( N .GT. 0 ) THEN
#ifdef HPUX
           CALL VEC_$IINIT ( VEC, N, 0 )
#else
!!           DO 410 J1=1,N
!!              VEC(J1) = 0
!! 410       CONTINUE 
           CALL BZERO ( VEC, %VAL(4*N) )
#endif
      END IF
      RETURN
      END  !#!  NOUT_I4  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NOUT_I2 ( N, VEC )
! ************************************************************************
! *                                                                      *
! *   Subroutine  NOUT_I2  fill N elements of the vector  VEC by zeroes. *
! *                                                                      *
! *  ###  12-Dec-96     NOUT_I2    v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      INTEGER*2  VEC(N)
      INTEGER*4  J1
      IF ( N .GT. 0 ) THEN
#ifdef HPUX
           CALL VEC_$IINIT16 ( VEC, N, INT2(0) )
#else
!!           DO 410 J1=1,N
!!              VEC(J1) = 0
!! 410       CONTINUE 
           CALL BZERO ( VEC, %VAL(2*N) )
#endif
      END IF
      RETURN
      END  !#!  NOUT_I4  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NOUT_R8 ( N, VEC )
! ************************************************************************
! *                                                                      *
! *   Subroutine  NOUT_R8  fill N elements of the vector  VEC by zeroes. *
! *                                                                      *
! *  ###  12-Dec-96     NOUT_R8    v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*8     VEC(N)
      INTEGER*4  J1
      IF ( N .GT. 0 ) THEN
#ifdef HPUX
           CALL VEC_$DINIT ( VEC, N, 0.0D0 )
#else
!!           DO 410 J1=1,N
!!              VEC(J1) = 0.0D0
!! 410       CONTINUE 
           CALL BZERO ( VEC, %VAL(8*N) )
#endif
      END IF
      RETURN
      END  !#!  NOUT_R8  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NOUT_R4 ( N, VEC )
! ************************************************************************
! *                                                                      *
! *   Subroutine  NOUT_R4  fill N elements of the vector  VEC by zeroes. *
! *                                                                      *
! *  ###  12-Dec-96     NOUT_R4    v1.0  (c)  L. Petrov  12-Dec-96  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*4     VEC(N)
      INTEGER*4  J1
      IF ( N .GT. 0 ) THEN
!!           DO 410 J1=1,N
!!              VEC(J1) = 0.0D0
!! 410       CONTINUE 
           CALL BZERO ( VEC, %VAL(4*N) )
      END IF
      RETURN
      END  !#!  NOUT_R4  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TM_II ( N, MAT )
! ************************************************************************
! *                                                                      *
! *   Routine  TM_II  transform QUADRATIC matrix MAT of dimension L*L to *
! *   the matrix to be transponse with respect to the initial one.       *
! *                                                                      *
! *  ###  02-JAN-97      TM_II     V1.0 (c)   L. Petrov  02-JAN-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, J1, J2
      REAL*8     MAT(N,N), SWAP
!
      DO 410 J1=1,N-1
         DO 420 J2=J1+1,N
            SWAP       = MAT(J1,J2)
            MAT(J1,J2) = MAT(J2,J1)
            MAT(J2,J1) = SWAP
 420     CONTINUE
 410  CONTINUE
      RETURN
      END  !#!  TM_II  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MAT_SI ( N, MATS, MATI )
! ************************************************************************
! *                                                                      *
! *   Routine  MAT_SI  transform matrix MATS of dimension L*L in         *
! *   upper triangular representation to matrix MATI in rectangular      *
! *   representation.                                                    *
! *                                                                      *
! *  ###  02-JAN-97     MAT_SI     V1.0 (c)   L. Petrov  02-JAN-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, LA, J1, J2
      REAL*8     MATS(*), MATI(N,N)
!
      LA=0
      DO 410 J1=1,N
         DO 420 J2=1,J1
            LA=LA+1
            MATI(J1,J2)=MATS(LA)
            MATI(J2,J1)=MATS(LA)
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END  !#!  MAT_SI  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MAT_IS ( N, MATI, MATS )
! ************************************************************************
! *                                                                      *
! *   Routine  MAT_IS  transform matrix MATI of dimension L*L in         *
! *   rectangular representation to matrix MATS in upper triangular      *
! *   representation, saving diagonal and up-diagonal terms.             *
! *                                                                      *
! *  ###  02-JAN-97     MAT_IS     V1.0 (c)   L. Petrov  02-JAN-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, LA, J1, J2
      REAL*8     MATI(N,N), MATS(*)
!
      LA=0
      DO 410 J1=1,N
         DO 420 J2=1,J1
            LA=LA+1
            MATS(LA)=MATI(J2,J1)
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END  !#!  MAT_IS  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADDMAT_IT ( M1, M2, A, B )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_IT  add to the matrix A the matrix to be transposed   *
! *   with respect to B:    A = A + B(T).                                *
! *                                                                      *
! * ________________________ INPUT PARAMETERS: _________________________ *
! *                                                                      *
! *   M ( INTEGER*4 ) -- the first dimension of the matrix A.            *
! *   N ( INTEGER*4 ) -- the second dimension of the matrix A.           *
! *   B ( INTEGER*4 ) -- the second matrix. Dimension: M2*M1 .           *
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! *   A ( REAL*8    ) -- The first matrix. Dimension: M1*M2 .            *
! *                                                                      *
! *  ###  21-Jan-97   ADDMAT_IT    v1.1  (c)  L. Petrov  23-Jan-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, M2, J1, J2
      REAL*8     A(M1,M2), B(M2,M1)
!
      DO 410 J1=1,M1
         DO 420 J2=1,M2
            A(J1,J2) = A(J1,J2) + B(J2,J1)
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END  !#!  ADDMAT_IT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADDMAT_IT_S ( M, A, B )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_IT_S  add to the quadratic matrix A the matrix to be  *
! *   transposed with respect to A and put the result in symmetric       *
! *   matrix B:   B = A + A(T).                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   M ( INTEGER*4 ) -- Dimension of the matrix A.                      *
! *   A ( INTEGER*4 ) -- Input quadratic matrix. Dimension: M*M          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   B ( REAL*8    ) -- Output symmetric matrix in the upper triangular *
! *                      representation.                                 *
! *                                                                      *
! *  ###  16-JUL-99  ADDMAT_IT_S   v1.0  (c)  L. Petrov  16-JUL-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M, J1, J2, LC
      REAL*8     A(M,M), B(*)
!
      LC = 0
      DO 410 J1=1,M
         DO 420 J2=1,J1
            LC=LC+1
            B(LC) = A(J1,J2) + A(J2,J1)
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END  !#!  ADDMAT_IT_S  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION REDIM_MAT ( MO1, KO1, KO2, MN1, KN1, KN2 )
! ************************************************************************
! *                                                                      *
! *   Routine  REDIM_MAT  calculates a pair of indeces for the certain   *
! *   element of the rectangular matrix with different keeping scheme.   *
! *   Input:  assumed dimension of the matrix (MO1,*) (KO1,KO2),         *
! *   Output: assumed dimension of the matrix (MN1,*) (KN1,KN2).         *
! *   Thus, REDIM_MAT  can be used when we change declarations of the    *
! *   dimension of the matrix during passing the matrix as an argument   *
! *   into the subroutine. It finds a pair of indeces in "new"           *
! *   declarations of the dimensions which corresponds to the same       *
! *   element on "old" declarations of the dimensions.                   *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *         MO1 ( INTEGER*4  ) -- Old first dimension of the matrix      *
! *                               (second dimension can be arbitrary).   *
! *         KO1 ( INTEGER*4  ) -- Old first  index of the element.       *
! *         KO2 ( INTEGER*4  ) -- Old second index of the element.       *
! *                                                                      *
! * ________________________ OUTPUT PARAMETERS: ________________________ *
! *                                                                      *
! * <REDIM_MAT> ( INTEGER*4  ) -- offset of the matrix element from the  *
! *                               beginning of the matrix (offset        *
! *                               counted from the 1 for the first       *
! *                               element).                              *
! *         MN1 ( INTEGER*4  ) -- New first dimension of the matrix      *
! *                               (second dimension can be arbitrary).   *
! *         KN1 ( INTEGER*4  ) -- New first  index of the element.       *
! *         KN2 ( INTEGER*4  ) -- New second index of the element.       *
! *                                                                      *
! *  ###  24-FEB-97    REDIM_MAT    v1.0 (c)  L. Petrov  25-FEB-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REDIM_MAT, MO1, KO1, KO2, MN1, KN1, KN2
!
      REDIM_MAT = KO1 + (KO2-1)*MO1
      KN2 = REDIM_MAT/MN1
      KN1 = REDIM_MAT - KN2*MN1
!
      IF ( KN1 .EQ. 0 ) THEN
           KN1 = MN1
         ELSE
           KN2 = KN2 + 1
      END IF
!
      RETURN
      END  !#!  REDIM_MAT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NORM_VEC ( N, VEC, RD )
! ************************************************************************
! *                                                                      *
! *   Routine  NORM_VEC  calculate the Euclid norm of the vecot VEC      *
! *   and normalize it.                                                  *
! *                                                                      *
! *  ###  18-AUG-98     NORM_VEC   v1.2  (c)  L. Petrov 29-OCT-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N
      REAL*8     EPS 
      PARAMETER  ( EPS = 2.0D0*TINY(1.0D0) )
      REAL*8     VEC(N), RD
      REAL*8,    EXTERNAL :: DP_VV_V
!
      RD = SQRT ( DP_VV_V ( N, VEC, VEC ) )
      IF ( DABS(RD) > EPS ) THEN
           CALL MUL_VC_V ( N, VEC, 1.D0/RD )
      END IF
!
      RETURN
      END  !#!  NORM_VEC  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VM83 ( VEC1, VEC2, RES )
! ************************************************************************
! *                                                                      *
! *   Routine VM83 computes vector prodcut of two three-demension        *
! *   vectors VEC1, VEC2 and puts results in RES.                        *
! *                                                                      *
! *  ### 20-JAN-1989      VM83     v1.0 (c)  L. Petrov  20-JAN-1989 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     VEC1(3), VEC2(3), RES(3)
!
      RES(1) = VEC1(2)*VEC2(3) - VEC1(3)*VEC2(2)
      RES(2) = VEC1(3)*VEC2(1) - VEC1(1)*VEC2(3)
      RES(3) = VEC1(1)*VEC2(2) - VEC1(2)*VEC2(1)
!
      RETURN
      END  !#!  VM83  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE R8ELEM_ADD ( VEC1, IND1, VEC2, IND2 )
! ************************************************************************
! *                                                                      *
! *   Auxillary  function adds the IND1-th element of the vector VEC1    *
! *   and the IND2-th element of the vector VEC2 and puts result in the  *
! *   IND2-th element of the vector VEC1.                                *
! *                                                                      *
! *   VEC1(IND1) = VEC1(IND1) + VEC2(IND2)                               *
! *                                                                      *
! *  ### 05-JAN-1999   R8ELEM_ADD  v1.0  (c)  L. Petrov  28-FEB-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     VEC1(*), VEC2(*)
      ADDRESS__TYPE :: IND1, IND2
      VEC1(IND1) = VEC1(IND1) + VEC2(IND2)
      RETURN
      END  !#!  R8ELEM_ADD  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE R8ELEM_SUB ( VEC1, IND1, VEC2, IND2 )
! ************************************************************************
! *                                                                      *
! *   Auxillary  function subtract the IND2-th element of the vector     *
! *   VEC2 from the IND1-th element of the vector VEC1 and puts result   *
! *   in the IND1-the element of the vector VEC2.                        *
! *                                                                      *
! *   VEC1(IND1) = VEC1(IND1) - VEC2(IND2)                               *
! *                                                                      *
! *  ### 05-JAN-1999   R8ELEM_SUB  v1.0  (c)  L. Petrov  28-FEB-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     VEC1(*), VEC2(*)
      ADDRESS__TYPE :: IND1, IND2
      VEC1(IND1) = VEC1(IND1) - VEC2(IND2)
      RETURN
      END  !#!  R8ELEM_SUB  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TM83 ( MAT1, MAT2 )
! ************************************************************************
! *                                                                      *
! *   Routinme  TM83  computes MAT2 as a matrix transponed with respect  *
! *   to MAT1. Both matrix are 3*3                                       *
! *                                                                      *
! *  ###  31-AUG-99      TM83      v1.0  (c)  L. Petrov  31-AUG-99  ###  *
! *                                                                      *
! ************************************************************************
      REAL*8     MAT1(3,3), MAT2(3,3)
!
      MAT2(1,1) = MAT1(1,1)
      MAT2(2,1) = MAT1(1,2)
      MAT2(3,1) = MAT1(1,3)
!
      MAT2(1,2) = MAT1(2,1)
      MAT2(2,2) = MAT1(2,2)
      MAT2(3,2) = MAT1(2,3)
!
      MAT2(1,3) = MAT1(3,1)
      MAT2(2,3) = MAT1(3,2)
      MAT2(3,3) = MAT1(3,3)
!
      RETURN
      END  !#!  TM83  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CP_VV_V3 ( VEC1, VEC2, VECO )
! ************************************************************************
! *                                                                      *
! *   Subroutine  CP_VV_V  calculates cross product VEC1 and VEC2 of     *
! *   dimension 3.  CP_VV_V = VEC1 x VEC2                                *
! *                                                                      *
! *  ###  12-Dec-96   CP_VV_V      v1.0  (c)  L. Petrov   12-Dec-96 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     VEC1(3), VEC2(3), VECO(3)
!
      VECO(1) = VEC1(2)*VEC2(3) - VEC1(3)*VEC2(2)
      VECO(2) = VEC1(3)*VEC2(1) - VEC2(1)*VEC1(3)
      VECO(3) = VEC1(1)*VEC2(2) - VEC2(2)*VEC1(1)
!
      RETURN
      END  !#!  CP_VV_V3  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE R8_UPDATE ( ADDRESS, CONTRIBUTE )
      IMPLICIT   NONE
      ADDRESS__TYPE :: ADDRESS
      REAL*8     CONTRIBUTE, VAL
      CALL LIB$MOVC3 ( 8, %VAL(ADDRESS), VAL )
      VAL = VAL + CONTRIBUTE
      CALL LIB$MOVC3 ( 8, VAL, %VAL(ADDRESS) )
      RETURN
      END  !#!  R8_UPDATE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VEC_MULT_CONSTANT ( U, COUNT, A, R )
! ************************************************************************
! *                                                                      *
! *   L1 vector routine executes the following operation under vectors   *
! *   U, R dimensioned COUNT and constant A:                             *
! *                                                                      *
! *   R = A * U                                                          *
! *                                                                      *
! * ### 10-JUL-2003 VEC_MULT_CONSTANT v1.0 (c) L. Petrov 10-JUL-2003 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  COUNT, J1
      REAL*8     U(COUNT), R(COUNT), A
!
! @ #ifdef HPUX
! @       CALL VEC_$DMULT_CONSTANT ( U, COUNT, A, R )
! @ #else
      IF ( COUNT .LE. 0 ) RETURN
      DO 410 J1=1,COUNT
         R(J1) = A*U(J1)
 410  CONTINUE
! @ #endif
!
      RETURN
      END  !#!  VEC_MULT_CONSTANT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VEC_MULT_CONSTANT8 ( U, COUNT8, A, R )
! ************************************************************************
! *                                                                      *
! *   L1 vector routine executes the following operation under vectors   *
! *   U, R dimensioned COUNT and constant A:                             *
! *                                                                      *
! *   R = A * U                                                          *
! *                                                                      *
! * ### 10-JUL-2003 VEC_MULT_CONSTANT v1.0 (c) L. Petrov 10-JUL-2003 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  COUNT8, J1
      REAL*8     U(COUNT8), R(COUNT8), A
!
! @ #ifdef HPUX
! @       CALL VEC_$DMULT_CONSTANT ( U, COUNT, A, R )
! @ #else
      IF ( COUNT8 .LE. 0 ) RETURN
      DO 410 J1=1,COUNT8
         R(J1) = A*U(J1)
 410  CONTINUE
! @ #endif
!
      RETURN
      END  !#!  VEC_MULT_CONSTANT8  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VEC_MULT_VECTOR ( U, V, COUNT, R )
! ************************************************************************
! *                                                                      *
! *   L1 vector routine executes the following operation under vectors   *
! *   U, V, R dimensioned COUNT:                                         *
! *                                                                      *
! *   R = A * U                                                          *
! *                                                                      *
! * ### 10-JUL-2003  VEC_MULT_VECTOR  v1.0 (c) L. Petrov 10-JUL-2003 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  COUNT, J1
      REAL*8     U(COUNT), V(COUNT), R(COUNT)
!
! @ #ifdef HPUX
! @       CALL VEC_$DMULT_VECTOR ( U, V, COUNT, R )
! @ #else
      IF ( COUNT .LE. 0 ) RETURN
      DO 410 J1=1,COUNT
         R(J1) = U(J1)*V(J1)
 410  CONTINUE
! @ #endif
!
      RETURN
      END  !#!  VEC_MULT_VECTOR   #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VEC_MULT_VECTOR_I ( U, STRIDE_U, V, STRIDE_V, COUNT, &
     &                               R, STRIDE_R )
! ************************************************************************
! *                                                                      *
! *   L1 vector routine executes the following operation under vectors   *
! *   U, V, R dimensioned COUNT:                                         *
! *                                                                      *
! *   R = A * U                                                          *
! *                                                                      *
! *   EAch vector has its own stride.                                    *
! *                                                                      *
! * ### 10-JUL-2003 VEC_MULT_VECTOR_I v1.0 (c) L. Petrov 19-MAY-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  COUNT, STRIDE_U, STRIDE_V, STRIDE_R
      INTEGER*4  IND_U, IND_V, IND_R, J1
      REAL*8     U(*), V(*), R(*)
!
! @ #ifdef HPUX
! @        CALL VEC_$DMULT_VECTOR_I ( U, STRIDE_U, V, STRIDE_V, COUNT, R, STRIDE_R )
! @ #else
      IF ( COUNT .LE. 0 ) RETURN
      IND_U = 1
      IND_V = 1
      IND_R = 1
      DO 410 J1=1,COUNT
         R(IND_R) = U(IND_U)*V(IND_V)
         IND_U = IND_U + STRIDE_U
         IND_V = IND_V + STRIDE_V
         IND_R = IND_R + STRIDE_R
 410  CONTINUE
! @ #endif
!
      RETURN
      END  !#!  VEC_MULT_VECTOR_I  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MULTI_ADD_3 ( N, OUT_MAT, &
     &                         C1,  MAT1,  &
     &                         C2,  MAT2,  &
     &                         C3,  MAT3,  &
     &                         C4,  MAT4,  &
     &                         C5,  MAT5,  &
     &                         C6,  MAT6,  &
     &                         C7,  MAT7,  &
     &                         C8,  MAT8,  &
     &                         C9,  MAT9,  &
     &                         C10, MAT10, &
     &                         C11, MAT11, &
     &                         C12, MAT12, &
     &                         C13, MAT13, &
     &                         C14, MAT14, &
     &                         C15, MAT15, &
     &                         C16, MAT16  )
! ************************************************************************
! *                                                                      *
! *   Routine MUILTU_ADD_3 computes a sum of N 3x3 matrices and writes   *
! *   the result  in OUT_MAT. N should be in the range [1, 16]. This     *
! *   program supports the argument list of variable lengths.            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   N  ( INTEGER*4  ) -- Number of matrices to summ.                   *
! *   C1 ( REAL*8     ) -- Coefficient before the first input matrix 3x3.*
! * MAT1 ( REAL*8     ) -- First  input matrix 3x3.                      *
! *   C2 ( REAL*8     ) -- Coefficient before the sedond input matrix 3x3*
! * MAT2 ( REAL*8     ) -- Second input matrix 3x3.                      *
! *   C3 ( REAL*8     ) -- Coefficient before the third input matrix 3x3.*
! * MAT3 ( REAL*8     ) -- Third  input matrix 3x3.                      *
! *      etc.                                                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * OUT_MAT ( REAL*8     ) -- Output matrix.                             *
! *                                                                      *
! *  ### 07-JUN-2004  MULTI_ADD_3  v1.0 (c)  L. Petrov  07-JUN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*8     OUT_MAT(3,3), MAT1(3,3), MAT2(3,3), MAT3(3,3), MAT4(3,3), &
     &           MAT5(3,3), MAT6(3,3), MAT7(3,3), MAT8(3,3), MAT9(3,3), &
     &           MAT10(3,3), MAT11(3,3), MAT12(3,3), MAT13(3,3), MAT14(3,3), &
     &           MAT15(3,3), MAT16(3,3)
      REAL*8     C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, &
     &           C14, C15, C16
      REAL*8     TEMP_OUT(3,3)
!
      IF ( N .LT. 1  .OR. N .GT. 16 ) THEN
           CALL NOUT_R8 ( 9, OUT_MAT ) 
           RETURN 
      END IF
      CALL LIB$MOVC3 ( 72,  MAT1, OUT_MAT )
      CALL MUL_VC_V  ( 9, OUT_MAT, C1 )
!
      IF ( N .GE. 2 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C2, MAT2, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 3 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C3, MAT3, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 4 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C4, MAT4, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 5 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C5, MAT5, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 6 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C6, MAT6, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 7 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C7, MAT7, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 8 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C8, MAT8, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 9 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C9, MAT9, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 10 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C10, MAT10, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 11 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C11, MAT11, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 12 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C12, MAT12, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 13 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C13, MAT13, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 14 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C14, MAT14, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 15 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C15, MAT15, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
!
      IF ( N .GE. 16 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL ADDC_VV   ( 9, C16, MAT16, 1.0D0, TEMP_OUT, OUT_MAT )
      END IF
      RETURN 
      END  !#!  MULTI_ADD_3  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MULTI_MUL_3 ( N, OUT_MAT, MAT1, MAT2, MAT3, MAT4, MAT5, &
     &           MAT6, MAT7, MAT8, MAT9, MAT10, MAT11, MAT12, MAT13, MAT14, &
     &           MAT15, MAT16  )
! ************************************************************************
! *                                                                      *
! *   Routine MUILTU_MUL_3 computes a product of N 3x3 matrices and      *
! *   writes the result  in OUT_MAT. N should be in the range [1, 16].   *
! *   This program supports the argument list of variable lengths.       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   N  ( INTEGER*4  ) -- Number of matrices to summ.                   *
! * MAT1 ( REAL*8     ) -- First  input matrix 3x3.                      *
! * MAT2 ( REAL*8     ) -- Second input matrix 3x3.                      *
! * MAT3 ( REAL*8     ) -- Third  input matrix 3x3.                      *
! *      etc.                                                            *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * OUT_MAT ( REAL*8     ) -- Output matrix.                             *
! *                                                                      *
! *  ### 07-JUN-2004  MULTI_MUL_3  v1.0 (c)  L. Petrov  07-JUN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*8     OUT_MAT(3,3), MAT1(3,3), MAT2(3,3), MAT3(3,3), MAT4(3,3), &
     &           MAT5(3,3), MAT6(3,3), MAT7(3,3), MAT8(3,3), MAT9(3,3), &
     &           MAT10(3,3), MAT11(3,3), MAT12(3,3), MAT13(3,3), MAT14(3,3), &
     &           MAT15(3,3), MAT16(3,3)
      REAL*8     TEMP_OUT(3,3)
      INTEGER*4  IER
!
      IF ( N .LT. 1  .OR.  N .GT. 16 ) THEN
           CALL NOUT_R8 ( 9, OUT_MAT ) 
           RETURN 
      END IF
      CALL LIB$MOVC3 ( 72, MAT1, OUT_MAT )
!
      IF ( N .GE. 2 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT2, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 3 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT3, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 4 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT4, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 5 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT5, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 6 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT6, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 7 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT7, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 8 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT8, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 9 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT9, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 10 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT10, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 11 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT11, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 12 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT12, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 13 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT13, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 14 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT14, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 15 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT15, 3, 3, OUT_MAT, IER )
      END IF
!
      IF ( N .GE. 16 ) THEN
           CALL LIB$MOVC3 ( 72, OUT_MAT, TEMP_OUT )
           CALL MUL_MM_II_I ( 3, 3, TEMP_OUT, 3, 3, MAT16, 3, 3, OUT_MAT, IER )
      END IF
!
      RETURN
      END  !#!  MULTI_MUL_3  #!#
