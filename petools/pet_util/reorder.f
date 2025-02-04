      SUBROUTINE TRG_EXPAND ( M, IORD, A, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  TRG_EXPAND  moves right some columns and rows of the      *
! *   square symmetric matrix A in the upper triangular representation   *
! *   expanding it by adding zero columns/rows. The "holes" -- places    *
! *   where elements were -- are filled by zeros. IMPORTANT: elemetns    *
! *   can be moved only to the right end!  Vector IORD contains indeces  *
! *   of the old places of the elements.                                 *
! *                                                                      *
! *   Example:  M=5                                                      *
! *                                                                      *
! *   Before:    A   IORD   1    2    0    0    3                        *
! *                                                                      *
! *                        1.1  0.0  0.7  0.0  0.0                       *
! *                                                                      *
! *                             2.3  3.8  0.0  0.0                       *
! *                                                                      *
! *                                  6.1  0.0  0.0                       *
! *                                                                      *
! *                                       0.0  0.0                       *
! *                                                                      *
! *                                            0.0                       *
! *                                                                      *
! *   After :    A                                                       *
! *                                                                      *
! *                        1.1  0.0  0.0  0.0  0.7                       *
! *                                                                      *
! *                             2.3  0.0  0.0  3.8                       *
! *                                                                      *
! *                                  0.0  0.0  0.0                       *
! *                                                                      *
! *                                       0.0  0.0                       *
! *                                                                      *
! *                                            6.1                       *
! *                                                                      *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *     M ( INTEGER*4  ) -- Dimension of the vector (New, expanded       *
! *                         dimension).                                  *
! *  IORD ( INTEGER*4  ) -- Vector of indeces of the old placees of the  *
! *                         element. If K -- new index of the element,   *
! *                         then IORD(K) --  old index of the element.   *
! *                         IORD(K)=0 means that K-th element of expanded*
! *                         vector should be equal zero.                 *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *     A ( REAL*8     ) -- Expanding vector.                            *
! *  IUER ( INTEGER*4  ) -- Universal error handler.                     *
! *         Input:  swicth IUER=0 -- no error messages generated even in *
! *                 the case of error. IUER=-1 -- in the case of error   *
! *                 the message will pe put on stdout.                   *
! *         Output: 0 in the case of successfull completion and non-zero *
! *                 in the case of error.                                *
! *                                                                      *
! *  ###  14-FEB-97   VEC_EXPAND   v1.0  (c)  L. Petrov  14-FEB-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M, IORD(M), IUER
      REAL*8     A(*)
      INTEGER*4  J1, J2, J3, J4
      CHARACTER  STR*20, STR1*20
      INTEGER*4  I_LEN, I, J, LOC
      LOC(I,J)=MIN(I,J) + (MAX(I,J)*(MAX(I,J)-1))/2
!
      DO 410 J1=1,M
         IF ( IORD(J1) .LT. 0 ) THEN
              CALL CLRCH ( STR     )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IORD(J1), STR1 )
              CALL ERR_LOG ( 5171, IUER, 'TRG_EXPAND', STR(1:I_LEN(STR))// &
     &            '-th element of the IORD is negative: IORD('// &
     &             STR(1:I_LEN(STR))//') = '//STR1(1:I_LEN(STR1))// &
     &            '. Matrix A remained untouched.' )
              RETURN
           ELSE IF ( IORD(J1) .GT. J1 ) THEN
              CALL CLRCH ( STR     )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IORD(J1), STR1 )
              CALL ERR_LOG ( 5172, IUER, 'TRG_EXPAND', STR(1:I_LEN(STR))// &
     &            '-th element of the IORD is more than its index: IORD('// &
     &             STR(1:I_LEN(STR))//') = '//STR1(1:I_LEN(STR1))// &
     &            ': Attempt to move the element to the left. Matrix A '// &
     &            'remained untouched.' )
              RETURN
         END IF
 410  CONTINUE
!
      DO 420 J2=M,1,-1
         IF ( IORD(J2) .EQ. 0 ) THEN
              DO 430 J3=M,1,-1
                 A( LOC(J2,J3) ) = 0.D0
 430          CONTINUE
           ELSE
              DO 440 J4=J2,1,-1
                 IF ( IORD(J4) .EQ. 0 ) THEN
                      A( LOC(J2,J4) ) = 0.0D0
                   ELSE
                      A( LOC(J2,J4) ) = A( LOC(IORD(J2),IORD(J4)) )
                 END IF
 440          CONTINUE
         END IF
 420  CONTINUE
!!      DATA IPL / 1, 2, 3, 4, 5, 0, 6, 0, 0, 7 /
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  TRG_EXPAND  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VEC_EXPAND ( M, IORD, A, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VEC_EXPAND  moves right some elements of the vector A     *
! *   expanding it. The "holes" -- place where element was -- are filled *
! *   by zeros. IMPORTANT: elemetns can be moved only to the right end!  *
! *   Vector IORD contains indeces of the old places of the elements.    *
! *                                                                      *
! *   Example:  M=10                                                     *
! *                                                                      *
! *   Before:    A ( 5.5.  0.0  3.4  7.7  8.8  2.6  7.0  0.0  0.0  0.0 ) *
! *                                                                      *
! *           IORD (  1     2    0    4    0    0    0    6    0    7  ) *
! *                                                                      *
! *   After :    A ( 5.5.  3.4  0.0  7.7  0.0  0.0  0.0  2.6  0.0  7.0 ) *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *     M ( INTEGER*4  ) -- Dimension of the vector (New, expanded       *
! *                         dimension).                                  *
! *  IORD ( INTEGER*4  ) -- Vector of indeces of the old placees of the  *
! *                         element. If K -- new index of the element,   *
! *                         then IORD(K) --  old index of the element.   *
! *                         IORD(K)=0 means that K-th element of expanded*
! *                         vector should be equal zero.                 *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *     A ( REAL*8     ) -- Expanding vector.                            *
! *  IUER ( INTEGER*4  ) -- Universal error handler.                     *
! *         Input:  swicth IUER=0 -- no error messages generated even in *
! *                 the case of error. IUER=-1 -- in the case of error   *
! *                 the message will pe put on stdout.                   *
! *         Output: 0 in the case of successfull completion and non-zero *
! *                 in the case of error.                                *
! *                                                                      *
! *  ###  14-FEB-97   VEC_EXPAND   v1.0  (c)  L. Petrov  14-FEB-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M, IORD(M), IUER
      REAL*8     A(*)
      INTEGER*4  J1, J2
      CHARACTER  STR*20, STR1*20
      INTEGER*4  I_LEN
!
      DO 410 J1=1,M
         IF ( IORD(J1) .LT. 0 ) THEN
              CALL CLRCH ( STR     )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IORD(J1), STR1 )
              CALL ERR_LOG ( 5181, IUER, 'VEC_EXPAND', STR(1:I_LEN(STR))// &
     &            '-th element of the IORD is negative: IORD('// &
     &             STR(1:I_LEN(STR))//') = '//STR1(1:I_LEN(STR1))// &
     &            '. Matrix A remained untouched.' )
              RETURN
           ELSE IF ( IORD(J1) .GT. J1 ) THEN
              CALL CLRCH ( STR     )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IORD(J1), STR1 )
              CALL ERR_LOG ( 5182, IUER, 'VEC_EXPAND', STR(1:I_LEN(STR))// &
     &            '-th element of the IORD is more than its index: IORD('// &
     &             STR(1:I_LEN(STR))//') = '//STR1(1:I_LEN(STR1))// &
     &            ': Attempt to move the lement to the left. Vector A '// & 
     &            'remained untouched.' )
              RETURN
         END IF
 410  CONTINUE
!
      DO 420 J2=M,1,-1
         IF ( IORD(J2) .EQ. 0 ) THEN
              A(J2) = 0.D0
           ELSE
              A( J2 ) = A ( IORD(J2) )
         END IF
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  VEC_EXPAND  #!#
