        SUBROUTINE INVA ( N, A, EPS, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  INVA  inverse arbitrary ( of course, non-singular )     *
! *   square matrix using elimination method with search of maximal in   *
! *   modulo diagonal element.                                           *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       N  ( INTEGER*4 )   -- Dimension of the matrix.                 *
! *     EPS  ( REAL*8, OPT ) -- minimal acceptable value of diagonal     *
! *                             element. Parameter EPS controls loss of  *
! *                             precision during matrix inversion.       *
! *                             Default value is 1.D-15.                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       A  ( REAL*8    )  --  Matrix in rectangular form dimension of  *
! *                             N*N.                                     *
! *                             . . . . . . . . . . . . . . . . . . . .  *
! *                             input: initial matrix which we are going *
! *                                    to invert.                        *
! *                             output: matrix to be inverse to the      *
! *                                     initial matrix.                  *
! *                             . . . . . . . . . . . . . . . . . . . .  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *      If IUER is [1, 99999] is means that no one element was found    *
! *      larger in module than EPS at the IUER-th step. It means that    *
! *      the matrix is near to be singular. NB: Matrix is spoiled in     *
! *      this case: it is not equal to the initial matrix.               *
! *      IUER=100001  --  parameter N is too small: N<1                  *
! *      IUER=100002  --  parameter N is too high (larger than 100000 ). *
! *                                                                      *
! *  ###  19-MAY-1989     INVA     v3.2  (c)  L. Petrov 13-APR-2009 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  N, IUER
        REAL*8     A(N,N), AMAIN, AMAIN_NEG, AMAX, WORK
        REAL*8     EPS, EPS_U, EPS_DEF
        REAL*8,     ALLOCATABLE :: AW(:)
        PARAMETER ( EPS_DEF=1.D-15 )
        INTEGER*4   J1, J2, J3, J4, J5, J6, IMAIN
        INTEGER*4,  ALLOCATABLE :: IA(:)
!
        IF ( LOC(EPS) .EQ. 0 ) THEN
             EPS_U = EPS_DEF
           ELSE
             EPS_U = EPS
        END IF
        IF ( N .LT. 1 ) THEN
             CALL ERR_LOG ( 100001, IUER, 'INVA', 'Parameter N<1' )
             RETURN
        END IF
        ALLOCATE ( IA(N) )
        ALLOCATE ( AW(N) )
!
! ----- Initialization
!
        CALL NOUT_I4 ( N, IA )
!
! ----- Cycle for rows elimination
!
        DO 410 J1=1,N
! - - - - - - - - - - - - - - - - - - - - - - - - - -
!
! -------- Search of the row among non-eliminated rows which has maximal in
! -------- module element.
!
! -------- ( If this element turns out to be less then the specified precision
! -------- EPS_U then error condition is fixed and IUER is assigned to J1 and
! -------- execution is terminated)
!
! -------- Variable "pivot element": AMAIN is assigned to the value of that
! -------- element.
!
! -------- Variable "pivot index": IMAIN is assigned the index of the row
! -------- which contains this element.
!
! -------- Value J1 is assigned to the element of array IA: "counter of
! -------- eliminated rows" with index IMAIN.
!
! ___________________________________________________________
           IMAIN = 0                           !  1-st step |
           AMAX  = EPS_U                       !  1-st step |
           DO 420 J2=1,N                       !  1-st step |
              IF ( IA(J2) .EQ. 0 ) THEN        !  1-st step |
                   WORK = DABS( A(J2,J2) )     !  1-st step |
                   IF ( WORK .GT. AMAX ) THEN  !  1-st step |
                        AMAX  = WORK           !  1-st step |
                        IMAIN = J2             !  1-st step |
                   END IF                      !  1-st step |
              END IF                           !  1-st step |
  420      CONTINUE                            !  1-st step |
           IF ( IMAIN .EQ. 0 ) GOTO 810        !  1-st step |
           IA(IMAIN) = J1                      !  1-st step |
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! -------- "Pivot element" is replaced by the element to be reciprocal
!
! ____________________________________________________________
           A(IMAIN,IMAIN) = 1.D0/A(IMAIN,IMAIN)   !  2-nd step
           AMAIN = A(IMAIN,IMAIN)                 !  2-nd step
           AMAIN_NEG = -AMAIN                     !  2-nd step
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! -------- Transformation of the elements of the "main column" (except the main
! -------- element)
!
! _________________________________________________________
           DO 430 J3=1,N                       !  3-rd step
              IF ( J3 .EQ. IMAIN ) GOTO 430    !  3-rd step
              A(J3,IMAIN) = A(J3,IMAIN)*AMAIN  !  3-rd step
              AW(J3) = A(IMAIN,J3)*AMAIN_NEG   !  3-rd step
  430      CONTINUE                            !  3-rd step
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! -------- Transformation of the elements which stand not on the "main row"
! -------- and "main column"
!
! _____________________________________________________________________
!          !  Accelerating procedure written in Assembler  !  4-th step
!#           CALL INV_C ( N, IMAIN, A )                    !  4-th step
           DO 440 J4=1,N                                   !  4-th step
              IF ( J4 .EQ. IMAIN ) GOTO 440                !  4-th step
              WORK = A(IMAIN,J4)                           !  4-th step
              DO 450 J5=1,N                                !  4-th step
                 A(J5,J4) = A(J5,J4) - WORK*A(J5,IMAIN)    !  4-th step
  450         CONTINUE                                     !  4-th step
  440     CONTINUE                                         !  4-th step
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! -------- Transformation of elements which stands on the main row (except
! -------- "pivot element" )
!
! __________________________________________________________
            DO 460 J6=1,N                       !  5-th step
               IF ( J6 .EQ. IMAIN ) GOTO 460    !  5-th step
               A(IMAIN,J6) = AW(J6)             !  5-th step
  460       CONTINUE                            !  5-th step
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  410   CONTINUE
!
        DEALLOCATE ( IA )
        DEALLOCATE ( AW )
        CALL ERR_LOG ( 0, IUER )
        RETURN
!
  810   CONTINUE
!
        DEALLOCATE ( IA )
        DEALLOCATE ( AW )
        CALL ERR_LOG ( J1, IUER, 'INVA', 'Matrix is (almost) singular' )
!
        RETURN
        END  SUBROUTINE   INVA  !#!#
