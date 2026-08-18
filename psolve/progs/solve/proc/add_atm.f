      SUBROUTINE ADD_ATM ( FAST_MODE, CONSTRAINT, APARM, N, A, &
     &                     B3DOBJ, B1B3DOBJ, CNSTR_REC )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ADD_ATM PROGRAM SPECIFICATION
!
! 1.1 Add appropriate atmosphere constraints.
!
! 1.2 REFERENCES:
!
! 2.  ADD_ATM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8    A(*), CONSTRAINT
      INTEGER*4 N,    APARM(M_GPA)
!
! A - Normal equation matrix
! APARM - Cross reference for atmosphere parameter numbers
! CONSTRAINT - Hard-wired as 1/(SIGMA**2) (SIGMA is provided by user)
! N - Number of atmosphere parameters
!
! 2.3 OUTPUT Variables:
!
! A - Normal equation matrix with constraint added
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: do_atm
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  I, J, N4
      INTEGER*8  POS1, POS2, POS3
      INTEGER*8, EXTERNAL :: INDX8
!C
      INCLUDE   'fast.i'
      INCLUDE   'cnstr.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTR_REC
      CHARACTER  TYP*1
      INTEGER*4  FAST_MODE, NBL, IR, IC, J1, J2
      INTEGER*8  IAD_DIAG, IAD_CC, IAD_LL, IAD_CL
      INTEGER*4  IUER
      REAL*8     VAL_DIAG, EPS
      PARAMETER  ( EPS = 1.D-12 )
      ADDRESS__TYPE :: FULL_B3D, FULL_B1B3D
!
!     Transformation INT2 --> INT4
!C
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  910524  Modify for new parameterization scheme to include
!                       off-diagonal elements
!   pet  970117  Added support of B3D parametrization
!   pet  970226  Added support of B1B3D parametrization
!   pet  980119  Re-wrote to support data structure CNSTR
!
! 5.  ADD_ATM PROGRAM STRUCTURE
!
!C
      IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD   .OR. &
     &     FAST_MODE .EQ. F__B1D     ) THEN
!
! -------- Full matrix case
!
! -------- Testing: if there were not any observations and => all diagonal
! -------- elements on matrices which corresponds to atmpospoere parameters
! -------- are zero -- not to impose  constraint
!
           DO I=1,N
              POS1=INDX8(APARM(I),APARM(I))
              IF ( A(POS1) .GT. EPS ) GOTO 810
           ENDDO
           RETURN
 810       CONTINUE
         ELSE IF ( FAST_MODE .EQ. F__B3D  .OR.  FAST_MODE .EQ. F__B1B3D ) THEN
!
! -------- B3D or B1B3D case
!
! -------- Testing: if there were not any observations and => ALL diagonal
! -------- elements on matrices which corresponds to atmpospoere parameters
! -------- are zero -- not to impose  constraint
!
           DO 410 J1=1,N
!
! ----------- Find IAD_DIAG -- the address of the element in submatrices B3D or
! ----------- B1B3D which corrspond to the element ( APARM(J1), APARM(J1) ) of
! ----------- the full matrix
!
              IF ( FAST_MODE .EQ. F__B3D ) THEN
                   IAD_DIAG = FULL_B3D   ( B3DOBJ, APARM(J1), &
     &                                     APARM(J1), TYP, NBL, IR, IC )
                ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                   IAD_DIAG = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, APARM(J1), &
     &                                     APARM(J1), TYP, NBL, IR, IC )
              END IF
              IF ( IR .EQ. -1 .OR. IC .EQ. -1 ) THEN
                   WRITE ( 6, * ) ' j1=',j1,' aparm(j1) = ',aparm(j1), &
     &                    ' nbl=',nbl,' ir=',ir,' ic=',ic, &
     &                    ' iad_diag=',iad_diag
                   WRITE ( 6, * ) ' b3dobj%ad_b0 = ',b3dobj%ad_b0
                   IUER = -1
                   CALL ERR_LOG ( 3431, IUER, 'ADD_ATM','Internal error' )
                   RETURN
              END IF
!
! ----------- Extract the element ...
!
              CALL LIB$MOVC3 ( 8, %VAL(IAD_DIAG), VAL_DIAG )
!
! ----------- ... and compare it with machine zero
!
              IF ( VAL_DIAG .GT. EPS ) GOTO 820
 410       CONTINUE
           RETURN
 820     CONTINUE
      END IF
!
      DO 420 J2=2,N
!
! ------ Find addresses for the elements of j2,j2 atmosphere,
! ------ j2-1,j2-1 atmosphere and for j2,j2-1 atmosphere
!
         CALL ADD_CNSTR ( APARM(J2-1), APARM(J2-1),  CONSTRAINT, CNSTR_REC )
         CALL ADD_CNSTR ( APARM(J2-1), APARM(J2),   -CONSTRAINT, CNSTR_REC )
         CALL ADD_CNSTR ( APARM(J2),   APARM(J2),    CONSTRAINT, CNSTR_REC )
 420  CONTINUE
!
      RETURN
      END  !#!  ADD_ATM  #!#
