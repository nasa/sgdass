      SUBROUTINE REORDER_X ( LPARM1, IARCS, LPARM2, TEMPRA )
      IMPLICIT NONE
!
! 1.  REORDER_X PROGRAM SPECIFICATION
!
! 1.1 Construct a x-arc parm list with the order of the local parms
!     maintained, but the order of the global parms corresponding to that
!     of the i-arc list.
!
! 1.2 REFERENCES:
!
! 2.  REORDER_X INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
!
! 2.2 INPUT Variables:
!
      CHARACTER*20 LPARM1(*), LPARM2(*)
      INTEGER*4 IARCS
!
! IARCS - Number of arc parameters in an individual list
! IPARM1 - Locals from x_arc list
! IPARM2 - Globals from y_arc list
!
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 TEMPRA(M_GPA)
!
! TEMPRA - Cross reference list
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'baccm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arc_j
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 K, I, J, R, TPARAMS
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!  jmg  960610 Remove holleriths.
!
! 5.  REORDER_X PROGRAM STRUCTURE
!
      DO I=1,IARCS
         LTPARM(I) = LPARM1(I)
      ENDDO
!
!   move the globals in y-arc list to tparm list, behind the locals
!   in x-arc list
!
      DO I=1,NPARM2
         LTPARM(IARCS+I) = LPARM2(I)
      ENDDO
      TPARAMS = IARCS + NPARM2
!
!   here's where things get dicey:  we need the cross-reference list from
!   the new matrix to the old matrix.  This is to let us process in terms
!   of the new order by being able to transform back to the old order . . .
!
      CALL CXEPAR ( LTPARM, IXC2J, TPARAMS, LPARM1, NPARM3 )
!
!   now reorder the parameter index list IND_COV so that the globals
!   are in the yparm order
!   note that this works for lists with arc+global parms, because is is
!   implicit that both lists contain the arc parameters, but globals are
!   different order
!
      K=1
      DO 20 I=1,TPARAMS
         DO 10 J=1,END_JDX
            IF ( IXC2J(I) .EQ. IND_JCOV(J) ) THEN
                 TEMPRA(K)=IND_JCOV(J)
                 K=K+1
                 GOTO 20
            ENDIF
 10      CONTINUE
 20   CONTINUE
      DO I=1,END_JDX
         IND_JCOV(I)=TEMPRA(I)
      ENDDO
      RETURN
      END
