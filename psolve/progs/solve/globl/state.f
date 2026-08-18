      SUBROUTINE STATE()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  STATE PROGRAM SPECIFICATION
!
! 1.1 Set up logical flags for control of program execution in GLOBL.
!
! 1.2 REFERENCES:
!
! 2.  STATE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glocm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: globl
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 KBIT
      CHARACTER*(NAME_SIZE) CNAME
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  STATE PROGRAM STRUCTURE
!
! Make normal equations in PROC?
!
      MKNRMEQ=.NOT.ARCTHER
!
! Eliminate arc parameters in ARCPE?
!
      ELARCPM=(IOCGM.NE.0.OR.ISOLU.EQ.1).AND..NOT.ARCTHER
!
! Add to CGM with ADDER?
!
      ADMKCGM=IOCGM.NE.0
      INF2COV= &
     &  (ISOLU.EQ.1.AND.IARCNM.EQ.1).OR.(ISOLU.EQ.0.AND.ICONT.EQ.0)
!
! Solve for arc parameters in BACK?
!
      ARCCALC=ISOLU.EQ.1.OR.(ICONT.EQ.0.AND.ISOLU.EQ.0.AND.IOCGM.NE.0)
!
! Produce output with COVP?
!
      KOUTPUT=ISOLU.EQ.1.OR.(ICONT.EQ.0.AND.ISOLU.EQ.0)
!
! Produce output with ADJST?
!
      KGLBOUT=ARCCALC.AND.INF2COV
      MAKCVRN=KBIT( PRE_IBATCH, INT2(9))
      RETURN
      END
