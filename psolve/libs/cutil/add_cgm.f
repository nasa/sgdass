      SUBROUTINE ADD_CGM(fname,cmerg)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ADD_CGM PROGRAM SPECIFICATION
!
! 1.1 Set up the buffer for the call to program ADDER.
!
! 1.2 REFERENCES:
!
! 2.  ADD_CGM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
      Character*(*) fname,cmerg
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: globl
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 NPNOR
      INTEGER*2 ADDRBUF(66),I
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  950316  Created
!
! 5.  ADD_CGM PROGRAM STRUCTURE
!
! Set up ADDER buffer with CGM name
!
      CALL CHAR2HOL( fname, ADDRBUF(1), INT2(1), INT2(64) )
      CALL CHAR2HOL( cmerg, ADDRBUF(33), INT2(1), INT2(64) )
!
! Set to add another CGM to the CGM
!
      ADDRBUF(65)=1 !ADD_OR_SUB
      ADDRBUF(66)=0 !ARC_OR_CGM
!
! Write the buffer
!
      CALL USE_BUFFER( ADDRBUF, INT2(66), 'OWC' )
!
! Run program ADDER to add the arc
!
      CALL RUN_PROG( 'ADDER', 'WAIT', INT2(0) )
      FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMF'//PRE_LETRS
      RETURN
      END
