      SUBROUTINE DO_BLC ( WHO_STA, LPARM, IPARMS, BAS_CLK_SIGMA, CNSTROBJ, &
     &                    IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DO_BLC PROGRAM SPECIFICATION
!
! 1.1 Apply baseline clock constraints.
!
! 1.2 REFERENCES:
!
! 2.  DO_BLC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4     IPARMS
      CHARACTER     LPARM(M_GPA)*(*), WHO_STA(*)*8
      INTEGER*4     IUER, IER
!
! A - The normal equation matrix
! IPARM - Array of parameter names
! IPARMS - Number of parameters
! WHO_STA - Array of station names
!
! 2.3 OUTPUT Variables:
!
! A - Modified normal equations matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'cnstr.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4    END_APARM, J1, APARM(M_GPA)
      LOGICAL*2    LDUM
      CHARACTER*3  PARMTYP
      CHARACTER*20 DUMRA
      REAL*8       BAS_CLK_SIGMA
      LOGICAL*4    FALSE_L4
      PARAMETER  ( FALSE_L4 = .FALSE. )
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
!     Transformation INT2 --> INT4
!
! 4.  HISTORY
!   MWH  941013  Created
!  jmg  960610   Remove holleriths.
!  pet  970117   Added support of B3D parametrization
!  pet  970226   Added support of B1B3D parametrization
!  pet  980119   Rewrote to support CNSTROBJ data structure.
!  pet  980205   Declared the sigma of the constrain in solve.i instead of
!                hard-coded value. Write down information about the type
!                of applied constraint.
!  pet  980722       Made BAS_CLK_SIGMA formal parameter instead of named
!                    constatnt in solve.i block.
!  pet  2002.09.17   Changed internal logic: the new version puts equations of
!                    constraitns in CNSTROBJ, while the old version put normal
!                    equations of constraints
!
! 5.  DO_BLC PROGRAM STRUCTURE
!
!   Set up the constraint:  it is hard-wired here . . .
!
!C
      PARMTYP    = 'BCL'
      LDUM       = .FALSE.
      CALL CINDEX_PARM ( PARMTYP, APARM, LPARM, IPARMS, END_APARM, DUMRA, &
     &                   LDUM, WHO_STA(1) )
      DO 410 J1=1,END_APARM
!
! ------ Add information about the type of the constraint applied
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_NAM ( 'BLC_VAL', J1, 'Baseline clock', 'sec', 0.0D0, &
     &                      BAS_CLK_SIGMA, FALSE_L4, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8561, IUER, 'DO_BLC', 'Error in an '// &
     &            'attempt to put information about baseline dependent '// &
     &            'clock constraint' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_EQU ( 'BLC_VAL', J1, APARM(J1), 1.0D0, &
     &                      FALSE_L4, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8562, IUER, 'DO_BLC', 'Error in an '// &
     &            'attempt to put information about baseline dependent clock'// &
     &            'constraint equations' )
              RETURN
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_BLC  #!#
