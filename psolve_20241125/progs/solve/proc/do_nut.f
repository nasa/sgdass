      SUBROUTINE DO_NUT ( APARM, END_LPARM, WHO, CNSTROBJ, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DO_NUT PROGRAM SPECIFICATION
!
! 1.1 Apply earth orientation parameter constraints.
!
! 1.2 REFERENCES:
!
! 2.  DO_NUT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
      INCLUDE 'cnstr.i'
!
! 2.2 INPUT Variables:
!
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  APARM(M_GPA), END_LPARM
      CHARACTER  WHO(3)*20
      INTEGER*4  IUER
!
! A - Normal equations matrix
! END_LPARM - Number of NUT parameters
! LPARM - Array of NUT parameter numbers
! WHO - Array of NUT parameter names
!
! 2.3 OUTPUT Variables:
!
! A - Modified normal equations matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES: add_NUT
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  J1, IER
      LOGICAL*4    FALSE_L4
      PARAMETER  ( FALSE_L4 = .FALSE. )
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   pet  970117 Added support of B3D   parametrization
!   pet  970226 Added support of B1B3D parametrization
!   pet  2002.09.17  Changed logic. According to new logic constraints on
!                    nutation angle in longitude and nutation angle in
!                    obliquity are applied.
!
! 5.  DO_NUT PROGRAM STRUCTURE
!
!   extract covariance elements from data file, form constraint and add
!
!     Transformation INT2 --> INT4
!
      DO 410 J1=1,END_LPARM
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_NAM ( 'NUT_OFF', J1, 'Nutation offset', 'mas', 0.0D0, &
     &                      NUTCONS(J1)*PI__NUM/(180.0D0*3600.0D0*1000.0D0), &
     &                      FALSE_L4, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8571, IUER, 'DO_NUT', 'Error in an '// &
     &            'attempt to put information about nutation angles '// &
     &            'constraint' )
              RETURN
         END IF
!
! ------ Add constraint equation
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADDCNS_EQU ( 'NUT_OFF', J1, APARM(J1), 1.0D0, &
     &                      FALSE_L4, CNSTROBJ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8572, IUER, 'DO_NUT', 'Error in an '// &
     &            'attempt to put information about nutation angles '// &
     &            'constraint equations' )
              RETURN
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_NUT  #!#
