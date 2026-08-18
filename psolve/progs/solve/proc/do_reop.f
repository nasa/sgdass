      SUBROUTINE DO_REOP ( LPARM, END_LPARM, WHO, CNSTR_REC, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DO_EOP PROGRAM SPECIFICATION
!
! 1.1 Apply earth orientation parameter constraints.
!
! 1.2 REFERENCES:
!
! 2.  DO_EOP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4  IUER
      INTEGER*4  LPARM(M_GPA), END_LPARM
      CHARACTER  WHO(3)*20
!
! A - Normal equations matrix
! END_LPARM - Number of eop parameters
! LPARM - Array of eop parameter numbers
! WHO - Array of eop parameter names
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
!     CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES: add_eop
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  IER
      REAL*8     FSTJD, CVINF(6)
      INTEGER*4  NUM, I, INC
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   pet  970117  Added support of B3D parametrization
!   pet  970226  Added support of B1B3D parametrization
!   pet  970610  Added support of new EOP parametrization
!   pet  980119  Support of CNSTR_REC data structure added.
!   pet  2002.05.08  Added support of argument IUER
!   jwr  2002.12.10  l2tol2 added to help in (-i2) conversion.
!
! 5.  DO_EOP PROGRAM STRUCTURE
!
!   extract covariance elements from data file, form constraint and add
!
      INCLUDE   'cnstr.i'
      TYPE ( CNSTR__STRU ) ::  CNSTR_REC
      INTEGER*2  INT2_ARG
!
!     Transformation INT2 --> INT4
!C
      FSTJD = UT1INV(1)
      INC = UT1INV(2) + 0.1D0
      NUM = UT1INV(3) + 0.1D0
      IF ( EOPRCONS(1) .LE. 0.0D0  .OR. &
     &     EOPRCONS(2) .LE. 0.0D0  .OR. &
     &     EOPRCONS(3) .LE. 0.0D0       ) THEN
!
! -------- If at least one diagonal element of the covariance matrix is zero
! -------- then no constraints will be imposed
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
      IF ( INC .EQ.0  .OR.  EOPDLY_CHR(1:4) .EQ. 'NONE' ) THEN
           FSTJD = UT1INB(1)
           INC = UT1INB(2) + 0.1D0
           NUM = UT1INB(3) + 0.1D0
      ENDIF
!
      IF ( EOP_STYLE(1) .EQ. 0 ) THEN
           DO I=1,NROT
              CALL COV_REOP ( TROT(I), FSTJD, INC, NUM, PI__NUM, CVINF, WHO, &
     &                        EOPRCONS )
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADD_EOP  ( LPARM, CVINF, WHO, END_LPARM, TRUE__L2, &
     &             CNSTR_REC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8511, IUER, 'DO_EOP', 'Error in an '// &
     &                 'attempt to impose constraint on EOP rate' )
                   RETURN
              END IF
           ENDDO
        ELSE
           CALL COV_REOP ( TROT_A1, FSTJD, INC, NUM, PI__NUM, CVINF, WHO, &
     &                     EOPRCONS )
!
           CALL ERR_PASS ( IUER, IER )
           CALL ADD_EOP  ( LPARM, CVINF, WHO, END_LPARM, TRUE__L2, &
     &          CNSTR_REC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8512, IUER, 'DO_EOP', 'Error in an '// &
     &              'attempt to impose constraint on EOP rate' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_REOP  #!#
