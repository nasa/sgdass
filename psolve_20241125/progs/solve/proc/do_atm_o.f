      SUBROUTINE DO_ATM_O ( WHO_STA, LPARM, IPARMS, CNSTROBJ, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DO_ATM PROGRAM SPECIFICATION
!
! 1.1 Apply atmosphere constraints.
!
! 1.2 REFERENCES:
!
! 2.  DO_ATM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4    IPARMS
      CHARACTER    LPARM(M_GPA)*(*), WHO_STA(*)*8
      INTEGER*4    IUER
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
      INCLUDE 'precm.i'
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'cnstr.i'
      INCLUDE 'fast.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES: add_atm
!
! 3.  LOCAL VARIABLES
!
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      LOGICAL*4    FALSE_L4
      PARAMETER  ( FALSE_L4 = .FALSE. )
      INTEGER*4  END_APARM, I, J, APARM(M_GPA)
      CHARACTER  DUMRA*20
!
      INTEGER*4  IER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: CHECK_STABIT
!
!     Transformation INT2 --> INT4
!
! 4.  HISTORY
!  jmg  960610  Remove holleriths.
!  pet  971203  Added logic for bypassing deselected station
!  pet  980109  Added information and error messages
!  pet  980119  Entirely re-wrote to support data structure CNSTR
!  pet  980205  Added writing information about the type of applied constraint.
!  pet  980508  Corrected a coding error
!  pet  2002.09.18   Changed internal logic: the new version puts equations of
!                    constraitns in CNSTROBJ, while the old version put normal
!                    equations of constraints
!
! 5.  DO_ATM PROGRAM STRUCTURE
!
!     Set up the constraint:  it is hard-wired here . . .
!
      DO I=1,NUMSTA
!
! ------ Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
         IF ( CHECK_STABIT ( I ) ) THEN
              CALL CINDEX_PARM_O ( 'AT1', APARM, LPARM, IPARMS, END_APARM, &
     &                              DUMRA, FALSE__L2, WHO_STA(I) )
!
! ----------- Add the constraint
!
              IF ( END_APARM .GT. 0 ) THEN
!
! ---------------- Apply constraints
!
                   DO J=1,END_APARM
!
! ------------------- Add information about the type of the constraint applied
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_NAM ( 'OAT_RATE', J, 'Atmopshere '// &
     &                    'rate for segments', 'psec/hr', 0.0D0, &
     &                     SACNST(I)*1.D-12/3600.0D0, FALSE_L4, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8551, IUER, 'DO_ATM_O', &
     &                         'Trap of internal control: attempt to impose '// &
     &                         'constraints on atmosphere rate' )
                           RETURN
                      END IF
!
! ------------------- Add the constraint
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( 'OAT_RATE', J, APARM(J), &
     &                                   1.0D0, FALSE_L4, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8551, IUER, 'DO_ATM_O', &
     &                         'Trap of internal control: attempt to impose '// &
     &                         'constraints on atmosphere rate' )
                           RETURN
                      END IF
                   END DO
              END IF
         END IF
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_ATM_O  #!#
