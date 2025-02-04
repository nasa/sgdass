      SUBROUTINE DO_CLK_O ( WHO_STA, LPARM, IPARMS, CNSTROBJ, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DO_CLK_O PROGRAM SPECIFICATION
!
! 1.1 Apply clock constraints as requested.
!
! 1.2 REFERENCES:
!
! 2.  DO_CLK_O INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IPARMS
      CHARACTER LPARM(M_GPA)*(*), WHO_STA(*)*(*)
!
! A - Normal equation matrix
! IPARM - Array of parameter names
! IPARMS - Number of parameters
! WHO_STA - Array of station names
!
! 2.3 OUTPUT Variables:
!
! A - Normal equation matrix with constraints added
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'cnstr.i'
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES:  add_clk_o
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*4    FALSE_L4
      PARAMETER  ( FALSE_L4 = .FALSE. )
      INTEGER*4  APARM(M_GPA), END_APARM, I, J
      INTEGER*4  IER, IUER
      CHARACTER  DUMRA*20
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: CHECK_STABIT
!
!     Transformation INT2 --> INT4
!
! 4.  HISTORY
!   WHO  WHEN    WHAT
!   jmg  960610  Remove holleriths.
!   pet  971024  Support of B3D and B1B3D case added
!   pet  971203  Added logic for bypassing deselected station
!   pet  980119  Entirely re-wrote to support data structure CNSTR
!   pet  2002.09.18   Changed internal logic: the new version puts equations of
!                     constraitns in CNSTROBJ, while the old version put normal
!                     equations of constraints
!
! 5.  DO_CLK_O PROGRAM STRUCTURE
!
      DO I=1,NUMSTA
!
! ------ Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
         IF ( CHECK_STABIT ( I ) ) THEN
              CALL CINDEX_PARM_O ( 'CL1', APARM, LPARM, IPARMS, END_APARM, &
     &             DUMRA, FALSE__L2, WHO_STA(I) )
              IF ( END_APARM .GT. 0 ) THEN
                   DO J=1,END_APARM
!
! ------------------- Add information about the type of the constraint applied
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_NAM ( 'OCL_RATE', J, 'Clock rate '// &
     &                    'for segments', '10^-14 sec/sec', 0.0D0, &
     &                     SCCNST(I)*1.D-14, FALSE_L4, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8541, IUER, 'DO_CLK_O', &
     &                         'Error in an attempt to put information about '// &
     &                         'clock rate constraint' )
                           RETURN
                      END IF
!
! ------------------- Add the constraint
!
                      IER = -1
                      CALL ADDCNS_EQU ( 'OCL_RATE', J, APARM(J), &
     &                                   1.0D0, FALSE_L4, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8542, IUER, 'DO_CLK_O', &
     &                         'Error in an attempt to put clock rate '// &
     &                         'constraint equation coefficients' )
                           RETURN
                      END IF
                   END DO
              END IF
         END IF
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_CLK_O  #!#
