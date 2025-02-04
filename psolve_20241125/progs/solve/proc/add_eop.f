      SUBROUTINE ADD_EOP ( LPARM, CVINF, WHO, END_LPARM, F_RATE, CNSTROBJ, &
     &                     IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Updated to specificaly type integers which
!-------------------------------------------------
! 1.  ADD_EOP PROGRAM SPECIFICATION
!
! 1.1 Add earth orientation constraints to normal equations matrix.
!
! 1.2 REFERENCES:
!
! 2.  ADD_EOP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8       CVINF(6)
      INTEGER*4    IUER
      INTEGER*4    LPARM(M_GPA), END_LPARM
      CHARACTER    STR*32
      CHARACTER*20 WHO(3)
      LOGICAL*2 F_RATE
      LOGICAL*4    FALSE_L4
      PARAMETER  ( FALSE_L4 = .FALSE. )
!
! A - Normal equations matrix
! CVINF - Earth orientation covariances
! END_LPARM - Number of earth orientation parameters
! LPARM - Cross index of eop parameter numbers
! WHO - Array of eop parameter names
! F_RATE - logical flag. .TRUE., if eop-rate is constrainted;
!                        .FALSE. if eop values are constrainted.
!
! 2.3 OUTPUT Variables:
!
! A - Modified normal equations matrix
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: do_eop
!       CALLED SUBROUTINES: none
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 I, J
      INTEGER*2 IER  
      INTEGER*8 XPPOS, XYPOS, XUPOS, YPPOS, YUPOS, UTPOS, POS(6), POS_1(6), POS_2(6)
      DATA XPPOS/0/, XYPOS/0/, XUPOS/0/, YPPOS/0/, YUPOS/0/, UTPOS/0/, &
     &     POS/6*0/
      INTEGER*8, EXTERNAL :: INDX8
!CC
      INCLUDE   'cnstr.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
!     Transformation INT2 --> INT4
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   pet  970117  Added support of B3D parametrization
!   pet  970226  Added support of B1B3D parametrization
!   pet  970610  Added support of new EOP parametrization
!   pet  980119  Rewrote to support CNSTROBJ data structure.
!   pet  980205  Added writing information about the type of applied constraint.
!   pet  2002.09.26  Significantly re-wrote. Changed the logic: the new version
!                    puts equations of constraints into the object CNSTROBJ,
!                    while the old one put normal equations of constraints
!
! 5.  ADD_EOP PROGRAM STRUCTURE
!
!   note that eop parms are in a specific order in the list, IFthey are
!   are in the list at all: x-wobble, y-wobble, ut1-tai.
!   positions of elements in the list
!
!
! --- Forming array POS which will keep addresses of the elements of normal
! --- matrix for X-wobble, Y-wobble, UT1 (if they are estimeted). This part
! --- of code "doesn't know" whether all 3 parameters are estimated. It does
! --- as if all of them are being estimated
!
      DO I=1,END_LPARM
         DO J=1,I
            POS  ( INDX8(I,J) ) = INDX8( LPARM(I), LPARM(J) )
            POS_1( INDX8(I,J) ) = LPARM(I)
            POS_2( INDX8(I,J) ) = LPARM(J)
         ENDDO
      ENDDO
!
! --- Who is in the list:  use is made of the fact that earth orientation
! --- parameters occur in a specific order:  x-wobble, y-wobble, ut1-tai.
!
! --- This part of code solves the problem when at least one of all three
! --- parameters is not estimated. And it assigns specific addresses for them
!
      IF ( INDEX ( WHO(1),'X WOBBLE') .NE. 0  .OR. &
     &     INDEX ( WHO(1),'X WGRate') .NE. 0       ) THEN
           XPPOS=1
           IF ( INDEX ( WHO(2), 'Y WOBBLE' ) .NE. 0  .OR. &
     &          INDEX ( WHO(2), 'Y WGRate' ) .NE. 0       ) THEN
                XYPOS=2
                YPPOS=3
                IF ( INDEX ( WHO(3), 'UT1'     ) .NE. 0  .OR. &
     &               INDEX ( WHO(3), 'UT1GRate') .NE. 0       ) THEN
                     XUPOS=4
                     YUPOS=5
                     UTPOS=6
                ENDIF
              ELSE IF ( INDEX ( WHO(2), 'UT1'      ) .NE. 0  .OR. &
     &                  INDEX ( WHO(2), 'UT1GRate' ) .NE. 0       ) THEN
                XUPOS=2
                UTPOS=3
           ENDIF
        ELSE IF ( INDEX ( WHO(1), 'Y WOBBLE' ) .NE. 0  .OR. &
     &            INDEX ( WHO(1), 'Y WGRate' ) .NE. 0       ) THEN
           YPPOS=1
           IF ( INDEX ( WHO(2), 'UT1'      ) .NE. 0  .OR. &
     &          INDEX ( WHO(2), 'UT1GRate' ) .NE. 0       ) THEN
                YUPOS=2
                UTPOS=3
           ENDIF
        ELSE IF ( INDEX ( WHO(1), 'UT1'      ) .NE. 0  .OR. &
     &            INDEX ( WHO(1), 'UT1GRate' ) .NE. 0       ) THEN
           UTPOS=1
      ENDIF
!
! --- Now add in the appropriate constraints
!
      IF ( XPPOS .NE. 0  .AND.  DABS(CVINF(1)) .GT. 1.D-30  ) THEN
!
! -------- Add information about the type of the constraint applied
!
!
           IF ( F_RATE ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_NAM ( 'EOR_XPL', 1, 'Constraint on '// &
     &              'X pole rate', 'rad/day', 0.0D0, 1.D0/DSQRT(CVINF(1)), &
     &               FALSE_L4, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8531, IUER, 'ADD_EOP', 'Error '// &
     &                   'in defining EOR_XPL constraint' )
                     RETURN
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_EQU ( 'EOR_XPL', 1, LPARM(1), 1.0D0, &
     &               FALSE_L4, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8532, IUER, 'ADD_EOP', 'Error '// &
     &                   'in setting EOR_XPL constraint' )
                     RETURN
                END IF
              ELSE
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_NAM ( 'EOP_XPL', 1, 'Constraint on '// &
     &              'X pole value', 'rad', 0.0D0, 1.D0/DSQRT(CVINF(1)), &
     &               FALSE_L4, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8533, IUER, 'ADD_EOP', 'Error '// &
     &                   'in defining EOP_XPL constraint' )
                     RETURN
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_EQU ( 'EOP_XPL', 1, LPARM(1), 1.0D0, &
     &               FALSE_L4, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8534, IUER, 'ADD_EOP', 'Error '// &
     &                   'in setting EOP_XPL constraint' )
                     RETURN
                END IF
           END IF  ! f_rate
      END IF
!
      IF ( YPPOS .NE. 0  .AND.  DABS(CVINF(3)) .GT. 1.D-30  ) THEN
!
! -------- Add information about the type of the constraint applied
!
           IF ( F_RATE ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_NAM ( 'EOR_YPL', 1, 'Constraint on '// &
     &              'Y pole rate', 'rad/day', 0.0D0, 1.D0/DSQRT(CVINF(3)), &
     &               FALSE_L4, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8535, IUER, 'ADD_EOP', 'Error '// &
     &                   'in defining EOR_XPL constraint' )
                     RETURN
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_EQU ( 'EOR_YPL', 1, LPARM(2), 1.0D0, &
     &               FALSE_L4, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8536, IUER, 'ADD_EOP', 'Error '// &
     &                   'in defining EOR_XPL constraint' )
                     RETURN
                END IF
              ELSE
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_NAM ( 'EOP_YPL', 1, 'Constraint on '// &
     &              'Y pole value', 'rad', 0.0D0, 1.D0/DSQRT(CVINF(3)),FALSE_L4, &
     &               CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8537, IUER, 'ADD_EOP', 'Error '// &
     &                   'in defining EOR_YPL constraint' )
                     RETURN
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_EQU ( 'EOP_YPL', 1, LPARM(2), 1.0D0, &
     &               FALSE_L4, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8538, IUER, 'ADD_EOP', 'Error '// &
     &                   'in defining EOR_YPL constraint' )
                     RETURN
                END IF
           END IF
      END IF
!C
      IF ( UTPOS .NE. 0  .AND.  DABS(CVINF(6)) .GT. 1.D-30  ) THEN
!
! -------- Add information about the type of the constraint applied
!
           IF ( F_RATE ) THEN
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_NAM ( 'EOR_UT1', 1, 'Constraint on '//'UT1 rate', &
     &              'rad/day', 0.0D0, 1.D0/DSQRT(CVINF(6)), FALSE_L4, CNSTROBJ, &
     &               IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8539, IUER, 'ADD_EOP', 'Error '// &
     &                   'in defining EOR_UT1 constraint' )
                     RETURN
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_EQU ( 'EOR_UT1', 1, LPARM(3), 1.0D0, &
     &               FALSE_L4, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8540, IUER, 'ADD_EOP', 'Error '// &
     &                   'in setting EOR_UT1 constraint' )
                     RETURN
                END IF
              ELSE
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_NAM ( 'EOP_UT1', 1, 'Constraint on '//'UT1 angle', &
     &              'rad', 0.0D0, 1.D0/DSQRT(CVINF(6)), FALSE_L4, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8541, IUER, 'ADD_EOP', 'Error '// &
     &                   'in defining EOP_UT1 constraint' )
                     RETURN
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL ADDCNS_EQU ( 'EOP_UT1', 1, LPARM(3), 1.0D0, &
     &               FALSE_L4, CNSTROBJ, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8542, IUER, 'ADD_EOP', 'Error '// &
     &                   'in setting EOP_UT1 constraint' )
                     RETURN
                END IF
           END IF
      END IF
!
      IF ( XYPOS .NE. 0 ) THEN
           CALL GETENVAR ( 'NO_OFD_EOP_CONSTRAINTS', STR )
           IF ( STR(1:1) == 'Y'  .OR.  STR(1:1) == 'y' ) THEN
                XYPOS = 0
                XUPOS = 0
                YUPOS = 0
           END IF
      END IF
!
! --- Setting off-diagonal elements of weights matrix of constraints
!
!@      IF ( XYPOS .NE. 0 ) THEN
!@           CALL ERR_PASS ( IUER, IER )
!@           CALL ADD_OFD  ( 'EOP_XPL', 1, &
!@     &                     'EOP_YPL', 1, CVINF(2), FALSE_L4, &
!@     &                     CNSTROBJ, IER )
!@           IF ( IER .NE. 0 ) THEN
!@                CALL ERR_LOG ( 8543, IUER, 'ADD_EOP', 'Error '// &
!@     &              'in setting cross EOP_XPL/EOP_YPL constraint' )
!@                RETURN
!@           END IF
!@      END IF
!@!
!@      IF ( XUPOS .NE. 0  ) THEN
!@           CALL ERR_PASS ( IUER, IER )
!@           CALL ADD_OFD  ( 'EOP_XPL', 1, &
!@     &                     'EOP_UT1', 1, CVINF(4), FALSE_L4, &
!@     &                     CNSTROBJ, IER )
!@           IF ( IER .NE. 0 ) THEN
!@                CALL ERR_LOG ( 8544, IUER, 'ADD_EOP', 'Error '// &
!@     &              'in setting cross EOP_XPL/EOP_UT1 constraint' )
!@                RETURN
!@           END IF
!@      END IF
!@!
!@      IF ( YUPOS .NE. 0  ) THEN
!@           CALL ERR_PASS ( IUER, IER )
!@           CALL ADD_OFD  ( 'EOP_YPL', 1, &
!@     &                     'EOP_UT1', 1, CVINF(5), FALSE_L4, &
!@     &                     CNSTROBJ, IER )
!@           IF ( IER .NE. 0 ) THEN
!@                CALL ERR_LOG ( 8545, IUER, 'ADD_EOP', 'Error '// &
!@     &              'in setting cross EOP_YPL/EOP_UT1 constraint' )
!@                RETURN
!@           END IF
!@      END IF
!@!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ADD_EOP  #!#
