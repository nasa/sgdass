      SUBROUTINE DO_PWC ( WHO_STA, IPARM, IPARMS, IWDS, CNSTROBJ, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DO_PWC PROGRAM SPECIFICATION
!
! 1.1 Apply continuous station position constraints.
!
! 1.2 REFERENCES:
!
! 2.  DO_PWC INTERFACE
!
! 2.1 Parameter File
      INCLUDE  'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4   IUER
      INTEGER*2   IWDS, IPARMS, IPARM(IWDS,M_GPA)
      CHARACTER*8 WHO_STA(*)
!
! A - The normal equation matrix
! IPARM - Array of parameter names
! IPARMS - Number of parameters
! IWDS - Length allowed for each parameter name
! WHO_STA - Array of station names
!
! 2.3 OUTPUT Variables:
!
! A - Modified normal equations matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE  'socom.i'
      INCLUDE  'glbcm.i'
      INCLUDE  'glbc4.i'
      INCLUDE  'prfil.i'
      INCLUDE  'cnstr.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: cnstr
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  I, END_APARM, APARM(M_GPA)
      CHARACTER  DUMRA*20
      REAL*8     SIGMA
      CHARACTER  PARNAM(3)*3
      DATA       PARNAM / 'STX', 'STY', 'STZ' /
      LOGICAL*4   TRUE_L4
      PARAMETER ( TRUE_L4 = .TRUE. )
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
      INTEGER*4   ICNS, IER, J1, J2
!
! 4.  HISTORY
!   MWH  920512  Created, based on do_atm
!   pet  980206  Write down information about the type of applied constraint.
!   pet  2002.09.25   Completely re-wrote. Changed internal logic: the
!                     new version puts equations of constraitns in CNSTROBJ,
!                     while the old version modified normal matrix directly.
!
! 5.  DO_PWC PROGRAM STRUCTURE
!
!   Set up the constraint:  it is hard-wired here . . .
!
      ICNS = 0
      DO I=1,NUMSTA  ! Cycle over stations
         IF ( PSITED(I) .NE. 0  .AND.  PWCCNST .NE. 0 ) THEN
!
! ----------- Aga! Position of this station was parameterized by linear spline
!
              SIGMA = PWCCNST/1000.D0 * PWC_INTRVL
!
! ----------- Add information about the type of the constraint applied
!
              DO 410 J1=1,3  ! Cycle over componentd
                 CALL INDEX_PARM ( PARNAM(J1), APARM, IPARM, IPARMS, IWDS, &
     &                END_APARM, DUMRA, FALSE__L2, WHO_STA(I) )
                 IF ( END_APARM .GT. 0 ) THEN
                      DO 420 J2=2,END_APARM
                         ICNS = ICNS + 1
!
! ---------------------- Put  definition of the constraint equation
!
                         CALL ERR_PASS ( IUER, IER )
                         CALL ADDCNS_NAM ( 'STA_PWC', ICNS, 'Station '// &
     &                       'velocity between segments', 'meter/year', 0.0D0, &
     &                        SIGMA, TRUE_L4, CNSTROBJ, IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8731, IUER, 'DO_PWC', 'Error in '// &
     &                            'storing information about station '// &
     &                            'velocity constraints betwen segments' )
                              RETURN
                         END IF
!
! ---------------------- Put coefficients of the equation of constraint
!
                         CALL ERR_PASS   ( IUER, IER )
                         CALL ADDCNS_EQU ( 'STA_PWC', ICNS, APARM(J2-1), &
     &                                     -12.D0, TRUE_L4, CNSTROBJ, &
     &                                      IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8732, IUER, 'DO_PWC', 'Error in '// &
     &                            'an attempt to store station velocity '// &
     &                            'constraints between segments' )
                              RETURN
                         END IF
!
! ---------------------- Put coefficients of the equation of constraint
!
                         CALL ERR_PASS   ( IUER, IER )
                         CALL ADDCNS_EQU ( 'STA_PWC', ICNS, APARM(J2), &
     &                                     12.0D0, TRUE_L4, CNSTROBJ, IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8733, IUER, 'DO_PWC', 'Error in '// &
     &                            'an attempt to store station velocity '// &
     &                            'constraints between segments' )
                              RETURN
                         END IF
 420                  CONTINUE
                 END IF
 410          CONTINUE
         ENDIF
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DO_PWC  #!#
