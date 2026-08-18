      SUBROUTINE DO_GRADR ( FAST_MODE, WHO_STA, LPARM, IPARMS, CNSTROBJ, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DO_GRADR PROGRAM SPECIFICATION
!
! 1.1 Apply gradient  constraints. dsm 2/14/94
!
! 1.2 REFERENCES:
!
! 2.  DO_GRADR INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IPARMS
      CHARACTER LPARM(M_GPA)*20, WHO_STA(*)*8
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
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES: add_gradr
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4    IUER, IER
      INTEGER*4    J1, END_APARM, I, J, APARM(M_GPA), KOFF_GRA, KRAT_GRA, KSTA
      LOGICAL*2    LDUM
      CHARACTER    PARMTYP*3, DUMRA*20
      REAL*8       GRADI
      INTEGER*2    INT2_ARG
      INTEGER*4    INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4,  EXTERNAL :: CHECK_STABIT
!
! 4.  HISTORY
!   MWH  910524  Include interval size in calculation of constraint
!   jmg  960610  Remove holleriths.
!   pet  970117  Added support of B3D parametrization
!   pet  970226  Added support of B1B3D parametrization
!   pet  971203  Added logic for bypassing deselected station
!   pet  980119  Support of CNSTROBJ data structure added.
!   pet  980205  Add writing information about the type of applied constraint.
!   pet  2001.07.03  Fix a bug: the previous version crashed in attempt to
!                    impose gradients constraints in the case when no
!                    atmosphere gradient are estimated. The new version doesn't
!                    try to do this foolish business.
!   pet  2002.09.17  Changed internal logic: the new version puts equations of
!                    constraitns in CNSTROBJ, while the old version put normal
!                    equations of constraints
!   pet  2005.08.15  Changed the logic: the pre2005 logic was to impose
!                    constraint on the gradient offset of only the first
!                    node of linear spline. The post2005 logic is impose 
!                    constraints on all nodes of the spline. Tjhe pre2005
!                    logic can be invoked if environment variable GRAD_PRE2005
!                    is set to YES
!
! 5.  DO_GRADR PROGRAM STRUCTURE
!
!   Set up the constraint:  it is hard-wired here . . .
!
      INCLUDE   'cnstr.i'
      INCLUDE   'fast.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      LOGICAL*4    FALSE_L4
      PARAMETER  ( FALSE_L4 = .FALSE. )
      CHARACTER  STR*128
      LOGICAL*4  FL_GRAD_PRE2005
      INTEGER*4  FAST_MODE
!
      KOFF_GRA = 0
      KRAT_GRA = 0
      KSTA = 0
      CALL GETENVAR ( 'GRAD_PRE2005', STR )
      CALL TRAN ( 11, STR,  STR )
      IF ( STR == 'YES' ) THEN
           FL_GRAD_PRE2005 = .TRUE.
         ELSE 
           FL_GRAD_PRE2005 = .FALSE.
      END IF
      DO I=1,NUMSTA
!
! ------ Check: was the I-th station in solution?
!
!@  write ( 6, * ) ' do_gradr: cons: ', gradcons ! %%%%%%%%%%%%%%%%%%%
!@  write ( 6, * ) ' do_gradr: int:  ', grad_interval ! %%%%%%%%%%%%%%%%
         IF ( NUMGRAD(I) .GT. 0  .AND.  CHECK_STABIT ( I )  ) THEN
              DO J=1,2 ! Cycle over gradient offset and gradient rate
                 KSTA = KSTA+1
                 IF ( J .EQ. 1 ) THEN
                      PARMTYP = 'GRE'
                    ELSE IF ( J .EQ. 2 ) THEN
                      PARMTYP = 'GRN'
                 END IF
!
                 LDUM    = .FALSE.
                 CALL CINDEX_PARM ( PARMTYP, APARM, LPARM, IPARMS, END_APARM, &
     &                              DUMRA, LDUM, WHO_STA(I) )
!
! -------------- Add information about the type of the constraint applied
!
                 IF ( DABS(GRADCONS(1)) .GT. 1.D-30 ) THEN
                      KOFF_GRA = KOFF_GRA + 1
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_NAM ( 'GRAD_OFF', KOFF_GRA, &
     &                    'Atmosphere gradient offset', 'mm', 0.0D0, &
     &                     GRADCONS(1), FALSE_L4, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8581, IUER, 'DO_GRADR', &
     &                         'Error in an attempt to put information about '// &
     &                         'atmosphere gradients constraint' )
                           RETURN
                      END IF
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_EQU ( 'GRAD_OFF', KOFF_GRA, &
     &                                   APARM(1), 1.0D0, FALSE_L4, &
     &                                   CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8582, IUER, 'DO_GRADR', &
     &                         'Error in an attempt to put '// &
     &                         'atmosphere gradients constraint equation' )
                           RETURN
                      END IF
                 END IF
!
                 IF ( DABS(GRADCONS(2)) .GT. 1.D-30  .AND. &
     &                END_APARM .GT. 1 ) THEN
!
                      GRADI = GRAD_INTERVAL/24.D0  ! Number of gradient intervals/day
                      DO 410 J1=2,END_APARM
                         IF ( DABS(GRADCONS(1)) .GT. 1.D-30  .AND. &
     &                        .NOT. FL_GRAD_PRE2005                ) THEN
!
! --------------------------- NB: If environment variable GRAD_PRE2005 we 
! --------------------------- apply constraints on gradient offsets ONLY
! --------------------------- on the first node. It is rather a bug.
!
                              KOFF_GRA = KOFF_GRA + 1
                              CALL ERR_PASS ( IUER, IER )
                              CALL ADDCNS_NAM ( 'GRAD_OFF', KOFF_GRA, &
     &                            'Atmosphere gradient offset', 'mm', 0.0D0, &
     &                             GRADCONS(1), FALSE_L4, CNSTROBJ, IER )
                              IF ( IER .NE. 0 ) THEN
                                   CALL ERR_LOG ( 8582, IUER, 'DO_GRADR', &
     &                                 'Error in an attempt to put '// &
     &                                 'information about atmosphere '// &
     &                                 'gradients constraint' )
                                   RETURN
                              END IF
!
                              CALL ERR_PASS ( IUER, IER )
                              CALL ADDCNS_EQU ( 'GRAD_OFF', KOFF_GRA, &
     &                                          APARM(J1), 1.0D0, &
     &                                          FALSE_L4, CNSTROBJ, IER )
                              IF ( IER .NE. 0 ) THEN
                                   CALL ERR_LOG ( 8583, IUER, 'DO_GRADR', &
     &                                 'Error in an attempt to put '// &
     &                                 'atmosphere gradients constraint '// &
     &                                 'equation' )
                                   RETURN
                              END IF
                         END IF
!
                         KRAT_GRA = KRAT_GRA + 1
                         CALL ERR_PASS ( IUER, IER )
                         CALL ADDCNS_NAM ( 'GRAD_RAT', KRAT_GRA, &
     &                       'Atmosphere gradient rate', 'mm/day', 0.0D0, &
     &                        GRADCONS(2), FALSE_L4, CNSTROBJ, IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8584, IUER, 'DO_GRADR', &
     &                            'Error in an attempt to put information '// &
     &                            'about atmosphere rate gradients constraint' )
                              RETURN
                         END IF
!
!@  write ( 6, * ) 'do_gradi: gradi = ', gradi ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                         CALL ERR_PASS ( IUER, IER )
                         CALL ADDCNS_EQU ( 'GRAD_RAT', KRAT_GRA, &
     &                        APARM(J1), 1.0D0/GRADI, FALSE_L4, &
     &                        CNSTROBJ, IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8585, IUER, 'DO_GRADR', &
     &                            'Error in an attempt to put atmosphere '// &
     &                            'rate gradients constraint equation' )
                              RETURN
                         END IF
!
                         CALL ERR_PASS ( IUER, IER )
                         CALL ADDCNS_EQU ( 'GRAD_RAT', KRAT_GRA, &
     &                        APARM(J1-1), -1.0D0/GRADI, FALSE_L4, &
     &                        CNSTROBJ, IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8586, IUER, 'DO_GRADR', &
     &                            'Error in an attempt to put atmosphere '// &
     &                            'rate gradients constraint equations' )
                              RETURN
                         END IF
 410                  CONTINUE
                 END IF
              END DO
         END IF
      ENDDO
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   !#!  DO_GRADR  #!#
