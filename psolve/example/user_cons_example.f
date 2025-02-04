      PROGRAM    USER_CONS_EXAMPLE
! ************************************************************************
! *                                                                      *
! *   Program  USER_CONS_EXAMPLE  is an example of the program which     *
! *   imposes NO-NET-TRANSLATION constraints on station positions.       *
! *                                                                      *
! *   If it is compiled and linked with FL_GLOBAL_MODE equal to .TRUE.   *
! *   then it will impose constraints on global parameters. In this case *
! *   the name of this program should be specified as a qualifier of     *
! *   the keyword USER_CONSTRAINTS.                                      *
! *                                                                      *
! *   If it is compiled and linked with FL_GLOBAL_MODE equal to .FALSE.  *
! *   then it will impose constraints on local parameters. In this case  *
! *   the name of this program should be specified as a qualifier of     *
! *   the keyword USER_PROGRAM.                                          *
! *                                                                      *
! * ### 25-SEP-2002 USER_CONS_EXAMPLE v1.0 (c) L. Petrov 25-SEP-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'socom.i'
      INCLUDE   'cnstr.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER  C_PAR(MAX_PAR)*20
      LOGICAL*2  FL_TRUE_L2
      PARAMETER  ( FL_TRUE_L2 = .TRUE. )
      LOGICAL*4  FL_GLOBAL_MODE              ! <--   Global ( .TRUE.  ) or
      PARAMETER  ( FL_GLOBAL_MODE = .TRUE. ) ! <--   Local  ( .FALSE. ) mode
      INTEGER*4  CNI_MODE
      INTEGER*2  NPARMS, INT2_ARG
      INTEGER*4  IUER
!
      CALL PRE_PROG()  ! Mandatory first statment for Solve programs
!
! --- Reading common areas
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_2 ( 'R'  )
      CALL USE_GLBFIL_4 ( 'RC' )
      CALL USE_PARFIL   ( 'ORC' )  ! Reading  prfil.i
      CALL USE_COMMON   ( 'ORC' )  ! Reading  socom.i
      CALL SOCOM_EXT()
!
      IF ( FL_GLOBAL_MODE ) THEN
!
! -------- In global mode we irriversibly re-arrange socom area to keep only
! -------- golobal parameters
!
           CALL DEPAR()
           CNI_MODE = CNI__UGL ! Mode: user constraints on global parameters
         ELSE
           CNI_MODE = CNI__ULC ! Mode: user constraints on local  parameters
      END IF
!
! --- Get the list of parameter names
!
      CALL GET_NAMES ( C_PAR, INT2(20), MAX_PAR, NPARMS, FL_TRUE_L2, &
     &                 FL_TRUE_L2 )
!
! --- Call a subroutine which will actuall make this work
!
      IUER = -1
      CALL UCNS_DO ( INT4(NPARMS), C_PAR, CNSTROBJ, CNI_MODE, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1511, IUER, 'USER_CONS_EXAMPLE', 'Failure to '// &
     &         'compute user consrtaints' )
           CALL EXIT ( 1 )
      END IF
!
      CALL END_PROG()  ! Mandatory last statment for Solve programs
      END  !#!  USER_CONS_EXAMPLE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UCNS_DO ( NPAR, C_PAR, CNSTROBJ, CNI_MODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  UCNS_DO  actually computes coefficents of No net          *
! *   translation constraints imposed in station positions and write     *
! *   them down in disk file.                                            *
! *                                                                      *
! *  ### 25-SEP-2002     UCNS_DO   v1.0 (c)  L. Petrov  25-SEP-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NPAR, CNI_MODE, IUER
      CHARACTER  C_PAR(NPAR)*20
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'socom.i'
      INCLUDE   'cnstr.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER  COMP(3)*1
      DATA       COMP / 'X', 'Y', 'Z' /
      CHARACTER  GET_DBNAME*10
      CHARACTER  OBJ_NAM*10
      LOGICAL*4  FL_GLOBAL
      INTEGER*4  J1, J2, IEQU, IER
      REAL*8     SIGMA
      PARAMETER  ( SIGMA = 1.D-4 )
!
      IF ( CNI_MODE .EQ. CNI__ULC ) THEN
           OBJ_NAM = GET_DBNAME ()
           FL_GLOBAL = .TRUE.
         ELSE
           OBJ_NAM = 'GLOBAL    '
           FL_GLOBAL = .FALSE.
      END IF
!
! --- Initialize object CNSTROBJ
!
      CALL ERR_PASS ( IUER, IER )
      CALL INIT_CNS ( CNI_MODE, OBJ_NAM, CNSTROBJ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1521, IUER, 'UCNS_DO', 'Failure in an attempt to '// &
     &         'initialize CNSTROBJ' )
           RETURN
      END IF
!
! --- Cycle over componetns: X, Y, Z
!
      DO 410 J1=1,3
         IEQU = 0
!
! ------ Scan all parameters and pick up station coordinates
!
         DO 420 J2=1,NPAR
            IF ( C_PAR(J2)(10:20) .EQ. COMP(J1)//' COMPONENT' ) THEN
                 IEQU = IEQU + 1
                 IF ( IEQU .EQ. 1 ) THEN
!
! ------------------- Insert information about constraint, name, description,
! ------------------- abreviation, right hand side, reciprocal weight (sigma),
! ------------------- type
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL ADDCNS_NAM ( 'U_NNTPOS', J1, 'User No-net '// &
     &                     'translation for pos.', 'meter', 0.0D0, SIGMA, &
     &                     FL_GLOBAL, CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 1522, IUER, 'UCNS_DO', 'Error in '// &
     &                         'an attempt to put information about U_NNTPOS '// &
     &                         'constraints into CNSTROBJ' )
                           RETURN
                      END IF
!
! ------------------- Tell that it is a user defined constraint
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL SET_USER_CNS ( 'U_NNTPOS', CNSTROBJ, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 1523, IUER, 'UCNS_DO', 'Error in '// &
     &                         'an attempt to tell that the U_NNTPOS is '// &
     &                         'a user constraints' )
                           RETURN
                      END IF
                 END IF
!
! -------------- Insert the coefficicent of constraint equation into
! -------------- the CNSTROBJ object
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL ADDCNS_EQU ( 'U_NNTPOS', J1, J2, 1.0D0, FL_GLOBAL, &
     &                             CNSTROBJ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 1524, IUER, 'UCNS_DO', 'Failure in '// &
     &                    'putting a coefficient of an equation of the '// &
     &                    'U_NNTPOS constraint' )
                      RETURN
                 END IF
            END IF
 420     CONTINUE
 410  CONTINUE
!
! --- Write down contents of CNSTROBJ object in disk file. This disk file will
! --- be later read by NORML
!
      CALL ERR_PASS    ( IUER, IER )
      CALL WRITE_CNSTR ( CNSTROBJ, CNI_MODE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1525, IUER, 'UCNS_DO', 'Failure to write the '// &
     &         'object CNSTROBJ with user constraints in a disk file' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  UCNS_DO  #!#
