      SUBROUTINE OPA_SUP ( SOLVE_INIT, OPA, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  OPA_SUP  executes the action of OPA: it creates        *
! *   a superfile. OPA_SUP calls program  liptn .                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * SOLVE_INIT ( CHARACTER ) -- Solve user initials.                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   OPA ( RECORD    ) -- Data structure which keeps internal           *
! *                        information of OPA: configuration parameters, *
! *                        session name, status codes, action codes.     *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 07-SEP-2000    OPA_SUP    v1.1 (c)  L. Petrov  19-SEP-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'opa.i'
      TYPE ( OPA__STRU ) ::  OPA
      CHARACTER  SOLVE_INIT*2
      INTEGER*4  IUER
!
      CHARACTER  COMSTR*256, LOCK_FINAM*128, STR*32
      INTEGER*4  IS, ISIG, ICOD, IER
      INTEGER*4  SYSTEM, UNLINK, I_LEN
      LOGICAL*4  LSUI, CHECK_SOLVE_INITIALS, CHECK_SOLVE_COMPLETE
!
! --- Check solve lock. 
!
      CALL CHECK_SOLVE_LOCK()
!
! --- Build command line for launching liptn
!
      COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'liptn '//SOLVE_INIT//' '// &
     &         '1 1 '//OPA%DB_NAME//' 0'
!
! --- Launch liptn
!
      IS = SYSTEM ( COMSTR(1:I_LEN(COMSTR))//CHAR(0) )
!
! --- Remove lock from the current Solve user initials
!
      CALL REMOVE_SOLVE_LOCK()
      IF ( IS .NE. 0 ) THEN
!
! -------- Completion code is not 0. Extract ISIG -- signal number which
! -------- caused termination of the command and ICOD -- completion code
!
           ISIG = 0
           ICOD = 0
           CALL MVBITS ( IS, 0, 8, ISIG, 0 )
           CALL MVBITS ( IS, 8, 8, ICOD, 0 )
           IF ( ICOD .GE. 128 ) ICOD = ICOD-256
!
           CALL CLRCH ( STR )
           CALL INCH  ( IS, STR )
           CALL ERR_LOG ( 4212, IUER, 'OPA_SUP', 'Error '//STR(1:I_LEN(STR))// &
     &         ' in executing command line '//COMSTR )
           RETURN
      END IF
!
! --- Block Solve user initials
!
      CALL ERR_PASS ( IUER, IER )
      LSUI = CHECK_SOLVE_INITIALS ( 'W', SOLVE_INIT(1:I_LEN(SOLVE_INIT)), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4213, -1, 'OPA_SUP', 'Trap of internal control: '// &
     &         'error in checking solve initials '//SOLVE_INIT )
           RETURN
      END IF
      IF ( .NOT. LSUI ) THEN
           CALL ERR_LOG ( 4214, -1, 'OPA_SUP', 'Trap of internal control: '// &
     &         'Solve initials '//SOLVE_INIT//' are in use' )
           RETURN
      END IF
!
! --- Check whether liptn completed successfully
!
      IF ( .NOT. CHECK_SOLVE_COMPLETE ( SOLVE_INIT ) ) THEN
           CALL ERR_LOG ( 4215, IUER, 'OPA_SUP', 'Attempt to make superfile '// &
     &         'for database '//OPA%DB_NAME//' was not successfull' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_SUP  #!#
