      SUBROUTINE OPA_SUBMIT ( OPA, ACTION_CODE, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  OPA_SUBMIT  calls dclient for submittimng to the IVS Data *
! *   Center the data product.                                           *
! *   If ACTION_CODE is OPA__SBD then the pair of databases is to be     *
! *      submitted,                                                      *
! *   if ACTION_CODE is OPA__SBE then the EOPS file is to be submitted,  *
! *   if ACTION_CODE is OPA__SNX then the listing in Sinex format is     *
! *   to be submitted to IVS.                                            *
! *                                                                      *
! *   Dclient requres a user confirnation before proceeding the          *
! *   operation.                                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         OPA ( RECORD    ) -- Data structure which keeps internal     *
! *                              information of OPA: configuration       *
! *                              parameters, session name, status codes, *
! *                              action codes.                           *
! * ACTION_CODE ( INTEGER*4 ) -- code of the actions to be executed.     *
! *                              If action code is OPA__SBD then the     *
! *                              the pair of databases is to be          *
! *                              submitted, If the action code is        *
! *                              OPA__SBE then EOPS file will be         *
! *                              submitted.                              *
! *        IVRB ( INTEGER*4 ) -- Verbosity level. 0 means suppress all   *
! *                              information messages except error       *
! *                              messages.                               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 22-SEP-2000  OPA_SUBMIT   v1.6 (c)  L. Petrov  18-DEC-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'opa.i'
      INCLUDE    'precm.i'
      TYPE ( OPA__STRU ) ::  OPA
      INTEGER*4  IVRB, ACTION_CODE, IUER
!
      CHARACTER  COMSTR*256, LINKSTR*256, ORIG_SNX_FILE*256, ALT_SNX_FILE*256, &
     &           CONF_OPT*3, DB_NAME*9, CENTURY*2, STR*32
      LOGICAL*4  LEX
      INTEGER*4  IS, IYEAR, ISIG, ICOD, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, SYSTEM, UNLINK
!
! --- Build the line for dclient
!
      CALL CLRCH (  COMSTR )
      CALL CLRCH ( LINKSTR )
!
! --- Set confirm option.
!
      IF ( OPA%FL_CONFIRM ) THEN
           CONF_OPT = '   '  ! dclient will ask confirmation of the operation
         ELSE
           CONF_OPT = ' -q' ! Dclient will work wihtout confirmation request
      END IF
      IF ( ACTION_CODE .EQ. OPA__SBD ) THEN
!
! -------- Submitting a pair of databases (X- and S- band)
!
           COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'dclient '//CONF_OPT//' '// &
     &              '-t DBH -c '//OPA%DBS_CONF(1:I_LEN(OPA%DBS_CONF))//' '// &
     &              '-f '//OPA%DB_NAME
        ELSE IF ( ACTION_CODE .EQ. OPA__SBE             .AND. &
     &            OPA%SESSION_TYPE(1:7) .EQ. 'DIURNAL'        ) THEN
!
! -------- Submitting EOPS series from 24-hour VLBI experiments
!
           COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'dclient '//CONF_OPT//' '// &
     &              '-t EOPS -c '//OPA%EOS_CONF(1:I_LEN(OPA%EOS_CONF))//' '// &
     &              '-f '//OPA%EOPS_FILE
        ELSE IF ( ACTION_CODE .EQ. OPA__SBE               .AND. &
     &            OPA%SESSION_TYPE(1:9) .EQ. 'INTENSIVE'        ) THEN
!
! -------- Submitting EOPI series from Intensive VLBI experiments
!
           COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'dclient '//CONF_OPT//' '// &
     &              '-t EOPI -c '//OPA%EOS_CONF(1:I_LEN(OPA%EOS_CONF))//' '// &
     &              '-f '//OPA%EOPS_FILE
        ELSE IF ( ACTION_CODE .EQ. OPA__SBM               .AND. &
     &            OPA%SESSION_TYPE(1:9) .EQ. 'INTENSIVE'        ) THEN
!
! -------- Submitting EOPM series from Intensive VLBI experiments
!
           COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'dclient '//CONF_OPT//' '// &
     &              '-t EOPI -c '//OPA%EOM_CONF(1:I_LEN(OPA%EOM_CONF))//' '// &
     &              '-f '//OPA%EOPM_FILE
        ELSE IF ( ACTION_CODE .EQ. OPA__SNX ) THEN
!
           CALL CHIN ( OPA%DB_NAME(1:2), IYEAR )
           IF ( IYEAR .GT. 70 ) THEN
                CENTURY = '19'
              ELSE
                CENTURY = '20'
           END IF
           CALL CLRCH ( DB_NAME )
           DB_NAME = OPA%DB_NAME
           IF ( DB_NAME(9:9) .EQ. ' ' ) DB_NAME(9:9) = '_'
!
           ORIG_SNX_FILE = OPA%SESSION_DIR(1:I_LEN(OPA%SESSION_DIR))//CENTURY// &
     &                     OPA%DB_NAME(1:2)//'/'// &
     &                     OPA%SESS_CODE(1:I_LEN(OPA%SESS_CODE))//'/'// &
     &                     OPA%SESS_CODE(1:I_LEN(OPA%SESS_CODE))//'.snx'
           ALT_SNX_FILE = OPA%SESSION_DIR(1:I_LEN(OPA%SESSION_DIR))//CENTURY// &
     &                    OPA%DB_NAME(1:2)//'/'// &
     &                    OPA%SESS_CODE(1:I_LEN(OPA%SESS_CODE))//'/'// &
     &                    DB_NAME//'_'// &
     &                    OPA%STANDALONE_ID(1:I_LEN(OPA%STANDALONE_ID))//'.snx'
!
           INQUIRE ( FILE=ORIG_SNX_FILE, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 5361, IUER, 'OPA_SUBMIT', 'Cannot submit '// &
     &              'Sinex file '//ORIG_SNX_FILE(1:I_LEN(ORIG_SNX_FILE))// &
     &              ' because it does not exist' )
                RETURN
           END IF
!
           INQUIRE ( FILE=ALT_SNX_FILE, EXIST=LEX )
           IF ( LEX ) THEN
                IS = UNLINK ( ALT_SNX_FILE(1:I_LEN(ALT_SNX_FILE))//CHAR(0) )
                IF ( IS .NE. 0 ) THEN
                     CALL GERROR ( STR )
                     CALL ERR_LOG ( 5362, IUER, 'OPA_SUBMIT', 'Error '// &
     &                    STR(1:I_LEN(STR))//' in an attempt to remove the '// &
     &                   'old Sinex listing '//ALT_SNX_FILE )
                     RETURN
                END IF
           END IF
!
           LINKSTR = 'ln -s '//ORIG_SNX_FILE(1:I_LEN(ORIG_SNX_FILE))//' '// &
     &                         ALT_SNX_FILE(1:I_LEN(ALT_SNX_FILE))
!
           IF ( OPA%SESSION_TYPE(1:9) .EQ. 'INTENSIVE' ) THEN
                COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'dclient '//CONF_OPT//' '// &
     &                  '-t ISNX -c '//OPA%SNX_CONF(1:I_LEN(OPA%SNX_CONF))//&
     &                  ' -f '//ALT_SNX_FILE
              ELSE 
                COMSTR = PRE_SOL_DIR(1:PRE_SOL_LEN)//'dclient '//CONF_OPT//' '// &
     &                  '-t DSNX -c '//OPA%SNX_CONF(1:I_LEN(OPA%SNX_CONF))//&
     &                  ' -f '//ALT_SNX_FILE
           END IF
        ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( ACTION_CODE, STR )
           CALL ERR_LOG ( 5363, IUER, 'OPA_SUBMIT', 'Wrong action code '// &
     &                    STR(1:I_LEN(STR))//' as supplied' )
           RETURN
      END IF
!
      IF ( ILEN(LINKSTR) .GT. 0 ) THEN
           IS = SYSTEM ( LINKSTR(1:I_LEN(LINKSTR))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_PASS ( 0, IER )
                CALL ERR_LOG ( 5364, IER, 'OPA_SUBMIT', 'Error in an '// &
     &              'attempt to create a soft link by a command '// &
     &               LINKSTR(1:I_LEN(LINKSTR))//' nevertheless continue' )
                RETURN
           END IF
      END IF
!
! --- Launch command and wait
!
      IS = SYSTEM ( COMSTR(1:I_LEN(COMSTR))//CHAR(0) )
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
           IF ( ICOD .EQ. -64 ) THEN
!
! ------------- Request for submitting IVDS product has been cancelled by user
! ------------- during negotiating process
!
                CALL ERR_PASS ( OPA__CAN, IUER )
                RETURN
              ELSE
                CALL CLRCH ( STR )
                CALL INCH  ( ICOD, STR )
                WRITE ( 6, * ) 'OPA_SUBMIT: system IS = ', IS
                CALL ERR_LOG ( 5365, IUER, 'OPA_SUBMIT', 'Error '// &
     &                         STR(1:I_LEN(STR))//' in executing command '// &
     &                         COMSTR )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  OPA_SUBMIT  #!#
