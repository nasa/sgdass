      SUBROUTINE CREATE_OPC ( OPC_FILE, DB_NAME, SESS_CODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CREATE_OPC  creates operational analysis control file     *
! *   OPC_FILE and initializes it.                                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  OPC_FILE ( CHARACTER ) -- Operational analysis control file.        *
! *   DB_NAME ( CHARACTER ) -- Database name (without leading dollar     *
! *                            sign )                                    *
! * SESS_CODE ( CHARACTER ) -- Session code as defined in master file.   *
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
! *  ### 14-AUG-2000   CREATE_OPC  v1.3 (c)  L. Petrov  24-SEP-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'opa.i'
      CHARACTER  OPC_FILE*(*), DB_NAME*(*), SESS_CODE*(*)
      CHARACTER  GET_CDATE*19
      INTEGER*4  IUER
      INTEGER*4  IOS, LUN
      CHARACTER  STR*80
      INTEGER*4  I_LEN, GET_UNIT
!
! --- Create operational analysis cpontrol file
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=OPC_FILE, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5141, IUER, 'CREATE_OPC', 'Fortran Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to create file '//OPC_FILE )
           RETURN
      END IF
!
      WRITE ( LUN, '(A)' )  OPC__LABEL//' Created on '//GET_CDATE()
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) 'Experiment:            '// &
     &                                             SESS_CODE(1:I_LEN(SESS_CODE))
      WRITE ( LUN, '(A)' ) 'Database:              '//DB_NAME(1:I_LEN(DB_NAME))
      WRITE ( LUN, '(A)' ) 'Arc_line:              '//'! '// &
     &                                             SESS_CODE(1:I_LEN(SESS_CODE))
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) 'Global_arclist_update: '//'? ?'
      WRITE ( LUN, '(A)' ) 'Superfile:             '//'? ?'
      WRITE ( LUN, '(A)' ) 'Site_weights:          '//'? ?'
      WRITE ( LUN, '(A)' ) 'Baseline_weights:      '//'? ?'
      WRITE ( LUN, '(A)' ) 'EOPS_solution:         '//'? ?'
      WRITE ( LUN, '(A)' ) 'Multi-EOP_solution:    '//'? ?'
      WRITE ( LUN, '(A)' ) 'Standalone_solution:   '//'? ?'
      WRITE ( LUN, '(A)' ) 'EOPK_series:           '//'? ?'
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) 'Database_submission:   '//'? ?'
      WRITE ( LUN, '(A)' ) 'EOPS_submission:       '//'? ?'
      WRITE ( LUN, '(A)' ) 'Multi-EOP_submission:  '//'? ?'
      WRITE ( LUN, '(A)' ) 'SINEX_submission:      '//'? ?'
      WRITE ( LUN, '(A)' ) '#'
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  CREATE_OPC  #!#
