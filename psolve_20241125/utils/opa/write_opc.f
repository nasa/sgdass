      SUBROUTINE WRITE_OPC ( OPC_FILE, OPA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WRITE_OPC  writes operational analysis control file into  *
! *   OPC_FILE in according to the current values of the datastructure   *
! *   OPA.                                                               *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  OPC_FILE ( CHARACTER ) -- Operational analysis control file.        *
! *       OPA ( RECORD    ) -- Data structure which keeps settings       *
! *                            of OPA and current information related    *
! *                            to processing this session.               *
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
! *  ### 15-AUG-2000   WRITE_OPC   v1.6 (c)  L. Petrov  12-SEP-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'opa.i'
      CHARACTER  OPC_FILE*(*)
      TYPE ( OPA__STRU ) ::  OPA
      INTEGER*4  IUER
      CHARACTER  STR*80, GET_CDATE*19, LINE_STSACT*3
      INTEGER*4  IOS, LUN
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN
!
! --- Open file
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=OPC_FILE, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( IOS, STR )
           CALL ERR_LOG ( 5171, IUER, 'WRITE_OPC', 'Fortran Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to create file '//OPC_FILE )
           RETURN
      END IF
!
! --- Write it
!
      WRITE ( LUN, '(A)' )  OPC__LABEL//' Updated on '//GET_CDATE()
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) 'Experiment:            '// &
     &                     OPA%SESS_CODE(1:I_LEN(OPA%SESS_CODE))
      WRITE ( LUN, '(A)' ) 'Database:              '// &
     &                     OPA%DB_NAME(1:I_LEN(OPA%DB_NAME))
      WRITE ( LUN, '(A)' ) 'Arc_line:              '// &
     &                     OPA%ARC_LINE(1:I_LEN(OPA%ARC_LINE))
!
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) 'Global_arclist_update: '// &
     &                     LINE_STSACT ( OPA%STS(OPA__GAL),  OPA%ACT(OPA__GAL) )
      WRITE ( LUN, '(A)' ) 'Superfile:             '// &
     &                     LINE_STSACT ( OPA%STS(OPA__SUP),  OPA%ACT(OPA__SUP) )
      WRITE ( LUN, '(A)' ) 'Site_weights:          '// &
     &                     LINE_STSACT ( OPA%STS(OPA__STW),  OPA%ACT(OPA__STW) )
      WRITE ( LUN, '(A)' ) 'Baseline_weights:      '// &
     &                     LINE_STSACT ( OPA%STS(OPA__BAW),  OPA%ACT(OPA__BAW) )
      WRITE ( LUN, '(A)' ) 'EOP_solution:          '// &
     &                     LINE_STSACT ( OPA%STS(OPA__EOS),  OPA%ACT(OPA__EOS) )
      WRITE ( LUN, '(A)' ) 'Multi-EOP_solution:    '// &
     &                     LINE_STSACT ( OPA%STS(OPA__EOM),  OPA%ACT(OPA__EOM) )
      WRITE ( LUN, '(A)' ) 'Standalone_solution:   '// &
     &                     LINE_STSACT ( OPA%STS(OPA__STN),  OPA%ACT(OPA__STN) )
      WRITE ( LUN, '(A)' ) 'EOPK_series:           '// &
     &                     LINE_STSACT ( OPA%STS(OPA__EOK),  OPA%ACT(OPA__EOK) )
      WRITE ( LUN, '(A)' ) 'SNR_analysis:          '// &
     &                     LINE_STSACT ( OPA%STS(OPA__SNR),  OPA%ACT(OPA__SNR) )
      WRITE ( LUN, '(A)' ) 'VDB_update:            '// &
     &                     LINE_STSACT ( OPA%STS(OPA__VDB),  OPA%ACT(OPA__VDB) )
!
      WRITE ( LUN, '(A)' ) '#'
      WRITE ( LUN, '(A)' ) 'Database_submission:   '// &
     &                     LINE_STSACT ( OPA%STS(OPA__SBD),  OPA%ACT(OPA__SBD) )
      WRITE ( LUN, '(A)' ) 'EOP_submission:        '// &
     &                     LINE_STSACT ( OPA%STS(OPA__SBE),  OPA%ACT(OPA__SBE) )
      WRITE ( LUN, '(A)' ) 'Multi-EOP_submission:  '// &
     &                     LINE_STSACT ( OPA%STS(OPA__SBM),  OPA%ACT(OPA__SBM) )
      WRITE ( LUN, '(A)' ) 'SINEX_submission:      '// &
     &                     LINE_STSACT ( OPA%STS(OPA__SNX),  OPA%ACT(OPA__SNX) )
      WRITE ( LUN, '(A)' ) '#'
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRITE_OPC #!#
