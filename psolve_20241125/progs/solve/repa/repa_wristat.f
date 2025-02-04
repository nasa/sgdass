      SUBROUTINE REPA_WRISTAT ( REP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine REPA_WRISTAT writes the current REPA status into the       *
! *   output file. The name of the output file is  REP%CNF%STAT_FILE.    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *        REP ( RECORD    ) -- Object which keeps internal parameters   *
! *                             for program REPA (REsiduals Plots and    *
! *                             Ambiguities).                            *
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
! * ### 01-DEC-2004  REPA_WRISTAT  v1.2 (c)  L. Petrov  01-JUN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'diagi.i'
      INCLUDE   'repa.i' 
      TYPE     ( REP__TYPE ) :: REP
      INTEGER*4  IUER
      INTEGER*4  LUN, IOS
      CHARACTER  STR*80
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, GET_UNIT
!
! --- Open the output file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=REP%CNF%STAT_FILE, IOSTAT=IOS, STATUS='UNKNOWN' )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 7841, IUER, 'REPA_WRISTAT', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open for writing '// &
     &         'REPA status file '//REP%CNF%STAT_FILE )
           RETURN 
      END IF
!
! --- Write there the status file label
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) REPA__STS_LABEL 
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 7842, IUER, 'REPA_WRISTAT', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt writing REPA label '// &
     &         'into the status file '//REP%CNF%STAT_FILE )
           RETURN 
      END IF
!
! --- Write the contents
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# REPA status file. Updated on '// &
     &                               GET_CDATE()
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) 'Db_Name:        '//REP%DBNAME_STR 
      WRITE ( UNIT=LUN, FMT='(A)' ) 'Baseline:       '//REP%CNF%BASELINE
      IF ( REP%CNF%BOU_IND .LE. 0 ) REP%CNF%BOU_IND = 1
      WRITE ( UNIT=LUN, FMT='(A)' ) 'Box_Plot:       '//REPA__NAME_BOX(REP%CNF%BOU_IND)
      WRITE ( UNIT=LUN, FMT='(A)' ) 'Argument:       '//REP__CH_ARG(REP%CNF%ARG_IND)
      WRITE ( UNIT=LUN, FMT='(A)' ) 'Value:          '//REP__CH_VAL(REP%CNF%VAL_IND)
      WRITE ( UNIT=LUN, FMT='(A)' ) 'Mode:           '//REP__CH_MOD(REP%CNF%MOD_IND)
      CALL CLRCH ( STR ) 
      CALL  INCH ( REP%CNF%PAGE, STR ) 
      WRITE ( UNIT=LUN, FMT='(A)' ) 'Page:           '//STR
      WRITE ( UNIT=LUN, FMT='(A)' ) 'Box_Symmetric:  '//REP%CNF%BOX_SYMMETRIC
      WRITE ( UNIT=LUN, FMT='(A)' ) 'Marked_Source:  '//REP%CNF%MARKED_SOURCE 
!
! --- Code the file
!
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  REPA_WRISTAT 
