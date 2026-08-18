      PROGRAM  HAUSR
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  HAUSR PROGRAM SPECIFICATION
!
! 1.1 Hausr sets up the Solution Archive (SAR) File.
!
! 1.2 REFERENCES:
!
! 2.  HAUSR INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc2.i'
      INCLUDE 'hausr.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: cumuloop, outfl, setup, outwithitall
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4   IOS, NRECORDS
      INTEGER*2   DESCRIP(5),  ICNTRL
      LOGICAL*2   KBIT
      INTEGER*4   I4P0, I4P1, I4P2, I4P60
      DATA        I4P0, I4P1, I4P2, I4P60 / 0, 1, 2, 60 /
      CHARACTER   FILE_DESCRIPTOR*30, SPL*1, PRNT*1, BUFSTR*80
      EQUIVALENCE (DESCRIP(1), FILE_DESCRIPTOR)
      CHARACTER   STR*54, GET_VERSION*54
      INTEGER*2   INT2_ARG
      INTEGER*4   I_LEN
!
! 4.  HISTORY
!  WHO  WHEN       WHAT
!  pet  03-APR-99  Added comments. Removed call of setup
!  pet  2021.01.12 Increased dimensiuon for integer arrays SOURCES and BASELINES
!
! 5.  HAUSR PROGRAM STRUCTURE
!
      CALL PRE_PROG()
      INCLUDE 'hausr_version.i' ! Set revision date of the current version
!
      ICNTRL  = 1
!
! --- A note on ICNTRL:  0 => HAUSR is being run interactively.  Hence,
! --- it is assumed that the archive file has been processed, and the
! --- statistics are available as record type 69 somewhere at end of file.
! --- ICNTRL .ne. 0 => run in (batch) mode in which statistics are NOT in
! --- the file. The common GLBFxx must be available, as statistics
! --- information is accumulated there.
! ---
! --- If HAUSR bombs without finishing the processing of the archive
! --- file, it may be restarted ( immediately, so as not to change the
! --- GLBFxx file), with ICNTRL = 1.  HAUSR will not reprocess records
! --- previously processed:  a flag(NABF(20)) is set to indicate that
! --- processing has occurred on a record.
!
      IF ( ICNTRL .EQ. 0 ) THEN
!
! -------- Clear the screen
!
           CALL START_MN()
           STR = GET_VERSION()
           CALL SETCR_MN ( 79-I_LEN(STR), 0 )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
           CALL REVERSE_OFF_MN()
!
! -------- Determine what to do
!
           CALL NL_MN()
           CALL ADDSTR_F ( "  Do you want printer output? " )
           CALL GETSTR_F ( BUFSTR )
           READ   ( BUFSTR, 2202 ) PRNT
 2202      FORMAT ( A1 )
           CALL CASEFOLD(PRNT )
           IF ( PRNT .EQ. 'N' ) THEN
                IPRNT = 1
           END IF
!
           CALL NL_MN()
           CALL ADDSTR_F ( "  Write to the temp file? " )
           CALL GETSTR_F ( BUFSTR )
           READ ( BUFSTR, 2204 ) SPL
 2204      FORMAT (A1)
           CALL CASEFOLD(SPL )
           IF ( SPL .EQ. 'Y' ) THEN
                OPEN ( 23, FILE=PRE_SCR_DIR(1:PRE_SD_LEN)//'TEMP'//PRE_LETRS, &
     &                 IOSTAT=IOS )
                CALL FERR ( INT2(IOS), "Opening temp file", INT2(0), INT2(0) )
                WRITE ( BUFSTR, 1105 ) PRE_SCR_DIR(1:PRE_SD_LEN)//'TEMP'// &
     &                               PRE_LETRS
 1105           FORMAT (2X, 'Statistics in file ',A)
                CALL ADDSTR_F ( BUFSTR )
                CALL NL_MN()
          END IF
          CALL END_MN()
      END IF
!
! --- Call routines which control processing of SARFxx information
!
      CALL OUTFL_HAUSR ( 'O' )
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_2 ( 'R'  )
      CALL USE_GLBFIL_4 ( 'RC' )
!
      CALL CUMULOOP (ICNTRL )
!
      CALL OUTWITHITALL()
!
      CALL OUTFL_HAUSR ( 'C' )
!
      IF ( KBIT ( PRE_IBATCH, INT2(9)) .OR. I_ARCNAME(1:3).EQ.'CGM' ) THEN
!
! -------- Schedule program corel if we want to write correlation file
!
           CALL RUN_PROG ( 'COREL', 'WAIT', INT2(0) )
      ENDIF
!
! --- Close and end
!
      CALL END_PROG()
      END  !#!  HAUSR  #!#
