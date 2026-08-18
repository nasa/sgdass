      SUBROUTINE BATCH()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  BATCH PROGRAM SPECIFICATION
!
! 1.1
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!  This program is the batch controller for SOLVE.
!  It reads a control file and runs pieces of SOLVE to do large solutions
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! 1.2 REFERENCES:
!
! 2.  BATCH INTERFACE
!
! 2.1 Parameter File
      INCLUDE   'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE   'socom.i'
      INCLUDE   'precm.i'
      INCLUDE   'batcm.i'
      INCLUDE   'ba2cm.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc2.i'
      INCLUDE   'glbp.i'
      INCLUDE   'bdata.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'fast.i'
      INCLUDE   'vcat.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: ctrls, prces
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4  FC_UNLINK
      ADDRESS__TYPE :: PTR_CH
      INTEGER*2  ERROR, IWARNING, TRIMLEN, IERR, I, NARCS_GET, &
     &           SUPVER_WEI(MAX4_WEIREC)
      LOGICAL*2  KEXIST
      CHARACTER  IONCTL*8, CGMOUT*64, FNAME*64, CMERG(10)*64, STR*54
      CHARACTER  BUF_WARNING*248, BUFFER_WARNING*255, EMESSAGE*255, &
     &           SUPNAM_WEI(MAX4_WEIREC)*10, BASELINE_WEI(MAX4_WEIREC)*16
      CHARACTER  FIL_PREF(4)*4, MODE_STR*32
      DATA       FIL_PREF /         &
     &                      'CNQL', &
     &                      'CNQG', &
     &                      'ULCC', &
     &                      'UGLC'  &
     &                    /
      CHARACTER  GET_VERSION*54
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM  ! defined in glbp.i
      TYPE ( VCAT__TYPE    ) ::  VCAT
      INTEGER*4  IOS, J1
      LOGICAL*4  TRUE_L4, FALSE_L4
      PARAMETER  ( TRUE_L4  = .TRUE.  )
      PARAMETER  ( FALSE_L4 = .FALSE. )
      REAL*8     ARR_WEI(4,MAX4_WEIREC)
      LOGICAL*4  LEX
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4, EXTERNAL :: I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE  910913  Added code to purge *.XPND control file.
!   AEE  920204  Removed hard coded path for fclib.i
!   KDB  951010  Update date in initial batch message, using sccsid.
!   kdb  951130  06/13/96 11:41:52 accidentally replaced with an actual sccs version.
!                Restore it.
!   kdb  961010  Trap error (suspected premature deletion of .XPND file).
!                Implement this as the start of a new feature to delay ferr
!                messages to the end of processing.
!   jmg  980604  Modify USRPxx file setup to make first line "0" on restart.
!   pet  980615  Increased the lenght of batch control file from  36 to 128
!                symbols. Increased the length of the buffer for communication
!                with enter from 72 to 128 bytes.
!   pet  990111  Added actual argument GLBMEM to ctrlfl and preces
!   pet  990118  Added a formal parameter for call of ctrls in order to
!                kep the total number of arcs
!   kdb  990305  Restore variable reference in finisol call (incgm_sol) that
!                was apparently accidentally removed during code formatting.
!   pet  1999.07.31.  Change the logic: forces BATCH to purge USRGxx and USRPxx
!                     files in all cases, regardless wre user partials applied
!                     or not.
!   pet  2000.10.16   Extended parameter list for call of finisol
!   pet  2000.10.30   Moved setting the final batch status from prcess to batch
!                     in order to prevent appearence of messages about
!                     successfull solve finish before all thigs are really done.
!   pet  2001.09.05   Added varaibles ARR_WEI, SUPNAM_WEI, SUPVER_WEI
!   pet  2001.12.13   Added support of a new type of solutions: GLOBAL_ONLY
!                     ( SOLTYP = 'G' )
!   pet  2002.09.25   Forced removal of stale files with constraint equations
!   pet  2003.08.20   The previous version considered warnings from &
!                     apnd_2_spool as fatal. The new version does not think
!                     they are fatal.
!   pet  2020.04.30   Split into batch and batch_main. Fixed PRE_IP(2) start/restart &
!                     logic.
!   pet  2020.06.08   Aadd defintion of vcat and passed it to ctrls
!
! 5.  BATCH PROGRAM STRUCTURE
!
! SYNOPSIS:
!
!  GET RUN STRING
!  OPEN FILE
!  READ FILE SET INTERNAL FLAGS FROM $SETUP, $FLAGS, $CARRY, $OUTPUT
!  REMEMBER LOCATION OF $ARCS
!  SET OVERALL SOLUTION PARAMETERS IN SOLVE SCRATCH FILES
!  DO WHILE MORE ARCS
!    GET ARC FROM SUPER FILES
!    SET FLAGS
!    RUN GLOBL
!
!CCCC
      INCLUDE 'batch_version.i' ! Set revision date of the current version
!
! --- Initializations
!
      IWARNING = 0
      BUF_WARNING = ' '
      BUFFER_WARNING = ' '
      DO I=1,10
         CMERG(I) = ' '
      ENDDO
      NARCS = 0
      VCAT%STATUS = VCAT__UNDF
      CALL ERR_MODE ( 'NO_PROBE' ) 
!
      CALL GETENVAR ( 'MEMORY_DEBUG', STR )
      IF ( STR(1:1) .EQ. 'Y' .OR. STR(1:1) .EQ. 'y' .OR. &
     &     STR(1:1) .EQ. 'O' .OR. STR(1:1) .EQ. 'o'      ) THEN
!
! -------- Setting status "memory debug"
!
           CALL DEBUG_MEM ( TRUE_L4  )
         ELSE
           CALL DEBUG_MEM ( FALSE_L4 )
      END IF
!
      CALL STATUS_SET ( '-BATCH', STA__INI )
      CALL SET_SIGNAL_CTRLC ( 2 )
      CALL TIM_INIT() ! initialize timer
!
! --- Parse the run string and process the control file
!
      CALL CLRCH ( STR )
      STR = GET_VERSION ()
      CALL CLRCH ( MODE_STR )
      IF ( KBIT ( PRE_IP(2), INT2(8) ) ) THEN
           MODE_STR = 'Non recovery mode'
         ELSE
           MODE_STR = 'Recovery mode'
      END IF
      WRITE ( 6, '("[", A ,"] ",A)' ) STR(1:I_LEN(STR)), &
     &                                MODE_STR(1:I_LEN(MODE_STR))
      CALL CTRLS ( IONCTL, CGMOUT, KUSER_PART, USER_PART_PROG, KGLOBONLY, &
     &             KUSER_CONST, USER_CONST_PROG, CMERG, NARCS_GET, GLBMEM, &
     &             VCAT )
!
      NARCS = NARCS_GET
      MERGCGM = CMERG(1)
      OUTCGM  = CGMOUT
      BATCH_CNF_FINAM = CFNAME_ORIGINAL
      CALL USE_GLBFIL   ( 'OW' )
      CALL USE_GLBFIL_4 ( 'WC' )
!
! --- Remove stale files with constraints, built-in and user, local and global.
! --- This files will be re-created if necessary, but the old versions may
! --- inflict harm
!
      DO 410 J1=1,4
         FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//FIL_PREF(J1)//PRE_LETRS
         INQUIRE ( FILE=FNAME, EXIST=LEX )
         IF ( LEX ) THEN
              CALL UNLINK ( FNAME(1:I_LEN(FNAME))//CHAR(0) )
         END IF
 410  CONTINUE
!
      IF ( KUSER_PART ) THEN
!
! -------- Put 0 in USRGxx file
!
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRG'//PRE_LETRS
           CALL BIN_EXIST ( FNAME, KEXIST )
!
           IF ( KEXIST                                         .AND. &
     &          .NOT. RESTRT                                   .AND. &
     &          ( SOLTYP .EQ. 'C'  .OR. SOLTYP .EQ. 'F' .OR. &
     &            SOLTYP .EQ. 'G'                            )      ) THEN
!
! ------------ Remove the old file USRGxx
!
               CALL BIN_UNLINK ( FNAME, IERR )
!
! ------------ Create a new one
!
               OPEN  ( 67, FILE=FNAME, IOSTAT=IOS )
               WRITE ( 67, * ) 0
               CLOSE(67)
             ELSE IF ( .NOT. KEXIST   .OR. SOLTYP .EQ. 'I' ) THEN
               OPEN  ( 67, FILE=FNAME, IOSTAT=IOS )
               WRITE ( 67, * ) 0
               CLOSE ( 67 )
             ELSE
               OPEN  ( 67, FILE=FNAME, IOSTAT=IOS )
               CALL FERR ( INT2(IOS), "BATCH(batch) Opening user partial "// &
     &                     "file USRGxx", INT2(0), INT2(0) )
               CLOSE ( 67 )
          ENDIF
        ELSE ! not kuser_part
!
! ------- Create a new USRGxx file and write down 0 there to initialize it
!
          FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRG'//PRE_LETRS
          OPEN  ( 67, FILE=FNAME, IOSTAT=IOS )
          WRITE ( 67, * ) 0
          CLOSE ( 67 )
!
! ------- Create a new USRPxx file and write down 0 there to initilize it
!
          FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRP'//PRE_LETRS
          OPEN  ( 67, FILE=FNAME, IOSTAT=IOS )
          WRITE ( 67, * ) 0
          CLOSE ( 67 )
      ENDIF
!
! --- Process each experiment listed in the control file
!
      CALL SHFEOP()
      FNAME = CGMNMR
      DO I=1,10
         IF ( CMERG(I) .NE. ' ' ) CALL ADD_CGM ( FNAME, CMERG(I) )
      ENDDO
!
! --- Processing the control file
!
      CALL PRCES ( IONCTL, IWARNING, BUF_WARNING, GLBMEM, SUPNAM_WEI, &
     &             SUPVER_WEI, BASELINE_WEI, ARR_WEI, VCAT )
!
! --- Check for problems which were found earlier, but delayed until a more
! --- opportune time putting the warning message
! --- Solve processing has been completed)
!
      IF ( IWARNING .NE. 0 ) THEN
           WRITE ( BUFFER_WARNING, "(I5,': ',A)" ) IWARNING, &
     &                               BUF_WARNING(1:TRIMLEN(BUF_WARNING))
           WRITE (  6, '(A)' ) BUFFER_WARNING(1:I_LEN(BUFFER_WARNING))
           WRITE ( 23, '(A)' ) BUFFER_WARNING(1:I_LEN(BUFFER_WARNING))
      ENDIF
!
! --- Looks okay, so continue. Then delete XPND-file
!
      IF ( INDEX ( CFNAME, '.XPND' ) .EQ. 0 ) THEN
           CFNAME = CFNAME(:TRIMLEN(CFNAME))//'.XPND'
      ENDIF
      ERROR = FC_UNLINK( PTR_CH(CFNAME(:TRIMLEN(CFNAME))//CHAR(0)) )
!
      IF ( SOLARCH_SOL ) THEN
           IF ( FAST_DBG .EQ. F__MON ) WRITE ( 6, FMT='(A)' ) ' '
#ifdef HPUX
           WRITE ( 6, '(A)' ) 'Solution is being written to archive ...'
!
! -------- Write solution in SOLARCH
!
           CALL FINISOL ( RUN_INITS, USER_TAG, SOL_TAG, IVER, SOLTYP, ID, &
     &                    ITEM_LUS, CFNAME_ORIGINAL, ARC_FILE_TYPE, &
     &                    ARC_FILE_PATH, ARC_FILE_KEY, ARC_FILE_VER, &
     &                    INCGM_TYPE, INCGM_USER, INCGM_SOL, EMESSAGE, IOS )
           IF ( IOS .NE. 0 ) CALL FERR ( INT2(9900), EMESSAGE, INT2(0), &
     &                                   INT2(0) )
           WRITE ( 6, '(A)' ) 'Solution has been written to archive'
#else
           WRITE ( 6, '(A)' ) 'Solution archive feature temporarily is not '// &
     &                        'supported'
           CALL EXIT ( 1 )
#endif
      END IF
!
! --- Celaning up somne fields in of glbfil in order to prevent inadvertent
! --- user's attempt to to recover after completion of the solution
!
      CALL BATCH_CLEANUP()
!
! --- Set status flag: Batch is over
!
      CALL STATUS_SET ( 'BATCH', STA__END )
      END  SUBROUTINE  BATCH  !#!#
