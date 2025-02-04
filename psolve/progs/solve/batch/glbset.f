      SUBROUTINE GLBSET ( IEOPLL, IMODE )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      LOGICAL*2 L4TOL2
!
! 1.  GLBSET PROGRAM SPECIFICATION
!
! 1.1 Set overall (global) solution parameters.
!
! 1.2 REFERENCES:
!
! 2.  GLBSET INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IEOPLL,imode
!
! IEOPLL - Earth orientation plot flag
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc2.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'ba2cm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrls
!       CALLED SUBROUTINES: use_spllk, saves
!
! 3.  LOCAL VARIABLES
!
      CHARACTER    CSTAT*3, FNAME*63, CBUF*128, CBUF1*256, STR*128
      CHARACTER    GET_VERSION*54, GET_CDATE*19
      INTEGER*2    IERR, ICT
      INTEGER*4    IOS
      INTEGER*2    I, IBUF(64), IBUF1(128)
      INTEGER*4    LEN1, I12
      EQUIVALENCE ( IBUF(1), CBUF ) , ( IBUF1(1), CBUF1 )
      LOGICAL*4    KEXIST
      INTEGER*4    I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215  replaced 0J with I4P0
!   KDB   960416  remove unprintable characters from message identifying
!                 machine used for solution and source of executables.
!   PET   960419  accommodated changes in the format of SAVES
!   P. Tomasi July 1 1999  removed definition of DBNAME_MES
!   PVT JULY 2 1999  LEN modified in LEN1 in order to avoid conflict with
!                    intrinsic function
!   pet 05-JUL-99    Removed unused variables.
!   pet 1999.10.07   Added initilaization of correaltion file CORLxx in
!                    non-recovery mode -- file is truncated.
!   pet 1999.10.11   Added initilaization of versions in non-restart mode
!   pet 2002.04.01   Added printing start run date stamp into the spool file
!   pet 2020.07.15   Updated run string
!
!
! 5.  GLBSET PROGRAM STRUCTURE
!
!CCCCC
!
! --- Open and read glbfil off disk
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_4 (  'R' )
      I_ARCNAME    = ' '
      CONTROL_FILE = CFNAME_ORIGINAL
!
! --- Copying reference epochs
!
      SIT_EST_EPOCH = SIT_EST_EPOCH_VAL
      SOU_EST_EPOCH = SOU_EST_EPOCH_VAL
!
! --- Take care of the spool file setup (unit 23)
!
      CALL SET_SPOOL ( TRUE__L2 )
      IPRNT=23
      IF ( RSTOUT .EQ. 'Y'  .AND.  .NOT. RESTRT ) THEN
!
! -------- Rewind spool file
!
           CALL USE_SPOOL ( 'OIC' )
           STR = GET_VERSION ()
!
! -------- Initialise date of modification for all versions
!
           CALL SET_VERSION ( STR(1:5), STR(12:21), '##'//TRIM(STR(23:)) )
           CALL USE_SPOOL ( 'O' )
         ELSE
           CALL USE_SPOOL ( 'A' )
      ENDIF
!
! --- Write run-string to  spool file
!
      CALL CLRCH ( STR )
      STR = 'Run with initials '//PRE_LETRS//' with control file '//TRIM(CFNAME_ORIGINAL)
      WRITE ( 23, '(A)' ) TRIM(STR)
      CALL CLRCH ( CBUF  )
      CALL CLRCH ( CBUF1 )
      IERR = FC_GETENV ( PTR_CH('SOLVE_DIR'//CHAR(0)), PTR_NC(IBUF) )
      IF ( IERR .LE. 0 ) CBUF = SOLVE_PROG_DIR
      IERR = FC_GETHOSTNAME ( IBUF1, LEN1 )
!
! --- Convert nulls in the hostname and alternate executable path to blanks.
!
      DO ICT = 1,8
         IF (ICHAR(CBUF1(ICT:ICT)).EQ.0) CBUF1(ICT:ICT) = ' '
      ENDDO
      DO ICT = 1,40
         IF (ICHAR(CBUF(ICT:ICT)).EQ.0) CBUF(ICT:ICT) = ' '
      ENDDO
!
      WRITE ( 23, '("Run done on ", A, " with executables from ",A)') &
     &        CBUF1(1:I_LEN(CBUF1)), CBUF(1:I_LEN(CBUF))
      WRITE ( 23, '("Run started at ",A," local time")' ) GET_CDATE()
      WRITE ( 23, '(1X)' )
      DO I=1,10
         IF ( ID(I) .NE. ' ' ) THEN
              WRITE(23,'(A63)') 'ID '//ID(I)
         ENDIF
      ENDDO
      WRITE ( 23, '(1X)' )
      CALL USE_SPOOL ( 'C' )
!
! --- Plot file setup
!
      IF ( IEOPLL .EQ. 1 )  THEN
           CALL USE_SPLLK ( PRE_SCR_DIR, EOPL_BASE, EOPL_LU, 'OIC' )
         ELSE IF ( IEOPLL .EQ. 2 )  THEN
           CALL USE_SPLLK ( PRE_SCR_DIR, EOPL_BASE, EOPL_LU, 'OSC' )
      ENDIF
!
      RECVR=0
      FS_FULL(1)=.FALSE.
      FS_FULL(2)=.FALSE.
      FS_FULL(3)=.FALSE.
!
! --- Station table parameters
!
      STAPOSEPOCH = POSEPOCH
      STAPOSNUM = POSNUM
!
! --- Write common out to disk
!
      CALL USE_GLBFIL   ( 'W' )
      CALL USE_GLBFIL_4 ( 'W' )
!
! --- Open GLBFIL, write GLBC2 and GLBC3 to disk
!
      CALL USE_GLBFIL_2('W' )
      CALL USE_GLBFIL_3('WC' )
!
! --- Set up prgfil
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'PRGF'//PRE_LETRS
!
! --- Create the file if doesn't exist
!
      CSTAT='OLD'
      INQUIRE ( FILE=FNAME, EXIST=KEXIST )
      IF ( .NOT. KEXIST ) CSTAT='NEW'
!
! --- Open the file
!
      OPEN ( UNIT=12, IOSTAT=IOS, STATUS=CSTAT, FILE=FNAME, ERR=9000 )
!
! --- ... and instatntly close it
!
      CLOSE ( UNIT=12 )
!
! --- Set KMORED TO .FALSE. in GLBFIL
!
      IF ( RESTRT ) THEN
           IF ( IMODE .EQ. 0 ) THEN
!
! ------------- Recovering
!
                DBNAME_MES = SAVED_ARCNAME_MES
                CALL SAVES ( TRUE__L2, 0, 0, SCNOUT, KCORL, &
     &               IEOPLL, DBNAME_MES, LENCNT, LENARC )
           ENDIF
         ELSE
!
! -------- Not recovering
!
           CALL SAVES ( FALSE__L2, 0, 0, SCNOUT, KCORL, IEOPLL, &
     &         ' ', LENCNT, LENARC )
!
! -------- Clear the name of the last saved arc in NOT-recovery mode
!
           CALL CLRCH ( SAVED_ARCNAME_MES )
!
! -------- Set correlations file ...
!
           FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'CORL'//PRE_LETRS
!
! -------- ... open it ...
!
           OPEN ( UNIT=12, FILE=FNAME, STATUS='UNKNOWN', IOSTAT=I12 )
           IF ( I12 .NE. 0 ) THEN
                WRITE ( 6, * ) ' I12 = ',I12
                CALL FERR ( INT2(7010), 'BATCH(glbset): error in attempt '// &
     &              'to open file '//FNAME, INT2(0), INT2(0) )
               STOP 'BATCH Abnormal termination'
           END IF
!
! -------- ... truncate ...
!
           REWIND  ( UNIT=12 )
           ENDFILE ( UNIT=12 )
!
! -------- ... and close
!
           CLOSE ( UNIT=12 )
      ENDIF
!
! --- Reset the SARfile
!
      CALL ACS_SARFIL ( 'OISC' )
!
! --- We are done
!
      RETURN
!
! --- Issue error and abort if error in file open
!
9000  CONTINUE
      WRITE ( *, 9966 ) IOS, FNAME
9966  FORMAT ( "(GLBSET) ERROR",I7," ACCESSING ",A," PROGRESS FILE PROCESSING", &
     &         "ABORTED" )
!
10000 CONTINUE
      CLOSE ( UNIT=12 )
      RETURN
      END  !#!  GLBSET  #!#
