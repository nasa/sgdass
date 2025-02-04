      SUBROUTINE CTRLS ( IONCTL, OUTCGM, KUSER_PART, USER_PART_PROG, &
     &                   KGLOBONLY, KUSER_CONST, USER_CONST_PROG, CMERG, &
     &                   NARCS, GLBMEM, VCAT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CTRLS PROGRAM SPECIFICATION
!
! 1.1 Parse run string and call CTRFL to parse control file.
!
! 1.2 REFERENCES:
!
! 2.  CTRLS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) IONCTL, OUTCGM, USER_PART_PROG, USER_CONST_PROG
      CHARACTER     CMERG(10)*(*)
      LOGICAL*2     KUSER_PART, KGLOBONLY, KUSER_CONST
      INTEGER*2     NARCS
!
! IONCTL - Ionosphere control, either ON, OFF or DEFAULT
!
! 2.4 COMMON BLOCKS USED
      INCLUDE  'ba2cm.i'
      INCLUDE  'precm.i'
      INCLUDE  'glbc4.i'
      INCLUDE  'glbp.i'
      INCLUDE  'bindisp.i'
      INCLUDE  'flyby.i'
      INCLUDE  'dmapp.i'
      INCLUDE   'vcat.i'
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM  ! defined in glbp.i
      TYPE ( VCAT__TYPE    ) ::  VCAT
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: batch
!       CALLED SUBROUTINES: rtimer,cfopen,ctrfl,rstors,glbset
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4     MP 
      PARAMETER  ( MP = 32*1024 )
      CHARACTER     ID2*60, TEST*5, GONOGO*1, TOKEN*256, TOKEN_NEXT*256, &
     &              FNAME*128, BATSIL*3
      INTEGER*2     TYP, JERR, SOL_ITEMS, LENGTH
      INTEGER*4     IB, IE, IOS, N_SES, N_MIS
      LOGICAL*4     KEXIST, SUPOK, FL_GVF 
      CHARACTER     ERRSTR*255
      CHARACTER     EXPAND_CTLFILE*140
      LOGICAL*2     EXPANDED, FOUND
      CHARACTER     STRING1*32000, STRING1_SAVE*32000, EMESSAGE*255
      CHARACTER     BUF(MP)*32000
      CHARACTER     ARC_LIST1*128, ARC_LIST2*10, ARC_LIST3*10, &
     &              SUPFILE_NAME*160, STR*4096, STR1*4096, ACTUAL_CGM_DIR*128
      CHARACTER     IND_PROC_STR*3, NUM_PROC_STR*3
!
      LOGICAL*4     LEX
      INTEGER*2     MODE_I2, CFREAD, CFOPEN, TRIMLEN
      INTEGER*4     ILINE_CT, IAC, IDAT_I4, J1, J2, J3, NP, NS, IUER
      PARAMETER   ( NS = 8 )
      INTEGER*4,    EXTERNAL :: FSTREAM, ILEN, I_LEN
      LOGICAL*4,    EXTERNAL :: KBIT 
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  900206  Fixeds bug which disabled BATCH proceed flag
!                (Third run string parameter)
!   AEE  910419  Added SRCCMP for proper_motions
!   AEE  910913  Added code to expand the $ARC section of control file.
!   AEE  910924  Added 'ARCFILE filename' keyword to indicate expantion.
!   AEE  911003  Fixed bug RECOVER bug introduced by ARC include implementaion.
!   AEE  911028  Allowed comments in ARC include files, used trimlen for *.XPND
!   AEE  911101  Allowed commenting out include files in $ARC section.
!   KDB  921027  Solution archiving interface
!   KDB  951010  Update date in initial batch message, unsing sccsid.
!   KDB  960320  Blank lines in the arcfile gave range errors.  Now they will
!                be ignored.
!   kdb  970115  If the program can't find the input arc file, abort even in
!                foreground mode.  This addresses a problem in which a user
!                forged on, then realized the problem, panicked and killed the
!                program when he wasn't supposed to, leaving a partial pending
!                entry.  The goal is to minimize obscure catalog errors that
!                can strand the user, even if they could be manually avoided
!                by the users.
!   KDB  970204  New site weighting feature.  Pass new variable to ba2cm.i.
!   PET  970523  Support of environment variable CGM_DIR is added.
!   PET  970902  Support ACCEOP_FLG is added.
!   KDB  980223  Batch interface for sinex output
!   pet  980615  Increased length of the varaible EXPAND_CTRLFILE
!                from 36 to 128 bytes
!   pet  990111  Added formal argument GLBMEM and call of GLO_INIT
!   pet  990114  Added check: does the superfile really exist
!   pet  990118  Added computation of the total number of arcs to be processed
!   pet  990405  Added call of DBG_BEG
!   pet  990426  Corrected a bug connected in handling comments in arc-files
!                handling
!   pet  990507  Changed a bit logic: input CGM is read at the beginning in
!                NO TRAIN mode
!   pet 1999.05.12   Forced BATCH to stop outright if it doesn't find control
!                    file
!   pet 2000.03.05   Added check of consistency flags WEIGTHS and NORATE_FLAG
!   pet 2000.09.07   Added support of environment variable BATCH_SILENT which
!                    suppresses information messages while control
!                    file is read.
!   pet 2000.09.22   Replace call of SYSTEM_SH with system call UNLINK
!   pet 2000.11.21   Inserted call of DISCONT_INIT. Moved ot from arscset since
!                    some data structures should be initialized before the
!                    first call of arcset in the mode with input CGM.
!   pet 2001.12.13   Added support of a new type of solutions: GLOBAL_ONLY
!   pet 2002.02.22   Improved rerror message. New versoion of ctrls gives
!                    descriptive message in the case when control file did
!                    not contain any arc-line.
!   pet 2006.02.08   Added support the case when a superfile was crated 
!                    with UPDTB
!   pet 2007.08.10   Added logic for bypassing recovery
!   pet 2023.09.22   Added logic for defining and undefining enviroment variables
!   pet 2023.09.22   Added logic for expanding enviroment varaibels when a substring &
!                    in a form ${MY_VAR} is encountered
!
! 5.  CTRLS PROGRAM STRUCTURE
!
!CCCC
!
! --- Get the current time and display current version info to screen
!
      CALL RTIMER()
!
! --- Set up ERRFIL
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'ERRF'//PRE_LETRS
!
! --- Purge the file if it currently exists
!
      INQUIRE ( FILE=FNAME, EXIST=KEXIST )
      IF ( KEXIST ) CALL BIN_UNLINK ( FNAME, INT2(IOS) )
!
! --- Open the ERRFxx file ... and close it instantly
!
      OPEN ( UNIT=13, IOSTAT=IOS, STATUS='NEW', FILE=FNAME, ERR=9000 )
      CLOSE(13)
!
! --- Parse the run string
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      CALL SPLITSTRING ( STRING, IND_PROC_STR, STRING )
      CALL SPLITSTRING ( STRING, NUM_PROC_STR, STRING )
!
! --- Decode the number of processors and the processor index
!
      CALL CHIN ( NUM_PROC_STR, NUM_PROC )
      CALL CHIN ( IND_PROC_STR, IND_PROC )
!
! --- Sanity check
!
      IF ( NUM_PROC < 1  .OR.  NUM_PROC > 128 ) THEN
           NUM_PROC = 1
           IND_PROC = 1
      END IF
      IF ( IND_PROC < 1  .OR.  IND_PROC > 128 ) THEN
           NUM_PROC = 1
           IND_PROC = 1
      END IF
!
! --- Get the batch control file name.
! --- Create another control file with the same name but with .XPND extention
! --- which its $ARC section is expanded if the original control file's $ARC
! --- section contains file(s) names that are made up of ARCs themselves:
!
      CALL SPLITSTRING ( STRING, CFNAME, STRING )
      EXPANDED= .FALSE.  ! default if not expanding $ARC section.
!
      INQUIRE ( FILE=CFNAME, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4011, -3, 'CTRLS', 'Control file '// &
     &          CFNAME(1:I_LEN(CFNAME))//' is not found' )
           STOP 'BATCH(ctrls) Abnormal termination'
      END IF
      BATSIL = '   '
      CALL GETENVAR ( "BATCH_SILENT", BATSIL )
      IF ( BATSIL(1:1) .EQ. 'y' ) BATSIL(1:1) = 'Y'
!
      LENCNT = 0 ! Initialization of a line counter for control file
      LENARC = 0 ! Initialization of a line counter for arc file
      OPEN ( 30, FILE=CFNAME, IOSTAT=IOS, STATUS='OLD' )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( ERRSTR, "('Failure to open control file ',A )") CFNAME
           CALL FERR ( INT2(1032), ERRSTR, INT2(0), INT2(0) )
      END IF
!
      EXPAND_CTLFILE = CFNAME(:TRIMLEN(CFNAME))//'.XPND'
!
      OPEN ( 35, FILE=EXPAND_CTLFILE, IOSTAT=IOS, STATUS='UNKNOWN' )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( ERRSTR, "('Failure to open expanded control file ', A )") &
     &             EXPAND_CTLFILE
           CALL FERR ( INT2(1033), ERRSTR, INT2(0), INT2(0) )
      END IF
      SUPOK = .TRUE.
!
! --- Copy every thing up to $ARC line from original control file
! --- to the new one:
!
      READ ( 30, "(A)", END=60, IOSTAT=IOS ) STRING1
      LENCNT = LENCNT + 1
      CALL FERR ( IOS, "Reading batch control file", INT2(0), INT2(0) )
      DO WHILE ( STRING1(1:5) .NE. '$ARCS' )
         IF ( STRING1 .NE. ' ' ) THEN
              STR = STRING1
              CALL CHASHL ( STR )
              IF ( STR(1:1) == '@' ) THEN
                   WRITE ( 6, * ) 'Expand '//TRIM(STR(2:))
                   IUER = -1
                   IE = INDEX ( STR(2:), ' ' ) + 1
                   IF ( IE > 2 ) THEN
                        CALL CLRCH ( STR(IE:) )
                   END IF
                   CALL RD_TEXT ( STR(2:), MP, BUF, NP, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 4011, -2, 'CTRLS', 'Error in an attempt '// &
     &                      'to read the include file '//STR(2:) )
                        CALL EXIT ( 1 )
                   END IF
                   DO 410 J1=1,NP
                      WRITE ( 35, '(A)', IOSTAT=IOS ) TRIM(BUF(J1))
 410               CONTINUE 
                 ELSE IF ( STR(1:7)  == 'DEFINE ' ) THEN
                   CALL SPLIT_STRING ( STR, TOKEN, STR )
                   CALL SPLIT_STRING ( STR, TOKEN, STR )
                   CALL CHASHL ( STR )
                   CALL SETENV   ( TRIM(TOKEN)//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
                   WRITE ( 6, * ) 'VAR: '//TRIM(TOKEN)//' defined as '//TRIM(STR)
                 ELSE IF ( STR(1:9) == 'UNDEFINE ' ) THEN
                   CALL SPLIT_STRING ( STR, TOKEN, STR )
                   CALL SPLIT_STRING ( STR, TOKEN, STR )
                   CALL SETENV   ( TRIM(TOKEN)//CHAR(0), CHAR(0), %VAL(1) )
                   WRITE ( 6, * ) 'VAR: '//TRIM(TOKEN)//' is undefined'
                 ELSE IF ( STR(1:2) == '${' ) THEN
                   IE = INDEX ( STR, '}' )
                   IF ( IE < 3 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 4012, -2, 'CTRLS', 'Error in processing line '// &
     &                       TRIM(STR)//' -- there is no matching }' )
                        CALL EXIT ( 1 )
                   END IF
                   WRITE ( 6, * ) 'Expand '//STR(3:IE-1)
                   CALL GETENVAR ( STR(3:IE-1), STR1 )
                   IF ( ILEN(STR1) < 1 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 4013, -2, 'CTRLS', 'Error in processing line '// &
     &                       TRIM(STR)//' -- environment variable '//STR(3:IE-1)// &
     &                       ' is not defined' )
                        CALL EXIT ( 1 )
                   END IF
                   STR = TRIM(STR1)//STR(4:)
                   IUER = -1
                 ELSE
                   WRITE ( 35, "(A)", IOSTAT=IOS ) STRING1(1:TRIMLEN(STRING1))
                   CALL FERR ( INT2(IOS), "Writing expanded control file", INT2(0), &
     &                         INT2(0) )
              ENDIF
         ENDIF
         READ ( 30, "(A)", END=60, IOSTAT=IOS ) STRING1
         CALL FERR ( INT2(IOS), "Reading batch control file", INT2(0), INT2(0) )
         LENCNT = LENCNT + 1
      ENDDO
!
      WRITE ( 35, "(A)", IOSTAT=IOS ) STRING1(1:TRIMLEN(STRING1)) ! Copy $ARCS line to new file.
      CALL FERR ( INT2(IOS), "Writing expanded control file", INT2(0), INT2(0) )
!
      CALL SPLIT_AND_CASEFOLD ( STRING1, TOKEN, STRING1 )
      CALL SPLIT_AND_CASEFOLD ( STRING1, TOKEN, STRING1 )
      IF ( TOKEN == 'GVF'  .OR.  TOKEN == 'GVH'  ) THEN
           FL_GVF = .TRUE.
         ELSE 
           FL_GVF = .FALSE.
      END IF
!
! --- Now expand the arc section:
! --- The arc section can be expressed as a list of arcs in the control
! --- file, a full path to an arc file or the user and solution
! --- (and optional version) tags of a prior solution whose arc file
! --- should be used.  This last option can only be used if this version
! --- of SOLVE includes solution archive system code.
!
      READ ( 30, "(A)", END=60, IOSTAT=IOS ) STRING1
      LENCNT = LENCNT + 1
      CALL FERR ( INT2(IOS), "Reading batch control file", INT2(0), INT2(0) )
      ARC_FILE_TYPE = 'NONE'
      SUPOK = .TRUE.
!
! --- Opening SUPCAT file for further test of superfile presence
!
      IF ( FL_GVF ) THEN
           IUER = -1
           CALL CH_DB_GVF ( VCAT, STRING1, N_SES, N_MIS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4014, -2, 'CTRLS', 'Error in an attempt '// &
     &              'to retrieve the list of database in GVF format' )
                CALL EXIT ( 1 )
           END IF
           IF ( N_MIS > 0 ) THEN
                WRITE ( 6, 210 ) N_MIS
 210            FORMAT ( 'BATCH(ctrls): ', I5, ' databases ', &
     &                   'are not found in the control file' )
                CALL EXIT ( 1 )
           END IF
           NARCS = N_SES
         ELSE 
           CALL CHSUP ( STRING1, FOUND, INT2(0) )
           NARCS = 0
!
           WRITE ( 6, FMT='(A$)' ) '  arcnames are being parsed...'//CHAR(13)
!
! -------- Parse bottom part of the control file where superfiles are specified
!
           G_WARNING = .TRUE. ! Set it temporary in order to force CHSUP to print
!                        ! warnings. Then G_WARNING will be setup in according
!                        ! with default and value in control file
!
! -------- Save the string
!
           STRING1_SAVE = STRING1
!
! -------- Extract two tokens. We are curious to learn whether ARCFILE NONE
!
           CALL SPLIT_AND_CASEFOLD ( STRING1, TOKEN,      STRING1 )
           CALL SPLIT_AND_CASEFOLD ( STRING1, TOKEN_NEXT, STRING1 )
           IF ( TOKEN .EQ. 'ARCFILE'  .AND.  TOKEN_NEXT .EQ. 'NONE' ) THEN
!
! ------------- Read the next line
! 
                LENGTH = CFREAD ( STRING1 )
              ELSE
!
! ------------- Save the line
!
                STRING1 = STRING1_SAVE
           END IF
           DO WHILE ( STRING1(1:1).NE.'$' .AND. STRING1(1:1).NE. 'ARCFILE' )
              IF ( INDEX ( STRING1, 'ARCFILE' ) .EQ. 0 .OR. &
     &             INDEX ( STRING1, '*' )       .EQ. 1       ) THEN ! No expansion needed.
                 IF ( STRING1 .EQ. ' '     ) GOTO 60 ! empty line means the end
!
                 IF ( STRING1(1:1) .EQ. ' ' ) THEN
!
! ------------------- Special trick for the case when the first not-blank symbol
! ------------------- of the string has position more than 2
!
                      CALL CHASHL ( STRING1 )
                      STRING1 = ' '//STRING1
                   ELSE IF ( STRING1(1:1) .EQ. '$' ) THEN
!
! ------------------- Also trick for the case when the first symbol is "$"
!
                      STRING1 = ' '//STRING1
                 END IF
!
                 STR = STRING1 
                 CALL CHASHL  ( STR )
                 CALL CHIN ( STR(1:8), IDAT_I4 )
                 IF ( STRING1(1:2) .EQ. ' $' .OR. &
     &                IDAT_I4 > 19700000 .AND.  IDAT_I4 < 21000000 ) THEN
!
                      IF ( STRING1(1:2) .EQ. ' $' ) THEN
                           MODE_I2 = 1
                         ELSE 
                           MODE_I2 = 2
                      END IF
                      CALL CHSUP ( STRING1, FOUND, MODE_I2 )
                      IF ( .NOT. FOUND ) SUPOK = .FALSE.
                      IF ( FOUND ) THEN
!
! ------------------------ Get a full file name for this arc including path
!
                         IUER = -1
                         CALL GET_SUPERFILE_NAME ( STRING1, SUPFILE_NAME, IUER )
                         IF ( IUER .NE. 0 ) THEN
                              CALL ERR_LOG ( 4015, -2, 'CTRLS', &
     &                            'Error in parsing the line '//STRING(1:32)// &
     &                            ' of the control file' )
                              SUPOK = .FALSE.
                         END IF
!
! --------------------- Check: does it exist?
!
                         INQUIRE ( FILE = SUPFILE_NAME, EXIST = LEX )
                         IF ( .NOT. LEX ) THEN
                              IUER = -1
                              CALL ERR_LOG ( 4016, IUER, 'CTRLS','Superfile '// &
     &                             SUPFILE_NAME(1:I_LEN(SUPFILE_NAME))// &
     &                            ' was not found' )
                            SUPOK = .FALSE.
                         END IF
!
! ---------------------- Increment of superfiles counter
!
                         NARCS = NARCS + 1
                      END IF ! found
                   ELSE IF ( STRING1(1:1) .EQ. '*' .OR. STRING1(1:2) .EQ. ' *') THEN
!
! ------------------- Legitimate case: comment
!
                      CONTINUE
                   ELSE
!
! ------------------- ?? We don't know how to treat this case
!
                      CALL ERR_LOG ( 4017, -3, 'CTRLS', &
     &                    'Unrecognised format of arc line :'// &
     &                     STRING1(1:I_LEN(STRING1))//' ' )
                    SUPOK = .FALSE.
                 END IF ! string1(2:2)
!
                 WRITE ( 35, "(A)", IOSTAT=IOS ) STRING1(1:TRIMLEN(STRING1) )
                 CALL FERR ( INT2(IOS), "Writing expanded control file", INT2(0), &
     &                       INT2(0) )
                 READ ( 30, "(A)", END=60, IOSTAT=IOS ) STRING1
                 CALL FERR ( INT2(IOS), "Reading batch control file", INT2(0), &
     &                       INT2(0) )
                 LENCNT = LENCNT + 1
              ELSE  ! Open arc file (filename) for expantion.
                 EXPANDED = .TRUE.
!
! -------------- Read off ARCFILE token
!
                 CALL SPLITSTRING ( STRING1, TOKEN, STRING1 )
!
! -------------- Get full path or us tag
!
                 CALL SPLITSTRING ( STRING1, ARC_LIST1, STRING1 )
!
! -------------- See if sol tag
!
                 CALL SPLITSTRING ( STRING1, ARC_LIST2, STRING1 )
!
! -------------- See if version
!
!@                 CALL SPLITSTRING ( STRING1, ARC_LIST3, STRING1 )
!@                 CALL RESOLVE_ARC ( ARC_LIST1, ARC_LIST2, ARC_LIST3, &
!@     &                              ARC_FILE_TYPE, ARC_FILE_PATH, ARC_FILE_KEY, &
!@     &                              ARC_FILE_VER, EMESSAGE, INT2(IOS) )
!@                 IF ( IOS .NE. 0 ) THEN
!@!
!@! ------------------- Set to background mode so that GLOBL will automatically
!@! ------------------- abort.
!@!
!@                      CALL SBIT ( PRE_IP(2), INT2(6), INT2(0) )
!@                      CALL FERR ( INT2(1035), EMESSAGE, INT2(0), INT2(0) )
!@                 ENDIF
!
                 OPEN ( UNIT=40, FILE=ARC_FILE_PATH, IOSTAT=IOS, STATUS='OLD' )
                 IF ( IOS .NE. 0 ) THEN
                      WRITE ( ERRSTR, &
     &                        "('(ctrls) Failure to open arc file ',A)")ARC_FILE_PATH
                      CALL FERR ( INT2(1036), ERRSTR, INT2(0), INT2(0) )
                 END IF
                 ILINE_CT = 0
!
                 READ ( 40, "(A)", END=50, IOSTAT=IOS ) STRING1
                 CALL FERR ( INT2(IOS), "Reading arc file", INT2(0), INT2(0) )
                 ILINE_CT = ILINE_CT + 1
                 LENARC = LENARC + 1
!
                 DO WHILE ( .TRUE. ) ! read lines from include file.
                    IF ( TRIMLEN(STRING1) .NE. 0 ) THEN
                       IF ( STRING1(1:1) .EQ. ' ' ) THEN
!
! ------------------------- Special trick for the case when the first not-blank
! ------------------------- symbol of the string has position more than 2
!
                            CALL CHASHL ( STRING1 )
                            STRING1 = ' '//STRING1
                          ELSE IF ( STRING1(1:1) .EQ. '$' ) THEN
!
! ------------------------- Also trick for the case when the forst symbol is "$"
!
                            STRING1 = ' '//STRING1
                       END IF
!
                       STR = STRING1 
                       CALL CHASHL  ( STR )
                       CALL CHIN ( STR(1:8), IDAT_I4 )
                       IF ( STRING1(1:2) .EQ. ' $' ) THEN
                            CALL ERR_LOG ( 4018, -2, 'CTRLS', 'Error in parsing '// &
     &                          'the line '//STRING(1:32)//' of the control files '// &
     &                          ' -- superfiles are not supported any more.' )
                            CALL EXIT ( 1 )
                         ELSE IF ( IDAT_I4 > 19700000 .AND.  IDAT_I4 < 21000000 ) THEN
!
                            IF ( STRING1(1:2) .EQ. ' $' ) THEN
                                 MODE_I2 = 1
                              ELSE 
                                 MODE_I2 = 2
                            END IF
!
                            CALL CHSUP ( STRING1, FOUND, MODE_I2 )
                            IF ( .NOT. FOUND ) SUPOK = .FALSE.
!
! ------------------------------ Get a full file name for this arc including path
!
                            IUER = -1
                            CALL GET_SUPERFILE_NAME ( STRING1, SUPFILE_NAME, IUER )
                            IF ( IUER .NE. 0 ) THEN
                                 CALL ERR_LOG ( 4019, -2, 'CTRLS', &
     &                               'Error in parsing the line '//STRING(1:32)// &
     &                               ' of the superfile' )
                                 SUPOK = .FALSE.
                            END IF
!
! ------------------------- Check: does it exist?
!
                            INQUIRE ( FILE = SUPFILE_NAME, EXIST = LEX )
                            IF ( .NOT. LEX ) THEN
                                 IUER = -1
                                 CALL ERR_LOG ( 4020, IUER, 'CTRLS', &
     &                               'Superfile '// &
     &                                SUPFILE_NAME(1:I_LEN(SUPFILE_NAME))// &
     &                               ' was not found' )
                                 SUPOK = .FALSE.
                            END IF
!
! ------------------------- Increment of superfiles counter
!
                            NARCS = NARCS + 1
!
                            WRITE ( 35,"(A)", IOSTAT=IOS) &
     &                              STRING1(1:TRIMLEN(STRING1))
                            CALL FERR ( INT2(IOS), "(ctrls) Writing expanded "// &
     &                          "control file", INT2(0), INT2(0) )
                         ELSE IF ( STRING1(1:1) .EQ. '*'   .OR. &
     &                             STRING1(1:2) .EQ. ' *'       ) THEN
!
! ------------------------- Ligitimate case: comment
!
                            CONTINUE
                         ELSE
                            WRITE ( 35, "(A)", IOSTAT=IOS) &
     &                              STRING1(1:TRIMLEN(STRING1))
                            CALL FERR ( INT2(IOS), "(ctrls) Writing expanded "// &
     &                                 "control file", INT2(0), INT2(0) )
                        END IF ! string1(1:1), string1(2:2)
                      ELSE
                        WRITE ( *, &
     &                  '("***Line ",i10," in your arcfile is blank.")')ILINE_CT
                    END IF ! trimlen > 0
!
                    READ ( 40, "(A)", END=50, IOSTAT=IOS ) STRING1
                    CALL FERR ( INT2(IOS), "Reading arc file", INT2(0), INT2(0) )
                    LENARC = LENARC + 1
                    ILINE_CT = ILINE_CT + 1
                 ENDDO
!
 50              CONTINUE
                 CLOSE ( UNIT=40 )
                 READ ( 30, "(A)", END=60, IOSTAT=IOS ) STRING1
                 LENCNT = LENCNT + 1
                 CALL FERR ( INT2(IOS), "Reading batch control file", INT2(0), &
     &                       INT2(0) )
             ENDIF
           ENDDO ! Cycle through the control file
      ENDIF
!
 60   CONTINUE
      CLOSE ( UNIT=35 )
      CLOSE ( UNIT=30 )
!
      IF ( NARCS .EQ. 0 ) THEN
           CALL FERR ( INT2(186), '(CTRLS) Batch control file does '// &
     &         'not contain any superfile', INT2(0), INT2(0) )
           STOP 'BATCH(ctrls)  Abnormal termination'
      END IF
!
      IF ( .NOT. SUPOK ) THEN
           CALL FERR ( INT2(188), '(CTRLS) Superfile(s) missing from '// &
     &         'superfile catalogue', INT2(0), INT2(0) )
           STOP 'BATCH(ctrls)  Abnormal termination'
      ENDIF
!
      CFNAME_ORIGINAL = CFNAME
      CFNAME = EXPAND_CTLFILE
!
      TYP = CFOPEN ( CFNAME )
      write ( 6 ,* ) 'Open control file '//TRIM(CFNAME)
!
      CALL SPLITSTRING ( STRING, GONOGO, STRING )
      IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( 6, FMT='(A)' ) &
     &    ' Arcnames                       '
!
! --- Parse the control file (except ARC file)
!
      CALL CTRLFL ( PRE_LETRS, ARCREC, ITARCS, WEIGHTS, LF_WEI, WEIGHT_FILE, &
     &            SOLTYP, CGMNMR, INCGM_TYPE, INCGM_USER, INCGM_SOL, &
     &            B_ARCDIR, ID, USER_PROG, B_KPERMARC, &
     &            USER_BUFF, STAFLG, FIXSTA_CHR, STADIU, VELFLG, SRCFLG, &
     &            FIXSRC_CHR, PROFLG, NUTFLG, UT1FLG, PRCFLG, RELFLG, IONCTL, &
     &            STACRY, SRCCRY, &
     &            RSTOUT, MINOUT, BASOUT, FWDOUT, SCNOUT, ATMFLG, INTRVL, &
     &            CLKPOL_FLG, CLKPOL_DEG, CLKFLG, CKNTRVL, FCNPR, TBLOUT, &
     &            AXSFLG, OFFLG, RATFLG, ACCEOP_FLG, &
     &            IEOP_FLG, REOP_FLG, IEOPL_BA, OUTCGM, POSELL, &
     &            BLCFLG, IOS_EST_BATCH, BASDF, USER_TAG, SOL_TAG, SOLARCH_SOL, KUSER_PART, &
     &            USER_PART_PROG, KGLOBONLY, EOPMID, KHFEOPEST, IONFLG, &
     &            POSEPOCH, POSNUM, MODOUTFLG, RESFILE, KMIN_SIG, &
     &            GRADFLG, GRINTRVL, KUSER_CONST, USER_CONST_PROG, CMERG, &
     &            KOUTNRM, KZERONRM, WEIGHT_TYPE_GEN, WEIGHT_ALGORITHM, &
     &            SIT_EST_EPOCH_VAL, SOU_EST_EPOCH_VAL, EOP_EPOCH_MJD, &
     &            EOP_EPOCH_SEC, EOP_BEFORE_SEC_TAI, EOP_AFTER_SEC_TAI, &
     &            PARU_FILE, MF_WEI )
!
      IF ( KCORL .AND. &
     &     B_ARCDIR(1).EQ. ' '  .AND. &
     &     IDBNAME(1:3) .NE. 'CGM' ) THEN
           CALL FERR ( INT2(110), &
     &         'BATCH(ctrls): Covariance output requires '//'saved arcfils', &
     &          INT2(0), INT2(0) )
      ENDIF
!
      IF ( ( WEIGHTS .EQ. 'M'  .OR.  WEIGHTS .EQ. 'A' ) .AND. &
     &       WEIGHT_ALGORITHM .EQ. WEIGHT__MYWAY ) THEN
           IF ( NORATE_FLAG ) THEN
                CALL ERR_LOG ( 4021, -3, 'CTRLS', 'Incompatible '// &
     &              'options: WEIGHT '//WEIGHTS//' with algortihm MYWAY '// &
     &              'and NORATE_FLAG YES cannot be used together. '// &
     &              'Recommendation: set NORATE_FLAG to NO' )
                STOP 'BATCH(ctrls) Abnormal termination'
           END IF
      END IF
!
! --- If this solution is going to be catalogued,
! --- call solution archiving subroutines, which will make sure
! --- the solution and run initials are valid, reserve space for the solution
! --- items (control file, arc file, etc.) in the primary storage area,
! --- and reserve a spot in the solution catalog for the solution.
!
      IF ( SOLARCH_SOL ) THEN
!
! -------- First set a bit array telling which types of items are in this
! -------- solution.
!
           SOL_ITEMS = 0
           CALL SBIT ( SOL_ITEMS, INT2(1), INT2(1) ) ! All solutions have control files
           IF ( ARC_FILE_TYPE .NE. 'NONE' ) CALL SBIT ( SOL_ITEMS, INT2(2), &
     &          INT2(1) ) ! arc file
           CALL SBIT ( SOL_ITEMS, INT2(3), INT2(1) ) ! Spool file
           CALL SBIT ( SOL_ITEMS, INT2(4), INT2(1) ) ! Progress file
           IF ( SOLTYP .EQ. 'F'  .OR.  SOLTYP .EQ. 'C'  .OR. &
     &          SOLTYP .EQ. 'S'  .OR.  SOLTYP .EQ. 'G'       ) THEN ! output cgm
                CALL SBIT ( SOL_ITEMS, INT2(5), INT2(1) )
           END IF
!
           IF ( KCORL ) THEN
                CALL SBIT ( SOL_ITEMS, INT2(6), INT2(1) )  ! Covariance
                CALL SBIT ( SOL_ITEMS, INT2(7), INT2(1) )  ! Correlations
           END IF
!
!@           CALL STARTSOL ( PRE_LETRS, USER_TAG, SOL_TAG, SOL_ITEMS, SOLTYP, &
!@     &                     ID, INCGM_TYPE, INCGM_USER, INCGM_SOL, ITEM_LUS, &
!@     &                     IVER, EMESSAGE, JERR )
!@           IF ( JERR .NE. 0 ) THEN
!@!
!@! ------------- Set to background mode so that globl will automatically abort.
!@! ------------- Otherwise, the user may continue and run into problems when
!@! ------------- SOLARCH tries to finish up, but can't because the set up was
!@! ------------- only partially completed.
!@!
!@                CALL SBIT ( PRE_IP(2), INT2(6), INT2(0) )
!@                CALL FERR ( INT2(1037), 'BATCH(ctrls): '//EMESSAGE, INT2(0), &
!@     &               INT2(0) )
!@           END IF
!
           RUN_INITS = PRE_LETRS
      END IF
!
! --- Determine whether we are to recover an interrupted job
!
      IF ( .NOT. KBIT ( PRE_IP(2), INT2(8) ) ) THEN
           CALL RSTORS ( IPASS, ARCNUM, SOLTY2, RESTRT, ETIME0, ETIMP0, &
     &                   SCNOUT, KCORL, IEOPL_BA, LENCNT, LENARC, &
     &                   CFNAME_ORIGINAL )
         ELSE 
!
! -------- If PRE_IP(2) shows that we are prohibited from trying to recover,
! -------- do not do it.
!
           RESTRT = .FALSE.
      END IF
!
      IF ( RESTRT .AND. ( IPASS .EQ. 1 .OR. ARCNUM .EQ. 0 ) ) THEN
!
! -------- If not recovering, then set global solution parameters
!
           CALL GLBSET ( IEOPL_BA, INT2(1) )
         ELSE IF ( .NOT. RESTRT ) THEN
!
! -------- Clean some fields in glbcm if we are not restarting
!
           CALL BATCH_CLEANUP()
      ENDIF
      CALL DBG_BEG ( RESTRT, NARCS )
!
! --- Initilialize internal data structures for parameterization of station
! --- positions with episodic motion and/or liinear spline
!
      CALL DISCONT_INIT ( ESMMAP, NESM, ESMSITES, ESMDATES, PWCMAP )
!
! --- Set flag indicating that substitution files has not been initialized
!
      SRC_SUBSTITUTE_INIT = .FALSE.
      SIT_SUBSTITUTE_INIT = .FALSE.
      VEL_SUBSTITUTE_INIT = .FALSE.
!
      GLBMEM%LEN_GLO = -1 ! set flag "GLBMEM is not initialized"
      GLBMEM%L_GPA   = -1 ! set flag "GLO_INIT has not been called before"
      IF ( .NOT. TRAIN  .AND.  SOLTYP .NE. 'I' ) THEN
           IF ( SOLTYP .EQ. 'B' ) IPASS=2 ! We have to set it...
!
! -------- Initialize data structure GLBMEM.
! -------- Grab dynamic memory if needed. Restore solution if we are in
! -------- resotration mode of global solution.
!
           IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( 6, FMT='(A$)') &
     &          '  Initialization of global arrays ...         '//CHAR(13)
!
           IUER = -1
           CALL GLO_INIT ( RESTRT, CGMNMR, GLBMEM, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4022, -2, 'CTRLS', 'Error during '// &
     &              'attempt to initialize data structure for batch '// &
     &              'solution' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           IF ( RESTRT .AND. ( IPASS .EQ. 2 .AND. ARCNUM .GE. 1 ) ) THEN
!
! ------------- Read COVFxx file. We think that CGM was already inverted
!
                IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( 6, FMT='(A$)') &
     &               '  Reading input COVFxx file ...          '//CHAR(13)
                CALL GLO_RESTORE ( 5, GLBMEM, IUER )
              ELSE IF ( RESTRT .AND. ( IPASS .EQ. 2 .AND. ARCNUM .LE. 0 ) ) THEN
!
! ------------- Read CGMBxx file. We assume that CGM is formed, but not
! ------------- inverted yet.
!
                IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( 6, FMT='(A$)') &
     &               '  Reading input CGMBxx file ...          '//CHAR(13)
                CALL GLO_RESTORE ( 4, GLBMEM, IUER )
              ELSE IF ( RESTRT .AND. IPASS .EQ. 1 ) THEN
!
! ------------- Read CGMBxx file. We assume that CGM is not formed and
! ------------- inverted yet.
!
                IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( 6, FMT='(A$)') &
     &               '  Reading input CGMBxx file ...          '//CHAR(13)
                CALL GLO_RESTORE ( 1, GLBMEM, IUER )
              ELSE IF ( .NOT. RESTRT  .AND. GLBMEM%L_GPA .GT. 0 ) THEN
!
! ------------- Read an input CGM
!
                IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( 6, FMT='(A$)') &
     &               '  Reading input CGM file ...              '//CHAR(13)
                CALL GLO_RESTORE ( 3, GLBMEM, IUER )
           END IF
!
           IF ( RESTRT .AND. IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4023, -2, 'CTRLS', 'Failure '// &
     &              'in attempt to restore CGM in "NO TRAIN" mode' )
                CALL EXIT ( 1 )
           END IF
           IF ( BATSIL(1:1) .NE. 'Y' ) WRITE ( 6, FMT='(A)') &
     &             '  Initialization is completed              '
      END IF
      TEST_FIELD = 0
!
      IF ( .NOT. RESTRT ) THEN
!
! -------- Check status of output CGM file:
! -------- in non-SOLARCH mode, where solution will not be catalogued:
! -------- Upon completion, GLOBL puts the output cgm into a permanent file.
! -------- So make sure that the solution about to be run will not try
! -------- to overwrite a permanent cgm from another solution.
! --------
! -------- if in SOLARCH mode, where solution will be catalogued:
! -------- Upon completion, GLOBL puts the output cgm into a scratch file and
! -------- lets another piece of software, the SOLARCH solution handler,
! -------- move the scratch file to its permanent location later.
! -------- This scratch file is used over and over again for all the runs
! -------- done by this set of run initials, so it's ok to overwrite it as
! -------- long as we're not in recovery mode, where the cgm output so far
! -------- is still needed. However, GLOBL is geared to not overwrite
! -------- existing cgms, so if the scratch file from the previous run
! -------- is still there, wipe it out to make GLOBL happy.
!
! -------- Examine environment variable CGM_DIR
!
           CALL CLRCH ( ACTUAL_CGM_DIR )
           CALL CLRCH ( STR )
           CALL GETENVAR ( 'PSOLVE_CGM_DIR', STR )
           IF ( STR(1:1) .NE. ' ' ) THEN
!
! ------------- Oh! It exists
!
                IF ( STR(ILEN(STR):ILEN(STR)) .NE. '/' .AND. &
     &               ILEN(STR) .LT. LEN(STR) ) STR(ILEN(STR)+1:) = '/'
!
! ------------- Well, substitute CGM_DIR
!
                ACTUAL_CGM_DIR = STR
              ELSE
                ACTUAL_CGM_DIR = CGM_DIR
            END IF
!
            IAC = ILEN(ACTUAL_CGM_DIR)
!
            IF ( OUTCGM .NE. ' ' ) THEN
                 IF ( SOLARCH_SOL .AND. TRIMLEN(OUTCGM) .EQ.6   .AND. &
     &                OUTCGM(1:6) .EQ. 'CGMC'//PRE_LETRS               ) THEN
!
! ------------------- We delete old CGM
!
                      CALL UNLINK ( ACTUAL_CGM_DIR(1:IAC)// &
     &                              OUTCGM(1:TRIMLEN(OUTCGM)) )
                 END IF
!
                 ID2 = ID(1)
                 CALL SPLITSTRING ( ID2, TEST, ID2 )
!
! -------------- Form full name of output CGM
!
                 IF ( OUTCGM(1:1) .EQ. '/' ) THEN
                      FNAME = OUTCGM
                    ELSE
                      IF ( TEST(1:4) .EQ. 'TEST'  .OR. &
     &                     TEST(1:4) .EQ. 'test'        ) THEN
                           FNAME = SCRATCH_DIR//OUTCGM
                        ELSE
                           FNAME = ACTUAL_CGM_DIR(1:IAC)//OUTCGM
                      ENDIF
                 ENDIF
!
                 IF ( OUTCGM(1:4) .EQ. 'NONE' .AND. TRIMLEN(OUTCGM) .EQ.4 ) THEN
                      CONTINUE
                    ELSE
                      INQUIRE ( FILE=FNAME, EXIST=KEXIST )
                      IF ( KEXIST ) THEN
                           ERRSTR='BATCH(CTRLS): CGM file '// &
     &                             FNAME(1:I_LEN(FNAME))//' already exists'
                           CALL FERR ( INT2(3091), TRIM(ERRSTR), INT2(0), INT2(0) )
                           CALL EXIT ( 1 )
                      ENDIF
                 ENDIF
            ENDIF
!
            CALL GLBSET ( IEOPL_BA, INT2(0) )
            IPASS=1
            IF ( .NOT. TRAIN  .AND.  SOLTYP .EQ. 'B' ) IPASS = 2
      ENDIF
!
9000  CONTINUE
      RETURN
      END  !#!  CTRLS  #!#
