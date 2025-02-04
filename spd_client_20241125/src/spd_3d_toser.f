      PROGRAM    SPD_3D_TOSER_MAIN
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      CHARACTER  STR*128
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = INT8(4) * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL SPD_3D_TOSER()
      END  PROGRAM  SPD_3D_TOSER_MAIN
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  SPD_3D_TOSER()
! ************************************************************************
! *                                                                      *
! *   Program SPD_3D_TOSER  reads the directory with ascii files with    *
! *   the 3D Slanted Path Delay, parses them, transforms to the binary   *
! *   format, and writes the output files in the binary spd format into  *
! *   the output directory.                                              *
! *                                                                      *
! *  ### 20-FEB-2009  SPD_3D_TOSER  v2.4 (c)  L. Petrov 18-NOV-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( SPD_3D__TYPE   ) :: SPD
      TYPE     ( SPD_DEL__TYPE  ) :: SPD_IN
      INTEGER*4  M_MOD, M_INP, M_FIL
      PARAMETER  ( M_MOD = 512 )
      PARAMETER  ( M_INP = 512 )
      PARAMETER  ( M_FIL = SPD__M_FIL )
      CHARACTER  DIRIN*128, DIRBSPD*128, FILBSPD(M_FIL)*128, FILIN(M_FIL)*128, &
     &           FILOUT*128, PREF_OUT*128, FIL_DAT*21, FILNAM*128, INP_FMT*128, &
     &           MOD_TEXT(M_MOD)*128, INP_TEXT(M_INP)*128, STR*128, MODE*8, POSTFIX*8, &
     &           CH_NL*1
      ADDRESS__TYPE :: DIR_DESC(16)
      REAL*8     TAI_END, FIL_SEC, TIM_STEP, EPS
      PARAMETER  ( EPS = 120.0 )
      INTEGER*4  L_MOD, L_INP, IND_STA, LEV, IS, J1, J2, J3, J4, J5, J6, &
     &           IL, L_FIL, L_BSPD, IVRB, ID, PID, NREC, MJD_END, FIL_MJD, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
#ifdef GNU
      INTEGER*4     NTHR, NTHR_SAVED
#else
      ADDRESS__TYPE NTHR, NTHR_SAVED
#endif
      LOGICAL*4  FL_OUT_TERM 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, GETPID, GET_FILE_FROM_DIR, LINDEX, RENAME, &
     &                       SYSTEM
#ifdef GNU
      LOGICAL*4, INTRINSIC :: ISATTY
      INTRINSIC  FLUSH
      INTEGER*4, EXTERNAL     :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
#else
      LOGICAL*4, EXTERNAL  :: ISATTY
      ADDRESS__TYPE, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS 
#endif
!
      INP_FMT = 'SPD_3D_ASCII_NO_O'
      FL_OUT_TERM = ISATTY ( 6 )
      IF ( FL_OUT_TERM ) THEN
           CH_NL = CHAR(13) ! New line character
         ELSE 
           CH_NL = CHAR(32) 
      END IF
!
      IVRB = 0
      IF ( IARGC() < 3 ) THEN
           IF ( IARGC() .GE. 1 ) THEN
                CALL GETARG ( 1, DIRIN )
                IF ( DIRIN == 'version'   .OR.  &
     &               DIRIN == '--version' .OR.  &
     &               DIRIN == '-v' ) THEN
!
                     WRITE ( 6, '(A)' ) SPD__VERSION
                     CALL EXIT ( 0 )
                 END IF
           END IF
           WRITE ( 6, '(A)' ) 'Usage: spd_3d_toser {dir_in} {pref_out} {create/update/summary} [ivrb]'
           CALL EXIT ( 1 ) 
         ELSE 
           CALL GETARG ( 1, DIRIN )
           CALL GETARG ( 2, PREF_OUT )
           CALL GETARG ( 3, MODE     )
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, STR )
                CALL CHIN   ( STR, IVRB )
           END IF
      END IF
!
      IF ( MODE == 'create' .OR. MODE == 'update' .OR. MODE == 'summary' ) THEN
           CONTINUE 
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6501, IUER, 'SPD_3D_TOSER', 'Unsupported'// &
     &         ' 3rd argument '//MODE(1:I_LEN(MODE))//' only '// &
     &         'create, update, or summary are supported' )
           CALL EXIT ( 1 )
      END IF
!
! --- Set the number of threads. We honor OMP_NUM_THREADS environment variable
!
      CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
      IF ( ILEN(STR) == 0 ) THEN
           NTHR = 1
         ELSE 
           CALL CHIN ( STR, NTHR )
      END IF
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
      NTHR = OMP_GET_MAX_THREADS()
!
! --- Initialize SPD object
!
      IUER = -1
      CALL SPD_INIT ( SPD, IUER )
!
      ID = LINDEX ( PREF_OUT, '/' )
      IF ( ID .LE. 1 ) THEN
           DIRBSPD = '.'
         ELSE 
           DIRBSPD = PREF_OUT(1:ID-1)
      END IF
!
      IF ( MODE == 'update' .OR. MODE == 'summary' ) THEN
!
! -------- Aga. We are in the update mode
!
           IUER = -1
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) '  spd_3d_toser: '//GET_CDATE()//'  '// &
                                   'Read BSPD files from directory '// &
     &                              DIRBSPD(1:I_LEN(DIRBSPD))
           END IF
!
! -------- Initialize SPD_IN object for input SPD delays
!
           CALL SPD_INIT ( SPD_IN, IUER )
!
! -------- Get information about PID of this process
!
           PID = GETPID()
           WRITE ( UNIT=POSTFIX(1:8), FMT='(I8)' ) PID
           CALL BLANK_TO_ZERO ( POSTFIX(1:8) )
!
! -------- Read the BSPD directory, extact relevant bspd files and read their headers
!
           LEV    = 0
           L_BSPD = 0
           NREC   = 0
           DO 410 J1=1,1024*1024
!
! ------------ Get the new file
!
               IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRBSPD, FILNAM )
               IF ( LEV == 0 ) GOTO 810
               IF ( IS .NE. 0 ) THEN
                    IUER = -1
                    CALL ERR_LOG ( 6502, IUER, 'SPD_3D_TOSER', 'Error in '// &
     &                  'reading directory '//DIRBSPD(1:I_LEN(DIRBSPD))//' -- '// &
     &                   FILNAM )
                    CALL EXIT ( 1 )
               END IF
!
! ------------ check its name
!
               IF ( ILEN(FILNAM) < 5 ) GOTO 810
               IF ( FILNAM(ILEN(FILNAM)-4:ILEN(FILNAM)) .NE. '.bspd' ) GOTO 410
               IF ( INDEX ( FILNAM, '#' ) > 0 ) GOTO 410
!
! ------------ Put the file name in the list 
!
               L_BSPD = L_BSPD + 1
               FILBSPD(L_BSPD) = FILNAM
               IF ( IVRB .GE. 2 ) THEN
                    WRITE ( 6, 110 ) J1, L_BSPD, FILBSPD(L_BSPD)(1:I_LEN(FILBSPD(L_BSPD))), CH_NL
                    IF ( .NOT. FL_OUT_TERM ) WRITE ( 6, * ) ' '
                    CALL FLUSH ( 6 )
 110                FORMAT ( '  Read BSPD file ', I6, ' ( ', I6, ' ) ', A, A$ )
               END IF
!
! ------------ Read the file header
!
               IUER = -1
               CALL SPD_3D_BIN_READ_HEAD ( FILNAM, SPD_IN, IUER )
               IF ( IUER .NE. 0 ) THEN
                    IUER = -1
                    CALL ERR_LOG ( 6503, IUER, 'SPD_3D_TOSER', 'Error in '// &
     &                  'reading the header of the input file '//FILNAM )
                    CALL EXIT ( 1 )
               END IF
!
               IF ( NREC == 0 ) THEN
!
! ----------------- This was the first file. Store some fields of the header
!
                    NREC     = SPD_IN%LAB%TOT_NUM_DEL
                    MJD_END  = SPD_IN%TIM%MJD_END
                    TAI_END  = SPD_IN%TIM%TAI_END
                    TIM_STEP = SPD_IN%TIM%TIM_STEP
                  ELSE 
!
! ----------------- This was not the first bspd file. Compare its header parameters
! ----------------- with the parameters from the 1st file. Any discreapancy is considered 
! ----------------- a fatal mistake
!
                    IF ( SPD_IN%LAB%TOT_NUM_DEL .NE. NREC    .OR. &
     &                   SPD_IN%TIM%MJD_END     .NE. MJD_END .OR. &
     &                   SPD_IN%TIM%TAI_END     .NE. TAI_END      ) THEN
                         WRITE ( 6, * ) ' '
                         WRITE ( 6, * ) 'SPD_IN%LAB%TOT_NUM_DEL ', SPD_IN%LAB%TOT_NUM_DEL, ' NREC=    ', NREC
                         WRITE ( 6, * ) 'SPD_IN%TIM%MJD_END= ', SPD_IN%TIM%MJD_END, ' MJD_END= ', MJD_END
                         WRITE ( 6, * ) 'SPD_IN%TIM%TAI_END= ', SPD_IN%TIM%TAI_END, ' TAI_END= ', TAI_END
                         IUER = -1
                         CALL ERR_LOG ( 6504, IUER, 'SPD_3D_TOSER', 'Find that '// &
     &                       'file '//FILBSPD(L_BSPD)(1:I_LEN(FILBSPD(L_BSPD)))// &
     &                       ' has different number of records and/or last epoch '// &
     &                       ' than '//FILBSPD(1) )
!!                         CALL EXIT ( 1 )
                      END IF
               END IF
 410       CONTINUE 
 810       CONTINUE 
           IF ( L_BSPD == 0 .OR. NREC == 0 ) THEN
!
! ------------- No relevant BSPD files were found? Set 'create' mode
!
                MODE = 'create'
           END IF
           IF ( L_BSPD > 0 ) THEN
!
! ------------- Sort bspd files
!
                CALL SORT_FAST_CH ( L_BSPD, FILBSPD )
           END IF
         ELSE 
           NREC    = 0
           MJD_END = 0
           TAI_END = 0.0D0
           TIM_STEP = 0.0D0
      END IF
      IF ( ( MODE == 'update' .OR. MODE == 'summary' ) .AND. IVRB .GE. 2 ) THEN
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, * ) ' '
           ENDIF
           IUER = -1
           STR = MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
           WRITE ( 6, 120 ) STR(1:21), NREC
 120       FORMAT ( '  spd_3d_toser: Last date of data in bspd files is ', A, &
     &              ' They have ', I6 , ' records' )
      END IF
!
! --- Read directory DIRIN with SPD files
!
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) ' spd_3d_toser: Read input directory '//DIRIN(1:I_LEN(DIRIN))
      END IF
      LEV = 0
      L_FIL = 0
      DO 420 J2=1,1024*1024*1024
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRIN, FILNAM )
         IF ( LEV == 0 ) GOTO 820
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 6505, -2, 'SPD_3D_TOSER', 'Error in '// &
     &            'reading directory '//DIRIN(1:I_LEN(DIRIN))//' -- '// &
     &             FILNAM )
              CALL EXIT ( 1 )
         END IF
!
! ------ Check the file name
!
         IL = ILEN(FILNAM)
         IF ( IL < 8 ) GOTO 820
         IF ( FILNAM(IL-7:IL) == '.spd.bz2' ) IL=IL-4
         IF ( FILNAM(IL-3:IL) .NE. '.spd'   ) GOTO 420
         IF ( INDEX ( FILNAM, '#' ) > 0 ) GOTO 420
!
         IF ( MODE == 'update' ) THEN
!
! ----------- Get the date embedded in the file name
!
              FIL_DAT = FILNAM(IL-16:IL-13)//'.'// &
     &                  FILNAM(IL-12:IL-11)//'.'// &
     &                  FILNAM(IL-10:IL-9)//'_'// &
     &                  FILNAM(IL-7:IL-6)//':'// &
     &                  FILNAM(IL-5:IL-4)//':00.0'
!
! ----------- Get the date from extracted from the file name
!
              IUER = -1
              CALL DATE_TO_TIME ( FIL_DAT, FIL_MJD, FIL_SEC, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6506, IUER, 'SPD_3D_TOSER', 'Error in '// &
     &                 'reading directory '//DIRIN(1:I_LEN(DIRIN))//' -- '// &
     &                  FILNAM )
                   CALL EXIT ( 1 )
              END IF
              IF ( IVRB .GE. 3 ) THEN
                   WRITE ( 6, * ) ' FIL_MJD = ', FIL_MJD, ' FIL_SEC= ', FIL_SEC
              END IF
              IF ( FIL_MJD*86400.0 + FIL_SEC - EPS < &
     &             MJD_END*86400.0 + TAI_END         ) THEN
!
! ---------------- Epoch of this file is too early. Skip it
!
                   GOTO 420
              END IF
         END IF
!
! ------ Add this file name to the list
!
         L_FIL = L_FIL + 1
         IF ( L_FIL > M_FIL ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( M_FIL, STR )
              CALL ERR_LOG ( 6507, -2, 'SPD_3D_TOSER', 'Too many spd files '// &
     &            'were found in '//DIRIN(1:I_LEN(DIRIN))//' directory: '// &
     &            'more than '//STR )
              CALL EXIT ( 1 )
         END IF
         FILIN(L_FIL) = FILNAM 
 420  CONTINUE 
 820  CONTINUE 
!
      IF ( L_BSPD > 0 ) THEN
           CALL SORT_FAST_CH ( L_BSPD, FILBSPD )
      END IF
      IF ( MODE == 'summary' ) THEN
           IF ( L_BSPD == 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6508, IUER, 'SPD_3D_TOSER', 'No bdsp '// &
     &              'files was found in directory '//DIRBSPD )
                CALL EXIT ( 1 )
              ELSE
                IF ( ILEN(PREF_OUT) == 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6509, IUER, 'SPD_3D_TOSER', 'The '// &
     &                   'directory with binary slant path delay files '// &
     &                    TRIM(DIRBSPD)//' has no prefix' )
                     CALL EXIT ( 1 )
                END IF
                IUER = -1
                CALL CREATE_BSPD_SUMMARY ( DIRBSPD, L_BSPD, FILBSPD, PREF_OUT, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6510, IUER, 'SPD_3D_TOSER', 'Error in '// &
     &                   'an attempt to generate summary' )
                     CALL EXIT ( 1 )
                END IF
                CALL EXIT ( 0 ) 
           END IF
      END IF
!
      IF ( L_FIL == 0 ) THEN
           IF ( MODE == 'update' .AND. NREC > 0 ) THEN
                IUER = -1
                STR  = MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
                IF ( IVRB .GE. 2 ) THEN
                     WRITE ( 6, '(A)' ) ' '
                     WRITE ( 6, '(A)' ) 'Nothing to update. The last date is '//STR(1:19)
                END IF
!
                IUER = -1
                CALL CREATE_BSPD_SUMMARY ( DIRBSPD, L_BSPD, FILBSPD, PREF_OUT, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 6511, IUER, 'SPD_3D_TOSER', 'Error in '// &
     &                   'an attempt to create BSPD summary' )
                     CALL EXIT ( 1 )
                END IF
!
                CALL EXIT ( 0 )
              ELSE 
                CALL ERR_LOG ( 6512, -2, 'SPD_3D_TOSER', 'No spd files were '// &
     &              'found in '//DIRIN(1:I_LEN(DIRIN))//' directory' )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( IVRB == 7 ) THEN
           WRITE ( 6, * ) 'Quit because IVRB=7'
           CALL EXIT ( 0 )
      END IF
!
! --- Sort files by name. This will force chronological sorting
!
      CALL SORT_FAST_CH ( L_FIL, FILIN )
      IF ( MODE == 'update' .AND. IVRB .GE. 1 ) THEN
           IL = ILEN(FILIN(1))
           IF ( FILIN(1)(IL-7:IL) == '.spd.bz2' ) IL=IL-4
           STR = FILIN(1)(IL-16:IL-13)//'.'// &
     &                         FILIN(1)(IL-12:IL-11)//'.'// &
     &                         FILIN(1)(IL-10:IL-9)//'_'// &
     &                         FILIN(1)(IL-7:IL-6)//':'// &
     &                         FILIN(1)(IL-5:IL-4)//':00.0'
!
           IL = ILEN(FILIN(L_FIL))
           IF ( FILIN(L_FIL)(IL-7:IL) == '.spd.bz2' ) IL=IL-4
           FIL_DAT = FILIN(L_FIL)(IL-16:IL-13)//'.'// &
     &                  FILIN(L_FIL)(IL-12:IL-11)//'.'// &
     &                  FILIN(L_FIL)(IL-10:IL-9)//'_'// &
     &                  FILIN(L_FIL)(IL-7:IL-6)//':'// &
     &                  FILIN(L_FIL)(IL-5:IL-4)//':00.0'
           WRITE ( 6, 130 ) L_FIL, STR(1:21), FIL_DAT(1:21)
 130       FORMAT ( '  Found ', I5, ' new SPD files in a range ', A, 2X, A )
      END IF
      IF ( IVRB == 7 ) THEN
           WRITE ( 6, * ) 'Quit because IVRB=7'
           CALL EXIT ( 0 )
      END IF
!
      IF ( MODE == 'update' ) THEN
!
! -------- In the update mode we for safety make a temporary copy of bspd files.
! -------- We append a suffus with the PID to the file name
!
           IF ( IVRB .GE. 1 ) THEN
                WRITE ( 6, 140 ) L_FIL, L_BSPD
                CALL FLUSH ( 6 )
 140            FORMAT ( '  Number of new spd-files: ', I6, ' Number of bspd files: ', I6  )
           END IF
           DO 430 J3=1,L_BSPD
              IF ( IVRB .GE. 2 ) THEN
                   WRITE ( 6, 150 ) J3, L_BSPD, FILBSPD(J3)(1:I_LEN(FILBSPD(J3))), CH_NL
                   IF ( .NOT. FL_OUT_TERM ) WRITE ( 6, * ) ' '
                   CALL FLUSH ( 6 )
 150               FORMAT ( '  Copy file ', I6, ' ( ', I6, ' ) ', A, A$ )
               END IF
               IS = SYSTEM ( 'cp '//FILBSPD(J3)(1:I_LEN(FILBSPD(J3)))//' '// &
     &                       FILBSPD(J3)(1:I_LEN(FILBSPD(J3)))//'__'//POSTFIX//CHAR(0) )
               IF ( IS .NE. 0 ) THEN
                    CALL CLRCH   ( STR )
                    CALL GERROR  ( STR )
                    CALL ERR_LOG ( 6513, -2, 'SPD_3D_TOSER', 'Error in '// &
     &                  'an attempt to copy file '// &
     &                  FILBSPD(J3)(1:I_LEN(FILBSPD(J3)))//' -- '//STR )
                    CALL EXIT ( 1 )
               END IF
               IS = SYSTEM ( 'chmod g+wr,o+wr '// &
     &                        FILBSPD(J3)(1:I_LEN(FILBSPD(J3)))//'__'//POSTFIX//CHAR(0) )
               FILBSPD(J3) = FILBSPD(J3)(1:I_LEN(FILBSPD(J3)))//'__'//POSTFIX
 430       CONTINUE 
 830       CONTINUE 
      END IF
!
      IF ( MODE == 'create' ) THEN
           L_BSPD = 0
      END IF
!
      DO 440 J4=1,L_FIL
         IF ( IVRB .GE. 2 ) THEN
              WRITE ( 6, 160 ) J4, L_FIL, FILIN(J4)(1:I_LEN(FILIN(J4))), CH_NL
              IF ( .NOT. FL_OUT_TERM ) WRITE ( 6, * ) ' '
              CALL FLUSH ( 6 )
 160          FORMAT ( '  Process file ', I6, ' ( ', I6, ' ) ', A, A$ )
         END IF
!
! ------ Read the SPD ascii file
!
         IUER = -1
         CALL SPD_3D_READ ( FILIN(J4), INP_FMT, SPD, M_MOD, L_MOD, &
     &                      MOD_TEXT, M_INP, L_INP, INP_TEXT, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6514, IUER, 'SPD_3D_TOSER', 'Error in reading '// &
     &            'the input SPD file '//FILIN(J4) )
              CALL EXIT ( 1 )
         END IF
!
! ------ Now we run the cycle over stations
!
!$OMP    PARALLEL DO IF ( NTHR > 1 ), &
!$OMP    PRIVATE ( J5, IUER, IL, FILOUT )
         DO 450 J5=1,SPD%NSTA
!
! --------- Build the name of the output bspd file
!
            CALL CLRCH ( FILOUT )
            FILOUT = PREF_OUT
            IL = ILEN(FILOUT)
            CALL TRAN ( 12, SPD%STA(J5)%NAME, FILOUT(ILEN(FILOUT)+1:) )
            FILOUT = FILOUT(1:IL+8)//'.bspd'
            CALL BLANK_TO_UNDSCR ( FILOUT(1:I_LEN(FILOUT)) )
!
            IF ( MODE == 'update' ) THEN
!
! -------------- Do not forget the suffix in the update mode, since we will update
! -------------- the saved copy
!
                 FILOUT = FILOUT(1:I_LEN(FILOUT))//'__'//POSTFIX
            END IF
            IF ( MODE == 'create' .AND. J4 == 1 ) THEN
!
! -------------- Create the output file for the j5-th file with contents of the J4-th epoch
!
                 IUER = -1
                 CALL SPD_3D_WRITE ( SPD__WRITE_BIN, J5, SPD, FILOUT, &
     &                               L_MOD, MOD_TEXT, L_INP, INP_TEXT, 3, IUER )
                 IF ( IUER .NE. 0 ) THEN
!$OMP                 CRITICAL
                      WRITE ( 6, * ) ' '
                      IUER = -1
                      CALL ERR_LOG ( 6515, IUER, 'SPD_3D_TOSER', 'Error in '// &
     &                    'writing the output file '//FILOUT )
                      CALL EXIT ( 1 )
!$OMP                 END CRITICAL
                 END IF
                 IS = SYSTEM ( 'chmod g+wr,o+wr '//FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
!
!$OMP            CRITICAL
                 L_BSPD = L_BSPD + 1
!$OMP            END CRITICAL
                 FILBSPD(L_BSPD) = FILOUT
              ELSE
!
! -------------- Update the output file with the contents of the J4th epoch
!
                 IUER = -1
                 CALL SPD_3D_WRITE_DEL_ADD ( J4+NREC, J5, SPD, FILOUT, IUER )
                 IF ( IUER .NE. 0 ) THEN
!$OMP                 CRITICAL
                      WRITE ( 6, * ) ' '
                      IUER = -1
                      CALL ERR_LOG ( 6516, IUER, 'SPD_3D_TOSER', 'Error in '// &
     &                    'writing the output file '//FILOUT )
                      CALL EXIT ( 1 )
!$OMP                 END CRITICAL
                 END IF
            END IF
 450     CONTINUE 
!$OMP    END PARALLEL DO
         CALL SPD_FREE ( SPD, 2 ) 
 440  CONTINUE 
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) ' '
      END IF
      IF ( MODE == 'update' ) THEN
!
! -------- Rename files back with locking
!
           IF ( IVRB .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) 'Renaming temporary files'
           END IF
           IUER = -1
           CALL BSPD_MOVE_LOCK ( L_BSPD, FILBSPD, '__'//POSTFIX, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 6517, IUER, 'SPD_3D_TOSER', 'Error in '// &
     &              'an attempt to rename file with postfix '// &
     &              '__'//POSTFIX )
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Created the summary
!
      IUER = -1
      CALL CREATE_BSPD_SUMMARY ( DIRBSPD, L_BSPD, FILBSPD, PREF_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6518, IUER, 'SPD_3D_TOSER', 'Error in '// &
     &         'an attempt to create BSPD summary' )
           CALL EXIT ( 1 )
      END IF
!
! --- Run check 
!
      IUER = -1
      CALL BSPD_CHECK ( DIRBSPD, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6519, IUER, 'SPD_3D_TOSER', 'Check of directory '// &
     &         'with slant path delay in binary format '//TRIM(DIRBSPD)// &
     &         ' detected errors' )
           CALL EXIT ( 1 )
      END IF
!
! --- Hurrah!
!
      IF ( MODE == 'update' ) THEN
           WRITE ( 6, '(A)' ) 'spd_3d_toser  '//GET_CDATE()//'  '// &
                              'Finished update of slant path delay files '// &
     &                        'in directory '//DIRBSPD(1:I_LEN(DIRBSPD))
      END IF
      CALL EXIT ( 0 )
      END  SUBROUTINE  SPD_3D_TOSER  !#!#
