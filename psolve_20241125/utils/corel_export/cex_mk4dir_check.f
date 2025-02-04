      SUBROUTINE CEX_MK4DIR_CHECK ( DATA_DIR, CEX, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine CEX_MK4DIR_CHECK
! *                                                                      *
! * ## 07-APR-2005  CEX_MK4DIR_CHECK  v1.1 (c) L. Petrov  03-SEP-2019 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'corel_export.i'
      TYPE      ( CEX__TYPE ) :: CEX
      CHARACTER  DATA_DIR*(*)
      INTEGER*4  IVRB, IUER
      INTEGER*4  IP, IS, LCHAN, STATB(12), J1, J2, J3, &
     &           L_SCA, L_ROO, NBUF, IER
      ADDRESS__TYPE :: DIR_DESC, SUBDIR_DESC
      INTEGER*4  DIR_BIT, MCHAN
      PARAMETER  ( DIR_BIT = 14 ) ! Bit of status file indicating that the
!                                 ! file is a directory
      PARAMETER  ( MCHAN = 32 )
      CHARACTER  STR*80, NAM1*256, NAM2*256, COREL_DIR*256, SUBDIR_NAME*256
      CHARACTER  STA_NAMES(2)*8, EXPNAME_CH*80, EXPDESC_CH*80, PINAME_CH*80, &
     &           FGROUP_CH(MCHAN)*80, SIDEBAND_CH(MCHAN)*80, RECMODE_CH*80, &
     &           VEX_FINAM*80, BUF(8)*128, FINAM*128
      INTEGER*4  BITS_SAMPLE
      REAL*8     SKY_FREQ(MCHAN), LO_FREQ(MCHAN,2), APLENGTH_R8, SAMPLE_RATE
      INTEGER*2  CHAN_BBC_I2(MCHAN,2)
      LOGICAL*4  FL_VEX, FL_DIG, LEX
      INTEGER*4, EXTERNAL :: FSTREAM, READDIR, FOR_STAT, I_LEN, ILEN
      ADDRESS__TYPE, EXTERNAL :: OPENDIR, CLOSEDIR
!
      FINAM = DATA_DIR(1:I_LEN(DATA_DIR))//'expno.txt'
      INQUIRE ( FILE=FINAM, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4841, IUER, 'CEX_MK4DIR_CHECK', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' was not found' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FINAM, 8, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4842, IUER, 'CEQ_MK4DIR_CHECK', 'Error in reading '// &
     &         'file '//FINAM )
           RETURN 
      END IF
!
      COREL_DIR = DATA_DIR(1:I_LEN(DATA_DIR))//BUF(1)(1:I_LEN(BUF(1)))//'/'
!
! --- Open the data directory
!
      DIR_DESC = OPENDIR ( COREL_DIR(1:I_LEN(COREL_DIR))//CHAR(0) )
      IF ( DIR_DESC .LE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4841, IUER, 'CEX_MK4DIR_CHECK', 'Error in attempt '// &
     &         'to read directory '//COREL_DIR(1:I_LEN(COREL_DIR))// &
     &         '  OPRENDIR: '//STR )
           RETURN
      END IF
      IF ( IVRB .GE. 1 ) THEN
            WRITE ( 6, '(A)' ) 'COREL_EXPORT: Check directory tree '// &
     &                          COREL_DIR(1:I_LEN(COREL_DIR))
      END IF
!
! --- Scan directory filenames
!
      L_SCA = 0
      L_ROO = 0
      DO 410 J1=1,1024*1024 ! (almost) infinite loop for reading the files
!
! ------ Read the next line of the direcotory file
!
         IP = READDIR ( %VAL(DIR_DESC) )
         IF ( IP .EQ. 0 ) GOTO 810
!
! ------ Extract the filename form the internal data structures
!
         CALL GET_NAME_FROM_DIR ( %VAL(IP), NAM1 )
!
! ------ Get the status of the file
!
         IS = FOR_STAT ( COREL_DIR(1:I_LEN(COREL_DIR))//NAM1, STATB )
         FL_DIG = .FALSE.
         DO 420 J2=1,I_LEN(NAM1)
            IF ( INDEX ( '0123456789', NAM1(J2:J2) ) .GT. 0 ) FL_DIG=.TRUE.
 420     CONTINUE
         IF ( .NOT. FL_DIG ) GOTO 410
         IF ( BTEST ( STATB(3), DIR_BIT  )  .AND. NAM1(1:1) .NE. '.' ) THEN
!
! ----------- This file is directory, but not the root directory or current
! ----------- directory. Go further
!
              L_SCA = L_SCA + 1
!
! ----------- Build the full path name of this subdirectory
!
              CALL CLRCH ( SUBDIR_NAME )
              SUBDIR_NAME = COREL_DIR(1:I_LEN(COREL_DIR))//NAM1(1:I_LEN(NAM1))
              SUBDIR_NAME(I_LEN(SUBDIR_NAME)+1:) = '/'
!
! ----------- Open subdirectory
!
              SUBDIR_DESC = OPENDIR ( SUBDIR_NAME(1:I_LEN(SUBDIR_NAME))// &
     &                                CHAR(0) )
              IF ( SUBDIR_DESC .LE. 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 4842, IUER, 'CEX_MK4DIR_CHECK', 'Error '// &
     &                 'in attempt to read subdirectory '// &
     &                  SUBDIR_NAME(1:I_LEN(SUBDIR_NAME))//' '// &
     &                 '  OPRENDIR: '//STR )
                   RETURN
              END IF
!
! ----------- Scan subdirectory in persue of the root file
!
              FL_VEX = .FALSE.
              DO 430 J3=1,1024*1024
!
! -------------- Read the next line of the directory file
!
                 IP = READDIR ( %VAL(SUBDIR_DESC) )
                 IF ( IP .EQ. 0 ) GOTO 830
!
! -------------- Extract the filename form the internal data structures
!
                 CALL GET_NAME_FROM_DIR ( %VAL(IP), NAM2 )
                 IF ( NAM2(1:1) .NE. '.'  .AND.  NAM2(3:3) .NE. '.' ) THEN
!
! ------------------- This file name should be the root file since the main
! ------------------- part of the name is more than 2 characters
!
                      FL_VEX = .TRUE.
                      L_ROO = L_ROO + 1
                      CALL CLRCH ( VEX_FINAM )
                      VEX_FINAM = SUBDIR_NAME(1:I_LEN(SUBDIR_NAME))//NAM2
!
! ------------------- Check the type of the file. It should not be a directory
!
                      IS = FOR_STAT ( VEX_FINAM(1:I_LEN(VEX_FINAM)), STATB )
                      IF ( BTEST ( STATB(3), DIR_BIT  ) ) THEN
                           CALL ERR_LOG ( 4843, IUER, 'CEX_MK4DIR_CHECK', 'File '// &
     &                          VEX_FINAM(1:I_LEN(VEX_FINAM))//' turned out '// &
     &                         'to be directory, but the ovex root file '// &
     &                         'was expected. Check the name of the '// &
     &                         'directory tree '//COREL_DIR(1:I_LEN(COREL_DIR))// &
     &                         ' -- are you sure that you specified correct '// &
     &                         'directory name?' )
                           RETURN
                      END IF
!@!
!@! ------------------- Parse the root file. We assume that it is in VEX format.
!@! ------------------- WE extract some useful stuff there
!@!
!@                      CALL READ_VEX ( VEX_FINAM, STA_NAMES, MCHAN, EXPNAME_CH, &
!@     &                     EXPDESC_CH, PINAME_CH, LCHAN, SKY_FREQ, LO_FREQ, &
!@     &                     APLENGTH_R8, FGROUP_CH, SIDEBAND_CH, CHAN_BBC_I2, &
!@     &                     RECMODE_CH, SAMPLE_RATE, BITS_SAMPLE, INT2(6), IER )
!@                      IF ( IER .NE. 0 ) THEN
!@                           CALL ERR_LOG ( 4844, IUER, 'CEX_MK4DIR_CHECK', 'Error '// &
!@     &                         'in attempt to parse OVEX file '//VEX_FINAM )
!@                           RETURN
!@                      END IF
!@!
!@! ------------------- Transform experiment name to the letters of lower register
!@!
!@                      CALL TRAN ( 12, EXPNAME_CH, EXPNAME_CH )
!@                      IF ( EXPNAME_CH .NE. GEX%SESS_CODE ) THEN
!@                           CALL ERR_LOG ( 4845, IUER, 'CEX_MK4DIR_CHECK', &
!@     &                         'Experiment code mismatch in file '// &
!@     &                          VEX_FINAM(1:I_LEN(VEX_FINAM))//' : "'// &
!@     &                          EXPNAME_CH(1:I_LEN(EXPNAME_CH))//'" while '// &
!@     &                         'Schedule file defines '//GEX%SESS_CODE )
!@                           RETURN
!@                      END IF
!@!
!@! ------------------- Save experiment description line
!@!
!@                      CALL CLRCH ( GEX%SESS_DESC )
!@                      GEX%SESS_DESC = EXPDESC_CH
                 END IF
 430          CONTINUE
 830          CONTINUE
!
! ----------- Close subdirectory
!
              IP = CLOSEDIR ( %VAL(SUBDIR_DESC) )
              IF ( .NOT. FL_VEX ) THEN
                   WRITE ( 6, '(/A)' ) 'Warning: No root file was found in '// &
     &                                 'directory '//NAM1(1:I_LEN(NAM1))
                   WRITE ( 6, '(A)'  ) 'Please check this subdirectory '// &
     &                                 'and main data directory '// &
     &                                  COREL_DIR(1:I_LEN(COREL_DIR))
              END IF
              IF ( IVRB .GE. 1 ) THEN
                   WRITE ( 6, '(I6,") Scan ",A,2X,A$)' ) L_SCA, NAM1(1:32), &
     &                                                   CHAR(13)
              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
!
! --- Close the main directory
!
      IP = CLOSEDIR ( %VAL(DIR_DESC) )
!
      IF ( L_SCA .EQ. 0 ) THEN
           CALL ERR_LOG ( 4846, IUER, 'CEX_MK4DIR_CHECK', 'No subdirectories '// &
     &         'were found in directory '//COREL_DIR(1:I_LEN(COREL_DIR))// &
     &         ' -- please check whether this directory is REALLY correct '// &
     &         ' one'  )
           RETURN
      END IF
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A$)' ) CHAR(13)
           IF ( L_SCA .EQ. L_ROO ) THEN
                WRITE ( 6, '(3X,I8,A)' ) L_SCA, ' scans were found. '// &
     &                                  'Directory tree is OK'
              ELSE
                WRITE ( 6, '(3X,I8,A)' ) L_SCA, ' scans were found, but only ', &
     &                                  L_ROO, ' root files'
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  CEX_MK4DIR_CHECK 
