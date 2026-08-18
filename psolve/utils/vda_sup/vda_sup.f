      PROGRAM    VDA_SUP
! ************************************************************************
! *                                                                      *
! *   Program VDA_SUP processed a file with a VLBI geodetic database in  *
! *   VDA format and generates the output file with suppression status.  *
! *   The output file contains 6 quantities per observation:             *
! *                                                                      *
! *   1) Flag whether the observation is good, i.e. can be used in data  *
! *      analysis for at least one observable or combination of          *
! *      observables.                                                    *
! *   2) Flag whether the observation is recoverable, i.e. can be used   *
! *      in data analysis for a given data type.                         *
! *   3) Flag whether the observation was used in data analysis for      *
! *      a given data type as recorded in a database file.               *
! *   4) Bit field for the automatic suppression status.                 *
! *   5) Bit field for the user action for suppression.                  *
! *   6) Bit field for the user action for restoration.                  *
! *                                                                      *
! *   The output file also contains the data type for which the flags    *
! *   of the use a given observation in a solution is reported.          *
! *                                                                      *
! *   VDA_SUP processes database files compressed with bzip2. It         *
! *   decompresses such a database file on the file to TMP_DIR directory.*
! *   TMP_DIR is /tmp by default. VDA_SUP honors environment variable    *
! *   TMP_DIR that defines an alternative temporary directory.           *
! *                                                                      *
! *   Caveats: databases that uses the old, pre-2006 scheme of           *
! *   suppression flags that have not been processed with modern pSolve  *
! *   have no information about whether a given observation is good or   *
! *   recoverable. VDA_SUP prints a warning in this case. The flags      *
! *   whether a given observations was used are still available.         *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 06-APR-2025    VDA_SUP    v1.0 (d)  L. Petrov  07-APR-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INTEGER*4  MLIN_BYTES, MIND
      PARAMETER  ( MLIN_BYTES = 40 ) 
      PARAMETER  ( MIND = 32 ) 
      CHARACTER  FIL_VDA*128, FIL_INP*128, DATYP_STR*6, STR*128, VDA_MAGIC*27, &
     &           VDA_SUP__MAGIC*70, DB_NAME*10, OUT(MAX_OBS+128)*128, &
     &           VDA_SUP__LABEL*17, COM*256, TMP_DIR*128, FILOUT*128
      CHARACTER  DATYP__ABR__ARR(FIRST__DTP:LAST__DTP)*6, DATYP__DEF_STR*6
      PARAMETER  ( VDA_SUP__LABEL = 'vda sup  20250407' )
      PARAMETER  ( VDA_SUP__MAGIC = '# Suppression status of the VLBI experiment. Format version 2025.04.06' )
      PARAMETER  ( VDA_MAGIC = 'VGOSDA Format of 2019.09.09' )
      PARAMETER  ( DATYP__DEF_STR = 'G_GXS ' )
      CHARACTER, ALLOCATABLE :: VDA(:)*128
      LOGICAL*4  FL_USED, FL_GOOD, FL_RECO, FL_COMPRESS, FL_NO_AUTOSUP, LEX
      INTEGER*8  SIZE_I8
      INTEGER*4  NL, MP, NP, UNIX_DATE, IVAL, IS, ID, NO, IL, J1, J2, J3, J4, &
     &           LIND, IND(2,MIND), KIND, AUTO_SUP(MAX_OBS), USER_SUP(MAX_OBS), &
     &           USER_REC(MAX_OBS), N_GOOD, N_USED, N_RECO, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: FILE_INFO, ILEN, I_LEN, LINDEX, LTM_DIF, UNLINK
      LOGICAL*4, EXTERNAL :: META_SUPR_INQ, SUPR_INQ
!
      TMP_DIR = '/tmp'
!
! --- Get paramgeters
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, * ) 'Usage: vda_sup vda_file filout [datyp]'
           CALL EXIT ( 0 ) 
         ELSE
           CALL GETARG ( 1, FIL_VDA )
           CALL GETARG ( 2, FILOUT  )
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 2, DATYP_STR )
                IDATYP = LTM_DIF ( 0, LAST__DTP, DATYP__ABR__ARR,  DATYP_STR ) - 1
              ELSE 
                CALL MEMCPY ( DATYP__ABR__ARR, DATYP__ABR, %VAL(DATYP__LEN) )
                IDATYP = LTM_DIF ( 0, LAST__DTP, DATYP__ABR__ARR,  DATYP__DEF_STR ) - 1
                CALL CLRCH ( DATYP_STR )
           END IF           
      END IF
!
      FL_NO_AUTOSUP = .FALSE.
      FL_COMPRESS   = .FALSE.
      IL = ILEN(FIL_VDA)
      IF ( IL > 5 ) THEN
!
! -------- Check the suffix of the input data file.
! -------- If the suffix is .bz2, run file decompression
!
           IF ( FIL_VDA(IL-3:IL) == '.bz2' ) THEN
                FL_COMPRESS = .TRUE.
!
! ------------- Create a  temporary file in TMP_DIR directory
!
                ID = LINDEX ( FIL_VDA, '/' ) + 1
!
! ------------- Check for the environment variable TMP_DIR
!
                CALL GETENVAR ( 'TMP_DIR', STR )
                IF ( ILEN(STR) > 0 ) THEN
                     TMP_DIR = STR
                END IF
                FIL_INP = TRIM(TMP_DIR)//'/'//FIL_VDA(ID:IL-4)
              ELSE
                FIL_INP = FIL_VDA
           END IF
           IF ( FL_COMPRESS ) THEN
!
! ------------- Check whether the temporary output file exist. 
!
                INQUIRE ( FILE=FIL_INP, EXIST=LEX )
                IF ( LEX ) THEN
!
! ------------------ If exist, then remove it
!
                     IS = UNLINK ( TRIM(FIL_INP)//CHAR(0) )
                     IF ( IS .NE. 0 ) THEN
                          CALL GERROR  ( STR )
                          CALL ERR_LOG ( 5601, IUER, 'VDA_SUP', 'Error in an attempt '// &
     &                        'to remove an existing temporay file '//TRIM(FIL_INP)// &
     &                        ' because of '//TRIM(STR)//'. You may assign TMP_DIR '// &
     &                        ' environment variable to circumbent the problem' )
                          CALL EXIT ( 1 )
                     END IF
                END IF
                COM = 'lbzip2 -dkc '//TRIM(FIL_VDA)//' > '//FIL_INP
                IS = SYSTEM ( TRIM(COM)//CHAR(0) ) 
                IF ( IS .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 5601, IUER, 'VDA_SUP', 'Error in an attempt '// &
     &                   'to decompress the input file with command '//COM )
                     CALL EXIT ( 1 )
                END IF 
           END IF
      END IF
!
! --- Check the file size
!
      IS = FILE_INFO ( TRIM(FIL_INP)//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           IUER = -1
           CALL ERR_LOG ( 5602, IUER, 'VDA_SUP', 'Error in an attempt '// &
     &         'to collect information about input VLBI database file '// &
     &          TRIM(FIL_INP)//' : '//STR )
           CALL EXIT ( 1 )
      END IF
!
! --- Determine the number of records
!
      MP = SIZE_I8/MLIN_BYTES
!
! --- ... and allocate dynamic memory for the contents of the input file
!
      ALLOCATE ( VDA(MP) )
!
! --- Read input file
!
      IUER = -1
      CALL RD_TEXT ( FIL_INP, MP, VDA, NP, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5603, IUER, 'VDA_SUP', 'Error in an attempt '// &
     &         'to read the input VLBI database file '//TRIM(FIL_INP)// &
     &         ' : '//STR )
           CALL EXIT ( 1 )
      END IF
      IF ( FL_COMPRESS ) THEN
!
! -------- Remove a temporary decompressed input database file 
!
           IS = UNLINK ( TRIM(FIL_INP)//CHAR(0) )
      END IF
!
! --- Check the first line (magic)
!
      IF ( VDA(1)(1:LEN(VDA_MAGIC)) .NE. VDA_MAGIC ) THEN
           STR = VDA(1)
           CALL TRAN ( 13, STR, STR )
           IUER = -1
           CALL ERR_LOG ( 5604, IUER, 'VDA_SUP', 'Wrong format of the '// &
     &         'the input VLBI database file '//TRIM(FIL_VDA)// &
     &         ' : the first line is '//TRIM(STR)//' while VDA magic '// &
     &         VDA_MAGIC//' was expected' )
           CALL EXIT ( 1 )
      END IF
!
      KIND = 0
      DO 410 J1=1,NP
         IF ( INDEX ( VDA(J1), 'DATA.4 AUTO_SUP' ) > 0 .OR. &
     &        INDEX ( VDA(J1), 'DATA.4 USER_SUP' ) > 0 .OR. &
     &        INDEX ( VDA(J1), 'DATA.4 USER_REC' ) > 0 .OR. &
     &        INDEX ( VDA(J1), 'DATA.4 DATYP'    ) > 0 .OR. &
     &        INDEX ( VDA(J1), 'DATA.1 EXP_CODE' ) > 0 .OR. &
     &        INDEX ( VDA(J1), 'FILE.1 /'        ) > 0      ) THEN
!
! ----------- Splic the line into words
!
              CALL EXWORD ( VDA(J1), MIND, LIND, IND, ' ', IUER )
              IF ( LIND .GE. 3 ) THEN
                   CALL CHIN ( VDA(J1)(IND(1,3):IND(2,3)), KIND )
              END IF
!
! ----------- ... and extract fields (also known as lcodes)
!
              IF ( VDA(J1)(IND(1,2):IND(2,2)) == 'AUTO_SUP' ) THEN
                   CALL CHIN ( VDA(J1)(IND(1,7):IND(2,7)), AUTO_SUP(KIND) )
                ELSE IF ( VDA(J1)(IND(1,2):IND(2,2)) == 'USER_SUP' ) THEN
                   CALL CHIN ( VDA(J1)(IND(1,7):IND(2,7)), USER_SUP(KIND) )
                ELSE IF ( VDA(J1)(IND(1,2):IND(2,2)) == 'USER_REC' ) THEN
                   CALL CHIN ( VDA(J1)(IND(1,7):IND(2,7)), USER_REC(KIND) )
                ELSE IF ( VDA(J1)(IND(1,2):IND(2,2)) == 'DATYP' ) THEN
                   IF ( ILEN(DATYP_STR) == 0 ) THEN
                        CALL CHIN ( VDA(J1)(IND(1,7):IND(2,7)), IVAL )
                        IDATYP = IVAL
                   END IF
                ELSE IF ( VDA(J1)(IND(1,2):IND(2,2)) == 'EXP_CODE' ) THEN
                   EXP_CODE = VDA(J1)(IND(1,7):IND(2,7))
                ELSE IF ( VDA(J1)(IND(1,1):IND(2,1)) == 'FILE.1' ) THEN
                   ID = LINDEX ( VDA(J1)(IND(1,2):IND(2,2)), '/' )
                   DB_NAME = VDA(J1)(IND(1,2)+ID:IND(1,2)+ID+10)
              END IF
         END IF
 410  CONTINUE 
!
! --- Prepare the header of the output file
!
      NO = 0
      NO = NO + 1; OUT(NO) = VDA_SUP__MAGIC
      NO = NO + 1; OUT(NO) = '# '
      NO = NO + 1; OUT(NO) = '# VLBI experiment code: '//EXP_CODE 
      NO = NO + 1; OUT(NO) = '# VLBI database name:   '//DB_NAME
      NO = NO + 1; OUT(NO) = '# '
      NO = NO + 1; OUT(NO) = '# Generated by '//VDA_SUP__LABEL
      NO = NO + 1; OUT(NO) = '# Using database file '//FIL_VDA
      NO = NO + 1; OUT(NO) = '# on '//GET_CDATE()
      NO = NO + 1; OUT(NO) = '# '
      NO = NO + 1; OUT(NO) = '# Data type: '//DATYP__ABR__ARR(IDATYP)
      NO = NO + 1; OUT(NO) = '# '
      NO = NO + 1; OUT(NO) = '# --------------------------------------------------------------------------------'
      NO = NO + 1; OUT(NO) = '# Bytes  Format Units Label   Explanations'
      NO = NO + 1; OUT(NO) = '# --------------------------------------------------------------------------------'
      NO = NO + 1; OUT(NO) = '#  6-12  I7     ---   IOBS     Observation index'
      NO = NO + 1; OUT(NO) = '# 15-15  L1     ---   FL_GOOD  flag whether this is observation good'
      NO = NO + 1; OUT(NO) = '# 17-17  L1     ---   FL_RECO  flag whether this is observation recoverable'
      NO = NO + 1; OUT(NO) = '# 19-19  L1     ---   FL_USED  flag whether this is observation is used in a solution'
      NO = NO + 1; OUT(NO) = '# 22-53  B32    ---   AUTO_SUP Bit field of the automatic suppression '
      NO = NO + 1; OUT(NO) = '# 55-86  B32    ---   USER_SUP Bit field of the user action for suppression '
      NO = NO + 1; OUT(NO) = '# 88-119 B32    ---   USER_REC Bit field of the user action for restoration '
      NO = NO + 1; OUT(NO) = '# --------------------------------------------------------------------------------'
      NO = NO + 1; OUT(NO) = '# '
      N_USED = 0
      N_GOOD = 0
      N_RECO = 0
      DO 420 J2=1,KIND
         NO = NO + 1
         IF ( .NOT. BTEST ( AUTO_SUP(J2), INIT__SPS ) ) THEN
!
! ----------- The database uses the old, pre-2006 suppression scheme.
! ----------- We set bit INIT__SPS to initializse AUTO_SUP, howether
! ----------- infromation about auto-supression is not present.
! ----------- Therefore, whether a given observations is good or 
! ----------- recoverable is not available
!
              AUTO_SUP(J2) = IBSET ( AUTO_SUP(J2), INIT__SPS )
              FL_NO_AUTOSUP = .TRUE.
         END IF
         IF ( BTEST ( AUTO_SUP(J2), INIT__SPS ) ) THEN
!
! ----------- Check whether a given obsevations was recoverable, good,
! ----------- and used in a soluition
!
              FL_RECO = META_SUPR_INQ ( AUTO_SUP(J2), USER_SUP(J2), USER_REC(J2), RECO__SPS )
              FL_GOOD = META_SUPR_INQ ( AUTO_SUP(J2), USER_SUP(J2), USER_REC(J2), GOOD__SPS )
              FL_USED = META_SUPR_INQ ( AUTO_SUP(J2), USER_SUP(J2), USER_REC(J2), USED__SPS )
         END IF
!
! ------ Update counters
!
         IF ( FL_USED ) N_USED = N_USED + 1
         IF ( FL_GOOD ) N_GOOD = N_GOOD + 1
         IF ( FL_RECO ) N_RECO = N_RECO + 1
!
! ------ Write the output record
!
         WRITE ( UNIT=OUT(NO), FMT=110 ) J2, FL_USED, FL_GOOD, FL_RECO, AUTO_SUP(J2), &
     &                                   USER_SUP(J2), USER_REC(J2)
         CALL BLANK_TO_ZERO ( OUT(NO)(22:53)  ) 
         CALL BLANK_TO_ZERO ( OUT(NO)(55:86)  ) 
         CALL BLANK_TO_ZERO ( OUT(NO)(88:119) ) 
 110     FORMAT ( 'Obs: ', I7, 1X, 3(1X, L1), 1X, 3(1X, B32) )
 420  CONTINUE 
      IF ( KIND == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5605, IUER, 'VDA_SUP', 'No observations '// &
     &         'were found in the input VLBI database file '//FIL_VDA )
           CALL EXIT ( 1 )
      END IF
!
! --- Write the trailer
!
      NO = NO + 1; OUT(NO) = '# '
      IF ( .NOT. FL_NO_AUTOSUP ) THEN
           CALL INCH   ( N_RECO, STR(1:7) )
           CALL CHASHR (         STR(1:7) )
           NO = NO + 1; OUT(NO) = '# Number of recoverable observation: '//STR(1:7)
           CALL INCH   ( N_GOOD, STR(1:7) )
           CALL CHASHR (         STR(1:7) )
           NO = NO + 1; OUT(NO) = '# Number of good        observation: '//STR(1:7)
         ELSE 
           NO = NO + 1; OUT(NO) = '# WARNING: automatic suppression flag was not stored in the database'
           NO = NO + 1; OUT(NO) = '# Therefore, the reported number of recoverable and good observations'
           NO = NO + 1; OUT(NO) = '# is unknown. The database '//EXP_CODE//' should be reprocessed with pSolve'
           NO = NO + 1; OUT(NO) = '# '
      END IF
      CALL INCH   ( N_USED, STR(1:7) )
      CALL CHASHR (         STR(1:7) )
      NO = NO + 1; OUT(NO) = '# Number of used        observation: '//STR(1:7)
      NO = NO + 1; OUT(NO) = '# '
!
! --- Write down the output file
!
      IUER = -1
      CALL WR_TEXT ( NO, OUT, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5605, IUER, 'VDA_SUP', 'Error in an attempt '// &
     &         'to write the output file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM  VDA_SUP  !#!#
