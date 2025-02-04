      PROGRAM    EPHEDISP_TO_BIN_LAUNCH
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = MALO__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL EPHEDISP_TO_BIN()
      END  PROGRAM  EPHEDISP_TO_BIN_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EPHEDISP_TO_BIN()
! ************************************************************************
! *                                                                      *
! *   Program  EPHEDISP_TO_BIN transforms the set of files of time       *
! *   series of site displacements in EPHEDISP format to the set of      *
! *   files of site displacements in BINDISP format.                     *
! *                                                                      *
! *   It is assumed that all EPHEDISP files are located in the specified *
! *   directory. All these files has a common prefix. All files which    *
! *   have this prefix are in EPHEDISP format and belong to the family   *
! *   of consecutive site displacement files.                            *
! *                                                                      *
! *   Each site displacement file in EPEHDISP format contains            *
! *   displacements for all sites within the specified range. Each       *
! *   site displacement file in BINDISP contains displacement for this   *
! *   specific site for all consecutive epochs.                          *
! *                                                                      *
! *   EPHEDISP_TO_BIN is designed to work in the environment when        *
! *   another process simultaneously reads BINDISP files. It writes      *
! *   BINDISP files first into the temporary filenames. Then it checks   *
! *   the write lock file. If the lock is set EPHEDISP_TO_BIN sets read  *
! *   lock and waits for lifting the lock. When the write lock is        *
! *   lifted, it renames temporary filenames into the permanent          *
! *   filenames. Then it removes read lock file.                         *
! *                                                                      *
! *   EPHEDISP_TO_BIN creates the summary file with information on       *
! *   station names, station coordinates and the dates range.            *
! *                                                                      *
! *    Usage: ephedisp_to_bin <ephe_prefix> <output_dir> [date_begin]    *
! *                           [date_end] [verbosity_level]               *
! *    <ephe_prefix> -- Prefix of the full path of the displacement      *
! *                     file(s) in EPHEDISP format. All files which      *
! *                     begin from this prefix will be selected in       *
! *                     alphabetic order. It is assumed the file names   *
! *                     are selected in such a way that the alphabetic   *
! *                     order corresponds to the chronological order.    *
! *                     EPHEDISP_TO_BIN checks for the order of selected *
! *                     files. If it finds at least one point out of     *
! *                     order it generates the fatal error message and   *
! *                     terminates.                                      *
! *    <output_dir>  -- directory name where the set of output site      *
! *                     displacement files in BINDISP format is to be    *
! *                     put.                                             *
! *    [date_begin]  -- if specified then defines the date of the first  *
! *                     epoch. If EPHEDISP files define displacements    *
! *                     for earlier epochs, these values will be         *
! *                     discarded. Format: YYYY.DD.MM-hh:mm:ss or begin  *
! *    [date_end]    -- if specified then defines the date of the last   *
! *                     epoch. If EPHEDISP file define displacements     *
! *                     for earlier epochs, these values will be         *
! *                     discarded. Format: YYYY.DD.MM-hh:mm:ss or end    *
! *    [verbosity_level] -- verbosity level.                             *
! *                         0 -- silent mode,                            *
! *                         1 -- more verbose mode,                      *
! *                         2 --  shows progress.                        *
! *                                                                      *
! *    Example: /users/lpetrov/bin/ephedisp_to_bin \                     *
! *             /sol9/space/users/epavlis/scripts/2cmp/TEST/vsgd_2007_ \ *
! *             $HOME/TEST_BIN \                                         *
! *             2007.01.03-00:00:00 \                                    *
! *             2007.05.31-24:00:00 \                                    *
! *             2                                                        *
! *                                                                      *
! *  All files which beginns with                                        *
! *      /sol9/space/users/epavlis/scripts/2cmp/TEST/vsgd_2007_  are     *
! *      selected in chronological order. In this example, the directory *
! *      /sol9/space/users/epavlis/scripts/2cmp/TEST/ had these files:   *
! *      /sol9/space/users/epavlis/scripts/2cmp/TEST/vsgd_2007_03.eph    *
! *      /sol9/space/users/epavlis/scripts/2cmp/TEST/vsgd_2007_05.eph    *
! *      /sol9/space/users/epavlis/scripts/2cmp/TEST/vsgd_2007_04.eph    *
! *      /sol9/space/users/epavlis/scripts/2cmp/TEST/bin_aa              *
! *      /sol9/space/users/epavlis/scripts/2cmp/TEST/0.test              *
! *                                                                      *
! *  These files were selected (in this order):                          *
! *      /sol9/space/users/epavlis/scripts/2cmp/TEST/vsgd_2007_03.eph    *
! *      /sol9/space/users/epavlis/scripts/2cmp/TEST/vsgd_2007_04.eph    *
! *      /sol9/space/users/epavlis/scripts/2cmp/TEST/vsgd_2007_05.eph    *
! *                                                                      *
! * ### 09-DEC-2002 EPHEDISP_TO_BIN  v3.3 (c) L. Petrov  29-DEC-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'malo.i'
      INCLUDE   'bindisp.i'
      TYPE ( BINDISP_DATA     ), POINTER :: BDS(:,:)
      TYPE ( BINDISP_HEADER_2 ) ::  HDR2
      TYPE ( BINDISP_HEADER_4 ) ::  HDR4
      TYPE ( BINDISP_HEADER_8 ) ::  HDR8
      TYPE ( BDSSUM_STAREC    ) ::  BDSUM
      TYPE ( BINDISP_MODEL    ) ::  BIN_MOD
      TYPE ( MALO__TYPE       ) ::  MALO
      REAL*8       TIM_EPS, RD_AREA
      INTEGER*4    M_STA, M_EPC, M_EPC_FIL
      PARAMETER  ( M_STA = 8192 ) 
      PARAMETER  ( M_EPC = 512*1024   ) 
      PARAMETER  ( M_EPC_FIL = 256    ) 
      PARAMETER  ( TIM_EPS = 128.0D0  )
      PARAMETER  ( RD_AREA = 3000.0D0 )
      CHARACTER, ALLOCATABLE :: FILS(:)*128
      REAL*8,    ALLOCATABLE :: DSPL(:,:,:)
      CHARACTER  EPHE_PREF*128, OUTPUT_DIR*128, DATE_BEG_REQ*20, DATE_END_REQ*20
      CHARACTER  C_STA(M_STA)*8, STR*256, STR1*256, &
     &           USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128, &
     &           POSTFIX*128, FILIN*128, &
     &           FILOUT(M_STA)*128, HEADER(M__HDR)*(LEN__HDR), &
     &           DATE_FIL*21, FILSUM*128, FILNAM*128, DIRNAM*128
      REAL*8     UEN_TO_XYZ(3,3,M_STA), DSP_CFS(3), SEC_BEG_REQ, SEC_END_REQ, &
     &           TAI_END, TAI_BEG, TAI_EPC, TIM_GAP, TAI_FIL, &
     &           SEC_BEG_STA(M_STA), SEC_END_STA(M_STA), SEC_VAL, &
     &           BDS_TAI_BEG, BDS_TAI_END, SMP_INTRV, TIM_INT
      INTEGER*2  KX, KY, KZ
      INTEGER*4  MJD_BEG_REQ, MJD_END_REQ, MJD_TAG, MJD_STA(M_EPC), &
     &           MJD_EPC, MJD_END, MJD_BEG, MJD_FIL, PID, LEV, IB, &
     &           MJD_BEG_STA(M_STA), MJD_END_STA(M_STA), K_DSP_STA(M_STA), &
     &           MJD_VAL, IR, N_STA, IDAY, BDS_MJD_BEG, BDS_MJD_END, UNIX_DATE, &
     &           LREC, NREC, NTIM
      LOGICAL*1  FL_PREF 
      INTEGER*8  SIZE_I8
      ADDRESS__TYPE :: DIR_DESC(16)
      CHARACTER  ENDIAN_FMT*1, FLOAT_FMT
#ifdef BIG_ENDIAN
      PARAMETER  ( ENDIAN_FMT = 'B' ) ! ENDIAN defiend as preprocessor D option
#else
! if LITTLE_ENDIAN
      PARAMETER  ( ENDIAN_FMT = 'L' ) ! ENDIAN defiend as preprocessor D option
#endif
      PARAMETER  ( FLOAT_FMT  = 'I' ) ! IEEE 754/854 float format
      REAL*8     EPS_SEC
      PARAMETER  ( EPS_SEC = 1.0D0 )
      CHARACTER  MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4  L_FIL, L_STA, L_BTS, IVRB, ISTA, KSTA, LUN, IOS, IS,  &
     &           I_FIL, IP, I_STA, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           J10, J11, K_STA, J_EPC, K_EPC, L_EPC, N_EPC, &
     &           L_EPC_FIL, IL, SEEK_SET, ARG_LEN, IUER
      INTEGER*8  K_DSP
      REAL*8,    EXTERNAL :: MJD_SEC_TO_JD
      INTEGER*4, EXTERNAL :: GET_UNIT, GETPID, ILEN, I_LEN, LINDEX, LTM_DIF, &
     &                       GET_FILE_FROM_DIR, FILE_INFO, READ
      ADDRESS__TYPE, EXTERNAL :: LSEEK
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
!
      IF ( IARGC() .LT. 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: ephedisp_to_bin <ephe_prefix> '// &
     &                        '<output_dir> [date_begin] '// &
     &                        '[date_end] [verbosity_level]'
           CALL EXIT ( 1 )
         ELSE
!
! -------- Get arguments
!
           CALL GETARG ( 1, EPHE_PREF  )
           CALL GETARG ( 2, OUTPUT_DIR )
           IF ( OUTPUT_DIR(I_LEN(OUTPUT_DIR):I_LEN(OUTPUT_DIR)) .NE. '/' ) THEN
                OUTPUT_DIR = OUTPUT_DIR(1:I_LEN(OUTPUT_DIR))//'/'
           END IF
!
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, DATE_BEG_REQ )
              ELSE
                DATE_BEG_REQ = 'begin'
           END IF
!
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, DATE_END_REQ )
              ELSE
                DATE_END_REQ = 'end'
           END IF
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, STR )
                CALL CHIN   ( STR, IVRB )
              ELSE
                IVRB = 0
           END IF
      END IF
!
! --- Decode argument date_beg
!
      IF ( DATE_BEG_REQ .EQ. 'update' ) THEN
           IUER = -1
           CALL LEARN_BDS_START_INTRV ( OUTPUT_DIR,  SMP_INTRV,   &
     &                                  BDS_MJD_BEG, BDS_TAI_BEG, &
     &                                  BDS_MJD_END, BDS_TAI_END, &
     &                                  L_STA, C_STA, IUER  )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4801, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &              'an attempt lear the start/stop dates and sampling '// &
     &              'interval from the summary file' )
                CALL EXIT ( 1 )
           END IF
           TIM_INT = SMP_INTRV
         ELSE IF ( DATE_BEG_REQ .EQ. 'begin' ) THEN
           L_STA = MALO__MSTA
         ELSE 
           L_STA = MALO__MSTA
           IUER = -1
           CALL DATE_TO_TIME ( DATE_BEG_REQ, MJD_BEG_REQ, SEC_BEG_REQ, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4802, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &              'an attempt to decode input date '//DATE_BEG_REQ )
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Decode argument date_end
!
      IF ( DATE_END_REQ .NE. 'end' ) THEN
           IUER = -1
           CALL DATE_TO_TIME ( DATE_END_REQ, MJD_END_REQ, SEC_END_REQ, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4803, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &              'an attempt to decode input date '//DATE_END_REQ )
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Check the argument
!
      IL = ILEN(EPHE_PREF)
      IF ( IL .LE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4804, IUER, 'EPHEDISP_TO_BIN', 'Argument '// &
     &          'EPHE_PREF is an empty string' )
           RETURN
      END IF
!
! --- Extract directory name portion of the prefix
!
      CALL CLRCH ( DIRNAM )
      IB = LINDEX ( EPHE_PREF, '/' )
      IF ( IB .GT. 0 ) THEN
           DIRNAM = EPHE_PREF(1:IB)
         ELSE
!
! -------- Relative file name...
!
           DIRNAM = './'
      END IF
!
      IF ( IB .EQ. IL ) THEN
           IUER = -1
           CALL ERR_LOG ( 4805, IUER, 'EPHEDISP_TO_BIN', 'Argument '// &
     &         'EPHE_PREF has the last character / and, therefore, specifies '// &
     &         'directory' )
           RETURN
      END IF
!
      ALLOCATE  ( FILS(M_EPC), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           IUER = -1
           CALL ERR_LOG ( 4806, IUER, 'EPHEDISP_TO_BIN', 'Error in an '// &
     &                   'attempt to allocate '//STR(1:I_LEN(STR))// &
     &                   ' bytes of dynamic memory for FILS' )
           CALL EXIT ( 1 ) 
      END IF
      L_FIL = 0
!
! --- Recursive walk over the directory tree
!
      FL_PREF = .FALSE.
      LEV = 0
      DO 410 J1=1,1024*1024*1024
         CALL CLRCH ( FILNAM )
         IP = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRNAM, FILNAM )
         IF ( IP .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4807, IUER, 'EPHEDISP_TO_BIN', FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV .EQ. 0 ) GOTO 810
!
! ------ Do not consider garbage files with tilda and # character
!
         IF ( FILNAM(I_LEN(FILNAM):I_LEN(FILNAM)) .EQ. '~' ) GOTO 410
         IF ( INDEX ( FILNAM, '#' ) .GT. 0 )  GOTO 410
!
! ------ For some poorely understood reasons, there maybe files with "//"
! ------ characteres. We have to replaces them with "/", otherwise sorting
! ------ fill be wrong
!
         DO 420 J2=1,16
            IP = INDEX ( FILNAM, '//' ) 
            IF ( IP .GT. 0 ) THEN
                 FILNAM = FILNAM(1:IP)//FILNAM(IP+2:)
               ELSE
                 GOTO 820
            END IF
 420     CONTINUE 
 820     CONTINUE 
!
! ------ Check whether the file name has the same prefix a FIL_PREF
!
         IP = LINDEX ( FILNAM, '/' ) + 1
         IF ( FILNAM(IP:IP+IL-IB-1) .EQ. EPHE_PREF(IB+1:IL) ) THEN
              IR = ILEN(FILNAM)
              IF ( FILNAM(IR-17:IR-17) == '_' ) THEN
                   DATE_FIL = FILNAM(IR-16:IR-13)//'.'//FILNAM(IR-12:IR-11)//'.'// &
     &                        FILNAM(IR-10:IR-6)//':'//FILNAM(IR-5:IR-4)//':00.0'
                 ELSE 
                   DATE_FIL = FILNAM(IR-10:IR-7)//'.'//FILNAM(IR-5:IR-4)//'.'// &
     &                        '01_00:00:00.0'
              END IF
              IUER = -1
              CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TAI_FIL, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4808, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &                 'an attempt to decode date '//DATE_FIL// &
     &                 ' in the filename: '//FILNAM )
                  CALL EXIT ( 1 )
              END IF
              FL_PREF = .TRUE.
              IF ( DATE_BEG_REQ .EQ. 'begin' ) THEN
                   CONTINUE 
                ELSE IF ( DATE_BEG_REQ .EQ. 'update' ) THEN
                   IF ( (MJD_FIL*86400.0D0 + TAI_FIL) + TIM_EPS < &
     &                  (BDS_MJD_END*86400.0D0 + BDS_TAI_END + SMP_INTRV) ) GOTO 410
                ELSE
                   IF ( (MJD_FIL*86400.0D0 + TAI_FIL) + TIM_EPS < &
     &                  (MJD_BEG_REQ*86400.0D0 + SEC_BEG_REQ)     ) GOTO 410
              END IF
              IF ( DATE_END_REQ .NE. 'end' ) THEN
                   IF ( (MJD_FIL*86400.0D0 + TAI_FIL) - TIM_EPS > &
     &                  (MJD_END_REQ*86400.0D0 + SEC_END_REQ)     ) GOTO 410
              END IF
              L_FIL = L_FIL + 1
              FILS(L_FIL) = FILNAM
         END IF
 410  CONTINUE
 810  CONTINUE
      IF ( L_FIL .EQ. 0 ) THEN
           IF ( DATE_BEG_REQ .EQ. 'update' .AND. FL_PREF ) THEN
                IUER = -1
                STR  = MJDSEC_TO_DATE ( BDS_MJD_END, BDS_TAI_END, IUER )
                WRITE ( 6, '(A)' ) 'Nothing to update. The last date is '//STR(1:19)
                CALL EXIT ( 0 )
              ELSE 
                IUER = -2
                CALL ERR_LOG ( 4809, IUER, 'EPHEDISP_TO_BIN', 'No file '// &
     &              'with prefix '//EPHE_PREF(1:IL)//' was found' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Sort the array of file names in alphabetic order (it is equivalent to
! --- chronological order according to file naming convention)
!
      CALL SORT_FAST_CH ( L_FIL, FILS )
!
! --- Read the 1-st EPEHDISP files. Result of reading is put in STA record
!
      IF ( IVRB > 3 ) THEN
           WRITE ( 6, * ) 'FILS(1)   = ', TRIM(FILS(1))
      END If
      IUER = -1
      CALL MALO_EPHEDISP_READ ( FILS(1), MALO, %VAL(0), MALO__INQ, L_STA, &
     &                          L_EPC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4810, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &        'an attempt to read the first file in ephedisp format '// &
     &         FILS(1) )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB > 3 ) THEN
           WRITE ( 6, * ) 'L_STA     = ', L_STA
           WRITE ( 6, * ) 'MALO%NSTA = ', MALO%NSTA
      END IF
!
      IF ( L_EPC == 0 ) L_EPC = 1
      MJD_BEG   = MALO%MJD_BEG
      TAI_BEG   = MALO%TAI_BEG
      IF ( DATE_BEG_REQ == 'begin' ) THEN
           TIM_INT = MALO%TIM_STEP
        ELSE IF ( DATE_BEG_REQ == 'update' ) THEN
           CONTINUE 
        ELSE
!
! -------- Read the second file
!
           IUER = -1
           CALL MALO_EPHEDISP_READ ( FILS(2), MALO, %VAL(0), MALO__INQ, L_STA, &
     &                               L_EPC, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 4811, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &              'an attempt to read the second file in ephedisp format '// &
     &               FILS(2) )
                CALL EXIT ( 1 )
           END IF
           TIM_INT = (MALO%MJD_BEG - MJD_BEG)*86400.0D0 + (MALO%TAI_BEG - TAI_BEG)
      END IF
!
! --- Read the last EPEHDISP files. Result of reading is put in STA record
!
      IUER = -1
      CALL MALO_EPHEDISP_READ ( FILS(L_FIL), MALO, %VAL(0), MALO__INQ, L_STA, &
     &                          L_EPC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4812, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &        'an attempt to read the last file in ephedisp format '// &
     &         FILS(L_FIL) )
           CALL EXIT ( 1 )
      END IF
      DO 430 J3=1,MALO%NSTA
         C_STA(J3) = MALO%STA(J3)%NAME
         CALL MAKE_UEN_TO_XYZ  ( MALO%STA(J3)%COO(1), UEN_TO_XYZ(1,1,J3) )
 430  CONTINUE 
!
      MJD_END = MALO%MJD_END
      TAI_END = MALO%TAI_END
      N_STA = MALO%NSTA
      IF ( TIM_INT == 0.0D0 ) THEN
           N_EPC = L_FIL
         ELSE 
           N_EPC   = IDNINT ( ((MJD_END - MJD_BEG)*86400.0D0 + &
     &                         (TAI_END - TAI_BEG))/TIM_INT  ) + 1
      END IF
      IF ( DATE_BEG_REQ .EQ. 'update' ) THEN
           J_EPC = N_EPC + IDNINT( (BDS_MJD_END*86400.0D0 + BDS_TAI_END - &
     &                              BDS_MJD_BEG*86400.0D0 - BDS_TAI_BEG   )/ &
     &                              SMP_INTRV ) + 1
         ELSE 
           J_EPC = N_EPC
      END IF
!
      IF ( IVRB > 2 ) THEN
           WRITE ( 6, * ) 'N_STA= ', N_STA, ' N_EPC= ', N_EPC, ' J_EPC= ', J_EPC, &
     &           ' TIM_INT= ', TIM_INT
           WRITE ( 6, * ) 'MJD_BEG= ', MJD_BEG, ' TAI_BEG= ', TAI_BEG
           WRITE ( 6, * ) 'MJD_END= ', MJD_END, ' TAI_END= ', TAI_END
      END IF
      NTIM = MAX(MALO%NTIM,M_EPC_FIL)
!
      ALLOCATE ( BDS(J_EPC,N_STA), STAT=IOS ) 
      IF ( IOS .NE. 0 ) THEN
           IUER = -1
           CALL CLRCH ( STR )
           CALL IINCH8 ( SIZEOF(BDS(1,1))*L_STA*N_EPC, STR ) 
           CALL ERR_LOG ( 4813, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'for array BDS' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( DSPL(3,N_STA,NTIM), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           IUER = -1
           CALL CLRCH ( STR )
           CALL IINCH ( 8*3*L_STA*M_EPC_FIL, STR ) 
           CALL ERR_LOG ( 4814, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'for array DSPL' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( DATE_BEG_REQ .EQ. 'update' ) THEN
           LREC = IDNINT( (BDS_MJD_END*86400.0D0 + BDS_TAI_END - &
     &                     BDS_MJD_BEG*86400.0D0 - BDS_TAI_BEG   )/ &
     &                     SMP_INTRV ) + 1
           K_DSP = N_STA*LREC
           K_DSP_STA = LREC
           K_EPC = LREC
           MJD_BEG_STA = BDS_MJD_BEG
           SEC_BEG_STA = BDS_TAI_BEG
           MJD_END_STA = BDS_TAI_END
           SEC_END_STA = BDS_TAI_END
           MJD_BEG = BDS_MJD_BEG
           TAI_BEG = BDS_TAI_BEG
!
           DO 440 J4=1,N_STA
!
! ----------- Build the name of the input binary file
!
              CALL CLRCH ( FILIN )
              FILIN = OUTPUT_DIR(1:I_LEN(OUTPUT_DIR))// &
     &                C_STA(J4)(1:I_LEN(C_STA(J4)))//'.bds'// &
     &                POSTFIX(1:I_LEN(POSTFIX))
!
              IS = FILE_INFO ( FILIN(1:I_LEN(FILIN))//CHAR(0), UNIX_DATE, SIZE_I8 )
              NREC = (SIZE_I8 - M__HDR*LEN__HDR)/LEN__BDS
              IF ( J4 == 1 .AND. LREC > NREC ) THEN
                   LREC = NREC
                   K_DSP = N_STA*LREC
                   K_DSP_STA = LREC
                   K_EPC = LREC
                ELSE IF ( LREC == NREC ) THEN
                   CONTINUE 
                ELSE IF ( LREC < NREC ) THEN
                   WRITE ( 6, * ) 'NREC(data)= ', NREC, ' LREC(sum)= ', LREC
                   IUER = -1
                   CALL ERR_LOG ( 4815, IUER, 'EPHEDISP_TO_BIN', 'Trap of '// &
     &                 'internal control: the number of data records in file '// &
     &                  FILIN(1:I_LEN(FILIN))//' is different from the number '// &
     &                 'deduced from the summary' )
                   CALL EXIT ( 1 )
              END IF
!
              IUER = -1
              CALL BINF_OPEN ( FILIN, 'OLD', LUN, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4816, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &                 'opening for redin input file '//FILIN )
                   CALL EXIT ( 1 )
              END IF
!
! ----------- Position the file to the beginning of the first data record record
!
              IS = LSEEK ( %VAL(LUN), %VAL(M__HDR*LEN__HDR), %VAL(SEEK_SET) )
              IF ( IS .NE. M__HDR*LEN__HDR ) THEN
                   CALL CLRCH   ( STR )
                   CALL GERROR  ( STR )
                   IUER = -1
                   CALL ERR_LOG ( 4817, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &                 'an attempt to position the input file '// &
     &                  FILIN(1:I_LEN(FILIN))//' to the beginning of first '// &
     &                 'data record: '//STR )
                   CALL EXIT ( 1 )
              END IF
!
              IS = READ ( %VAL(LUN), BDS(1:NREC,J4), %VAL(NREC*LEN__BDS) )
              IF ( IS == -1 ) THEN
                   CALL CLRCH   ( STR )
                   CALL GERROR  ( STR )
                   IUER = -1
                   CALL ERR_LOG ( 4818, IUER, 'EPHEDISP_TO_BIN', 'Error '// &
     &                  STR(1:I_LEN(STR))//' in reading data section of file '// &
     &                  FILIN(1:I_LEN(FILIN))//' : '//STR )
                   CALL EXIT ( 1 )
                ELSE IF ( IS < NREC*LEN__BDS ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 4819, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &                 'reading the date section of the file '// &
     &                  FILIN(1:I_LEN(FILIN))//' -- not all bytes have been read' )
                   CALL EXIT ( 1 )
              END IF     
!
              CALL BINF_CLOSE ( LUN, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4820, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &             'an attempt to close the output file '//FILOUT )
                   CALL EXIT ( 1 )
              END IF
 440       CONTINUE 
         ELSE 
           K_DSP = 0
           K_DSP_STA = 0
           K_EPC = 0
           MJD_EPC = MJD_BEG
           TAI_EPC = TAI_BEG
           MJD_BEG_STA = 0
           MJD_END_STA = 0
      END IF
!
! --- Cycle over EPHEDISP files. We read all EPHEDISP files and get some
! --- information: the list of stations, first and last date of available
! --- site displacements
!
      DO 450 J5=1,L_FIL
!
! ------ Read the J5-th EPEHDISP files. Results are put in STA record
!
         IUER = -1
         CALL MALO_EPHEDISP_READ ( FILS(J5), MALO, DSPL, MALO__REA, N_STA, &
     &                             NTIM, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 4821, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &            'an attempt to read the file in ephedisp format '// &
     &             FILS(J5) )
              CALL EXIT ( 1 )
         END IF
!
         IF ( IVRB .GE. 2 ) THEN
              WRITE ( 6, 110 ) J5, L_FIL, FILS(J5)(1:I_LEN(FILS(J5))), &
     &                         CHAR(13)
 110          FORMAT ( 3X,I7,' (',I7,') ',A,4X,A$ )
              CALL FLUSH ( 6 )
         END IF
!
         IF ( MALO%NSTA .NE. L_STA ) THEN
              IF ( (MALO%MJD_BEG*86400.0D0 + MALO%TAI_BEG) - &
     &             (MJD_BEG*86400.0D0 + TAI_BEG)           < -TIM_EPS ) THEN
                   CALL CLRCH ( STR  )
                   CALL CLRCH ( STR1 )
                   IUER = -1
                   STR  = MJDSEC_TO_DATE ( MALO%MJD_BEG, MALO%TAI_BEG, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        WRITE ( 6, * ) 'M1 out of range ', MALO%MJD_BEG, MALO%TAI_BEG
                        CALL EXIT ( 1 )
                   END IF
                   IUER = -1
                   STR1 = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        WRITE ( 6, * ) 'M2 out of range ', MALO%MJD_BEG, MALO%TAI_BEG
                        CALL EXIT ( 1 )
                   END IF
                   CALL ERR_LOG ( 4822, IUER, 'EPHEDISP_TO_BIN', 'Strange, '// &
     &                 'but the first date in file '// &
     &                  FILS(J5)(1:I_LEN(FILS(J5)))//', '//STR(1:21)// &
     &                 ' is before the first date '//STR1(1:21) )
                   CALL EXIT ( 1 )
              END IF
              IF ( (MALO%MJD_END*86400.0D0 + MALO%TAI_END) - &
     &             (MJD_END*86400.0D0 + TAI_END)           > TIM_EPS ) THEN
                   IUER = -1
                   CALL CLRCH ( STR  )
                   CALL CLRCH ( STR1 )
                   IUER = -1
                   STR  = MJDSEC_TO_DATE ( MALO%MJD_END, MALO%TAI_END, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        WRITE ( 6, * ) 'M3 out of range ', MALO%MJD_END, MALO%TAI_END
                        CALL EXIT ( 1 )
                   END IF
                   IUER = -1
                   STR1 = MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        WRITE ( 6, * ) 'M4 out of range ', MALO%MJD_END, MALO%TAI_END
                        CALL EXIT ( 1 )
                   END IF
                   CALL ERR_LOG ( 4823, IUER, 'EPHEDISP_TO_BIN', 'Strange, '// &
     &                 'but the last date in file '// &
     &                  FILS(J5)(1:I_LEN(FILS(J5)))//', '//STR(1:21)// &
     &                 ' is beyond of the last date '//STR1(1:21) )
                   CALL EXIT ( 1 )
              END IF
              TIM_GAP = (MALO%MJD_BEG*86400.0D0 + MALO%TAI_BEG) - &
     &                  (MJD_EPC*86400.0D0 + TAI_EPC + TIM_INT)
              IF ( J5 == 2 .AND. TIM_INT == 0.0D0 ) THEN
                   TIM_INT = TIM_GAP
                   TIM_GAP = 0.0D0
              END IF
              IF ( J5 > 1 .AND. DABS(TIM_GAP) > TIM_EPS ) THEN
                   IUER = -1
                   CALL CLRCH ( STR )
                   WRITE ( UNIT=STR(1:10), FMT='(F10.1)' ) TIM_GAP
                   CALL ERR_LOG ( 4824, IUER, 'EPHEDISP_TO_BIN', 'Sadly, '// &
     &                 'there is a gap of '//STR(1:I_LEN(STR))//' seconds '// &
     &                 'between files '//FILS(J5-1)(1:I_LEN(FILS(J5-1)))// &
     &                 ' and '//FILS(J5) )
                   CALL EXIT ( 1 )
              END IF
         END IF
!
         DO 460 J6=1,MALO%NTIM
            K_EPC = K_EPC + 1
            DO 470 J7=1,N_STA
               IF ( MALO%STA(J7)%NAME .NE. C_STA(J7) ) THEN
                    IUER = -1
                    WRITE ( 6, * ) ' '
                    WRITE ( 6, * ) 'J7= ', J7, ' N_STA= ', N_STA
                    WRITE ( 6, * ) 'C_STA(J7)         = ', C_STA(J7)
                    WRITE ( 6, * ) 'MALO%STA(J7)%NAME = ', MALO%STA(J7)%NAME
                    CALL ERR_LOG ( 4825, IUER, 'EPHEDISP_TO_BIN', 'Trap of '// &
     &                  'internal control: station '//MALO%STA(J7)%NAME// &
     &                  ' in file '//FILS(J5)(1:I_LEN(FILS(J5)))// &
     &                  ' is not in the place where it was defined in the '// &
     &                  'last file '//FILS(L_FIL) )
                    CALL EXIT ( 1 )
               END IF
               IF ( MJD_BEG_STA(J7) == 0 ) THEN
                    MJD_BEG_STA(J7) = MALO%MJD_BEG
                    SEC_BEG_STA(J7) = MALO%TAI_BEG
               END IF
               MJD_END_STA(J7) = MALO%MJD_BEG
               SEC_END_STA(J7) = MALO%TAI_BEG + TIM_INT*(J6-1)
               IDAY = IDINT ( SEC_END_STA(J7)/86400.0D0 )
               MJD_END_STA(J7) = MJD_END_STA(J7) + IDAY
               SEC_END_STA(J7) = SEC_END_STA(J7) - IDAY*86400.D0
!
! ------------ Transform displacement from Up, East, North reference
! ------------ frame to X, Y, Z crust fist frame...
!
               CALL MUL_MV_IV_V ( 3, 3, UEN_TO_XYZ(1,1,J7), 3, DSPL(1,J7,J6), &
     &                            3, DSP_CFS, IUER )
!
! ------------ ... and eventually encode them
!
! ------------ Encode displacemebnt extension
!
               KX = DSP_CFS(1)/VTD__BDS_MAX
               KY = DSP_CFS(2)/VTD__BDS_MAX
               KZ = DSP_CFS(3)/VTD__BDS_MAX
               BDS(K_EPC,J7)%EXT_DSP  = 0
               CALL MVBITS (  KX, 15, 1,  BDS(K_EPC,J7)%EXT_DSP,  1 )
               CALL MVBITS (  KY, 15, 1,  BDS(K_EPC,J7)%EXT_DSP,  2 )
               CALL MVBITS (  KZ, 15, 1,  BDS(K_EPC,J7)%EXT_DSP,  3 )
               CALL MVBITS (  KX,  0, 4,  BDS(K_EPC,J7)%EXT_DSP,  4 )
               CALL MVBITS (  KY,  0, 4,  BDS(K_EPC,J7)%EXT_DSP,  8 )
               CALL MVBITS (  KZ,  0, 4,  BDS(K_EPC,J7)%EXT_DSP, 12 )
!
! ------------ Encodine the residual from the extension
!
               BDS(K_EPC,J7)%X_DSP = NINT( (DSP_CFS(1) - KX*VTD__BDS_MAX)*1.D5 )
               BDS(K_EPC,J7)%Y_DSP = NINT( (DSP_CFS(2) - KY*VTD__BDS_MAX)*1.D5 )
               BDS(K_EPC,J7)%Z_DSP = NINT( (DSP_CFS(3) - KZ*VTD__BDS_MAX)*1.D5 )
               K_DSP = K_DSP + 1
               K_DSP_STA(J7) = K_DSP_STA(J7) + 1
 470        CONTINUE 
 460     CONTINUE 
         MJD_EPC = MALO%MJD_END
         TAI_EPC = MALO%TAI_END
 450  CONTINUE 
      IF ( IVRB > 0 ) WRITE ( 6, * ) ' '
      IF ( DATE_BEG_REQ .NE. 'update'  .AND.  K_EPC .NE. N_EPC ) THEN
           WRITE ( 6, * ) 'K_EPC= ', K_EPC, ' N_EPC= ', N_EPC
           IUER  = -1
           CALL ERR_LOG ( 4826, IUER, 'EPHEDISP_TO_BIN', 'Trap of '// &
     &         'internal control: not all epochs were found' )
           CALL EXIT ( 1 )
      END IF
!
      CALL CLRCH ( STR )
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) STR(1:79)
           WRITE ( 6, 120 ) L_FIL, N_EPC, J_EPC, N_STA, K_DSP
 120       FORMAT ( 'The total number of files:  ',I7/ &
     &              'The number of new epochs:   ',I7/ &
     &              'The total number of epochs: ',I7/ &
     &              'The total number of sites:  ',I7/ &
     &              'The total number of displacements:  ',I15 )
      END IF
!
! --- Get information about user name and PID of this process
!
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      PID = GETPID()
      WRITE ( UNIT=POSTFIX(1:8), FMT='(I8)' ) PID
      CALL BLANK_TO_ZERO ( POSTFIX(1:8) )
!
! --- Build the postfix which will be appended to the temporary file names
!
      POSTFIX = '.'//USER_NAME(1:I_LEN(USER_NAME))//'__'//POSTFIX(1:8)
!
! --- Cycle over all stations
!
      I_FIL = 0 
      DO 480 J8=1,N_STA
         IF ( K_DSP_STA(J8) .LE. 0 ) GOTO 480
         ISTA = J8
!
! ------ Build the name of the output file
!
         CALL CLRCH ( FILOUT(J8) )
         FILOUT(J8) = OUTPUT_DIR(1:I_LEN(OUTPUT_DIR))// &
     &                C_STA(J8)(1:I_LEN(C_STA(J8)))//'.bds'// &
     &                POSTFIX(1:I_LEN(POSTFIX))
         CALL NOUT ( M__HDR*LEN__HDR, HEADER )
!
! ------ Create the BINDISP file header. It consists of 8 records
!
! ------ 1-st header record: format label
!
         HEADER(1) = 'BINDISP '
!
! ------ 2-nd header record: format decription
!
         IUER = -1
         CALL DATE_TO_TIME ( BINDISP_VERSION_DATE, MJD_VAL, SEC_VAL, IUER )
         HDR2%MJD_FMT    = MJD_VAL
         HDR2%ENDIAN_FMT = ENDIAN_FMT
         HDR2%FLOAT_FMT  = FLOAT_FMT
         HDR2%N_MOD      = 1
         CALL LIB$MOVC3 ( LEN__HDR, HDR2, %REF(HEADER(2)) )
!
! ------ 3-rd header record: site name
!
         HEADER(3) = C_STA(J8)
!
! ------ 4-th header record: the number of data records and the sampling
! ------                     interval
!
         HDR4%NUM_REC = K_DSP_STA(J8)
         HDR4%SAMPLING_INTERVAL = TIM_INT
         CALL LIB$MOVC3 ( LEN__HDR, HDR4, %REF(HEADER(4)) )
!
! ------ 5,6,7-th records: X,Y,Z site coordinates in the crust-fixed reference
! ------                   frame
!
         CALL LIB$MOVC3 ( LEN__HDR, MALO%STA(ISTA)%COO(1), %REF(HEADER(5)) )
         CALL LIB$MOVC3 ( LEN__HDR, MALO%STA(ISTA)%COO(2), %REF(HEADER(6)) )
         CALL LIB$MOVC3 ( LEN__HDR, MALO%STA(ISTA)%COO(3), %REF(HEADER(7)) )
!
! ------ 8-th header record: first epoch: MJD and SEC
!
         HDR8%MJD_FIRST = MJD_BEG_STA(J8)
         HDR8%TAI_FIRST = SEC_BEG_STA(J8)
         CALL LIB$MOVC3 ( LEN__HDR, HDR8, %REF(HEADER(8)) )
!
         BIN_MOD%LOAD_TYPE = 'unknown '
         BIN_MOD%LOAD_NAME = 'unknown '
         BIN_MOD%LOAD_VERS = '     eph'
         IF ( INDEX ( FILS(1), 'atm' ) > 0 ) THEN
              BIN_MOD%LOAD_TYPE = 'atm     '
           ELSE IF ( INDEX ( FILS(1), 'lws' ) > 0 ) THEN
              BIN_MOD%LOAD_TYPE = 'lws     '
           ELSE IF ( INDEX ( FILS(1), 'nto' ) > 0 ) THEN
              BIN_MOD%LOAD_TYPE = 'lws     '
         END IF
         IF ( INDEX ( FILS(1), 'geosfp_507' ) > 0 ) THEN
              BIN_MOD%LOAD_NAME = 'geos-507'
           ELSE IF ( INDEX ( FILS(1), 'geosfp_511' ) > 0 ) THEN
              BIN_MOD%LOAD_NAME = 'geos-511'
           ELSE IF ( INDEX ( FILS(1), 'geosfp_513' ) > 0 ) THEN
              BIN_MOD%LOAD_NAME = 'geos-513'
           ELSE IF ( INDEX ( FILS(1), 'geosfp_515' ) > 0 ) THEN
              BIN_MOD%LOAD_NAME = 'geos-515'
           ELSE IF ( INDEX ( FILS(1), 'geosfpit' ) > 0 ) THEN
              BIN_MOD%LOAD_NAME = 'geosfpit'
           ELSE IF ( INDEX ( FILS(1), 'geosfp' ) > 0 ) THEN
              BIN_MOD%LOAD_NAME = 'geosfp  '
           ELSE IF ( INDEX ( FILS(1), 'merra2' ) > 0 ) THEN
              BIN_MOD%LOAD_NAME = 'merra2  '
           ELSE IF ( INDEX ( FILS(1), 'merra' ) > 0 ) THEN
              BIN_MOD%LOAD_NAME = 'merra   '
         END IF
!
! ------ 9,10,11-th record: model type, name, version
!
         CALL LIB$MOVC3 ( LEN__HDR, %REF(BIN_MOD%LOAD_TYPE), %REF(HEADER(9))  )
         CALL LIB$MOVC3 ( LEN__HDR, %REF(BIN_MOD%LOAD_NAME), %REF(HEADER(10)) )
         CALL LIB$MOVC3 ( LEN__HDR, %REF(BIN_MOD%LOAD_VERS), %REF(HEADER(11)) )
!
! ------ Open the temporary output file
!
         LUN = GET_UNIT()
         OPEN ( UNIT=LUN, FILE=FILOUT(J8), STATUS='UNKNOWN', ACCESS='DIRECT', &
     &          FORM='UNFORMATTED', RECL=LEN__BDS, IOSTAT=IOS )
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              IUER = -1
              CALL ERR_LOG ( 4827, IUER, 'EPHEDISP_TO_BIN', 'Error '// &
     &             STR(1:I_LEN(STR))//' in an attempt to open output file '// &
     &             FILOUT(J8) )
              CALL EXIT ( 1 )
         END IF
!
! ------ Write the header
!
         DO 490 J9=1,M__HDR
            WRITE ( UNIT=LUN, REC=J9, IOSTAT=IOS ) HEADER(J9)
            IF ( IOS .NE. 0 ) THEN
                 WRITE ( 6, * ) ' J9=',J9,' IOS=',IOS
                 IUER = -1
                 CALL ERR_LOG ( 4828, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &               'writing in the header of the output file '//FILOUT(J8) )
                 CALL EXIT ( 1 )
            END IF
 490     CONTINUE
!
! ------ Write the body
!
         DO 4100 J10=1,J_EPC
            WRITE ( UNIT=LUN, REC=J10+M__HDR, IOSTAT=IOS ) BDS(J10,ISTA)
            IF ( IOS .NE. 0 ) THEN
                 WRITE ( 6, * ) ' J10=',J10,' IOS=',IOS
                 IUER = -1
                 CALL ERR_LOG ( 4829, IUER, 'EPHEDISP_TO_BIN', &
     &               'Error in writing in the body of the output '// &
     &               'file '//FILOUT(J8) )
                 CALL EXIT ( 1 )
            END IF
 4100    CONTINUE
!
! ------ Close output file
!
         CLOSE ( UNIT=LUN  )
         IF ( IVRB .GE. 2 ) THEN
              WRITE ( 6, 130 ) J8, N_STA, C_STA(J8), CHAR(13)
 130          FORMAT ( 3X,I5,' (',I5,') Written file for station ',A,A$ )
              CALL FLUSH ( 6 )
         END IF
         I_FIL = I_FIL + 1
 480  CONTINUE
!
! --- Build summary file name
!
      FILSUM = OUTPUT_DIR(1:I_LEN(OUTPUT_DIR))//SUMMARY_BDS_FILE//POSTFIX
!
! --- Open summary file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILSUM, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) ' IOS=',IOS
           IUER = -1
           CALL ERR_LOG ( 4830, IUER, 'EPHEDISP_TO_BIN', 'Error in '// &
     &         'an attempt to open summary file date '//FILSUM )
           CALL EXIT ( 1 )
      END IF
!
! --- Write the header of summery file
!
      WRITE ( LUN, '(A)' ) BINDISP_SUMMARY__LABEL
!
! --- Write current date
!
      WRITE ( LUN, '(A)' ) 'LAST_UPDATE: '//GET_CDATE()
!
! --- Write minimal date of displacement
!
      IUER = -1
      WRITE ( LUN, 140 ) 'MIN_EPOCH: ', MJD_BEG, TAI_BEG, &
     &                    MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, * ) 'Out of range MJD_BEG, TAI_BEG ', MJD_BEG, TAI_BEG
           CALL EXIT ( 1 )
      END IF
!
! --- Write maximal date of displacement
!
      IUER = -1
      WRITE ( LUN, 140 ) 'MAX_EPOCH: ', MJD_END, TAI_END, &
     &                    MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, * ) 'Out of range MJD_END, TAI_END ', MJD_END, TAI_END
           CALL EXIT ( 1 )
      END IF
 140  FORMAT ( A, I5,' ',F7.1,' ',A )
!
! --- Write the total number of epochs
!
      WRITE ( LUN, '(A,I9)'  ) 'L_EPC: ', J_EPC
!
! --- Write the total number of sites
!
      WRITE ( LUN, '(A,I9)'  ) 'L_STA: ', N_STA
!
! --- ... and the total number of displacements
!
      WRITE ( LUN, '(A,I15)' ) 'L_DSP: ', K_DSP
!
! --- Write the area of displacements validity
!
      WRITE ( LUN, '(A,2X,F11.5,2X,F11.5,2X,"sec")' ) 'SMP_INTRV:', TIM_INT, TIM_INT
      WRITE ( LUN, '(A,1X,F14.6)' ) 'RD_AREA:', RD_AREA
      WRITE ( LUN, '(A)' )  '#'
      WRITE ( LUN, '(A,I2)' )  'L_MOD: ', 1
      WRITE ( LUN, '(A,I2,1X,A,1X,A,1X,A)' )  'MODEL: ', 1, &
     &        BIN_MOD%LOAD_TYPE, BIN_MOD%LOAD_NAME, BIN_MOD%LOAD_VERS
      WRITE ( LUN, '(A)' )  '#'
      WRITE ( LUN, '(A)' )  '#     Num Site ID  Dates begin         / Date '// &
     &                      'end             N points  smp_intrv  '// &
     &                      'X-coordinate  Y coordinate  Z coordinate'
      WRITE ( LUN, '(A)' )  '#'
!
! --- Cycle over stations
!
      I_STA = 0
      DO 4110 J11=1,N_STA
         IF ( K_DSP_STA(J11) .LE. 0 ) GOTO 4110
         ISTA = J11
!
         I_STA = I_STA + 1
         CALL CLRCH ( STR  )
         CALL LIB$MOVC3 ( LEN__BDSUM, %REF(STR), BDSUM )
         BDSUM%REC_ID = 'STA:'
         BDSUM%FILL_4 = ' / '
         CALL CLRCH ( STR1 )
!
! ------ Write information about the station:
! ------ name,
! ------ coordinates,
! ------ start date,
! ------ end date,
! ------ number of displacements for this station
! ------ sampling interval in days
! ------ X coordinate
! ------ Y coordinate
! ------ Z coordinate
!
         WRITE ( UNIT=BDSUM%SITE_IND, FMT='(I4)' ) I_STA
         BDSUM%SITE_ID = C_STA(J11)
!
         IF ( MJD_BEG_STA(J11) .GT. 0 ) THEN
              IUER = -1
              STR1 = MJDSEC_TO_DATE ( MJD_BEG_STA(J11), SEC_BEG_STA(J11), IUER )
              IF ( IUER .NE. 0 ) THEN
                   WRITE ( 6, * ) 'M5 out of range ', MJD_BEG_STA(J11), SEC_BEG_STA(J11), ' J11= ', J11
                   CALL EXIT ( 1 )
              END IF
              BDSUM%DATE_BEG = STR1(1:19)
            ELSE
              WRITE ( 6, * ) ' '
              WRITE ( 6, * ) 'Name: ', C_STA(J11)
              WRITE ( 6, * ) 'Number of points: ',K_DSP_STA(J11)
              WRITE ( 6, * ) 'J11=',J11,' MJD_BEG_STA(J11) = ', MJD_BEG_STA(J11)
              IUER = -1
              CALL ERR_LOG ( 4831, IUER, 'EPHEDISP_TO_BIN', 'Trap of '// &
     &            'internal control: wrong MJD_BEG_STA'  )
              CALL EXIT ( 1 )
        END IF
!
         IF ( MJD_END_STA(J11) .GT. 0 ) THEN
              IUER = -1
              STR1 = MJDSEC_TO_DATE ( MJD_END_STA(J11), SEC_END_STA(J11), IUER )
              IF ( IUER .NE. 0 ) THEN
                   WRITE ( 6, * ) 'M6 out of range ', MJD_END_STA(J11), SEC_END_STA(J11), ' J11= ', J11
                   CALL EXIT ( 1 )
              END IF
              BDSUM%DATE_END = STR1(1:19)
            ELSE
              WRITE ( 6, * ) ' '
              WRITE ( 6, * ) 'J11= ',J11, ' MJD_END_STA(J11) = ', MJD_END_STA(J11)
              IUER = -1
              CALL ERR_LOG ( 4832, IUER, 'EPHEDISP_TO_BIN', 'Trap of '// &
     &            'internal control: wrong MJD_END_STA'  )
              CALL EXIT ( 1 )
         END IF
!
         WRITE ( UNIT=BDSUM%NUM_PTS, FMT='(I9)' ) K_DSP_STA(J11)
         WRITE ( UNIT=BDSUM%SAMPLE_INT, FMT='(F16.11)' ) &
     &           TIM_INT/86400.0D0
         WRITE ( UNIT=BDSUM%X_COORD, FMT='(F13.4)' ) MALO%STA(ISTA)%COO(1)
         WRITE ( UNIT=BDSUM%Y_COORD, FMT='(F13.4)' ) MALO%STA(ISTA)%COO(2)
         WRITE ( UNIT=BDSUM%Z_COORD, FMT='(F13.4)' ) MALO%STA(ISTA)%COO(3)
         BDSUM%ENDIAN_FMT = ENDIAN_FMT
         BDSUM%FLOAT_FMT  = FLOAT_FMT
!
         CALL LIB$MOVC3 ( LEN__BDSUM, BDSUM, %REF(STR) )
         WRITE ( LUN, '(A)' ) STR(1:LEN__BDSUM)
 4110 CONTINUE
!
! --- Close summary file
!
      CLOSE ( UNIT=LUN )
      FILOUT(N_STA+1) = FILSUM
!
! --- Move the files with displacements and the summary file from their
! --- temporary place to a permanent place
!
      IUER = -1
      CALL MOVE_LOCK ( N_STA+1, FILOUT, POSTFIX, IUER )
      IF ( IUER .NE. -1 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4833, IUER, 'EPHEDISP_TO_BIN', 'Error in an '// &
     &         'attempt to move files from temporary to permanent place' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( IVRB .GE. 1 ) THEN
           CALL CLRCH ( STR )
           WRITE ( 6, '(A)' ) STR(1:79)
           L_BTS = K_DSP*LEN__HDR + I_FIL*M__HDR
           CALL IINCH ( L_BTS, STR )
           WRITE ( 6, * ) ' Processed files: ',L_FIL,'  Written bytes: ', &
     &                    STR(1:I_LEN(STR))
           IP = LINDEX ( FILSUM, POSTFIX(1:I_LEN(POSTFIX)) ) - 1
           IF ( IP .LE. 1 ) IP = I_LEN(FILSUM)
           WRITE ( 6, * ) ' Summary file: '//FILSUM(1:IP)
      END IF
!
! --- Deal done!
!
      END  SUBROUTINE  EPHEDISP_TO_BIN  !#!#
