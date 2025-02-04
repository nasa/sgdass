      PROGRAM    BDS_ADD
! ************************************************************************
! *                                                                      *
! *   Program  BDS_ADD
! *                                                                      *
! *  ### 13-JUL-2014    BDS_ADD    v1.0 (c)  L. Petrov  13-JUL-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'malo.i'
      INCLUDE   'bindisp.i'
      TYPE     ( BINDISP_DATA ) ::  BDS1, BDS2, BDSO
      CHARACTER  DIR1*128, DIR2*128, DIRO*128, STR*128,  &
     &           C1_STA(M__BDSLEN)*8, C2_STA(M__BDSLEN)*8, &
     &           HEADER(M__HDR)*(LEN__HDR), &
     &           FIL1*128, FIL2*128, FILO*128
      INTEGER*4  BDS1_MJD_BEG, BDS1_MJD_END, L1_STA, L1_EPC, &
     &           BDS2_MJD_BEG, BDS2_MJD_END, L2_STA, L2_EPC
      REAL*8     BDS1_TAI_BEG, BDS1_TAI_END, BDS1_SMP_INTRV, &
     &           BDS2_TAI_BEG, BDS2_TAI_END, BDS2_SMP_INTRV
      INTEGER*4  DIR_DESC, IS, J1, J2, J3, J4, IOS, &
     &           LUN1, LUN2, LUNO, IUER
      INTEGER*2  MODE_I2
      DATA       MODE_I2 / O'0775' /
      REAL*8,    EXTERNAL :: MJD_SEC_TO_JD
      INTEGER*4, EXTERNAL :: GET_UNIT, GETPID, ILEN, I_LEN, LINDEX, LTM_DIF, &
     &                       GET_FILE_FROM_DIR, FILE_INFO, READ, OPENDIR, &
     &                       MKDIR
!
      IF ( IARGC() .LT. 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage bds_add dir1 dir2 dirsum'
           CALL EXIT ( 1 )
        ELSE
           CALL GETARG ( 1, DIR1 )
           CALL GETARG ( 2, DIR2 )
           CALL GETARG ( 3, DIRO )
      END IF
!
      IUER = -1
      CALL LEARN_BDS_START_INTRV ( DIR1,  BDS1_SMP_INTRV,   &
     &                             BDS1_MJD_BEG, BDS1_TAI_BEG, &
     &                             BDS1_MJD_END, BDS1_TAI_END, &
     &                             L1_STA, C1_STA, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6701, IUER, 'BDS_ADD', 'Cannot learn contents '// &
     &         'of the first BDS-directory '//DIR1  )
           CALL EXIT ( 1 )
      END IF
      L1_EPC = IDNINT ( (BDS1_MJD_END*86400.0D0 + BDS1_TAI_END - &
     &                   BDS1_MJD_BEG*86400.0D0 - BDS1_TAI_BEG)/BDS1_SMP_INTRV ) + 1
!
      IUER = -1
      CALL LEARN_BDS_START_INTRV ( DIR2,  BDS2_SMP_INTRV,   &
     &                             BDS2_MJD_BEG, BDS2_TAI_BEG, &
     &                             BDS2_MJD_END, BDS2_TAI_END, &
     &                             L2_STA, C2_STA, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6702, IUER, 'BDS_ADD', 'Cannot learn contents '// &
     &         'of the second BDS-directory '//DIR2  )
           CALL EXIT ( 1 )
      END IF
      L2_EPC = IDNINT ( (BDS2_MJD_END*86400.0D0 + BDS2_TAI_END - &
     &                   BDS2_MJD_BEG*86400.0D0 - BDS2_TAI_BEG)/BDS2_SMP_INTRV ) + 1
!
      DIR_DESC = OPENDIR ( DIRO(1:I_LEN(DIRO))//CHAR(0) )
      IF ( DIR_DESC == 0 )    THEN
           IS = MKDIR ( DIRO(1:I_LEN(DIRO))//CHAR(0), %VAL(MODE_I2) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                IUER = -1
                CALL ERR_LOG ( 6703, IUER, 'BDS_ADD', 'Failure to create '// &
     &              'directory '//DIRO(1:I_LEN(DIRO))//' -- '//STR )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      WRITE ( 6 ,* ) 'L1_STA, L2_STA = ', L1_STA, L2_STA
      WRITE ( 6 ,* ) 'L1_EPC, L2_EPC = ', L1_EPC, L2_EPC
      IF ( L1_STA .NE. L2_STA ) THEN
           IUER = -1
           CALL ERR_LOG ( 6704, IUER, 'BDS_ADD', 'The number of stations '// &
     &         'in summary files of two directories is different' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( L1_EPC .NE. L2_EPC ) THEN
           IUER = -1
           CALL ERR_LOG ( 6705, IUER, 'BDS_ADD', 'The number of epochs '// &
     &         'in summary files of two directories is different' )
           CALL EXIT ( 1 )
      END IF
!
      DO 410 J1=1,L1_STA
         LUN1 = GET_UNIT()
         FIL1 = C1_STA(J1)
         FIL1 = DIR1(1:I_LEN(DIR1))//'/'//FIL1(1:ILEN(FIL1))//'.bds'
         OPEN ( UNIT=LUN1, FILE=FIL1, STATUS='UNKNOWN', ACCESS='DIRECT', &
     &          FORM='UNFORMATTED', RECL=LEN__BDS, IOSTAT=IOS )
         IF ( IOS .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6706, IUER, 'BDS_ADD', 'Error in '// &
     &            'opening for reading input file '//FIL1 )
              CALL EXIT ( 1 )
         END IF
!
         LUN2 = GET_UNIT()
         FIL2 = C1_STA(J1)
         FIL2 = DIR2(1:I_LEN(DIR2))//'/'//FIL2(1:ILEN(FIL2))//'.bds'
         OPEN ( UNIT=LUN2, FILE=FIL2, STATUS='OLD', ACCESS='DIRECT', &
     &          FORM='UNFORMATTED', RECL=LEN__BDS, IOSTAT=IOS )
         IF ( IOS .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6707, IUER, 'BDS_ADD', 'Error in '// &
     &            'opening for reading input file '//FIL2 )
              CALL EXIT ( 1 )
         END IF
!
         LUNO = GET_UNIT()
         FILO = C1_STA(J1)
         FILO = DIRO(1:I_LEN(DIRO))//'/'//FILO(1:ILEN(FILO))//'.bds'
         OPEN ( UNIT=LUNO, FILE=FILO, STATUS='UNKNOWN', ACCESS='DIRECT', &
     &          FORM='UNFORMATTED', RECL=LEN__BDS, IOSTAT=IOS )
         IF ( IOS .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6708, IUER, 'BDS_ADD', 'Error in '// &
     &            'opening for writing input file '//FILO )
              CALL EXIT ( 1 )
         END IF
!
         DO 420 J2=1,M__HDR
            READ  ( UNIT=LUN1, REC=J2, IOSTAT=IOS ) HEADER(J2)
            IF ( IOS .NE. 0 ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 6709, IUER, 'BDS_ADD', 'Error in '// &
     &               'reading the input file '//FIL1 )
                 CALL EXIT ( 1 )
            END IF
!
            READ  ( UNIT=LUN2, REC=J2, IOSTAT=IOS ) HEADER(J2)
            IF ( IOS .NE. 0 ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 6710, IUER, 'BDS_ADD', 'Error in '// &
     &               'reading the input file '//FIL2 )
                 CALL EXIT ( 1 )
            END IF
!
            WRITE ( UNIT=LUNO, REC=J2, IOSTAT=IOS ) HEADER(J2)
            IF ( IOS .NE. 0 ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 6711, IUER, 'BDS_ADD', 'Error in '// &
     &               'reading the output file '//FILO )
                 CALL EXIT ( 1 )
            END IF
 420     CONTINUE
         DO 430 J3=1,L1_EPC
            READ  ( UNIT=LUN1, REC=J3+M__HDR, IOSTAT=IOS ) BDS1
            IF ( IOS .NE. 0 ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 6712, IUER, 'BDS_ADD', 'Error in '// &
     &               'reading the input file '//FIL2 )
                 CALL EXIT ( 1 )
            END IF
!
            READ  ( UNIT=LUN2, REC=J3+M__HDR, IOSTAT=IOS ) BDS2
            IF ( IOS .NE. 0 ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 6713, IUER, 'BDS_ADD', 'Error in '// &
     &               'reading the input file '//FIL2 )
                 CALL EXIT ( 1 )
            END IF
!
            BDSO%X_DSP = BDS1%X_DSP + BDS2%X_DSP 
            BDSO%Y_DSP = BDS1%Y_DSP + BDS2%Y_DSP 
            BDSO%Z_DSP = BDS1%Z_DSP + BDS2%Z_DSP 
!
            WRITE ( UNIT=LUNO, REC=J3+M__HDR, IOSTAT=IOS ) BDSO
            IF ( IOS .NE. 0 ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 6714, IUER, 'BDS_ADD', 'Error in '// &
     &               'writing the output file '//FILO )
                 CALL EXIT ( 1 )
            END IF
 430     CONTINUE
         CLOSE ( UNIT=LUN1 )
         CLOSE ( UNIT=LUN2 )
         CLOSE ( UNIT=LUNO )
 410  CONTINUE 
!
      END  PROGRAM  BDS_ADD  !#!  
