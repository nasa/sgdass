       PROGRAM    BINDISP_MERGE_MAIN
       IMPLICIT   NONE 
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = INT8(4) * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL BINDISP_MERGE()
       END  PROGRAM  BINDISP_MERGE_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE BINDISP_MERGE()
! ************************************************************************
! *                                                                      *
! *   Program BINDISP_MERGE merges time series of loading displacments   *
! *   in BINDISP format from two inpuit directoeries. The loading        *
! *   time series in the first directory starts earlier and end earlier  *
! *   than in the second directory. Procedure BINDISP_MERGE takes the    *
! *   time series from the 1st directory and appends the time series     *
! *   from the second directory for the dates after the last date of     *
! *   the time series from the second directory:                         *
! *                                                                      *
! *     time series from the 1st    directory 111111111111111111         *
! *     time series from the 2st    directory          22222222222222    *
! *                                                                      *
! *     time series from the output directory 11111111111111111122222    *
! *                                                                      *
! *   Restiction: the number and the order of stations with loading      *
! *   time series should be the same.                                    *
! *                                                                      *
! * ### 03-APR-2018  BINDISP_MERGE  v2.2 (c)  L. Petrov  18-OCT-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INTEGER*4    MC_EPC
      PARAMETER  ( MC_EPC = 16 )
      TYPE ( BINDISP_HEADER_4 ) ::  HDR4
      TYPE ( BINDISP_DATA     ) ::  DSPL_COM(MC_EPC,2), DSPL_VAL
      CHARACTER  DIRIN(2)*128,  DIROUT*128
      CHARACTER  FILSUM(3)*128, BUF(M__BDSLEN,2)*256, FMT_VERSION*10, &
     &           STR*128, STR1*128, STR2*128, DAT_BEG_STR(2)*21, &
     &           DAT_END_STR(2)*21, LOAD_NAME*8, FILE_IO_LOCK*128, &
     &           FILE_READ_LOCK*128, FILE_WRITE_LOCK*128
      CHARACTER  FILS(M__BDSLEN,4)*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, N_SUM(2), L_STA(2), L_EPC(3), &
     &           MJD_BEG(2), MJD_END(3), MJD_MIX_BEG, NSP, NEP, IOS, LUN, &
     &           ISTA(M__BDSLEN), ID1, ID2, IDAY, LUN1, LUN2, LUNO, &
     &           IP, IS, SEEK_SET, ARG_LN, IND, IVRB, LC_EPC, &
     &           STAT_BLOCK(16), FD_READ_LOCK, FD_WRITE_LOCK, IUER
      INTEGER*1, ALLOCATABLE :: DAT(:)
      REAL*8     TAI_BEG(2), TAI_END(3), TAI_MIX_BEG, TIM_INT(2), EPS
      REAL*4     BIAS_R4(3)
      INTEGER*8  OFFSET_RET
      PARAMETER  ( EPS = 1.0D-3 )
      LOGICAL*1  LEX
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, FOR_STAT, GETPID, GET_UNIT, &
     &                       LINDEX, READ, RENAME, TIME, UNLINK, WRITE
      INTEGER*8, EXTERNAL :: LSEEK
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
      IVRB = 1 !  Default verbosity
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: bindisp_merge dir1 dir2 dirout [ivrb]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, DIRIN(1) ) 
           CALL GETARG ( 2, DIRIN(2) ) 
           CALL GETARG ( 3, DIROUT   ) 
           IF ( IARGC() .GE.  4 ) THEN
                CALL GETARG ( 4, STR ) 
                CALL CHIN   ( STR, IVRB )
           END IF
      END IF
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LN )
!
! --- Build the names of lock files
!
      FILE_IO_LOCK    = TRIM(DIROUT)//'/'//VTD__IO_LOCK_NAME
      FILE_READ_LOCK  = TRIM(DIROUT)//'/'//VTD__READ_LOCK_NAME
      FILE_WRITE_LOCK = TRIM(DIROUT)//'/'//VTD__WRITE_LOCK_NAME
!
! --- Parse summary files from both directories
!
      DO 410 J1=1,2
         ISTA(J1) = 0
!
! ------ Get the summary file name
!
         FILSUM(J1) = TRIM(DIRIN(J1))//'/bds_summary.txt' 
!
! ------ Read the summary file of the set of BINDISP files.
!
         IUER = -1
         CALL RD_TEXT  ( FILSUM(J1), M__BDSLEN, BUF(1,J1), N_SUM(J1), IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 2801, IUER, 'BINDISP_MERGE', 'Error in '// &
     &             'reading the summary file '//FILSUM(1) )
              RETURN
         END IF
!
! ------ Check whether the first line contains the label of the format
!
         IF ( BUF(1,J1)(1:LEN(BINDISP_SUMMARY__LABEL)) == BINDISP_SUMMARY__LABEL ) THEN
              FMT_VERSION = '2014.06.20'
            ELSE IF ( BUF(1,J1)(1:LEN(BINDISP_SUMMARY__LABEL)) == BINDISP_SUMMARY__LABEL_1 ) THEN
              FMT_VERSION = '2002.12.12'
            ELSE IF ( BUF(1,J1)(1:LEN(BINDISP_SUMMARY__LABEL)) == BINDISP_SUMMARY__LABEL_2 ) THEN
              FMT_VERSION = '2002.12.12'
            ELSE 
              IUER = -1
              CALL ERR_LOG ( 2802, IUER, 'BINDISP_MERGE', 'Format violation '// &
     &            'in the summary file '//FILSUM(J1)(1:I_LEN(FILSUM(J1)))// &
     &            ' -- the first line does not have the signature which '// &
     &            'was expected: '//BINDISP_SUMMARY__LABEL )
              CALL EXIT ( 1 )
         END IF
!
! ------ Read summary. We need to extract the number of sites, number of epochs,
! ------ start and stop dates, the time interval
!
         DO 420 J2=1,N_SUM(J1)
            IF ( BUF(J2,J1)(1:6) .EQ. 'L_STA:' ) THEN
!
! -------------- The number of stations
!
                 READ ( UNIT=BUF(J2,J1)(7:16), FMT='(I10)' ) L_STA(J1)
              ELSE IF ( BUF(J2,J1)(1:6) .EQ. 'L_EPC:' ) THEN
!
! -------------- Teh number of epochs
!
                 READ ( UNIT=BUF(J2,J1)(7:16), FMT='(I10)' ) L_EPC(J1)
              ELSE IF ( BUF(J2,J1)(1:10) .EQ. 'MIN_EPOCH:' ) THEN
!
! -------------- Start date of the time series
!
                 READ ( UNIT=BUF(J2,J1)(12:16), FMT='(I5)'   ) MJD_BEG(J1)
                 READ ( UNIT=BUF(J2,J1)(18:24), FMT='(F7.1)' ) TAI_BEG(J1)
                 DAT_BEG_STR(J1) = BUF(J2,J1)(26:46)
              ELSE IF ( BUF(J2,J1)(1:10) .EQ. 'MAX_EPOCH:' ) THEN
!
! -------------- Stop  date of the time series
!
                 READ ( UNIT=BUF(J2,J1)(12:16), FMT='(I5)'   ) MJD_END(J1)
                 READ ( UNIT=BUF(J2,J1)(18:24), FMT='(F7.1)' ) TAI_END(J1)
                 DAT_END_STR(J1) = BUF(J2,J1)(26:46)
              ELSE IF ( BUF(J2,J1)(1:10) .EQ. 'SMP_INTRV:' ) THEN
!
! -------------- Time interval of the time series
!
                 READ ( UNIT=BUF(J2,J1)(13:23), FMT='(F11.5)' ) TIM_INT(J1)
              ELSE IF ( BUF(J2,J1)(1:4) .EQ. 'STA:' ) THEN
!
! -------------- Station name. We form the file name with the time series
!
                 ISTA(J1) = ISTA(J1) + 1
                 FILS(ISTA(J1),J1) = TRIM(DIRIN(J1))//'/'//TRIM(BUF(J2,J1)(11:18))//'.bds'
                 INQUIRE ( FILE=FILS(ISTA(J1),J1), EXIST=LEX )
                 IF ( .NOT. LEX ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 2803, IUER, 'BINDISP_MERGE', 'Cannot '// &
     &                    'find binary dsisplacement file '//FILS(ISTA(J1),J1) )
                      CALL EXIT ( 1 )
                 END IF
                 IF ( J1 == 2 )  THEN
!
! ------------------- Check whether the station name is the same for both directories.
!
                      ID1 = LINDEX ( FILS(ISTA(J1),1), '/' )
                      ID2 = LINDEX ( FILS(ISTA(J1),2), '/' )
                      IF ( FILS(ISTA(J1),2)(ID2+1:) .NE. FILS(ISTA(J1),1)(ID1+1:) ) THEN
                           IUER = -1
                           CALL ERR_LOG ( 2804, IUER, 'BINDISP_MERGE', 'Input '// &
     &                         'directories '//TRIM(DIRIN(1))//' and '//TRIM(DIRIN(2))// &
     &                         ' have different files: '//TRIM(FILS(ISTA(J1),1))// &
     &                         ' and '//FILS(ISTA(J1),2) )
                           CALL EXIT ( 1 )
                      END IF
                 END IF
            END IF
 420     CONTINUE
 410  CONTINUE 
      IF ( L_STA(1) .NE. L_STA(2) ) THEN
           CALL CLRCH ( STR1 )
           CALL CLRCH ( STR2 )
           CALL INCH  ( L_STA(1), STR1 )
           CALL INCH  ( L_STA(2), STR2 )
           IUER = -1
           CALL ERR_LOG ( 2805, IUER, 'BINDISP_MERGE', 'The numer of '// &
     &         'stations in summary files '//TRIM(FILSUM(1))//'  and '// &
     &          TRIM(FILSUM(2))//' is different: '//TRIM(STR1)//' and '// &
     &          TRIM(STR2)//' respectively. Such datasets cannot be merged' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( MJD_BEG(2)*86400.0D0 + TAI_BEG(2) < MJD_BEG(1)*86400.0D0 + TAI_BEG(1) ) THEN
           IUER = -1
           CALL ERR_LOG ( 2806, IUER, 'BINDISP_MERGE', 'The start erpoch '// &
     &         'of the 2nd displacements from '//TRIM(DIRIN(2))//' is earlier '// &
     &         'than the start epoch of the 1st displacement '//TRIM(DIRIN(1)) )
           CALL EXIT ( 1 )
      END IF
!
! --- Compute how many epochs to skip in the 2nd series
!
      NSP = IDNINT( ( ( MJD_END(1)*86400.0D0 + TAI_END(1) ) - &
     &                ( MJD_BEG(2)*86400.0D0 + TAI_BEG(2) ) + TIM_INT(2) )/TIM_INT(1) )
      IF ( NSP < 1 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2807, IUER, 'BINDISP_MERGE', 'No overlap between the '// &
     &         'end of the the 1st displacements from '//TRIM(DIRIN(1))//' '// &
     &          DAT_END_STR(1)//' and start date of the 2nd displacement from '// &
     &          TRIM(DIRIN(2))//' '//DAT_BEG_STR(2) )
           CALL EXIT ( 1 )
      END IF
      LC_EPC = IDNINT ( ( (MJD_END(1)*86400.0D0 + TAI_END(1)) - &
     &                    (MJD_BEG(2)*86400.0D0 + TAI_BEG(2)) &
     &                  )/TIM_INT(1) )
      IF ( LC_EPC < MC_EPC ) THEN
           IUER = -1
           CALL CLRCH ( STR )
           CALL INCH  ( MC_EPC, STR )
           CALL ERR_LOG ( 2808, IUER, 'BINDISP_MERGE', 'Overlap between the '// &
     &         'end of the the 1st displacements from '//TRIM(DIRIN(1))//' '// &
     &          DAT_END_STR(1)//' and start date of the 2nd displacement from '// &
     &          TRIM(DIRIN(2))//' '//DAT_BEG_STR(2)//' is too small, less than '// &
     &          TRIM(STR)//' epochs' )
           CALL EXIT ( 1 )
      END IF 

! --- Compute how many new epochs to add from the 2nd series to the end of first series
!
      NEP = IDNINT( ( ( MJD_END(2)*86400.0D0 + TAI_END(2) ) - &
     &                ( MJD_END(1)*86400.0D0 + TAI_END(1) )   )/TIM_INT(1) - EPS )
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) 'NEP      = ', NEP
           WRITE ( 6, * ) 'NSP      = ', NSP
           WRITE ( 6, * ) 'L_EPC(1) = ', L_EPC(1)
           WRITE ( 6, * ) 'L_EPC(2) = ', L_EPC(2)
           WRITE ( 6, * ) 'TIM_INT  = ', TIM_INT
      END IF
!
! --- Allocate memory for a time series
!
      ALLOCATE ( DAT(LEN__BDS*(L_EPC(1)+L_EPC(2)+M__HDR*2)), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2809, IUER, 'BINDISP_MERGE', 'Failure to allocate '// &
     &         'dynamic memory for bufffer DAT' )
           CALL EXIT ( 1 )
      END IF
!
! --- Cycle over stations
!
      DO 430 J3=1,ISTA(1)
         ID1 = LINDEX ( FILS(J3,1), '/' )
!
! ------ Form the output file name
!
         FILS(J3,4) = TRIM(DIROUT)//'/'//FILS(J3,1)(ID1+1:)
         IP = GETPID()
         CALL INCH   ( IP,    STR(1:8) )
         CALL CHASHR (        STR(1:8) ) 
         CALL BLANK_TO_ZERO ( STR(1:8) ) 
         FILS(J3,3) = TRIM(FILS(J3,4))//'__'//STR(1:8)
         IF ( IVRB .GE. 2 ) THEN
              WRITE ( 6, '(A, 2X, I4, " ( ", I4, ")" )' ) 'Process station '// &
     &                FILS(J3,1)(ID1+1:ID1+12), J3, ISTA(1)
         END IF
!
! ------ Check whether the output file exists. If yes, then 
!
         INQUIRE ( FILE=FILS(J3,3), EXIST=LEX )
         IF ( LEX ) THEN
!
! ----------- Exists? Then remove it
!
              IS = UNLINK ( TRIM(FILS(J3,3))//CHAR(0) )
              IF ( IS .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 2810, IUER, 'BINDISP_MERGE', 'Cannot remove '// &
     &                 'stale output file '//TRIM(FILS(J3,3))//' -- '//STR )
                   CALL EXIT ( 1 )
              END IF
         END IF
!
! ------ Open input files
!
         IUER = -1
         CALL BINF_OPEN ( FILS(J3,1), 'OLD', LUN1, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 2811, IUER, 'BINDISP_MERGE', 'Error in an '// &
     &            'attempt to open for reading input file '//FILS(J3,1) )
              CALL EXIT ( 1 )
         END IF
!
         IUER = -1
         CALL BINF_OPEN ( FILS(J3,2), 'OLD', LUN2, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 2812, IUER, 'BINDISP_MERGE', 'Error in an '// &
     &            'attempt to open for reading input file '//FILS(J3,2) )
              CALL EXIT ( 1 )
         END IF
!
! ------ Read the entire input file from the 1st directory
!
         IS = READ ( %VAL(LUN1), DAT, %VAL((L_EPC(1)+M__HDR)*LEN__BDS) )
         IF ( IS == -1 ) THEN
              CALL CLRCH ( STR )
              CALL GERROR ( STR )
              IUER = -1
              CALL ERR_LOG ( 2813, IUER, 'BINDISP_MERGE', 'Error in reading '// &
     &            'the the input file '//TRIM(FILS(J3,1))// &
     &            ' -- '//STR )
              CALL EXIT ( 1 )
            ELSE IF ( IS .NE. (L_EPC(1)+M__HDR)*LEN__BDS ) THEN
              IUER = -1
              CALL ERR_LOG ( 2814, IUER, 'BINDISP_MERGE', 'Error in reading '// &
     &            'the input file '//FILS(J3,1) )
              CALL EXIT ( 1 )
         END IF
!
         MJD_END(3) = MJD_END(1)
         TAI_END(3) = TAI_END(1) - MC_EPC*TIM_INT(1)
         IF ( TAI_END(3) < 0.0 - EPS ) THEN
              IDAY = (TAI_END(3) - EPS)/86400.0D0
              MJD_END(3) = MJD_END(3) + IDAY
              TAI_END(3) = TAI_END(3) - 86400.0D0*IDAY
         END IF
!
! ------ Copy the last section of the 1st displacement into DSPL_COM
!
         CALL MEMCPY   ( DSPL_COM(1,1), DAT(1+(M__HDR+L_EPC(1)-MC_EPC)*LEN__BDS), %VAL(MC_EPC*LEN__BDS) )
!
         DO 440 J4=1,MC_EPC
            TAI_END(3) = TAI_END(3) + TIM_INT(1)
            IF ( TAI_END(3) > 86400.0D0 - EPS ) THEN
                 IDAY = (TAI_END(3) - EPS)/86400.0D0
                 MJD_END(3) = MJD_END(3) + IDAY
                 TAI_END(3) = TAI_END(3) - 86400.0D0*IDAY
            END IF
!
! --------- Get the index of the point in the 2nd directory
!
            IND = IDNINT ( ( (MJD_END(3)*86400.0D0 + TAI_END(3)) - (MJD_BEG(2)*86400.0D0 + TAI_BEG(2)) )/ &
     &                       TIM_INT(2) ) + 1
!
! --------- Position the file with time series at the point with index IND
!
            OFFSET_RET = LSEEK( %VAL(LUN2), %VAL((M__HDR+(IND-1))*LEN__BDS), %VAL(SEEK_SET) )
            IF ( OFFSET_RET .NE. (M__HDR+(IND-1))*LEN__BDS ) THEN
                 CALL CLRCH  ( STR )
                 CALL GERROR ( STR )
                 IUER = -1
                 CALL ERR_LOG ( 2815, IUER, 'BINDISP_MERGE', 'Failure in '// &
     &               'an attempt to seek for the data record in the input file '// &
     &                TRIM(FILS(J3,2))//' -- '//STR )
                 RETURN 
            END IF
            IF ( IVRB .GE. 3 .AND. J3 == 1 ) THEN
                 STR = MJDSEC_TO_DATE ( MJD_END(3), TAI_END(3), IUER )
                 WRITE ( 6, 120 ) J4, STR(1:19), IND
 120             FORMAT ( 'J4= ', I6, ' Date: ', A, ' Ind = ', I6 )
            END IF
!
! --------- Read the point from the 2nd time series into array DSPL_COM
!
            IS = READ ( %VAL(LUN2), DSPL_COM(J4,2), %VAL(LEN__BDS) )
!
            IF ( IS == -1 ) THEN
                 CALL CLRCH ( STR )
                 CALL GERROR ( STR )
                 IUER = -1
                 CALL ERR_LOG ( 2816, IUER, 'BINDISP_MERGE', 'Error in reading '// &
        &            'the the input file '//TRIM(FILS(J3,2))// &
        &            ' -- '//STR )
                 CALL EXIT ( 1 )
               ELSE IF ( IS .NE. LEN__BDS ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 2817, IUER, 'BINDISP_MERGE', 'Error in reading '// &
        &            'the input file '//FILS(J3,2) )
                 CALL EXIT ( 1 )
            END IF
 440     CONTINUE 
!
         BIAS_R4 = 0.0
         CALL SER_BIAS ( MC_EPC, DSPL_COM, BIAS_R4 )
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, 110 ) J3, FILS(J3,1)(ID1+1:ID1+12), 1.0E3*BIAS_R4
 110          FORMAT ( I5, ' ) ', A, ' Bias= ', 3(F9.3, 1X), ' mm' )
         END IF
!
         L_EPC(3) = L_EPC(1)
         MJD_END(3) = MJD_END(1)
         TAI_END(3) = TAI_END(1)
!
! ------ Read time series points from the 2nd directory
!
         DO 450 J5=NSP,L_EPC(2)
!
! --------- Update the last epoch of the output series
!
            TAI_END(3) = TAI_END(3) + TIM_INT(1)
            IF ( TAI_END(3) > 86400.0D0 - EPS ) THEN
                 IDAY = (TAI_END(3) - EPS)/86400.0D0
                 MJD_END(3) = MJD_END(3) + IDAY
                 TAI_END(3) = TAI_END(3) - 86400.0D0*IDAY
            END IF
!
! --------- Get the index of the point in the 2nd directory
!
            IND = IDNINT ( ( (MJD_END(3)*86400.0D0 + TAI_END(3)) - (MJD_BEG(2)*86400.0D0 + TAI_BEG(2)) )/ &
     &                       TIM_INT(2) ) + 1
            IF ( IND > L_EPC(2) ) THEN
!
! -------------- Oh, the index beyond the last date
!
                 TAI_END(3) = TAI_END(3) - TIM_INT(1)
                 IF ( TAI_END(3) < - EPS ) THEN
                      IDAY = (TAI_END(3) - EPS)/86400.0D0
                      TAI_END(3) = TAI_END(3) + IDAY*86400.D0
                      MJD_END(3) = MJD_END(3) - IDAY
                 END IF
                 GOTO 850
            END IF
!
            IF ( J5 == NSP ) THEN
                 MJD_MIX_BEG = MJD_END(3)
                 TAI_MIX_BEG = TAI_END(3)
            END IF
            L_EPC(3) = L_EPC(3) + 1
!
            IF ( IVRB .GE. 3 .AND. J3 == 1 ) THEN
                 STR = MJDSEC_TO_DATE ( MJD_END(3), TAI_END(3), IUER )
                 WRITE ( 6, 130 ) J5, STR(1:19), L_EPC(3), IND
 130             FORMAT ( 'J5= ', I6, ' Date: ', A, ' Lepc = ', I6, ' Ind = ', I6 )
            END IF
!
! --------- Position the file with time series at the point with index IND
!
            OFFSET_RET = LSEEK( %VAL(LUN2), %VAL((M__HDR+(IND-1))*LEN__BDS), %VAL(SEEK_SET) )
            IF ( OFFSET_RET .NE. (M__HDR+(IND-1))*LEN__BDS ) THEN
                 CALL CLRCH  ( STR )
                 CALL GERROR ( STR )
                 IUER = -1
                 CALL ERR_LOG ( 2818, IUER, 'BINDISP_MERGE', 'Failure in '// &
     &               'an attempt to seek for the data record in the input file '// &
     &                TRIM(FILS(J3,2))//' -- '//STR )
                 RETURN 
            END IF
!
! --------- Read the point afrom the 2nd time series into array DAT
!
            IS = READ ( %VAL(LUN2), DAT(1+(M__HDR+L_EPC(3)-1)*LEN__BDS), %VAL(LEN__BDS) )
!
            IF ( IS == -1 ) THEN
                 CALL CLRCH ( STR )
                 CALL GERROR ( STR )
                 IUER = -1
                 CALL ERR_LOG ( 2819, IUER, 'BINDISP_MERGE', 'Error in reading '// &
        &            'the the input file '//TRIM(FILS(J3,2))// &
        &            ' -- '//STR )
                 CALL EXIT ( 1 )
               ELSE IF ( IS .NE. LEN__BDS ) THEN
                 IUER = -1
                 CALL ERR_LOG ( 2820, IUER, 'BINDISP_MERGE', 'Error in reading '// &
        &            'the input file '//FILS(J3,2) )
                 CALL EXIT ( 1 )
            END IF
!
! --------- Correct for bias
!
            CALL MEMCPY ( DSPL_VAL, DAT(1+(M__HDR+L_EPC(3)-1)*LEN__BDS), %VAL(LEN__BDS) )
            DSPL_VAL%X_DSP = DSPL_VAL%X_DSP - NINT(1.0E5*BIAS_R4(1))
            DSPL_VAL%Y_DSP = DSPL_VAL%Y_DSP - NINT(1.0E5*BIAS_R4(2))
            DSPL_VAL%Z_DSP = DSPL_VAL%Z_DSP - NINT(1.0E5*BIAS_R4(3))
            CALL MEMCPY ( DAT(1+(M__HDR+L_EPC(3)-1)*LEN__BDS), DSPL_VAL, %VAL(LEN__BDS) )
 450     CONTINUE 
 850     CONTINUE
!
! ------ Update header records
!
         HDR4%NUM_REC = L_EPC(3)
         HDR4%SAMPLING_INTERVAL = TIM_INT(1)
         CALL MEMCPY ( DAT((4-1)*LEN__BDS+1),  HDR4, %VAL(LEN__BDS) )
         LOAD_NAME = 'mixed   ' 
         CALL MEMCPY ( DAT((10-1)*LEN__BDS+1), LOAD_NAME )
!
! ------ Open the output file
!
         IUER = -1
         CALL BINF_OPEN ( FILS(J3,3), 'NEW', LUNO, IUER )
         IF ( IUER .NE.  0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 2821, IUER, 'BINDISP_MERGE', 'Error in an '// &
     &            'attempt to open for writing output file '//FILS(J3,3) )
              CALL EXIT ( 1 )
         END IF
!
! ------ Write the time series into the output file
!
         IS = WRITE ( %VAL(LUNO), DAT, %VAL((L_EPC(3)+M__HDR)*LEN__BDS) )
         IF ( IS == -1 ) THEN
              CALL CLRCH ( STR )
              CALL GERROR ( STR )
              IUER = -1
              CALL ERR_LOG ( 2822, IUER, 'BINDISP_MERGE', 'Error in writing '// &
     &            'the input file '//TRIM(FILS(J3,1))//' -- '//STR )
              CALL EXIT ( 1 )
            ELSE IF ( IS .NE. (L_EPC(3)+M__HDR)*LEN__BDS ) THEN
              IUER = -1
              CALL ERR_LOG ( 2823, IUER, 'BINDISP_MERGE', 'Error in reading '// &
     &            'the input file '//FILS(J3,1) )
              CALL EXIT ( 1 )
         END IF
!
! ------ Close the files
!
         CALL BINF_CLOSE ( LUN1, IUER )
         CALL BINF_CLOSE ( LUN2, IUER )
         CALL BINF_CLOSE ( LUNO, IUER )
 430  CONTINUE 
!
! --- Set the read lock
!
      IUER = -1
      CALL SET_READ_LOCK ( FILE_IO_LOCK, FILE_READ_LOCK, FILE_WRITE_LOCK, &
     &                     VTD__LOCK_TIMEOUT, FD_READ_LOCK, FD_WRITE_LOCK, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2824, IUER, 'VTD_LOAD_BINDISP', 'Error in setting '// &
     &         'up write lock while reading loading' )
           CALL EXIT ( 1 )
      END IF
!
! --- Rename from the temporary file to the final file
!
      DO 470 J7=1,ISTA(1)
         IS = RENAME ( TRIM(FILS(J7,3))//CHAR(0), TRIM(FILS(J7,4))//CHAR(0) )
         IF ( IS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL GERROR ( STR )
              IUER = -1
              CALL ERR_LOG ( 2825, IUER, 'BINDISP_MERGE', 'Error in renaming '// &
     &            'file '//TRIM(FILS(J7,3))//' to '//TRIM(FILS(J7,4))// &
     &            ' -- '//STR )
!
! ----------- Remove read lock file
!
              IS = UNLINK ( TRIM(FILE_READ_LOCK)//CHAR(0) )
              RETURN
         END IF
 470  CONTINUE 
!
! --- Lift read lock
!
      CALL LIFT_READ_WRITE_LOCKS ( FD_READ_LOCK, FD_WRITE_LOCK )
!
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) 'Processed ', INT2(ISTA(1)), ' stations'
      END IF
!
! --- Update summary file
!
      DO 480 J8=1,N_SUM(1)
         IF ( BUF(J8,1)(1:12) == 'LAST_UPDATE:' ) THEN
!
! ----------- Update last modfification date
!
              BUF(J8,1)(14:) =  GET_CDATE()
           ELSE IF ( BUF(J8,1)(1:10) == 'MAX_EPOCH:' ) THEN
!
! ----------- Update maximum epoch
!
              IUER = -1
              WRITE ( UNIT=BUF(J8,1), FMT=140 ) 'MAX_EPOCH: ', MJD_END(3), TAI_END(3), &
     &                                MJDSEC_TO_DATE ( MJD_END(3), TAI_END(3), IUER )
 140          FORMAT ( A, I5,' ',F7.1,' ',A )
           ELSE IF ( BUF(J8,1)(1:6) == 'L_EPC:' ) THEN
!
! ----------- Update the number of epochs
!
              WRITE ( UNIT=BUF(J8,1), FMT='(A,I9)' ) 'L_EPC: ', L_EPC(3)
           ELSE IF ( BUF(J8,1)(1:6) == 'L_DSP:' ) THEN
!
! ----------- Update the total number of displacements
!
              WRITE ( UNIT=BUF(J8,1), FMT='(A,I15)' ) 'L_DSP: ', L_EPC(3)*ISTA(1)
           ELSE IF ( BUF(J8,1)(1:6) == 'MODEL:' ) THEN
!
! ----------- Add comment
!
              IUER = -1
              STR  = MJDSEC_TO_DATE ( MJD_END(1),  TAI_END(1),  IUER )
              IUER = -1
              STR1 = MJDSEC_TO_DATE ( MJD_MIX_BEG, TAI_MIX_BEG, IUER )
              IUER = -1
              STR2 = MJDSEC_TO_DATE ( MJD_END(3), TAI_END(3),   IUER )
              IF ( BUF(J8+1,1)(1:1) == '#' ) THEN
                   BUF(J8+1,1) = '# Combined from '//BUF(J8,1)(20:27)//' displacements for '// &
     &                           '[ '//BUF(3,1)(26:44)//', '//STR(1:19)//' ] and '// &
     &                            BUF(J8,2)(20:27)//' for [ '//STR1(1:19)//', '//STR2(1:19)//' ]'
                   BUF(J8,1)(20:27) = 'mixed   '
              END IF
           ELSE IF ( BUF(J8,1)(1:4) == 'STA:' ) THEN
!
! ----------- Update the last epoch for a given station
!
              IUER = -1
              STR = MJDSEC_TO_DATE ( MJD_END(3), TAI_END(3), IUER )
              WRITE ( UNIT=BUF(J8,1)(65:70), FMT='(I6)' ) L_EPC(3)
              BUF(J8,1)(42:60) = STR(1:19)
         END IF 
 480  CONTINUE 
!
! --- Write down update summary into the output directory
!
      FILSUM(3) = TRIM(DIROUT)//'/bds_summary.txt'
      IUER = -1
      CALL WR_TEXT ( N_SUM(1), BUF(1,1), FILSUM(3), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2826, IUER, 'BINDISP_MERGE', 'Error in writing '// &
     &         'into the summary file '//FILSUM(3) )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, * ) 'Written output summmary file '//TRIM(FILSUM(3))
      END IF
      END  SUBROUTINE  BINDISP_MERGE  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SER_BIAS ( M_ARR, DSPL, BIAS_R4 )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  SER_BIAS
! *                                                                      *
! *  ### 04-APR-2018  SER_BIAS     v1.0 (c)  L. Petrov  04-APR-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M_ARR
      INCLUDE   'bindisp.i'
      TYPE ( BINDISP_DATA     ) ::  DSPL(M_ARR,2)
      REAL*4     BIAS_R4(3)
      REAL*4     ARR(M_ARR,3,2)
      REAL*8     T1(M_ARR), X1(M_ARR), X2(M_ARR)
      INTEGER*4  J1
!
      BIAS_R4 = 0.0
      DO 410 J1=1,M_ARR
         ARR(J1,1,1) = 1.0E-5*DSPL(J1,1)%X_DSP
         ARR(J1,2,1) = 1.0E-5*DSPL(J1,1)%Y_DSP
         ARR(J1,3,1) = 1.0E-5*DSPL(J1,1)%Z_DSP
!
         ARR(J1,1,2) = 1.0E-5*DSPL(J1,2)%X_DSP
         ARR(J1,2,2) = 1.0E-5*DSPL(J1,2)%Y_DSP
         ARR(J1,3,2) = 1.0E-5*DSPL(J1,2)%Z_DSP
         BIAS_R4(1) = BIAS_R4(1) + ( ARR(J1,1,2) - ARR(J1,1,1) )
         BIAS_R4(2) = BIAS_R4(2) + ( ARR(J1,2,2) - ARR(J1,2,1) )
         BIAS_R4(3) = BIAS_R4(3) + ( ARR(J1,3,2) - ARR(J1,3,1) )
!
!         T1(J1) = J1
!         X1(J1) = ARR(J1,3,1)
!         X2(J1) = ARR(J1,3,2)
!
!!   write ( 6, * ) 'j1 = ', j1, ' dspl = ', ARR(J1,1,1), ARR(J1,1,2), ' ds= ', DSPL(J1,1)%X_DSP ! %%%
 410  CONTINUE 
      BIAS_R4 = BIAS_R4/M_ARR
!   call diagi_2 ( m_arr, t1, x1, m_arr, t1, x2, j1 ) ! %%%
      RETURN
      END  SUBROUTINE  SER_BIAS   !#!  
