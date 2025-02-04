      SUBROUTINE PIMA_PARSE_VLBA_TSYS ( FIL_LOG, NBUF, BUF, M_TIM, M_FRQ, &
     &           N_TSYS, N_FRQ, MJD_TSYS, UTC_TSYS, IF_FRQ, LO_FRQ, &
     &           POL_FRQ, SCAN_NAME, SOURCE_TSYS, TSYS, STA_NAM, YEAR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_PARSE_VLBA_LOG
! *                                                                      *
! * ## 11-NOV-2016 PIMA_PARSE_VLBA_LOG v1.0 (c) L. Petrov 11-NOV-2016 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      INTEGER*4  NBUF, M_TIM, M_FRQ, N_TSYS, N_FRQ, YEAR, IVRB, IUER
      CHARACTER  FIL_LOG*(*) , BUF(NBUF)*(*), SCAN_NAME(M_TIM)*(*), &
     &           POL_FRQ(M_FRQ)*(*), SOURCE_TSYS(M_TIM)*(*), STA_NAM*(*)
      INTEGER*4  MJD_TSYS(M_TIM)
      REAL*8     UTC_TSYS(M_TIM), UTC_ONS(2,M_TIM), IF_FRQ(M_FRQ), &
     &           LO_FRQ(M_FRQ), TSYS(M_FRQ,M_TIM)
      LOGICAL*1  LEX
      INTEGER*4    M_SUM, MIND, M_VLBA, M_BND
      PARAMETER  ( M_SUM  = 128*1024 )
      PARAMETER  ( MIND   = 128 )
      PARAMETER  ( M_VLBA =  11 )
      PARAMETER  ( M_BND  =   9 )
      CHARACTER    STA_NAM8(M_VLBA)*8, STA_NAM2(M_VLBA)*2, BAND_NAME(M_BND)*1
      INTEGER*4    N$
      DATA         (STA_NAM2(N$), STA_NAM8(N$), N$=1,M_VLBA) / &
     &                 'br', 'BR-VLBA ', &
     &                 'fd', 'FD-VLBA ', &
     &                 'hn', 'HN-VLBA ', &
     &                 'kp', 'KP-VLBA ', &
     &                 'la', 'LA-VLBA ', &
     &                 'ov', 'OV-VLBA ', &
     &                 'mk', 'MK-VLBA ', &
     &                 'nl', 'NL-VLBA ', &
     &                 'pt', 'PIETOWN ', &
     &                 'sc', 'SC-VLBA ', &
     &                 'gb', 'GBT-VLBA'  &
     &                                                       /
      DATA         ( BAND_NAME(N$), N$=1,M_BND) &
     &                  / &
     &               'p', &
     &               'l', &
     &               's', &
     &               'c', &
     &               'x', &
     &               'u', &
     &               'k', &
     &               'q', &
     &               'w'  &
     &                    /                
!
      CHARACTER  FIL_SUM*128, SOU_SCA(PIM__MSCA)*10, DATE_STR*128, &
     &           YEAR_STR*4, SCAN_NAME_STR*32, EXP_NAME*16, FIL_CNT*128, &
     &           CNT(PIM__MSCA)*256, EXPER_DIR*256, FIL_FRQ*128
      CHARACTER, ALLOCATABLE :: BUF_SUM(:)*512, BUF_FRQ(:)*256
      REAL*8     TIM_SCA_BEG(PIM__MSCA), TIM_SCA_END(PIM__MSCA), SEC, MJD_R8, &
     &           FRQ_ARR(PIM__MFRQ)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, LS, N_SUM, IND(2,MIND), LIND, &
     &           SCA_IND, ID, NSCA, MJD_SCA_BEG(PIM__MSCA), ISTA, NS, DOY, &
     &           MJD_SCA_END(PIM__MSCA), IYEAR, MJD_BEG, N_CNT, NF, LF, &
     &           IL, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX, LTM_DIF
!
      IL = ILEN ( FIL_LOG )
      FIL_SUM = FIL_LOG(1:IL-8)//'.sum'
      INQUIRE ( FILE=FIL_SUM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1411, IUER, 'PIMA_PARSE_VLBA_TSYS', 'Did not find '// &
     &         'summary file '//FIL_SUM )
           RETURN 
      END IF
      ISTA = LTM_DIF ( 0, M_VLBA, STA_NAM2, FIL_LOG(IL-6:IL-5) )
      IF ( ISTA < 1 ) THEN
           CALL ERR_LOG ( 1412, IUER, 'PIMA_PARSE_VLBA_TSYS', 'Station '// &
     &          FIL_LOG(IL-6:IL-5)//' is not in the VLBA. Please check '// &
     &         'the name of the log file '//FIL_LOG )
           RETURN 
      END IF
      STA_NAM = STA_NAM8(ISTA)
!
      ALLOCATE ( BUF_SUM(M_SUM), STAT=IER )
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FIL_SUM, M_SUM, BUF_SUM, N_SUM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1413, IUER, 'PIMA_PARSE_VLBA_TSYS', 'Failure '// &
     &         'in reading summary file '//FIL_SUM )
           RETURN 
      END IF
!
      LS = 0
      NSCA = 0
      DO 410 J1=1,N_SUM
         IF ( LEN(BUF_SUM(J1)) == 0 ) GOTO 410
         IF ( BUF_SUM(J1)(1:11) == '  Start Day' ) THEN
              YEAR_STR = BUF_SUM(J1)(32:35)
              CALL CHIN ( YEAR_STR, IYEAR )
         END IF
         IF ( BUF_SUM(J1)(1:9) == 'SCAN  DAY' ) THEN
              LS = LS + 1
         END IF
         IF ( BUF_SUM(J1)(1:25) == ' TIME RANGE OF RECORDINGS' ) THEN
              LS = 0
         END IF
         IF ( LS > 0 .AND. BUF_SUM(J1)(1:4) .NE. '    ' ) THEN
              CALL EXWORD ( BUF_SUM(J1), MIND, LIND, IND, ' '//CHAR(9), IER )
              CALL CHIN ( BUF_SUM(J1)(1:4), SCA_IND )
              IF ( SCA_IND > 0 .AND. SCA_IND .LE. PIM__MSCA ) THEN
                   NSCA = SCA_IND
                   SOU_SCA(NSCA) = BUF_SUM(J1)(IND(1,4):IND(2,4))
!
                   CALL CHIN ( BUF_SUM(J1)(IND(1,2):IND(2,2)), DOY )
                   DATE_STR = YEAR_STR//'.01.01_'//BUF_SUM(J1)(IND(1,3):IND(2,3))
                   CALL DATE_TO_TIME ( DATE_STR, MJD_SCA_BEG(NSCA), &
     &                                 TIM_SCA_BEG(NSCA), IER )
                   MJD_SCA_BEG(NSCA) = MJD_SCA_BEG(NSCA) + DOY - 1
!
                   CALL CHIN ( BUF_SUM(J1+1)(IND(1,2):IND(2,2)), DOY )
                   DATE_STR = YEAR_STR//'.01.01_'//BUF_SUM(J1+1)(IND(1,3):IND(2,3))
                   CALL DATE_TO_TIME ( DATE_STR, MJD_SCA_END(NSCA), &
     &                                 TIM_SCA_END(NSCA), IER )
                   MJD_SCA_END(NSCA) = MJD_SCA_END(NSCA) + DOY - 1
              END IF
         END IF
 410  CONTINUE 
      DEALLOCATE   ( BUF_SUM )
      WRITE ( 6, * ) 'NSCA =', NSCA, ' IYEAR= ', IYEAR
!
!      do 510 j1=1,nsca
!         write ( 6, 210 ) j1, sou_sca(j1), mjd_sca_beg(j1), tim_sca_beg(j1), mjd_sca_end(j1), tim_sca_end(j1) ! %%%%
! 210     format ( i5, ' sou: ', a, ' beg: ', i5, 1x, f8.1,' end: ', i5, 1x, f8.1 ) ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 510  continue 
!
      N_TSYS = 0
      DO 420 J2=1,NBUF
         IF ( ILEN(BUF(J2)) == 0  ) GOTO 420
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, ' '//CHAR(9), IER )
         IF ( LIND < 3 ) GOTO 420
         IF ( BUF(J2)(IND(1,2):IND(2,2)) == 'Scan' ) THEN
              SCAN_NAME_STR = BUF(J2)(IND(1,3):IND(2,3))
              CALL CHIN ( BUF(J2)(IND(1,3)+2:IND(2,3)), SCA_IND )
         END IF
         IF ( BUF(J2)(1:1) == '#' ) GOTO 420
         READ ( UNIT=BUF(J2)(IND(1,2):IND(2,2)), FMT='(F14.8)' ) MJD_R8
         N_TSYS = N_TSYS + 1
         MJD_TSYS(N_TSYS) = MJD_R8
         UTC_TSYS(N_TSYS) = (MJD_R8 - MJD_TSYS(N_TSYS))*86400.0D0
         READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(I4)' ) N_FRQ
         DO 430 J3=1,N_FRQ
            READ ( UNIT=BUF(J2)(IND(1,5+(J3-1)*2):IND(2,5+(J3-1)*2)), FMT='(F7.2)' ) TSYS(J3,N_TSYS)
 430     CONTINUE 
         SCAN_NAME(N_TSYS)   = SCAN_NAME_STR
         SOURCE_TSYS(N_TSYS) = SOU_SCA(SCA_IND)
 420  CONTINUE 
!
      ID = LINDEX ( FIL_LOG, '/' )
      EXP_NAME = FIL_LOG(ID+1:IL-8)
      WRITE ( 6, * ) 'Exp_name: ', EXP_NAME
!
      CALL CLRCH ( FIL_CNT )
      DO 440 J4=1,M_BND
         FIL_CNT = FIL_LOG(1:IL-8)//'_'//BAND_NAME(J4)//'_pima.cnt'
         INQUIRE ( FILE=FIL_CNT, EXIST=LEX )
         IF ( LEX ) GOTO 840
 440  CONTINUE 
 840  CONTINUE 
      WRITE ( 6, * ) 'FIL_CNT = ', TRIM(FIL_CNT)
      IF ( ILEN(FIL_CNT) == 0 ) THEN
           CALL ERR_LOG ( 1414, IUER, 'PIMA_PARSE_VLBA_TSYS', 'Did not '// &
     &         'find PIMA control file for experiment '//EXP_NAME )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FIL_CNT, PIM__MSCA, CNT, N_CNT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1415, IUER, 'PIMA_PARSE_VLBA_TSYS', 'Failure '// &
     &         'in reading PIMA control file '//FIL_CNT )
           RETURN 
      END IF
!
      CALL CLRCH ( EXPER_DIR )
      DO 450 J5=1,N_CNT
         IF ( CNT(J5)(1:10) == 'EXPER_DIR:' ) THEN
              EXPER_DIR = CNT(J5)(11:)
              CALL CHASHL ( EXPER_DIR )
         END IF
 450  CONTINUE 
      IF ( ILEN(EXPER_DIR) == 0 ) THEN
           CALL ERR_LOG ( 1416, IUER, 'PIMA_PARSE_VLBA_TSYS', 'Did not '// &
     &         'find keyword EXPER_DIR in PIMA control file '//FIL_CNT )
           RETURN 
      END IF
      WRITE ( 6, * ) 'EXPER_DIR = ', TRIM(EXPER_DIR)
      FIL_FRQ = TRIM(EXPER_DIR)//'/'//TRIM(EXP_NAME)//'.frq'
      INQUIRE ( FILE=FIL_FRQ, EXIST=LEX )
      IF ( ILEN(FIL_CNT) == 0 ) THEN
           CALL ERR_LOG ( 1414, IUER, 'PIMA_PARSE_VLBA_TSYS', 'Did not '// &
     &         'find PIMA control file for experiment '//EXP_NAME )
           RETURN 
      END IF
!
      ALLOCATE ( BUF_FRQ(8*PIM__MFRQ + PIM__MCHN), STAT=IER )
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_FRQ, 8*PIM__MFRQ + PIM__MCHN, BUF_FRQ, NF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1413, IUER, 'PIMA_PARSE_VLBA_TSYS', 'Failure '// &
     &         'in reading summary file '//FIL_SUM )
           RETURN 
      END IF
!
      LF = 0
      DO 460 J6=1,NF
         IF ( BUF_FRQ(J6)(1:8) == 'Ind_grp:' ) THEN
              LF = LF + 1
              CALL EXWORD ( BUF_FRQ(J6), MIND, LIND, IND, ' '//CHAR(9), IER )
              READ ( UNIT=BUF_FRQ(J6)(IND(1,7):IND(2,7)), FMT='(F20.10)' ) FRQ_ARR(LF)
              POL_FRQ(LF) = 'RCP'
         END IF
 460  CONTINUE 
      WRITE ( 6, * ) 'LF = ', LF
      IF ( LF == N_FRQ/2 ) THEN
           IND = 0
           DO 470 J7=1,LF
              IF_FRQ(1+(J7-1)*2)  = 1.D-6*FRQ_ARR(J7)
              LO_FRQ(1+(J7-1)*2)  = 0.0D0
              POL_FRQ(1+(J7-1)*2) = 'RCP'
!
              IF_FRQ(2+(J7-1)*2)  = 1.D-6*FRQ_ARR(J7)
              LO_FRQ(2+(J7-1)*2)  = 0.0D0
              POL_FRQ(2+(J7-1)*2) = 'LCP'
 470       CONTINUE 
         ELSE
           IF_FRQ(1:N_FRQ) = 1.D-6*FRQ_ARR(1:N_FRQ)
           LO_FRQ(1:N_FRQ) = 0.0D0
      END IF
!
      DEALLOCATE ( BUF_FRQ )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PARSE_VLBA_TSYS !#!#
