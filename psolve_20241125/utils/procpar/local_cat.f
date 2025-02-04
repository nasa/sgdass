      PROGRAM    LOCAL_CAT
! ************************************************************************
! *                                                                      *
! *   Program  LOCAL_CAT  reads the file with source position series     *
! *   in getpar .lso format, computes the weithed average of source      *
! *   positions and wrms of devisions from the weighted mean.            *
! *   It ignores recors in ./lso files fir the sources with less than    *
! *   2 observatinos. NB: in computing statistics, the total number of   *
! *   of observations etc, it does not count the sessions with 0 or 1    *
! *   used observations.                                                 *
! *                                                                      *
! *  ### 21-MAY-2003   LOCAL_CAT   v1.2 (c)  L. Petrov  03-JUN-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M_SOU, M_SES
      PARAMETER  ( M_SOU = 2048, M_SES = 2048 )
      CHARACTER  FILIN*128, FILOUT*128, STR*256
      LOGICAL*4  LEX
      REAL*8     COO(2,M_SES,M_SOU), COV(3,M_SES,M_SOU), TIM(M_SES,M_SOU)
      REAL*8     COO_ARR(M_SOU,2), SIG_ARR(M_SOU,2), CORR_ARR(M_SOU)
      INTEGER*4  NOBS_ARR(M_SOU), NTOB_ARR(M_SOU), NSES_ARR(M_SOU)
      CHARACTER  OUT*256, DAT_SES(M_SES,M_SOU)*10, NAME_ARR(M_SOU), &
     &           DAT_BEG_ARR(M_SOU)*10, DAT_END_ARR(M_SOU)*10
      REAL*8     TM, ALP, DEL, SIG_ALP, SIG_DEL, ALP_AV, DEL_AV, &
     &           ALP_WW, DEL_WW, ALP_DS, DEL_DS, COV1_AV, COV2_AV, COV3_AV, &
     &           CORR
      REAL*8     PI, RAD__TO__MAS, MAS__TO__RAD
      PARAMETER  ( PI = 3.141592653589793D0 )
      PARAMETER  ( RAD__TO__MAS = 3600.D0*1000.D0*180.D0/PI )
      PARAMETER  ( MAS__TO__RAD = PI/(3600.D0*180.D0*1000.D0) )
      INTEGER*4  NOBS(M_SES,M_SOU), NTOB(M_SES,M_SOU), NSES(M_SOU)
      CHARACTER  C_SOU(M_SOU)*8, SORT_SOU(M_SOU)*8, MOU(12)*3, VERS_STR*3
      DATA       MOU / &
     &                  'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
     &                  'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'  &
     &               /
      INTEGER*4  IOS, L_SOU, KOBS, KSES, IND, IS, IM, IY, J1, J2, J3, J4, J5, &
     &           IUER
      INTEGER*4  I_LEN, LTM_DIF
!
!      FILIN  = '/tmp/2003c.lso'
!      FILOUT = '/tmp/2003c.lso_sou'
!
      IF ( IARGC() .LT. 2 ) THEN
           WRITE ( 6, * ) 'Usage: local_cat <input_file> <output_file>'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, FILOUT )
      END IF
!
! --- Check the input file
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 511, -1, 'LOCAL_CAT', 'Input file '// &
     &          FILIN(1:I_LEN(FILIN))//' was not found' )
           CALL EXIT ( 1 )
      END IF
!
! --- Open the input file
!
      OPEN ( UNIT=11, FILE=FILIN, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 512, -1, 'LOCAL_CAT', 'Error '//STR(1:I_LEN(STR))// &
     &         ' in an attempt to open file '//FILIN )
           CALL EXIT ( 2 )
      END IF
!
! --- Check the first line of the input file
!
      READ ( UNIT=11, FMT='(A)' ) STR
      IF ( STR(1:46) .EQ. '# GETPAR_LSO format version 1.1  of 2001.12.23' ) THEN
           VERS_STR = '1.1'
        ELSE IF ( STR(1:46) .EQ. '# GETPAR_LSO format version 2.0  of 2021.06.02' ) THEN
           VERS_STR = '2.0'
        ELSE 
           IUER = -1
           CALL ERR_LOG ( 513, IUER, 'LOCAL_CAT', 'Wrong header of the '// &
     &         'input lso file '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
! --- First pass. Read all records of lso-file
!
      L_SOU = 0
      DO 410 J1=1,1024*1024
         READ ( UNIT=11, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .EQ. -1 ) THEN
              GOTO 810
            ELSE IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) 'J1=',J1,' IOS=',IOS
              CALL ERR_LOG ( 514, -1, 'LOCAL_CAT', 'Error in reading the '// &
     &             'input lso file '//FILIN )
              CALL EXIT ( 4 )
         END IF
         IF ( STR(1:8) .NE. 'SOU_LCO:' ) GOTO 410  ! not a valid data record
!
         IF ( VERS_STR == "1.1" ) THEN
              CALL CHIN ( STR(151:154), KOBS )
           ELSE IF ( VERS_STR == "2.0" ) THEN
              CALL CHIN ( STR(163:167), KOBS )
         END IF
         IF ( KOBS .LT. 2 ) GOTO 410  ! Too few observations: bypass
!
! ------ Decode right ascension and transform into radians
!
         IUER = -1
         IF ( VERS_STR == "1.1" ) THEN
              CALL HR_TAT ( STR(63:79),   ALP, IUER )
           ELSE IF ( VERS_STR == "2.0" ) THEN
              CALL HR_TAT ( STR(75:91),   ALP, IUER )
         END IF
!
! ------ Decode declination and transform into radians
!
         IUER = -1
         IF ( VERS_STR == "1.1" ) THEN
              CALL GR_TAT ( STR(100:116), DEL, IUER )
           ELSE IF ( VERS_STR == "2.0" ) THEN
              CALL GR_TAT ( STR(112:128), DEL, IUER )
         END IF
!
! ------ Decode formal uncertainties and transform then in radians
!
         IF ( VERS_STR == "1.1" ) THEN
              READ ( UNIT=STR(84:93),   FMT='(F10.5)' )  SIG_ALP
              READ ( UNIT=STR(121:130), FMT='(F10.5)' )  SIG_DEL
              READ ( UNIT=STR(47:56),   FMT='(F10.5)' )  TM
              READ ( UNIT=STR(138:144), FMT='(F7.3)'  )  CORR
            ELSE IF ( VERS_STR == "2.0" ) THEN
              READ ( UNIT=STR(96:105),  FMT='(F10.5)' )  SIG_ALP
              READ ( UNIT=STR(133:142), FMT='(F10.5)' )  SIG_DEL
              READ ( UNIT=STR(59:68),   FMT='(F10.5)' )  TM
              READ ( UNIT=STR(150:156), FMT='(F7.3)'  )  CORR
         END IF
!
         SIG_ALP = SIG_ALP*MAS__TO__RAD
         SIG_DEL = SIG_DEL*MAS__TO__RAD
         IS = LTM_DIF ( 1, L_SOU, C_SOU, STR(11:18) )
         IF ( IS .LE. 0 ) THEN
              L_SOU = L_SOU + 1
              IS = L_SOU
              NSES(L_SOU) = 0
              C_SOU(L_SOU) = STR(11:18)
         END IF
!
! ------ Store results
!
         NSES(IS) = NSES(IS) + 1
         NOBS(NSES(IS),IS)  = KOBS
         CALL CHIN ( STR(158:161), NTOB(NSES(IS),IS) )
         COO(1,NSES(IS),IS) = ALP
         COO(2,NSES(IS),IS) = DEL
         COV(1,NSES(IS),IS) = SIG_ALP**2
         COV(2,NSES(IS),IS) = SIG_ALP*SIG_DEL*CORR
         COV(3,NSES(IS),IS) = SIG_DEL**2
         TIM(NSES(IS),IS)   = (TM-2000.0D0)*365.25D0*86400.0D0
         DAT_SES(NSES(IS),IS) = 'xx'//STR(22:23)//'.mm.'//STR(27:28)
         IF ( VERS_STR == "1.1" ) THEN
              CALL CHIN ( STR(22:23), IY )
              IM = LTM_DIF ( 1, 12, MOU, STR(24:26) )
            ELSE IF ( VERS_STR == "2.0" ) THEN
              CALL CHIN ( STR(33:34), IY )
              IM = LTM_DIF ( 1, 12, MOU, STR(35:36) )
         END IF
         IF ( IY .LT. 70 ) THEN
              DAT_SES(NSES(IS),IS)(1:2) = '20'
            ELSE
              DAT_SES(NSES(IS),IS)(1:2) = '19'
         END IF
         CALL INCH ( IM, DAT_SES(NSES(IS),IS)(6:7) )
         CALL CHASHR (   DAT_SES(NSES(IS),IS)(6:7) )
         CALL BLANK_TO_ZERO ( DAT_SES(NSES(IS),IS)(6:7) )
 410  CONTINUE
 810  CONTINUE
      CLOSE ( UNIT=11 )
!
! --- Second pass: look for all sources
!
      DO 420 J2=1,L_SOU
         NOBS_ARR(J2) = 0
         NTOB_ARR(J2) = 0
         NSES_ARR(J2) = 0
         IF ( NSES(J2) .GE. 2 ) THEN
!
! ----------- The source was observed in more than one session. Lets
! ----------- compute statistics
!
              ALP_AV = 0.0D0
              DEL_AV = 0.0D0
              ALP_WW = 0.0D0
              DEL_WW = 0.0D0
              KSES = 0
!
! ----------- Compute weighted average
!
              DO 430 J3=1,NSES(J2)
                 IF ( NOBS(J3,J2) .GE. 2 ) THEN
                      KSES = KSES + 1
                      ALP_AV = ALP_AV + COO(1,J3,J2)/DSQRT(COV(1,J3,J2))
                      DEL_AV = DEL_AV + COO(2,J3,J2)/DSQRT(COV(3,J3,J2))
                      ALP_WW = ALP_WW + 1.0D0/DSQRT(COV(1,J3,J2))
                      DEL_WW = DEL_WW + 1.0D0/DSQRT(COV(3,J3,J2))
                      IF ( KSES .EQ. 1 ) THEN
                           DAT_BEG_ARR(J2) = DAT_SES(J3,J2)
                      END IF
                 END IF
 430          CONTINUE
              IF ( KSES .LT. 2 ) THEN
                   GOTO 420
              END IF
!
! ----------- Compute the wrms
!
              ALP_AV = ALP_AV/ALP_WW
              DEL_AV = DEL_AV/DEL_WW
              COV1_AV = 0.0D0
              COV2_AV = 0.0D0
              COV3_AV = 0.0D0
              ALP_WW  = 0.0D0
              DEL_WW  = 0.0D0
              ALP_DS  = 0.0D0
              DEL_DS  = 0.0D0
              DO 440 J4=1,NSES(J2)
                 IF ( NOBS(J4,J2) .GE. 2 ) THEN
                      ALP_DS = ALP_DS + ( COO(1,J4,J2) - ALP_AV)**2/COV(1,J4,J2)
                      DEL_DS = DEL_DS + ( COO(2,J4,J2) - DEL_AV)**2/COV(3,J4,J2)
!
                      ALP_WW = ALP_WW + 1.0D0/COV(1,J4,J2)
                      DEL_WW = DEL_WW + 1.0D0/COV(3,J4,J2)
!
                      COV1_AV = COV1_AV + COV(1,J4,J2)
                      COV2_AV = COV2_AV + COV(2,J4,J2)
                      COV3_AV = COV3_AV + COV(3,J4,J2)
!
                      NSES_ARR(J2) = NSES_ARR(J2) + 1
                      NOBS_ARR(J2) = NOBS_ARR(J2) + NOBS(J4,J2)
                      NTOB_ARR(J2) = NTOB_ARR(J2) + NTOB(J4,J2)
                      DAT_END_ARR(J2) = DAT_SES(J4,J2)
                 END IF
 440          CONTINUE
              COO_ARR(J2,1) = ALP_AV
              COO_ARR(J2,2) = DEL_AV
              SIG_ARR(J2,1) = DSQRT ( ALP_DS/ALP_WW )
              SIG_ARR(J2,2) = DSQRT ( DEL_DS/DEL_WW )
              CORR_ARR(J2)  = COV2_AV/DSQRT(COV1_AV*COV3_AV)
            ELSE IF ( NSES(J2) .EQ. 1 ) THEN
!
! ----------- Copy soutce coordinates and other information
!
              NSES_ARR(J2) = 1
              NOBS_ARR(J2) = NOBS(1,J2)
              NTOB_ARR(J2) = NTOB(1,J2)
              COO_ARR(J2,1) = COO(1,1,J2)
              COO_ARR(J2,2) = COO(2,1,J2)
              SIG_ARR(J2,1) = DSQRT ( COV(1,1,J2) )
              CORR_ARR(J2)  = COV(2,1,J2)/ DSQRT( COV(1,1,J2)*COV(3,1,J2) )
              SIG_ARR(J2,2) = DSQRT ( COV(3,1,J2) )
              DAT_BEG_ARR(J2) = DAT_SES(1,J2)
              DAT_END_ARR(J2) = DAT_SES(1,J2)
         END IF
 420  CONTINUE
!
! --- Source the source names
!
      CALL LIB$MOVC3 ( L_SOU*8, %REF(C_SOU), %REF(SORT_SOU) )
      CALL SORT_CH   ( L_SOU, SORT_SOU )
!
! --- Open the output file
!
      OPEN ( UNIT=22, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) 'IOS=',IOS
           CALL ERR_LOG ( 515, -1, 'LOCAL_CAT', 'Error in an attempt to '// &
     &         'open the output file '//FILOUT )
           CALL EXIT ( 5 )
      END IF
!
! --- Write the header comments
!
      WRITE ( 22, '(A)' ) '# GETPAR_SOU format version 1.0  of 2001.05.25'
      WRITE ( 22, '(A)' ) '# Made by LOCAL_CAT using file '// &
     &                       FILIN(1:I_LEN(FILIN))
      DO 450 J5=1,L_SOU
         IND = LTM_DIF ( 1, L_SOU, C_SOU, SORT_SOU(J5) )
         IF ( NSES_ARR(IND) .LE. 0 ) GOTO 450
!
! ------ Prepare the data line
!
         CALL CLRCH ( OUT )
         OUT(1:8) = 'SOU_GCO:'
         OUT(21:22) = 'R:'
         OUT(43:44) = '-+'
         OUT(58:59) = 'D:'
         OUT(80:81) = '-+'
         OUT(95:96) = 'C:'
         OUT(106:114) = 'Obs_used:'
         OUT(124:131) = 'Obs_tot:'
         OUT(141:149) = 'Ses_used:'
         OUT(157:164) = 'Ses_tot:'
         OUT(172:180) = 'Date_beg:'
         OUT(193:201) = 'Date_end:'
!
         OUT(11:18)   = C_SOU(IND)
         CALL RH_TAT ( COO_ARR(IND,1), 8, OUT(24:41), -3 )
         OUT(43:44) = '-+'
         CALL RG_TAT ( COO_ARR(IND,2), 7, OUT(62:78), -3 )
         WRITE (  UNIT=OUT(46:55),   FMT='(F10.4)' ) SIG_ARR(IND,1)*RAD__TO__MAS
         WRITE (  UNIT=OUT(83:92),   FMT='(F10.4)' ) SIG_ARR(IND,2)*RAD__TO__MAS
         WRITE (  UNIT=OUT(99:104),  FMT='(F6.4)'  ) CORR_ARR(IND)
         WRITE (  UNIT=OUT(116:122), FMT='(I7)'    ) NOBS_ARR(IND)
         CALL CHASHR ( OUT(116:122) )
         WRITE (  UNIT=OUT(133:139), FMT='(I7)'    ) NTOB_ARR(IND)
         CALL CHASHR ( OUT(133:139) )
         WRITE (  UNIT=OUT(151:155), FMT='(I5)'    ) NSES_ARR(IND)
         CALL CHASHR ( OUT(151:155) )
         OUT(166:170) = OUT(151:155)
         OUT(182:191) = DAT_BEG_ARR(IND)
         OUT(203:212) = DAT_END_ARR(IND)
!
! ------ Write down the data line
!
         WRITE ( 22, '(A)' ) OUT(1:212)
 450  CONTINUE
      CLOSE ( UNIT=22 )
      WRITE ( 6, * ) L_SOU, ' local sources'
      WRITE ( 6, * ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
!
      END  !#!  LOCAL_CAT  #!#
