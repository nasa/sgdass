      SUBROUTINE READ_SOU ( FILIN, MAX_SOU, L_SOU, SOUCAT, C_SOU, MODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_SOU  reads the catalogue in either getpar format     *
! *   or in astro_cat format and returns the array of record with        *
! *   information foudn in the catalogue.                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   FILIN ( CHARACTER ) -- File with the source catalogue.             *
! * MAX_SOU ( INTEGER*4 ) -- Maximal number of sources in the catalogue. *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   L_SOU ( INTEGER*4 ) -- The number of sources in the catalogue.     *
! *  SOUCAT ( RECORD    ) -- Array of object which contains information  *
! *                          extractd from parsing the catalogue.        *
! *                          Dimension: MAX_SOU.                         *
! *   C_SOU ( CHARACTER ) -- Array of soure names. Dimension: MAX_SOU.   *
! *    MODE ( INTEGER*4 ) -- format of the catalogue:                    *
! *                          1) getpar-sou                               *
! *                          2) astro_cat                                *
! *                          3) getpar-lso                               *
! *                          4) sou-tab file                             *
! *                          5) solve mod-sou                            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 21-JAN-2004    READ_SOU   v2.2 (c)  L. Petrov  06-FEB-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'getpar.i'
      INTEGER*4  MAX_SOU, L_SOU, MODE, IUER
      CHARACTER  FILIN*(*), C_SOU(MAX_SOU)*(*)
      TYPE ( SOURCE_CAT__TYPE ) :: SOUCAT(MAX_SOU)
      CHARACTER,  ALLOCATABLE   :: BUF(:)*256
      CHARACTER  STR*80
      INTEGER*4  J1, NBUF, IOS, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      ALLOCATE ( BUF(MAX_SOU), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( IOS, STR )
           CALL ERR_LOG ( 2831, IUER, 'READ_SOU', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FILIN, MAX_SOU, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2832, IUER, 'READ_SOU', 'Error in reading '// &
     &         'source catalogie '//FILIN )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      IF ( BUF(1)(1:62) == &
     &    '# GETPAR_SOU format version 1.0  of 2001.05.25' ) THEN
           MODE = 1
         ELSE IF ( BUF(1)(1:62) == &
     &    '# VLBI SOURCE POSITION CATALOGUE  Format version of 2004.08.20' ) THEN
           MODE = 2
         ELSE IF ( BUF(1)(1:62) == &
     &    '# VLBI SOURCE POSITION CATALOGUE  Format version of 2010.07.21' ) THEN
           MODE = 3
         ELSE IF ( BUF(1)(1:62) == &
     &    '# VLBI SOURCE POSITION CATALOGUE  Format version of 2010.09.11' ) THEN
           MODE = 4
         ELSE IF ( BUF(1)(1:62) == '# SOURCE-NAMES  v 2.0 2005.09.06' ) THEN
           MODE = 5
         ELSE IF ( BUF(1)(1:32) == '$$  SOU-MODFILE Format pre-2000 ' ) THEN
           MODE = 6
         ELSE IF ( BUF(1)(1:67) == '# CATRES Flux and Spectral index file. Format version of 2004.12.18' ) THEN
           MODE = 7
         ELSE
           CALL ERR_LOG ( 2833, IUER, 'READ_SOU', 'Unsupported format of '// &
     &         'the source catalogue '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- the first line is '//BUF(1) )
           DEALLOCATE ( BUF )
           RETURN
      END IF
!
      L_SOU = 0
      DO 410 J1=1,NBUF ! Cycle over Goddard solution
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '$' ) GOTO 410
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410
!
! ------ Get the entry from the intput catalogue and compute a semi-major
! ------ axis of the error ellipse
!
         L_SOU = L_SOU + 1
         CALL ERR_PASS       ( IUER, IER )
         CALL READ_SOU_LINE  ( MODE, BUF(J1), SOUCAT(L_SOU), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2834, IUER, 'READ_SOU', 'Error in parsing line '// &
     &             BUF(J1)(1:I_LEN(BUF(J1)))//' of the input file '// &
     &             FILIN )
              RETURN
         END IF
         C_SOU(L_SOU) = SOUCAT(L_SOU)%IVS_NAME
         SOUCAT(L_SOU)%IND_LINE = J1
 410  CONTINUE
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_SOU  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_SOU_LINE ( MODE, STR, SOUCAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_SOU_LINE  parses one line of the buffer with source  *
! *   catalogue and puts results in SOURCAT object.                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MODE ( INTEGER*4 ) -- Format of the catalogue:                    *
! *                          1) getpar-sou                               *
! *                          2) astro_cat                                *
! *                          3) getpar-lso                               *
! *     STR ( CHARACTER ) -- Line with contents of the catalogue record. *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  SOUCAT ( RECORD    ) -- Array of object which contains information  *
! *                          extractd from parsing the catalogue.        *
! *                                                                      *
! * ### 03-DEC-2001  READ_SOU_LINE   v4.3 (c) L. Petrov 11-SEP-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'getpar.i'
      TYPE ( SOURCE_CAT__TYPE ) :: SOUCAT
      INTEGER*4  MIND
      PARAMETER  ( MIND = 32 )
      INTEGER*4  MODE, IUER
      CHARACTER  STR*(*), SGN_CHR*1, ALP_STR*16, DEC_STR*16, J2000_NAME*10, &
     &           REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      REAL*8     SEC, ERR2, TETA
      INTEGER*4  IHR, IMN, IDG, SGN, LIND, IND(2,MIND), ALP_NUM, DEL_NUM, &
     &           IP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Initialization
!
      SOUCAT%CALIB = 0
      SOUCAT%NSES_TOTAL = 0
      SOUCAT%NSES_USED  = 0
      CALL CLRCH  ( SOUCAT%J2000_NAME )
      CALL CLRCH  ( SOUCAT%DAT_BEG  )
      CALL CLRCH  ( SOUCAT%DAT_END  )
      CALL CLRCH  ( SOUCAT%SESS     )
      SOUCAT%FLUX_TOT_S = 0.0D0
      SOUCAT%FLUX_UNR_S = 0.0D0
      SOUCAT%FLUX_TOT_C = 0.0D0
      SOUCAT%FLUX_UNR_C = 0.0D0
      SOUCAT%FLUX_TOT_X = 0.0D0
      SOUCAT%FLUX_UNR_X = 0.0D0
      SOUCAT%FLUX_TOT_U = 0.0D0
      SOUCAT%FLUX_UNR_U = 0.0D0
      SOUCAT%FLUX_TOT_K = 0.0D0
      SOUCAT%FLUX_UNR_K = 0.0D0

      SOUCAT%EPOCH_J2000_SEC = 0.0D0
!
      IF ( MODE .EQ. 1 ) THEN
           SOUCAT%IVS_NAME = STR(11:18)
           READ ( UNIT=STR(25:41), FMT='(I2,1X,I2,1X,F11.8)' ) IHR, IMN, SEC
           SOUCAT%ALP = ( DBLE(IHR) + DBLE(IMN)/60.D0 + SEC/3600.0D0 )/ &
     &                    12.0D0*PI__NUM
!
           READ ( UNIT=STR(46:55), FMT='(F10.0)' ) SOUCAT%ALP_ERR
           SOUCAT%ALP_ERR = SOUCAT%ALP_ERR*MAS__TO__RAD
!
           READ ( UNIT=STR(62:78), FMT='(A1,I2,1X,I2,1X,F10.7)' ) SGN_CHR, &
     &                                   IDG, IMN, SEC
           IF ( SGN_CHR .EQ. '-' ) THEN
                SGN = -1
              ELSE
                SGN =  1
           END IF
           SOUCAT%DEL = SGN*( DABS(DBLE(IDG)) + DBLE(IMN)/60.D0 + &
     &                        SEC/3600.0D0 )/180.0D0*PI__NUM
!
           READ ( UNIT=STR(83:92), FMT='(F10.0)' ) SOUCAT%DEL_ERR
           SOUCAT%DEL_ERR = SOUCAT%DEL_ERR*MAS__TO__RAD
!
           IF ( STR(98:104) == ' ******' ) THEN
                STR(98:104) = '0.99999'
           END IF
           READ ( UNIT=STR(98:104),  FMT='(F7.0)' ) SOUCAT%CORR
           READ ( UNIT=STR(116:122), FMT='(I7)' ) SOUCAT%NOBS_USED
           READ ( UNIT=STR(133:139), FMT='(I7)' ) SOUCAT%NOBS_TOTAL
           READ ( UNIT=STR(151:155), FMT='(I5)' ) SOUCAT%NSES_USED
           READ ( UNIT=STR(166:170), FMT='(I5)' ) SOUCAT%NSES_TOTAL
           SOUCAT%DAT_BEG = STR(182:191) 
           SOUCAT%DAT_END = STR(203:212) 
         ELSE IF ( MODE .EQ. 2 ) THEN
           IF ( STR(1:1) .EQ. 'c'  .OR.  STR(1:1) .EQ. 'C' ) THEN
                SOUCAT%CALIB = 1
           END IF
           IF ( STR(1:1) .EQ. 'n'  .OR.  STR(1:1) .EQ. 'N' ) THEN
                SOUCAT%CALIB = 2
           END IF
           IF ( STR(1:1) .EQ. 'u'  .OR.  STR(1:1) .EQ. 'U' ) THEN
                SOUCAT%CALIB = 3
           END IF
           IF ( STR(1:1) .EQ. 'g'  .OR.  STR(1:1) .EQ. 'G' ) THEN
                SOUCAT%CALIB = 4
           END IF
           SOUCAT%IVS_NAME = STR(4:11)
           SOUCAT%J2000_NAME = STR(13:22)
           READ ( UNIT=STR(25:39), FMT='(I2,1X,I2,1X,F9.6)' ) IHR, IMN, SEC
           SOUCAT%ALP = ( DBLE(IHR) + DBLE(IMN)/60.D0 + SEC/3600.0D0 )/ &
     &                  12.0D0*PI__NUM
!
           READ ( UNIT=STR(58:63), FMT='(F6.0)' ) SOUCAT%ALP_ERR
           SOUCAT%ALP_ERR = SOUCAT%ALP_ERR*MAS__TO__RAD
!
           READ ( UNIT=STR(41:57), FMT='(A1,I2,1X,I2,1X,F8.5)' ) SGN_CHR, IDG, &
     &                                                           IMN, SEC
           IF ( SGN_CHR .EQ. '-' ) THEN
                SGN = -1
              ELSE
                SGN =  1
           END IF
           SOUCAT%DEL = SGN*( DABS(DBLE(IDG)) + DBLE(IMN)/60.D0 + &
     &                        SEC/3600.0D0 )/180.0D0*PI__NUM
!
! -------- Build B1950 name. For > 95% IVS name == B1950 name, but there
! -------- are exceptions. Let us check whether the name folows B1950 convention
!
           CALL CHIN ( SOUCAT%B1950_NAME(1:4), ALP_NUM )
           CALL CHIN ( SOUCAT%B1950_NAME(6:7), DEL_NUM )
           IF ( ALP_NUM .GE. 0 .AND. ALP_NUM < 2400  .AND. &
     &          DEL_NUM .GE. 0 .AND. DEL_NUM < 90    .AND. &
     &          ( SOUCAT%B1950_NAME(5:5) == '+'  .OR. &
     &            SOUCAT%B1950_NAME(5:5) == '-'       ) ) THEN
!
! ------------- Yes
!
                SOUCAT%B1950_NAME = SOUCAT%IVS_NAME
              ELSE
!
! ------------- No. Then build the name from coordinates
!
                CALL SOUCOO_TO_NAME ( SOUCAT%ALP, SOUCAT%DEL, &
     &                                J2000_NAME, SOUCAT%B1950_NAME )
           END IF
           SOUCAT%B1950_NAME = SOUCAT%IVS_NAME  !!!!! 2021.09.18_21:26:06
!
           READ ( UNIT=STR(65:70), FMT='(F6.0)' ) SOUCAT%DEL_ERR
           SOUCAT%DEL_ERR = SOUCAT%DEL_ERR*MAS__TO__RAD
!
           READ ( UNIT=STR(73:78), FMT='(F6.0)' ) SOUCAT%CORR
           READ ( UNIT=STR(80:85), FMT='(I6)' ) SOUCAT%NOBS_USED
           SOUCAT%NSES_USED = 1
!
           IF ( INDEX ( STR(89:92), 'n/a' ) > 0 ) STR(89:92) = '-1.0'
           READ ( UNIT=STR(89:92), FMT='(F4.0)' ) SOUCAT%FLUX_TOT_X
           IF ( STR(88:88) == '<'   ) SOUCAT%FLUX_TOT_X = 0.0
           IF ( STR(88:88) == '-'   ) SOUCAT%FLUX_TOT_X = 0.0
!
           IF ( INDEX ( STR(95:98), 'n/a' ) > 0 ) STR(95:98) = '-1.0'
           READ ( UNIT=STR(95:98), FMT='(F4.0)' ) SOUCAT%FLUX_UNR_X
           IF ( STR(94:94) == '<'   ) SOUCAT%FLUX_UNR_X = 0.0
           IF ( STR(94:94) == '-'   ) SOUCAT%FLUX_UNR_X = 0.0
!
           IF ( INDEX ( STR(102:105), 'n/a' ) > 0 ) STR(102:105) = '-1.0'
           READ ( UNIT=STR(102:105), FMT='(F4.0)' ) SOUCAT%FLUX_TOT_S
           IF ( STR(101:101) == '<' ) SOUCAT%FLUX_TOT_S = 0.0
           IF ( STR(101:101) == '-' ) SOUCAT%FLUX_TOT_S = 0.0
!
           IF ( INDEX ( STR(108:111), 'n/a' ) > 0 ) STR(108:111) = '-1.0'
           READ ( UNIT=str(108:111), FMT='(F4.0)' ) SOUCAT%FLUX_UNR_S
           IF ( STR(107:107) == '<' ) SOUCAT%FLUX_UNR_S = 0.0
           IF ( STR(107:107) == '-' ) SOUCAT%FLUX_UNR_S = 0.0
         ELSE IF ( MODE .EQ. 3  .OR.  MODE .EQ. 4 ) THEN
           IF ( STR(1:1) .EQ. 'c'  .OR.  STR(1:1) .EQ. 'C' ) THEN
                SOUCAT%CALIB = 1
           END IF
           IF ( STR(1:1) .EQ. 'n'  .OR.  STR(1:1) .EQ. 'N' ) THEN
                SOUCAT%CALIB = 2
           END IF
           IF ( STR(1:1) .EQ. 'u'  .OR.  STR(1:1) .EQ. 'U' ) THEN
                SOUCAT%CALIB = 3
           END IF
           IF ( STR(1:1) .EQ. 'g'  .OR.  STR(1:1) .EQ. 'G' ) THEN
                SOUCAT%CALIB = 4
           END IF
           SOUCAT%IVS_NAME = STR(4:11)
           SOUCAT%J2000_NAME = STR(13:22)
           READ ( UNIT=STR(25:39), FMT='(I2,1X,I2,1X,F9.6)' ) IHR, IMN, SEC
           SOUCAT%ALP = ( DBLE(IHR) + DBLE(IMN)/60.D0 + SEC/3600.0D0 )/ &
     &                  12.0D0*PI__NUM
!
           READ ( UNIT=STR(58:63), FMT='(F5.0)' ) SOUCAT%ALP_ERR
           SOUCAT%ALP_ERR = SOUCAT%ALP_ERR*MAS__TO__RAD
!
           READ ( UNIT=STR(41:57), FMT='(A1,I2,1X,I2,1X,F8.5)' ) SGN_CHR, IDG, &
     &                                                           IMN, SEC
           IF ( SGN_CHR .EQ. '-' ) THEN
                SGN = -1
              ELSE
                SGN =  1
           END IF
           SOUCAT%DEL = SGN*( DABS(DBLE(IDG)) + DBLE(IMN)/60.D0 + &
     &                        SEC/3600.0D0 )/180.0D0*PI__NUM
!
! -------- Build B1950 name. For > 95% IVS name == B1950 name, but there
! -------- are exceptions. Let us check whether the name folows B1950 convention
!
           CALL CHIN ( SOUCAT%IVS_NAME(1:4), ALP_NUM )
           CALL CHIN ( SOUCAT%IVS_NAME(6:7), DEL_NUM )
           IF ( ALP_NUM .GE. 0 .AND. ALP_NUM < 2400  .AND. &
     &          DEL_NUM .GE. 0 .AND. DEL_NUM < 90    .AND. &
     &          ( SOUCAT%B1950_NAME(5:5) == '+'  .OR. &
     &            SOUCAT%B1950_NAME(5:5) == '-'       ) ) THEN
!
! ------------- Yes
!
                SOUCAT%B1950_NAME = SOUCAT%IVS_NAME
              ELSE
!
! ------------- No. Then build the name from coordinates
!
                CALL SOUCOO_TO_NAME ( SOUCAT%ALP, SOUCAT%DEL, &
     &                                J2000_NAME, SOUCAT%B1950_NAME )
           END IF
!
           READ ( UNIT=STR(65:70), FMT='(F6.0)' ) SOUCAT%DEL_ERR
           SOUCAT%DEL_ERR = SOUCAT%DEL_ERR*MAS__TO__RAD
!
           READ ( UNIT=STR(73:78), FMT='(F6.0)' ) SOUCAT%CORR
           READ ( UNIT=STR(80:85), FMT='(I6)' ) SOUCAT%NOBS_USED
           SOUCAT%NSES_USED = 1
!
           IF ( MODE == 3 ) THEN
                IF ( INDEX ( STR(88:93), ' n/a ' ) > 0 ) STR(88:93) = ' -1.0 '
                READ ( UNIT=STR(89:93), FMT='(F5.3)' ) SOUCAT%FLUX_TOT_X
                IF ( STR(88:88) == '<'   ) SOUCAT%FLUX_TOT_X = 0.0
                IF ( STR(88:88) == '-'   ) SOUCAT%FLUX_TOT_X = 0.0
!
                IF ( INDEX ( STR(95:100), ' n/a ' ) > 0 ) STR(95:100) = ' -1.0 '
                READ ( UNIT=STR(96:100), FMT='(F5.3)' ) SOUCAT%FLUX_UNR_X
                IF ( STR(95:95) == '<'   ) SOUCAT%FLUX_UNR_X = 0.0
                IF ( STR(95:95) == '-'   ) SOUCAT%FLUX_UNR_X = 0.0
!
                IF ( INDEX ( STR(103:108), ' n/a ' ) > 0 ) STR(103:108) = ' -1.0 '
                READ ( UNIT=STR(104:108), FMT='(F5.3)' ) SOUCAT%FLUX_TOT_K
                IF ( STR(103:103) == '<' ) SOUCAT%FLUX_TOT_K = 0.0
                IF ( STR(103:103) == '-' ) SOUCAT%FLUX_TOT_K = 0.0
!
                IF ( INDEX ( STR(110:115), ' n/a ' ) > 0 ) STR(110:115) = ' -1.0 '
                READ ( UNIT=STR(111:115), FMT='(F5.3)' ) SOUCAT%FLUX_UNR_K
                IF ( STR(110:110) == '<' ) SOUCAT%FLUX_UNR_K = 0.0
                IF ( STR(110:110) == '-' ) SOUCAT%FLUX_UNR_K = 0.0
!
                IF ( INDEX ( STR(118:118), '<' ) > 0     ) STR(118:123) = ' -1.0 '
                IF ( INDEX ( STR(118:123), ' n/a ' ) > 0 ) STR(118:123) = ' -1.0 '
                READ ( UNIT=STR(119:123), FMT='(F5.3)' ) SOUCAT%FLUX_TOT_S
                IF ( STR(118:118) == '<' ) SOUCAT%FLUX_TOT_S = 0.0
                IF ( STR(118:118) == '-' ) SOUCAT%FLUX_TOT_S = 0.0
!
                IF ( INDEX ( STR(125:125), '<' ) > 0     ) STR(125:130) = ' -1.0 '
                IF ( INDEX ( STR(125:130), ' n/a ' ) > 0 ) STR(125:130) = ' -1.0 '
                READ ( UNIT=STR(126:130), FMT='(F5.3)' ) SOUCAT%FLUX_UNR_S
                IF ( STR(125:125) == '<' ) SOUCAT%FLUX_UNR_S = 0.0
                IF ( STR(125:125) == '-' ) SOUCAT%FLUX_UNR_S = 0.0
              ELSE IF ( MODE == 4 ) THEN
                IF ( INDEX ( STR(88:93), ' n/a ' ) > 0 ) STR(88:93) = ' -1.0 '
                IF ( STR(88:88) == '<' .OR. STR(88:88) == '-' ) THEN
                     SOUCAT%FLUX_TOT_S = 0.0
                   ELSE 
                     READ ( UNIT=STR(88:93), FMT='(F6.3)' ) SOUCAT%FLUX_TOT_S
                END IF
!
                IF ( INDEX ( STR(95:100), ' n/a ' ) > 0 ) STR(95:100) = ' -1.0 '
                IF ( STR(95:95) == '<' .OR. STR(95:95) == '-' ) THEN
                     SOUCAT%FLUX_UNR_S = 0.0
                   ELSE 
                     READ ( UNIT=STR(95:100), FMT='(F6.3)' ) SOUCAT%FLUX_UNR_C
                END IF
!
                IF ( INDEX ( STR(103:108), ' n/a ' ) > 0 ) STR(103:108) = ' -1.0 '
                IF ( STR(103:103) == '<' .OR. STR(103:103) == '-' ) THEN
                     SOUCAT%FLUX_TOT_C = 0.0
                   ELSE
                     READ ( UNIT=STR(103:108), FMT='(F6.3)' ) SOUCAT%FLUX_TOT_C
                END IF
!
                IF ( INDEX ( STR(110:115), ' n/a ' ) > 0 ) STR(110:115) = ' -1.0 '
                IF ( STR(110:110) == '<' .OR. STR(110:110) == '-' ) THEN
                     SOUCAT%FLUX_UNR_C = 0.0
                   ELSE
                     READ ( UNIT=STR(110:115), FMT='(F6.3)' ) SOUCAT%FLUX_UNR_C
                END IF
!
                IF ( INDEX ( STR(118:123), ' n/a ' ) > 0 ) STR(118:123) = ' -1.0 '
                IF ( STR(118:118) == '<' .OR. STR(118:118) == '-' ) THEN
                     SOUCAT%FLUX_TOT_X = 0.0
                    ELSE 
                     READ ( UNIT=STR(118:123), FMT='(F6.3)' ) SOUCAT%FLUX_TOT_X
                END IF
!
                IF ( INDEX ( STR(125:130), ' n/a ' ) > 0 ) STR(125:130) = ' -1.0 '
                IF ( STR(125:125) == '<' .OR. STR(125:125) == '-' ) THEN
                     SOUCAT%FLUX_UNR_X = 0.0
                   ELSE
                     READ ( UNIT=STR(125:130), FMT='(F6.3)' ) SOUCAT%FLUX_UNR_X
                END IF
!
                IF ( INDEX ( STR(133:138), ' n/a ' ) > 0 ) STR(133:138) = ' -1.0 '
                IF ( STR(133:133) == '<' .OR. STR(133:133) == '-' ) THEN
                     SOUCAT%FLUX_TOT_U = 0.0
                   ELSE 
                     READ ( UNIT=STR(133:138), FMT='(F6.3)' ) SOUCAT%FLUX_TOT_U
                END IF
!
                IF ( INDEX ( STR(140:145), ' n/a ' ) > 0 ) STR(140:145) = ' -1.0 '
                IF ( STR(140:140) == '<' .OR. STR(140:140) == '-' ) THEN
                     SOUCAT%FLUX_UNR_U = 0.0
                   ELSE 
                     READ ( UNIT=STR(140:145), FMT='(F6.3)' ) SOUCAT%FLUX_UNR_U
                END IF
!
                IF ( INDEX ( STR(148:153), ' n/a ' ) > 0 ) STR(148:153) = ' -1.0 '
                IF ( STR(148:148) == '<' .OR. STR(148:148) == '-' ) THEN
                     SOUCAT%FLUX_TOT_K = 0.0
                   ELSE 
                     READ ( UNIT=STR(148:153), FMT='(F6.3)' ) SOUCAT%FLUX_TOT_K
                END IF
!
                IF ( INDEX ( STR(155:160), ' n/a ' ) > 0 ) STR(155:160) = ' -1.0 '
                IF ( STR(155:155) == '<' .OR. STR(155:155) == '-' ) THEN
                     SOUCAT%FLUX_UNR_K = 0.0
                   ELSE 
                     READ ( UNIT=STR(155:160), FMT='(F6.3)' ) SOUCAT%FLUX_UNR_K
                END IF
           END IF
         ELSE IF ( MODE .EQ. 5 ) THEN
           IF ( STR(43:43) .EQ. 'c'  .OR.  STR(43:43) .EQ. 'C' ) THEN
                SOUCAT%CALIB = 1
           END IF
           IF ( STR(43:43) .EQ. 'n'  .OR.  STR(43:43) .EQ. 'N' ) THEN
                SOUCAT%CALIB = 2
           END IF
           IF ( STR(43:43) .EQ. 'u'  .OR.  STR(43:43) .EQ. 'U' ) THEN
                SOUCAT%CALIB = 3
           END IF
           IF ( STR(43:43) .EQ. 'g'  .OR.  STR(43:43) .EQ. 'G' ) THEN
                SOUCAT%CALIB = 4
           END IF
           SOUCAT%IVS_NAME   = STR(1:8)
           SOUCAT%J2000_NAME = STR(11:20)
           SOUCAT%B1950_NAME = STR(23:30)
           SOUCAT%DB_NAME = STR(33:42)
!
           CALL ERR_PASS ( IUER, IER )
           CALL HR_TAT ( STR(46:58), SOUCAT%ALP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2841, IUER, 'READ_SOU_LINE', 'Error in '// &
     &              'decoding right ascension in line '//STR )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GR_TAT ( STR(60:72), SOUCAT%DEL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2842, IUER, 'READ_SOU_LINE', 'Error in '// &
     &              'decoding declination in line '//STR )
                RETURN
           END IF
           READ ( UNIT = STR(75:80), FMT='(F6.0)', IOSTAT=IER ) SOUCAT%SOU_ERR
           IF ( IER == 0 ) THEN
                SOUCAT%SOU_ERR = SOUCAT%SOU_ERR*MAS__TO__RAD
              ELSE
                SOUCAT%SOU_ERR = 999.99*MAS__TO__RAD
           END IF
           READ ( UNIT = STR(82:89), FMT='(I8)', IOSTAT=IER ) SOUCAT%NOBS_TOTAL
           IF ( IER .NE. 0 ) SOUCAT%NOBS_TOTAL = 0
         ELSE IF ( MODE .EQ. 6 ) THEN
           SOUCAT%CALIB = 1
           IP = INDEX ( STR, '!' ) - 1
           IF ( IP .LE. 0 ) IP =I_LEN(STR)
           CALL EXWORD ( STR, MIND, LIND, IND, REG, -2 )
           IF ( LIND < 7 ) THEN
                CALL ERR_LOG ( 2843, IUER, 'READ_SOU_LINE', 'Too few words '// &
     &              'in line '//STR )
                RETURN
           END IF
!
           SOUCAT%IVS_NAME = STR(IND(1,1):IND(2,1))
           ALP_STR = STR(IND(1,2):IND(2,2))//'_'// &
     &               STR(IND(1,3):IND(2,3))//'_'// &
     &               STR(IND(1,4):IND(2,4))
!
           DEC_STR = STR(IND(1,5):IND(2,5))//'_'// &
     &               STR(IND(1,6):IND(2,6))//'_'// &
     &               STR(IND(1,7):IND(2,7))
!
           CALL ERR_PASS ( IUER, IER )
           CALL HR_TAT   ( ALP_STR, SOUCAT%ALP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2844, IUER, 'READ_SOU_LINE', 'Error in '// &
     &              'decoding right ascension in line '//ALP_STR )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GR_TAT   ( DEC_STR, SOUCAT%DEL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2845, IUER, 'READ_SOU_LINE', 'Error in '// &
     &              'decoding declination in line '//DEC_STR )
                RETURN
           END IF
           IF ( LIND .GE. 8 ) THEN
                READ ( UNIT=STR(IND(1,8):IND(2,8)), FMT='(F12.0)', &
     &                 IOSTAT=IER ) SOUCAT%SOU_ERR
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 2845, IUER, 'READ_SOU_LINE', 'Error in '// &
     &                   'decoding position error: '//STR(IND(1,8):IND(2,8)) )
                     RETURN
                END IF
!
                IF ( SOUCAT%SOU_ERR > 900.0 ) THEN
                     SOUCAT%CALIB = 4
                END IF
                SOUCAT%SOU_ERR = SOUCAT%SOU_ERR*MAS__TO__RAD
           END IF
         ELSE IF ( MODE .EQ. 7 ) THEN
           SOUCAT%J2000_NAME = STR(1:10)
           SOUCAT%IVS_NAME   = STR(81:88)
!
           CALL ERR_PASS ( IUER, IER )
           CALL HR_TAT ( STR(14:25), SOUCAT%ALP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2846, IUER, 'READ_SOU_LINE', 'Error in '// &
     &              'decoding right ascension in line '//STR(14:25) )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GR_TAT ( STR(26:37), SOUCAT%DEL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2847, IUER, 'READ_SOU_LINE', 'Error in '// &
     &              'decoding declination in line '//STR(26:37) )
                RETURN
           END IF
           READ ( UNIT=STR(39:48), FMT='(F10.1)', IOSTAT=IER ) SOUCAT%FLUX_TOT_S
           IF ( IER .NE. 0 ) SOUCAT%FLUX_TOT_S = -1
           SOUCAT%CALIB = 2
      END IF
!
      IF ( MODE == 1  .OR. &
     &     MODE == 2  .OR. &
     &     MODE == 3  .OR. &
     &     MODE == 4       ) THEN
           CALL ERROR_ELLIPSE ( SOUCAT%ALP_ERR*DCOS(DABS(SOUCAT%DEL)), &
     &                          SOUCAT%DEL_ERR, SOUCAT%CORR, SOUCAT%SOU_ERR, &
     &                          ERR2, TETA )
      END IF
!
      SOUCAT%S_VEC(1) = DCOS(SOUCAT%DEL)*DCOS(SOUCAT%ALP)
      SOUCAT%S_VEC(2) = DCOS(SOUCAT%DEL)*DSIN(SOUCAT%ALP)
      SOUCAT%S_VEC(3) = DSIN(SOUCAT%DEL)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_SOU_LINE
