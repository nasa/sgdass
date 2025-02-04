      SUBROUTINE SPD_3D_CONF ( CONF_FILE, SPD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_CONF  parses configuration file of the SPD_3D      *
! *   program supplied in the input variable CONF_FILE and ppopulates    *
! *   the fields of the object SPD in accordance with values from        *
! *   the input file.                                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * CONF_FILE ( CHARACTER ) -- Name of the configuration file.           *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *       SPD ( SPD_3D__TYPE ) -- Object that holds data associated      *
! *                               with computing path delay through      *
! *                               the atmosphere.                        *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 26-NOV-2008  SPD_3D_CONF  v3.3 (c)  L. Petrov  12-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'ners_local.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      CHARACTER  CONF_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 512 )
      PARAMETER  ( MIND =   8 )
      CHARACTER  BUF(MBUF)*80, BUFN(MBUF)*80
      CHARACTER  STR*128, STR1*128, REG*3
      PARAMETER  ( REG = CHAR(32)//CHAR(0)//CHAR(9) )
      LOGICAL*1  LEX
      INTEGER*4  J1, J2, J3, LN, IND(2,MIND), LIND, NBUF, N_CNF, L_FRQ, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Read the configuration file
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL RD_TEXT ( CONF_FILE, MBUF, BUF, NBUF, IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5411, IUER, 'SPD_3D_CONF', 'Error in an attempt '// &
     &         'to read configuration file '//CONF_FILE )
           RETURN 
      END IF
!
! --- Check the configuration file label
!
      IF ( BUF(1)(1:LEN(SPD_3D_CONF__LABEL)) == SPD_3D_CONF__LABEL ) THEN
           CONTINUE 
         ELSE 
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 5412, IUER, 'SPD_3D_CONF', 'Wrong format label '// &
     &         'at the first line of the configuration file '// &
     &          CONF_FILE(1:I_LEN(CONF_FILE))//' -- '//STR(1:I_LEN(STR))// &
     &         ' while '//SPD_3D_CONF__LABEL//' expected' )
           RETURN 
      END IF
      SPD%CONF%BSPL_3WAV = 'NO      '
!
! --- Cycle over lines of the file
!
      N_CNF = 0
      SPD%CONF%N_FRQ = -1
      SPD%CONF%N_LAY =  0
      L_FRQ = 0
      DO 410 J1=2,NBUF
!
! ------ Split the line into words
!
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, -2 )
         CALL CLRCH  ( STR )
         CALL INCH   ( J1, STR )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'REFR_EXPR:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__RADIO_DAVIS_1985 ) THEN
                   SPD%CONF%REFR_EXPR = SPD__RADIO_DAVIS_1985
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__RADIO_RUEGER_2002 ) THEN
                   SPD%CONF%REFR_EXPR = SPD__RADIO_RUEGER_2002
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__RADIO_APARICIO_2011 ) THEN
                   SPD%CONF%REFR_EXPR = SPD__RADIO_APARICIO_2011 
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__W532_CIDDOR_1996 ) THEN
                   SPD%CONF%REFR_EXPR = SPD__W532_CIDDOR_1996
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__W1064_CIDDOR_1996 ) THEN
                   SPD%CONF%REFR_EXPR = SPD__W1064_CIDDOR_1996
                 ELSE 
                   CALL ERR_LOG ( 5413, IUER, 'SPD_3D_CONF', 'Failure in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of control '// &
     &                 'file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' '//BUF(J1)(1:I_LEN(BUF(J1)))//' -- unsupported '// &
     &                 'refractivity expression ' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SPD_ALG:' ) THEN
              IF ( INDEX ( SPD__ALG_LIN_Y, BUF(J1)(IND(1,2):IND(2,2)) ) > 0 ) THEN
                   SPD%CONF%SPD_ALG = SPD__ALG_LIN_Y
                 ELSE IF ( INDEX ( SPD__ALG_LIN_YZ, BUF(J1)(IND(1,2):IND(2,2)) ) > 0 ) THEN
                   SPD%CONF%SPD_ALG = SPD__ALG_LIN_YZ
                 ELSE IF ( INDEX ( SPD__ALG_NONLIN_LOC, BUF(J1)(IND(1,2):IND(2,2)) ) > 0 ) THEN
                   SPD%CONF%SPD_ALG = SPD__ALG_NONLIN_LOC
                 ELSE IF ( INDEX ( SPD__ALG_NONLOC, BUF(J1)(IND(1,2):IND(2,2)) ) > 0 ) THEN
                   SPD%CONF%SPD_ALG = SPD__ALG_NONLOC
                 ELSE IF ( INDEX ( SPD__ALG_Y_NONLOC, BUF(J1)(IND(1,2):IND(2,2)) ) > 0 ) THEN
                   SPD%CONF%SPD_ALG = SPD__ALG_Y_NONLOC
                 ELSE IF ( INDEX ( SPD__ALG_STRAIGHT, BUF(J1)(IND(1,2):IND(2,2)) ) > 0 ) THEN
                   SPD%CONF%SPD_ALG = SPD__ALG_STRAIGHT
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'thermdef' ) THEN
                   SPD%CONF%SPD_ALG = 'thermdef'
                 ELSE 
                   CALL ERR_LOG ( 5414, IUER, 'SPD_3D_CONF', 'Failure in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of control '// &
     &                 'file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' '//BUF(J1)(1:I_LEN(BUF(J1)))//' -- unsupported '// &
     &                 'path delay algorithm' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SOB_ALG:' ) THEN
              IF ( INDEX ( SOB__ALG_NONE, BUF(J1)(IND(1,2):IND(2,2)) ) > 0 ) THEN
                   SPD%CONF%SOB_ALG = SOB__ALG_NONE
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SOB__ALG_RTE_STRA(1:ILEN(SOB__ALG_RTE_STRA)) ) THEN
                   SPD%CONF%SOB_ALG = SOB__ALG_RTE_STRA
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SOB__ALG_RTE_STRA_1AZ(1:ILEN(SOB__ALG_RTE_STRA_1AZ)) ) THEN
                   SPD%CONF%SOB_ALG = SOB__ALG_RTE_STRA_1AZ
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SOB__ALG_RTE_BENT(1:ILEN(SOB__ALG_RTE_BENT)) ) THEN
                   SPD%CONF%SOB_ALG = SOB__ALG_RTE_BENT
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SOB__ALG_RTE_BENT_1AZ(1:ILEN(SOB__ALG_RTE_BENT_1AZ)) ) THEN
                   SPD%CONF%SOB_ALG = SOB__ALG_RTE_BENT_1AZ
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SOB__ALG_ZPD(1:ILEN(SOB__ALG_ZPD)) ) THEN
                   SPD%CONF%SOB_ALG = SOB__ALG_ZPD
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == SOB__ALG_MZPD(1:ILEN(SOB__ALG_MZPD)) ) THEN
                   SPD%CONF%SOB_ALG = SOB__ALG_MZPD
                 ELSE 
                   CALL ERR_LOG ( 5415, IUER, 'SPD_3D_CONF', 'Failure in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of control '// &
     &                 'file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' >>'//BUF(J1)(IND(1,2):IND(2,2))//'<< -- unsupported '// &
     &                 'attenuation algorithm' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'COMPRESS:' ) THEN
              SPD%CONF%COMPR = BUF(J1)(IND(1,2):IND(2,2)) 
              IF ( SPD%CONF%COMPR == 'no'   ) SPD%CONF%COMPR = SPD__NO
              IF ( SPD%CONF%COMPR == 'none' ) SPD%CONF%COMPR = SPD__NO
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FIL_STA:' ) THEN
              SPD%CONF%FIL_STA = BUF(J1)(IND(1,2):IND(2,2)) 
              CALL SPD_CHECK_SHARE_FILE ( SPD%CONF%FIL_STA, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5416, IUER, 'SPD_3D_CONF', 'Station'// &
     &                 ' file '//SPD%CONF%FIL_STA(1:I_LEN(SPD%CONF%FIL_STA))// &
     &                 ' specified at line '//STR(1:I_LEN(STR))// &
     &                 ' of control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' is not found' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FIL_GEOID:' ) THEN
              SPD%CONF%FIL_GEOID = BUF(J1)(IND(1,2):IND(2,2)) 
              CALL ERR_PASS ( IUER, IER )
              CALL SPD_CHECK_SHARE_FILE ( SPD%CONF%FIL_GEOID, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5417, IUER, 'SPD_3D_CONF', 'Geoid '// &
     &                 ' file '//SPD%CONF%FIL_GEOID(1:I_LEN(SPD%CONF%FIL_GEOID))// &
     &                 ' specified at line '//STR(1:I_LEN(STR))// &
     &                 ' of control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' is not found' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FIL_OH:' ) THEN
              SPD%CONF%FIL_OH = BUF(J1)(IND(1,2):IND(2,2)) 
              CALL ERR_PASS ( IUER, IER )
              CALL SPD_CHECK_SHARE_FILE ( SPD%CONF%FIL_OH, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5418, IUER, 'SPD_3D_CONF', 'File '// &
     &                 ' with heights above the geoid on a lat-lon grid '// &
     &                   SPD%CONF%FIL_OH(1:I_LEN(SPD%CONF%FIL_OH))// &
     &                 ' specified at line '//STR(1:I_LEN(STR))// &
     &                 ' of control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' is not found' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FIL_LEAPSEC:' ) THEN
              SPD%CONF%FIL_LEAPSEC = BUF(J1)(IND(1,2):IND(2,2)) 
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == SPD__NERS ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL RD_TEXT ( NERS__CONFIG, MBUF, BUFN, LN, IER )
                   IF ( IER .NE. 0 ) THEN 
                        CALL ERR_LOG ( 5419, IUER, 'SPD_3D_CONF', 'Error in reading '// &
     &                      'NERS configuration file '//NERS__CONFIG )
                        RETURN 
                   END IF
                   DO 420 J2=1,LN
                      IF ( INDEX ( BUFN(J2), 'LEAPSEC_FILE:' ) > 0 ) THEN
                           SPD%CONF%FIL_LEAPSEC = BUFN(J2)(ILEN('LEAPSEC_FILE:')+1:)
                           CALL CHASHL ( SPD%CONF%FIL_LEAPSEC ) 
                      END IF
 420               CONTINUE 
              END IF
              CALL ERR_PASS ( IUER, IER )
              CALL SPD_CHECK_SHARE_FILE ( SPD%CONF%FIL_LEAPSEC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5420, IUER, 'SPD_3D_CONF', 'Leap second'// &
     &                 ' file '//SPD%CONF%FIL_LEAPSEC(1:I_LEN(SPD%CONF%FIL_LEAPSEC))// &
     &                 ' specified at line '//STR(1:I_LEN(STR))// &
     &                 ' of control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' is not found' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FIL_DESC:' ) THEN
              SPD%CONF%FIL_DESC = BUF(J1)(IND(1,2):IND(2,2)) 
              CALL ERR_PASS ( IUER, IER )
              CALL SPD_CHECK_SHARE_FILE ( SPD%CONF%FIL_DESC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5421, IUER, 'SPD_3D_CONF', 'File with '// &
     &                 ' model descripton '//SPD%CONF%FIL_DESC(1:I_LEN(SPD%CONF%FIL_DESC))// &
     &                 ' specified at line '//STR(1:I_LEN(STR))// &
     &                 ' of control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' is not found' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FIL_FMT:' ) THEN
              SPD%CONF%FIL_FMT = BUF(J1)(IND(1,2):IND(2,2)) 
              CALL ERR_PASS ( IUER, IER )
              CALL SPD_CHECK_SHARE_FILE ( SPD%CONF%FIL_FMT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5422, IUER, 'SPD_3D_CONF', 'File with '// &
     &                 ' format descripton '//SPD%CONF%FIL_FMT(1:I_LEN(SPD%CONF%FIL_FMT))// &
     &                 ' specified at line '//STR(1:I_LEN(STR))// &
     &                 ' of control file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' is not found' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TIM_INTRV:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)', IOSTAT=IER ) &
     &               SPD%CONF%TIM_INTRV
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5423, IUER, 'SPD_3D_CONF', 'Failure to'// &
     &                 ' decode line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' wrong format of time interval' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'N_EL:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I12)', IOSTAT=IER ) &
     &               SPD%CONF%N_EL
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5424, IUER, 'SPD_3D_CONF', 'Failure to'// &
     &                 ' decode line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' wrong format of the number of knots for elevations' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'N_AZ:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I12)', IOSTAT=IER ) &
     &               SPD%CONF%N_AZ
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5425, IUER, 'SPD_3D_CONF', 'Failure to'// &
     &                 ' decode line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' wrong format of the number of knots for azimuths' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'N_FRQ:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I12)', IOSTAT=IER ) &
     &               SPD%CONF%N_FRQ
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5426, IUER, 'SPD_3D_CONF', 'Failure to'// &
     &                 ' decode line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' wrong format of the number of frequencies' )
                   RETURN 
              END IF
              IF ( SPD%CONF%N_FRQ < 0 .OR. SPD%CONF%N_FRQ > SPD__M_FRQ ) THEN
                   CALL ERR_LOG ( 5427, IUER, 'SPD_3D_CONF', 'Failure to'// &
     &                 ' decode line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' wrong the number of frequences: should be in range '// &
     &                 ' [0, SPD__M_FRQ]' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FREQ:' .OR. &
     &                BUF(J1)(IND(1,1):IND(2,1)) == 'FRQ:'       ) THEN
              L_FRQ = L_FRQ + 1
              IF ( SPD%CONF%N_FRQ == -1 ) THEN
                   CALL ERR_LOG ( 5428, IUER, 'SPD_3D_CONF', 'Error in '// &
     &                 ' parsing line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' the number of frequencies (keyword N_FRQ:) was '// &
     &                 ' not defined before the keyword FREQ:' )
                   RETURN 
                ELSE IF ( SPD%CONF%N_FRQ == 0 ) THEN
                   CALL ERR_LOG ( 5429, IUER, 'SPD_3D_CONF', 'Error in '// &
     &                 ' parsing line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' the number of frequencies (keyword N_FRQ: 0). '// &
     &                 'Therefore, keyword FREQ: cannot be defined' )
                   RETURN 
                ELSE IF ( L_FRQ > SPD%CONF%N_FRQ ) THEN
                   CALL ERR_LOG ( 5430, IUER, 'SPD_3D_CONF', 'Error in '// &
     &                 ' parsing line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- this keyword is used more than N_FRQ times' )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F15.6)', IOSTAT=IER ) SPD%CONF%FRQ_ARR(L_FRQ)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5431, IUER, 'SPD_3D_CONF', 'Error in '// &
     &                 ' decoding line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- a real number was expected' )
                   RETURN 
              END IF 
              IF ( SPD%CONF%FRQ_ARR(L_FRQ) < SPD__FRQ_MIN .OR. &
     &             SPD%CONF%FRQ_ARR(L_FRQ) > SPD__FRQ_MAX      ) THEN
                   WRITE ( UNIT=STR1(1:28), FMT='("[ ", 1PD11.4, ", ", 1PD11.4, " ]")' ) &
     &                     SPD__FRQ_MIN, SPD__FRQ_MAX
                   CALL ERR_LOG ( 5432, IUER, 'SPD_3D_CONF', 'Error in '// &
     &                 ' decoding line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- the freequency should be in range '// &
     &                 STR1(1:I_LEN(STR1))//' Hz' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'N_LAYERS:' ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2):IND(2,2)), SPD%CONF%N_LAY )
              IF ( SPD%CONF%N_LAY .LE. 0 .OR. SPD%CONF%N_LAY > SPD__M_LAY ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( SPD__M_LAY, STR1 ) 
                   CALL ERR_LOG ( 5432, IUER, 'SPD_3D_CONF', 'Error in '// &
     &                 ' decoding line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' -- the number of layers should be in a range of '// &
     &                 ' [1, '//TRIM(STR1)//']' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'LAYER_BOTTOM:' ) THEN
              READ ( UNIT= BUF(J1)(IND(1,2):IND(2,2)), FMT='(F12.5)', IOSTAT=IER ) SPD%CONF%LAY_BOT
              IF ( IER .NE. 0 .OR. SPD%CONF%LAY_BOT < -500.0 ) THEN
                   CALL ERR_LOG ( 5432, IUER, 'SPD_3D_CONF', 'Error in '// &
     &                 ' decoding line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' a real number > -500.0 was expected' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'LAYER_STEP:' ) THEN
              READ ( UNIT= BUF(J1)(IND(1,2):IND(2,2)), FMT='(F12.5)', IOSTAT=IER ) SPD%CONF%LAY_STEP
              IF ( IER .NE. 0 .OR. SPD%CONF%LAY_STEP < 1.0 ) THEN
                   CALL ERR_LOG ( 5432, IUER, 'SPD_3D_CONF', 'Error in '// &
     &                 ' decoding line '//STR(1:I_LEN(STR))//' of the'// &
     &                 ' configuration file '//CONF_FILE(1:I_LEN(CONF_FILE))// &
     &                 ' a real number greater than 1.0 was expected' )
                   RETURN 
              END IF
              N_CNF = N_CNF + 1
            ELSE 
              CALL ERR_LOG ( 5433, IUER, 'SPD_3D_CONF', 'Unknown keyword '// &
     &             BUF(J1)(IND(1,1):IND(2,1))//' -- failure to parse '// &
     &            'the '//STR(1:I_LEN(STR))//'th line of the configuration '// &
     &             CONF_FILE )
              RETURN 
         END IF
 410  CONTINUE 
!
! --- Check whether all keywords were present in the configuration file
!
      IF ( SPD%CONF%SOB_ALG == SOB__ALG_MZPD  ) THEN
           IF ( N_CNF .NE. M__SPD_CONF + L_FRQ + 2 ) THEN
                CALL CLRCH ( STR  )
                CALL CLRCH ( STR1 )
                CALL INCH  ( N_CNF, STR )
                CALL INCH  ( M__SPD_CONF + 3, STR1 )
                CALL ERR_LOG ( 5434, IUER, 'SPD_3D_CONF', 'Only '// &
     &               STR(1:I_LEN(STR))//' keywords out of '//STR1(1:I_LEN(STR1))// &
     &              ' were found in the configuration file '//CONF_FILE )
                RETURN 
           END IF
           DO 430 J3=1,SPD%CONF%N_LAY
              SPD%CONF%LAY_ARR(J3) = SPD%CONF%LAY_BOT + (J3-1)*SPD%CONF%LAY_STEP
 430       CONTINUE
        ELSE
           IF ( N_CNF .NE. M__SPD_CONF + L_FRQ - 1 ) THEN
                CALL CLRCH ( STR  )
                CALL CLRCH ( STR1 )
                CALL INCH  ( N_CNF, STR )
                CALL INCH  ( M__SPD_CONF + L_FRQ - 1, STR1 )
                CALL ERR_LOG ( 5434, IUER, 'SPD_3D_CONF', 'Only '// &
     &               STR(1:I_LEN(STR))//' keywords out of '//STR1(1:I_LEN(STR1))// &
     &              ' were found in the configuration file '//CONF_FILE )
                RETURN 
           END IF
      END IF
      IF ( SPD%CONF%SPD_ALG == SPD__ALG_LIN_Y      .OR. &
     &     SPD%CONF%SPD_ALG == SPD__ALG_LIN_YZ     .OR. &
     &     SPD%CONF%SPD_ALG == SPD__ALG_NONLOC     .OR. &
     &     SPD%CONF%SPD_ALG == SPD__ALG_Y_NONLOC   .OR. &
     &     SPD%CONF%SPD_ALG == SPD__ALG_NONLIN_LOC      ) THEN
           CONTINUE 
         ELSE 
           IF ( SPD%CONF%SOB_ALG == SOB__ALG_RTE_BENT ) THEN
                CALL ERR_LOG ( 5435, IUER, 'SPD_3D_CONF', 'Incompatible '// &
     &              'options SPD_ALG: '//SPD%CONF%SPD_ALG//' and '// &
     &              'SPD_ALG: '//SPD%CONF%SOB_ALG//' -- and SPD_ALG '// &
     &              'alogorithm that computed bent trajectory is needed' )
                RETURN 
           END IF 
      END IF
      IF ( SPD%CONF%SOB_ALG    ==   SOB__ALG_ZPD             .AND. &
     &     SPD%CONF%REFR_EXPR .NE.  SPD__RADIO_APARICIO_2011       ) THEN
           CALL ERR_LOG ( 5436, IUER, 'SPD_3D_CONF', 'Unsupported '// &
     &         'combination of REFR_EXPR: '//SPD%CONF%REFR_EXPR//' and '// &
     &         'SPD_ALG: '//SPD%CONF%SOB_ALG//' -- spd_3d_refra should '// &
     &         'should be upgraded' )
           RETURN 
      END IF
      IF ( SPD%CONF%SOB_ALG    ==   SOB__ALG_MZPD            .AND. &
     &     SPD%CONF%REFR_EXPR .NE.  SPD__RADIO_APARICIO_2011       ) THEN
           CALL ERR_LOG ( 5437, IUER, 'SPD_3D_CONF', 'Unsupported '// &
     &         'combination of REFR_EXPR: '//SPD%CONF%REFR_EXPR//' and '// &
     &         'SPD_ALG: '//SPD%CONF%SOB_ALG//' -- spd_3d_refra should '// &
     &         'should be upgraded' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_LOAD_LEAPSEC ( SPD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5436, IUER, 'SPD_3D_CONF', 'Error in an attempt '// &
     &         'to load leap second data file' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_CONF  !#!  
