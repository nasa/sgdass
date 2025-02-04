      PROGRAM    GEN_SEQ_PRC
! ************************************************************************
! *                                                                      *
! *   Program  GEN_SEQ_PRC generates the procedure file based on the     *
! *   frequency sequence definition file and the template.               *
! *                                                                      *
! *  ###  08-OCT-2021  GEN_SEQ_PRC  v1.21 (c) L. Petrov  24-OCT-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INTEGER*4  MP, MIND, MSTA, MIF, MBND, MPRC
      PARAMETER  ( MP   = 4096 )
      PARAMETER  ( MIND =  32 )      
      PARAMETER  ( MSTA =  14 )      
      PARAMETER  ( MIF  =  16 )      
      PARAMETER  ( MBND =   8 )      
      PARAMETER  ( MPRC =   7 )      
      CHARACTER  FIL_IN*128, DIR_OUT*128, FIL_PRC_OUT*128, FIL_VEX_OUT*128
      CHARACTER  BUF(MP)*256, OUT(MP)*256, CS_STA(MSTA)*8, C_STA, STR*128
      CHARACTER  DBE_STR(MBND)*128, LO_FRQ_STR(MBND)*16, LO_STR(MBND)*128, &
     &           UDC_STR(MBND)*128, TMPL_DIR*128, LO_NAME*2, BAND_NAME*1, &
     &           TMPL_FIL(MSTA)*128, IF_FRQ_STR*6, BW_FRQ_STR*4, &
     &           SB_STR*3, PROG__LABEL*40
      PARAMETER  ( PROG__LABEL = 'gen_seq_prc  1.22  version of 2023.12.15' )
      CHARACTER  PRC_NAMES(MPRC)*7, BND*1
      REAL*8     LUFF(MSTA), BW_HS, FRQ_UDC, IF_OFFSET
      TYPE       FRQ_SEQ
                 CHARACTER  STA_NAM(MSTA)*8
                 CHARACTER  STA_ACR(MSTA)*2
                 CHARACTER  STA_WIR(MSTA)*16
                 CHARACTER  SIB_LO(MBND)*3
                 CHARACTER  BAND_NAM(MBND)*2
                 CHARACTER  HDS_NAM*16
                 CHARACTER  MOD_NAM*16
                 INTEGER*4  NBAND
                 INTEGER*4  NIF(MBND)
                 INTEGER*4  BBC_IND(MIF,MBND)
                 REAL*8     FRQ_BAND(MBND)
                 REAL*8     FRQ_LO(MBND)
                 REAL*8     FRQ_UDC(MBND)
                 REAL*8     FRQ_IF(MIF,MBND)
                 REAL*8     BW_IF(MIF,MBND)
                 REAL*8     RANGE(3,MBND)
                 CHARACTER  BAND_SUB(MIF,MBND)*1
                 CHARACTER  DAS(MSTA)*16
      END TYPE   FRQ_SEQ
      INTEGER*4  DUR(MPRC,MSTA)
      TYPE ( FRQ_SEQ ) :: SEQ
      LOGICAL*1  LEX, FL_DEFINE, FL_IMAGE_CHSEL, FL_8IF
      CHARACTER  BAND_DBBC3*1, POLAR_DBBC3(2)*1, PRLZ_STR(2)*1, SBD_STR*1
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, K_BBC, IVRB, IS, ID, NF, IND_CORE, &
     &           NUM_NIF, IP, IB, IE, IBL, IEL, I_BND, NP, NO, LIND, IND_FRQ, &
     &           IND_CHN, IND_BBC, IND_MK5_BBC, &
     &           NUM_BND, L_STA, IND(2,MIND), I_STA, IND_CH, MIN_NIF, MAX_NIF, &
     &           IL, LAST_LO, NN, IUER
      INTEGER*4  IPA, IPD, IPH, IPO, IPL, IPR, IPU
!
      DATA       PRLZ_STR / 'H', 'V' /
      DATA       TMPL_DIR / '/cont/prc' /
      DATA       BW_HS / 32.0D0 /
      DATA       CS_STA / &
     &                   'GGAO12M ', & 
     &                   'JILIN12',  & 
     &                   'KOKEE12M', & 
     &                   'KOKEE   ', & 
     &                   'MACGO12M', & 
     &                   'NYALE13N', &
     &                   'NYALE13S', &
     &                   'NYALES20', &
     &                   'ONSA13NE', &
     &                   'ONSA13SW', &
     &                   'RAEGYEB ', &
     &                   'RAEGSMAR', &
     &                   'WESTFORD', &
     &                   'WETTZ13S'  &
     &                 /
      DATA       PRC_NAMES / &
     &                       'preses ', &
     &                       'setmode', &
     &                       'setscan', &
     &                       'preob  ', &
     &                       'midob  ', &
     &                       'postob ', &
     &                       'postses'  &
     &                     /
!
!@      INTEGER*4    MK5B_BBC(16), MK5B_FRQ(16)
!@      DATA       ( MK5B_BBC(NN), MK5B_SUB(NN), NN=1,16 ) / &
!@     &                       1,  'L', & ! 01
!@     &                       1,  'U', & ! 02
!@     &                       2,  'U', & ! 03
!@     &                       3,  'U', & ! 04
!@     &                       4,  'U', & ! 05
!@     &                       9,  'U', & ! 06
!@     &                      10,  'U', & ! 07
!@     &                      11,  'U', & ! 08
!@     &                      12,  'U', & ! 09
!@     &                      13,  'U', & ! 10
!@     &                      14,  'U', & ! 11
!@     &                       5,  'U', & ! 12
!@     &                       6,  'U', & ! 13
!@     &                       7,  'U', & ! 14
!@     &                       8,  'L', & ! 15
!@     &                       8,  'U'  & ! 16
!@     &             /
!                
      INTEGER*4    MK5B_ORIG_BBC(16),  MK5B_VDIF_BBC(16), &
     &             MK5B_ORIG_CHAN(16), MK5B_VDIF_CHAN(16)
      CHARACTER    MK5B_ORIG_SUB(16)*1, MK5B_VDIF_SUB(16)*1
!
      DATA       ( MK5B_ORIG_BBC(NN), MK5B_ORIG_SUB(NN), MK5B_ORIG_CHAN(NN), NN=1,16 ) / &
     &                        9,  'U',   11,  & ! 01
     &                       10,  'U',   12,  & ! 02
     &                       11,  'U',   13,  & ! 03
     &                       12,  'U',   14,  & ! 04
     &                       13,  'U',   15,  & ! 05
     &                       14,  'U',   16,  & ! 06
!
     &                        1,  'L',   01,  & ! 07
     &                        1,  'U',   02,  & ! 08
     &                        2,  'U',   03,  & ! 09
     &                        3,  'U',   04,  & ! 10
     &                        4,  'U',   05,  & ! 11
     &                        5,  'U',   06,  & ! 12
     &                        6,  'U',   07,  & ! 13
     &                        7,  'U',   08,  & ! 14
     &                        8,  'L',   09,  & ! 15
     &                        8,  'U',   10   & ! 16
     &           /
!
      DATA       ( MK5B_VDIF_BBC(NN), MK5B_VDIF_SUB(NN), MK5B_VDIF_CHAN(NN), NN=1,16 ) / &
     &                        9,  'U',  11,  & ! 01
     &                       10,  'U',  12,  & ! 02
     &                       11,  'U',  13,  & ! 03
     &                       12,  'U',  14,  & ! 04
     &                       13,  'U',  15,  & ! 05
     &                       14,  'U',  16,  & ! 06
!
     &                        1,  'L',  09,  & ! 07
     &                        1,  'U',  01,  & ! 08
     &                        2,  'U',  02,  & ! 09
     &                        3,  'U',  03,  & ! 10
     &                        4,  'U',  04,  & ! 11
     &                        5,  'U',  05,  & ! 12
     &                        6,  'U',  06,  & ! 13
     &                        7,  'U',  07,  & ! 14
     &                        8,  'L',  10,  & ! 15
     &                        8,  'U',  08   & ! 16
     &           /
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LINDEX, LTM_DIF
      CHARACTER, EXTERNAL :: GET_CDATE*19, GET_UTC_CDATE*19, GET_TZ_CDATE*26
!
      DUR = 0
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gen_seq_prc fil_seq dir_out [verb]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_IN  )
           CALL GETARG ( 2, DIR_OUT )
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, STR )
                CALL CHIN   (  STR, IVRB )
              ELSE 
                IVRB = 0
           END IF
      END IF
!
! --- Read the input sequence file
!
      IUER = -1
      CALL RD_TEXT ( FIL_IN, MP, BUF, NP, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER  = -1
           CALL ERR_LOG ( 6701, IUER, 'GEN_SEQ_PRC', 'Cannot find '// &
     &         'input file with frequency sequence '//FIL_IN )
           CALL EXIT ( 1 )
      END IF
!
! --- Check whether the first line is magic
!
      IF ( BUF(1)(1:LEN(FRQ_SEQ__LABEL)) == FRQ_SEQ__LABEL ) THEN
           CONTINUE 
         ELSE
           CALL CLRCH ( STR ) 
           STR = BUF(1)
           CALL TRAN ( 13, STR, STR )
           IUER = -1
           CALL ERR_LOG ( 6702, IUER, 'GEN_SEQ_PRC', 'The first line '// &
     &         'of the input file with frequency sequence '//TRIM(FIL_IN)// &
     &         ' has line '//TRIM(STR)//' while magic '//FRQ_SEQ__LABEL// &
     &         ' was exected' )
           CALL EXIT ( 1 )
      END IF
!
! --- Parse the input sequence file
!
      SEQ%NBAND = 0
      L_STA = 0
      DO 410 J1=2,NP
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'STATION' ) THEN
!
! ----------- Station record
!
              L_STA = L_STA + 1
              SEQ%STA_NAM(L_STA) = BUF(J1)(IND(1,2):IND(2,2))
              CALL TRAN ( 11, SEQ%STA_NAM(L_STA), SEQ%STA_NAM(L_STA) )
              I_STA = LTM_DIF ( 0, MSTA, CS_STA, SEQ%STA_NAM(L_STA) )
              IF ( I_STA .LE. 0 ) THEN
                   IUER  = -1
                   CALL ERR_LOG ( 6703, IUER, 'GEN_SEQ_PRC', 'Station '//SEQ%STA_NAM(L_STA)// &
     &                 ' is not supported by gen_seq_prc' )
                   CALL EXIT ( 1 )
              END IF
              SEQ%STA_ACR(L_STA) = BUF(J1)(IND(1,3):IND(2,3))
              CALL TRAN ( 12, SEQ%STA_ACR(L_STA), SEQ%STA_ACR(L_STA) )
              IF ( SEQ%STA_NAM(L_STA) == 'GGAO12M ' ) THEN
                   LUFF(L_STA) = 22500.0D0
                 ELSE
                   LUFF(L_STA) = 21500.0D0
              END IF
              SEQ%STA_WIR(L_STA) = BUF(J1)(IND(1,4):IND(2,4))
              SEQ%DAS(L_STA)     = BUF(J1)(IND(1,5):IND(2,5))
!
              IF ( SEQ%STA_NAM(L_STA) == 'WESTFORD' ) THEN
                   FL_IMAGE_CHSEL = .TRUE.
                 ELSE
                   FL_IMAGE_CHSEL = .FALSE.
              END IF
!
! ----------- Form the template file name
!
              TMPL_FIL(L_STA) = TRIM(TMPL_DIR)//'/'//SEQ%STA_ACR(L_STA)//'_template.prc'
!
! ----------- ... and check whether the template file exists
!
              INQUIRE ( FILE=TMPL_FIL(L_STA), EXIST=LEX ) 
              IF ( .NOT. LEX ) THEN
                   IUER  = -1
                   CALL ERR_LOG ( 6704, IUER, 'GEN_SEQ_PRC', 'Cannot find'// &
     &                 ' template file '//TRIM(TMPL_FIL(L_STA))// &
     &                 ' for station '//SEQ%STA_NAM(L_STA) )
                   CALL EXIT ( 1 )
              END IF
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'HDS_NAME' ) THEN
!
! ----------- Get the hardware set up name 
!
              SEQ%HDS_NAM = BUF(J1)(IND(1,2):IND(2,2))
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MOD_NAME' ) THEN
!
! ----------- Get the observein mode name 
!
              SEQ%MOD_NAM = BUF(J1)(IND(1,2):IND(2,2))
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BAND' ) THEN
!
! ----------- Get the IF definition
!
! ----------- First parse the band name
!
              IF ( SEQ%NBAND == 0 ) THEN
                   SEQ%NBAND = SEQ%NBAND + 1
                   SEQ%NIF(SEQ%NBAND) = 0
                   SEQ%BAND_NAM(SEQ%NBAND) = BUF(J1)(IND(1,2):IND(2,2))
                   CALL TRAN ( 12, SEQ%BAND_NAM(SEQ%NBAND), SEQ%BAND_NAM(SEQ%NBAND) )
                 ELSE
                   STR = BUF(J1)(IND(1,2):IND(1,2)+1)
                   CALL TRAN ( 12, STR, STR )
                   IUER = -1
                   I_BND = ADD_CLIST ( MBND, SEQ%NBAND, SEQ%BAND_NAM, STR, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER  = -1
                        CALL ERR_LOG ( 6705, IUER, 'GEN_SEQ_PRC', 'Too many bands '// &
     &                      'in the input file with frequency sequence '//FIL_IN )
                        CALL EXIT ( 1 )
                   END IF
              END IF
              SEQ%NIF(SEQ%NBAND) = SEQ%NIF(SEQ%NBAND) + 1
              IF ( SEQ%NIF(SEQ%NBAND) > MIF ) THEN
                   IUER  = -1
                   CALL ERR_LOG ( 6706, IUER, 'GEN_SEQ_PRC', 'Too many IFs '// &
     &                 'in band '//SEQ%BAND_NAM(SEQ%NBAND)//' in the input '// &
     &                 'file with frequency sequence '//FIL_IN )
                   CALL EXIT ( 1 )
              END IF
!
! ----------- Then read the IF frequency
!
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F9.2)' ) SEQ%FRQ_IF(SEQ%NIF(SEQ%NBAND),SEQ%NBAND)
!
! ----------- ... and the IF bandwifth
!
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F9.2)' ) SEQ%BW_IF(SEQ%NIF(SEQ%NBAND),SEQ%NBAND)
!
              SEQ%BAND_SUB(SEQ%NIF(SEQ%NBAND),SEQ%NBAND) = BUF(J1)(IND(1,5):IND(2,5))
!!            write ( 6, * ) 'nb= ', int2(seq%nband), ' nif= ', int2(seq%nif(seq%nband)), ' frq= ', sngl(seq%frq_if(seq%nif(seq%nband),seq%nband))
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'RANGE' ) THEN
              STR = BUF(J1)(IND(1,2):IND(1,2)+1)
              CALL TRAN ( 12, STR, STR )
              I_BND = ADD_CLIST ( MBND, SEQ%NBAND, SEQ%BAND_NAM, STR, IUER )
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F9.2)' ) SEQ%RANGE(1,I_BND)
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F9.2)' ) SEQ%RANGE(2,I_BND)
              IF ( LIND .GE. 5 ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F9.2)' ) SEQ%RANGE(3,I_BND)
                 ELSE 
                   SEQ%RANGE(3,I_BND) = SEQ%RANGE(2,I_BND)
              END IF
         END IF
 410  CONTINUE 
!
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, 210 ) SEQ%NBAND
 210       FORMAT ( 'The number of bands: ', I2 ) 
           DO 420 J2=1,SEQ%NBAND
              WRITE ( 6, 220 ) J2, SEQ%BAND_NAM(J2), SEQ%NIF(J2)
 220          FORMAT ( I2, ' band ', A, ' Number of IFs: ', I3 )
 420       CONTINUE 
      END IF
!
! --- Generate the output file based on the template
!
      DO 430 J3=1,L_STA
!
! ------ Generate strings for the output procedure file
!
         MAX_NIF =   0
         MIN_NIF = 128
         SEQ%SIB_LO = '?'
         NUM_NIF = 0
         NUM_BND = 0
         DO 440 J4=1,SEQ%NBAND
            MIN_NIF = MIN ( MIN_NIF, SEQ%NIF(J4) )
            MAX_NIF = MAX ( MAX_NIF, SEQ%NIF(J4) )
!
            IF ( SEQ%STA_WIR(J3) == "gs_udc" .OR. &
     &           SEQ%STA_WIR(J3) == "k2_udc" .OR. &
     &           SEQ%STA_WIR(J3) == "mg_udc" .OR. &
     &           SEQ%STA_WIR(J3) == "wf_udc" .OR. &
     &           SEQ%STA_WIR(J3) == "ws_01"  .OR. &
     &           SEQ%STA_WIR(J3) == "yj_udc"      ) THEN
!
! -------------- Set the band LO frequency
!
                 IF ( SEQ%NIF(J4) < 16 ) THEN
                      SEQ%FRQ_BAND(J4) = SEQ%FRQ_IF(1,J4) - SEQ%BW_IF(SEQ%NIF(SEQ%NBAND),SEQ%NBAND) + 512.0
                    ELSE
                      SEQ%FRQ_BAND(J4) = SEQ%FRQ_IF(1,J4) + 512.0
                 END IF
                 SEQ%FRQ_LO(J4) = SEQ%FRQ_BAND(J4) - 1024 + SEQ%BW_IF(SEQ%NIF(SEQ%NBAND),SEQ%NBAND)/2.0
                 SEQ%SIB_LO(J4) = 'usb'
               ELSE IF ( SEQ%STA_WIR(J3) == "kk_mark5b" ) THEN
                 SEQ%FRQ_BAND(J4) = SEQ%RANGE(1,J4)
                 SEQ%FRQ_LO(J4)   = SEQ%RANGE(1,J4) 
                 SEQ%SIB_LO(J4) = 'usb'
!@                 IF ( SEQ%FRQ_IF(1,J4) > 8100.0D0 .AND. SEQ%FRQ_IF(1,J4) < 8600.0D0 ) THEN
!@                      SEQ%FRQ_LO(J4) = 7600.0D0
!@                      SEQ%SIB_LO(J4) = 'usb'
!@                    ELSE IF ( SEQ%FRQ_IF(1,J4) > 8600.0D0 ) THEN
!@                      SEQ%FRQ_LO(J4) = 8100.0D0
!@                      SEQ%SIB_LO(J4) = 'usb'
!@                    ELSE IF ( SEQ%FRQ_IF(1,J4) < 80000.0D0 ) THEN
!@                      SEQ%FRQ_LO(J4) =  600.0D0
!@                      SEQ%SIB_LO(J4) = 'usb'
!@                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "mark5b" ) THEN
                 SEQ%FRQ_LO(J4) = SEQ%RANGE(1,J4)
                 SEQ%SIB_LO(J4) = 'usb'
               ELSE IF ( SEQ%STA_WIR(J3) == "nn_01" .OR. &
     &                   SEQ%STA_WIR(J3) == "on_01"      ) THEN
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  0.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  7700.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  7700.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) = 11600.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1d1u2d3d" ) THEN
!
! ------------- "1d"  4.0 lsb,  2-4  GHz
! ------------- "2u"  4.0 usb,  4-6  GHz
! ------------- "2d"  8.0 lsb,  6-8  GHz
! ------------- "2u"  8.0 lsb,  8-10 GHz
! ------------- "3d" 12.0 lsb, 10-12  GHz
! ------------- "3u" 12.0 usb, 12-14 GHz
!
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) = 12000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1d2d2d2u" ) THEN
!
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1d1u2u3d" ) THEN
!
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) = 12000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1u1u2u2u" ) THEN
!
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1d2u3d3d" ) THEN
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) = 12000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) = 12000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1u1u2d3d" ) THEN
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) = 12000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1u1u1u1u" ) THEN
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1u2d2u3d" ) THEN
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  8000 
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) = 12000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1u2u2u3d" ) THEN
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  8000 
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) = 12000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1u1u3d3u" ) THEN
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) = 12000 
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) = 12000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1u1u3u3u" ) THEN
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) = 12000 
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) = 12000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1d2d2u3d" ) THEN
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  8000 
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) = 12000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1d2d2u3u" ) THEN
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  8000 
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) = 12000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "sa_1d1u1u2u" ) THEN
                 IF ( J4 == 1 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'lsb'
                 END IF
                 IF ( J4 == 2 ) THEN
                      SEQ%FRQ_LO(J4) =  4000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 3 ) THEN
                      SEQ%FRQ_LO(J4) =  4000 
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
                 IF ( J4 == 4 ) THEN
                      SEQ%FRQ_LO(J4) =  8000.0
                      SEQ%SIB_LO(J4) = 'usb'
                 END IF
               ELSE IF ( SEQ%STA_WIR(J3) == "ntsc_cdas" ) THEN
                 SEQ%FRQ_LO(J4) = SEQ%RANGE(1,J4)
                 SEQ%SIB_LO(J4) = 'usb'
               ELSE 
                 IUER = -1
                 CALL ERR_LOG ( 6707, IUER, 'GEN_SEQ_PRC', 'Wiring '// &
     &                          SEQ%STA_WIR(J3)//' is not supported' )
                 CALL EXIT ( 1 )
            END IF
            IF ( SEQ%FRQ_LO(J4) < 10000.0 ) THEN
                 WRITE ( UNIT=LO_STR(J4),     FMT=110      ) SEQ%BAND_NAM(J4)(1:1), &
     &                                        SEQ%FRQ_LO(J4), SEQ%SIB_LO(J4)
                 WRITE ( UNIT=LO_FRQ_STR(J4), FMT='(F6.1)' ) SEQ%FRQ_LO(J4)
               ELSE
                 WRITE ( UNIT=LO_STR(J4),     FMT=115      ) SEQ%BAND_NAM(J4)(1:1), &
     &                                                       SEQ%FRQ_LO(J4), SEQ%SIB_LO(J4)
                 WRITE ( UNIT=LO_FRQ_STR(J4), FMT='(F7.1)' ) SEQ%FRQ_LO(J4)
            END IF
 110        FORMAT ( 'lo=lo', A, '0,', F6.1, ',', A, ',lcp,', '5' )
 115        FORMAT ( 'lo=lo', A, '0,', F7.1, ',', A, ',lcp,', '5' )
!
! --------- Set the UDC frequency
!
            SEQ%FRQ_UDC(J4) = (SEQ%FRQ_LO(J4) + LUFF(J3))/4.0
            WRITE ( UNIT=UDC_STR(J4), FMT=120 ) SEQ%FRQ_UDC(J4)
 120        FORMAT ( F6.1 )
!
! --------- Set the channels sequence
!
            DBE_STR(J4) = '@'
            DO 450 J5=SEQ%NIF(J4),1,-1
                IF ( INDEX ( SEQ%STA_WIR(J3), 'mark5' ) < 1 ) THEN
                    IND_CH = -NINT( (SEQ%FRQ_IF(J5,J4)- SEQ%FRQ_BAND(J4))/SEQ%BW_IF(SEQ%NIF(SEQ%NBAND),SEQ%NBAND) )
                    IF ( SEQ%DAS(J3) .NE. 'dbbc3' .AND. &
     &                   DABS ( IND_CH*SEQ%BW_IF(SEQ%NIF(SEQ%NBAND),SEQ%NBAND) + SEQ%FRQ_IF(J5,J4)- SEQ%FRQ_BAND(J4) ) > 0.001D0 ) THEN
!
                         WRITE ( 6, * ) 'Fraction: ', SNGL((SEQ%FRQ_IF(J5,J4)- SEQ%FRQ_BAND(J4))/SEQ%BW_IF(SEQ%NIF(SEQ%NBAND),SEQ%NBAND))
                         WRITE ( 6, * ) 'J4= ', INT2(J4), ' J5= ', INT2(J5), &
          &                             ' FRQ_IF= ', SEQ%FRQ_IF(J5,J4), &
          &                             ' FRQ_BND= ', SEQ%FRQ_BAND(J4), ' BW_HS= ', SEQ%BW_IF(SEQ%NIF(SEQ%NBAND),SEQ%NBAND)
                         IUER = -1
                         CALL ERR_LOG ( 6708, IUER, 'GEN_SEQ_PRC', 'Error in '// &
          &                  'the frequency setup: a frequency that is '// &
          &                  'a fractional part of an IF is not supported' )
                         RETURN 
                     END IF
                     IF ( FL_IMAGE_CHSEL ) THEN
                          IND_CH = 32 - IND_CH
                     END IF
                     CALL INCH ( IND_CH, STR ) 
                     DBE_STR(J4) = TRIM(DBE_STR(J4))//':'//TRIM(STR)
               END IF
               NUM_NIF = NUM_NIF + 1
 450        CONTINUE 
            NUM_BND = NUM_BND + 1
 440     CONTINUE 
!
! ------ Read the template procedure file
!
         IUER = -1
         CALL RD_TEXT ( TMPL_FIL(J3), MP, BUF, NP, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER  = -1
              CALL ERR_LOG ( 6709, IUER, 'GEN_SEQ_PRC', 'Error in '// &
     &            'reading template file '//TMPL_FIL )
              CALL EXIT ( 1 )
         END IF
         WRITE ( 6, '(A)' ) PROG__LABEL//' Processing '//TRIM(TMPL_FIL(J3))
         FL_8IF = .FALSE.
         DO 460 J6=1,NP
            IF ( BUF(J6)(1:6) == 'lo=loe' ) THEN
                 FL_8IF = .TRUE.
!                 LO_STR(8) = LO_STR(4)
!                 LO_STR(7) = LO_STR(4)
!                 LO_STR(6) = LO_STR(3)
!                 LO_STR(5) = LO_STR(3)
!                 LO_STR(4) = LO_STR(2)
!                 LO_STR(3) = LO_STR(2)
!                 LO_STR(2) = LO_STR(1)
!                 LO_FRQ_STR(8) = LO_FRQ_STR(4)
!                 LO_FRQ_STR(7) = LO_FRQ_STR(4)
!                 LO_FRQ_STR(6) = LO_FRQ_STR(3)
!                 LO_FRQ_STR(5) = LO_FRQ_STR(3)
!                 LO_FRQ_STR(4) = LO_FRQ_STR(2)
!                 LO_FRQ_STR(3) = LO_FRQ_STR(2)
!                 LO_FRQ_STR(2) = LO_FRQ_STR(1)
            END IF
 460     CONTINUE 
!
         NO = 0 
!
         IND_MK5_BBC = 0
         LAST_LO     = 0
         FL_DEFINE = .FALSE.
!
! ------ Parse the procedure template file
!
         DO 470 J7=1,NP
 970        CONTINUE 
            CALL EXWORD ( BUF(J7), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
            IF ( INDEX ( BUF(J7), '@update_date@' ) > 0 ) THEN
                 IB = INDEX ( BUF(J7), '@update_date@' ) 
                 BUF(J7)(IB:) = GET_TZ_CDATE()
              ELSE IF ( INDEX ( BUF(J7), '@vers@' ) > 0 ) THEN
                 NO = NO + 1; 
                 OUT(NO) = '" Procedure file for station '//SEQ%STA_NAM(J3)// &
     &                     ' '//SEQ%STA_ACR(J3)//' generated by '//PROG__LABEL
                 NO = NO + 1; 
                 OUT(NO) = '" Used experiment setup file: '//TRIM(FIL_IN)
                 NO = NO + 1; 
                 OUT(NO) = '" Used station template file: '//TRIM(TMPL_FIL(J3))
                 NO = NO + 1; 
                 OUT(NO) = '"'
                 GOTO 470
            ENDIF
            IF ( INDEX ( BUF(J7), '@time_stamp@' ) > 0 ) THEN
                 NO = NO + 1
                 OUT(NO) = '" Hardware setup  '//TRIM(SEQ%HDS_NAM)
                 NO = NO + 1
                 OUT(NO) = '" Frequency setup '//TRIM(SEQ%MOD_NAM)
                 NO = NO + 1
                 OUT(NO) = '" '
                 NF = 0
                 DO 480 J8=1,SEQ%NBAND
                    DO 490 J9=1,SEQ%NIF(J8)
                       NF = NF + 1
                       NO = NO + 1
                       WRITE ( OUT(NO), FMT=230 ) NF, J8, SEQ%BAND_NAM(J8), J9, &
     &                                            SEQ%FRQ_IF(J9,J8), SEQ%BW_IF(J9,J8), &
     &                                            SEQ%FRQ_LO(J8), SEQ%FRQ_UDC(J8), SEQ%SIB_LO(J8)
 230                   FORMAT ( '" IF: ', I3, ' Band: ', I1, ' Band_name: ', A, ' IF_ind: ', I2, &
     &                          ' Sky freq: ', F8.1, ' Bandwidth: ', F5.1, &
     &                          ' LO freq: ', F8.1, ' UDC freq: ', F8.1, ' MHz  Side: ', A )
 490                CONTINUE 
                    NO = NO + 1
                    OUT(NO) = '" '
 480             CONTINUE 
                 GOTO 470
            END IF
!
! --------- The procedure file may have less than four bands.
! --------- Drop recordings the procedure file for the bands that are not defined
!
            IPA = INDEX ( BUF(J7), 'add : rdbe'  )
            IPD = INDEX ( BUF(J7), '=dbe_chsel=' )
            IPH = INDEX ( BUF(J7), '-h udc'      )
            IPO = INDEX ( BUF(J7), 'lo=lo'       )
            IPR = INDEX ( BUF(J7), 'rfd_atten='  )
            IPU = INDEX ( BUF(J7), 'udceth0 udc' )
!
            IF ( IPA > 0 ) THEN
                 STR = BUF(J7)(IPA+10:IPA+10)
                 CALL TRAN ( 12, STR(1:1), STR(1:1) )
                 I_BND = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, STR(1:1) )
                 IF ( I_BND < 1 ) GOTO 470
            END IF
            IF ( IPD > 0 ) THEN
                 I_BND = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, BUF(J7)(5:5) )
!!   write ( 6, * ) 'GSP j7= ', int2(j7), ' i_bnd= ', int2(i_bnd) ! %%%
                 IF ( I_BND < 1 ) GOTO 470
            END IF
            IF ( IPH > 0 ) THEN
                 STR = BUF(J7)(IPH+6:IPH+6)
                 CALL TRAN ( 12, STR(1:1), STR(1:1) )
                 I_BND = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, STR )
                 IF ( I_BND < 1 ) GOTO 470
            END IF
            IF ( IPO > 0 ) THEN
                 IF ( BUF(J7)(1:1) .NE. '"' ) THEN
                      BAND_NAME = BUF(J7)(6:6) 
                   ELSE
                      BAND_NAME = BUF(J7)(16:16) 
                 END IF
                 IF ( FL_8IF ) THEN
                      IP = INDEX ( 'abcdefgh', BAND_NAME )
                      STR = 'aabbccdd'
                      BAND_NAME = STR(IP:IP)
                 END IF
                 I_BND = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, BAND_NAME )
                 IF ( I_BND < 1 ) GOTO 470
            END IF
            IF ( IPR > 0 ) THEN
!
! -------------- Check, whether we need remove attenuation for the lower band
!
                 IF ( INDEX ( BUF(J7), 'rfd_atten=0' ) > 0 .AND. &
     &                LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, 'a ' ) < 1 ) THEN
                      GOTO 470
                 END IF
!
                 IF ( INDEX ( BUF(J7), 'rfd_atten=1' ) > 0 .AND. &
     &                LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, 'a ' ) < 1 ) THEN
                      GOTO 470
                 END IF
!
! -------------- Check, whether we need remove attenuation for the upper band
!
                 IF ( INDEX ( BUF(J7), 'rfd_atten=0' ) > 0 .AND. &
     &                LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, 'b ' ) < 1 .AND. &
     &                LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, 'c ' ) < 1 .AND. &
     &                LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, 'd ' ) < 1       ) THEN
                      GOTO 470
                 END IF
!
                 IF ( INDEX ( BUF(J7), 'rfd_atten=1' ) > 0 .AND. &
     &                LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, 'b ' ) < 1 .AND. &
     &                LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, 'c ' ) < 1 .AND. &
     &                LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, 'd ' ) < 1       ) THEN
                      GOTO 470
                 END IF
            END IF
            IF ( IPU > 0 ) THEN
                 STR = BUF(J7)(IPU+11:IPU+11)
                 CALL TRAN ( 12, STR(1:1), STR(1:1) )
                 I_BND = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, STR )
                 IF ( I_BND < 1 ) GOTO 470
            END IF
!
! --------- Replace %-expressions in the template file
!
            IF ( INDEX ( BUF(J7), '@%' ) > 0 ) THEN
                 IB = INDEX ( BUF(J7), '_' )
                 IP = LTM_DIF ( 0, MPRC, PRC_NAMES, BUF(J7)(IND(1,2):IB-1) )
                 IF ( IP < 1 ) THEN
                      WRITE ( 6, * ) 'Procedure name: ', BUF(J7)(IND(1,2):IB-1)
                      CALL CLRCH ( STR )
                      CALL INCH ( J7, STR )
                      IUER = -1
                      CALL ERR_LOG ( 6710, IUER, 'GEN_SEQ_PRC', 'Error in '// &
     &                    'parcing line '//TRIM(STR)//' of the procedure '// &
     &                    'template file '//TMPL_FIL(J3) )
                      CALL EXIT ( 1 )
                 END IF
                 IB = INDEX ( BUF(J7), '%' ) 
                 IE = INDEX ( BUF(J7)(IB+1:), '%' ) + IB
                 CALL CHIN  ( BUF(J7)(IB+1:IE-1), DUR(IP,J3) )
                 BUF(J7) = BUF(J7)(1:IB-1)//BUF(J7)(IE+1:)
            END IF
            IF ( BUF(J7)(1:3) == 'bbc' .AND. ILEN(BUF(J7)) .LE. 6 ) THEN
                 CALL CHIN ( BUF(J7)(4:6), IND_BBC )
                 IF ( IND_BBC > 2*NUM_NIF ) GOTO 470
            END IF
!
! --------- Replace @-expressions in the template file
!
            IF ( INDEX ( BUF(J7), '@chs_en' ) > 0 ) THEN
                 IB = INDEX ( BUF(J7), '@chs_en' ) 
                 NO = NO + 1
                 IF ( MIN_NIF == 16 ) THEN
                      OUT(NO) = BUF(J7)(1:IB-1)//'4:chsel_disable:psn_enable;'
                    ELSE
                      IF ( MAX_NIF > 8 ) THEN
                           OUT(NO) = BUF(J7)(1:IB-1)//'4:chsel_enable:psn_enable;'
                         ELSE
                           OUT(NO) = BUF(J7)(1:IB-1)//'2:chsel_enable:psn_enable;'
                      END IF
                 END IF
                 GOTO 470
              ELSE IF ( INDEX ( BUF(J7), '@chsel' ) > 0 ) THEN
!
! -------------- Channel definition
!
                 IF ( MIN_NIF < 16 ) THEN
                      IB = INDEX ( BUF(J7), '@chsel' ) 
                      IE = IB + LEN('@chsel?@')
                      I_BND = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, BUF(J7)(IB+6:IB+6) )
                      NO = NO + 1
                      OUT(NO) = BUF(J7)(1:IB-1)//TRIM(DBE_STR(I_BND)(3:))//BUF(J7)(IE:)
                 END IF 
                 GOTO 470
               ELSE IF ( INDEX ( BUF(J7), '@hds@' ) > 0 ) THEN
!
! -------------- Frequency name
!
                 IB = INDEX ( BUF(J7), '@hds@' ) 
                 IE = IB + LEN('@hds@' )
                 NO = NO + 1
                 OUT(NO) = BUF(J7)(1:IB-1)//TRIM(SEQ%HDS_NAM)//BUF(J7)(IE:)
                 GOTO 470
               ELSE IF ( INDEX ( BUF(J7), '@mode@' ) > 0 ) THEN
!
! -------------- Frequency name
!
                 IB = INDEX ( BUF(J7), '@mode@' ) 
                 IE = IB + LEN('@mode@' )
                 NO = NO + 1
                 OUT(NO) = BUF(J7)(1:IB-1)//TRIM(SEQ%MOD_NAM)//BUF(J7)(IE:)
                 GOTO 470
               ELSE IF ( INDEX ( BUF(J7), '@lo@' ) > 0 ) THEN
!
! -------------- LO frequency defintion
!
                 IB = INDEX ( BUF(J7), '@lo@' ) 
                 IE = IB + LEN('@lo@' )
                 IF ( BUF(J7)(1:1) .NE. '"' ) THEN
                      BAND_NAME = BUF(J7)(6:6) 
                   ELSE
                      BAND_NAME = BUF(J7)(16:16) 
                 END IF
                 IF ( FL_8IF ) THEN
                      IP = INDEX ( 'abcdefgh', BAND_NAME )
                      STR = 'aabbccdd'
                      BAND_NAME = STR(IP:IP)
                 END IF                 
                 I_BND = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, BAND_NAME )
                 NO = NO + 1
                 OUT(NO) = BUF(J7)(1:IB-1)//TRIM(LO_FRQ_STR(I_BND))//BUF(J7)(IE:)
                 IF ( INDEX ( BUF(J7), '@sib@' ) > 0 ) THEN
                      IB = INDEX ( OUT(NO), '@sib@' ) 
                      IE = IB + LEN('@sib@' )
                      OUT(NO) = OUT(NO)(1:IB-1)//TRIM(SEQ%SIB_LO(I_BND))//OUT(NO)(IE:)
                 END IF
                 GOTO 470
               ELSE IF ( INDEX ( BUF(J7), '@udc_lo@' ) > 0 ) THEN
!
! -------------- UDC frequency definition
!
                 IB = INDEX ( BUF(J7), '@udc_lo@' ) 
                 IE = IB + LEN('@udc_lo@' )
                 IP = INDEX ( BUF(J7), '-h udc' ) 
                 IF ( IP > 1 ) THEN
                      STR = BUF(J7)(IP+6:IP+6)
                   ELSE IF ( IP < 1 ) THEN
                      IP = INDEX ( BUF(J7), 'udceth0 udc' ) 
                      STR = BUF(J7)(IP+11:IP+11)
                 END IF
                 CALL TRAN ( 12, STR(1:1), STR(1:1) )
                 I_BND = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, STR )
                 NO = NO + 1
                 IF ( INDEX ( BUF(J7), 'udceth0 udc' ) > 0 ) THEN
                      OUT(NO) = BUF(J7)(1:IB-1)//TRIM(UDC_STR(I_BND))//BUF(J7)(IE:)
                    ELSE 
                      IBL = INDEX ( LO_STR(I_BND), ',' )
                      IEL = INDEX ( LO_STR(I_BND)(IBL+1:), ',' ) + IBL-1
                      OUT(NO) = BUF(J7)(1:IB-1)//LO_STR(I_BND)(IBL+1:IEL)//BUF(J7)(IE:)
                 END IF
                 GOTO 470
               ELSE IF ( INDEX ( BUF(J7), '@bit_rate@' ) > 0 ) THEN
                 IB = INDEX ( BUF(J7), '@bit_rate@' ) 
                 IE = IB + LEN('@bit_rate@' )
                 IF ( SEQ%NIF(1) < 16 ) THEN
                      STR = "8192"
                    ELSE
                      STR = "16384"
                 END IF
                 NO = NO + 1
                 OUT(NO) = BUF(J7)(1:IB-1)//TRIM(STR)//BUF(J7)(IE:)
                 GOTO 470
               ELSE IF ( INDEX ( BUF(J7), '@pcal_step_10@' ) > 0 ) THEN
                 LO_NAME = BUF(J7)(5:5)//' '
                 I_BND = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, LO_NAME )
                 IF ( I_BND < 1 ) THEN
!@                      CALL CLRCH ( STR )
!@                      CALL INCH ( J7, STR )
!@                      IUER = -1
!@                      CALL ERR_LOG ( 6711, IUER, 'GEN_SEQ_PRC', 'Error in '// &
!@     &                    'parcing line '//TRIM(STR)//' of the procedure '// &
!@     &                    'template file '//TMPL_FIL(J3) )
!@                      CALL EXIT ( 1 )
                      GOTO 470
                 END IF
                 WRITE ( UNIT=STR(1:7), FMT='(F5.3,"e6")' ) MOD(SEQ%FRQ_LO(I_BND)+1024.0,10.0D0)
                 IB = INDEX ( BUF(J7), '@pcal_step_10@'  ) 
                 IE = IB + LEN('@pcal_step_10@')
                 NO = NO + 1
                 OUT(NO) = BUF(J7)(1:IB-1)//TRIM(STR)//BUF(J7)(IE:)
                 GOTO 470
               ELSE IF ( INDEX ( BUF(J7), '@pcal_step_5@' ) > 0 ) THEN
                 LO_NAME = BUF(J7)(5:5)//' '
                 I_BND = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, LO_NAME )
                 IF ( I_BND < 1 ) THEN
!@                      CALL CLRCH ( STR )
!@                      CALL INCH ( J7, STR )
!@                      IUER = -1
!@                      CALL ERR_LOG ( 6712, IUER, 'GEN_SEQ_PRC', 'Error in '// &
!@     &                    'parcing line '//TRIM(STR)//' of the procedure '// &
!@     &                    'template file '//TRIM(TMPL_FIL(J3))//' -- cannot '// &
!@     &                    ' find LO with name '//LO_NAME )
!@                      WRITE ( 6, * ) 'Known LO names: ', SEQ%BAND_NAM(1:SEQ%NBAND)
!@                      CALL EXIT ( 1 )
                      GOTO 470
                 END IF
                 WRITE ( UNIT=STR(1:7), FMT='(F5.3,"e6")' ) MOD(SEQ%FRQ_LO(I_BND)+1024.0,5.0D0)
                 IB = INDEX ( BUF(J7), '@pcal_step_5@'  ) 
                 IE = IB + LEN('@pcal_step_5@')
                 NO = NO + 1
                 OUT(NO) = BUF(J7)(1:IB-1)//TRIM(STR)//BUF(J7)(IE:)
                 GOTO 470
               ELSE IF ( INDEX ( BUF(J7), '@if_offset@' ) > 0 ) THEN
                 IB = INDEX ( BUF(J7), '@if_offset@' )
                 IE = IB + LEN('@if_offset@')
                 IL = 0
                 IF ( INDEX ( BUF(J7), ",a," ) > 0 ) BAND_NAME = 'a'
                 IF ( INDEX ( BUF(J7), ",b," ) > 0 ) BAND_NAME = 'b'
                 IF ( INDEX ( BUF(J7), ",c," ) > 0 ) BAND_NAME = 'c'
                 IF ( INDEX ( BUF(J7), ",d," ) > 0 ) BAND_NAME = 'd'
                 IL = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, BAND_NAME )
                 IF ( IL .EQ. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 6713, IUER, 'GEN_SEQ_PRC', 'No LO was found '// &
     &                    'in parsing line '//BUF(J7) )
                      CALL EXIT ( 1 )
                 END IF 
!
                 IF ( IL .NE. LAST_LO ) THEN
                      IND_MK5_BBC = 1
                      LAST_LO = IL
                    ELSE
                      IND_MK5_BBC = IND_MK5_BBC + 1
                 END IF
                 IF ( SEQ%DAS(J3) == 'mark5b_geo' .OR. SEQ%DAS(J3) == 'vdif_geo' ) THEN
!
! ------------------- Special setup for mark5b geo setup
!
                      CALL CHIN ( BUF(J7)(4:5), IND_BBC )
                      IF ( IND_BBC == 1 .OR. IND_BBC == 8 ) THEN
                           SEQ%BAND_SUB(IND_MK5_BBC,IL) = 'L'
                         ELSE
                           SEQ%BAND_SUB(IND_MK5_BBC,IL) = 'U'
                      END IF
                      SEQ%BBC_IND(IND_MK5_BBC,IL) = IND_BBC
                      IF ( SEQ%BAND_SUB(IND_MK5_BBC,IL) == 'L' ) THEN
                           IND_MK5_BBC = IND_MK5_BBC + 1
                           SEQ%BAND_SUB(IND_MK5_BBC,IL) = 'U'
                           SEQ%BBC_IND(IND_MK5_BBC,IL) = IND_BBC
                      END IF 
                 END IF 
                 IF_OFFSET = SEQ%FRQ_IF(IND_MK5_BBC,IL) - SEQ%RANGE(1,IL)
                 WRITE ( UNIT=STR, FMT='(F9.2)' ) IF_OFFSET
                 CALL CHASHL ( STR )
                 BUF(J7) = BUF(J7)(1:IB-1)//TRIM(STR)//BUF(J7)(IE:) 
!
                 IB = INDEX ( BUF(J7), '@if_width@' )
                 IE = IB + LEN('@if_width@')
                 WRITE ( UNIT=STR, FMT='(F9.2)' ) SEQ%BW_IF(1,1)
                 CALL CHASHL ( STR )
                 BUF(J7) = BUF(J7)(1:IB-1)//TRIM(STR)//BUF(J7)(IE:) 
!
                 IB = INDEX ( BUF(J7), '@if_width@' )
                 IF ( IB > 0 ) THEN
                      IE = IB + LEN('@if_width@')
                      WRITE ( UNIT=STR, FMT='(F9.2)' ) SEQ%BW_IF(1,1)
                      CALL CHASHL ( STR )
                      BUF(J7) = BUF(J7)(1:IB-1)//TRIM(STR)//BUF(J7)(IE:) 
                 END IF
!
                 NO = NO + 1
                 OUT(NO) = BUF(J7)
                 GOTO 470
!!
               ELSE IF ( INDEX ( BUF(J7), '@if_mask@' ) > 0 ) THEN
                 IB = INDEX ( BUF(J7), '@if_mask@' )
                 IE = IB + LEN('@if_mask@')
                 IF ( DABS ( SEQ%BW_IF(1,1) - 16.0D0 ) < 1.D-10 ) THEN
                      BUF(J7) = BUF(J7)(1:IB-1)//"0xffffffff"//BUF(J7)(IE:) 
                    ELSE
                      BUF(J7) = BUF(J7)(1:IB-1)//"0x55555555"//BUF(J7)(IE:) 
                 END IF
!
                 IB = INDEX ( BUF(J7), '@if2_width@' )
                 IE = IB + LEN('@if_width@')
                 WRITE ( UNIT=STR, FMT='(F9.2)' ) 2*SEQ%BW_IF(1,1)
                 CALL CHASHL ( STR )
                 BUF(J7) = BUF(J7)(1:IB-1)//TRIM(STR)//BUF(J7)(IE+1:) 
!
                 NO = NO + 1
                 OUT(NO) = BUF(J7)
                 GOTO 470
               ELSE IF ( INDEX ( BUF(J7), '@sideband@' ) > 0 ) THEN
                 IP = INDEX ( BUF(J7), 'core3h=' )
                 IF ( IP > 0 ) THEN
                      CALL CHIN ( BUF(J7)(IP+7:IP+7), IND_CORE )
                    ELSE
                      IP = INDEX ( BUF(J7), 'mode=' )
                      IF ( IP > 0 ) THEN
                           CALL CHIN ( BUF(J7)(IP+5:IP+5), IND_CORE )
                      END IF
                 END IF
                 IF ( IP > 0 ) THEN
                      I_BND = (IND_CORE-1)/2 + 1
                      IS = INDEX ( BUF(J7), '@sideband@' ) 
                      IF ( SEQ%SIB_LO(I_BND) == 'lsb' ) THEN
                           BUF(J7) = BUF(J7)(1:IS-1)//'0x33333333'//BUF(J7)(IS+10:)
                         ELSE IF ( SEQ%SIB_LO(I_BND) == 'usb' ) THEN
                           BUF(J7) = BUF(J7)(1:IS-1)//'0xcccccccc'//BUF(J7)(IS+10:)
                         ELSE 
                           BUF(J7) = BUF(J7)(1:IS-1)//'0xcccccccc'//BUF(J7)(IS+10:)
                      END IF
                 END IF
                 IP = INDEX ( BUF(J7), '@' )
                 IF ( IP > 0 ) THEN
                      GOTO 970
                    ELSE
                      NO = NO + 1
                      OUT(NO) = BUF(J7)
                      GOTO 470
                 END IF
               ELSE IF ( INDEX ( BUF(J7), '@dbbc3=core3h=' ) > 0 ) THEN
                 LO_NAME = BUF(J7)(6:6)
                 I_BND = LTM_DIF ( 0, SEQ%NBAND, SEQ%BAND_NAM, LO_NAME )
                 IF ( I_BND < 1 ) THEN
                      NO = NO + 1
                      OUT(NO) = BUF(J7)(8:22)//'stop'
                    ELSE
                      NO = NO + 1
                      OUT(NO) = BUF(J7)(8:)
                 END IF
                 GOTO 470
               ELSE IF ( INDEX ( BUF(J7), '@dbbc3_bbc@' ) > 0 ) THEN
                 BAND_DBBC3 = 'a'
                 K_BBC = 0
                 POLAR_DBBC3(1) = 'h'
                 POLAR_DBBC3(2) = 'v'
                 DO 4100 J10=1,SEQ%NBAND
                    DO 4110 J11=1,2
                       NO = NO + 1
                       OUT(NO) = '" band '//BAND_DBBC3//' polar: '//POLAR_DBBC3(J11)
                       CALL TRAN ( 11, OUT(NO)(8:8), OUT(NO)(8:8) )
                       DO 4120 J12=1,SEQ%NIF(J10)
                          IF ( SEQ%STA_WIR(J3) == "ws_01" ) THEN
                               IF_OFFSET = SEQ%FRQ_LO(J10) - SEQ%FRQ_IF(J12,J10) + 1536.0D0
                             ELSE IF ( SEQ%STA_WIR(J3) == "nn_01"       .OR. &
     &                                 SEQ%STA_WIR(J3) == "on_01"       .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1d1u2d3d" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1d2d2d2u" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1d1u2u3d" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1d2u3d3d" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1u1u1u1u" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1u1u2d3d" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1u2d2u3d" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1u2u2u3d" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1u1u3d3u" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1d2d2u3d" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1u1u3u3u" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1u1u2u2u" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1d1u1u2u" .OR. &
     &                                 SEQ%STA_WIR(J3) == "sa_1d2d2u3u"      ) THEN
                               IF ( SEQ%SIB_LO(J10) == 'lsb' ) THEN
                                    IF_OFFSET = SEQ%FRQ_LO(J10) - SEQ%FRQ_IF(SEQ%NIF(J10)+1-J12,J10) - 32.0
                                  ELSE IF ( SEQ%SIB_LO(J10) == 'usb' ) THEN
                                    IF_OFFSET = SEQ%FRQ_IF(SEQ%NIF(J10)+1-J12,J10) - SEQ%FRQ_LO(J10) + 32.0
                               END IF
                             ELSE 
                               IUER = -1
                               CALL ERR_LOG ( 6714, IUER, 'GEN_SEQ_PRC', 'Wiring '// &
     &                              SEQ%STA_WIR(J3)//' is not supported' )
                               CALL EXIT ( 1 )
                          END IF
                          WRITE ( UNIT=IF_FRQ_STR, FMT='(F6.1)' ) IF_OFFSET 
                          CALL CHASHL ( IF_FRQ_STR )
                          K_BBC = K_BBC + 1
                          CALL INCH ( K_BBC, STR(1:3) )
                          CALL CHASHR (  STR(1:3) )
                          CALL BLANK_TO_ZERO ( STR(1:3) )
                          WRITE ( UNIT=BW_FRQ_STR, FMT='(F4.1)' ) SEQ%BW_IF(J12,J10)
                          NO = NO + 1
                          OUT(NO) = 'bbc'//STR(1:3)//'='//TRIM(IF_FRQ_STR)//','//BAND_DBBC3//','//BW_FRQ_STR
 4120                  CONTINUE 
                       BAND_DBBC3 = CHAR(ICHAR(BAND_DBBC3)+1)
 4110               CONTINUE 
 4100            CONTINUE 
                 GOTO 470
            END IF
            NO = NO + 1
            OUT(NO) = BUF(J7)
 470     CONTINUE 
!
         ID = LINDEX ( FIL_IN, '/' ) 
         IF ( ID < 1 ) ID = 1
         FIL_PRC_OUT = TRIM(DIR_OUT)//FIL_IN(ID:ILEN(FIL_IN)-4)//'_'//SEQ%STA_ACR(J3)//'.prc'
!
! ------ Write the output procedure file
!
         IUER = -1
         CALL WR_TEXT ( NO, OUT, FIL_PRC_OUT, IUER )
         IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         WRITE ( 6, '(A)' ) 'Written output file '//TRIM(FIL_PRC_OUT)
         WRITE ( 6, '(A)' ) 'Used template  file '//TRIM(TMPL_FIL(J3))
 430  CONTINUE 
!
      FIL_VEX_OUT = TRIM(DIR_OUT)//FIL_IN(ID:ILEN(FIL_IN)-4)//'_vex.frq'
      IND_CHN = 0
      IND_BBC = 0
      NO = 0
      NO = NO + 1 ; OUT(NO) = '*   Hardware  setup: '//TRIM(SEQ%HDS_NAM)
      NO = NO + 1 ; OUT(NO) = '*   Frequency setup: '//TRIM(SEQ%MOD_NAM)
      DO 4130 J13=1,SEQ%NBAND
         NO = NO + 1 ; OUT(NO) = '*'
!
         IF ( SEQ%DAS(1) == 'rdbe'      .OR. &
     &        SEQ%DAS(1) == 'r2dbe'     .OR. &
     &        SEQ%DAS(1) == 'dbbc3'     .OR. &
     &        SEQ%DAS(1) == 'cdas_rpol' .OR. &
     &        SEQ%DAS(1) == 'cdas_dual'      ) THEN
              SBD_STR = 'L'
              DO 4140 J14=1,2
                 IF ( SEQ%DAS(1) == 'cdas_rpol' ) THEN
                     IF ( J14 == 1 ) THEN
                          PRLZ_STR(J14) = 'R'
                       ELSE IF ( J14 == 2 ) THEN
                          GOTO 4140
                      END IF
                 END IF
                 DO 4150 J15=SEQ%NIF(J13),1,-1
                    IND_CHN = IND_CHN + 1
                    IND_BBC = IND_BBC + 1
                    WRITE ( UNIT=STR, FMT=140 ) PRLZ_STR(J14), SEQ%FRQ_IF(J15,J13) + SEQ%BW_IF(J15,J13), &
     &                                          SBD_STR, SEQ%BW_IF(J15,J13), IND_CHN, IND_BBC
 140                FORMAT  ( '    chan_def = &', A, ' : ', F9.2, ' MHz : ', A, ' : ', F6.2, &
     &                        ' MHz : &CH', I3, ' : &BBC', I3, ' : &L_cal;' )
                    CALL BLANK_TO_ZERO ( STR(57:59) )
                    CALL BLANK_TO_ZERO ( STR(67:69) )
                    NO = NO + 1
                    OUT(NO) = TRIM(STR)
 4150             CONTINUE 
 4140         CONTINUE 
           ELSE IF ( SEQ%DAS(1) == 'mark5b_geo' ) THEN
              DO 4160 J16=1,SEQ%NIF(J13)
                 IND_CHN = IND_CHN + 1
                 IF ( SEQ%FRQ_IF(J16,J13) < 3000.0 ) THEN
                      BND = 'S'
                    ELSE
                      BND = 'X'
                 END IF
                 IF ( MK5B_ORIG_SUB(IND_CHN) == 'U' ) THEN
                      WRITE ( UNIT=STR, FMT=150 )  BND, SEQ%FRQ_IF(J16,J13), &
     &                                             MK5B_ORIG_SUB(IND_CHN), SEQ%BW_IF(J16,J13), &
     &                                             MK5B_ORIG_CHAN(IND_CHN), MK5B_ORIG_BBC(IND_CHN)
                    ELSE 
                      WRITE ( UNIT=STR, FMT=150 )  BND, SEQ%FRQ_IF(J16,J13) + SEQ%BW_IF(J16,J13), &
     &                                             MK5B_ORIG_SUB(IND_CHN), SEQ%BW_IF(J16,J13), &
     &                                             MK5B_ORIG_CHAN(IND_CHN), MK5B_ORIG_BBC(IND_CHN)
                 END IF
 150             FORMAT  ( '    chan_def = &', A, ' : ', F9.2, ' MHz : ', A, ' : ', F6.2, &
     &                     ' MHz : &CH', I3, ' : &BBC', I3, ' : &L_cal;' )
                 CALL BLANK_TO_ZERO ( STR(57:59) )
                 CALL BLANK_TO_ZERO ( STR(67:69) )
                 NO = NO + 1
                 OUT(NO) = TRIM(STR)
 4160         CONTINUE 
           ELSE IF ( SEQ%DAS(1) == 'vdif_geo' ) THEN
              DO 4170 J17=1,SEQ%NIF(J13)
                 IND_CHN = IND_CHN + 1
                 IF ( SEQ%FRQ_IF(J17,J13) < 3000.0 ) THEN
                      BND = 'S'
                    ELSE
                      BND = 'X'
                 END IF
                 IF ( MK5B_VDIF_SUB(IND_CHN) == 'U' ) THEN
                      WRITE ( UNIT=STR, FMT=150 )  BND, SEQ%FRQ_IF(J17,J13), &
     &                                             MK5B_VDIF_SUB(IND_CHN), SEQ%BW_IF(J17,J13), &
     &                                             MK5B_VDIF_CHAN(IND_CHN), MK5B_VDIF_BBC(IND_CHN)
                    ELSE 
                      WRITE ( UNIT=STR, FMT=150 )  BND, SEQ%FRQ_IF(J17,J13) + SEQ%BW_IF(J17,J13), &
     &                                             MK5B_VDIF_SUB(IND_CHN), SEQ%BW_IF(J17,J13), &
     &                                             MK5B_VDIF_CHAN(IND_CHN), MK5B_VDIF_BBC(IND_CHN)
                 END IF
                 CALL BLANK_TO_ZERO ( STR(57:59) )
                 CALL BLANK_TO_ZERO ( STR(67:69) )
                 NO = NO + 1
                 OUT(NO) = TRIM(STR)
 4170         CONTINUE 
         END IF
 4130 CONTINUE 
      WRITE ( UNIT=STR, FMT='(F8.1)' ) 2.0D0*SEQ%BW_IF(1,1)
      CALL CHASHL ( STR )
      NO = NO + 1 ; OUT(NO) = '*'
      NO = NO + 1 ; OUT(NO) = '    sample_rate = '//TRIM(STR)//' Ms/sec;'
!
      DO 4180 J18=1,NO
         CALL EXWORD ( BUF(J18), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9)//'_', IUER )
         IF ( LIND .GE. 6 ) THEN
              IF ( BUF(J18)(IND(1,6):IND(2,6)) == '@DUR@' ) THEN
                   IP = LTM_DIF ( 0, MPRC, PRC_NAMES, BUF(J18)(IND(1,1):IND(2,1)) )
                   CALL CLRCH ( BUF(J18)(IND(1,6)-2:IND(2,6)+2) )
                   CALL INCH  ( DUR(IP,1), BUF(J18)(IND(1,6):IND(1,6)) )
              END IF
         END IF
 4180 CONTINUE 
!
      IUER = -1
      CALL WR_TEXT ( NO, OUT, FIL_VEX_OUT, IUER )
!
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, '(A)' ) 'Written output file '//TRIM(FIL_VEX_OUT)
      END  PROGRAM  GEN_SEQ_PRC  !#!#
