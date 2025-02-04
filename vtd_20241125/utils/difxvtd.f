      PROGRAM    DIFXVTD
! ************************************************************************
! *                                                                      *
! *   Program DIFXVTD computes VLBI apriori model for the use by DiFX    *
! *   correlator. It is assumed *.input and *.calc files are created     *
! *   with vex2difx program. These files describe each job and           *
! *   parameters for computation of the interferometric mode. DIFXVTD    *
! *   reads specified input files and associated calc files that should  *
! *   not be specified directly or all input files in the specified      *
! *   directory and process each job. DIFXVTD creates an output file     *
! *   with extension .im that keeps interpolation polynomials for DiFX.  *
! *   The interpolation polynomials of the 5th degree are computed for   *
! *   the central epoch of 120s long interval.                           *
! *                                                                      *
! *   Usage:  Usage dif2vtd -c vtd_config [-v verb] [-m embed|external]  *
! *                 input_file_or_directory ...                          *
! *                                                                      *
! *   vtd_config -- configuration file for VTD.                          *
! *   verb       -- verbosity parameter: 0 -- silent, 1 -- informative   *
! *                 messages (default); > 1 debugging mode.              *
! *   embed      -- station and source positions embedded in calc file   *
! *                 are used (default).                                  *
! *   external   -- station and source positions specified in the vtd    *
! *                 control file are used.                               *
! *                                                                      *
! *   Arguments followed by options are considered as either file or     *
! *   a directory. Files without extension .input are ignored. If        *
! *   a directory name is specified, DIFXVTD travels the entire          *
! *   directory tree and collect all input files.                        *
! *                                                                      *
! *  ### 06-APR-2021     DIFXVTD   v1.1 (c)  L. Petrov  02-MAY-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'difxvtd.i'
      INTEGER*4  MINP
      PARAMETER  ( MINP = 8193 )
      TYPE       ( DFI__TYPE ) :: DFI
      TYPE       ( VTD__TYPE ) :: VTD
      CHARACTER  DIRINP*128, FILINP(MINP)*128, FILCALC*128, &
     &           FILCNT*128, STR*128, FILIN*128
      INTEGER*8  DIR_DESC(16), STACK_SIZE_IN_BYTES, STACK_SIZE_IN_GB
      PARAMETER  ( STACK_SIZE_IN_GB = 2 )
      LOGICAL*1  LEX      
      INTEGER*4  J1, J2, J3, J4, IL, IP, IS, LEV, L_INP, IVAL, DFI_MOD, &
     &           IA, IVRB, IUER
#ifdef DARWIN
#      define    FUNC_OPENDIR  OPENDIR$INODE64
#else
#      define    FUNC_OPENDIR  OPENDIR
#endif
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR, SET_STACKSIZE 
      INTEGER*4, EXTERNAL :: CLOSEDIR, GET_FILE_FROM_DIR, ILEN, I_LEN
!
! --- Set stacksize
!
      STACK_SIZE_IN_BYTES = INT8(1024*1024*1024)*STACK_SIZE_IN_GB
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage difxvtd -c vtd_config [-v verb] [-m embed|external] input_file_or_directory ... '
           CALL EXIT ( 1 ) 
         ELSE 
           IF ( IARGC() > MINP ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( MINP, STR )
                IUER = -1
                CALL ERR_LOG ( 5201, IUER, 'DIFXVTD', 'Too many input files: '// &
     &              'more than '//STR )
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
! --- Defaults
!
      IVRB = 1
      DFI_MOD = DFI__EMBED
!
      CALL GETARG ( 1, STR )
      IF ( STR(1:2) .NE. '-c' ) THEN
           IUER = -1
           CALL ERR_LOG ( 5202, IUER, 'DIFXVTD', 'The first argument should be -c' )
           CALL EXIT ( 1 )
      END IF
      IA = 2
      CALL GETARG ( IA , FILCNT )
      DO 410 J1=1,2
         CALL GETARG ( IA+1, STR )
         IF ( STR(1:2) == '-v' ) THEN
              CALL GETARG ( IA+2, STR )
              CALL CHIN   ( STR, IVRB )
              IF ( IVRB < 0 .OR. IVRB > 6 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 5203, IUER, 'DIFXVTD', 'Error in parsing value '// &
     &                           'of argument -v: an integer in [0, 6] range '// &
     &                           ' was expected, but got '//STR )
                   CALL EXIT ( 1 )
              END IF
              IA = IA + 2
         END IF
!
         CALL GETARG ( IA, STR )
         IF ( STR(1:2) == '-m' ) THEN
              CALL GETARG ( IA+1, STR )
              IF ( STR == 'embed' ) THEN
                   DFI_MOD = DFI__EMBED
                 ELSE IF ( STR == 'external' ) THEN
                   DFI_MOD = DFI__EXTERNAL
                 ELSE
                   IUER = -1
                   CALL ERR_LOG ( 5204, IUER, 'DIFXVTD', 'Error in parsing value '// &
     &                           'of argument -m: embed or external were '// &
     &                           ' expected, but got '//STR )
                   CALL EXIT ( 1 )
              END IF
              IA = IA + 2
         END IF
 410  CONTINUE 
      IF ( IA .GE. IARGC() ) THEN
           IUER = -1
           CALL ERR_LOG ( 5205, IUER, 'DIFXVTD', 'No imput files were specified' )
           CALL EXIT ( 1 )
      END IF
!
      L_INP = 0
      IA  = IA + 1
      DO 420 J2=IA,IARGC()
         CALL GETARG ( J2, STR )
         DIR_DESC(1) = FUNC_OPENDIR ( TRIM(STR)//CHAR(0) )
         IF ( DIR_DESC(1) .EQ. 0 ) THEN
!
! ----------- The name was file
!
              IF ( ILEN(STR) < 7 ) GOTO 420
              IF ( STR(ILEN(STR)-5:ILEN(STR)) .NE. '.input' ) GOTO 420
!
              L_INP = L_INP + 1
              FILINP(L_INP) = STR
              INQUIRE ( FILE=FILINP(L_INP), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 5206, IUER, 'DIFXVTD', 'Input file '// &
     &                  TRIM(FILINP(L_INP))//' does not exist' )
                   CALL EXIT ( 1 )
              END IF
            ELSE
              IP = CLOSEDIR ( %VAL(DIR_DESC(1)) )
!
! ----------- The name was a directory
!
              LEV = 0
              DIRINP = STR
              DO 430 J3=1,MINP*32
                 IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRINP, FILIN )
                 IF ( IS .NE. 0 ) THEN
                      IUER = -2
                      CALL ERR_LOG ( 5207, IUER, 'DIFXVTD',  'Error in '// &
     &                    'reading input directory '//TRIM(DIRINP)// &
     &                    '  '//FILIN )
                      CALL EXIT ( 1 )
                 END IF
                 IF ( LEV == 0 ) GOTO 830 ! End of work
                 IL = ILEN(FILIN)
                 IF ( IL < 6 ) GOTO 430
                 IF ( INDEX ( FILIN, '#' ) > 0 ) GOTO 430
                 IF ( INDEX ( FILIN, '~' ) > 0 ) GOTO 430
                 IF ( INDEX ( FILIN, '.input' ) > 0 ) THEN
                      L_INP = L_INP + 1
                      FILINP(L_INP) = FILIN
                 END IF
 430          CONTINUE 
 830          CONTINUE 
         END IF
 420  CONTINUE 
      IF ( L_INP < 1 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5208, IUER, 'DIFXVTD', 'No input files in '// &
     &                   'directory '//TRIM(DIRINP)//' were found' )
           CALL EXIT ( 1 )
      END IF
      CALL SORT_FAST_CH ( L_INP, FILINP )
!
      DO 440 J4=1,L_INP
         IUER = -1
         CALL VTD_INIT ( VTD,  IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 5209, IUER, 'DIFXVTD', 'Error in an attempt to '// &
     &            'initialize VTD oibject' )
              CALL EXIT ( 1 ) 
         END IF
!
! ------ Read and parse configuration file
!
         IUER = -1
         CALL VTD_CONF ( FILCNT, VTD, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 5210, IUER, 'DIFXVTD', 'Error in an attempt '// &
     &            'to read configuration file '//FILCNT )
              CALL EXIT ( 1 ) 
         END IF
         IF ( IVRB > 1 ) THEN
              VTD%CONF%IVRB = IVRB
         END IF
!
         IL = ILEN(FILINP(J4))
         FILCALC = FILINP(J4)(1:IL-6)//'.calc'
         IUER = -1
         CALL PARSE_DIFX_INPUT ( FILINP(J4), FILCALC, DFI, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 5211, IUER, 'DIFXVTD', 'Error in parsing DiFX input '// &
     &            'file '//FILINP(L_INP) )
              CALL EXIT ( 1 )
         END IF
!         CALL DUMP_DFI ( DFI )
!
         IUER = -1
         CALL DIFX_VTD_COMP_THEO ( DFI, VTD, DFI_MOD, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 5212, IUER, 'DIFXVTD', 'Error in parsing DiFX input '// &
     &            'file '//FILINP(L_INP) )
              CALL EXIT ( 1 )
         END IF
         IF ( IVRB > 0 ) THEN
              WRITE ( 6, 110 ) J4, L_INP, TRIM(DFI%IM_FILE)
 110          FORMAT ( 'DIFXVTD: ', I4,' ( ', I4, ' ) Written file ', A )
         END IF
 440  CONTINUE 
!
      END  PROGRAM  DIFXVTD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PARSE_DIFX_INPUT ( FILINP, FILCALC, DFI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PARSE_DIFX_INPUT
! *                                                                      *
! * ### 08-APR-2021  PARSE_DIFX_INPUT v1.0 (c) L. Petrov 08-APR-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'difxvtd.i'
      CHARACTER  FILINP*(*), FILCALC*(*)
      TYPE       ( DFI__TYPE ) :: DFI
      INTEGER*4  IUER
      INTEGER*4    MBUF, MIND
      PARAMETER  ( MBUF = 8192 )
      PARAMETER  ( MIND =   32 )
      CHARACTER  BUFI(MBUF)*128, BUFC(MBUF)*128, STR*32, STA_NAM*16
      INTEGER*4  J1, J2, J3, J4, J5, IP, IVAL, NI, NC, IND_FRQ, IND_STA, &
     &           IND_POL, IND_SOU, IND_EOP, IND_SCA, IND(2,MIND), LIND, IER
      REAL*8     VAL_R8
      INTEGER*4, EXTERNAL ::  ILEN, I_LEN, LTM_DIF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILINP,  MBUF, BUFI, NI, IER )
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILCALC, MBUF, BUFC, NC, IER )
!
      CALL NOUT ( SIZEOF(DFI), DFI )
      DO 410 J1=1,NI
         IF ( BUFI(J1)(1:14) == 'CALC FILENAME:' ) THEN
              DFI%CALC_FIL = BUFI(J1)(21:)
            ELSE IF ( BUFI(J1)(1:10) == 'START MJD:'          ) THEN
              CALL CHIN ( BUFI(J1)(21:), DFI%INPUT_MJD_START )
            ELSE IF ( BUFI(J1)(1:14) == 'START SECONDS:'      ) THEN
              IF ( INDEX ( BUFI(J1)(21:), '.' ) < 1 ) THEN
                   BUFI(J1) = BUFI(J1)(1:ILEN(BUFI(J1)))//'.'
              END IF
              READ ( UNIT=BUFI(J1)(21:), FMT='(F10.5) ' ) DFI%INPUT_UTC_START
            ELSE IF ( BUFI(J1)(1:19) == 'EXECUTE TIME (SEC):' ) THEN
              IF ( INDEX ( BUFI(J1)(21:), '.' ) < 1 ) THEN
                   BUFI(J1) = BUFI(J1)(1:ILEN(BUFI(J1)))//'.'
              END IF
              READ ( UNIT=BUFI(J1)(21:), FMT='(F10.5) ' ) DFI%DUR
            ELSE IF ( BUFI(J1)(1:13) == 'FREQ ENTRIES:'       ) THEN
              CALL CHIN ( BUFI(J1)(21:), DFI%N_FRQ )
            ELSE IF ( BUFI(J1)(1:10) == 'FREQ (MHZ)'          ) THEN
              STR = BUFI(J1)(12:20)
              IP = INDEX ( STR, ':' ) 
              CALL CHIN ( STR(1:IP-1), IND_FRQ )
              IND_FRQ = IND_FRQ + 1
              READ ( UNIT=BUFI(J1)(21:), FMT='(F21.10) ' ) DFI%FRQ(IND_FRQ)
            ELSE IF ( BUFI(J1)(1:8)  == 'BW (MHZ)'            ) THEN
              STR = BUFI(J1)(10:20)
              IP = INDEX ( STR, ':' ) 
              CALL CHIN ( STR(1:IP-1), IND_FRQ )
              IND_FRQ = IND_FRQ + 1
              READ ( UNIT=BUFI(J1)(21:), FMT='(F21.10) ' ) DFI%BW(IND_FRQ)
            ELSE IF ( BUFI(J1)(1:8)  == 'SIDEBAND'            ) THEN
              STR = BUFI(J1)(10:20)
              IP = INDEX ( STR, ':' ) 
              CALL CHIN ( STR(1:IP-1), IND_FRQ )
              IND_FRQ = IND_FRQ + 1
              IF ( BUFI(J1)(21:21) == 'L' ) THEN
                   DFI%SUB_BAND(IND_FRQ) = 1
                ELSE IF ( BUFI(J1)(21:21) == 'U' ) THEN
                   DFI%SUB_BAND(IND_FRQ) = 2
              END IF
            ELSE IF ( BUFI(J1)(1:18) == 'TELESCOPE ENTRIES:'  ) THEN
              CALL CHIN ( BUFI(J1)(21:), DFI%N_STA )
            ELSE IF ( BUFI(J1)(1:14) == 'TELESCOPE NAME'      ) THEN
              STR = BUFI(J1)(16:20)
              IP = INDEX ( STR, ':' ) 
              CALL CHIN ( STR(1:IP-1), IND_FRQ )
              IND_FRQ = IND_FRQ + 1
              DFI%SHR_STA_NAM(IND_FRQ) = BUFI(J1)(21:)
            ELSE IF ( BUFI(J1)(1:13) == 'CLOCK REF MJD'       ) THEN
              STR = BUFI(J1)(15:20)
              IP = INDEX ( STR, ':' ) 
              CALL CHIN ( STR(1:IP-1), IND_STA )
              IND_STA = IND_STA + 1
              READ ( UNIT=BUFI(J1)(21:), FMT='(F21.10) ' ) VAL_R8
              DFI%MJD_CLO_REF(IND_STA) =  INT ( VAL_R8 )
              DFI%UTC_CLO_REF(IND_STA) = (VAL_R8 - DFI%MJD_CLO_REF(DFI__MSTA))*86400.0D0
            ELSE IF ( BUFI(J1)(1:16) == 'CLOCK POLY ORDER'    ) THEN
              STR = BUFI(J1)(18:20)
              IP = INDEX ( STR, ':' ) 
              CALL CHIN ( STR(1:IP-1), IND_STA )
              IND_STA = IND_STA + 1
              CALL CHIN ( BUFI(J1)(21:), DFI%N_CLP(IND_STA) )
            ELSE IF ( BUFI(J1)(1:11) == 'CLOCK COEFF'         ) THEN
              STR = BUFI(J1)(13:20)
              IP = INDEX ( STR, '/' ) 
              CALL CHIN ( STR(1:IP-1), IND_STA )
              IND_STA = IND_STA + 1
              STR = STR(IP+1:)
              IP = INDEX ( STR, ':' ) 
              CALL CHIN ( STR(1:IP-1), IND_POL )
              READ ( UNIT=BUFI(J1)(21:), FMT='(F21.10)' ) DFI%CLO_POL(IND_POL,IND_STA)
              DFI%CLO_POL(IND_POL,IND_STA) = 1.D-6*DFI%CLO_POL(IND_POL,IND_STA)
         END IF
 410  CONTINUE 
!
      IND_STA = 0
      IND_SOU = 0
      IND_EOP = 0
      DO 420 J2=1,NC
         IP = INDEX ( BUFC(J2), ':' )
         CALL EXWORD ( BUFC(J2), MIND, LIND, IND, ' :', IER )
         IF ( BUFC(J2)(1:9) == 'TELESCOPE' .AND. INDEX ( BUFC(J2), 'NAME:' ) > 0 ) THEN
              STA_NAM = BUFC(J2)(IND(1,4):IND(2,4))
!?              IND_STA = LTM_DIF ( 0, DFI%N_STA, DFI%SHR_STA_NAM, STA_NAM )
              IND_STA = IND_STA + 1
           ELSE IF ( BUFC(J2)(1:10) == 'START MJD:' ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,3):IND(2,3)) , FMT='(F22.5)' ) VAL_R8
              DFI%CALC_MJD_START = INT(VAL_R8)
              DFI%CALC_UTC_START = IDNINT( 86400.D0*(VAL_R8 - DFI%CALC_MJD_START) )
           ELSE IF ( BUFC(J2)(1:9) == 'TELESCOPE' .AND. INDEX ( BUFC(J2), 'MOUNT:' ) > 0 ) THEN
              DFI%MOUNT(IND_STA) = BUFC(J2)(IND(1,4):IND(2,4))
           ELSE IF ( BUFC(J2)(1:9) == 'TELESCOPE' .AND. INDEX ( BUFC(J2), 'OFFSET' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)) , FMT='(F22.5)' ) DFI%AXIS_OFF(IND_STA) 
           ELSE IF ( BUFC(J2)(1:9) == 'TELESCOPE' .AND. INDEX ( BUFC(J2), 'X (m):' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)),  FMT='(F22.5)' ) DFI%STA_POS(1,IND_STA) 
           ELSE IF ( BUFC(J2)(1:9) == 'TELESCOPE' .AND. INDEX ( BUFC(J2), 'Y (m):' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)),  FMT='(F22.5)' ) DFI%STA_POS(2,IND_STA) 
           ELSE IF ( BUFC(J2)(1:9) == 'TELESCOPE' .AND. INDEX ( BUFC(J2), 'Z (m):' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)),  FMT='(F22.5)' ) DFI%STA_POS(3,IND_STA) 
         END IF 
         IF ( BUFC(J2)(1:12) == 'NUM SOURCES:' ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,3):IND(2,3)), FMT='(I8)' ) DFI%N_SOU
           ELSE IF ( BUFC(J2)(1:6) == 'SOURCE' .AND. INDEX ( BUFC(J2), 'NAME:' ) > 0 ) THEN
              IND_SOU = IND_SOU + 1
              DFI%SOU_NAM(IND_SOU) = BUFC(J2)(IND(1,4):IND(2,4))
           ELSE IF ( BUFC(J2)(1:6) == 'SOURCE' .AND. INDEX ( BUFC(J2), 'RA:' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,4):IND(2,4)), FMT='(F22.5)' ) DFI%SOU_COO(1,IND_SOU) 
           ELSE IF ( BUFC(J2)(1:6) == 'SOURCE' .AND. INDEX ( BUFC(J2), 'DEC:' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,4):IND(2,4)), FMT='(F22.5)' ) DFI%SOU_COO(2,IND_SOU) 
         END IF
         IF ( BUFC(J2)(1:10) == 'NUM SCANS:' ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,3):IND(2,3)), FMT='(I5)' ) DFI%N_SCA
            ELSE IF ( BUFC(J2)(1:4) == 'SCAN' .AND. INDEX ( BUFC(J2), 'IDENTIFIER:' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,2):IND(2,2)), FMT='(I5)'    ) IND_SCA
              IND_SCA = IND_SCA + 1
              DFI%SCAN_ID(IND_SCA) = BUFC(J2)(IND(1,4):IND(2,4))
            ELSE IF ( BUFC(J2)(1:4) == 'SCAN' .AND. INDEX ( BUFC(J2), 'POINTING' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,2):IND(2,2)), FMT='(I5)'    ) IND_SCA
              IND_SCA = IND_SCA + 1
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)), FMT='(I5)'    ) DFI%SCAN_IND_SOU(IND_SCA)
              DFI%SCAN_IND_SOU(IND_SCA) = DFI%SCAN_IND_SOU(IND_SCA) + 1
            ELSE IF ( BUFC(J2)(1:4) == 'SCAN' .AND. INDEX ( BUFC(J2), 'START' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,2):IND(2,2)), FMT='(I5)'    ) IND_SCA
              IND_SCA = IND_SCA + 1
              IF ( INDEX ( BUFC(J2)(IND(1,5):IND(2,5)), '.' ) < 1 ) THEN
                   IND(2,5) = IND(2,5) + 1
                   BUFC(J2)(IND(2,5):IND(2,5)) = '.'
              END IF 
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)), FMT='(F12.5)' ) DFI%SCAN_START_OFFSET(IND_SCA)
            ELSE IF ( BUFC(J2)(1:4) == 'SCAN' .AND. INDEX ( BUFC(J2), 'DUR' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,2):IND(2,2)), FMT='(I5)'    ) IND_SCA
              IND_SCA = IND_SCA + 1
              IF ( INDEX ( BUFC(J2)(IND(1,5):IND(2,5)), '.' ) < 1 ) THEN
                   IND(2,5) = IND(2,5) + 1
                   BUFC(J2)(IND(2,5):IND(2,5)) = '.'
              END IF 
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)), FMT='(F12.5)' ) DFI%SCAN_DUR(IND_SCA)
         END IF
         IF ( BUFC(J2)(1:9) == 'NUM EOPS:' ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,3):IND(2,3)), FMT='(I8)' ) DFI%N_EOP
            ELSE IF ( BUFC(J2)(1:3) == 'EOP' .AND. INDEX ( BUFC(J2), 'TIME' ) > 0 ) THEN
              IND_EOP = IND_EOP + 1
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)), FMT='(I8)' ) DFI%MJD_EOP(IND_EOP)
              DFI%UTC_EOP(IND_EOP) = 0.0D0
            ELSE IF ( BUFC(J2)(1:3) == 'EOP' .AND. INDEX ( BUFC(J2), 'TAI_UTC' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)), FMT='(I8)' ) IVAL
              DFI%UTC_MTAI = -IVAL
            ELSE IF ( BUFC(J2)(1:3) == 'EOP' .AND. INDEX ( BUFC(J2), 'XPOLE' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)), FMT='(F22.15)' ) DFI%EOP(1,IND_EOP)
              DFI%EOP(1,IND_EOP) = ARCSEC__TO__RAD*DFI%EOP(1,IND_EOP)
            ELSE IF ( BUFC(J2)(1:3) == 'EOP' .AND. INDEX ( BUFC(J2), 'YPOLE' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)), FMT='(F22.15)' ) DFI%EOP(2,IND_EOP)
              DFI%EOP(2,IND_EOP) = ARCSEC__TO__RAD*DFI%EOP(2,IND_EOP)
            ELSE IF ( BUFC(J2)(1:3) == 'EOP' .AND. INDEX ( BUFC(J2), 'UT1_UTC' ) > 0 ) THEN
              READ ( UNIT=BUFC(J2)(IND(1,5):IND(2,5)), FMT='(F22.15)' ) DFI%EOP(3,IND_EOP)
              DFI%EOP(3,IND_EOP) = SEC__TO__RAD*(DFI%EOP(3,IND_EOP) + DFI%UTC_MTAI)
            ELSE IF ( BUFC(J2)(1:12) == 'IM FILENAME:' ) THEN
              DFI%IM_FILE = BUFC(J2)(IND(1,3):IND(2,3))
         END IF
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_DIFX_INPUT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DUMP_DFI ( DFI )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'difxvtd.i'
      TYPE       ( DFI__TYPE ) :: DFI
!
         write ( 6, * ) 'dfi%n_sta=       ', dfi%n_sta
         write ( 6, * ) 'dfi%n_sou=       ', dfi%n_sou
         write ( 6, * ) 'dfi%sou_nam=     ', dfi%sou_nam(1:dfi%n_sou)
         write ( 6, * ) 'dfi%shr_sta_nam= ', dfi%shr_sta_nam(1:dfi%n_sta)
         write ( 6, * ) 'dfi%sta_pos1=    ', dfi%sta_pos(1:3,1)
         write ( 6, * ) 'dfi%sta_pos2=    ', dfi%sta_pos(1:3,2)
         write ( 6, * ) 'dfi%sou_coo =    ', dfi%sou_coo(1:2,1)
         write ( 6, * ) 'dfi%n_eop=       ', dfi%n_eop
         write ( 6, * ) 'dfi%mjd_eop=     ', dfi%mjd_eop(1)
         write ( 6, * ) 'dfi%eop_1=       ', dfi%eop(1:3,1)
         write ( 6, * ) 'dfi%eop_2=       ', dfi%eop(1:3,2)
         write ( 6, * ) 'dfi%eop_3=       ', dfi%eop(1:3,3)
         write ( 6, * ) 'dfi%eop_4=       ', dfi%eop(1:3,4)
         write ( 6, * ) 'dfi%eop_5=       ', dfi%eop(1:3,5)
      RETURN
      END  SUBROUTINE DUMP_DFI  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIFX_VTD_COMP_THEO ( DFI, VTD, DFI_MOD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DIFX_VTD_COMP_THEO 
! *                                                                      *
! * ## 26-APR-2021 DIFX_VTD_COMP_THEO v1.0 (c) L. Petrov  26-APR-2021 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'difxvtd.i'
      TYPE       ( DFI__TYPE ) :: DFI
      TYPE       ( VTD__TYPE ) :: VTD
      TYPE       ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  DFI_MOD, IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 16*1024 )
      CHARACTER  BUF(MBUF)*256, DATE_STR*21, STA_NAM*8, STR*256, STR1*80, STR2*80
      CHARACTER  STACOO__LABEL1*46, STACOO__LABEL2*34, OUT(MBUF)*256
      PARAMETER  ( STACOO__LABEL1 = '# GETPAR_STA format version 1.0  of 2001.05.25' )
      PARAMETER  ( STACOO__LABEL2 = '$$  SIT-MODFILE Format 2001.09.26 ' )
      REAL*8     D1, DN, WORK(UEOP__MP), TAI_SEC, STA_POS(3), DIST, &
     &           DELAY, RATE, DER_DEL(VTD__NDER), DER_RAT(VTD__NDER)
      INTEGER*4  M_EPC, M_CMP, MIND
      PARAMETER  ( M_EPC = 16 )
      PARAMETER  ( M_CMP =  8 )
      PARAMETER  ( MIND  = 32 )
      INTEGER*4  MC__DEL, MC__DRY, MC__WET, MC__AZ, MC__EL, MC__U, MC__V, MC__W
      PARAMETER  ( MC__DEL = 1 )
      PARAMETER  ( MC__DRY = 2 )
      PARAMETER  ( MC__WET = 3 )
      PARAMETER  ( MC__AZ  = 4 )
      PARAMETER  ( MC__EL  = 5 )
      PARAMETER  ( MC__U   = 6 )
      PARAMETER  ( MC__V   = 7 )
      PARAMETER  ( MC__W   = 8 )
!
      INTEGER*4    N1
      REAL*8       UNSCL_CMP(M_CMP)
      CHARACTER    DELAY_COMP(M_CMP)*11
      DATA         ( DELAY_COMP(N1), UNSCL_CMP(N1), N1=1, M_CMP ) &
     &                 / &
     &                  'DELAY (us):', -1.0D-6,      &
     &                  'DRY (us):  ', 1.0D-6,       &
     &                  'WET (us):  ', 1.0D-6,       &
     &                  'AZ:        ', DEG__TO__RAD, &
     &                  'EL GEOM:   ', DEG__TO__RAD, &
     &                  'U (m):     ', 1.0D0,        &
     &                  'V (m):     ', 1.0D0,        &
     &                  'W (m):     ', 1.0D0         &
     &                 /
      REAL*8     TIM_ARR(M_EPC), DEL_ARR(M_EPC,M_CMP), &
     &           POLY_COEF(0:DFI__DEG_POL,M_CMP), COEF_ARR(DFI__DEG_POL+1)
      REAL*8     DIST_MIN 
      PARAMETER  ( DIST_MIN = 5.0D0 )
      INTEGER*4  MJD_BEG, MJD_END, MJD_EPC
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           I_FMT, IOS, NBUF, N_EPC, NS_EPC, NO, NO_SRC_1ST, MJD_SCA, &
     &           IP, LIND, IND(2,MIND), IER
      REAL*8     UTC_BEG, UTC_END, TAI_BEG, TAI_END, TAI_EPC, UTC_SCA
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      NS_EPC = DFI__DEG_POL + 1
!
! --- Read the file with station coordinates
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD%CONF%FINAM_STACOO, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5211, IUER, 'DIFX_COMP_THEO', 'Error in an '// &
     &         'attempt to read input file with station coordinates'// &
     &          VTD%CONF%FINAM_STACOO )
           RETURN 
      END IF
!
      I_FMT = 0
      IF ( BUF(1)(1:LEN(STACOO__LABEL1)) .EQ. STACOO__LABEL1 ) THEN
           I_FMT = 1
         ELSE IF ( BUF(1)(1:LEN(STACOO__LABEL2)) .EQ. STACOO__LABEL2 ) THEN
           I_FMT = 2
         ELSE
           CALL ERR_LOG ( 5212, IUER, 'DIFX_COMP_THEO', 'Error in an '// &
     &         'attempt to parse input file with station coordinates '// &
     &          TRIM(VTD%CONF%FINAM_STACOO)// &
     &         ' -- format of this file was not recognized' )
           RETURN 
      END IF
!
! --- Load catalogues, ephemerides, EOP series and other data files
!
      MJD_BEG = DFI%CALC_MJD_START
      UTC_BEG = DFI__DUR_POL *  INT((DFI%CALC_UTC_START/DFI__DUR_POL))
      UTC_END = DFI__DUR_POL * (INT((UTC_BEG + DFI%DUR)/DFI__DUR_POL) + 1)
      IF ( UTC_END .GE. 86400.0D0 ) THEN
           MJD_END = DFI%CALC_MJD_START + 1
           UTC_END = UTC_END - 86400.0D0
         ELSE
           MJD_END = DFI%CALC_MJD_START
      END IF
      TAI_BEG = UTC_BEG - DFI%UTC_MTAI
      TAI_END = UTC_END - DFI%UTC_MTAI
!
      STR = MJDSEC_TO_DATE ( DFI%CALC_MJD_START, DFI%CALC_UTC_START + 1.D-6, IER )
      NO = 0
      NO = NO + 1 ;
      OUT(NO) = 'CALC SERVER:        NONE'
      NO = NO + 1 ;
      OUT(NO) = 'CALC PROGRAM:       difxvtd'
      NO = NO + 1 ;
      CALL EXWORD ( VTD__LABEL, MIND, LIND, IND, ' ', IER )
      OUT(NO) = 'CALC VERSION:       '//VTD__LABEL(IND(1,2):IND(2,2))
      NO = NO + 1 ;
      OUT(NO) = 'START YEAR:         '//STR(1:4)
      NO = NO + 1 ;
      OUT(NO) = 'START MONTH:        '//STR(6:7)
      NO = NO + 1 ;
      OUT(NO) = 'START DAY:          '//STR(9:10)
      NO = NO + 1 ;
      OUT(NO) = 'START HOUR:         '//STR(12:13)
      NO = NO + 1 ;
      OUT(NO) = 'START MINUTE:       '//STR(15:16)
      NO = NO + 1 ;
      OUT(NO) = 'START SECOND:       '//STR(18:19)
      NO = NO + 1 ;
      CALL CLRCH ( STR )
      CALL INCH  ( DFI__DEG_POL, STR )
      OUT(NO) = 'POLYNOMIAL ORDER:   '//STR
      NO = NO + 1 ;
      CALL CLRCH ( STR )
      CALL INCH  ( NINT(DFI__DUR_POL), STR )
      OUT(NO) = 'INTERVAL (SECS):    '//TRIM(STR)
      NO = NO + 1 ;
      OUT(NO) = 'ABERRATION CORR:    EXACT'
      NO = NO + 1 ;
      CALL CLRCH ( STR )
      CALL INCH  ( DFI%N_STA, STR )
      OUT(NO) = 'NUM TELESCOPES:     '//TRIM(STR)
!
      DO 410 J1=1,DFI%N_STA
         VTD%STA(J1)%MOUNT_TYPE     = DFI%MOUNT(J1)
         VTD%STA(J1)%SHORT_NAME     = DFI%SHR_STA_NAM(J1)
         VTD%STA(J1)%IVS_NAME       = '????????'
         VTD%STA(J1)%AXIS_OFFSET    = DFI%AXIS_OFF(J1)
         VTD%STA(J1)%COO_TRS(1:3,1) = DFI%STA_POS(1:3,J1)
         VTD%STA(J1)%VEL_TRS(1:3)   = 0.0D0
         DO 420 J2=1,NBUF
            IF ( BUF(J2)(1:1) .EQ. '$' ) GOTO 420
            IF ( I_FMT .EQ. 1   .AND.   BUF(J2)(1:8) .EQ. 'STA_GCX:' ) THEN
                 READ ( UNIT=BUF(J2)(31:45),  FMT='(F15.2)', IOSTAT=IOS ) STA_POS(1)
                 READ ( UNIT=BUF(J2)(65:79),  FMT='(F15.2)', IOSTAT=IOS ) STA_POS(2)
                 READ ( UNIT=BUF(J2)(99:113), FMT='(F15.2)', IOSTAT=IOS ) STA_POS(3)
                 STA_POS = 0.001D0 * STA_POS
                 STA_NAM = BUF(J1)(11:18) 
                 CALL VTD_NAME_REPAIR ( STA_NAM )
                 DIST = DSQRT ( (STA_POS(1) - VTD%STA(J1)%COO_TRS(1,1))**2 + &
     &                          (STA_POS(2) - VTD%STA(J1)%COO_TRS(2,1))**2 + &
     &                          (STA_POS(3) - VTD%STA(J1)%COO_TRS(3,1))**2   )
                 IF ( DIST < DIST_MIN ) THEN
                      VTD%STA(J1)%IVS_NAME = STA_NAM
                      DFI%IVS_STA_NAM(J1)  = STA_NAM
                 END IF
              ELSE IF ( I_FMT .EQ. 2  .AND.  BUF(J2)(5:5) .NE. ' ' ) THEN
                 READ ( UNIT=BUF(J2)(16:27), FMT='(F12.3)', IOSTAT=IOS ) STA_POS(1)
                 READ ( UNIT=BUF(J2)(32:43), FMT='(F12.3)', IOSTAT=IOS ) STA_POS(2)
                 READ ( UNIT=BUF(J2)(48:59), FMT='(F12.3)', IOSTAT=IOS ) STA_POS(3)
                 STA_NAM = BUF(J2)(5:12)
                 CALL VTD_NAME_REPAIR ( STA_NAM )
                 DIST = DSQRT ( (STA_POS(1) - VTD%STA(J1)%COO_TRS(1,1))**2 + &
     &                          (STA_POS(2) - VTD%STA(J1)%COO_TRS(2,1))**2 + &
     &                          (STA_POS(3) - VTD%STA(J1)%COO_TRS(3,1))**2   )
                 IF ( DIST < DIST_MIN ) THEN
                      VTD%STA(J1)%IVS_NAME = STA_NAM
                      DFI%IVS_STA_NAM(J1)  = STA_NAM
                 END IF
            END IF
 420     CONTINUE 
         IF ( VTD%STA(J1)%IVS_NAME == '????????' ) THEN
              IF ( VTD%CONF%FL_WARN ) THEN
                   WRITE ( 6, '(A)' ) 'DIFX_VTD_COMP_THEO -- did not find positions of '//TRIM(DFI%SHR_STA_NAM(J1))// &
     &                                ' station in '//TRIM(VTD%CONF%FINAM_STACOO)
              END IF
              DFI%IVS_STA_NAM(J1) = DFI%SHR_STA_NAM(J1) 
         END IF
         VTD%STA(J1)%N_ECC          = 1
         VTD%STA(J1)%ECC_MJD_BEG(1) = MJD_BEG
         VTD%STA(J1)%ECC_MJD_END(1) = MJD_BEG + 10
         VTD%STA(J1)%ECC_TAI_BEG(1) = 0.0
         VTD%STA(J1)%ECC_TAI_END(1) = 0.0
         VTD%STA(J1)%ECC_TRS(1:3,1) = 0.0D0
         VTD%STA(J1)%N_EPC          = 1
         VTD%STA(J1)%MJD_REF        = MJD_BEG
         VTD%STA(J1)%TAI_REF        = 0.0
         VTD%STA(J1)%MJD_EPC        = MJD_BEG
         VTD%STA(J1)%TAI_EPC        = 0.0
         VTD%STA(J1)%STA_TYP        = VTD__GR
!
         NO = NO + 1
         CALL CLRCH ( STR )
         CALL INCH  ( J1-1, STR )
         OUT(NO) = 'TELESCOPE '//TRIM(STR)//' NAME:'
!!         OUT(NO)(21:) = DFI%SHR_STA_NAM(J1)
         OUT(NO)(21:) = VTD%STA(J1)%IVS_NAME
 410  CONTINUE 
!
      DFI%N_STA = DFI%N_STA + 1
      VTD%STA(DFI%N_STA)            = VTD%STA(DFI%N_STA-1)
      VTD%STA(DFI%N_STA)%SHORT_NAME = '00'
      VTD%STA(DFI%N_STA)%IVS_NAME   = 'GEOCENTR'
      VTD%STA(DFI%N_STA)%MOUNT_TYPE     = 'AZEL'
      VTD%STA(DFI%N_STA)%AXIS_OFFSET    = 0.0
      VTD%STA(DFI%N_STA)%COO_TRS(1:3,1) = 0.0D0
      VTD%STA(DFI%N_STA)%VEL_TRS(1:3)   = 0.0D0
      VTD%STA(DFI%N_STA)%STA_TYP        = VTD__GC
      VTD%L_STA = DFI%N_STA
      DFI%IVS_STA_NAM(DFI%N_STA)    = 'GEOCENTR'
!
      DO 430 J3=1,DFI%N_SOU
         VTD%SOU(J3)%IVS_NAME   = DFI%SOU_NAM(J3)
         VTD%SOU(J3)%J2000_NAME = DFI%SOU_NAM(J3)
         VTD%SOU(J3)%ALPHA      = DFI%SOU_COO(1,J3)
         VTD%SOU(J3)%DELTA      = DFI%SOU_COO(2,J3)
         VTD%SOU(J3)%S_CRS(1)   = DCOS(VTD%SOU(J3)%ALPHA)* DCOS(VTD%SOU(J3)%DELTA)
         VTD%SOU(J3)%S_CRS(2)   = DSIN(VTD%SOU(J3)%ALPHA)* DCOS(VTD%SOU(J3)%DELTA)
         VTD%SOU(J3)%S_CRS(3)   = DSIN(VTD%SOU(J3)%DELTA)
         VTD%SOU(J3)%MJD_REF    = MJD_BEG
         VTD%SOU(J3)%TAI_REF    = 0.0
         IF ( VTD%CONF%DIR_NZO == 'NONE' ) THEN
              VTD%SOU(J3)%OBJ_TYPE   = VTD__MG
            ELSE
              VTD%SOU(J3)%OBJ_TYPE = VTD__ES ! currently only support all NZO
         ENDIF
 430  CONTINUE 
      VTD%L_SOU = DFI%N_SOU
!
      VTD%UEOP%NP = DFI%N_EOP
      DO 440 J4=1,DFI%N_EOP
         TAI_SEC = DFI%UTC_EOP(J4) - DFI%UTC_MTAI + (DFI%MJD_EOP(J4) - J2000__MJD)*86400.0D0
         VTD%UEOP%TIM(J4) = TAI_SEC - 43200.0D0
         VTD%UEOP%VAL(J4,UEOP__XPL)       = DFI%EOP(1,J4)
         VTD%UEOP%VAL(J4,UEOP__YPL)       = DFI%EOP(2,J4)
         VTD%UEOP%VAL(J4,UEOP__UT1_M_TAI) = DFI%EOP(3,J4)
 440  CONTINUE 
!
! --- Compute interpolating coefficients for each components: X_pole, Y_pole
! --- and UT1
!
      DO 450 J5=1,3
         CALL ERR_PASS ( IUER, IER )
         CALL MAKE_SPLINE ( 3, VTD%UEOP%NP, VTD%UEOP%TIM, VTD%UEOP%VAL(1,J5), &
     &                      D1, DN, VTD%UEOP%SPL(1,J5), WORK, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5213, IUER, 'DIFX_COMP_THEO', 'Error in an attempt '// &
     &            'to compute interpolating spline' )
              RETURN
         END IF
 450  CONTINUE
      VTD%UEOP%STATUS = UEOP__LOADED
!
      IF ( VTD%CONF%DIR_NZO == 'NONE' ) THEN
!
!--------- This forces VTD to read source coordinates and EOP outside DiFX
!--------- Note that this will change the delay model if coords/EOP different
!
          VTD%STATUS_SOU = VTD__BYPS
      ENDIF
      VTD%STATUS_STA = VTD__BYPS
      VTD%STATUS_EOP = VTD__BYPS
!
      CALL ERR_PASS  ( IUER, IER )
      CALL VTD_LOAD  ( VTD, DFI%N_STA, DFI%IVS_STA_NAM, DFI%N_SOU, DFI%SOU_NAM, &
     &                 MJD_BEG, TAI_BEG, MJD_END, TAI_END, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5214, IUER, 'DIFX_COMP_THEO', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           RETURN
      END IF
!
      NO = NO + 1
      OUT(NO) = 'NUM SCANS:         '
      CALL CLRCH ( STR )
      CALL INCH  ( DFI%N_SCA, STR )
      OUT(NO)(21:) = STR
!
      DO 460 J6=1,DFI%N_SCA
         OBS_TYP%PLRZ    = 'RR'     
         OBS_TYP%FRQ_REF(1) = 1.57542D9
         OBS_TYP%FRQ_REF(2) = 0.0D0
         OBS_TYP%N_BND      = 1
         OBS_TYP%DELAY_TYPE = VTD__PH__DTP
         OBS_TYP%FRQ_ION_EFF(1) = 5.0D0
         OBS_TYP%FRQ_ION_EFF(2) = 0.0D0
         OBS_TYP%EXP_NAME   = 'Test_01'
         OBS_TYP%SCAN_NAME  = DFI%SCAN_ID(J6)
         OBS_TYP%STATUS     = VTD__BND 
!
         UTC_BEG = DFI__DUR_POL * ( INT( (DFI%CALC_UTC_START + DFI%SCAN_START_OFFSET(J6))/DFI__DUR_POL ))
         UTC_END = DFI__DUR_POL * ( INT( (DFI%CALC_UTC_START + DFI%SCAN_START_OFFSET(J6) + DFI%SCAN_DUR(J6))/DFI__DUR_POL) + 1)
         IF ( UTC_END - UTC_BEG  < 0.500001D0*DFI__DUR_POL ) UTC_END = UTC_END + DFI__DUR_POL
         N_EPC = NINT(UTC_END - UTC_BEG)/DFI__DUR_POL + 1
!
         CALL CLRCH ( STR )
         CALL INCH  ( J6-1, STR )
         NO = NO + 1;
         WRITE ( UNIT=OUT(NO), FMT=110 ) 'SCAN '//TRIM(STR)//' POINTING SRC:', DFI%SOU_NAM(DFI%SCAN_IND_SOU(J6))
         NO = NO + 1;
         WRITE ( UNIT=OUT(NO), FMT=110 ) 'SCAN '//TRIM(STR)// ' NUM PHS CTRS:1'
         NO = NO + 1;
         WRITE ( UNIT=OUT(NO), FMT=110 ) 'SCAN '//TRIM(STR)//' PHS CTR 0 SRC:', DFI%SOU_NAM(DFI%SCAN_IND_SOU(J6))
         NO = NO + 1;
         WRITE ( UNIT=OUT(NO), FMT=120 ) 'SCAN '//TRIM(STR)//' NUM POLY:    ', N_EPC
         CALL CHASHL ( OUT(NO)(21:) )
 110     FORMAT ( A,A )
 120     FORMAT ( A,I3 )
!
         DO 470 J7=1,N_EPC
            CALL CLRCH ( STR )
            CALL CLRCH ( STR1 )
            CALL CLRCH ( STR2 )
            CALL INCH  ( J6-1, STR )
            CALL INCH  ( J7-1, STR1 )
            MJD_SCA = DFI%CALC_MJD_START
            UTC_SCA = UTC_BEG + (J7-1)*DFI__DUR_POL 
            IF ( UTC_SCA > 86400.0D0 ) THEN
                 MJD_SCA = MJD_SCA + 1
                 UTC_SCA = UTC_SCA - 86400.0D0
            END IF
            CALL INCH  ( MJD_SCA, STR2 )
            NO = NO + 1
            OUT(NO) = 'SCAN '//TRIM(STR)//' POLY '//TRIM(STR1)//' MJD: '
            OUT(NO)(21:) = STR2
!
            CALL CLRCH ( STR2 )
            CALL INCH  ( NINT(UTC_SCA), STR2 )
            NO = NO + 1
            OUT(NO) = 'SCAN '//TRIM(STR)//' POLY '//TRIM(STR1)//' SEC: '
            OUT(NO)(21:) = STR2
!
            NO_SRC_1ST = NO
            DO 480 J8=1,DFI%N_STA-1
               DO 490 J9=1,NS_EPC
                  MJD_EPC = DFI%CALC_MJD_START
                  TAI_EPC = UTC_BEG + (J7-1)*DFI__DUR_POL + (J9-1-DFI__DEG_POL/2.0D0)*DFI__DUR_POL/DFI__DEG_POL - DFI%UTC_MTAI
                  IF ( TAI_EPC < 0.0D0 ) THEN
                       MJD_EPC = MJD_EPC - 1
                       TAI_EPC = TAI_EPC + 86400.0D0
                     ELSE IF ( TAI_EPC > 86400.0D0 ) THEN
                       MJD_EPC = MJD_EPC + 1
                       TAI_EPC = TAI_EPC - 86400.0D0
                  END IF
!
                  CALL ERR_PASS  ( IUER, IER )
                  CALL VTD_DELAY ( DFI%SOU_NAM(DFI%SCAN_IND_SOU(J6)), 'GEOCENTR', &
     &                             VTD%STA(J8)%IVS_NAME, MJD_EPC, TAI_EPC, OBS_TYP, VTD, &
     &                             DELAY, RATE, DER_DEL, DER_RAT, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 5215, IUER, 'DIFX_COMP_THEO', 'Error in '// &
     &                     'computing interferometric path delay' )
                       RETURN
                  END IF
                  TIM_ARR(J9) = (J9-1-DFI__DEG_POL/2.0D0)*DFI__DUR_POL/DFI__DEG_POL
                  DEL_ARR(J9,MC__DEL) = DELAY/UNSCL_CMP(MC__DEL)
                  DEL_ARR(J9,MC__DRY) = DER_DEL(VTD__TROP2)/UNSCL_CMP(MC__DRY)
                  DEL_ARR(J9,MC__WET) = 0.0D0
                  DEL_ARR(J9,MC__AZ)  = DER_DEL(VTD__AZIM2)/UNSCL_CMP(MC__AZ)
                  DEL_ARR(J9,MC__EL)  = DER_DEL(VTD__ELEV2)/UNSCL_CMP(MC__EL)
                  DEL_ARR(J9,MC__U)   = DER_DEL(VTD__UVX)/UNSCL_CMP(MC__U)
                  DEL_ARR(J9,MC__V)   = DER_DEL(VTD__UVY)/UNSCL_CMP(MC__V)
                  DEL_ARR(J9,MC__W)   = DER_DEL(VTD__UVW)/UNSCL_CMP(MC__W)
 490           CONTINUE 
!
               CALL CLRCH ( STR1 )
               CALL CLRCH ( STR2 )
!?               CALL INCH  ( J6-1, STR1 )
               CALL INCH  ( 0, STR1 )
               CALL INCH  ( J8-1, STR2 )
               DO 4100 J10=1,M_CMP
                  CALL ERR_PASS   ( IUER, IER )
                  CALL CHCR  ( NS_EPC, TIM_ARR(1), DFI__DUR_POL, TIM_ARR, DEL_ARR(1,J10), COEF_ARR )
                  CALL CHMAP ( NS_EPC, TIM_ARR(1), DFI__DUR_POL, COEF_ARR, POLY_COEF(0,J10), IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 5216, IUER, 'DIFX_COMP_THEO', 'Error in '// &
     &                     'computing the interpolational polynomial' )
                       RETURN
                  END IF
                  NO = NO + 1
                  CALL CLRCH ( OUT(NO) )
                  OUT(NO) = 'SRC '//TRIM(STR1)//' ANT '//TRIM(STR2)//' '//DELAY_COMP(J10)
!!                  WRITE ( UNIT=OUT(NO)(26:), FMT='(6(1PD22.15,2X))' ) POLY_COEF(0:DFI__DEG_POL,J10)
                  WRITE ( UNIT=STR, FMT='(6(1PD22.15,A))' ) (POLY_COEF(N1,J10), CHAR(9), N1=0,DFI__DEG_POL)
                  DO 4110 J11=0,DFI__DEG_POL
                     IP = INDEX ( STR, 'D' ) 
                     IF ( IP > 1 ) THEN
                          STR = STR(1:IP-1)//'e'//STR(IP+1:)
                     END IF
 4110             CONTINUE 
                  IF ( ILEN(OUT(NO)) < 21 ) THEN
                       OUT(NO)(21:) = STR(1:I_LEN(STR)-1)
                     ELSE
                       OUT(NO)(ILEN(OUT(NO))+1:) = STR(1:I_LEN(STR)-1)
                  END IF                  
 4100          CONTINUE 
 480        CONTINUE 
!
            DO 4120 J12=NO_SRC_1ST+1,NO
               NO = NO + 1
               OUT(NO) = OUT(J12)(1:4)//'1'//OUT(J12)(6:)
 4120       CONTINUE 
 470     CONTINUE 
 460  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NO, OUT, DFI%IM_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5217, IUER, 'DIFX_COMP_THEO', 'Error in attempt '// &
     &         'to write the interferometric model file '//DFI%IM_FILE )
           RETURN 
      END IF 
!
      CALL ERR_PASS  ( IUER, IER )
      CALL VTD_QUIT ( VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5218, IUER, 'DIFX_COMP_THEO', 'Error in quitting VTD' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  DIFX_VTD_COMP_THEO  !#!#
