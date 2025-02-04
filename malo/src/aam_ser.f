      PROGRAM    AAM_SER_LAUNCH
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
      CALL AAM_SER()
      END  PROGRAM  AAM_SER_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE AAM_SER()
! ************************************************************************
! *                                                                      *
! *   Program AAM_SER generates a plot of the time series of the         *
! *   atmospheric angular momentum.                                      *
! *                                                                      *
! *  ### 06-AUG-2015    AAM_SER    v1.0 (c)  L. Petrov  12-AUG-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'diagi.i'
      TYPE     ( DIAGI_STRU ) ::  DIAGI_S
      CHARACTER  DIR_AAM*128, COMP*3, FILOUT*128, DATE_BEG*19, DATE_END*19, &
     &           FILNAM*128, DATA_SOURCE*128, DATA_TITLE*128, STR*128, &
     &           STR1*128, STR2*128
      CHARACTER  FIL_AAM(MALO__FIL)*128, DATE_FIL*19, EXT*4, EXO*8, &
     &           TIT*128, UNITS*128
      INTEGER*4  LEV, L_AAM
      REAL*8     AAM(MALO__FIL), TIM_BEG, TIM_END, TIM_FIL, TAI_MOM, &
     &           IMOM_IB(3), IMOM_NOIB(3), HMOM(3), EXF_IB(3), EXF_NOIB(3), &
     &           TIM(MALO__FIL), AAM_VAL(MALO__FIL,15), PLO_VAL(MALO__FIL), &
     &           TIM_ARR(MALO__FIL), VAL_MEAN
      CHARACTER    ASE__LABEL*38
      PARAMETER  ( ASE__LABEL = 'AAM_SER_INTRP   Version of 2015.11.30' )
      INTEGER*8  DIR_DESC(16)
      INTEGER*4  MJD_BEG, MJD_END, MJD_FIL, MJD_FIL_BEG, MJD_FIL_END, &
     &           MJD_MOM, J1, J2, J3, IL, IP, IS, L_FIL, LUN, IDAY, IUER
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3
      INTEGER*4  DIAGI_LEN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, GET_UNIT, LINDEX
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR, GET_FILE_FROM_DIR 
      REAL*8,    EXTERNAL :: FSPL8 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: aam_ser aam_dir comp date_beg date_end [filout]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, DIR_AAM )
           CALL GETARG ( 2, COMP )
           CALL GETARG ( 3, DATE_BEG )
           CALL GETARG ( 4, DATE_END  )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, FILOUT )
              ELSE
                FILOUT = '/XW'
           END IF
      END IF      
      EXT = '.txt'
!
      IF ( DATE_BEG == 'begin' ) THEN
           MJD_BEG = 40000
           TIM_BEG = 0.0
         ELSE
           IUER = -1
           CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TIM_BEG, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5101, IUER, 'AAM_SER', 'Failure in '// &
     &              'parsing begin date '//DATE_BEG )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( DATE_END == 'end' ) THEN
           MJD_END = 80000
           TIM_END = 0.0
         ELSE
           IUER = -1
           CALL DATE_TO_TIME ( DATE_END, MJD_END, TIM_END, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5102, IUER, 'AAM_SER', 'Failure in '// &
     &              'parsing begin date '//DATE_END )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( FILOUT == '/XW' ) THEN
           CONTINUE 
           EXO = '.xw'
         ELSE
           IL = ILEN(FILOUT)
           IP = LINDEX ( FILOUT, '.' ) 
           IF ( IP == IL .OR. IP < 1 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5103, IUER, 'AAM_SER', 'No extension of '// &
     &              'the output file '//FILOUT(1:I_LEN(FILOUT))//' was found' )
                CALL EXIT ( 1 )
           END IF
           EXO = FILOUT(IP:IL)
           IF ( EXO == '.txt' ) THEN
                CONTINUE 
              ELSE IF ( EXO == '.ps' ) THEN
                CONTINUE 
              ELSE IF ( EXO == '.gif' ) THEN
                CONTINUE 
              ELSE
                IUER = -1
                CALL ERR_LOG ( 5104, IUER, 'AAM_SER', 'Unsupported extension '// &
     &              'of the output file '//FILOUT(1:I_LEN(FILOUT))//' : '// &
     &               EXO(1:I_LEN(EXO))//' while one of .txt, .ps., .gif or '// &
     &              'filename /XW were expected' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      LEV = 0
      L_FIL = 0
      DO 410 J1=1,16*MALO__FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_AAM, FILNAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 5105, IUER, 'AAM_SER', 'Error in '// &
     &            'reading input directory '// &
     &             DIR_AAM(1:I_LEN(DIR_AAM))//'  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
!
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, EXT ) .LE. 0 ) GOTO 410
         IF ( INDEX ( FILNAM, '#' ) .GT. 0 ) GOTO 410
         IF ( INDEX ( FILNAM, '+' ) .GT. 0 ) GOTO 410
         IF ( INDEX ( FILNAM, EXT//'~' ) .GT. 0 ) GOTO 410
         IL = ILEN(FILNAM)
         IF ( IL < 17 ) GOTO 410
!
         DATE_FIL = FILNAM(IL-16:IL-13)//'.'// &
     &              FILNAM(IL-12:IL-11)//'.'// &
     &              FILNAM(IL-10:IL-9)//'_'// &
     &              FILNAM(IL-7:IL-6)//':'// &
     &              FILNAM(IL-5:IL-4)//':'//'00'
         IUER = -1
         CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 5106, IUER, 'AAM_SER', 'Failure in '// &
     &            'extracting the data from the AAM file '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( (MJD_FIL*86400.0D0 + TIM_FIL) < (MJD_BEG*86400.0D0 + TIM_BEG) ) GOTO 410
         IF ( (MJD_FIL*86400.0D0 + TIM_FIL) > (MJD_END*86400.0D0 + TIM_END) ) GOTO 410
!
         L_FIL = L_FIL + 1
         IF ( L_FIL > MALO__FIL )  THEN
              CALL CLRCH ( STR )
              CALL INCH  ( MALO__FIL, STR )
              IUER = -1
              CALL ERR_LOG ( 5107, IUER, 'AAM_SER', 'Too many '// &
     &            'files in directory '//DIR_AAM(1:I_LEN(DIR_AAM))// &
     &             ' -- more than '//STR )
              CALL EXIT ( 1 )
         END IF
         CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER )
         FIL_AAM(L_FIL) = FILNAM 
 410  CONTINUE 
 810  CONTINUE 
!
      IF ( L_FIL == 0 ) THEN
           CALL ERR_LOG ( 5108, IUER, 'AAM_SER', 'No AAM files '// &
     &         'within the specified range was found in the directory '// &
     &          DIR_AAM )
           CALL EXIT ( 1 )
      END IF
      CALL SORT_FAST_CH ( L_FIL, FIL_AAM )
      VAL_MEAN = 0.0D0
!
      DO 420 J2=1,L_FIL
         IUER = -1
         CALL PARSE_AAM ( FIL_AAM(J2), MJD_MOM, TAI_MOM, &
     &                    IMOM_NOIB, IMOM_IB, HMOM, EXF_NOIB, EXF_IB, &
     &                    DATA_SOURCE, DATA_TITLE, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 5109, IUER, 'AAM_SER', 'Error in parsing '// &
     &            'the AAM file '//FIL_AAM(J2) )
              CALL EXIT ( 1 )
         END IF
         IF ( J2 == 1 ) THEN
              MJD_BEG = MJD_MOM
              TIM_BEG = TAI_MOM 
         END IF
         TIM(J2) = ( ( MJD_MOM*86400.0D0 + TAI_MOM ) - &
     &               ( J2000__MJD*86400.0D0 + 43200.0D0 )   )/ &
     &               86400.D0/365.25D0 + 2000.0
         TIM_ARR(J2) = ( MJD_MOM*86400.0D0 + TAI_MOM ) - &
     &                 ( MJD_BEG*86400.0D0 + TIM_BEG ) 
!
         AAM_VAL(J2,1) = IMOM_NOIB(1)
         AAM_VAL(J2,2) = IMOM_NOIB(2)
         AAM_VAL(J2,3) = IMOM_NOIB(3)
!
         AAM_VAL(J2,4) = IMOM_IB(1)
         AAM_VAL(J2,5) = IMOM_IB(2)
         AAM_VAL(J2,6) = IMOM_IB(3)
!
         AAM_VAL(J2,7) = HMOM(1)
         AAM_VAL(J2,8) = HMOM(2)
         AAM_VAL(J2,9) = HMOM(3)
!
         AAM_VAL(J2,10) = EXF_NOIB(1)
         AAM_VAL(J2,11) = EXF_NOIB(2)
         AAM_VAL(J2,12) = EXF_NOIB(3)
!
         AAM_VAL(J2,13) = EXF_IB(1)
         AAM_VAL(J2,14) = EXF_IB(2)
         AAM_VAL(J2,15) = EXF_IB(3)
!
         IF ( COMP == 'I1N' ) THEN
              PLO_VAL(J2) = IMOM_NOIB(1)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'NoIB I31 matter term of the AAM from '//DATA_TITLE
              UNITS = 'kg/m**2'
           ELSE IF ( COMP == 'I2N' ) THEN
              PLO_VAL(J2) = IMOM_NOIB(2)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'NoIB I32 matter term of the AAM from '//DATA_TITLE
              UNITS = 'kg/m**2'
           ELSE IF ( COMP == 'I3N' ) THEN
              PLO_VAL(J2) = IMOM_NOIB(3)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'NoIB I33 matter term of the AAM from '//DATA_TITLE
              UNITS = 'kg/m**2'
           ELSE IF ( COMP == 'I1I' ) THEN
              PLO_VAL(J2) = IMOM_IB(1)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'IB I31 matter term of the AAM from '//DATA_TITLE
              UNITS = 'kg/m**2'
           ELSE IF ( COMP == 'I2I' ) THEN
              PLO_VAL(J2) = IMOM_IB(2)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'IB I32 matter term of the AAM from '//DATA_TITLE
              UNITS = 'kg/m**2'
           ELSE IF ( COMP == 'I3I' ) THEN
              PLO_VAL(J2) = IMOM_IB(3)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'IB I33 matter term of the AAM from '//DATA_TITLE
              UNITS = 'kg/m**2'
           ELSE IF ( COMP == 'H1' ) THEN
              PLO_VAL(J2) = HMOM(1)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'H1  motion term of the AAM from '//DATA_TITLE
              UNITS = 'kg/(s*m**2)'
           ELSE IF ( COMP == 'H2' ) THEN
              PLO_VAL(J2) = HMOM(2)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'H2  motion term of the AAM from '//DATA_TITLE
              UNITS = 'kg/(s*m**2)'
           ELSE IF ( COMP == 'H3' ) THEN
              PLO_VAL(J2) = HMOM(3)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'H3  motion term of the AAM from '//DATA_TITLE
              UNITS = 'kg/(s*m**2)'
           ELSE IF ( COMP == 'E1N' ) THEN
              PLO_VAL(J2)   = EXF_NOIB(1)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'Earth rotation excitation, NoIB comp 1 from '//DATA_TITLE
              UNITS = '1/s'
           ELSE IF ( COMP == 'E2N' ) THEN
              PLO_VAL(J2)   = EXF_NOIB(2)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'Earth rotation excitation, NoIB comp 2 from '//DATA_TITLE
              UNITS = '1/s'
           ELSE IF ( COMP == 'E3N' ) THEN
              PLO_VAL(J2)   = EXF_NOIB(3)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'Earth rotation excitation, NoIB comp 3 from '//DATA_TITLE
              UNITS = '1/s'
           ELSE IF ( COMP == 'E1I' ) THEN
              PLO_VAL(J2)   = EXF_IB(1)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'Earth rotation excitation, IB comp 1 from '//DATA_TITLE
              UNITS = '1/s'
           ELSE IF ( COMP == 'E2I' ) THEN
              PLO_VAL(J2)   = EXF_IB(2)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'Earth rotation excitation, IB comp 2 from '//DATA_TITLE
              UNITS = '1/s'
           ELSE IF ( COMP == 'E3I' ) THEN
              PLO_VAL(J2)   = EXF_IB(3)
              VAL_MEAN = VAL_MEAN + PLO_VAL(J2)
              TIT = 'Earth rotation excitation, IB comp 3 from '//DATA_TITLE
              UNITS = '1/s'
         END IF
 420  CONTINUE 
      VAL_MEAN = VAL_MEAN/L_FIL
      IF ( EXO == '.xw' ) THEN
           PLO_VAL = PLO_VAL - VAL_MEAN
           IF ( TIM(L_FIL) - TIM(1) < 0.1 ) THEN
                TIM = TIM - 2000.0
           END IF
           CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', TIT(1:I_LEN(TIT))//' '//UNITS )
           CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Year' )
           IUER = -1
           CALL DIAGI_1 ( L_FIL, TIM, PLO_VAL, IUER )
         ELSE IF ( EXO == '.gif' .OR. EXO == '.ps' ) THEN
           PLO_VAL = PLO_VAL - VAL_MEAN
!
! -------- Clear DIAGI_S object
!
           DIAGI_LEN = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
           CALL NOUT ( DIAGI_LEN, DIAGI_S )
!
! -------- Setting defaults values of the plotting parameters
!
           IUER = -1
           CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, STR, STR, &
     &                       ICL1, ICL2, ICL3, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5110, IUER, 'AAM_SER', 'Error in setting '// &
     &              'default values for the plot' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Setting up the values of the DIAGI internal data structure for the further
! -------- plotting
!
           IF ( EXO == '.gif' ) THEN
                DIAGI_S%IDEV  = 10
              ELSE IF ( EXO == '.ps' ) THEN
                DIAGI_S%IDEV  = 8
           END IF
!
           DIAGI_S%NCLR      = 1
           DIAGI_S%NPOI(1)   = L_FIL
           DIAGI_S%ADR_X8(1) = LOC(TIM)
           DIAGI_S%ADR_Y8(1) = LOC(PLO_VAL)
           DIAGI_S%LER(1)    = .FALSE.
           DIAGI_S%ICOL(1)   = ICL1
           DIAGI_S%IBST(1)   = 0
           DIAGI_S%ILST(1)   = ILST
           DIAGI_S%IOST(1)   = IOST
           DIAGI_S%IPST(1)   = IPST
           DIAGI_S%IWST(1)   = IWST
           DIAGI_S%ICLR      = 1
           DIAGI_S%XMIN      = 1.0
           DIAGI_S%XMAX      = 0.0
           DIAGI_S%YMIN      = 1.0
           DIAGI_S%YMAX      = 0.0
           DIAGI_S%ZAG       = TIT(1:I_LEN(TIT))//' '//UNITS
           DIAGI_S%NAME      = FILOUT
           DIAGI_S%ARG_UNITS = 'Years'
           DIAGI_S%ITRM      = 0
           DIAGI_S%IBATCH    = 1
           DIAGI_S%STATUS    = DIA__DEF
!
! -------- Calling the main routine of DiaGI
!
           CALL DIAGI     ( DIAGI_S, IUER )
         ELSE IF ( EXO == '.txt' ) THEN
           STR  = MJDSEC_TO_DATE ( MJD_BEG, TIM_BEG, IUER )
           STR1 = MJDSEC_TO_DATE ( MJD_MOM, TAI_MOM, IUER )
           LUN = GET_UNIT()
           OPEN ( FILE=FILOUT, UNIT=LUN, STATUS='unknown', IOSTAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 5111, IUER, 'AAM_SER', 'Failure in an '// &
     &              'attempt to open output file '//FILOUT )
                CALL EXIT ( 1 )
           END IF
           WRITE ( LUN, '(A)' ) AAM_SER__LABEL
           WRITE ( LUN, '(A)' ) '#'
           WRITE ( LUN, '(A)' ) '# This table contains the atmospheric angular momentum,'
           WRITE ( LUN, '(A)' ) '# mass and motion term separately as well as the atmospheric'
           WRITE ( LUN, '(A)' ) '# excitation function of the Earth rotation.'
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A)' ) '# The atmospheric angular momentum was computed by integration'
           WRITE ( LUN, '(A)' ) '# of the 3D model of distribution of air density and wind speed'
           WRITE ( LUN, '(A)' ) '# derived from the output of the numerical weather model'
           WRITE ( LUN, '(A)' ) '# Mass term was computed using both an inverted barometer and '
           WRITE ( LUN, '(A)' ) '# non-inverted barometer hypotheses.'
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A)' ) '# The variant with the non-inverted barometer hypothesis invoked'
           WRITE ( LUN, '(A)' ) '# means that the change of the sea level height induced '
           WRITE ( LUN, '(A)' ) '# by atmospheric pressure change was ignored'
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A)' ) '# The variant with the inverted barometer hypothesis invoked'
           WRITE ( LUN, '(A)' ) '# means that the reaction of the sea level height induced'
           WRITE ( LUN, '(A)' ) '# by atmospheric pressure change complete counerbalances'
           WRITE ( LUN, '(A)' ) '# the atmospheric pressure and as a results the bottom pressure that'
           WRITE ( LUN, '(A)' ) '# is the some of the atmospheric pressure and the sea column pressure'
           WRITE ( LUN, '(A)' ) '# is constant. The mass term over the ocean was excluded from integration'
           WRITE ( LUN, '(A)' ) '# '
           WRITE ( LUN, '(A)' ) '# Therefore, the mass term with the inverted barometer hypothesis invoked'
           WRITE ( LUN, '(A)' ) '# is computed by integrarging only over the land. The mass term with the '
           WRITE ( LUN, '(A)' ) '# non-inverted barometer hypothesis invoked # is computed by integrarging'
           WRITE ( LUN, '(A)' ) '# over entire Earth that includes both land and sea.'
           WRITE ( LUN, '(A)' ) '#'
           WRITE ( LUN, '(A)' ) '# Format:'
           WRITE ( LUN, '(A)' ) '#'
           WRITE ( LUN, '(A)' ) '#  Column  1:   1:19   A19    Calendar date'
           WRITE ( LUN, '(A)' ) '#  Column  2:  21:25   I5     Calendar date, MJD part. Units: day'
           WRITE ( LUN, '(A)' ) '#  Column  3:  27:32   F7.1   Calendar date, TAI part. Units: sec'
           WRITE ( LUN, '(A)' ) '#  Column  4:  36:48   E13.6  Mass term   of the angular momentum I31. NoIB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  5:  50:62   E13.6  Mass term   of the angular momentum I32. NoIB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  6:  64:76   E13.6  Mass term   of the angular momentum I33. NoIB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  4:  80:92   E13.6  Mass term   of the angular momentum I31.   IB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  5:  94:106  E13.6  Mass term   of the angular momentum I32.   IB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  6: 108:120  E13.6  Mass term   of the angular momentum I33.   IB Units: kg/m^2'
           WRITE ( LUN, '(A)' ) '#  Column  7: 124:136  E13.6  Motion term of the angular momentum H1.       Units: kg/(s*m^2)'
           WRITE ( LUN, '(A)' ) '#  Column  8: 138:150  E13.6  Motion term of the angular momentum H2.       Units: kg/(s*m^2)'
           WRITE ( LUN, '(A)' ) '#  Column  9: 152:164  E13.6  Motion term of the angular momentum H3.       Units: kg/(s*m^2)'
           WRITE ( LUN, '(A)' ) '#  Column 10: 168:180  E13.6  Atmospheric Earth rotation excitation function, NoIB, component 1. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#  Column 11: 182:194  E13.6  Atmospheric Earth rotation excitation function, NoIB, component 2. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#  Column 12: 196:208  E13.6  Atmospheric Earth rotation excitation function, NoIB, component 3. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#  Column 13: 212:224  E13.6  Atmospheric Earth rotation excitation function,   IB, component 1. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#  Column 14: 226:238  E13.6  Atmospheric Earth rotation excitation function,   IB, component 2. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#  Column 15: 240:252  E13.6  Atmospheric Earth rotation excitation function,   IB, component 3. Units: 1/s'
           WRITE ( LUN, '(A)' ) '#'
           WRITE ( LUN, '(A)' ) '# Generated by: '//ASE__LABEL
           WRITE ( LUN, '(A)' ) '# Generated on: '//GET_CDATE()
           WRITE ( LUN, '(A)' ) '# Data source:  '//DATA_SOURCE(1:I_LEN(DATA_SOURCE))
           WRITE ( LUN, '(A)' ) '# Data title:   '//DATA_TITLE(1:I_LEN(DATA_TITLE))
           WRITE ( LUN,  130  ) '# Data start date: ', STR(1:23),  MJD_BEG, TIM_BEG
           WRITE ( LUN,  130  ) '# Data end date:   ', STR1(1:23), MJD_MOM, TAI_MOM
 130       FORMAT ( A, A, ' MJD: ', I5, ' UTC: ', F6.0 )
           WRITE ( LUN,  140  ) '# Number of epochs:        ', L_FIL
 140       FORMAT ( A, I7 )
           WRITE ( LUN, '(A)' ) '#                                                                                                                                                                                                                                                           '
           WRITE ( LUN, '(A)' ) '#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------'
           WRITE ( LUN, '(A)' ) '#  TIME TAG                         AAM MASS TERM, non-IB                       AAM MASS TERM, IB                           AAM MOTION TERM                             EXCITATION FUNCTION, non-IB                 EXCITATION FUNCTION, IB                 '
           WRITE ( LUN, '(A)' ) '#                                                                                                                                                                                                                                                           '
           WRITE ( LUN, '(A)' ) '#  Date             MJD   UTC       I31_noIB      I32_noIB      I33_noIB        I31_IB        I32_IB        I33_IB          H1            H2            H3              EF1_noIB      EF2_noIB      EF3_noIB        EF1_IB        EF2_IB        EF3_IB      '
           WRITE ( LUN, '(A)' ) '#                                   kg/m^2        kg/m^2        kg/m^2          kg/m^2        kg/m^2        kg/m^2          kg/(s*m^2)    kg/(s*m^2)    kg/(s*m^2)      1/s           1/s           1/s             1/s           1/s           1/s         '
           WRITE ( LUN, '(A)' ) '#                                                                                                                                                                                                                                                           '
!
           DO 430 J3=1,L_FIL
              MJD_MOM = MJD_BEG
              TAI_MOM = TIM_BEG + TIM_ARR(J3)
              IDAY = TAI_MOM/86400.0D0
              MJD_MOM = MJD_MOM + IDAY
              TAI_MOM = TAI_MOM - IDAY*86400.0D0
              STR  = MJDSEC_TO_DATE ( MJD_MOM, TAI_MOM, IUER )
              WRITE ( LUN, 150 ) STR(1:19), MJD_MOM, TAI_MOM, AAM_VAL(J3,1:15)
 150          FORMAT ( A, 1X, I5, 1X, F7.1, 1X, 5(3(1X,1PD13.6),2X) )
 430       CONTINUE 
!
           CLOSE ( UNIT=LUN )
      END IF
      END  SUBROUTINE  AAM_SER  !#!#
