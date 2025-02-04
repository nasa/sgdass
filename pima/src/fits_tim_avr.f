      SUBROUTINE FITS_TIM_AVR ( PIM, FILIN, TIM_AVR, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Rouitine performs time averaging of the calibrated visibility data *
! *   for the specific source over time.                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FILIN   ( CHARACTER ) -- name of the input file with calibrated      *
! *                          visibilities in VLBI FITS format.           *
! * TIM_AVR ( REAL*8    ) -- averaging time in seconds.                  *
! * FILOUT  ( CHARACTER ) -- name of the output file with calibrated     *
! *                          visibilities in VLBI FITS format.           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *                                                                      *
! *   PIM   ( PIMA__TYP ) -- Object with information related to          *
! *                          program PIMA.                               *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the case of errors. *
! *                                                                      *
! *  ### 19-DEC-2020  FITS_TIM_AVR  v2.1 (c)  L. Petrov 09-JAN-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( FITS_PRIM__STRU ), POINTER :: TABL_INP(:), TABL_AVR(:)
      INTEGER*4  IUER
!
      CHARACTER    FITS_TIM_AVR__LABEL*30
      PARAMETER  ( FITS_TIM_AVR__LABEL = 'FITS_TIM_AVR v 0.5  2020.12.19')
      CHARACTER  FILIN*(*), FILOUT*(*)
      REAL*8     TIM_AVR
!
      INTEGER*8  FPTR, DESC(PIM__MSTA)
      CHARACTER  STR*512, SOU_NAME*10, EXTNAME*8, &
     &           ANT_NAM_ARR(PIM__MSTA,PIM__MSUB)*8, &
     &           C_STA(PIM__MSTA)*8, &
     &           EXP_NAME*32, USER_NAME*128, USER_REALNAME*128, &
     &           USER_E_ADDRESS*128
      INTEGER*4  GC_INP, GC_AVR
      REAL*8     JD, FREQ_REF, VAL, JD_NEW, STA_COO(3,PIM__MSTA), &
     &           FREQ_OFF(PIM__MFRQ), FREQ_CHW(PIM__MFRQ), FREQ_TBW(PIM__MFRQ), &
     &           STA_COO_GLO(3,PIM__MSTA), STA_COO_OUT(3,PIM__MSTA), TIM, &
     &           TIM_BEG, TIM_DAYS, TIM_MDN, TIM_MEAN, TIM_DUR, &
     &           TIM_BAS_BEG(PIM__MSCA,PIM__MBAS), &
     &           TIM_BAS_END(PIM__MSCA,PIM__MBAS), &
     &           SUM_WEI(PIM__MFRQ,PIM__MPLR), SUM_SQR_WEI(2,PIM__MFRQ,PIM__MPLR), &
     &           RMS_WEI_SQR, WEI
      REAL*8       TIM_EPS
      PARAMETER  ( TIM_EPS = 1.D-3 )
      INTEGER*4    N_GRP, NUM_MIN_AP_SIG
      PARAMETER  ( N_GRP = 7 )
      PARAMETER  ( NUM_MIN_AP_SIG = 8 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, &
     &           J24, J25, J26, J27, J28, KP, IND_SCA, &
     &           K_STA(PIM__MSUB), L_STA, FT_STATUS, ANY_VAL, IP, KFRQ, KPOL, &
     &           IND_ANT_KEY(PIM__MSUB), IND_ANT_TAB(PIM__MSUB), &
     &           IND_STC_TAB(PIM__MSUB), IND_STC_KEY(PIM__MSUB), &
     &           FREQ_SDB(PIM__MFRQ), NAXIS2(PIM__MSUB), NAXIS3(PIM__MSUB), &
     &           IND, IS,  NHDTYPE, HDUTYPE, L_SUB, L_BAS, LIS_BAS(PIM__MBAS), &
     &           I_BAS, K_BAS(PIM__MBAS), IVAL, IND_AVR, NUM_SCA_BAS(PIM__MSCA), &
     &           REF_SCA_AVR(PIM__MSCA,PIM__MBAS), IER
      INTEGER*4, ALLOCATABLE :: NP_INP(:), NP_AVR(:), REF_INP_AVR(:), REF_INP_SCA(:)
      LOGICAL*1  FL_FOUND_SUB, FL_SHOW_NUM_FRQ, LEX
      INTEGER*4  IND_FRS_TAB, IND_FRS_KEY, IND_CHW_TAB, IND_CHW_KEY, &
     &           IND_TBW_TAB, IND_TBW_KEY, IND_SDB_TAB, IND_SDB_KEY 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, ADD_LIS, ADD_CLIST, LTM_DIF, IFIND_PL, UNLINK
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, GET_CDATE*19
#ifdef GNU
      INTEGER*4, EXTERNAL :: PIMA_COMPAR_TABL
#else
      INTEGER*2, EXTERNAL :: PIMA_COMPAR_TABL
#endif
!
! --- Put the input file name into PIM%FILE
!
      PIM%L_FIL = 1
      ALLOCATE ( PIM%FILE(PIM%L_FIL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%L_FIL*SIZEOF(PIM%FILE(1)), STR )
           CALL ERR_LOG ( 1911, IUER, 'FITS_TIM_AVR', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
      CALL NOUT ( SIZEOF(PIM%FILE(1)), PIM%FILE(1) )
      PIM%FILE(1)%NAME = FILIN
      PIM%FILE(1)%KEY  => NULL()
!
! --- Open input visibility file
!
      CALL FFITS_OPEN ( PIM%FILE(1)%NAME, PIM%FILE(1)%FITS_DESC, 'OLD', &
     &                  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1912, IUER, 'FITS_TIM_AVR', 'Error in an attempt '// &
     &         'to open FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
! --- Read the keys from the input file
!
      CALL ERR_PASS       ( IUER, IER )
      CALL FFITS_GET_KEYP ( PIM%FILE(1)%FITS_DESC, PIM__MHDR, -PIM__MKWD, &
     &                      PIM%FILE(1)%M_KWD, PIM%FILE(1)%L_HDR, &
     &                      PIM%FILE(1)%L_KWD, PIM%FILE(1)%KEY, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1913, IUER, 'FITS_TIM_AVR', 'Error in an attempt '// &
     &         'to get keys from FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
      GC_AVR = 0
      K_STA = 0
      L_STA = 0
      CALL CLRCH ( ANT_NAM_ARR )
      L_SUB = 0
!
      IND_ANT_TAB = 0
      IND_STC_TAB = 0
      IND_FRS_TAB = 0
      IND_CHW_TAB = 0
      IND_TBW_TAB = 0
      IND_SDB_TAB = 0
!
      IND_ANT_KEY = 0
      IND_STC_KEY = 0
      IND_FRS_KEY = 0
      IND_CHW_KEY = 0
      IND_TBW_KEY = 0
      IND_SDB_KEY = 0
!
! --- Extracrt the keys the input file
!
      DO 410 J1=1,PIM%FILE(1)%L_HDR
         DO 420 J2=1,PIM%FILE(1)%L_KWD(J1)
            IF ( PIM%FILE(1)%KEY(J2,J1) == 'OBJECT  =' ) THEN
                 SOU_NAME = PIM%FILE(1)%KEY(J2,J1)(12:21)
            END IF
            IF ( J1 == 1 .AND. PIM%FILE(1)%KEY(J2,J1)(1:9) == 'GCOUNT  =' ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2,J1)(23:30), FMT='(I8)' ) GC_INP
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:9) == 'NAXIS2  =' ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2,J1)(25:30), FMT='(I6)' ) NAXIS2(J1) 
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:9) == 'NAXIS3  =' ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2,J1)(25:30), FMT='(I6)' ) NAXIS3(J1) 
                 IF ( J1 == 1 ) KPOL = NAXIS3(J1)
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:20) == "CTYPE4  = 'FREQ    '" ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2+2,J1)(15:30), FMT='(F16.2)' ) FREQ_REF
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:9) == 'NO_IF   =' ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2,J1)(23:30), FMT='(I8)' ) KFRQ
            END IF
!
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:16) == 'GACO    =      1' ) THEN
                 PIM%FILE(1)%KEY(J2,J1) = 'GACO    =      1                         / GAIN CORRECTION USED: 0 NO, 1 YES'
            END IF
!
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:20) == "PTYPE5  = 'DATE    '" ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2+2,J1)(15:30), FMT='(F16.7)' ) JD
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:20) == "CTYPE4  = 'FREQ    '" ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2+2,J1)(15:30), FMT='(F16.7)' ) VAL
                 IF ( VAL .NE. FREQ_REF ) THEN
                      CALL ERR_LOG ( 1914, IUER, 'FITS_TIM_AVR', &
     &                    'An attempt to merge UVA-files for data '// &
     &                    ' with different reference frequencies' )
                      RETURN
                 END IF
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:8) == "EXTNAME " ) THEN
                 EXTNAME = PIM%FILE(1)%KEY(J2,J1)(12:19) 
            END IF             
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:20) == "TTYPE1  = 'ANNAME  '" .AND. &
     &           EXTNAME == 'AIPS AN ' ) THEN
                 L_SUB = L_SUB + 1
                 K_STA(L_SUB) = NAXIS2(J1)
                 IND_ANT_TAB(L_SUB) = J1
                 IND_ANT_KEY(L_SUB) = J2
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:20) == "TTYPE2  = 'STABXYZ '" ) THEN
                 IND_STC_TAB(L_SUB) = J1
                 IND_STC_KEY(L_SUB) = J2
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:9) == 'NO_IF   =' ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2,J1)(23:30), FMT='(I8)' ) IVAL
                 IF ( FL_SHOW_NUM_FRQ ) THEN
                      WRITE ( 6, '(A,I4)' ) 'FITS_TIM_AVR File: '//TRIM(PIM%FILE(1)%NAME), IVAL
                      GOTO 420
                 END IF
                 IF ( IVAL .NE. KFRQ ) THEN
                      CALL ERR_LOG ( 1915, IUER, 'FITS_TIM_AVR', &
     &                    'An attempt to merge UVA-files for data '// &
     &                    ' with different number of frequencies. '//  &
     &                    'Offending file: '//PIM%FILE(1)%NAME )
                      RETURN
                 END IF
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:9) == 'OBSERVER=' ) THEN
                 EXP_NAME = PIM%FILE(1)%KEY(J2,J1)(12:)
                 IP = INDEX ( EXP_NAME, "'" )
                 IF ( IP > 0 ) CALL CLRCH ( EXP_NAME(IP:) )
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(11:20) == "'IF FREQ '" ) THEN
                 IND_FRS_TAB = J1
                 IND_FRS_KEY = J2
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(11:20) == "'CH WIDTH'" ) THEN
                 IND_CHW_TAB = J1
                 IND_CHW_KEY = J2
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(11:27) == "'TOTAL BANDWIDTH'" ) THEN
                 IND_TBW_TAB = J1
                 IND_TBW_KEY = J2
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(11:27) == "'SIDEBAND'" ) THEN
                 IND_SDB_TAB = J1
                 IND_SDB_KEY = J2
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
! --- Varios checks
!
      IF ( L_SUB == 0 ) THEN
           CALL ERR_LOG ( 1916, IUER, 'FITS_TIM_AVR', 'No ANNAME keyword '// &
     &         'was found in FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      IF ( IND_STC_TAB(1) == 0 .OR. IND_STC_KEY(1) == 0 ) THEN
           CALL ERR_LOG ( 1917, IUER, 'FITS_TIM_AVR', 'No STABXYZ  keyword '// &
     &         'was found in FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      IF ( IND_FRS_TAB == 0 .OR. IND_FRS_KEY == 0 ) THEN
           CALL ERR_LOG ( 1918, IUER, 'FITS_TIM_AVR', 'No IF FREQ  keyword '// &
     &         'was found in FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      IF ( IND_CHW_TAB == 0 .OR. IND_CHW_KEY == 0 ) THEN
           CALL ERR_LOG ( 1919, IUER, 'FITS_TIM_AVR', 'No CH WIDTH keyword '// &
     &         'was found in FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      IF ( IND_TBW_TAB == 0 .OR. IND_TBW_KEY == 0 ) THEN
           CALL ERR_LOG ( 1920, IUER, 'FITS_TIM_AVR', 'No TOTAL BANDWIDTH '// &
     &         'keyword was found in FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      IF ( IND_SDB_TAB == 0 .OR. IND_SDB_KEY == 0 ) THEN
           CALL ERR_LOG ( 1921, IUER, 'FITS_TIM_AVR', 'No SIDEBAND '// &
     &         'keyword was found in FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
! --- Read inpupt data
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_FRS_TAB, 1, &
     &                   PIM%FILE(1)%KEY(IND_FRS_KEY,IND_FRS_TAB), &
     &                   KFRQ, FREQ_OFF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1922, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'getting bandwidth frequency array from the '// &
     &         'FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_CHW_TAB, 1, &
     &                   PIM%FILE(1)%KEY(IND_CHW_KEY,IND_CHW_TAB), &
     &                   KFRQ, FREQ_CHW, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1923, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'getting channel width array from the '// &
     &         'FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_TBW_TAB, 1, &
     &                   PIM%FILE(1)%KEY(IND_TBW_KEY,IND_TBW_TAB), &
     &                   KFRQ, FREQ_TBW, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1924, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'getting total bandwidth array from the '// &
     &         'FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_GETI4 ( PIM%FILE(1)%FITS_DESC, IND_SDB_TAB, 1, &
     &                   PIM%FILE(1)%KEY(IND_SDB_KEY,IND_SDB_TAB), &
     &                   KFRQ, FREQ_SDB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1925, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'getting sideband id array from the '// &
     &         'FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
!
! --- Allocate dynamic memory for temporary arrays
!
      ALLOCATE ( TABL_INP(GC_INP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1926, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'attempt to allocate dynamic memory for the data '// &
     &         'structure TABL' )
           RETURN
      END IF 
!
      ALLOCATE ( REF_INP_AVR(GC_INP), REF_INP_SCA(GC_INP), NP_INP(GC_INP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1926, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'attempt to allocate dynamic memory for the arrays '// &
     &         'REF_INP_AVR, REF_INP_SCA, NP_INP' )
           RETURN
      END IF 
!
! --- Position the file to the header where the data reside
!
      FT_STATUS = 0
      CALL FFMAHD ( %VAL(PIM%FILE(1)%FITS_DESC), %VAL(1), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1926, IUER, 'GET_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 1927, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'position to the first table fo the fits file '// &
     &          FILIN )
           RETURN
      END IF
      DO 430 J3=1,GC_INP
         ALLOCATE ( TABL_INP(J3)%GRP_ARR(N_GRP),  STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1928, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &            'attempt to allocate dynamic memory for '// &
     &           'an element GRP_ARR in the data structure TABL_INP' )
              RETURN 
         END IF
!
         ALLOCATE ( TABL_INP(J3)%UV_DATA(3,KFRQ,KPOL),  STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1929, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &            'attempt to allocate dynamic memory for '// &
     &           'an element GRP_ARR in the data structure TABL_INP' )
              RETURN 
         END IF
!
! ------ Read group parameters
!
         FT_STATUS = 0
         CALL FFGGPE ( %VAL(PIM%FILE(1)%FITS_DESC), %VAL(J3), %VAL(INT8(1)), &
     &                 %VAL(INT8(N_GRP)), %VAL(0.0), TABL_INP(J3)%GRP_ARR, ANY_VAL, &
     &                 FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 1929, IUER, 'FITS_TIM_AVR', FT_STATUS )
              CALL ERR_LOG ( 1930, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &            'attempt to read the auxiliary information from the '// &
     &            'input file '//PIM%FILE(1)%NAME  )
              RETURN 
         END IF
!
! ------ Read UV-data
!
         FT_STATUS = 0
         CALL FFGPVE ( %VAL(PIM%FILE(1)%FITS_DESC), %VAL(J3), %VAL(INT8(1)), &
     &                 %VAL(INT8(3*KFRQ*KPOL)), %VAL(0.0), TABL_INP(J3)%UV_DATA, &
     &                 ANY_VAL, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 1930, IUER, 'FITS_TIM_AVR', FT_STATUS )
              CALL ERR_LOG ( 1931, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &            'attempt to read the uv-data from the output input '// &
     &            'fits-file '//PIM%FILE(1)%NAME  )
              RETURN 
         END IF
 430  CONTINUE 
!
      L_BAS = 0
      DO 440 J4=1,GC_INP
         I_BAS = ADD_LIS ( PIM__MBAS, L_BAS, LIS_BAS, NINT(TABL_INP(J4)%GRP_ARR(4)), IER )
 440  CONTINUE 
!
      GC_AVR = 0
      NUM_SCA_BAS = 0
      REF_SCA_AVR = 0
      DO 450 J5=1,GC_INP
         I_BAS = IFIND_PL ( L_BAS, LIS_BAS, NINT(TABL_INP(J5)%GRP_ARR(4)), IER )
         TIM = (TABL_INP(J5)%GRP_ARR(5) + TABL_INP(J5)%GRP_ARR(6))*86400.0D0
         IF ( NUM_SCA_BAS(I_BAS) == 0 ) THEN
              GC_AVR = GC_AVR + 1
              NP_INP(GC_AVR) = 0
              NUM_SCA_BAS(I_BAS) = NUM_SCA_BAS(I_BAS) + 1
              TIM_BAS_BEG(NUM_SCA_BAS(I_BAS),I_BAS) = TIM
              TIM_BAS_END(NUM_SCA_BAS(I_BAS),I_BAS) = TIM
              REF_SCA_AVR(NUM_SCA_BAS(I_BAS),I_BAS) = GC_AVR
            ELSE
              IF ( (TIM - TIM_BAS_BEG(NUM_SCA_BAS(I_BAS),I_BAS) - TIM_EPS) > TIM_AVR ) THEN
                   GC_AVR = GC_AVR + 1
                   NP_INP(GC_AVR) = 0
                   NUM_SCA_BAS(I_BAS) = NUM_SCA_BAS(I_BAS) + 1
                   TIM_BAS_BEG(NUM_SCA_BAS(I_BAS),I_BAS) = TIM
                   TIM_BAS_END(NUM_SCA_BAS(I_BAS),I_BAS) = TIM
                   REF_SCA_AVR(NUM_SCA_BAS(I_BAS),I_BAS) = GC_AVR
              END IF
         END IF
         IND_SCA = 0
         DO 460 J6=NUM_SCA_BAS(I_BAS),1,-1
            IF ( TIM .GE. TIM_BAS_BEG(J6,I_BAS) - TIM_EPS ) THEN
                 IND_SCA = J6
                 TIM_BAS_END(J6,I_BAS) = TIM
                 GOTO 860
            END IF
 460     CONTINUE 
 860     CONTINUE 
!         
         REF_INP_AVR(J5) = REF_SCA_AVR(IND_SCA,I_BAS)
         REF_INP_SCA(J5) = IND_SCA
         NP_INP(REF_SCA_AVR(IND_SCA,I_BAS)) = NP_INP(REF_SCA_AVR(IND_SCA,I_BAS)) + 1
 450  CONTINUE 
!
      ALLOCATE ( NP_AVR(GC_AVR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1932, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'attempt to allocate dynamic memory for NP_AVR' )
           RETURN
      END IF
!
! --- Write random group parameters into the first table
!
      ALLOCATE ( TABL_AVR(GC_AVR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1932, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'attempt to allocate dynamic memory for the data '// &
     &         'structure TABL' )
           RETURN
      END IF 
!
      DO 4100 J10=1,GC_AVR
         ALLOCATE ( TABL_AVR(J10)%GRP_ARR(N_GRP),  STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1933, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &            'attempt to allocate dynamic memory for '// &
     &           'an element GRP_ARR in the data structure TABL' )
              RETURN 
         END IF
         ALLOCATE ( TABL_AVR(J10)%UV_DATA(3,KFRQ,KPOL),  STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1934, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &            'attempt to allocate dynamic memory for '// &
     &           'an element GRP_ARR in the data structure TABL' )
              RETURN 
         END IF
         TABL_AVR(J10)%GRP_ARR = 0.0
         TABL_AVR(J10)%UV_DATA = 0.0
 4100 CONTINUE 
!
! --- Averaging
!
      DO 4110 J11=1,GC_AVR
         TIM_MEAN = 0.0D0
         TIM_DUR  = 0.0D0
         TIM_BEG  = -1.0001D10
         TIM_DAYS = -1.0D0
         NP_AVR(J11) = 0
         SUM_WEI = 0.0D0
         SUM_SQR_WEI = 0.0D0
         DO 4120 J12=1,GC_INP
            IF ( REF_INP_AVR(J12) == J11 ) THEN
!
! -------------- The J12th sample of the input array corresponds 
! -------------- to the J11th sample of the output array
!
                 TABL_AVR(J11)%GRP_ARR(4) = TABL_INP(J12)%GRP_ARR(4)
                 TABL_AVR(J11)%GRP_ARR(1:3) = TABL_AVR(J11)%GRP_ARR(1:3) + &
     &                                        TABL_INP(J12)%GRP_ARR(1:3)
                 TIM = (TABL_INP(J12)%GRP_ARR(5) + TABL_INP(J12)%GRP_ARR(6))*86400.0D0
                 I_BAS = IFIND_PL ( L_BAS, LIS_BAS, NINT(TABL_INP(J12)%GRP_ARR(4)), IER )
                 IF ( TIM_BEG < -1.0D-10 ) THEN
                      TIM_DAYS = TABL_INP(J12)%GRP_ARR(5) 
                      TIM_MDN  = TABL_INP(J12)%GRP_ARR(6) 
                      TIM_BEG = TIM
                 END IF
                 IND_SCA = REF_INP_SCA(J12)
                 TIM_DUR  = TIM_BAS_END(IND_SCA,I_BAS) - TIM_BAS_BEG(IND_SCA,I_BAS) + TABL_INP(J12)%GRP_ARR(7)
                 TIM_MEAN = TIM_MEAN + TIM
!
                 DO 4130 J13=1,KPOL
                    DO 4140 J14=1,KFRQ
                       IF ( TABL_INP(J12)%UV_DATA(3,J14,J13) > 0.0D0 ) THEN
                            TABL_AVR(J11)%UV_DATA(3,J14,J13) = TABL_AVR(J11)%UV_DATA(3,J14,J13) + &
     &                                            1.0/TABL_INP(J12)%UV_DATA(3,J14,J13)
                            WEI = SQRT(TABL_INP(J12)%UV_DATA(3,J14,J13))
                          ELSE 
                            WEI = 1.0D0
                       END IF
                       TABL_AVR(J11)%UV_DATA(1,J14,J13) = TABL_AVR(J11)%UV_DATA(1,J14,J13) + &
     &                         WEI*TABL_INP(J12)%UV_DATA(1,J14,J13)
                       TABL_AVR(J11)%UV_DATA(2,J14,J13) = TABL_AVR(J11)%UV_DATA(2,J14,J13) + &
     &                         WEI*TABL_INP(J12)%UV_DATA(2,J14,J13)
                       SUM_SQR_WEI(1,J14,J13) = SUM_SQR_WEI(1,J14,J13) + WEI*TABL_INP(J12)%UV_DATA(1,J14,J13)**2
                       SUM_SQR_WEI(2,J14,J13) = SUM_SQR_WEI(2,J14,J13) + WEI*TABL_INP(J12)%UV_DATA(2,J14,J13)**2
                       SUM_WEI(J14,J13) = SUM_WEI(J14,J13) + WEI
 4140               CONTINUE 
 4130            CONTINUE 
                 NP_AVR(J11) = NP_AVR(J11) + 1
            END IF
 4120    CONTINUE 
         DO 4150 J15=1,KPOL
            DO 4160 J16=1,KFRQ
               IF ( TABL_AVR(J11)%UV_DATA(3,J16,J15) > 0.0D0 .AND. SUM_WEI(J16,J15) > 0.0D0 ) THEN
                    TABL_AVR(J11)%UV_DATA(1,J16,J15) = TABL_AVR(J11)%UV_DATA(1,J16,J15)/SUM_WEI(J16,J15)
                    TABL_AVR(J11)%UV_DATA(2,J16,J15) = TABL_AVR(J11)%UV_DATA(2,J16,J15)/SUM_WEI(J16,J15)
                    IF ( NP_AVR(J11) < NUM_MIN_AP_SIG ) THEN
                         TABL_AVR(J11)%UV_DATA(3,J16,J15) = 2*(NP_AVR(J11)-1)**2/TABL_AVR(J11)%UV_DATA(3,J16,J15)
                       ELSE
                         IF ( NP_AVR(J11) > 1 ) THEN
                              RMS_WEI_SQR = &
     &                          ( SUM_SQR_WEI(1,J16,J15) - TABL_AVR(J11)%UV_DATA(1,J16,J15)**2 + &
     &                            SUM_SQR_WEI(2,J16,J15) - TABL_AVR(J11)%UV_DATA(2,J16,J15)**2   )/2.0D0/ &
     &                            (NP_AVR(J11) - 1)
                              RMS_WEI_SQR = ( SUM_SQR_WEI(2,J16,J15) - TABL_AVR(J11)%UV_DATA(2,J16,J15)**2)/ &
     &                                      SUM_WEI(J16,J15)/(NP_AVR(J11) - 1)
                            ELSE
                              RMS_WEI_SQR = 1.0D0/SUM_WEI(J16,J5)
                          END IF
                          TABL_AVR(J11)%UV_DATA(3,J16,J15) = 1.D0/RMS_WEI_SQR
                     END IF
               END IF
 4160       CONTINUE 
 4150    CONTINUE 
!
         IF ( NP_AVR(J11) > 0  ) THEN
              TIM_MEAN = TIM_MEAN/NP_AVR(J11)
              TABL_AVR(J11)%GRP_ARR(5) = TIM_DAYS
              TABL_AVR(J11)%GRP_ARR(6) = TIM_MDN + (TIM_MEAN - TIM_BEG)/86400.D0
!!
              IF ( TABL_AVR(J11)%GRP_ARR(6) > 1.0D0 ) THEN
                   TABL_AVR(J11)%GRP_ARR(5) = TABL_INP(J11)%GRP_ARR(5) + 1.0D0
                   TABL_AVR(J11)%GRP_ARR(6) = TABL_INP(J11)%GRP_ARR(6) - 1.0D0
              END IF
              TABL_AVR(J11)%GRP_ARR(1:3) = TABL_AVR(J11)%GRP_ARR(1:3)/NP_AVR(J11)
            ELSE
              TABL_AVR(J11)%GRP_ARR(5) = 0.0
              TABL_AVR(J11)%GRP_ARR(6) = 0.0D0
              TABL_AVR(J11)%GRP_ARR(7) = 1.0/86400.0D0
         END IF
         TABL_AVR(J11)%GRP_ARR(7) = TIM_DUR/86400.D0
 4110 CONTINUE 
!
! --- Create the output FITS-file
!
      INQUIRE ( FILE=FILOUT, EXIST=LEX ) 
      IF ( LEX ) THEN
           IS = UNLINK ( TRIM(FILOUT)//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL GERROR ( STR )
                CALL ERR_LOG ( 1935, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &               'an attempt remove existing output fits-file '// &
     &                TRIM(FILOUT)//' -- '//STR )
                RETURN 
           END IF
      END IF
      FT_STATUS = 0
      FPTR = 0
      CALL FFINIT ( FPTR, FILOUT(1:I_LEN(FILOUT))//CHAR(0), FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 1927, IER, 'FITS_TIM_AVR', FT_STATUS )
           CALL ERR_LOG ( 1936, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'an attempt to open output fits-file '//FILOUT )
           RETURN 
      END IF
!
      DO 4200 J20=1,PIM%FILE(1)%L_KWD(1)
         IF ( PIM%FILE(1)%KEY(J20,1)(1:9) == 'GCOUNT  =' ) THEN
              WRITE ( UNIT=PIM%FILE(1)%KEY(J20,1)(23:30), FMT='(I8)' ) GC_AVR
         END IF
         STR = PIM%FILE(1)%KEY(J20,1)
         FT_STATUS = 0
         CALL FFPREC ( %VAL(FPTR), %REF(STR(1:I_LEN(STR))//CHAR(0)), FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              WRITE ( 6, * ) ' j1 = ', j1 ! %%
              CALL FT_PRINTERROR ( 1928, IUER, 'FITS_TIM_AVR', FT_STATUS )
              RETURN 
         END IF      
 4200 CONTINUE 
!
! --- Position the table counter to the first table
!
      CALL FFMAHD ( %VAL(FPTR), %VAL(1), NHDTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1929, IUER, 'FITS_TIM_AVR', &
     &                          FT_STATUS )
           RETURN 
      END IF      
!
      DO 4210 J21=1,GC_AVR
!
! ------ Write down group parameters
!
         CALL FFPGPE ( %VAL(FPTR), %VAL(J21), %VAL(1), %VAL(N_GRP), &
     &                 TABL_AVR(J21)%GRP_ARR, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 1767, IER, 'FITS_TIM_AVR', FT_STATUS )
              CALL ERR_LOG ( 1937, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &            'attempt to write the uv-data in the output fits-file '// &
     &             FILOUT )
              RETURN 
         END IF
!
! ------ Write down UV data
!
         CALL FFPPRE ( %VAL(FPTR), %VAL(J21), %VAL(INT8(1)), &
     &                 %VAL(INT8(3*KFRQ)), TABL_AVR(J21)%UV_DATA, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 1768, IER, 'FITS_TIM_AVR', FT_STATUS )
              CALL ERR_LOG ( 1938, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &            'attempt to write the uv-data in the output fits-file '// &
     &             FILOUT )
              RETURN 
         END IF
 4210 CONTINUE 
      CALL FFMAHD ( %VAL(PIM%FILE(1)%FITS_DESC), %VAL(2), NHDTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1929, IUER, 'FITS_TIM_AVR', &
     &                          FT_STATUS )
           RETURN 
      END IF      
!
      FT_STATUS = 0
      CALL FFCPFL ( %VAL(PIM%FILE(1)%FITS_DESC), %VAL(FPTR), %VAL(0), %VAL(2), &
     &              %VAL(PIM%FILE(1)%L_HDR), FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1926, IUER, 'GET_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 1939, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'position to the next table fo the fits file '// &
     &          FILIN )
           RETURN
      END IF
!
! --- Well, that is it. Close the FITS files
!
      FT_STATUS = 0
      CALL FFCLOS ( %VAL(PIM%FILE(1)%FITS_DESC), FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 1777, IER, 'FITS_TIM_AVR', FT_STATUS )
           CALL ERR_LOG ( 1940, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'attempt to put keys in the output file '//FILOUT )
           RETURN
      END IF
!
      FT_STATUS = 0
      CALL FFCLOS ( %VAL(FPTR), FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 1778, IER, 'FITS_TIM_AVR', FT_STATUS )
           CALL ERR_LOG ( 1941, IUER, 'FITS_TIM_AVR', 'Error in '// &
     &         'attempt to put keys in the output file '//FILOUT )
           RETURN
      END IF
!
      DEALLOCATE ( REF_INP_AVR  )
      DEALLOCATE ( REF_INP_SCA  )
      DEALLOCATE ( NP_INP       )
      DEALLOCATE ( NP_AVR       )
      DEALLOCATE ( TABL_AVR     )
      DEALLOCATE ( TABL_INP     )
      CALL ERR_LOG ( 0, IUER    )
!
      RETURN
      END  SUBROUTINE  FITS_TIM_AVR  !#!#
