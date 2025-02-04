      SUBROUTINE UVA_MERGE ( PIM, NFIL, FILIN, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Rouitine UVA_MERGE 
! *                                                                      *
! *  ### 18-SEP-2012   UVA_MERGE   v2.1 (c)  L. Petrov  15-MAY-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( FITS_PRIM__STRU ), POINTER :: TABL(:)
      CHARACTER  UVA_MERGE__LABEL*28
      PARAMETER  ( UVA_MERGE__LABEL = 'UVA_MERGE  v 2.1  2013.10.25' )
!
      INTEGER*4  NFIL, IUER
      CHARACTER  FILIN(NFIL)*(*), FILOUT*(*)
      INTEGER*8  FPTR, DESC(PIM__MSTA)
      CHARACTER  STR*512, SOU_NAME*10, EXTNAME*8, &
     &           ANT_NAM_ARR(PIM__MSTA,PIM__MSUB,NFIL)*8, &
     &           C_STA(PIM__MSTA)*8, &
     &           EXP_NAME(NFIL)*32, USER_NAME*128, USER_REALNAME*128, &
     &           USER_E_ADDRESS*128
      INTEGER*4  GC(NFIL), GC_ACC
      REAL*8     JD(NFIL), FREQ_REF, VAL, JD_NEW, STA_COO(3,PIM__MSTA,NFIL), &
     &           FREQ_OFF(PIM__MFRQ), FREQ_CHW(PIM__MFRQ), FREQ_TBW(PIM__MFRQ), &
     &           STA_COO_GLO(3,PIM__MSTA), STA_COO_OUT(3,PIM__MSTA)
      INTEGER*4    N_GRP
      PARAMETER  ( N_GRP = 7 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, &
     &           J24, J25, J26, J27, J28, &
     &           K_STA(PIM__MSUB,NFIL), L_STA, FT_STATUS, ANY_VAL, L_SUB, KFRQ, &
     &           IND_ANT_KEY(PIM__MSUB), IND_ANT_TAB(PIM__MSUB), &
     &           IND_STC_TAB(PIM__MSUB), IND_STC_KEY(PIM__MSUB), &
     &           ID, IP, IL, CRF_ANT(PIM__MSTA), IVAL, GCC, &
     &           ISUB_OLD, ISUB_NEW, ISTA_OLD(2), ISTA_NEW(2), &
     &           NHDTYPE, FREQ_SDB(PIM__MFRQ), NOSTA(PIM__MSTA), &
     &           LN_SUB, LN_STA(PIM__MSUB), LISN_STA(PIM__MSTA,PIM__MSUB), &
     &           LF_SUB, LF_STA(PIM__MSUB), LISF_STA(PIM__MSTA,PIM__MSUB), &
     &           IND_SUB(PIM__MSUB), GCC_LAST, NAXIS2(PIM__MSUB), IND, &
     &           ISTA_N(2), IER
      LOGICAL*1  FL_FOUND_SUB, FL_SHOW_NUM_FRQ 
      INTEGER*4  IND_FRS_TAB, IND_FRS_KEY, IND_CHW_TAB, IND_CHW_KEY, &
     &           IND_TBW_TAB, IND_TBW_KEY, IND_SDB_TAB, IND_SDB_KEY 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, ADD_LIS, ADD_CLIST, LTM_DIF, IFIND_PL
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, GET_CDATE*19
#ifdef GNU
      INTEGER*4, EXTERNAL :: PIMA_COMPAR_TABL
#else
      INTEGER*2, EXTERNAL :: PIMA_COMPAR_TABL
#endif
!
      PIM%L_FIL = NFIL
      ALLOCATE ( PIM%FILE(PIM%L_FIL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%L_FIL*SIZEOF(PIM%FILE(1)), STR )
           CALL ERR_LOG ( 1711, IUER, 'UVA_MERGE', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
      CALL NOUT ( SIZEOF(PIM%FILE(1)), PIM%FILE(1) )
      PIM%FILE(1)%NAME = FILIN(1)
      PIM%FILE(1)%KEY  => NULL()
!
      CALL GETENVAR ( 'PIMAVAR_SHOW_NUM_FRQ', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_SHOW_NUM_FRQ = .TRUE.
         ELSE
           FL_SHOW_NUM_FRQ = .FALSE.
      END IF
!
      CALL FFITS_OPEN ( PIM%FILE(1)%NAME, PIM%FILE(1)%FITS_DESC, 'OLD', &
     &                  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1712, IUER, 'UVA_MERGE', 'Error in an attempt '// &
     &         'to open FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      CALL ERR_PASS       ( IUER, IER )
      CALL FFITS_GET_KEYP ( PIM%FILE(1)%FITS_DESC, PIM__MHDR, -PIM__MKWD, &
     &                      PIM%FILE(1)%M_KWD, PIM%FILE(1)%L_HDR, &
     &                      PIM%FILE(1)%L_KWD, PIM%FILE(1)%KEY, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1713, IUER, 'UVA_MERGE', 'Error in an attempt '// &
     &         'to get keys from FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      DO 410 J1=1,PIM%FILE(1)%L_HDR
         DO 420 J2=1,PIM%FILE(1)%L_KWD(J1)
!!            WRITE ( 6, * ) INT2(J2), INT2(J1), ' Key: '//PIM%FILE(1)%KEY(J2,J1)
            IF ( PIM%FILE(1)%KEY(J2,J1) == 'OBJECT  =' ) THEN
                 SOU_NAME = PIM%FILE(1)%KEY(J2,J1)(12:21)
            END IF
            IF ( J1 == 1 .AND. PIM%FILE(1)%KEY(J2,J1)(1:9) == 'GCOUNT  =' ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2,J1)(23:30), FMT='(I8)' ) GC(1)
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:20) == "CTYPE4  = 'FREQ    '" ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2+2,J1)(15:30), FMT='(F16.2)' ) FREQ_REF
            END IF
            IF ( PIM%FILE(1)%KEY(J2,J1)(1:9) == 'NO_IF   =' ) THEN
                 READ ( UNIT=PIM%FILE(1)%KEY(J2,J1)(23:30), FMT='(I8)' ) KFRQ
            END IF
 420     CONTINUE 
 410  CONTINUE 
      GC_ACC = 0
!
      CALL ERR_PASS    ( IUER, IER )
      CALL FFITS_CLOSE ( PIM%FILE(1)%FITS_DESC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1714, IUER, 'UVA_MERGE', 'Error in an '// &
     &         'attempt to close FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      K_STA = 0
      L_STA = 0
      CALL CLRCH ( ANT_NAM_ARR )
      DO 430 J3=1,PIM%L_FIL
         L_SUB = 0
!
! ------ Open J3-th file
!
         PIM%FILE(J3)%NAME = FILIN(J3)
         CALL ERR_PASS   ( IUER, IER )
         CALL FFITS_OPEN ( PIM%FILE(J3)%NAME, PIM%FILE(J3)%FITS_DESC, 'OLD', &
     &                     IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1715, IUER, 'UVA_MERGE', 'Error in an attempt '// &
     &            'to open FITS UV-file '//PIM%FILE(J3)%NAME )
              RETURN
         END IF
!
! ------ Read the keys from the J3-th file
!
         CALL ERR_PASS       ( IUER, IER )
         CALL FFITS_GET_KEYP ( PIM%FILE(J3)%FITS_DESC, PIM__MHDR, -PIM__MKWD, &
     &                         PIM%FILE(J3)%M_KWD, PIM%FILE(J3)%L_HDR, &
     &                         PIM%FILE(J3)%L_KWD, PIM%FILE(J3)%KEY, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1716, IUER, 'UVA_MERGE', 'Error in an attempt '// &
     &            'to get keys from FITS UV-file '//PIM%FILE(J3)%NAME )
              RETURN
         END IF
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
         DO 440 J4=1,PIM%FILE(J3)%L_HDR
            EXTNAME = '????????'
            DO 450 J5=1,PIM%FILE(J3)%L_KWD(J4)
               IF ( PIM%FILE(J3)%KEY(J5,J4) == 'OBJECT  =' ) THEN
                    IF ( PIM%FILE(1)%KEY(J2,J1)(12:21) .NE. SOU_NAME ) THEN
                         CALL ERR_LOG ( 1717, IUER, 'UVA_MERGE', &
     &                       'An attempt to merge UVA-files for sources '// &
     &                       ' with different names: '//SOU_NAME// &
     &                       ' from file '// &
     &                       PIM%FILE(1)%NAME(1:I_LEN(PIM%FILE(1)%NAME))// &
     &                       ' and '//PIM%FILE(1)%KEY(J2,J1)(12:21)// &
     &                       ' from file '//PIM%FILE(J3)%NAME )
                         RETURN
                    END IF
               END IF
               IF ( J4 == 1 .AND. PIM%FILE(J3)%KEY(J5,J4)(1:9) == 'GCOUNT  =' ) THEN
                    READ ( UNIT=PIM%FILE(J3)%KEY(J5,J4)(23:30), FMT='(I8)' ) GC(J3)
                    GC_ACC = GC_ACC + GC(J3)
               END IF
               IF ( PIM%FILE(J3)%KEY(J5,J4)(1:9) == 'NAXIS2  =' ) THEN
                    READ ( UNIT=PIM%FILE(J3)%KEY(J5,J4)(25:30), FMT='(I6)' ) NAXIS2(J4) 
               END IF
               IF ( PIM%FILE(J3)%KEY(J5,J4)(1:20) == "PTYPE5  = 'DATE    '" ) THEN
                    READ ( UNIT=PIM%FILE(J3)%KEY(J5+2,J4)(15:30), FMT='(F16.7)' ) JD(J3)
                    IF ( J3 == 1 ) THEN
                         JD_NEW = JD(1)
                       ELSE 
                         JD_NEW = MIN ( JD_NEW, JD(J3) )
                    END IF 
               END IF
               IF ( PIM%FILE(J3)%KEY(J5,J4)(1:20) == "CTYPE4  = 'FREQ    '" ) THEN
                    READ ( UNIT=PIM%FILE(J3)%KEY(J5+2,J4)(15:30), FMT='(F16.7)' ) VAL
                    IF ( VAL .NE. FREQ_REF ) THEN
                         CALL ERR_LOG ( 1718, IUER, 'UVA_MERGE', &
     &                       'An attempt to merge UVA-files for data '// &
     &                       ' with different reference frequencies' )
                         RETURN
                    END IF
               END IF
               IF ( PIM%FILE(J3)%KEY(J5,J4)(1:8) == "EXTNAME " ) THEN
                    EXTNAME = PIM%FILE(J3)%KEY(J5,J4)(12:19) 
               END IF             
               IF ( PIM%FILE(J3)%KEY(J5,J4)(1:20) == "TTYPE1  = 'ANNAME  '" .AND. &
     &              EXTNAME == 'AIPS AN ' ) THEN
                    L_SUB = L_SUB + 1
                    K_STA(L_SUB,J3) = NAXIS2(J4)
                    IND_ANT_TAB(L_SUB) = J4
                    IND_ANT_KEY(L_SUB) = J5
               END IF
               IF ( PIM%FILE(J3)%KEY(J5,J4)(1:20) == "TTYPE2  = 'STABXYZ '" ) THEN
                    IND_STC_TAB(L_SUB) = J4
                    IND_STC_KEY(L_SUB) = J5
               END IF
               IF ( PIM%FILE(J3)%KEY(J5,J4)(1:9) == 'NO_IF   =' ) THEN
                    READ ( UNIT=PIM%FILE(J3)%KEY(J5,J4)(23:30), FMT='(I8)' ) IVAL
                    IF ( FL_SHOW_NUM_FRQ ) THEN
                         WRITE ( 6, '(A,I4)' ) 'UVA_MERGE File: '//TRIM(PIM%FILE(J3)%NAME), IVAL
                         GOTO 450
                    END IF
                    IF ( IVAL .NE. KFRQ ) THEN
                         CALL ERR_LOG ( 1719, IUER, 'UVA_MERGE', &
     &                       'An attempt to merge UVA-files for data '// &
     &                       ' with different number of frequencies. '//  &
     &                       'Offending file: '//PIM%FILE(J3)%NAME )
                         RETURN
                    END IF
               END IF
               IF ( PIM%FILE(J3)%KEY(J5,J4)(1:9) == 'OBSERVER=' ) THEN
                    EXP_NAME(J3) = PIM%FILE(J3)%KEY(J5,J4)(12:)
                    IP = INDEX ( EXP_NAME(J3), "'" )
                    IF ( IP > 0 ) CALL CLRCH ( EXP_NAME(J3)(IP:) )
               END IF
               IF ( PIM%FILE(J3)%KEY(J5,J4)(11:20) == "'IF FREQ '" ) THEN
                    IND_FRS_TAB = J4
                    IND_FRS_KEY = J5
               END IF
               IF ( PIM%FILE(J3)%KEY(J5,J4)(11:20) == "'CH WIDTH'" ) THEN
                    IND_CHW_TAB = J4
                    IND_CHW_KEY = J5
               END IF
               IF ( PIM%FILE(J3)%KEY(J5,J4)(11:27) == "'TOTAL BANDWIDTH'" ) THEN
                    IND_TBW_TAB = J4
                    IND_TBW_KEY = J5
               END IF
               IF ( PIM%FILE(J3)%KEY(J5,J4)(11:27) == "'SIDEBAND'" ) THEN
                    IND_SDB_TAB = J4
                    IND_SDB_KEY = J5
               END IF
 450        CONTINUE 
 440     CONTINUE 
         IF ( FL_SHOW_NUM_FRQ ) THEN
              GOTO 430
         END IF
!
         IF ( L_SUB == 0 ) THEN
              CALL ERR_LOG ( 1721, IUER, 'UVA_MERGE', 'No ANNAME keyword '// &
     &            'was found in FITS UV-file '//PIM%FILE(J3)%NAME )
              RETURN
         END IF
!
         IF ( IND_STC_TAB(1) == 0 .OR. IND_STC_KEY(1) == 0 ) THEN
              CALL ERR_LOG ( 1722, IUER, 'UVA_MERGE', 'No STABXYZ  keyword '// &
     &            'was found in FITS UV-file '//PIM%FILE(J3)%NAME )
              RETURN
         END IF
!
         IF ( IND_FRS_TAB == 0 .OR. IND_FRS_KEY == 0 ) THEN
              CALL ERR_LOG ( 1723, IUER, 'UVA_MERGE', 'No IF FREQ  keyword '// &
     &            'was found in FITS UV-file '//PIM%FILE(J3)%NAME )
              RETURN
         END IF
!
         IF ( IND_CHW_TAB == 0 .OR. IND_CHW_KEY == 0 ) THEN
              CALL ERR_LOG ( 1724, IUER, 'UVA_MERGE', 'No CH WIDTH keyword '// &
     &            'was found in FITS UV-file '//PIM%FILE(J3)%NAME )
              RETURN
         END IF
!
         IF ( IND_TBW_TAB == 0 .OR. IND_TBW_KEY == 0 ) THEN
              CALL ERR_LOG ( 1725, IUER, 'UVA_MERGE', 'No TOTAL BANDWIDTH '// &
     &            'keyword was found in FITS UV-file '//PIM%FILE(J3)%NAME )
              RETURN
         END IF
!
         IF ( IND_SDB_TAB == 0 .OR. IND_SDB_KEY == 0 ) THEN
              CALL ERR_LOG ( 1726, IUER, 'UVA_MERGE', 'No SIDEBAND '// &
     &            'keyword was found in FITS UV-file '//PIM%FILE(J3)%NAME )
              RETURN
         END IF
!
         DO 460 J6=1,L_SUB
            DO 470 J7=1,K_STA(J6,J3)
!
! ------------ Get the antenna name
!
               CALL ERR_PASS ( IUER, IER )
               CALL FFITS_GETCH ( PIM%FILE(J3)%FITS_DESC, IND_ANT_TAB(J6), J7, &
     &                            PIM%FILE(J3)%KEY(IND_ANT_KEY(J6),IND_ANT_TAB(J6)), &
     &                            1, ANT_NAM_ARR(J7,J6,J3), IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J7, STR )
                    CALL ERR_LOG ( 1727, IUER, 'UVA_MERGE', 'Error in '// &
     &                  'reading station name of the '// &
     &                   STR(1:I_LEN(STR))//'-th '// &
     &                  'station in the FITS-IDI file '//PIM%FILE(J3)%NAME  )
                    RETURN
               END IF
               ID = INDEX ( ANT_NAM_ARR(J7,J6,J3), CHAR(0) )
               IF ( ID > 0 ) CALL REPEAT ( CHAR(0), LEN(ANT_NAM_ARR(J7,J6,J3))-ID+1, &
     &                                                  ANT_NAM_ARR(J7,J6,J3)(ID:) )
!
               CALL ERR_PASS ( IUER, IER )
               CALL FFITS_GETR8 ( PIM%FILE(J3)%FITS_DESC, IND_STC_TAB(J6), J7, &
     &                            PIM%FILE(J3)%KEY(IND_STC_KEY(J6),IND_STC_TAB(J6)), &
     &                            3, STA_COO(1,J7,J3), IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J7, STR )
                    CALL ERR_LOG ( 1728, IUER, 'UVA_MERGE', 'Error in '// &
     &                  'reading station coordinates of the '// &
     &                   STR(1:I_LEN(STR))//'-th '// &
     &                  'station in the FITS-IDI file '//PIM%FILE(J3)%NAME  )
                    RETURN
               END IF
!
               CALL ERR_PASS ( IUER, IER )
               ID = ADD_CLIST ( PIM__MSTA, L_STA, C_STA, ANT_NAM_ARR(J7,J6,J3), IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 1729, IUER, 'UVA_MERGE', 'Trap of internal '// &
     &                  'control: List L_STA/C_STA is overflown' )
                    RETURN 
               END IF
 470        CONTINUE       
 460     CONTINUE       
!
         IF ( J3 == 1 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_FRS_TAB, 1, &
     &                           PIM%FILE(1)%KEY(IND_FRS_KEY,IND_FRS_TAB), &
     &                           KFRQ, FREQ_OFF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1730, IUER, 'UVA_MERGE', 'Error in '// &
     &                 'getting bandwidth frequency array from the '// &
     &                 'FITS-IDI file '//PIM%FILE(1)%NAME  )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_CHW_TAB, 1, &
     &                           PIM%FILE(1)%KEY(IND_CHW_KEY,IND_CHW_TAB), &
     &                           KFRQ, FREQ_CHW, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1731, IUER, 'UVA_MERGE', 'Error in '// &
     &                 'getting channel width array from the '// &
     &                 'FITS-IDI file '//PIM%FILE(1)%NAME  )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_TBW_TAB, 1, &
     &                           PIM%FILE(1)%KEY(IND_TBW_KEY,IND_TBW_TAB), &
     &                           KFRQ, FREQ_TBW, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1732, IUER, 'UVA_MERGE', 'Error in '// &
     &                 'getting total bandwidth array from the '// &
     &                 'FITS-IDI file '//PIM%FILE(1)%NAME  )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL FFITS_GETI4 ( PIM%FILE(1)%FITS_DESC, IND_SDB_TAB, 1, &
     &                           PIM%FILE(1)%KEY(IND_SDB_KEY,IND_SDB_TAB), &
     &                           KFRQ, FREQ_SDB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1733, IUER, 'UVA_MERGE', 'Error in '// &
     &                 'getting sideband id array from the '// &
     &                 'FITS-IDI file '//PIM%FILE(1)%NAME  )
                   RETURN
              END IF
         END IF
!
         CALL ERR_PASS    ( IUER, IER )
         CALL FFITS_CLOSE ( PIM%FILE(J3)%FITS_DESC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1734, IUER, 'UVA_MERGE', 'Error in an '// &
     &            'attempt to close FITS UV-file '//PIM%FILE(J3)%NAME )
              RETURN
         END IF
 430  CONTINUE
      IF ( FL_SHOW_NUM_FRQ ) THEN
           CALL EXIT ( 2 )
      END IF
!
      CALL SORT_CH ( L_STA, C_STA )
!
! --- Allocate dynamic memory for temporary arrays
!
      ALLOCATE ( TABL(GC_ACC), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1735, IUER, 'UVA_MERGE', 'Error in '// &
     &         'attempt to allocate dynamic memory for the data '// &
     &         'structure TABL' )
           RETURN
      END IF 
!
      GCC = 0
      LN_SUB = 0
      DO 480 J8=1,PIM%L_FIL
         PIM%FILE(J8)%NAME = FILIN(J8)
         CALL ERR_PASS   ( IUER, IER )
         CALL FFITS_OPEN ( PIM%FILE(J8)%NAME, PIM%FILE(J8)%FITS_DESC, 'OLD', &
     &                     IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1736, IUER, 'UVA_MERGE', 'Error in an attempt '// &
     &            'to open FITS UV-file '//PIM%FILE(J8)%NAME )
              RETURN
         END IF
!
         LF_SUB = 0
         LF_STA = 0
         LISF_STA = 0
         GCC_LAST = GCC
         DO 490 J9=1,GC(J8)
            GCC = GCC + 1
            ALLOCATE ( TABL(GCC)%GRP_ARR(N_GRP),  STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1737, IUER, 'UVA_MERGE', 'Error in '// &
     &               'attempt to allocate dynamic memory for '// &
     &               'an element GRP_ARR in the data structure TABL' )
                 RETURN 
            END IF
! ??????????????????????????????????????????????????????????????? NAXIS3 ?????????????????
            ALLOCATE ( TABL(GCC)%UV_DATA(3,KFRQ,1), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1738, IUER, 'UVA_MERGE', 'Error in '// &
     &               'attempt to allocate dynamic memory for '// &
     &               'an element UV_DATA in the data structure TABL' )
                 CALL EXIT ( 1 )
            END IF
!
! --------- Read group parameters
!
            FT_STATUS = 0
            CALL FFGGPE ( %VAL(PIM%FILE(J8)%FITS_DESC), %VAL(J9), %VAL(1), &
     &                    %VAL(N_GRP), %VAL(0.0), TABL(GCC)%GRP_ARR, ANY_VAL, &
     &                    FT_STATUS )
            IF ( FT_STATUS .NE. 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL FT_PRINTERROR ( 1761, IER, 'UVA_MERGE', FT_STATUS )
                 CALL ERR_LOG ( 1739, IUER, 'UVA_MERGE', 'Error in '// &
     &               'attempt to read the aucilliry information from the '// &
     &               'input file '//PIM%FILE(J8)%NAME  )
                 RETURN 
            END IF
!
! --------- Get subarray index and station index in the ISUB_OLD subarray.
! --------- We also computed the list of station for the ISUB_OLD -th
! --------- subarray. Suffex _OLD emphasizes that this list is in 
! --------- the original order for the J8-th file
!
            ISUB_OLD    = NINT ( 100*TABL(GCC)%GRP_ARR(4) - 100.0*NINT(TABL(GCC)%GRP_ARR(4)) ) + 1 
            ISTA_OLD(1) = NINT ( (TABL(GCC)%GRP_ARR(4) - (ISUB_OLD-1)*0.01)/256   )
            ISTA_OLD(2) = NINT (  TABL(GCC)%GRP_ARR(4) - (ISUB_OLD-1)*0.01 - 256*ISTA_OLD(1) )
!
            LF_SUB = MAX ( LF_SUB, ISUB_OLD )
            ID = ADD_LIS ( PIM__MSTA, LF_STA(ISUB_OLD), LISF_STA(1,ISUB_OLD), &
     &                     ISTA_OLD(1), IER )
            ID = ADD_LIS ( PIM__MSTA, LF_STA(ISUB_OLD), LISF_STA(1,ISUB_OLD), &
     &                     ISTA_OLD(2), IER )
 490     CONTINUE 
!
! ------ Now We try to fit all subarrays in the J8-th file in the global
! ------ list of subarrays. If a given subarray is not idenitical to some
! ------ global sybarray, we append it to the global list of subarrays
!
         DO 4100 J10=1,LF_SUB
!
! --------- Re-indeing J10-th subarray. LSIF_STA will conains global station
! --------- indices sin the list L_STA/C_STA
!
            DO 4110 J11=1,LF_STA(J10)
               LISF_STA(J11,J10) = LTM_DIF ( 0, L_STA, C_STA, ANT_NAM_ARR(LISF_STA(J11,J10),J10,J8) )
 4110       CONTINUE 
!
! --------- Sort this station list
!
            CALL SORT_I ( LF_STA(J10), LISF_STA(1,J10) )
 4100    CONTINUE 
         IF ( J8 == 1 ) THEN
!
! ----------- This is the first file. We make the subarrays list of the 
! ----------- first fist file it global list
!
              LN_SUB   = LF_SUB
              LN_STA   = LF_STA
              LISN_STA = LISF_STA
              DO 4120 J12=1,LN_SUB
                 IND_SUB(J12) = J12
                 DO 4130 J13=1,K_STA(J12,J8)
                    IND = LTM_DIF ( 0, L_STA, C_STA, ANT_NAM_ARR(J13,J12,J8) )
                    STA_COO_GLO(1:3,IND) = STA_COO(1:3,J13,J8)
 4130            CONTINUE 
 4120         CONTINUE 
           ELSE 
!
! ----------- We process the file which is not the first
!
              DO 4140 J14=1,LF_SUB
                 FL_FOUND_SUB = .FALSE.
!
! -------------- Update the global station coordinate array
!
                 DO 4150 J15=1,K_STA(J14,J8)
                    IND = LTM_DIF ( 0, L_STA, C_STA, ANT_NAM_ARR(J15,J14,J8) )
                    STA_COO_GLO(1:3,IND) = STA_COO(1:3,J15,J8)
 4150            CONTINUE 
!
! -------------- Check the J14-th subarray in the current file against
! -------------- any global subarrays
!
                 DO 4160 J16=1,LN_SUB
!
! ----------------- Check, whether the local subarray with index j13 is the same
! ----------------- as the global subarray with index j14
!
                    IF ( LF_STA(J14) == LN_STA(J16) ) THEN
                         DO 4170 J17=1,LN_STA(J16)
                            IF ( LISN_STA(J17,J16) .NE. LISF_STA(J17,J14) ) THEN
!
! ------------------------------ They differ! Try next global subarray
!
                                 GOTO 4160
                            END IF                                 
 4170                    CONTINUE 
!
! ---------------------- Ura! The are the same. Update the cross reference
! ---------------------- index IND_SUB and set the flag
!
                         IND_SUB(J14) = J16
                         FL_FOUND_SUB = .TRUE.
                    END IF
 4160            CONTINUE 
                 IF ( .NOT. FL_FOUND_SUB ) THEN
!
! ------------------- What? We did not find the J13-th subarray in the 
! ------------------- global subarray lists? Not a big deal.
! ------------------- Append this subarray to the end of the global 
! ------------------- list
!
                      LN_SUB = LN_SUB + 1
                      IF ( LN_SUB > PIM__MSUB ) THEN
                           CALL CLRCH ( STR ) 
                           CALL INCH  ( PIM__MSUB, STR )
                           CALL ERR_LOG ( 1740, 7444, IUER, 'UVA_MERGE', &
     &                         'Too many subarrays, more than '// &
     &                          STR(1:I_LEN(STR))// &
     &                         ' please increment parameter PIM__MSUB' )
                           RETURN 
                      END IF
!
                      LN_STA(LN_SUB) = LF_STA(J14)
                      LISN_STA(1:LN_STA(LN_SUB),LN_SUB) = LISF_STA(1:LF_STA(J14),J14) 
                      IND_SUB(J14) = LN_SUB
                 END IF
 4140         CONTINUE 
         END IF
!
! ------ Make the second run through visibility data
!
         GCC = GCC_LAST 
         DO 4180 J18=1,GC(J8)
            GCC = GCC + 1
!
! --------- Read UV-data
!
            FT_STATUS = 0
            CALL FFGPVE ( %VAL(PIM%FILE(J8)%FITS_DESC), %VAL(J18), %VAL(1), &
     &                    %VAL(INT8(3*KFRQ)), %VAL(0.0), TABL(GCC)%UV_DATA, &
     &                    ANY_VAL, FT_STATUS )
            IF ( FT_STATUS .NE. 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL FT_PRINTERROR ( 1762, IER, 'UVA_MERGE', FT_STATUS )
                 CALL ERR_LOG ( 1741, IUER, 'UVA_MERGE', 'Error in '// &
     &               'attempt to read the uv-data from the output input '// &
     &               'fits-file '//PIM%FILE(J8)%NAME  )
                 RETURN 
            END IF
!
! --------- Get the old subarray index in the subarray list for the J8-th file
! --------- and station indeces for the ISUB_OLD -th subarray of the J8-th file
!
            ISUB_OLD    = NINT ( 100*TABL(GCC)%GRP_ARR(4) - 100.0*NINT(TABL(GCC)%GRP_ARR(4)) ) + 1 
            ISTA_OLD(1) = NINT ( (TABL(GCC)%GRP_ARR(4) - (ISUB_OLD-1)*0.01)/256  )
            ISTA_OLD(2) = NINT (  TABL(GCC)%GRP_ARR(4) - (ISUB_OLD-1)*0.01 - 256*ISTA_OLD(1) )
!
! --------- Get the global subarray index
!
            ISUB_NEW = IND_SUB(ISUB_OLD)
!
! --------- Get the station indices in the global station list
!
            ISTA_N(1) = LTM_DIF ( 0, L_STA, C_STA, ANT_NAM_ARR(ISTA_OLD(1),ISUB_OLD,J8) )
            ISTA_N(2) = LTM_DIF ( 0, L_STA, C_STA, ANT_NAM_ARR(ISTA_OLD(2),ISUB_OLD,J8) )
!
! --------- Get the station indices in the station list fot the ISUB_NEW -th
! --------- array
!
            ISTA_NEW(1) = IFIND_PL ( LN_STA(ISUB_NEW), LISN_STA(1,ISUB_NEW), ISTA_N(1) )
            ISTA_NEW(2) = IFIND_PL ( LN_STA(ISUB_NEW), LISN_STA(1,ISUB_NEW), ISTA_N(2) )
            IF ( ISTA_NEW(1) < 1 ) THEN
                 write ( 6, * ) 'isub_old= ', isub_old, ' ista_old= ', ista_old ! %%%
                 write ( 6, * ) 'sta: ', ant_nam_arr(ista_old(1),isub_old,j8), ' ', &
     &                                   ant_nam_arr(ista_old(2),isub_old,j8)
                 write ( 6, * ) 'isub_new= ', isub_new, ' ista_new= ', ista_new ! %%%
                 write ( 6, * ) 'ista_n = ', ista_n
                 write ( 6, * ) 'ln_sta(isub_new)= ', ln_sta(isub_new)
                 write ( 6, * ) 'lisn_sta= ', lisn_sta(1:ln_sta(isub_new),isub_new) ! %%%
                 CALL ERR_LOG ( 1742, IUER, 'UVA_MERGE', 'Trap of internal '// &
     &               'control: cannot find 1st station index in the subarray '// &
     &               'station list' )
                 RETURN
            END IF
            IF ( ISTA_NEW(2) < 1 ) THEN
                 CALL ERR_LOG ( 1743, IUER, 'UVA_MERGE', 'Trap of internal '// &
     &               'control: cannot find 2nd station index in the subarray '// &
     &               'station list' ) 
                 RETURN 
            END IF
!
            TABL(GCC)%GRP_ARR(4) = 256*ISTA_NEW(1) + ISTA_NEW(2) + 0.01*(ISUB_NEW-1) ! baseline
            TABL(GCC)%GRP_ARR(5) = TABL(GCC)%GRP_ARR(5) + IDNINT ( JD(J8) - JD_NEW )
 4180    CONTINUE 
!
         CALL ERR_PASS    ( IUER, IER )
         CALL FFITS_CLOSE ( PIM%FILE(J8)%FITS_DESC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1742, IUER, 'UVA_MERGE', 'Error in an '// &
     &            'attempt to close FITS UV-file '//PIM%FILE(J8)%NAME )
              RETURN
         END IF
 480  CONTINUE 
!
! --- Sort the data structure TABL first over time, then over baseline index
!
      CALL FOR_QSORT ( TABL, GC_ACC, SIZEOF(TABL(1)), PIMA_COMPAR_TABL )
!
      DO 4190 J19=1,PIM%FILE(1)%L_HDR
         DO 4200 J20=1,PIM%FILE(1)%L_KWD(J19)
            IF ( J19 == 1 .AND. PIM%FILE(1)%KEY(J20,J19)(1:9) == 'GCOUNT  =' ) THEN
                 CALL INCH ( GC_ACC, PIM%FILE(1)%KEY(J20,J19)(23:30) )
                 CALL CHASHR  (      PIM%FILE(1)%KEY(J20,J19)(23:30) )
               ELSE IF ( J19 == 3 .AND. PIM%FILE(1)%KEY(J20,J19)(1:9) == 'NAXIS2  =' ) THEN
                 CALL INCH   ( L_STA, PIM%FILE(1)%KEY(J20,J19)(28:30) )
                 CALL CHASHR (        PIM%FILE(1)%KEY(J20,J19)(28:30) )
               ELSE IF ( PIM%FILE(1)%KEY(J20,J19)(1:9) == 'DATE-OBS=' ) THEN
                 CALL CLRCH ( STR )
                 STR = PIM%FILE(1)%KEY(J20,J19)(11:22)
                 CALL INCH ( NFIL, STR(41:43) )
                 CALL CHASHL ( STR(41:43) )
                 IF ( NFIL < 10 ) THEN
                      STR = STR(1:11)//'('//STR(41:41)//')'
                   ELSE IF ( NFIL < 100 ) THEN
                      STR = STR(1:11)//'('//STR(41:42)//')'
                   ELSE
                      STR = STR(1:11)//'('//STR(41:43)//')'
                 END IF 
                 PIM%FILE(1)%KEY(J20,J19)= PIM%FILE(1)%KEY(J20,J19)(1:10)// &
     &                                     STR(1:I_LEN(STR))//"'"
                 PIM%FILE(1)%KEY(J20,J19)(42:) = "/ Observation date (first epoch)"
               ELSE IF ( PIM%FILE(1)%KEY(J20,J19)(1:9) == 'OBSERVER=' ) THEN
                 CALL CLRCH ( STR )
                 STR = PIM%FILE(1)%KEY(J20,J19)(11:41)
                 IP = ILEN(STR(1:31)) - 1
                 CALL INCH ( NFIL, STR(41:43) )
                 CALL CHASHL ( STR(41:33) )
                 IF ( NFIL < 10 ) THEN
                      STR = STR(1:IP)//'('//STR(41:41)//')'
                   ELSE IF ( NFIL < 100 ) THEN
                      STR = STR(1:IP)//'('//STR(41:42)//')'
                   ELSE 
                      STR = STR(1:IP)//'('//STR(41:43)//')'
                 END IF 
                 PIM%FILE(1)%KEY(J20,J19)= PIM%FILE(1)%KEY(J20,J19)(1:10)// &
     &                                     STR(1:I_LEN(STR))//"'"
                 PIM%FILE(1)%KEY(J20,J19)(42:) = "/ Experiment (first epoch)"
               ELSE IF ( PIM%FILE(1)%KEY(J20,J19)(1:9) == 'ORIGIN  =' ) THEN
                 PIM%FILE(1)%KEY(J20,J19)(1:9) = 'OLD_ORIG='
            END IF
 4200    CONTINUE 
!
         IF ( J19 == 1 ) THEN
              PIM%FILE(1)%L_KWD(J19) = PIM%FILE(1)%L_KWD(J19) + 1
              PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19) = "ORIGIN  = '"// &
     &                  UVA_MERGE__LABEL//"' / Origin of data"
!
              PIM%FILE(1)%L_KWD(J19) = PIM%FILE(1)%L_KWD(J19) + 1
              CALL CLRCH ( PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19) )
              PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(1:9) = "NUM_EXP = '"    
              WRITE ( UNIT=PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(28:30), FMT='(I3)' ) NFIL
              PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(42:) = '/ Number of experiments'
!
              DO 4210 J21=1,NFIL
                 PIM%FILE(1)%L_KWD(J19) = PIM%FILE(1)%L_KWD(J19) + 1
                 CALL CLRCH ( PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19) )
                 CALL INCH ( J21, STR )
                 PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19) = &
     &              'EXP_000 =                                / Experiment name #'//STR
                 IF ( J21 < 10 ) THEN
                      PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(8:8) = STR(1:1)
                    ELSE IF ( J21 < 100 ) THEN
                      PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(7:8) = STR(1:2)
                    ELSE
                      PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(6:8) = STR(1:3)
                 END IF
                 PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(61:) = STR
                 PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(11:11) = "'"
                 IP = ILEN(EXP_NAME(J21))
                 PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(13:11+IP) = EXP_NAME(J21)
                 PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(13+IP:13+IP) = "'"
 4210         CONTINUE 
!
              DO 4220 J22=1,NFIL
                 PIM%FILE(1)%L_KWD(J19) = PIM%FILE(1)%L_KWD(J19) + 1
                 CALL CLRCH ( PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19) )
                 CALL INCH ( J22, STR )
                 PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19) = &
     &              "DATE_000= '          '                   / Experiment date #"//STR
                 IF ( J22 < 10 ) THEN
                      PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(8:8) = STR(1:1)
                    ELSE IF ( J22 < 100 ) THEN
                      PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(7:8) = STR(1:2)
                    ELSE
                      PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(6:8) = STR(1:3)
                 END IF
                 PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(61:) = STR
                 CALL ERR_PASS ( IUER, IER )
                 STR = JD_TO_DATE ( JD(J22), IER )
                 IF ( IER .NE. 0 ) THEN
                 END IF
                 STR(5:5) = '-'
                 STR(8:8) = '-'
                 PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(12:21) = STR(1:10)
 4220         CONTINUE 
!
              PIM%FILE(1)%L_KWD(J19) = PIM%FILE(1)%L_KWD(J19) + 1
              CALL CLRCH ( PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19) )
              STR = GET_CDATE()
              PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19) = "CRE_DATE= '"// &
     &                        STR(1:19)//"'          / Creation date"
!
              PIM%FILE(1)%L_KWD(J19) = PIM%FILE(1)%L_KWD(J19) + 1
              CALL CLRCH ( PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19) )
              PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19) = &
     &                   "CRE_WHO = '                              / Person who created this file"
              CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
              STR = USER_REALNAME
              IP = ILEN(STR)
              IF ( IP > 28 ) IP = 28
              PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(12:11+IP) = STR(1:IP)
              PIM%FILE(1)%KEY(PIM%FILE(1)%L_KWD(J19),J19)(12+IP:12+IP) = "'"
         END IF         
 4190 CONTINUE 
!
! --- Create the output FITS-file
!
      FT_STATUS = 0
      FPTR = 0
      CALL FFINIT ( FPTR, FILOUT(1:I_LEN(FILOUT))//CHAR(0), FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 1763, IER, 'UVA_MERGE', FT_STATUS )
           CALL ERR_LOG ( 1743, IUER, 'UVA_MERGE', 'Error in '// &
     &         'an attempt to open output fits-file '//FILOUT )
           RETURN 
      END IF
!
      DO 4230 J23=1,2
         IF ( J23 > 1 ) THEN
!
! ----------- Create a new table
!
              FT_STATUS = 0
              CALL FFCRHD ( %VAL(FPTR), FT_STATUS )
              IF ( FT_STATUS .NE. 0 ) THEN
                   CALL FT_PRINTERROR ( 1764, IUER, 'UVA_MERGE', &
     &                  FT_STATUS )
                   RETURN 
              END IF
         END IF
!
         DO 4240 J24=1,PIM%FILE(1)%L_KWD(J23)
            STR = PIM%FILE(1)%KEY(J24,J23)
            FT_STATUS = 0
            CALL FFPREC ( %VAL(FPTR), %REF(STR(1:I_LEN(STR))//CHAR(0)), FT_STATUS )
            IF ( FT_STATUS .NE. 0 ) THEN
                 WRITE ( 6, * ) ' j1 = ', j1 ! %%
                 CALL FT_PRINTERROR ( 1765, IUER, 'UVA_MERGE', FT_STATUS )
                 RETURN 
            END IF      
 4240    CONTINUE 
 4230 CONTINUE 
!
! --- Position the table counter to the first table
!
      CALL FFMAHD ( %VAL(FPTR), %VAL(1), NHDTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 1766, IUER, 'UVA_MERGE', &
     &                          FT_STATUS )
           RETURN 
      END IF      
!
! --- Write random group parameters into the first table
!
      DO 4250 J25=1,GC_ACC
!
! ------ Write down group parameters
!
         CALL FFPGPE ( %VAL(FPTR), %VAL(J25), %VAL(1), %VAL(N_GRP), &
     &                 TABL(J25)%GRP_ARR, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 1767, IER, 'UVA_MERGE', FT_STATUS )
              CALL ERR_LOG ( 1744, IUER, 'UVA_MERGE', 'Error in '// &
     &            'attempt to write the uv-data in the output fits-file '// &
     &             FILOUT )
              RETURN 
         END IF
!
! ------ Write down UV data
!
         CALL FFPPRE ( %VAL(FPTR), %VAL(J25), %VAL(INT8(1)), &
     &                 %VAL(INT8(3*KFRQ)), TABL(J25)%UV_DATA, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 1768, IER, 'UVA_MERGE', FT_STATUS )
              CALL ERR_LOG ( 1745, IUER, 'UVA_MERGE', 'Error in '// &
     &            'attempt to write the uv-data in the output fits-file '// &
     &             FILOUT )
              RETURN 
         END IF
 4250 CONTINUE 
!
! --- Write down frequency parameters
!
      FT_STATUS = 0
      CALL FFMAHD ( %VAL(FPTR), %VAL(2), NHDTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 1881, IER, 'UVA_MERGE', FT_STATUS )
           CALL ERR_LOG ( 1842, IUER, 'UVA_MERGE', 'Error in '// &
     &         'attempt to move to the 2nd table' )
           RETURN
      END IF
!
      FT_STATUS = 0
      CALL FFPCLD ( %VAL(FPTR), %VAL(2), %VAL(INT8(1)), %VAL(INT8(1)), &
     &              %VAL(INT8(KFRQ)), FREQ_OFF, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 1769, IER, 'UVA_MERGE', FT_STATUS )
           CALL ERR_LOG ( 1746, IUER, 'UVA_MERGE', 'Error in '// &
     &         'attempt to write the array of frequency offsets '// &
     &         'into the output file '//FILOUT )
           RETURN
      END IF
!
      FT_STATUS = 0
      CALL FFPCLD ( %VAL(FPTR), %VAL(3), %VAL(INT8(1)), %VAL(INT8(1)), &
     &              %VAL(INT8(KFRQ)), FREQ_CHW, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 1770, IER, 'UVA_MERGE', FT_STATUS )
           CALL ERR_LOG ( 1747, IUER, 'UVA_MERGE', 'Error in '// &
     &         'attempt to write the array of channel width '// &
     &         'into the output file '//FILOUT )
           RETURN
      END IF
!
      FT_STATUS = 0
      CALL FFPCLD ( %VAL(FPTR), %VAL(4), %VAL(INT8(1)), %VAL(INT8(1)), &
     &              %VAL(INT8(KFRQ)), FREQ_TBW, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 1771, IER, 'UVA_MERGE', FT_STATUS )
           CALL ERR_LOG ( 1748, IUER, 'UVA_MERGE', 'Error in '// &
     &         'attempt to write the array of total bandwidths '// &
     &         'into the output file '//FILOUT )
           RETURN
      END IF
!
      FT_STATUS = 0
      CALL FFPCLK ( %VAL(FPTR), %VAL(5), %VAL(INT8(1)), %VAL(INT8(1)), &
     &              %VAL(INT8(KFRQ)), FREQ_SDB, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 1772, IER, 'UVA_MERGE', FT_STATUS )
           CALL ERR_LOG ( 1749, IUER, 'UVA_MERGE', 'Error in '// &
     &         'attempt to write the array of total bandwidths '// &
     &         'into the output file '//FILOUT )
           RETURN
      END IF
!
      DO 4260 J26=1,LN_SUB
         FT_STATUS = 0
         CALL FFCRHD ( %VAL(FPTR), FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL FT_PRINTERROR ( 1764, IUER, 'UVA_MERGE', &
     &             FT_STATUS )
              RETURN 
         END IF
         DO 4270 J27=1,PIM%FILE(1)%L_KWD(3)
            IF ( PIM%FILE(1)%KEY(J27,3)(1:9) == 'NAXIS2  =' ) THEN
                 WRITE ( UNIT=PIM%FILE(1)%KEY(J27,3)(29:30), FMT='(I2)' ) LN_STA(J26)
            END IF
            IF ( PIM%FILE(1)%KEY(J27,3)(1:9) == 'EXTVER  =' ) THEN
                 WRITE ( UNIT=PIM%FILE(1)%KEY(J27,3)(29:30), FMT='(I2)' ) J26
            END IF
            STR = PIM%FILE(1)%KEY(J27,3)
            FT_STATUS = 0
            CALL FFPREC ( %VAL(FPTR), %REF(STR(1:I_LEN(STR))//CHAR(0)), FT_STATUS )
            IF ( FT_STATUS .NE. 0 ) THEN
                 WRITE ( 6, * ) ' j1 = ', j1 ! %%
                 CALL FT_PRINTERROR ( 1765, IUER, 'UVA_MERGE', FT_STATUS )
                 RETURN 
            END IF      
 4270    CONTINUE 
!
         FT_STATUS = 0
         CALL FFMAHD ( %VAL(FPTR), %VAL(2+J26), NHDTYPE, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 1773, IER, 'UVA_MERGE', FT_STATUS )
              CALL ERR_LOG ( 1750, IUER, 'UVA_MERGE', 'Error in '// &
     &            'attempt to move to the 2nd table' )
             RETURN
         END IF
!
! ------ Populate intermediate arrays to be written in the antetnna table
!
         CALL CLRCH ( STR )
         IL = 1
         DO 4280 J28=1,LN_STA(J26)
            ID = I_LEN(C_STA(LISN_STA(J28,J26)))
            IF ( ID > 0 .AND. ID < LEN(C_STA(LISN_STA(J28,J26))) ) THEN
                 CALL REPEAT ( CHAR(0), LEN(C_STA(LISN_STA(J28,J26)))-ID, &
     &                                      C_STA(LISN_STA(J28,J26))(ID+1:) )
            END IF
            STA_COO_OUT(1:3,J28) = STA_COO_GLO(1:3,LISN_STA(J28,J26))
            STR(IL:IL+8) = C_STA(LISN_STA(J28,J26))//CHAR(0)
            DESC(J28) = LOC(STR(IL:IL))
            IL = IL + 9
            NOSTA(J28) = J28
 4280    CONTINUE 
!
! ------ Write information into some (not all!) fields of the antenna table
!
         CALL FFPCLS ( %VAL(FPTR), %VAL(1), %VAL(INT8(1)), &
     &                 %VAL(INT8(1)), %VAL(INT8(LN_STA(J26))), DESC, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 1774, IER, 'UVA_MERGE', FT_STATUS )
              CALL ERR_LOG ( 1751, IUER, 'UVA_MERGE', 'Error in '// &
     &            'attempt to write the station name array into '// &
     &            'the output file '//FILOUT )
              RETURN
         END IF
!
         CALL FFPCLK ( %VAL(FPTR), %VAL(4), %VAL(INT8(1)), %VAL(INT8(1)), &
     &                 %VAL(INT8(LN_STA(J26))), NOSTA, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 1775, IER, 'UVA_MERGE', FT_STATUS )
              CALL ERR_LOG ( 1752, IUER, 'UVA_MERGE', 'Error in '// &
     &            'attempt to the station index array in the output file '// &
     &             FILOUT )
              RETURN
         END IF
!
         CALL FFPCLD ( %VAL(FPTR), %VAL(2), %VAL(INT8(1)), %VAL(INT8(1)), &
     &                 %VAL(INT8(3*LN_STA(J26))), STA_COO_OUT, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 1776, IER, 'UVA_MERGE', FT_STATUS )
              CALL ERR_LOG ( 1753, IUER, 'UVA_MERGE', 'Error in '// &
     &            'attempt to write the array of station coordinates into '// &
     &            'the output file '//FILOUT)
              RETURN
         END IF
 4260 CONTINUE 
!
! --- Well, that is it. Close the FITS file
!
      FT_STATUS = 0
      CALL FFCLOS ( %VAL(FPTR), FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 1777, IER, 'UVA_MERGE', FT_STATUS )
           CALL ERR_LOG ( 1754, IUER, 'UVA_MERGE', 'Error in '// &
     &         'attempt to put keys in the output file '//FILOUT )
           RETURN
      END IF
!
      DEALLOCATE ( TABL )
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  SUBROUTINE UVA_MERGE  !#!#
