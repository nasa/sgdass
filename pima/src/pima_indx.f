      SUBROUTINE PIMA_INDX ( PIM, L_CST, STA_CAT, L_CSO, SOU_CAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_INDX  processes a set of files in FITS-IDI format    *
! *   specified in the PIMA control files and creates numerous index     *
! *   tables.                                                            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    L_CST ( INTEGER*4     ) -- The number of entries in the global    *
! *                               station catalogue that was produced    *
! *                               after parsing keyword STA_NAMES in     *
! *                               the PIMA configuration file.           *
! *  STA_CAT ( PIM_STA__TYPE ) -- Arrays of objects of the global        *
! *                               station catalogue. All stations in the *
! *                               experiment are supposed to be found    *
! *                               there. Dimension: L_CST.               *
! *    L_CSO ( INTEGER*4     ) -- The number of entries in the global    *
! *                               source catalogue that was produced     *
! *                               after parsing keyword SOU_NAMES in     *
! *                               the PIMA configuration file.           *
! *  SOU_CAT ( PIM_SOU__TYPE ) -- Arrays of objects of the global        *
! *                               source catalogue. All sources in the   *
! *                               experiment are supposed to be found    *
! *                               there. Dimension: L_SOU.               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
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
! *  ### 06-JAN-2006   PIMA_INDX  v2.45 (c)  L. Petrov  30-OCT-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  L_CST, L_CSO, IUER
      TYPE     ( PIMA__TYPE    ) :: PIM
      TYPE     ( PIM_FRQ__TYPE ) :: FRQ_ORIG(PIM__MFRQ,PIM__MFRG,PIM__MFIL)
      TYPE     ( PIM_STA__TYPE ) :: STA_CAT(L_CST)
      TYPE     ( PIM_SOU__TYPE ) :: SOU_CAT(L_CSO)
!
      REAL*8     VAL
      INTEGER*4  MARR
      PARAMETER  ( MARR = 2048 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, J24,  &
     &           J25, J26, J27, J28, J29, J30, J31, J32, J33, J34, J35,  &
     &           J36, J37, J38, J39, J40, J41, J42, J43, J44, J45, J46,  &
     &           J47, J48, J49, J50, J51, J52, J53, J54, J55, J56, J57,  &
     &           J58, J59, J60, J61, J62, J63, J64, J65, J66, J67, J68,  &
     &           J69, J70, J71, J72, J73, J74, J75, J76, J77, J78, J79,  &
     &           J80, J81, &
     &           KP, K_FRQ, K_FRG, L_FRQ, MFRQ, N_DUP, &
     &           IND_SOU_TAB(PIM__MFIL), &
     &           IND_ANT_TAB(PIM__MFIL),  &
     &           IND_ARG_TAB(PIM__MFIL),  &
     &           IND_NLEV(PIM__MFIL),  &
     &           IND_POL_TYPA(PIM__MFIL), &
     &           IND_POL_TYPB(PIM__MFIL), &
     &           IND_POL_ANG(PIM__MFIL),  &
     &           IND_FRQ_TAB(PIM__MFIL),  &
     &           IND_NX_TAB,   &
     &           IND_AT_TAB,   &
     &           IND_GC_TAB(PIM__MFIL),   &
     &           IND_CL_TAB(PIM__MFIL),   &
     &           IND_UV_TAB(PIM__MFIL), &
     &           IND_FLG_TAB(PIM__MFIL), &
     &           IND_MOD_TAB(PIM__MFIL), &
     &           IND_WEA_TAB(PIM__MFIL), &
     &           IND_MDC_TAB(PIM__MFIL), &
     &           IND_ANT_IND,  &
     &           IND_SOU_IND,  &
     &           IND_SOU_NAM,  &
     &           IND_SOU_ALP,  &
     &           IND_SOU_DEC,  &
     &           IND_ANT_NAME,      &
     &           IND_STA_NAME,      &
     &           IND_UV_DAT(PIM__MUVT,PIM__MFIL), &
     &           IND_UV_TIM(PIM__MUVT,PIM__MFIL), &
     &           IND_UV_BAS(PIM__MUVT,PIM__MFIL), &
     &           IND_UV_SOU(PIM__MUVT,PIM__MFIL), &
     &           IND_UV_FRQ(PIM__MUVT,PIM__MFIL), &
     &           IND_UV_NX2(PIM__MUVT,PIM__MFIL), &
     &           IND_FLAG_SOURCE_ID(PIM__MFIL),   &
     &           IND_FLAG_STATION_ID(PIM__MFIL),  &
     &           IND_FLAG_FRQ_ID(PIM__MFIL),      &
     &           IND_FLAG_TIME_RANGE(PIM__MFIL),  &
     &           IND_FLAG_SEVERITY(PIM__MFIL),    &
     &           IND_FLAG_REASON(PIM__MFIL),      &
     &           IND_MOD_TIME(PIM__MFIL),         &
     &           IND_MOD_TIME_INTV(PIM__MFIL),    &
     &           IND_MOD_SOU_ID(PIM__MFIL), &
     &           IND_MOD_ANT_ID(PIM__MFIL), &
     &           IND_MOD_FRQ_ID(PIM__MFIL), &
     &           IND_MOD_FRQVAR(PIM__MFIL), &
     &           IND_MOD_FARROT(PIM__MFIL), &
     &           IND_MOD_GDELAY(PIM__MFIL), &
     &           LEN_MOD_GDELAY(PIM__MFIL), &
     &           IND_MOD_PDELAY(PIM__MFIL), &
     &           IND_MOD_FRG(PIM__MFIL),    &
     &           LEN_MOD_PDELAY(PIM__MFIL), &
     &           IND_MOD_PRATE(PIM__MFIL),  &
     &           LEN_MOD_PRATE(PIM__MFIL),  &
     &           IND_MOD_GRATE(PIM__MFIL),  &
     &           IND_MOD_DISP(PIM__MFIL),   &
     &           IND_MOD_DDISP(PIM__MFIL),  &
     &           IND_WEA_TIME(PIM__MFIL),   &
     &           IND_WEA_TIME_INTV(PIM__MFIL), &
     &           IND_WEA_ANT_ID(PIM__MFIL),    &
     &           IND_WEA_PRES(PIM__MFIL),   &
     &           IND_WEA_TEMP(PIM__MFIL),   &
     &           IND_WEA_DEW(PIM__MFIL),    &
     &           IND_MDC_ANT_ID(PIM__MFIL), &
     &           IND_MDC_SOU_ID(PIM__MFIL), &
     &           IND_MDC_TIME(PIM__MFIL),   &
     &           IND_MDC_CLO(PIM__MFIL),    &
     &           IND_MDC_RAT(PIM__MFIL),    &
     &           IND_MDC_ATM(PIM__MFIL),    &
     &           IND_MDC_ATD(PIM__MFIL),    &
     &           IND_MDC_GDL(PIM__MFIL),    &
     &           IND_MDC_GRT(PIM__MFIL),    &
     &           IND_STA_COO(PIM__MFIL),    &
     &           IND_STA_VEL(PIM__MFIL),    &
     &           IND_NAXIS2, IND_FRQ_BFRQ,  IND_FRQ_NFRQ, IND_FRQ_REF, &
     &           IND_FRQ_BWID,  IND_FRQ_CWID, IND_FRQ_SBND, IND_FRQ_IBBC,  &
     &           KSTA(PIM__MFIL), LSTA(PIM__MFIL), KSOU(PIM__MFIL), &
     &           KUV(PIM__MUVT,PIM__MFIL), &
     &           BAS_ID, SOU_ID, IND_EPC, IUV, NSTA, IND_STA, &
     &           NSOU_ORIG, IND_SOU, IND_SOU_ORIG, I_SOU, IU_SOU, K_BAD_SOU, &
     &           SOU_ORIG_REF(PIM__MSOU), SOU_ORIG_REF_UNSORTED(PIM__MSOU), &
     &           ISOU_FIL(PIM__MSOU,PIM__MFIL), &
     &           IND_FRGS(PIM__MFRQ*PIM__MFRG), &
     &           I4_ARR(MARR), KEP, MJD, MJD_FILE_BEG, IND_STA1, IND_STA2, &
     &           IP, IND, IND_FRQ, IND_FRG, KFLAG, KMOD, KWEA(PIM__MFIL), &
     &           KMDC(PIM__MFIL), IDAY, L_NOMOD, WEA_IND_STA, MDC_IND_STA, &
     &           MDC_IND_SOU, N_WEA, N_MDC, IND_REF_STA, &
     &           NLEV_ARR_ORIG(PIM__MSTA), NLEV_UNSORTED(PIM__MSTA), &
     &           N_BAD, N_SUP, FRG_ID, NO_CHAN, &
     &           N_BAD_NEW, IND_GRP_MIN, IND_GRP_MAX, IND_GRP, IND_FRQ_2ND, &
     &           IND_MAX, SPLIT_IER, PIM_MIN_FRG, PIM_MAX_FRG, IND_UV_BEG, &
     &           PREV_UV_IND, IND_FIRST_CHN, IND_LAST_CHN, UV_IND, L_SWAP, &
     &           IND_SOU_UNSRT, IND_SWAP_UNSRT, FIRST_NZERO, &
     &           POLAR_TYPA_UNSORTED(PIM__MSTA), POLAR_TYPB_UNSORTED(PIM__MSTA), &
     &           POLAR_TYPA_SORTED(PIM__MSTA), POLAR_TYPB_SORTED(PIM__MSTA), &
     &           ISOU_REN, IER
      LOGICAL*4  FL_WEA_TAB, FL_MOD_TAB, FL_GC_TAB, FL_FLAG_TAB, FL_MDC_TAB, &
     &           FL_PRINT_UV_INFO
      INTEGER*4, ALLOCATABLE :: FLAG_SOU_ID(:), FLAG_STA_ID(:,:), &
     &                          FLAG_FRQ_ID(:), FLAG_SEVERITY(:)
      REAL*4,    ALLOCATABLE :: FLAG_TIME_RANGE(:,:)
      REAL*8     JD_DAT, SEC, TIM, TIM_FILE_BEG(PIM__MFIL), &
     &           SOU_FITS(3), TIM_FLAG_BEG, TIM_FLAG_END, TIM_OBS_BEG, &
     &           TIM_OBS_END, TIM_MOD_BEG, TIM_MOD_END, TIM_MOD_LAST_END, &
     &           TIM_UV, TIM_TAG, TIM_USED(PIM__MSOU), &
     &           FRQ_MIN, FRQ_MAX, TIM_DIF, FITS_RA, FITS_DEC, &
     &           ALPHA_INP(PIM__MSOU), DELTA_INP(PIM__MSOU), &
     &           POLAR_ANG_UNSORTED(PIM__MFRQ,PIM__MSTA), &
     &           POLAR_ANG_SORTED(PIM__MFRQ,PIM__MSTA)
      REAL*8,    ALLOCATABLE :: FREQ(:,:), BAND_WIDTH(:,:), CHAN_WIDTH(:,:)
      INTEGER*4, ALLOCATABLE :: SIDE_BAND(:,:), BB_SP_CHAN_IND(:,:)
      REAL*8     R8_VAL, R8_ARR(MARR)
      REAL*4     R4_VAL, R4_ARR(MARR)
      INTEGER*8  TIM_I8, FRQ_I8, FREQ_FIRST_ARR_I8(PIM__MFRQ,PIM__MFRG), &
     &           FREQ_LAST_ARR_I8(PIM__MFRQ,PIM__MFRG), &
     &           FREQ_GRP_ARR_MIN_I8(PIM__MFRG), FREQ_GRP_ARR_MAX_I8(PIM__MFRG), &
     &           NPOL_BAD
      INTEGER*8, ALLOCATABLE :: TIM_I8_NSRT(:)
      CHARACTER  STR*512, STA_NAME_FIL(PIM__MSTA,PIM__MFIL)*8, &
     &           C_STA(PIM__MSTA)*16,  C_STA_ORIG(PIM__MSTA)*16, &
     &           C_SOU(PIM__MSOU)*10, C_SOU_ORIG(PIM__MSOU)*16, C_SWAP(PIM__MSOU)*10, &
     &           C_SOU_UNSORTED(PIM__MSOU)*10, &
     &           C_SOU_DB(PIM__MCSO)*16, C_SOU_J2000(PIM__MCSO)*10, &
     &           C_SOU_B1950(PIM__MCSO)*8, C_SOU_IVS(PIM__MCSO)*8,  &
     &           TABLE_NAME*32, STR1*32, STR2*32, STR3*32, STR4*32, &
     &           CORR_FLAG_REASON*128, &
     &           FLG_FILE*128, MOD_FILE*128, MDU_FILE*128, MDC_FILE*128, &
     &           WEA_FILE*128, DUP_FILE*128, STR_FRQ*(16*PIM__MFRQ), &
     &           C_FRQ(PIM__MFRQ)*(16*PIM__MFRQ), EXT_NAME*32, STR_DUP*128, &
     &           STR_FREQ_REPL*128, STR_MJD_MINUS_ONE*128, &
     &           STR_ZERO_FRG_ID_BYPASS*128, &
     &           STR_ZERO_MOD_ID_BYPASS*128, STR_UTC_MINUS_TAI_VALUE*128, &
     &           STR_PIMAVAR_AP_LEN*128, STR_PIMAVAR_AP_TOL*128, &
     &           STR_PIMAVAR_NSTA*8, STR_PIMAVAR_NLEV*32, &
     &           STR_PIMAVAR_AUT_NOSOURCE_STRICT*32, STR_PIMAVAR_POL_XY_TO_HV*32, &
     &           STR_POL_CORRECT*128, STR_NO_IM*128, STR_SET_FRG*8, STR_SWAP_NOERR*1, &
     &           STR_KEEP_FRG*8, STR_SOU_RENAME*1024, SOU_REN_OLD(PIM__MSOU)*16, &
     &           SOU_REN_NEW(PIM__MSOU)*16, STR_FL_PRINT_UV_INFO*3
!
      TYPE ( ANT_NAME_TYPE ), POINTER :: ANT(:) => NULL()
!
      INTEGER*4  MOD_IND_STA, MOD_IND_SOU, MOD_TIM_IND, REF_GRP(PIM__MFRG), &
     &           IND_UV, LUN_MOD, LUN_MDU, LUN_MDC, LUN_FLG, LUN_WEA, LUN_DUP, &
     &           IB, IE, ID1, ID2, ID3, IS, IOS, MOD_IND_STA_ORIG, IVAL1, IVAL2, &
     &           SET_FRG
      REAL*8     MOD_TIM_DAYS, MOD_TIM_INTVL_R8, MOD_TIM_MIN
      REAL*4     WEA_TIM_DAYS, WEA_TIM_INTVL_R4, WEA_PRES, WEA_TEMP, &
     &           WEA_DEW, MOD_TIM_INTVL_R4, AP_LEN_R4, COS_ARC, COS_ARC_MAX
      REAL*8     WEA_TIM_BEG, WEA_TIM_INTVL_R8
      REAL*8     MDC_TIM_DAYS, MDC_TIM_CEN, FRQ_IF, FREQ_ORIG, FREQ_REPL
      REAL*8     STA_COO(3,PIM__MSTA), STA_VEL(3,PIM__MSTA) 
      LOGICAL*4  FL_FOUND, FL_FAILURE, FL_FOU(2), FL_FRQ_IN, FL_FRQ_NOT_IN, &
     &           FL_AUT_NOSOURCE_STRICT, FL_ERROR
      INTEGER*4, ALLOCATABLE :: UV_BAD(:)
      REAL*8       PIMA__JD_DATE_1970, PIMA__JD_DATE_2050
      PARAMETER  ( PIMA__JD_DATE_1970 = 2436934.5D0 )
      PARAMETER  ( PIMA__JD_DATE_2050 = 2469806.5D0 )
!
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
#ifdef GNU
      INTEGER*4, EXTERNAL :: PIMA_COMPAR_STA,    PIMA_COMPAR_SOU, &
     &                       PIMA_COMPAR_FRQ,    PIMA_COMPAR_FRG, &
     &                       PIMA_COMPAR_UV_IND, PIMA_COMPAR_R8,  &
     &                       PIMA_COMPAR_I8,     PIMA_COMPAR_MOD
#else
      INTEGER*2, EXTERNAL :: PIMA_COMPAR_STA,    PIMA_COMPAR_SOU, &
     &                       PIMA_COMPAR_FRQ,    PIMA_COMPAR_FRG, &
     &                       PIMA_COMPAR_UV_IND, PIMA_COMPAR_R8,  &
     &                       PIMA_COMPAR_I8,     PIMA_COMPAR_MOD
#endif
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
      INTEGER*8, EXTERNAL :: IFIND_PL8, IFIND_SORT_PL8
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, IFIND_SORT_PL, LTM_DIF, MULTI_INDEX
      REAL*8     DP_VV_V
!
      CALL CLRCH    ( STR_SET_FRG    )
      CALL CLRCH    ( STR_SWAP_NOERR )
      CALL CLRCH    ( STR_KEEP_FRG   )
      CALL GETENVAR ( 'PIMAVAR_FREQ_REPL',           STR_FREQ_REPL           )
      CALL GETENVAR ( 'PIMAVAR_MJD_MINUS_ONE',       STR_MJD_MINUS_ONE       )
      CALL GETENVAR ( 'PIMAVAR_ZERO_FRG_ID_BYPASS',  STR_ZERO_FRG_ID_BYPASS  )
      CALL GETENVAR ( 'PIMAVAR_ZERO_MOD_ID_BYPASS',  STR_ZERO_MOD_ID_BYPASS  )
      CALL GETENVAR ( 'PIMAVAR_UTC_MINUS_TAI_VALUE', STR_UTC_MINUS_TAI_VALUE )
      CALL GETENVAR ( 'PIMAVAR_AP_LEN',              STR_PIMAVAR_AP_LEN )
      CALL GETENVAR ( 'PIMAVAR_AP_TOL',              STR_PIMAVAR_AP_TOL )
      CALL GETENVAR ( 'PIMAVAR_NSTA',                STR_PIMAVAR_NSTA )
      CALL GETENVAR ( 'PIMAVAR_NLEV',                STR_PIMAVAR_NLEV )
      CALL GETENVAR ( 'PIMAVAR_AUT_NOSOURCE_STRICT', STR_PIMAVAR_AUT_NOSOURCE_STRICT )
      CALL GETENVAR ( 'PIMAVAR_POL_XY_TO_HV',        STR_PIMAVAR_POL_XY_TO_HV )
      CALL GETENVAR ( 'PIMAVAR_POL_CORRECT',         STR_POL_CORRECT )
      CALL GETENVAR ( 'PIMAVAR_NO_IM',               STR_NO_IM   )
      CALL GETENVAR ( 'PIMAVAR_SET_FRG',             STR_SET_FRG )
      CALL GETENVAR ( 'PIMAVAR_SWAP_NOERR',          STR_SWAP_NOERR )
      CALL GETENVAR ( 'PIMAVAR_KEEP_FRG',            STR_KEEP_FRG   )
      IF ( STR_PIMAVAR_AUT_NOSOURCE_STRICT == 'YES' .OR. &
     &     STR_PIMAVAR_AUT_NOSOURCE_STRICT == 'yes'      ) THEN
           FL_AUT_NOSOURCE_STRICT = .TRUE.
         ELSE 
           FL_AUT_NOSOURCE_STRICT = .FALSE.
      END IF
      CALL GETENVAR ( 'PIMAVAR_SOU_RENAME',          STR_SOU_RENAME )
      IF ( ILEN(STR_SET_FRG) .NE. 0 ) THEN
           CALL CHIN ( STR_SET_FRG, SET_FRG )
      END IF
      CALL GETENVAR ( 'PIMAVAR_PRINT_UV_INFO',  STR_FL_PRINT_UV_INFO )
      CALL TRAN     ( 11, STR_FL_PRINT_UV_INFO, STR_FL_PRINT_UV_INFO )
      IF ( STR_FL_PRINT_UV_INFO == 'YES' ) THEN
           FL_PRINT_UV_INFO = .TRUE.
         ELSE 
           FL_PRINT_UV_INFO = .FALSE.
      END IF
!
      CALL TRAN ( 11, STR_KEEP_FRG, STR_KEEP_FRG )
!
! --- Get the number of keys from each original FITS-IDI files to be processed
!
      DO 410 J1=1,PIM%L_FIL
         CALL ERR_PASS   ( IUER, IER )
         CALL FFITS_OPEN ( PIM%FILE(J1)%NAME, PIM%FILE(J1)%FITS_DESC, 'OLD', &
     &                     IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7101, IUER, 'PIMA_INDX', 'Error in an attempt '// &
     &            'to open FITS UV-file '//PIM%FILE(J1)%NAME )
              RETURN
         END IF
!
         CALL ERR_PASS       ( IUER, IER )
         CALL FFITS_GET_KEYP ( PIM%FILE(J1)%FITS_DESC, PIM__MHDR, PIM__MKWD, &
     &                         PIM%FILE(J1)%M_KWD, PIM%FILE(J1)%L_HDR, &
     &                         PIM%FILE(J1)%L_KWD, PIM%FILE(J1)%KEY, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7102, IUER, 'PIMA_INDX', 'Error in an attempt '// &
     &            'to get keys from FITS UV-file '//PIM%FILE(J1)%NAME )
              RETURN
         END IF
!
         CALL ERR_PASS    ( IUER, IER )
         CALL FFITS_CLOSE ( PIM%FILE(J1)%FITS_DESC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7103, IUER, 'PIMA_INDX', 'Error in an '// &
     &            'attempt to close FITS UV-file '//PIM%FILE(J1)%NAME )
              RETURN
         END IF
 410  CONTINUE
!
! --- Extract source names in separate array in order to facilitate
! --- source name search in the future
!
      DO 420 J2=1,L_CSO
         C_SOU_DB(J2)    = SOU_CAT(J2)%DB_NAME(1:16)
         C_SOU_J2000(J2) = SOU_CAT(J2)%J2000_NAME(1:10)
         C_SOU_B1950(J2) = SOU_CAT(J2)%B1950_NAME(1:8)
         C_SOU_IVS(J2)   = SOU_CAT(J2)%IVS_NAME(1:8)
 420  CONTINUE
!
      ISOU_REN = 0
      IF ( ILEN(STR_SOU_RENAME) .NE. 0 ) THEN
!
! -------- Parse the source name substitution PIMA environment variable
! -------- PIMAVAR_SOU_RENAME consists of one or more substring separated
! -------- with & character.
! -------- Each substring has a form @OldName@NewName@
! -------- PIMA_INDX fills a  pair of arrays SOU_REN_OLD/SOU_REN_OLD of 
! -------- dimension ISOU_REN
!
           IB = 1
           IE = INDEX ( STR_SOU_RENAME(IB:), '&' ) 
           IF ( IE < IB ) IE = ILEN(STR_SOU_RENAME) 
           DO 430 J3=1,PIM__MSOU
              ID1 = MULTI_INDEX ( 1, STR_SOU_RENAME(IB:IE), '@' )
              ID2 = MULTI_INDEX ( 2, STR_SOU_RENAME(IB:IE), '@' )
              ID3 = MULTI_INDEX ( 3, STR_SOU_RENAME(IB:IE), '@' )
              IF ( ID1 < 1 .OR. ID2 < 1 .OR. ID3 < 1 ) THEN
                   CALL ERR_LOG ( 7104, IUER, 'PIMA_INDX', 'Error in '// &
     &                 'parsing substring '//STR_SOU_RENAME(IB:IE)//' of the '// &
     &                 'variable PIMAVAR_SOU_RENAME -- the substring should '// &
     &                 'have @ character three times' )
                   RETURN 
              END IF
              IF ( ID2 .LE. ID1 + 1 .OR. ID3 .LE. ID2 + 1 ) THEN
                   CALL ERR_LOG ( 7105, IUER, 'PIMA_INDX', 'Error in '// &
     &                 'parsing substring '//STR_SOU_RENAME(IB:IE)//' of the '// &
     &                 'variable PIMAVAR_SOU_RENAME -- the substring has '// &
     &                 '@@ inside which prevents its parsing' )
                   RETURN 
              END IF
              ISOU_REN = ISOU_REN + 1
              CALL CLRCH ( SOU_REN_OLD(ISOU_REN) )
              CALL CLRCH ( SOU_REN_NEW(ISOU_REN) )
              SOU_REN_OLD(ISOU_REN) = STR_SOU_RENAME(ID1+IB:ID2+IB-2)
              SOU_REN_NEW(ISOU_REN) = STR_SOU_RENAME(ID2+IB:ID3+IB-2)
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                   WRITE ( 6, 119 ) ISOU_REN, SOU_REN_OLD(ISOU_REN), SOU_REN_NEW(ISOU_REN)
 119               FORMAT ( 'PIMA_INDX  PIMAVAR_SOU_RENAME ', I2,' old name: ', A, ' new name: ', A )
              END IF
!
! ----------- Check whether we need to process the next substition substring
!
              IE = INDEX ( STR_SOU_RENAME(IB+1:), '&' ) + IB + 1
              IF ( IE < IB + 2 ) GOTO 830
!
              IB = IE
              IE = INDEX ( STR_SOU_RENAME(IB+1:), '&' ) + IB
              IF ( IE < IB + 1 ) IE = ILEN(STR_SOU_RENAME) 
 430       CONTINUE 
 830       CONTINUE 
      END IF
!
! --- Get the string GENERATOR. This will be used further to distinguish
! --- several flavours of FITS-IDI files produced by correlators
!
      CALL CLRCH ( PIM%GENERATOR )
      PIM%GENERATOR = 'Undefined'
      IER = 0
      CALL PIMA_GET_KEY_CH ( PIM, 1, ' ', 'ORIGIN', STR, IER )
      IF ( IER .EQ. 0 ) THEN
           IF ( STR(1:4) == 'AIPS' ) THEN
                PIM%GENERATOR = 'AIPS'
              ELSE
                PIM%GENERATOR = STR
           END IF
         ELSE
           IER = 0
           CALL PIMA_GET_KEY_CH ( PIM, 1, ' ', 'ARRNAM', STR, IER )
           IF ( IER == 0 .AND. ILEN(STR) > 0 ) THEN
                PIM%GENERATOR = STR
              ELSE 
                IER = 0
                CALL PIMA_GET_KEY_CH ( PIM, 1, ' ', 'CORRELAT', STR, IER )
                PIM%GENERATOR = STR
           END IF
      END IF
!
! --- Get correlator name and correlator version
!
      IER = 0
      CALL PIMA_GET_KEY_CH ( PIM, 1, ' ', 'CORRELAT', STR, IER )
      IF ( IER .EQ. 0 ) THEN
           PIM%CORR_NAME = STR
           IF ( STR(1:3)  == 'ASC' ) PIM%GENERATOR = 'ASC'
           IF ( STR(1:32) == 'SFXC' ) PIM%GENERATOR = 'SFXC'
         ELSE
           CALL PIMA_GET_KEY_CH ( PIM, 1, ' ', 'TELESCOP', STR, IER )
           IF ( IER .EQ. 0 ) THEN
                PIM%CORR_NAME = STR
              ELSE
                PIM%CORR_NAME = 'Unknown '
           END IF
      END IF
!
      IER = 0
      CALL PIMA_GET_KEY_CH ( PIM, 1, ' ', 'FXCORVER', STR, IER )
      IF ( IER .EQ. 0 ) THEN
           PIM%CORR_VERS = STR
         ELSE
           PIM%CORR_VERS = 'Unknown '
      END IF
!
      IF ( PIM%GENERATOR == 'AIPS' ) THEN
           TABLE_NAME = 'AIPS AT '
         ELSE
           TABLE_NAME = 'ARRAY_GEOMETRY'
      END IF
!
! --- Get the time system  TAI or pseudo-UTC
!
      IER = 0
      CALL PIMA_GET_KEY_CH ( PIM, 1, TABLE_NAME, 'TIMSYS', &
     &                       STR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_GET_KEY_CH ( PIM, 1, TABLE_NAME, 'TIMESYS', &
     &                            STR, IER )
      END IF
      IF ( IER == 0 ) THEN
           PIM%TIM_SCL = PIMA__UTC ! Set default time scale
         ELSE
           IF ( STR(1:3) == 'UTC' ) THEN
                PIM%TIM_SCL = PIMA__UTC
              ELSE IF ( STR(1:3) == 'TAI' .OR. STR(1:3) == 'IAT' ) THEN
                PIM%TIM_SCL = PIMA__TAI
              ELSE
                CALL ERR_LOG ( 7104, IUER, 'PIMA_INDX', 'Unrecoznized '// &
     &              'value of TIMSYS/TIMEYS in the ARRAY_GEOMETRY section: '// &
     &               STR(1:8)//' while UTC, TAI, or IAT were expected' )
                RETURN
           END IF
      END IF
!
! --- Get the reference frequency of the first file
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_KEY_R8 ( PIM, 1, TABLE_NAME, 'REF_FREQ', &
     &                       PIM%REF_FREQ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7105, IUER, 'PIMA_INDX', 'Failure to get '// &
     &         'the value of reference frequency REF_FREQ' )
           RETURN
      END IF
!
! --- Get the spectral channel width
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_KEY_R8 ( PIM, 1, TABLE_NAME, 'CHAN_BW', &
     &                       PIM%CHAN_BW, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7106, IUER, 'PIMA_INDX', 'Failure to get '// &
     &         'the value of reference frequency CHAN_BW' )
           RETURN
      END IF
!
! --- Get the number of spectral channels in one IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_KEY_I4 ( PIM, 1, TABLE_NAME, 'NO_CHAN', &
     &                       PIM%NCHN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7107, IUER, 'PIMA_INDX', 'Failure to get '// &
     &         'the the number of spectral channels' )
           RETURN
      END IF
!
! --- Get the observation code from the FITS-IDI file
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_KEY_CH ( PIM, 1, TABLE_NAME, 'OBSCODE', &
     &                       PIM%OBS_CODE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7108, IUER, 'PIMA_INDX', 'Failure to get '// &
     &         'the observing session code' )
           RETURN
      END IF
!
      IF ( PIM%GENERATOR == 'AIPS' ) THEN
           TABLE_NAME = 'AIPS AN '
         ELSE
           TABLE_NAME = 'ARRAY_GEOMETRY'
      END IF
!
! --- Get the TAI minus UTC value which will be used later for converting
! --- pseudo-UTC into TAI time
!
      IF ( STR_UTC_MINUS_TAI_VALUE .NE. "" ) THEN
           READ ( UNIT=STR_UTC_MINUS_TAI_VALUE, FMT='(F12.2)' ) PIM%UTC_MTAI
        ELSE
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_GET_KEY_R8 ( PIM, 1, TABLE_NAME, 'IATUTC', VAL, IER )
           PIM%UTC_MTAI = -VAL
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7109, IUER, 'PIMA_INDX', 'Failure to get '// &
     &              'the calue of funtion TAI - UTC' )
                RETURN
           END IF
!
           IF ( VAL < -1.0 .OR. VAL > 60.0 ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR(1:15), FMT='(1PD15.8)' ) VAL
                CALL ERR_LOG ( 7110, IUER, 'PIMA_INDX', 'Wrong value of '// &
     &               'IATUTC keyword -(UTC - TAI): '//STR )
                RETURN
           END IF
      END IF
!
      IF ( PIM%GENERATOR == 'AIPS' ) THEN
           TABLE_NAME = 'AIPS AT '
         ELSE
           TABLE_NAME = 'UV_DATA'
      END IF
!
! --- Get the so-called "reference pixel" for UV data
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_KEY_R8 ( PIM, 1, TABLE_NAME, 'REF_PIXL', &
     &                       PIM%REF_PIXL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7111, IUER, 'PIMA_INDX', 'Failure to get '// &
     &         'the reference pixel' )
           RETURN
      END IF
!
! --- Get the STK_1 the first Stokes parameter coordinate value
!
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_GET_KEY_I4 ( PIM, 1, TABLE_NAME, 'STK_1', &
     &                       PIM%STK_1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7112, IUER, 'PIMA_INDX', 'Failure to get '// &
     &         'the reference pixel' )
           RETURN
      END IF
!
      IF ( PIM%GENERATOR == 'AIPS' ) THEN
           PIM%VIS_SCAL = 1.0D0
         ELSE IF ( PIM%GENERATOR == 'EVN' ) THEN
           PIM%VIS_SCAL = 1.0D0
         ELSE IF ( PIM%GENERATOR == 'SFXC' ) THEN
           PIM%VIS_SCAL = 1.0D0
         ELSE IF ( PIM%GENERATOR == 'SHAO Correlator' ) THEN
           PIM%VIS_SCAL = 1.0D0
         ELSE IF ( PIM%GENERATOR == 'PARSEK' ) THEN
           PIM%VIS_SCAL = 1.0D0
         ELSE IF ( PIM%GENERATOR == 'ASC' ) THEN
           PIM%VIS_SCAL = 1.0D0
         ELSE
!
! -------- Get the visibility scaling parameter. Its origin is unclear.
!
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_GET_KEY_R8 ( PIM, 1, 'UV_DATA', 'VIS_SCAL', &
     &                            PIM%VIS_SCAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7113, IUER, 'PIMA_INDX', 'Failure to get '// &
     &              'the visibility scaling factor. Generator: '// &
     &              PIM%GENERATOR )
                RETURN
           END IF
      END IF
      IF ( ILEN(PIM%CONF%MKDB_2ND_BAND_FILE) > 0 ) THEN
           PIM%NBND = 2
         ELSE
           PIM%NBND = 1
      END IF
!
      IF ( ILEN(STR_FREQ_REPL) > 0 ) THEN
           IB = INDEX ( STR_FREQ_REPL(2:), '@' ) + 1
           IF ( IB < 2 .OR. IB > ILEN(STR_FREQ_REPL)-2 ) THEN
                CALL ERR_LOG ( 7114, IUER, 'PIMA_INDX', 'Error '// &
     &              'in syntax of PIMAVAR_FREQ_REPL: '// &
     &               STR_FREQ_REPL )
                RETURN
           END IF
           READ ( UNIT=STR_FREQ_REPL(2:IB-1), FMT='(F20.0)', IOSTAT=IOS ) &
     &            FREQ_ORIG
           IF ( IOS .NE. 0 ) THEN
                CALL ERR_LOG ( 7115, IUER, 'PIMA_INDX', 'Error '// &
     &              'in syntax of PIMAVAR_FREQ_REPL: '// &
     &               STR_FREQ_REPL(1:I_LEN(STR_FREQ_REPL))// &
     &              ' -- cannot decode freq_orig: '// &
     &               STR_FREQ_REPL(2:IB-1) )
                RETURN
           END IF
           READ ( UNIT=STR_FREQ_REPL(IB+1:ILEN(STR_FREQ_REPL)-1), &
     &            FMT='(F20.0)', IOSTAT=IOS ) FREQ_REPL
           IF ( IOS .NE. 0 ) THEN
                CALL ERR_LOG ( 7116, IUER, 'PIMA_INDX', 'Error '// &
     &              'in syntax of PIMAVAR_FREQ_REPL: '// &
     &               STR_FREQ_REPL(1:I_LEN(STR_FREQ_REPL))// &
     &              ' -- cannot decode freq_orig: '// &
     &               STR_FREQ_REPL(IB+1:) )
                RETURN
           END IF
      END IF
!
      ALLOCATE ( ANT(PIM%L_FIL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7117, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &                   'dynamic memory for data structure ANT' )
           RETURN 
      END IF
!
! --- Initializations
!
      KEP = 0
      NSOU_ORIG = 0
      K_BAD_SOU = 0
      PIM%TAPER_FN = 'UKNOWN  '
      PIM%NFRQ = 0
      PIM%NFRG = 0
      PIM%NPOL = 1
      PIM%NUV  = 0
      PIM%NSTA = 0
      NLEV_UNSORTED = 2
      FL_WEA_TAB  = .FALSE.
      FL_MOD_TAB  = .FALSE.
      FL_FLAG_TAB = .FALSE.
      FL_MDC_TAB  = .FALSE.
      IND_NLEV    = 0
      IND_POL_TYPA = 0
      IND_POL_TYPB = 0
      IND_POL_ANG  = 0
      IND_NLEV     = 0
      C_SOU_ORIG   = '                '
      L_FRQ = 0
!
! --- Now again go through all files and collect indexes of tables and
! --- keywords.
!
      FL_FAILURE = .FALSE.
      L_SWAP = 0
      DO 440 J4=1,PIM%L_FIL
         IND_SOU_TAB(J4) = 0
         IND_AT_TAB      = 0
         IND_NX_TAB      = 0
         IND_GC_TAB(J4)  = 0
         IND_CL_TAB(J4)  = 0
         IND_ANT_TAB(J4) = 0
         IND_FRQ_TAB(J4) = 0
         IND_UV_TAB(J4)  = 0
         IND_FLG_TAB(J4) = 0
         IND_WEA_TAB(J4) = 0
         IND_MOD_TAB(J4) = 0
         IND_MDC_TAB(J4) = 0
         IND_MDC_ATM(J4) = 0
         IND_MDC_ATD(J4) = 0
         IND_MDC_GDL(J4) = 0
         IND_MDC_GRT(J4) = 0
         IND_MOD_GDELAY(J4) = 0
         IND_MOD_GRATE(J4)  = 0
         IND_MOD_PRATE(J4)  = 0
         IND_MOD_PDELAY(J4) = 0
         IND_MOD_FRG(J4)    = 0
         IND_STA_COO(J4)    = 0
         IND_STA_VEL(J4)    = 0
         IND_ANT_NAME = 0
         IND_STA_NAME = 0
         IND_ANT_IND  = 0
         IND_SOU_NAM  = 0
         IND_SOU_ALP  = 0
         IND_SOU_DEC  = 0
         IND_SOU_IND  = 0
         IND_FRQ_REF  = 0
         IND_FRQ_BWID = 0
         IND_FRQ_CWID = 0
         IND_FRQ_SBND = 0
         IND_FRQ_IBBC = 0
         IND_FRQ_NFRQ = 0
         IND_FRQ_BFRQ = 0
         CALL NOUT_I4 ( PIM__MUVT, IND_UV_DAT(1,J4) )
         CALL NOUT_I4 ( PIM__MUVT, IND_UV_DAT(1,J4) )
         CALL NOUT_I4 ( PIM__MUVT, IND_UV_TIM(1,J4) )
         CALL NOUT_I4 ( PIM__MUVT, IND_UV_BAS(1,J4) )
         CALL NOUT_I4 ( PIM__MUVT, IND_UV_SOU(1,J4) )
         CALL NOUT_I4 ( PIM__MUVT, IND_UV_NX2(1,J4) )
!
         IND_FLAG_SOURCE_ID(J4)  = 0
         IND_FLAG_STATION_ID(J4) = 0
         IND_FLAG_FRQ_ID(J4)     = 0
         IND_FLAG_TIME_RANGE(J4) = 0
         IND_FLAG_SEVERITY(J4)   = 0
         IND_FLAG_REASON(J4)     = 0
!
! ------ Open the J4 -th FITS-IDI file
!
         CALL ERR_PASS   ( IUER, IER )
         CALL FFITS_OPEN ( PIM%FILE(J4)%NAME, PIM%FILE(J4)%FITS_DESC, 'OLD', &
     &                     IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7118, IUER, 'PIMA_INDX', 'Error in an attempt '// &
     &            'to open FITS UV-file '//PIM%FILE(J4)%NAME )
              RETURN
         END IF
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              WRITE ( 6, 210 ) J4, PIM%L_FIL, &
     &                PIM%CONF%UVFILE_NAME(J4)(1:I_LEN(PIM%CONF%UVFILE_NAME(J4)))
 210          FORMAT ( 'PIMA_INDX: read file ',I3,' ( ',I3,' )  ',A )
         END IF
!
! ------ Scan all tables
!
         DO 470 J7=1,PIM%FILE(J4)%L_HDR
            CALL CLRCH ( EXT_NAME )
            DO 480 J8=1,PIM%FILE(J4)%L_KWD(J7)
               IF ( PIM%FILE(J4)%KEY(J8,J7)(1:8) == 'EXTNAME ' ) THEN
                    EXT_NAME = PIM%FILE(J4)%KEY(J8,J7)(11:)
                 ELSE IF ( PIM%FILE(J4)%KEY(J8,J7)(1:8)== "NO_CHAN " ) THEN
                    READ ( UNIT=PIM%FILE(J4)%KEY(J8,J7)(21:30), &
     &                     FMT='(I10)', IOSTAT=IER ) NO_CHAN
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 7119, IUER, 'PIMA_INDX', &
     &                       'Failure to get the the number '// &
     &                       'of spectral channels from file '// &
     &                        PIM%FILE(J4)%KEY(J8,J7) )
                         RETURN
                    END IF
                    IF ( NO_CHAN .NE. PIM%NCHN ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( PIM%NCHN, STR )
                         CALL ERR_LOG ( 7120, IUER, 'PIMA_INDX', &
     &                       'The number of spectral channels, '// &
     &                        PIM%FILE(J4)%KEY(J8,J7)(21:30)// &
     &                       ' has been changed in file '// &
     &                        PIM%FILE(J4)%NAME(1:I_LEN(PIM%FILE(J4)%NAME))// &
     &                       ' from '//STR )
                         RETURN
                    END IF
                 ELSE IF ( PIM%FILE(J4)%KEY(J8,J7)(1:8)== "HISTORY " ) THEN
                    IF ( PIM%FILE(J4)%KEY(J8,J7)(9:16)== "CORRVERS" ) THEN
                         PIM%CORR_VERS = PIM%FILE(J4)%KEY(J8,J7)(20:35)
                    END IF
               END IF
 480        CONTINUE
            IF ( EXT_NAME(1:10) ==  "'SOURCE  '" ) THEN
                 IND_SOU_TAB(J4) = J7
               ELSE IF ( EXT_NAME(1:10) == "'AIPS SU '" ) THEN
                 IND_SOU_TAB(J4) = J7
               ELSE IF ( EXT_NAME(1:10) == "'ANTENNA '" ) THEN
                 IND_ANT_TAB(J4) = J7
               ELSE IF ( EXT_NAME(1:10) == "'AIPS AT '" ) THEN
                 IND_ANT_TAB(J4) = J7
               ELSE IF ( EXT_NAME(1:10) == "'FLAG    '" ) THEN
                 IND_FLG_TAB(J4) = J7
                 FL_FLAG_TAB = .TRUE.
               ELSE IF ( EXT_NAME(1:13) == "'MODEL_COMPS'" ) THEN
                 IND_MDC_TAB(J4) = J7
                 FL_MDC_TAB = .TRUE.
               ELSE IF ( EXT_NAME(1:22) == "'INTERFEROMETER_MODEL'" .OR. &
     &                   EXT_NAME(1:23) == "'INTERFEROMETER_ MODEL'"     ) THEN
                 IND_MOD_TAB(J4) = J7
                 FL_MOD_TAB = .TRUE.
                 IF ( STR_NO_IM(1:1) == 'Y' .OR. STR_NO_IM(1:1) == 'y' ) THEN
                      FL_MOD_TAB = .FALSE.
                 END IF
               ELSE IF ( EXT_NAME(1:10) == "'WEATHER '" ) THEN
                 IND_WEA_TAB(J4) = J7
                 FL_WEA_TAB = .TRUE.
               ELSE IF ( EXT_NAME(1:10) == "'UV_DATA '" ) THEN
                 PIM%FILE(J4)%NUM_UV_TAB = PIM%FILE(J4)%NUM_UV_TAB + 1
                 PIM%FILE(J4)%IND_UV_TAB(PIM%FILE(J4)%NUM_UV_TAB) = J7
                 IND_UV_TAB(J4) = J7
               ELSE IF ( EXT_NAME(1:10) == "'AIPS UV '" ) THEN
                 PIM%FILE(J4)%NUM_UV_TAB = PIM%FILE(J4)%NUM_UV_TAB + 1
                 PIM%FILE(J4)%IND_UV_TAB(PIM%FILE(J4)%NUM_UV_TAB) = J7
                 IND_UV_TAB(J4) = J7
               ELSE IF ( EXT_NAME(1:11) == "'FREQUENCY'" ) THEN
                 IND_FRQ_TAB(J4) = J7
               ELSE IF ( EXT_NAME(1:11) == "'AIPS FQ '" ) THEN
                 IND_FRQ_TAB(J4) = J7
               ELSE IF ( EXT_NAME(1:10) == "'AIPS NX '" ) THEN
                 IND_NX_TAB = J7
               ELSE IF ( EXT_NAME(1:10) == "'AIPS AT '" ) THEN
                 IND_AT_TAB = J7
               ELSE IF ( EXT_NAME(1:10) == "'AIPS GC '" ) THEN
                 IND_GC_TAB(J4) = J7
               ELSE IF ( EXT_NAME(1:10) == "'AIPS CL '" ) THEN
                 IND_CL_TAB(J4) = J7
               ELSE IF ( EXT_NAME(1:16) == "'ARRAY_GEOMETRY'" ) THEN
                 IND_ARG_TAB(J4) = J7
            END IF
!
            DO 490 J9=1,PIM%FILE(J4)%L_KWD(J7)
               IF ( IND_FLG_TAB(J4) == J7 ) THEN
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'SOURCE_ID'" ) > 0 ) THEN
                         IND_FLAG_SOURCE_ID(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'ANTS    '" ) > 0 ) THEN
                         IND_FLAG_STATION_ID(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'FREQID  '" ) > 0 ) THEN
                         IND_FLAG_FRQ_ID(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'TIMERANG'" ) > 0 ) THEN
                         IND_FLAG_TIME_RANGE(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'SEVERITY'" ) > 0 ) THEN
                         IND_FLAG_SEVERITY(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'REASON  '" ) > 0 ) THEN
                         IND_FLAG_REASON(J4) = J9
                    END IF
               END IF
               IF ( IND_MOD_TAB(J4) == J7 ) THEN
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'TIME    '" ) > 0 ) THEN
                         IND_MOD_TIME(J4) =J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'TIME_INTERVAL'" ) > 0 ) THEN
                         IND_MOD_TIME_INTV(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'SOURCE_ID'" ) > 0 ) THEN
                         IND_MOD_SOU_ID(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'ANTENNA_NO'" ) > 0 ) THEN
                         IND_MOD_ANT_ID(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'FREQID  '" ) > 0 ) THEN
                         IND_MOD_FRQ_ID(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'FREQ.VAR'" ) > 0 ) THEN
                         IND_MOD_FRQVAR(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'I.FAR.ROT'" ) > 0 ) THEN
                         IND_MOD_FARROT(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'GDELAY_1'" ) > 0 ) THEN
                         IND_MOD_GDELAY(J4) = J9
!
! ---------------------- Get dimension of the array
!
                         STR = PIM%FILE(J4)%KEY(IND_MOD_GDELAY(J4)+1,J7)(12:14)
                         IP = INDEX ( STR, 'D' )
                         IF ( IP < 1 ) THEN
                              CALL ERR_LOG ( 7121, IUER, 'PIMA_INDX', &
     &                            'Cannot find length of GDELAY_1' )
                              RETURN
                         END IF
                         CALL CHIN ( STR(1:IP-1), LEN_MOD_GDELAY(J4) )
                         IF ( LEN_MOD_GDELAY(J4) > PIM__MDPL*PIM__MFRQ ) THEN
                              CALL CLRCH ( STR )
                              CALL INCH  ( LEN_MOD_GDELAY(J4), STR )
                              WRITE ( 6, * ) ' PIM__MDPL = ', ' PIM__MFRQ= ', PIM__MFRQ
                              CALL ERR_LOG ( 7122, IUER, 'PIMA_INDX', &
     &                            'Lenth of GDELAY_1 is too big: '// &
     &                             STR(1:I_LEN(STR))//' -- parameter '// &
     &                            ' PIM__MDPL should be raised'  )
                              RETURN
                         END IF
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'PDELAY_1'" ) > 0 ) THEN
                         IND_MOD_PDELAY(J4) = J9
!
! ---------------------- Get dimension of the array
!
                         STR = PIM%FILE(J4)%KEY(IND_MOD_PDELAY(J4)+1,J7)(12:16)
                         IP = INDEX ( STR, 'D' )
                         IF ( IP < 1 ) THEN
                              CALL ERR_LOG ( 7123, IUER, 'PIMA_INDX', &
     &                            'Cannot find length of PDELAY_1 STR='// &
     &                            STR(1:I_LEN(STR)) )
                              RETURN
                         END IF
                         CALL CHIN ( STR(1:IP-1), LEN_MOD_PDELAY(J4) )
                         IF ( LEN_MOD_PDELAY(J4) > PIM__MDPL*PIM__MFRQ ) THEN
                              CALL CLRCH ( STR )
                              CALL INCH  ( LEN_MOD_PDELAY(J4), STR )
                              CALL ERR_LOG ( 7124, IUER, 'PIMA_INDX', &
     &                            'Lenth of PDELAY_1 is too big: '// &
     &                             STR(1:I_LEN(STR))//' -- parameter '// &
     &                            ' PIM__MDEL should be raised'  )
                              RETURN
                         END IF
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'GRATE_1 '" ) > 0 ) THEN
                         IND_MOD_GRATE(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'FREQID  '" ) > 0 ) THEN
                         IND_MOD_FRG(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'PRATE_1 '" ) > 0 ) THEN
                         IND_MOD_PRATE(J4) = J9
!
! ---------------------- Get dimension of the array
!
                         STR = PIM%FILE(J4)%KEY(IND_MOD_PRATE(J4)+1,J7)(12:16)
                         IP = INDEX ( STR, 'D' )
                         IF ( IP < 1 ) THEN
                              CALL ERR_LOG ( 7125, IUER, 'PIMA_INDX', &
     &                            'Cannot find length of PRATE_1' )
                              RETURN
                         END IF
                         CALL CHIN ( STR(1:IP-1), LEN_MOD_PRATE(J4) )
                         IF ( LEN_MOD_PRATE(J4) > PIM__MDPL*PIM__MFRQ ) THEN
                              CALL CLRCH ( STR )
                              CALL INCH  ( LEN_MOD_PRATE(J4), STR )
                              CALL ERR_LOG ( 7126, IUER, 'PIMA_INDX', &
     &                            'Lenth of PRATE_1 is too big: '// &
     &                             STR(1:I_LEN(STR))//' -- parameter '// &
     &                            ' PIM__MDEL should be raised'  )
                              RETURN
                         END IF
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'DISP_1  '" ) > 0 ) THEN
                         IND_MOD_DISP(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'DDISP_1 '" ) > 0 ) THEN
                         IND_MOD_DDISP(J4) = J9
                    END IF
               END IF
!
               IF ( IND_WEA_TAB(J4) == J7 ) THEN
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'TIME    '" ) > 0 ) THEN
                         IND_WEA_TIME(J4) =J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'TIME_INTERVAL'" ) > 0 ) THEN
                         IND_WEA_TIME_INTV(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'ANTENNA_NO'" ) > 0 ) THEN
                         IND_WEA_ANT_ID(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'TEMPERATURE'" ) > 0 ) THEN
                         IND_WEA_TEMP(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'PRESSURE'" ) > 0 ) THEN
                         IND_WEA_PRES(J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'DEWPOINT'" ) > 0 ) THEN
                         IND_WEA_DEW(J4) = J9
                    END IF
               END IF
!
               IF ( PIM%FILE(J4)%KEY(J9,J7)(1:8) == 'EXTNAME ' ) THEN
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:21) == "'FREQUENCY'" ) THEN
                         IND_FRQ_TAB(J4) = J7
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:21) == "'AIPS FQ '" ) THEN
                         IND_FRQ_TAB(J4) = J7
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'AIPS NX '" ) THEN
                         IND_NX_TAB = J7
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'AIPS AT '" ) THEN
                         IND_AT_TAB = J7
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'AIPS GC '" ) THEN
                         IND_GC_TAB(J4) = J7
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'AIPS CL '" ) THEN
                         IND_CL_TAB(J4) = J7
                    END IF
               END IF
!
               IF ( PIM%FILE(J4)%KEY(J9,J7)(1:8) == 'NAXIS2  ' ) THEN
                    IND_NAXIS2 = J9
               END IF
!
               IF ( IND_ANT_TAB(J4) == J7 ) THEN
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'ANNAME  '" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'ANNAME          '" ) > 0 ) THEN
                         IND_ANT_NAME = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'ANTENNA_NO'" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'ANTENNA_NO      '" ) > 0 ) THEN
                         IND_ANT_IND  = J9
                    END IF
               END IF
!
               IF ( IND_ARG_TAB(J4) == J7 ) THEN
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'ANNAME  '" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'ANNAME          '" ) > 0 ) THEN
                         IND_STA_NAME = J9
                    END IF
               END IF
!
               IF ( IND_SOU_TAB(J4) == J7 ) THEN
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'SOURCE  '" ) > 0 .AND. &
     &                   PIM%FILE(J4)%KEY(J9,J7)(1:5) == 'TTYPE' ) THEN
                         IND_SOU_NAM = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'SOURCE          '" ) > 0 .AND. &
     &                   PIM%FILE(J4)%KEY(J9,J7)(1:5) == 'TTYPE' ) THEN
                         IND_SOU_NAM = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'RAEPO   '" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'RAEPO           '" ) > 0 ) THEN
                         IND_SOU_ALP = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'DECEPO  '" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'DECEPO          '" ) > 0 ) THEN
                         IND_SOU_DEC = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'ID_NO.  '" ) > 0 ) THEN
                         IND_SOU_IND = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'SOURCE_ID'" ) > 0 ) THEN
                         IND_SOU_IND = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'ID. NO.         '" ) > 0 ) THEN
                         IND_SOU_IND = J9
                    END IF
               END IF
               IF ( IND_UV_TAB(J4) == J7 ) THEN
                    IND_UV_NX2(PIM%FILE(J4)%NUM_UV_TAB,J4) = IND_NAXIS2
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'DATE    '" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'DATE            '" ) > 0 ) THEN
                         IND_UV_DAT(PIM%FILE(J4)%NUM_UV_TAB,J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'TIME    '" ) > 0 ) THEN
                         IND_UV_TIM(PIM%FILE(J4)%NUM_UV_TAB,J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'BASELINE'" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'BASELINE        '" ) > 0 ) THEN
                         IND_UV_BAS(PIM%FILE(J4)%NUM_UV_TAB,J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'SOURCE  '" ) > 0         .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'SOURCE          '" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'SOURCE_ID'" ) > 0 ) THEN
                         IND_UV_SOU(PIM%FILE(J4)%NUM_UV_TAB,J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'FREQID  '" ) > 0 ) THEN
                         IND_UV_FRQ(PIM%FILE(J4)%NUM_UV_TAB,J4) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'INTTIM  '" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'INTTIM          '" ) > 0 ) THEN
                         PIM%FILE(J4)%IND_INTTIM_KEY(PIM%FILE(J4)%NUM_UV_TAB) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'FLUX    '" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'VISIBILITIES    '" ) > 0 ) THEN
                         PIM%FILE(J4)%IND_FLUX_KEY(PIM%FILE(J4)%NUM_UV_TAB) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'WEIGHT  '" ) > 0 ) THEN
                         PIM%FILE(J4)%IND_WEIGHT_KEY(PIM%FILE(J4)%NUM_UV_TAB) = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "MAXIS2  =" ) > 0 ) THEN
                         CALL CHIN ( PIM%FILE(J4)%KEY(J9,J7)(29:30), PIM%NSTK )
                    END IF
               END IF
!
               IF ( IND_CL_TAB(J4) == J7 ) THEN
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'TIME            '" ) > 0 ) THEN
                         IND_UV_TIM(1,J4) = J9
                    END IF
               END IF
               IF ( IND_AT_TAB == J7 ) THEN
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'FREQID          '" ) > 0 ) THEN
                         IND_UV_FRQ(1,J4) = J9
                    END IF
               END IF
!
               IF ( IND_FRQ_TAB(J4) == J7 ) THEN
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "NAXIS2  =" ) > 0 ) THEN
                         READ ( UNIT=PIM%FILE(J4)%KEY(J9,J7)(19:30), &
     &                          FMT='(I12)' ) PIM%FILE(J4)%NFRG
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "NO_BAND " ) > 0 ) THEN
                         IND_FRQ_NFRQ = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "REF_FREQ" ) > 0 ) THEN
                         IND_FRQ_REF  = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'BANDFREQ'" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'IF FREQ         '" ) > 0 ) THEN
                         IND_FRQ_BFRQ = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'TOTAL_BANDWIDTH'"  ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'TOTAL BANDWIDTH '" ) > 0 ) THEN
                         IND_FRQ_BWID = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'CH_WIDTH'" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'CH WIDTH        '" ) > 0 ) THEN
                         IND_FRQ_CWID = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'SIDEBAND'" ) > 0 .OR. &
     &                   INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'SIDEBAND        '" ) > 0 ) THEN
                         IND_FRQ_SBND = J9
                    END IF
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "'BB_CHAN '" ) > 0 ) THEN
                         IND_FRQ_IBBC = J9
                    END IF
               END IF
!
               IF ( IND_AT_TAB == J7 ) THEN
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "REF_FREQ" ) > 0 ) THEN
                         IND_FRQ_REF  = J9
                    END IF
               END IF
               IF ( PIM%GENERATOR == 'AIPS'  .AND.  IND_GC_TAB(J4) == J7 ) THEN
                    IF ( INDEX ( PIM%FILE(J4)%KEY(J9,J7), "NO_BAND " ) > 0 ) THEN
                         IND_FRQ_NFRQ = J9
                    END IF
               END IF
!
               IF ( IND_MDC_TAB(J4) == J7 ) THEN
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'TIME    '" ) THEN
                         IND_MDC_TIME(J4) = J9
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:22) == "'ANTENNA_NO'" ) THEN
                         IND_MDC_ANT_ID(J4) = J9
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:21) == "'SOURCE_ID'" ) THEN
                         IND_MDC_SOU_ID(J4) = J9
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'CLOCK_1 '" ) THEN
                         IND_MDC_CLO(J4) = J9
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'DCLOCK_1'" ) THEN
                         IND_MDC_RAT(J4) = J9
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:21) == "'ATMOS   '" ) THEN
                         IND_MDC_ATM(J4) = J9
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'DATMOS  '" ) THEN
                         IND_MDC_ATD(J4) = J9
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'GDELAY  '" ) THEN
                         IND_MDC_GDL(J4) = J9
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'GRATE   '" ) THEN
                         IND_MDC_GRT(J4) = J9
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(1:9) == "TAPER_FN=" ) THEN
                         PIM%TAPER_FN = PIM%FILE(J4)%KEY(J9,J7)(12:19)
                    END IF
!
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(1:9) == "NO_POL  =" ) THEN
                         READ ( UNIT=PIM%FILE(J4)%KEY(J9,J7)(11:30), &
     &                          FMT='(I20)' ) PIM%NPOL
                    END IF
               END IF
               IF ( IND_ANT_TAB(J4) == J7 ) THEN
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:21) == "'NO_LEVELS'" ) THEN
                         IND_NLEV(J4) = J9
                    END IF
               END IF
               IF ( IND_ANT_TAB(J4) == J7 ) THEN
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:21) == "'POLTYA  '" ) THEN
                         IND_POL_TYPA(J4) = J9
                    END IF
               END IF
               IF ( IND_ANT_TAB(J4) == J7 ) THEN
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:21) == "'POLTYB  '" ) THEN
                         IND_POL_TYPB(J4) = J9
                    END IF
               END IF
               IF ( IND_ANT_TAB(J4) == J7 ) THEN
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:21) == "'POLAA   '" ) THEN
                         IND_POL_ANG(J4) = J9
                    END IF
               END IF
               IF ( IND_ARG_TAB(J4) == J7 ) THEN
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'STABXYZ '" ) THEN
                         IND_STA_COO(J4) = J9
                    END IF
                    IF ( PIM%FILE(J4)%KEY(J9,J7)(11:20) == "'DERXYZ  '" ) THEN
                         IND_STA_VEL(J4) = J9
                    END IF
               END IF
 490        CONTINUE
 470     CONTINUE
!
! ------ Check kludge variables and set the minimum and maximum file frequency group
!
         CALL GETENVAR ( 'PIMAVAR_MIN_FRG', STR )
         IF ( ILEN(STR) > 0 ) THEN
              CALL CHIN ( STR, PIM_MIN_FRG )
            ELSE 
              PIM_MIN_FRG = 1
         END IF
!
         CALL GETENVAR ( 'PIMAVAR_MAX_FRG', STR )
         IF ( ILEN(STR) > 0 ) THEN
              CALL CHIN ( STR, PIM_MAX_FRG )
            ELSE 
              PIM_MAX_FRG = PIM__MFRG
         END IF
!
         IF ( PIM%GENERATOR == 'ASC' .AND. PIM%NSTK == 4 ) THEN
              PIM%NPOL = 2 
         END IF
         IF ( PIM%GENERATOR == 'EVN' .AND. PIM%NSTK == 4 ) THEN
              PIM%NPOL = 2 
         END IF
         IF ( PIM%GENERATOR == 'SFXC' .AND. PIM%NSTK == 2 ) THEN
              PIM%NPOL = 2 
         END IF
         IF ( IND_SOU_TAB(J4) == 0 ) THEN
              CALL ERR_LOG ( 7127, IUER, 'PIMA_INDX', 'Source table was '// &
     &            'not found in file '//PIM%FILE(J4)%NAME )
              RETURN
         END IF
         IF ( IND_ANT_TAB(J4) == 0 ) THEN
              CALL ERR_LOG ( 7128, IUER, 'PIMA_INDX', 'Antenna table was '// &
     &            'not found in file '//PIM%FILE(J4)%NAME )
              RETURN
         END IF
         IF ( PIM%FILE(J4)%NUM_UV_TAB == 0 ) THEN
              CALL ERR_LOG ( 7129, IUER, 'PIMA_INDX', 'UV table was '// &
     &            'not found in file '//PIM%FILE(J4)%NAME )
              RETURN
         END IF
         IF ( IND_FRQ_TAB(J4) == 0 ) THEN
              CALL ERR_LOG ( 7130, IUER, 'PIMA_INDX', 'Frequency table was '// &
     &            'not found in file '//PIM%FILE(J4)%NAME )
              RETURN
         END IF
!
         IF ( IND_ANT_NAME == 0 ) THEN
              CALL ERR_LOG ( 7131, IUER, 'PIMA_INDX', 'Keyword ANNAME was '// &
     &            'not found in the antenna table in reading file '// &
     &             PIM%FILE(J4)%NAME )
              RETURN
         END IF
!
         IF ( IND_STA_NAME == 0 ) THEN
              CALL ERR_LOG ( 7132, IUER, 'PIMA_INDX', 'Keyword ANNAME was '// &
     &            'not found in the array geometry table in reading file '// &
     &             PIM%FILE(J4)%NAME )
              RETURN
         END IF
         IF ( IND_ANT_IND == 0 ) THEN           
              CALL ERR_LOG ( 7133, IUER, 'PIMA_INDX', 'Keyword ANTENNA_NO '// &
     &            'was not found in the antenna table in reading file '// &
     &             PIM%FILE(J4)%NAME )
              RETURN
         END IF
         IF ( IND_SOU_NAM == 0 ) THEN
              CALL ERR_LOG ( 7134, IUER, 'PIMA_INDX', 'Keyword SOURCE '// &
     &            'was not found in the source table in reading file '// &
     &             PIM%FILE(J4)%NAME )
              RETURN
         END IF
         IF ( IND_SOU_IND == 0 ) THEN
              CALL ERR_LOG ( 7135, IUER, 'PIMA_INDX', 'Keyword ID_NO. '// &
     &            'was not found in the source table in reading file '// &
     &             PIM%FILE(J4)%NAME )
              RETURN
         END IF
!
         DO 4100 J10=1,PIM%FILE(J4)%NUM_UV_TAB
            IF ( IND_UV_DAT(J10,J4) == 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J10, STR )
                 CALL ERR_LOG ( 7136, IUER, 'PIMA_INDX', 'Keyword DATE '// &
     &               'was not found in the '//STR(1:I_LEN(STR))// &
     &               'th UV_DATA table in reading file '//PIM%FILE(J4)%NAME )
                 RETURN
            END IF
!
            IF ( IND_UV_TIM(J10,J4) == 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J10, STR )
                 CALL ERR_LOG ( 7137, IUER, 'PIMA_INDX', 'Keyword TIME '// &
     &               'was not found in the '//STR(1:I_LEN(STR))// &
     &               'th UV_DATA table in reading file '//PIM%FILE(J4)%NAME )
                 RETURN
            END IF
!
            IF ( IND_UV_BAS(J10,J4) == 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J10, STR )
                 CALL ERR_LOG ( 7138, IUER, 'PIMA_INDX', 'Keyword BASELINE '// &
     &               'was not found in the '//STR(1:I_LEN(STR))// &
     &               'th UV_DATA table in reading file '//PIM%FILE(J4)%NAME )
                 RETURN
            END IF
!
            IF ( IND_UV_SOU(J10,J4) == 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J10, STR )
                 CALL ERR_LOG ( 7139, IUER, 'PIMA_INDX', 'Keyword SOURCE '// &
     &               'was not found in the '//STR(1:I_LEN(STR))// &
     &               'th UV_DATA table in reading file '//PIM%FILE(J4)%NAME )
                 RETURN
            END IF
!
            IF ( IND_UV_FRQ(J10,J4) == 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J10, STR )
                 CALL ERR_LOG ( 7140, IUER, 'PIMA_INDX', 'Keyword FREQID '// &
     &               'was not found in the '//STR(1:I_LEN(STR))// &
     &               'th UV_DATA table in reading file '//PIM%FILE(J4)%NAME )
                 RETURN
            END IF
!
            IF ( IND_UV_NX2(J10,J4) == 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J10, STR )
                 CALL ERR_LOG ( 7141, IUER, 'PIMA_INDX', 'Keyword NAXIS2 '// &
     &               'was not found in the '//STR(1:I_LEN(STR))// &
     &               'th UV_DATA table in reading file '//PIM%FILE(J4)%NAME )
                 RETURN
            END IF
!
            IF ( PIM%FILE(J4)%IND_INTTIM_KEY(J10) == 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J10, STR )
                 CALL ERR_LOG ( 7142, IUER, 'PIMA_INDX', 'Keyword INTTIM '// &
     &               'was not found in the '//STR(1:I_LEN(STR))// &
     &               'th UV_DATA table in reading file '//PIM%FILE(J4)%NAME )
                 RETURN
            END IF
!
            IF ( PIM%FILE(J4)%IND_FLUX_KEY(J10) == 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J10, STR )
                 CALL ERR_LOG ( 7143, IUER, 'PIMA_INDX', 'Keyword FLUX '// &
     &               'was not found in the '//STR(1:I_LEN(STR))// &
     &               'th UV_DATA table in reading file '//PIM%FILE(J4)%NAME )
                 RETURN
            END IF
!
            IF ( PIM%GENERATOR == 'AIPS' ) THEN
                 CONTINUE
               ELSE IF ( PIM%GENERATOR == 'EVN'  .OR. &
     &                   PIM%GENERATOR == 'SFXC' .OR. &
     &                   PIM%GENERATOR == 'ASC'       ) THEN
                 CONTINUE
               ELSE
                 IF ( PIM%FILE(J4)%IND_WEIGHT_KEY(J10) == 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J10, STR )
                      WRITE ( 6, * ) ' PIM%GENERATOR >>'// &
     &                                 PIM%GENERATOR(1:I_LEN(PIM%GENERATOR))//'<< '
                      CALL ERR_LOG ( 7144, IUER, 'PIMA_INDX', 'Keyword '// &
     &                    'WEIGHT was not found in the '//STR(1:I_LEN(STR))// &
     &                    'th UV_DATA table in reading file '// &
     &                    PIM%FILE(J4)%NAME )
                      RETURN
                 END IF
            END IF
 4100    CONTINUE
!
         IF ( IND_FRQ_NFRQ == 0 ) THEN
              CALL ERR_LOG ( 7145, IUER, 'PIMA_INDX', 'Keyword NO_BAND '// &
     &            'was not found in the FREQUENCY table in reading file '// &
     &             PIM%FILE(J4)%NAME )
              RETURN
         END IF
         IF ( IND_FRQ_BFRQ == 0 ) THEN
              CALL ERR_LOG ( 7146, IUER, 'PIMA_INDX', 'Keyword BANDFREQ '// &
     &            'was not found in the FREQUENCY table in reading file '// &
     &             PIM%FILE(J4)%NAME )
              RETURN
         END IF
         IF ( IND_FRQ_BWID == 0 ) THEN
              CALL ERR_LOG ( 7147, IUER, 'PIMA_INDX', 'Keyword TOTAL_BANDWIDTH '// &
     &            'was not found in the FREQUENCY table in reading file '// &
     &             PIM%FILE(J4)%NAME )
              RETURN
         END IF
         IF ( IND_FRQ_CWID == 0 ) THEN
              CALL ERR_LOG ( 7148, IUER, 'PIMA_INDX', 'Keyword CH_WIDTH '// &
     &            'was not found in the FREQUENCY table in reading file '// &
     &             PIM%FILE(J4)%NAME )
              RETURN
         END IF
         IF ( IND_FRQ_SBND == 0 ) THEN
              CALL ERR_LOG ( 7149, IUER, 'PIMA_INDX', 'Keyword SIDEBAND '// &
     &            'was not found in the FREQUENCY table in reading file '// &
     &             PIM%FILE(J4)%NAME )
              RETURN
         END IF
!
         IF ( FL_MDC_TAB ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, 1, 'MODEL_COMPS', 'NO_POL', &
     &                               PIM%NPOL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7150, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                 'the number of polarizations' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, 1, 'MODEL_COMPS', 'FFT_SIZE', &
     &                               PIM%FFT_SIZE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7151, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                 'the FFT size' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, 1, 'MODEL_COMPS', 'OVERSAMP', &
     &                               PIM%OVERSAMPLE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG ( 7152, IER, 'PIMA_INDX', 'Failure to get '// &
     &                 'the oversampliing parameter' )
                   PIM%OVERSAMPLE = 0
!!                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, 1, 'MODEL_COMPS', 'ZERO_PAD', &
     &                               PIM%ZERO_PAD, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG ( 7153, IER, 'PIMA_INDX', 'Failure to get '// &
     &                 'the zero padding parameter' )
                   PIM%ZERO_PAD = 0
!!                   RETURN
              END IF
            ELSE
              PIM%FFT_SIZE = 1
              PIM%OVERSAMPLE = 0
              PIM%ZERO_PAD = 0
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                   WRITE ( 6, '(A)' ) 'PIMA_INDX:  Warning: section MODEL_COMPS was not found'
              END IF
         END IF
!
         IF ( PIM%GENERATOR == 'AIPS' ) THEN
              CONTINUE
            ELSE IF ( PIM%GENERATOR == 'EVN'  .OR. &
     &                PIM%GENERATOR == 'SFXC' .OR. &
     &                PIM%GENERATOR == 'ASC'       ) THEN
              CONTINUE
            ELSE
              IF ( IND_FRQ_IBBC == 0 ) THEN
                   CALL ERR_LOG ( 7154, IUER, 'PIMA_INDX', 'Keyword BB_CHAN '// &
     &                 'was not found in the FREQUENCY table in reading file '// &
     &                  PIM%FILE(J4)%NAME )
                   RETURN
              END IF
         END IF
!
         IF ( PIM%GENERATOR == 'AIPS' ) THEN
              TABLE_NAME = 'AIPS AT '
            ELSE
              TABLE_NAME = 'ARRAY_GEOMETRY'
         END IF
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_GET_KEY_CH ( PIM, J4, TABLE_NAME, 'RDATE', STR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7155, IUER, 'PIMA_INDX', 'Failure to get '// &
     &            'the reference date' )
              RETURN
         END IF
         IF ( STR(5:5) == '-'  .AND.  STR(8:8) == '-' ) THEN
              STR(5:5) = '.'
              STR(8:8) = '.'
              STR(11:21) = '_00:00:00.0'
            ELSE IF ( STR(3:3) == '/' .AND. STR(6:6) == '/' ) THEN
              STR = '20'//STR(7:8)//'.'//STR(4:5)//'.'//STR(1:2)//'_00:00:00.0'
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL DATE_TO_TIME ( STR(1:21), PIM%FILE(J4)%MJD_REF, SEC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7156, IUER, 'PIMA_INDX', 'Failure to '// &
     &            'convert reference date '//STR(1:21) )
              RETURN
         END IF
!
! ------ Get frequency information.
! ------ Get the reference frequency
!
         CALL CLRCH ( STR )
         IF ( PIM%GENERATOR == 'AIPS' ) THEN
              STR = PIM%FILE(J4)%KEY(IND_FRQ_REF,IND_AT_TAB)(10:)
            ELSE
              STR = PIM%FILE(J4)%KEY(IND_FRQ_REF,IND_FRQ_TAB(J4))(10:)
         END IF
         CALL CHASHL ( STR )
         IP = INDEX ( STR, ' ' )
         IF ( IP .LE. 1 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J10, STR )
              CALL ERR_LOG ( 7157, IUER, 'PIMA_INDX', 'Failure to get '// &
     &            'the reference frequency from the FREQUENCY table '// &
     &            'in FITS-IDI file '// &
     &             PIM%FILE(J4)%NAME(1:I_LEN(PIM%FILE(J4)%NAME))//' '// &
     &             PIM%FILE(J4)%KEY(IND_FRQ_REF,IND_FRQ_TAB(J4)) )
              RETURN
         END IF
!
         READ ( UNIT=STR(1:IP-1), FMT='(F24.0)', IOSTAT=IER ) PIM%FILE(J4)%REF_FREQ
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) ' IND_FRQ_REF=',IND_FRQ_REF,' IND_FRQ_TAB(J4)=',IND_FRQ_TAB(J4)
              WRITE ( 6, * ) ' STR >>'//STR(1:IP-1)//'<< IER=',IER
              CALL CLRCH ( STR )
              CALL INCH  ( J10, STR )
              CALL ERR_LOG ( 7158, IUER, 'PIMA_INDX', 'Failure to decode '// &
     &            'the reference frequency from the FREQUENCY table '// &
     &            'in FITS-IDI file '// &
     &            PIM%FILE(J4)%NAME(1:I_LEN(PIM%FILE(J4)%NAME))//' '// &
     &            PIM%FILE(J4)%KEY(IND_FRQ_REF,IND_FRQ_TAB(J4)) )
              RETURN
         END IF
!
! ------ Get the number of frequencies
!
         IF ( PIM%GENERATOR == 'AIPS' ) THEN
              STR = PIM%FILE(J4)%KEY(IND_FRQ_NFRQ,IND_GC_TAB(J4))(10:)
            ELSE
              STR = PIM%FILE(J4)%KEY(IND_FRQ_NFRQ,IND_FRQ_TAB(J4))(10:)
         END IF
         CALL CHASHL ( STR )
         IP = INDEX ( STR, ' ' )
         IF ( IP .LE. 1 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J10, STR )
              CALL ERR_LOG ( 7159, IUER, 'PIMA_INDX', 'Failure to get '// &
     &            'the number frequencies from the FREQUENCY table '// &
     &            'in FITS-IDI file '// &
     &             PIM%FILE(J4)%NAME(1:I_LEN(PIM%FILE(J4)%NAME))//' '// &
     &             PIM%FILE(J4)%KEY(IND_FRQ_NFRQ,IND_FRQ_TAB(J4)) )
              RETURN
         END IF
!
         READ ( UNIT=STR(1:IP-1), FMT='(I4)', IOSTAT=IER ) PIM%FILE(J4)%NFRQ
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) ' STR = ', STR(1:I_LEN(STR))
              CALL CLRCH ( STR )
              CALL INCH  ( J10, STR )
              CALL ERR_LOG ( 7160, IUER, 'PIMA_INDX', 'Failure to decode '// &
     &            'the number frequencies from the FREQUENCY table '// &
     &            'in FITS-IDI file '// &
     &            PIM%FILE(J4)%NAME(1:I_LEN(PIM%FILE(J4)%NAME))//' '// &
     &            PIM%FILE(J4)%KEY(IND_FRQ_NFRQ,IND_FRQ_TAB(J4)) )
              RETURN
         END IF
!
         IF ( PIM%FILE(J4)%NFRQ > PIM__MFRQ ) THEN
              CALL CLRCH ( STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( PIM%FILE(J4)%NFRQ, STR  )
              CALL INCH  ( PIM__MFRQ,         STR1 )
              CALL ERR_LOG ( 7161, IUER, 'PIMA_INDX', 'Too many '// &
                  'frequencies: '//STR(1:I_LEN(STR))//' -- more than '// &
     &            ' PIM__MFRQ: '//STR1(1:I_LEN(STR1))// &
     &            '. Consider to increase parameter PIM__MFRQ' )
              RETURN
         END IF
!
         IF ( PIM%FILE(J4)%NFRG > PIM__MFRG ) THEN
              CALL CLRCH ( STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( PIM%FILE(J4)%NFRG, STR  )
              CALL INCH  ( PIM__MFRG,         STR1 )
              CALL ERR_LOG ( 7162, IUER, 'PIMA_INDX', 'Too many '// &
                  'frequency groups: '//STR(1:I_LEN(STR))//' -- more than '// &
     &            ' PIM__MFRG: '//STR1(1:I_LEN(STR1))// &
     &            '. Consider to increase parameter PIM__MFRG' )
              RETURN
         END IF
!
         IF ( PIM%NFRQ .NE. 0 .AND. PIM%FILE(J4)%NFRQ .NE. PIM%NFRQ ) THEN
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL INCH  ( PIM%FILE(J4)%NFRQ, STR )
              CALL INCH  ( PIM%NFRQ, STR1 )
              CALL ERR_LOG ( 7163, IUER, 'PIMA_INDX', 'Trap of internal '// &
     &            'control: the number of frequencies within one file group '// &
     &            ' defined in file '// &
     &             PIM%FILE(J4)%NAME(1:I_LEN(PIM%FILE(J4)%NAME))//' -- '// &
     &             STR(1:I_LEN(STR))//' is not the same as in the previous '// &
     &            'file: '//STR1 )
              RETURN
         END IF
!
         PIM%NFRQ = PIM%FILE(J4)%NFRQ
!
         ALLOCATE ( FREQ(PIM%FILE(J4)%NFRQ,PIM%FILE(J4)%NFRG), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, STR )
              CALL ERR_LOG ( 7164, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &            'temporary array FREQ' )
              RETURN
         END IF
!
         ALLOCATE ( BAND_WIDTH(PIM%FILE(J4)%NFRQ,PIM%FILE(J4)%NFRG), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, STR )
              CALL ERR_LOG ( 7165, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &            'temporary array BAND_WIDTH' )
              RETURN
         END IF
!
         ALLOCATE ( CHAN_WIDTH(PIM%FILE(J4)%NFRQ,PIM%FILE(J4)%NFRG), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 8*PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, STR )
              CALL ERR_LOG ( 7166, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &            'temporary array CHAN_WIDTH' )
              RETURN
         END IF
!
         ALLOCATE ( SIDE_BAND(PIM%FILE(J4)%NFRQ,PIM%FILE(J4)%NFRG), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 4*PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, STR )
              CALL ERR_LOG ( 7167, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &            'temporary array SIDE_BAND' )
              RETURN
         END IF
!
         ALLOCATE ( BB_SP_CHAN_IND(PIM%FILE(J4)%NFRQ,PIM%FILE(J4)%NFRG), &
     &              STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 4*PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, STR )
              CALL ERR_LOG ( 7168, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &             STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &            'temporary array BB_SP_CHAN_IND' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL FFITS_GETR8 ( PIM%FILE(J4)%FITS_DESC, IND_FRQ_TAB(J4), 1, &
     &                      PIM%FILE(J4)%KEY(IND_FRQ_BFRQ,IND_FRQ_TAB(J4)), &
     &                      PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, FREQ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J4, STR )
              CALL ERR_LOG ( 7169, IUER, 'PIMA_INDX', 'Error in '// &
     &            'getting bandwidth frequency array for the '// &
     &             STR(1:I_LEN(STR))//'-th UV data of the '// &
     &            'FITS-IDI file '//PIM%FILE(J4)%NAME  )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL FFITS_GETR8 ( PIM%FILE(J4)%FITS_DESC, IND_FRQ_TAB(J4), 1, &
     &              PIM%FILE(J4)%KEY(IND_FRQ_BWID,IND_FRQ_TAB(J4)), &
     &              PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, BAND_WIDTH, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J4, STR )
              CALL ERR_LOG ( 7170, IUER, 'PIMA_INDX', 'Error in '// &
     &            'getting array of frequency bandwidth for the '// &
     &             STR(1:I_LEN(STR))//'-th UV data of the '// &
     &            'FITS-IDI file '//PIM%FILE(J4)%NAME  )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL FFITS_GETR8 ( PIM%FILE(J4)%FITS_DESC, IND_FRQ_TAB(J4), 1, &
     &              PIM%FILE(J4)%KEY(IND_FRQ_CWID,IND_FRQ_TAB(J4)), &
     &              PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, CHAN_WIDTH, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J4, STR )
              CALL ERR_LOG ( 7171, IUER, 'PIMA_INDX', 'Error in '// &
     &            'getting array of frequency bandwidthfor the '// &
     &             STR(1:I_LEN(STR))//'-th UV data of the '// &
     &            'FITS-IDI file '//PIM%FILE(J4)%NAME  )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL FFITS_GETI4 ( PIM%FILE(J4)%FITS_DESC, IND_FRQ_TAB(J4), 1, &
     &              PIM%FILE(J4)%KEY(IND_FRQ_SBND,IND_FRQ_TAB(J4)), &
     &              PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, SIDE_BAND, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J4, STR )
              CALL ERR_LOG ( 7172, IUER, 'PIMA_INDX', 'Error in '// &
     &            'getting array of frequency bandwidthf or the '// &
     &             STR(1:I_LEN(STR))//'-th UV data of the '// &
     &            'FITS-IDI file '//PIM%FILE(J4)%NAME  )
              RETURN
         END IF
!
         IF ( PIM%GENERATOR == 'AIPS' ) THEN
              CALL NOUT_I4 ( PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, &
     &                       BB_SP_CHAN_IND )
            ELSE IF ( PIM%GENERATOR == 'EVN'  .OR. &
     &                PIM%GENERATOR == 'SFXC' .OR. &
     &                PIM%GENERATOR == 'ASC'       ) THEN
              CALL NOUT_I4 ( PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, &
     &                       BB_SP_CHAN_IND )
            ELSE
              CALL ERR_PASS ( IUER, IER )
              CALL FFITS_GETI4 ( PIM%FILE(J4)%FITS_DESC, IND_FRQ_TAB(J4), 1, &
     &              PIM%FILE(J4)%KEY(IND_FRQ_IBBC,IND_FRQ_TAB(J4)), &
     &              PIM%FILE(J4)%NFRQ*PIM%FILE(J4)%NFRG, BB_SP_CHAN_IND, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J4, STR )
                   CALL ERR_LOG ( 7173, IUER, 'PIMA_INDX', 'Error in '// &
     &                 'getting array of frequency bandwidth for the '// &
     &                  STR(1:I_LEN(STR))//'-th UV data of the '// &
     &                 'FITS-IDI file '//PIM%FILE(J4)%NAME  )
                   RETURN
              END IF
         END IF
!
         CALL CLRCH ( STR_FRQ )
         K_FRG = 0
         CALL NOUT_I4 ( PIM__MFRQ*PIM__MFRG, IND_FRGS  )
         IF ( PIM%FILE(J4)%NFRG > PIM_MAX_FRG ) THEN
              PIM%FILE(J4)%NFRG = PIM_MAX_FRG
         END IF
         DO 4110 J11=PIM_MIN_FRG,PIM%FILE(J4)%NFRG
            K_FRQ = 0
            DO 4120 J12=1,PIM%FILE(J4)%NFRQ
               IF ( SIDE_BAND(J12,J11) == 1 ) THEN
                    FRQ_ORIG(J12,J11,J4)%FREQ = PIM%FILE(J4)%REF_FREQ &
     &                                        + FREQ(J12,J11)
                  ELSE IF ( SIDE_BAND(J12,J11) == -1 ) THEN
                    FRQ_ORIG(J12,J11,J4)%FREQ = PIM%FILE(J4)%REF_FREQ &
     &                                        + FREQ(J12,J11) &
     &                                        - BAND_WIDTH(J12,J11) &
     &                                        + CHAN_WIDTH(J12,J11)
                  ELSE
                    CALL CLRCH ( STR )
                    CALL INCH  ( SIDE_BAND(J12,J11), STR )
                    CALL ERR_LOG ( 7174, IUER, 'PIMA_INDX', 'Error in '// &
     &                  'parsing the frequency table: unsupported value of '// &
     &                  ' SIDEBAND: '//STR(1:I_LEN(STR))//' while one of '// &
     &                  ' 1 or -1 was expected while processing FITS-IDI '// &
     &                  'file '//PIM%FILE(J4)%NAME  )
                    RETURN
               END IF
               IF ( ILEN(STR_FREQ_REPL) > 0 ) THEN
                    IF ( DABS(FRQ_ORIG(J12,J11,J4)%FREQ - FREQ_ORIG) < 1.0 ) THEN
                         IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
                              WRITE ( 6, * ) 'PIMA_INDEX: Replace frequency ', &
     &                                FRQ_ORIG(J12,J11,J4)%FREQ, ' with ', &
     &                                FREQ_REPL
                         END IF
                         FRQ_ORIG(J12,J11,J4)%FREQ = FREQ_REPL
                    END IF
               END IF
               IF ( IS_R8_NAN(FRQ_ORIG(J12,J11,J4)%FREQ) ) THEN
                    FRQ_ORIG(J12,J11,J4)%FREQ = 0.0D0
               END IF
               IF ( J12 == 1 ) THEN
                    FRQ_MIN = FRQ_ORIG(J12,J11,J4)%FREQ
                    FRQ_MAX = FRQ_ORIG(J12,J11,J4)%FREQ
                 ELSE
                    FRQ_MIN = MIN ( FRQ_MIN, FRQ_ORIG(J12,J11,J4)%FREQ )
                    FRQ_MAX = MAX ( FRQ_MAX, FRQ_ORIG(J12,J11,J4)%FREQ )
               END IF
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                    WRITE ( 6, 216 ) PIM%FILE(J4)%NAME(1:I_LEN(PIM%FILE(J4)%NAME)), &
     &                               J12, J11, FRQ_ORIG(J12,J11,J4)%FREQ, &
     &                               SIDE_BAND(J12,J11), FREQ(J12,J11), &
     &                               BAND_WIDTH(J12,J11), PIM%FILE(J4)%REF_FREQ
 216                FORMAT ( 'File ', A, ' File_Ind_Frq: ', I4,' File_Ind_Grp: ', I2, &
     &                       ' Frq= ', 1PD16.8, ' Sb: ', I2, &
     &                       ' Frq_orig: ', 1PD16.8, ' Bw_orig: ', 1PD16.8, &
     &                       ' REF_FRQ: ', 1D16.8 )
               END IF
 4120       CONTINUE
!
! --------- Now we check FRQ_MIN and FRQ_MAX in the array of minimal and maximal
! --------- frequency in the group
!
            IF ( PIM%NFRG > 0 ) THEN
                 IND_GRP_MIN = IFIND_PL8 ( INT8(PIM%NFRG), FREQ_GRP_ARR_MIN_I8, &
     &                                     NINT ( FRQ_MIN, KIND=8 ) )
                 IND_GRP_MAX = IFIND_PL8 ( INT8(PIM%NFRG), FREQ_GRP_ARR_MAX_I8, &
     &                                     NINT ( FRQ_MAX, KIND=8 ) )
              ELSE 
                 IND_GRP_MIN = 0
                 IND_GRP_MAX = 0
            END IF
            IF ( STR_KEEP_FRG == 'YES' ) THEN
                 IND_GRP_MIN = 0
                 IND_GRP_MAX = 0
            END IF
            IF ( IND_GRP_MIN < 1 .OR. IND_GRP_MAX < 1 ) THEN
!
! -------------- If minimal OR maximal frequency is not found, this mean
! -------------- we came across a new group
!
                 PIM%NFRG = PIM%NFRG + 1
                 IF ( PIM%NFRG > PIM__MFRG ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( PIM__MFRG, STR )
                      CALL ERR_LOG ( 7175, IUER, 'PIMA_INDX', 'Too many '// &
     &                    'frequency groups: more than '//STR )
                      RETURN
                 END IF
                 FREQ_GRP_ARR_MIN_I8(PIM%NFRG) = NINT ( FRQ_MIN, KIND=8 )
                 FREQ_GRP_ARR_MAX_I8(PIM%NFRG) = NINT ( FRQ_MAX, KIND=8 )
                 IND_GRP = PIM%NFRG
               ELSE IF ( IND_GRP_MIN == IND_GRP_MAX ) THEN
                 IND_GRP = IND_GRP_MIN
               ELSE
!
! -------------- A situation is possible when the frequency groups have
! -------------- the same either first or last frequency. We need to
! -------------- check both min and max frequency whether it belongs to
! -------------- the group
!
                 IF ( NINT ( FRQ_MIN, KIND=8 ) == FREQ_GRP_ARR_MIN_I8(IND_GRP_MIN) .AND. &
                      NINT ( FRQ_MAX, KIND=8 ) == FREQ_GRP_ARR_MAX_I8(IND_GRP_MIN)       ) THEN
                      IND_GRP = IND_GRP_MIN
                    ELSE
                      IND_GRP = IND_GRP_MAX
                 END IF
            END IF
!
            DO 4130 J13=1,PIM%FILE(J4)%NFRQ
               IF ( IS_R8_NAN(BAND_WIDTH(J13,J11)) ) THEN
                    BAND_WIDTH(J13,J11) = 0.0D0
               END IF
               IF ( IS_R8_NAN(CHAN_WIDTH(J13,J11)) ) THEN
                    CHAN_WIDTH(J13,J11) = 0.0D0
               END IF
               FRQ_ORIG(J13,J11,J4)%FREQ_I8        = NINT ( FRQ_ORIG(J13,J11,J4)%FREQ, KIND=8 )
               FRQ_ORIG(J13,J11,J4)%BAND_WIDTH     = BAND_WIDTH(J13,J11)
               FRQ_ORIG(J13,J11,J4)%CHAN_WIDTH     = CHAN_WIDTH(J13,J11)
               FRQ_ORIG(J13,J11,J4)%SIDE_BAND      = SIDE_BAND(J13,J11)
               FRQ_ORIG(J13,J11,J4)%BB_SP_CHAN_IND = BB_SP_CHAN_IND(J13,J11)
               FRQ_ORIG(J13,J11,J4)%FRQ_GRP        = J11
!
               FREQ_FIRST_ARR_I8(J13,IND_GRP) = FRQ_ORIG(J13,J11,J4)%FREQ_I8
               FREQ_LAST_ARR_I8(J13,IND_GRP)  = FRQ_ORIG(J13,J11,J4)%FREQ_I8 + &
     &                                          NINT ( BAND_WIDTH(J13,J11), KIND=8 )
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                    WRITE  ( 6, 123 ) J13, J11, FREQ_FIRST_ARR_I8(J13,IND_GRP), BAND_WIDTH(J13,J11)
 123                FORMAT ( 'File_Frq_num: ', I2, ' File_Frq_Grp: ', I1,' Freq: ', I12, ' Bandwidth: ', F12.1, ' Hz'  )
               END IF
 4130       CONTINUE
!
            CALL SORT_I8 ( INT8(PIM%NFRQ), FREQ_FIRST_ARR_I8(1,IND_GRP) )
            CALL SORT_I8 ( INT8(PIM%NFRQ), FREQ_LAST_ARR_I8(1,IND_GRP)  )
            DO 4140 J14=1,PIM%FILE(J4)%NFRQ
               IND_FRQ = IFIND_PL8 ( INT8(PIM%NFRQ), &
     &                               FREQ_FIRST_ARR_I8(1,IND_GRP), &
     &                               FRQ_ORIG(J14,J11,J4)%FREQ_I8 )
               PIM%FRQ(IND_FRQ,IND_GRP) = FRQ_ORIG(J14,J11,J4)
               PIM%FRQ(IND_FRQ,IND_GRP)%FRQ_GRP = 0
               IF ( IND_FRQ < PIM%NFRQ ) THEN
!
! ----------------- It is possible that frequencies for LCP and RCP follow
! ----------------- each others. Then we have two slots with the same frequency.
! ----------------- We need to fill them. We search, whether the data have
! ----------------- the second time the same frequency. If yes, then put it
!
                    IND_FRQ_2ND = IFIND_PL8 ( INT8(PIM%NFRQ), &
     &                                        FREQ_FIRST_ARR_I8(IND_FRQ+1,IND_GRP), &
     &                                        FRQ_ORIG(J14,J11,J4)%FREQ_I8 ) + &
     &                                        IND_FRQ
                    IF ( IND_FRQ_2ND > IND_FRQ ) THEN
                         PIM%FRQ(IND_FRQ_2ND,IND_GRP) = FRQ_ORIG(J14,J11,J4)
                         PIM%FRQ(IND_FRQ_2ND,IND_GRP)%FRQ_GRP = 0
                    END IF
               END IF
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                    WRITE  ( 6, 125 ) J14, IND_FRQ, J11, IND_GRP, &
     &                                PIM%FRQ(IND_FRQ,IND_GRP)%FREQ, PIM%NFRG
 125                FORMAT ( 'Frq_NUM: ', I3, ' ind_frq: ', I3, &
     &                       ' File_frq_grp: ', I1,' Glob_frq_grp: ', I1, &
     &                       ' Freq: ', 1PD16.8, ' PIM%NFRG: ', I1 )
               END IF
 4140       CONTINUE
 4110    CONTINUE
!
         IF ( PIM%GENERATOR == 'AIPS' ) THEN
              TABLE_NAME = 'AIPS AN '
            ELSE
              TABLE_NAME = 'ANTENNA '
         END IF
!
! ------ Get the number of antennas.
! ------ NB: the number of antennas may be more than then number of 
! ------ stations!!
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_GET_KEY_I4 ( PIM, J4, TABLE_NAME, 'NAXIS2', ANT(J4)%N_ARR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7176, IUER, 'PIMA_INDX', 'Failure to get '// &
     &            'the number of antennas in FITS-IDI file '//PIM%FILE(J4)%NAME  )
              RETURN
         END IF
         PIM%FILE(J4)%N_STA = ANT(J4)%N_ARR
!
         ALLOCATE ( ANT(J4)%NAM(ANT(J4)%N_ARR), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7177, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &            'dynamic memory for array ANT(J4)%NAM' )
              RETURN
         END IF
!
         ALLOCATE ( ANT(J4)%REF(ANT(J4)%N_ARR), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7178, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &            'dynamic memory for array ANT(J4)%REF' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_GET_KEY_I4 ( PIM, J4, 'ARRAY_GEOMETRY', 'NAXIS2', LSTA(J4), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7179, IUER, 'PIMA_INDX', 'Failure to get '// &
     &            'the number of stations in FITS-IDI file '//PIM%FILE(J4)%NAME  )
              RETURN
         END IF
         IF ( LSTA(J4) > PIM__MSTA ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( PIM__MSTA, STR ) 
              CALL ERR_LOG ( 7180, IUER, 'PIMA_INDX', 'Too many stations '// &
     &            'in the FITS-IDI file '// &
     &            PIM%FILE(J4)%NAME(1:I_LEN(PIM%FILE(J4)%NAME))// &
     &            ' more than PIM__MSTA= '//STR )
              RETURN
         END IF
!
! ------ Read the array geometry table
!
         IND_STA = 0
         DO 4150 J15=1,LSTA(J4)
            CALL ERR_PASS ( IUER, IER )
            CALL FFITS_GETCH ( PIM%FILE(J4)%FITS_DESC, IND_ARG_TAB(J4), J15, &
     &                         PIM%FILE(J4)%KEY(IND_STA_NAME,IND_ARG_TAB(J4)), &
     &                         1, STA_NAME_FIL(J15,J4), IER )
!
! --------- Replace binary zeroes with blanks for further comparison
!
            IP = INDEX ( STA_NAME_FIL(J15,J4), CHAR(0) )
            IF ( IP > 0 ) CALL CLRCH ( STA_NAME_FIL(J15,J4)(IP:) )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J15, STR )
                 CALL ERR_LOG ( 7181, IUER, 'PIMA_INDX', 'Error in '// &
     &               'reading the name of the '//STR(1:I_LEN(STR))//'-th '// &
     &               'station in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                 RETURN
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL FFITS_GETR8 ( PIM%FILE(J4)%FITS_DESC, IND_ARG_TAB(J4), J15, &
     &                         PIM%FILE(J4)%KEY(IND_STA_COO(J4),IND_ARG_TAB(J4)), &
     &                         3, STA_COO(1,J15), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J15, STR )
                 CALL ERR_LOG ( 7182, IUER, 'PIMA_INDX', 'Error in '// &
     &               'reading station coordinates of the '// &
     &                STR(1:I_LEN(STR))//'-th '// &
     &               'station in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                 RETURN
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL FFITS_GETR8 ( PIM%FILE(J4)%FITS_DESC, IND_ARG_TAB(J4), J15, &
     &                         PIM%FILE(J4)%KEY(IND_STA_VEL(J4),IND_ARG_TAB(J4)), &
     &                         3, STA_VEL(1,J15), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J15, STR )
                 CALL ERR_LOG ( 7183, IUER, 'PIMA_INDX', 'Error in '// &
     &               'reading station velocities of the '// &
     &                STR(1:I_LEN(STR))//'-th '// &
     &               'station in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                 RETURN
            END IF
!
            IND_STA = 0
            IF ( PIM%NSTA > 0 ) THEN
                 IND_STA = LTM_DIF ( 0, PIM%NSTA, C_STA, STA_NAME_FIL(J15,J4) )
            END IF
!
            IF ( IND_STA == 0 ) THEN
                 FL_FOUND = .FALSE.
                 DO 4160 J16=1,L_CST
                    IF ( STA_CAT(J16)%NAME == STA_NAME_FIL(J15,J4) ) THEN
                         FL_FOUND = .TRUE.
                         PIM%NSTA = PIM%NSTA + 1
                         PIM%STA(PIM%NSTA)%NAME      = STA_CAT(J16)%NAME
                         PIM%STA(PIM%NSTA)%IVS_NAME  = STA_CAT(J16)%IVS_NAME
                         PIM%STA(PIM%NSTA)%ORIG_NAME = STA_NAME_FIL(J15,J4)
                         PIM%STA(PIM%NSTA)%COO(1)    = STA_CAT(J16)%COO(1)
                         PIM%STA(PIM%NSTA)%COO(2)    = STA_CAT(J16)%COO(2)
                         PIM%STA(PIM%NSTA)%COO(3)    = STA_CAT(J16)%COO(3)
                         PIM%STA(PIM%NSTA)%COO_ORIG(1) = STA_COO(1,J15)
                         PIM%STA(PIM%NSTA)%COO_ORIG(2) = STA_COO(2,J15)
                         PIM%STA(PIM%NSTA)%COO_ORIG(3) = STA_COO(3,J15)
                         PIM%STA(PIM%NSTA)%VEL_ORIG(1) = STA_VEL(1,J15)*1.D-3/YEAR__TO__SEC
                         PIM%STA(PIM%NSTA)%VEL_ORIG(2) = STA_VEL(2,J15)*1.D-3/YEAR__TO__SEC
                         PIM%STA(PIM%NSTA)%VEL_ORIG(3) = STA_VEL(3,J15)*1.D-3/YEAR__TO__SEC
                         PIM%STA(PIM%NSTA)%IND_ORIG  = J15
                         PIM%STA(PIM%NSTA)%PCAL%PCAL_AVAIL = .FALSE.
                         PIM%STA(PIM%NSTA)%PCAL%PCAL_USE   = .FALSE.
                         PIM%STA(PIM%NSTA)%CABLE%CAB_AVAIL = .FALSE.
                         PIM%STA(PIM%NSTA)%TSYS%AVAIL = .FALSE.
                         PIM%STA(PIM%NSTA)%GAIN%AVAIL = .FALSE.
                         PIM%STA(PIM%NSTA)%STMO(1:PIM%NFRG)%OPA_AVAIL   = .FALSE.
                         PIM%STA(PIM%NSTA)%STMO(1:PIM%NFRG)%TREC_AVAIL  = .FALSE.
                         PIM%STA(PIM%NSTA)%STMO(1:PIM%NFRG)%TSPI_AVAIL  = .FALSE.
                         PIM%STA(PIM%NSTA)%STMO(1:PIM%NFRG)%TSYS_AVAIL  = .FALSE.
                         PIM%STA(PIM%NSTA)%STMO(1:PIM%NFRG)%TTOA_AVAIL  = .FALSE.
                         PIM%STA(PIM%NSTA)%STMO(1:PIM%NFRG)%TSRAT_AVAIL = .FALSE.
                         PIM%STA(PIM%NSTA)%L_MOD      = 0
                         C_STA(PIM%NSTA) = STA_NAME_FIL(J15,J4)
                         IND_STA = PIM%NSTA
                         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                              WRITE ( 6, * ) 'PIMA_INDX-1927 Unsorted Ind_sta= ', INT2(IND_STA), ' STA_NAME= ', PIM%STA(PIM%NSTA)%NAME, &
     &                                       ' IVS_NAME= ', PIM%STA(PIM%NSTA)%IVS_NAME, ' ORIG_NAME= ', PIM%STA(PIM%NSTA)%ORIG_NAME
                         END IF
                    END IF
 4160            CONTINUE
!
                 IF ( .NOT. FL_FOUND ) THEN
                      CALL ERR_LOG ( 7184, IUER, 'PIMA_INDX', 'Cannot find '// &
     &                    'station '//STA_NAME_FIL(J15,J4)//' from '// &
     &                    ' antenna table of the UV-file '// &
     &                    PIM%FILE(J4)%NAME(1:I_LEN(PIM%FILE(J4)%NAME))// &
     &                    ' in station file '//PIM%CONF%STA_NAMES_FILE )
                      RETURN
                 END IF
            END IF
 4150    CONTINUE 
!
! ------ Now read antenna table
!
         DO 4170 J17=1,ANT(J4)%N_ARR
!
! --------- Get the antenna name
!
            CALL ERR_PASS ( IUER, IER )
            CALL FFITS_GETCH ( PIM%FILE(J4)%FITS_DESC, IND_ANT_TAB(J4), J17, &
     &                         PIM%FILE(J4)%KEY(IND_ANT_NAME,IND_ANT_TAB(J4)), &
     &                         1, ANT(J4)%NAM(J17), IER )
!
! --------- Replace binary zeroes with blanks for further comparison
!
            IP = INDEX ( ANT(J4)%NAM(J17), CHAR(0) )
            IF ( IP > 0 ) CALL CLRCH ( ANT(J4)%NAM(J17)(IP:) )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J17, STR )
                 CALL ERR_LOG ( 7185, IUER, 'PIMA_INDX', 'Error in '// &
     &               'reading the name of the '//STR(1:I_LEN(STR))//'-th '// &
     &               'station in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                 RETURN
            END IF
!
! --------- Get the cross referencing table that maps antenna array to 
! --------- the station array
!
            CALL ERR_PASS ( IUER, IER )
            CALL FFITS_GETI4 ( PIM%FILE(J4)%FITS_DESC, IND_ANT_TAB(J4), J17, &
     &                         PIM%FILE(J4)%KEY(IND_ANT_IND,IND_ANT_TAB(J4)), &
     &                         1, ANT(J4)%REF(J17), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J17, STR )
                 CALL ERR_LOG ( 7186, IUER, 'PIMA_INDX', 'Error in '// &
     &               'reading the index of the '//STR(1:I_LEN(STR))//'-th '// &
     &               'station in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                 RETURN
            END IF
!
            IF ( IND_NLEV(J4) > 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J4)%FITS_DESC, IND_ANT_TAB(J4), J17, &
     &                              PIM%FILE(J4)%KEY(IND_NLEV(J4),IND_ANT_TAB(J4)), &
     &                              1, NLEV_ARR_ORIG(ANT(J4)%REF(J17)), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J17, STR )
                      CALL ERR_LOG ( 7187, IUER, 'PIMA_INDX', 'Error in '// &
     &                    'reading the number of level at the '//TRIM(STR)// &
     &                    '-th station in the FITS-IDI file '//PIM%FILE(J4)%NAME )
                      RETURN
                 END IF
!
                 IF ( ILEN(STR_PIMAVAR_NLEV) > 0 ) THEN
                      CALL CHIN ( STR_PIMAVAR_NLEV, NLEV_ARR_ORIG(ANT(J4)%REF(J17)) )
                 END IF
                 IF ( ( PIM%GENERATOR == 'Mitaka correlator' .OR. &
     &                  PIM%GENERATOR == 'VERA'              .OR. &
     &                  PIM%GENERATOR == 'KJCC correlator'        ) .AND. &
     &                  NLEV_ARR_ORIG(ANT(J4)%REF(J17)) == 1              ) THEN
!
                        NLEV_ARR_ORIG(ANT(J4)%REF(J17)) = 4
                 END IF
                 IF ( PIM%GENERATOR == 'ASC'  .AND. &
     &                NLEV_ARR_ORIG(ANT(J4)%REF(J17)) == 0 ) THEN
!
                      NLEV_ARR_ORIG(ANT(J4)%REF(J17)) = 2
                 END IF
                 NLEV_UNSORTED(ANT(J4)%REF(J17)) = NLEV_ARR_ORIG(ANT(J4)%REF(J17))
               ELSE
                 NLEV_ARR_ORIG(ANT(J4)%REF(J17)) = -1
            END IF
            IF ( IND_POL_TYPA(J4) > 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL CLRCH ( STR )
                 CALL FFITS_GETCH ( PIM%FILE(J4)%FITS_DESC, IND_ANT_TAB(J4), J17, &
     &                              PIM%FILE(J4)%KEY(IND_POL_TYPA(J4),IND_ANT_TAB(J4)), &
     &                              1, STR, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J17, STR )
                      CALL ERR_LOG ( 7188, IUER, 'PIMA_INDX', 'Error in '// &
     &                    'reading the polarization type of the '//TRIM(STR)// &
     &                    'antenna in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                      RETURN
                 END IF
                 IF ( PIM%CORR_VERS == "DiFX-2.5.2" .AND. PIM%STK_1 == -5 ) THEN
!
! ------------------- A kludge fix of the DiFX bug
!
                      STR(1:1) = 'H'
                 END IF
                 CALL TRAN ( 11, STR, STR )
                 IF ( STR_PIMAVAR_POL_XY_TO_HV == 'YES' ) STR(1:1) = 'H'
                 POLAR_TYPA_UNSORTED(ANT(J4)%REF(J17)) = INDEX ( PIMA__POL_STR, STR(1:1) )
                 IF ( POLAR_TYPA_UNSORTED(ANT(J4)%REF(J17)) < 1 ) THEN
                      CALL CLRCH ( STR1 )
                      CALL INCH  ( J17, STR1 )
                      CALL ERR_LOG ( 7189, IUER, 'PIMA_INDX', 'Trap of '// &
     &                    'internal control: wrong polarization typeA '// &
     &                    STR(1:1)//' for the '//TRIM(STR1)//' antenna '// &
     &                    ANT(J4)%NAM(J17)//' was found in the FITS-IDI file '// &
     &                    PIM%FILE(J4)%NAME  )
                      RETURN
                 END IF
               ELSE
!
! -------------- Some reasonable default: R-polarization
!
                 POLAR_TYPA_UNSORTED(ANT(J4)%REF(J17)) = PIMA__POL_R
            END IF
            IF ( IND_POL_TYPB(J4) > 0 .AND. PIM%NSTK > 1 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL CLRCH ( STR )
                 CALL FFITS_GETCH ( PIM%FILE(J4)%FITS_DESC, IND_ANT_TAB(J4), J17, &
     &                              PIM%FILE(J4)%KEY(IND_POL_TYPB(J4),IND_ANT_TAB(J4)), &
     &                              1, STR, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J17, STR )
                      CALL ERR_LOG ( 7190, IUER, 'PIMA_INDX', 'Error in '// &
     &                    'reading the polarization type of the '//TRIM(STR)// &
     &                    'antenna in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                      RETURN
                 END IF
                 CALL TRAN ( 11, STR, STR )
                 IF ( PIM%CORR_VERS == "DiFX-2.5.2" .AND. PIM%STK_1 == -5 ) THEN
!
! ------------------- A kludge fix of the DiFX bug
!
                      STR(1:1) = 'V'
                 END IF
                 IF ( STR(1:1) == '?' ) THEN
                      IF ( POLAR_TYPA_UNSORTED(ANT(J4)%REF(J17)) == PIMA__POL_R ) THEN
                           STR = 'L'
                         ELSE IF ( POLAR_TYPA_UNSORTED(ANT(J4)%REF(J17)) == PIMA__POL_L ) THEN
                           STR = 'R'
                      END IF
                 END IF
                 IF ( STR_PIMAVAR_POL_XY_TO_HV == 'YES' ) STR(1:1) = 'V'
                 POLAR_TYPB_UNSORTED(ANT(J4)%REF(J17)) = INDEX ( PIMA__POL_STR, STR(1:1) )
                 IF ( POLAR_TYPB_UNSORTED(ANT(J4)%REF(J17)) < 1 ) THEN
                      CALL CLRCH ( STR1 )
                      CALL CLRCH ( STR2 )
                      CALL CLRCH ( STR3 )
                      CALL INCH  ( ANT(J4)%REF(J17), STR1 )
                      CALL INCH  ( PIM%STK_1, STR2 )
                      CALL INCH  ( PIM%NSTK,  STR3 )
                      CALL ERR_LOG ( 7191, IUER, 'PIMA_INDX', 'Trap of '// &
     &                    'internal control: wrong polarization typeB '// &
     &                     STR(1:1)//' for the '//TRIM(STR1)//'-th antenna '// &
     &                    'was found in the FITS-IDI file '//TRIM(PIM%FILE(J4)%NAME)// &
     &                    ' , while STK_1= '//TRIM(STR2)//' and NSTK= '//TRIM(STR3)  )
                      RETURN
                 END IF
               ELSE
!
! -------------- Some reasonable default: R-polarization
!
                 POLAR_TYPB_UNSORTED(ANT(J4)%REF(J17)) = PIMA__POL_R
            END IF
!
            IF ( IND_POL_ANG(J4) > 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(J4)%FITS_DESC, IND_ANT_TAB(J4), J17, &
     &                              PIM%FILE(J4)%KEY(IND_POL_ANG(J4),IND_ANT_TAB(J4)), &
     &                              PIM%NFRQ, POLAR_ANG_UNSORTED(1,ANT(J4)%REF(J17)), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J17, STR )
                      CALL ERR_LOG ( 7192, IUER, 'PIMA_INDX', 'Error in '// &
     &                    'reading the polarization angle of the '//TRIM(STR)// &
     &                    'antenna in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                      RETURN
                 END IF
                 POLAR_ANG_UNSORTED(1:PIM%NFRQ,ANT(J4)%REF(J17)) = POLAR_ANG_UNSORTED(1:PIM%NFRQ,ANT(J4)%REF(J17))*DEG__TO__RAD
               ELSE
!
! -------------- Some reasonable default: zero angle
!
                 POLAR_ANG_UNSORTED = 0.0D0
            END IF
 4170    CONTINUE
!
         IF ( PIM%GENERATOR == 'AIPS' ) THEN
              TABLE_NAME = 'AIPS SU '
            ELSE
              TABLE_NAME = 'SOURCE  '
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_GET_KEY_I4 ( PIM, J4, TABLE_NAME, 'NAXIS2', KSOU(J4), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7193, IUER, 'PIMA_INDX', 'Failure to get '// &
     &            'the number of stations in FITS-IDI file '//PIM%FILE(J4)%NAME  )
              RETURN
         END IF
!
         PIM%FILE(J4)%N_SOU = KSOU(J4)
         DO 4180 J18=1,KSOU(J4)
            CALL ERR_PASS ( IUER, IER )
            CALL FFITS_GETCH ( PIM%FILE(J4)%FITS_DESC, IND_SOU_TAB(J4), J18, &
     &                         PIM%FILE(J4)%KEY(IND_SOU_NAM,IND_SOU_TAB(J4)), 1, &
     &                         PIM%FILE(J4)%SOU_NAME_ORIG(J18), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J18, STR )
                 CALL ERR_LOG ( 7194, IUER, 'PIMA_INDX', 'Error in '// &
     &               'reading the name of the '//STR(1:I_LEN(STR))//'-th '// &
     &               'source in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                 RETURN
            END IF
!
! --------- Replace binary zeroes with blanks for further comparison
!
            IP = INDEX ( PIM%FILE(J4)%SOU_NAME_ORIG(J18), CHAR(0) )
            IF ( IP > 0 ) CALL CLRCH ( PIM%FILE(J4)%SOU_NAME_ORIG(J18)(IP:) )
!
            IF ( ISOU_REN > 0 ) THEN
                 IP = LTM_DIF ( 0, ISOU_REN, SOU_REN_OLD, PIM%FILE(J4)%SOU_NAME_ORIG(J18) )
                 IF ( IP > 0 ) THEN
                      PIM%FILE(J4)%SOU_NAME_ORIG(J18) = SOU_REN_NEW(IP)
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                           WRITE ( 6, 129 ) SOU_REN_OLD(IP), SOU_REN_NEW(IP)
 129                       FORMAT ( 'PIMA_INDX  Replace original source name ', A, &
     &                              ' with ', A )
                      END IF
                 END IF 
            END IF            
!
            CALL ERR_PASS ( IUER, IER )
            CALL FFITS_GETI4 ( PIM%FILE(J4)%FITS_DESC, IND_SOU_TAB(J4), J18, &
     &                         PIM%FILE(J4)%KEY(IND_SOU_IND,IND_SOU_TAB(J4)), &
     &                         1, ISOU_FIL(J18,J4), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J18, STR )
                 CALL ERR_LOG ( 7195, IUER, 'PIMA_INDX', 'Error in '// &
     &               'reading the index of the '//STR(1:I_LEN(STR))//'-th '// &
     &               'source in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                 RETURN
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL FFITS_GETR8 ( PIM%FILE(J4)%FITS_DESC, IND_SOU_TAB(J4), J18, &
     &                         PIM%FILE(J4)%KEY(IND_SOU_ALP,IND_SOU_TAB(J4)), &
     &                         1, R8_ARR(1+(J18-1)*2), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J18, STR )
                 CALL ERR_LOG ( 7196, IUER, 'PIMA_INDX', 'Error in '// &
     &               'getting right ascension of the '//STR(1:I_LEN(STR))// &
     &               '-th source in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                 RETURN
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            CALL FFITS_GETR8 ( PIM%FILE(J4)%FITS_DESC, IND_SOU_TAB(J4), J18, &
     &                         PIM%FILE(J4)%KEY(IND_SOU_DEC,IND_SOU_TAB(J4)), &
     &                         1, R8_ARR(2+(J18-1)*2), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J18, STR )
                 CALL ERR_LOG ( 7197, IUER, 'PIMA_INDX', 'Error in '// &
     &               'getting declination of the '//STR(1:I_LEN(STR))// &
     &               '-th source in the FITS-IDI file '//PIM%FILE(J4)%NAME  )
                 RETURN
            END IF
!
! --------- Fixing crazy right ascensions that may be written by a crazy
! --------- EVN hadrware correlator
!
            IF ( R8_ARR(1+(J18-1)*2) < 0.0D0 ) R8_ARR(1+(J18-1)*2) = R8_ARR(1+(J18-1)*2) + 360.0D0
!
            IND_SOU = 0
            IF ( NSOU_ORIG > 0 ) THEN
                 IND_SOU = LTM_DIF ( 0, NSOU_ORIG, C_SOU_ORIG, &
     &                               PIM%FILE(J4)%SOU_NAME_ORIG(J18) )
            END IF
            IF ( IND_SOU == 0 ) THEN
                 FITS_RA  = R8_ARR(1+(J18-1)*2)*DEG__TO__RAD
                 FITS_DEC = R8_ARR(2+(J18-1)*2)*DEG__TO__RAD
                 SOU_FITS(1) = DCOS(FITS_DEC)*DCOS(FITS_RA)
                 SOU_FITS(2) = DCOS(FITS_DEC)*DSIN(FITS_RA)
                 SOU_FITS(3) = DSIN(FITS_DEC)
!
! -------------- Search the source PIM%FILE(J4)%SOU_NAME_ORIG(J18)(1:10)
! -------------- in this order:
! -------------- first:  in DB_NAME    list
! -------------- second: in J2000_NAME list
! -------------- third:  in B1950_NAME list
! -------------- fourth: in IVS_NAME   list
!
                 I_SOU = LTM_DIF ( 1, L_CSO, C_SOU_DB, PIM%FILE(J4)%SOU_NAME_ORIG(J18)(1:16) )
                 IF ( I_SOU .LE. 0 ) THEN
                      I_SOU = LTM_DIF ( 1, L_CSO, C_SOU_J2000, PIM%FILE(J4)%SOU_NAME_ORIG(J18)(1:10) )
                      IF ( I_SOU .LE. 0 ) THEN
                           I_SOU = LTM_DIF ( 1, L_CSO, C_SOU_B1950, PIM%FILE(J4)%SOU_NAME_ORIG(J18)(1:8) )
                           IF ( I_SOU .LE. 0 ) THEN
                                I_SOU = LTM_DIF ( 1, L_CSO, C_SOU_IVS, PIM%FILE(J4)%SOU_NAME_ORIG(J18)(1:8) )
                           END IF
                      END IF
                 END IF
!
                 IF ( I_SOU > 0 ) THEN
                      IF ( SOU_CAT(I_SOU)%NSYN > 0 ) THEN
!
! ------------------------ A special case of source name synonym when
! ------------------------ within an experiment the same name is used
! ------------------------ for different sources. The name is unique
! ------------------------ within a fits file, but not unique within the
! ------------------------ experiment.
! ------------------------ In that case we check all synomyms and picked that
! ------------------------ name which has associated coordinates the closest
! ------------------------ to the source coordinates in fits file
!
                           COS_ARC_MAX = DP_VV_V ( 3, SOU_FITS, SOU_CAT(I_SOU)%S_VEC )
                           IND_MAX = I_SOU
                           DO 4190 J19=1,SOU_CAT(I_SOU)%NSYN
                              COS_ARC = DP_VV_V ( 3, SOU_FITS, &
     &                                               SOU_CAT(SOU_CAT(I_SOU)%SYN_IND(J19))%S_VEC )
                              IF ( COS_ARC > COS_ARC_MAX ) THEN
                                   IND_MAX = SOU_CAT(I_SOU)%SYN_IND(J19)
                                   COS_ARC_MAX = COS_ARC
                              END IF
 4190                      CONTINUE
!
                           IF ( IND_MAX .NE. I_SOU ) THEN
                                I_SOU = 0
                                GOTO 8180
                           END IF
                      END IF ! NSYN
                      IU_SOU = 0
                      IF ( PIM%NSOU > 0 ) THEN
!
! ------------------------ Check, whether the source has already been used
!
                           IU_SOU = LTM_DIF ( 1, PIM%NSOU, C_SOU, &
     &                                        SOU_CAT(I_SOU)%IVS_NAME )
                           IF ( IU_SOU > 0 ) THEN
!
! ----------------------------- The source with the same IVS name has already
! ----------------------------- been used under different DB-name. Let us check
! ----------------------------- its coordinates. They should be the same
!
                                IF ( SOU_CAT(I_SOU)%IVS_NAME .NE. 'NOFRINGE' .AND. &
     &                               ( DABS(SOU_CAT(I_SOU)%ALPHA - PIM%SOU(IU_SOU)%ALPHA ) > 1.D-10 .OR. &
     &                                 DABS(SOU_CAT(I_SOU)%DELTA - PIM%SOU(IU_SOU)%DELTA ) > 1.D-10      ) ) THEN
!
                                     K_BAD_SOU = K_BAD_SOU + 1
                                     WRITE ( 6, 240 ) SOU_CAT(I_SOU)%IVS_NAME, &
     &                                                PIM%FILE(J4)%SOU_NAME_ORIG(J18), &
     &                                                C_SOU_ORIG(SOU_ORIG_REF(IU_SOU))
 240                                 FORMAT ( 'Position of the same source with IVS name ',A, &
     &                                        ' and db_names ',A, ' and ', A, ' are different!' )
                                END IF
                                NSOU_ORIG = NSOU_ORIG + 1
                                C_SOU_ORIG(NSOU_ORIG) = PIM%FILE(J4)%SOU_NAME_ORIG(J18)
                                SOU_ORIG_REF(NSOU_ORIG) = IU_SOU
                           END IF
                      END IF
!
                      IF ( IU_SOU == 0 ) THEN
                           PIM%NSOU = PIM%NSOU + 1
                           PIM%SOU(PIM%NSOU)%NAME       = SOU_CAT(I_SOU)%IVS_NAME
                           PIM%SOU(PIM%NSOU)%IVS_NAME   = SOU_CAT(I_SOU)%IVS_NAME
                           PIM%SOU(PIM%NSOU)%NISO       = SOU_CAT(I_SOU)%NISO
                           PIM%SOU(PIM%NSOU)%ISO_IND    = SOU_CAT(I_SOU)%ISO_IND
                           PIM%SOU(PIM%NSOU)%NSYN       = SOU_CAT(I_SOU)%NSYN
                           PIM%SOU(PIM%NSOU)%SYN_IND    = SOU_CAT(I_SOU)%SYN_IND
                           PIM%SOU(PIM%NSOU)%DB_NAME    = PIM%FILE(J4)%SOU_NAME_ORIG(J18)
                           PIM%SOU(PIM%NSOU)%J2000_NAME = SOU_CAT(I_SOU)%J2000_NAME
                           PIM%SOU(PIM%NSOU)%B1950_NAME = SOU_CAT(I_SOU)%B1950_NAME
                           PIM%SOU(PIM%NSOU)%S_VEC(1)   = SOU_CAT(I_SOU)%S_VEC(1)
                           PIM%SOU(PIM%NSOU)%S_VEC(2)   = SOU_CAT(I_SOU)%S_VEC(2)
                           PIM%SOU(PIM%NSOU)%S_VEC(3)   = SOU_CAT(I_SOU)%S_VEC(3)
                           PIM%SOU(PIM%NSOU)%ALPHA      = SOU_CAT(I_SOU)%ALPHA
                           PIM%SOU(PIM%NSOU)%DELTA      = SOU_CAT(I_SOU)%DELTA
                           PIM%SOU(PIM%NSOU)%ALPHA_INP  = R8_ARR(1+(J18-1)*2)*DEG__TO__RAD
                           PIM%SOU(PIM%NSOU)%DELTA_INP  = R8_ARR(2+(J18-1)*2)*DEG__TO__RAD
                           C_SOU(PIM%NSOU) = PIM%SOU(PIM%NSOU)%NAME
                           ALPHA_INP(PIM%NSOU) = PIM%SOU(PIM%NSOU)%ALPHA_INP  
                           DELTA_INP(PIM%NSOU) = PIM%SOU(PIM%NSOU)%DELTA_INP  
                           IF ( SOU_CAT(I_SOU)%IND_SWAP == 0 ) THEN
                                C_SWAP(PIM%NSOU) = ' '
                              ELSE
                                C_SWAP(PIM%NSOU) = SOU_CAT(SOU_CAT(I_SOU)%IND_SWAP)%IVS_NAME
                                L_SWAP = L_SWAP + 1
                           END IF
!
                           NSOU_ORIG = NSOU_ORIG + 1
                           IND_SOU = NSOU_ORIG
                           C_SOU_ORIG(IND_SOU) = PIM%FILE(J4)%SOU_NAME_ORIG(J18)
                           SOU_ORIG_REF(IND_SOU) = PIM%NSOU
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                                CALL RH_TAT ( PIM%SOU(PIM%NSOU)%ALPHA_INP, 6, STR1(1:16), IER )
                                CALL RG_TAT ( PIM%SOU(PIM%NSOU)%DELTA_INP, 5, STR2(1:16), IER )
                                CALL RH_TAT ( PIM%SOU(PIM%NSOU)%ALPHA_INP, 6, STR3(1:16), IER )
                                CALL RG_TAT ( PIM%SOU(PIM%NSOU)%DELTA_INP, 5, STR4(1:16), IER )
                                WRITE ( 6, '(A,I4,A)' ) 'PIMD_INFX Sou: ', PIM%NSOU, &
     &                                                  '  Found source  '// &
     &                                                  PIM%FILE(J4)%SOU_NAME_ORIG(J18)(1:10)// &
     &                                                  ' RA_inp: '//STR1(1:16)//' DEC_inp: '//STR2(1:16)// &
     &                                                  ' RA_use: '//STR3(1:16)//' DEC_use: '//STR4(1:16)
                           END IF
                           IF ( PIM%SOU(PIM%NSOU)%NISO > 0 ) THEN
                                DO 4200 J20=1,PIM%SOU(PIM%NSOU)%NISO
                                   IU_SOU = LTM_DIF ( 0, PIM%NSOU, C_SOU, &
     &                                                   SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%IVS_NAME )
                                   IF ( IU_SOU .LE. 0 ) THEN
                                        PIM%NSOU = PIM%NSOU + 1
                                        PIM%SOU(PIM%NSOU)%NAME       = SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%IVS_NAME
                                        PIM%SOU(PIM%NSOU)%IVS_NAME   = SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%IVS_NAME
                                        PIM%SOU(PIM%NSOU)%NISO       = SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%NISO
                                        PIM%SOU(PIM%NSOU)%ISO_IND    = SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%ISO_IND
                                        PIM%SOU(PIM%NSOU)%DB_NAME    = PIM%FILE(J4)%SOU_NAME_ORIG(J18)
                                        PIM%SOU(PIM%NSOU)%J2000_NAME = SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%J2000_NAME
                                        PIM%SOU(PIM%NSOU)%B1950_NAME = SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%B1950_NAME
                                        PIM%SOU(PIM%NSOU)%S_VEC(1)   = SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%S_VEC(1)
                                        PIM%SOU(PIM%NSOU)%S_VEC(2)   = SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%S_VEC(2)
                                        PIM%SOU(PIM%NSOU)%S_VEC(3)   = SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%S_VEC(3)
                                        PIM%SOU(PIM%NSOU)%ALPHA      = SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%ALPHA
                                        PIM%SOU(PIM%NSOU)%DELTA      = SOU_CAT(SOU_CAT(I_SOU)%ISO_IND(J20))%DELTA
                                        PIM%SOU(PIM%NSOU)%ALPHA_INP  = R8_ARR(1+(J18-1)*2)*DEG__TO__RAD
                                        PIM%SOU(PIM%NSOU)%DELTA_INP  = R8_ARR(2+(J18-1)*2)*DEG__TO__RAD
                                        ALPHA_INP(PIM%NSOU) = PIM%SOU(PIM%NSOU)%ALPHA_INP  
                                        DELTA_INP(PIM%NSOU) = PIM%SOU(PIM%NSOU)%DELTA_INP  
                                        C_SOU(PIM%NSOU) = PIM%SOU(PIM%NSOU)%NAME
!
                                        NSOU_ORIG = NSOU_ORIG + 1
                                        IND_SOU = NSOU_ORIG
                                        C_SOU_ORIG(IND_SOU) = PIM%FILE(J4)%SOU_NAME_ORIG(J18)
                                        SOU_ORIG_REF(IND_SOU) = PIM%NSOU
                                        IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                                              CALL RH_TAT ( PIM%SOU(PIM%NSOU)%ALPHA_INP, 6, STR1(1:16), IER )
                                              CALL RG_TAT ( PIM%SOU(PIM%NSOU)%DELTA_INP, 5, STR2(1:16), IER )
                                              CALL RH_TAT ( PIM%SOU(PIM%NSOU)%ALPHA,     6, STR3(1:16), IER )
                                              CALL RG_TAT ( PIM%SOU(PIM%NSOU)%DELTA,     5, STR4(1:16), IER )
                                              IF ( STR2(1:1) == ' ' ) STR2(1:1) = '+'
                                              IF ( STR4(1:1) == ' ' ) STR4(1:1) = '+'
                                              WRITE ( 6, 244 ) PIM%NSOU, &
     &                                                         PIM%SOU(PIM%NSOU-1)%NAME, &
     &                                                         PIM%SOU(PIM%NSOU-1)%DB_NAME, &
     &                                                         J20, &
     &                                                         PIM%SOU(PIM%NSOU)%NAME, &
     &                                                         PIM%SOU(PIM%NSOU)%DB_NAME, &
     &                                                         STR1(1:16), STR2(1:16), &
     &                                                         STR3(1:16), STR4(1:16)
 244                                          FORMAT ( 'PIMA_INDX Sou: ', I4, '  multiple prev ', &
     &                                                   A,  ' prev_db ', A, ' mul: ', I1, &
     &                                                  ' || ', ' spawned ', A, ' spawned_db ', A, &
     &                                                  ' RA_inp: ',A, ' DEC_inp: ', A, &
     &                                                  ' RA_use: ',A, ' DEC_use: ', A  )
                                        END IF
                                     ELSE
                                        CONTINUE 
                                   END IF
 4200                           CONTINUE
                           END IF
                           GOTO 8180
                       END IF ! IU_SOU
                 END IF ! I_SOU > 0
 8180            CONTINUE
                 IF ( I_SOU .LE. 0                    .AND. &
     &                R8_ARR(1+(J18-1)*2) .NE. 0.0D0  .AND.  &
     &                R8_ARR(2+(J18-1)*2) .NE. 0.0D0         ) THEN
!
                      CALL RH_TAT ( R8_ARR(1+(J18-1)*2)*DEG__TO__RAD, 4, &
     &                              STR1(1:14), IER )
                      CALL RG_TAT ( R8_ARR(2+(J18-1)*2)*DEG__TO__RAD, 3, &
     &                              STR2(1:14), IER )
                      IF ( STR2(1:1) == ' ' ) STR2(1:1) = '+'
                      FL_FAILURE = .TRUE.
                      WRITE ( 6, '(A)' ) 'Cannot find source '// &
     &                                    PIM%FILE(J4)%SOU_NAME_ORIG(J18)// &
     &                                   ' with right ascension '// &
     &                                    STR1(1:14)// &
     &                                   ' and declination '//STR2(1:14)
                  END IF ! I_SOU = 0
                  IF ( PIM%CONF%DEBUG_LEVEL > 4 ) THEN
                       CALL RH_TAT ( PIM%SOU(PIM%NSOU)%ALPHA_INP, 6, &
     &                               STR1(1:16), -3 )
                       CALL RG_TAT ( PIM%SOU(PIM%NSOU)%DELTA_INP, 5, &
     &                               STR2(1:16), -3 )
                       IF ( STR2(1:1) == ' ' ) STR2(1:1) = '+'
                       CALL RH_TAT ( PIM%SOU(PIM%NSOU)%ALPHA, 6, &
     &                               STR3(1:16), -3 )
                       CALL RG_TAT ( PIM%SOU(PIM%NSOU)%DELTA, 5, &
     &                               STR4(1:16), -3 )
                       IF ( STR4(1:1) == ' ' ) STR4(1:1) = '+'
                       WRITE ( 6, 151 ) J4, J18, PIM%FILE(J4)%SOU_NAME_ORIG(J18), &
     &                                  I_SOU, IU_SOU, PIM%NSOU, STR1(1:16), STR2(1:16), &
     &                                  STR3(1:16), STR4(1:16)
 151                   FORMAT ( 'PIMA_INDX: SI File ', I3, ' Sou: ', I5, &
     &                          ' Orig_name: ', A, ' I_sou: ', I5, &
     &                          ' IU_sou: ', I4, ' nsou= ', I4, 44X, &
     &                          ' RA_INP: ', A, ' DEC_INP: ', A, &
     &                          ' RA_APR: ', A, ' DEC_APR: ', A )
                  END IF
               ELSE
                  IF ( PIM%CONF%DEBUG_LEVEL > 4 ) THEN
                       WRITE ( 6, 153 ) J4, J18, PIM%FILE(J4)%SOU_NAME_ORIG(J18), &
     &                                  IND_SOU, PIM%NSOU
 153                   FORMAT ( 'PIMA_INDX: SI File ', I3, ' Sou: ', I5, &
     &                          ' Orig_name: ', A, &
     &                          ' IND_SOU: ', I3, ' nsou= ', I4 )
                  END IF
            END IF ! IND_SOU
 4180    CONTINUE
!
         DO 4210 J21=1,PIM%FILE(J4)%NUM_UV_TAB
            STR = PIM%FILE(J4)%KEY(IND_UV_NX2(J21,J4), &
     &                             PIM%FILE(J4)%IND_UV_TAB(J21))(10:)
            CALL CHASHL ( STR )
            IP = INDEX ( STR, ' ' )
            IF ( IP .LE. 1 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J21, STR )
                 CALL ERR_LOG ( 7198, IUER, 'PIMA_INDX', 'Failure to get '// &
     &               'the number of UV-data from the '//STR(1:I_LEN(STR))// &
     &               'th UV_DATA table in FITS-IDI file '// &
     &               PIM%FILE(J4)%NAME(1:I_LEN(PIM%FILE(J4)%NAME))//' '// &
     &               PIM%FILE(J4)%KEY(IND_UV_NX2(J21,J4), &
     &                                PIM%FILE(J4)%IND_UV_TAB(J21)) )
                 RETURN
            END IF
!
            READ ( UNIT=STR(1:IP-1), FMT='(I11)', IOSTAT=IER ) KUV(J21,J4)
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( J21, STR )
                 CALL ERR_LOG ( 7199, IUER, 'PIMA_INDX', 'Failure to decode '// &
     &               'the number of UV-data from the '//STR(1:I_LEN(STR))// &
     &               'th UV_DATA table in FITS-IDI file '// &
     &               PIM%FILE(J4)%NAME(1:I_LEN(PIM%FILE(J4)%NAME))//' '// &
     &               PIM%FILE(J4)%KEY(IND_UV_NX2(J21,J4), &
     &                                PIM%FILE(J4)%IND_UV_TAB(J21)) )
                 RETURN
            END IF
            PIM%NUV = PIM%NUV + KUV(J21,J4)
 4210    CONTINUE
!
         CALL ERR_PASS    ( IUER, IER )
         CALL FFITS_CLOSE ( PIM%FILE(J4)%FITS_DESC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7200, IUER, 'PIMA_INDX', 'Error in an '// &
     &            'attempt to close FITS UV-file '//PIM%FILE(J4)%NAME )
              RETURN
         END IF
!
         DEALLOCATE ( BB_SP_CHAN_IND )
         DEALLOCATE ( SIDE_BAND      )
         DEALLOCATE ( FREQ           )
         DEALLOCATE ( BAND_WIDTH     )
         DEALLOCATE ( CHAN_WIDTH     )
  440 CONTINUE
      IF ( FL_FAILURE ) THEN
           CALL ERR_LOG ( 7201, IUER, 'PIMA_INDX', 'Cannot find '// &
     &         'one or more sources in the source from the table '// &
     &         'of the UV-files '// &
     &         ' in source file '//PIM%CONF%SOU_NAMES_FILE )
           RETURN
      END IF
!
      IF ( K_BAD_SOU > 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( K_BAD_SOU, STR )
           CALL ERR_LOG ( 7202, IUER, 'PIMA_INDX', STR(1:I_LEN(STR))// &
     &         ' different db-sources have the same IVS name' )
           RETURN
      END IF
!
      ALLOCATE ( PIM%FREQ_ARR(PIM%NCHN,PIM%NFRQ,PIM%NFRG), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*PIM%NCHN*PIM%NFRQ*PIM%NFRG, STR )
           CALL ERR_LOG ( 7203, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' byte of dynamic memory for the '// &
     &         'array PIM%FREQ_ARR' )
           RETURN
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A,I4)' ) 'PIMA_INDX: Stations: ', PIM%NSTA
           WRITE ( 6, '(A,I4)' ) 'PIMA_INDX: Sources:  ', PIM%NSOU
           WRITE ( 6, '(A)'    ) 'PIMA_INDX: Various list sorting '//GET_CDATE()
           CALL FLUSH ( 6 )
      END IF
!
! --- Sorting various lists
!
      C_SOU_UNSORTED = C_SOU
      SOU_ORIG_REF_UNSORTED = SOU_ORIG_REF
      CALL FOR_QSORT ( PIM%STA, PIM%NSTA, SIZEOF(PIM%STA(1)), PIMA_COMPAR_STA )
      CALL FOR_QSORT ( PIM%SOU, PIM%NSOU, SIZEOF(PIM%SOU(1)), PIMA_COMPAR_SOU )
!
      IF ( PIM%NFRG > 1 ) THEN
!
! -------- Sort the frequency groups
!
           CALL FOR_QSORT ( PIM%FRQ, PIM%NFRG, SIZEOF(PIM%FRQ(1,1))*PIM__MFRQ, &
     &                      PIMA_COMPAR_FRG )
      END IF
      DO 4220 J22=1,PIM%NFRG
!
! ------ Sort frequencies within each frequency group
!
         CALL FOR_QSORT ( PIM%FRQ(1,J22), PIM%NFRQ, SIZEOF(PIM%FRQ(1,1)), &
     &                    PIMA_COMPAR_FRQ )
!
! ------ Fill array FREQ_ARR
!
         DO 4230 J23=1,PIM%NFRQ
            DO 4240 J24=1,PIM%NCHN
               PIM%FREQ_ARR(J24,J23,J22) = PIM%FRQ(J23,J22)%FREQ + &
                                    (J24-1)*PIM%FRQ(J23,J22)%CHAN_WIDTH
 4240       CONTINUE
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                 WRITE  ( 6, 127 ) J23, J22, PIM%FREQ_ARR(1,J23,J22), FREQ_FIRST_ARR_I8(J23,J22), FREQ_LAST_ARR_I8(J23,J22)
 127             FORMAT ( 'FREQ_arr: ', I4, ' Freq_grp: ', I2, ' Freq: ', 1PD16.8, &
     &                    ' Freq_first_i8: ', I15, &
     &                    ' Freq_last_i8: ', I15 )
            END IF
 4230    CONTINUE
 4220 CONTINUE
!
      DO 4250 J25=1,PIM%NSTA
         C_STA_ORIG(J25) = C_STA(J25)
         C_STA(J25) = PIM%STA(J25)%NAME
         PIM%STA(J25)%MDC%CLO_MODEL_STATUS = PIMA__UNDF
 4250 CONTINUE
!
! --- Put NLEV (the number of levels) into the station sorted array PIM%NLEV
! --- and poplarization codes
!
      NPOL_BAD = 0
      DO 4260 J26=1,PIM%NSTA
         IND_STA = LTM_DIF ( 1, PIM%NSTA, C_STA_ORIG, C_STA(J26) )
         PIM%NLEV(J26) = NLEV_UNSORTED(IND_STA)
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
              WRITE ( 6, * ) 'PIMA_INDX-2506 J26= ', INT2(J26), ' C_STA= ', C_STA(J26), &
     &                       ' C_STA_ORIG= ', C_STA_ORIG(J26), ' PIM%STA(J26)%IVS_NAME= ', PIM%STA(J26)%IVS_NAME
         END IF
         IF ( ILEN(STR_POL_CORRECT) > 0 ) THEN
              IS = INDEX ( STR_POL_CORRECT, TRIM(C_STA(J26)) )
              IF ( IS > 0 ) THEN
                   ID1 = INDEX ( STR_POL_CORRECT(IS+1:),  ':' ) + IS
                   IF ( ID1 == IS ) THEN
                        CALL ERR_LOG ( 7204, IUER, 'PIMA_INDX', 'Error in pasing '// &
     &                      'PIMAVAR_POL_CORRECT environment variable for station '// &
     &                       TRIM(C_STA(J26))//' -- a colon should follow the station name ' )
                        RETURN 
                   END IF
                   ID2 = INDEX ( STR_POL_CORRECT(ID1+1:), ':' ) + ID1
                   IF ( ID2 == ID1 ) THEN
                        CALL ERR_LOG ( 7205, IUER, 'PIMA_INDX', 'Error in pasing '// &
     &                      'PIMAVAR_POL_CORRECT environment variable for station '// &
     &                       TRIM(C_STA(J26))//' -- two numerical values separated '// &
     &                       ' by a colon should follow the station name' )
                        RETURN 
                   END IF
                   ID3 = INDEX ( STR_POL_CORRECT(ID2+1:), ',' ) + ID2
                   IF ( ID3 .LE. ID2 ) ID3 = ILEN(STR_POL_CORRECT) + 1
                   IF ( PIM%CONF%DEBUG_LEVEL > 0 ) THEN
                        CALL CHIN ( STR_POL_CORRECT(ID1+1:ID2-1), IVAL1 )
                        CALL CHIN ( STR_POL_CORRECT(ID2+1:ID3-1), IVAL2 )
                        WRITE ( 6, 131 ) C_STA(J26), POLAR_TYPA_UNSORTED(IND_STA), POLAR_TYPB_UNSORTED(IND_STA), &
     &                                   IVAL1, IVAL2
 131                    FORMAT ( 'Polarization codes for station ', A, ' were changed from ', &
     &                            I11,', ', I11, ' to ', I11, ', ', I11 )
                   END IF
!
! ---------------- The polarization code can be either a letter or a number.
! ---------------- First try whether the code is defined as a letter
!
                   POLAR_TYPA_UNSORTED(IND_STA) = LTM_DIF ( 0, PIMA__POL_MAX, PIMA__POL, STR_POL_CORRECT(ID1+1:ID2-1) )
                   POLAR_TYPB_UNSORTED(IND_STA) = LTM_DIF ( 0, PIMA__POL_MAX, PIMA__POL, STR_POL_CORRECT(ID2+1:ID3-1) )
!
! ---------------- If not, let us try whether the code is defined as a number
!
                   IF ( POLAR_TYPA_UNSORTED(IND_STA) < PIMA__POL_MIN ) THEN
                        CALL CHIN ( STR_POL_CORRECT(ID1+1:ID2-1), POLAR_TYPA_UNSORTED(IND_STA) )
                   END IF 
                   IF ( POLAR_TYPB_UNSORTED(IND_STA) < PIMA__POL_MIN ) THEN
                        CALL CHIN ( STR_POL_CORRECT(ID2+1:ID3-1), POLAR_TYPB_UNSORTED(IND_STA) )
                   END IF
                   IF ( POLAR_TYPA_UNSORTED(IND_STA) < PIMA__POL_MIN .OR. &
     &                  POLAR_TYPA_UNSORTED(IND_STA) > PIMA__POL_MAX      ) THEN
                        CALL INCH ( PIMA__POL_MIN, STR1 )
                        CALL INCH ( PIMA__POL_MAX, STR2 )
                        CALL ERR_LOG ( 7206, IUER, 'PIMA_INDX', 'Error in pasing '// &
     &                      'PIMAVAR_POL_CORRECT environment variable: pol typa for station '// &
     &                       TRIM(C_STA(J26))//' should be '// &
     &                      'in a range of '//TRIM(STR1)//' to '//TRIM(STR2)//', but got '//  &
     &                       STR_POL_CORRECT(ID1+1:ID2-1) )
                        RETURN 
                   END IF 
                   IF ( POLAR_TYPB_UNSORTED(IND_STA) < PIMA__POL_MIN .OR. &
     &                  POLAR_TYPB_UNSORTED(IND_STA) > PIMA__POL_MAX      ) THEN
                        CALL INCH ( PIMA__POL_MIN, STR1 )
                        CALL INCH ( PIMA__POL_MAX, STR2 )
                        CALL ERR_LOG ( 7207, IUER, 'PIMA_INDX', 'Error in pasing '// &
     &                      'PIMAVAR_POL_CORRECT environment variable: pol typa for station '// &
     &                       TRIM(C_STA(J26))//' should be '// &
     &                      'in a range of '//TRIM(STR1)//' to '//TRIM(STR2)//', but got '//  &
     &                       STR_POL_CORRECT(ID1+1:ID2-1) )
                        RETURN 
                   END IF 
              END IF
         END IF
         PIM%STA(J26)%POL_TYP(1) = POLAR_TYPA_UNSORTED(IND_STA)
         PIM%STA(J26)%POL_TYP(2) = POLAR_TYPB_UNSORTED(IND_STA)
         PIM%STA(J26)%POL_ANG(1:PIM%NFRQ) = POLAR_ANG_UNSORTED(1:PIM%NFRQ,IND_STA)
!
         IF ( PIM%STA(J26)%POL_TYP(1) < PIMA__POL_MIN .OR. &
     &        PIM%STA(J26)%POL_TYP(1) > PIMA__POL_MAX .OR. &
     &        PIM%STA(J26)%POL_TYP(2) < PIMA__POL_MIN .OR. &
     &        PIM%STA(J26)%POL_TYP(2) > PIMA__POL_MAX      ) THEN
!
              WRITE ( 6, 133 ) C_STA(J26), PIM%STA(J26)%POL_TYP(1), PIM%STA(J26)%POL_TYP(2), &
     &                         PIMA__POL_MIN, PIMA__POL_MAX
 133          FORMAT ( 'PIMA_INDX: Wrong polarization codes for station ', A, &
     &                 'TYPEA: ', I11, '  TYPEB: ', I11/ &
     &                 '           it should be in a range of ', I2, ' to ', I2 )
              NPOL_BAD = NPOL_BAD + 1
         END IF
 4260 CONTINUE
      IF ( NPOL_BAD > 0 ) THEN
           WRITE ( 6, 137 ) NPOL_BAD
 137       FORMAT ( 'PIMA_INDX: Wrong polarization code was found for ', I2, ' stations'/ &
     &              '           Solution: a) to fix damaged FITS-IDI file '/ &
     &              '                     b) to use environment variable PIMAVAR_POL_CORRECT (see the manual)'/ &
     &              '                     '/&
     &              '           Format: comma-separated fields SS:X:Y, where '/&
     &              '                   SS is the station name as defined in the FITS file, '/&
     &              '                   X is typa polarization code (1st pol)'/&
     &              '                   Y is typb polarization code (2nd pol)'/&
     &              '                     as defined in extedned FITS-IDI specifications '/&
     &              '                     (see also pima.i for variables that start with PIMA__POL_'/&
     &              '           '/&
     &              '                   Examples: PV:1:2,YS:1:2'/&
     &              '                             YS:R:L,YJ:H:V'/&
     &              '           '/&
     &              'This is the fatal error.' )
           CALL EXIT ( 1 )
      END IF
!
! --- Update of the SOU_ORIG_REF -- cross reference from the original
! --- source name to the IVS source name in the sorted list
!
      DO 4270 J27=1,PIM%NSOU
         PIM%C_SOU(J27) = PIM%SOU(J27)%NAME
 4270 CONTINUE
!
! --- Check for sources that are part of a multiple object. We need to update
! --- their indexes
!
      FL_FAILURE = .FALSE.
      DO 4280 J28=1,PIM%NSOU
         IF ( PIM%SOU(J28)%NISO > 0 ) THEN
              DO 4290 J29=1,PIM%SOU(J28)%NISO
                 IND_SOU = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, &
     &                               SOU_CAT(PIM%SOU(J28)%ISO_IND(J29))%IVS_NAME )
                 PIM%SOU(J28)%ISO_IND(J29) = IND_SOU
                 IF ( IND_SOU < 1 ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL ERR_LOG ( 7208, IER, 'PIMA_INDX', 'Trap of internal '// &
     &                    'control: cannot find corresponding index for '// &
     &                    'the multiple source '//PIM%C_SOU(J28) )
                      FL_FAILURE = .TRUE.
                 END IF
 4290         CONTINUE
         END IF
!
         IND_SOU_UNSRT = LTM_DIF ( 0, PIM%NSOU, C_SOU_UNSORTED, PIM%C_SOU(J28) )
         IF ( ILEN(C_SWAP(IND_SOU_UNSRT)) > 0 ) THEN
              PIM%SOU(J28)%IND_SWAP = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, C_SWAP(IND_SOU_UNSRT) )
              IF ( PIM%SOU(J28)%IND_SWAP .GE. 1 ) THEN
                   IND_SWAP_UNSRT = LTM_DIF ( 0, PIM%NSOU, C_SOU_UNSORTED, PIM%C_SOU(PIM%SOU(J28)%IND_SWAP) )
                   PIM%SOU(J28)%ALPHA_INP = ALPHA_INP(IND_SWAP_UNSRT)
                   PIM%SOU(J28)%DELTA_INP = DELTA_INP(IND_SWAP_UNSRT)
                ELSE
                   WRITE ( 6, * ) 'L_SWAP= ', L_SWAP
                   WRITE ( 6, * ) 'C_SWAP(IND_SOU_UNSRT)= ', C_SWAP(IND_SOU_UNSRT)
                   IF ( STR_SWAP_NOERR(1:1) == 'y' .OR. STR_SWAP_NOERR(1:1) == 'Y' ) THEN
                        IER = -1
                        CALL ERR_LOG ( 7209, IER, 'PIMA_INDX', 'Trap of internal '// &
     &                      'control: cannot find swap index for source '//PIM%C_SOU(J28)// &
     &                      ', but nevertheless, continue since PIMAVAR_SWAP_NOERR = YES' )
                     ELSE
                        IER = -1
                        CALL ERR_LOG ( 7210, IER, 'PIMA_INDX', 'Trap of internal '// &
     &                      'control: cannot find swap index for source '//PIM%C_SOU(J28) )
                        FL_FAILURE = .TRUE.
                   END IF
              END IF
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
              IF ( IS_R8_NAN ( PIM%SOU(J28)%ALPHA_INP ) ) THEN
                   IER = -1
                   CALL ERR_LOG ( 7211, IER, 'PIMA_INDX', 'Trap of internal '// &
     &                 'control: ALPHA_INP is nan for source '//PIM%SOU(J28)%NAME )
                   RETURN 
              END IF
              IF ( IS_R8_NAN ( PIM%SOU(J28)%DELTA_INP ) ) THEN
                   IER = -1
                   CALL ERR_LOG ( 7212, IER, 'PIMA_INDX', 'Trap of internal '// &
     &                 'control: DELTA_INP is nan for source '//PIM%SOU(J28)%NAME )
                   RETURN 
              END IF
              CALL RH_TAT ( PIM%SOU(J28)%ALPHA_INP, 6, STR1(1:16), IER )
              CALL RG_TAT ( PIM%SOU(J28)%DELTA_INP, 5, STR2(1:16), IER )
              CALL RH_TAT ( PIM%SOU(J28)%ALPHA,     6, STR3(1:16), IER )
              CALL RG_TAT ( PIM%SOU(J28)%DELTA,     5, STR4(1:16), IER )
              IF ( STR2(1:1) == ' ' ) STR2(1:1) = '+'
              IF ( STR4(1:1) == ' ' ) STR4(1:1) = '+'
              WRITE ( 6, 144 ) J28, PIM%SOU(J28)%NAME, PIM%SOU(J28)%J2000_NAME, &
     &                         STR1(1:16), STR2(1:16), STR3(1:16), STR4(1:16)
 144          FORMAT ( 'PIMA_INDX sou ', I4, ' Name: ', A, ' J_name: ', A, &
     &                 ' RA_inp ', A, ' DEC_inp ', A, &
     &                 ' RA_apr ', A, ' DEC_apr ', A  )
         END IF
 4280 CONTINUE
!
      IF ( FL_FAILURE ) THEN
           CALL ERR_LOG ( 7211, IUER, 'PIMA_INDX', 'Failure in checking source '// &
     &                   'catalogue '//PIM%CONF%SOU_NAMES_FILE )
           RETURN 
      END IF
!
      DO 4300 J30=1,NSOU_ORIG
         IF ( SOU_ORIG_REF_UNSORTED(J30) < 1 .OR. &
     &        SOU_ORIG_REF_UNSORTED(J30) > NSOU_ORIG ) THEN
              WRITE ( 6, * ) 'J1 = ', J1, &
     &                       ' SOU_ORIG_REF_UNSORTED(J30) = ', SOU_ORIG_REF_UNSORTED(J30)
              CALL ERR_LOG ( 7212, IUER, 'PIMA_INDX', 'Trap of internal '// &
     &            'control: wrong index of SOU_ORIG_REF_UNSORTED(J30) ' )
              RETURN
         END IF
         SOU_ORIG_REF(J30) = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, &
     &                                 C_SOU_UNSORTED(SOU_ORIG_REF_UNSORTED(J30)) )
 4300 CONTINUE
!
      CALL ERR_PASS   ( IUER, IER )
      CALL FFITS_OPEN ( PIM%FILE(1)%NAME, PIM%FILE(1)%FITS_DESC, 'OLD', &
     &                  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7213, IUER, 'PIMA_INDX', 'Error in an '// &
     &         'attempt to open FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      CALL ERR_PASS    ( IUER, IER )
      CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, PIM%FILE(1)%IND_UV_TAB(1), 1, &
     &            PIM%FILE(1)%KEY(IND_UV_DAT(1,1),PIM%FILE(1)%IND_UV_TAB(1)), &
     &            1, JD_DAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 1, STR )
           CALL ERR_LOG ( 7214, IUER, 'PIMA_INDX', 'Error in '// &
     &          'getting Julian date for the '//STR(1:I_LEN(STR))// &
     &          '-th UV data of the FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
      PIM%MJD_0 = NINT ( JD_DAT - 2400000.5D0 )
      IF ( STR_MJD_MINUS_ONE == 'YES' ) THEN
           PIM%MJD_0 = PIM%MJD_0 - 1
           PIM%TAI_0 = PIM%TAI_0 - 86400.0D0
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      IF ( PIM%GENERATOR == 'AIPS' ) THEN
           CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, IND_CL_TAB(1), 1, &
     &                        PIM%FILE(1)%KEY(IND_UV_TIM(1,1),IND_CL_TAB(1)), &
     &                        1, TIM, IER )
         ELSE
           CALL FFITS_GETR8 ( PIM%FILE(1)%FITS_DESC, &
     &              PIM%FILE(1)%IND_UV_TAB(1), 1, &
     &              PIM%FILE(1)%KEY(IND_UV_TIM(1,1),PIM%FILE(1)%IND_UV_TAB(1)), &
     &              1, TIM, IER )
      END IF
!
      CALL ERR_PASS    ( IUER, IER )
      CALL FFITS_CLOSE ( PIM%FILE(1)%FITS_DESC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7215, IUER, 'PIMA_INDX', 'Error in an '// &
     &         'attempt to close FITS UV-file '//PIM%FILE(1)%NAME )
           RETURN
      END IF
!
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 1, STR )
           CALL ERR_LOG ( 7216, IUER, 'PIMA_INDX', 'Error in '// &
     &         'getting time in seconds  for the '//STR(1:I_LEN(STR))// &
     &         '-th UV data of the FITS-IDI file '//PIM%FILE(1)%NAME  )
           RETURN
      END IF
      IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
           PIM%TAI_0 = TIM*86400.0D0
        ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
           PIM%TAI_0 = TIM*86400.0D0 - PIM%UTC_MTAI
      END IF
!
      ALLOCATE ( PIM%UV_IND(PIM%NUV), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NUV*SIZEOF(PIM%UV_IND(1)), STR )
           CALL ERR_LOG ( 7217, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &         'UV indexes' )
           RETURN
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_INDX: Read UV-data         '//GET_CDATE()
           CALL FLUSH ( 6 )
      END IF
      IUV = 0
      DO 4340 J34=1,PIM%L_FIL
         CALL ERR_PASS   ( IUER, IER )
         CALL FFITS_OPEN ( PIM%FILE(J34)%NAME, PIM%FILE(J34)%FITS_DESC, &
     &                     'OLD', IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7218, IUER, 'PIMA_INDX', 'Error in an '// &
     &             'attempt to open FITS UV-file '//PIM%FILE(J34)%NAME )
              RETURN
         END IF
!
! ------ Build reference table from FITS UV antenna index to PIM%STA
!
         DO 4350 J35=1,ANT(J34)%N_ARR
            IND_STA = LTM_DIF ( 0, PIM%NSTA, C_STA, ANT(J34)%NAM(J35) )
            PIM%REF_STA(ANT(J34)%REF(J35),J34) = IND_STA
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN 
                 WRITE ( 6, * ) 'PIMA_INDX-2742  Ind_fil ', INT2(J34), ' J35= ', INT2(J35), ' Ant_nam: ', ANT(J34)%NAM(J35), &
     &                          ' PIM%REF_STA(', INT2(PIM%REF_STA(ANT(J34)%REF(J35),J34)), ')= ', INT2(IND_STA)
            END IF            
 4350    CONTINUE
         DEALLOCATE ( ANT(J34)%NAM )
         DEALLOCATE ( ANT(J34)%REF )
!
! ------ Build reference table from FITS UV source index to PIM%SOU
!
         DO 4360 J36=1,KSOU(J34)
            IND_SOU_ORIG = LTM_DIF ( 0, NSOU_ORIG, C_SOU_ORIG, &
     &                               PIM%FILE(J34)%SOU_NAME_ORIG(J36) )
            IF ( IND_SOU_ORIG > 0 ) THEN
                 IND_SOU = SOU_ORIG_REF(IND_SOU_ORIG)
               ELSE
!
! -------------- The situation is possible that the source is in the table
! -------------- but has never been observed
!
                 IND_SOU = 0
            END IF
            PIM%REF_SOU(ISOU_FIL(J36,J34),J34) = IND_SOU
 4360    CONTINUE
!
! ------ Build the reference table for frequencies and frequency groups
!
         PIM%FILE(J34)%REF_FRQ = 0
         PIM%FILE(J34)%REF_FRG = 0
         PIM%FILE(J34)%REV_FRQ = 0
         PIM%FILE(J34)%REV_FRG = 0
!
         CALL GETENVAR ( 'PIMA_DUP_FREQ', STR_DUP )
!
! ------ Now we build the direct cross referennce table from file-defined
! ------ frequencies to the global frequency array and the reverse
! ------ reference table
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
              WRITE ( 6, * ) 'PIMA_INDX File ind: ', INT2(J34), ' File: ', &
     &                       TRIM(PIM%FILE(J34)%ORIG_NAME), &
     &                       ' FILE_NFRQ= ', INT2(PIM%FILE(J34)%NFRQ), &
     &                       ' FILE_NFRG= ', INT2(PIM%FILE(J34)%NFRG)
         END IF
!
         DO 4370 J37=1,PIM%FILE(J34)%NFRG
            IF ( REF_GRP(J37) == 0 ) THEN
                 WRITE ( 6, * ) 'J37=', J37,  ' J34= ', J34
                 WRITE ( 6, * ) ' NFRG_FIL= ', PIM%FILE(J34)%NFRG, &
     &                          ' NFRQ_FIL= ', PIM%FILE(J34)%NFRQ, &
     &                          ' NFRQ_GLO= ', PIM%NFRG
                 CALL ERR_LOG ( 7219, IUER, 'PIMA_INDX', 'Trap of '// &
     &               'internal control: cannot find a frequency group' )
                 RETURN
            END IF
            DO 4380 J38=1,PIM%FILE(J34)%NFRQ
!
! ------------ We need to build a cross-reference table from the file-defined
! ------------ frequency group index to the global frequency group index
! 
! ------------ IND_FRQ index of the J38-th frequency from the J34-th file in the global frequency table
! ------------ IND_FRG index of the J38-th frequency group from the J34-th file in the global frequency group table
!
               IND_FIRST_CHN = 0
               IND_LAST_CHN  = 0
               IND_FRG = 0
               DO 4390 J39=1,PIM%NFRG
                  IND_FIRST_CHN = IFIND_PL8 ( INT8(PIM%NFRQ), &
     &                               FREQ_FIRST_ARR_I8(1,J39), &
     &                               FRQ_ORIG(J38,J37,J34)%FREQ_I8 )
                  IND_LAST_CHN  = IFIND_PL8 ( INT8(PIM%NFRQ), &
     &                               FREQ_LAST_ARR_I8(1,J39), &
     &                               FRQ_ORIG(J38,J37,J34)%FREQ_I8 + &
     &                               NINT ( FRQ_ORIG(J38,J37,J34)%BAND_WIDTH, KIND=8 ) )
                  IF ( IND_FIRST_CHN > 0 .AND. IND_LAST_CHN > 0 ) THEN
                       IND_FRQ = IND_FIRST_CHN
                       IND_FRG = J39
                       GOTO 8390
                  END IF
 4390          CONTINUE
 8390          CONTINUE
               IF ( IND_FRQ < 1 .OR. IND_FRG < 1 ) THEN
                    WRITE ( 6, * ) ' RR: J37= ', J37, ' J38= ', J38
                    WRITE ( 6, * ) ' IND_FRQ= ', IND_FRQ, ' IND_FRG= ', IND_FRG
                    WRITE ( 6, * ) ' KFRG=1: FREQ_FIRST_ARR_I8(1:PIM%NFRQ,1) = ', FREQ_FIRST_ARR_I8(1:PIM%NFRQ,1)
                    WRITE ( 6, * ) ' KFRG=1: FREQ_LAST_ARR_I8(1:PIM%NFRQ,1)  = ', FREQ_LAST_ARR_I8(1:PIM%NFRQ,1)
!
                    CALL CLRCH ( STR1 ) 
                    CALL CLRCH ( STR2 ) 
                    WRITE ( UNIT=STR1, FMT='(I14)' ) FRQ_ORIG(J38,J37,J34)%FREQ_I8
                    WRITE ( UNIT=STR2, FMT='(I14)' ) FRQ_ORIG(J38,J37,J34)%FREQ_I8 + &
     &                                               NINT ( FRQ_ORIG(J38,J37,J34)%BAND_WIDTH )
                    CALL ERR_LOG ( 7220, IUER, 'PIMA_INDX', 'Trap of '// &
     &                  'internal control: cannot find in the '// &
     &                  'internal table the IF that starts at '// &
     &                   STR1(1:I_LEN(STR1))//' Hz and ends at '// &
     &                   STR2(1:I_LEN(STR2))//' Hz' )
                    RETURN 
               END IF
!
! ------------ IND_FRQ -- frequency index       in the global table
! ------------ IND_FRG -- frequency group index in the global table
!
               IF ( IND_FRQ == 0 ) THEN
                    WRITE ( 6, * ) ' SS: J37= ', J37, ' J38= ', J38
                    WRITE ( 6, * ) ' IND_FRQ= ', IND_FRQ, ' IND_FRG= ', IND_FRG
                    WRITE ( 6, * ) ' KFRG=1: FREQ_FIRST_ARR_I8(1:PIM%NFRQ,1) = ', FREQ_FIRST_ARR_I8(1:PIM%NFRQ,1)
                    WRITE ( 6, * ) ' KFRG=1: FREQ_LAST_ARR_I8(1:PIM%NFRQ,1)  = ', FREQ_LAST_ARR_I8(1:PIM%NFRQ,1)
                    CALL CLRCH ( STR )
                    WRITE ( UNIT=STR(1:14), FMT='(I14)' ) FRQ_ORIG(J38,J37,J34)%FREQ_I8
                    CALL ERR_LOG ( 7221, IUER, 'PIMA_INDX', 'Trap of '// &
     &                 'internal control: cannot find frequency '// &
     &                  STR(1:I_LEN(STR))//' defined in file '// &
     &                  PIM%FILE(J34)%NAME )
                    RETURN
               END IF
!
! ------------ %REF_FRQ and %REF_FRG are index in the global frequency table
! ------------ of frequency group table.
!
! ------------ The arguments for %REF_FRQ, %REF_FRG are
! ------------ frequency index and frequency group defined for the J34-th file
!
               PIM%FILE(J34)%REF_FRQ(J38,J37) = IND_FRQ
               PIM%FILE(J34)%REF_FRG(J38,J37) = IND_FRG
!
! ------------ %REV_FRQ and %REV_FRG are indexes of frequency and frequency group 
! ------------ from the global frequency table in the frequency table of the 
! ------------ J34-th file
!
               PIM%FILE(J34)%REV_FRQ(IND_FRQ,IND_FRG) = J38
               PIM%FILE(J34)%REV_FRG(IND_FRQ,IND_FRG) = J37
               IF ( PIM%FILE(J34)%REF_FRQ(J38,J37) == 0 ) THEN
                    WRITE ( 6, * ) 'J37=', J37, ' J38= ', J38, &
     &                             ' J20= ', J20, &
     &                             ' FRQ= ', FRQ_ORIG(J38,J37,J34)%FREQ_I8
                    WRITE ( 6, * ) ' NFRG_FIL= ', PIM%FILE(J34)%NFRG, &
     &                             ' NFRQ_FIL= ', PIM%FILE(J34)%NFRQ, &
     &                             ' NFRQ_GLO= ', PIM%NFRG
                    CALL ERR_LOG ( 7222, IUER, 'PIMA_INDX', 'Trap of '// &
     &                  'internal control: cannot find a frequency' )
                    RETURN
               END IF
               IF ( IND_FRQ < PIM%NFRQ ) THEN
!
! ----------------- It is possible that frequencies for LCP and RCP follow
! ----------------- each others. Then we have two slots with the same frequency.
! ----------------- We need to fill them. We search, whether the data have
! ----------------- the second time the same frequency. If yes, then put it
!
                    IF ( FREQ_FIRST_ARR_I8(1,IND_FRG) > 0 ) THEN
                         IND_FRQ_2ND = IFIND_PL8 ( INT8(PIM%NFRQ), &
     &                                   FREQ_FIRST_ARR_I8(IND_FRQ+1,IND_FRG), &
     &                                   FRQ_ORIG(J38,J37,J34)%FREQ_I8 ) + IND_FRQ
                       ELSE
                         FIRST_NZERO = PIM%NFRQ
                         DO 5400 J40=1,PIM%NFRQ
                            IF ( FREQ_FIRST_ARR_I8(J40,IND_FRG) > 0 ) THEN
                                 FIRST_NZERO = J40
                                 GOTO 8400
                            END IF 
 5400                    CONTINUE 
 8400                    CONTINUE 
                         IND_FRQ_2ND = IFIND_PL8 ( INT8(PIM%NFRQ), &
     &                                   FREQ_FIRST_ARR_I8(IND_FRQ+1,IND_FRG), &
     &                                   FRQ_ORIG(J38,J37,J34)%FREQ_I8 ) + IND_FRQ - &
     &                                   (FIRST_NZERO - 1)
                    END IF
                    IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                         WRITE ( 6, * ) 'PINA_INDX-2638 IND_FRQ', IND_FRQ, &
     &                                  ' FRQ_ORIG= ', FRQ_ORIG(J38,J37,J34)%FREQ_I8, &
     &                                  ' J34,J37,J38= ', INT2(J34), INT2(J37), INT2(J38), &
     &                                  ' f8 = ', FREQ_FIRST_ARR_I8(IND_FRQ+1,IND_FRG), &
     &                                  ' IND_FRQ_2ND= ', IND_FRQ_2ND, ' PIM%NFRQ= ', PIM%NFRQ
                    END IF
                    IF ( IND_FRQ_2ND > PIM__MFRQ ) THEN
                         CALL ERR_LOG ( 7223, IUER, 'PIMA_INDX', 'Trap of internal '// &
     &                       'control: frequency index '//STR(1:I_LEN(STR))// &
     &                       ' is beyond the maximum PIMA__MFRQ= '// &
     &                       STR1(1:I_LEN(STR1))//'. This means either the experiment '// &
     &                       'has more than '//STR1(1:I_LEN(STR1))//' IFs or the '// &
     &                       'the FITS-file defines zero frequencies, which is '// &
     &                       ' an indication of some error. You can re-run pima with '// &
     &                       'DEBUG_LEVEL: 6 to see diagnostics' )
                         RETURN 
                    END IF
                    IF ( STR_DUP == 'FIRST' ) THEN
                         IF ( J38 .NE. 1 ) GOTO 4380
                    END IF
                    IF ( STR_DUP == 'LAST' ) THEN
                         IF ( J38 .NE. PIM%FILE(J34)%NFRQ ) GOTO 4380
                    END IF
                    IF ( IND_FRQ_2ND > IND_FRQ ) THEN
                         PIM%FILE(J34)%REF_FRQ(J38,J37) = IND_FRQ_2ND
                         PIM%FILE(J34)%REF_FRG(J38,J37) = IND_FRG
!
                         PIM%FILE(J34)%REV_FRQ(IND_FRQ_2ND,IND_FRG) = J38
                         PIM%FILE(J34)%REV_FRG(IND_FRQ_2ND,IND_FRG) = J37
                    END IF
               END IF
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                    WRITE ( 6, 220 ) J34, PIM%FILE(J34)%NAME(1:I_LEN(PIM%FILE(J34)%NAME)), &
     &                               J38, J37, &
     &                               PIM%FILE(J34)%REF_FRQ(J38,J37), &
     &                               PIM%FILE(J34)%REF_FRG(J38,J37), &
     &                               FRQ_ORIG(J38,J37,J34)%FREQ, &
     &                               PIM%FILE(J34)%REV_FRQ(J38,IND_FRG), &
     &                               PIM%FILE(J34)%REV_FRG(J38,IND_FRG)
 220                FORMAT ( 'PIMA_INDX: File ', I3, ' Name: ', A, &
     &                       ' File_ind_frq: ', I3, ' File_ind_frg: ', I3, &
     &                       ' Glob_ind_frq: ', I3, ' Glob_ind_frg: ', I3, &
     &                       ' Freq: ', F14.1, ' Hz', &
     &                       ' Ind_rev_frq: ', I3, ' Ind_rev_frg: ', I3 )
                    CALL FLUSH ( 6 )
               END IF
 4380       CONTINUE
 4370    CONTINUE
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
              WRITE ( 6, '(A,I11)' ) '# num_uv_tab= ', PIM%FILE(J34)%NUM_UV_TAB
              CALL FLUSH ( 6 )
         END IF
!
         DO 4400 J40=1,PIM%FILE(J34)%NUM_UV_TAB
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                 WRITE ( 6, '(A,I6,A,I11)' ) '# file_id ', J34, ' nuv= ', KUV(J40,J34)
                 CALL FLUSH ( 6 )
            END IF
            DO 4410 J41=1,KUV(J40,J34)
               IF ( PIM%CONF%DEBUG_LEVEL .EQ. 15 ) THEN
                    IF ( MOD(J41,1024) == 0 .OR. J41 == KUV(J40,J34) ) THEN
                         WRITE ( 6, '(A,I11, " ( ", I11, " ) " )' ) '#  ', J41, KUV(J40,J34)
                         CALL FLUSH ( 6 )
                    END IF
               END IF
               CALL ERR_PASS ( IUER, IER )
               CALL FFITS_GETR8 ( PIM%FILE(J34)%FITS_DESC, &
     &                            PIM%FILE(J34)%IND_UV_TAB(J40), J41, &
     &                            PIM%FILE(J34)%KEY(IND_UV_DAT(J40,J34), &
     &                            PIM%FILE(J34)%IND_UV_TAB(J40)), &
     &                            1, JD_DAT, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J41, STR )
                    CALL ERR_PASS ( IUER, IER )
                    CALL ERR_LOG ( 7224, IER, 'PIMA_INDX', 'Error in '// &
     &                  'getting Julian date for the '//STR(1:I_LEN(STR))// &
     &                  '-th UV data of the FITS-IDI file '// &
     &                  PIM%FILE(J34)%NAME  )
                    KUV(J40,J34) = J41-1
                    GOTO 4410
               END IF
               IF ( JD_DAT < PIMA__JD_DATE_1970 .OR. &
     &              JD_DAT > PIMA__JD_DATE_2050      ) THEN
!
                    CALL CLRCH ( STR )
                    CALL INCH  ( J41, STR )
                    CALL CLRCH ( STR1 )
                    WRITE ( UNIT=STR1(1:15), FMT='(1PD15.7)' ) JD_DAT
!
                    CALL ERR_PASS ( IUER, IER )
                    CALL ERR_LOG ( 7225, IER, 'PIMA_INDX', 'Wrong '// &
     &                  'JD date was found '//STR1(1:I_LEN(STR1))// &
     &                  ' for the '//STR(1:I_LEN(STR))// &
     &                  '-th UV data of the FITS-IDI '// &
     &                  'file '//PIM%FILE(J34)%NAME(1:I_LEN(PIM%FILE(J34)%NAME))// &
     &                  ' Hint: if CHECK_SEVERITY < 2, PIMA would have proceeded' )
                    IF ( PIM%CONF%CHECK_SEVERITY < 2 ) THEN
                         WRITE ( 6, '(A)' ) 'Nevertheless, continue'
                         GOTO 4410
                       ELSE
                         CALL ERR_PASS ( IER, IUER )
                         RETURN
                    END IF
               END IF
               MJD = NINT ( JD_DAT - 2400000.5D0 )
!
               CALL ERR_PASS ( IUER, IER )
               IF ( PIM%GENERATOR == 'AIPS' ) THEN
                    CALL FFITS_GETR8 ( PIM%FILE(J34)%FITS_DESC, &
     &                         PIM%FILE(J34)%IND_UV_TAB(J40), J41, &
     &                         PIM%FILE(J34)%KEY(IND_UV_TIM(J40,J34), &
     &                         IND_CL_TAB(J40)), 1, TIM, IER )
                  ELSE
                    CALL FFITS_GETR8 ( PIM%FILE(J34)%FITS_DESC, &
     &                         PIM%FILE(J34)%IND_UV_TAB(J40), J41, &
     &                         PIM%FILE(J34)%KEY(IND_UV_TIM(J40,J34), &
     &                         PIM%FILE(J34)%IND_UV_TAB(J40)), &
     &                         1, TIM, IER )
               END IF
!
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J41, STR )
                    CALL ERR_PASS ( IUER, IER )
                    CALL ERR_LOG ( 7226, IER, 'PIMA_INDX', 'Error in '// &
     &                  'getting time in seconds  for the '// &
     &                   STR(1:I_LEN(STR))//'-th UV data of the FITS-IDI '// &
     &                  'file '//PIM%FILE(J34)%NAME  )
                    IF ( PIM%CONF%CHECK_SEVERITY < 2 ) THEN
                         WRITE ( 6, '(A)' ) 'Nevertheless, continue'
                         GOTO 4410
                       ELSE
                         CALL ERR_PASS ( IER, IUER )
                         RETURN
                    END IF
               END IF
               IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                    SEC = TIM*86400.0D0
                  ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                    SEC = TIM*86400.0D0 - PIM%UTC_MTAI
               END IF
               IF ( SEC < 0.0D0 ) THEN
                    SEC = SEC + 86400.0D0
                    MJD = MJD - 1
               END IF
               IF ( SEC > 86400.0D0 ) THEN
                    SEC = SEC - 86400.0D0
                    MJD = MJD + 1
               END IF
               TIM = (MJD - PIM%MJD_0)*86400.0D0 + (SEC - PIM%TAI_0)
               IF ( TIM < 0.D0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J40, STR  )
                    CALL CLRCH ( STR1      )
                    CALL INCH  ( J41, STR1 )
                    WRITE ( 6, * ) ' TIM= ', TIM, ' KUV(J40,J34) = ', KUV(J40,J34)
                    TIM = 0.001
                    CALL ERR_PASS ( IUER, IER )
                    CALL ERR_LOG ( 7227, IER, 'PIMA_INDX', 'Too early '// &
     &                  'time tag for the '// &
     &                   STR1(1:I_LEN(STR1))//'-th UV data point in '// &
     &                  'the '//STR(1:I_LEN(STR))//' th table of the '// &
     &                  'FITS-IDI file '// &
     &                   PIM%FILE(J34)%NAME(1:I_LEN(PIM%FILE(J34)%NAME))// &
     &                  ' -- please check whether the files are properly '// &
     &                  'ordered' )
                    IF ( PIM%CONF%CHECK_SEVERITY < 2 ) THEN
                         WRITE ( 6, '(A)' ) 'Nevertheless, continue'
                         GOTO 4410
                       ELSE
                         CALL ERR_PASS ( IER, IUER )
                         RETURN
                    END IF
               END IF
               TIM_I8 = NINT ( TIM*1.D5, KIND=8 )
               IF ( PIM%NEPC == 0 ) THEN
                    IND_EPC = 0
                  ELSE
                    IF ( TIM_I8 == PIM%TIM_I8(PIM%NEPC) ) THEN
!
! ---------------------- This is done for performance improvement, since the data
! ---------------------- *most of the time* are in time order
!
                         IND_EPC = PIM%NEPC
                       ELSE
                         IND_EPC = IFIND_PL8 ( INT8(PIM%NEPC), PIM%TIM_I8, TIM_I8 )
                    END IF
               END IF
               IF ( IND_EPC .LE. 0 ) THEN
                    PIM%NEPC = PIM%NEPC + 1
                    IF ( PIM%NEPC > PIM__MEPC ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( PIM__MEPC, STR )
                         CALL ERR_LOG ( 7228, IUER, 'PIMA_INDX', 'Too many '// &
     &                       'time epochs for UV data: more than PIM__MEPC: '// &
     &                        STR )
                         RETURN
                    END IF
                    PIM%TIM_R8(PIM%NEPC) = TIM
                    PIM%TIM_I8(PIM%NEPC) = TIM_I8
                    IND_EPC = PIM%NEPC
               END IF
!
               CALL ERR_PASS ( IUER, IER )
               CALL FFITS_GETI4 ( PIM%FILE(J34)%FITS_DESC, &
     &                PIM%FILE(J34)%IND_UV_TAB(J40), J41, &
     &                PIM%FILE(J34)%KEY(IND_UV_BAS(J40,J34), &
     &                                  PIM%FILE(J34)%IND_UV_TAB(J40)), &
     &                1, BAS_ID, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J41, STR )
                    CALL ERR_LOG ( 7229, IUER, 'PIMA_INDX', 'Error in '// &
     &                  'getting baseline id for the '//STR(1:I_LEN(STR))// &
     &                  '-th UV data of the FITS-IDI file '// &
     &                   PIM%FILE(J34)%NAME  )
                    RETURN
               END IF
!
               CALL ERR_PASS ( IUER, IER )
               CALL FFITS_GETI4 ( PIM%FILE(J34)%FITS_DESC, &
     &                            PIM%FILE(J34)%IND_UV_TAB(J40), J41, &
     &                            PIM%FILE(J34)%KEY(IND_UV_SOU(J40,J34), &
     &                                     PIM%FILE(J34)%IND_UV_TAB(J40)), &
     &                            1, SOU_ID, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J41, STR )
                    CALL ERR_LOG ( 7230, IUER, 'PIMA_INDX', 'Error in '// &
     &                  'getting source id for the '//STR(1:I_LEN(STR))// &
     &                  '-th UV data of the FITS-IDI file '// &
     &                   PIM%FILE(J34)%NAME  )
                    RETURN
               END IF
               IF ( SOU_ID < 1  .OR.  SOU_ID > PIM%FILE(J34)%N_SOU ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J41, STR )
                    CALL CLRCH ( STR1 )
                    CALL INCH  ( SOU_ID, STR1 )
                    CALL ERR_PASS ( IUER, IER )
                    CALL ERR_LOG ( 7231, IER, 'PIMA_INDX', 'Wrong value of '// &
     &                  'source id '//STR1(1:I_LEN(STR1))//' for the '// &
     &                   STR(1:I_LEN(STR))// &
     &                  '-th UV data of the FITS-IDI file '// &
     &                   PIM%FILE(J34)%NAME  )
                    IF ( PIM%CONF%CHECK_SEVERITY < 2 ) THEN
                         WRITE ( 6, '(A)' ) 'Nevertheless, continue'
                         KUV(J40,J34) = J41-1
                         GOTO 4410
                       ELSE
                         CALL ERR_PASS ( IER, IUER )
                         RETURN
                    END IF
               END IF
!
               CALL ERR_PASS ( IUER, IER )
               CALL FFITS_GETI4 ( PIM%FILE(J34)%FITS_DESC, &
     &                            PIM%FILE(J34)%IND_UV_TAB(J40), J41, &
     &                            PIM%FILE(J34)%KEY(IND_UV_FRQ(J40,J34), &
     &                                     PIM%FILE(J34)%IND_UV_TAB(J40)), &
     &                            1, FRG_ID, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J41, STR )
                    CALL ERR_LOG ( 7232, IUER, 'PIMA_INDX', 'Error in '// &
     &                  'getting source id for the '//STR(1:I_LEN(STR))// &
     &                  '-th UV data of the FITS-IDI file '// &
     &                   PIM%FILE(J34)%NAME  )
                    RETURN
               END IF
!
               IF ( FRG_ID < 1  .OR.  FRG_ID > PIM__MFRG ) THEN
                    IF ( STR_ZERO_FRG_ID_BYPASS == 'YES' ) THEN
                         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                              WRITE ( 6, * ) 'PIMA_INDX Obs ', J41, &
     &                                       ' has frg_id ', FRG_ID
                         END IF
                         FRG_ID = 1
                       ELSE
                         CALL CLRCH ( STR )
                         CALL INCH  ( J41, STR )
                         CALL CLRCH ( STR1 )
                         CALL INCH  ( FRG_ID, STR1 )
                         CALL ERR_PASS ( IUER, IER )
                         CALL ERR_LOG ( 7233, IUER, 'PIMA_INDX', 'Wrong value '// &
     &                       'of frequency group id '//STR1(1:I_LEN(STR1))// &
     &                       ' for the '//STR(1:I_LEN(STR))// &
     &                       '-th UV data of the FITS-IDI file '// &
     &                       PIM%FILE(J34)%NAME  )
                         IF ( PIM%CONF%CHECK_SEVERITY < 2 ) THEN
                              WRITE ( 6, '(A)' ) 'Nevertheless, continue'
                              KUV(J40,J34) = J41-1
                              GOTO 4410
                           ELSE
                              CALL ERR_PASS ( IER, IUER )
                              RETURN
                         END IF
                    END IF
               END IF
!
               CALL ERR_PASS ( IUER, IER )
               CALL FFITS_GETR4 ( PIM%FILE(J34)%FITS_DESC, &
     &                            PIM%FILE(J34)%IND_UV_TAB(J40), J41, &
     &                            PIM%FILE(J34)%KEY(PIM%FILE(J34)%IND_INTTIM_KEY(J40), &
     &                                     PIM%FILE(J34)%IND_UV_TAB(J40)), &
     &                            1, AP_LEN_R4, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J41, STR )
                    CALL ERR_LOG ( 7234, IUER, 'PIMA_INDX', 'Error in '// &
     &                  'getting source id for the '//STR(1:I_LEN(STR))// &
     &                  '-th UV data of the FITS-IDI file '// &
     &                   PIM%FILE(J34)%NAME  )
                    RETURN
               END IF
               IF ( ILEN(STR_PIMAVAR_AP_LEN) > 0 ) THEN
                    READ ( UNIT=STR_PIMAVAR_AP_LEN, FMT='(F10.5)' ) AP_LEN_R4
               END IF
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 23 ) THEN
                    WRITE ( 6, * ) 'PIMA_INDX File ', J34, ' AP_LEN= ', AP_LEN_R4
               END IF
!
               IND_STA1 = BAS_ID/256
               IND_STA2 = BAS_ID - IND_STA1*256
               IUV = IUV + 1
               PIM%UV_IND(IUV)%TIM_IND = IND_EPC
               PIM%UV_IND(IUV)%FIL_IND = J34
               PIM%UV_IND(IUV)%TAB_IND = J40
               PIM%UV_IND(IUV)%POI_IND = J41
               PIM%UV_IND(IUV)%POI_AUT_IND(1) = 0
               PIM%UV_IND(IUV)%POI_AUT_IND(2) = 0
               PIM%UV_IND(IUV)%STA_IND(1) = PIM%REF_STA(IND_STA1,J34)
               PIM%UV_IND(IUV)%STA_IND(2) = PIM%REF_STA(IND_STA2,J34)
               IF ( SOU_ID == 0 ) THEN
                    PIM%UV_IND(IUV)%POI_IND = PIMA__NO_SOUID
                  ELSE
                    PIM%UV_IND(IUV)%SOU_IND = PIM%REF_SOU(SOU_ID,J34)
               END IF
               PIM%UV_IND(IUV)%SCA_IND    = 0
               PIM%UV_IND(IUV)%OBS_IND    = 0
               PIM%UV_IND(IUV)%FRG_IND    = PIM%FILE(J34)%REF_FRG(1,FRG_ID)
               PIM%UV_IND(IUV)%AP_LEN     = AP_LEN_R4
               IF ( IUV == 1 ) THEN
                    PIM%AP_LEN_MIN = AP_LEN_R4
                    PIM%AP_LEN_MAX = AP_LEN_R4
                  ELSE
                    PIM%AP_LEN_MIN = MIN ( PIM%AP_LEN_MIN, AP_LEN_R4 )
                    PIM%AP_LEN_MAX = MAX ( PIM%AP_LEN_MAX, AP_LEN_R4 )
               END IF
               PIM%UV_IND(IUV)%NEXT_UV_IND = 0
!
               IF ( PIM%UV_IND(IUV)%STA_IND(1) .LE. 0 ) THEN
                    WRITE ( 6, * ) ' IND_STA1=',IND_STA1, ' IND_STA2= ', IND_STA2, &
     &                             ' IUV= ', IUV, ' PIM%REF_STA(IND_STA1,J34) = ', &
     &                               PIM%REF_STA(IND_STA1,J34)
                    WRITE ( 6, * ) ' PIM%REF_STA =', PIM%REF_STA(:,J34), &
     &                             ' UV_INDS: ', PIM%UV_IND(IUV)%STA_IND(1:2) 
                    CALL CLRCH ( STR )
                    CALL INCH  ( IUV, STR )
                    CALL ERR_LOG ( 7235, IUER, 'PIMA_INDX', 'Track of '// &
     &                  'internal control: no station index is associated '// &
     &                  'with visibility '//STR )
                    RETURN
               END IF
!
               IF ( PIM%UV_IND(IUV)%STA_IND(2) .LE. 0 ) THEN
                    WRITE ( 6, * ) ' IND_STA2=',IND_STA2, &
     &                             ' PIM%REF_STA(IND_STA2,J34) = ', &
     &                               PIM%REF_STA(IND_STA2,J34)
                    WRITE ( 6, * ) ' PIM%UV_IND(IUV)%FIL_IND = ', PIM%UV_IND(IUV)%FIL_IND
                    WRITE ( 6, * ) ' PIM%REF_STA =', PIM%REF_STA
                    CALL CLRCH ( STR )
                    CALL INCH  ( IUV, STR )
                    CALL ERR_LOG ( 7236, IUER, 'PIMA_INDX', 'Track of '// &
     &                  'internal control: no station index is associated '// &
     &                  'with visibility '//STR )
                    RETURN
               END IF
               IF ( FL_PRINT_UV_INFO ) THEN
                    WRITE  ( 6, 224 ) IUV, PIM%UV_IND(IUV)%FIL_IND, &
     &                                     PIM%UV_IND(IUV)%TAB_IND, &
     &                                     PIM%UV_IND(IUV)%POI_IND, &
     &                                     PIM%UV_IND(IUV)%TIM_IND, &
     &                                     PIM%UV_IND(IUV)%STA_IND, &
     &                                     SOU_ID
 224                FORMAT ( 'UV_RAW: ', i9, ' Fil_ind: ', I3, ' uv_fil_ind: ', I9, &
     &                       ' poi_ind: ', I9, ' tim_ind: ', I6, &
     &                       ' sta_ind: ', I3, 1X, I3, ' sou_id: ', I4 )
               END IF
 4410       CONTINUE
 8410       CONTINUE
 4400    CONTINUE
!
         CALL ERR_PASS    ( IUER, IER )
         CALL FFITS_CLOSE ( PIM%FILE(J34)%FITS_DESC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7237, IUER, 'PIMA_INDX', 'Error in an '// &
     &            'attempt to close FITS UV-file '//PIM%FILE(J34)%NAME )
              RETURN
         END IF
 4340 CONTINUE
!
      DEALLOCATE ( ANT )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_INDX: Sort UV-data         '//GET_CDATE()
           CALL FLUSH ( 6 )
      END IF
!
! --- Allocate memory for unsorted array. We needed it for awhile before
! --- we fix indexes in UV arrays
!
      ALLOCATE   ( TIM_I8_NSRT(PIM%NEPC), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*PIM%NEPC, STR )
           CALL ERR_LOG ( 7238, IUER, 'PIMA_INDX', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &         'TIM_I8_NSRT array' )
           RETURN
      END IF
!
! --- Copy the time unsorted index into TIM_I8_NSRT
!
      CALL LIB$MOVC3 ( 8*PIM%NEPC, PIM%TIM_I8, TIM_I8_NSRT )
!
! --- Sort time indexes
!
      CALL FOR_QSORT ( PIM%TIM_I8, PIM%NEPC, SIZEOF(PIM%TIM_I8(1)), PIMA_COMPAR_I8 )
      CALL FOR_QSORT ( PIM%TIM_R8, PIM%NEPC, SIZEOF(PIM%TIM_R8(1)), PIMA_COMPAR_R8 )
!
! --- Correct time indexes in UV_IND array for changes in the order of time
! --- indexes in the array TIM_R8 which just has been sorted
!
      DO 4420 J42=1,PIM%NUV
         IF ( PIM%UV_IND(J42)%TIM_IND .LE. 0  .OR. &
     &        PIM%UV_IND(J42)%TIM_IND .GT. PIM%NEPC  ) THEN
!
              PIM%UV_IND(J42)%POI_IND = PIMA__NO_TIM
              PIM%UV_IND(J42)%POI_AUT_IND(1) = -1
              PIM%UV_IND(J42)%POI_AUT_IND(2) = -1
!
              WRITE  ( 6, 230 ) J42
 230          FORMAT ( '$$$ PIMA_INDEX Point ', I8,' does not have time ', &
     &                 'index. Nevertheless, continue' )
              GOTO 4420
         END IF
!
! ------ Fix the time index which was changed after sorting
!
         IND_EPC = IFIND_SORT_PL8 ( INT8(PIM%NEPC), PIM%TIM_I8, &
     &                              TIM_I8_NSRT(PIM%UV_IND(J42)%TIM_IND) )
         IF ( IND_EPC .LE. 0 ) THEN
              WRITE ( 6, * ) ' J42=',J42,' IND_EPC=',IND_EPC, &
     &                       ' TIM_IND = ', PIM%UV_IND(J42)%TIM_IND, &
     &                       ' TIM_I8_NSRT=',TIM_I8_NSRT(PIM%UV_IND(J42)%TIM_IND)
              WRITE ( 6, * ) ' TIM_I8(1:8) = ', PIM%TIM_I8(1:8)
              WRITE ( 6, * ) ' PIM%NEPC = ', PIM%NEPC
              WRITE ( 6, * ) ' TIM_I8 last = ', PIM%TIM_I8(PIM%NEPC-7:PIM%NEPC)
              CALL ERR_LOG ( 7239, IUER, 'PIMA_INDX', 'Trap of internal '// &
     &            'control: index of the epoch was not found after sorting' )
              DEALLOCATE ( TIM_I8_NSRT )
              RETURN
         END IF
!
         PIM%UV_IND(J42)%TIM_IND = IND_EPC
         PIM%UV_IND(J42)%ORIG_IND = J42
 4420 CONTINUE
      DEALLOCATE ( TIM_I8_NSRT )
!
! --- Sort UV data
!
      CALL FOR_QSORT ( PIM%UV_IND, PIM%NUV, SIZEOF(PIM%UV_IND(1)), &
     &                 PIMA_COMPAR_UV_IND )
!
      PIM%FRG_USE = PIM%CONF%FRG_USE 
      PIM%VIRT_NFRG = PIM%CONF%FRG_LIST(2) - PIM%CONF%FRG_LIST(1) + 1
      IF ( PIM%FRG_USE == PIMA__MERGE  .AND.  PIM%VIRT_NFRG > 1 ) THEN
           DO 4430 J43=1,PIM%NUV-1
              IF ( PIM%UV_IND(J43)%TIM_IND    == PIM%UV_IND(J43+1)%TIM_IND    .AND. &
     &             PIM%UV_IND(J43)%STA_IND(1) == PIM%UV_IND(J43+1)%STA_IND(1) .AND. &
     &             PIM%UV_IND(J43)%STA_IND(2) == PIM%UV_IND(J43+1)%STA_IND(2) .AND. &
     &             PIM%UV_IND(J43)%SOU_IND    == PIM%UV_IND(J43+1)%SOU_IND          ) THEN
!
                   PIM%UV_IND(J43)%NEXT_UV_IND = J43+1
              END IF
 4430      CONTINUE 
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A,I2)' ) 'PIMA_INDX  PIM%VIRT_NFRG = ', &
     &                            PIM%VIRT_NFRG
           CALL FLUSH ( 6 )
      END IF
!
      N_DUP = 0
      DUP_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.dup'
!
      IP = I_LEN(PIM%CONF%EXPER_DIR)
      IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
           DUP_FILE = PIM%CONF%EXPER_DIR(1:IP)//DUP_FILE
         ELSE
           DUP_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//DUP_FILE
      END IF
      LUN_DUP = GET_UNIT()
      OPEN ( UNIT=LUN_DUP, FILE=DUP_FILE, STATUS='UNKNOWN', &
     &       IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 7240, IUER, 'PIMA_INDX', 'Failure to '// &
     &         'open for wriring information file '// &
     &          DUP_FILE(1:I_LEN(DUP_FILE))//' -- error '//STR )
           RETURN
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_INDX: Check duplicate points'
           CALL FLUSH ( 6 )
      END IF
!
      N_BAD = 0
      ALLOCATE ( UV_BAD(2*PIM%NUV), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*PIM%NUV, STR )
           CALL ERR_LOG ( 7241, IUER, 'PIMA_INDX', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array UV_BAD' )
           RETURN
      END IF
!
! --- Check for duplicate points and match autocorrelation uv data with
! --- cross-correlation data
!
      TIM_USED = 1.0D21
      DO 4440 J44=1,PIM%NUV
         IF ( PIM%UV_IND(J44)%POI_IND == PIMA__NO_SOUID ) THEN
              N_BAD = N_BAD + 1
              UV_BAD(N_BAD) = J44
              GOTO 4440
         END IF
         IF ( PIM%UV_IND(J44)%POI_IND        .LE. 0 ) GOTO 4440
         IF ( PIM%UV_IND(J44)%POI_AUT_IND(1) .LT. 0 ) GOTO 4440
         IF ( PIM%UV_IND(J44)%POI_AUT_IND(2) .LT. 0 ) GOTO 4440
!
         PREV_UV_IND = 0
         IF ( PIM%FRG_USE == PIMA__MERGE  .AND. &
     &        PIM%VIRT_NFRG > 1           .AND.  &
     &        J44 > 1 ) THEN
!
              IND_UV_BEG = MAX ( 1, J44-PIM__MFRG )
              DO 4450 J45=IND_UV_BEG,J44-1
                 IF ( PIM%UV_IND(J45)%NEXT_UV_IND == J44 ) THEN
                      PREV_UV_IND = J44
                 END IF
 4450         CONTINUE 
!
! ----------- Check for a broken chain
!
              IF ( PIM%UV_IND(J44)%FRG_IND == 1 ) THEN
                   UV_IND = J44
                   DO 4460 J46=1,PIM%VIRT_NFRG-1
                      IF ( PIM%UV_IND(UV_IND)%NEXT_UV_IND == 0 ) THEN
!
! ------------------------ Aga! The next UV index is missing. Mark this point as bad
!
                           N_BAD = N_BAD + 1
                           UV_BAD(N_BAD) = J44
                           PIM%UV_IND(J44)%POI_IND = PIMA__CHAIN
                           GOTO 4440
                      END IF
                      UV_IND = PIM%UV_IND(UV_IND)%NEXT_UV_IND 
 4460              CONTINUE 
              END IF
         END IF
!
         IF ( PIM%UV_IND(J44)%TIM_IND > 0  .AND.  PIM%UV_IND(J44)%SOU_IND > 0 ) THEN
              IF ( TIM_USED(PIM%UV_IND(J44)%SOU_IND) > 1.0D20 ) THEN
!
! ---------------- Set the time tag of the first used point
!
                   TIM_USED(PIM%UV_IND(J44)%SOU_IND) = PIM%TIM_R8(PIM%UV_IND(J44)%TIM_IND)
              END IF
!
! ----------- Check the time interval between the current and the last used
! ----------- point for that source. If time interval does not exceed
! ----------- PIM%CONF%MIN_SCAN_LEN, the interval should be commensurate
! ----------- to the AP_LEN (with tolerance of PIM%CONF%AP_TOLERANCE).
! ----------- If this condition is viloated, the point is consideted
! ----------- as "different AP" duplicate
!
              TIM_DIF = PIM%TIM_R8(PIM%UV_IND(J44)%TIM_IND) - TIM_USED(PIM%UV_IND(J44)%SOU_IND)
              IF ( TIM_DIF > PIM%CONF%AP_TOLERANCE .AND. &
     &             DABS( IDNINT ( TIM_DIF/PIM%UV_IND(J44)%AP_LEN )*PIM%UV_IND(J44)%AP_LEN - TIM_DIF ) > PIM%CONF%AP_TOLERANCE .AND. &
     &             TIM_DIF < PIM%CONF%MIN_SCAN_LEN .AND. &
     &             PREV_UV_IND == 0                      ) THEN
!
! ---------------- Aga. We found the duplicate. Mark it
!
                   PIM%UV_IND(J44)%POI_IND = PIMA__DFAP
                   PIM%UV_IND(J44)%POI_AUT_IND(1) = -1
                   PIM%UV_IND(J44)%POI_AUT_IND(2) = -1
!
                   IF ( PIM%UV_IND(J44)%SOU_IND .GE. 1 .AND. &
     &                  PIM%UV_IND(J44)%SOU_IND .LE. PIM%NSOU ) THEN
                        STR(1:8) = PIM%C_SOU(PIM%UV_IND(J44)%SOU_IND)
                      ELSE
                        STR(1:8) = 'Unknown '
                   END IF
                   IF ( PIM%UV_IND(J44)%STA_IND(1) .GE. 1 .AND. &
     &                  PIM%UV_IND(J44)%STA_IND(1) .LE. PIM%NSTA ) THEN
                        STR(11:18) = PIM%STA(PIM%UV_IND(J44)%STA_IND(1))%IVS_NAME
                      ELSE
                        STR(11:18) = 'Unknown '
                   END IF
                   IF ( PIM%UV_IND(J44)%STA_IND(2) .GE. 1 .AND. &
     &                  PIM%UV_IND(J44)%STA_IND(2) .LE. PIM%NSTA ) THEN
                        STR(21:28) = PIM%STA(PIM%UV_IND(J44)%STA_IND(2))%IVS_NAME
                      ELSE
                        STR(21:28) = 'Unknown '
                   END IF
                   N_DUP = N_DUP + 1
                   WRITE ( UNIT=LUN_DUP, FMT=250 ) N_DUP, J44, &
     &                     PIM%UV_IND(J44)%TIM_IND, &
     &                     PIM%UV_IND(J44)%FRG_IND, &
     &                     STR(1:8), STR(11:18), STR(21:28), &
     &                     PIM%UV_IND(J44)%FIL_IND, &
     &                     PIM%UV_IND(J44)%FIL_IND, &
     &                     PIM%UV_IND(J44)%POI_IND, &
     &                     PIM%UV_IND(J44)%POI_IND, '@A'
                 ELSE
                   TIM_USED(PIM%UV_IND(J44)%SOU_IND) = PIM%TIM_R8(PIM%UV_IND(J44)%TIM_IND)
              END IF
            ELSE
!
! ----------- The point without time index. Mark it as duplicate
!
              PIM%UV_IND(J44)%POI_IND = PIMA__NO_TIM
              PIM%UV_IND(J44)%POI_AUT_IND(1) = -1
              PIM%UV_IND(J44)%POI_AUT_IND(2) = -1
!
              IF ( PIM%UV_IND(J44)%SOU_IND .GE. 1 .AND. &
     &             PIM%UV_IND(J44)%SOU_IND .LE. PIM%NSOU ) THEN
                   STR(1:8) = PIM%C_SOU(PIM%UV_IND(J44)%SOU_IND)
                 ELSE
                   STR(1:8) = 'Unknown '
              END IF
              IF ( PIM%UV_IND(J44)%STA_IND(1) .GE. 1 .AND. &
     &             PIM%UV_IND(J44)%STA_IND(1) .LE. PIM%NSTA ) THEN
                   STR(11:18) = PIM%STA(PIM%UV_IND(J44)%STA_IND(1))%IVS_NAME
                 ELSE
                   STR(11:18) = 'Unknown '
              END IF
              IF ( PIM%UV_IND(J44)%STA_IND(2) .GE. 1 .AND. &
     &             PIM%UV_IND(J44)%STA_IND(2) .LE. PIM%NSTA ) THEN
                   STR(21:28) = PIM%STA(PIM%UV_IND(J44)%STA_IND(2))%IVS_NAME
                 ELSE
                   STR(21:28) = 'Unknown '
              END IF
              N_DUP = N_DUP + 1
              WRITE ( UNIT=LUN_DUP, FMT=250 ) N_DUP, J44, &
     &                PIM%UV_IND(J44)%TIM_IND, &
     &                PIM%UV_IND(J44)%FRG_IND, &
     &                STR(1:8), STR(11:18), STR(21:28), &
     &                PIM%UV_IND(J44)%FIL_IND, &
     &                PIM%UV_IND(J44)%FIL_IND, &
     &                PIM%UV_IND(J44)%POI_IND, &
     &                PIM%UV_IND(J44)%POI_IND, '@B'
         END IF
!
! ------ Check PIM__MFIL points before the current.
! ------ If two points have the same
! ------ 1) time index
! ------ 2) source index
! ------ 3) the same pair of stations
! ------ 4) the same frequency code
! ------ 5) the same next UV index
! ------ they are considered as duplicates
!
         DO 4470 J47=J44-1,1,-1
            IF ( PIM%UV_IND(J47)%POI_IND        .LE. 0 ) GOTO 4470
            IF ( PIM%UV_IND(J47)%POI_AUT_IND(1) .LT. 0 ) GOTO 4470
            IF ( PIM%UV_IND(J47)%POI_AUT_IND(2) .LT. 0 ) GOTO 4470
            IF ( PIM%UV_IND(J47)%TIM_IND        .LE. 0 ) GOTO 4470
            IF ( PIM%UV_IND(J47)%TIM_IND .NE. PIM%UV_IND(J44)%TIM_IND ) GOTO 8470
!
            IF ( PIM%UV_IND(J47)%STA_IND(1) .NE. PIM%UV_IND(J47)%STA_IND(2) ) THEN
!
! -------------- Check only cross correlation points
!
                 IF ( PIM%UV_IND(J47)%TIM_IND == PIM%UV_IND(J44)%TIM_IND .AND. &
     &                PIM%UV_IND(J47)%FRG_IND == PIM%UV_IND(J44)%FRG_IND .AND. &
     &                PIM%UV_IND(J47)%SOU_IND == PIM%UV_IND(J44)%SOU_IND .AND. &
     &                PIM%UV_IND(J47)%STA_IND(1)  == PIM%UV_IND(J44)%STA_IND(1)  .AND. &
     &                PIM%UV_IND(J47)%STA_IND(2)  == PIM%UV_IND(J44)%STA_IND(2)        ) THEN
!
                      PIM%UV_IND(J47)%POI_IND = -J47
                      PIM%UV_IND(J47)%POI_AUT_IND(1) = -1
                      PIM%UV_IND(J47)%POI_AUT_IND(2) = -1
!
                      IF ( PIM%UV_IND(J47)%SOU_IND .GE. 1 .AND. &
     &                     PIM%UV_IND(J47)%SOU_IND .LE. PIM%NSOU ) THEN
                           STR(1:8) = PIM%C_SOU(PIM%UV_IND(J47)%SOU_IND)
                         ELSE
                           STR(1:8) = 'Unknown '
                      END IF
                      IF ( PIM%UV_IND(J47)%STA_IND(1) .GE. 1 .AND. &
     &                     PIM%UV_IND(J47)%STA_IND(1) .LE. PIM%NSTA ) THEN
                           STR(11:18) = PIM%STA(PIM%UV_IND(J47)%STA_IND(1))%IVS_NAME
                         ELSE
                           STR(11:18) = 'Unknown '
                      END IF
                      IF ( PIM%UV_IND(J47)%STA_IND(2) .GE. 1 .AND. &
     &                     PIM%UV_IND(J47)%STA_IND(2) .LE. PIM%NSTA ) THEN
                           STR(21:28) = PIM%STA(PIM%UV_IND(J47)%STA_IND(2))%IVS_NAME
                         ELSE
                           STR(21:28) = 'Unknown '
                      END IF
                      N_DUP = N_DUP + 1
                      WRITE ( UNIT=LUN_DUP, FMT=250 ) N_DUP, J47, &
     &                        PIM%UV_IND(J47)%TIM_IND, &
     &                        PIM%UV_IND(J47)%FRG_IND, &
     &                        STR(1:8), STR(11:18), STR(21:28), &
     &                        PIM%UV_IND(J44)%FIL_IND, &
     &                        PIM%UV_IND(J47)%FIL_IND, &
     &                        PIM%UV_IND(J47)%POI_IND, &
     &                        PIM%UV_IND(J44)%POI_IND, '@C'
  250                 FORMAT ( '$$$ PIMA_INDEX Duplicate # ', I8, &
     &                         ' point ', I8, ' tim_ind: ', I7, &
     &                         ' FRG_IND: ', I2, ' Sou: ',A, &
     &                         ' Sta_1: ', A, ' Sta_2: ', A, &
     &                         ' Fil_ind_1: ', I4, ' Fil_ind_2: ', I4, &
     &                         ' Poi_ind_1: ', I11, ' Poi_ind_2: ', I11, &
     &                         2X, A )
                 END IF
!
! -------------- Set index of the autocorrelation point
!
                 IF ( PIM%UV_IND(J47)%TIM_IND == PIM%UV_IND(J44)%TIM_IND .AND. &
     &                PIM%UV_IND(J47)%FRG_IND == PIM%UV_IND(J44)%FRG_IND .AND. &
     &                PIM%UV_IND(J47)%SOU_IND == PIM%UV_IND(J44)%SOU_IND .AND. &
     &                PIM%UV_IND(J47)%STA_IND(1)  == PIM%UV_IND(J44)%STA_IND(1)  .AND. &
     &                PIM%UV_IND(J47)%STA_IND(2)  == PIM%UV_IND(J44)%STA_IND(1)        ) THEN
!
                      PIM%UV_IND(J44)%POI_AUT_IND(1) = J47
                 END IF
                 IF ( PIM%UV_IND(J47)%TIM_IND == PIM%UV_IND(J44)%TIM_IND .AND. &
     &                PIM%UV_IND(J47)%FRG_IND == PIM%UV_IND(J44)%FRG_IND .AND. &
     &                PIM%UV_IND(J47)%SOU_IND == PIM%UV_IND(J44)%SOU_IND .AND. &
     &                PIM%UV_IND(J47)%STA_IND(1)  == PIM%UV_IND(J44)%STA_IND(2)  .AND. &
     &                PIM%UV_IND(J47)%STA_IND(2)  == PIM%UV_IND(J44)%STA_IND(2)        ) THEN
!
                      PIM%UV_IND(J44)%POI_AUT_IND(2) = J47
                 END IF
            END IF
  4470   CONTINUE
  8470   CONTINUE
!
! ------ Update autocorrelation index
!
         IF ( PIM%UV_IND(J44)%POI_IND > 0  .AND. &
     &        PIM%UV_IND(J44)%STA_IND(1) .NE. PIM%UV_IND(J44)%STA_IND(2) ) THEN
!
! ----------- The J44 -th point is the cross-correlation point
! ----------- Set autocorrelation point indexes for the J44-th point
!
! ----------- First, search backward till we reach the previous time index
!
              DO 4480 J48=J44-1,1,-1
                 IF ( PIM%UV_IND(J48)%TIM_IND .NE. PIM%UV_IND(J44)%TIM_IND ) GOTO 8480
!
                 IF ( PIM%UV_IND(J48)%POI_IND > 0                               .AND. &
     &                PIM%UV_IND(J48)%FRG_IND == PIM%UV_IND(J44)%FRG_IND        .AND. &
     &                ( PIM%UV_IND(J48)%SOU_IND == PIM%UV_IND(J44)%SOU_IND      .OR.  &
     &                  .NOT. FL_AUT_NOSOURCE_STRICT                                  ) .AND. &
     &                PIM%UV_IND(J48)%STA_IND(1)  == PIM%UV_IND(J44)%STA_IND(1) .AND. &
     &                PIM%UV_IND(J48)%STA_IND(2)  == PIM%UV_IND(J44)%STA_IND(1)       ) THEN
!
                      PIM%UV_IND(J44)%POI_AUT_IND(1) = J48
                 END IF
                 IF ( PIM%UV_IND(J48)%POI_IND > 0                                 .AND. &
     &                PIM%UV_IND(J48)%FRG_IND == PIM%UV_IND(J44)%FRG_IND          .AND. &
     &                ( PIM%UV_IND(J48)%SOU_IND == PIM%UV_IND(J44)%SOU_IND        .OR.  &
     &                  .NOT. FL_AUT_NOSOURCE_STRICT                                    ) .AND. &
     &                PIM%UV_IND(J48)%STA_IND(1)  == PIM%UV_IND(J44)%STA_IND(2)   .AND. &
     &                PIM%UV_IND(J48)%STA_IND(2)  == PIM%UV_IND(J44)%STA_IND(2)         ) THEN
!
                     PIM%UV_IND(J44)%POI_AUT_IND(2) = J48
                 END IF
 4480         CONTINUE
 8480         CONTINUE
!
! ----------- Second, search forward till we reach the next time index
!
              DO 4490 J49=J44+1,PIM%NUV
                 IF ( PIM%UV_IND(J49)%TIM_IND .NE. PIM%UV_IND(J44)%TIM_IND ) GOTO 8490
!
                 IF ( PIM%UV_IND(J49)%POI_IND > 0                               .AND. &
     &                PIM%UV_IND(J49)%FRG_IND == PIM%UV_IND(J44)%FRG_IND        .AND. &
     &                ( PIM%UV_IND(J49)%SOU_IND == PIM%UV_IND(J44)%SOU_IND .OR.       &
     &                  .NOT. FL_AUT_NOSOURCE_STRICT                                  ) .AND. &
     &                PIM%UV_IND(J49)%STA_IND(1)  == PIM%UV_IND(J44)%STA_IND(1) .AND. &
     &                PIM%UV_IND(J49)%STA_IND(2)  == PIM%UV_IND(J44)%STA_IND(1)       ) THEN
!
                      PIM%UV_IND(J44)%POI_AUT_IND(1) = J49
                 END IF
                 IF ( PIM%UV_IND(J49)%POI_IND > 0                               .AND. &
     &                PIM%UV_IND(J49)%FRG_IND == PIM%UV_IND(J44)%FRG_IND        .AND. &
     &                ( PIM%UV_IND(J49)%SOU_IND == PIM%UV_IND(J44)%SOU_IND .OR.       &
     &                  .NOT. FL_AUT_NOSOURCE_STRICT                                  ) .AND. &
     &                PIM%UV_IND(J49)%STA_IND(1)  == PIM%UV_IND(J44)%STA_IND(2) .AND. &
     &                PIM%UV_IND(J49)%STA_IND(2)  == PIM%UV_IND(J44)%STA_IND(2)       ) THEN
!
                      PIM%UV_IND(J44)%POI_AUT_IND(2) = J49
                 END IF
 4490         CONTINUE
 8490         CONTINUE
!
              IF ( PIM%UV_IND(J44)%POI_AUT_IND(1) .LE. 0 .OR. &
     &             PIM%UV_IND(J44)%POI_AUT_IND(2) .LE. 0      ) THEN
                   N_BAD = N_BAD + 1
                   UV_BAD(N_BAD) = J44
                   PIM%UV_IND(J44)%POI_IND = PIMA__AUTO_1 ! No autocorrelation
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 16 ) THEN
                        WRITE ( 6, * ) 'PIMA_INDX: point ', J44, ' has no matching autocorrelation' 
                   END IF 
              END IF
         END IF
         IF ( FL_PRINT_UV_INFO ) THEN
              WRITE  ( 6, 254 ) J44, PIM%UV_IND(J44)%FIL_IND, &
     &                          PIM%UV_IND(J44)%TAB_IND, &
     &                          PIM%UV_IND(J44)%POI_IND, &
     &                          PIM%UV_IND(J44)%TIM_IND, &
     &                          PIM%UV_IND(J44)%STA_IND, &
     &                          PIM%UV_IND(J44)%SOU_IND, &
     &                          PIM%UV_IND(J44)%ORIG_IND
 254          FORMAT ( 'UV_GLO: ', I9, ' Fil_ind: ', I3, ' uv_fil_ind: ', I9, &
     &                 ' poi_ind: ', I9, ' tim_ind: ', I6, ' sta_ind: ', I3, 1X, I3, &
     &                 ' sou_ind: ', I4, ' orig_ind: ', I9  )
         END IF
 4440 CONTINUE
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           CLOSE ( UNIT=LUN_DUP )
      END IF
!
      IF ( N_DUP > 0  .AND.  PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A,I7,A)' ) 'PIMA_INDX  ', N_DUP, &
     &            ' duplicate UV points were found'
      END IF
!
      IF ( N_BAD > 0  .AND.  PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A,I7,A)' ) 'PIMA_INDX  ', N_BAD, &
     &            ' UV points without autocorrelation were found'
      END IF
      IF ( PIM%NUV_EXC > 0 ) THEN
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                WRITE ( 6, '(A,I9,A)' ) 'PIMA_INDX:  Suppress ', PIM%NUV_EXC, &
     &                     ' excluded UV points'
           END IF
!
           N_SUP = 0
           DO 4500 J50=1,PIM%NUV
!
! ----------- Search in the array of excluded UV points
!
              IP = IFIND_SORT_PL ( PIM%NUV_EXC, PIM%UV_EXC, PIM%UV_IND(J50)%ORIG_IND  )
              IF ( IP > 0 ) THEN

! ---------------- Found??? Oh! This is the outcast. Mark it with stigma
!
                   N_SUP = N_SUP + 1
                   N_BAD = N_BAD + 1
                   UV_BAD(N_BAD) = J50
                   PIM%UV_IND(J50)%POI_IND = PIMA__EXCL
                   PIM%UV_IND(J50)%POI_AUT_IND(1) = -1
                   PIM%UV_IND(J50)%POI_AUT_IND(2) = -1
              END IF
 4500      CONTINUE
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                WRITE ( 6, '(A,I9,A)' ) 'PIMA_INDX:  ', N_SUP, &
     &                     ' points have been suppressed'
           END IF
      END IF
!
! --- Copying station and source names to PIM
!
      DO 4510 J51=1,PIM%NSTA
         PIM%C_STA(J51) = PIM%STA(J51)%IVS_NAME
 4510 CONTINUE
!
! --- Check the reference station: is it in the list?
!
      IND_REF_STA = LTM_DIF ( 1, PIM%NSTA, PIM%C_STA, PIM%CONF%STA_REF )
      IF ( IND_REF_STA .LE. 0 ) THEN
           CALL LIST_TO_LINE ( PIM%NSTA, PIM%C_STA, ', ', STR )
           CALL ERR_PASS ( IUER, IER )
           CALL ERR_LOG ( 7242, IER, 'PIMA_INDX', 'Cannot find '// &
     &         'reference station '//PIM%CONF%STA_REF//' in the '// &
     &         'station list: '//STR(1:I_LEN(STR))//' --  '// &
     &         'Please check configuration parameter STA_REF' )
           IF ( PIM%CONF%CHECK_SEVERITY < 2 ) THEN
                WRITE ( 6, '(A)' ) 'Nevertheless, continue'
              ELSE
                CALL ERR_PASS ( IER, IUER )
                RETURN
           END IF
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A,I8)' ) 'PIMA_INDX: Split data into scans N_BAD= ', N_BAD
           CALL FLUSH ( 6 )
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL PIMA_SCAN_SPLIT ( PIM, N_BAD, UV_BAD, IER )
      SPLIT_IER = IER
      IF ( IER .NE. 0 .AND. N_BAD > 0 .AND. PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL ERR_LOG ( 7243, IER, 'PIMA_INDX', 'Failure in an attempt '// &
     &         'to split the array of UV data into scans' )
        ELSE IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7244, IUER, 'PIMA_INDX', 'Failure in an attempt '// &
     &         'to split the array of UV data into scans' )
           RETURN
      END IF
!
      IF ( PIM%CONF%EXCLUDE_UV_FINAM == PIMA__EXC_AUTO ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL PIMA_UPDATE_BADUV_LIST ( PIM, N_BAD, UV_BAD, N_BAD_NEW, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7245, IUER, 'PIMA_INDX', 'Failure in '// &
     &              'an attempt to update the list of bad points' )
                DEALLOCATE ( UV_BAD )
                RETURN
           END IF
         ELSE
           N_BAD_NEW = N_BAD
      END IF
      DEALLOCATE ( UV_BAD )
!
      IF ( N_BAD_NEW > 0  .AND. PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
           WRITE ( 6, '(A,I7,A)' ) 'PIMA_INDX  ', N_BAD_NEW, ' bad new points were detected'
           WRITE ( 6, '(A)' ) 'PIMA_INDX  Abnormal exit without storing .pim file'
           CALL EXIT ( 23 )
      END IF
      IF ( SPLIT_IER > 0 ) THEN
           CALL ERR_PASS ( SPLIT_IER, IUER )
           RETURN
      END IF
!
      IF ( FL_FLAG_TAB ) THEN
!
! -------- Apply correlator flags
!
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) 'PIMA_INDX  Apply correlator flags'
!
                FLG_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.flg'
                IP = I_LEN(PIM%CONF%EXPER_DIR)
                IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                     FLG_FILE = PIM%CONF%EXPER_DIR(1:IP)//FLG_FILE
                   ELSE
                     FLG_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//FLG_FILE
                END IF
!
                LUN_FLG = GET_UNIT()
                OPEN ( UNIT=LUN_FLG, FILE=FLG_FILE, STATUS='UNKNOWN', &
     &                 IOSTAT=IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IER, STR )
                     CALL ERR_LOG ( 7246, IUER, 'PIMA_INDX', 'Failure to '// &
     &                   'open information file '// &
     &                    FLG_FILE(1:I_LEN(FLG_FILE))//' -- error '//STR )
                     RETURN
                END IF
           END IF
           DO 4520 J52=1,PIM%L_FIL
              IF ( IND_FLG_TAB(J52) == 0 ) GOTO 4520
!
              CALL ERR_PASS   ( IUER, IER )
              CALL FFITS_OPEN ( PIM%FILE(J52)%NAME, &
     &                          PIM%FILE(J52)%FITS_DESC, 'OLD', IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7247, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to open FITS UV-file '// &
     &                  PIM%FILE(J52)%NAME )
                   RETURN
              END IF
!
! ----------- We need to get MJD date of the J52-th file
!
              CALL FFITS_GETR8 ( PIM%FILE(J52)%FITS_DESC, &
     &                           PIM%FILE(J52)%IND_UV_TAB(1), 1, &
     &                           PIM%FILE(J52)%KEY(IND_UV_DAT(1,J52), &
                                                   PIM%FILE(J52)%IND_UV_TAB(1)), &
     &                           1, JD_DAT, IER )
              MJD_FILE_BEG = NINT ( JD_DAT - 2400000.5D0 )
!
! ----------- Get time tag of the first UV data in the J52-th file, We need it
! ----------- in order to learn the MJD date associated with the time tag
!
              CALL ERR_PASS ( IUER, IER )
              CALL FFITS_GETR8 ( PIM%FILE(J52)%FITS_DESC, &
     &              PIM%FILE(J52)%IND_UV_TAB(1), 1, &
     &              PIM%FILE(J52)%KEY(IND_UV_TIM(1,J52),PIM%FILE(J52)%IND_UV_TAB(1)), &
     &              1, TIM_FILE_BEG(J52), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7248, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                 'the time of the first UF data in the FITS-IDI file '// &
     &                 PIM%FILE(J52)%NAME  )
                   RETURN
              END IF
              IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                   CONTINUE
                 ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                   TIM_FILE_BEG(J52) = TIM_FILE_BEG(J52) - PIM%UTC_MTAI
              END IF
!
! ----------- Learn the number of flag records in the J52-th file
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, J52, 'FLAG    ', 'NAXIS2', KFLAG, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7249, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                 'the length of FLAG table in FITS-IDI file '// &
     &                  PIM%FILE(J52)%NAME  )
                   RETURN
              END IF
!
! ----------- Allocate dynamic memory for temporary flag data
!
              ALLOCATE ( FLAG_SOU_ID(KFLAG),       STAT=IER )
              ALLOCATE ( FLAG_STA_ID(2,KFLAG),     STAT=IER )
              ALLOCATE ( FLAG_FRQ_ID(KFLAG),       STAT=IER )
              ALLOCATE ( FLAG_TIME_RANGE(2,KFLAG), STAT=IER )
              ALLOCATE ( FLAG_SEVERITY(KFLAG),     STAT=IER )
!
              DO 4530 J53=1,KFLAG
!
! -------------- Read flag data
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J52)%FITS_DESC, IND_FLG_TAB(J52), J53, &
     &                              PIM%FILE(J52)%KEY(IND_FLAG_STATION_ID(J52), &
     &                              IND_FLG_TAB(J52)), 2, FLAG_STA_ID(1,J53), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7250, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                    'the value of key station ID  from the FLAG table '// &
     &                    'in FITS-IDI file '//PIM%FILE(J52)%NAME  )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J52)%FITS_DESC, IND_FLG_TAB(J52), J53, &
     &                              PIM%FILE(J52)%KEY(IND_FLAG_SOURCE_ID(J52), &
     &                              IND_FLG_TAB(J52)), 1, FLAG_SOU_ID(J53), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7251, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                    'the value of key SOURCE_ID  from the FLAG table '// &
     &                    'in FITS-IDI file '//PIM%FILE(J52)%NAME  )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J52)%FITS_DESC, IND_FLG_TAB(J52), J53, &
     &                              PIM%FILE(J52)%KEY(IND_FLAG_FRQ_ID(J52), &
     &                              IND_FLG_TAB(J52)), 1, FLAG_FRQ_ID(J53), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7252, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                    'the value of key SOURCE_ID  from the FLAG table '// &
     &                    'in FITS-IDI file '//PIM%FILE(J52)%NAME  )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(J52)%FITS_DESC, IND_FLG_TAB(J52), J53, &
     &                              PIM%FILE(J52)%KEY(IND_FLAG_TIME_RANGE(J52), &
     &                              IND_FLG_TAB(J52)), 2, FLAG_TIME_RANGE(1,J53), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7253, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                    'the value of key TIMERANG  from the FLAG table '// &
     &                    'in FITS-IDI file '//PIM%FILE(J52)%NAME  )
                      RETURN
                 END IF
                 IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                      CONTINUE
                    ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                      FLAG_TIME_RANGE(1,J53) = FLAG_TIME_RANGE(1,J53) - &
     &                                         PIM%UTC_MTAI/86400.0D0
                      FLAG_TIME_RANGE(2,J53) = FLAG_TIME_RANGE(2,J53) - &
     &                                         PIM%UTC_MTAI/86400.0D0
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J52)%FITS_DESC, IND_FLG_TAB(J52), J53, &
     &                              PIM%FILE(J52)%KEY(IND_FLAG_SEVERITY(J52), &
     &                              IND_FLG_TAB(J52)), 1, FLAG_SEVERITY(J53), IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7254, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                    'the value of key SEVERITY from the FLAG table '// &
     &                    'in FITS-IDI file '//PIM%FILE(J52)%NAME  )
                      RETURN
                 END IF
!
                 IER = 0
                 CALL CLRCH ( CORR_FLAG_REASON )
                 CALL FFITS_GETCH ( PIM%FILE(J52)%FITS_DESC, IND_FLG_TAB(J52), J53, &
     &                              PIM%FILE(J52)%KEY(IND_FLAG_REASON(J52), &
     &                              IND_FLG_TAB(J52)), 1, CORR_FLAG_REASON, IER )
                 IP = INDEX ( CORR_FLAG_REASON, CHAR(0) )
                 IF ( IP > 0 ) CALL CLRCH ( CORR_FLAG_REASON(IP:) ) ! Do not know why, but there is a gabage at the end
                 IF ( FLAG_SEVERITY(J53) == -1 ) THEN
!
! ------------------- Sset severity code if it is -1 (as it was in harware VLBA correlator)
!
                      IF ( INDEX ( CORR_FLAG_REASON, '2-16 ghz synthesizer #2' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, '2-16 ghz synthesizer #2' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'antenna not in point mod' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 0
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Antenna not pointed'      ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 0
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Antenna position error' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 0
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'antenna position error t' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 0
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Antenna off source' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 0
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Antenna not pointing' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 0
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'BBC Synth Unlocked' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Synth 1 out of lock' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Synth 2 out of lock' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Synth 3 out of lock' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Synth 4 out of lock' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Synth 5 out of lock' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Synth 6 out of lock' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Synth 7 out of lock' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Synth 8 out of lock' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'channel 1 bbc synthesize' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'channel 2 bbc synthesize' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'channel 3 bbc synthesize' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'channel 4 bbc synthesize' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'channel 5 bbc synthesize' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'channel 6 bbc synthesize' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'channel 7 bbc synthesize' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'channel 8 bbc synthesize' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Ellipsoid posn error'  )    > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'ellipsoid position error' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'observing system idle' )    > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Recorder 1 head posn err' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Recorder 1 not running' )   > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Recorder 2 head posn err' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Recorder 2 not running' )   > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Source change in progres' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 0
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Source change' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 0
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'source change in progres' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 0
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Subreflector error' )       > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Subreflector posn error' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Synthesizer 1 unlocked' )   > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Synthesizer 2 unlocked' )   > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Synthesizer 3 unlocked' )   > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'System idle' ) > 0              ) THEN
                           FLAG_SEVERITY(J53) = 0
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'subreflector position er' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, '2-16 ghz synthesizer #1' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, '2-16 ghz synthesizer #2' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, '2-16 ghz synthesizer #3' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Syn 1 out of lock (L404)' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'This Band/Pol not observed' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Subref focus error' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Subref rotation error' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Syn 2 out of lock (L404)' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Antenna azimith error' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                         ELSE IF ( INDEX ( CORR_FLAG_REASON, 'Antenna elevation error' ) > 0 ) THEN
                           FLAG_SEVERITY(J53) = 2
                       ELSE
                         CALL CLRCH ( STR )
                         CALL INCH  ( J1, STR )
                         CALL ERR_LOG ( 7255, IUER, 'PIMA_INDX', 'Unkwown '// &
     &                       'string reason >>'//CORR_FLAG_REASON(1:I_LEN(CORR_FLAG_REASON))// &
     &                       '<< was found in parsing input fits-idi file '// &
     &                        PIM%FILE(J52)%NAME(1:I_LEN(PIM%FILE(J52)%NAME))// &
     &                        ' -- you will need to edit pima_indx and update the list '// &
     &                        'of supported reasons'  )
                         RETURN 
                      END IF
                 END IF
!
! -------------- Learn the time tag with respect to the experiment start for
! -------------- beginning of flagged interval and its end
!
                 TIM_FLAG_BEG = &
     &                (MJD_FILE_BEG - PIM%MJD_0)*86400.0 + &
     &                (FLAG_TIME_RANGE(1,J53)*86400.0D0 - PIM%TAI_0)
                 TIM_FLAG_END = &
     &                (MJD_FILE_BEG - PIM%MJD_0)*86400.0 + &
     &                (FLAG_TIME_RANGE(2,J53)*86400.0D0 - PIM%TAI_0)
!
                 IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                      IF ( FLAG_SOU_ID(J53) > 0 ) THEN
                           IF ( PIM%REF_SOU(FLAG_SOU_ID(J53),J52) > 0 ) THEN
                                STR = PIM%C_SOU(PIM%REF_SOU(FLAG_SOU_ID(J53),J52))
                              ELSE
                                STR = 'Unknown '
                           END IF
                         ELSE
                           STR = 'Unknown '
                      END IF
!
                      WRITE ( UNIT=LUN_FLG, FMT=110, IOSTAT=IER ) J52, J53, &
     &                        PIM%C_STA(PIM%REF_STA(FLAG_STA_ID(1,J53),J52)), &
     &                        STR(1:8), FLAG_FRQ_ID(J53), FLAG_SEVERITY(J53), &
     &                        MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_FLAG_BEG, -2 ), &
     &                        MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_FLAG_END, -2 ), &
     &                        CORR_FLAG_REASON(1:I_LEN(CORR_FLAG_REASON))
 110                  FORMAT ( 'File: ',I3,' Rec: ', I5, ' Sta: ', A, ' Sou: ',A, &
     &                         ' Frg: ', I1, '  Flag: ', I2, &
     &                         ' Date: [ ',A, ' , ', A, ' ] ',A )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( IER, STR )
                           CALL ERR_LOG ( 7256, IUER, 'PIMA_INDX', 'Failure '// &
     &                         'in writing into file '// &
     &                          FLG_FILE(1:I_LEN(FLG_FILE))//' -- error '//STR )
                           RETURN
                      END IF
                 END IF
!
! -------------- Scan all observations to learn whether this flagged interval
! -------------- is applicable to this observation
!
                 DO 4540 J54=1,PIM%NOBS
                    IF ( FLAG_SOU_ID(J53) > 0 ) THEN
!
! ---------------------- If the SOURCE_ID flag is not zero, then we check
! ---------------------- whether the source id match to the source observed
! ---------------------- in the J54 -th observation
!
                         IF ( PIM%OBS(J54)%ROOT_SOU_IND .NE. PIM%REF_SOU(FLAG_SOU_ID(J53),J52) ) GOTO 4540
                    END IF
!
! ----------------- First check the range of time tags for the J54-th observation
!
                    TIM_OBS_BEG = PIM%TIM_R8(PIM%OBS(J54)%TIM_BEG_IND)
                    TIM_OBS_END = PIM%TIM_R8(PIM%OBS(J54)%TIM_END_IND)
                    IF ( ( ( TIM_FLAG_BEG > TIM_OBS_BEG - PIM%CONF%AP_TOLERANCE .AND. &
     &                       TIM_FLAG_BEG < TIM_OBS_END + PIM%CONF%AP_TOLERANCE       ) .OR. &
     &                     ( TIM_FLAG_END > TIM_OBS_BEG - PIM%CONF%AP_TOLERANCE .AND. &
     &                       TIM_FLAG_END < TIM_OBS_END + PIM%CONF%AP_TOLERANCE       ) &
     &                   ) &
     &                   .AND. &
     &                   ( FLAG_STA_ID(2,J53) == 0 .AND. &
     &                     ( PIM%OBS(J54)%STA_IND(1) == PIM%REF_STA(FLAG_STA_ID(1,J53),J52) .OR. &
     &                       PIM%OBS(J54)%STA_IND(2) == PIM%REF_STA(FLAG_STA_ID(1,J53),J52)      ) &
     &                   ) &
     &                 ) THEN
!
! -------------------- OK. It turned out the flagged interval is within this
! -------------------- observation. Then run over all accumulation periods to
! -------------------- learn whether it should be flagged or not
!
                       DO 4550 J55=1,PIM%OBS(J54)%NUVS
                          DO 4560 J56=1,PIM%OBS(J54)%NUM_EPC(J55)
                             IND_UV = PIM%OBS(J54)%UV_IND(J56,J55)
                             IF ( IND_UV > 0 .AND. &
     &                            PIM%OBS(J54)%GLO_FRG_INDS(J55) == FLAG_FRQ_ID(J53)) THEN
!
                                  TIM_UV = PIM%TIM_R8(PIM%UV_IND(IND_UV)%TIM_IND)
                                  IF ( TIM_UV > TIM_FLAG_BEG - PIM%CONF%AP_TOLERANCE .AND. &
     &                                 TIM_UV < TIM_FLAG_END + PIM%CONF%AP_TOLERANCE       ) THEN
!
! ------------------------------------ Copy the severity flag to this AP
!
                                       IF ( FLAG_SEVERITY(J53) < PIM%OBS(J54)%CORR_FLAG(J56,J55) ) THEN
                                            PIM%OBS(J54)%CORR_FLAG(J56,J55) = FLAG_SEVERITY(J53)
                                       END IF
                                  END IF
                             END IF
 4560                     CONTINUE
 4550                  CONTINUE
                    END IF
 4540            CONTINUE
 4530         CONTINUE
!
              DEALLOCATE ( FLAG_SEVERITY )
              DEALLOCATE ( FLAG_TIME_RANGE )
              DEALLOCATE ( FLAG_FRQ_ID )
              DEALLOCATE ( FLAG_STA_ID )
              DEALLOCATE ( FLAG_SOU_ID )
!
              CALL ERR_PASS    ( IUER, IER )
              CALL FFITS_CLOSE ( PIM%FILE(J52)%FITS_DESC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7257, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to close FITS UV-file '// &
     &                  PIM%FILE(J52)%NAME )
                   RETURN
              END IF
 4520    CONTINUE
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              CLOSE ( UNIT=LUN_FLG )
         END IF
      END IF
!
      IF ( FL_MOD_TAB ) THEN
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) 'PIMA_INDX  Extract the correlator model'
                MOD_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mod'
                MDU_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mdu'
                MDC_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mdc'
!
                IP = I_LEN(PIM%CONF%EXPER_DIR)
                IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                     MOD_FILE = PIM%CONF%EXPER_DIR(1:IP)//MOD_FILE
                   ELSE
                     MOD_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//MOD_FILE
                END IF
                IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                     MDU_FILE = PIM%CONF%EXPER_DIR(1:IP)//MDU_FILE
                   ELSE
                     MDU_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//MDU_FILE
                END IF
                IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                     MDC_FILE = PIM%CONF%EXPER_DIR(1:IP)//MDC_FILE
                   ELSE
                     MDC_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//MDC_FILE
                END IF
!
                LUN_MOD = GET_UNIT()
                OPEN ( UNIT=LUN_MOD, FILE=MOD_FILE, STATUS='UNKNOWN', &
     &                 IOSTAT=IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IER, STR )
                     CALL ERR_LOG ( 7258, IUER, 'PIMA_INDX', 'Failure to '// &
     &                   'open information file '// &
     &                    MOD_FILE(1:I_LEN(MOD_FILE))//' -- error '//STR )
                     RETURN
                END IF
!
                LUN_MDU = GET_UNIT()
                OPEN ( UNIT=LUN_MDU, FILE=MDU_FILE, STATUS='UNKNOWN', &
     &                 IOSTAT=IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IER, STR )
                     CALL ERR_LOG ( 7259, IUER, 'PIMA_INDX', 'Failure to '// &
     &                   'open information file '// &
     &                    MDU_FILE(1:I_LEN(MDU_FILE))//' -- error '//STR )
                     RETURN
                END IF
           END IF
!
! -------- First pass: collect information about the interferometric model
! -------- used by the correlator
!
           DO 4570 J57=1,PIM%L_FIL
              CALL ERR_PASS   ( IUER, IER )
              CALL FFITS_OPEN ( PIM%FILE(J57)%NAME, &
     &                          PIM%FILE(J57)%FITS_DESC, 'OLD', IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7260, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to open FITS UV-file '// &
     &                  PIM%FILE(J57)%NAME )
                   RETURN
              END IF
!
! ----------- Learn the number of model records in the J57-th file
!
              CALL ERR_PASS ( IUER, IER )
              IF ( PIM%GENERATOR == 'OLD_ASC' ) THEN
                   CALL PIMA_GET_KEY_I4 ( PIM, J57, 'INTERFEROMETER_ MODEL', &
         &                               'NAXIS2', KMOD, IER )
                 ELSE 
                   CALL PIMA_GET_KEY_I4 ( PIM, J57, 'INTERFEROMETER_MODEL', &
         &                               'NAXIS2', KMOD, IER )
              END IF
              IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 7261, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                'the length of the INTERFEROMETER_MODEL table in '// &
     &                'FITS-IDI file '//PIM%FILE(J57)%NAME  )
                  RETURN
              END IF
              IF ( IND_MOD_GDELAY(J57) == 0 ) THEN
                   CALL ERR_LOG ( 7262, IUER, 'PIMA_INDX', 'No keyword '// &
     &                 ' GRATE_1 found in the INTERFEROMETER_MODEL table '// &
     &                 'in the FITS-IDI file '//PIM%FILE(J57)%NAME  )
                   RETURN
              END IF
              IF ( IND_MOD_FRG(J57) == 0 ) THEN
                   CALL ERR_LOG ( 7263, IUER, 'PIMA_INDX', 'No keyword '// &
     &                 ' FREQID found in the INTERFEROMETER_MODEL table '// &
     &                 'in the FITS-IDI file '//PIM%FILE(J57)%NAME  )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              IF ( PIM%GENERATOR == 'OLD_ASC' ) THEN
                   CALL PIMA_GET_KEY_I4 ( PIM, 1, 'INTERFEROMETER_ MODEL', 'NPOLY', &
     &                                    PIM%NMOD, IER )
                 ELSE 
                   CALL PIMA_GET_KEY_I4 ( PIM, 1, 'INTERFEROMETER_MODEL', 'NPOLY', &
     &                                    PIM%NMOD, IER )
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7264, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                 'the maximal degree of the interferometryc model '// &
     &                 'polynomial' )
                   RETURN
              END IF
              PIM%NMOD = PIM%NMOD - 1
!
              DO 4580 J58=1,KMOD
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J57)%FITS_DESC, IND_MOD_TAB(J57), &
     &                              J58, PIM%FILE(J57)%KEY(IND_MOD_ANT_ID(J57), &
     &                              IND_MOD_TAB(J57)), 1, MOD_IND_STA, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7265, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword ANTENNA_NO from the '// &
     &                    'INTERFEROMETER_MODEL table in FITS-IDI file '// &
     &                     PIM%FILE(J57)%NAME  )
                     RETURN
                 END IF
!
                 IF ( MOD_IND_STA .LE. 0  .OR.  MOD_IND_STA > PIM%NSTA ) THEN
                      IF ( STR_ZERO_MOD_ID_BYPASS == "YES" ) THEN
                           IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                                WRITE ( 6, * ) 'PIMA_INDX Obs ', J58, &
     &                                         ' has frg_id ', FRG_ID
                           END IF
                           GOTO 4580
                        ELSE
                           CALL CLRCH ( STR )
                           CALL INCH  ( J62, STR )
                           CALL CLRCH ( STR1 )
                           CALL INCH  ( MOD_IND_STA, STR1 )
                           CALL ERR_LOG ( 7266, IUER, 'PIMA_INDX', 'Failure in '// &
     &                         'parsing the keyword ANTENNA_NO from the '// &
     &                         'INTERFEROMETER_MODEL table in FITS-IDI file '// &
     &                          PIM%FILE(J57)%NAME(1:I_LEN(PIM%FILE(J57)%NAME))// &
     &                          ' record: '//STR(1:I_LEN(STR))// &
     &                          ' -- its value '//STR1(1:I_LEN(STR1))// &
     &                          ' is out of range' )
                           RETURN
                      END IF
                END IF
!
! -------------- Replace the index of the source in the station table for
! -------------- the J57-th file with the station index in the global table
!
                 MOD_IND_STA = PIM%REF_STA(MOD_IND_STA,J57)
                 PIM%STA(MOD_IND_STA)%L_MOD = PIM%STA(MOD_IND_STA)%L_MOD + 1
 4580         CONTINUE
!
              CALL ERR_PASS    ( IUER, IER )
              CALL FFITS_CLOSE ( PIM%FILE(J57)%FITS_DESC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7267, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to close FITS UV-file '// &
     &                  PIM%FILE(J57)%NAME )
                   RETURN
              END IF
 4570      CONTINUE
!
           DO 4590 J59=1,PIM%NSTA
              ALLOCATE ( PIM%STA(J59)%MOD(PIM%STA(J59)%L_MOD), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( PIM%STA(J59)%L_MOD*SIZEOF(PIM%STA(J59)%MOD), &
     &                          STR )
                   CALL ERR_LOG ( 7268, IUER, 'PIMA_INDX', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                  'memory for PIM%STA(J59)%MOD object' )
                   RETURN
              END IF
!
! ----------- Set again the counter to zero. It will be restored back
! ----------- in the next cycle
!
              PIM%STA(J59)%L_MOD = 0
 4590      CONTINUE
!
           DO 4600 J60=1,PIM%L_FIL
              IF ( LEN_MOD_GDELAY(J60) .NE. PIM%NMOD+1 ) THEN
                   LEN_MOD_GDELAY(J60) = PIM%NMOD+1
              END IF
!
              CALL ERR_PASS   ( IUER, IER )
              CALL FFITS_OPEN ( PIM%FILE(J60)%NAME, &
     &                          PIM%FILE(J60)%FITS_DESC, 'OLD', IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7269, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to open FITS UV-file '// &
     &                  PIM%FILE(J60)%NAME )
                   RETURN
              END IF
!
! ----------- We need to get MJD date of the J60-th file
!
              CALL FFITS_GETR8 ( PIM%FILE(J60)%FITS_DESC, &
     &                           PIM%FILE(J60)%IND_UV_TAB(1), 1, &
     &                           PIM%FILE(J60)%KEY(IND_UV_DAT(1,J60), &
                                                   PIM%FILE(J60)%IND_UV_TAB(1)), &
     &                           1, JD_DAT, IER )
              MJD_FILE_BEG = NINT ( JD_DAT - 2400000.5D0 )
!
! ----------- Learn the number of interferometry records in the J60-th file
!
              CALL ERR_PASS ( IUER, IER )
              IF ( PIM%GENERATOR == 'OLD_ASC' ) THEN
                   CALL PIMA_GET_KEY_I4 ( PIM, J60, 'INTERFEROMETER_ MODEL', &
     &                                    'NAXIS2', KMOD, IER )
                 ELSE 
                  CALL PIMA_GET_KEY_I4 ( PIM, J60, 'INTERFEROMETER_MODEL', &
     &                                    'NAXIS2', KMOD, IER )
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7270, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                'the length of the INTERFEROMETER_MODEL table in '// &
     &                'FITS-IDI file '//PIM%FILE(J60)%NAME  )
                  RETURN
              END IF
!
              DO 4610 J61=1,KMOD
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J60)%FITS_DESC, IND_MOD_TAB(J60), &
     &                              J61, PIM%FILE(J60)%KEY(IND_MOD_TIME(J60), &
     &                              IND_MOD_TAB(J60)), 1, MOD_TIM_DAYS, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7271, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword TIME from the INTERFEROMETER_MODEL '// &
     &                    'table in FITS-IDI file '//PIM%FILE(J60)%NAME  )
                     RETURN
                 END IF
                 IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                      CONTINUE
                    ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                      MOD_TIM_DAYS = MOD_TIM_DAYS - PIM%UTC_MTAI/86400.0D0
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(J60)%FITS_DESC, IND_MOD_TAB(J60), &
     &                              J61, PIM%FILE(J60)%KEY(IND_MOD_TIME_INTV(J60), &
     &                              IND_MOD_TAB(J60)), 1, MOD_TIM_INTVL_R4, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7272, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword TIME_INTERVAL from the '// &
     &                    'INTERFEROMETER_MODEL table in FITS-IDI file '// &
     &                     PIM%FILE(J60)%NAME  )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J60)%FITS_DESC, IND_MOD_TAB(J60), &
     &                              J61, PIM%FILE(J60)%KEY(IND_MOD_ANT_ID(J60), &
     &                              IND_MOD_TAB(J60)), 1, MOD_IND_STA, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7273, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword ANTENNA_NO from the '// &
     &                    'INTERFEROMETER_MODEL table in FITS-IDI file '// &
     &                     PIM%FILE(J60)%NAME  )
                      RETURN
                 END IF
                 IF ( MOD_IND_STA .LE. 0  .OR.  MOD_IND_STA > PIM%NSTA ) THEN
                      IF ( STR_ZERO_MOD_ID_BYPASS == "YES" ) THEN
                           GOTO 4610
                      END IF
                 END IF
!
! -------------- Replace the index of the station in the station table for
! -------------- the J60-th file with the station index in the global table
!
                 MOD_IND_STA_ORIG = MOD_IND_STA 
                 MOD_IND_STA = PIM%REF_STA(MOD_IND_STA,J60)
                 IF ( MOD_IND_STA < 1 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( MOD_IND_STA_ORIG, STR )
                      WRITE ( 6, * ) ' IND_STA(1:8) ', PIM%REF_STA(1:8,J60) 
                      CALL ERR_LOG ( 7274, IUER, 'PIMA_INDX', 'Cannot find '// &
     &                    ' antenna ID '//STR(1:I_LEN(STR))//' in the list '// &
     &                    ' of known IDs when processing INTERFEROMETER_MODEL '// &
     &                    'table in FITS-IDI file '//PIM%FILE(J60)%NAME  )
                      RETURN
                 END IF
                 PIM%STA(MOD_IND_STA)%L_MOD = PIM%STA(MOD_IND_STA)%L_MOD + 1
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J60)%FITS_DESC, IND_MOD_TAB(J60), &
     &                              J61, PIM%FILE(J60)%KEY(IND_MOD_SOU_ID(J60), &
     &                              IND_MOD_TAB(J60)), 1, MOD_IND_SOU, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7275, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword SOURCE_ID from the '// &
     &                    'INTERFEROMETER_MODEL table in FITS-IDI file '// &
     &                     PIM%FILE(J60)%NAME  )
                      RETURN
                 END IF
                 IF ( MOD_IND_SOU < 1 ) THEN
!
! ------------------- The model is not related to a source in the schedule
!
                      PIM%STA(MOD_IND_STA)%L_MOD = PIM%STA(MOD_IND_STA)%L_MOD - 1
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 7 ) THEN
                           WRITE ( 6, * ) 'PIMA_INDX-4754 Fil: ', INT2(J60), ' Rec: ', INT2(J61), &
     &                                    ' SOURCE_ID= ', MOD_IND_SOU
                      END IF
                      GOTO 4610
                 END IF
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 7          .AND. &
     &                MOD_IND_SOU .LE. PIM%FILE(J60)%N_SOU .AND. &
     &                MOD_IND_STA .GE. 1                   .AND. &
     &                MOD_IND_STA .LE. PIM%FILE(J60)%N_STA       ) THEN
!
                      WRITE ( 6, * ) 'PIMA_INDX-MOD0 Fil: ', INT2(J60), ' Rec: ', INT2(J61), &
     &                               ' SOURCE_ID= ', MOD_IND_SOU, ' Sou_nam: '// &
     &                                PIM%FILE(J60)%SOU_NAME_ORIG(MOD_IND_SOU), &
     &                                ' MOD_IND_STA= ', MOD_IND_STA, &
     &                                ' Sta: ', PIM%STA(MOD_IND_STA)%NAME
                    ELSE IF ( PIM%CONF%DEBUG_LEVEL .GE. 7  .AND. &
     &                MOD_IND_SOU .LE. PIM%FILE(J60)%N_SOU       ) THEN
                      WRITE ( 6, * ) 'PIMA_INDX-MOD  Fil: ', INT2(J60), ' Rec: ', INT2(J61), &
     &                               ' SOURCE_ID= ', MOD_IND_SOU, ' Sou_nam: '// &
     &                                PIM%FILE(J60)%SOU_NAME_ORIG(MOD_IND_SOU), &
     &                                ' MOD_IND_STA= ', MOD_IND_STA
                 END IF
!
! -------------- Replace the index of the source in the source table for
! -------------- the J60-th file with the source index in the global table
!
                 PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%SOU_IND = &
     &                   PIM%REF_SOU(MOD_IND_SOU,J60)
                 IF ( MOD_TIM_DAYS*86400.0D0 - IDNINT(MOD_TIM_DAYS*86400.0D0) < PIM%CONF%AP_TOLERANCE ) THEN
                      MOD_TIM_DAYS = IDNINT(MOD_TIM_DAYS*86400.0D0)/86400.0D0
                 END IF
                 TIM_TAG = (MJD_FILE_BEG - PIM%MJD_0)*86400.0 + &
     &                     (MOD_TIM_DAYS*86400.0D0 - PIM%TAI_0)
!
                 PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%TIM_BEG = &
     &                  (MJD_FILE_BEG - PIM%MJD_0)*86400.0 + &
     &                  (MOD_TIM_DAYS*86400.0D0 - PIM%TAI_0)
!
! -------------- Check, whether MOD_TIM_INTVL_R4 is close to an integer
! -------------- number of 0.01*sec. If yes, then assign MOD_TIM_INTVL_R8
! -------------- to the integer number of 0.01 seconds
!
                 IF ( NINT(MOD_TIM_INTVL_R4*86400.0*1.D2) - &
     &                1.D2*MOD_TIM_INTVL_R4*86400.0 &
     &                < MOD_TIM_INTVL_R4*86400.0*1.D-2 ) THEN
                      MOD_TIM_INTVL_R8 = 1.D-2*NINT(MOD_TIM_INTVL_R4*86400.0*1.D2)
                    ELSE
                      MOD_TIM_INTVL_R8 = MOD_TIM_INTVL_R4*86400.0D0
                 END IF
                 PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%TIM_END = &
     &               PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%TIM_BEG + &
     &               MOD_TIM_INTVL_R8
                 CALL NOUT_R8 ( (1+PIM__MDPL)*PIM__MFRQ, PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%GDEL_POL )
                 CALL NOUT_R8 ( (1+PIM__MDPL)*PIM__MFRQ, PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%PDEL_POL )
                 CALL NOUT_R8 ( (1+PIM__MDPL)*PIM__MFRQ, PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%PRAT_POL )
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J60)%FITS_DESC, IND_MOD_TAB(J60), &
     &                              J61, PIM%FILE(J60)%KEY(IND_MOD_GDELAY(J60), &
     &                              IND_MOD_TAB(J60)), LEN_MOD_GDELAY(J60), &
     &                              PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%GDEL_POL, &
     &                              IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7277, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword value of GDELAY_1 in the '// &
     &                    'INTERFEROMETER_MODEL table in FITS-IDI file '// &
     &                     PIM%FILE(J60)%NAME  )
                     RETURN
                 END IF
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                      STR  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                        PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%TIM_BEG, -2 )
                      STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                        PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%TIM_END, -2 )
                      WRITE ( UNIT=LUN_MOD, FMT=120, IOSTAT=IER ) &
     &                        PIM%C_STA(MOD_IND_STA), &
     &                        PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%TIM_BEG, &
     &                        PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%TIM_END, &
     &                        PIM%C_SOU(PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%SOU_IND ), &
     &                        PIM%FILE(J60)%SOU_NAME_ORIG(MOD_IND_SOU), &
     &                        J60, PIM%STA(MOD_IND_STA)%L_MOD, J61, KMOD, &
     &                        STR(1:21), STR1(1:21), &
     &                        PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%GDEL_POL(0:LEN_MOD_GDELAY(J60)-1,1)
 120                  FORMAT ( 'MOD: Sta ',A, ' Tim_beg: ', F20.10, &
     &                         ' Tim_end: ', F20.10, ' | ', A, 2X, A, &
     &                         ' File: ', I3, ' L_MOD: ', I8, ' Ind: ', I8, &
     &                         ' Kmod: ', I8, 2X, A, 2X, A, ' | Gdelay: ', 6(1PD20.12,1X) )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( IER, STR )
                           CALL ERR_LOG ( 7276, IUER, 'PIMA_INDX', 'Failure '// &
     &                         'in writing into file '// &
     &                          MOD_FILE(1:I_LEN(MOD_FILE))//' -- error '//STR )
                           RETURN
                      END IF
                 END IF
!
! -------------- Array PDELAY has dimension NPOLY*NFRQ. So we first read
! -------------- it in the one-dimensional temporary array R8_ARR...
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J60)%FITS_DESC, IND_MOD_TAB(J60), &
     &                              J61, PIM%FILE(J60)%KEY(IND_MOD_PDELAY(J60), &
     &                              IND_MOD_TAB(J60)), LEN_MOD_PDELAY(J60), &
     &                              R8_ARR, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7278, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword value of PDELAY_1 in the '// &
     &                    'INTERFEROMETER_MODEL table in FITS-IDI file '// &
     &                     PIM%FILE(J60)%NAME  )
                     RETURN
                 END IF
!
! -------------- Get frequency group id
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J60)%FITS_DESC, IND_MOD_TAB(J60), &
     &                              J61, PIM%FILE(J60)%KEY(IND_MOD_FRG(J60), &
     &                              IND_MOD_TAB(J60)), 1, FRG_ID, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7279, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword value of PRATE_1 in the '// &
     &                    'INTERFEROMETER_MODEL table in FITS-IDI file '// &
     &                     PIM%FILE(J60)%NAME  )
                      RETURN
                 END IF
                 IF ( ILEN(STR_SET_FRG) > 0 ) THEN
                      FRG_ID = SET_FRG
                 END IF
                 IF ( FRG_ID > PIM%NFRG .OR. FRG_ID < 1 ) THEN
                      WRITE ( 6, * ) 'IND_FRG = ', IND_FRG, ' FRG_ID= ', FRG_ID, ' PIM%NFRG= ', PIM%NFRG
                      CALL ERR_LOG ( 7280, IUER, 'PIMA_INDX', 'Trap of '// &
     &                    'internal control: the frequency group index is out '// &
     &                    'of range while processing '// &
     &                    'INTERFEROMETER_MODEL table in FITS-IDI file '// &
     &                     TRIM(PIM%FILE(J60)%NAME)//' . To override, you can '// &
     &                    'set environment variable PIMAVAR_SET_FRG to the frequency '// &
     &                    'group index. This will override the value in the FITS-IDI file' )
                      RETURN
                 END IF
                 IND_FRG = PIM%FILE(J60)%REF_FRG(1,FRG_ID)
!
! -------------- ... Then cast PDELAY to appropriate slots of %PDEL_POL
!
                 KP = 0
                 DO 4620 J62=1,PIM%NFRQ
                    IF ( PIM%FRQ(J62,IND_FRG)%SIDE_BAND == 1 ) THEN
                         FRQ_IF =  FRQ_ORIG(J62,FRG_ID,J60)%FREQ
                       ELSE
                         IF ( PIM%CORR_NAME == 'VLBA' .OR. PIM%CORR_NAME == 'DIFX' ) THEN
                              FRQ_IF = -FRQ_ORIG(J62,FRG_ID,J60)%FREQ - FRQ_ORIG(J62,FRG_ID,J60)%BAND_WIDTH
                            ELSE
                              FRQ_IF =  FRQ_ORIG(J62,FRG_ID,J60)%FREQ + FRQ_ORIG(J62,FRG_ID,J60)%BAND_WIDTH
                         END IF
                    END IF
                    IF ( DABS(FRQ_IF) < PIMA__MIN_FRQ ) THEN
                         IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
                              WRITE ( 6, * ) 'PIMA_INDX: IND_FRQ: ', INT2(J62), &
     &                                       ' of ', INT2(PIM%NFRQ), ' Station: ', &
     &                                       PIM%STA(MOD_IND_STA)%IVS_NAME, ' J61= ', &
     &                                       INT2(J4),' found ZERO reference frequency'
                         END IF
                    END IF
                    DO 4630 J63=1,LEN_MOD_PDELAY(J60)/PIM%NFRQ
                       KP = KP + 1
!
! -------------------- Normalization of the phase delay model to rad.
! -------------------- NB: it is refererred to the IF reference frquency of
! -------------------- each frequency group!!
! -------------------- It is not the same as the fringe reference frequnecy!!
!
!
                       IF ( DABS(FRQ_IF) > PIMA__MIN_FRQ ) THEN
                            PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%PDEL_POL(J63-1,J62) = &
     &                              R8_ARR(KP)/FRQ_IF
                          ELSE   
                            PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%PDEL_POL(J63-1,J62) = 0.0D0
                       END IF
 4630               CONTINUE
 4620            CONTINUE
!
! -------------- The same with %PRAT_POL
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J60)%FITS_DESC, IND_MOD_TAB(J60), &
     &                              J61, PIM%FILE(J60)%KEY(IND_MOD_PRATE(J60), &
     &                              IND_MOD_TAB(J60)), LEN_MOD_PRATE(J60), &
     &                              R8_ARR, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7281, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword value of PRATE_1 in the '// &
     &                    'INTERFEROMETER_MODEL table in FITS-IDI file '// &
     &                     PIM%FILE(J60)%NAME  )
                     RETURN
                 END IF
!
                 KP = 0
                 DO 4640 J64=1,PIM%NFRQ
                    IF ( PIM%FRQ(J64,FRG_ID)%SIDE_BAND == 1 ) THEN
                         FRQ_IF =  FRQ_ORIG(J64,FRG_ID,J60)%FREQ
                       ELSE
                         IF ( PIM%CORR_NAME == 'VLBA' .OR. PIM%CORR_NAME == 'DIFX' ) THEN
                              FRQ_IF = -FRQ_ORIG(J64,FRG_ID,J60)%FREQ - FRQ_ORIG(J64,FRG_ID,J60)%BAND_WIDTH
                            ELSE
                              FRQ_IF =  FRQ_ORIG(J64,FRG_ID,J60)%FREQ + FRQ_ORIG(J64,FRG_ID,J60)%BAND_WIDTH
                         END IF
                    END IF
                    DO 4650 J65=1,LEN_MOD_PRATE(J60)/PIM%NFRQ
                       KP = KP + 1
!
! -------------------- Normalization of the phase delay rate model to
! -------------------- dimensionless units.
! -------------------- NB: it is refererred to the global reference frequency!!
! -------------------- It is not the same as the fringe reference frequnecy!!
!
                       IF ( DABS(FRQ_IF) > PIMA__MIN_FRQ ) THEN
                            PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%PRAT_POL(J65-1,J64) = &
     &                              R8_ARR(KP)/FRQ_IF
                          ELSE
                            PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%PRAT_POL(J65-1,J64) = 0.0D0
                       END IF
                       IF ( PIM%CONF%DEBUG_LEVEL .GE. 13 ) THEN
                            WRITE ( UNIT=LUN_MOD, FMT=128, IOSTAT=IER ) J64, J65-1, PIM%STA(MOD_IND_STA)%MOD(PIM%STA(MOD_IND_STA)%L_MOD)%PRAT_POL(J65-1,J64)
 128                        FORMAT ( '  Prat: Nfrq= ', I2, ' Npoly: ', I1, ' prat: ', 1PD20.12 )
                       END IF
 4650               CONTINUE
 4640            CONTINUE
 4610         CONTINUE
!
              CALL ERR_PASS    ( IUER, IER )
              CALL FFITS_CLOSE ( PIM%FILE(J60)%FITS_DESC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7282, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to close FITS UV-file '// &
     &                  PIM%FILE(J60)%NAME )
                   RETURN
              END IF
 4600      CONTINUE
!
! -------- Sort the interferometry model for each station:
! -------- first by source names, second by time
!
           DO 4660 J66=1,PIM%NSTA
              CALL FOR_QSORT ( PIM%STA(J66)%MOD, PIM%STA(J66)%L_MOD, &
     &                         SIZEOF(PIM%STA(J66)%MOD(1)), PIMA_COMPAR_MOD )
 4660      CONTINUE
!
           DO 4670 J67=1,PIM%NOBS
              FL_FOU(1) = .FALSE.
              FL_FOU(2) = .FALSE.
              TIM_OBS_BEG =  1.D30
              TIM_OBS_END = -1.D30
              DO 4680 J68=1,PIM%OBS(J67)%NUVS
                 IF ( PIM%OBS(J67)%UV_IND(1,J68) > 0 ) THEN
                      IF ( FL_FOU(1) ) THEN
                           TIM_OBS_BEG = MIN ( TIM_OBS_BEG, &
     &                                         PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J67)%UV_IND(1,J68))%TIM_IND) )
                         ELSE
                           TIM_OBS_BEG = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J67)%UV_IND(1,J68))%TIM_IND)
                      END IF
                      FL_FOU(1) = .TRUE.
                 END IF
                 IF ( PIM%OBS(J67)%UV_IND(PIM%OBS(J67)%NUM_EPC(J68),J68) > 0 ) THEN
                      IF ( FL_FOU(2) ) THEN
                           TIM_OBS_END = MAX ( TIM_OBS_END, &
     &                                         PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J67)%UV_IND(PIM%OBS(J67)%NUM_EPC(J68),J68))%TIM_IND) )
                         ELSE
                           TIM_OBS_END = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J67)%UV_IND(PIM%OBS(J67)%NUM_EPC(J68),J68))%TIM_IND)
                      END IF
                      FL_FOU(2) = .TRUE.
                 END IF
 4680         CONTINUE
              IF ( .NOT. FL_FOU(1) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J67, STR )
                   CALL ERR_LOG ( 7283, IUER, 'PIMA_INDX', 'Trap of internal '// &
     &                 'control: did not found a valid UV index for the 1st '// &
     &                 'AP of observation #'//STR )
                   RETURN
              END IF
              IF ( .NOT. FL_FOU(2) ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J67, STR )
                   CALL ERR_LOG ( 7284, IUER, 'PIMA_INDX', 'Trap of internal '// &
     &                 'control: did not found a valid UV index for the last '// &
     &                 'AP of observation #'//STR )
                   RETURN
              END IF
              DO 4690 J69=1,2
                 DO 4700 J70=1,PIM%STA(PIM%OBS(J67)%STA_IND(J69))%L_MOD
!
! ----------------- Do not consider sources others than the source observed
! ----------------- in the J67-th observation
!
                    IF ( PIM%STA(PIM%OBS(J67)%STA_IND(J69))%MOD(J70)%SOU_IND .NE. &
     &                   PIM%OBS(J67)%ROOT_SOU_IND ) GOTO 4700
!
                    TIM_MOD_BEG = PIM%STA(PIM%OBS(J67)%STA_IND(J69))%MOD(J70)%TIM_BEG
                    TIM_MOD_END = PIM%STA(PIM%OBS(J67)%STA_IND(J69))%MOD(J70)%TIM_END
!
! ----------------- Check whether the model interval fits into the observation
! ----------------- range.
! ----------------- We consider four cases:
! ----------------- a) Observation start is within the model range
! ----------------- b) Observation end   is within the model range
! ----------------- c) the model range is within the observation range
! ----------------- d) the observation range is within the model range
!
                    IF ( ( TIM_MOD_BEG < TIM_OBS_BEG + PIM%CONF%AP_TOLERANCE .AND. &
     &                     TIM_MOD_END > TIM_OBS_BEG - PIM%CONF%AP_TOLERANCE       &
     &                   ) .OR. &
     &                   ( TIM_MOD_BEG < TIM_OBS_END + PIM%CONF%AP_TOLERANCE .AND. &
     &                     TIM_MOD_END > TIM_OBS_END - PIM%CONF%AP_TOLERANCE       &
     &                   ) .OR. &
     &                   ( TIM_MOD_BEG > TIM_OBS_BEG - PIM%CONF%AP_TOLERANCE .AND. &
     &                     TIM_MOD_END < TIM_OBS_END + PIM%CONF%AP_TOLERANCE       &
     &                   ) .OR. &
     &                   ( TIM_OBS_BEG > TIM_MOD_BEG - PIM%CONF%AP_TOLERANCE .AND. &
     &                     TIM_OBS_END < TIM_MOD_END + PIM%CONF%AP_TOLERANCE       &
     &                   ) ) THEN
!
                         IF ( PIM%OBS(J67)%MOD_IND_BEG(J69) == 0 ) THEN
                              PIM%OBS(J67)%MOD_IND_BEG(J69) = J70
                         END IF
!
                         PIM%OBS(J67)%MOD_IND_END(J69) = J70
                         PIM%STA(PIM%OBS(J67)%STA_IND(J69))%MOD(J70)%SCANNAME = &
     &                           PIM%SCA(PIM%OBS(J67)%SCA_IND)%SCAN_NAME
                    END IF
 4700            CONTINUE
 4690         CONTINUE
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                   IF ( PIM%OBS(J67)%MOD_IND_BEG(1) > 0 .AND. &
     &                  PIM%OBS(J67)%MOD_IND_END(1) > 0 .AND. &
     &                  PIM%OBS(J67)%MOD_IND_BEG(2) > 0 .AND. &
     &                  PIM%OBS(J67)%MOD_IND_END(2) > 0       ) THEN
!
                        WRITE ( UNIT=LUN_MDU, FMT=130 ) J67, &
     &                     PIM%OBS(J67)%MOD_IND_BEG, &
     &                     PIM%OBS(J67)%MOD_IND_END, &
     &                     PIM%C_SOU(PIM%OBS(J67)%SOU_IND), &
     &                     PIM%C_STA(PIM%OBS(J67)%STA_IND(1)), &
     &                     PIM%C_STA(PIM%OBS(J67)%STA_IND(2)), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     & PIM%STA(PIM%OBS(J67)%STA_IND(1))%MOD(PIM%OBS(J67)%MOD_IND_BEG(1))%TIM_BEG, -2 ), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     & PIM%STA(PIM%OBS(J67)%STA_IND(1))%MOD(PIM%OBS(J67)%MOD_IND_END(1))%TIM_END, -2 ), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     & PIM%STA(PIM%OBS(J67)%STA_IND(2))%MOD(PIM%OBS(J67)%MOD_IND_BEG(2))%TIM_BEG, -2 ), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     & PIM%STA(PIM%OBS(J67)%STA_IND(2))%MOD(PIM%OBS(J67)%MOD_IND_END(2))%TIM_END, -2 )
 130                    FORMAT ( 'Obs: ',I6,' MOD_BEG: ', I8, 1X, I8, &
     &                      ' MOD_END: ', I8, 1X, I8, &
     &                      ' C_SOU: ', A, &
     &                      ' C_STA: ', A8, 1X, A8, ' TIM_1: ', A30, 1X, A30, &
     &                      ' TIM_2: ', A21, 1X, A21 )
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( IER, STR )
                             CALL ERR_LOG ( 7285, IUER, 'PIMA_INDX', 'Failure '// &
     &                           'in writing into file '// &
     &                            MDU_FILE(1:I_LEN(MDU_FILE))// &
     &                           ' -- error '//STR )
                             RETURN
                        END IF
                      ELSE
                        STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                         PIM%TIM_R8(PIM%OBS(J67)%TIM_BEG_IND), &
     &                                         -2 )
                        WRITE ( UNIT=LUN_MDU, FMT=135 ) J67, &
     &                          PIM%OBS(J67)%MOD_IND_BEG, &
     &                          PIM%OBS(J67)%MOD_IND_END, &
     &                          PIM%C_SOU(PIM%OBS(J67)%SOU_IND), &
     &                          PIM%C_STA(PIM%OBS(J67)%STA_IND(1)), &
     &                          PIM%C_STA(PIM%OBS(J67)%STA_IND(2)), &
     &                          STR(1:22)
 135                    FORMAT ( 'NO MODEL for obs ', I6, &
     &                           ' MOD_BEG: ', I4, 1X, I4, &
     &                           ' MOD_END: ', I4, 1X, I4, &
     &                           ' C_SOU: ', A, &
     &                           ' C_STA: ', A8, 1X, A8, &
     &                           ' TIM: ', A22 )
                 END IF
              END IF
 4670      CONTINUE
!
           L_NOMOD = 0
           DO 4710 J71=1,PIM%NOBS
              IF ( PIM%OBS(J71)%MOD_IND_BEG(1) .LE. 0 .OR. &
     &             PIM%OBS(J71)%MOD_IND_END(1) .LE. 0      ) THEN
                   L_NOMOD = L_NOMOD + 1
                   WRITE ( 6, 140 ) J71, PIM%C_STA(PIM%OBS(J71)%STA_IND(1))
 140               FORMAT ( 'NOMOD: obs. ',I6,' Station: ',A, ' no a priori model was found' )
              END IF
              IF ( PIM%OBS(J71)%MOD_IND_BEG(2) .LE. 0 .OR. &
     &             PIM%OBS(J71)%MOD_IND_END(2) .LE. 0      ) THEN
                   L_NOMOD = L_NOMOD + 1
                   WRITE ( 6, 140 ) J71, PIM%C_STA(PIM%OBS(J71)%STA_IND(2))
              END IF
 4710      CONTINUE
!
           IF ( L_NOMOD > 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( L_NOMOD, STR )
                CALL ERR_PASS ( IUER, IER )
                CALL ERR_LOG ( 7286, IER, 'PIMA_INDX', 'No model was found '// &
     &              'for '//STR(1:I_LEN(STR))//' observations' )
                IF ( PIM%CONF%CHECK_SEVERITY .GE. 3 ) THEN
                     CALL ERR_PASS ( IER, IUER )
                     RETURN
                END IF
              ELSE IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) 'PIMA_INDX  A priori model was found '// &
     &                             'for all observations'
           END IF
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                CLOSE ( UNIT=LUN_MOD )
                CLOSE ( UNIT=LUN_MDU )
           END IF
         ELSE
          IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
               WRITE ( 6, '(A)' ) 'PIMA_INDX  A priori model was not found '// &
     &                            'in input FITS files'
          END IF
      END IF
!
      IF ( FL_WEA_TAB ) THEN
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) 'PIMA_INDX  Extract the weather model'
                WEA_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.wea'
!
                IP = I_LEN(PIM%CONF%EXPER_DIR)
                IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                     WEA_FILE = PIM%CONF%EXPER_DIR(1:IP)//WEA_FILE
                   ELSE
                     WEA_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//WEA_FILE
                END IF
!
                LUN_WEA = GET_UNIT()
                OPEN ( UNIT=LUN_WEA, FILE=WEA_FILE, STATUS='UNKNOWN', &
     &                 IOSTAT=IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IER, STR )
                     CALL ERR_LOG ( 7287, IUER, 'PIMA_INDX', 'Failure to '// &
     &                   'open information file '// &
     &                    WEA_FILE(1:I_LEN(WEA_FILE))//' -- error '//STR )
                     RETURN
                END IF
           END IF
!
! -------- First pass: collect information about the weather
!
           DO 4720 J72=1,PIM%L_FIL
              IF ( IND_WEA_TAB(J72) == 0 ) GOTO 4720
!
              CALL ERR_PASS   ( IUER, IER )
              CALL FFITS_OPEN ( PIM%FILE(J72)%NAME, &
     &                          PIM%FILE(J72)%FITS_DESC, 'OLD', IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7288, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to open FITS UV-file '// &
     &                  PIM%FILE(J72)%NAME )
                   RETURN
              END IF
!
! ----------- Learn the number of weather records in the J72-th file
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, J72, 'WEATHER ', 'NAXIS2', &
     &                               KWEA(J72), IER )
              IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 7289, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                'the length of the WEATHER table in '// &
     &                'FITS-IDI file '//PIM%FILE(J72)%NAME  )
                  RETURN
              END IF
!
              DO 4730 J73=1,KWEA(J72)
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J72)%FITS_DESC, IND_WEA_TAB(J72), J73, &
     &                              PIM%FILE(J72)%KEY(IND_WEA_ANT_ID(J72), &
     &                              IND_WEA_TAB(J72)), 1, WEA_IND_STA, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7290, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword ANTENNA_NO from the '// &
     &                    'WEATHER table in FITS-IDI file '// &
     &                     PIM%FILE(J72)%NAME  )
                     RETURN
                 END IF
                 WEA_IND_STA = PIM%REF_STA(WEA_IND_STA,J72)
                 PIM%STA(WEA_IND_STA)%WEATHER%NPOI = &
     &                   PIM%STA(WEA_IND_STA)%WEATHER%NPOI + 1
 4730         CONTINUE
!
              CALL ERR_PASS    ( IUER, IER )
              CALL FFITS_CLOSE ( PIM%FILE(J72)%FITS_DESC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7291, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to close FITS UV-file '// &
     &                  PIM%FILE(J72)%NAME )
                   RETURN
              END IF
 4720      CONTINUE
!
           N_WEA = 0
           DO 4740 J74=1,PIM%NSTA
              IF ( PIM%STA(J74)%WEATHER%NPOI > 0 ) THEN
                   N_WEA = N_WEA + 1
                   PIM%STA(J74)%WEATHER%AVAIL = .TRUE.
!
                   ALLOCATE ( PIM%STA(J74)%WEATHER%TIME_BEG(PIM%STA(J74)%WEATHER%NPOI), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J74)%WEATHER%NPOI, STR )
                        CALL ERR_LOG ( 7292, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J74)%WEATHER%PRES '// &
     &                       'object' )
                        RETURN
                   END IF
!
                   ALLOCATE ( PIM%STA(J74)%WEATHER%TIME_END(PIM%STA(J74)%WEATHER%NPOI), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J74)%WEATHER%NPOI, STR )
                        CALL ERR_LOG ( 7293, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J74)%WEATHER%PRES '// &
     &                       'object' )
                        RETURN
                   END IF
!
                   ALLOCATE ( PIM%STA(J74)%WEATHER%PRES(PIM%STA(J74)%WEATHER%NPOI), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J74)%WEATHER%NPOI, STR )
                        CALL ERR_LOG ( 7294, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J74)%WEATHER%PRES '// &
     &                       'object' )
                        RETURN
                   END IF
!
                   ALLOCATE ( PIM%STA(J74)%WEATHER%TEMP(PIM%STA(J74)%WEATHER%NPOI), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J74)%WEATHER%NPOI, STR )
                        CALL ERR_LOG ( 7295, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J74)%WEATHER%TEMP '// &
     &                       'object' )
                        RETURN
                   END IF
!
                   ALLOCATE ( PIM%STA(J74)%WEATHER%HUMID(PIM%STA(J74)%WEATHER%NPOI), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J74)%WEATHER%NPOI, STR )
                        CALL ERR_LOG ( 7296, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J74)%WEATHER%HUMID '// &
     &                       'object' )
                        RETURN
                   END IF

! ---------------- Set again the counter to zero. It will be restored back
! ---------------- in the next cycle
!
                   PIM%STA(J74)%WEATHER%NPOI = 0
                 ELSE
                   PIM%STA(J74)%WEATHER%AVAIL = .FALSE.
              END IF
 4740      CONTINUE
!
           DO 4750 J75=1,PIM%L_FIL
              IF ( IND_WEA_TAB(J75) == 0 ) GOTO 4750
!
              CALL ERR_PASS   ( IUER, IER )
              CALL FFITS_OPEN ( PIM%FILE(J75)%NAME, &
     &                          PIM%FILE(J75)%FITS_DESC, 'OLD', IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7297, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to open FITS UV-file '// &
     &                  PIM%FILE(J75)%NAME )
                   RETURN
              END IF
!
! ----------- We need to get MJD date of the J75-th file
!
              CALL FFITS_GETR8 ( PIM%FILE(J75)%FITS_DESC, &
     &                           PIM%FILE(J75)%IND_UV_TAB(1), 1, &
     &                           PIM%FILE(J75)%KEY(IND_UV_DAT(1,J75), &
                                                   PIM%FILE(J75)%IND_UV_TAB(1)), &
     &                           1, JD_DAT, IER )
              MJD_FILE_BEG = NINT ( JD_DAT - 2400000.5D0 )
!
              DO 4760 J76=1,KWEA(J75)
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J75)%FITS_DESC, IND_WEA_TAB(J75), J76, &
     &                              PIM%FILE(J75)%KEY(IND_WEA_ANT_ID(J75), &
     &                              IND_WEA_TAB(J75)), 1, WEA_IND_STA, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7298, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword ANTENNA_NO from the '// &
     &                    'WEATHER table in FITS-IDI file '// &
     &                     PIM%FILE(J75)%NAME  )
                     RETURN
                 END IF
!
                 WEA_IND_STA = PIM%REF_STA(WEA_IND_STA,J75)
                 PIM%STA(WEA_IND_STA)%WEATHER%NPOI = &
     &                   PIM%STA(WEA_IND_STA)%WEATHER%NPOI + 1
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(J75)%FITS_DESC, IND_WEA_TAB(J75), J76, &
     &                              PIM%FILE(J75)%KEY(IND_WEA_TIME(J75), &
     &                              IND_WEA_TAB(J75)), 1, WEA_TIM_DAYS, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7299, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword TIME from the WEATHER '// &
     &                    'table in FITS-IDI file '//PIM%FILE(J75)%NAME  )
                     RETURN
                 END IF
                 IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                      CONTINUE
                    ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                      WEA_TIM_DAYS = WEA_TIM_DAYS - PIM%UTC_MTAI/86400.0D0
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(J75)%FITS_DESC, IND_WEA_TAB(J75), J76, &
     &                              PIM%FILE(J75)%KEY(IND_WEA_TIME_INTV(J75), &
     &                              IND_WEA_TAB(J75)), 1, WEA_TIM_INTVL_R4, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7300, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword TIME_INTERVAL from the '// &
     &                    'WEATHER table in FITS-IDI file '// &
     &                     PIM%FILE(J33)%NAME  )
                     RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(J75)%FITS_DESC, IND_WEA_TAB(J75), J76, &
     &                              PIM%FILE(J75)%KEY(IND_WEA_PRES(J75), &
     &                              IND_WEA_TAB(J75)), 1, WEA_PRES, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7301, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword PRESSURE from the '// &
     &                    'WEATHER table in FITS-IDI file '// &
     &                     PIM%FILE(J33)%NAME  )
                     RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(J75)%FITS_DESC, IND_WEA_TAB(J75), J76, &
     &                              PIM%FILE(J75)%KEY(IND_WEA_TEMP(J75), &
     &                              IND_WEA_TAB(J75)), 1, WEA_TEMP, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7302, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword TEMPERATURE from the '// &
     &                    'WEATHER table in FITS-IDI file '// &
     &                     PIM%FILE(J33)%NAME  )
                     RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR4 ( PIM%FILE(J75)%FITS_DESC, IND_WEA_TAB(J75), J76, &
     &                              PIM%FILE(J75)%KEY(IND_WEA_DEW(J75), &
     &                              IND_WEA_TAB(J75)), 1, WEA_DEW, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7303, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword DEWPOINT from the '// &
     &                    'WEATHER table in FITS-IDI file '// &
     &                     PIM%FILE(J33)%NAME  )
                     RETURN
                 END IF
!
                 WEA_TIM_BEG = (MJD_FILE_BEG - PIM%MJD_0)*86400.0 + &
     &                         (WEA_TIM_DAYS*86400.0D0 - PIM%TAI_0)
                 IF ( (WEA_TIM_BEG *100.0 - IDNINT(WEA_TIM_BEG *100.0) ) < &
     &                 PIM%CONF%AP_TOLERANCE*100.0D0 ) THEN
                       WEA_TIM_BEG  = IDNINT(WEA_TIM_BEG *100.0)/100.0D0
                 END IF
!
! -------------- Check, whether WEA_TIM_INTVL_R4 is close to an integer
! -------------- number of 0.01*sec. If yes, then assign WEA_TIM_INTVL_R8
! -------------- to the integer number of 0.01 seconds
!
                 IF ( NINT(WEA_TIM_INTVL_R4*86400.0*1.D2) - &
     &                1.D2*WEA_TIM_INTVL_R4*86400.0 &
     &                < WEA_TIM_INTVL_R4*86400.0*1.D-2 ) THEN
                      WEA_TIM_INTVL_R8 = 1.D-2*NINT(WEA_TIM_INTVL_R4*86400.0*1.D2)
                    ELSE
                      WEA_TIM_INTVL_R8 = WEA_TIM_INTVL_R4*86400.0D0
                 END IF
!
                 PIM%STA(WEA_IND_STA)%WEATHER%TIME_BEG(PIM%STA(WEA_IND_STA)%WEATHER%NPOI) = &
     &                   WEA_TIM_BEG
                 PIM%STA(WEA_IND_STA)%WEATHER%TIME_END(PIM%STA(WEA_IND_STA)%WEATHER%NPOI) = &
                         PIM%STA(WEA_IND_STA)%WEATHER%TIME_BEG(PIM%STA(WEA_IND_STA)%WEATHER%NPOI) + WEA_TIM_INTVL_R8
                 PIM%STA(WEA_IND_STA)%WEATHER%PRES(PIM%STA(WEA_IND_STA)%WEATHER%NPOI) = &
     &                   WEA_PRES*100.D0
                 PIM%STA(WEA_IND_STA)%WEATHER%TEMP(PIM%STA(WEA_IND_STA)%WEATHER%NPOI) = &
     &                   WEA_TEMP + 273.15D0
                 PIM%STA(WEA_IND_STA)%WEATHER%HUMID(PIM%STA(WEA_IND_STA)%WEATHER%NPOI) = &
     &                   WEA_DEW
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                      WRITE ( UNIT=LUN_WEA, FMT=150, IOSTAT=IER ) J75, J76, &
     &                        PIM%C_STA(WEA_IND_STA), &
     &                        MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                         PIM%STA(WEA_IND_STA)%WEATHER%TIME_BEG(PIM%STA(WEA_IND_STA)%WEATHER%NPOI), &
     &                                         -2 ), &
     &                        MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                         PIM%STA(WEA_IND_STA)%WEATHER%TIME_END(PIM%STA(WEA_IND_STA)%WEATHER%NPOI), &
     &                                         -2 ), &
     &                        PIM%STA(WEA_IND_STA)%WEATHER%PRES(PIM%STA(WEA_IND_STA)%WEATHER%NPOI), &
     &                        PIM%STA(WEA_IND_STA)%WEATHER%TEMP(PIM%STA(WEA_IND_STA)%WEATHER%NPOI), &
     &                        PIM%STA(WEA_IND_STA)%WEATHER%HUMID(PIM%STA(WEA_IND_STA)%WEATHER%NPOI)
 150                  FORMAT ( 'File: ',I3,' Rec: ', I5, ' Sta: ', A, &
     &                         '  Time: [ ',A21, ' , ', A21, ' ] ', &
     &                         ' Pres: ', F8.1,' Temp: ', F6.2, &
     &                         ' Humid: ', F10.5  )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( IER, STR )
                           CALL ERR_LOG ( 7304, IUER, 'PIMA_INDX', 'Failure '// &
     &                         'in writing into file '// &
     &                          WEA_FILE(1:I_LEN(WEA_FILE))//' -- error '//STR )
                           RETURN
                      END IF
                 END IF
 4760         CONTINUE
!
              CALL ERR_PASS    ( IUER, IER )
              CALL FFITS_CLOSE ( PIM%FILE(J75)%FITS_DESC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7305, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to close FITS UV-file '// &
     &                  PIM%FILE(J75)%NAME )
                   RETURN
              END IF
 4750      CONTINUE
!
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                WRITE ( 6, '(A,I2,A)' ) 'PIMA_INDX  Weather data were '// &
     &                                  'extracted for ', N_WEA, ' stations'
           END IF
        ELSE
          IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
               WRITE ( 6, '(A)' ) 'PIMA_INDX  weather data were not found '// &
     &                            'in input FITS files'
          END IF
      END IF
      IF ( FL_MDC_TAB ) THEN
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) 'PIMA_INDX  Extract the model components'
!
                LUN_MDC = GET_UNIT()
                OPEN ( UNIT=LUN_MDC, FILE=MDC_FILE, STATUS='UNKNOWN', &
     &                 IOSTAT=IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IER, STR )
                     CALL ERR_LOG ( 7306, IUER, 'PIMA_INDX', 'Failure to '// &
     &                   'open information file '// &
     &                    MDC_FILE(1:I_LEN(MDC_FILE))//' -- error '//STR )
                     RETURN
                END IF
           END IF
!
! -------- First pass: collect information about the model components
!
           DO 4770 J77=1,PIM%L_FIL
              IF ( IND_MDC_TAB(J77) == 0 ) GOTO 4770
!
              CALL ERR_PASS   ( IUER, IER )
              CALL FFITS_OPEN ( PIM%FILE(J77)%NAME, &
     &                          PIM%FILE(J77)%FITS_DESC, 'OLD', IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7307, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to open FITS UV-file '// &
     &                  PIM%FILE(J77)%NAME )
                   RETURN
              END IF
!
! ----------- Learn the number of model component records in the J77-th file
!
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_GET_KEY_I4 ( PIM, J77, 'MODEL_COMPS', 'NAXIS2', &
     &                               KMDC(J77), IER )
              IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 7308, IUER, 'PIMA_INDX', 'Failure to get '// &
     &                'the length of the MODEL_COMPS table in '// &
     &                'FITS-IDI file '//PIM%FILE(J77)%NAME  )
                  RETURN
              END IF
!
! ----------- Learn the number of records with model components
!
              DO 4780 J78=1,KMDC(J77)
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J77)%FITS_DESC, IND_MDC_TAB(J77), &
     &                              J78, PIM%FILE(J77)%KEY(IND_MDC_ANT_ID(J77), &
     &                              IND_MDC_TAB(J77)), 1, MDC_IND_STA, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7309, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword ANTENNA_NO from the '// &
     &                    'MODEL_COMPS table in FITS-IDI file '// &
     &                     PIM%FILE(J77)%NAME  )
                      RETURN
                 END IF
                 IF ( MDC_IND_STA < 1  .OR.  MDC_IND_STA > PIM%NSTA ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J78, STR )
                      CALL CLRCH ( STR1 )
                      CALL INCH  ( MDC_IND_STA, STR1 )
                      CALL CLRCH ( STR2 )
                      CALL INCH  ( PIM%NSTA, STR2 )
                           IER = -1
                      CALL ERR_LOG ( 7310, IER, 'PIMA_INDX', 'Failure in '// &
     &                    'parsing the keyword ANTENNA_NO from the '// &
     &                    'MODEL_COMPS table in FITS-IDI file '// &
     &                     PIM%FILE(J77)%NAME(1:I_LEN(PIM%FILE(J77)%NAME))// &
     &                     ' record: '//STR(1:I_LEN(STR))// &
     &                     ' -- its value '//STR1(1:I_LEN(STR1))// &
     &                     ' is out of range [1,'//STR2(1:I_LEN(STR2))//']' )
                      IF ( PIM%CONF%CHECK_SEVERITY < 2 ) THEN
                           WRITE ( 6, '(A)' ) 'Nevertheless, continue'
                           GOTO 4780
                        ELSE
                           CALL ERR_PASS ( IER, IUER )
                           RETURN
                      END IF
                 END IF
                 MDC_IND_STA = PIM%REF_STA(MDC_IND_STA,J77)
                 IF ( MDC_IND_STA > 0 ) THEN
                      PIM%STA(MDC_IND_STA)%L_MDC = PIM%STA(MDC_IND_STA)%L_MDC + 1
                 END IF
 4780         CONTINUE
!
              CALL ERR_PASS    ( IUER, IER )
              CALL FFITS_CLOSE ( PIM%FILE(J77)%FITS_DESC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7311, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to close FITS UV-file '// &
     &                  PIM%FILE(J77)%NAME )
                   RETURN
              END IF
 4770      CONTINUE
!
! -------- Now allocate memory for model components
!
           N_MDC = 0
           DO 4790 J79=1,PIM%NSTA
              PIM%STA(J79)%MDC%CLO_MODEL_STATUS = PIMA__UNDF
              IF ( PIM%STA(J79)%L_MDC > 0 ) THEN
                   N_MDC = N_MDC + 1
                   PIM%STA(J79)%MDC%CLO_MODEL_STATUS = PIMA__MDC_SCA_INCLUDED
!
                   ALLOCATE ( PIM%STA(J79)%MDC%IND_SOU(PIM%STA(J79)%L_MDC), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 4*PIM%STA(J79)%L_MDC, STR )
                        CALL ERR_LOG ( 7312, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J79)%IND_SOU '// &
     &                       'object' )
                        RETURN
                   END IF
!
                   ALLOCATE ( PIM%STA(J79)%MDC%TIME_CEN(PIM%STA(J79)%L_MDC), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J79)%L_MDC, STR )
                        CALL ERR_LOG ( 7313, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J79)%TIME_CEN '// &
     &                       'object' )
                        RETURN
                   END IF
!
                   ALLOCATE ( PIM%STA(J79)%MDC%CLOCK_OFFSET(PIM%STA(J79)%L_MDC), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J79)%L_MDC, STR )
                        CALL ERR_LOG ( 7314, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J79)%CLOCK_OFFSET '// &
     &                       'object' )
                        RETURN
                   END IF
!
                   ALLOCATE ( PIM%STA(J79)%MDC%CLOCK_RATE(PIM%STA(J79)%L_MDC), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J79)%L_MDC, STR )
                        CALL ERR_LOG ( 7315, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J79)%CLOCK_RATE '// &
     &                       'object' )
                        RETURN
                   END IF
!
                   ALLOCATE ( PIM%STA(J79)%MDC%ATMO_DELAY(PIM%STA(J79)%L_MDC), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J79)%L_MDC, STR )
                        CALL ERR_LOG ( 7316, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J79)%ATMO_DELAY '// &
     &                       'object' )
                        RETURN
                   END IF
!
                   ALLOCATE ( PIM%STA(J79)%MDC%ATMO_RATE(PIM%STA(J79)%L_MDC), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J79)%L_MDC, STR )
                        CALL ERR_LOG ( 7317, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J79)%ATMO_RATE '// &
     &                       'object' )
                        RETURN
                   END IF
!
                   ALLOCATE ( PIM%STA(J79)%MDC%GDELAY(PIM%STA(J79)%L_MDC), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J79)%L_MDC, STR )
                        CALL ERR_LOG ( 7318, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J79)%GDELAY'// &
     &                       'object' )
                        RETURN
                   END IF
!
                   ALLOCATE ( PIM%STA(J79)%MDC%GRATE(PIM%STA(J79)%L_MDC), &
     &                        STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( 8*PIM%STA(J79)%L_MDC, STR )
                        CALL ERR_LOG ( 7319, IUER, 'PIMA_INDX', 'Failure to '// &
     &                       'allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &                       'dynamic memory for PIM%STA(J79)%GRATE'// &
     &                       'object' )
                        RETURN
                   END IF
              END IF
!
              PIM%STA(J79)%L_MDC = 0
 4790      CONTINUE
!
! -------- Now read the records with information about the model components
!
           DO 4800 J80=1,PIM%L_FIL
              IF ( IND_MDC_TAB(J80) == 0 ) GOTO 4800
!
              CALL ERR_PASS   ( IUER, IER )
              CALL FFITS_OPEN ( PIM%FILE(J80)%NAME, &
     &                          PIM%FILE(J80)%FITS_DESC, 'OLD', IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7320, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to open FITS UV-file '// &
     &                  PIM%FILE(J80)%NAME )
                   RETURN
              END IF
!
! ----------- We need to get MJD date of the J80-th file
!
              CALL FFITS_GETR8 ( PIM%FILE(J80)%FITS_DESC, &
     &                           PIM%FILE(J80)%IND_UV_TAB(1), 1, &
     &                           PIM%FILE(J80)%KEY(IND_UV_DAT(1,J80), &
                                                   PIM%FILE(J80)%IND_UV_TAB(1)), &
     &                           1, JD_DAT, IER )
              MJD_FILE_BEG = NINT ( JD_DAT - 2400000.5D0 )
!
              DO 4810 J81=1,KMDC(J80)
!
! -------------- Get antenna ID
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J80)%FITS_DESC, IND_MDC_TAB(J80), &
     &                              J81, PIM%FILE(J80)%KEY(IND_MDC_ANT_ID(J80), &
     &                              IND_MDC_TAB(J80)), 1, MDC_IND_STA, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7321, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword ANTENNA_NO from the '// &
     &                    'MODEL_COMPS table in FITS-IDI file '// &
     &                     PIM%FILE(J80)%NAME  )
                     RETURN
                 END IF
                 IF ( MDC_IND_STA < 1  .OR.  MDC_IND_STA > PIM%NSTA ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J81, STR )
                      CALL CLRCH ( STR1 )
                      CALL INCH  ( MDC_IND_STA, STR1 )
                      CALL CLRCH ( STR2 )
                      CALL INCH  ( PIM%NSTA, STR2 )
                           IER = -1
                      CALL ERR_LOG ( 7322, IER, 'PIMA_INDX', 'Failure in '// &
     &                    'parsing the keyword ANTENNA_NO from the '// &
     &                    'MODEL_COMPS table in FITS-IDI file '// &
     &                     PIM%FILE(J80)%NAME(1:I_LEN(PIM%FILE(J77)%NAME))// &
     &                     ' record: '//STR(1:I_LEN(STR))// &
     &                     ' -- its value '//STR1(1:I_LEN(STR1))// &
     &                     ' is out of range [1,'//STR2(1:I_LEN(STR2))//']' )
                      IF ( PIM%CONF%CHECK_SEVERITY < 2 ) THEN
                           WRITE ( 6, '(A)' ) 'Nevertheless, continue'
                           GOTO 4810
                        ELSE
                           CALL ERR_PASS ( IER, IUER )
                           RETURN
                      END IF
                 END IF
!
                 MDC_IND_STA = PIM%REF_STA(MDC_IND_STA,J80)
                 PIM%STA(MDC_IND_STA)%L_MDC = PIM%STA(MDC_IND_STA)%L_MDC + 1
!
! -------------- Get source ID
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETI4 ( PIM%FILE(J80)%FITS_DESC, IND_MDC_TAB(J80), &
     &                              J81, PIM%FILE(J80)%KEY(IND_MDC_SOU_ID(J80), &
     &                              IND_MDC_TAB(J80)), 1, MDC_IND_SOU, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7323, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword SOURCE_ID from the '// &
     &                    'MODEL_COMPS table in FITS-IDI file '// &
     &                     PIM%FILE(J80)%NAME  )
                     RETURN
                 END IF
!
                 PIM%STA(MDC_IND_STA)%MDC%IND_SOU(PIM%STA(MDC_IND_STA)%L_MDC) = &
     &                   PIM%REF_SOU(MDC_IND_SOU,J80)
!
! -------------- Get time tag
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J80)%FITS_DESC, IND_MDC_TAB(J80), &
     &                              J81, PIM%FILE(J80)%KEY(IND_MDC_TIME(J80), &
     &                              IND_MDC_TAB(J80)), 1, MDC_TIM_DAYS, IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7324, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword TIME from the MODEL_COMPS '// &
     &                    'table in FITS-IDI file '//PIM%FILE(J80)%NAME  )
                     RETURN
                 END IF
                 IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                      CONTINUE
                    ELSE IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                      MDC_TIM_DAYS = MDC_TIM_DAYS - PIM%UTC_MTAI/86400.0D0
                 END IF
!
                 MDC_TIM_CEN  = (MJD_FILE_BEG - PIM%MJD_0)*86400.0 + &
     &                          (MDC_TIM_DAYS*86400.0D0 - PIM%TAI_0)
                 PIM%STA(MDC_IND_STA)%MDC%TIME_CEN(PIM%STA(MDC_IND_STA)%L_MDC) = &
     &                   MDC_TIM_CEN
!
! -------------- Get clock offset
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J80)%FITS_DESC, IND_MDC_TAB(J80), &
     &                              J81, PIM%FILE(J80)%KEY(IND_MDC_CLO(J80), &
     &                              IND_MDC_TAB(J80)), 1, &
     &                              PIM%STA(MDC_IND_STA)%MDC%CLOCK_OFFSET(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                              IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7325, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword CLOCK_1 from the '// &
     &                    'MODEL_COMP table in FITS-IDI file '// &
     &                     PIM%FILE(J80)%NAME  )
                     RETURN
                 END IF
!
! -------------- Get clock rate
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J80)%FITS_DESC, IND_MDC_TAB(J80), &
     &                              J81, PIM%FILE(J80)%KEY(IND_MDC_RAT(J80), &
     &                              IND_MDC_TAB(J80)), 1, &
     &                              PIM%STA(MDC_IND_STA)%MDC%CLOCK_RATE(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                              IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7326, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword DCLOCK_1 from the '// &
     &                    'MODEL_COMP table in FITS-IDI file '// &
     &                     PIM%FILE(J80)%NAME  )
                     RETURN
                 END IF
!
! -------------- Get contribution of the atmosphere to the delay
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J80)%FITS_DESC, IND_MDC_TAB(J80), &
     &                              J81, PIM%FILE(J80)%KEY(IND_MDC_ATM(J80), &
     &                              IND_MDC_TAB(J80)), 1, &
     &                              PIM%STA(MDC_IND_STA)%MDC%ATMO_DELAY(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                              IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7327, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword ATMOS from the '// &
     &                    'MODEL_COMP table in FITS-IDI file '// &
     &                     PIM%FILE(J80)%NAME  )
                     RETURN
                 END IF
!
! -------------- Get contribution of the atmosphere to the delay rate
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J80)%FITS_DESC, IND_MDC_TAB(J80), &
     &                              J81, PIM%FILE(J80)%KEY(IND_MDC_ATD(J80), &
     &                              IND_MDC_TAB(J80)), 1, &
     &                              PIM%STA(MDC_IND_STA)%MDC%ATMO_RATE(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                              IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7328, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword DATMOS from the '// &
     &                    'MODEL_COMP table in FITS-IDI file '// &
     &                     PIM%FILE(J80)%NAME  )
                     RETURN
                 END IF
!
! -------------- Get the apriori group delay used by the correlator software
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J80)%FITS_DESC, IND_MDC_TAB(J80), &
     &                              J81, PIM%FILE(J80)%KEY(IND_MDC_GDL(J80), &
     &                              IND_MDC_TAB(J80)), 1, &
     &                              PIM%STA(MDC_IND_STA)%MDC%GDELAY(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                              IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7329, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword GDELAY from the '// &
     &                    'MODEL_COMP table in FITS-IDI file '// &
     &                     PIM%FILE(J80)%NAME  )
                     RETURN
                 END IF
!
! -------------- Get the apriori group delay rate used by the correlator software
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL FFITS_GETR8 ( PIM%FILE(J80)%FITS_DESC, IND_MDC_TAB(J80), &
     &                              J81, PIM%FILE(J80)%KEY(IND_MDC_GRT(J80), &
     &                              IND_MDC_TAB(J80)), 1, &
     &                              PIM%STA(MDC_IND_STA)%MDC%GRATE(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                              IER )
!
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7330, IUER, 'PIMA_INDX', 'Failure to '// &
     &                    'get the keyword GRATE from the '// &
     &                    'MODEL_COMP table in FITS-IDI file '// &
     &                     PIM%FILE(J80)%NAME  )
                     RETURN
                 END IF
!
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                      STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                       PIM%STA(MDC_IND_STA)%MDC%TIME_CEN(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                                       -2 )
                      IF ( MDC_IND_STA > 0 ) THEN
                           STR1 = PIM%C_STA(MDC_IND_STA)
                         ELSE
                           STR1 = 'unknown '
                      END IF
                      IF ( PIM%STA(MDC_IND_STA)%MDC%IND_SOU(PIM%STA(MDC_IND_STA)%L_MDC) > 0 ) THEN
                           STR2 = PIM%C_SOU(PIM%STA(MDC_IND_STA)%MDC%IND_SOU(PIM%STA(MDC_IND_STA)%L_MDC))
                         ELSE
                           STR2 = 'unknown '
                      END IF
                      WRITE ( UNIT=LUN_MDC, FMT=160, IOSTAT=IER ) &
     &                        STR1(1:8), &
     &                        STR2(1:8), &
     &                        STR(1:22), &
     &                        PIM%STA(MDC_IND_STA)%MDC%CLOCK_OFFSET(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                        PIM%STA(MDC_IND_STA)%MDC%CLOCK_RATE(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                        PIM%STA(MDC_IND_STA)%MDC%ATMO_DELAY(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                        PIM%STA(MDC_IND_STA)%MDC%ATMO_RATE(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                        PIM%STA(MDC_IND_STA)%MDC%GDELAY(PIM%STA(MDC_IND_STA)%L_MDC), &
     &                        PIM%STA(MDC_IND_STA)%MDC%GRATE(PIM%STA(MDC_IND_STA)%L_MDC)
 160                  FORMAT ( 'CLOCK_MODEL  Sta: ',A, &
     &                         ' Sou_name: ', A, &
     &                         ' Tim_cen:  ', A, &
     &                         ' Clock_offset: ', 1PD15.8,  &
     &                         ' Clock_rate: ',   1PD15.8,  &
     &                         ' Atmos_delay: ',  1PD15.8,  &
     &                         ' Atmos_rate: ',   1PD15.8,  &
     &                         ' Group_delay: ',  1PD20.12, &
     &                         ' Delay_rate: ',   1PD20.12  )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( IER, STR )
                           CALL ERR_LOG ( 7331, IUER, 'PIMA_INDX', 'Failure '// &
     &                         'in writing into file '// &
     &                          MDC_FILE(1:I_LEN(MDC_FILE))//' -- error '//STR )
                           RETURN
                      END IF
                 END IF
 4810         CONTINUE
!
              CALL ERR_PASS    ( IUER, IER )
              CALL FFITS_CLOSE ( PIM%FILE(J80)%FITS_DESC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7332, IUER, 'PIMA_INDX', 'Error '// &
     &                 'in an attempt to close FITS UV-file '// &
     &                  PIM%FILE(J80)%NAME )
                   RETURN
              END IF
 4800      CONTINUE
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                CLOSE ( UNIT=LUN_MDC )
                WRITE ( 6, '(A,I2,A,A)' ) 'PIMA_INDX  model components were '// &
     &                                    'extracted for ', N_MDC, ' stations ', &
     &                                     GET_CDATE()
                CALL FLUSH ( 6 ) 
           END IF
!
           CALL ERR_PASS    ( IUER, IER )
           CALL PIMA_GET_ADDCLO ( PIM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7333, IUER, 'PIMA_INDX', 'Failure in '// &
     &              'an attempt to to com,pute additional clock delay and '// &
     &              'clock rate' )
                RETURN
           END IF
      END IF
!
      IF ( PIM%FRG_USE == PIMA__MERGE ) THEN
           CALL ERR_PASS    ( IUER, IER )
           CALL PIMA_FRG_MERGE ( PIM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7334, IUER, 'PIMA_INDX', &
     &              'Error in an attempt to merge frequency groups' )
                RETURN
           END IF
        ELSE IF ( PIM%FRG_USE == PIMA__COMBINE ) THEN
           CALL ERR_PASS    ( IUER, IER )
           CALL PIMA_FRG_COMBINE ( PIM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7335, IUER, 'PIMA_INDX', &
     &              'Error in an attempt to combine frequency groups' )
                RETURN
           END IF
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_INDX  done '//GET_CDATE()
           CALL FLUSH ( 6 )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_INDX  !#!#
