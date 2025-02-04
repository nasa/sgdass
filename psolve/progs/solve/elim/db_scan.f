      SUBROUTINE DB_SCAN ( DBNAME, IDB2, IDBF, N_OBS, DBOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DB_SCAN  scans the content of the superfile or scratch    *
! *   file of the certain database and fills field of the object DBOBJ.  *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *  DBNAME ( CHARACTER ) -- Database name.                              *
! *    IDB2 ( INTEGER*2 ) -- Index of the considered database in the     *
! *                          scratch file.                               *
! *    IDBF ( INTEGER*4 ) -- Index the first observation of the          *
! *                          database in the scratch file.               *
! *   N_OBS ( INTEGER*4 ) -- Total number of observations in the session *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  18-SEP-97     DB_SCAN    v2.3  (c)  L. Petrov 04-JUL-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'oborg.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      CHARACTER  DBNAME*(*)
      INTEGER*2  IDB2
      INTEGER*4  IDBF, N_OBS, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
!
      REAL*8     FRACTC_OLD, EPS_SEC
      INTEGER*2  ISTAR_OLD
      LOGICAL*4  FL_USED, FL_RECO, FL_GOOD
      LOGICAL*4  SUPR_INQ, META_SUPR_INQ
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IP, IP_MIN, IP_SOU, IP_STA, &
     &           ISTA1, ISTA2, IP_STA1, IP_STA2, LEN_DBOBJ, KL, KL_MIN, IER
!
      INTEGER*2   JSITN(4,MAX_ARC_STA),   ITT(MAX_ARC_STA)
      INTEGER*4   JCAPPL(MAX_ARC_STA),    JCAVAL(MAX_ARC_STA)
      INTEGER*2   JCAFFL(7,MAX_ARC_STA),  NFCAL,NAMSTA
      INTEGER*2   JSITI(MAX_ARC_STA),     ITTB(MAX_ARC_BSL)
      INTEGER*2   AX_TYPES(MAX_ARC_STA),  OBCAPL, MCAPL
      REAL*8      LATS(MAX_ARC_STA),      HEIGHTS(MAX_ARC_STA)
      REAL*8      BARO_CALS(MAX_ARC_STA), BARO_HEIGHTS(MAX_ARC_STA)
      REAL*8      ET(2,MAX_ARC_BSL),      AX_OFFS(MAX_ARC_STA), &
     &            SE(MAX_ARC_STA),        SS(MAX_ARC_SRC)
      CHARACTER   FCAL_NAMES(112)*8
!
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      CHARACTER   STR*80, STR1*80
      PARAMETER ( EPS_SEC = 0.1D0  ) ! min time difference to acknowledge the
!                                    ! current scan as a new scan (sec)
      INTEGER*4,  EXTERNAL :: IFIND_PL, I_LEN, NSTBA
      LOGICAL*2,  EXTERNAL :: KBIT
      LOGICAL*4,  EXTERNAL :: CHECK_STABIT
!
! --- Test of data structure length
!
      LEN_DBOBJ = (LOC(DBOBJ%LAST_FIELD) - LOC(DBOBJ%FIRST_FIELD)) + 4
      IF ( LEN_DBOBJ .NE. ML_DBOBJ ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( ML_DBOBJ, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( LEN_DBOBJ, STR1 )
           CALL ERR_LOG ( 6641, IUER, 'DB_SCAN', 'Internal error: '// &
     &         'Declared size of DBOBJ data structure (obser.i) '// &
     &          STR(1:I_LEN(STR))//' doesn''t coincide with the '// &
     &         'actual size: '//STR1(1:I_LEN(STR1)) )
           RETURN
      END IF
!
! --- Some initializations
!
      CALL ACS_OBSFIL ( 'O' )
!
      NUMSCA     = 0
      FRACTC_OLD = -99999.9D0
      ISTAR_OLD  = -1
!
! --- Zeroing DBOBJ
!
      CALL NOUT ( ML_DBOBJ, DBOBJ )
      DBOBJ%NAME   = DBNAME
      DBOBJ%STATUS = DBOBJ__UNF
      DBOBJ%IDATYP = IDATYP
      DBOBJ%L_OBS  = 0
      DBOBJ%U_OBS  = 0
      DBOBJ%R_OBS  = 0
      DBOBJ%CG_OBS = 0
      DBOBJ%F_AMB  = .FALSE.
      DBOBJ%F_ION  = .FALSE.
      DBOBJ%F_AMB_CHANGED = .FALSE.
!
! --- Read station names, status array, eccentricity data, monument
! --- names, and set up a correspondence table between the stations
! --- in NAMFIL (JSIT's) and those in PARFIL (ISIT's).
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_4 ( 'RC' )
      CALL NCORT ( JSITN, JSITI, JCAPPL, NUMSTA, ITT, IDB2, &
     &             IDATYP, ITTB, ET, SE, SS, OBCAPL, MCAPL, JCAVAL, &
     &             LATS, HEIGHTS, AX_TYPES, AX_OFFS, BARO_CALS, &
     &             BARO_HEIGHTS, JCAFFL, FCAL_NAMES, NFCAL, NAMSTA, &
     &             CALCV )
!
      DO 410 J1=IDBF,IDBF+N_OBS-1
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
!
         IF ( DABS(FRACTC - FRACTC_OLD)*86400.D0 .GT. EPS_SEC  .OR. &
     &        ISTAR .NE. ISTAR_OLD ) THEN
!
! ----------- If it is the next scan -- update scan counter
!
              NUMSCA = NUMSCA + 1
              FRACTC_OLD = FRACTC
              ISTAR_OLD  = ISTAR
         END IF
!
         IF ( NUMSCA .GT. MO_SCA ) THEN
             CALL CLRCH ( STR  )
             CALL CLRCH ( STR1 )
             CALL INCH  ( NUMSCA, STR  )
             CALL INCH  ( MO_SCA, STR1 )
             CALL ERR_LOG ( 6642, IUER, 'DB_SCAN', 'Numbers of scans in '// &
     &           'database '//DBNAME//' ('//STR(1:I_LEN(STR))//') exceeded '// &
     &           'the upper limit: '//STR1//' -- MO_SCA in obser.i should '// &
     &           'be increased and SOLVE executables needed to be recompiled' )
             RETURN
         END IF
         IF ( J1 .EQ. IDBF ) THEN
!
! ----------- Storing the date of the first observation
!
              DBOBJ%FJD_F = FJD
              DBOBJ%UTC_F = FRACT*86400.D0
              DBOBJ%TCT_F = FRACTC*86400.D0
         END IF
         IF ( J1 .EQ. IDBF+N_OBS-1 ) THEN
!
! ----------- Storing the date of the last observation
!
              DBOBJ%FJD_L = FJD
              DBOBJ%UTC_L = FRACT*86400.D0
              DBOBJ%TCT_L = FRACTC*86400.D0
              DBOBJ%SES_SPAN = DBOBJ%TCT_L - DBOBJ%TCT_F
         END IF
!
! ------ Setting flags of suppression status
!
         IF ( SUPMET == SUPMET__META ) THEN
              CALL AUTO_SUP_UPD ( ISITE, ISTAR, ELEV, AUTO_SUP )
              FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                  USED__SPS )
              FL_RECO = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                  RECO__SPS )
              FL_GOOD = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, &
     &                                  GOOD__SPS )
            ELSE
              CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                           ICORR, GIONSG, PHIONS, IWVBIT1, ISITE, &
     &                           JSITI, ITT, ISTAR, ELEV, KIONO, &
     &                           SNR, SNR_S, SUPSTAT, UACSUP )
!
! ----------- Determine: is this observation used?
!
              FL_USED = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
              FL_RECO = SUPR_INQ ( SUPSTAT, UACSUP, RECO__SPS )
              FL_GOOD = SUPR_INQ ( SUPSTAT, UACSUP, GOOD__SPS )
         END IF
!
! ------ Now updating list of observed sources among all observations
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADC_LIS  ( MO_SOU, DBOBJ%L_SOU, DBOBJ%LIS_SOU, DBOBJ%KL_SOU, &
     &                   INT4(ISTAR), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6643, IUER, 'DB_SCAN', 'Error in adding a '// &
     &             'source to the list of sources' )
              RETURN
         END IF
         IF ( FL_USED ) THEN
!
! ----------- ... and among used observations
!
              CALL ADC_LIS ( MO_SOU, DBOBJ%U_SOU, DBOBJ%UIS_SOU, DBOBJ%KU_SOU, &
     &                       INT4(ISTAR), IER )
         END IF
!
! ------ Add the first station to the list of stations
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADC_LIS  ( MO_STA, DBOBJ%L_STA, DBOBJ%LIS_STA, DBOBJ%KL_STA, &
     &                   INT4(ISITE(1)), IER )
         IF ( IER .NE. 0 ) THEN
             CALL ERR_LOG ( 6644, IUER, 'DB_SCAN', 'Error in adding a '// &
     &            'station to the list of stations' )
             RETURN
         END IF
         IF ( FL_USED ) THEN
!
! ----------- ... and among used observations
!
              CALL ADC_LIS ( MO_STA, DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%KU_STA, &
     &                       INT4(ISITE(1)), IER )
         END IF
!
! ------ Add the second station to the list of stations
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADC_LIS  ( MO_STA, DBOBJ%L_STA, DBOBJ%LIS_STA, DBOBJ%KL_STA, &
     &                   INT4(ISITE(2)), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6645, IUER, 'DB_SCAN', 'Error in adding a '// &
     &            'station to the list of stations' )
              RETURN
         END IF
         IF ( FL_USED ) THEN
!
! ----------- ... and among used observations
!
              CALL ADC_LIS ( MO_STA, DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%KU_STA, &
     &                       INT4(ISITE(2)), IER )
         END IF
!
! ------ Add the baseline code to the list of baselines
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADC_LIS  ( MO_BAS, DBOBJ%L_BAS, DBOBJ%LIS_BAS, DBOBJ%KL_BAS, &
     &                   NSTBA ( INT4(ISITE(1)), INT4(ISITE(2)) ), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6646, IUER, 'DB_SCAN', 'Error in adding a '// &
     &            'baseline to the list of baselines' )
              RETURN
         END IF
         IF ( FL_USED ) THEN
!
! ----------- ... and among used observations
!
              CALL ADC_LIS ( MO_BAS, DBOBJ%U_BAS, DBOBJ%UIS_BAS, DBOBJ%KU_BAS, &
     &                       NSTBA ( INT4(ISITE(1)), INT4(ISITE(2)) ), IER )
         END IF
!
! ------ Increment various observation counters
!
         DBOBJ%L_OBS = DBOBJ%L_OBS + 1
         IF ( FL_USED ) THEN
              DBOBJ%U_OBS = DBOBJ%U_OBS + 1
            ELSE IF ( FL_RECO ) THEN
              DBOBJ%R_OBS = DBOBJ%R_OBS + 1
         END IF
         IF ( FL_GOOD ) THEN
              DBOBJ%CG_OBS = DBOBJ%CG_OBS + 1
         END IF
 410  CONTINUE
!
! --- Closing scratch file
!
      CALL ACS_OBSFIL ( 'C' )
!
      DBOBJ%L_SCA = NUMSCA
!
! --- Sorting station adn source lists
!
      CALL SORT_I2 ( DBOBJ%L_STA, DBOBJ%LIS_STA, DBOBJ%KL_STA )
      CALL SORT_I2 ( DBOBJ%L_SOU, DBOBJ%LIS_SOU, DBOBJ%KL_SOU )
!
      DO 420 J2=1,DBOBJ%L_SOU
!
! ------ Putting names of the sourcres to DBOBJ data structure
!
         IP_SOU = DBOBJ%LIS_SOU(J2)
         CALL CLRCH ( DBOBJ%C_SOU(IP_SOU) )
         DBOBJ%C_SOU(J2) = ISTRN_CHR( IP_SOU ) ! ISTRN_CHR from prfil.i
 420  CONTINUE
!
! --- ... stations list
!
      DO 430 J3=1,DBOBJ%L_STA
!
! ------ Putting names of the stations to DBOBJ data structure
!
         IP_STA = DBOBJ%LIS_STA(J3)
         CALL CLRCH ( DBOBJ%C_STA(IP_STA) )
         DBOBJ%C_STA(J3) = ISITN_CHR( IP_STA ) ! ISITN_CHR from prfil.i
 430  CONTINUE
!
! --- Let's check: whether there were the observations at the same baseline but
! --- in opposite order (for, example: NYALES-ONSALA and ONSALA-NYALES)
!
      DO 440 J4=1,DBOBJ%L_BAS
         IP = DBOBJ%LIS_BAS(J4)
         IP_MIN = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, -IP ) 
         IF ( IP > 0  .AND. IP_MIN > 0 ) THEN
              KL     = DBOBJ%KL_BAS(IP)- (DBOBJ%KL_BAS(IP)/100000)*100000
              KL_MIN = DBOBJ%KL_BAS(IP_MIN)- (DBOBJ%KL_BAS(IP_MIN)/100000)*100000
              CALL NBAST ( IP, ISTA1, ISTA2 )
              IP_STA1 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA1 )
              IP_STA2 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA2 )
!@              IF ( CHECK_STABIT ( INT2(ISTA1) ) .AND. &
!@     &             CHECK_STABIT ( INT2(ISTA2) ) .AND. &
!@     &             KL     .NE. 0                .AND. &
!@     &             KL_MIN .NE. 0                      ) THEN
              IF ( CHECK_STABIT ( INT2(ISTA1) ) .AND. &
     &             CHECK_STABIT ( INT2(ISTA2) )       ) THEN
                   CALL ERR_LOG ( 6647, IUER, 'DB_SCAN', 'Database '// &
     &                  DBOBJ%NAME//' contains observations made '// &
     &                 'at the baselines '//'"'// &
     &                  DBOBJ%C_STA(IP_STA1)//'/'//DBOBJ%C_STA(IP_STA2)// &
     &                 '" and '// &
     &                 '"'//DBOBJ%C_STA(IP_STA2)//'/'// &
     &                      DBOBJ%C_STA(IP_STA1)//'"' )
                   RETURN
              END IF
         END IF
!
! ------ Baseline list will be sorted in according woth increasing modules
! ------ of baseline codes (since baseline code may be negative). To do it
! ------ array DBOBJ.KU_BAS will be spoiled temorarily: the oldest
! ------ decimal digits will be occupied by cmodule of baseline code
! ------ (but 5 youngest digits remained intact).
!
         DBOBJ%KL_BAS(J4) = 100000*ABS(DBOBJ%LIS_BAS(J4)) + DBOBJ%KL_BAS(J4)
  440 CONTINUE
!
! --- After that we sort (in increasong order) a pair of tied arrays:
! --- DBOBJ.KL_BAS and DBOBJ.LIS_BAS in according with increasing
! --- "spoiled" array DBOBJ.KL_BAS
!
      CALL SORT_I2 ( DBOBJ%L_BAS, DBOBJ%KL_BAS, DBOBJ%LIS_BAS )
!
! --- And now -- removing "spoliage" from the array DBOBJ.KL_BAS
!
      DO 450 J5=1,DBOBJ%L_BAS
         DBOBJ%KL_BAS(J5) = DBOBJ%KL_BAS(J5) - 100000*ABS(DBOBJ%LIS_BAS(J5))
!
         CALL NBAST ( DBOBJ%LIS_BAS(J5), ISTA1, ISTA2 )
         IP_STA1 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA1 )
         IP_STA2 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA2 )
         DBOBJ%C_BAS(J5)=DBOBJ%C_STA(IP_STA1)//'/'//DBOBJ%C_STA(IP_STA2)
  450 CONTINUE
!
! --- Then we sort lists of used stations and sources. It may be easily done
! --- without problems:
!
      CALL SORT_I2 ( DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%KU_STA )
      CALL SORT_I2 ( DBOBJ%U_SOU, DBOBJ%UIS_SOU, DBOBJ%KU_SOU )
!
! --- The same technology for sorting used baseline list
!
      DO 460 J6=1,DBOBJ%U_BAS
         DBOBJ%KU_BAS(J6) = 100000*ABS(DBOBJ%UIS_BAS(J6)) + &
     &                                 DBOBJ%KU_BAS(J6)
  460 CONTINUE
      CALL SORT_I2 ( DBOBJ%U_BAS, DBOBJ%KU_BAS, DBOBJ%UIS_BAS )
      DO 470 J7=1,DBOBJ%U_BAS
         DBOBJ%KU_BAS(J7) = DBOBJ%KU_BAS(J7) - 100000*ABS(DBOBJ%UIS_BAS(J7))
  470 CONTINUE
!
! --- Deal done.
!
      DBOBJ%STATUS = DBOBJ__DON
      CALL USE_COMMON ( 'OWC' )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  DB_SCAN  #!#
