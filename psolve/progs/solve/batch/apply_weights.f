      SUBROUTINE APPLY_WEIGHTS ( DBNAME, VER, NUMSTA, WEIGHT_TYPE, &
     &           L_WEI, SUPNAM_WEI, SUPVER_WEI, BASELINE_WEI, ARR_WEI, &
     &           IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  APPLY_WEIGHTS  applies weights for the superfile          *
! *   DBNAME, version VER. First it scans SUPNAM_WEI, SUPVER_WEI arrays  *
! *   and find elements there which correspond to DBNAME/VER. Then it    *
! *   reads NAMFIL cards and computers (or copies) reweights from        *
! *   ARR_WEI to NAMFIL slots.                                           *
! *                                                                      *
! *   It is assumed that NAMFIL is opened before APPLY_WEIGHTS           *
! *   (this file remains opened after completion of APPLY_WEIGHTS).      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       DBNAME ( CHARACTER ) -- Database name (including leading $)    *
! *          VER ( INTEGER*2 ) -- Database version number.               *
! *       NUMSTA ( INTEGER*2 ) -- Number of stations in this session.    *
! *  WEIGHT_TYPE ( CHARACTER ) -- Type of additicve corrections to       *
! *                               weights: one of                        *
! *                               "A" -- session-dependent reweights;    *
! *                               "B" -- baseline-dependent reweights;   *
! *                               "S" -- site-dependent reweights.       *
! *        L_WEI ( INTEGER*4 ) -- Number of reweight records read from   *
! *                               the file.
! *   SUPNAM_WEI ( CHARACTER ) -- Array of superfile names. Dimension:   *
! *                               L_WEI.                                 *
! *   SUPVER_WEI ( INTEGER*2 ) -- Array of superfile versions.           *
! *                               Dimension: L_WEI.                      *
! * BASELINE_WEI ( CHARACTER ) -- Arrays of baseline names. It is empty  *
! *                               for session types, has only first 8    *
! *                               characters filled for site-dependent   *
! *                               weighting, and has 16 characters long  *
! *                               baseline name for baseline weighting.  *
! *                               Dimension: L_WEI.                      *
! *      ARR_WEI ( REAL*8    ) -- Array of re-weights values.            *
! *                               Dimension: 4,L_WEI                     *
! *                               1) group delay solution, delay         *
! *                                  reweight (picoseconds);             *
! *                               2) group delay solution, rate          *
! *                                  reweight (femtoseconds/second);     *
! *                               3) phase delay solution, delay         *
! *                                  reweight (picoseconds);             *
! *                               4) phase delay solution, rate          *
! *                                  reweight (femtoseconds/second).     *
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
! *  ### 05-SEP-2001 APPLY_WEIGHTS  v1.2 (c) L. Petrov  15-MAY-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*4  L_WEI, IUER
      CHARACTER  DBNAME*(*), WEIGHT_TYPE*(*), SUPNAM_WEI(L_WEI)*(*), &
     &           BASELINE_WEI(L_WEI)*(*)
      INTEGER*2  VER, NUMSTA, SUPVER_WEI(L_WEI)
      REAL*8     ARR_WEI(4,L_WEI)
      REAL*8     WEIGHTS_VAL(4,MAX_ARC_BSL)
      CHARACTER  BAS_NAM(MAX_ARC_BSL)*17, SIT_NAM(MAX_ARC_STA)*8, STR*32, &
     &           STR1*32, STRVER*4, JBUF*76
      LOGICAL*4  FL_WEI
      INTEGER*4  NN, IND_SIT1, IND_SIT2, K_WEI, J1, J2, J3, J4
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*2  IPAR_I2, IERR_I2
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Initialization
!
      CALL NOUT_R8 ( INT4(MAX_ARC_BSL)*4, WEIGHTS_VAL )
!
! --- Transform version number for type character (for further error messages)
!
      CALL CLRCH ( STRVER )
      CALL INCH  ( INT4(VER), STRVER )
!
! --- First, scan array of weights and gather information related to this
! --- database, this version into arrays WEIGHTS_VAL, BAS_NAM, SIT_NAM
!
      K_WEI = 0
      DO 410 J1=1,L_WEI
         IF ( SUPNAM_WEI(J1) .EQ. DBNAME(1:10) .AND. &
     &        ( SUPVER_WEI(J1) .EQ. VER .OR. SUPVER_WEI(J1) == 0 ) ) THEN
!
! ----------- We found this database, this version
!
              IF ( WEIGHT_TYPE .EQ. 'A' ) THEN
!
! ---------------- Session-dependent weights: copy the first record
!
                   CALL COPY_R8 ( 4, ARR_WEI(1,J1), WEIGHTS_VAL )
                 ELSE IF ( WEIGHT_TYPE .EQ. 'B' ) THEN
!
! ---------------- Weights of baseline type
!
                   K_WEI = K_WEI + 1
                   IF ( K_WEI .GT. (NUMSTA*(NUMSTA-1))/2 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( K_WEI, STR )
                        CALL CLRCH ( STR1 )
                        CALL INCH  ( (INT4(NUMSTA)*(INT4(NUMSTA)-1)/2), STR1 )
                        CALL ERR_LOG ( 3581, IUER, 'APPLY_WEIGHTS', &
     &                      'Too many baseline weights record for '// &
     &                      'session '//DBNAME//' version '//STRVER//'  '// &
     &                      STR(1:I_LEN(STR))//' -- More than '//STR1 )
                        RETURN
                   END IF
!@                   BAS_NAM(K_WEI) = BASELINE_WEI(J1)(1:8)//'-'// &
!@     &                              BASELINE_WEI(J1)(9:16)
!
! ---------------- The line above is perfectly correct, but a bug in
! ---------------- HP FORTRAN90 2.5 causes memory leakage
!
                   BAS_NAM(K_WEI)(1:8)   = BASELINE_WEI(J1)
                   BAS_NAM(K_WEI)(9:9)   = '-'
                   BAS_NAM(K_WEI)(10:17) = BASELINE_WEI(J1)(9:16)
                   CALL COPY_R8 ( 4, ARR_WEI(1,J1), WEIGHTS_VAL(1,K_WEI) )
                 ELSE IF ( WEIGHT_TYPE .EQ. 'S' ) THEN
!
! ---------------- Weights of site type
!
                   K_WEI = K_WEI + 1
                   IF ( K_WEI .GT. NUMSTA ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( (INT4(NUMSTA)*(INT4(NUMSTA)-1)/2), STR )
                        CALL ERR_LOG ( 3582, IUER, 'APPLY_WEIGHTS', &
     &                      'Too many site weights records for '// &
     &                      'session '//DBNAME//' version '//STRVER// &
     &                      ' -- More than '//STR )
                        RETURN
                   END IF
                   SIT_NAM(K_WEI) = BASELINE_WEI(J1)(1:8)
                   CALL COPY_R8 ( 4, ARR_WEI(1,J1), WEIGHTS_VAL(1,K_WEI) )
              END IF
         END IF
 410  CONTINUE
!
      IF ( K_WEI .EQ. 0 ) THEN
!@           WRITE ( 6, * ) ' L_WEI=',L_WEI
           CALL ERR_LOG ( 3583, IUER, 'APPLY_WEIGHTS', 'No weights '// &
     &         'were found for session '//DBNAME//' version '//STRVER )
           RETURN
      END IF
!
! --- Well. Now scan baseline by baseline of this session and insert
! --- appropriate weights into the slots of NAMFIL
!
      DO 420 J2=1,(NUMSTA*(NUMSTA-1))/2
         IF ( J2 .EQ. 1 ) THEN
              IPAR_I2 = 1
            ELSE
              IPAR_I2 = 0
         END IF
!
! ------ Read the REWT card from the NAMFIL
!
         CALL GETCARD ( INT2(1), 'REWT', IPAR_I2, JBUF, IERR_I2 )
         CALL VTD_NAME_REPAIR ( JBUF(6:13)  )
         CALL VTD_NAME_REPAIR ( JBUF(15:22) )
         IF ( INT4(IERR_I2) .EQ. 1 ) GOTO 420 ! No cards any more? Nothing to do
!
         IF ( WEIGHT_TYPE .EQ. 'A' ) THEN
!
! ----------- Session weights: put weights to all baseline cards
!
              WRITE ( UNIT=JBUF(23:62), FMT='(4F10.2)') &
     &                ( WEIGHTS_VAL(NN,1), NN=1,4 )
            ELSE IF ( WEIGHT_TYPE .EQ. 'B' ) THEN
!
! ----------- Baseline weights
!
              FL_WEI = .FALSE.
              DO 430 J3=1,K_WEI
!
! -------------- Scan all baseline names (including reversed names) and put
! -------------- weights into REWT card as soon as we find them
!
!@                 IF ( BAS_NAM(J3) .EQ. JBUF(6:22)                   .OR. &
!@     &                BAS_NAM(J3) .EQ. JBUF(15:22)//'-'//JBUF(6:13)  ) THEN
!
! -------------- The lines above is perfectly correct, but a bug in
! -------------- HP FORTRAN90 2.5 causes memory leakage.
!
                 IF ( BAS_NAM(J3) .EQ. JBUF(6:22)                   .OR. &
     &                ( BAS_NAM(J3)(1:8)   .EQ. JBUF(15:22) .AND. &
     &                  BAS_NAM(J3)(10:17) .EQ. JBUF(6:13)        ) ) THEN
                      WRITE ( UNIT=JBUF(23:62), FMT='(4F10.2)') &
     &                        ( WEIGHTS_VAL(NN,J3), NN=1,4)
                      FL_WEI = .TRUE.
                 END IF
 430          CONTINUE
              IF ( .NOT. FL_WEI ) THEN
                   DO 530 J3=1,K_WEI
                      WRITE ( 6, '(I3,2X,A)' ) J3, BAS_NAM(J3)(1:17)
 530               CONTINUE 
                   CALL ERR_LOG ( 3584, IUER, 'APPLY_WEIGHTS', 'No '// &
     &                 'weights for baseline '//JBUF(6:22)//' session '// &
     &                 DBNAME//' version '//STRVER//' were found :-(' )
                   RETURN
              END IF
            ELSE IF ( WEIGHT_TYPE .EQ. 'S' ) THEN
!
! ----------- Site weights. Look for both sites. We scan SIT_NAM array in
! ----------- search of indices of the sites whcih form of the current baseline
!
              IND_SIT1 = 0
              IND_SIT2 = 0
              DO 440 J4=1,K_WEI
                 IF ( SIT_NAM(J4) .EQ. JBUF(6:13)  ) IND_SIT1 = J4
                 IF ( SIT_NAM(J4) .EQ. JBUF(15:22) ) IND_SIT2 = J4
 440          CONTINUE
              IF ( IND_SIT1 .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3585, IUER, 'APPLY_WEIGHTS', 'No '// &
     &                 'weights for site '//JBUF(6:13)//' session '// &
     &                 DBNAME//' version '//STRVER//' were found :-(' )
                   RETURN
              END IF
              IF ( IND_SIT2 .EQ. 0 ) THEN
                   CALL ERR_LOG ( 3586, IUER, 'APPLY_WEIGHTS', 'No weights '// &
     &                 'for site '//JBUF(15:22)//' session '//DBNAME// &
     &                 ' version '//STRVER//' were found :-(' )
                   RETURN
              END IF
!
! ----------- Now transform site-dependent weights into the baseline-dependent
! ----------- weights
!
              WRITE ( UNIT=JBUF(23:62), FMT='(4F10.2)') &
     &          DSQRT( WEIGHTS_VAL(1,IND_SIT1)**2 + WEIGHTS_VAL(1,IND_SIT2)**2), &
     &          DSQRT( WEIGHTS_VAL(2,IND_SIT1)**2 + WEIGHTS_VAL(2,IND_SIT2)**2), &
     &          DSQRT( WEIGHTS_VAL(3,IND_SIT1)**2 + WEIGHTS_VAL(3,IND_SIT2)**2), &
     &          DSQRT( WEIGHTS_VAL(4,IND_SIT1)**2 + WEIGHTS_VAL(4,IND_SIT2)**2)
            ELSE
               CALL ERR_LOG ( 3587, IUER, 'APPLY_WEIGHTS', 'Unknwon weights '// &
     &                       'type '//WEIGHT_TYPE )
               RETURN
         END IF
!
! ------ Now re-write weights card.
!
         CALL PUTCARD ( INT2(1), 'REWT', INT2(4), JBUF, IERR_I2 )
         IF ( INT4(IERR_I2) .NE. 0 ) THEN
              WRITE ( 6, * ) ' IERR_I2=',IERR_I2
              CALL ERR_LOG ( 3588, IUER, 'APPLY_WEIGHTS', 'Error in attempt '// &
     &                      'to write weights to NAMFIL' )
              RETURN
         END IF
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  APPLY_WEIGHTS  #!#
