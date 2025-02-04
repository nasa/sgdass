      SUBROUTINE SNGCHK ( DBOBJ, IT, R_SOU, RIS_SOU, R_STA, RIS_STA, R_BAS, &
     &                    RIS_BAS, SNGCHK_CMP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SNGCHK  makes singularity check for the normal matrix     *
! *   using statistics collected during processing the database when     *
! *   the normal matrix was build and estimation model.                  *
! *   It makes the following checks:                                     *
! *     1) Does the number of USED in estimation observations of either  *
! *        source exceed the specified limit (SNGCHK_SOUMIN) if source   *
! *        dependent parameters for this source are estimated.           *
! *     2) Does the number of USED in estimation observations of either  *
! *        station exceed the specified limit (SNGCHK_STAMIN).           *
! *     3) Does the number of USED in estimation observations of the     *
! *        baseline exceed the specified limit (SNGCHK_BASMIN) if        *
! *        baseline-dependent parameters such as baseline-dependent      *
! *        clocks are estimated for this baseline.                       *
! *                                                                      *
! *   Routine  SNGCHK doesn't do anything if singularity check action    *
! *   SNGCHK_ACTION is set as NONE. Otherwise SNGCHK makes checks,       *
! *   generate a warning message, prints it at the screen, in the        *
! *   spool-file and in the error-file. If SNGCHK_ACTION is set as REPR  *
! *   then routine SNGCHK deselects sources/stations/baselines which     *
! *   failed the test and makes reparameterization of the solution.      *
! *                                                                      *
! *   SNGCHK generates completion code which is used by other routines.  *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *      IT ( INTEGER*4 ) -- Verbosity level. If IT=0 no messages will   *
! *                          be written in the spool- and error-files    *
! *                          and at the screen.                          *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *      R_SOU ( INTEGER*4 ) -- Number of sources to be deselected from  *
! *                             estimation.                              *
! *    RIS_SOU ( INTEGER*4 ) -- List of the sources which status         *
! *                             "estimate at least one coordinate" is    *
! *                             forcible tuened off. Arrays contains the *
! *                             indices of the source names in the array *
! *                             ISTRN_CHR from prfil.i  Length of the    *
! *                             list is R_SOU elements.                  *
! *      R_STA ( INTEGER*4 ) -- Number of stations to be deselected      *
! *    RIS_STA ( INTEGER*4 ) -- List of the sources to be deselected.    *
! *                             Arrays contains the indices of the       *
! *                             station names in the array ISITN_CHR     *
! *                             from prfil.i. Length of the list is      *
! *                             R_STA elements.                          *
! *      R_BAS ( INTEGER*4 ) -- Number of baselines to be deselected     *
! *    RIS_BAS ( INTEGER*4 ) -- List of the baselines to be deselected.  *
! *                             Arrays contains the indices of the       *
! *                             station names in the array ?? from       *
! *                             prfil.i Length of the list is R_BAS      *
! *                             elements.                                *
! * SNGCHK_CMP ( INTEGER*4 ) -- Completion code of the routine.          *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! *  Hystory of changes:                                                 *
! *                                                                      *
! *  pet  17-JUL-98  Coding error fixed at the line where ISH is         *
! *                  calculated.                                         *
! *                                                                      *
! *  pet  29-APR-99  Deactivated station  control when SNGCHK_STAMIN     *
! *                  is zero.                                            *
! *  pet  29-APR-99  Deactivated baseline control when SNGCHK_STAMIN     *
! *                  is zero.                                            *
! *  pet  2003.08.18 Minor code rearrangement considering bug in         *
! *                  HP FORTRAN90 compiler.                              *
! *  pet  2007.07.05 Forced SNGCHK set the nornal color hust after       *
! *                  printing any line in red color.                     *
! *                                                                      *
! *  ###  07-JUL-98     SNGCHK    v1.53  (c)  L. Petrov 24-JUL-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'obser.i'
      INCLUDE    'prfil.i'
      INCLUDE    'precm.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'glbc2.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'socom.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      INTEGER*4  SNGCHK_CMP, IUER
      LOGICAL*4  LSOU_FAIL, LSTA_FAIL, LBAS_FAIL, LFAIL
      INTEGER*4  R_SOU, RIS_SOU(MO_SOU), &
     &           R_STA, RIS_STA(MO_STA), &
     &           R_BAS, RIS_BAS(MO_BAS),  IT
      INTEGER*4  J1, J2, J3, J4, IS, IC, IC1, IC2, IP1, IP2, IB1, IB2, &
     &           ICU, IBU, ISH, IP, IT_REP, IOS, IER
      CHARACTER  COND_MES*80, FIL_ERR*128, WORK_DIR*128
      LOGICAL*4  LOP
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2, EXTERNAL ::  KBIT
      LOGICAL*4, EXTERNAL ::  CHECK_STABIT, DATYP_INQ
      INTEGER*4, EXTERNAL ::  ILEN, I_LEN, LTM_DIF, IFIND_PL, NSTBA
!
! --- Initialization
!
      LFAIL     = .FALSE.
      LSOU_FAIL = .FALSE.
      LSTA_FAIL = .FALSE.
      LBAS_FAIL = .FALSE.
      SNGCHK_CMP = SNGCHK_CMP__UNDF
      R_SOU = 0
      R_STA = 0
      R_BAS = 0
      INQUIRE ( UNIT=13, OPENED=LOP ) ! 
!
      IF ( .NOT. LOP ) THEN
           FIL_ERR = PRE_SCR_DIR(1:PRE_SD_LEN)//'ERRF'//PRE_LETRS
           OPEN ( UNIT=13, FILE=FIL_ERR, IOSTAT=IOS, STATUS='UNKNOWN', &
     &            ACCESS='APPEND' )
      END IF
!
! --- Checks of internal control
!
      IF ( SNGCHK_ACTION .EQ. SNGCHK_ACT__UNDF ) THEN
           CALL ERR_LOG ( 8421, IUER, 'SNGCHK', 'Error of internal control: '// &
     &         'Variable SNGCHK from glbc4.i (singularity check) was not '// &
     &         'defined' )
           SNGCHK_CMP = SNGCHK_CMP__FAIL
           RETURN
      END IF
!
      IF ( DBOBJ%STATUS .NE. DBOBJ__DON ) THEN
           CALL ERR_LOG ( 8422, IUER, 'SNGCHK', 'Error of internal control: '// &
     &         'Fields of the data structure DBOBJ was not filled' )
           SNGCHK_CMP = SNGCHK_CMP__FAIL
           RETURN
      END IF
!
      IF ( SNGCHK_ACTION .EQ. SNGCHK_ACT__NONE ) THEN
!
! -------- Singluraity check action: "NONE" -- nothing to do!
!
           SNGCHK_CMP = SNGCHK_CMP__CONT
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Source control
!
      DO 410 J1=1,INT4(NUMSTR)
!
! ------ We bypass the source if both its coordinates were not estimated
! ------ or source was deselected from solution
!
         IF ( .NOT. KBIT( LSTAR(1,1), INT2(J1) ) .AND. &
     &        .NOT. KBIT( LSTAR(1,2), INT2(J1) )       ) GOTO 410
         IF ( .NOT. KBIT( ISRSEL(1),  INT2(J1) )       ) GOTO 410
         IF ( ISLTY2 .EQ. 'F' .OR. ISLTY2 .EQ. 'B' ) THEN
!
! ----------- In batch mode we check: are coordinates of the J1-th source
! ----------- estimated as global parameters or as local parameters
!
              ISH = (IACSRC-1)/4 + 1 ! Shift of the beginning of the list of
!                                    ! sources as CHARACTER*8 to be carried
              IF ( NACSRC .GT. 0 ) THEN
!
! ---------------- We search J1-th source in the list of "carried" sources.
! ---------------- The meaning of this list depends on the vales of logical
! ---------------- variables KCSRC.
! ---------------- If KCSRC .TRUE.  this list contains sources which positions
! ----------------                  are estimated for each arc
! ---------------- If KCSRC .FALSE. this list contains sources which positions
! ----------------                  are estimated as global parameters
!
                   IC = LTM_DIF ( 1, NACSRC, LSELAR(ISH), ISTRN_CHR(J1) )
                   IF ( .NOT. KCSRC  .AND.  IC .GT. 0 ) THEN
!
! --------------------- We found J1-th source in the list of "carried" sources
! --------------------- estimated as global parameter. We stop further check for
! --------------------- this source
!
                       GOTO 410
                   END IF
!
                   IF ( KCSRC  .AND.  IC .LE. 0 ) THEN
!
! --------------------- We didn't found J1-th source in the list of the sources
! --------------------- which are not "carried" -- that means that J1-th source
! --------------------- is estimated as global parameter. We stop further check
! --------------------- for this source
!
                       GOTO 410
                   END IF
                 ELSE ! NACSRC = 0
!
! ---------------- List of the sources to be "carried" is empty
!
                   IF ( KCSRC ) GOTO 410 ! all sources are treated aas global
              END IF
         END IF
!
! ------ IP -- index of the J1-th source in the list of observerd sources
!
         IP = LTM_DIF ( 1, DBOBJ%L_SOU, DBOBJ%C_SOU, ISTRN_CHR(J1) )
         IS = -1
         IF ( IP .GT. 0 ) THEN
!
! ----------- IS -- index of the J1-th source in the list of the sources
! ----------- used in estimation
!
              IS = IFIND_PL ( DBOBJ%U_SOU, DBOBJ%UIS_SOU, DBOBJ%LIS_SOU(IP) )
         END IF
!
         IF ( ( IP .LE. 0  .OR.  IS .LE. 0 )  .AND. &
     &          SNGCHK_SOUMIN .GT. 0                 ) THEN
!
! ----------- J1-th source is missed in one of the lists
!
              LSOU_FAIL = .TRUE.
              CALL SNGCHK_FAIL ( IT, LFAIL )
!
! ----------- Add it to the list of missed sources
!
              CALL ADD_LIS ( MO_SOU, R_SOU, RIS_SOU, J1, -3 )
!
              IF ( IT .NE. 0 ) THEN
                   CALL SET_COLOR ( 1 ) ! set red color
                   WRITE (  6, 110 ) ISTRN_CHR(J1)
                   WRITE ( 13, 110 ) ISTRN_CHR(J1)
                   WRITE ( 23, 110 ) ISTRN_CHR(J1)
 110               FORMAT ( 'No observations of the source ',A )
                   CALL SET_COLOR ( 0 ) ! set normal color
              END IF
           ELSE IF ( IP .GT. 0  .AND.  IS .GT. 0  ) THEN
              IF ( DBOBJ%KU_SOU(IS) .LT. SNGCHK_SOUMIN ) THEN
!
! ---------------- The J1-th source is present in the lists but the number of
! ---------------- observations actually used in the estimation is less than
! ---------------- the specified limit
!
                   LSOU_FAIL = .TRUE.
                   CALL SNGCHK_FAIL ( IT, LFAIL )
!
! ---------------- Add it to the list of missed sources
!
                   CALL ADD_LIS ( MO_SOU, R_SOU, RIS_SOU, J1, -3 )
!
                   IF ( IT .NE. 0 ) THEN
                        CALL SET_COLOR ( 1 ) ! set red color
                        WRITE (  6, 120 ) ISTRN_CHR(J1), DBOBJ%KU_SOU(IS), &
     &                                    SNGCHK_SOUMIN
                        CALL SET_COLOR ( 0 ) ! set normal color
                        WRITE ( 13, 120 ) ISTRN_CHR(J1), DBOBJ%KU_SOU(IS), &
     &                                    SNGCHK_SOUMIN
                        WRITE ( 23, 120 ) ISTRN_CHR(J1), DBOBJ%KU_SOU(IS), &
     &                                    SNGCHK_SOUMIN
 120                    FORMAT ( 'Numb. of obs. of the source ',A, &
     &                           ' is ',I4,' what is less than limit ',I4 )
                   END IF
              END IF
         END IF
 410  CONTINUE
!
! --- Station  control
!
      DO 420 J2=1,INT4(NUMSTA)
         IF ( .NOT. CHECK_STABIT( INT2(J2) ) ) GOTO 420 ! Bypass deselected
!                                                       ! station
!
! ------ IP -- index of the J2-th station in the list of participated stations
!
         IP = LTM_DIF ( 1, DBOBJ%L_STA, DBOBJ%C_STA, ISITN_CHR(J2) )
         IS = -1
         IF ( IP .GT. 0 ) THEN
!
! ----------- IS -- index of the J2-th station in the list of the stations
! ----------- used in estimation
!
              IS = IFIND_PL ( DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%LIS_STA(IP) )
         END IF
!
         IF ( ( IP .LE. 0  .OR.  IS .LE. 0 ) .AND. SNGCHK_STAMIN .GT. 0 ) THEN
!
! ----------- J2-th station is missed in one of the lists
!
              LSTA_FAIL = .TRUE.
              CALL SNGCHK_FAIL ( IT, LFAIL )
!
! ----------- Add it to the list of missed stations
!
              CALL ADD_LIS ( MO_STA, R_STA, RIS_STA, J2, -3 )
!
              IF ( IT .NE. 0 ) THEN
                   CALL SET_COLOR ( 1 ) ! set red color
                   WRITE (  6, 130 ) ISITN_CHR(J2)
                   CALL SET_COLOR ( 0 ) ! set normal color
                   WRITE ( 13, 130 ) ISITN_CHR(J2)
                   WRITE ( 23, 130 ) ISITN_CHR(J2)
 130               FORMAT ( 'No observations at the station  ',A )
              END IF
           ELSE IF ( IP .GT. 0  .AND.  IS .GT. 0  ) THEN
!
! ----------- STAMIN
!
              IF ( DBOBJ%KU_STA(IS) .LT. SNGCHK_STAMIN ) THEN
!
! ---------------- The J2-th station is present in the lists but the number of
! ---------------- observations actually used in the estimation is less than
! ---------------- specified limit
!
                   LSTA_FAIL = .TRUE.
                   CALL SNGCHK_FAIL ( IT, LFAIL )
!
! ---------------- Add it to the list of missed stations
!
                   CALL ADD_LIS ( MO_STA, R_STA, RIS_STA, J2, -3 )
!
                   IF ( IT .NE. 0 ) THEN
                        CALL SET_COLOR ( 1 ) ! set red color
                        WRITE (  6, 140 ) ISITN_CHR(J2), DBOBJ%KU_STA(IS), &
     &                                    SNGCHK_STAMIN
                        CALL SET_COLOR ( 0 ) ! set normal color
                        WRITE ( 13, 140 ) ISITN_CHR(J2), DBOBJ%KU_STA(IS), &
     &                                    SNGCHK_STAMIN
                        WRITE ( 23, 140 ) ISITN_CHR(J2), DBOBJ%KU_STA(IS), &
     &                                    SNGCHK_STAMIN
 140                    FORMAT ( 'Numb. of obs. at the station ',A, &
     &                           ' is ',I4,' what is less than limit ',I4 )
                   END IF
              END IF
         END IF
 420  CONTINUE
!
! --- Baseline control
!
      DO 430 J3=1,INT4(NUMSTA)-1
!
! ------ IP2 -- index of the first station of the baseline in the list
! ------ of participated stations
!
         IP1 = LTM_DIF ( 1, DBOBJ%L_STA, DBOBJ%C_STA, ISITN_CHR(J3) )
!
         DO 440 J4=J3+1,INT4(NUMSTA)
!
! --------- If the baseline under consideration was deselected we will not do
! --------- further any checks
!
            IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                 IF ( .NOT. KBIT ( IBLSEL_P(1,J3), INT2(J4) ) ) GOTO 440
                 IF ( .NOT. KBIT ( IBLSEL_P(1,J4), INT2(J3) ) ) GOTO 440
               ELSE
                 IF ( .NOT. KBIT ( IBLSEL_G(1,J3), INT2(J4) ) ) GOTO 440
                 IF ( .NOT. KBIT ( IBLSEL_G(1,J4), INT2(J3) ) ) GOTO 440
            END IF
!
! --------- IP2 -- index of the second station of the baseline in the list
! --------- of participated stations
!
            IP2 = LTM_DIF ( 1, DBOBJ%L_STA, DBOBJ%C_STA, ISITN_CHR(J4) )
            IC1 = -1
            IC2 = -1
            IB1 = -1
            IB2 = -1
            IF ( IP1 .GT. 0  .AND.  IP2 .GT. 0 ) THEN
!
! -------------- IC1  -- Index of the baseline under consideration in direct
! --------------         order of stations
! -------------- IC2  -- Index of the baseline under consideration in reverse
! --------------         order of stations
!
                 IC1 = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, NSTBA(J3,J4) )
                 IC2 = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, NSTBA(J4,J3) )
!
! -------------- IB1, IB2 -- the same but indices in the list of used
! --------------             observations
!
                 IB1 = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, NSTBA(J3,J4) )
                 IB2 = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, NSTBA(J4,J3) )
            END IF
!
! --------- Find IBU -- index of the baseline under consideration in the list
! --------- of used baseline. Some trick: if we have observations at two
! --------- baselines formed from the same stations we will consider the
! --------- baseline where more observations have been used
!
!
            IF ( IB1 .GE. IB2 ) THEN
                 IBU = IB1
                 ICU = IC1
              ELSE
                 IBU = IB2
                 ICU = IC2
            END IF
!
            IF ( IBU .LE. 0  .AND.  SNGCHK_BASMIN .GT. 0 ) THEN
!
! -------------- Baseline under investigation is missed
!
                 LBAS_FAIL = .TRUE.
                 CALL SNGCHK_FAIL ( IT, LFAIL )
!
! -------------- Add it to the list of missed stations
!
                 CALL ADD_LIS ( MO_BAS, R_BAS, RIS_BAS, NSTBA(J3,J4), -3 )
!
                 IF ( IT .NE. 0 ) THEN
                      CALL SET_COLOR ( 1 ) ! set color
                      WRITE (  6, 150 ) ISITN_CHR(J3), ISITN_CHR(J4)
                      CALL SET_COLOR ( 0 ) ! set normal color
                      WRITE ( 13, 150 ) ISITN_CHR(J3), ISITN_CHR(J4)
                      WRITE ( 23, 150 ) ISITN_CHR(J3), ISITN_CHR(J4)
 150                  FORMAT ( 'No observations at the baseline ',A,'/',A )
                 END IF
              ELSE IF ( IBU .GT. 0 ) THEN
!
! -------------- BASMIN
!
                 IF ( DBOBJ%KU_BAS(IBU) .LT. SNGCHK_BASMIN ) THEN
!
! ------------------- The baseline under consideration is present in the lists
! ------------------- but the number of observations actually used in the
! ------------------- estimation is less than the specified limit
!
                      LBAS_FAIL = .TRUE.
                      CALL SNGCHK_FAIL ( IT, LFAIL )
!
! ------------------- Add it to the list of missed baselines
!
                      CALL ADD_LIS ( MO_BAS, R_BAS, RIS_BAS, NSTBA(J3,J4), -3 )
!
                      IF ( IT .NE. 0 ) THEN
                         CALL SET_COLOR ( 1 ) ! set red color
                         WRITE (  6, 160 ) DBOBJ%C_BAS(ICU), DBOBJ%KU_BAS(IBU), &
     &                                     SNGCHK_BASMIN
                         CALL SET_COLOR ( 0 ) ! set normal color
                         WRITE ( 13, 160 ) DBOBJ%C_BAS(ICU), DBOBJ%KU_BAS(IBU), &
     &                                     SNGCHK_BASMIN
                         WRITE ( 23, 160 ) DBOBJ%C_BAS(ICU), DBOBJ%KU_BAS(IBU), &
     &                                     SNGCHK_BASMIN
 160                     FORMAT ( 'Num_obs at the baseline ',A, &
     &                            ' is ',I4,' what is less than limit ',I4 )
                      END IF
                  END IF
            END IF
 440     CONTINUE
 430  CONTINUE
!
      IF ( LFAIL ) THEN
           CALL CLRCH ( COND_MES )
!
! -------- Determination of condition code and generating the message
!
           IF ( SNGCHK_ACTION .EQ. SNGCHK_ACT__WARN ) THEN
                SNGCHK_CMP = SNGCHK_CMP__CONT
                COND_MES = 'Execution is continuing'
              ELSE IF ( SNGCHK_ACTION .EQ. SNGCHK_ACT__REPR ) THEN
                SNGCHK_CMP = SNGCHK_CMP__BACK
                COND_MES = 'Solution is reparameterized'
              ELSE IF ( SNGCHK_ACTION .EQ. SNGCHK_ACT__STOP ) THEN
                SNGCHK_CMP = SNGCHK_CMP__STOP
                COND_MES = 'Execution will be terminated'
              ELSE IF ( SNGCHK_ACTION .EQ. SNGCHK_ACT__SKIP ) THEN
                IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6) ) ) THEN
!
! ------------------ Interactive mode
!
                     SNGCHK_CMP = SNGCHK_CMP__STOP
                     COND_MES = 'Execution will be terminated'
                   ELSE
!
! ------------------ Batch mode
!
                     SNGCHK_CMP = SNGCHK_CMP__SKIP
                     COND_MES = 'Database will be skipped'
                END IF
           END IF
!
! -------- The database failed one of the tests
!
           IF ( IT .GE. 1 ) THEN
                CALL SET_COLOR ( 1 ) ! set red color
                WRITE  (  6, 170 ) DBOBJ%NAME(1:I_LEN(DBOBJ%NAME)), &
     &                             COND_MES(1:I_LEN(COND_MES))
                CALL SET_COLOR ( 0 ) ! set normal color
                WRITE  ( 13, 170 ) DBOBJ%NAME(1:I_LEN(DBOBJ%NAME)), &
     &                             COND_MES(1:I_LEN(COND_MES))
                WRITE  ( 23, 170 ) DBOBJ%NAME(1:I_LEN(DBOBJ%NAME)), &
     &                             COND_MES(1:I_LEN(COND_MES))
 170            FORMAT ( 'Database ',A,' failed singularity check. ',A )
           END IF
           IF ( SNGCHK_ACTION .EQ. SNGCHK_ACT__REPR ) THEN
!
! ------------- Reparameterization of the solutiuon by deleting from the list
! ------------- of participated sources/stations/baselines objects which
! ------------- appeared to be missed
!
                CALL ERR_PASS ( IUER, IER )
                IT_REP = IT
                IF ( IT .EQ. -1 ) IT_REP = 1
                CALL REPARAM  ( DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%U_BAS, &
     &                          DBOBJ%UIS_BAS, R_SOU, RIS_SOU, R_STA, RIS_STA, &
     &                          R_BAS, RIS_BAS, IT, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8423, IUER, 'SNGCHK', 'Error during '// &
     &                   'attempt to reparameterize the solution' )
                     SNGCHK_CMP = SNGCHK_CMP__FAIL
                     RETURN
                END IF
           END IF
!
           IF ( IT .GE. 1 ) THEN
                WRITE (  6, '(A)' ) '$$$ PROC(sngchk) $$$'
                CLOSE ( UNIT=13 ) ! closing error-file
                IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6) ) ) THEN
!
! ------------------ Interactive mode
!
                     CALL HIT_CONT ( 'Hit any key to continue', %VAL(0) )
                     CALL START_MN()   ! start curses again
                END IF
           END IF
         ELSE
           SNGCHK_CMP = SNGCHK_CMP__CONT
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SNGCHK  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SNGCHK_FAIL ( IT, LFAIL )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine  SNGCHK_FAIL  prepairs screen and output files   *
! *   for writing there diagnostic information to be generated by the    *
! *   routine SNGCHK.                                                    *
! *                                                                      *
! *  ###  07-JUL-98  SNGCHK_FAIL   v1.3  (c)  L. Petrov  24-JUL-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      LOGICAL*4  LFAIL
      CHARACTER  ERR_FNAME*128
      INTEGER*4  IT, I13, I_LEN
      LOGICAL*2  KBIT
!
      IF ( LFAIL ) RETURN
      LFAIL = .TRUE.
      IF ( IT .GE. 1 ) THEN
           IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6) ) ) THEN
                WRITE ( 6, * ) ' SNGCHK_fail: Disabling curses...'
                CALL END_MN()  ! postpone curser
                CALL UN_CURSES ( )
                CALL SET_COLOR ( 1 ) ! set red color
           END IF
!
! -------- Open spool-file
!
           CALL USE_SPOOL ( 'O' )
           ERR_FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'ERRF'//PRE_LETRS
           OPEN ( UNIT=13, FILE=ERR_FNAME, STATUS='UNKNOWN', ACCESS='APPEND', &
     &            IOSTAT=I13 )
           IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6) ) ) THEN
                CALL SET_COLOR ( 0 ) ! set normal color
           END IF
!
           WRITE ( 13, '(A)' ) '$$$ PROC(sngchk) $$$'
           WRITE ( 23, '(A)' ) '$$$ PROC(sngchk) $$$'
      END IF
!
      RETURN
      END  !#!  SNGCHK_FAIL  #!#
