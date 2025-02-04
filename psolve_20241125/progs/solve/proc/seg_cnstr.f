      SUBROUTINE SEG_CNSTR ( L_STA, CLIS_STA, L_PAR, CLIS_PAR, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SEG_CNSTR  imposes constraints on segmented parameters.   *
! *   It adds corrections to the normal matrix.                          *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *     L_STA ( INTEGER*4 ) -- The number of stations participated in    *
! *                            this session.                             *
! *  CLIS_STA ( CHARACTER ) -- The list of station names participated in *
! *                            this session. Length of the list is L_STA *
! *     L_PAR ( INTEGER*4 ) -- The total number of parameters to be      *
! *                            estimated.                                *
! *  CLIS_PAR ( CHARACTER ) -- The list of names of the parameters to be *
! *                            estimated. Length of the list is L_PAR.   *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *  CNSTROBJ ( RECORD    ) -- The data structure with information about *
! *                            constraints (position where the matrix    *
! *                            should be modified and the value of       *
! *                            constraints).                             *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *            Input: mode switch:                                       *
! *                   IUER=0 -- no error messages  will be generated     *
! *                             even in the case of error.               *
! *                   IUER=-1 - in the case of error the message will    *
! *                             be put in stdout.                        *
! *            Output: 0 in the case of successful completion and        *
! *                    non-zero in the case of error.                    *
! *                                                                      *
! *    Comments:                                                         *
! *    1) It is assumed that SOCOM common ares has been read from disk   *
! *       previously.                                                    *
! *    2) It is assumed that the length of the time span for each        *
! *       parameter is the same for all segments (but allowed to vary    *
! *       from parameter to parameter).                                  *
! *                                                                      *
! *   When       Who  What                                               *
! *   24-OCT-97  pet  Ceased to support case of non-uniform clock        *
! *                   segments. Ceased to support case of old clock      *
! *                   parameterization.                                  *
! *   19-JAN-98  pet  Rewrote the program not to apply constraints at    *
! *                   once but to gather them in data structure          *
! *                   CNSTROBJ and then apply.                           *
! *   04-MAY-99  pet  Corrected an error in change weights when          *
! *                   non-uniform intervals for atmosphere are used.     *
! *   2000.01.25 pet  General cleanup.                                   *
! *   2002.09.18 pet  Compltely re-wrote. Changed internal logic: the    *
! *                   new version puts equations of constraitns in       *
! *                   CNSTROBJ, while the old version put normal         *
! *                   equations of constraints                           *
! *                                                                      *
! *  ###  31-MAR-97    SEG_CNSTR   v4.0  (c)  L. Petrov 18-SEP-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'astro_constants.i'
      INCLUDE    'solve.i'
      INCLUDE    'erm.i'
      INCLUDE    'socom.i'
      INCLUDE    'socom_plus.i'
      INCLUDE    'cnstr.i'
      INTEGER*4  L_STA, L_PAR, IUER
      CHARACTER  CLIS_STA(L_STA)*8, CLIS_PAR(L_PAR)*20
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  ISTA, J1, IER
      LOGICAL*4  IS_NUM
      LOGICAL*2  KBIT
      REAL*8     GNAME_JD
      REAL*8     CLO_INT_SEC, ATM_INT_SEC, XPL_INT_DAY, YPL_INT_DAY, &
     &           UT1_INT_DAY
!
! --- Specifing minimal acceptable values of atmosphere, clock and EOP
! --- parameters
!
      REAL*8       CLO_SEC_MIN, ATM_SEC_MIN, EOP_SEC_MIN
      PARAMETER  ( CLO_SEC_MIN = 0.01*3600.0D0 )
      PARAMETER  ( ATM_SEC_MIN = 0.01*3600.0D0  )
      PARAMETER  ( EOP_SEC_MIN = 0.01*3600.0D0  )
      INTEGER*4  ICLO_CNS, IND_CLO(MAX_ARC_STA), &
     &           IATM_CNS, IND_ATM(MAX_ARC_STA), &
     &           IXPL_CNS, IND_XPL, IYPL_CNS, IND_YPL, IUT1_CNS, IND_UT1
      REAL*8     DAT_CUR, DAT_CLO(MAX_ARC_STA), DAT_ATM(MAX_ARC_STA)
      INTEGER*4  ILEN, I_LEN, LTM_DIF
!
      IF ( SOCOM_PLUS_FIRST .NE. SPL__DONE ) THEN
           CALL ERR_LOG ( 8511, IUER, 'SEG_CNSTR', 'socom_plus has not '// &
     &         'been initialized. It is fatal error' )
           RETURN
      END IF
!
      ICLO_CNS = 0
      IATM_CNS = 0
      IXPL_CNS = 0
      IYPL_CNS = 0
      IUT1_CNS = 0
!
      DO 410 J1=1,L_PAR
!%  write ( 6, * ) ' seg_cnst: 101 j1= ', j1, ' l_par= ', l_par ; call flush (6 ) ! %%%%%
!
! ====== Constraints on clock rate
!
         IF ( CLIS_PAR(J1)(9:10) .EQ. 'C0' .AND. &
     &        IS_NUM(CLIS_PAR(J1)(11:20))  ) THEN
!
              ISTA = LTM_DIF ( 0, L_STA, CLIS_STA, CLIS_PAR(J1) )
              IF ( ISTA .LE. 0 ) THEN
                   write ( 6, * ) 'l_sta = ', l_sta !%%%
                   write ( 6, * ) 'clis_sta= ', clis_sta(1:l_sta)//' '
                   CALL ERR_LOG ( 8512, IUER, 'SEG_CNSTR', 'Unrecognized '// &
     &                 'station in parameter '//CLIS_PAR(J1) )
                   RETURN
              END IF
              IND_CLO(ISTA) = J1
              DAT_CLO(ISTA) = GNAME_JD ( CLIS_PAR(J1)(11:20) )
         END IF
!
         IF ( CLIS_PAR(J1)(9:10) .EQ. 'c0' .AND. &
     &        IS_NUM(CLIS_PAR(J1)(11:20))  ) THEN
!
              ISTA = LTM_DIF ( 0, L_STA, CLIS_STA, CLIS_PAR(J1) )
              IF ( ISTA .LE. 0 ) THEN
                   CALL ERR_LOG ( 8513, IUER, 'SEG_CNSTR', 'Unrecognized '// &
     &                 'station in parameter '//CLIS_PAR(J1) )
                   RETURN
              END IF
!
              DAT_CUR = GNAME_JD ( CLIS_PAR(J1)(11:20) )
              ICLO_CNS = ICLO_CNS + 1
              CLO_INT_SEC = (DAT_CLO(ISTA) - DAT_CUR)*86400.0D0
!%  write ( 6, * ) ' seg_cnst: clo_int_sec = ', clo_int_sec,' ista=',ista, ' sccnst =', sccnst(ista) ! %%%
!!              CLO_INT_SEC = 1.0D0*(IDINT(CLO_INT_SEC))
!
! ----------- Add information about the type of the constraint applied
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( 'CLO_RATE', ICLO_CNS, 'Clock rate between '// &
     &             'segments', '10^-14 sec/sec', 0.0D0, SCCNST(ISTA)*1.D-14, &
     &             .FALSE., CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8514, IUER, 'SEG_CNSTR', 'Error in '// &
     &                 'storing information about clock rate constraint' )
                   RETURN
              END IF
!
! ----------- Put coefficients of the equation of constraint
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_EQU ( 'CLO_RATE', ICLO_CNS, J1, 1.0D0/CLO_INT_SEC, &
     &                          .FALSE., CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8515, IUER, 'SEG_CNSTR', 'Error in '// &
     &                 'an attempt to store clock rate constraint' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_EQU ( 'CLO_RATE', ICLO_CNS, IND_CLO(ISTA), &
     &                          -1.0D0/CLO_INT_SEC, .FALSE., CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8516, IUER, 'SEG_CNSTR', 'Error in '// &
     &                 'an attempt to store clock rate constraint' )
                   RETURN
              END IF
!%                write ( 77, * ) 'clo_rate: ',iclo_cns, j1, ind_clo(ista), & ! %%
!     &                           1.0d0/clo_int_sec  ! %%%%%%%%%%%%%%%%%%%%%%%%%%
!
              IND_CLO(ISTA) = J1
              DAT_CLO(ISTA) = DAT_CUR
         END IF
!
! ====== Constraints on atmosphere rate
!
         IF ( CLIS_PAR(J1)(9:10) .EQ. 'A0' .AND. &
     &        IS_NUM(CLIS_PAR(J1)(11:20))  ) THEN
!%  write ( 6, * ) ' seg_cnst: 178' ; call flush (6 ) ! %%%%%
!
              ISTA = LTM_DIF ( 0, L_STA, CLIS_STA, CLIS_PAR(J1) )
              IF ( ISTA .LE. 0 ) THEN
                   CALL ERR_LOG ( 8517, IUER, 'SEG_CNSTR', 'Unrecognized '// &
     &                 'station in parameter '//CLIS_PAR(J1) )
                   RETURN
              END IF
              IND_ATM(ISTA) = J1
              DAT_ATM(ISTA) = GNAME_JD ( CLIS_PAR(J1)(11:20) )
         END IF
         IF ( CLIS_PAR(J1)(9:10) .EQ. 'a0' .AND. &
     &        IS_NUM(CLIS_PAR(J1)(11:20))  ) THEN
!%  write ( 6, * ) ' seg_cnst: 191' ; call flush (6 ) ! %%%%%
!
              ISTA = LTM_DIF ( 0, L_STA, CLIS_STA, CLIS_PAR(J1) )
              IF ( ISTA .LE. 0 ) THEN
                   CALL ERR_LOG ( 8518, IUER, 'SEG_CNSTR', 'Unrecognized '// &
     &                 'station in parameter '//CLIS_PAR(J1) )
                   RETURN
              END IF
!
              DAT_CUR = GNAME_JD ( CLIS_PAR(J1)(11:20) )
              IATM_CNS = IATM_CNS + 1
              ATM_INT_SEC = (DAT_ATM(ISTA) - DAT_CUR)*86400.0D0
!%  write ( 6, * ) ' seg_cnst: atm_int_sec = ', atm_int_sec,' ista=',ista, ' sacnst =', sacnst(ista) ! %%%
!!              ATM_INT_SEC = 1.0D0*(IDINT(ATM_INT_SEC))
!
! ----------- Add information about the type of the constraint applied
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( 'ATM_RATE', IATM_CNS, 'Atmosphere rate '// &
     &             'between segments', 'psec/hr', 0.0D0, &
     &             SACNST(ISTA)/3600.0D0*1.D-12, .FALSE., &
     &             CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8519, IUER, 'SEG_CNSTR', 'Error in '// &
     &                 'storing information about atmopshere rate constraint' )
                   RETURN
              END IF
!
! ----------- Put coefficients of the equation of constraint
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_EQU ( 'ATM_RATE', IATM_CNS, J1, 1.0D0/ATM_INT_SEC, &
     &                          .FALSE., CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8520, IUER, 'SEG_CNSTR', 'Error in '// &
     &                 'an attempt to store atmosphere rate constraint' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_EQU ( 'ATM_RATE', IATM_CNS, IND_ATM(ISTA), &
     &                          -1.0D0/ATM_INT_SEC, .FALSE., CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8521, IUER, 'SEG_CNSTR', 'Error in '// &
     &                 'an attempt to store atmopshere rate constraint' )
                   RETURN
              END IF
!
              IND_ATM(ISTA) = J1
              DAT_ATM(ISTA) = DAT_CUR
         END IF
!%  write ( 6, * ) ' seg_cnst: 242' ; call flush (6 ) ! %%%%%
!
! ====== Constraints on X pole wobble
!
         IF ( CLIS_PAR(J1)(1:10) .EQ. 'X WOBBLE 0'   .AND. &
     &        IS_NUM(CLIS_PAR(J1)(11:20))            .AND. &
     &        KBIT ( CONSTRAINT_BITS, POL_RAT__CNB ) .AND. &
     &        SEOCNST(1) .GT. 0.0D0                        ) THEN
!
              IXPL_CNS = IXPL_CNS + 1
              IF ( IXPL_CNS .GT. 1 ) THEN
!
! ---------------- Units: SEOCNST(1)      -- mas/day ( "sigma" of constraint )
! ----------------        ROT_INTERVAL(1) -- days ( lenght of the time span )
! ----------------        EOP_INTERVAL    -- days ( lenght of the time span )
! ----------------        EOP_INT_DAY     -- days ( lenght of the time span )
! ----------------        X_pole          -- radians
!
                   IF ( UNF_EOP ) THEN
                        XPL_INT_DAY = EOP_INTERVAL
                      ELSE
                        XPL_INT_DAY = ROT_INTERVAL(1)
                   END IF
!
! ---------------- Add information about the type of the constraint applied
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_NAM ( 'XPL_RATE', IXPL_CNS-1, 'X-pole rate '// &
     &                 'between segments', 'mas/day', 0.0D0, &
     &                  SEOCNST(1)*MAS__TO__RAD, .FALSE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8522, IUER, 'SEG_CNSTR', 'Error in '// &
     &                      'storing information about xpole constraint' )
                        RETURN
                   END IF
!
! ---------------- Put coefficients of the equation of constraint
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_EQU ( 'XPL_RATE', IXPL_CNS-1, J1, &
     &                               1.0D0/XPL_INT_DAY, .FALSE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8523, IUER, 'SEG_CNSTR', 'Error in '// &
     &                      'an attempt to store xpole constraint' )
                        RETURN
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_EQU ( 'XPL_RATE', IXPL_CNS-1, IND_XPL, &
     &                             -1.0D0/XPL_INT_DAY, .FALSE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8524, IUER, 'SEG_CNSTR', 'Error in '// &
     &                      'an attempt to store xpole constraint' )
                        RETURN
                   END IF
              END IF
              IND_XPL = J1
         END IF
!%  write ( 6, * ) ' seg_cnst: 298' ; call flush (6 ) ! %%%%%
!
! ====== Constraints on Y pole wobble
!
         IF ( CLIS_PAR(J1)(1:10) .EQ. 'Y WOBBLE 0'   .AND. &
     &        IS_NUM(CLIS_PAR(J1)(11:20))            .AND. &
     &        KBIT ( CONSTRAINT_BITS, POL_RAT__CNB ) .AND. &
     &        SEOCNST(1) .GT. 0.0D0                        ) THEN
!
              IYPL_CNS = IYPL_CNS + 1
              IF ( IYPL_CNS .GT. 1 ) THEN
!
! ---------------- Units: SEOCNST(1)      -- mas/day ( "sigma" of constraint )
! ----------------        ROT_INTERVAL(1) -- days ( lenght of the time span )
! ----------------        EOP_INTERVAL    -- days ( lenght of the time span )
! ----------------        EOP_INT_DAY     -- days ( lenght of the time span )
! ----------------        Y_pole          -- radians
!
                   IF ( UNF_EOP ) THEN
                        YPL_INT_DAY = EOP_INTERVAL
                      ELSE
                        YPL_INT_DAY = ROT_INTERVAL(1)
                   END IF
!
! ---------------- Add information about the type of the constraint applied
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_NAM ( 'YPL_RATE', IYPL_CNS-1, 'Y-pole rate '// &
     &                 'between segments', 'mas/day', 0.0D0, &
     &                  SEOCNST(1)*MAS__TO__RAD, .FALSE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8525, IUER, 'SEG_CNSTR', 'Error in '// &
     &                      'storing information about ypole constraint' )
                        RETURN
                   END IF
!
! ---------------- Put coefficients of the equation of constraint
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_EQU ( 'YPL_RATE', IYPL_CNS-1, J1, &
     &                               1.0D0/YPL_INT_DAY, .FALSE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8526, IUER, 'SEG_CNSTR', 'Error in '// &
     &                      'an attempt to store information about ypole '// &
     &                      'constraint' )
                        RETURN
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_EQU ( 'YPL_RATE', IYPL_CNS-1, IND_YPL, &
     &                             -1.0D0/YPL_INT_DAY, .FALSE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8527, IUER, 'SEG_CNSTR', 'Error in '// &
     &                      'an attempt to store information about ypole '// &
     &                      'constraint' )
                        RETURN
                   END IF
              END IF
              IND_YPL = J1
         END IF
!%  write ( 6, * ) ' seg_cnst: 358' ; call flush (6 ) ! %%%%%
!
! ====== Constraints on UT1-TAI angle
!
         IF ( CLIS_PAR(J1)(1:10) .EQ. 'UT1-TAI  0' .AND. &
     &        IS_NUM(CLIS_PAR(J1)(11:20))            .AND. &
     &        KBIT ( CONSTRAINT_BITS, UT1_RAT__CNB ) .AND. &
     &        SEOCNST(2) .GT. 0.0D0                        ) THEN
!
              IUT1_CNS = IUT1_CNS + 1
              IF ( IUT1_CNS .GT. 1 ) THEN
!
! ---------------- Units: SEOCNST(2)    -- msec/day ("sigma" of constraint)
! ----------------        ROT_INTERVAL(2) -- days ( lenght of the time span )
! ----------------        EOP_INTERVAL    -- days ( lenght of the time span )
! ----------------        UT1_INT_DAY     -- days ( lenght of the time span )
! ----------------        UT1 angle       -- seconds of time (NB!!)
!
                   IF ( UNF_EOP ) THEN
                        UT1_INT_DAY = EOP_INTERVAL
                      ELSE
                        UT1_INT_DAY = ROT_INTERVAL(2)
                   END IF
!
! ---------------- Add information about the type of the constraint applied
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_NAM ( 'UT1_RATE', IUT1_CNS-1, 'UT1 rate '// &
     &                 'between segments', 'msec/day', 0.0D0, &
     &                  SEOCNST(2)/1000.0D0, .FALSE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8528, IUER, 'SEG_CNSTR', 'Error in '// &
     &                      'storing information about ut1 rate constraint' )
                        RETURN
                   END IF
!
! ---------------- Put coefficients of the equation of constraint
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_EQU ( 'UT1_RATE', IUT1_CNS-1, J1, &
     &                             1.0D0/UT1_INT_DAY, .FALSE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8529, IUER, 'SEG_CNSTR', 'Error in '// &
     &                      'an attempt to store ut1 rate constraint' )
                        RETURN
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_EQU ( 'UT1_RATE', IUT1_CNS-1, IND_UT1, &
     &                             -1.0D0/UT1_INT_DAY, .FALSE., CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8539, IUER, 'SEG_CNSTR', 'Error in '// &
     &                      'an attempt to store ut1 constraint equation' )
                        RETURN
                   END IF
              END IF
              IND_UT1 = J1
         END IF
!%  write ( 6, * ) ' seg_cnst: 416' ; call flush (6 ) ! %%%%%
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SEG_CNSTR  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   GNAME_JD ( STR )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine  GNAME_JD transforms date and time from GET_NAME *
! *   date format (f.e. 8906262156) to julian date.                      *
! *                                                                      *
! *  ###  24-OCT-97    GNAME_JD    v1.0  (c)  L. Petrov  24-OCT-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER  STR*(*)
      INTEGER*2  IYEAR_I2, IMONTH_I2, IDAY_I2, kval
      INTEGER*4  IHOUR_I4, IMIN_I4, ival
      REAL*8     GNAME_JD
      REAL*8,    EXTERNAL :: FJLDY
!
!@  write ( 6, * ) ' str(1:10) = ', str(1:10), '       >>',str(1:2)//'<< ' ! %%%%%%%
      READ ( UNIT=STR(1:2), FMT='(I2)' ) KVAL
      READ ( UNIT=STR(3:4), FMT='(I2)' ) IMONTH_I2
      READ ( UNIT=STR(5:6), FMT='(I2)' ) IDAY_I2 
      READ ( UNIT=STR(7:10), FMT='(2I2)' ) IHOUR_I4, IMIN_I4
      READ ( UNIT=STR(1:10), FMT='(5I2)' ) IYEAR_I2, IMONTH_I2, IDAY_I2
     &                       
!@  write ( 6, * ) ' dat: ', iyear_i2, imonth_i2, iday_i2, &
!@     &                       ihour_i4, imin_i4 ! %%%%%%%%%
      GNAME_JD = FJLDY ( IMONTH_I2, IDAY_I2, IYEAR_I2 ) + &
     &                   ( IHOUR_I4 + IMIN_I4/60.0 ) /24.0
!
      RETURN
      END  !#!  GNAME_JD  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   IS_NUM ( STR )
! ************************************************************************
! *                                                                      *
! *   Logical function IS_NUM returns .TRUE. if the string STR contains  *
! *   only digits, blanks and horizontal tabulation characters.          *
! *                                                                      *
! *  ### 18-SEP-2002    IS_NUM     v1.0 (c)  L. Petrov  18-SEP-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER  STR*(*), PATTERN*13
      PARAMETER  ( PATTERN = '1234567890 '//CHAR(0)//CHAR(9) )
      LOGICAL*4  IS_NUM
      INTEGER*4  J1
      INTEGER*4  I_LEN
!
      DO 410 J1=1,I_LEN(STR)
         IF ( INDEX ( PATTERN, STR(J1:J1)) .LE. 0 ) THEN
              IS_NUM = .FALSE.
              RETURN
         END IF
 410  CONTINUE
      IS_NUM = .TRUE.
      RETURN
      END  !#!  IS_NUM  #!#
