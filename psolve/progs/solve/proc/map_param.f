      SUBROUTINE MAP_PARAM ( FAST_MODE, FAST_DBG, B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MAP_PARAM  investigates structure of the parameters    *
! *   to be estimated using B3D or B1B3D algorithm, separates them on    *
! *   global and and segmented. And after that it fills fields of the    *
! *   object B3DOBJ.                                                     *
! *                                                                      *
! *   The order of parameters for FULL case and for B3D case is          *
! *   different:                                                         *
! *                                                                      *
! *   X X X X X X X X X X X X X X X X X X X X X X X X X  (FULL case)     *
! *                                                                      *
! *                                                                      *
! *   ggggg llllll lllll llllll lllll llllll llllll lll  (B3D case)      *
! *                                                                      *
! *   0-bl  1-bl   2-bl  3-bl   4-bl  5-bl   6-bl   7-bl                 *
! *                                                                      *
! *   The routine just finds correspondence between each parameter       *
! *   for FULL case and for B3D case. Parameters in B3D case has two     *
! *   dimensional address: number of block (0 for global) and the index  *
! *   in block. The order of parameters in segmented block: first all    *
! *   clock parameters (in order of stations which is the same as for    *
! *   full case) and after -- all atmosphere parameters. So in the case  *
! *   when the time span for clocks and atmosphere is the same the order *
! *   of parameters in one segment is the following:                     *
! *                                                                      *
! *   CL(st.1) CL(st.2) ... CL(st.n) | AT(st.1) AT(st.2) ... AT(st.n)    *
! *      (totally n-1 stations)      | (totally n stations)              *
! *                                                                      *
! *   When the length of atmosphere span is shorter than time span for   *
! *   clock (but is a multiple!) then one block contains parameters for  *
! *   one segment for clocks and K segments for atmosphere. K is equal   *
! *   the multiplicity time span of atmosphere and clocks for all blocks *
! *   except the last. Last block contains equal number of segments for  *
! *   atmosphere and clocks. Why? This plot shows fragment matrix of     *
! *   conditions equations and illustrate the case when the time span of *
! *   one atmosphere segment in 3 times shorter time span of one clock   *
! *   segment.                                                           *
! *                                                                      *
! *   G  CAA   C                                                         *
! *   G  C AA  C                                                         *
! *   G  C  A  CA                                                        *
! *                                                                      *
! *   G        CAA   C                                                   *
! *   G        C AA  C                                                   *
! *   G        C  A  CA                                                  *
! *                                                                      *
! *   G              CAA   C                                             *
! *   G              C AA  C                                             *
! *   G              C  A  CA                                            *
! *                                                                      *
! *   G                    CAA   C                                       *
! *   G                    C AA  C                                       *
! *   G                    C  A  CA                                      *
! *                                                                      *
! *   This plot disclose another problem in treating non-equal segmented *
! *   parameters. When conditions of equations are made we should decide *
! *   in which block coefficient should be put. We see that in first     *
! *   two lines clock parameters should be put in current and in next    *
! *   block, but atmosphere parameters should be put only in current     *
! *   block, while in third string next atmosphere parameter should be   *
! *   put in next block. To handle all these case logical variable NEXT  *
! *   is calculated for each parameters. It is .TRUE. when parameters    *
! *   of next segment should be put to the next block and it it is FALSE *
! *   parameter of the next segment should be put in current block.      *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! * FAST_MODE ( INTEGER*4 ) -- switch for the mode. Acceptable modes are *
! *                            F__B3D and F__B1B3D.                      *
! * FAST_DBG  ( INTEGER*4 ) -- Verbosity mode switch for debug. Should   *
! *                            be zero for normal work.                  *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *     IUER ( INTEGER*4 ) -- Universal error handler.                   *
! *            Input: mode switch:                                       *
! *                   IUER=0 -- no error messages  will be generated     *
! *                             even in the case of error.               *
! *                   IUER=-1 - in the case of error the message will    *
! *                             be put in stdout.                        *
! *            Output: 0 in the case of successful completion and        *
! *                    non-zero in the case of error.                    *
! *                                                                      *
! * 2019.08.06 pet Added support a case when estimation of atmspheric    *
! *                parameters is switched off at one or more sites.      *
! *                                                                      *
! *  ###  07-JAN-1997   MAP_PARAM    v4.2    L. Petrov  06-AUG-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'fast.i'
!
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  FAST_MODE, FAST_DBG, IUER
!
      INTEGER*2  ILPAR_LEN2
      INTEGER*4  NPARAM2, NPARAM3, NPARAM3_NEW
      PARAMETER ( ILPAR_LEN2 = 20 )
      CHARACTER  LPARM(M_GPA)*(ILPAR_LEN2), LPARM_GLO(M_GPA)*(ILPAR_LEN2), &
     &           LPARM_GLO_NEW(M_GPA)*(ILPAR_LEN2), FINAM_PARAM*80
      CHARACTER  STR*32, STR1*32, STR2*32, STR3*32, STR4*32, MIN_OBJECT*5, &
     &           MAX_OBJECT*5
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, K1, NP_A, NP_C, NP_E, &
     &           NP_MIN, NP_MAX, IATM, ICLO, IEOP
      INTEGER*4  LIS_CLO(MAX_STA), L_CLO, LIS_ATM(MAX_STA), L_ATM, L_EOP
      INTEGER*4  IL, PRV_SGM, ISEG_CLO, ISEG_ATM, ISEG_EOP, M_STA, L_STA, &
     &           IBL, IPL, K_GLO, K_LOC, IND_CLO, IND_ATM, IND_EOP
      LOGICAL*2  KSHORT, KGLOBAL
      REAL*8     CLO_TIME, CLO_LAST, CLO_MAX, &
     &           ATM_TIME, ATM_LAST, ATM_MAX, &
     &           EOP_TIME, EOP_LAST, EOP_MAX
      REAL*8     EPS_SEC
      PARAMETER  ( EPS_SEC = 0.1 ) !  max acceptble difference of first epoch
      INTEGER*4  I_LEN, LTM_DIF, IFIND_PL
      INTEGER*4  IER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4  CLK_EPOCH, ATM_EPOCH
      LOGICAL*2, EXTERNAL :: KBIT
!
      COMMON  / MAP_DBG / CLO_MAX, ATM_MAX, EOP_MAX, &
     &                    L_STA, L_CLO, L_ATM, L_EOP, &
     &                    NP_MIN, NP_MAX, MIN_OBJECT, NP_A, NP_C, NP_E, &
     &                    K_LOC, K_GLO
!
      CALL CLRCH ( FINAM_PARAM )
      FINAM_PARAM = '/tmp/param.fil'  !  The name of debug file
!
      IF ( SOCOM_PLUS_FIRST .NE. SPL__DONE ) THEN
           CALL ERR_LOG ( 8341, IUER, 'MAP_PARAM', 'socom_plus has not '// &
     &         'been initialized. It is fatal error' )
           RETURN
      END IF
!
      IF ( .NOT. FAST_ELIG ) THEN
           CALL ERR_LOG ( 8342, IUER, 'MAP_PARAM', 'Session '// &
     &          B3DOBJ%DBNAME_MES//' marked as uneligible for '// &
     &         'implementation fast algorithms. What''s to do? '// &
     &         '1) Test is FAST_ELIG (from socom_plus.i) is set up .false. '// &
     &         'legally. 2) Test environment variable UNF_SEG. 3) If '// &
     &         'there are grounds for marking this session uneligible, '// &
     &         'Use mode FAST_MODE  NONE and/or switch off environment '// &
     &         'variable FAST_MODE__STRICTLY' )
           RETURN
      END IF
!
! --- Get names of the parameters -- array LPARM
!
      KSHORT  = .TRUE.
      KGLOBAL = .FALSE.
      KGLOBONLY = .FALSE.
      CALL GET_NAMES ( LPARM, ILPAR_LEN2, M_GPA, NPARAM2, KSHORT, KGLOBAL )
      IF ( NPARAM2 .NE. NPARAM ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NPARAM2, STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( NPARAM, STR1 )
           DO 400 J1=1,NPARAM2
              WRITE ( 6, 110 ) J1, LPARM(J1) 
 110          FORMAT ( 'MAP_PARAM: ', I5, ')  >>', A20, '<<  ' )
 400       CONTINUE 
           CALL ERR_LOG ( 8343, IUER, 'MAP_PARAM', 'Parametr NPARAM2 ('// &
     &          STR(1:I_LEN(STR))//') is not equal to NPARAM: ('// &
     &          STR1(1:I_LEN(STR1))//') . It is indication of some '// &
     &         'inconsistency PARCN and GET_NAMES.' )
           RETURN
      END IF
      IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! -------- Read part of GLB  -file where information about global parameters
! -------- is kept
!
           CALL USE_GLBFIL_2 ( 'ORC' )
!
! -------- Setting flag for further call of GERT_NAMES which will point out
! -------- on neccesity to build list of only ghlobal parameters
!
           CALL DEPAR()
!
! -------- Building a list of global parameters -- LPARM_GLO
!
           KGLOBONLY = .TRUE.
           CALL GET_NAMES ( LPARM_GLO, ILPAR_LEN2, M_GPA, NPARAM3, &
     &                      KSHORT, KGLOBAL )
!
! -------- Remove local source admittance parameters. In fact GET_NAMES should
! -------- be called with L2_TRUE last argument. I'm hesitatant to turn it on
! -------- now (2007.08.10), because I'm afrfraid of causing a new regression bug.
! -------- This should be tested later.
!
           NPARAM3_NEW = 0
           DO 510 K1=1,NPARAM3
              IF ( LPARM_GLO(K1)(1:11) .NE. 'LCL_SOU_ADM' ) THEN
                   NPARAM3_NEW = NPARAM3_NEW + 1
                   LPARM_GLO_NEW(NPARAM3_NEW) = LPARM_GLO(K1)
              END IF
 510       CONTINUE 
           NPARAM3 = NPARAM3_NEW
           IF ( NPARAM3 > 0 ) THEN
                CALL LIB$MOVC3 ( 20*NPARAM3, %REF(LPARM_GLO_NEW), &
     &                           %REF(LPARM_GLO) )
           END IF
!
           KGLOBONLY = .FALSE.
!
! -------- Read SOCOM once more since DEPAR destructed some data structures
!
           CALL USE_COMMON ( 'ORC' )
         ELSE
           NPARAM3 = 0
      END IF
!
! --- Zeroing B3DOBJ structures
!
      CALL NOUT_I4 ( M_GPA, B3DOBJ%PL  )
      CALL NOUT_I4 ( M_GPA, B3DOBJ%BLO )
!
! --- Moving in B3D the total number of parameters
!
      B3DOBJ%N_PAR = NPARAM2 
      K_GLO        = NPARAM3 ! number of global parameters for the
!                            ! case B1B3D only!
! --- Other initializations
!
      B3DOBJ%N_CLO = 0
      B3DOBJ%N_ATM = 0
      B3DOBJ%N_EOP = 0
      M_STA = INT4 ( MAX_STA )
      L_STA = INT4 ( NUMSTA  )
      L_CLO = 0
      L_ATM = 0
      L_EOP = 0
      IF ( UNF_EOP ) THEN
           L_EOP = 3
      END IF
!
! --- FIRST RUN
!     ~~~~~~~~~
!
! --- Scanning LPARM and filling the lists of stations which atmosphere and
! --- clocks are being estimated. Clock-lists and atmosphere lists contains
! --- indeces stations from the list of NAMES ISITN_CHR. Example: f.e.
! --- ISITN_CHR contains 4 names: WESTFORD, WETTZELL, CRIMEA, GILCREEK and
! --- L_CLO = 3, and LIS_CLO=1,3,4  That means that clock parameters for
! --- WESTFORD, CRIMEA, GILCREEK will be estimated, bur for the station
! --- WETTZELL will not.
!
      CLO_MAX =  0.0
      ATM_MAX =  0.0
      EOP_MAX =  0.0
      DO 410 J1=1,B3DOBJ%N_PAR
         IF ( ( LPARM(J1)(9:10) .EQ. 'c0'  .OR. &
     &          LPARM(J1)(9:10) .EQ. 'C0'       ) .AND. UNF_CLO ) THEN
!
! --------- Increment the total number of clock paramters (for all stations,
! --------- for all segments)
!
            B3DOBJ%N_CLO = B3DOBJ%N_CLO + 1
!
! --------- IL -- Index station for J1 parameter in the list ISITN_CHR
!
            IL = LTM_DIF ( 0, L_STA, ISITN_CHR, LPARM(J1)(1:8) )
            IF ( IL .LE. 0 ) THEN
                 CALL ERR_LOG ( 8344, IUER, 'MAP_PARAM', 'Internal error: '// &
     &               'array LPARM or array ISITN_CHR from prfil.i is '// &
     &               'corrupted' )
                 RETURN
            END IF
!
! --------- Adding this index to the clock-list
!
            CALL ADD_LIS ( M_STA, L_CLO, LIS_CLO, IL, -3 )
         END IF
!
         IF ( ( LPARM(J1)(9:10) .EQ. 'a0'  .OR. &
     &          LPARM(J1)(9:10) .EQ. 'A0'       ) .AND. UNF_ATM ) THEN
!
! --------- Increment the total number of atmosphere paramters (for all
! --------- stations, for all segments)
!
            B3DOBJ%N_ATM = B3DOBJ%N_ATM + 1
!
! --------- IL -- Index station for J1 parameter in the list ISITN_CHR
!
            IL = LTM_DIF ( 0, L_STA, ISITN_CHR, LPARM(J1)(1:8) )
            IF ( IL .LE. 0 ) THEN
                 CALL ERR_LOG ( 8345, IUER, 'MAP_PARAM', 'Internal error: '// &
     &               'array LPARM or array ISITN_CHR from prfil.i is '// &
     &               'corrupted' )
                 RETURN
            END IF
!
! --------- Adding this index to the atmposphere-list
!
            CALL ADD_LIS ( M_STA, L_ATM, LIS_ATM, IL, -3 )
         END IF
!
         IF ( ( LPARM(J1)(1:8) .EQ. 'X WOBBLE'  .OR. &
     &          LPARM(J1)(1:8) .EQ. 'Y WOBBLE'  .OR. &
     &          LPARM(J1)(1:8) .EQ. 'UT1-TAI '       ) .AND. UNF_EOP ) THEN
!
! ----------- Increment the total number of atmosphere paramters (for all
! ----------- stations, for all segments)
!
              B3DOBJ%N_EOP = B3DOBJ%N_EOP + 1
         END IF
 410  CONTINUE
!
! --- Calculation last epoch fro clock and atmosphere (in hours)
!
      IF ( UNF_CLO ) THEN
           CLO_MAX = ( JDATE_CLO(NUM_CLO) - JDATE_CLO(1) )
           CLO_MAX = DBLE ( INT ( CLO_MAX*3600.D0*100.D0 + 0.1D0 ) ) &
     &               /(3600.D0*100.D0)
      END IF
      IF ( UNF_ATM ) THEN
           ATM_MAX = ( JDATE_ATM(NUM_ATM) - JDATE_ATM(1) )
           ATM_MAX = DBLE ( INT ( ATM_MAX*3600.D0*100.D0 + 0.1D0 ) ) &
     &               /(3600.D0*100.D0)
      END IF
      IF ( UNF_EOP ) THEN
           EOP_MAX = ( JDATE_EOP(NUM_EOP) - JDATE_EOP(1) )
           EOP_MAX = DBLE ( INT ( EOP_MAX*3600.D0*100.D0 + 0.1D0 ) ) &
     &               /(3600.D0*100.D0)
      END IF
!
! --- Calculation B3DOBJ.NX_ATM -- the number of additional segments for
! ---                              atmosphere after the last clock segment
! ---             B3DOBJ.NX_CLO -- the number of additional segments for clock
! ---                              after the last atmosphere segment
! ---             B3DOBJ.NX_EOP -- the number of additional segments for clock
! ---                              after the last EOP segment
!
      B3DOBJ%NX_CLO = 0
      B3DOBJ%NX_ATM = 0
      B3DOBJ%NX_EOP = 0
      IF ( CLO_MAX .GT. ATM_MAX  .AND.  UNF_CLO  .AND.  UNF_ATM ) THEN
!
           B3DOBJ%NX_CLO = INT ( (CLO_MAX - ATM_MAX) /CLO_INTERVAL + 0.49 )
           IF ( ABS ((CLO_MAX - ATM_MAX)/CLO_INTERVAL - B3DOBJ%NX_CLO) .GT. &
     &          0.0001 ) THEN
                CALL ERR_LOG ( 8346, IUER, 'MAP_PARAM', 'Last epoch for '// &
     &              'atmospohere doesn''t coincide with last epoch for '// &
     &              'clock and lays off integer number of epoch interval. '// &
     &              'You may look at posthumous file '//FINAM_PARAM )
                GOTO 810
           END IF
      END IF
!
      IF ( ATM_MAX .GT. CLO_MAX  .AND.  UNF_CLO  .AND.  UNF_ATM ) THEN
           B3DOBJ%NX_ATM = INT ( (ATM_MAX - CLO_MAX) /ATM_INTERVAL + 0.49 )
           IF ( ABS ((ATM_MAX - CLO_MAX)/ATM_INTERVAL - B3DOBJ%NX_ATM) .GT. &
     &          0.0001 ) THEN
                CALL ERR_LOG ( 8347, IUER, 'MAP_PARAM', 'Last epoch for '// &
     &              'clock doesn''t coincide with last epoch for '// &
     &              'atmospohere and lays off integer number of epoch '// &
     &              'interval. You may look at posthumous file '//FINAM_PARAM )
                GOTO 810
           END IF
      END IF
!
      IF ( EOP_MAX .GT. CLO_MAX  .AND.  UNF_CLO  .AND.  UNF_EOP ) THEN
           B3DOBJ%NX_EOP = INT ( (EOP_MAX - CLO_MAX) /EOP_INTERVAL + 0.49 )
           IF ( ABS ((EOP_MAX - CLO_MAX)/EOP_INTERVAL - B3DOBJ%NX_EOP) .GT. &
     &          0.0001 ) THEN
                CALL ERR_LOG ( 8348, IUER, 'MAP_PARAM', 'Last epoch for '// &
     &              'clock doesn''t coincide with last epoch for '// &
     &              'EOP and lays off integer number of epoch '// &
     &              'interval. You may look at posthumous file '//FINAM_PARAM )
                GOTO 810
           END IF
      END IF
!
! --- Calculation total number of segmented paramters
!
      B3DOBJ%N_SGM = B3DOBJ%N_CLO + B3DOBJ%N_ATM + B3DOBJ%N_EOP
!
! --- Calculation number of local parameters (not local, not segmented)
!
      IF ( FAST_MODE .EQ. F__B3D ) THEN
           K_LOC = 0
        ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
           K_LOC = B3DOBJ%N_PAR - B3DOBJ%N_SGM - K_GLO
      END IF
!
! --- Calculcation NP_C -- the number of segments for clocks
!                  NP_A -- the number of segments for atmosphere and
!                  NP_E -- the number of segments for EOP
!
      NP_C = 0
      NP_A = 0
      NP_E = 0
      IF ( UNF_CLO .AND. L_CLO > 0 ) NP_C = (B3DOBJ%N_CLO - B3DOBJ%NX_CLO)/L_CLO
      IF ( UNF_ATM .AND. L_ATM > 0 ) NP_A = (B3DOBJ%N_ATM - B3DOBJ%NX_ATM)/L_ATM
      IF ( UNF_EOP .AND. L_EOP > 0 ) NP_E = (B3DOBJ%N_EOP - B3DOBJ%NX_EOP)/L_EOP
!
      IF ( UNF_CLO .AND. MOD(B3DOBJ%N_CLO,  MAX(L_CLO,1)) .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( L_STA, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( B3DOBJ%N_CLO, STR1 )
!
           CALL ERR_LOG ( 8349, IUER, 'MAP_PARAM', 'The number of stations ('// &
     &          STR(1:I_LEN(STR))//' where clock function was estimated is '// &
     &         'not a multiple of the total number of clock parameters ('// &
     &          STR1(1:I_LEN(STR1))//'). You may look at posthumous file '// &
     &          FINAM_PARAM )
           GOTO 810
      END IF
!
! --- Two tests for validity obtained parameters
!
      IF ( UNF_ATM .AND. MOD(B3DOBJ%N_ATM, MAX(L_ATM,1) ) .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( L_ATM, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( B3DOBJ%N_ATM, STR1 )
!
           CALL ERR_LOG ( 8350, IUER, 'MAP_PARAM', 'The number of '// &
     &         'stations ('//STR(1:I_LEN(STR))//') where atmosphere was '// &
     &         'estimated is not a multiple of the total number of '// &
     &         'atmospheric parameters ('//STR1(1:I_LEN(STR1))//'). '// &
     &         'You may look at posthumous file '//FINAM_PARAM )
           GOTO 810
      END IF
!
! --- Calculations B3DOBJ.NBS   -- the number of blocks of segmented param.,
! ---------------- B3DOBJ.K_ATM -- multiplicity for atmosphere
! ---------------- B3DOBJ.K_CLO -- multiplicity for clocks
! ---------------- B3DOBJ.K_EOP -- multiplicity for EOP
! ---------------- B3DOBJ.SB    -- the nubmer of segmented parameters at one
! ----------------                 block except the last
! ---------------- B3DOBJ.SX    -- the nubmer of segmented parameters at the
!                                  last block
!
      NP_MAX = MAX ( NP_C, NP_A, NP_E )
      IF ( NP_MAX .EQ. NP_C ) MAX_OBJECT = 'CLOCK'
      IF ( NP_MAX .EQ. NP_A ) MAX_OBJECT = 'ATMOS'
      IF ( NP_MAX .EQ. NP_E ) MAX_OBJECT = 'EOP  '
!
      NP_MIN = 2*NP_MAX
      IF ( NP_C .GT. 0  .AND.  NP_C .LT. NP_MIN ) THEN
           NP_MIN = NP_C
           MIN_OBJECT = 'CLOCK'
      END IF
      IF ( NP_A .GT. 0  .AND.  NP_A .LT. NP_MIN ) THEN
           NP_MIN = NP_A
           MIN_OBJECT = 'ATMOS'
      END IF
      IF ( NP_E .GT. 0  .AND.  NP_E .LT. NP_MIN ) THEN
           NP_MIN = NP_E
           MIN_OBJECT = 'EOP  '
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,' np_c = ',np_c,' np_a=',np_a,' np_e =',np_e   ! %%%%%%%%%
!        type *,' np_min = ',np_min,' np_max =',np_max         ! %%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- Calculation multiplicity
!
      B3DOBJ%K_CLO = (NP_C -1 ) / ( NP_MIN - 1)
      B3DOBJ%K_ATM = (NP_A -1 ) / ( NP_MIN - 1)
      B3DOBJ%K_EOP = (NP_E -1 ) / ( NP_MIN - 1)
!
! --- B3DOBJ.NBS  -- nmber of common segmented
!
      B3DOBJ%NBS   =  NP_MIN
!
! --- B3DOBJ.SB  -- total number of parameters in one segment
! --- (except the last one)
!
      B3DOBJ%SB    = L_CLO* B3DOBJ%K_CLO + L_ATM* B3DOBJ%K_ATM + &
     &               L_EOP* B3DOBJ%K_EOP
!
! --- B3DOBJ.SX  -- total number of parameters in last segment
!
      B3DOBJ%SX    = L_CLO* (B3DOBJ%NX_CLO+1) + L_ATM* (B3DOBJ%NX_ATM+1) + &
     &               L_EOP* (B3DOBJ%NX_EOP+1)
!
      IF ( (UNF_CLO .AND. B3DOBJ%K_CLO *(NP_MIN-1) .NE. (NP_C-1) .AND.        &
     &      NP_C > 0                                                   ) .OR. &
     &     (UNF_ATM .AND. B3DOBJ%K_ATM *(NP_MIN-1) .NE. (NP_A-1) .AND.        &
     &      NP_A > 0                                                   ) .OR. &
     &     (UNF_EOP .AND. B3DOBJ%K_EOP *(NP_MIN-1) .NE. (NP_E-1) .AND.        &
     &      NP_E > 0                                                   )      ) THEN
!
           CALL CLRCH ( STR  )
           CALL INCH  ( NP_C -1,   STR   )
           CALL CLRCH ( STR1 )
           CALL INCH  ( NP_A -1,   STR1  )
           CALL CLRCH ( STR2 )
           CALL INCH  ( NP_E -1,   STR2  )
           WRITE ( 6, * ) 'UNF_CLO= ', UNF_CLO, ' UNF_ATM= ', UNF_ATM, ' UNF_EOP= ', UNF_EOP
           CALL ERR_LOG ( 8351, IUER, 'MAP_PARAM', 'The number of '// &
     &         'segments for clocks ('//STR(1:I_LEN(STR))//') '// &
     &         'for atmosphere ('//STR1(1:I_LEN(STR1))//') or '// &
     &         'for EOP ('//STR2(1:I_LEN(STR2))//')  '// &
     &         'is not a multiple each other. This makes '// &
     &         'impossible implementing B3D algorithm. You may look '// &
     &         'at posthumous file '//FINAM_PARAM(1:I_LEN(FINAM_PARAM))// &
     &         ' for some information. Hint: either change the span '// &
     &         'of segments for atmosphere/clock/EOP or disable '// &
     &         'B3D-mode...  ;-(' )
           GOTO 810
      END IF
!
      IF ( B3DOBJ%SX .GT. B3DOBJ%SB ) THEN
           CALL ERR_LOG ( 8352, IUER, 'MAP_PARAM', 'Internal error: '// &
     &         'B3DOBJ%SX > B3DOBJ%SB . This implementing B3D '// &
     &         'algorithm doesn''t support this case. You may look at '// &
     &         'posthumous file '//FINAM_PARAM )
           GOTO 810
      END IF
!
      IF ( B3DOBJ%NBS .GT. MAX_SEG ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( B3DOBJ%NBS, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( MAX_SEG, STR1 )
           CALL ERR_LOG ( 8353, IUER, 'MAP_PARAM', 'The number of '// &
     &         'blocks is too large: '//STR(1:I_LEN(STR))//'. Parmeter '// &
     &         'MAX_SEG from fast.i appeared to be insufficient: MAX_SEG = '// &
     &          STR1 )
           STOP 'Abnormal termination'
      END IF
!
! --- SECOND RUN
!
! --- Some initialization
!
      ISEG_CLO =  0
      ISEG_ATM =  0
      ISEG_EOP =  0
      CLO_LAST = 1.D20
      ATM_LAST = 1.D20
      EOP_LAST = 1.D20
      B3DOBJ%N_GLO = 0
      B3DOBJ%N_LOC = 0
!
! --- The objective of the seond run is to fill fields .PL, .BLO, .CURR and
! --- .NEXT which reflect correspondence between elements in full matrix and
! --- the set of submatrix for of B3D parametrization. The fields .INFxxx for
! --- inverse transformation are also generated
!
      DO 420 J2=1,B3DOBJ%N_PAR
         B3DOBJ%CURR(J2) = .TRUE. ! default condition of staying
         B3DOBJ%NEXT(J2) = .TRUE. ! default condition of transition
         IF ( ( LPARM(J2)(9:10) .EQ. 'c0'  .OR. &
     &          LPARM(J2)(9:10) .EQ. 'C0'       ) .AND. UNF_CLO ) THEN
!
! --------- CLOCK
!           ~~~~~
!
! --------- Extracting the time of the beginning if the clock segment
!
            CALL DFOR_MEN ( LPARM(J2)(11:20), CLO_TIME, IER )
!
! --------- Calculcation the index of the clock segment for the J2-th
! --------- parameter
!
            IF ( CLO_TIME .GT. CLO_LAST + 1.0D0 ) THEN
                 ISEG_CLO = ISEG_CLO + 1
              ELSE IF ( CLO_TIME .LT. CLO_LAST - 1.0D0 ) THEN
                 ISEG_CLO = 1
            END IF
            CLO_LAST = CLO_TIME
!
! --------- Find the place of the station in the list ISITN_CHR
!
            IL = LTM_DIF ( 0,  L_STA, ISITN_CHR, LPARM(J2)(1:8) )
            IF ( IL .LE. 0 ) THEN
                 CALL ERR_LOG ( 8354, IUER, 'MAP_PARAM', 'Internal error: '// &
     &               'array LPARM or array ISITN_CHR from prfil.i is '// &
     &               'corrupted' )
                 RETURN
            END IF
!
! --------- Find the place of the station in the list LIS_CLO
!
            IND_CLO = IFIND_PL ( L_CLO, LIS_CLO, IL )
            IF ( IND_CLO .LE. 0 ) THEN
                 CALL ERR_LOG ( 8355, IUER, 'MAP_PARAM', 'Internal error: '// &
     &               'array LIS_CLO is corrupted' )
                 RETURN
            END IF
!
! --------- Filling .PL and .BLO for the case of ...
!
            IF ( B3DOBJ%K_CLO .EQ. 1 ) THEN
!
! -------------- ... with unit clock multiplicity
!
                 B3DOBJ%PL(J2) = IND_CLO
                 B3DOBJ%BLO(J2)= ISEG_CLO
              ELSE
!
! -------------- Calculate number of block in tied B3D structure for J2-th
! -------------- clock parameter
!
                 B3DOBJ%BLO(J2)= (ISEG_CLO - 1)/B3DOBJ%K_CLO + 1
!
! -------------- Calculate palce of the parameter in current block
!
                 B3DOBJ%PL(J2) = ( ISEG_CLO - 1 - (B3DOBJ%BLO(J2) -1)* &
     &                             B3DOBJ%K_CLO )*L_CLO + IND_CLO
!
! -------------- Set up condition of transition to the next segment
!
                 IF ( MOD (ISEG_CLO,B3DOBJ%K_CLO) .NE. 1) &
     &                B3DOBJ%NEXT(J2) = .FALSE.
                 IF ( ISEG_CLO .GT. (B3DOBJ%NBS-1)*B3DOBJ%K_CLO ) THEN
                      B3DOBJ%CURR(J2) = .FALSE.
                      B3DOBJ%NEXT(J2) = .TRUE.
                 END IF
            END IF
!
            IF ( MOD( (ISEG_CLO - 1),B3DOBJ%K_CLO ) .EQ. 0 ) THEN
!
! -------------- Test: does exist atmospohere epoch which corresponds the clock
! -------------- epoch under consideration
!
                 ATM_EPOCH = .FALSE.
                 DO 430 J3=1,B3DOBJ%N_PAR
                    IF ( LPARM(J3)(9:10) .EQ. 'A0' .OR. &
     &                   LPARM(J3)(9:10) .EQ. 'a0'      ) THEN
                         IF ( LPARM(J2)(11:20) .EQ. &
     &                        LPARM(J3)(11:20)      ) ATM_EPOCH = .TRUE.
                    END IF
  430            CONTINUE
                 IF ( CLO_TIME .GT. ATM_MAX ) ATM_EPOCH = .TRUE.
!!!                             atm_epoch = .true.  !@@
                 IF ( .NOT. ATM_EPOCH ) THEN
                      CALL ERR_LOG ( 8356, IUER, 'MAP_PARAM', 'For clock '// &
     &                    'epoch '//LPARM(J2)(11:20)//' corresponding '// &
     &                    'epoch for atmosphere parameter was not found. '// &
     &                    'B3D algorithm cannot work porperly in such case.'// &
     &                    ' You may look at posthumous file '//FINAM_PARAM )
                      GOTO 810
                 END IF
            END IF
           ELSE IF ( ( LPARM(J2)(9:10) .EQ. 'a0' .OR. &
     &                 LPARM(J2)(9:10) .EQ. 'A0'      ) .AND. UNF_ATM ) THEN
!
! --------- ATMOSPHERE
!           ~~~~~~~~~~
!
! --------- Extracting the time of the beginning if the clock segment
!
            CALL DFOR_MEN ( LPARM(J2)(11:20), ATM_TIME, IER )
            IF ( ATM_TIME .GT. ATM_LAST + 1.0D0 ) THEN
                 ISEG_ATM = ISEG_ATM + 1
              ELSE IF ( ATM_TIME .LT. ATM_LAST - 1.0D0 ) THEN
                 ISEG_ATM = 1
            END IF
            ATM_LAST = ATM_TIME
!
! --------- Find the place of the station in the list ISITN_CHR
!
            IL = LTM_DIF ( 0,  L_STA, ISITN_CHR, LPARM(J2)(1:8) )
            IF ( IL .LE. 0 ) THEN
                 CALL ERR_LOG ( 8357, IUER, 'MAP_PARAM', 'Internal error: '// &
     &               'array LPARM or array ISITN_CHR from prfil.i is '// &
     &               'corrupted' )
                 RETURN
            END IF
!
! --------- And now find the place of the station in the list LIS_ATM
!
            IND_ATM = IFIND_PL ( L_ATM, LIS_ATM, IL )
            IF ( IND_ATM .LE. 0 ) THEN
                 CALL ERR_LOG ( 8358, IUER, 'MAP_PARAM', 'Internal error: '// &
     &               'array LPARM or array ISITN_CHR from prfil.i is '// &
     &               'corrupted' )
                 RETURN
            END IF
!
! --------- Calculation of the total numbers of clock parameters at this
! --------- segment (they stay before atmospheric parameters)
!
            IF (  ISEG_ATM .LE. (B3DOBJ%NBS-1)*B3DOBJ%K_ATM ) THEN
                  PRV_SGM = L_CLO*B3DOBJ%K_CLO
                ELSE
!
! --------------- It is the last common segment with incomplete
! --------------- number of parameters
!
                  PRV_SGM = L_CLO*(B3DOBJ%NX_CLO+1)
            END IF
!
            IF ( B3DOBJ%K_ATM .EQ. 1 ) THEN
                 B3DOBJ%BLO(J2)= ISEG_ATM
                 B3DOBJ%PL(J2) = PRV_SGM + IND_ATM
            END IF
            IF ( B3DOBJ%K_ATM .GT. 1 ) THEN
!
! -------------- Calculate a number of blocks in tied B3D structure for this
! -------------- parameter
!
                 B3DOBJ%BLO(J2)= (ISEG_ATM - 1)/B3DOBJ%K_ATM + 1
!
! -------------- Calculate palce of the parameter in BLOCK
!
                 B3DOBJ%PL(J2) = PRV_SGM + ( (ISEG_ATM - 1) - &
     &               (B3DOBJ%BLO(J2) -1)*B3DOBJ%K_ATM )*L_ATM + IND_ATM
!
! -------------- Set up condition of transition to the next segment
!
                 IF ( MOD (ISEG_ATM,B3DOBJ%K_ATM) .NE. 1) &
     &                B3DOBJ%NEXT(J2) = .FALSE. ! Prohibit transition to next block
                 IF ( ISEG_ATM .GT. (B3DOBJ%NBS-1)*B3DOBJ%K_ATM ) THEN
                      B3DOBJ%CURR(J2) = .FALSE.
                      B3DOBJ%NEXT(J2) = .TRUE.
                 END IF
            END IF
!
            IF ( MOD( (ISEG_ATM - 1),B3DOBJ%K_ATM ) .EQ. 0 ) THEN
!
! -------------- Test: does exist clock epoch which corresponds the atmospohere
! -------------- epoch under consideration
!
                 CLK_EPOCH = .FALSE.
                 DO 440 J4=1,B3DOBJ%N_PAR
                    IF ( LPARM(J4)(9:10) .EQ. 'C0' .OR. &
     &                   LPARM(J4)(9:10) .NE. 'c0'      ) THEN
                         IF ( LPARM(J2)(11:20) .EQ. &
     &                        LPARM(J4)(11:20)      ) CLK_EPOCH = .TRUE.
                    END IF
 440             CONTINUE
                 IF ( ATM_TIME .GT. CLO_MAX ) CLK_EPOCH = .TRUE.
                 IF ( .NOT. CLK_EPOCH ) THEN
                      CALL ERR_LOG ( 8359, IUER, 'MAP_PARAM', 'For '// &
     &                    'atmosphere epoch '//LPARM(J2)(11:20)// &
     &                    ' corresponding epoch for clock parameters was '// &
     &                    'not found. B3D algorithm cannot work porperly '// &
     &                    'in such case. You may look at posthumous '// &
     &                    'file '//FINAM_PARAM )
                      GOTO 810
                 END IF
            END IF
!
           ELSE IF ( ( LPARM(J2)(1:8) .EQ. 'X WOBBLE' .OR. &
     &                 LPARM(J2)(1:8) .EQ. 'Y WOBBLE' .OR. &
     &                 LPARM(J2)(1:8) .EQ. 'UT1-TAI '      ) .AND. UNF_EOP ) THEN
!
! --------- Extracting the time of the beginning if the clock segment
!
            CALL DFOR_MEN ( LPARM(J2)(11:20), EOP_TIME, IER )
            IF ( EOP_TIME .GT. EOP_LAST + 1.0D0 ) THEN
                 ISEG_EOP = ISEG_EOP + 1
              ELSE IF ( EOP_TIME .LT. EOP_LAST - 1.0D0 ) THEN
                 ISEG_EOP = 1
            END IF
            EOP_LAST = EOP_TIME
!
            IF ( LPARM(J2)(1:8) .EQ. 'X WOBBLE' ) IND_EOP = 1
            IF ( LPARM(J2)(1:8) .EQ. 'Y WOBBLE' ) IND_EOP = 2
            IF ( LPARM(J2)(1:8) .EQ. 'UT1-TAI ' ) IND_EOP = 3
!
! --------- Calculation of the total numbers of clock atmospheric parameters
! --------- at this segment (they stay before EOP parameters)
!
            IF (  ISEG_EOP .LE. (B3DOBJ%NBS-1)*B3DOBJ%K_EOP ) THEN
                  PRV_SGM = L_CLO*B3DOBJ%K_CLO + L_ATM*B3DOBJ%K_ATM
                ELSE
!
! --------------- It is the last common segment with incomplete
! --------------- number of parameters
!
                  PRV_SGM = L_CLO*(B3DOBJ%NX_CLO+1) + L_ATM*(B3DOBJ%NX_ATM+1)
            END IF
!
            IF ( B3DOBJ%K_EOP .EQ. 1 ) THEN
                 B3DOBJ%BLO(J2)= ISEG_EOP
                 B3DOBJ%PL(J2) = PRV_SGM + IND_EOP
              ELSE IF ( B3DOBJ%K_EOP .GT. 1 ) THEN
!
! -------------- Calculate a number of blocks in tied B3D structure for this
! -------------- parameter
!
                 B3DOBJ%BLO(J2)= (ISEG_EOP - 1)/B3DOBJ%K_EOP + 1
!
! -------------- Calculate palce of the parameter in BLOCK
!
                 B3DOBJ%PL(J2) = PRV_SGM + ( (ISEG_EOP - 1) - &
     &               (B3DOBJ%BLO(J2) -1)*B3DOBJ%K_EOP )*L_EOP + IND_EOP
!
! -------------- Set up condition of transition to the next segment
!
                 IF ( MOD (ISEG_EOP,B3DOBJ%K_EOP) .NE. 1) &
     &                B3DOBJ%NEXT(J2) = .FALSE. ! Prohibit transition to next block
                 IF ( ISEG_EOP .GT. (B3DOBJ%NBS-1)*B3DOBJ%K_EOP ) THEN
                      B3DOBJ%CURR(J2) = .FALSE.
                      B3DOBJ%NEXT(J2) = .TRUE.
                 END IF
            END IF
!
            IF ( MOD( (ISEG_EOP - 1),B3DOBJ%K_EOP ) .EQ. 0 ) THEN
!
! -------------- Test: does exist clock epoch which corresponds the EOP
! -------------- epoch under consideration
!
                 CLK_EPOCH = .FALSE.
                 DO 450 J5=1,B3DOBJ%N_PAR
                    IF ( LPARM(J5)(9:10) .EQ.  'C0' .OR. &
     &                   LPARM(J5)(9:10) .NE.  'c0'      ) THEN
                         IF ( LPARM(J2)(11:20) .EQ. &
     &                        LPARM(J5)(11:20)      ) CLK_EPOCH = .TRUE.
                    END IF
 450             CONTINUE
                 IF ( EOP_TIME .GT. CLO_MAX ) CLK_EPOCH = .TRUE.
                 IF ( .NOT. CLK_EPOCH ) THEN
                      CALL ERR_LOG ( 8360, IUER, 'MAP_PARAM', 'For '// &
     &                    'EOP epoch '//LPARM(J2)(11:20)// &
     &                    ' corresponding epoch for clock parameters was '// &
     &                    'not found. B3D algorithm cannot work porperly '// &
     &                    'in such case. You may look at posthumous '// &
     &                    'file '//FINAM_PARAM )
                      GOTO 810
                 END IF
            END IF
           ELSE  ! not segmented parametr
            IF ( K_LOC .NE. 0 ) THEN
!
! -------------- If local parameters may occur we test is this parameter in
! -------------- a special list for global parameters
!
                 DO 460 J6=1,K_GLO
                    IF ( LPARM(J2) .EQ. LPARM_GLO(J6) ) THEN
!
! ---------------------- Oh, yes -- then we treat it as a  global parameter
!
                         B3DOBJ%N_GLO  = B3DOBJ%N_GLO + 1
                         B3DOBJ%PL(J2) = B3DOBJ%N_GLO
                         GOTO 860  !  exit from cycle
                    END IF
 460             CONTINUE
!
! -------------- Since parameter was not found in the list of global parameters
! -------------- then it should be treated as a local one
!
                 B3DOBJ%N_LOC  = B3DOBJ%N_LOC + 1
                 B3DOBJ%PL(J2) = B3DOBJ%N_LOC
                 B3DOBJ%BLO(J2)= -1
              ELSE
!
! -------------- In the case when there are no local parameters we treat all
! -------------- non-segmeneted parameters as global
!
                 B3DOBJ%N_GLO  = B3DOBJ%N_GLO + 1
                 B3DOBJ%PL(J2) = B3DOBJ%N_GLO
            END IF
 860        CONTINUE
         END IF
!
         IF ( B3DOBJ%BLO(J2) .GT. 0  .AND.  B3DOBJ%PL(J2) .GT. MAX_PSG ) THEN
              CALL CLRCH ( STR          )
              CALL INCH  ( MAX_PSG, STR )
              WRITE ( 6, * ) ' J2 = ', J2, ' B3DOBJ%BLO(J2) = ', B3DOBJ%BLO(J2) 
              CALL ERR_LOG ( 8361, IUER, 'MAP_PARAM', 'The number of '// &
     &             'segmented parameters in one segment is too large. '// &
     &             'Parmeter MAX_PSG from fast.i appeared to be '// &
     &             'insufficient: MAX_PSG = '//STR )
              RETURN
         END IF
!
! ------ Now calculation .INF_GLO, INF_SGM, INF_SGX fields. They faciliated
! ------ inverse operataion: knowing the place of the element in B3D set
! ------ of submatrices to find it place in full matrix
!
         IPL = B3DOBJ%PL(J2)   !  Temporary variables since some compilers
         IBL = B3DOBJ%BLO(J2)  !  don't understand construction like
!                              !  "B3DOBJ.INF_GLO ( B3DOBJ.PL(J2) )"
         IF ( B3DOBJ%BLO(J2) .EQ. -1        ) THEN
              B3DOBJ%INF_LOC ( IPL ) = J2
            ELSE IF ( B3DOBJ%BLO(J2) .EQ. 0 ) THEN
              B3DOBJ%INF_GLO ( IPL ) = J2
            ELSE IF ( B3DOBJ%BLO(J2) .GT. 0 ) THEN
              B3DOBJ%INF_SGM ( IPL, IBL ) = J2
         END IF
         IF ( B3DOBJ%BLO(J2) .EQ. B3DOBJ%NBS  ) THEN
              B3DOBJ%INF_SGX ( IPL ) = J2
         END IF
 420  CONTINUE
!
      DO 470 J7=1,NP_MIN
         ICLO = 1 + B3DOBJ%K_CLO*(J7-1)
         IATM = 1 + B3DOBJ%K_ATM*(J7-1)
         IEOP = 1 + B3DOBJ%K_EOP*(J7-1)
         IF ( UNF_CLO  .AND.  UNF_ATM  .AND. NP_C > 0 .AND. NP_A > 0 ) THEN
              IF ( ABS(JDATE_CLO(ICLO) - JDATE_ATM(IATM))*86400.0D0 .GT. EPS_SEC ) THEN
!
                   CALL CLRCH ( STR        )
                   CALL INCH  ( ICLO, STR  )
                   CALL CLRCH ( STR1       )
                   CALL INCH  ( IATM, STR1 )
                   CALL CLRCH ( STR2       )
                   WRITE ( UNIT=STR2, FMT='(F20.10)' ) JDATE_CLO(ICLO)
                   CALL CHASHL( STR2 )
                   CALL CLRCH ( STR3       )
                   WRITE ( UNIT=STR3, FMT='(F20.10)' ) JDATE_ATM(IATM)
                   CALL CHASHL( STR3 )
!
                   CALL CLRCH   ( STR4 )
                   WRITE ( UNIT = STR4, FMT='(F20.2)') &
     &                     ABS( JDATE_CLO(ICLO)- JDATE_ATM(IATM) )*86400.D0
                   CALL CHASHL( STR4 )
!
                   CALL ERR_LOG ( 8362, IUER, 'MAP_PARAM', 'The moment '// &
     &                 'for clock''s '//STR(1:I_LEN(STR))//'-th epoch: '// &
     &                  STR2(1:I_LEN(STR2))//' differs more than it is '// &
     &                 'acceptable from the moment for the atmosphere''s '// &
     &                  STR1(1:I_LEN(STR1))//'-th epoch: '// &
     &                  STR3(1:I_LEN(STR3))//' . The difference is '// &
     &                  STR4(1:I_LEN(STR4))//' sec. Such a session cannot '// &
     &                 'be analysed in B3D mode. Turn FAST_MODE on the mode NONE' )
                   RETURN
              END IF
         END IF
!C
         IF ( UNF_CLO .AND. UNF_EOP ) THEN
            IF ( ABS(JDATE_CLO(ICLO) - JDATE_EOP(IEOP))*24.0D0*3600.0D0 .GT. &
     &           EPS_SEC  ) THEN
!
              CALL CLRCH ( STR        )
              CALL INCH  ( ICLO, STR  )
              CALL CLRCH ( STR1       )
              CALL INCH  ( IATM, STR1 )
              CALL CLRCH ( STR2       )
              WRITE ( UNIT=STR2, FMT='(F20.10)' ) JDATE_CLO(ICLO)
              CALL CHASHL( STR2 )
              CALL CLRCH ( STR3       )
              WRITE ( UNIT=STR3, FMT='(F20.10)' ) JDATE_EOP(IEOP)
              CALL CHASHL( STR3 )
!
              CALL CLRCH   ( STR4 )
              WRITE ( UNIT = STR4, FMT='(F20.2)') &
     &                ABS(JDATE_CLO(ICLO) - JDATE_EOP(IEOP))*86400.D0
              CALL CHASHL( STR4 )
!
              CALL ERR_LOG ( 8363, IUER, 'MAP_PARAM', 'The moment '// &
     &            'for clock''s '//STR(1:I_LEN(STR))//'-th epoch: '// &
     &             STR2(1:I_LEN(STR2))//' differs more than it is '// &
     &            'acceptable from the moment for the EOP '// &
     &             STR1(1:I_LEN(STR1))//'-th epoch: '// &
     &             STR3(1:I_LEN(STR3))//' . The difference is '// &
     &             STR4(1:I_LEN(STR4))//' sec. Such a session cannot '// &
     &            'be analysed in B3D mode. Turn FAST_MODE on the mode NONE' )
              RETURN
            END IF
          END IF
!C
         IF ( UNF_ATM .AND. UNF_EOP ) THEN
            IF ( ABS(JDATE_ATM(IATM) - JDATE_EOP(IEOP))*24.0D0*3600.0D0 .GT. &
     &           EPS_SEC  ) THEN
!
              CALL CLRCH ( STR        )
              CALL INCH  ( ICLO, STR  )
              CALL CLRCH ( STR1       )
              CALL INCH  ( IATM, STR1 )
              CALL CLRCH ( STR2       )
              WRITE ( UNIT=STR2, FMT='(F20.10)' ) JDATE_ATM(IATM)
              CALL CHASHL( STR2 )
              CALL CLRCH ( STR3       )
              WRITE ( UNIT=STR3, FMT='(F20.10)' ) JDATE_EOP(IEOP)
              CALL CHASHL( STR3 )
!
              CALL CLRCH   ( STR4 )
              WRITE ( UNIT = STR4, FMT='(F20.2)') &
     &                ABS(JDATE_ATM(IATM) - JDATE_EOP(IEOP))*86400.D0
              CALL CHASHL( STR4 )
!
              CALL ERR_LOG ( 8364, IUER, 'MAP_PARAM', 'The moment '// &
     &            'for atmosphere '//STR(1:I_LEN(STR))//'-th epoch: '// &
     &             STR2(1:I_LEN(STR2))//' differs more than it is '// &
     &            'acceptable from the moment for the EOP '// &
     &             STR1(1:I_LEN(STR1))//'-th epoch: '// &
     &             STR3(1:I_LEN(STR3))//' . The difference is '// &
     &             STR4(1:I_LEN(STR4))//' sec. Such a session cannot '// &
     &            'be analysed in B3D mode. Turn FAST_MODE on the mode NONE' )
              RETURN
            END IF
          END IF
 470  CONTINUE
!
! --- Internal control
!
      IF ( K_LOC .NE. B3DOBJ%N_LOC ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( B3DOBJ%N_LOC, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( K_LOC, STR1 )
           CALL ERR_LOG ( 8365, IUER, 'MAP_PARAM', 'Trap of internal '// &
     &         'control: B3DOBJ%N_LOC = '//STR(1:I_LEN(STR))//'  K_LOC = '// &
     &          STR1(1:I_LEN(STR1))//' -- they are not coincide, but should!'// &
     &         ' Send bug report to pet@leo.gsfc.nasa.gov' )
           GOTO 810
      END IF
!
      IF ( K_GLO .GT. 0  .AND.  K_GLO .NE. B3DOBJ%N_GLO ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( B3DOBJ%N_GLO, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( K_GLO, STR1 )
           CALL ERR_LOG ( 8366, IUER, 'MAP_PARAM', 'Trap of internal '// &
     &         'control: B3DOBJ%N_GLO = '//STR(1:I_LEN(STR))//'  K_GLO = '// &
     &          STR1(1:I_LEN(STR1))//' -- they are not coincide, but should!'// &
     &         ' Send bug report to pet@leo.gsfc.nasa.gov' )
           GOTO 810
      END IF
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           CALL ERR_PASS  ( IUER, IER  )
           CALL DBG_PARAM ( B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8991, IUER, 'MAP_PARAM', 'Error in DBG_PARAM' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
 810  CONTINUE
!
! --- This section generate posthumulous file and write down some
! --- infromation for help to find what has happened
!
      CALL DBG_PARAM ( B3DOBJ, -3 )
!
      RETURN
      END  !#!  MAP_PARAM  #!#
