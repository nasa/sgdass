      SUBROUTINE UPTDB_PTOC ( GVH, SEG_TH1, SEG_SL1, N_STA, N_SOU, &
     &                        N_BAS, NUM_CLRF, NUM_CLBR, L_ACM, L_CAL, &
     &                        IUER )
! ************************************************************************
! *                                                                      *
! *   Routine UPTDB_PTOC 
! *                                                                      *
! *  ### 05-DEC-2005   UPTDB_PTOC  v1.4 (c)  L. Petrov  21-NOV-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vtd.i'
      TYPE     ( GVH__STRU ) :: GVH
      TYPE     ( CAL__INFO__TYPE ) :: CAL_INFO(M__CAL)
      INTEGER*4    VTD__NDER_USED
      PARAMETER  ( VTD__NDER_USED = 17 )
      INTEGER*4  SEG_TH1, SEG_SL1, N_STA, N_SOU, N_BAS, NUM_CLRF, &
     &           NUM_CLBR, L_ACM, L_CAL, IUER
      CHARACTER  STR*128
      INTEGER*4  IER
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! === Second band status
!
      IF ( SEG_SL1 .LE. 0 ) GOTO 810 ! skip setting ptoc for SL1 segment
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'BAND_2ND', GVH__I2, GVH__BAS, 1, 1, &
     &    'Bit field with status of information about the second band '// &
     &    'observations', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6511, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
! === Effective ionospheric frequencies
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EFF_FREQ', GVH__R8, GVH__BAS, 3, 2, &
     &    'Effective ionospheric frequencies for gr.del, ph.del, '// &
     &    'ph.rate per band (Hz)', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6512, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
! === Clock reference stations
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_CLRF', GVH__I4, GVH__SES, 1, 1, &
     &     'Number of clock reference stations', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6513, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'STA_CLRF', GVH__C1, GVH__SES, 8, &
     &     NUM_CLRF, 'Names of clock reference stations', &
     &     SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' NUM_CLRF = ', NUM_CLRF
           CALL ERR_LOG ( 6514, IUER, 'UPDATE_PTOC', 'Failure to put Lcode '// & 
     &          'STA_CLRF in the table of conents' )
           RETURN 
      END IF
!
      IF ( L_ACM > 0 ) THEN
!
! ======== Information about apriori clock model
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'STAT_ACM', GVH__C1, GVH__SES, 8, M_ACM, &
     &         'Station names with a priori clock model', SEG_SL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6515, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &              'internal control' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'CLODRACM', GVH__R8, GVH__SES, M_ACM, 1, &
     &         'A priori clock offset rate for stations from '// &
     &         'STAT_ACM (d/l)', SEG_SL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6516, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &              'internal control' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'CLOOFACM', GVH__R8, GVH__SES, M_ACM, 1, &
     &         'A priori clock offset for stations from STAT_ACM (sec)', &
     &          SEG_SL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6517, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &              'internal control' )
                RETURN 
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUM_CLBR', GVH__I4, GVH__SES, 1, 1, &
     &    'Number of clock breaks in the experiment', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6518, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      IF ( NUM_CLBR > 0 ) THEN
!
! ======== Information about clock breaks
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'STA_CLBR', GVH__C1, GVH__SES, 8, &
     &          NUM_CLBR, 'Names of stations with clock breaks', &
     &          SEG_SL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6519, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &              'internal control' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'MJD_CLBR', GVH__I4, GVH__SES, NUM_CLBR, &
     &          1, 'Modified Julian date of clock break epochs', &
     &          SEG_SL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6520, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &              'internal control' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'UTC_CLBR', GVH__R8, GVH__SES, NUM_CLBR, &
     &          1, 'UTC time tag of clock break epochs', SEG_SL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6521, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &              'internal control' )
                RETURN 
           END IF
      END IF
!
! === Group and phase delay ambiguities
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'N_GRAMB ', GVH__I4, GVH__BAS, 2, 1, &
     &    'Number of group delay ambiguities to be added to measured '// &
     &    'group delays per band', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6522, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'N_PHAMB ', GVH__I4, GVH__BAS, 2, 1, &
     &    'Number of phase delay ambiguities to be added to measured '// &
     &    'phase delays per band', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6523, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
! === Editig status
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EDIT_STS', GVH__I4, GVH__SES, 1, 1, &
     &    'Bit field of database editing status for different types '// &
     &    'of solutions', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6524, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'AUTO_SUP', GVH__I4, GVH__BAS, 1, 1, &
     &    'Bit field of automatic suppression status for '// &
     &    'combination of observables', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6525, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'USER_SUP', GVH__I4, GVH__BAS, 1, 1, &
     &    'Bit field of analyst defined suppression status for '// &
     &    'combination of observables', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6526, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'USER_REC', GVH__I4, GVH__BAS, 1, 1, &
     &    'Bit field of analyst defined recovery status for '// &
     &    'combination of observables', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6527, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
! === Baseline reweighting
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RWDELVAL', GVH__R8, GVH__SES, SLV__MAX_SOLTYP, &
     &                N_BAS, 'Additive baseline-dependent '// &
     &               'reweighting parameters for delays (sec)', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6528, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RWRATVAL', GVH__R8, GVH__SES, SLV__MAX_SOLTYP, &
     &     N_BAS, 'Additive baseline-dependent reweighting parameters for '// &
     &    'delay rates (d/l)', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6529, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'RWBASNAM', GVH__C1, GVH__SES, 16, &
     &                N_BAS, 'Baseline names for additive '// &
     &                'baseline-dependent reweighting parameters ', &
     &                SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6530, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
! === Data type and suppression method
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DATYP   ', GVH__I2, GVH__SES, 1, &
     &                1, 'Type of the observable or a combination '// &
     &                'of observables used in the solution', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6531, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SUPMET  ', GVH__I2, GVH__SES, 1, &
     &                1, 'Code of the suppression method '// &
     &                  'used in the solution', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6532, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
! === Solution parameterization
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DGCL_EST', GVH__I2, GVH__SES, N_STA, &
     &                SLV__MAX_SOLTYP, 'Degree of global clock function '// &
     &               'polynomial per station, per solution type', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6533, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'CLO_INTR', GVH__R8, GVH__SES, SLV__MAX_SOLTYP, 1, &
     &               'Length of time span between spline nodes for clock '// &
     &               'function per soltyp (sec)', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6534, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'ATM_INTR', GVH__R8, GVH__SES, SLV__MAX_SOLTYP, 1, &
     &               'Length of time span between spline nodes for atm. '// &
     &               'path delay per soltyp (sec)', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6535, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'TIL_INTR', GVH__R8, GVH__SES, SLV__MAX_SOLTYP, 1, &
     &               'Length of time span between spline nodes for '// &
     &               'atmosphere tilt per soltyp (sec)', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6536, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EOP_EST ', GVH__I4, GVH__SES, 11, 1, &
     &                'Estimation status for EOP-related parameters', &
     &                SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6537, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'STPS_EST', GVH__I4, GVH__SES, 3, N_STA, &
     &               'Estimation status for station positions per '// &
     &               'component, per station', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6538, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SOCO_EST', GVH__I4, GVH__SES, 2, N_SOU, &
     &               'Estimation status for source coordinates per '// &
     &               'component, per object', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6539, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'BSCL_EST', GVH__I4, GVH__SES, N_BAS, 1, &
     &               'Estimation status for baseline dependent clock, '// &
     &               'per baseline', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6540, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
! === Reciprocal weights of constraints
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'CLO_CNS ', GVH__R8, GVH__SES, N_STA, &
     &                SLV__MAX_SOLTYP, 'Reciprocal weights of constraints '// &
     &               'on clock rate per station, per solution type', &
     &                SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6541, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'ATM_CNS ', GVH__R8, GVH__SES, N_STA, &
     &                SLV__MAX_SOLTYP, 'Reciprocal weights of constraints '// &
     &               'on atm. path delay rate per station, soltype', &
     &                SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6542, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'TLOF_CNS', GVH__R8, GVH__SES, N_STA, &
     &                SLV__MAX_SOLTYP, 'Reciprocal weights of constraints '// &
     &               'on atm. tilt offset per station, per soltype', &
     &                SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6543, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'TLRT_CNS', GVH__R8, GVH__SES, N_STA, &
     &                SLV__MAX_SOLTYP, 'Reciprocal weights of constraints '// &
     &               'on atm. tilt rate per station, per soltype', &
     &                SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6544, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EOP_CNS ', GVH__R8, GVH__SES, 11, &
     &                SLV__MAX_SOLTYP, 'Reciprocal weights of constraints '// &
     &               'on EOP related parameters', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6545, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'STPS_CNS', GVH__R8, GVH__SES, N_STA, &
     &                SLV__MAX_SOLTYP, 'Reciprocal weights of constraints '// &
     &               'on site positions per site, per solution type', &
     &                SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6546, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SOCO_CNS', GVH__R8, GVH__SES, N_SOU, &
     &                SLV__MAX_SOLTYP, 'Reciprocal weights of constraints '// &
     &               'on source coordinates per object, per soltype', &
     &                SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6547, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'BSCL_CNS', GVH__R8, GVH__SES, N_BAS, &
     &                SLV__MAX_SOLTYP, 'Reciprocal weights of constraints '// &
     &               'on basdep. clock, per baseline, per soltype', &
     &                SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6548, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'STA_USE ', GVH__I4, GVH__SES, N_STA, 1, &
     &                'Bit field of station selection status', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6549, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'SOU_USE ', GVH__I4, GVH__SES, N_SOU, 1, &
     &                'Bit field of source selection status', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6550, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'BAS_USE ', GVH__I4, GVH__SES, N_BAS, 1, &
     &                'Bit field of baseline selection status', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6551, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
 810  CONTINUE 
      IF ( SEG_TH1 .LE. 0 ) GOTO 820 ! skip setting ptoc for TH1 segment
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'TH_PROG ', GVH__C1, GVH__SES, 64, 1, &
     &                'Name and version of the program which computed '// &
     &                'theoretical path delays', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6552, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'TH_RUDAT', GVH__C1, GVH__SES, 26, 1, &
     &                'Date and time of theoretical delay computation', &
     &                SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6553, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'THGR_DEL', GVH__R8, GVH__BAS, 1, 1, &
     &               'Theoretical group delay (sec)', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6554, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'THPH_DEL', GVH__R8, GVH__BAS, 1, 1, &
     &               'Theoretical phase delay (sec)', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6555, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'THPH_RAT', GVH__R8, GVH__BAS, 1, 1, &
     &               'Theoretical phase delay date (d/l)', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6556, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DER_DEL ', GVH__R8, GVH__BAS, VTD__NDER_USED, 1, &
     &               'Array of partial derivatives of theoretical path '// &
     &               'delay wrt parameters of the model', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6557, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DER_RAT ', GVH__R8, GVH__BAS, VTD__NDER_USED, 1, &
     &               'Array of partial derivatives of theoretical '// &
     &               'delay rate wrt parameters of the model', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6558, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'N_APREOP', GVH__I4, GVH__SES, 1, 1, &
     &               'Number of nodes with apriori EOP', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6559, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'MJD_EOP ', GVH__I4, GVH__SES, 1, 1, &
     &               'Modified Julian date of the first epoch for '// &
     &               'the table of apriori EOP', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6560, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'TAI_EOP ', GVH__I4, GVH__SES, 1, 1, &
     &                'TAI time tag of first epoch of the table of '// &
     &                'apriori EOP (sec)', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6561, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'STEP_EOP', GVH__I4, GVH__SES, 1, 1, &
     &                'Step of the EOP table of apriori EOP (sec)', &
     &                SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6562, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'EOP_TAB ', GVH__R8, GVH__SES, &
     &                INT4(MAX_EROT_VALUES), 3, 'Table of aprori EOP '// &
     &               'as Euler angles with frequencies > 2 cpd filtered out', &
     &                SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6563, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'APR_EOP ', GVH__R8, GVH__BAS, 3, 2, &
     &                'Aprori EOP array as Euler angles and its '//&
     &                'derivatives (rad)', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6564, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'AZIMUTH ', GVH__R8, GVH__STA, 1, 1, &
     &                'Apparent source azimuth at both stations of '// &
     &                'the baseline (rad)', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6565, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'ELEV    ', GVH__R8, GVH__STA, 1, 1, &
     &                'Apparent source elevation at both stations of '// &
     &                'the baseline (rad)', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6566, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'NUT_DER ', GVH__R8, GVH__BAS, 2, 1, &
     &                'Partial derivatives wth nutation deaily '// &
     &                'offset parameters (sec)', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6567, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN 
      END IF
!
 820  CONTINUE 
      IF ( L_CAL > 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL GVH_PTOC ( GVH, 'CAL_STS ', GVH__I4, GVH__SES, N_STA, &
          &                L_CAL, 'Bit field of using available calibrations '//&
          &               'per station, per calibration', SEG_SL1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6569, IUER, 'UPDATE_PTOC', 'Trap of '// &
          &         'internal control' )
                RETURN 
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'TEC_STS ', GVH__I4, GVH__SES, N_STA, 1, &
     &    'Flag of availability/usage of the external ionosphere '// &
     &    'calibration', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6570, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'UV_COOR ', GVH__R8, GVH__BAS, 2, 1, &
     &    'UV coordinates of the baseline vector projection '// &
     &    'calibration', SEG_TH1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6571, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PTOC ( GVH, 'DB_VERS ', GVH__I2, GVH__SES, 1, 1, &
     &    'Database version counter', SEG_SL1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6572, IUER, 'UPDATE_PTOC', 'Trap of '// &
     &         'internal control' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  UPTDB_PTOC  !#!#

