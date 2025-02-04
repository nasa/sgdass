      SUBROUTINE SCATIE_DO ( DBOBJ, OBSSCA, OBSBAS, PAMBI, KAMB, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  SCATIE_DO  resolves phase delay ambiguities at both     *
! *   bands using SCATIE algorithm for all recoverable observations of   *
! *   the session. It is assumed that                                    *
! *     -- information about observables at both bands is available;     *
! *     -- phase delays were initialized;                                *
! *     -- phase delays spacings are the same for all observations       *
! *        at all observations within a band.                            *
! *     -- solution type is G_GXS__DTP                                   *
! *                                                                      *
! *     Algorithm  SCATIE  redistribute integer-numbered phase delay     *
! *   ambiguities at both X- and S- bands to keep closures at all        *
! *   triangulars. Since phase closure is within 3-15% of phase turns    *
! *   for observations (except cases with severe instrumental problems)  *
! *   phase delays which is phase + station-dependent feed horn          *
! *   correction + baseline-dependent integer_ambiguity*phase_delay_     *
! *   ambiguity_spacing should also conserve closure. Algorithm adds or  *
! *   subtract additional integer_ambiguity for some baselines in order  *
! *   to restore closure for all triangulars if it is violated. This     *
! *   operation is done for at both bands independently. Algorithm make  *
! *   corrections to baseline-dependent ambiguities in according their   *
! *   order in the lists without any preferences. In general there       *
! *   exists more than one way to redistribute phase delay ambiguities   *
! *   in order to restore closures. Algorithm SCATIE uses the first      *
! *   found variant. All recoverable observations are analyzed and taken *
! *   into account.                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      KAMB ( INTEGER*4 ) -- Number of observations where ambiguity is *
! *                            changed for at least one band.            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about      *
! *                            the session.                              *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  28-SEP-98     SCATIE_DO  v2.0  (c)  L. Petrov  21-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'pamb.i'
      INTEGER*4  KAMB, IUER
      TYPE ( DBOBJ_O__STRU ) :: DBOBJ
      TYPE ( BAS_O__STRU   ) :: OBSBAS(DBOBJ%L_OBS)
      TYPE ( SCA_O__STRU   ) :: OBSSCA(*)
      TYPE ( PAMBI__STRU   ) :: PAMBI(DBOBJ%L_OBS)
!
      INTEGER*4  LIS_OBS(MO_BAS), L_OBS, J1, IER
      INTEGER*2  ISCA_OLD
      CHARACTER  STR*32
      LOGICAL*4  FL_RECO, FL_URPH
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( DBOBJ%IDATYP .NE. G_GXS__DTP  .AND. &
     &     DBOBJ%IDATYP .NE. PX_GS__DTP         ) THEN
!
           CALL DATYP_SHOW ( DBOBJ%IDATYP, STR )
           CALL ERR_LOG ( 5231, IUER, 'SCATIE_DO', 'Attempt to use '// &
     &         'SCATIE while solution type "'//STR(1:I_LEN(STR))// &
     &         '" is set up. Please, change slution type to G-Gxs or PX_GS '// &
     &         'combination' )
           RETURN
      END IF
!
      ISCA_OLD = INT2(0)
      L_OBS    = 0
      KAMB     = 0
!
! --- Look at all scans
!
      DO 410 J1=1,DBOBJ%L_OBS
         IF ( OBSBAS(J1)%IND_SCA .NE. ISCA_OLD  .OR. J1 .EQ. DBOBJ%L_OBS ) THEN
!
! ----------- We complete a work with old scan because J1-th observation
! ----------- belongs to the next scan. We prepared list of observations of
! ----------- the previous scan. Try to check ambiguity closures for all
! ----------- linear independet triangles and to correct them if necessary.
! ----------- Of course, there is a sence to do it only of we have no less than
! ----------- 3 observations.
!
              IF ( L_OBS .GE. 3 ) THEN
!
! ---------------- Calling SCATIE_SOLVE to handle a set of recoverable
! ---------------- observations made during ISCA_OLD-th scan
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL SCATIE_SOLVE ( 1, L_OBS, LIS_OBS, INT4(ISCA_OLD), &
     &                                 DBOBJ, OBSBAS, OBSSCA, PAMBI, KAMB, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( INT4(ISCA_OLD), STR )
                        CALL ERR_LOG ( 5232, IUER, 'SCATIE_DO', 'Error in '// &
     &                      'resolving phase delay ambiguities for the '// &
     &                       STR(1:I_LEN(STR))//'-th scan' )
                        RETURN
                   END IF
              END IF
!
! ----------- We analysed observations of the ISCA_OLD-th scan. THen initiliaze
! ----------- list of recovereable observation of the next scan
!
              ISCA_OLD = OBSBAS(J1)%IND_SCA
              L_OBS = 0
         END IF
!
! ------ If observation is recoverable we add to the list of recoverable
! ------ observations of the current scan
!
         IF ( SUPMET == SUPMET__META ) THEN
              FL_RECO = META_SUPR_INQ ( OBSBAS(J1)%AUTO_SUP, &
     &                                  OBSBAS(J1)%USER_SUP, &
     &                                  OBSBAS(J1)%USER_REC, &
     &                                  RECO__SPS )
              FL_URPH = BTEST ( OBSBAS(J1)%AUTO_SUP, INT4(WPAS__SPS) )
            ELSE
              FL_RECO = SUPR_INQ ( OBSBAS(J1)%SUPSTAT(1), OBSBAS(J1)%UACSUP, &
     &                             RECO__SPS )
              FL_URPH = SUPR_INQ ( OBSBAS(J1)%SUPSTAT(1), OBSBAS(J1)%UACSUP, &
     &                             URPH__SPS ) 
         END IF
!
         IF (       FL_RECO  .AND.  &
     &        .NOT. FL_URPH         ) THEN
!
! ----------- Add the J1-th observation to the list of observation of the scan
!
              L_OBS          = L_OBS + 1
              LIS_OBS(L_OBS) = J1
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SCATIE_DO  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SCATIE_SOLVE ( IPAR, L_OBS, LIS_OBS, ISCA, DBOBJ, OBSBAS, &
     &                          OBSSCA, PAMBI, KAMB, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  SCATIE_SOLVE  applyes SCATIE algorithm for phase delay  *
! *   ambiguity resolution at both X and S band for observations of one  *
! *   scan.                                                              *
! *                                                                      *
! *     Algorithm  SCATIE  redistribute integer-numbered phase delay     *
! *   ambiguities at both X- and S- bands to keep closures at all        *
! *   triangulars. Since phase closure is within 3-15% of phase turns    *
! *   for observations (except cases with severe instrumental problems)  *
! *   phase delays which is phase + station-dependent feed horn          *
! *   correction + baseline-dependent integer_ambiguity*phase_delay_     *
! *   ambiguity_spacing should also conserve closure. Algorithm adds or  *
! *   subtract additional integer_ambiguity for some baselines in order  *
! *   to restore closure for all triangulars if it is violated. This     *
! *   operation is done for at both bands independently. Algorithm make  *
! *   corrections to baseline-dependent ambiguities in according their   *
! *   order in the lists without any preferences. In general there       *
! *   exists more than one way to redistribute phase delay ambiguities   *
! *   in order to restore closures. Algorithm SCATIE uses the first      *
! *   found variant. All recoverable observations are analyzed and taken *
! *   into account.                                                      *
! *                                                                      *
! *   Briefly, algorithm works as folloows:                              *
! *     a) it creates list of stations, baselines, linearly independent  *
! *        triangulars of the scan;                                      *
! *     b) it set status "not done" for all baselines;                   *
! *     c) it scans all linearly independent triangulars, calculates     *
! *        integer excess of misclosure and adds it with opposite sign   *
! *        to the integer ambiguity of the first "not used baseline" of  *
! *        the triangular under consideration;                           *
! *     d) it sets status "used baseline" to all baselines of the        *
! *        triangular under consideration.                               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    IPAR ( INTEGER*4 ) -- Mode switcher.                              *
! *                          IPAR=1 -- all recoverable observations      *
! *                                     are analyzed;                    *
! *                          IPAR=2 -- all used in group delay solutiion *
! *                                    observations are analyzed;        *
! *   L_OBS ( INTEGER*4 ) -- Number of recoverable observations which    *
! *                          belong to the ISCA-th scan.                 *
! * LIS_OBS ( INTEGER*4 ) -- List of observations which belong to the    *
! *                          ISCA-th scan. The k-th element of the list  *
! *                          LIS_OBS is the index of the observation in  *
! *                          the database to be under consideration.     *
! *    ISCA ( INTEGER*4 ) -- Index of the scan under consideration.      *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *  OBSSCA ( RECORD    ) -- Array of data structures which keeps        *
! *                          scan-dependent information about the        *
! *                          session.                                    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    KAMB ( INTEGER*4 ) -- Number of ambiguity was changes at X-band   *
! *                          plus number of changes at S-band.           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  OBSBAS ( RECORD    ) -- Array of data structures which keeps        *
! *                          baseline dependent information about        *
! *                          the session.                                *
! *   PAMBI ( RECORD    ) -- Array of data structures keeping            *
! *                          information about phase delays, their       *
! *                          errors, ambiguities and etc.                *
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
! *  ###  28-SEP-98  SCATIE_SOLVE  v1.0  (c)  L. Petrov  28-SEP-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'pamb.i'
      INTEGER*4  IPAR, L_OBS, LIS_OBS(L_OBS), ISCA
      INTEGER*4  KAMB, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(DBOBJ%L_OBS)
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( PAMBI__STRU ) ::  PAMBI(DBOBJ%L_OBS)
      CHARACTER  STR*20
      REAL*8     ECLS_S, ECLS_X
      INTEGER*4  ICLS_S, ICLS_X, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, &
     &           IOBS_TRI(3), IER, ISTA1, ISTA2, IAMB_BAS, ICMP, IND_OBS, &
     &           IBAS, IPL_BAS, ISG_BAS(3)
      INTEGER*4  L_STA, LIS_STA(MO_STA), L_BAS, LIS_BAS(MO_BAS), &
     &           L_TRI, LIS_TRI(3,MO_TRI)
      LOGICAL*4  USE_OBS(L_OBS), USED_TRI, DONE_BAS(MO_BAS)
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
      INTEGER*4, EXTERNAL :: NSTBA, IFIND_PL, I_LEN
!
      IF ( L_OBS .LT. 3 ) THEN
!
! -------- Less than three observations: nothing to do!
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- I. FORMING STATION/BASELINE/TRIANGLE LIST for this scan
!
      L_STA = 0
      L_BAS = 0
!
! --- Scan all recoverable observations of the scan and form the lists of
! --- stations and baselines
!
      DO 410 J1=1,L_OBS
         IF ( IPAR .EQ. 2 ) THEN
!
! ----------- Check status of the observation: used in solution or not
!
              IF ( SUPMET == SUPMET__META ) THEN
                   USE_OBS(J1) = META_SUPR_INQ ( OBSBAS(LIS_OBS(J1))%AUTO_SUP, &
     &                                           OBSBAS(LIS_OBS(J1))%USER_SUP, &
     &                                           OBSBAS(LIS_OBS(J1))%USER_REC, &
     &                                           USED__SPS )
                 ELSE
                   USE_OBS(J1) = SUPR_INQ ( OBSBAS(LIS_OBS(J1))%SUPSTAT(1), &
     &                                      OBSBAS(LIS_OBS(J1))%UACSUP, &
     &                                      USED__SPS )
              END IF
            ELSE IF ( IPAR .EQ. 1 ) THEN
!
! ----------- We consider all observations as used
!
              USE_OBS(J1) = .TRUE.
            ELSE
              CALL CLRCH   ( STR )
              CALL INCH    ( IPAR, STR )
              CALL ERR_LOG ( 5241, IUER, 'SCATIE_SOLVE', 'Wrong value of '// &
     &                      'parameter IPAR: '//STR )
         END IF
!
         ISTA1 = INT4( OBSBAS(LIS_OBS(J1))%ISITE(1) )
         ISTA2 = INT4( OBSBAS(LIS_OBS(J1))%ISITE(2) )
         IBAS  = NSTBA ( ISTA1, ISTA2 )
!
! ------ Add station code to the list of stations
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADD_LIS  ( MO_STA, L_STA, LIS_STA, ISTA1, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5241, IUER, 'SCATIE_SOLVE', 'Error in '// &
     &            'adding a station to the list of stations' )
              RETURN
         END IF
!
! ------ Add station code to the list of stations
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADD_LIS  ( MO_STA, L_STA, LIS_STA, ISTA2, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5242, IUER, 'SCATIE_SOLVE', 'Error in '// &
     &            'adding a station to the list of stations' )
              RETURN
         END IF
!
! ------ Add baseline code to the list of baselines
!
         CALL ERR_PASS ( IUER, IER )
         CALL ADD_LIS  ( MO_BAS, L_BAS, LIS_BAS, IBAS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5243, IUER, 'SCATIE_SOLVE', 'Error in '// &
     &            'adding a baseline to the list of baselines' )
              RETURN
         END IF
!
! ------ We initialize also array DONE_BAS
!
         DONE_BAS(J1)   = .FALSE.
 410  CONTINUE
!
! --- Sorting baselines in increasing order of modules of baseline indices
!
      CALL SORT_IA ( L_BAS, LIS_BAS )
!
! --- Sorting list of stations
!
      CALL SORT_I  ( L_STA, LIS_STA )
!
! --- Calculation the number of closed triangles
!
      L_TRI = (L_STA - 2) * (L_STA - 1) / 2
      IF ( L_STA .LT. 3 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Creating the list of closed triangles (if there are any)
!
      CALL ERR_PASS ( IUER, IER )
      CALL TRI_GRP  ( L_STA, LIS_STA, L_BAS, LIS_BAS, MO_TRI, L_TRI, LIS_TRI, &
     &                IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5244, IUER, 'SCATIE_SOLVE', 'Error during '// &
     &         'building the list of closed triangles' )
           RETURN
      END IF
!
! --- II. CALCULATION OF INTEGER MISCLOSURE FOR ALL TRIANGLES AND
! ---     APPLYING CORRECTION
!
      DO 420 J2=1,L_TRI ! cycle over all linearly independent triangulars
!
! ------ Initialization
!
         USED_TRI= .TRUE.
         DO 430 J3=1,3
            IOBS_TRI(J3) = -1
 430     CONTINUE
!
! ------ Scan all observation of the scan and examine the triangle list.
! ------ We should form the list of observations IOBS_TRI which forms
! ------ the J2-th triangle. Each element of IOBS_TRI keeps an index
! ------ of the observation in the database. The set status "used" or not used
! ------ triangular. If at least one observation of the triangular is not used
! ------ then the triangular is marked as not used.
!
         DO 440 J4=1,L_OBS ! over observations of the scan
            ISTA1 = INT4( OBSBAS(LIS_OBS(J4))%ISITE(1) )
            ISTA2 = INT4( OBSBAS(LIS_OBS(J4))%ISITE(2) )
            IBAS  = NSTBA ( ISTA1, ISTA2 )
            IPL_BAS = IFIND_PL ( L_BAS, LIS_BAS, IBAS )
            DO 450 J5=1,3
               IF ( LIS_TRI(J5,J2) .EQ. IPL_BAS ) THEN
                    IOBS_TRI(J5) = LIS_OBS(J4)
                    IF ( .NOT. USE_OBS(J4) ) USED_TRI = .FALSE.
               END IF
 450        CONTINUE
 440     CONTINUE
!
         IF ( IOBS_TRI(1) .LT. 1  .OR.  IOBS_TRI(1) .GT. DBOBJ%L_OBS  .OR. &
     &        IOBS_TRI(2) .LT. 1  .OR.  IOBS_TRI(2) .GT. DBOBJ%L_OBS  .OR. &
     &        IOBS_TRI(3) .LT. 1  .OR.  IOBS_TRI(3) .GT. DBOBJ%L_OBS      ) THEN
!
              WRITE ( 6, * ) ' L_OBS = ',L_OBS
              WRITE ( 6, * ) ' LIS_OBS = ',(LIS_OBS(J4), J4=1,L_OBS)
              WRITE ( 6, * ) ' IOBS_TRI = ', IOBS_TRI
              CALL ERR_LOG ( 5245, IUER, 'SCATIE_SOLVE', 'Trap of internal '// &
     &            'control: observations were not found to fill a triangle' )
              RETURN
         END IF
!
! ------ Calculation of integer phase delay ambiguity misclosure and its
! ------ formal error ( in phase turns ) for both bands.
!
         CALL ERR_PASS ( IUER, IER )
         CALL AMB_CLS  ( DBOBJ, OBSBAS, IOBS_TRI, ICLS_X, ICLS_S, ECLS_X, &
     &                   ECLS_S, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(OBSBAS(LIS_OBS(1))%IND_SCA), STR )
              CALL ERR_LOG ( 5246, IUER, 'SCATIE_SOLVE', 'Error in '// &
     &            'calculation of triangle misclosure for the scan '//STR )
              RETURN
         END IF
!
         IF ( ICLS_X .NE. 0  .OR.  ICLS_S .NE. 0 ) THEN
!
! ----------- Triangle misclosure has been detected for at least one band
!
              IPL_BAS = -1
!
! ----------- Look over 3 components of the triangle: search of a baseline
! ----------- which has not been yet used.
!
              DO 460 J6=1,3
                 IF ( .NOT. DONE_BAS(LIS_TRI(J6,J2)) ) THEN
                      IPL_BAS = LIS_TRI(J6,J2)
                      IBAS    = LIS_BAS(IPL_BAS)
                      IND_OBS = IOBS_TRI(J6)
                      ICMP = J6  ! This component of the triangle is not used
                 END IF
 460          CONTINUE
!
              IF ( IPL_BAS .LT. 1  .OR.  IPL_BAS .GT. L_BAS ) THEN
                   CALL ERR_LOG ( 5247, IUER, 'SCATIE_SOLVE', 'Trap of '// &
     &                 'internal control: a fresh baseline for triangle '// &
     &                 'was not found' )
                   RETURN
              END IF
!
              IF ( IND_OBS .LT. 1  .OR.  IND_OBS .GT. DBOBJ%L_OBS ) THEN
                   CALL ERR_LOG ( 5248, IUER, 'SCATIE_SOLVE', 'Trap of '// &
     &                 'internal control: an index of the fresh baseline '// &
     &                 'was not found' )
                   RETURN
              END IF
!
! ----------- Computation of the signs for triangle closures
!
              CALL ERR_PASS ( IUER, IER )
              CALL SIGN_TRI ( LIS_TRI(1,J2), L_BAS, LIS_BAS, ISG_BAS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5249, IUER, 'SCATIE_SOLVE', 'Error in '// &
     &                 'calculation signs for the triangle' )
                   RETURN
              END IF
!
              IF ( ICLS_X .NE. 0 ) THEN
!
! ---------------- Update of X-band ambiguities
!
                   IAMB_BAS = -ISG_BAS(ICMP)*ICLS_X
                   CALL AMB_UPDATE ( PAMB__XBAND, IAMB_BAS, OBSBAS(IND_OBS), &
     &                               PAMBI(IND_OBS), -3 )
              END IF
!
              IF ( ICLS_S .NE. 0 ) THEN
!
! ---------------- Update of the S-band phase ambiguities
!
                   IAMB_BAS = -ISG_BAS(ICMP)*ICLS_S
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!                   type *,' isca = ',isca                                  ! %%%
!                   type *,' ipl_bas = ',ipl_bas, ' icmp=',icmp,            ! %%%
!     #                    ' ind_obs = ',ind_obs, ' iobs_tri = ', iobs_tri, ! %%%
!     #            ' isg_bas(icmp) = ',isg_bas(icmp),' icls_s = ',icls_s,   ! %%%
!     # ' corr=',dfloat(iamb_bas(2,ipl_bas))/obsbas(ind_obs).freq_obsv_ph_opp ! %
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                   CALL AMB_UPDATE ( PAMB__SBAND, IAMB_BAS, OBSBAS(IND_OBS), &
     &                               PAMBI(IND_OBS), -3 )
              END IF
              KAMB = KAMB + 1
         END IF
!
! ------ Setting status: all baseline of the J2-th triangle have been used
! ------ in order to prevent using this baseline for further possible
! ------ modification of baseline-dependent phase delay ambiguity
!
         DO 470 J7=1,3
            DONE_BAS(LIS_TRI(J7,J2)) = .TRUE.
 470     CONTINUE
 420  CONTINUE
!
! --- III. VERIFICATION PASS. Look at integer phase delay ambiguity misclosure.
! --- Error message will be generated in the case if misclosure will be
! --- detected. "Doveryaja proveryaj, a proveryaja doveryaj!"
!
      DO 480 J8=1,L_TRI
!
! ------ Scan all observation of the scan and examine the triangle list.
! ------ We should form the list of observations IOBS_TRI which forms a triangle
!
         DO 490 J9=1,L_OBS
            ISTA1 = INT4( OBSBAS(LIS_OBS(J9))%ISITE(1) )
            ISTA2 = INT4( OBSBAS(LIS_OBS(J9))%ISITE(2) )
            IBAS  = NSTBA ( ISTA1, ISTA2 )
            IPL_BAS = IFIND_PL ( L_BAS, LIS_BAS, IBAS )
            DO 4100 J10=1,3
               IF ( LIS_TRI(J10,J8) .EQ. IPL_BAS ) THEN
                    IOBS_TRI(J10) = LIS_OBS(J9)
               END IF
 4100       CONTINUE
 490     CONTINUE
!
! ------ Calculation of integer phase delay ambiguity misclosure and its
! ------ formal error ( in phase turns ) for both bands the second time
! ------ (for checking purposes)
!
         CALL ERR_PASS ( IUER, IER )
         CALL AMB_CLS  ( DBOBJ, OBSBAS, IOBS_TRI, ICLS_X, ICLS_S, ECLS_X, &
     &                   ECLS_S, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(OBSBAS(LIS_OBS(1))%IND_SCA), STR )
              CALL ERR_LOG ( 5250, IUER, 'SCATIE_SOLVE', 'Error in '// &
     &            'calculation of triangle misclosure for the scan '//STR )
              RETURN
         END IF
!
! ------ Check: do we still have ambiguity misclosure?
!
         IF ( ICLS_X .NE. 0  .OR. ICLS_S .NE. 0 ) THEN
!
! ----------- Oh-h-h! We still have them! Algorithm failed. We should strew our
! ----------- head with ash
!
              WRITE ( 6, * ) '### ICLS_X = ',ICLS_X,' ICLS_S = ',ICLS_S
              WRITE ( 6, * ) ' J8 = ',J8,' L_TRI = ',L_TRI,' ISCA = ',ISCA
              WRITE ( 6, * ) ' IOBS_TRI = ', IOBS_TRI
              WRITE ( 6, * ) ' L_OBS = ',L_OBS
              WRITE ( 6, * ) ' LIS_OBS = ',( LIS_OBS(J1), J1=1,L_OBS )
!
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(OBSBAS(LIS_OBS(1))%IND_SCA), STR )
              CALL ERR_LOG ( 5251, IUER, 'SCATIE_SOLVE', 'Trap of '// &
     &            'internal control: failure in redistribution of '// &
     &            'misclosure error among baselines of a triangle was '// &
     &            'detected when scan '//STR(1:I_LEN(STR))//' was being '// &
     &            'processed' )
              RETURN
         END IF
 480  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SCATIE_SOLVE  #!#
