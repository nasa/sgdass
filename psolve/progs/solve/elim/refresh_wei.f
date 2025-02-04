      SUBROUTINE REFRESH_WEI ( OBSHLD, DBOBJ, OBSBAS, WEIGR_BAS, WEIPH_BAS, &
     &                         IUER )
! ************************************************************************
! *                                                                      *
! *   Routine   REFRESH_WEI  refreshs weights for all observations       *
! *   saved in the data structure OBSBAS in according with arrays        *
! *   WEIGR_BAS, WEIPH_BAS. Weights applied to each observations are     *
! *   quadratic sum of the a priori weight (from correleator) and        *
! *   old reweigt constant. Old reweight constants kept in OBSHLD.       *
! *   After refreshing weights applied to each observation will be       *
! *   quadratic sum of the a prioti weight and new rewight constant kept *
! *   in arrys WEIGR_BAS and WEIPH_BAS. Object OBSHLD remains untouched  *
! *   after weights refreshing.                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    OBSHLD ( RECORD    ) -- Data structure which keeps the current    *
! *                            status of the database and some           *
! *                            session-dependent information.            *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! * WEIGR_BAS ( REAL*8    ) -- Array of NEW baseline-dependent           *
! *                            corrections to weights when group delay   *
! *                            observables are in use. Array is ordered  *
! *                            in according with the list of baselines   *
! *                            kept in the object DBOBJ.                 *
! * WEIPH_BAS ( REAL*8    ) -- Array of NEW baseline-dependent           *
! *                            corrections to weights when phase delay   *
! *                            observables are in use. Array is ordered  *
! *                            in according with the list of baselines   *
! *                            kept in the object DBOBJ.                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                            Input: switch IUER=0 -- no error messages *
! *                                   will be generated even in the case *
! *                                   of error. IUER=-1 -- in the case   *
! *                                   of error the message will pe put   *
! *                                   on stdout.                         *
! *                            Output: 0 in the case of successful       *
! *                                    completion and non-zero in the    *
! *                                    case of error.                    *
! *                                                                      *
! *  ### 29-JAN-1998  REFRESH_WEI  v1.3 (c)  L. Petrov  20-JAN-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      TYPE ( HLD_O__STRU ) ::  OBSHLD
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(*)
      INTEGER*4  IUER
      CHARACTER  STR*32
      REAL*8     WEIGR_BAS(MO_BAS), WEIPH_BAS(MO_BAS)
      REAL*8     EPS, WEI_MIN, SIG_OLD, SIG_ACO, SIG_PURE, SIG_ACN, SIG_NEW
      PARAMETER  ( EPS = 1.D-6 )
      PARAMETER  ( WEI_MIN = 1.D-15 )
      INTEGER*4  J1
!
      INTEGER*4  IP_BAS
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: IFIND_PL, NSTBA
!
! --- Cycle over all baselines
!
      DO 410 J1=1,DBOBJ%L_OBS
!
! ------ IP_BAS -- Index of the baseline for the J1-th observation
!
         IP_BAS = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, &
     &                       NSTBA( INT4(OBSBAS(J1)%ISITE(1)), &
     &                              INT4(OBSBAS(J1)%ISITE(2)) ) )
         IF ( IP_BAS .LE. 0  .OR. IP_BAS .GT. DBOBJ%L_BAS ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 6311, IUER, 'REFRESH_WEI', 'Internal error: '// &
     &            'invalid index for observation '//STR  )
              RETURN
         END IF
!
! ------ GROUP DELAY CASE
!
!@         IF ( WEIGR_BAS(IP_BAS) < WEI_MIN ) THEN
!@              CALL CLRCH ( STR )
!@              CALL INCH  ( J1, STR )
!@              CALL ERR_LOG ( 6312, IUER, 'REFRESH_WEI', 'Internal error: '// &
!@     &            'weight is zero for observation '//TRIM(STR)//' at baseline '// &
!@     &             DBOBJ%C_BAS(IP_BAS) )
!@              RETURN
!@         END IF
!@!
!@         IF ( OBSHLD%WEIGR_BAS(IP_BAS) < WEI_MIN ) THEN
!@              WRITE ( 6, * ) WEIGR_BAS(IP_BAS), OBSHLD%WEIGR_BAS(IP_BAS) 
!@              CALL CLRCH ( STR )
!@              CALL INCH  ( J1, STR )
!@              CALL ERR_LOG ( 6313, IUER, 'REFRESH_WEI', 'Internal error: '// &
!@     &            'weight is zero for observation '//TRIM(STR)//' at baseline '// &
!@     &             DBOBJ%C_BAS(IP_BAS) )
!@              RETURN
!@         END IF
!
! ------ Look: if the weight in array WEIGR_BAS differs form the weight in the
! ------ array OBSHLD%WEIGR_BAS (which were actully used before) more
! ------ than the specified limit -- then we should refresh weights.
!
         IF ( DABS( WEIGR_BAS(IP_BAS) - OBSHLD%WEIGR_BAS(IP_BAS) ) .GT. &
     &        EPS*DABS(WEIGR_BAS(IP_BAS))  .AND. &
     &        OBSHLD%WEIGR_BAS(IP_BAS) > WEI_MIN                        ) THEN
!
! ----------- SIG_OLD  -- Old total correction to sigma applied for weights
! -----------             calculation
! ----------- SIG_ACO  -- Old baseline-dependent quadratically additive
! -----------             correction to sigma
! ----------- SIG_PURE -- Correction to sigma without quadratical correction
! ----------- SIG_ACO  -- New quadratically additive correction to sigma
! ----------- SIG_NEW  -- New total sigma to be applied for weights calculation
!
              SIG_OLD  = OBSBAS(J1)%TAUGR_ERR_COR
              SIG_ACO  = 1.D0/OBSHLD%WEIGR_BAS(IP_BAS)
              SIG_ACN  = 1.D0/WEIGR_BAS(IP_BAS)
!
! ----------- Comment: SIG_PURE cannot be imaginary. But rounding error may
! ----------- make SIG_OLD**2 - SIG_ACO**2 very small negative. To prevent
! ----------- failure of making square toor we take abs from the difference.
!
              SIG_PURE = DSQRT ( DABS( SIG_OLD**2 - SIG_ACO**2 ) )
              SIG_NEW  = DSQRT ( SIG_PURE**2 + SIG_ACN**2 )
!
              OBSBAS(J1)%TAUGR_ERR_COR = SIG_NEW
         END IF
!
! ------ PHASE DELAY CASE
!
!
! ------ Look: if the weight in array WEIPH_BAS differs form the weight in the
! ------ array OBSHLD.WEIPH_BAS (which were actully used before) more
! ------ than the specified limit -- then we should refresh weights.
!
         IF ( DABS( WEIPH_BAS(IP_BAS) - OBSHLD%WEIPH_BAS(IP_BAS) ) .GT. &
     &        EPS*DABS(WEIPH_BAS(IP_BAS))                              ) THEN
!
! ----------- SIG_OLD  -- Old total correctiuon to sigma applied for weights
! -----------             calculation
! ----------- SIG_ACO  -- Old baseline-dependent quadratically additive
! -----------             correction to sigma
! ----------- SIG_PURE -- Correction to sigma without quadratical correction
! ----------- SIG_ACO  -- New quadratically additive correction to sigma
! ----------- SIG_NEW  -- New total sigma to be applied for weights calculation
!
              SIG_OLD  = OBSBAS(J1)%TAUPH_ERR_COR
              SIG_ACO  = 1.D0/OBSHLD%WEIPH_BAS(IP_BAS)
              SIG_ACN  = 1.D0/WEIPH_BAS(IP_BAS)
!
! ----------- Comment: SIG_PURE cannot be imaginary. But rounding error may
! ----------- make SIG_OLD**2 - SIG_ACO**2 very small negative. To prevent
! ----------- failure of making square toor we take abs from the difference.
!
              SIG_PURE = DSQRT ( DABS( SIG_OLD**2 - SIG_ACO**2 ) )
              SIG_NEW  = DSQRT ( SIG_PURE**2 + SIG_ACN**2 )
!
              OBSBAS(J1)%TAUPH_ERR_COR = SIG_NEW
         END IF
  410 CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REFRESH_WEI  #!#
