      SUBROUTINE AMB_CLS ( DBOBJ, OBSBAS, IOBS_TRI, ICLS_X, ICLS_S, ECLS_X, &
     &                     ECLS_S, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  AMB_CLS  calculates integer phase delay ambiguity         *
! *   misclosure and its formal error ( in phase turns ) for both bands. *
! *   Integer misclosure is the difference between phase delay triangle  *
! *   misclosure and phase triangle misclosure expressed in phase turns. *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *  IOBS_TRI ( INTEGER*4 ) -- Array dimension of 3 which contains       *
! *                            indices of the observations of the same   *
! *                            scan at the baselines which produces a    *
! *                            triangle closure.                         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    ICLS_X ( INTEGER*4 ) -- Integer number of ambiguities misclosure  *
! *                            at the band X.                            *
! *    ICLS_S ( INTEGER*4 ) -- Integer number of ambiguities misclosure  *
! *                            at the band S.                            *
! *    ECLS_X ( REAL*8    ) -- Formal error of phase delay misclosure    *
! *                            at the band X (in phase turns).           *
! *    ECLS_S ( REAL*8    ) -- Formal error of phase delay misclosure    *
! *                            at the band S (in phase turns).           *
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
! *  ###  24-SEP-98    AMB_CLS     v1.1  (c)  L. Petrov  29-SEP-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INTEGER*4  IOBS_TRI(3), ICLS_X, ICLS_S, IUER
      REAL*8     ECLS_X, ECLS_S
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(DBOBJ%L_OBS)
      INTEGER*4  NBAS_TRI(3), ISG_TRI(3), IND_TAU(2), IND_FRE(2), ISG_COR(2)
      REAL*8     OBS_1, OBS_2, OBS_3, ERR_1, ERR_2, ERR_3, CLS_PHD, CLS_PHS, &
     &           ECLS_ARR(3), ER_CLS, PAMB_SP, PAMB_SP2, PAMB_SP3
      INTEGER*4  J1, J2, ICLS_ARR(2), IAMB, IER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  IFIND_PL, NSTBA
!
! --- Create an array dimension of 3 which contains indices of the baselines
! --- in the list DBOBJ.LIS_BAS for triangle closure.
!
      DO 410 J1=1,3
         NBAS_TRI(J1) = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, &
     &                           NSTBA ( INT4(OBSBAS(IOBS_TRI(J1))%ISITE(1)), &
     &                                   INT4(OBSBAS(IOBS_TRI(J1))%ISITE(2)) ) )
 410  CONTINUE
!
! --- Calculation of a signature of closed the triangle: which signs should be
! --- put before observables to form a closure
!
      CALL ERR_PASS ( IUER, IER )
      CALL SIGN_TRICLS ( NBAS_TRI, DBOBJ%L_BAS, DBOBJ%LIS_BAS, &
     &                   ISG_TRI, ISG_COR, IND_FRE, IND_TAU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6401, IUER, 'AMB_MSC', 'Error in calculation of '// &
     &         'signs for a triangle closures' )
           RETURN
      END IF
!
! --- Do the business twice: firstly for X-band then for the S-band
!
      DO 420 J2=1,2
         IF ( J2 .EQ. 1 ) THEN
!
! ----------- Getting phase delay ambiguity spacings and
! ----------- observables for X-band.
!
              PAMB_SP  = 1.D0/OBSBAS(IOBS_TRI(1))%FREQ_OBSV_PH
              PAMB_SP2 = 1.D0/OBSBAS(IOBS_TRI(2))%FREQ_OBSV_PH
              PAMB_SP3 = 1.D0/OBSBAS(IOBS_TRI(3))%FREQ_OBSV_PH
              IF ( PAMB_SP .NE. PAMB_SP2  .OR.  PAMB_SP .NE. PAMB_SP3 ) THEN
                   WRITE ( 6, * ) ' IOBS = ',IOBS_TRI(1),' PAMB_SP  = ',PAMB_SP
                   WRITE ( 6, * ) ' IOBS = ',IOBS_TRI(2),' PAMB_SP2 = ',PAMB_SP2
                   WRITE ( 6, * ) ' IOBS = ',IOBS_TRI(3),' PAMB_SP3 = ',PAMB_SP3
                   CALL ERR_LOG ( 6402, IUER, 'AMB_MSC', 'Different '// &
     &                 'ambiguity spacings at X band for triangle detected' )
                   RETURN
              END IF
!
              OBS_1 = OBSBAS(IOBS_TRI(1))%TAUPH_OBS
              OBS_2 = OBSBAS(IOBS_TRI(2))%TAUPH_OBS
              OBS_3 = OBSBAS(IOBS_TRI(3))%TAUPH_OBS
              ERR_1 = OBSBAS(IOBS_TRI(1))%TAUPH_ERR
              ERR_2 = OBSBAS(IOBS_TRI(2))%TAUPH_ERR
              ERR_3 = OBSBAS(IOBS_TRI(3))%TAUPH_ERR
           ELSE IF ( J2 .EQ. 2 ) THEN
!
! ----------- The same for the S-band
!
              PAMB_SP  = 1.D0/OBSBAS(IOBS_TRI(1))%FREQ_OBSV_PH_OPP
              PAMB_SP2 = 1.D0/OBSBAS(IOBS_TRI(2))%FREQ_OBSV_PH_OPP
              PAMB_SP3 = 1.D0/OBSBAS(IOBS_TRI(3))%FREQ_OBSV_PH_OPP
              IF ( PAMB_SP .NE. PAMB_SP2  .OR.  PAMB_SP .NE. PAMB_SP3 ) THEN
                   WRITE ( 6, * ) ' IOBS = ',IOBS_TRI(1),' PAMB_SP  = ',PAMB_SP
                   WRITE ( 6, * ) ' IOBS = ',IOBS_TRI(2),' PAMB_SP2 = ',PAMB_SP2
                   WRITE ( 6, * ) ' IOBS = ',IOBS_TRI(3),' PAMB_SP3 = ',PAMB_SP3
                   CALL ERR_LOG ( 6403, IUER, 'AMB_MSC', 'Different '// &
     &                 'ambiguity spacings at S band for triangle detected' )
                   RETURN
              END IF
!
              OBS_1 = OBSBAS(IOBS_TRI(1))%TAUPH_OBS_OPP
              OBS_2 = OBSBAS(IOBS_TRI(2))%TAUPH_OBS_OPP
              OBS_3 = OBSBAS(IOBS_TRI(3))%TAUPH_OBS_OPP
              ERR_1 = OBSBAS(IOBS_TRI(1))%TAUPH_ERR_OPP
              ERR_2 = OBSBAS(IOBS_TRI(2))%TAUPH_ERR_OPP
              ERR_3 = OBSBAS(IOBS_TRI(3))%TAUPH_ERR_OPP
         END IF
!
! ------ Producing a phase delay closure -- CLS_PHD
!
         CLS_PHD = ISG_TRI(1)*OBS_1 + ISG_TRI(2)*OBS_2 + ISG_TRI(3)*OBS_3
!
! ------ Fixing differences in epochs
!
         IF ( J2 .EQ. 1 ) THEN
             CLS_PHD = CLS_PHD + &
     &                 ISG_COR(1)*OBSBAS(IOBS_TRI(IND_FRE(1)))%RATE_OBS * &
     &                 OBSBAS(IOBS_TRI(IND_TAU(1)))%TAU_C + &
     &                 ISG_COR(2)*OBSBAS(IOBS_TRI(IND_FRE(2)))%RATE_OBS * &
     &                 OBSBAS(IOBS_TRI(IND_TAU(2)))%TAU_C
            ELSE IF ( J2 .EQ. 2 ) THEN
             CLS_PHD = CLS_PHD + &
     &                 ISG_COR(1)*OBSBAS(IOBS_TRI(IND_FRE(1)))%RATE_OBS_OPP * &
     &                 OBSBAS(IOBS_TRI(IND_TAU(1)))%TAU_C + &
     &                 ISG_COR(2)*OBSBAS(IOBS_TRI(IND_FRE(2)))%RATE_OBS_OPP * &
     &                 OBSBAS(IOBS_TRI(IND_TAU(2)))%TAU_C
         END IF
!
! ------ Fixing possible phase ambiguity jumps. As a result we produce
! ------ ambifuty free misclosure CLS_PHS
!
         CLS_PHS = CLS_PHD
         IAMB = NINT ( CLS_PHS/PAMB_SP )
         CLS_PHS  = CLS_PHS - IAMB*PAMB_SP
         IF ( CLS_PHS .GT. 0.5*PAMB_SP ) THEN
              CLS_PHS = CLS_PHS - PAMB_SP
         END IF
         IF ( CLS_PHS .LT. -0.5*PAMB_SP ) THEN
              CLS_PHS = CLS_PHS + PAMB_SP
         END IF
!
! ------ Computing formal erros
!
         ER_CLS = DSQRT ( ERR_1**2 + ERR_2**2 + ERR_3**2 )
!
! ------ Calculation ambiguity misclosure and its sigma (in phase turns).
!
         ICLS_ARR(J2) = NINT( (CLS_PHD - CLS_PHS)/PAMB_SP )
         ECLS_ARR(J2) = ER_CLS/PAMB_SP
 420  CONTINUE
!
! --- Putting stuff into the output variables
!
      ICLS_X = ICLS_ARR(1)
      ICLS_S = ICLS_ARR(2)
!
      ECLS_X = ECLS_ARR(1)
      ECLS_S = ECLS_ARR(2)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  AMB_CLS  #!#
