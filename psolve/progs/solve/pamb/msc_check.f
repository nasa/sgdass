      SUBROUTINE MSC_CHECK ( L_OBS, LIS_OBS, DBOBJ, OBSSCA, OBSBAS, PAMBI, &
     &                       XGR_LIM, SGR_LIM, XPH_LIM, SPH_LIM, IVERB, &
     &                       L_STA, LIS_STA, L_BAS, LIS_BAS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MSC_CHECK  checks misclosure of all closed triangles      *
! *   for X-band group delay, S-band group delay, X-band phase delay     *
! *   and S-band phase delay. If it find such a triangle it tries to     *
! *   detect a defective baseline and to exclude it from the lists.      *
! *   Lists of baselines and stations may be changed in the result of    *
! *   work MSC_CHECK.                                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     L_OBS ( INTEGER*4 ) -- Number of observations in the list of     *
! *                            observations of the same scan to be       *
! *                            checked.                                  *
! *   LIS_OBS ( INTEGER*4 ) -- List of observations to be checked.       *
! *                            Dimension: L_OBS. LIS_OBS(k) is the index *
! *                            of the observation in the database.       *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about      *
! *                            the session.                              *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! *   XGR_LIM ( REAL*8    ) --
! *   SGR_LIM ( REAL*8    ) --
! *   XPH_LIM ( REAL*8    ) --
! *   SPH_LIM ( REAL*8    ) --
! *     IVERB ( INTEGER*4 ) --
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      L_BAS ( INTEGER*4 ) -- Number of baselines.                     *
! *    LIS_BAS ( INTEGER*4 ) -- List of baselines. Each element contains *
! *                             a baseline code assigned by routine      *
! *                             NSTBA                                    *
! *      L_STA ( INTEGER*4 ) -- Number of stations.                      *
! *    LIS_STA ( INTEGER*4 ) -- List of stations. Each element contains  *
! *                             a station code assigned by SDBH          *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  09-OCT-98    MSC_CHECK   v1.0  (c)  L. Petrov  02-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'pamb.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(DBOBJ%L_OBS)
      TYPE ( SCA_O__STRU ) ::  OBSSCA(DBOBJ%L_SCA)
      TYPE ( PAMBI__STRU ) ::   PAMBI(DBOBJ%L_OBS)
      INTEGER*4  L_OBS, LIS_OBS(L_OBS), L_STA, LIS_STA(L_STA), &
     &           L_BAS, LIS_BAS(L_BAS), L_TRI, LIS_TRI(3,MO_TRI), IVERB, IUER
      REAL*8     XGR_LIM, SGR_LIM, XPH_LIM, SPH_LIM
      INTEGER*4  IER, J1, J2, J3, J4, J5, LO_BAS, ISG(3), ISG_COR(2), &
     &           IND_FRE(2), IND_TAU(2), NOBS_TRI(3), ISTA1, ISTA2, IPL_BAS, &
     &           JPL_BAS, LB_BAS, LTR_BAS(3)
      REAL*8     CLS_XGR, CLS_SGR, CLS_XPH, CLS_SPH
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  I_LEN, NSTBA, IFIND_PL
!
! --- Calculation the number of closed trinagles
!
      L_TRI = (L_STA - 2) * (L_STA - 1) / 2
      IF ( L_STA .LT. 3 ) THEN
!
! -------- the is no sense even to try to create triangle list.
!
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
           CALL ERR_LOG ( 5341, IUER, 'MSC_CHECK', 'Error during '// &
     &         'building the list of closed triangles' )
           RETURN
      END IF
!
      IF ( L_TRI .LE. 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( IVERB .GE. 3 ) THEN
           WRITE ( 6, * ) '1. MSK_CHECK L_TRI = ',L_TRI
      END IF
!
      LO_BAS = L_BAS
!
      DO 410 J1=1,LO_BAS
         LB_BAS = 0
         DO 420 J2=1,L_TRI
!
! --------- Get signes to be used before baselines in summs which form
! --------- misclosure
!
            CALL ERR_PASS ( IUER, IER )
            CALL SIGN_TRICLS ( LIS_TRI(1,J2), L_BAS, LIS_BAS, ISG, ISG_COR, &
     &                         IND_FRE, IND_TAU, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 5342, IUER, 'MSC_CHECK', 'Error in getting '// &
     &               'signs for computing triangle misclosure' )
                 RETURN
            END IF
!
! --------- Find observations forming the j2-th triangle
!
            CALL NOUT_I4 ( 3, NOBS_TRI )
            DO 430 J3=1,L_OBS
               ISTA1 = INT4( OBSBAS(LIS_OBS(J3))%ISITE(1) )
               ISTA2 = INT4( OBSBAS(LIS_OBS(J3))%ISITE(2) )
               IPL_BAS = IFIND_PL ( L_BAS, LIS_BAS, NSTBA ( ISTA1, ISTA2 ) )
               JPL_BAS = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, &
     &                              NSTBA ( ISTA1, ISTA2 ) )
!
! ------------ Scan all three baseline slots for the triangle and fill it
! ------------ if the baseline fits the slot
!
               DO 440 J4=1,3
                  IF ( LIS_TRI(J4,J2) .EQ. IPL_BAS ) THEN
                       NOBS_TRI(J4) = LIS_OBS(J3)
                       LTR_BAS(J4)  = JPL_BAS
                  END IF
 440           CONTINUE
 430        CONTINUE
!
! --------- Check: did we find this triangle correctly??
!
            DO 450 J5=1,3
               IF ( NOBS_TRI(J5) .LT. 1           .OR. &
     &              NOBS_TRI(J5) .GT. DBOBJ%L_OBS      ) THEN
!
                    WRITE ( 6, * ) ' nobs_tri = ',nobs_tri
                    WRITE ( 6, * ) ' j2=',j2,' lis_tri(*,j2) = ',lis_tri(1,j2), &
     &                       lis_tri(2,j2), lis_tri(3,j2)
                    CALL ERR_LOG ( 5343, IUER, 'MSC_CHECK', 'Error of '// &
     &                  'internal contral: falsed triangle was detected' )
                    RETURN
               END IF
 450        CONTINUE
!
! --------- Group delay closure for X-band
!
            CLS_XGR = ISG(1)*OBSBAS(NOBS_TRI(1))%TAUGR_OBS + &
     &                ISG(2)*OBSBAS(NOBS_TRI(2))%TAUGR_OBS + &
     &                ISG(3)*OBSBAS(NOBS_TRI(3))%TAUGR_OBS
!
! --------- Fixing differences in epochs for X-band group delay
!
            CLS_XGR = CLS_XGR + &
     &                ISG_COR(1)*OBSBAS(NOBS_TRI(IND_FRE(1)))%RATE_OBS * &
     &                           OBSBAS(NOBS_TRI(IND_TAU(1)))%TAU_C + &
     &                ISG_COR(2)*OBSBAS(NOBS_TRI(IND_FRE(2)))%RATE_OBS * &
     &                           OBSBAS(NOBS_TRI(IND_TAU(2)))%TAU_C
!
! --------- Group delay closure for S-band
!
            CLS_SGR = ISG(1)*OBSBAS(NOBS_TRI(1))%TAUGR_OBS_OPP + &
     &                ISG(2)*OBSBAS(NOBS_TRI(2))%TAUGR_OBS_OPP + &
     &                ISG(3)*OBSBAS(NOBS_TRI(3))%TAUGR_OBS_OPP
!
! --------- Fixing differences in epochs for S-band group delay
!
            CLS_SGR = CLS_SGR + &
     &                ISG_COR(1)*OBSBAS(NOBS_TRI(IND_FRE(1)))%RATE_OBS * &
     &                           OBSBAS(NOBS_TRI(IND_TAU(1)))%TAU_C + &
     &                ISG_COR(2)*OBSBAS(NOBS_TRI(IND_FRE(2)))%RATE_OBS * &
     &                           OBSBAS(NOBS_TRI(IND_TAU(2)))%TAU_C
!
! --------- Phase delay closure for X-band
!
            CLS_XPH = ISG(1)*OBSBAS(NOBS_TRI(1))%TAUPH_OBS + &
     &                ISG(2)*OBSBAS(NOBS_TRI(2))%TAUPH_OBS + &
     &                ISG(3)*OBSBAS(NOBS_TRI(3))%TAUPH_OBS
!
! --------- Fixing differences in epochs for X-band group delay
!
            CLS_XPH = CLS_XPH + &
     &                ISG_COR(1)*OBSBAS(NOBS_TRI(IND_FRE(1)))%RATE_OBS * &
     &                           OBSBAS(NOBS_TRI(IND_TAU(1)))%TAU_C + &
     &                ISG_COR(2)*OBSBAS(NOBS_TRI(IND_FRE(2)))%RATE_OBS * &
     &                           OBSBAS(NOBS_TRI(IND_TAU(2)))%TAU_C
!
! --------- Phase delay closure for S-band
!
            CLS_SPH = ISG(1)*OBSBAS(NOBS_TRI(1))%TAUPH_OBS_OPP + &
     &                ISG(2)*OBSBAS(NOBS_TRI(2))%TAUPH_OBS_OPP + &
     &                ISG(3)*OBSBAS(NOBS_TRI(3))%TAUPH_OBS_OPP
!
! --------- Fixing differences in epochs for S-band group delay
!
            CLS_SPH = CLS_SPH + &
     &                ISG_COR(1)*OBSBAS(NOBS_TRI(IND_FRE(1)))%RATE_OBS * &
     &                           OBSBAS(NOBS_TRI(IND_TAU(1)))%TAU_C + &
     &                ISG_COR(2)*OBSBAS(NOBS_TRI(IND_FRE(2)))%RATE_OBS * &
     &                           OBSBAS(NOBS_TRI(IND_TAU(2)))%TAU_C
!
! --------- Check tolerance criterion for XGR
!
            IF ( DABS(CLS_XGR) .GT. XGR_LIM ) THEN
                 L_STA = 0
                 GOTO 810
            END IF
            IF ( IVERB .GE. 3 ) THEN
                 WRITE ( 6, 110 )  J2, 'XGR', CLS_XGR
 110             FORMAT ( 1X,'TRI =',I3,'  CLS_',A,'=',1PE12.5 )
            END IF
!
! --------- ... now for SGR
!
            IF ( DABS(CLS_SGR) .GT. SGR_LIM ) THEN
                 L_STA = 0
                 GOTO 810
            END IF
            IF ( IVERB .GE. 3 ) THEN
                 WRITE ( 6, 110 )  J2, 'SGR', CLS_SGR
            END IF
!
! --------- ... now for XPH
!
            IF ( DABS(CLS_XPH) .GT. XPH_LIM ) THEN
                 L_STA = 0
                 GOTO 810
            END IF
            IF ( IVERB .GE. 3 ) THEN
                 WRITE ( 6, 110 )  J2, 'XPH', CLS_XPH
            END IF
!
! --------- ... now for SPH
!
            IF ( DABS(CLS_SPH) .GT. SPH_LIM ) THEN
                 L_STA = 0
                 GOTO 810
            END IF
            IF ( IVERB .GE. 3 ) THEN
                 WRITE ( 6, 110 )  J2, 'SPH', CLS_SPH
            END IF
 420     CONTINUE
         GOTO 810
 410  CONTINUE
 810  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MSC_CHECK  #!#
