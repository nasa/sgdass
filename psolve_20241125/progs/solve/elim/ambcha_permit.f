      FUNCTION   AMBCHA_PERMIT ( IOBS, DBOBJ, OBSBAS, MP_OBS, LP_OBS, &
     &                           LPIS_OBS, ISGN_OBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine  AMBCHA_PERMIT  checks: can we change ambiguity  *
! *   for IOBS-th observation. We check all observations at the same     *
! *   scan as the IOBS-th and verify all observations at the stations    *
! *   which participated at the IOBS-th observation. If there is at      *
! *   least one used (not suppressed) observation among them then        *
! *   AMBCHA_PERMIT = .FALSE., otherwise .TRUE.  We also calculate the   *
! *   list of indices of the observations whose ambiguities have to be   *
! *   changed as a result of changes ambiguities of the IOBS-th          *
! *   observation in order to conserve misclosure of observables and     *
! *   an array of signs (+1 or -1) which should be put before the        *
! *   ambiguity when ambiguity correction will be applied.               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     IOBS ( INTEGER*4 ) -- Index of the observation under             *
! *                           consideration.                             *
! *    DBOBJ ( RECORD    ) -- Data structure which keeps general         *
! *                           information about the database such as     *
! *                           lists of the objects.                      *
! *   OBSBAS ( RECORD    ) -- Array of data structures which keeps       *
! *                           baseline dependent information about the   *
! *                           session.                                   *
! *   MP_OBS ( INTEGER*4 ) -- Expected maximal number of observations    *
! *                           at the scan under consideration.           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   LP_OBS ( INTEGER*4 ) -- Number of observations whose ambiguities   *
! *                           should be changed as a result of changes   *
! *                           of the ambiguities of the IOBS-th          *
! *                           observation. LP_OBS=0 if ambiguity change  *
! *                           is not permitted.                          *
! * LPIS_OBS ( INTEGER*4 ) -- List of indices of the observations whose  *
! *                           ambiguities should be changed as a result  *
! *                           of change ambiguities of the IOBS-th       *
! *                           observation in order to conserve           *
! *                           misclosures. IOBS-th observation belongs   *
! *                           to the list. Used dimension: LP_OBS.       *
! * ISGN_OBS ( INTEGER*4 ) -- Array of signs (-1 or +1) which should be  *
! *                           applied to the ambiguity changes for the   *
! *                           observations from the list LPIS_OBS.       *
! *                           AMB_NEW(k)=AMB_OLD(k) + ISGN_OBS(k)*NUMAMB *
! *                           where NUMAMB change in ambiguity.          *
! *                           Used dimension: LP_OBS.                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  03-NOV-98  AMBCHA_PERMIT  v1.1  (c) L. Petrov  07-JUN-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INTEGER*4  IOBS, MP_OBS, LP_OBS, LPIS_OBS(MP_OBS), ISGN_OBS(MP_OBS), IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(DBOBJ%L_OBS)
      INTEGER*2  ISCA_IOBS
      CHARACTER  STR*32
      LOGICAL*4  AMBCHA_PERMIT
      INTEGER*4  J1, J2, J3, IS, ISTA1, ISTA2, ISTA_REF, L_OBS, LIS_OBS(MO_BAS)
      LOGICAL*4  FL_USED
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
!
      IF ( MP_OBS .LT. 1  .OR.  MP_OBS .GT. 8192 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MP_OBS, STR )
           CALL ERR_LOG ( 6761, IUER, 'AMBCHA_PERMIT', 'Incorrect parameter '// &
     &         'MP_OBS: '//STR )
           RETURN
      END IF
!
! --- Initialization. Without special purpose...
!
      CALL NOUT_I4 ( MP_OBS, LPIS_OBS )
      CALL NOUT_I4 ( MP_OBS, ISGN_OBS )
!
      LP_OBS = 0
      AMBCHA_PERMIT = .FALSE.
!
      IF ( IOBS .LT. 1  .OR.  IOBS .GT. DBOBJ%L_OBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOBS, STR )
           CALL ERR_LOG ( 6762, IUER, 'AMBCHA_PERMIT', 'Incorrect parameter '// &
     &         'IOBS: '//STR )
           RETURN
      END IF
!
! --- First gather the list of observations of the same scan as IOBS-th one.
!
      ISCA_IOBS = OBSBAS(IOBS)%IND_SCA
      L_OBS = 0
      DO 410 J1=1,DBOBJ%L_OBS
         IF ( OBSBAS(J1)%IND_SCA .EQ. ISCA_IOBS ) THEN
!
! ----------- Add the J1-th observation to the list of observations of the scan
!
              CALL ADD_LIS ( MO_BAS, L_OBS, LIS_OBS, J1, -3 )
         END IF
 410  CONTINUE
!
      IF ( L_OBS .EQ. 1 ) THEN
!
! -------- It was only one observation at this scan. Nothing to do more!
!
           AMBCHA_PERMIT = .TRUE.
           LP_OBS = 1
           LPIS_OBS(LP_OBS) = IOBS
           ISGN_OBS(LP_OBS) = 1
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Now check two possibilities:
! --- 1) check all observations: whether the station participated which is the
! ---    first station of the IOBS-th observation
! --- 2) check all observations: whether the station participated which is the
! ---    second station of the IOBS-th observation
!
      DO 420 J2=1,2
!
! ------ Station to be checked
!
         ISTA_REF = INT4( OBSBAS(IOBS)%ISITE(J2))
         LP_OBS = 0
!
! ------ Set the main signs
!
         IF ( J2 .EQ. 1 ) IS =  1
         IF ( J2 .EQ. 2 ) IS = -1
         DO 430 J3=1,L_OBS
            IF ( LIS_OBS(J3) .EQ. IOBS ) THEN
                 CALL ADD_LIS ( MP_OBS, LP_OBS, LPIS_OBS, LIS_OBS(J3), -3 )
                 ISGN_OBS(LP_OBS) = 1
                 GOTO 430
            END IF
!
            ISTA1 = INT4( OBSBAS(LIS_OBS(J3))%ISITE(1))
            ISTA2 = INT4( OBSBAS(LIS_OBS(J3))%ISITE(2))
!
            IF ( SUPMET == SUPMET__META ) THEN
                 FL_USED = META_SUPR_INQ ( OBSBAS(LIS_OBS(J3))%AUTO_SUP, &
     &                                     OBSBAS(LIS_OBS(J3))%USER_SUP, &
     &                                     OBSBAS(LIS_OBS(J3))%USER_REC, &
     &                                     USED__SPS )
               ELSE 
                 FL_USED = SUPR_INQ ( OBSBAS(LIS_OBS(J3))%SUPSTAT(1), &
     &                                OBSBAS(LIS_OBS(J3))%UACSUP, USED__SPS )
            END IF
            IF ( ISTA1 .EQ. ISTA_REF ) THEN
!
! -------------- If the observation was used then good bye.
!
                 IF ( FL_USED ) GOTO 420
!
! -------------- ... but if it was not used -- then add it to the list and
! -------------- keep its sign
!
                 CALL ADD_LIS ( MP_OBS, LP_OBS, LPIS_OBS, LIS_OBS(J3), -3 )
                 ISGN_OBS(LP_OBS) = IS
            END IF
            IF ( ISTA2 .EQ. ISTA_REF ) THEN
!
! -------------- If the observation was used then Good bye.
!
                 IF ( FL_USED ) GOTO 420
!
! -------------- ... but if it was not used -- then add it to the list and
! -------------- keep its sign
!
                 CALL ADD_LIS ( MP_OBS, LP_OBS, LPIS_OBS, LIS_OBS(J3), -3 )
                 ISGN_OBS(LP_OBS) = -IS
            END IF
 430     CONTINUE
!
! ------ We came through all checks. Our conclusion: there is a possibility to
! ------ change ambiguity for the IOBS-th observation
!
         AMBCHA_PERMIT = .TRUE.
         CALL ERR_LOG ( 0, IUER )
         RETURN
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  AMBCHA_PERMIT  #!#
