      FUNCTION   ELIM_LIST ( N_OBS, IOBS, DBOBJ, OBSSCA, OBSBAS, E_TYP, IUER )
! ************************************************************************
! *                                                                      *
! *   Updating the lists for the elimination of an observation. If the   *
! *   number of observations made at the certain station or the certain  *
! *   source appears less than specified limit than all observation of   *
! *   that source or station will be rejected. ELIM_LIST returns the     *
! *   code of such a situation and the list of eliminated objects.       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    N_OBS ( INTEGER*4 ) -- Number of observations.                    *
! *    IOBS  ( INTEGER*4 ) -- Index of the eliminated observatoin in the *
! *                           database.                                  *
! *   OBSSCA ( RECORD    ) -- Array of data structures which keeps       *
! *                           scan-dependent information about the       *
! *                           session.                                   *
! *   OBSBAS ( RECORD    ) -- Array of data structures which keeps       *
! *                           baseline dependent information about the   *
! *                           session.                                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    E_TYP ( INTEGER*4 ) -- Type of the object which was removed from  *
! *                           used objects. Acceptable values:           *
! *                           E__UNF -- "Undefined" -- no object has     *
! *                                     been eliminated entirely.        *
! *                           E__SOU -- "Source"                         *
! *                           E__STA1 - "Station_1"                      *
! *                           E__STA2 - "Station_2"                      *
! *                           E__BAS -- "Baseline"                       *
! *                           Mentioned above symbolic names are defined *
! *                           in ../includes/obser.i                     *
! * <ELIM_TYP> ( INTEGER*4 )  -- Type of the object which was removed    *
! *                           from the list of estimated object.         *
! *                           Possible values are the same as for E_TYP. *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    DBOBJ ( RECORD    ) -- Data structure which keeps general         *
! *                           information about the database such as     *
! *                           lists of the objects.                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  24-SEP-97   ELIM_LIST   v3.1  (c)  L. Petrov  07-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INTEGER*4  ELIM_LIST, N_OBS, IOBS, E_TYP, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
!
      INTEGER*4  ISC, ISOU, ISTA1, ISTA2, IBAS, J1, J2, J3, J4
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: IFIND_PL, NSTBA
!
! --- Setting flag: manually suppress the observation
!
      IF ( SUPMET == SUPMET__META ) THEN
           OBSBAS(IOBS)%USER_SUP = IBSET ( OBSBAS(IOBS)%USER_SUP, &
     &                                     INT4(DBOBJ%IDATYP) )
         ELSE 
           CALL SUPR_OBS ( DBOBJ%IDATYP, OBSBAS(IOBS)%SUPSTAT, OBSBAS(IOBS)%UACSUP )
      END IF
!
! --- Decreasing the counter of used observations and increasing the counter
! --- of rejected ones
!
      DBOBJ%U_OBS = DBOBJ%U_OBS - 1
      DBOBJ%R_OBS = DBOBJ%R_OBS + 1
      E_TYP = E__UNF
      ELIM_LIST = E__UNF
!
      ISC = OBSBAS(IOBS)%IND_SCA
!
! --- Decreasing the number of observations for the source observed in
! --- an eliminated observation
!
      ISOU = IFIND_PL ( DBOBJ%U_SOU, DBOBJ%UIS_SOU, INT4(OBSSCA(ISC)%ISTAR) )
      DBOBJ%KU_SOU(ISOU) = DBOBJ%KU_SOU(ISOU)-1
      IF ( DBOBJ%KU_SOU(ISOU) .LT. 1 ) THEN
!
! -------- All observations of the source has been suppressed
!
! -------- Decreasing the source list
!
           IF ( ISOU .LT. DBOBJ%U_SOU ) THEN
                DO 410 J1=ISOU,DBOBJ%U_SOU-1
                   DBOBJ%UIS_SOU(J1) = DBOBJ%UIS_SOU(J1+1)
                   DBOBJ%KU_SOU(J1)  = DBOBJ%KU_SOU(J1+1)
 410            CONTINUE
           END IF
!
           DBOBJ%U_SOU = DBOBJ%U_SOU - 1
           E_TYP = E__SOU
      END IF
!
! --- Decreasing the number of observations for the baseline where
! --- an eliminated observation took place
!
      IBAS = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, &
     &       NSTBA( INT4(OBSBAS(IOBS)%ISITE(1)), INT4(OBSBAS(IOBS)%ISITE(2)) ) )
      DBOBJ%KU_BAS(IBAS) = DBOBJ%KU_BAS(IBAS)-1
      IF ( DBOBJ%KU_BAS(IBAS) .LT. 1 ) THEN
!
! -------- All observatiuons at the baseline has been suppressed
!
           IF ( IBAS .LT. DBOBJ%U_BAS ) THEN
                DO 420 J2=IBAS,DBOBJ%U_BAS-1
                   DBOBJ%UIS_BAS(J2) = DBOBJ%UIS_BAS(J2+1)
                   DBOBJ%KU_BAS(J2)  = DBOBJ%KU_BAS(J2+1)
 420            CONTINUE
           END IF
!
           DBOBJ%U_BAS = DBOBJ%U_BAS - 1
           E_TYP = E__BAS
      END IF
!
! --- Decreasing the number of observations for the first station of the
! --- baseline where an eliminated observation took place
!
      ISTA1 = IFIND_PL ( DBOBJ%U_STA, DBOBJ%UIS_STA, &
     &                   INT4(OBSBAS(IOBS)%ISITE(1)) )
      DBOBJ%KU_STA(ISTA1) = DBOBJ%KU_STA(ISTA1)-1
      IF ( DBOBJ%KU_STA(ISTA1) .LT. 1 ) THEN
!
! -------- All observations at the first station of the baseline appeared to
! -------- be deselected
!
           IF ( ISTA1 .LT. DBOBJ%U_STA ) THEN
                DO 430 J3=ISTA1,DBOBJ%U_STA-1
                   DBOBJ%UIS_STA(J3) = DBOBJ%UIS_STA(J3+1)
                   DBOBJ%KU_STA(J3)  = DBOBJ%KU_STA(J3+1)
 430            CONTINUE
           END IF
!
           DBOBJ%U_STA = DBOBJ%U_STA - 1
           E_TYP = E__STA1
      END IF
!
! --- Decreasing the number of observations for the second station of the
! --- baseline where an eliminated observation took place
!
      ISTA2 = IFIND_PL ( DBOBJ%U_STA, DBOBJ%UIS_STA, &
     &                   INT4(OBSBAS(IOBS)%ISITE(2)) )
      DBOBJ%KU_STA(ISTA2) = DBOBJ%KU_STA(ISTA2)-1
      IF ( DBOBJ%KU_STA(ISTA2) .LT. 1 ) THEN
!
! -------- All observations at the second station of the baseline appeared to
! -------- be deselected
!
           IF ( ISTA2 .LT. DBOBJ%U_STA ) THEN
                DO 440 J4=ISTA2,DBOBJ%U_STA-1
                   DBOBJ%UIS_STA(J4) = DBOBJ%UIS_STA(J4+1)
                    DBOBJ%KU_STA(J4) = DBOBJ%KU_STA(J4+1)
 440            CONTINUE
           END IF
!
           DBOBJ%U_STA = DBOBJ%U_STA - 1
           E_TYP = E__STA2
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ELIM_LIST  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MILE_LIST ( N_OBS, IOBS, DBOBJ, OBSSCA, OBSBAS )
! ************************************************************************
! *                                                                      *
! *   Updating the lists for the restoration of an observation.          *
! *   The number of observation of the object participated in the        *
! *   IOBS-th observations will be incremented.                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    N_OBS ( INTEGER*4 ) -- Number of observations.                    *
! *    IOBS  ( INTEGER*4 ) -- Index of the restored observatoin in the   *
! *                           database.                                  *
! *   OBSSCA ( RECORD    ) -- Array of data structures which keeps       *
! *                           scan-dependent information about the       *
! *                           session.                                   *
! *   OBSBAS ( RECORD    ) -- Array of data structures which keeps       *
! *                           baseline dependent information about the   *
! *                           session.                                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    DBOBJ ( RECORD    ) -- Data structure which keeps general         *
! *                           information about the database such as     *
! *                           lists of the objects.                      *
! *                                                                      *
! *  ###  25-SEP-97   MILE_LIST   v2.2  (c)  L. Petrov  07-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      INCLUDE   'fast.i'
      INCLUDE   'ncrec.i'
      INTEGER*4  N_OBS, IOBS, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
!
      INTEGER*4  ISC, ISOU, ISTA1, ISTA2, IBAS, J1, J2
      LOGICAL*4  F_RC
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: IFIND_PL, NSTBA
      LOGICAL*4, EXTERNAL :: RECV_OBS, META_SUPR_INQ
!
! --- Setting flag: lifting manually suppression of the observation what
! --- rehabilitate it wholly in the rights to participate in estimation
!
      IF ( SUPMET == SUPMET__META ) THEN
           F_RC = META_SUPR_INQ ( OBSBAS(IOBS)%AUTO_SUP, OBSBAS(IOBS)%USER_SUP, &
     &                            OBSBAS(IOBS)%USER_REC, RECO__SPS )
           IF ( F_RC ) THEN
                OBSBAS(IOBS)%USER_SUP = IBCLR ( OBSBAS(IOBS)%USER_SUP, &
     &                                          INT4(DBOBJ%IDATYP) )
           END IF
         ELSE 
           F_RC = RECV_OBS( DBOBJ%IDATYP, OBSBAS(IOBS)%SUPSTAT, &
     &                      OBSBAS(IOBS)%UACSUP )
      END IF
!
      IF ( .NOT. F_RC ) THEN
!
! -------- Unsupported case: observation was unrecoverable. Well, the only
! -------- thing we are able to do is to exit
!
           RETURN
      END IF
!
! --- Increasing the counter of used observations and decreasing the counter
! --- of rejected ones
!
      DBOBJ%U_OBS = DBOBJ%U_OBS + 1
      DBOBJ%R_OBS = DBOBJ%R_OBS - 1
!
      ISC = OBSBAS(IOBS)%IND_SCA
!
! --- Increasing the number of observations for the source observed in
! --- the restored observation
!
      ISOU = IFIND_PL ( DBOBJ%U_SOU, DBOBJ%UIS_SOU, INT4(OBSSCA(ISC)%ISTAR) )
      IF ( ISOU .GT. 0 ) THEN
           DBOBJ%KU_SOU(ISOU) = DBOBJ%KU_SOU(ISOU)+1
         ELSE
!
! -------- If there were no any observations of this source used in the
! -------- estimation we add this source to the end of the source list
!
           DBOBJ%U_SOU = DBOBJ%U_SOU + 1
           ISOU = DBOBJ%U_SOU
           DBOBJ%UIS_SOU(ISOU) = INT4(OBSSCA(ISC)%ISTAR)
           DBOBJ%KU_SOU(ISOU)  = 1
!
! -------- And then sort the list
!
           CALL SORT_I2 ( DBOBJ%U_SOU, DBOBJ%UIS_SOU, DBOBJ%KU_SOU )
      END IF
!
! --- Increasing the number of observations for the baseline where
! --- the restored observation took place
!
      IBAS = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, &
     &       NSTBA( INT4(OBSBAS(IOBS)%ISITE(1)), INT4(OBSBAS(IOBS)%ISITE(2)) ) )
      IF ( IBAS .GT. 0 ) THEN
           DBOBJ%KU_BAS(IBAS) = DBOBJ%KU_BAS(IBAS)+1
         ELSE
!
! -------- If there were no any observations at this baseline used in the
! -------- estimation we add this baseline to the end of the baselines list
!
           DBOBJ%U_BAS = DBOBJ%U_BAS + 1
           IBAS = DBOBJ%U_BAS
           DBOBJ%UIS_BAS(IBAS) = NSTBA( INT4(OBSBAS(IOBS)%ISITE(1)), &
     &                                  INT4(OBSBAS(IOBS)%ISITE(2))  )
           DBOBJ%KU_BAS(IBAS)  = 1
!
! -------- And then sort the list
!
           DO 410 J1=1,DBOBJ%U_BAS
               DBOBJ%KU_BAS(J1) = 100000*ABS(DBOBJ%UIS_BAS(J1)) + &
     &                                       DBOBJ%KU_BAS(J1)
  410      CONTINUE
           CALL SORT_I2 ( DBOBJ%U_BAS, DBOBJ%KU_BAS, DBOBJ%UIS_BAS )
           DO 420 J2=1,DBOBJ%U_BAS
              DBOBJ%KU_BAS(J2) = DBOBJ%KU_BAS(J2) - &
     &                           100000*ABS(DBOBJ%UIS_BAS(J2))
  420      CONTINUE
      END IF
!
! --- Increasing the number of observations for the first station of the
! --- baseline where the restored observation took place
!
      ISTA1 = IFIND_PL ( DBOBJ%U_STA, DBOBJ%UIS_STA, &
     &                   INT4(OBSBAS(IOBS)%ISITE(1)) )
      IF ( ISTA1 .GT. 0 ) THEN
           DBOBJ%KU_STA(ISTA1) = DBOBJ%KU_STA(ISTA1)+1
         ELSE
!
! -------- If there were no any observations made at this stations used in the
! -------- estimation we add this stations to the end of the stations list
!
           DBOBJ%U_STA = DBOBJ%U_STA + 1
           ISTA1 = DBOBJ%U_STA
           DBOBJ%UIS_STA(ISTA1) = INT4(OBSBAS(IOBS)%ISITE(1))
           DBOBJ%KU_STA(ISTA1)  = 1
!
! -------- And then sort the list
!
           CALL SORT_I2 ( DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%KU_STA )
      END IF
!
! --- Increasing the number of observations for the second station of the
! --- baseline where the restored observation took place
!
      ISTA2 = IFIND_PL ( DBOBJ%U_STA, DBOBJ%UIS_STA, &
     &                   INT4(OBSBAS(IOBS)%ISITE(2)) )
      IF ( ISTA2 .GT. 0 ) THEN
           DBOBJ%KU_STA(ISTA2) = DBOBJ%KU_STA(ISTA2)+1
         ELSE
!
! -------- If there were no any observations made at this station used in the
! -------- estimation we add this stations to the end of the list
!
           DBOBJ%U_STA = DBOBJ%U_STA + 1
           ISTA2 = DBOBJ%U_STA
           DBOBJ%UIS_STA(ISTA2) = INT4(OBSBAS(IOBS)%ISITE(2))
           DBOBJ%KU_STA(ISTA2)  = 1
!
! -------- And then sort the list
!
           CALL SORT_I2 ( DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%KU_STA )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MILE_LIST  #!#
