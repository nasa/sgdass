      SUBROUTINE SESTAT_OBS ( DBOBJ, ISTAR_I2, ISITE_I2, SUPMET, &
     &                        SUPSTAT, UACSUP, AUTO_SUP, USER_SUP, &
     &                        USER_REC, IUER )
! ************************************************************************
! *                                                                      *
! *   Procedure  SESTAT_OBS  updates data structure of database          *
! *   statistics for the next observation of the session.                *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *   ISTAR_I2 ( INTEGER*2 ) -- Index of the source.                     *
! *   ISITE_I2 ( INTEGER*2 ) -- Array of length 2 with indices of the    *
! *                             stations of the baseline.                *
! *     SUPMET ( INTEGER*2 ) -- Suppression method.                      *
! *    SUPSTAT ( INTEGER*2 ) -- 32-bits field of suppression status      *
! *                             which is set automatically in according  *
! *                             with circumstances of the observation    *
! *                             and  global settings (such as quality    *
! *                             code limit, applying ionosphere          *
! *                             calibration, etc. )                      *
! *     UACSUP ( INTEGER*2 ) -- 16-bits field of user action for         *
! *                             suppression.                             *
! *   AUTO_SUP ( INTEGER*4 ) -- Automatic suppression status.            *
! *   USER_SUP ( INTEGER*4 ) -- User action for suppression.             *
! *   USER_REC ( INTEGER*4 ) -- User action for recovery.                *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
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
! *  ###  07-JUL-98   SESTAT_OBS   v2.0  (c)  L. Petrov 06-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'solve.i'
      INCLUDE    'obser.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      INTEGER*2  ISTAR_I2, ISITE_I2(2), SUPMET, SUPSTAT(2), UACSUP
      INTEGER*4  AUTO_SUP, USER_SUP, USER_REC
      INTEGER*4  IUER, IER
      LOGICAL*4  FL_USED, FL_RECO, FL_GOOD
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: NSTBA
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
!
! --- Determine: is this observation used?
!
      IF ( SUPMET == SUPMET__META ) THEN
           FL_GOOD = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, GOOD__SPS )
           FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
           FL_RECO = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, RECO__SPS )
         ELSE 
           FL_GOOD = SUPR_INQ ( SUPSTAT, UACSUP, GOOD__SPS )
           FL_USED = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS )
           FL_RECO = SUPR_INQ ( SUPSTAT, UACSUP, RECO__SPS )
      END IF
!
! --- Now updating list of observed sources among all observations
!
      CALL ERR_PASS ( IUER, IER )
      CALL ADC_LIS  ( MO_SOU, DBOBJ%L_SOU, DBOBJ%LIS_SOU, DBOBJ%KL_SOU, &
     &                INT4(ISTAR_I2), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8411, IUER, 'SESTAT_OBS', 'Error in adding a '// &
     &         'source to the list of sources' )
           RETURN
      END IF
      IF ( FL_USED ) THEN
!
! -------- ... and among used observations
!
           CALL ADC_LIS ( MO_SOU, DBOBJ%U_SOU, DBOBJ%UIS_SOU, DBOBJ%KU_SOU, &
     &                    INT4(ISTAR_I2), IER )
      END IF
!
! --- Add the first station to the list of stations
!
      CALL ERR_PASS ( IUER, IER )
      CALL ADC_LIS  ( MO_STA, DBOBJ%L_STA, DBOBJ%LIS_STA, DBOBJ%KL_STA, &
     &                INT4(ISITE_I2(1)), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8412, IUER, 'SESTAT_OBS', 'Error in adding a '// &
     &         'station to the list of stations' )
           RETURN
      END IF
      IF ( FL_USED ) THEN
!
! -------- ... and among used observations
!
           CALL ADC_LIS ( MO_STA, DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%KU_STA, &
     &                    INT4(ISITE_I2(1)), IER )
      END IF
!
! --- Add the second station to the list of stations
!
      CALL ERR_PASS ( IUER, IER )
      CALL ADC_LIS  ( MO_STA, DBOBJ%L_STA, DBOBJ%LIS_STA, DBOBJ%KL_STA, &
     &                INT4(ISITE_I2(2)), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8413, IUER, 'SESTAT_OBS', 'Error in adding a '// &
     &         'station to the list of stations' )
           RETURN
      END IF
      IF ( FL_USED ) THEN
!
! -------- ... and among used observations
!
           CALL ADC_LIS ( MO_STA, DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%KU_STA, &
     &                    INT4(ISITE_I2(2)), IER )
      END IF
!
! --- Add the baseline code to the list of baselines
!
      CALL ERR_PASS ( IUER, IER )
      CALL ADC_LIS  ( MO_BAS, DBOBJ%L_BAS, DBOBJ%LIS_BAS, DBOBJ%KL_BAS, &
     &                NSTBA ( INT4(ISITE_I2(1)), INT4(ISITE_I2(2)) ), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8414, IUER, 'SESTAT_OBS', 'Error in adding a '// &
     &         'baseline to the list of baselines' )
           RETURN
      END IF
      IF ( FL_USED ) THEN
!
! -------- ... and among used observations
!
           CALL ADC_LIS ( MO_BAS, DBOBJ%U_BAS, DBOBJ%UIS_BAS, DBOBJ%KU_BAS, &
     &                    NSTBA ( INT4(ISITE_I2(1)), INT4(ISITE_I2(2)) ), IER )
      END IF
!
! --- Increment various observation counters
!
      DBOBJ%L_OBS = DBOBJ%L_OBS + 1
      IF ( FL_USED ) THEN
           DBOBJ%U_OBS = DBOBJ%U_OBS + 1
         ELSE IF ( FL_RECO ) THEN
           DBOBJ%R_OBS = DBOBJ%R_OBS + 1
      END IF
      IF ( FL_GOOD ) THEN
           DBOBJ%CG_OBS = DBOBJ%CG_OBS + 1
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SESTAT_OBS  #!#
