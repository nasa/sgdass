      FUNCTION SUP_UPDATE ( N_OBS, DBOBJ, OBSSCA, OBSBAS, QUALCODE_GOOD_LIM, &
     &                      IUER )
! ************************************************************************
! *                                                                      *
! *   Function  SUP_UPDATE  updates status of suppression for all        *
! *   observations in the session for 1) possible change of quality      *
! *   code limit; 2) possible change of suppression method. If the       *
! *   status: used or not used in solution be changed for at least one   *
! *   observation  SUP_UPDATE returns value .TRUE., otherwise .FALSE.    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    N_OBS ( INTEGER*4 ) -- Number of observations in the database.    *
! *   OBSSCA ( RECORD    ) -- Array of data structures which keeps       *
! *                           scan-dependent information about the       *
! *                           session.                                   *
! * QUALCODE_GOOD_LIM ( INTEGER*4 ) -- Mininal value of quality code     *
! *                           when the observation consider as a good    *
! *                           one. Letter-like quality codes ( "A", "B", *
! *                           "C", "D", "E" are considered as negative   *
! *                           and therfore bad ).                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    DBOBJ ( RECORD    ) -- Data structure which keeps general         *
! *                           information about the database such as     *
! *                           lists of the objects.                      *
! *   OBSBAS ( RECORD    ) -- Array of data structures which keeps       *
! *                           baseline dependent information about the   *
! *                           session.                                   *
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
! *  ###  30-APR-98   SUP_UPDATE   v1.2  (c)  L. Petrov  07-JUN-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'socom.i'
      LOGICAL*4  SUP_UPDATE
      INTEGER*4  N_OBS, QUALCODE_GOOD_LIM, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      INTEGER*4  M_OUT
      PARAMETER  ( M_OUT = 128 )
      INTEGER*4  J1, L_OUT, LIS_OUT(M_OUT), IQC_X, IQC_S, E_TYP, IER
      LOGICAL*4  USED_OLD, USED_NEW, CGOOD_OLD, CGOOD_NEW
      INTEGER*2  UACSUP_SAVE
      INTEGER*4  AUTO_SUP_SAVE
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
!
      IF ( DBOBJ%STATUS .NE. DBOBJ__DON ) THEN
           CALL ERR_LOG ( 6681, IUER, 'SUP_UPDATE', 'DBOBJ data structure '// &
     &         'appeared to be not initialized' )
           RETURN
      END IF
!
      SUP_UPDATE = .FALSE.
      DO 410 J1=1,DBOBJ%L_OBS
         IF ( SUPMET == SUPMET__META ) THEN
!
! ----------- Getting previous usage status
!
              USED_OLD  = META_SUPR_INQ ( OBSBAS(J1)%AUTO_SUP, &
     &                                    OBSBAS(J1)%USER_SUP, &
     &                                    OBSBAS(J1)%USER_REC, &
     &                                    USED__SPS )
!
! ----------- Getting previous status of conditionally good observation
!
              CGOOD_OLD = META_SUPR_INQ ( OBSBAS(J1)%AUTO_SUP, &
     &                                    OBSBAS(J1)%USER_SUP, &
     &                                    OBSBAS(J1)%USER_REC, &
     &                                    GOOD__SPS )
            ELSE 
              USED_OLD  = SUPR_INQ ( OBSBAS(J1)%SUPSTAT(1), OBSBAS(J1)%UACSUP, &
     &                               USED__SPS )
              CGOOD_OLD = SUPR_INQ ( OBSBAS(J1)%SUPSTAT(1), OBSBAS(J1)%UACSUP, &
     &                               GOOD__SPS )
         END IF
!
! ------ Decoding quality code for the X-band
!
         IQC_X = -1
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR, '0' ) .NE. 0 ) IQC_X =  0
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR, '1' ) .NE. 0 ) IQC_X =  1
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR, '2' ) .NE. 0 ) IQC_X =  2
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR, '3' ) .NE. 0 ) IQC_X =  3
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR, '4' ) .NE. 0 ) IQC_X =  4
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR, '5' ) .NE. 0 ) IQC_X =  5
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR, '6' ) .NE. 0 ) IQC_X =  6
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR, '7' ) .NE. 0 ) IQC_X =  7
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR, '8' ) .NE. 0 ) IQC_X =  8
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR, '9' ) .NE. 0 ) IQC_X =  9
!
! ------ Decoding quality code for the S-band
!
         IQC_S = -1
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR_OPP, '0' ) .NE. 0 ) IQC_S =  0
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR_OPP, '1' ) .NE. 0 ) IQC_S =  1
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR_OPP, '2' ) .NE. 0 ) IQC_S =  2
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR_OPP, '3' ) .NE. 0 ) IQC_S =  3
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR_OPP, '4' ) .NE. 0 ) IQC_S =  4
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR_OPP, '5' ) .NE. 0 ) IQC_S =  5
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR_OPP, '6' ) .NE. 0 ) IQC_S =  6
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR_OPP, '7' ) .NE. 0 ) IQC_S =  7
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR_OPP, '8' ) .NE. 0 ) IQC_S =  8
         IF ( INDEX ( OBSBAS(J1)%LQUAL_CHR_OPP, '9' ) .NE. 0 ) IQC_S =  9
!
! ------ Test of X-band quality code
!
         CALL SBIT ( OBSBAS(J1)%SUPSTAT, BQCX__SPS, INT2(0) )
         IF ( SUPMET == SUPMET__META ) THEN
              OBSBAS(J1)%AUTO_SUP = IBCLR ( OBSBAS(J1)%AUTO_SUP, &
     &                                      INT4(BQCX__SPS) )
         END IF
         IF ( IQC_X .LT. QUALCODE_GOOD_LIM ) THEN
              CALL SBIT ( OBSBAS(J1)%SUPSTAT, BQCX__SPS, INT2(1) )
              IF ( SUPMET == SUPMET__META ) THEN
                   OBSBAS(J1)%AUTO_SUP = IBSET ( OBSBAS(J1)%AUTO_SUP, &
     &                                           INT4(BQCX__SPS) )
              END IF
         END IF
!
! ------ Test of S-band quality code
!
         CALL SBIT ( OBSBAS(J1)%SUPSTAT, BQCS__SPS, INT2(0) )
         IF ( SUPMET == SUPMET__META ) THEN
              OBSBAS(J1)%AUTO_SUP = IBCLR ( OBSBAS(J1)%AUTO_SUP, &
     &                                      INT4(BQCS__SPS) )
         END IF
         IF ( IQC_S .LT. QUALCODE_GOOD_LIM ) THEN
              CALL SBIT ( OBSBAS(J1)%SUPSTAT, BQCS__SPS, INT2(1) )
              IF ( SUPMET == SUPMET__META ) THEN
                   OBSBAS(J1)%AUTO_SUP = IBSET ( OBSBAS(J1)%AUTO_SUP, &
     &                                           INT4(BQCS__SPS) )
              END IF
         END IF
!
! ------ Set new usage status code in according with (possibly) new
! ------ quality code limit and suppression method
!
         IF ( SUPMET .NE. SUPMET__META ) THEN
              CALL SUPUSE_SET ( OBSBAS(J1)%SUPSTAT )
         END IF
!
         IF ( SUPMET == SUPMET__META ) THEN
!
! ----------- Getting new usage status
!
              USED_NEW  = META_SUPR_INQ ( OBSBAS(J1)%AUTO_SUP, &
     &                                    OBSBAS(J1)%USER_SUP, &
     &                                    OBSBAS(J1)%USER_REC, &
     &                                    USED__SPS )
!
! ----------- Getting new status of conditionally good observation
!
              CGOOD_NEW = META_SUPR_INQ ( OBSBAS(J1)%AUTO_SUP, &
     &                                    OBSBAS(J1)%USER_SUP, &
     &                                    OBSBAS(J1)%USER_REC, &
     &                                    GOOD__SPS )
            ELSE 
              USED_NEW  = SUPR_INQ ( OBSBAS(J1)%SUPSTAT(1), OBSBAS(J1)%UACSUP, &
     &                               USED__SPS )
              CGOOD_NEW = SUPR_INQ ( OBSBAS(J1)%SUPSTAT(1), OBSBAS(J1)%UACSUP, &
     &                               GOOD__SPS )
         END IF
!
         IF ( USED_NEW  .AND.  .NOT. USED_OLD ) THEN
              UACSUP_SAVE   = OBSBAS(J1)%UACSUP
              AUTO_SUP_SAVE = OBSBAS(J1)%AUTO_SUP
!
! ----------- Usage status was changed: observation was previously bad, but
! ----------- become good
!
              CALL MILE_LIST ( N_OBS, J1, DBOBJ, OBSSCA, OBSBAS )
!
! ----------- Restoring saved UACSUP and AUTO_SUP code. 
! ----------- The true is that MILE_LIST modifies this code, 
! ----------- but we should not do it in our case now!
!
              OBSBAS(J1)%UACSUP = UACSUP_SAVE
              OBSBAS(J1)%AUTO_SUP = AUTO_SUP_SAVE 
              SUP_UPDATE = .TRUE.
            ELSE IF ( .NOT. USED_NEW  .AND.  USED_OLD ) THEN
              UACSUP_SAVE   = OBSBAS(J1)%UACSUP
              AUTO_SUP_SAVE = OBSBAS(J1)%AUTO_SUP
!
! ----------- Usage status was changed: observation was previously good, but
! ----------- become bad
!
              CALL ERR_PASS  ( IUER, IER )
              CALL ELIM_LIST ( N_OBS, J1, DBOBJ, OBSSCA, OBSBAS, E_TYP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6682, IUER, 'SUP_UPDATE', 'Error during '// &
     &                 'update of internal lists' )
                   RETURN
              END IF
!
! ----------- Restoring saved UACSUP and AUTO_SUP codes. 
! ----------- The true is that ELIM_LIST modifies this code, 
! ----------- but we should not do it in our case now!
!
              OBSBAS(J1)%UACSUP   = UACSUP_SAVE
              OBSBAS(J1)%AUTO_SUP = AUTO_SUP_SAVE 
              SUP_UPDATE = .TRUE.
         END IF
!
         IF ( CGOOD_NEW  .AND.  .NOT. CGOOD_OLD ) THEN
!
! ----------- Conditionally good status was changed: observation was previously
! ----------- conditionally bad but become conditionally good
!
              DBOBJ%CG_OBS = DBOBJ%CG_OBS + 1
              DBOBJ%R_OBS  = DBOBJ%R_OBS + 1
            ELSE IF ( .NOT. CGOOD_NEW  .AND.  CGOOD_OLD ) THEN
!
! ----------- Conditionally good status was changed: observation was previously
! ----------- conditionally good but become conditionally bad
!
              DBOBJ%CG_OBS = DBOBJ%CG_OBS - 1
              DBOBJ%R_OBS  = DBOBJ%R_OBS - 1
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  SUP_UPDATE  !#!#
