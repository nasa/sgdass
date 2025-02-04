      SUBROUTINE UPDATE_SUPSTAT()
! ************************************************************************
! *                                                                      *
! *   Routine  UPDATE_SUPSTAT
! *                                                                      *
! *  ### 11-JUN-2010 UPDATE_SUPSTAT v1.2 (c)  L. Petrov  31-MAR-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'oborg.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
!
      INTEGER*2  LDBNAM(5,15), IDBV(15)
      INTEGER*4  IDBE(15), NOBS, J1, J2, J3, J4
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: BAD_OBS, DATYP_INQ
      INTEGER*2 INT2_ARG
      INTEGER*4 INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
      NOBS = IDBE(1)
      CALL ACS_OBSFIL ( 'O' )
      DO 410 J1=1,NOBS
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
!
! ------ Clear the bit related to SNR
!
         CALL SBIT ( AUTO_SUP, LSNR__SPS, INT2(0) )
         CALL SBIT ( SUPSTAT,  LSNR__SPS, INT2(0) )
!
         IF ( SUPMET == SUPMET__META ) THEN
              IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) ) THEN
                   IF ( SNR < SNR_MIN_X ) THEN
                        CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
                        AUTO_SUP = IBSET ( AUTO_SUP, INT4(LSNR__SPS) )
                        USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
                     ELSE
                        CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
                        AUTO_SUP = IBCLR ( AUTO_SUP, INT4(LSNR__SPS) )
                   END IF
              END IF
!
             IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) ) THEN
                  IF ( SNR_S < SNR_MIN_S  .AND.          &
     &                 KBIT ( OPP_STATUS, OPP_SET1__BIT  )  ) THEN
!
                       CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
                       AUTO_SUP = IBSET ( AUTO_SUP, INT4(LSNR__SPS) )
                       USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
                     ELSE
                       CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
                       AUTO_SUP = IBCLR ( AUTO_SUP, INT4(LSNR__SPS) )
                  END IF
             END IF
!
             IF ( DATYP_INQ ( IDATYP, COMB__DTP  )      ) THEN
                  IF ( SNR < SNR_MIN_X  .OR.  SNR_S < SNR_MIN_S ) THEN
                       CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
                       AUTO_SUP = IBSET ( AUTO_SUP, INT4(LSNR__SPS) )
                       USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
                     ELSE
                       CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
                       AUTO_SUP = IBCLR ( AUTO_SUP, INT4(LSNR__SPS) )
                  END IF
             END IF
             IF ( DATYP_INQ ( IDATYP, FUSED__DTP  ) ) THEN
                  IF ( ( SNR_MIN_X > 0.0  .AND.  SNR   < SNR_MIN_X ) .AND. &
     &                 ( SNR_MIN_S > 0.0  .AND.  SNR_S < SNR_MIN_S )       ) THEN
                       CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
                       AUTO_SUP = IBSET ( AUTO_SUP, INT4(LSNR__SPS) )
                       USER_SUP = IBSET ( USER_SUP, INT4(IDATYP) )
                     ELSE               
                       CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
                       AUTO_SUP = IBCLR ( AUTO_SUP, INT4(LSNR__SPS) )
                  END IF
             END IF
             CALL AUTO_SUP_UPD ( ISITE, ISTAR, ELEV, AUTO_SUP )
           ELSE
             IF ( DATYP_INQ ( IDATYP, XBAND__DTP ) .OR. &
     &            DATYP_INQ ( IDATYP, COMB__DTP  )      ) THEN
                  IF ( SNR < SNR_MIN_X ) THEN
                       CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
                     ELSE
                       IF ( .NOT. BAD_OBS ( LQUAL_CHR ) ) THEN
                            CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
                       END IF
                  END IF
             END IF
!
             IF ( DATYP_INQ ( IDATYP, SBAND__DTP ) .OR. &
     &            DATYP_INQ ( IDATYP, COMB__DTP  )      ) THEN
                  IF ( SNR_S < SNR_MIN_S  .AND.             &
     &                 KBIT ( OPP_STATUS, OPP_SET1__BIT  )  ) THEN
                       CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(1) )
                     ELSE
                       IF ( .NOT. BAD_OBS ( LQUALXS_CHR ) ) THEN
                            CALL SBIT ( SUPSTAT, LSNR__SPS, INT2(0) )
                       END IF
                  END IF
             END IF
             CALL SUPUSE_SET ( SUPSTAT )
         END IF
         CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
 410  CONTINUE
      CALL ACS_OBSFIL ( 'C' )
!
      RETURN
      END  SUBROUTINE  UPDATE_SUPSTAT  !#!#
