      SUBROUTINE INIT_META_SUP ( )
! ************************************************************************
! *                                                                      *
! *   Routine INIT_META_SUP 
! *                                                                      *
! *  ### 06-JUN-2007 INIT_META_SUP v1.2 (c)  L. Petrov  30-MAR-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'oborg.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbcm.i'
!
      INTEGER*4  JSITN(4,MAX_ARC_STA)
      INTEGER*2  JSITI(MAX_ARC_STA), JCAPPL(MAX_ARC_STA), &
     &           ITT(MAX_ARC_STA), ITTB(MAX_ARC_BSL)
      REAL*8     LATS(MAX_ARC_STA),      HEIGHTS(MAX_ARC_STA)
      REAL*8     BARO_CALS(MAX_ARC_STA), BARO_HEIGHTS(MAX_ARC_STA)
      REAL*8     APP(2,2), AX_OFFS(MAX_ARC_STA)
      REAL*8     ET(2,MAX_ARC_BSL), SE(MAX_ARC_STA), SS(MAX_ARC_SRC)
      INTEGER*2  JCAFFL(7,MAX_ARC_STA), NFCAL, NAMSTA, NPARAM_OLD
      INTEGER*2  OBCAPL, MCAPL, IDATYP_SAVE, SUPMET_SAVE, SUPSTAT_SAVE(2) 
      INTEGER*2  AX_TYPES(MAX_ARC_STA), JCAVAL(MAX_ARC_STA), NOGOOD
      CHARACTER  FCAL_NAMES(112)*8
      INTEGER*4  J1, J2, J3, IQC_X, IQC_S
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: SUPR_INQ
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      CALL USE_GLBFIL_4 ( 'OR' )
      CALL NCORT ( JSITN, JSITI, JCAPPL, NUMSTA, ITT, INT2(1), &
     &             IDATYP, ITTB, ET, SE, SS, OBCAPL, MCAPL, JCAVAL, &
     &             LATS, HEIGHTS, AX_TYPES, AX_OFFS, BARO_CALS, &
     &             BARO_HEIGHTS, JCAFFL, FCAL_NAMES, NFCAL, NAMSTA, &
     &             CALCV )
      CALL USE_GLBFIL_4 ( 'WC' )
      CALL ACS_OBSFIL ( 'O' )
      IDATYP_SAVE = IDATYP
      DO 410 J1=1,NUMOBS
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
!
         AUTO_SUP = 0
         USER_SUP = 0
         USER_REC = 0
!         
         CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, ICORR, GIONSG, &
     &                      PHIONS, IWVBIT1, ISITE, JSITI, ITT, ISTAR, &
     &                      ELEV, KIONO, SNR, SNR_S, SUPSTAT, UACSUP )
         DO 420 J2=1,WPAS__SPS
            IF ( KBIT ( SUPSTAT, INT2(J2) ) ) THEN
                 AUTO_SUP = IBSET ( AUTO_SUP, J2 )
            END IF
 420     CONTINUE 
!
! ------ Set again fields NOFX, BQCX
!
         IQC_X = -1
         IF ( INDEX ( LQUAL_CHR, '0' ) .NE. 0 ) IQC_X =  0
         IF ( INDEX ( LQUAL_CHR, '1' ) .NE. 0 ) IQC_X =  1
         IF ( INDEX ( LQUAL_CHR, '2' ) .NE. 0 ) IQC_X =  2
         IF ( INDEX ( LQUAL_CHR, '3' ) .NE. 0 ) IQC_X =  3
         IF ( INDEX ( LQUAL_CHR, '4' ) .NE. 0 ) IQC_X =  4
         IF ( INDEX ( LQUAL_CHR, '5' ) .NE. 0 ) IQC_X =  5
         IF ( INDEX ( LQUAL_CHR, '6' ) .NE. 0 ) IQC_X =  6
         IF ( INDEX ( LQUAL_CHR, '7' ) .NE. 0 ) IQC_X =  7
         IF ( INDEX ( LQUAL_CHR, '8' ) .NE. 0 ) IQC_X =  8
         IF ( INDEX ( LQUAL_CHR, '9' ) .NE. 0 ) IQC_X =  9
         IF ( IQC_X .LT. 1  ) THEN
              AUTO_SUP = IBSET ( AUTO_SUP, INT4(NOFX__SPS) ) 
            ELSE
              AUTO_SUP = IBCLR ( AUTO_SUP, INT4(NOFX__SPS) ) 
         END IF
         IF ( IQC_X .LT. QUALCODE_GOOD_LIM ) THEN
              AUTO_SUP = IBSET ( AUTO_SUP, INT4(BQCX__SPS) ) 
            ELSE
              AUTO_SUP = IBCLR ( AUTO_SUP, INT4(BQCX__SPS) ) 
         END IF
!
! ------ Set again fields NOFS, BQCS
!
         IQC_S = -1
         IF ( INDEX ( LQUALXS_CHR, '0' ) .NE. 0 ) IQC_S =  0
         IF ( INDEX ( LQUALXS_CHR, '1' ) .NE. 0 ) IQC_S =  1
         IF ( INDEX ( LQUALXS_CHR, '2' ) .NE. 0 ) IQC_S =  2
         IF ( INDEX ( LQUALXS_CHR, '3' ) .NE. 0 ) IQC_S =  3
         IF ( INDEX ( LQUALXS_CHR, '4' ) .NE. 0 ) IQC_S =  4
         IF ( INDEX ( LQUALXS_CHR, '5' ) .NE. 0 ) IQC_S =  5
         IF ( INDEX ( LQUALXS_CHR, '6' ) .NE. 0 ) IQC_S =  6
         IF ( INDEX ( LQUALXS_CHR, '7' ) .NE. 0 ) IQC_S =  7
         IF ( INDEX ( LQUALXS_CHR, '8' ) .NE. 0 ) IQC_S =  8
         IF ( INDEX ( LQUALXS_CHR, '9' ) .NE. 0 ) IQC_S =  9
         IF ( IQC_S .LT. 1  ) THEN
              AUTO_SUP = IBSET ( AUTO_SUP, INT4(NOFS__SPS) ) 
            ELSE 
              AUTO_SUP = IBCLR ( AUTO_SUP, INT4(NOFS__SPS) ) 
         END IF
         IF ( IQC_S .LT. QUALCODE_GOOD_LIM ) THEN
              AUTO_SUP = IBSET ( AUTO_SUP, INT4(BQCS__SPS) ) 
            ELSE 
              AUTO_SUP = IBCLR ( AUTO_SUP, INT4(BQCS__SPS) ) 
         END IF
!
         IF (       KBIT ( JSITI(ISITE(1)), INT2(4) ) .AND. &
     &        .NOT. KBIT ( JSITI(ISITE(1)), INT2(5) ) .AND. &
     &              KBIT ( JSITI(ISITE(2)), INT2(4) ) .AND. &
     &        .NOT. KBIT ( JSITI(ISITE(2)), INT2(5) )       ) THEN
              AUTO_SUP = IBSET ( AUTO_SUP, INT4(IOUS__SPS) )
         END IF
         AUTO_SUP = IBSET ( AUTO_SUP, INT4(INIT__SPS) )
!
         SUPMET_SAVE = SUPMET
         SUPSTAT_SAVE(1) = SUPSTAT(1)
         SUPSTAT_SAVE(2) = SUPSTAT(2)
!
         DO 430 J3=FIRST__DTP,LAST__DTP 
            IF ( J3 == GRPRAT__DTP  .OR. &
     &           J3 == PHSRAT__DTP  .OR. &
     &           J3 == PHSRAT__DTP  .OR. &
     &           J3 == SNBRAT__DTP  .OR. &
     &           J3 == GRPONL__DTP  .OR. &
     &           J3 == PHSONL__DTP  .OR. &
     &           J3 == SNBONL__DTP  .OR. &
     &           J3 == RATONL__DTP       ) THEN
                 SUPMET = SUPMET__PRE98
               ELSE IF ( J3 == GX__DTP     .OR. &
     &                   J3 == GS__DTP     .OR. &
     &                   J3 == PX__DTP     .OR. &
     &                   J3 == PS__DTP     .OR. &
     &                   J3 == SNG_X__DTP  .OR. &
     &                   J3 == SNG_S__DTP       ) THEN
                 SUPMET = SUPMET__SNGBA
               ELSE 
                 SUPMET = SUPMET__COMB1
            END IF
!
            CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, ICORR, GIONSG, &
     &                         PHIONS, IWVBIT1, ISITE, JSITI, ITT, ISTAR, &
     &                         ELEV, KIONO, SNR, SNR_S, SUPSTAT, UACSUP )
!
            USER_SUP = IBSET ( USER_SUP, INT4(INIT__SPS) )
            USER_REC = IBSET ( USER_REC, INT4(INIT__SPS) )
!
            IDATYP = J3
            IF ( SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS ) ) THEN
                 CONTINUE 
               ELSE
                 USER_SUP = IBSET ( USER_SUP, J3 )
            END IF
 430     CONTINUE 
!
         IDATYP = IDATYP_SAVE 
         SUPMET = SUPMET_SAVE 
         SUPSTAT(1) = SUPSTAT_SAVE(1) 
         SUPSTAT(2) = SUPSTAT_SAVE(2) 
!
         CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
 410  CONTINUE 
      CALL ACS_OBSFIL ( 'C' )
      META_SUP = .TRUE.
      CALL USE_COMMON ( 'OW' )
!
      RETURN
      END  SUBROUTINE INIT_META_SUP  !#!#
