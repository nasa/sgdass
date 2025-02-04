      SUBROUTINE APPLY_TCN ( L_TCN, TCN, STA_NAM, IONO_ZEN_AVR, &
     &                       DEL_GPS_IONO, FREQ_IONO, ELEV, &
     &                       DER_DEL, ADDERR_GR_TAU )
! ************************************************************************
! *                                                                      *
! *   Routine APPLY_TCN computes the additive weight correction          *
! *   ADDERR_GR_TAU  in accordance with the TCN model.                   *
! *                                                                      *
! *  ### 21-OCT-2010   APPLY_TCN   v2.0 (c)  L. Petrov  04-NOV-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'vtd.i'
      INTEGER*4  L_TCN
      TYPE      ( TCN__TYPE ) TCN(L_TCN)
      CHARACTER  STA_NAM(2)*(*)
      REAL*8     IONO_ZEN_AVR(2), DEL_GPS_IONO(2), FREQ_IONO, ELEV(2), &
     &           DER_DEL(VTD__NDER), ADDERR_GR_TAU
      REAL*8     IONO_MF(2)
      REAL*8     VAR_SQ
      INTEGER*4  J1, J2
      LOGICAL*4, EXTERNAL :: MATCH_WILD 
      REAL*8,    EXTERNAL :: VTD_IONO_MF
!
      DO 410 J1=1,L_TCN
         IF ( TCN(J1)%MODE == TCN__REGR ) THEN
              IF ( ( MATCH_WILD ( STA_NAM(1), TCN(J1)%STA_NAM(1) ) .AND. &
     &               MATCH_WILD ( STA_NAM(2), TCN(J1)%STA_NAM(2) )       ) .OR. &
     &             ( MATCH_WILD ( STA_NAM(1), TCN(J1)%STA_NAM(2) ) .AND. &
     &               MATCH_WILD ( STA_NAM(2), TCN(J1)%STA_NAM(1) )       ) ) THEN
                   IONO_MF(1) = DER_DEL(VTD__DER_IONO1)
                   IONO_MF(2) = DER_DEL(VTD__DER_IONO2) 
                   VAR_SQ = (TCN(J1)%FLOOR*(TCN(J1)%FREQ/FREQ_IONO)**2)**2 &
     &                  + ( ( DEL_GPS_IONO(1) - IONO_ZEN_AVR(1)*IONO_MF(1) )* &
     &                        TCN(J1)%SLOPE )**2 &
     &                  + ( ( DEL_GPS_IONO(2) - IONO_ZEN_AVR(2)*IONO_MF(2) )* &
     &                        TCN(J1)%SLOPE )**2
                   ADDERR_GR_TAU = DSQRT ( ADDERR_GR_TAU**2 + VAR_SQ )
              END IF
           ELSE IF ( TCN(J1)%MODE == TCN__DEL  ) THEN
              IF ( ( MATCH_WILD ( STA_NAM(1), TCN(J1)%STA_NAM(1) ) .AND. &
     &               MATCH_WILD ( STA_NAM(2), TCN(J1)%STA_NAM(2) )       ) .OR. &
     &             ( MATCH_WILD ( STA_NAM(1), TCN(J1)%STA_NAM(2) ) .AND. &
     &               MATCH_WILD ( STA_NAM(2), TCN(J1)%STA_NAM(1) )       ) ) THEN
 !
                   VAR_SQ = (DER_DEL(VTD__IONO1) - DER_DEL(VTD__IONO2))**2
                   ADDERR_GR_TAU = DSQRT (   ADDERR_GR_TAU**2 &
     &                                     + TCN(J1)%FLOOR**2 &
     &                                     + TCN(J1)%SLOPE*VAR_SQ )
              END IF
         END IF
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE APPLY_TCN  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE IONO_TEST_NOI ( L_TCN, TCN, STA_NAM, IONO_ZEN_AVR, &
     &                           DEL_GPS_IONO, FREQ_IONO, ELEV, &
     &                           RAN_SEED_I4, MAG_COEF, TAU_OC )
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INTEGER*4  L_TCN, RAN_SEED_I4
      TYPE      ( TCN__TYPE ) TCN(L_TCN)
      CHARACTER  STA_NAM(2)*(*)
      REAL*8     IONO_ZEN_AVR(2), DEL_GPS_IONO(2), FREQ_IONO, ELEV(2), &
     &           MAG_COEF, TAU_OC
      REAL*8     IONO_MF(2)
      REAL*8     VAR_SQ
      INTEGER*4  J1, J2
      LOGICAL*4, EXTERNAL :: MATCH_WILD 
      REAL*8,    EXTERNAL :: VTD_IONO_MF, RGAUSS
!
      DO 410 J1=1,L_TCN
         IF ( TCN(J1)%MODE == 'RMS_REGR' ) THEN
              IF ( ( MATCH_WILD ( STA_NAM(1), TCN(J1)%STA_NAM(1) ) .AND. &
     &               MATCH_WILD ( STA_NAM(2), TCN(J1)%STA_NAM(2) )       ) .OR. &
     &             ( MATCH_WILD ( STA_NAM(1), TCN(J1)%STA_NAM(2) ) .AND. &
     &               MATCH_WILD ( STA_NAM(2), TCN(J1)%STA_NAM(1) )       ) ) THEN
                   IONO_MF(1) = VTD_IONO_MF ( ELEV(1), 450.D3 )
                   IONO_MF(2) = VTD_IONO_MF ( ELEV(2), 450.D3 )
                   VAR_SQ = (TCN(J1)%FLOOR*(TCN(J1)%FREQ/FREQ_IONO)**2)**2 &
     &                  + ( ( ( DEL_GPS_IONO(1) - IONO_ZEN_AVR(1)*IONO_MF(1) )* &
     &                          TCN(J1)%SLOPE )**2 &
     &                  +   ( ( DEL_GPS_IONO(2) - IONO_ZEN_AVR(2)*IONO_MF(2) )* &
     &                        TCN(J1)%SLOPE )**2 &
     &                    ) 
                   TAU_OC = TAU_OC + RGAUSS ( RAN_SEED_I4, MAG_COEF*DSQRT(VAR_SQ) )
              END IF
         END IF
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  IONO_TEST_NOI  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TROPO_TEST_NOI ( TROP_WZD, MAP_FUN, MAG_COEF, &
     &                            RAN_SEED_I4, TAU_OC )
! ************************************************************************
! *                                                                      *
! *   Routine TROPO_TEST_NOI 
! *                                                                      *
! * ### 23-OCT-2010  TROPO_TEST_NOI  v1.0 (c) L. Petrov  23-OCT-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     TROP_WZD(2), MAP_FUN(2), MAG_COEF, TAU_OC
      INTEGER*4  RAN_SEED_I4
      REAL*8     VAR_SQ
      REAL*8,    EXTERNAL :: RGAUSS
!
      VAR_SQ = (TROP_WZD(1)*MAP_FUN(1))**2 + (TROP_WZD(2)*MAP_FUN(2))**2
      TAU_OC = TAU_OC + RGAUSS ( RAN_SEED_I4, MAG_COEF*DSQRT(VAR_SQ) )
!
      RETURN
      END  SUBROUTINE TROPO_TEST_NOI  !#!  
