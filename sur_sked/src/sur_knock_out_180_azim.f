      SUBROUTINE SUR_KNOCK_OUT_180_AZIM ( SUR, AZIM_180_LIM, IVRB )
! ************************************************************************
! *                                                                      *
! *   Routine  SUR_KNOCK_OUT_180_AZIM
! *                                                                      *
! * # 12-JUL-2018 SUR_KNOCK_OUT_180_AZIM v1.0 (c) L. Petrov 12-JUL-2018 #*
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      TYPE     ( SUR__TYPE ) :: SUR
      REAL*8     AZIM_180_LIM
      INTEGER*4  IVRB
      REAL*8     DUR_SCAN_PREV, DUR_SCAN_THIS, DIF_AZ
      INTEGER*4  J1, J2, J3, J4, OLD_PREV_SCA
!
      IF ( IVRB > 0 ) THEN
           WRITE  ( 6, 110 ) AZIM_180_LIM/DEG__TO__RAD
 110       FORMAT ( 'Running SUR_KNOCK_OUT_180_AZIM with azimuth '// &
     &              'avoid zone ', F5.1, ' deg' )
      END IF
      DO 410 J1=2,SUR%L_SCN
         DO 420 J2=1,SUR%L_STA
            IF ( SUR%OBS_STA(J2,J1) == SUR__USED .AND. SUR%SCA_PREV(J2,J1) > 0 ) THEN
                 DIF_AZ = SUR%AZ_ACC_OBS(J2,J1) - SUR%AZ_ACC_OBS(J2,SUR%SCA_PREV(J2,J1)) 
                 IF ( IVRB > 5 ) THEN
                      WRITE ( 6, * ) 'SUR_KNOCK Scan ', int2(j1), ' station ', SUR%STA(J2)%NAME, ' Az_dist= ', sngl(DABS(DIF_AZ)/DEG__TO__RAD) ! %%%
                 END IF
                 IF ( DABS(DABS(DIF_AZ) - PI__NUM) < AZIM_180_LIM ) THEN
                      DUR_SCAN_PREV = ( SUR%TAI_OBS_END(SUR%SCA_PREV(J2,J1)) -     &
     &                                  SUR%TAI_OBS_BEG(SUR%SCA_PREV(J2,J1))   ) + &
     &                                ( SUR%MJD_OBS_END(SUR%SCA_PREV(J2,J1)) - &
     &                                  SUR%MJD_OBS_BEG(SUR%SCA_PREV(J2,J1))   )*86400.0D0
                      DUR_SCAN_THIS = ( SUR%TAI_OBS_END(J1) - &
     &                                  SUR%TAI_OBS_BEG(J1)   ) + &
     &                                ( SUR%MJD_OBS_END(J1) - &
     &                                  SUR%MJD_OBS_BEG(J1)   )*86400.0D0
                      IF ( DUR_SCAN_PREV .GE. DUR_SCAN_THIS ) THEN
!
! ------------------------ The previous scan was longer. Know iut the J1-th scan
!
                           SUR%OBS_STA(J2,J1) = SUR__UND 
                           IF ( J2 < SUR%L_SCN ) THEN
!
! ----------------------------- ... und update the index of the prvious scan
!
                                DO 430 J3=J2+1,SUR%L_SCN
                                   IF ( SUR%SCA_PREV(J2,J3) == J1 ) THEN
                                        SUR%SCA_PREV(J2,J3) = SUR%SCA_PREV(J2,J1)
                                        GOTO 830
                                   END IF
 430                            CONTINUE 
 830                            CONTINUE 
                           END IF
                           IF ( IVRB > 0 ) THEN
                                WRITE  ( 6, 110 ) SUR%STA(J2)%NAME, J1, DABS(DIF_AZ)/DEG__TO__RAD
 120                            FORMAT ( 'Station ', A, ' was knocked out from scan ', I4, &
     &                                   ' because dif_z = ', F6.2, ' deg' )
                           END IF
                         ELSE
                           SUR%OBS_STA(J2,SUR%SCA_PREV(J2,J1)) = SUR__UND 
                           OLD_PREV_SCA = SUR%SCA_PREV(J2,J1)
                           SUR%SCA_PREV(J2,OLD_PREV_SCA) = 0
                           DO 440 J4=J1-1,1,-1
                              IF ( SUR%SCA_PREV(J2,J4) == SUR__USED ) THEN
                                   SUR%SCA_PREV(J2,OLD_PREV_SCA) = J4
                                   GOTO 840
                              END IF
 440                       CONTINUE 
 840                       CONTINUE 
                           IF ( IVRB > 0 ) THEN
                                WRITE  ( 6, 120 ) SUR%STA(J2)%NAME, OLD_PREV_SCA, DABS(DIF_AZ)/DEG__TO__RAD
                           END IF
                      END IF
                   END IF
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  SUR_KNOCK_OUT_180_AZIM  !#!#
