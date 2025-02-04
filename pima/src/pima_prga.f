      SUBROUTINE PIMA_PRGA ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PRGA 
! *                                                                      *
! *  ### 12-APR-2011   PIMA_PRGA   v1.0 (c)  L. Petrov  12-APR-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      INTEGER*4  J1, J2, J3, IL
      CHARACTER  STR*128
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      WRITE ( 6, 110 ) PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE)), &
     &                 PIM%CONF%FRQ_GRP, PIM%NFRG
 110  FORMAT ( 'PIMA_PRGA   Experiment: ',A, ' Freq. group ',I1, ' ( ', I1, ') ' )
      WRITE ( 6, '(A)' ) ' '
!
      DO 410 J1=1,PIM%NSTA
         IF ( PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%AVAIL ) THEN
              DO 420 J2=1,PIM%NFRQ
                 CALL CLRCH ( STR )
                 DO 430 J3=0,PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%NTERM(J2,1)
                    IL = ILEN(STR) + 2
                    IF ( PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(J3,J2,1) == 0.0 ) THEN
                         STR(IL:) = '0.0'
                       ELSE IF ( PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(J3,J2,1) < 1.D-3 ) THEN
                         WRITE ( UNIT=STR(IL:), FMT='(1PD10.3)' ) PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(J3,J2,1)
                       ELSE 
                         WRITE ( UNIT=STR(IL:), FMT='(F10.6)'   ) PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%GAIN(J3,J2,1)
                    END IF
 430             CONTINUE 
                 CALL CHASHL ( STR )
!
                 IF ( PIM%NPOL == 1 ) THEN
                      WRITE ( 6, 120 ) J1, PIM%C_STA(J1), J2, &
     &                                 PIM%FRQ(J2,PIM%CONF%FRQ_GRP)%FREQ*1.D-6,       &
     &                                 PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%SENS(J2,1),  &
     &                                 PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%NTERM(J2,1), &
     &                                 STR(1:I_LEN(STR))
 120                  FORMAT ( I2,') ',A, '   IF ', I2, ' Freq: ', F9.2, &
     &                         ' MHz  Sens: ', F7.5, ' Jy/K  Npoly: ',I1,'  Poly: ', A )
                    ELSE 
                      WRITE ( 6, 130 ) J1, PIM%C_STA(J1), J2, &
     &                                 PIM%FRQ(J2,PIM%CONF%FRQ_GRP)%FREQ*1.D-6,       &
     &                                 PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%SENS(J2,1),  &
     &                                 PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%SENS(J2,2),  &
     &                                 PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%NTERM(J2,2), &
     &                                 STR(1:I_LEN(STR)), &
     &                                 PIM%STA(J1)%GAIN(PIM%CONF%FRQ_GRP)%TYP(1,1)
 130                  FORMAT ( I2,') ',A, '   IF ', I2, ' Freq: ', F9.2, &
     &                         ' MHz  Sens_RR: ', F7.5, ' Sens_LL: ', F7.5, &
     &                         ' Jy/K  Npoly: ',I1,'  Poly: ', A, ' Type: ', I2 )
                 END IF
 420          CONTINUE 
              WRITE ( 6, '(A)' ) ' ' 
            ELSE 
              WRITE ( 6, 140 ) J1, PIM%C_STA(J1)
 140          FORMAT ( I2,') ', A, ' Gain is not available' )
         END IF
 410  CONTINUE 
!
      CALL ERR_PASS ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_PRGA  !#!#
