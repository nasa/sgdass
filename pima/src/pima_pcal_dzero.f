      SUBROUTINE PIMA_PCAL_DZERO ( PIM, OPT_VAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PCAL_DZERO generates "the substitude" pcal for the    *
! *   values that have zero pcal or are missing.                         *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 04-MAY-2026 PIMA_PCAL_DZERO v1.0 (d)  L. Petrov  10-MAY-2026 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE )     :: PIM
      TYPE     ( PIM_PCAL__TYPE ) :: PCAL_SAVE
      CHARACTER  OPT_VAL*(*)
      INTEGER*4  IUER
      REAL*8     PCAL_AMP_MIN
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, IER
!
      PCAL_AMP_MIN = PIMA__PCAL_AMP_MIN 
!
      IF ( OPT_VAL == 'zero' .OR. OPT_VAL == "missed" ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 8411, IUER, 'PIMA_PCAL_DZERO', 'Wrong '// &
     &         'value '//TRIM(OPT_VAL)//' followed keyword pima_dzero' )
           RETURN 
      END IF
!
      IF ( OPT_VAL == 'zero'  .AND. PIM%CONF%BEG_FRQ == PIM%CONF%END_FRQ ) THEN
           CALL ERR_LOG ( 8412, IUER, 'PIMA_PCAL_DZERO', 'Algorithm "zero" '// &
     &         'requires at least two IFs' )
           RETURN 
      END IF
!
      DO 410 J1=1,PIM%NSTA
         DO 420 J2=1,PIM%NFRG
            IF ( PIM%STA(J1)%PCAL(J2)%PCAL_AVAIL ) THEN
                 PCAL_SAVE = PIM%STA(J1)%PCAL(J2)
                 ALLOCATE ( PCAL_SAVE%PHAS_RGR(PCAL_SAVE%NO_TONES,PIM%NFRQ,PCAL_SAVE%NPOI,PCAL_SAVE%NPOL), &
     &                      PCAL_SAVE%AMPL_RGR(PCAL_SAVE%NO_TONES,PIM%NFRQ,PCAL_SAVE%NPOI,PCAL_SAVE%NPOL), &
     &                      PCAL_SAVE%PHAS_SCA(PCAL_SAVE%NO_TONES,PIM%NFRQ,PCAL_SAVE%NPOI,PCAL_SAVE%NPOL), &
     &                      PCAL_SAVE%AMPL_SCA(PCAL_SAVE%NO_TONES,PIM%NFRQ,PCAL_SAVE%NPOI,PCAL_SAVE%NPOL), &
     &                      PCAL_SAVE%PRAT_SCA(PCAL_SAVE%NO_TONES,PIM%NFRQ,PCAL_SAVE%NPOI,PCAL_SAVE%NPOL), &
     &                      PCAL_SAVE%PHAS(PCAL_SAVE%NO_TONES,PIM%NFRQ,PCAL_SAVE%NPOI,PCAL_SAVE%NPOL),     &
     &                      PCAL_SAVE%AMPL(PCAL_SAVE%NO_TONES,PIM%NFRQ,PCAL_SAVE%NPOI,PCAL_SAVE%NPOL),     &
     &                      STAT=IER )
                 PCAL_SAVE%PHAS_RGR = PIM%STA(J1)%PCAL(J2)%PHAS_RGR 
                 PCAL_SAVE%AMPL_RGR = PIM%STA(J1)%PCAL(J2)%AMPL_RGR 
                 PCAL_SAVE%PHAS_SCA = PIM%STA(J1)%PCAL(J2)%PHAS_SCA 
                 PCAL_SAVE%AMPL_SCA = PIM%STA(J1)%PCAL(J2)%AMPL_SCA 
                 PCAL_SAVE%PRAT_SCA = PIM%STA(J1)%PCAL(J2)%PRAT_SCA 
                 PCAL_SAVE%PHAS     = PIM%STA(J1)%PCAL(J2)%PHAS
                 PCAL_SAVE%AMPL     = PIM%STA(J1)%PCAL(J2)%AMPL
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL PIMA_PCAL_FIX_ZERO ( PIM, J1, J2, PCAL_AMP_MIN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J2, STR )
                      CALL ERR_LOG ( 8412, IUER, 'PIMA_PCAL_DZERO', 'Error '// &
     &                    'in an attempt to compute the substitute pcal '// &
     &                    'for station '//PIM%C_STA(J1)//' frequency '// &
     &                    'group '//STR )
                      RETURN 
                 END IF
!
                 IF ( PIM%STA(J1)%PCAL(J2)%LPOI_SUBS > 0 ) THEN
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 8412, IUER, 'PIMA_PCAL_DZERO', 'Error '// &
     &                         'in an attempt to allocate dynamic memore for PCAL_SAVE' )
                           RETURN 
                      END IF
!
! ------------------- Compute the scam averaged pcal and the list of scan-based substitutes
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL PIMA_PCAL_SCAN_AVR ( PIM, PIM%STA(J1)%PCAL(J2), IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J2, STR )
                           CALL ERR_LOG ( 8413, IUER, 'PIMA_PCAL_DZERO', 'Error in '// &
     &                         'scan computing scan averaged phase calibration for '// &
     &                         'station '//PIM%C_STA(J1)//' polarizaion '//STR )
                           RETURN 
                      END IF
!
                      PIM%STA(J1)%PCAL(J2)%PHAS_RGR = PCAL_SAVE%PHAS_RGR 
                      PIM%STA(J1)%PCAL(J2)%AMPL_RGR = PCAL_SAVE%AMPL_RGR 
                      PIM%STA(J1)%PCAL(J2)%PHAS_SCA = PCAL_SAVE%PHAS_SCA 
                      PIM%STA(J1)%PCAL(J2)%AMPL_SCA = PCAL_SAVE%AMPL_SCA 
                      PIM%STA(J1)%PCAL(J2)%PRAT_SCA = PCAL_SAVE%PRAT_SCA
                      PIM%STA(J1)%PCAL(J2)%PHAS     = PCAL_SAVE%PHAS     
                      PIM%STA(J1)%PCAL(J2)%AMPL     = PCAL_SAVE%AMPL     
                    ELSE
                      PIM%STA(J1)%PCAL(J2)%LSCA_SUBS = 0
                      IF ( ASSOCIATED ( PIM%STA(J1)%PCAL(J2)%IND_SUBS ) ) THEN
                           DEALLOCATE ( PIM%STA(J1)%PCAL(J2)%IND_SUBS )
                      END IF
                      IF ( ASSOCIATED ( PIM%STA(J1)%PCAL(J2)%PHAS_SUBS ) ) THEN
                           DEALLOCATE ( PIM%STA(J1)%PCAL(J2)%PHAS_SUBS )
                      END IF
                      IF ( ASSOCIATED ( PIM%STA(J1)%PCAL(J2)%AMPL_SUBS ) ) THEN
                           DEALLOCATE ( PIM%STA(J1)%PCAL(J2)%AMPL_SUBS )
                      END IF
                 END IF
!
                 DEALLOCATE ( PCAL_SAVE%PHAS_RGR, &
     &                        PCAL_SAVE%AMPL_RGR, &
     &                        PCAL_SAVE%PHAS_SCA, &
     &                        PCAL_SAVE%AMPL_SCA, &
     &                        PCAL_SAVE%PRAT_SCA, &
     &                        PCAL_SAVE%PHAS,     &
     &                        PCAL_SAVE%AMPL      )
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PCAL_DZERO   !#!#
