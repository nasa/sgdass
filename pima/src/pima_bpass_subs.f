      SUBROUTINE PIMA_BPASS_SUBS ( PIM, IND_OBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_BPASS_SUBS
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ###  22-MAY-2026  PIMA_BPASS_SUBS  v1.0 (d) L. Petrov 22-MAY-2026 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_OBS, IUER
      CHARACTER  STR*128, NEW_BPS_FINAM*128, NEW_PBS_FINAM*128
      INTEGER*4  I_BPS, I_PBS, IER 
!
      IF ( IND_OBS < 1 .OR. IND_OBS > PIM%NOBS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I_BPS, STR )
           CALL ERR_LOG ( 7281, IUER, 'PIMA_BPASS_SUBS', 'Trap of '// &
     &         'internal control: parameter IND_OBS is wrong: '//STR )
           RETURN 
      END IF
!
      IF ( PIM%L_BPS_SUB > 0 ) THEN
           I_BPS = PIM%IND_BPS_SUB(IND_OBS)
           IF ( I_BPS == 0 ) THEN
                NEW_BPS_FINAM = PIM%CONF%BANDPASS_FILE
              ELSE
                NEW_BPS_FINAM = PIM%BPS_SUB_NAME(I_BPS)
           END IF
           IF ( NEW_BPS_FINAM .NE. PIM%BPASS(1)%FINAM ) THEN
                IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                     WRITE ( 6, 210 ) IND_OBS, TRIM(NEW_BPS_FINAM)
 210                 FORMAT ( 'PIMA_BPASS_SUBS: observation ', I6, &
     &                        ' replace bandpass file to ', A )
                END IF
                CALL ERR_PASS ( IUER, IER ) 
                CALL PIMA_READ_BPASS ( NEW_BPS_FINAM, PIM, IER )
                IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 7282, IUER, 'PIMA_BPASS_SUBS', 'Error in '// &
     &                  'reading the substitute bandpass file '//NEW_BPS_FINAM )
                    RETURN 
               END IF
           END IF
      END IF
!
      IF ( PIM%L_PBS_SUB > 0 ) THEN
           I_PBS = PIM%IND_PBS_SUB(IND_OBS)
           IF ( I_PBS == 0 ) THEN
                NEW_PBS_FINAM = PIM%CONF%POLARCAL_FILE
              ELSE
                NEW_PBS_FINAM = PIM%PBS_SUB_NAME(I_PBS)
           END IF
           IF ( NEW_PBS_FINAM .NE. PIM%PBP(1)%FINAM ) THEN
                IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                     WRITE ( 6, 220 ) IND_OBS, TRIM(NEW_PBS_FINAM)
 220                 FORMAT ( 'PIMA_BPASS_SUBS: observation ', I6, &
     &                        ' replace polarization bandpass file to ', A )
                END IF
                CALL ERR_PASS ( IUER, IER ) 
                CALL PIMA_READ_PBP ( NEW_PBS_FINAM, PIM, IER )
                IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 7283, IUER, 'PIMA_BPASS_SUBS', 'Error in '// &
     &                  'reading the substitute bandpass file '//NEW_PBS_FINAM )
                    RETURN 
               END IF
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_BPASS_SUBS  !#!  
