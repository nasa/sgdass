      SUBROUTINE PIMA_PCAL_CONTROL ( PIM, OPCODE, STA_NAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_PCAL_CONTROL
! *                                                                      *
! * ## 06-MAY-2011  PIMA_PCAL_CONTROL  v1.0 (c) L. Petrov 06-MAY-2011 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  OPCODE*(*), STA_NAM*(*)
      INTEGER*4  IUER
      INTEGER*4  IND_STA, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IND_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
      IF ( IND_STA .LE. 0 ) THEN
           CALL ERR_LOG ( 8591, IUER, 'PIMA_PCAL_CONTROL', &
     &         'Station '//STA_NAM(1:I_LEN(STA_NAM))// &
     &         ' did not participate in VLBI experiment '// &
     &         PIM%CONF%SESS_CODE )
           RETURN 
      END IF
!
      IF ( OPCODE == 'pcal_off' ) THEN
           PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE = .FALSE.
        ELSE IF ( OPCODE == 'pcal_on' ) THEN
           IF ( PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%NPOI > 0 ) THEN
                PIM%STA(IND_STA)%PCAL(PIM%CONF%FRQ_GRP)%PCAL_USE = .TRUE.
              ELSE 
                CALL ERR_LOG ( 8592, IUER, 'PIMA_PCAL_CONTROL', &
     &              'It is not possible to turn the phase '// &
     &              'calibration on for station '// &
     &               STA_NAM(1:I_LEN(STA_NAM))// &
     &              ' because no phase cal information is '// &
     &              'present in pima file for experiment '// &
     &               PIM%CONF%SESS_CODE )
                RETURN 
           END IF
        ELSE 
           CALL ERR_LOG ( 8593, IUER, 'PIMA_PCAL_CONTROL', &
     &         'Trap of internal control: unsupported keyword '//OPCODE )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PCAL_CONTROL  !#!#
