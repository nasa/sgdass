      SUBROUTINE REPA_LOAD_RES ( REP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  REPA_LOAD_RES loads residuals in the internal structures  *
! *   of REPA.                                                           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *        REP ( RECORD    ) -- Object which keeps internal parameters   *
! *                             for program REPA (REsiduals Plots and    *
! *                             Ambiguities).                            *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 02-DEC-2004  REPA_LOAD_RES  v1.2 (c)  L. Petrov  20-MAR-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'resfl.i' 
      INCLUDE   'diagi.i' 
      INCLUDE   'repa.i' 
      TYPE     ( REP__TYPE ) :: REP
      INTEGER*4  IUER
      CHARACTER  STR*32
      INTEGER*4  J1, IOS
      LOGICAL*4  IS_R8_NAN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Allocate memory for residuals
!
      IF ( ASSOCIATED ( REP%RES ) ) THEN
           DEALLOCATE ( REP%RES )
      END IF
!
      ALLOCATE ( REP%RES(REP%N_OBS), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL IINCH   ( SIZEOF(REP%RES(1))*REP%N_OBS, STR )
           CALL ERR_LOG ( 7761, IUER, 'REPA_LOAD_RES', 'Failure in an '// &
     &         'attempt to grab '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory' )
           RETURN 
      END IF
!
      CALL ACS_RESFIL ( 'O' )
!
! --- Read binary file with residuals and upload the information
!
      DO 410 J1=1,REP%N_OBS
         CALL USE_RESFIL ( J1, 'R' )
         IF ( IS_R8_NAN(RDOC)   ) RDOC  = 0.D0
         IF ( IS_R8_NAN(TAU_C)  ) TAU_C = 0.D0
         IF ( DABS(RDOC*1.D-9) < REPA__RES_DEL_MAX ) THEN
              REP%RES(J1)%RES_DEL = RDOC*1.D-9
            ELSE IF ( RDOC*1.D-9 >  REPA__RES_DEL_MAX ) THEN
              REP%RES(J1)%RES_DEL = REPA__RES_DEL_MAX 
            ELSE IF ( RDOC*1.D-9 < -REPA__RES_DEL_MAX ) THEN
              REP%RES(J1)%RES_DEL = -REPA__RES_DEL_MAX 
         END IF
         REP%RES(J1)%RES_DEL = RDOC*1.D-9
         REP%RES(J1)%TAU_C   = TAU_C
!
         IF ( IS_R8_NAN(RROC)   ) RROC = 0.D0
         IF ( IS_R8_NAN(RATE_C) ) RATE_C = 0.D0
         IF ( DABS(RROC*1.D-12) < REPA__RES_RAT_MAX ) THEN
              REP%RES(J1)%RES_RAT = RROC*1.D-12
            ELSE IF ( RROC*1.D-12 >  REPA__RES_RAT_MAX ) THEN
              REP%RES(J1)%RES_RAT = REPA__RES_RAT_MAX 
            ELSE IF ( RROC*1.D-12 < -REPA__RES_RAT_MAX ) THEN
              REP%RES(J1)%RES_RAT = -REPA__RES_RAT_MAX 
         END IF
         REP%RES(J1)%RATE_C   = RATE_C
!
         IF ( RDERR*1.D-9 < REPA__ERR_DEL_MAX ) THEN
              REP%RES(J1)%ERR_DEL = RDERR*1.D-9
            ELSE 
              REP%RES(J1)%ERR_DEL = REPA__ERR_DEL_MAX 
         END IF
!
         IF ( RDERR*1.D-12 < REPA__ERR_RAT_MAX ) THEN
              REP%RES(J1)%ERR_RAT = RRERR*1.D-12
            ELSE 
              REP%RES(J1)%ERR_RAT = REPA__ERR_RAT_MAX 
         END IF
 410  CONTINUE 
!
      CALL ACS_RESFIL ( 'C' )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  REPA_LOAD_RES 
