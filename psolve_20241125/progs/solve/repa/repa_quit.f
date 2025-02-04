      SUBROUTINE REPA_QUIT ( REP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  REPA_QIUT  deallocates memory grabbed by REPA and kept in *
! *   REP object.                                                        *
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
! *  ### 15-DEC-2004   REPA_QUIT   v1.0 (c)  L. Petrov  15-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'diagi.i'
      INCLUDE   'repa.i'
      TYPE     ( REP__TYPE ) :: REP
      INTEGER*4  IUER
      INTEGER*4  J1, ICLR, IER
!
      DO 410 J1=1,REP%N_BAS
         IF ( REP%DIAGI(J1)%STATUS == DIA__ALL  ) THEN
              CALL ERR_PASS  ( IUER, IER )
              CALL DIAGI_INT ( 2, REP%DIAGI(J1)%STATUS, ICLR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_PASS ( 7291, IUER, 'REPA_QIUT', 'Error in '// &
     &                 'DIAGI_INT during memory deallocation' )
                   RETURN 
              END IF
         END IF
         IF ( ASSOCIATED ( REP%PLT(J1)%ARG_GOO ) ) THEN
              DEALLOCATE ( REP%PLT(J1)%ARG_GOO )
              DEALLOCATE ( REP%PLT(J1)%VAL_GOO )
              DEALLOCATE ( REP%PLT(J1)%ERR_GOO )
              DEALLOCATE ( REP%PLT(J1)%IND_GOO )
         END IF
!
         IF ( ASSOCIATED ( REP%PLT(J1)%ARG_BAD ) ) THEN
              DEALLOCATE ( REP%PLT(J1)%ARG_BAD )
              DEALLOCATE ( REP%PLT(J1)%VAL_BAD )
              DEALLOCATE ( REP%PLT(J1)%ERR_BAD )
              DEALLOCATE ( REP%PLT(J1)%IND_BAD )
         END IF
!
         IF ( ASSOCIATED ( REP%PLT(J1)%ARG_UNR ) ) THEN
              DEALLOCATE ( REP%PLT(J1)%ARG_UNR )
              DEALLOCATE ( REP%PLT(J1)%VAL_UNR )
              DEALLOCATE ( REP%PLT(J1)%ERR_UNR )
              DEALLOCATE ( REP%PLT(J1)%IND_UNR )
         END IF
 410  CONTINUE 
!
      IF ( ASSOCIATED ( REP%DIAGI ) )  DEALLOCATE ( REP%DIAGI )
      IF ( ASSOCIATED ( REP%OBS   ) )  DEALLOCATE ( REP%OBS   )
      IF ( ASSOCIATED ( REP%RES   ) )  DEALLOCATE ( REP%RES   )
      IF ( ASSOCIATED ( REP%PLT   ) )  DEALLOCATE ( REP%PLT   )
      IF ( ASSOCIATED ( REP%COM   ) )  DEALLOCATE ( REP%COM   )
      IF ( ASSOCIATED ( REP%C_SOU ) )  DEALLOCATE ( REP%C_SOU )
      IF ( ASSOCIATED ( REP%C_STA ) )  DEALLOCATE ( REP%C_STA )
      IF ( ASSOCIATED ( REP%C_BAS ) )  DEALLOCATE ( REP%C_BAS )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  REPA_QUIT 
