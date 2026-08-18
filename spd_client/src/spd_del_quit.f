      SUBROUTINE SPD_DEL_QUIT ( SPD_DEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_DEL_QUIT releases allocated memory for object SPD_DEL *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   SPD_DEL ( SPD_DEL__TYPE ) -- Object with Slant Path Delays.        *
! *                                The internal fields of SPD_DEL        *
! *                                contain the coefficient of B-spline   *
! *                                expansion of path delay.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 24-AUG-2014  SPD_DEL_QUIT  v1.3 (d)  L. Petrov  08-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD_DEL__TYPE ) :: SPD_DEL
      CHARACTER  STR*128
      INTEGER*4  IUER
      INTEGER*4  IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( ASSOCIATED ( SPD_DEL%RES ) ) THEN
           DEALLOCATE ( SPD_DEL%RES )
      END IF
!
      IF ( ASSOCIATED ( SPD_DEL%SUR_PRS ) ) THEN
           DEALLOCATE ( SPD_DEL%SUR_PRS )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%SUR_PWP ) ) THEN
           DEALLOCATE ( SPD_DEL%SUR_PWP )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%SUR_TEM ) ) THEN
           DEALLOCATE ( SPD_DEL%SUR_TEM )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%DELS    ) ) THEN
           DEALLOCATE ( SPD_DEL%DELS    )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%OPA     ) ) THEN
           DEALLOCATE ( SPD_DEL%OPA     )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%TAT     ) ) THEN
           DEALLOCATE ( SPD_DEL%TAT     )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%MAP_ARR ) ) THEN
           DEALLOCATE ( SPD_DEL%MAP_ARR )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%TIM_ARR ) ) THEN
           DEALLOCATE ( SPD_DEL%TIM_ARR )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%FRQ_ARR ) ) THEN
           DEALLOCATE ( SPD_DEL%FRQ_ARR )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%ZEN_DEL ) ) THEN
           DEALLOCATE ( SPD_DEL%ZEN_DEL )
      END IF
!
      IF ( ASSOCIATED ( SPD_DEL%MOD%TEXT  ) ) THEN
           DEALLOCATE ( SPD_DEL%MOD%TEXT  )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%MET%TEXT  ) ) THEN
           DEALLOCATE ( SPD_DEL%MET%TEXT  )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%ELV%ELEV ) ) THEN
           DEALLOCATE ( SPD_DEL%ELV%ELEV )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%ELV%MAP ) ) THEN
           DEALLOCATE ( SPD_DEL%ELV%MAP )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%MF%EL_ARG ) ) THEN
           DEALLOCATE ( SPD_DEL%MF%EL_ARG )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%AZM%AZIM ) ) THEN
           DEALLOCATE ( SPD_DEL%AZM%AZIM )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%MF%MF_SPL ) ) THEN
           DEALLOCATE ( SPD_DEL%MF%MF_SPL )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%MF%MF_ARG ) ) THEN
           DEALLOCATE ( SPD_DEL%MF%MF_ARG )
      END IF
      IF ( ASSOCIATED ( SPD_DEL%MF%EL_SPL ) ) THEN
           DEALLOCATE ( SPD_DEL%MF%EL_SPL )
      END IF
!
      SPD_DEL%MF%STATUS = SPD__UNDF
      SPD_DEL%STATUS = SPD__UNDF
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  SPD_DEL_QUIT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SAT_QUIT ( SAT )
! ************************************************************************
! *                                                                      *
! *   Routine  SAT_QUIT  releases dynamic memory allocated for           *
! *   data structure SAT with ASCII reperesentation of results of        *
! *   SPD_3D program,                                                    *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 15-SEP-2014    SAT_QUIT   v1.0 (d)  L. Petrov  15-SEP-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD__ASCII__TYPE ) :: SAT
!
      IF ( ASSOCIATED(SAT%MLINE) ) THEN
           DEALLOCATE ( SAT%MLINE )
      END IF
      IF ( ASSOCIATED(SAT%ILINE) ) THEN
           DEALLOCATE ( SAT%ILINE )
      END IF
      IF ( ASSOCIATED(SAT%SLINE) ) THEN
           DEALLOCATE ( SAT%SLINE )
      END IF
      IF ( ASSOCIATED(SAT%ELINE) ) THEN
           DEALLOCATE ( SAT%ELINE )
      END IF
      IF ( ASSOCIATED(SAT%ALINE) ) THEN
           DEALLOCATE ( SAT%ALINE )
      END IF
      IF ( ASSOCIATED(SAT%FLINE) ) THEN
           DEALLOCATE ( SAT%FLINE )
      END IF
      IF ( ASSOCIATED(SAT%PLINE) ) THEN
           DEALLOCATE ( SAT%PLINE )
      END IF
      IF ( ASSOCIATED(SAT%DLINE) ) THEN
           DEALLOCATE ( SAT%DLINE )
      END IF
      IF ( ASSOCIATED(SAT%OLINE) ) THEN
           DEALLOCATE ( SAT%OLINE )
      END IF
      CALL NOUT ( SIZEOF(SAT), SAT )
!
      SAT%MLINE => NULL ()
      SAT%ILINE => NULL ()
      SAT%SLINE => NULL ()
      SAT%ELINE => NULL ()
      SAT%ALINE => NULL ()
      SAT%FLINE => NULL ()
      SAT%PLINE => NULL ()
      SAT%DLINE => NULL ()
      SAT%OLINE => NULL ()
!
      RETURN
      END  SUBROUTINE  SAT_QUIT  !#!#
