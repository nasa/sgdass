      SUBROUTINE CHECK_RES ( GAMB_F_ION, IBAND, OBS, GAMB, GAMB_OPP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CHECK_RES  checks residuals after group delay ambiguity   *
! *   resoution. It is assumed that PREPES_MB and IONO_AMB has already   *
! *   worked. The purpose of CHECK_RES is to make the final check.       *
! *   If it finds remaining ambiguities it resolves them, correct O-C,   *
! *   updates ambiguities counter. If ionsosphere calibration was taken  *
! *   into account it updates ionosphere calibration also.               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  GAMB_F_ION ( LOGICAL*4 ) -- Flag: whether to calculate ionosphere   *
! *                              correction.                             *
! *       IBAND ( INTEGER*4 ) -- Band code: X__BAND or S__BAND.          *
! *                              Parameters X__BAND, S__BAND are defined *
! *                              in gamb.i                               *
! *         OBS ( RECORD    ) -- Data structure which contains           *
! *                              band-independent information: time      *
! *                              of observation, baseline, lists of      *
! *                              objects, status flags etc.              *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *        GAMB ( RECORD    ) -- Data structures for group delay         *
! *                              ambiguity resolution program for the    *
! *                              database under consideration.           *
! *    GAMB_OPP ( RECORD    ) -- Data structures for group delay         *
! *                              ambiguity resolution program for the    *
! *                              the opposite band.                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 03-MAR-2000   CHECK_RES   v1.0 (c)  L. Petrov  03-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'gamb.i'
      TYPE ( OBS__STRU ) ::  OBS
      TYPE ( GAMB__STRU ) ::  GAMB, GAMB_OPP
      LOGICAL*4  GAMB_F_ION
      INTEGER*4  IBAND, IUER
      INTEGER*4  IBAS, IAMB, I_ACT, IOBS_BAS(MG_BAS), J1, J2, J3
      REAL*8     TT, OC, AV_BAS(MG_BAS), DOBS_TAU_XS
      INTEGER*4  IFIND_PL
!
      CALL NOUT_I4 ( MG_BAS, IOBS_BAS )
      CALL NOUT_R8 ( MG_BAS, AV_BAS   )
!
! --- First compute averaged value of clock offset for each baseline.
! --- Only used observations are participated in the opertaion
!
      DO 410 J1=1,OBS%NOBS
         IBAS = IFIND_PL ( GAMB%L_BAS, GAMB%LIS_BAS, OBS%IBA(J1) )
         IF ( IBAS .LE. 0 ) GOTO 410
         IF ( GAMB%K_BAS(IBAS) .GE. OBS%MINOBS ) THEN
              TT = ( OBS%TT(J1) - OBS%TAVALL )/OBS%TLNALL
              OC = GAMB%OCT(J1) - ( GAMB%DR_BAS(IBAS)*TT  + &
     &                              GAMB%SQ_BAS(IBAS)*(1.5D0*TT**2 - 0.5D0) )
              IF ( GAMB%USE(J1) ) THEN
                   AV_BAS(IBAS)   = AV_BAS(IBAS) + OC
                   IOBS_BAS(IBAS) = IOBS_BAS(IBAS) + 1
              END IF
         END IF
 410  CONTINUE
!
      DO 420 J2=1,GAMB%L_BAS
         IF ( IOBS_BAS(J2) .GT. 0 ) THEN
              AV_BAS(J2) = AV_BAS(J2)/IOBS_BAS(J2)
            ELSE
!
! ----------- We use SH_BAS as a last resort if there were no used observations
! ----------- at that baseline
!
              AV_BAS(J2) = GAMB%SH_BAS(J2)
         END IF
 420  CONTINUE
!
! --- No check all observations
!
      I_ACT = 0
      DO 430 J3=1,OBS%NOBS
         IBAS = IFIND_PL ( GAMB%L_BAS, GAMB%LIS_BAS, OBS%IBA(J3) )
         IF ( IBAS .LE. 0 ) GOTO 430
         IF ( GAMB%K_BAS(IBAS) .GE. OBS%MINOBS ) THEN
!
! ----------- Compute o-c ...
!
              TT = ( OBS%TT(J3) - OBS%TAVALL )/OBS%TLNALL
              OC = GAMB%OCT(J3) - ( AV_BAS(IBAS) + &
     &                              GAMB%DR_BAS(IBAS)*TT  + &
     &                              GAMB%SQ_BAS(IBAS)*(1.5D0*TT**2 - 0.5D0) )
!
! ----------- Compute ambiguity
!
              IAMB = NINT ( OC/GAMB%GAMBC )
              IF ( IAMB .NE. 0 ) THEN
!
! ---------------- Update ambiguities counter and O-C
!
                   GAMB%JMP(J3) = GAMB%JMP(J3) - IAMB*GAMB%GAMBC
                   GAMB%OCT(J3) = GAMB%OCT(J3) - IAMB*GAMB%GAMBC
!
! ---------------- special tricks for ionosphere
!
                   IF ( GAMB_F_ION ) THEN
                        IF ( IBAND .EQ. X__BAND ) THEN
!
! -------------------------- Get difference X-band i S-band
!
                             DOBS_TAU_XS = GAMB%OCT(J3) - GAMB_OPP%OCT(J3)
!
! -------------------------- New recalculate group ionosphere deleay...
!
                             GAMB%GION_TAU(J3) = -DOBS_TAU_XS * &
     &                            GAMB_OPP%FREQ_GR**2/ &
     &                            (GAMB%FREQ_GR**2 - GAMB_OPP%FREQ_GR**2)
                             GAMB_OPP%GION_TAU(J3) = -DOBS_TAU_XS * &
     &                            GAMB%FREQ_GR**2/ &
     &                            (GAMB%FREQ_GR**2 - GAMB_OPP%FREQ_GR**2)
                           ELSE IF ( IBAND .EQ. S__BAND ) THEN
!
! -------------------------- Get difference X-band and S-band
!
                             DOBS_TAU_XS = GAMB_OPP%OCT(J3) - GAMB%OCT(J3)
!
! -------------------------- New recalculate group ionosphere deleay...
!
                             GAMB_OPP%GION_TAU(J3) = -DOBS_TAU_XS * &
     &                            GAMB%FREQ_GR**2/ &
     &                            (GAMB_OPP%FREQ_GR**2 - GAMB%FREQ_GR**2)
                             GAMB%GION_TAU(J3) = -DOBS_TAU_XS * &
     &                            GAMB_OPP%FREQ_GR**2/ &
     &                            (GAMB_OPP%FREQ_GR**2 - GAMB%FREQ_GR**2)
                         END IF
!
! ---------------------- ... and at last apply it to group delay O-C
!
                         GAMB%OCT(J3)     = GAMB%OCT(J3) - GAMB%GION_TAU(J3)
                         GAMB_OPP%OCT(J3) = GAMB_OPP%OCT(J3) - &
     &                                           GAMB_OPP%GION_TAU(J3)
                   END IF
!
! ---------------- Increment a counter of our virtues
!
                   I_ACT = I_ACT + 1
              END IF
         END IF
 430  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  CHECK_RES  #!#
