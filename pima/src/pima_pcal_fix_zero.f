      SUBROUTINE PIMA_PCAL_FIX_ZERO ( PIM, ISTA, IFRG, PCAL_AMP_MIN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PCAL_FIX_ZERO
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ## 04-MAY-2026 PIMA_PCAL_FIX_ZERO v1.0 (d) L. Petrov 11-MAY-2026 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  ISTA, IFRG, IUER
      REAL*8       PCAL_AMP_MIN, PCAL_PHS_NMED
      INTEGER*4    PIM__NRP 
      PARAMETER  ( PIM__NRP = 7 ) ! The number of poits for the refrerence
      PARAMETER  ( PCAL_PHS_NMED = 4.0 ) ! Allowable devation wrt median
      REAL*8     DP(PIM__NRP,PIM__MSTA,PIM__MSTA), &
     &           DP_MED_BACK(PIM__MFRQ,PIM__MFRQ), &
     &           DP_MED_FORW(PIM__MFRQ,PIM__MFRQ), &
     &           PHS(PIM__NRP,PIM__MFRQ), &
     &           PHS_MED_BACK(PIM__MFRQ), PHS_MED_FORW(PIM__MFRQ), &
     &           AMP(PIM__NRP,PIM__MFRQ), &
     &           AMP_MED_BACK(PIM__MFRQ), AMP_MED_FORW(PIM__MFRQ), &
     &           DELTA_PH, DPM_BACK, DPM_FORW, PHS_DIF
      REAL*4     PHS_SUBS(PIM__MOBS*PIM__MFRQ), &
     &           AMP_SUBS(PIM__MOBS*PIM__MFRQ), &
     &           PHS_SUB_BACK, PHS_SUB_FORW
      LOGICAL*1  FL_G, FL_Z
      INTEGER*1, ALLOCATABLE :: MASK_G(:,:,:,:)
      INTEGER*4, ALLOCATABLE :: IND_G(:,:,:), IND_Z(:,:,:), IND_SUBS(:,:,:,:)
      INTEGER*1  MASK_F(PIM__MFRQ)
      INTEGER*4   PIM__MPCL
      PARAMETER  ( PIM__MPCL = PIM__MTON*PIM__MSCA )
      REAL*8       PHS_MAX, PHS_MIN_TOL
      PARAMETER  ( PHS_MAX     = 1.0D6 )
      PARAMETER  ( PHS_MIN_TOL = 0.5D0 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, J25, &
     &           J26, J27, J28, J29, J30, &
     &           NG(PIM__MTON,PIM__MPIF), NZ(PIM__MTON,PIM__MPIF), &
     &           NG_POL(PIM__MPIF), NZ_POL(PIM__MPIF), KPF, KPB, IND_TONE, &
     &           LP(PIM__MFRQ,PIM__MFRQ), LS(PIM__MFRQ), L_SUB, SGN, &
     &           IAMB_BACK, IAMB_FORW, IER
!
! --- Allocate memory for the mask of good observations and other arrays
! --- ind_sub ( nton, nfrq, npoi, npol )
!
      ALLOCATE ( MASK_G(PIM%NFRQ,PIM%STA(ISTA)%PCAL(IFRG)%NPOI,PIM%STA(ISTA)%PCAL(IFRG)%NO_TONES,PIM%STA(ISTA)%PCAL(IFRG)%NPOL), &
     &           IND_Z(PIM%STA(ISTA)%PCAL(IFRG)%NPOI,PIM%NPCT,PIM%NPOL), &
     &           IND_G(PIM%STA(ISTA)%PCAL(IFRG)%NPOI,PIM%NPCT,PIM%NPOL), &
     &           IND_SUBS(PIM%STA(ISTA)%PCAL(IFRG)%NO_TONES,PIM%NFRQ,PIM%STA(ISTA)%PCAL(IFRG)%NPOI,PIM%STA(ISTA)%PCAL(IFRG)%NPOL), &
    &            STAT=IER )
!
! --- Initialization
!
      NZ       = 0 
      NZ_POL   = 0 
      NG       = 0
      IND_Z    = 0
      IND_G    = 0
      IND_SUBS = 0
      MASK_G   = 0
!
! --- Check the mode of the use of phase cal tones
! --- IND_TONE > 0 means a sginle tone is uzed in data analysis
!
      CALL CHIN ( PIM%CONF%PHAS_CAL_CODE, IND_TONE )
      IF ( IND_TONE < 0 ) IND_TONE = 0
!
! --- Set the mask and count pcals with bad amplitudes
!
      DO 410 J1=1,PIM%STA(ISTA)%PCAL(IFRG)%NPOL
         DO 420 J2=1,PIM%STA(ISTA)%PCAL(IFRG)%NO_TONES
            DO 430 J3=1,PIM%STA(ISTA)%PCAL(IFRG)%NPOI
               FL_G = .FALSE.
               FL_Z = .FALSE.
               DO 440 J4=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                  IF ( PIM%STA(ISTA)%PCAL(IFRG)%AMPL(J2,J4,J3,J1) < PCAL_AMP_MIN ) THEN
                       FL_Z = .TRUE.
                       MASK_G(J4,J3,J2,J1) = 0
                     ELSE 
                       FL_G = .TRUE.
                       MASK_G(J4,J3,J2,J1) = 1
                  END IF
 440           CONTINUE 
!
               IF ( FL_Z .AND. FL_G ) THEN
                    NZ(J2,J1)  = NZ(J2,J1) + 1
                    NZ_POL(J1) = NZ_POL(J1) + 1
                    IND_Z(NZ(J2,J1),J2,J1) = J3
                  ELSE IF ( .NOT. FL_Z .AND. FL_G ) THEN
                    NG(J2,J1)  = NG(J2,J1) + 1
                    NG_POL(J1) = NG_POL(J1) + 1
                    IND_G(NG(J2,J1),J2,J1) = J3
               END IF
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      L_SUB = 0
      DO 450 J5=1,PIM%STA(ISTA)%PCAL(IFRG)%NPOL
         DO 460 J6=1,PIM%STA(ISTA)%PCAL(IFRG)%NO_TONES
!
! --------- Cycle over bad pcals BACKWRDS
!
            DO 470 J7=1,NZ(J6,J5)
               LP = 0
               DP = 0.0D0
!
! ------------ Compute differential phases over PIM__NRP observations 
! ------------ overall all compbinations of differences in IFs BACKWARD
!
               DO 480 J8=IND_Z(J7,J6,J5),1,-1
                  DO 490 J9=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                     IF ( MASK_G(J9,IND_Z(J7,J6,J5),J6,J5) == 1 ) THEN
                          MASK_F(J9) = 1
                       ELSE
                          MASK_F(J9) = 0
                     END IF
!
                     DO 4100 J10=J9+1,PIM%CONF%END_FRQ
                        IF ( LP(J10,J9) == PIM__NRP ) GOTO 4100
                        IF ( MASK_G(J10,J8,J6,J5) == 1 .AND. MASK_G(J9,J8,J6,J5) == 1 ) THEN
                             LP(J10,J9) = LP(J10,J9) + 1
                             DELTA_PH = PIM%STA(ISTA)%PCAL(IFRG)%PHAS(J6,J10,J8,J5) - &
     &                                  PIM%STA(ISTA)%PCAL(IFRG)%PHAS(J6,J9,J8,J5)
                             IF ( DELTA_PH > PI2 ) DELTA_PH = DELTA_PH - PI2
                             IF ( DELTA_PH < 0.0 ) DELTA_PH = DELTA_PH + PI2
                             DP(LP(J10,J9),J10,J9) = DELTA_PH
                        END IF
 4100                CONTINUE 
 490              CONTINUE 
 480           CONTINUE 
!
! ------------ Compute the median of phase-cal difference over pairs of IFs
! ------------ in the BACKWARD mode
! ------------ DP_MED_BACK(i,j) = phase(i) - phase(j)
!
               DP_MED_BACK = PHS_MAX
               DO 4110 J11=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                  DO 4120 J12=J11+1,PIM%CONF%END_FRQ
                     IF ( LP(J12,J11) > 0 ) THEN
                          CALL SORT_R8 ( LP(J12,J11), DP(1,J12,J11) )
!
! ----------------------- Compute the median of the difference
!
                          DP_MED_BACK(J12,J11) = DP ( MAX(1,LP(J12,J11)/2),J12,J11 ) 
                     END IF
 4120             CONTINUE 
 4110          CONTINUE 
!
! ------------ Compute phase-cal phases and amplidues among good records in the BACKWARD mode
!
               LS = 0
               PHS = PHS_MAX
               DO 4140 J14=IND_Z(J7,J6,J5),1,-1
                  DO 4150 J15=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                     IF ( MASK_G(J15,J14,J6,J5) == 1 ) THEN
                          IF ( LS(J15) == PIM__NRP ) GOTO 4150
                          LS(J15) = LS(J15) + 1
                          PHS(LS(J15),J15) = PIM%STA(ISTA)%PCAL(IFRG)%PHAS(J6,J15,J14,J5)
                          AMP(LS(J15),J15) = PIM%STA(ISTA)%PCAL(IFRG)%AMPL(J6,J15,J14,J5)
                     END IF
 4150             CONTINUE 
 4140          CONTINUE 
!
! ------------ Compute the median among the good phases or amplitudes
! ------------ in the BACKWARD mode
!
               PHS_MED_BACK = PHS_MAX
               DO 4160 J16=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                  IF ( LS(J16) > 0 ) THEN
                       CALL SORT_R8 ( LS(J16), PHS(1,J16) )
                       PHS_MED_BACK(J16) = PHS( MAX(1,LS(J16)/2),J16)
                       AMP_MED_BACK(J16) = AMP( MAX(1,LS(J16)/2),J16)
                  END IF
 4160          CONTINUE 
!
! ------------ Cycle over bad pcals FORWARD
!              ============================
!
               LP = 0
               DP = 0.0D0
!
! ------------ Compute differential phases over PIM__NRP observations 
! ------------ overall all compbinations of differences in IFs FORWARD
!
               DO 4180 J18=IND_Z(J7,J6,J5),PIM%STA(ISTA)%PCAL(IFRG)%NPOI
                  DO 4190 J19=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                     DO 4200 J20=J19+1,PIM%CONF%END_FRQ
                        IF ( LP(J20,J19) == PIM__NRP ) GOTO 4200
                        IF ( MASK_G(J20,J18,J6,J5) == 1 .AND. MASK_G(J19,J18,J6,J5) == 1 ) THEN
                             LP(J20,J19) = LP(J20,J19) + 1
                             DELTA_PH = PIM%STA(ISTA)%PCAL(IFRG)%PHAS(J6,J20,J18,J5) - &
     &                                  PIM%STA(ISTA)%PCAL(IFRG)%PHAS(J6,J19,J18,J5)
                             IF ( DELTA_PH > PI2 ) DELTA_PH = DELTA_PH - PI2
                             IF ( DELTA_PH < 0.0 ) DELTA_PH = DELTA_PH + PI2
                             DP(LP(J20,J19),J20,J19) = DELTA_PH
                        END IF
 4200                CONTINUE 
 4190             CONTINUE 
 4180          CONTINUE 
!
! ------------ Compute the median of phase-cal difference over pairs of IFs
! ------------ in the FOWARD mode
! ------------ DP_MED_FORW(i,j) = phase(i) - phase(j)
!
               DP_MED_FORW = PHS_MAX
               DO 4210 J21=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                  DO 4220 J22=J21+1,PIM%CONF%END_FRQ
                     IF ( LP(J22,J21) > 0 ) THEN
                          CALL SORT_R8 ( LP(J22,J21), DP(1,J22,J21) )
!
! ----------------------- Compute the median of the difference
!
                          DP_MED_FORW(J22,J21) = DP ( MAX(1,LP(J22,J21)/2),J22,J21 ) 
                     END IF
 4220             CONTINUE 
 4210          CONTINUE 
!
! ------------ Compute phase-cal phases or amplitudes among good records in the FORWARD mode
!
               LS = 0
               PHS = PHS_MAX
               DO 4240 J24=IND_Z(J7,J6,J5),PIM%STA(ISTA)%PCAL(IFRG)%NPOI
                  DO 4250 J25=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                      IF ( MASK_G(J25,J24,J6,J5) == 1 ) THEN
                           IF ( LS(J25) == PIM__NRP ) GOTO 4250
                           LS(J25) = LS(J25) + 1
                           PHS(LS(J25),J25) = PIM%STA(ISTA)%PCAL(IFRG)%PHAS(J6,J25,J24,J5)
                           AMP(LS(J25),J25) = PIM%STA(ISTA)%PCAL(IFRG)%AMPL(J6,J25,J24,J5)
                     END IF
 4250             CONTINUE 
 4240          CONTINUE 
!
! ------------ Compute the median among the good phases or ampltides in the FORWARD mode
!
               PHS_MED_FORW = PHS_MAX
               DO 4260 J26=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                  IF ( LS(J26) > 0 ) THEN
                       CALL SORT_R8 ( LS(J26), PHS(1,J26) )
                       PHS_MED_FORW(J26) = PHS( MAX(1,LS(J26)/2),J26)
                       AMP_MED_FORW(J26) = AMP( MAX(1,LS(J26)/2),J26)
                  END IF
 4260          CONTINUE 
!
! ------------ Cycle over IFs
!
               DO 4270 J27=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                  IF ( MASK_F(J27) == 0 ) THEN
!
! -------------------- This is the IF with zero pcal
! -------------------- ind_sub ( nton, nfrq, npoi, npol )
!
!
                       L_SUB = L_SUB + 1
                       IND_SUBS(J6,J27,IND_Z(J7,J6,J5),J5) = L_SUB
                       PHS_SUB_BACK    = PHS_MAX
                       PHS_SUB_FORW    = PHS_MAX
                       PHS_SUBS(L_SUB) = PHS_MAX
                       KPF = 0
                       KPB = 0
!
! -------------------- Check other IFs and pick up good pcal
!
                       DO 4280 J28=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
                          IF ( MASK_F(J28) == 1 ) THEN
                               IF ( J28 > J27 ) THEN
                                    DPM_BACK = -DP_MED_BACK(J28,J27)
                                    DPM_FORW = -DP_MED_FORW(J28,J27)
                                  ELSE
                                    DPM_BACK =  DP_MED_BACK(J27,J28)
                                    DPM_FORW =  DP_MED_FORW(J27,J28)
                               END IF
!
                               IF ( ABS(DPM_BACK) < PHS_MAX/2 ) THEN
                                    KPB = KPB + 1
                                    IF ( KPB == 1 ) THEN
!
! -------------------------------------- It was the first IF to be used as a substitute
!
                                         PHS_SUB_BACK = PHS_MED_BACK(J28) + DPM_BACK
!
! -------------------------------------- Resolve the ambiguity
!
                                         IF ( PHS_SUB_BACK > PI2 ) PHS_SUB_BACK = PHS_SUB_BACK - PI2
                                         IF ( PHS_SUB_BACK < 0.0 ) PHS_SUB_BACK = PHS_SUB_BACK + PI2
                                       ELSE
                                         IAMB_BACK = IDNINT ( (PHS_MED_BACK(J28) + DPM_BACK - PHS_SUB_BACK)/PI2 )
                                         PHS_SUB_BACK = (PHS_SUB_BACK*(KPB-1) + PHS_MED_BACK(J28) + DPM_BACK - IAMB_BACK*PI2 )/KPB 
                                    END IF
                               END IF
!
                               IF ( ABS(DPM_FORW) < PHS_MAX/2 ) THEN
                                    KPF = KPF + 1
                                    IF ( KPF == 1 ) THEN
                                         PHS_SUB_FORW = PHS_MED_FORW(J28) + DPM_FORW
                                         IF ( PHS_SUB_FORW > PI2 ) PHS_SUB_FORW = PHS_SUB_FORW - PI2
                                         IF ( PHS_SUB_FORW < 0.0 ) PHS_SUB_FORW = PHS_SUB_FORW + PI2
                                       ELSE
                                         IAMB_FORW = IDNINT ( (PHS_MED_FORW(J28) + DPM_FORW - PHS_SUB_FORW)/PI2 )
                                         PHS_SUB_FORW = (PHS_SUB_FORW*(KPF-1) + PHS_MED_FORW(J28) + DPM_FORW - IAMB_FORW*PI2 )/KPF
                                    END IF
                               END IF
                          END IF
 4280                  CONTINUE 
!
                       IF ( KPB == 0 .AND. KPF == 0 ) THEN
                            L_SUB = L_SUB - 1
                            GOTO 4270
                          ELSE IF ( KPB  > 0  .AND.  KPF == 0 ) THEN
                            PHS_SUBS(L_SUB) = PHS_SUB_BACK
                            AMP_SUBS(L_SUB) = AMP_MED_BACK(J27)
                          ELSE IF ( KPB == 0  .AND.  KPF  > 0 ) THEN
                            PHS_SUBS(L_SUB) = PHS_SUB_FORW
                            AMP_SUBS(L_SUB) = AMP_MED_FORW(J27)
                          ELSE IF ( KPB  > 0  .AND.  KPF  > 0 ) THEN
                            PHS_DIF = PHS_SUB_FORW - PHS_SUB_BACK
                            PHS_DIF = PHS_DIF - PI2*IDNINT(PHS_DIF/PI2)
                            IF ( DABS(PHS_DIF) < PHS_MIN_TOL ) THEN
                                 PHS_SUBS(L_SUB) = PHS_SUB_BACK + PHS_DIF/2.0
                               ELSE
                                 PHS_SUBS(L_SUB) = PHS_SUB_BACK 
                            END IF
                            AMP_SUBS(L_SUB) = ( AMP_MED_BACK(J27) + AMP_MED_FORW(J27) )/2.0
                       END IF
                       IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                            WRITE ( 6, 210 ) PIM%C_STA(ISTA), IFRG, L_SUB, J6, J27, &
     &                                       IND_Z(J7,J6,J5), J5, PHS_SUBS(L_SUB) 
 210                        FORMAT ( 'PIMA_PCAL_FIX_ZERO Sta: ', A, ' Ifrg: ', I1, ' L_sub: ', I6, &
     &                               ' Ind_tone: ', I5, ' Ind_frq: ', I4, ' Ind_pt: ', I6, &
     &                               ' Ind_pol: ', I1, ' Phas_sub: ', F18.5, ' rad' )
                       END IF
                  END IF
 4270          CONTINUE 
 470        CONTINUE 
 460     CONTINUE 
 450  CONTINUE 
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           IF ( PIM%STA(ISTA)%PCAL(IFRG)%NPOL == 1 .AND. IND_TONE > 0 ) THEN
                WRITE ( 6, 110 ) PIM%C_STA(ISTA), IFRG, NZ_POL(1), PIM%STA(ISTA)%PCAL(IFRG)%NPOI
 110            FORMAT ( 'PIMA_PCAL_FIX_ZERO Station: ', A, ' Ifrg= ', I1, &
     &                   ' Nz= ', I5, ' Npoi: ', I5 )
             ELSE IF ( PIM%STA(ISTA)%PCAL(IFRG)%NPOL == 2 .AND. IND_TONE > 0 ) THEN
                WRITE ( 6, 120 ) PIM%C_STA(ISTA), IFRG, NZ_POL(1:2), PIM%STA(ISTA)%PCAL(IFRG)%NPOI
 120            FORMAT ( 'PIMA_PCAL_FIX_ZERO Station: ', A, ' Ifrg= ', I1, &
     &                   ' Nz= ', I5, 1X, I5, ' Npoi: ', I5 )
           END IF
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
           WRITE ( 6, 130 ) L_SUB, PIM%C_STA(ISTA) 
 130       FORMAT ( 'In total, ', I6, ' pcal substitutions were found for ', A )
      END IF
!
      IF ( ASSOCIATED ( PIM%STA(ISTA)%PCAL(IFRG)%IND_SUBS ) ) THEN
           DEALLOCATE ( PIM%STA(ISTA)%PCAL(IFRG)%IND_SUBS )
      END IF
!
      IF ( ASSOCIATED ( PIM%STA(ISTA)%PCAL(IFRG)%PHAS_SUBS ) ) THEN
           DEALLOCATE ( PIM%STA(ISTA)%PCAL(IFRG)%PHAS_SUBS )
      END IF
!
      IF ( ASSOCIATED ( PIM%STA(ISTA)%PCAL(IFRG)%AMPL_SUBS ) ) THEN
           DEALLOCATE ( PIM%STA(ISTA)%PCAL(IFRG)%AMPL_SUBS )
      END IF
!
      PIM%STA(ISTA)%PCAL(IFRG)%LPOI_SUBS = L_SUB
      ALLOCATE ( PIM%STA(ISTA)%PCAL(IFRG)%IND_SUBS(PIM%STA(ISTA)%PCAL(IFRG)%NO_TONES,PIM%NFRQ,PIM%STA(ISTA)%PCAL(IFRG)%NPOI,PIM%STA(ISTA)%PCAL(IFRG)%NPOL), &
     &           PIM%STA(ISTA)%PCAL(IFRG)%PHAS_SUBS(L_SUB), &
     &           PIM%STA(ISTA)%PCAL(IFRG)%AMPL_SUBS(L_SUB), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7681, IUER, 'PIMA_PCAL_FIX_ZERO', 'Error in '// &
     &         'an attempt to allocate memory for %IND_SUB, %PHAS_SUB, '// &
     &         '%AMPL_SUB of the pcal objects' )
           RETURN 
      END IF
      PIM%STA(ISTA)%PCAL(IFRG)%IND_SUBS  = IND_SUBS
      PIM%STA(ISTA)%PCAL(IFRG)%PHAS_SUBS(1:L_SUB) = PHS_SUBS(1:L_SUB)
      PIM%STA(ISTA)%PCAL(IFRG)%AMPL_SUBS(1:L_SUB) = AMP_SUBS(1:L_SUB)
!
      DEALLOCATE ( MASK_G, IND_Z, IND_G, IND_SUBS )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
           IF ( L_SUB > 0 ) THEN
                WRITE ( 6, '(A)' ) ' '
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PCAL_FIX_ZERO  !#!#
