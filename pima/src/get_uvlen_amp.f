      SUBROUTINE GET_UVLEN_AMP ( VIS, GAP_SCAN, FL_WEI_USE, FL_AUTO, &
     &                           CUTOFF_NERR, M_POI, L_POI, L_SCA, &
     &                           UV_LEN, AMP, AMP_ERR, TAI_AVR, BAS_IND, &
     &                           IVRB, PHI_MIN, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_UVLEN_AMP  analyzes the array of calibrated           *
! *   visibilities, splits the data into scans, and averages the         *
! *   complex visibility over frequencies and over time within one       *
! *   scan at each baseline and computes the amplitude of the complex    *
! *   averaged amplitude. This routine also computes the statistical     *
! *   uncertainty of the mean value on the basis of the scatter with     *
! *   respect to the average amplitude. The averaged value with          *
! *   statistical uncertainties exceeding CUTOFF_NERRR*Amp are discarded *
! *   and not put in the output array. Negative weights stored together  *
! *   with visibility data are interpreted as flags, and such points     *
! *   are not used for computations. If FL_WEI_USE == .TRUE. the weights *
! *   of each individual visibility are used for computing the mean      *
! *   amplitude and its statistical uncertainty. If FL_WEI_USE == .FALSE.*
! *   then unit weights are used instead of positive weights stored      *
! *   together visibility data. However, if the weight is negative,      *
! *   the point is excluded from averaging in any case.                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         VIS ( VIS__TYPE ) -- object which keeps variables related to *
! *                              the visibility data for this source.    *
! *    GAP_SCAN ( REAL*8    ) -- The maximum gap in the data within one  *
! *                              scan. The data points with time tag     *
! *                              greater than GAP_SCAN are considered    *
! *                              to belonging to different scans.        *
! * FL_WEI_USE  ( LOGICAL*4 ) -- If .TRUE., then weights from the        *
! *                              file with visibilities will be used     *
! *                              for averaging and computing the         *
! *                              statistical uncertainties. If .FALSE.,  *
! *                              then the weights 1.0 will used for      *
! *                              computations, provided the weight in    *
! *                              the visibility data is positive (and    *
! *                              the point will be discarded if weight   *
! *                              is negative).                           *
! * FL_AUTO    ( LOGICAL*4 )  -- If .FALSE.,  then GEN_RADPLOT makes a   *
! *                              plot of calibrated amplitude versus     *
! *                              baseline length. IF .TRUE. then         *
! *                              GEN_RADPLOT makes a plot versus the     *
! *                              length of the baseline projection to    *
! *                              the direction which makes the scatter   *
! *                              of the amplitude with respect to        *
! *                              a smoothed curve minimal.               *
! * CUTOFF_NERR ( REAL*8    ) -- The points with the normalized          *
! *                              statistical uncertainties determined    *
! *                              as Err(Amp)/Amp exceeding CUTOFF_NERR   *
! *                              are flagged out and removed from the    *
! *                              output array. Here Err(Amp) is the      *
! *                              statistical error of the averaged       *
! *                              amplitude determined on the basis of    *
! *                              the scatter with respect to average and *
! *                              Amp is the averaged amplitude.          *
! *       M_POI ( INTEGER*4 ) -- The maximum number of points of the     *
! *                              output arrays.                          *
! *        IVRB ( INTEGER*4 ) -- Verbosity level.                        *
! *                              IVRB = 0 -- silent.                     *
! *                              IVRB = 2 -- the table is printed in the *
! *                                          output file FILOUT.         *
! *      FILOUT ( CHARACTER ) -- Output file. Used when IVRB, otherwise  *
! *                              is ignored.                             *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     L_POI ( INTEGER*4 ) -- The number of points of the averaged      *
! *                            visibility data.                          *
! *     L_SCA ( INTEGER*4 ) -- The number of scans.                      *
! *    UV_LEN ( REAL*8    ) -- Array of averaged UV lengths. Dimension:  *
! *                            M_POI.                                    *
! *       AMP ( REAL*8    ) -- Array of averaged visibilities.           *
! *                            Dimension: M_POI.                         *
! *   AMP_ERR ( REAL*8    ) -- Array of statistical uncertainties of the *
! *                            amplitudes of averaged visibilities.      *
! *                            The errors are computed on the basis of   *
! *                            deviation with respect to the averaged    *
! *                            value.                                    *
! *   TAI_AVR ( REAL*8    ) -- Array of time epochs with respect to      *
! *                            MJD_REF.                                  *
! *   BAS_IND ( INTEGER*4 ) -- Array of baseline indices.                *
! *   PHI_MIN ( REAL*8    ) -- projection angle that provides minimum    *
! *                            scatter with respect to a smooth curve    *
! *                            for a dependence calibrated amplitude     *
! *                            versus length of the projection of the    *
! *                            baseline to that direction angle.         *
! *                            Units: rad.                               *
! *                            If FL_AUTO = .FALSE., then PHI_MIN = 0.0  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the case of errors. *
! *                                                                      *
! *  ### 30-JAN-2007  GET_UVLEN_AMP  v4.2 (c)  L. Petrov 24-JUL-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE    'sou_map.i'
      TYPE     ( VIS__TYPE ) :: VIS
      INTEGER*4  M_POI, L_POI, L_SCA, BAS_IND(M_POI), IVRB, IUER
      REAL*8     GAP_SCAN, CUTOFF_NERR, UV_LEN(M_POI), AMP(M_POI), &
     &           AMP_ERR(M_POI), TAI_AVR(M_POI)
      CHARACTER  FILOUT*(*)
      LOGICAL*4  FL_WEI_USE, FL_AUTO
      INTEGER*4  M_SCA, M_BAS, M_ANG, M_PRJ, DEG, M_NOD, M_OVR, MIN_NOD
      REAL*8     WEI_MIN, SIG_SCL
      PARAMETER  ( M_SCA   = 8192 )
      PARAMETER  ( M_BAS   = 1024 )
      PARAMETER  ( M_PRJ = 32*1024 )
      PARAMETER  ( M_ANG   =  180 )
      PARAMETER  ( M_NOD   = 1024 )
      PARAMETER  ( DEG     =    3 )
      PARAMETER  ( M_OVR   =    6 )
      PARAMETER  ( MIN_NOD =    3 )
      PARAMETER  ( WEI_MIN = 1.D-15 )
      PARAMETER  ( SIG_SCL = 0.3 )
      REAL*8     TAI_SCA(M_SCA), WW_BAS(M_BAS), LEN_BAS(M_BAS), &
     &           U_BAS(M_BAS), V_BAS(M_BAS), TIM_AVR(M_BAS), &
     &           U_PRJ(M_PRJ), V_PRJ(M_PRJ), WW_PRJ(M_PRJ), &
     &           WW_SQ, RMS_SQ, UV_NOD(M_NOD), UV_SPL(M_NOD+DEG), &
     &           PHI, UV_MAX, AMP_MIN, AMP_MAX, SIG_CNS, WRMS, &
     &           WRMS_MIN, PHI_MIN
      COMPLEX*8  AMP_BAS(M_BAS)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           KP, IND_SCA(2,M_SCA), L_NOD, &
     &           MJD_SCA(M_SCA), I_BAS, L_BAS, IND_BAS(M_POI), &
     &           IND_STA_SUB(2), IND_STA(2), LUN, IP, IND_SUB, IER
      CHARACTER  STR*256, STR_RADPLOT_PHI_MIN*16
      LOGICAL*1, ALLOCATABLE :: FLAG(:)
      REAL*4,    ALLOCATABLE :: WEI(:,:)
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: ADD_LIS, IFIND_PL, ILEN, I_LEN, GET_UNIT
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
!
      IF ( IVRB == 2 ) THEN
           IF ( FILOUT == '-' ) THEN
                LUN = 6
              ELSE 
                LUN = GET_UNIT()
                OPEN ( FILE=FILOUT, UNIT=LUN, STATUS='UNKNOWN', IOSTAT=IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 1611, IUER, 'GET_UVLEN_AMP', 'Error in '// &
     &                   'attempt to open output file '//FILOUT )
                     RETURN 
                END IF
           END IF
      END IF
!
! --- Get the number of scans and their dates.
! --- Create array of scan indexes IND_SCA and dates of the beginning of the
! --- scan
!
      ALLOCATE ( FLAG(M_POI), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( M_POI, STR )
           CALL ERR_LOG ( 1611, IUER, 'GET_UVLEN_AMP', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dymanic memory' )
           RETURN
      END IF
!
      ALLOCATE ( WEI(VIS%NFRQ,VIS%NP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( VIS%NP*VIS%NFRQ, STR )
           CALL ERR_LOG ( 1612, IUER, 'GET_UVLEN_AMP', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dymanic memory' )
           RETURN
      END IF
!
      L_SCA = 0
      CALL NOUT_I4 ( 2*L_SCA, IND_SCA )
      IF ( VIS%NP .LE. 0 ) THEN
         ELSE IF ( VIS%NP == 1 ) THEN
           L_SCA = 1
           IND_SCA(1,L_SCA) = 1
           IND_SCA(2,L_SCA) = 1
         ELSE IF ( VIS%NP > 1 ) THEN
           L_SCA = 1
           IND_SCA(1,L_SCA) = 1
           MJD_SCA(L_SCA) = VIS%MJD(1)
           TAI_SCA(L_SCA) = VIS%TAI(1)
!
! -------- Check all points
!
           DO 410 J1=2,VIS%NP
              IF ( ( VIS%MJD(J1) - VIS%MJD(J1-1) )*86400.0D0 + &
     &             ( VIS%TAI(J1) - VIS%TAI(J1-1) ) > GAP_SCAN  ) THEN
!
! ---------------- The gap between points is greater than GAP_SCAN
!
                   IND_SCA(2,L_SCA) = J1-1
                   L_SCA = L_SCA + 1        ! increment the scan counter
                   IF ( L_SCA > M_SCA ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( M_SCA, STR )
                         CALL ERR_LOG ( 1611, IUER, 'GET_UVLEN_AMP', &
     &                       'Trap of internal control: too many scans. '// &
     &                       'Increase parameters M_SCA: '//STR )
                         DEALLOCATE ( FLAG )
                         DEALLOCATE ( WEI  )
                         RETURN
                   END IF
                   IND_SCA(1,L_SCA) = J1
                   MJD_SCA(L_SCA) = VIS%MJD(J1)
                   TAI_SCA(L_SCA) = VIS%TAI(J1)
              END IF
 410       CONTINUE
           IND_SCA(2,L_SCA) = VIS%NP
      END IF
!
! --- Compute the correlated flux density averaged over scans and over
! --- intermediate frequencies
!
      IP = 0
      L_POI = 0
      DO 420 J2=1,L_SCA  ! Cycle over scans
!
! ------ Initialization of accumulators
!
         CALL NOUT_R8 (   M_BAS, WW_BAS  )
         CALL NOUT_R4 ( 2*M_BAS, AMP_BAS )
         CALL NOUT_R8 (   M_BAS,   U_BAS )
         CALL NOUT_R8 (   M_BAS,   V_BAS )
         CALL NOUT_R8 (   M_BAS, LEN_BAS )
         CALL NOUT_R8 (   M_BAS, TIM_AVR )
         L_BAS = 0
!
! ------ Cycle over epochs of the scan
!
         DO 430 J3=IND_SCA(1,J2),IND_SCA(2,J2)
!
! --------- ... and over intermediate frequencies of the scan
!
            IP = IP + 1
            DO 440 J4=1,VIS%NFRQ
               IF ( IVRB .GE. 2 ) THEN
                    IND_SUB    = NINT(100.0*(VIS%IND_BAS(J3) - INT(VIS%IND_BAS(J3)))) + 1
                    IND_STA_SUB(1) = VIS%IND_BAS(J3)/256
                    IND_STA_SUB(2) = VIS%IND_BAS(J3) - IND_STA_SUB(1)*256
                    IND_STA(1) = VIS%LIS_STA(IND_STA_SUB(1),IND_SUB)
                    IND_STA(2) = VIS%LIS_STA(IND_STA_SUB(2),IND_SUB)
                    IF ( VIS%WEI(J4,J3) < 1.D6 ) THEN
                         WRITE  ( LUN, 210 ) IP, J4, VIS%C_STA(IND_STA(1)), &
     &                                       VIS%C_STA(IND_STA(2)), VIS%MJD(J3), &
     &                                       VIS%TAI(J3), ABS(VIS%VIS(J4,J3)), &
     &                                       PHAS_CMPL_R4(VIS%VIS(J4,J3)), VIS%WEI(J4,J3)
 210                     FORMAT ( 'Cmp: ', I5, ' Ifrq: ', I3, ' Sta: ', A8, ' / ', A8, &
     &                            ' MJD= ', I6, ' Tai: ', F8.2, ' Vis_amp: ', F9.6, &
     &                            ' Vis_phs: ', F8.5, ' Wei: ', F12.5 )
                       ELSE
                         WRITE  ( LUN, 220 ) IP, J4, VIS%C_STA(IND_STA(1)), &
     &                                       VIS%C_STA(IND_STA(2)), VIS%MJD(J3), &
     &                                       VIS%TAI(J3), ABS(VIS%VIS(J4,J3)), &
     &                                       PHAS_CMPL_R4(VIS%VIS(J4,J3)), VIS%WEI(J4,J3)
 220                     FORMAT ( 'Cmp: ', I5, ' Ifrq: ', I3, ' Sta: ', A8, ' / ', A8, &
     &                            ' MJD= ', I6, ' Tai: ', F8.2, ' Vis_amp: ', F9.6, &
     &                            ' Vis_phs: ', F8.5, ' Wei: ', 1PE12.5 )
                    END IF
               END IF
               IF ( FL_WEI_USE ) THEN
                    WEI(J4,J3) = VIS%WEI(J4,J3)
                  ELSE
                    IF ( VIS%WEI(J4,J3) > WEI_MIN ) THEN
                         WEI(J4,J3) =  1.0
                       ELSE
                         WEI(J4,J3) = -1.0
                    END IF
               END IF
               IF ( WEI(J4,J3) > WEI_MIN ) THEN
!
! ----------------- Only points with weights more than a limit are used
!
! ----------------- Get the baseline index
!
                    CALL ERR_PASS ( IUER, IER )
                    I_BAS = ADD_LIS ( M_BAS, L_BAS, IND_BAS, INT(VIS%IND_BAS(J3)), &
     &                                IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( M_BAS, STR )
                         CALL ERR_LOG ( 1612, IUER, 'GET_UVLEN_AMP', &
     &                       'Trap of internal control: too many baselines. '// &
     &                       'Increase parameters M_BAS: '//STR )
                         DEALLOCATE ( FLAG )
                         DEALLOCATE ( WEI  )
                         RETURN
                    END IF
!
! ----------------- Update accumulators
!
                    U_BAS(I_BAS)   =   U_BAS(I_BAS) + WEI(J4,J3)*VIS%UV(1,J4,J3)
                    V_BAS(I_BAS)   =   V_BAS(I_BAS) + WEI(J4,J3)*VIS%UV(2,J4,J3)
                    WW_BAS(I_BAS)  =  WW_BAS(I_BAS) + WEI(J4,J3)
                    AMP_BAS(I_BAS) = AMP_BAS(I_BAS) + WEI(J4,J3)*VIS%VIS(J4,J3)
                    TIM_AVR(I_BAS) = TIM_AVR(I_BAS) + WEI(J4,J3)*(VIS%TAI(J3) + &
     &                               (VIS%MJD(J3) - VIS%MJD_REF)*86400.0D0)
!
               END IF
 440        CONTINUE
 430     CONTINUE
!
         IF ( L_BAS > 0 ) THEN
!
! ----------- Cycle over baselines. Compute the averaged ampliutde, averaged
! ----------- baseline projection and the amplitude error for each baseline
! ----------- individually
!
              DO 450 J5=1,L_BAS
                 IF ( WW_BAS(J5) > WEI_MIN ) THEN
                      L_POI = L_POI + 1
                      IF ( L_POI > M_POI ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( L_POI, STR )
                           CALL ERR_LOG ( 1613, IUER, 'GET_UVLEN_AMP', &
     &                         'Trap of internal control: too many '// &
     &                         'points. Increase parameters M_POI: '//STR )
                           DEALLOCATE ( FLAG )
                           DEALLOCATE ( WEI  )
                           RETURN
                      END IF
!
                      U_PRJ(L_POI)   = U_BAS(J5)/WW_BAS(J5)
                      V_PRJ(L_POI)   = V_BAS(J5)/WW_BAS(J5)
                      UV_LEN(L_POI)  = DSQRT ( U_PRJ(L_POI)**2 + V_PRJ(L_POI)**2 )
                      WW_PRJ(L_POI)  = WW_BAS(J5)
                      AMP(L_POI)     = ABS(AMP_BAS(J5))/WW_BAS(J5)
                      TAI_AVR(L_POI) = TIM_AVR(J5)/WW_BAS(J5)
                      BAS_IND(L_POI) = IND_BAS(J5)
!
! ------------------- Compute the rms of the amplitude scatter with respect to
! ------------------- the average
!
                      WW_SQ  = 0.0D0
                      RMS_SQ = 0.0D0
                      KP = 0
                      DO 460 J6=IND_SCA(1,J2),IND_SCA(2,J2)
                         DO 470 J7=1,VIS%NFRQ
                            IF ( WEI(J7,J6) > WEI_MIN  .AND. &
     &                           INT(VIS%IND_BAS(J6)) == IND_BAS(J5)  ) THEN
!
                                 WW_SQ  = WW_SQ + WEI(J7,J6)**2
                                 RMS_SQ = RMS_SQ + &
     &                                    ( ( ABS(VIS%VIS(J7,J6)) - AMP(L_POI) )* &
     &                                        WEI(J7,J6) )**2
                                 KP = KP + 1
                            END IF
 470                     CONTINUE
 460                  CONTINUE
                      AMP_ERR(L_POI) = DSQRT ( RMS_SQ/(KP*WW_SQ) )
!
                      IF ( AMP_ERR(L_POI) > CUTOFF_NERR*AMP(L_POI) ) THEN
!
! ------------------------ If the error is too big, discard the point
!
                           L_POI = L_POI - 1
                      END IF
                 END IF
 450          CONTINUE
         END IF
 420  CONTINUE
!
      IF ( L_POI < M_OVR*MIN_NOD ) THEN
           FL_AUTO = .FALSE.
      END IF
      IF ( FL_AUTO ) THEN
!
! -------- We need to find the angle that makes the scatter with 
! -------- respect to a smooth curve minimum. Such a curve is
! -------- B-spline. We check all anbles in a range [0, pi]
!
           L_NOD = L_POI/M_OVR
           WRMS_MIN = 1.D8
           DO 480 J8=1,M_ANG
!
! ----------- This is a trial angle
!
              PHI = (J8-1)*PI__NUM/M_ANG
              UV_MAX  = -1.0D0
              AMP_MAX = -1.0D0
              AMP_MIN =  1.0D8
              DO 490 J9=1,L_POI
!
! -------------- Get projection length
!
                 UV_LEN(J9) = DABS ( U_PRJ(J9)*DSIN(PHI) + V_PRJ(J9)*DCOS(PHI) )
                 IF ( UV_LEN(J9) > UV_MAX ) UV_MAX = UV_LEN(J9)
                 IF ( AMP(J9) < AMP_MIN ) AMP_MIN = AMP(J9)
                 IF ( AMP(J9) > AMP_MAX ) AMP_MAX = AMP(J9)
 490          CONTINUE 
!
! ----------- Set constraints
!
              SIG_CNS = SIG_SCL*AMP_MIN
!
! ----------- Set knots of the B-spline
!
              DO 4100 J10=1,L_NOD
                 UV_NOD(J10) = (J10-1)*UV_MAX/(L_NOD-1)
 4100         CONTINUE 
              UV_NOD(L_NOD) = (1.0D0 + 0.00001)*UV_NOD(L_NOD) 
!
! ----------- Compute coefficients of the *smoothing* B-spline that fits
! ----------- AMP versus UV_LEN. Paramter rms will provide root mean square
! ----------- of residuals
!
              CALL ERR_PASS ( IUER, IER )
              CALL EBSPL_WLSQ_CNS ( L_NOD, L_POI, UV_LEN, AMP, WW_PRJ, &
     &                               DEG, UV_NOD, UV_SPL, &
     &                               SIG_CNS, 0.0D0, 0.0D0, WRMS, IER )
              IF ( IS_R8_NAN ( WRMS ) ) WRMS = 1.0D6
              IF ( WRMS < WRMS_MIN ) THEN
!
! ---------------- Check whether the WRMS for angle PHI provides the minimum
!
                   WRMS_MIN = WRMS
                   PHI_MIN  = PHI
              END IF
 480       CONTINUE 
!
           CALL GETENVAR ( 'RADPLOT_PHI_MIN', STR_RADPLOT_PHI_MIN )
           IF ( ILEN(STR_RADPLOT_PHI_MIN) > 0 ) THEN
                IF ( INDEX ( STR_RADPLOT_PHI_MIN, '.' ) < 1 ) THEN
                     STR_RADPLOT_PHI_MIN = TRIM(STR_RADPLOT_PHI_MIN)//'.0'
                END IF 
                READ ( UNIT=STR_RADPLOT_PHI_MIN, FMT='(F10.3)' ) PHI_MIN
                PHI_MIN = PHI_MIN*DEG__TO__RAD
           END IF 
!
! -------- Compute final array of UV projections for the angle that 
! -------- provides minimum scattering.
!
           DO 4110 J11=1,L_POI
              UV_LEN(J11) = DABS ( U_PRJ(J11)*DSIN(PHI_MIN) + V_PRJ(J11)*DCOS(PHI_MIN) )
 4110      CONTINUE 
         ELSE 
           PHI_MIN = 0.0D0
      END IF
!
      IF ( IVRB == 2 ) THEN
           IF ( FILOUT == '-' ) THEN
                CONTINUE 
              ELSE 
                CLOSE ( UNIT=LUN )
                WRITE ( 6, '(A)' ) 'UV data are written in '//FILOUT(1:I_LEN(FILOUT))
           END IF
      END IF
      DEALLOCATE ( FLAG )
      DEALLOCATE ( WEI  )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_UVLEN_AMP  !#!#
!@!
!@! ------------------------------------------------------------------------
!@!
!@      SUBROUTINE EBSPL_WLSQ_CNS ( MS, MF, ARG_VEC, FUN_VEC, WEI_VEC, &
!@     &                            DEG, ARG_NOD, SPL_VEC, &
!@     &                            CNS_VAL_SIG, CNS_DER_SIG, &
!@     &                            CNS_DR2_SIG, POSTFIT_WRMS, IUER )
!@! ************************************************************************
!@! *                                                                      *
!@! *   Routine  EBSPL_WLSQ computes coefficients of expansion of function *
!@! *   FUN_VEC of argument ARG_VEC which is defined as a table of         *
!@! *   MF arguments and values over the B-spline of degree DEG with       *
!@! *   MS knots using the weighted LSQ method. The number of knots MS     *
!@! *   should be no more than MF-2. Using another language, smoothing     *
!@! *   B-spline defined at MF knots is computed in such a way to minimize *
!@! *   the differences with the function FUN_VEC(ARG_VEC) in a mean       *
!@! *   quadratic sense with applying weights. The coefficients are stored *
!@! *   in array SPL_VEC of dimension [1-DEG:MS-1]. The weighted root mean *
!@! *   square of postfit residuals is computed as well. Weights of        *
!@! *   function values are supplied in array WEI_VEC. 
!@! *                                                                      *
!@! *   If CNS_VAL_SIG > 0.0D0, then a constraint on the value of the      *
!@! *      B-spline at each node is set to zero with recoprocal weight     *
!@! *      CNS_VAL_SIG.                                                    *
!@! *                                                                      *
!@! *   If CNS_DER_SIG > 0.0D0, then a constraint on the first derivatice  *
!@! *      of the B-spline at each node is set to zero with recoprocal     *
!@! *      weight CNS_DER_SIG.                                             *
!@! *                                                                      *
!@! *   If CNS_DR2_SIG > 0.0D0, then a constraint on the second derivatice *
!@! *      of the B-spline at each node is set to zero with recoprocal     *
!@! *      weight CNS_DR2_SIG.                                             *
!@! *                                                                      *
!@! *   Imposing constraints is necessary when there are some data losses. *
!@! *   NB: strong constraints ( CNS_VAL_SIG is small ) distort results.   *
!@! *                                                                      *
!@! * _________________________ Input parameters: ________________________ *
!@! *                                                                      *
!@! *           MS ( INTEGER*4 ) -- The number of knots of the B-spline.   *
!@! *           MF ( INTEGER*4 ) -- The number values of function that is  *
!@! *                               being expanded into B-spline basis.    *
!@! *      ARG_VEC ( REAL*8    ) -- Array arguments of the function to be  *
!@! *                               expanded. Dimension: MF.               *
!@! *      FUN_VEC ( REAL*8    ) -- Array values of the function to be     *
!@! *                               expanded. Dimension: MF.               *
!@! *      WEI_VEC ( REAL*8    ) -- Weight vector associated with values   *
!@! *                               of the function to be expanded.        *
!@! *                               Dimension: MF.                         *
!@! *          DEG ( INTEGER*4 ) -- Degree of the B-spline.                *
!@! *      ARG_NOD ( REAL*8    ) -- Array arguments of the B-spline. The   *
!@! *                               define knots of the spline.            *
!@! *                               Dimension: MS.                         *
!@! *  CNS_VAL_SIG ( REAL*8    ) -- Reciprocal weight of the constraint    *
!@! *                               imposed on value of B-spline           *
!@! *                               at every knot. If CNS_VAL_SIG <= 0.0D0,*
!@! *                               the constraint is not imposed.         *
!@! *  CNS_DER_SIG ( REAL*8    ) -- Reciprocal weight of the constraint    *
!@! *                               imposed on the first derivative        *
!@! *                               of B-spline at every knot.             *
!@! *                               If CNS_DER_SIG <= 0.0D0,               *
!@! *                               the constraint is not imposed.         *
!@! *  CNS_DR2_SIG ( REAL*8    ) -- Reciprocal weight of the constraint    *
!@! *                               imposed on the second derivative       *
!@! *                               of B-spline at every knot.             *
!@! *                               If CNS_DR2_SIG <= 0.0D0,               *
!@! *                               the constraint is not imposed.         *
!@! *                                                                      *
!@! * _________________________ Output parameters: _______________________ *
!@! *                                                                      *
!@! *      SPL_VEC ( REAL*8    ) -- Array of B-spline coefficients that    *
!@! *                               approximate the function.              *
!@! *                               Dimension: [1-DEG:MS-1].               *
!@! *  POSTFIT_WRMS ( REAL*8   ) -- Weighted Root mean square of postfit   *
!@!*                                residuals.                             *
!@! *                                                                      *
!@! * ________________________ Modified parameters: ______________________ *
!@! *                                                                      *
!@! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
!@! *                           Input: switch IUER=0 -- no error messages  *
!@! *                                  will be generated even in the case  *
!@! *                                  of error. IUER=-1 -- in the case of *
!@! *                                  error the message will be put on    *
!@! *                                  stdout.                             *
!@! *                           Output: 0 in the case of successful        *
!@! *                                   completion and non-zero in the     *
!@! *                                   case of error.                     *
!@! *                                                                      *
!@! * ### 25-MAR-2010  EBSPL_WLSQ_CNS v2.1 (c)  L. Petrov  15-NOV-2018 ### *
!@! *                                                                      *
!@! ************************************************************************
!@      IMPLICIT   NONE 
!@      INTEGER*4  MS, MF, DEG, IUER
!@      REAL*8     ARG_NOD(MS), ARG_VEC(MF), FUN_VEC(MF), WEI_VEC(MF), &
!@     &           SPL_VEC(1-DEG:MS-1), POSTFIT_WRMS, CNS_VAL_SIG, &
!@     &           CNS_DER_SIG, CNS_DR2_SIG
!@!
!@      REAL*8     ARG_VAL
!@      REAL*8     EPS
!@      PARAMETER  ( EPS = 1.D-6 )
!@      REAL*8,    ALLOCATABLE :: EQU_VEC(:), NOR_MAT(:), NOR_VEC(:)
!@      REAL*8     RC, RES, WW
!@      CHARACTER  STR*128
!@      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, LPAR, LPA2, PIV_IND, IER
!@      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, BSPL_DR2, DP_VV_V, EBSPL_VAL_R8 
!@      INTEGER*4, EXTERNAL :: I_LEN, IXMN8
!@!
!@      LPAR = MS + DEG - 1
!@      LPA2 = (LPAR*(LPAR+1))/2 
!@!
!@! --- Allocate dynamic memory for temporary arrays
!@!
!@      ALLOCATE ( EQU_VEC(LPAR), STAT=IER )
!@      IF ( IER .NE. 0 ) THEN
!@           CALL CLRCH ( STR )
!@           CALL IINCH ( LPAR, STR )
!@           CALL ERR_LOG ( 1911, IUER, 'EBSPL_WLSQ_CNS', 'Failure to allocate '// &
!@     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
!@     &          'temporary array EQU' )
!@           RETURN 
!@      END IF
!@      ALLOCATE ( NOR_MAT(LPA2), STAT=IER )
!@      IF ( IER .NE. 0 ) THEN
!@           CALL CLRCH ( STR )
!@           CALL IINCH ( LPA2, STR )
!@           CALL ERR_LOG ( 1912, IUER, 'EBSPL_WLSQ_CNS', 'Failure to allocate '// &
!@     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
!@     &          'temporary array NOR_MAT' )
!@           DEALLOCATE ( EQU_VEC )
!@           RETURN 
!@      END IF
!@      ALLOCATE ( NOR_VEC(LPAR), STAT=IER )
!@      IF ( IER .NE. 0 ) THEN
!@           CALL CLRCH ( STR )
!@           CALL IINCH ( LPAR, STR )
!@           CALL ERR_LOG ( 1913, IUER, 'EBSPL_WLSQ_CNS', 'Failure to allocate '// &
!@     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
!@     &          'temporary array NOR_VEC' )
!@           DEALLOCATE ( NOR_MAT )
!@           DEALLOCATE ( EQU_VEC )
!@           RETURN 
!@      END IF
!@!
!@! --- Initialization
!@!
!@      CALL NOUT_R8 ( LPAR, NOR_VEC )
!@      CALL NOUT_R8 ( LPAR, EQU_VEC )
!@      CALL NOUT_R8 ( LPA2, NOR_MAT )
!@!
!@! --- Cycle over array of arguments/function value and build the normal equation
!@!
!@      DO 410 J1=1,MF
!@         IF ( J1 == 1 ) THEN
!@              PIV_IND = 1
!@           ELSE IF ( J1 == MF ) THEN
!@              PIV_IND = MS - 1
!@           ELSE 
!@              PIV_IND = IXMN8 ( MS, ARG_NOD, ARG_VEC(J1) )
!@         END IF
!@!
!@         CALL NOUT_R8 ( LPAR, EQU_VEC )
!@!
!@! ------ Compute observation equation
!@!
!@         DO 420 J2=PIV_IND-DEG,PIV_IND
!@            EQU_VEC(J2+DEG) = BSPL_VAL ( MS, ARG_NOD, DEG, J2, ARG_VEC(J1) )
!@ 420     CONTINUE 
!@!
!@! ------ Compute its contribution to NOR_MAT ( normal matrix ) 
!@!
!@         CALL DIAD_CVT_S_PIV ( WEI_VEC(J1)**2, LPAR, PIV_IND, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
!@!
!@! ------ ... and NOR_VEC ( normal vector )
!@!
!@         DO 430 J3=PIV_IND,PIV_IND+DEG
!@            NOR_VEC(J3) = NOR_VEC(J3) + EQU_VEC(J3)*FUN_VEC(J1)*WEI_VEC(J1)**2
!@ 430     CONTINUE 
!@!
!@! ------ Constraints
!@!
!@ 410  CONTINUE 
!@      DO 440 J4=1,MS-1
!@!
!@! ------ Compute observation equation
!@!
!@         IF ( J4 == 1 ) THEN
!@              ARG_VAL = ARG_NOD(J4) + EPS*(ARG_NOD(2)  - ARG_NOD(1))
!@            ELSE IF ( J4 == MS ) THEN
!@              ARG_VAL = ARG_NOD(J4) - EPS*(ARG_NOD(MS) - ARG_NOD(MS-1))
!@            ELSE
!@              ARG_VAL = ARG_NOD(J4)
!@         END IF
!@         IF ( CNS_VAL_SIG > 0.0D0 ) THEN
!@              CALL NOUT_R8 ( LPAR, EQU_VEC )
!@              DO 450 J5=J4-DEG,J4
!@                 EQU_VEC(J5+DEG) = BSPL_VAL ( MS, ARG_NOD, DEG, J5, ARG_VAL )
!@ 450          CONTINUE 
!@!
!@! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!@!
!@              CALL DIAD_CVT_S_PIV ( 1.0D0/CNS_VAL_SIG**2, LPAR, J4, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
!@         END IF
!@!
!@         IF ( CNS_DER_SIG > 0.0D0 ) THEN
!@              CALL NOUT_R8 ( LPAR, EQU_VEC )
!@!
!@! ----------- Compute observation equation
!@!
!@              DO 460 J6=J4-DEG,J4
!@                 EQU_VEC(J6+DEG) = BSPL_DER ( MS, ARG_NOD, DEG, J6, ARG_VAL )
!@ 460          CONTINUE 
!@!
!@! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!@!
!@              CALL DIAD_CVT_S_PIV ( 1.0D0/CNS_DER_SIG**2, LPAR, J4, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
!@         END IF
!@         IF ( CNS_DR2_SIG > 0.0D0 ) THEN
!@              CALL NOUT_R8 ( LPAR, EQU_VEC )
!@!
!@! ----------- Compute observation equation
!@!
!@              DO 470 J7=J4-DEG,J4
!@                 EQU_VEC(J7+DEG) = BSPL_DR2 ( MS, ARG_NOD, DEG, J7, ARG_VAL )
!@ 470          CONTINUE 
!@!
!@! ----------- Compute its contribution to NOR_MAT ( normal matrix ) 
!@!
!@              CALL DIAD_CVT_S_PIV ( 1.0D0/CNS_DR2_SIG**2, LPAR, J4, DEG, EQU_VEC, EQU_VEC, NOR_MAT )
!@         END IF
!@ 440  CONTINUE 
!@!
!@! --- Invert normal matrix
!@!
!@      CALL ERR_PASS ( IUER, IER )
!@      CALL INVS ( LPAR, NOR_MAT, RC, IER )
!@      IF ( IER .NE. 0 ) THEN
!@           CALL ERR_LOG ( 1914, IUER, 'EBSPL_WLSQ_CNS', 'Failure to invert '// &
!@     &         'normal matrix' )
!@           DEALLOCATE ( NOR_VEC )
!@           DEALLOCATE ( NOR_MAT )
!@           DEALLOCATE ( EQU_VEC )
!@           CALL NOUT_R8 ( 1-DEG+MS, SPL_VEC )
!@           RETURN 
!@      END IF
!@!
!@! --- Compute array of spline coefficients using normal vector and the 
!@! --- inverse to normal matrix
!@!
!@      CALL MUL_MV_SV_V ( LPAR, NOR_MAT, LPAR, NOR_VEC, LPAR, SPL_VEC, -2 )
!@!
!@! --- Compute statstics of postfit residuals
!@!
!@      POSTFIT_WRMS = 0.0D0
!@      WW           = 0.0D0
!@      DO 480 J8=1,MF
!@         RES = FUN_VEC(J8) - EBSPL_VAL_R8 ( MS, DEG, ARG_VEC(J8), ARG_NOD, SPL_VEC )
!@         POSTFIT_WRMS = POSTFIT_WRMS + RES**2*WEI_VEC(J8)**2
!@         WW           = WW           +        WEI_VEC(J8)**2
!@ 480  CONTINUE 
!@      IF ( WW > 0.0D0 ) THEN
!@           POSTFIT_WRMS = DSQRT ( POSTFIT_WRMS/WW )
!@      END IF
!@!
!@      DEALLOCATE ( NOR_VEC )
!@      DEALLOCATE ( NOR_MAT )
!@      DEALLOCATE ( EQU_VEC )
!@!
!@      CALL ERR_LOG ( 0, IUER )
!@      RETURN
!@      END  SUBROUTINE  EBSPL_WLSQ_CNS  !#!#
