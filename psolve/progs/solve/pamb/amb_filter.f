      SUBROUTINE AMB_FILTER ( KP, TIM, ARF_VAL, ARF_SIG, SCAINF, IAMB, &
     &                        FUSE, NP_CHA, LSPLINE_S, SHF, WRMS, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  AMB_FILTER  filters function contaminated by ambiguity  *
! *   jumps and updates their ambiguities.                               *
! *                                                                      *
! *     It makes several runs. First it splits the data set onto         *
! *   segments by such a manner to be sure that there is no ambiguity    *
! *   jumps among the observations belonging to one segment. Then it     *
! *   finds the segment with maximal number of points.linear spline      *
! *   is calculated over the observations of the master segment. Then    *
! *   all other points are examined: do they suit master segment         *
! *   directly or they suit it after changing their ambiguities. If the  *
! *   point suits the linear spline it is added to the master segment    *
! *   and coefficients of the linear spline are updated. Finally, flag   *
! *   "not used in phase delay solution" is set for all observations     *
! *   which was used in filtering ( FUSE(k)=.TRUE. ) but didn't suit the *
! *   linear spline.                                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        KP ( INTEGER*4 ) -- Number of values of the function be       *
! *                            filtered.                                 *
! *       TIM ( REAL*8    ) -- Array of time arguments. Dimension: KP.   *
! *   ARF_VAL ( REAL*8    ) -- Array of values of the function to be     *
! *                            filtered. It is assumed that function is  *
! *                            normalized so that ambiguity spacings are *
! *                            1.0 Dimension: KP.                        *
! *   ARF_SIG ( REAL*8    ) -- Array of formal uncertainties of the      *
! *                            function to be filtered. (of course it is *
! *                            assumed that ARF_VAL and ARF_SIG were     *
! *                            normalized by the same manner).           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      IAMB ( INTEGER*4 ) -- Array of integer ambiguities. AMB_FILTER  *
! *                            updates ambiguity in this array. If does  *
! *                            not initialize IAMB. If it changes the    *
! *                            ambiguity of the k-th observation at IN   *
! *                            then IAMB(k) := IAMB(k) + IN              *
! *    NP_SCA ( INTEGER*4 ) -- Number of points which have been changed. *
! * LSPLINE_S ( RECORD    ) -- Data structure for temporary data used    *
! *                            by routine computing coefficients of      *
! *                            linear spline.                            *
! *       SHF ( REAL*8    ) -- Permanent shift of ARF remained after     *
! *                            ambiguity.                                *
! *      WRMS ( REAL*8    ) -- weighted root mean square of deviation    *
! *                            from linear spline.                       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      FUSE ( LOGICAL*4 ) -- Array of flags of usage of the point.     *
! *                            if the input value of FUSE(k) = .FALSE.   *
! *                            then the k-th point is not used in        *
! *                            computations. If AMB_FILTER finds that    *
! *                            the j-th point is not suit to linear      *
! *                            spline, it set FUSE(j) = .FALSE.          *
! *                            Dimension: KP.                            *
! *    SCAINF ( RECORD    ) -- Data structure which keeps a) values of   *
! *                            parameters which control work of          *
! *                            algorithm SCADAM; b) result of work of    *
! *                            algorithm SCADAM. some parameters of the *
! *                            filter are kept in SCAINF.                *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! *  ###  26-OCT-98   AMB_FILTER   v1.3  (c)  L. Petrov  15-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'pamb.i'
      INCLUDE   'lspline.i'
      INTEGER*4  KP, NP_CHA, IUER
      TYPE ( SCAINF__STRU ) ::  SCAINF
      TYPE ( LSPLINE__STRU ) ::  LSPLINE_S
      INTEGER*4  IAMB(KP), INDS(KP)
      REAL*8     TIM(KP), ARF_VAL(KP), ARF_SIG(KP), SHF, WRMS
      LOGICAL*4  FUSE(KP), F_UPD
      INTEGER*4  M_SEG, M_MULT, MSEG_MIN
      PARAMETER  ( M_SEG     = MO_SCA )
      PARAMETER  ( M_MULT    = 8      )
      PARAMETER  ( MSEG_MIN  = 8      )
      INTEGER*4  L_SEG, IND_SEG(MO_SCA), IAMB_OLD(M_SEG)
      REAL*8     SPL_VAL, SPL_SIG
      REAL*8     TIMN(MO_SCA), ARF_WEI(MO_SCA), ARR_TMP(MO_SCA), &
     &           ARR_TM2(MO_SCA), PSF_SHIFT, PSF_WRMS, TIM_MN, TIM_SS, &
     &           TSPAN, TSPAN_NEW, GLO_CNST, SHS, DRS, VAL_NEW, &
     &           AT_CUR, AT_MAX, TIMSEG(M_SEG), RATSEG(M_SEG), SIGSEG(M_SEG), &
     &           AV, WW, W2, DV
!      REAL*8     T2(2), X2(2), E2(2)
      REAL*8     AMB_TARGET
      PARAMETER  ( GLO_CNST = 0.0001/3600.D0 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, IAMB_PERM, IREP, &
     &           IC, IP, IMS, IPS, IFS, IES, IAMB_NEW, L_SPL, L_SPL_NEW, &
     &           IT_MAX, IER
!
      NP_CHA = 0
      IF ( KP .LE. 1 ) THEN
!
! -------- Nothing to do!
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Calculation of TIM_MN -- mean arguemnt and
! ---                TIM_SS -- semi span of arguements
!
      TIM_MN = ( TIM(KP) + TIM(1) )/2.D0
      TIM_SS = ( TIM(KP) - TIM(1) )/2.D0
!
! --- I. Make crude ambiguity resolution
! ---    We also calculate TIMN -- argument normalized to the range [-1.0, 1.0]
!
      DO 410 J1=1,KP
         TIMN(J1) = ( TIM(J1) - TIM_MN )/TIM_SS
         ARF_WEI(J1) = 1.D0/ARF_SIG(J1)
!
! ------ Crude resolution of the ambiguities
!
         IAMB_NEW = - NINT ( ARF_VAL(J1) )
         VAL_NEW  =   ARF_VAL(J1) + IAMB_NEW
         IF ( VAL_NEW .LT. -0.5D0 ) THEN
              VAL_NEW  = VAL_NEW  + 1.D0
              IAMB_NEW = IAMB_NEW + 1
         END IF
!
         IF ( VAL_NEW .GT. 0.5D0 ) THEN
              VAL_NEW  = VAL_NEW  - 1.D0
              IAMB_NEW = IAMB_NEW - 1
         END IF
!
! ------ Update ARF and ambiguities counter
!
         IF ( IAMB_NEW .NE. 0 ) THEN
              IAMB(J1)    = IAMB(J1)    + IAMB_NEW
              ARF_VAL(J1) = ARF_VAL(J1) + IAMB_NEW
         END IF
!
! ------ Put the current value of ambiguity counter to IAMB_OLD
!
         IAMB_OLD(J1) = IAMB(J1)
!
! ------ Copy the current values of ARF_VAL to the temporary array
!
         ARR_TMP(J1) = ARF_VAL(J1)
 410  CONTINUE
!
! --- II. Make the procedure of optimal scrolling for ARF. It finds the
! --- optimal permanent shift of all ARF values and resolves ambiguities
!
      CALL OSCRO_BAS ( KP, 1.D0, TIM, ARR_TMP, ARF_WEI, IAMB, ARR_TM2, &
     &                 PSF_SHIFT, PSF_WRMS )
!
! --- Apply the changes of ambiguity spacings to the ARF_VAL array
!
      DO 420 J2=1,KP
         ARF_VAL(J2) = ARF_VAL(J2) + ( IAMB(J2) - IAMB_OLD(J2) )
 420  CONTINUE
!
! --- III. Look at all ARF values for the station under consideration and split
! --- the data set onto segments
!
      IREP = KP - MSEG_MIN
      IF ( IREP .LT. 1 ) IREP = 1
      AT_MAX = 999.0
      IT_MAX = -1
!
! --- We start the search of the segments not from the first point but each
! --- time from the J3-th point. We do it in order to set up a competition
! --- between diuffernt wasy for searching the segmentation
!
      DO 430 J3=1,IREP
!
! ------ Find the segmantion and the index of the segment (IMS) with maximal
! ------ number of points within it (IPS)
!
         CALL ERR_PASS ( IUER, IER )
         CALL FIND_SEGMENT ( KP, FUSE, J3, TIM, TIMN, ARF_VAL, ARF_SIG, &
     &        SCAINF, TIM_SS, M_SEG, IND_SEG, IMS, IPS, IFS, IES, SHS, DRS, &
     &        WRMS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5401, IUER, 'AMB_FILTER', 'Error in '// &
     &            'FIND_SEGMENT' )
              RETURN
         END IF
!
! ------ Evaluation of the targeted function. The larger value of the targeted
! ------ function, the better properties of the segment
!
         AT_CUR = AMB_TARGET ( MSEG_MIN, IPS, WRMS )
         IF ( AT_CUR .LT. AT_MAX ) THEN
!
! ----------- Update the index and vlaue of the best segment
!
              AT_MAX = AT_CUR
              IT_MAX = J3
         END IF
 430  CONTINUE
!
! --- Find the best segment once more. We will use the shift which provided
! --- the maximal value of the targeted function at the previuous step
!
      CALL ERR_PASS ( IUER, IER )
      CALL FIND_SEGMENT ( KP, FUSE, IT_MAX, TIM, TIMN, ARF_VAL, ARF_SIG, &
     &          SCAINF, TIM_SS, M_SEG, IND_SEG, IMS, IPS, IFS, IES, SHS, DRS, &
     &          WRMS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5402, IUER, 'AMB_FILTER', 'Error in FIND_SEGMENT' )
           RETURN
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      t2(1) = tim(ifs)
!      t2(2) = tim(ies)
!      x2(1) = shs + drs*timn(ifs)
!      x2(2) = shs + drs*timn(ies)
!      e2(1) = wrms
!      e2(2) = wrms
!      j2 = 0
!      do 510 j1=1,kp
!         if ( ind_seg(j1) .eq. ims ) then
!              j2 = j2 + 1
!              arf_wei(j2) = tim(j1)
!              arr_tmp(j2) = arf_val(j1)
!              arr_tm2(j2) = arf_sig(j1)
!         end if
! 510  continue
!!      call diagi_2e ( j2, arf_wei, arr_tmp, arr_tm2, 2, t2, x2, e2, -3 ) ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- IV. Calculation of the time span and the number of spline segments
! --- over these observations
!
      TSPAN = TIM(IES) - TIM(IFS)
      L_SPL = IDINT ( TSPAN/SCAINF%SPL_SPAN + 0.0001 ) + 1
      IF ( IPS .LT. L_SPL+1 ) THEN
           WRITE ( 6, * ) ' l_spl = ',l_spl,' ips =',ips
           CALL ERR_LOG ( 5403, IUER, 'AMB_FILTER', 'Longest segment is not '// &
     &         'long enough to compute the linear spline' )
           RETURN
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         type *,' l_spl = ',l_spl,' ips = ',ips,      ! %%
!     #          ' tspan = ',tspan                     ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- Initialize internal data structure for further computation of linear
! --- spline
!
      CALL ERR_PASS ( IUER,  IER )
      CALL LSPLINE_INIT ( KP, TIM, IND_SEG, IMS, SCAINF%SPL_SPAN, &
     &                    GLO_CNST, SCAINF%SPL_CNST, LSPLINE_S, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5404, IUER, 'AMB_FILTER', 'Error during '// &
     &         'initialization of the data structures for calculation of '// &
     &         'linear spline' )
           RETURN
      END IF
!
! --- Computation of the linear spline over the points which belong to the
! --- largest segment
!
      CALL ERR_PASS ( IUER,  IER )
      CALL LSPLINE_CMP ( KP, TIM, ARF_VAL, ARF_SIG, IND_SEG, IMS, &
     &                   LSPLINE_S, IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' L_SEG = ',L_SEG
           WRITE ( 6, * ) ' IMS = ',IMS,' IPS = ',IPS
           CALL ERR_LOG ( 5405, IUER, 'AMB_FILTER', 'Error during '// &
     &         'computation of coefficients of linear spline' )
           RETURN
      END IF
!
! --- V. Look over all points in the main segemnt and deselect from there
! --- all points which deviate from the linear spline by more than SCAINF.DEFRG
! --- Linear spline will be updated after deselection of either point.
! --- We repeat this procedure up to M_MULT times in order to be sure that
! --- we avoided aftermaths
!
      DO 440 J4=1,M_MULT
         F_UPD = .FALSE.
         DO 450 J5=1,KP
            IF ( IND_SEG(J5) .EQ. IMS ) THEN
!
! -------------- Calculate the value of the linear spline at the argument
! -------------- TIM(J5)
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL LSPLINE_GET ( TIM(J5), LSPLINE_S, SPL_VAL, SPL_SIG, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 5406, IUER, 'AMB_FILTER', 'Error during '// &
     &                    'computation of the value of linear spline' )
                      RETURN
                 END IF
!
! -------------- Check: does the difference ARF deviates from the linear spline
! -------------- by more than the specified limit
!
                 IF ( DABS( ARF_VAL(J5) - SPL_VAL ) .GT. SCAINF%DEFRG ) THEN
!
! ------------------- Yes! Then we deselect the point...
!
                      IND_SEG(J5) = 0
                      IPS         = IPS-1
!
! ------------------- ... and update the linear spline for removing this point
!
                      CALL ERR_PASS    ( IUER, IER )
                      CALL LSPLINE_UPD ( -1, TIM(J5), ARF_VAL(J5), ARF_SIG(J5), &
     &                                   LSPLINE_S, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 5407, IUER, 'AMB_FILTER', 'Error '// &
     &                         'during update of coefficients of linear '// &
     &                         'spline' )
                           RETURN
                      END IF
                      F_UPD = .TRUE. ! set flag: linear spline has been updated
                 END IF
            END IF
 450     CONTINUE
         IF ( .NOT. F_UPD ) GOTO 840  ! Nothing to do more
 440  CONTINUE
 840  CONTINUE
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         if ( ips .ne. -888 ) then
!              type *,' after ips = ',ips  ! %%%%%%%%
!              call err_log ( 0, iuer )
!              return
!         end if
!                type *,' ips = ',ips  ! %%%%%%%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- VI. Look over all points out of main segemnt and eximane the
! --- difference between the ARF value and value of linear spline. We try
! --- to resolve ambiguity. If after ambiguity resolution the point appears
! --- within accaptable range (-+ SCAINF.DEFRG) from the linear spline we
! --- add the point with corrected ambiguity to the main segment and update
! --- linear spline for inclusion the new point.
! --- We repeat this procedure up to M_MULT times in order to avoid harmful
! --- aftermaths.
! --- We start from the central point and alternatively examine points
! --- at the right and the lsft by steadily widening the span. This tactic
! --- prevents dominancy of thepoint at one end of the spline
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!       type *,' m_mult = ',m_mult,' kp=',kp               ! %%
!       type *,' l_seg = ',l_seg,' ips=',ips,' ims=',ims   ! %%
!       type *,' icn_seg=',(icn_seg(j1),j1=1,l_seg)        ! %%
!! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      DO 460 J6=1,M_MULT
         F_UPD = .FALSE.
!
! ------ IC -- The index of the central point of the segment
!
         IC = (IFS + IES)/2
         DO 470 J7=1,2*KP
!
! --------- Set the index of the point under consideration
!
            IF ( MOD(J7,2) .EQ. 1 ) THEN
!
! -------------- If the running index is odd we try to examine the next point
! -------------- at the right part of the range
!
                 IP = IC + (J7-1)/2
                 IF ( IP .GT. KP ) GOTO 470
               ELSE
!
! -------------- If the running index is even we try to examine the next point
! -------------- at the left part of the range
!
                 IP = IC - J7/2
                 IF ( IP .LT. 1  ) GOTO 470
            END IF
!
            IF ( FUSE(IP) ) THEN
!
! -------------- Calculate the value of the linear spline at the argument
! -------------- TIM(IP)
!
                 CALL ERR_PASS    ( IUER, IER )
                 CALL LSPLINE_GET ( TIM(IP), LSPLINE_S, SPL_VAL, SPL_SIG, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 5408, IUER, 'AMB_FILTER', 'Error during '// &
     &                    'computation of the value of linear spline' )
                      RETURN
                 END IF
!
! -------------- Resolve phase delay ambiguity
!
                 IAMB_NEW    = - NINT ( ARF_VAL(IP) - SPL_VAL )
                 ARF_VAL(IP) =   ARF_VAL(IP) + IAMB_NEW
!
! -------------- Additional correction for compensating possible
! -------------- rounding errors
!
                 IF ( ARF_VAL(IP) - SPL_VAL  .LT. -0.5D0 ) THEN
                      ARF_VAL(IP) = ARF_VAL(IP) + 1.D0
                      IAMB_NEW    = IAMB_NEW     + 1
                 END IF
!
                 IF ( ARF_VAL(IP) - SPL_VAL  .GT.  0.5D0 ) THEN
                      ARF_VAL(IP) = ARF_VAL(IP) - 1.D0
                      IAMB_NEW    = IAMB_NEW    - 1
                 END IF
                 IAMB(IP) = IAMB(IP) + IAMB_NEW
                 IF ( IAMB_NEW .NE. 0 ) THEN
                      NP_CHA = NP_CHA + 1
                 END IF
!
! -------------- Check deviation of the point from linear spline
!
                 IF ( DABS(ARF_VAL(IP) - SPL_VAL) .LE. SCAINF%DEFRG  .AND. &
     &                IND_SEG(IP) .NE. IMS ) THEN
!
! ------------------- Ura! The point appeared to be restorable. Add it to the
! ------------------- main segment...
!
                      IND_SEG(IP) = IMS
                      IPS     = IPS + 1
                      IF ( IP .LT. IFS ) IFS = IP
                      IF ( IP .GT. IES ) IES = IP
!
! ------------------- ... and update the linear spline for inclusion the
! ------------------- new point
!
                      CALL ERR_PASS    ( IUER, IER )
                      CALL LSPLINE_UPD ( 1, TIM(IP), ARF_VAL(IP), ARF_SIG(IP), &
     &                                   LSPLINE_S, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 5409, IUER, 'AMB_FILTER', 'Error '// &
     &                         'during update of coefficients of linear '// &
     &                         'spline' )
                           RETURN
                      END IF
!
                      F_UPD  = .TRUE.
                   ELSE IF ( DABS(ARF_VAL(IP) - SPL_VAL) .GT. SCAINF%DEFRG .AND. &
     &                IND_SEG(IP) .EQ. IMS ) THEN
!
! ------------------- Alas! Popint appeared to be bad. We have to update
! ------------------- the linear spline for extraction the point from
! ------------------- the main segment
!
                      CALL ERR_PASS    ( IUER, IER )
                      CALL LSPLINE_UPD ( -1, TIM(IP), ARF_VAL(IP), ARF_SIG(IP), &
     &                                   LSPLINE_S, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 5409, IUER, 'AMB_FILTER', 'Error '// &
     &                         'during update of coefficients of linear '// &
     &                         'spline' )
                           RETURN
                      END IF
!
! ------------------- Then we substract it from the main segment...
!
                      IND_SEG(IP) = 0
                      IPS     = IPS - 1
                      F_UPD  = .TRUE.
                 END IF
            END IF
!
! --------- Check: maybe the time span of the points over the main segment
! --------- increased and we can increase the number of boundary points used
! --------- for calculation of linear spline
!
            TSPAN_NEW = TIM(IES) - TIM(IFS)
            L_SPL_NEW = IDINT ( TSPAN/SCAINF%SPL_SPAN + 0.0001 ) + 1
!
            IF ( ( TSPAN_NEW - TSPAN ) .GT. 0.5D0*SCAINF%SPL_SPAN  .AND. &
     &           IPS .GT. L_SPL_NEW+1 ) THEN
!
! -------------- Reinitialize internal data structure for further computation
! -------------- of linear spline
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL LSPLINE_INIT ( KP, TIM, IND_SEG, IMS, SCAINF%SPL_SPAN, &
     &                               GLO_CNST, SCAINF%SPL_CNST, LSPLINE_S, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 5410, IUER, 'AMB_FILTER', 'Error during '// &
     &                    'initialization of the data structures for '// &
     &                    'calculation of linear spline' )
                      RETURN
                 END IF
                 L_SPL = L_SPL_NEW
                 TSPAN = TIM(IES) - TIM(IFS)
!
! -------------- Computation of the linear spline over the points which
! -------------- belong to the largest segment
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL LSPLINE_CMP ( KP, TIM, ARF_VAL, ARF_SIG, IND_SEG, IMS, &
     &                              LSPLINE_S, IER )
                 IF ( IER .NE. 0 ) THEN
                      WRITE ( 6, * ) ' L_SEG = ',L_SEG
                      WRITE ( 6, * ) ' IMS = ',IMS,' IPS = ',IPS
                      CALL ERR_LOG ( 5411, IUER, 'AMB_FILTER', 'Error during '// &
     &                    'computation of coefficients of linear spline' )
                      RETURN
                 END IF
!
! -------------- Computation of the rate of linear spline over the points which
! -------------- belong to the largest segment (we do it for the test on rate
! -------------- which is assumed to be implemented in the future --
! -------------- after 18-NOV-98)
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL LSPLINE_GETSEGRAT ( KP, LSPLINE_S, L_SEG, TIMSEG, RATSEG, &
     &                                    SIGSEG, IER )
                 IF ( IER .NE. 0 ) THEN
                      WRITE ( 6, * ) ' L_SEG = ',L_SEG
                      WRITE ( 6, * ) ' IMS = ',IMS,' IPS = ',IPS
                      CALL ERR_LOG ( 5412, IUER, 'AMB_FILTER', 'Error during '// &
     &                    'computation of coefficients of linear spline' )
                      RETURN
                 END IF
            END IF
 470     CONTINUE
!
! ------ Reinitialize internal data structure for further computation
! ------ of linear spline the last time
!
         TSPAN = TIM(IES) - TIM(IFS)
         L_SPL = IDINT ( TSPAN/SCAINF%SPL_SPAN + 0.0001 ) + 1
         CALL ERR_PASS ( IUER, IER )
         CALL LSPLINE_INIT ( KP, TIM, IND_SEG, IMS, SCAINF%SPL_SPAN, GLO_CNST, &
     &                       SCAINF%SPL_CNST, LSPLINE_S, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5413, IUER, 'AMB_FILTER', 'Error during '// &
     &            'initialization of the data structures for calculation of '// &
     &            'linear spline' )
              RETURN
         END IF
!
! ------ Computation of the linear spline over the points which belong to the
! ------ largest segment
!
         CALL ERR_PASS ( IUER, IER )
         CALL LSPLINE_CMP ( KP, TIM, ARF_VAL, ARF_SIG, IND_SEG, IMS, LSPLINE_S, &
     &                      IER )
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) ' L_SEG = ',L_SEG
              WRITE ( 6, * ) ' IMS = ',IMS,' IPS = ',IPS
              CALL ERR_LOG ( 5414, IUER, 'AMB_FILTER', 'Error during '// &
     &            'computation of coefficients of linear spline' )
              RETURN
         END IF
!
         IF ( .NOT. F_UPD ) GOTO 860  ! Nothing to do more
 460  CONTINUE
 860  CONTINUE
!
! --- VII. Scan all observations and set flag "not used" for all observations
! --- which remained out of main segment. We also calculate some useful
! --- statisitcs at this step
!
      AV = 0.0D0
      WW = 0.0D0
      W2 = 0.0D0
      DV = 0.0D0
!
      DO 480 J8=1,KP
!
! ------ Calculate the value of the linear spline at the argument
! ------ TIM(J8)
!
         CALL ERR_PASS    ( IUER, IER )
         CALL LSPLINE_GET ( TIM(J8), LSPLINE_S, SPL_VAL, SPL_SIG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5415, IUER, 'AMB_FILTER', 'Error during '// &
     &            'computation of the value of linear spline' )
              RETURN
         END IF
!
! ------ Resolve phase delay ambiguity
!
         IAMB_NEW     = - NINT ( ARF_VAL(J8) - SPL_VAL )
         ARF_VAL(J8) =   ARF_VAL(J8) + IAMB_NEW
!
! ------ Additional correction for compensating possible rounding errors
!
         IF ( ARF_VAL(J8) - SPL_VAL  .LT. -0.5D0 ) THEN
              ARF_VAL(J8) = ARF_VAL(J8) + 1.D0
              IAMB_NEW     = IAMB_NEW    + 1
         END IF
!
         IF ( ARF_VAL(J8) - SPL_VAL  .GT.  0.5D0 ) THEN
              ARF_VAL(J8) = ARF_VAL(J8) - 1.D0
              IAMB_NEW    = IAMB_NEW    - 1
         END IF
         IAMB(J8) = IAMB(J8) + IAMB_NEW
         IF ( IAMB_NEW .NE. 0 ) THEN
              NP_CHA = NP_CHA + 1
         END IF
!
! ------ Update FUSE flag
!
         IF ( FUSE(J8) ) THEN
              IF ( IND_SEG(J8) .NE. IMS   .OR. &
     &             DABS(ARF_SIG(J8)) .GT. SCAINF%ARFMS ) THEN
!
                   FUSE(J8) = .FALSE.
                   IND_SEG(J8) = 0
                   NP_CHA = NP_CHA + 1
              END IF
            ELSE
              CONTINUE
         END IF
!
! ------ Update of weighted average over used observations
!
         IF ( FUSE(J8) ) THEN
              AV = AV + ARF_VAL(J8)/ARF_SIG(J8)
              DV = DV + ( ( ARF_VAL(J8) - SPL_VAL )/ARF_SIG(J8) )**2
              WW = WW + 1.D0/ARF_SIG(J8)
              W2 = W2 + 1.D0/ARF_SIG(J8)**2
         END IF
 480  CONTINUE
!
! --- VIII. Find permanent station dependent ambiguity and permanment shift
!
      AV = AV/WW
      WRMS = DSQRT ( DV/W2 )
      IAMB_PERM = - NINT ( AV )
      SHF       =   AV + IAMB_PERM
!
! --- Correction ambiguity counter and ARF values for permanent ambiguity and
! --- permanent shift
!
      DO 490 J9=1,KP
         ARF_VAL(J9) = ARF_VAL(J9) + IAMB_PERM - SHF
         IAMB(J9)    = IAMB(J9) + IAMB_PERM
 490  CONTINUE
!
! --- And final calculation of linear spline. We should update it since we
! --- changed ARF
!
      CALL ERR_PASS ( IUER, IER )
      CALL LSPLINE_INIT ( KP, TIM, IND_SEG, IMS, SCAINF%SPL_SPAN, GLO_CNST, &
     &                    SCAINF%SPL_CNST, LSPLINE_S, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5416, IUER, 'AMB_FILTER', 'Error during '// &
     &         'initialization of the data structures for calculation of '// &
     &         'linear spline' )
           RETURN
      END IF
!
! --- Computation of the linear spline over the points which belong to the
! --- largest segment
!
      CALL ERR_PASS ( IUER, IER )
      CALL LSPLINE_CMP ( KP, TIM, ARF_VAL, ARF_SIG, IND_SEG, IMS, LSPLINE_S, &
     &                   IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5417, IUER, 'AMB_FILTER', 'Error during '// &
     &         'computation of coefficients of linear spline' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  AMB_FILTER  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FIND_SEGMENT ( KP, FUSE, IBEG, TIM, TIMN, ARF_VAL, ARF_SIG, &
     &           SCAINF, TIM_SS, M_SEG, IND_SEG, IMS, IPS, IFS, IES, &
     &           SHS, DRS, WRMS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FIND_SEGMENT  divide the points of the function specified *
! *   onto segments using several criterions:                            *
! *   1) points in one segment should has arguments within range         *
! *      SCAINF.TIM1;                                                    *
! *   2) points in one segment should differ from each other by no more  *
! *      than 1.0;                                                       *
! *   3) points in one segment should differ from linear regression over *
! *      these points by no more than SCAINF.DEFRG                       *
! *                                                                      *
! *   Procedure puts all points to the segments and fille the array      *
! *   IND_SEG which contains the indes of the segment for each point.    *
! *   Then the segment which has the most of points is found. Parameters *
! *   of linear regression: shift and drift are calculated as well as    *
! *   wrms of the deviation of the points from the linear regression.    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      KP ( INTEGER*4 ) -- The number of points to be considered.      *
! *    FUSE ( LOGICAL*4 ) -- Logical array of useness of the points.     *
! *                          If FUSE(k) = .FALSE. then the k-th point is *
! *                          ignored.                                    *
! *    IBEG ( INTEGER*4 ) -- The first point in the arrays starting      *
! *                          which we consider the points. Points with   *
! *                          indices less than IBEG are ignored.         *
! *     TIM ( REAL*8    ) -- Array of the arguments of the function.     *
! *                          Dimension: KP.                              *
! *    TIMN ( REAL*8    ) -- Array of the arguments of the function      *
! *                          normalized to the range [-1, 1].            *
! *                          Dimension: KP.                              *
! * ARF_VAL ( REAL*8    ) -- Array of the values of the function.        *
! *                          Dimension: KP.                              *
! * ARF_SIG ( REAL*8    ) -- Array of the formal uncertainties of the    *
! *                          function. Dimension: KP.                    *
! *  SCAINF ( RECORD    ) -- Data structure which keeps a) values of     *
! *                          parameters which control work of algorithm  *
! *                          SCADAM; b) result of work of algorithm      *
! *                          SCADAM. Some parameters of the filter are   *
! *                          kept in SCAINF.                             *
! *  TIM_SS ( REAL*8    ) -- Semi span of the argiment, which has been   *
! *                          actually used for calculation of the TIMN.  *
! *   M_SEG ( INTEGER*4 ) -- Maximal number of allowed segments.         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * IND_SEG ( INTEGER*4 ) -- Array which contains the index of the       *
! *                          segment for each point. If IND_SEG(k) = l   *
! *                          then it means that the k-th point belongs   *
! *     IMS ( INTEGER*4 ) -- Index the segment which provides the        *
! *                          maximum number of points.                   *
! *     IPS ( INTEGER*4 ) -- Number of point in the segment with index   *
! *                          IMS.                                        *
! *     IFS ( INTEGER*4 ) -- Index of the first point in the segment     *
! *                          with index IMS.                             *
! *     IES ( INTEGER*4 ) -- Index of the last point in the segment      *
! *                          with index IMS.                             *
! *     SHS ( REAL*8    ) -- Parameter of the linear regerssion over the *
! *                          points from the segment with index IMS:     *
! *                          shift at the zero epoch TIMN.               *
! *     DRS ( REAL*8    ) -- Parameter of the linear regerssion over the *
! *                          points from the segment with index IMS:     *
! *                          drift.                                      *
! *    WRMS ( REAL*8    ) -- Wrms of the deviation of the points from    *
! *                          the linear regression over the points from  *
! *                          the segment with index IMS.                 *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! * IUER ( INTEGER*4, OPT) -- Universal error handler.                   *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  11-NOV-98  FIND_SEGMENT  v1.0  (c)  L. Petrov  12-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'pamb.i'
      TYPE ( SCAINF__STRU ) ::  SCAINF
      INTEGER*4  KP, IBEG, M_SEG, IND_SEG(M_SEG), IMS, IPS, IFS, IES, IUER
      LOGICAL*4  FUSE(KP)
      REAL*8     TIM(KP), TIMN(KP), ARF_VAL(KP), ARF_SIG(KP), TIM_SS, WRMS
      INTEGER*4  ICN_SEG(M_SEG), IFC_SEG(M_SEG), IEC_SEG(M_SEG)
      INTEGER*4  J1, J2, J3, J4, J5, J6, L_SEG, IER
      REAL*8     COV(3,M_SEG), SH(M_SEG), DR(M_SEG), SIG_SH, SIG_DR, VAL_PRD, &
     &           DV, WW, VAL_FIRST, RATE_PRD, SHS, DRS
!
! --- Initialization
!
      CALL NOUT_I4 ( MO_SCA, IND_SEG )
      CALL NOUT_I4 ( M_SEG,  ICN_SEG )
      CALL NOUT_I4 ( M_SEG,  IFC_SEG )
      CALL NOUT_I4 ( M_SEG,  IEC_SEG )
      L_SEG = 0
      TIM_SS = ( TIM(KP) - TIM(1) )/2.D0
!
! --- NB: We stert from the IBERG-th point, not from the first one
!
      DO 410 J1=IBEG,KP
!
! ------ We will work here with normalized argument in order to avoid
! ------ numerical problems in calculation of linear regression coefficients
!
         IF ( FUSE(J1) ) THEN
              IF ( L_SEG .GT. 0 ) THEN
!
! ---------------- We have already some segment(s). Look first over the
! ---------------- segments to decide to which segment the J1-th point can
! ---------------- belong
!
                   DO 420 J2=1,L_SEG
!
! ------------------- Calculate predicted value on the basis of
! ------------------- linear regression
!
                      VAL_PRD = SH(J2) + DR(J2)*TIMN(J1)
!
! ------------------- Check: does the observation deviate from the regression
! ------------------- fit by more than the limit?
!
                      IF ( DABS(ARF_VAL(J1) - VAL_PRD ) .GT. SCAINF%DEFRG ) THEN
                           GOTO 420 ! alas yes -- it is not appropriate segment
                      END IF
!
! ------------------- Check: is the segment is too long?
!
                      IF ( TIM(J1) - TIM(IFC_SEG(J2)) .GT. SCAINF%FRZTR ) THEN
                           GOTO 420 ! yes ...
                      END IF
!
! ------------------- Check: is the rate of change too high? We check the rate
! ------------------- between the examined point and the value of regression
! ------------------- line at the epoch of the first observation of the segment
! ------------------- the rate should not exceed the specified limit
!
                      VAL_FIRST = SH(J2) + DR(J2)*TIMN(IFC_SEG(J2))
!
                      RATE_PRD  = ( ARF_VAL(J1) - VAL_FIRST         )/ &
     &                            ( TIMN(J1)    - TIMN(IFC_SEG(J2)) )/TIM_SS
                      IF ( DABS(RATE_PRD) .GT. 1.0D0/SCAINF%FRZTR ) THEN
                           GOTO 420
                      END IF
!
! ------------------- Scan all points of the J2-th segment and look at the
! ------------------- difference between the examined point and the points
! ------------------- of the the points of the segment
!
                      DO 430 J3=IBEG,KP
                         IF ( IND_SEG(J3) .EQ. J2 ) THEN
!
! --------------------------- If the difference is too large -- don't add
! --------------------------- the point to the segment
!
                              IF ( DABS(ARF_VAL(J1) - ARF_VAL(J3)) .GT. &
     &                             SCAINF%DEFRG                       ) GOTO 420
                         END IF
 430                  CONTINUE
!
! ------------------- Well all checks are over. We decided to add this point
! ------------------- to the J2-th segment
!
                      ICN_SEG(J2) = ICN_SEG(J2) + 1
                      IEC_SEG(J2) = J1
                      IND_SEG(J1) = J2
!
! ------------------- Update parameters of the linear regression for inclusion
! ------------------- of the J1-th point ot the J2-th segment
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL REGR_ITE ( 1, KP, IND_SEG, J2, J1, TIMN, ARF_VAL, &
     &                     ARF_SIG, COV(1,J2), SH(J2), DR(J2), SIG_SH, SIG_DR, &
     &                     IER )
                      IF ( IER .NE. 0 ) THEN
!
! ------------------------ Debugging printout
!
                           WRITE ( 6, * ) ' J2=',J2,' ICN_SEG(J2) = ',ICN_SEG(J2)
                           DO 440 J4=1,KP
                              IF ( IND_SEG(J4) .EQ. J2 ) THEN
                                   WRITE ( 6, * ) ' J4=',J4,' TIMN(J4) =',TIMN(J4), &
     &                                    ' ARF_VAL(J4) =',ARF_VAL(J4), &
     &                                    ' ARF_SIG(J4) =',ARF_SIG(J4)
                              END IF
 440                       CONTINUE
!
                           CALL ERR_LOG ( 5461, IUER, 'AMB_FILTER', 'Error '// &
     &                         'during update of regression coefficients' )
                           RETURN
                      END IF
                      GOTO 410
 420               CONTINUE
              END IF ! l_seg=0
!
! ----------- We checked all segments and found that the point under
! ----------- consideration doesn't suit any segemnt. Well. We have to create
! ----------- a new segment...
!
              L_SEG = L_SEG + 1
              IND_SEG(J1)    = L_SEG
              ICN_SEG(L_SEG) = ICN_SEG(L_SEG) + 1
              IFC_SEG(L_SEG) = J1
              IEC_SEG(L_SEG) = J1
!
! ----------- ... And "calculate" the average over 1 observation (in order to
! ----------- put this information to SH and DR)
!
              CALL ERR_PASS ( IUER, IER )
              CALL REGR_ITE ( 1, KP, IND_SEG, L_SEG, J1, TIMN, ARF_VAL, &
     &             ARF_SIG, COV(1,L_SEG), SH(L_SEG), DR(L_SEG), SIG_SH, &
     &             SIG_DR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5402, IUER, 'AMB_FILTER', 'Error during '// &
     &                           'computation of regression coefficients' )
                   RETURN
              END IF
         END IF
 410  CONTINUE
!
! --- II. Find the segment which contains the  max number of points: IPS.
! --- Index of such a segment in the list of segments is IMS
!
      IPS = -1
      IMS =  0
      DO 450 J5=1,L_SEG
         IF ( ICN_SEG(J5) .GT. IPS ) THEN
              IMS = J5          ! IMS -- index the longest segment
              IPS = ICN_SEG(J5)
              IFS = IFC_SEG(J5)
              IES = IEC_SEG(J5)
!
              SHS = SH(J5)
              DRS = DR(J5)
         END IF
 450  CONTINUE
!
! --- III. Then we calculate statistics over the points from the main segment:
! --- wrms of the deviation from the linear regression
!
      DV = 0.0D0
      WW = 0.0D0
      DO 460 J6=1,KP
         IF ( IND_SEG(J6) .EQ. IMS ) THEN
              VAL_PRD = SH(IMS) + DR(IMS)*TIMN(J6)
              DV = DV + ( ( ARF_VAL(J6) - VAL_PRD )/ARF_SIG(J6) )**2
              WW = WW + ( 1.D0/ARF_SIG(J6) )**2
         END IF
 460  CONTINUE
      IF ( IPS .GT. 0  ) THEN
           WRMS = DSQRT ( DV/WW )
        ELSE
           WRMS = 1.D10
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  FIND_SEGMENT  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   AMB_TARGET ( MSEG_MIN, IPS, WRMS )
! ************************************************************************
! *                                                                      *
! *   Function  AMB_TARGET  evaluate the score whcih is ascrbed to the   *
! *   segment. It corresponds to the suitability of the segment for      *
! *   its selection as a pivot segement. The score depends on the number *
! *   points in the segment and weighted root mean square of the         *
! *   deviation of the points from the linear regression. The less score *
! *   the less sutable the segment for its selection.                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * MSEG_MIN ( INTEGER*4 ) -- Minimum number of points in the segment.   *
! *                           if segment has less then MSEG_MIN points   *
! *                           it has few chances for selection.          *
! *      IPS ( INTEGER*4 ) -- The number of points in the segment.       *
! *     WRMS ( REAL*8    ) -- Wrms of the deviation of the points from   *
! *                           the linear regression over the points from *
! *                           the segment with index IMS.                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * <AMB_TARGET> ( REAL*8 ) -- Value of the targeted function.           *
! *                                                                      *
! *  ###  12-NOV-98   AMB_TARGET   v1.0  (c)  L. Petrov  12-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  KP, MSEG_MIN, IPS
      REAL*8     AMB_TARGET, WRMS
!
      IF ( IPS .LT. MSEG_MIN ) THEN
           AMB_TARGET = 1.0 + WRMS
         ELSE
           AMB_TARGET = WRMS + (1.0/IPS)**2
      END IF
!
      RETURN
      END  !#!  AMB_TARGET  #!#
