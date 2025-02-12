      SUBROUTINE HFINT_INIT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Routine HFINT_INIT calculates coefficients of cubic spline for     *
! *   interpolation high frequency EOP. It is assumed that these         *
! *   coefficients will be used by FLYBY_MAP. Coeffiecients and other    *
! *   interpolation related stuff will be stored in SOCOM_PLUS.          *
! *                                                                      *
! *   Cunning tricks. The dates of the first and last observations of    *
! *   the sessions are calculated by OBSTM. But the first and last nodes *
! *   are put a bit before and a bit after the last observations.        *
! *   The amount of offset is setup by parameter ENDS_SHARE.             *
! *                                                                      *
! *   |----*------------------------------*----|                         *
! *                                                                      *
! *   For better calculation rirst derivatives on the ends two extra     *
! *   points are added located on the distant STEP*ENDS_COEF from the    *
! *   the forst and last knodes. (STEP -- usual interval between nodes,  *
! *   ENDS_COEF -- parameter).                                           *
! *                                                                      *
! *   Default number of knodes is setup by parameter L_HFE.              *
! *                                                                      *
! *   Environment variables.                                             *
! *                                                                      *
! *       HFINT         -- declare the number of nodes (default L_HFE,   *
! *                        max -- MAX4_HFE from solve.i).                *
! *       HFINT_DISABLE -- if value is 'Yes' (or "Y", "y" etc) then      *
! *                        coefficients will not be calculated and       *
! *                        FLYBY_MAP will calculate hf-EOP directly for  *
! *                        each observation.                             *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4 ) -- Universal error habdler.                   *
! *            Input: mode swicth:                                       *
! *                   IUER=0 -- no error messages  will be generated     *
! *                             even in the case of error.               *
! *                   IUER=-1 - in the case of error the message will    *
! *                             be put in stdout.                        *
! *            Output: 0 in the case of successful completion and        *
! *                    non-zero in the case of error.                    *
! *                                                                      *
! *  ###  29-APR-1997    HFINT_INIT  v2.1 (c) L. Petrov 11-MAR-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'heo.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'oborg.i'
      CHARACTER  STR*20, STR1*20
      INTEGER*4  IUER, L_HFE, LMIN_HFE, J1, IER
      PARAMETER  ( L_HFE = 48 )   ! Default number of nodes.
      PARAMETER  ( LMIN_HFE = 6 ) ! Min number of knodes
      REAL*8     ENDS_SHARE, ENDS_COEF, FIRST_JDATE, LAST_JDATE, T0, STEP, &
    &            T1, TN, D1, DN, DUMMY, T_SEC, UT1_M_TDB 
      REAL*8     UT1_HFE, XP1_HFE, YP1_HFE, UTN_HFE, XPN_HFE, YPN_HFE, &
     &           VEC_HEO(3), TIM_HFE(L_HFE)
      PARAMETER  ( ENDS_SHARE = 0.10D0  )
      PARAMETER  ( ENDS_COEF  = 0.001D0 )
!
      INTEGER*4, EXTERNAL :: I_LEN
!
      STATUS_HFE = HFE__UNDF  ! Status -- undefined
!
      IF ( FL_NOFLYBY ) THEN ! Nothing to do!
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Test: but do we really need interpolation?
!
      IF ( KHFEOP.EQ.1  .OR.  KHFEOP.EQ.3  .OR.  STAT_HEO .EQ. HEO__READ ) THEN
!
! -------- Test environment variable HFINT_DISABLE
!
           CALL CLRCH  ( STR )
           CALL GETENVAR ( 'HFINT_DISABLE', STR )
           IF ( STR(1:1) .EQ. 'Y'  .OR.  STR(1:1) .EQ. 'y' ) THEN
!
! ------------- Good bye, my love, good bye...
!
                STATUS_HFE = HFE__NONE
                NUM_HFE = 0
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
!
! -------- Specify NUM_HFE -- the number of nodes
!
           NUM_HFE = L_HFE
           CALL CLRCH  ( STR )
           CALL GETENVAR ( 'HFINT', STR )
           IF ( STR(1:1) .NE. ' ' ) THEN
                CALL CHIN ( STR, NUM_HFE )
                IF ( NUM_HFE .LT. 0  .OR. NUM_HFE .GT. 999 ) THEN
                     CALL ERR_LOG ( 8701, IUER, 'HWINT_INIT', &
     &                   'Environment variable HFINT has wrong value: '//STR )
                     RETURN
                END IF
           END IF
!
! -------- Test of validity NUM_HFE
!
           IF ( NUM_HFE .LT. LMIN_HFE ) NUM_HFE = LMIN_HFE
           IF ( NUM_HFE .GT. MAX4_HFE ) THEN
                CALL INCH ( NUM_HFE,  STR  )
                CALL INCH ( MAX4_HFE, STR1 )
                CALL ERR_LOG ( 8702, IUER, 'HWINT_INIT', &
     &              'The number of nodes for interpolation high '// &
     &              'frequency EOP ('//STR(1:I_LEN(STR))// &
     &              ' exceeded the limit: MAX4_HFE (from solve.i): '// &
     &               STR1(1:I_LEN(STR1)) )
                RETURN
           END IF
!
! -------- Learn first and last time of the observations in session
!
           CALL OBSTM ( FIRST_JDATE, LAST_JDATE )
!
! -------- Calculate STEP of time for arguments, and
! -------- T0 -- initial epoch for interpolation
!
           STEP = (LAST_JDATE - FIRST_JDATE) *(1.D0 + 2.D0*ENDS_SHARE)/ &
     &            (NUM_HFE-1)
           T0 = FIRST_JDATE - (LAST_JDATE - FIRST_JDATE) *ENDS_SHARE
!
! -------- Calculate hf-EOP near the first node for more accurate calculation
! -------- first derivative on the beginning of the interval
!
           T1 = T0 + STEP*ENDS_COEF
           IF ( KHFEOP .EQ. 1  .OR. KHFEOP .EQ. 3 ) THEN
!
! ------------- Read HEO variations from the file in HFE format
!
                CALL GET_HF_EOP ( T1, UT1_HFE, XP1_HFE, YP1_HFE, DUMMY, DUMMY, &
     &                            DUMMY, INT2(1) )
                UT1_HFE = UT1_HFE/1000.D0
                XP1_HFE = XP1_HFE*MAS__TO__RAD
                YP1_HFE = YP1_HFE*MAS__TO__RAD
              ELSE IF ( STAT_HEO .EQ. HEO__READ ) THEN
                T_SEC = (T1 - J2000__JD)*86400.0D0
                UT1_M_TDB = UT1_M_TAI - 32.184D0
!
! ------------- Read HEO variations from the file in HEO format
!
                IF ( STAT_HEO .NE. HEO__READ ) THEN
                     CALL ERR_LOG ( 8703, IUER, 'HFINT_INIT', 'Trap of '// &
     &                   'internal control: harnonic Earth orientation '// &
     &                   'file has not been read' )
                     RETURN 
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL GET_HEO ( T_SEC, HEO_EPOCH_SEC, UT1_M_TDB, L_HEO, &
     &                         %VAL(ADR_HEO), VEC_HEO, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8704, IUER, 'HFINT_INIT', 'Error in an '// &
     &                   'attempt to compute harmonic variations in the '// &
     &                   'Earth Orinetation' )
                     RETURN 
                END IF
!
                XP1_HFE =  VEC_HEO(2)
                YP1_HFE =  VEC_HEO(1)
!
! ------------- Transform variation in UT1 from rad to sec (Stupid! Stupid!!)
!
                UT1_HFE = -VEC_HEO(3)/(1000.0D0*MSEC__TO__RAD)/1.002737D0
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!                    write ( 6, * ) ' XP1_HFE   = ', XP1_HFE   ! %%%
!                    write ( 6, * ) ' YP1_HFE   = ', YP1_HFE   ! %%%
!                    write ( 6, * ) ' UT1_HFE   = ', UT1_HFE   ! %%%
!                    call pause ( 'hfint_init' )               ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           END IF
!
           DO 410 J1=1,NUM_HFE
!
! ----------- Calcullate hf-EOP for each knode
!
              EPOCH_HFE(J1) = T0 + STEP*(J1-1)
              TIM_HFE(J1) = STEP*(J1-1)
              IF ( KHFEOP .EQ. 1  .OR.  KHFEOP .EQ. 3 ) THEN
!
! ---------------- Read HEO variations from the file in HFE format
!
                   CALL GET_HF_EOP ( EPOCH_HFE(J1), UT_HFE(J1), XP_HFE(J1), &
     &                               YP_HFE(J1), DUMMY, DUMMY, DUMMY, INT2(1) )
                   UT_HFE(J1) = UT_HFE(J1)/1000.D0
                   XP_HFE(J1) = XP_HFE(J1)/206264806.2D0
                   YP_HFE(J1) = YP_HFE(J1)/206264806.2D0
                ELSE IF ( STAT_HEO .EQ. HEO__READ ) THEN
                   T_SEC = (EPOCH_HFE(J1) - J2000__JD)*86400.0D0
                   UT1_M_TDB = UT1_M_TAI - 32.184D0
!
! ---------------- Read HEO variations from the file in HEO format
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL GET_HEO ( T_SEC, HEO_EPOCH_SEC, UT1_M_TDB, L_HEO, &
     &                            %VAL(ADR_HEO), VEC_HEO, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8705, IUER, 'HFINT_INIT', 'Error in '// &
     &                      'an attempt to compute harmonic variations '// &
     &                      'in the Earth Orinetation' )
                        RETURN 
                   END IF
!
                   XP_HFE(J1) =  VEC_HEO(2)
                   YP_HFE(J1) =  VEC_HEO(1)
!
! ---------------- Transform variation in UT1 from rad to sec (Stupid! Stupid!!)
!
                   UT_HFE(J1) = -VEC_HEO(3)/(1000.0D0*MSEC__TO__RAD)/1.002737D0
              END IF
 410       CONTINUE
!
! -------- Calculate hf-EOP near the last node for more accurate calculation
! -------- first derivative on the end of the interval
!
           TN = EPOCH_HFE(NUM_HFE) - STEP*ENDS_COEF
           IF ( KHFEOP .EQ. 1  .OR. KHFEOP .EQ. 3 ) THEN
                CALL GET_HF_EOP ( TN, UTN_HFE, XPN_HFE, YPN_HFE, DUMMY, &
     &                            DUMMY, DUMMY, INT2(1) )
                UTN_HFE = UTN_HFE/1000.D0
                XPN_HFE = XPN_HFE/206264806.2D0
                YPN_HFE = YPN_HFE/206264806.2D0
              ELSE IF ( STAT_HEO .EQ. HEO__READ ) THEN
                T_SEC = (TN - J2000__JD)*86400.0D0
                UT1_M_TDB = UT1_M_TAI - 32.184D0
!
! ------------- Read HEO variations from the file in HEO format
!
                CALL ERR_PASS ( IUER, IER )
                CALL GET_HEO  ( T_SEC, HEO_EPOCH_SEC, UT1_M_TDB, L_HEO, &
     &                          %VAL(ADR_HEO), VEC_HEO, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8706, IUER, 'HFINT_INIT', 'Error in an '// &
     &                   'attempt to compute harmonic variations in the '// &
     &                   'Earth Orinetation' )
                     CALL EXIT ( 1 )
                END IF
!
                XPN_HFE =  VEC_HEO(2)
                YPN_HFE =  VEC_HEO(1)
!
! ------------- Transform variation in UT1 from rad to sec (Stupid! Stupid!!)
!
                UTN_HFE = -VEC_HEO(3)/(1000.0D0*MSEC__TO__RAD)/1.002737D0
           END IF
!
! -------- Calculation coefficients of cubic spline for UT1
!
           D1 = (UT1_HFE - UT_HFE(1))       /(T1 - EPOCH_HFE(1)) ! derivaties
           DN = (UT_HFE(NUM_HFE) - UTN_HFE) /(EPOCH_HFE(NUM_HFE) - TN)
!
           CALL ERR_PASS ( IUER, IER )
           CALL MAKE_SPLINE ( 2, NUM_HFE, EPOCH_HFE, UT_HFE, D1, DN, UT_SPL, &
     &          WORK_HFE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8707, IUER, 'HWINT_INIT', &
     &              'Error during calculation coefficients of cubic spline' )
                RETURN
           END IF
!
! -------- Calculation coefficients of cubic spline for X pole
!
           D1 = (XP1_HFE - XP_HFE(1))       /(T1 - EPOCH_HFE(1)) ! derivaties
           DN = (XP_HFE(NUM_HFE) - XPN_HFE) /(EPOCH_HFE(NUM_HFE) - TN)
           IER = -1
           CALL MAKE_SPLINE ( 2, NUM_HFE, EPOCH_HFE, XP_HFE, D1, DN,XP_SPL, &
     &          WORK_HFE, IER )
!
! -------- Calculation coefficients of cubic spline for Y pole
!
           D1 = (YP1_HFE - YP_HFE(1))       /(T1 - EPOCH_HFE(1)) ! derivaties
           DN = (YP_HFE(NUM_HFE) - YPN_HFE) /(EPOCH_HFE(NUM_HFE) - TN)
           IER = -1
           CALL MAKE_SPLINE ( 2, NUM_HFE, EPOCH_HFE, YP_HFE, D1, DN,YP_SPL, &
     &          WORK_HFE, IER )
!
           IXMN_HFE = 1 ! Initail values of node counter
           STATUS_HFE = HFE__DONE ! Deal done
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!             call diagi_setdef ( -1, 'DIAGI_CTIT', 'XPL' )     ! %%%
!             call diagi_1      ( num_hfe, tim_hfe, xp_hfe  )   ! %%%
!             call diagi_setdef ( -1, 'DIAGI_CTIT', 'YPL' )     ! %%%
!             call diagi_1      ( num_hfe, tim_hfe, yp_hfe  )   ! %%%
!             call diagi_setdef ( -1, 'DIAGI_CTIT', 'UT1' )     ! %%%
!             call diagi_1      ( num_hfe, tim_hfe, ut_hfe )    ! %%%
!         write ( 6, 210 ) ( j1, epoch_hfe(j1), xp_hfe(j1), yp_hfe(j1), ut_hfe(j1), &
!     &                    j1=1,num_hfe ) ! %%%%%%%%%%%%%%%%%%%%%%%%%%
! 210     format ( i2, 1x, 0pf15.5, 1x, 1pe15.7, 1x, 1pe15.7, 1x, 1pe15.7 )
!         call pause ('aaa' ) 
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  HFINT_INIT  #!#
