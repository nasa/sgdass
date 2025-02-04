      SUBROUTINE PLOT_SPL ( SPL, FL_SPLINE_ONLY, FL_UEN, ICMP, FILTMPL, &
     &                      FILPLT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PLOT_SPL makes a plot of evolition of site position       *
! *   modeled with a spline. The plot name is built from the FILOUT      *
! *   name, station name, componebt name and have the suffix .sav .      *
! *   This plot-file can be read with DiaGi.                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *            SPL ( RECORD    ) -- Object with station name, spline     *
! *                                 coefficients, epoch nodes etc.       *
! *                                 Contents of that object describes    *
! *                                 site position evolution modeled with *
! *                                 a spline.                            *
! * FL_SPLINE_ONLY ( LOGICAL*4 ) -- If .FALSE. the site evolution model  *
! *                                 which will be plotted consists of    *
! *                                 both spline, global position and     *
! *                                 velocity. If .TRUE. the site         *
! *                                 evolution model consists of only     *
! *                                 spline.                              *
! *         FL_UEN ( LOGICAL*4 ) -- Flag which specifies whether the     *
! *                                 Up, East, North site coordinate      *
! *                                 components (if .TRUE) should be      *
! *                                 plotted. If .FALSE. then X, Y, Z     *
! *                                 components should be plotted.        *
! *           ICMP ( INTEGER*4 ) -- The coordinate component in the      *
! *                                 range 1,2,3 whose position should    *
! *                                 be plotted.                          *
! *        FILTMPL ( CHARACTER ) -- Template file name.                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  FILPLT ( CHARACTER ) -- Name of the output plot file. It is built   *
! *                          this way: tttt_ssss_c.sav where             *
! *                          tttt -- FILTMPL file;                       *
! *                          ssss -- station name (capital letters);     *
! *                          c    -- complonent code (capital letter).   *
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
! *  ### 10-MAR-2005    PLOT_SPL   v1.0 (c)  L. Petrov  10-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'bsp.i'
      INCLUDE   'diagi.i'
      INTEGER*4  ICMP, IUER
      CHARACTER  FILTMPL*(*), FILPLT*128
      LOGICAL*4  FL_SPLINE_ONLY, FL_UEN
      TYPE       ( BSPSTA__TYPE ) :: SPL
      TYPE       ( BSPSTA__TYPE ) :: SPL_USE
      TYPE       ( DIAGI_STRU   ) :: DIAGI_S
      INTEGER*4  MP, MS
      PARAMETER  ( MS = 64 )
      PARAMETER  ( MP = M__SPN*MS )
      REAL*8     ARG(MP), TIM(MP), VAL(MP), ERR(MP)
      REAL*8     ARG_NOD(M__SPN), TIM_NOD(M__SPN), VAL_NOD(M__SPN)
      REAL*8     ARG_STEP, POS_VAL(3), POS_SIG(3), EPS
      PARAMETER  ( EPS =1.D-4 )
      INTEGER*4  J1, J2, J3, J4, J5, IUEN, NP
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, IER
      CHARACTER  ZAG*128, UNIT*128, CMP(3,2)*1
      DATA       CMP / 'X', 'Y', 'Z',  'U', 'E', 'N' /
      REAL*8,    EXTERNAL :: BSPL_VAL 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IFIND_PL, LTM_DIF
!
! --- Make a local copy of SPL
!
      SPL_USE = SPL
      IF ( FL_UEN ) THEN
!
! -------- Transform spline coefficients and their covariance from 
! -------- crust fixed coordinate system XYZ to local topocentric 
! -------- coordinate system UEN
!
           CALL SPL_TO_UEN ( SPL_USE%L_NOD, SPL_USE%DEGREE, SPL_USE%APR_COO, &
     &                       SPL_USE%POS, SPL_USE%COV )
           IUEN = 2
         ELSE 
           IUEN = 1
      END IF
      NP = 0
      DO 410 J1=2,SPL_USE%L_NOD
         IF ( ( SPL_USE%TIM(J1) - SPL_USE%TIM(J1-1) ) > 1.0D0 ) THEN
!
! ----------- Set the step of the argument. It dependes on the length 
! ----------- of the span between adjacent knots
!
              ARG_STEP = (SPL_USE%TIM(J1) - SPL_USE%TIM(J1-1) - 2.D0*EPS )/(MS-1)
              DO 420 J2=1,MS
                 NP = NP + 1
                 ARG(NP) = SPL_USE%TIM(J1-1) + ARG_STEP*(J2-1) + EPS
                 TIM(NP) = ARG(NP)/(JYEAR__DAYS*86400.0D0) + 2000.0D0
!
! -------------- Compute the vector of site positions and formal uncertainties
!
                 CALL DISPL_BSPL ( FL_SPLINE_ONLY, SPL_USE%L_NOD, &
     &                             SPL_USE%TIM, SPL_USE%DEGREE, &
     &                             SPL_USE%TIM_COO, 1.0D0, SPL_USE%POS, &
     &                             SPL_USE%COV, ARG(NP), POS_VAL, POS_SIG ) 
                 VAL(NP) = POS_VAL(ICMP)*1.D3
                 ERR(NP) = POS_SIG(ICMP)*1.D3
 420          CONTINUE 
         END IF
!
! ------ Compute site position for J1-1 th knot
!
         ARG_NOD(J1-1) = SPL_USE%TIM(J1-1) + EPS
         TIM_NOD(J1-1) = ARG_NOD(J1-1)/(JYEAR__DAYS*86400.0D0) + 2000.0D0
         CALL DISPL_BSPL ( FL_SPLINE_ONLY, SPL_USE%L_NOD, SPL_USE%TIM, &
     &                     SPL_USE%DEGREE, SPL_USE%TIM_COO, 1.0D0, SPL_USE%POS, &
     &                     SPL_USE%COV, ARG_NOD(J1-1), POS_VAL, POS_SIG ) 
         VAL_NOD(J1-1) = POS_VAL(ICMP)*1.D3
 410  CONTINUE 
!
! --- Compute position for the last knot
!
      ARG_NOD(SPL_USE%L_NOD) = SPL_USE%TIM(SPL_USE%L_NOD) - EPS
      TIM_NOD(SPL_USE%L_NOD) = ARG_NOD(SPL_USE%L_NOD)/(JYEAR__DAYS*86400.0D0) + 2000.0D0
      CALL DISPL_BSPL ( FL_SPLINE_ONLY, SPL_USE%L_NOD, SPL_USE%TIM, &
     &                  SPL_USE%DEGREE, SPL_USE%TIM_COO, 1.0D0, SPL_USE%POS, &
     &                  SPL_USE%COV, ARG_NOD(SPL_USE%L_NOD), POS_VAL, POS_SIG ) 
      VAL_NOD(SPL_USE%L_NOD) = POS_VAL(ICMP)*1.D3
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  integer*4 k1
!  real*8    mom1, int1, bspl_mom1_full, bspl_int1_full
!      if ( iuen == 1  .and.  icmp ==3 ) then
!           mom1 = 0.0d0
!           int1 = 0.0d0
!           write ( 6, * ) ' tim(1) = ', spl_use%tim(1) ! %%%
!           write ( 6, * ) ' tim(2) = ', spl_use%tim(2) ! %%%
!           write ( 6, * ) ' tim(3) = ', spl_use%tim(3) ! %%%
!           write ( 6, * ) ' tim(4) = ', spl_use%tim(4) ! %%%
!           write ( 6, * ) ' disp: ', spl_use%pos(3,spl_use%l_nod+0) ! %%%%
!           write ( 6, * ) ' velo: ', spl_use%pos(3,spl_use%l_nod+1) ! %%%%
!           do 510 k1=1-spl_use%degree,spl_use%l_nod-1
!              write ( 6, * ) ' k1=',k1,' spl_pos=',spl_use%pos(3,k1), &
!     &           ' mom: ', bspl_mom1_full ( spl_use%l_nod, &
!     &                     spl_use%tim(1), spl_use%degree, k1 )
!              int1 = int1 + spl_use%pos(3,k1)* &
!     &                      bspl_int1_full ( spl_use%l_nod, &
!     &                                       spl_use%tim(1), spl_use%degree, k1 )
!              mom1 = mom1 + spl_use%pos(3,k1)* &
!     &                      bspl_mom1_full ( spl_use%l_nod, &
!     &                                       spl_use%tim(1), spl_use%degree, k1 )
! 510       CONTINUE 
!           write (6 ,  * ) 'z: int1: ', int1
!           write (6 ,  * ) 'z: mom1: ', mom1
!      end if
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- Clear DIAGI_S object
!
      CALL NOUT ( SIZEOF(DIAGI_S), DIAGI_S )
!
! --- Setting defaults values of the plotting parameters
!
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, -1 )
!
      FILPLT = FILTMPL(1:I_LEN(FILTMPL))//'_'// &
     &         SPL_USE%STATION(1:I_LEN(SPL_USE%STATION))//'_'// &
     &         CMP(ICMP,IUEN)//'.sav'
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      DIAGI_S%IDEV      = 5
      DIAGI_S%NCLR      = 2
      DIAGI_S%NPOI(1)   = NP
      DIAGI_S%ADR_X8(1) = LOC(TIM)
      DIAGI_S%ADR_Y8(1) = LOC(VAL)
      DIAGI_S%ADR_E8(1) = LOC(ERR)
      DIAGI_S%LER(1)    = .TRUE.
      DIAGI_S%ICOL(1)   = ICL1
      DIAGI_S%IBST(1)   = 4
      DIAGI_S%ILST(1)   = 2
      DIAGI_S%IOST(1)   = IOST
      DIAGI_S%IPST(1)   = 1
      DIAGI_S%IWST(1)   = 3
!
      DIAGI_S%NPOI(2)   = SPL_USE%L_NOD 
      DIAGI_S%ADR_X8(2) = LOC(TIM_NOD)
      DIAGI_S%ADR_Y8(2) = LOC(VAL_NOD)
      DIAGI_S%ADR_E8(2) = 0
      DIAGI_S%LER(2)    = .FALSE.
      DIAGI_S%ICOL(2)   = ICL1
      DIAGI_S%IBST(2)   = 0
      DIAGI_S%ILST(2)   = 1
      DIAGI_S%IOST(2)   = IOST
      DIAGI_S%IPST(2)   = 3
      DIAGI_S%IWST(2)   = IWST
!
      DIAGI_S%ICLR      = 1
      DIAGI_S%XMIN      = 1.0
      DIAGI_S%XMAX      = 0.0
      DIAGI_S%YMIN      = 1.0
      DIAGI_S%YMAX      = 0.0
      DIAGI_S%ZAG       = 'Station '//SPL_USE%STATION//' '//CMP(ICMP,IUEN)// &
     &                    '-coordinate (mm)'
      DIAGI_S%NAME      = FILPLT 
      DIAGI_S%ARG_UNITS = 'Time in years'
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 2
      DIAGI_S%STATUS    = DIA__DEF
!
! --- Calling the main routine of DiaGI
!
      CALL ERR_PASS  ( IUER, IER )
      CALL DIAGI     ( DIAGI_S, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1681, IUER, 'PLOT_SPL', 'Error in DiaGi' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PLOT_SPL 
