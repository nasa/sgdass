      SUBROUTINE PARTL ( DERIV, POLYONLY, PLACE, B3DOBJ )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PARTL PROGRAM SPECIFICATION
!
! 1.1
! ************************************************************************
! *                                                                      *
! * Preamble                                                             *
! * PARTL is at the very heart of an understanding of how SOLVE          *
! * works.  To understand SOLVE you must understand how it con-          *
! * structs the list of estimated parameters and that logic              *
! * is contained here.  What is to be estimated is contained in          *
! * in various bit arrays and flag words in SOCOM. The list of           *
! * estimated parameters is then constructed by interogating those       *
! * arrays and words in an invariant order, which is contained in        *
! * the code below.  Basically the list is contructed roughtly           *
! * as follows:                                                          *
! *                                                                      *
! *    Site 1 - source coords                                            *
! *             axis offset                                              *
! *             clocks:                                                  *
! *                epoch 1 -                                             *
! *                  polynomial coeffiecients                            *
! *                  diurnal coeffiecients                               *
! *                epoch 2 -                                             *
! *                    .......                                           *
! *                    .....                                             *
! *                epoch 3                                               *
! *                .....                                                 *
! *                epoch n                                               *
! *             atmospheres                                              *
! *                epoch 1                                               *
! *                ......                                                *
! *                epoch l                                               *
! *                                                                      *
! *    Site 2 -                                                          *
! *    ......                                                            *
! *    Site m                                                            *
! *                                                                      *
! *    Source 1 -                                                        *
! *       RA                                                             *
! *       Dec                                                            *
! *    Source 2                                                          *
! *    .....                                                             *
! *    Source n                                                          *
! *                                                                      *
! *    Earth rotation -                                                  *
! *       epoch 1 -                                                      *
! *         x-pole -                                                     *
! *           0th order                                                  *
! *           ....                                                       *
! *           3rd order                                                  *
! *         y-pole                                                       *
! *           ....                                                       *
! *         UT1 -                                                        *
! *           ....                                                       *
! *                                                                      *
! *    Gamma                                                             *
! *                                                                      *
! *    Earth tide                                                        *
! *       Love l                                                         *
! *       Love h                                                         *
! *       lag                                                            *
! *                                                                      *
! *       or                                                             *
! *                                                                      *
! *       Site 1 -                                                       *
! *         Love l                                                       *
! *         Love h                                                       *
! *         lag                                                          *
! *       Site 2                                                         *
! *       .....                                                          *
! *       Site n                                                         *
! *                                                                      *
! *   Precession constant                                                *
! *                                                                      *
! *   Nutation -                                                         *
! *       Dpsi                                                           *
! *       Deps                                                           *
! *                                                                      *
! *       or                                                             *
! *                                                                      *
! *       Time series coefficients for nutation in longitude             *
! *          Term 1                                                      *
! *            In-phase                                                  *
! *            Out-of-phase                                              *
! *          Term 2                                                      *
! *          .                                                           *
! *          Term 6                                                      *
! *                                                                      *
! *       Time series coefficients for nutation in obliquity             *
! *          Term 1                                                      *
! *            In-phase                                                  *
! *            Out-of-phase                                              *
! *          Term 2                                                      *
! *          .                                                           *
! *          Term 6                                                      *
! *                                                                      *
! *   Baseline dependent clocks                                          *
! *       Baseline 1,1                                                   *
! *       Baseline 1,2 (=Baseline 2,1)                                   *
! *       .....                                                          *
! *       Baseline n,n-1                                                 *
! *                                                                      *
! * Subroutine PARTL takes the derivative values from OBSFIL and         *
! * uses them to set up derivative arrays according to the flags         *
! * seu up by SETFL.  The derivative flags are packed, one flag          *
! * per bit.                                                             *
! *                                                                      *
! *   Units:                                                             *
! *                                                                      *
! *   site positions:        meter                                       *
! *   site velocity:         meter/year                                  *
! *   axis offset:           meter                                       *
! *   clock function:        sec                                         *
! *   baseline clock:        sec                                         *
! *   clock rate:            dimensionless                               *
! *   clock aceleration:     1/sec                                       *
! *   atmopshere delay:      sec                                         *
! *   atmopshere grad:       mm                                          *
! *   right ascension:       rad                                         *
! *   declination:           rad                                         *
! *   right ascension rate:  rad/year                                    *
! *   declination rate:      rad/year                                    *
! *   polar motion:          rad                                         *
! *   UT1:                   sec (!! but partials: -sec !!)              *
! *   X rate, Y rate:        rad/day                                     *
! *   UT1 rate:              sec/day                                     *
! *   Nutation angles:       rad                                         *
! *   Gamma:                 dimensionless                               *
! *                                                                      *
! ******************************************************************     *
!
! 1.2 REFERENCES:
!
! 2.  PARTL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      logical*2 polyonly
!
! POLYONLY - True for stations with only polynopmial clocks (no stochastics)
!
! 2.3 OUTPUT Variables:
!
      REAL*8 DERIV(M_GPA,2)
!
! DERIV - Array of partial derivatives of the parameters
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'oborg.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'prfil.i'
      INCLUDE 'fast.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
!     CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 JBYTES, JBLOCKS, FILDES
      CHARACTER FNAME*(NAME_SIZE) 
      REAL*8 SIN,COS,SIGN
      REAL*8 TIM, TRACE_CONVERT, DELT, DEFTEMP
      REAL*8, SAVE :: USER_DERIV(M_GPA*2), PART_UT1XY(2,2,MAX_SDC)
      REAL*8 TIM_ARG, TIM_SINCE_REF, TIM_VEL_YR, TIM_PRM_YR, TIM_PRM_SEC
      REAL*8 T, TSIN, TARG, ARG, SAVFJD, DAYS_PER_CYCLE, T1, T2
!
      REAL*8    PHI, ELON, H, COEF, MTT, MTTR, GRAD(2), GRADOT(2), CNVRS_UT
      PARAMETER  ( CNVRS_UT    =   1.002737909D0 * 12.0D0 * 60.0D0 * 60.0D0/PI__NUM )
      LOGICAL*2 LOOK, IOK, KFIRST
      LOGICAL*2 KBIT, NOCONST
      LOGICAL*2 KBITP, KBIT_LATM, KBIT_ICLOCK, KBIT_LPROP, KBIT_LSTAR
      INTEGER*2 IEOP, IDS, ND, ISTOPINC, IRATE
      INTEGER*2 I, J , K, NP, ISTA, N, IUHR, IDIURN, IPOLY, JCLOCK
      INTEGER*4 NCOUNT, NCOUNT_CLO, NCOUNT_ATM, NCOUNT_EOP, NCOUNT_BSL, NSIN
      INTEGER*2 JDIURN, IORD, IUHRS, JCLOCKS, IATM, JATM, IAORD
      INTEGER*2 ISTR, IRADEC, ISTOP, JPOLY, NRT, IXYU, INUT, IP1
      INTEGER*2 IROTT
      SAVE KFIRST,SAVFJD
      DATA KFIRST / .TRUE. /
      COMMON    / PARTFILE / FNAME, FILDES
!
      LOGICAL*4  USE_RATE, FL_SOU_USE
      REAL*8     BACK_FRACTION, FORWARD_FRACTION, DELTA_TIM, DBRK_TIM, &
     &           EOP_INT, DER_EROT(3), DER_EROT_RATE(3)
      REAL*8       EPSILON_0, NUT_FCT(2), DIF_DEL
      PARAMETER  ( EPSILON_0 = 0.4090928041D0 )  ! rad
      INTEGER*4  N_EOP, NUPT, &
     &           IND_PT(M_GPA), IND_GRAD, NPAR_NONUSER
      INTEGER*4  INT2_ARG
      INTEGER*4  NEXT_NODE, J2, J3, J4, J5, J6, KSTA, KBAS
      SAVE       IND_PT
      INTEGER*4  IOBS, I4_ARR(2), IEL, IHPE, ISPE, INOD, KNOT_PIVOT, J1, &
     &           NERM_SKIP, KNOT_EXTRA, IERM, IHEO, IP, IUER
      INTEGER*4  IXYU_START  (2),     IXYU_STOP  (2)
      DATA       IXYU_START / 1, 3 /, IXYU_STOP / 2, 3 /
      TYPE       ( PLACE__STRU ) :: PLACE
      TYPE       ( B3D__STRU   ) :: B3DOBJ
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER
      INTEGER*4, EXTERNAL :: IXMN8, LTM_DIF
      LOGICAL*4, EXTERNAL :: CHECK_STABIT, IS_R8_NAN
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!
! ************************************************************************
! *                                                                      *
! * T. HERRING MOD ADDED 05NOV81 BY CAK FOR DIURNAL SIN AND COS          *
! * clock partials to be formed using flags 15 and 16 of LCLK            *
! * with flag 14 denoting a continuation.                                *
! *                                                                      *
! * MALLAMA, MAY 1984, Added rate continuity logic for clocks            *
! *                                                                      *
! * JWR 85NOV01                                                          *
! * This routine was virtually rewritten.  The following are some of     *
! * the major elements of that rewrite.                                  *
! * 'OBORG' put in PARTL and calling list trimed. Appropiate changes     *
! * to the detailed use of the partials arrays in OBORG also made.       *
! * Logic modified so that both delay and rate partials are comput-      *
! * ed. Corresponding changes made to PROC and CRES.                     *
! * Clock derivative logic rewritten from scratch to make tractable.     *
! * Use of structured code enforced throughout.                          *
! * Daily nutation logic added.                                          *
! * All code relative to atmosphrese, sources, and UT1 PM rewritten      *
! * doing away with Doug's mask logic.                                   *
! *                                                                      *
! * 6/26/86 KDB BASELINE CLOCKS - minor changes (do loop range,          *
! *             etc.) to make consistent ICLOCK handling                 *
! *             throughout SOLVE                                         *
! * 11/5/86 MK  Continued atmospheres and atmosphere rate added.         *
! * 87.06.12 ejh parms expanded from 192 to 384                          *
! * 90.01.24 JWR Bug in the order of getting earth tide partials         *
! *          fixed.                                                      *
! * 90.12.20 MWH Replaced KBIT calls with statement functions            *
! * 91.05.22 MWH Implemented new clock and atmosphere parameterization   *
! *                 scheme                                               *
! * 91.05.23 MWH Add call to CEKPARTIAL to overwrite the wet partial     *
! * 91.11.08 AEE Moved call to CEKPARTIAL to CFACALC                     *
! * 94.05.09 MSH Fixed sign of baseline-dependent clock partial to       *
! *              correspond to station order                             *
! * 96.01.17 KDB Fixed 32 site update; htest applied to an integer*2     *
! *              variable (such as iclsta) only can test the first       *
! *              16 bits, so replace htest calls of iclsta,              *
! *              lsitec, lsitev and laxof with                           *
! *              kbit calls.  (The htest calls were done via kbitp.)     *
! * 97.04.14 jwr Major rewrite and simplification at the time of the     *
! *              introduction of Petrov's B3D. The logic logic for       *
! *              segmented atmospheres was massively simplified by       *
! *              taking advantage of the fact that the obserations       *
! *              strictly  time ordered. THe old code was ~7 levels      *
! *              deep; the new code is about 2 (and should run much      *
! *              faster because it is not always searching the           *
! *              entire list of epochs.                                  *
! *                                                                      *
! *              Also high frequency EOP logic rewritten to estimate     *
! *              offsets at the segment breaks rather than the rates     *
! *              between epochs. This speeds up the processing and       *
! *              B3D possible for HF EOP.                                *
! *                                                                      *
! * 97.04.25 pet Changed the logic for calculation current epoch in      *
! *              estimation high frequency EOP and atmosphere.           *
! *                                                                      *
! * 97.04.28 pet Added stuff for implementation new scheme of            *
! *              clock break handling.                                   *
! *                                                                      *
! * 97.06.02 pet Excluded treating zero-elements in user-partials.       *
! *                                                                      *
! * 97.07.11 pet Corrected substantial bug in calculating the time       *
! *              of next epoch for fast algorithm.                       *
! *                                                                      *
! * 97.12.02 pet Added logic for bypassing deselected station            *
! * 98.01.12 pet Added entire bypassing the deselected station.          *
! * 98.03.30 pet Corrected a bug in UNF_CLO section: theoretical         *
! *              delay should be added to delta time ONLY if delta       *
! *              time is applied for the second station of the           *
! *              baseline.                                               *
! * 98.05.06 pet Applied TAU_ACM when we calculate time at the           *
! *              second station.                                         *
! * 98.05.08 pet Corrected a bug: F__PUT__IND  was missed for            *
! *              atmosphere rate.                                        *
! * 98.06.16 pet Corrected a bug caused by rounding errors in            *
! *              calculation derivatives for segmented paraemters        *
! * 98.09.23 pet Corrected a bug: coefficients of equations of           *
! *              conditions were calculated wrong for segmented UT1      *
! *                                                                      *
! * 1999.05.04 pet Fixed a bug: variables IPOLY and JPOLY was not        *
! *                initialized before the first loop after comments      *
! *                starting with words "BEGIN ATMOSPHERE SECTION".       *
! *                Removed unused variables.                             *
! *                                                                      *
! * 1999.11.11 pet Added logic for utilizing atmosphere gradients.       *
! *                If Calc 8.2 and earlier was used then partials        *
! *                wrt atmosphere gradients are computed using MTT       *
! *                mapping function. If Calc 9.1 and older is used       *
! *                then partials have been already computed.             *
! *                                                                      *
! * 2000.01.25 pet Added support of a variable NORATE_FLAG defined       *
! *                in glbc4.i . If NORATE_FLAG is .TRUE. then            *
! *                computation of partial derivatives for delay          *
! *                rate is suppressed. Part of code which does it        *
! *                is bypassed. User partials in this mode are           *
! *                considered as containing partials of delay only       *
! *                and therefore sized for twice less size.              *
! *                                                                      *
! * 2000.06.13 pet Removed code whcih supported esitmation of Love       *
! *                numbers an some terms of nutation in the past         *
! *                since these partials deerivatives were removed        *
! *                from oborg.                                           *
! *                                                                      *
! * 2001.03.05 pet Added support of UPT__CMP and UPT__DEL formats        *
! *                of user partials.                                     *
! *                                                                      *
! * 2002.12.24 pet Changed the logic of setting reference epoch for      *
! *                site/source position: the previous version used       *
! *                variable TIME0 (epoch for mapping of site position).  *
! *                The new version uses variable SIT_EST_EPOCH.          *
! *                The old version used J2000.0 epoch for source         *
! *                coordinates. The new version uses variable            *
! *                SOU_EST_EPOCH.                                        *
! *                                                                      *
! * 2003.08.12 pet Replaced statement function kbitp with function kbitp *
! *                Apparantly due to a bug in compiler HP Fortran 2.5.1  *
! *                in some places the compiler considered expression     *
! *                with statement function as loop invariant, what was   *
! *                wrong.                                                *
! *                                                                      *
! * 2003.08.14 pet Simplified logic for handling with source positions,  *
! *                source proper motion and baseline dependent clocks.   *
! *                                                                      *
! * 2003.08.15 pet Simplified logic for handling atmosphere gradients    *
! *                                                                      *
! * 2003.09.01 pet Fixed a typo related to logic of handling source      *
! *                proper motion. The previous version computed this     *
! *                partial derivative incorrectly.                       *
! * 2003.09.24 pet Corrected the error related to fxing a bug in         *
! *                computing partials wrt atmosphere gradients.          *
! *                The problem is that duw to rounding there may be      *
! *                an obsrvation with the time tag after the last epoch  *
! *                for gradients.                                        *
! * 2005.04.01 pet Added support of estimation non-linear site position  *
! *                displacements models with expansion with the B-spline *
! *                basis and harmonic site position variations.          *
! * 2006.01.29 pet Added support of estimation of parameters related to  *
! *                the Earth's rotation model.                           *
! * 2006.07.07 pet Added support for EHEO and ERM.                       *
! * 2007.08.09 pet Added support of estimation of source structure delay *
! *                admittance.                                           *
! * 2021.07.02 pet Fixed a bug for eerm partials for delay rate.         *
! * 2022.08.21 pet Added support of ionosphere scale estimation.         *
! * 2023.03.13 pet Changed unigsd for proper motions to rad/sec.         *
! *                                                                      *
! ************************************************************************
!
! 5. PARTL PROGRAM STRUCTURE
!
! --- Statement functions (to reduce number of calls to kbit)
!
!       kbitp(i,j) = btest(i, j-int2(1) )
       kbit_latm(i,j,k) = btest( latm(i-int2(1)+(k+int2(15))/int2(16),j), &
     &                  k-((k+int2(15))/int2(16)-int2(1))*int2(16)-int2(1) )
!
!     kbit_lstar(i,j,k) = htest( lstar(i-int2(1)+(k+int2(15))/int2(16),j), &
!     &                  k-((k+int2(15))/int2(16)-int2(1))*int2(16)-int2(1) )
!!
!     kbit_lprop(i,j,k) = htest( lprop(i-int2(1)+(k+int2(15))/int2(16),j), &
!     &                   k-((k+int2(15))/int2(16)-int2(1))*int2(16)-int2(1) )
!!
!     kbit_iclock(i,j,k) = htest( iclock(i-int2(1)+(k+int2(15))/int2(16),j), &
!     &                    k-((k+int2(15))/int2(16)-int2(1))*int2(16)-int2(1) )
!
!--------------------------------------------------------
!
! --- Establish the reference time for plate motion model, in centuries
! --- since J2000. Default is 80/10/17:00:00:00
!
!      DATA REF/-0.19207390575253448D0/
!
!      REF = TIME0/100.0d0 - (2444529.5d0/36524.219d0
!     #        + 0.192073905752534480d0)
      IF ( NORATE_FLAG ) THEN
           USE_RATE = .FALSE.
           IRATE = 1
         ELSE
           USE_RATE = .TRUE.
           IRATE = 2
      END IF
!
      IF ( NUT_USE_CODE == NUT__XY ) THEN
           NUT_FCT(1) = 1.0D0/DSIN(EPSILON_0)
           NUT_FCT(2) = 1.0D0
         ELSE 
           NUT_FCT(1) = 1.0D0
           NUT_FCT(2) = 1.0D0
      END IF
!
! --- Zero out the derivative array, so that only the non-zero derivatives
! --- need to be initialized
!
      CALL NOUT_R8 ( 2*M_GPA, DERIV )
!
! --- Calculate the epoch of the observation
!
      TIM = FJD + FRACTC
      IF ( SOCOM_PLUS_FIRST  .NE.  SPL__DONE  ) THEN
           CALL ERR_LOG ( 9101, -1, 'PARTL', 'socom has not '// &
     &         'been extended to socom_plus' )
           STOP 'Error of internal control detected. Abnormal termination'
      END IF
!
! --- Get amount of time in years elapsed from the reference epoch for
! --- station velocities and source proper motions
!
      TIM_VEL_YR  = ( TIM - SIT_EST_EPOCH )/JYEAR__DAYS
      TIM_PRM_YR  = ( TIM - SOU_EST_EPOCH )/JYEAR__DAYS
      TIM_PRM_SEC = TIM_PRM_YR*YEAR__TO__SEC
!
! --- Process the station derivatives, including station coordinates,
! --- antenna axis offsets, clock polynomial coefficients, and
! --- atmosphere parameters
!
! --- Run over sites 1 and 2
!
      DO NP = 1,2 !running over station 1 and 2
         ISTA = ISITE(NP)
!
! ------ If station is deselected -- bypass it
!
         IF ( .NOT. CHECK_STABIT ( ISTA ) ) GOTO 810
!
! ------ First initialize SIGN so that the derivatives will have the correct
! ------ sign depending on whether we are handling site 1 or 2.
!
         IF ( NP.EQ.1 ) THEN
              SIGN = -1.D0
            ELSE
              SIGN =  1.D0
         END IF
!
! ------ Initialize NCOUNT for the first derivative associated with this site
!
         NCOUNT = NSPARM(ISTA)
!
! ----- BEGIN SITE POSITION SECTION
!
        IF ( KBITP( IUEN, INT2(1) ) ) CALL ROTATE_PARTS ( ISTA, NP )
        DO J = 1,3 ! Running over x,y,z
           IF ( KBIT ( LSITEC(1,J), ISTA) ) THEN !coordinate turned on
                IF ( PSITED(ISTA) .EQ. 0 ) THEN
                     NCOUNT=NCOUNT+1
                     DERIV(NCOUNT,1) = BP(J,NP,1)
                     IF ( USE_RATE ) THEN
                          DERIV(NCOUNT,2) = BP(J,NP,2)
                     END IF
                     CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                  ELSE
!
! ------------------ Station position are modeled by linear spline
!
                     N=1
                     DO I=1,PWCNUM(1)-1
                        IF (TIM.GE.PWCEP(I)) N=I
                     ENDDO
                     NCOUNT = NCOUNT + N
                     T1 = TIM-PWCEP(N)
                     IF ( NP .EQ. 2 ) T1 = T1 + (DT/8.64D10 - TAU_ACM/86400.D0)
                     T1 = T1*86400.D0
                     T2 = (PWCEP(N+1) - PWCEP(N))*86400.D0
                     DERIV(NCOUNT,1) = BP(J,NP,1)*(T2-T1)/T2
                     IF ( USE_RATE ) THEN
                          DERIV(NCOUNT,2)=-BP(J,NP,1)/T2+BP(J,NP,2)*(T2-T1)/T2
                     END IF
                     CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                     NCOUNT = NCOUNT+1
                     DERIV(NCOUNT,1) = BP(J,NP,1)*T1/T2
                     IF ( USE_RATE ) THEN
                          DERIV(NCOUNT,2) = BP(J,NP,1)/T2+BP(J,NP,2)*T1/T2
                     END IF
                     CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                     NCOUNT = NCOUNT + PWCNUM(1) - N - 1
                ENDIF ! PSITED
           ENDIF
        ENDDO
!
! ----- Station velocity
!
        DO J = 1,3 ! Running over x,y,z
           IF ( KBIT( LSITEV(1,J), ISTA ) ) THEN !velocity turned on
                NCOUNT=NCOUNT+1
                DERIV(NCOUNT,1) = BP(J,NP,1)*TIM_VEL_YR
                IF ( USE_RATE ) THEN
                     DERIV(NCOUNT,2) = BP(J,NP,2)*TIM_VEL_YR
                END IF
                CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
           ENDIF
        ENDDO
!
! ----- Sideral diurnal radial station position
!
        IF ( KBITP ( IUEN, INT2(2) ) ) THEN ! sideral diurnal
             IF ( KFIRST ) THEN
                  SAVFJD=FJD
                  KFIRST=.FALSE.
             ENDIF
             IF ( .NOT. KBITP( IUEN, INT2(1) ) ) CALL ROTATE_PARTS ( ISTA, NP )
             TSIN=FJD-SAVFJD
             TSIN=TSIN+FRACTC+1.d0
             IF ( NP .EQ. 2 ) TSIN = TSIN + (DT/8.64D10 - TAU_ACM/8.64D4)
             TSIN=TSIN*1.002737909350795D0 ! Sideral time rate
             TARG = 2.D0*PI__NUM*TSIN
             SIN = DSIN(TARG)
             COS = DCOS(TARG)
             NCOUNT=NCOUNT+1
             DO I=1,IRATE
                DERIV(NCOUNT,I)=BP(1,NP,I)*SIN
             ENDDO
             CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
             NCOUNT=NCOUNT+1
             DO I=1,IRATE
                DERIV(NCOUNT,I)=BP(1,NP,I)*COS
             ENDDO
             CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
        ENDIF ! sideral diurnal
!
        IF ( FL_HPESOL ) THEN
             TIM_ARG = ( (FJD - J2000__JD) + FRACTC )*86400.0D0 - 32.184D0
             DO IHPE = 1,L_HPE ! Running over Harmonics
                DO J = 1,3 ! Running over X,Y,Z coordinates!
                   IF ( HPESOL(ISTA,IHPE)%FL_EST ) THEN
                        NCOUNT = NCOUNT + 1
                        DERIV(NCOUNT,1) = BP(J,NP,1)* &
     &                        DCOS( HPESOL(ISTA,IHPE)%PHASE + &
     &                              HPESOL(ISTA,IHPE)%FREQ*TIM_ARG )
                        IF ( USE_RATE ) THEN
                             DERIV(NCOUNT,2) = -BP(J,NP,2)* &
     &                             HPESOL(ISTA,IHPE)%FREQ*  &
     &                             DSIN( HPESOL(ISTA,IHPE)%PHASE + &
     &                                   HPESOL(ISTA,IHPE)%FREQ*TIM_ARG )
                        END IF
                        CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
!
                        NCOUNT = NCOUNT + 1
                        DERIV(NCOUNT,1) = BP(J,NP,1)* &
     &                        DSIN( HPESOL(ISTA,IHPE)%PHASE + &
     &                              HPESOL(ISTA,IHPE)%FREQ*TIM_ARG )
                        IF ( USE_RATE ) THEN
                             DERIV(NCOUNT,2) = BP(J,NP,2)* &
     &                             HPESOL(ISTA,IHPE)%FREQ*  &
     &                             DCOS( HPESOL(ISTA,IHPE)%PHASE + &
     &                                   HPESOL(ISTA,IHPE)%FREQ*TIM_ARG )
                        END IF
                        CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                   END IF
                END DO
             END DO
        END IF
!
        IF ( FL_SPESOL ) THEN
!
! ---------- Time arguments in TAI in seconds elapsed since J2000.0D
!
             TIM_ARG = ( (FJD - J2000__JD) + FRACTC)*86400.0D0 - 32.184D0
             DO ISPE = 1,L_SPE ! Running over stations with spline parameterization
                IF ( SPESOL(ISPE)%IND_STA == ISTA ) THEN
                   KNOT_PIVOT = SPESOL(ISPE)%IND_NOD + SPESOL(ISPE)%CORR_IND
                   DO J = 1,3 ! Running over X,Y,Z coordinates!
                      IF ( SPESOL(ISPE)%FL_CHECK_OVER ) THEN
!
! ------------------------ Check, whether we need to advance a node in the
! ------------------------ case if the node is within the session interval
! ------------------------ and we are PAST the node
!
                           NEXT_NODE = SPESOL(ISPE)%IND_NOD+1
                           IF ( TIM_ARG .GT. SPESOL(ISPE)%NOD_ARR(NEXT_NODE) ) THEN
                                NCOUNT = NCOUNT + SPESOL(ISPE)%CORR_IND
                           END IF
                      END IF
                      DO INOD = -SPESOL(ISPE)%DEGREE,0
                         NCOUNT = NCOUNT + 1
                         DERIV(NCOUNT,1) = BP(J,NP,1)* &
     &                         BSPL_VAL ( SPESOL(ISPE)%L_NOD,   &
     &                                    SPESOL(ISPE)%NOD_ARR, &
     &                                    SPESOL(ISPE)%DEGREE,  &
     &                                    KNOT_PIVOT+INOD, TIM_ARG )
                         IF ( USE_RATE ) THEN
                              DERIV(NCOUNT,2) = BP(J,NP,2)* &
     &                              BSPL_DER ( SPESOL(ISPE)%L_NOD,   &
     &                                         SPESOL(ISPE)%NOD_ARR, &
     &                                         SPESOL(ISPE)%DEGREE,  &
     &                                         KNOT_PIVOT+INOD, TIM_ARG )
                         END IF
                         CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                      END DO
                      IF ( SPESOL(ISPE)%FL_CHECK_OVER ) THEN
!
! ------------------------ Check, whether we need to advance a node in the
! ------------------------ case if the node is within the session interval
! ------------------------ and we are BEFORE the node
!
                           IF ( TIM_ARG .LE. SPESOL(ISPE)%NOD_ARR(NEXT_NODE) ) THEN
                                NCOUNT = NCOUNT + SPESOL(ISPE)%CORR_IND
                           END IF
                      END IF
                   END DO
                END IF
             END DO
        END IF
!
! ----- Test the axis offset flag
!
        IF ( KBIT( LAXOF(1), ISTA) ) THEN ! axis offset estimated
             NCOUNT=NCOUNT+1
             DERIV(NCOUNT,1)=AXOFP(NP,1)
             IF ( USE_RATE ) THEN
                  DERIV(NCOUNT,2)=AXOFP(NP,2)
             END IF
             CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
        ENDIF
!
! ===== END SITE POSITION SECTION
!
!
! ---- Handle the clock derivatives
!
       IF ( UNF_CLO .AND.  KBIT(ICLSTA(1,1+ICLSTR(ISTA)), ISTA) ) THEN
!
! --------- This section of code handles case of uniform interval for clocks
!
! ------- DELTA_TIM -- time in days elapsed since the first clock epoch
! ------- NB: 1) order of oprations (not to lose precision)
! -------     2) time epoch of the second station is a bit different
!
          DELTA_TIM = ( FJD - JDATE_CLO(1) ) + FRACTC
          IF ( NP .EQ. 2 ) DELTA_TIM = DELTA_TIM + (DT/8.64D10 - TAU_ACM/8.64D4)
!
! ------- Find the CLO segmentation epoch just prior this obs
!
          IF ( TIM .GT. JDATE_CLO(PLACE%CLO_SEG+1) ) THEN
              PLACE%CLO_SEG = IDINT ( ( TIM - JDATE_CLO(1) )/CLO_INTERVAL ) + 1
              IF ( PLACE%CLO_SEG .GE. NUM_CLO ) THEN
!
! ---------------- If this observation is a bit after the last epoch we'll
! ---------------- play back. Such a situation is possible since it is allowed
! ---------------- that the last observation of the session follows (a bit)
! ---------------- after the last epoch
!
                   PLACE%CLO_SEG = PLACE%CLO_SEG - 1
              END IF
          END IF
!
         IF ( NPL_CLO .GE. 1 ) THEN
              NCOUNT_CLO = NCOUNT + 2
!
! ----------- Global clock rate          ( 1-st order )
!
              DERIV(NCOUNT_CLO,1) = SIGN*DELTA_TIM
              IF ( USE_RATE ) THEN
                   DERIV(NCOUNT_CLO,2) = SIGN/86400.D0
              END IF
              CALL F__PUT_IND ( NCOUNT_CLO, F__GLO, DERIV, PLACE, B3DOBJ )
         END IF
         IF ( NPL_CLO .GE. 2 ) THEN
              NCOUNT_CLO = NCOUNT + 3
!
! ----------- Global clock rate of drift ( 2-nd order )
!
              DERIV(NCOUNT_CLO,1) = SIGN*DELTA_TIM**2
              IF ( USE_RATE ) THEN
                   DERIV(NCOUNT_CLO,2) = 2.D0*SIGN*DELTA_TIM/86400.D0
              END IF
              CALL F__PUT_IND ( NCOUNT_CLO, F__GLO, DERIV, PLACE, B3DOBJ )
         END IF
!
         IF ( NUM_BRK(ISTA) .GT. 0 ) THEN
!
! --------- Case when real clock breaks detected on the ISTA-th station. Clock
! --------- breaks parameteres are followed by global clocks polinomials
!
            DO 410 J1=1,NUM_BRK(ISTA)
               IF ( TIM .GE. JDATE_BRK(J1,ISTA) ) THEN
                  DBRK_TIM = ( FJD - JDATE_BRK(J1,ISTA) ) + FRACTC
                  IF ( NP .EQ. 2 ) DBRK_TIM = DBRK_TIM + &
     &                                        (DT/8.64D10 - TAU_ACM/8.64D4)
!
! --------------- Shift after break
!
                  NCOUNT_CLO = NCOUNT + (NPL_CLO + 1)*J1 + 1
                  DERIV(NCOUNT_CLO,1) = SIGN
                  DERIV(NCOUNT_CLO,2) = 0.D0
                  CALL F__PUT_IND ( NCOUNT_CLO, F__GLO, DERIV, PLACE, B3DOBJ )
!
                  IF ( NPL_CLO .GE. 1 ) THEN
!
! -------------------- Drift after break
!
                     NCOUNT_CLO = NCOUNT + (NPL_CLO + 1)*J1 + 2
                     DERIV(NCOUNT_CLO,1) = SIGN*DBRK_TIM
                     IF ( USE_RATE ) THEN
                          DERIV(NCOUNT_CLO,2) = SIGN/86400.D0
                     END IF
                     CALL F__PUT_IND ( NCOUNT_CLO, F__GLO, DERIV, PLACE, B3DOBJ )
                  END IF
                  IF ( NPL_CLO .GE. 2 ) THEN
!
! ------------------ Frequency drift after break
!
                     NCOUNT_CLO = NCOUNT + (NPL_CLO + 1)*J1 + 3
                     DERIV(NCOUNT_CLO,1) = SIGN *DBRK_TIM**2
                     IF ( USE_RATE ) THEN
                          DERIV(NCOUNT_CLO,2) = 2.D0 *SIGN *DBRK_TIM /86400.D0
                     END IF
                     CALL F__PUT_IND ( NCOUNT_CLO, F__GLO, DERIV, PLACE, B3DOBJ )
                  END IF
               END IF
 410        CONTINUE
         END IF
!
         FORWARD_FRACTION = DELTA_TIM/CLO_INTERVAL - (PLACE%CLO_SEG-1)
         BACK_FRACTION    = 1.D0 - FORWARD_FRACTION
!
         NCOUNT_CLO = NCOUNT + PLACE%CLO_SEG
         IF ( PLACE%CLO_SEG .GT. 1 ) NCOUNT_CLO = NCOUNT_CLO + NPL_CLO + &
     &                                            NUM_BRK(ISTA)*(NPL_CLO+1)
!
! ------ Do the non-zero derivative for the epoch just before the obs.
! ------ Note that for station 2 we had the theoretical delay to the
! ------ time tag because station 2 observes at a different time than
! ------ station 1. (8.64d10 converts microseconds to days.)
!
         DERIV(NCOUNT_CLO,1)=  SIGN*BACK_FRACTION
         IF ( USE_RATE ) THEN
              DERIV(NCOUNT_CLO,2)= -SIGN/(CLO_INTERVAL*86400.D0)
         END IF
         CALL F__PUT_IND ( NCOUNT_CLO, F__CUS, DERIV, PLACE, B3DOBJ )
!
         IF ( PLACE%CLO_SEG .EQ. 1 ) NCOUNT_CLO = NCOUNT_CLO + NPL_CLO + &
     &                                            NUM_BRK(ISTA)*(NPL_CLO+1)
!
         NCOUNT_CLO = NCOUNT_CLO + 1
         DERIV(NCOUNT_CLO,1)=  SIGN*FORWARD_FRACTION
         IF ( USE_RATE ) THEN
              DERIV(NCOUNT_CLO,2)=  SIGN/(CLO_INTERVAL*86400.D0)
         END IF
         CALL F__PUT_IND ( NCOUNT_CLO, F__NES, DERIV, PLACE, B3DOBJ )
!
! ------ Set ncount correctly for the next parameter after this sites CLO.
!
         NCOUNT = NCOUNT + NUM_CLO + NPL_CLO + NUM_BRK(ISTA)*(NPL_CLO+1)
       END IF
!
       IF ( .NOT. UNF_CLO   .AND.  NUMCLK(ISTA).GT.0 ) THEN  ! Clocks
          if ( polyonly .or. old_clocks ) then
!
! ---------- This code is now used only in cases where there are
! ---------- no constrained clocks
!
            look  = .true.
            iuhr = 0
            idiurn = 0
            ipoly = 0
!
            do while(look) ! Running over clock epochs for this station
               iuhr = iuhr+1
!
! ------------ See if this is the standard epoch for this observation.
! ------------ The standard epoch is found when: (1) the current epoch is the
! ------------ is the last in the list of epochs for this station or
! ------------ (2) the next epoch is a later time than the time of this
! ------------ observation.
!
               jclock = iuhr+iclstr(ista)
               if( iuhr.eq.numclk(ista).or. &
     &            (tim.lt.fjdcl(jclock+1).and. &
     &             kbit(iclsta(1,jclock+1),ista)) ) then
!
! ---------------- the standard epoch has been found
!
! ---------------- Initialize the pointers which point at the first epochs
! ---------------- for both polynomials and diurnal for this obs and the
! ---------------- last epoch applicable to this observation.
! ---------------- Only set IDIURN if the diurnal for this epoch has been set.
!
                  ipoly  = iuhr
                  istop  = iuhr
                  jpoly  = jclock
                  look   = .false.
                  if(kbitp( lclk(jclock), int2(14)).or.kbitp( lclk(jclock), &
     &               int2(15)) ) then
!
! ------------------- the diurnal is being estimated for this epoch
!
                      idiurn = iuhr
                      jdiurn = jclock
                  endif
              endif
          enddo
!
! ------- Now we have got to handle the problem of possible series of
! ------- continued polynomials which apply to this point.
!
          if(ipoly.gt.1) then
             do while(ipoly.gt.1.and.(kbitp( lclk(jpoly), int2(13)).or..not. &
     &          kbit(iclsta(1,jpoly),ista)) )
!
! ------------- it's a continued polynomail clock
!
                ipoly  = ipoly - 1
                jpoly  = jpoly - 1
             enddo
          endif
!
! ------- Now handle to problem of a possible series of continued diunal
! ------- epochs.
!
          if(idiurn.gt.1) then
             do while(idiurn.gt.1.and.(KBITP( lclk(jdiurn), int2(14)).or..not. &
     &          kbit(iclsta(1,jdiurn),ista)) )
!
! ------------- it's a continued diurnal clock
!
                idiurn = idiurn - 1
                jdiurn = jdiurn - 1
            enddo
          endif
!
! ------- That was hard work, but at least we know what epochs apply to this
! ------- observation
!
! ------- Now we have got to compute the clock derivatives for this
! ------- obseration and stick them in the correct slots in the DERIV array.
!
! ------- We must run over all epochs in order to increment NCOUNT correctly.
!
! ------- T is the time argument for the polynomials and TSIN is the argument
! -------   for the diurnals.
!
! ------- To be painfully correct we take into account that the time
! ------- applicable to site 2 must be the time tag of the obs plus
! ------- the VLBI delay. We take into accout that DT is is microseconds
! ------- and the time tag is days.
!
! ------- First initialize SIGN so that the derivatives will have the correct
! ------- sign depending on whether we are handling site 1 or 2.
!
          do iuhr = 1,numclk(ista) ! Run over epochs
             jclock = iuhr + iclstr(ista)
!
! ---------- First handle the polynomials
!
             do iord = 0,4 !RUNNING OVER 0TH THROUGH 4TH ORDER
                if ( kbitp( lclk(jclock), iord+int2(1)).and.kbit(iclsta(1, &
     &               jclock),ista)) then ! Flag is on
                     ncount = ncount + 1
                     if( iuhr.ge.ipoly.and.iuhr.le.istop) then
!
! ---------------------- This is an applicable epoch check to see when
! ---------------------- the next epoch with this order turned on
!
                         iuhrs=iuhr+1
                         jclocks=iuhrs+iclstr(ista)
                         do while(iuhrs.le.istop.and..not. &
     &                      ( kbitp( lclk(jclocks), iord+int2(1)).and. &
     &                        kbit(iclsta(1,jclocks),ista))) ! Check next
                              iuhrs=iuhrs+1
                              jclocks=iuhrs+iclstr(ista)
                         enddo
!
! ---------------------- If this an epoch which appies to this observation
! ---------------------- compute the derivatives taking into accout delay
! ---------------------- or rate
! ---------------------- Note in order to get the rate partial in the units of
! ---------------------- sec/sec we must divide by the number of seconds
! ---------------------- in a day i.e. 86400.
! ---------------------- Also take care of the sign for station 1 and
!
                         if(iuhr.eq.istop.or.iuhrs.gt.istop.or. &
     &                      iord.gt.1.or.iuhr.eq.ipoly) then ! All that's left
!
! ------------------------- The computation of the time parameter is done
! ------------------------- this painful way to preserve precision.
!
                            t = fjd - fjdcl(jclock)
                            t = t + fractc
                            if(np.eq.2) t = t + (dt/8.64d10 - tau_acm/8.64d4)
                            deriv(ncount,1) =  sign*t**iord
                            if ( use_rate ) then
                                 deriv(ncount,2) = (deriv(ncount,1)*iord)/ &
     &                                             (t*86400.d0)
                            end if
                            call f__put_ind ( ncount, f__glo, deriv, place, &
     &                                        b3dobj )
                           else !use the small part
                            t=fjdcl(jclocks)-fjdcl(jclock)
                            deriv(ncount,1)=sign*t**iord
                            deriv(ncount,2)=0.0d0
                            call f__put_ind ( ncount, f__glo, deriv, place, &
     &                                        b3dobj )
                          endif
                      endif
                  endif
              enddo
!
! ----------- See if the diurnal is turned on, but not continued
! ----------- Also if so, put into NSIN the parameter number of the sine
! ----------- derivitive. It will have the last sine parameter number before
! ----------- the standard epoch.
!
              if (         kbitp(lclk(jclock), int2(15))   .and. &
     &             ( .not. kbitp(lclk(jclock), int2(14)) ) .and. &
     &                     kbit(iclsta(1,jclock),ista)           ) then ! This is a new diurnal epoch
                  nsin   = ncount+1
                  ncount = ncount+2
!
! --------------- See if this is the diurnal epoch for this observation
! --------------- Remember that the time arguement for diurnals is the
! --------------- elapsed time since midnight.
!
                  if ( iuhr.eq.idiurn ) then ! Here is diurnal epoc for this obs
                      tsin=fractc
                      if (np.eq.2) tsin=tsin + (dt/8.64d10 - tau_acm/8.64d4)
                      targ = 2.d0*pi__num*tsin
                      sin = dsin(targ)
                      cos = dcos(targ)
!
! ------------------- Calculate sine and cosine partials for delay and rate.
!
                      deriv(nsin   ,1) =  sign*sin
                      if ( use_rate ) then
                           deriv(nsin   ,2) =  sign*cos*(2.d0*pi__num/86400.d0)
                      end if
                      call f__put_ind ( nsin, f__cus, deriv, place, b3dobj )
                      deriv(nsin+1 ,1) =  sign*cos
                      if ( use_rate ) then
                           deriv(nsin+1 ,2) = -sign*sin*(2.d0*pi__num/86400.d0)
                      end if
                      call f__put_ind ( nsin+1, f__nes, deriv, place, &
     &                     b3dobj )
                  endif
              endif
          enddo
        else
!
! ------ The next section of code implements the 1991 year clock
! ------ parameterization scheme for constrained clock parameters.
! ------ In this scheme, the constrained parameters are estimated as offsets
! ------ rather than as rates.  The specified rate constraint is automatically
! ------ converted to an offset constraint by taking into account the lengths
! ------ of the time intervals involved.  This scheme was implemented in
! ------ order to increase processing speed.  MWH  910624
!
          look  = .true.
          iuhr = -1
          idiurn = 0
          ipoly = 0
          istopinc=1
!
          do while(look) !running over clock epochs for this station
              iuhr = iuhr+istopinc
!
! ----------- See if this is the standard epoch for this observation.
! ----------- The standard epoch is found when: (1) the current epoch is the
! ----------- is the last in the list of epochs for this station or (2)
! ----------- or the next epoch is a later time than the time of this
! ----------- observation.
!
              jclock = iuhr+iclstr(ista)
              iuhrs = iuhr+1
              jclocks = iuhrs+iclstr(ista)
              istopinc = 1
              do while (iuhrs.lt.numclk(ista).and..not. &
     &           kbit(iclsta(1,jclocks),ista))
                iuhrs = iuhrs+1
                jclocks = iuhrs+iclstr(ista)
                istopinc = istopinc+1
              enddo
              if(iuhr.eq.numclk(ista)-istopinc.or. &
     &           (tim.lt.fjdcl(jclocks).and. &
     &           kbit(iclsta(1,jclocks),ista)) ) then
!
! -------------- the standard epoch has been found
!
! -------------- Initialize the pointers which point at the first epochs
! -------------- for both polynomials and diurnal for this obs and the
! -------------- last epoch applicable to this observation.
! -------------- Only set IDIURN if the diurnal for this epoch has been set.
!
                 ipoly  = iuhr
                 istop  = iuhr
                 jpoly  = jclock
                 look   = .false.
                 if (jclock.eq.0) jclock = 1
                 if(kbitp( LCLK(JCLOCK), INT2(14)).OR.kbitp( LCLK(JCLOCK), &
     &               INT2(15))) THEN
!
! ------------------ the diurnal is being estimated for this epoch
!
                     IDIURN = IUHR
                     JDIURN = JCLOCK
                     if (idiurn.eq.0) idiurn = 1
                 endif
              endif
          enddo
!
! ------- Now we have got to handle the problem of possible series of
! ------- continued polynomials which apply to this point.
!
          if ( ipoly .gt. 1 ) then
               do while ( ipoly.gt.1 .and. &
     &                    ( kbitp( lclk(jpoly), int2(13) ) .or. &
     &                      .not. kbit(iclsta(1,jpoly),ista)    )  )
!
! ----------------------- it's a continued polynomial clock
!
                          ipoly  = ipoly - 1
                          jpoly  = jpoly - 1
               enddo
          endif
!
! ------- Now handle to problem of a possible series of continued diunal
! ------- epochs.
!
          if(idiurn.gt.1) then
            do while(idiurn.gt.1.and.(KBITP( lclk(jdiurn), int2(14)).or..not. &
     &      kbit(iclsta(1,JDIURN),ISTA)))
!
! ------------- it's a continued diurnal clock
!
                idiurn = idiurn - 1
                jdiurn = jdiurn - 1
            enddo
          endif
!
! ------- That was hard work, but at least we know what epochs apply to this
! ------- observation
!
! ------- Now we have got to compute the clock derivatives for this
! ------- obseration and stick them in the correct slots in the DERIV array.
!
! ------- We must run over all epochs in order to increment NCOUNT
! ------- correctly.
!
! ------- T is the time argument for the polynomials and TSIN is the argument
! ------- for the diurnals.
!
! ------- To be painfully correct we take into account that the time
! ------- applicable to site 2 must be the time tag of the obs plus the
! ------- VLBI delay. We take into accout that DT is is microseconds and
! ------- the time tag is days.
!
          do iuhr = 1,numclk(ista) ! run over epochs
             jclock = iuhr + iclstr(ista)
!
! ---------- First handle the polynomials
!
             do iord = 0,4 !running over 0th through 4th order
                if ( kbitp( lclk(jclock), iord+int2(1)) .and. &
     &               kbit(iclsta(1,jclock),ista) ) then !flag is on
                    ncount = ncount + 1
!
! ----------------- check to see when the next epoch with this order turned on
!
                    iuhrs=iuhr+1
                    jclocks=iuhrs+iclstr(ista)
                    do while(iuhrs.le.NUMCLK(ISTA).and..not. &
     &                 (kbitp( lclk(jclocks), iord+int2(1)).and. &
     &                  kbit(iclsta(1,jclocks),ista))) !check next
                        iuhrs=iuhrs+1
                        jclocks=iuhrs+iclstr(ista)
                    enddo
!
                    if (iord.eq.0.and.istop.gt.0.and. .not.polyonly) then
                      if (iuhr.ge.ipoly) then
                          if (iuhr.eq.istop) then
                            t1 = fjd - fjdcl(jclock)
                            t1 = t1 + fractc
                            if (np.eq.2) t1 = t1 + (DT/8.64D10 - TAU_ACM/8.64D4)
                            t2 = fjdcl(jclocks) - fjdcl(jclock)
                            DERIV(NCOUNT,1) = SIGN*(T2-T1)/T2
                            if ( use_rate ) then
                                 deriv(ncount,2) = -sign/(t2*86400.d0)
                            end if
                            call f__put_ind ( ncount, f__cus, deriv, place, &
     &                           b3dobj )
                            place%clo_seg = jclock  ! iNDEX OF THE SEGMENT
                          else if (iuhr.eq.istop+istopinc) then
                            t1 = fjd - fjdcl(jclock-istopinc)
                            t1 = t1 + fractc
                            if (np.eq.2) t1 = t1 + (dt/8.64d10 - tau_acm/8.64d4)
                            t2 = fjdcl(jclock) - fjdcl(jclock-istopinc)
                            deriv(ncount,1) = sign*(t1/t2)
                            if ( use_rate ) then
                                 deriv(ncount,2) = sign/(t2*86400.d0)
                            end if
                            call f__put_ind ( ncount, f__nes, deriv, place, &
     &                           b3dobj )
                            place%clo_seg = jclock-1  ! iNDEX OF THE SEGMENT
                          endif
                        endif
                      else
                        if(iuhr.ge.ipoly.and.iuhr.le.istop) then
!
! ------------------------- If this an epoch which appies to this observation
! ------------------------- compute the derivatives taking into accout delay
! ------------------------- or rate.
! ------------------------- Note in order to get the rate partial in the units
! ------------------------- of sec/sec we must divide by the number of seconds
! ------------------------- in a day i.e. 86400. Also take care of the sign
! ------------------------- for station 1 and
!
                          if(iuhr.eq.istop.or.iuhrs.gt.istop.or. &
     &                       iord.gt.1.or.iuhr.eq.ipoly) then !all that's left
!
! -------------------------- The computation of the time parameter is done
! -------------------------- this painful way to preserve precision.
!
                              t = fjd - fjdcl(jclock)
                              t = t + fractc
                              if(np.eq.2) t = t + (dt/8.64d10 - tau_acm/8.64d4)
                              deriv(ncount,1) =  sign*t**iord
                              if ( use_rate ) then
                                   deriv(ncount,2) = (deriv(ncount, &
     &                                               1)*iord)/(t*86400.d0)
                              end if
                              call f__put_ind ( ncount, f__glo, deriv, place, &
     &                             b3dobj )
                            ELSE !use the small part
                              t=fjdcl(jclocks)-fjdcl(jclock)
                              deriv(ncount,1)=sign*t**iord
                              deriv(ncount,2)=0.0d0
                              call f__put_ind ( ncount, f__glo, deriv, place, &
     &                             b3dobj )
                          endif
                      endif
                  endif
                endif
              enddo
!
! ----------- See if the diurnal is turned on, but not continued
! ----------- Also if so, put into NSIN the parameter number of the sine
! ----------- derivitive. It will have the last sine parameter number before
! ----------- the standard epoch.
!
              if (    kbitp( lclk(jclock), int2(15))       .and. &
     &             ( .not.kbitp( lclk(jclock), int2(14)) ) .and. &
     &                     kbit(iclsta(1,jclock),ista)          ) then !THIS IS A NEW DIURNAL EPOCH
                  nsin   = ncount+1
                  ncount = ncount+2
!
! --------------- See if this is the diurnal epoch for this observation
! --------------- Remember that the time arguement for diurnals is the
! --------------- elapsed time since midnight.
!
                  if ( iuhr.eq.idiurn ) then ! Here is diurnal epoc for this obs
                      tsin=fractc
                      if (np.eq.2) tsin=tsin + (dt/8.64d10 - tau_acm/8.64d4)
                      targ = 2.d0*pi__num*tsin
                      sin = dsin(targ)
                      cos = dcos(targ)
!
! ------------------- Calculate sine and cosine partials for delay and rate.
!
                      deriv(nsin   ,1) =  sign*sin
                      if ( use_rate ) then
                           deriv(nsin   ,2) =  sign*cos*(2.d0*pi__num/86400.d0)
                      end if
                      call f__put_ind ( nsin, f__cus, deriv, place, b3dobj )
                      deriv(nsin+1 ,1) =  sign*cos
                      if ( use_rate ) then
                           deriv(nsin+1 ,2) = -sign*sin*(2.d0*pi__num/86400.d0)
                      end if
                      call f__put_ind ( nsin+1, f__nes, deriv, place, &
     &                     b3dobj )
                  endif
              endif
          enddo
        endif
      endif         !clocks
!
! === BEGIN ATMOSPHERE SECTION
! ============================
!
      IF ( UNF_ATM   .AND.   NUMATM(ISTA) .GT. 0 ) THEN ! Atmospheres
!
! ------ Here we handle modern (April 1997) code for segmented atmospheres.
!
! ------ DELTA_TIM -- time in days elapsed since the first atmosphere epoch
! ------ NB: 1) order of oprations (not to lose precision)
! ------     2) time eposch of the second station is a bit different
!
         DELTA_TIM = ( FJD - JDATE_ATM(1) ) + FRACTC
         IF ( NP .EQ. 2 ) DELTA_TIM = DELTA_TIM + (DT/8.64D10 - TAU_ACM/8.64D4)
!
! ------ Find the ATM segmentation epoch just prior the this obs
!
         IF ( TIM .GT. JDATE_ATM(PLACE%ATM_SEG+1) ) THEN
              PLACE%ATM_SEG = IDINT ( ( TIM - JDATE_ATM(1) )/ATM_INTERVAL ) + 1
              IF ( PLACE%ATM_SEG .GE. NUM_ATM ) THEN
!
! ---------------- If this observation is a bit after the last epoch we'll
! ---------------- play back. Such a situation is possible since it is allowed
! ---------------- that the last observation of the session follows (a bit)
! ---------------- after the last epoch
!
                   PLACE%ATM_SEG = PLACE%ATM_SEG - 1
              END IF
         END IF
!
         FORWARD_FRACTION = DELTA_TIM/ATM_INTERVAL - (PLACE%ATM_SEG-1)
         BACK_FRACTION    = 1.D0 - FORWARD_FRACTION
!
         NCOUNT_ATM = NCOUNT + PLACE%ATM_SEG
         DERIV(NCOUNT_ATM,1)= AP(NP,1)*BACK_FRACTION
         IF ( USE_RATE ) THEN
              DERIV(NCOUNT_ATM,2)= AP(NP,2)*BACK_FRACTION-AP(NP,1)/ &
     &                             (ATM_INTERVAL*86400.D0)
         END IF
         CALL F__PUT_IND ( NCOUNT_ATM, F__CUS, DERIV, PLACE, B3DOBJ )
!
         NCOUNT_ATM = NCOUNT_ATM + 1
         DERIV(NCOUNT_ATM,1)= AP(NP,1)*FORWARD_FRACTION
         IF ( USE_RATE ) THEN
              DERIV(NCOUNT_ATM,2)= AP(NP,2)*FORWARD_FRACTION+AP(NP,1)/ &
     &                             (ATM_INTERVAL*86400.D0)
         END IF
         CALL F__PUT_IND ( NCOUNT_ATM, F__NES, DERIV, PLACE, B3DOBJ )
!
! ------ Set ncount correctly for the next parameter after this sites atmospheres.
!
         NCOUNT = NCOUNT + NUM_ATM
      ENDIF
!
! --- The following is the very inefficient pre-1997 code.
! --- It is used only for the old,'batch mode' (i.e. non-segmented) cases.
!
      IF  ( .NOT. UNF_ATM   .AND.   NUMATM(ISTA) .GT. 0 ) THEN ! Atmospheres
        noconst = .true.
        do i=iatstr(ista)+1,iatstr(ista)+numatm(ista)
           if ( kbit_latm( int2(1), int2(3), i) ) noconst = .false.
        enddo
        if ( noconst .or. old_atms ) then
!
! ------- Initialization of some variable before cycle
!
          look  = .true.
          iatm  = 0
          ipoly = 0
          jpoly = 0
!
          do while(look) !running over atmosphere epochs
            iatm=iatm+1
            jatm=iatm+iatstr(ista)
            if ( iatm.eq.numatm(ista) .or. jatm .eq. max_atm .or. &
     &           tim.lt.tatm(jatm+1)) then
!
! -------------- The standard epoch has been found
!
                 ipoly = iatm
                 istop = iatm
                 jpoly = jatm
                 look  = .false.
            endif
!
            do while ( ipoly.gt.1 .and. kbit_latm( int2(1), int2(3), jpoly) )
!
! ------------ it's a continued polynomial
!
               ipoly=ipoly-1
               jpoly=jpoly-1
            enddo
          enddo
!
! ------- IATM tracks the epoch number for this station' epochs. JATM tracks t
! ------- flag position in the LATM bit array and the epoch time positon
! ------- in the TATM list.
!
          do iatm = 1,numatm(ista) ! Run over the epochs
            jatm = iatm + iatstr(ista)
            do iaord = 0,1 ! Running over 0th and 1st order
              if( kbit_latm ( int2(1), int2(iaord+1), jatm) ) then ! Flag is on
                  ncount = ncount + 1
                  if ( iatm .ge. ipoly .and. iatm.le.istop ) then
!
! --------------- this is an applicable epoch
!
                  if ( iaord .eq. 0 ) then
                       deriv(ncount,1)=ap(np,1)
                       deriv(ncount,2)=ap(np,2)
                       call f__put_ind ( ncount, f__glo, deriv, place, b3dobj )
                  endif
!
                  if ( iaord.eq.1 .and. iatm.eq.istop ) then
                       t = tim - tatm(jatm)
                       if (np.eq.2) t = t + (dt/8.64d10 - tau_acm/8.64d4)
                       t=t*86400.0d0
                       deriv(ncount,1)=ap(np,1)*t
                       if ( use_rate ) then
                            deriv(ncount,2)=ap(np,2)*t+ap(np,1)
                       end if
                       call f__put_ind ( ncount, f__glo, deriv, place, b3dobj )
                  endif
!
                  if ( iaord.eq.1 .and. iatm.lt.istop ) then
                       t = tatm(jatm+1) - tatm(jatm)
                       t=t*86400.0d0
                       deriv(ncount,1)=ap(np,1)*t
                       if ( use_rate ) then
                            deriv(ncount,2)=ap(np,2)*t
                       end if
                       call f__put_ind ( ncount, f__glo, deriv, place, b3dobj )
                  endif
!
                endif !this is an applicable epoch
              endif !the flag is on
            enddo !running over 0th and 1st order
          enddo !run over the epochs
        else
!
! ------- Initialization of some variable before cycle
!
          look=.true.
          iatm=-1
          ipoly = 0
          jpoly = 0
          do while(look) ! Running over atmosphere epochs
             iatm=iatm+1
             jatm=iatm+iatstr(ista)
             if ( iatm.eq.(numatm(ista)-1) .or. tim.lt.tatm(jatm+1) ) then
!
! --------------- The standard epoch has been found
!
                  ipoly = iatm
                  istop = iatm
                  jpoly = jatm
                  look  =.false.
             endif
!
             do while ( ipoly.gt.1 .and. kbit_latm( int2(1), int2(3), jpoly) )
!
! ------------- It's a continued polynomial
!
                ipoly=ipoly-1
                jpoly=jpoly-1
             enddo
          enddo
!
! ------- IATM tracks the epoch number for this station' epochs. JATM tracks
! ------- flag position in the LATM bit array and the epoch time positon
! ------- in the TATM list.
!
          do iatm = 1,numatm(ista) ! Run over the epochs
            jatm = iatm + iatstr(ista)
            do iaord = 0,1 ! Running over 0th and 1st order
              if ( kbit_latm ( int2(1), int2(iaord+1), jatm) ) then !flag is on
                ncount = ncount + 1
                if(iatm.ge.ipoly) then
!
! --------------- This is an applicable epoch
!
                  if ( istop.eq.0 ) then
                    deriv(ncount,1)=ap(np,1)
                    deriv(ncount,2)=ap(np,2)
                    call f__put_ind ( ncount, f__glo, deriv, place, b3dobj )
                  else
                    if(iatm.eq.istop) then
                      t1 = tim - tatm(jatm)
                      if (np.eq.2) t1 = t1 + (dt/8.64d10 - tau_acm/8.64d4)
                      t1=t1*86400.0d0
                      t2 = (tatm(jatm+1) - tatm(jatm))*86400.0d0
                      deriv(ncount,1)=ap(np,1)*(t2-t1)/t2
                      if ( use_rate ) then
                           deriv(ncount,2)=-ap(np,1)/t2 +ap(np,2)*(t2-t1)/t2
                      end if
                      call f__put_ind ( ncount, f__glo, deriv, place, b3dobj )
                    endif
!
                    if ( iatm .eq. (istop+1) ) then
                      t1 = tim - tatm(jatm-1)
                      if (np.eq.2) t1 = t1 + (dt/8.64d10 - tau_acm/8.64d4)
                      t1=t1*86400.0d0
                      t2 = (tatm(jatm) - tatm(jatm-1))*86400.0d0
                      deriv(ncount,1)=ap(np,1)*t1/t2
                      if ( use_rate ) then
                           deriv(ncount,2)=ap(np,1)/t2 + ap(np,2)*t1/t2
                      end if
                      call f__put_ind ( ncount, f__glo, deriv, place, b3dobj )
                    endif
                  endif
                endif !this is an applicable epoch
              endif !the flag is on
            enddo !running over 0th and 1st order
          enddo !run over the epochs
        endif
      endif        !atmospheres
!
! === END ATMOSPHERE SECTION
!
! --- Test gradient parameters
!
      IF ( NUMGRAD(ISTA) .GT.0  .AND. GRAD_INTERVAL .GT. 1.D-8 ) THEN ! Gradients
         IF ( CALCV .GE. 0.0D0  .AND.  CALCV .LT. 9.0 ) THEN
!
! ----------- Make computations of MTT mapping functions and gradiens if
! ----------- the old version of CALC was used
!
              CALL PLH ( VSITEC(1,ISTA), PHI, ELON, H )
              DEFTEMP=15.D0
              CALL MTTDRY ( ELEV(NP), DEFTEMP, 0.D0, PHI, H, MTT, MTTR, .FALSE. )
!
! ----------- Compute north and east gradient delay and rate partials
!
              GRAD(1) = SIGN * DCOS(AZ(NP))/DTAN(ELEV(NP))*MTT *MM__TO__SEC ! North
              GRAD(2) = SIGN * DSIN(AZ(NP))/DTAN(ELEV(NP))*MTT *MM__TO__SEC   ! East
!
              CALL GRADRAT ( AZ(NP), ELEV(NP), PHI, H, PI__NUM, GRADOT )
              GRADOT(1) = SIGN * GRADOT(1) *MM__TO__SEC
              GRADOT(2) = SIGN * GRADOT(2) *MM__TO__SEC
            ELSE IF ( CALCV .GE. 9.0  .OR.  CALCV .LE. -100.0D0 ) THEN
!
! ----------- Merely copy gradients if the modern version of Calc has been
! ----------- used. NB: Calc computes atmosphere gradients for Niell Mapping
! ----------- functions.
!
              GRAD(1)   = SIGN * AGRAD_PART(NP,1,1) *MM__TO__SEC
              GRAD(2)   = SIGN * AGRAD_PART(NP,2,1) *MM__TO__SEC
              GRADOT(1) = SIGN * AGRAD_PART(NP,1,2) *MM__TO__SEC
              GRADOT(2) = SIGN * AGRAD_PART(NP,2,2) *MM__TO__SEC
          END IF
!
! ------- Compute the gradient segment index
!
          IF ( .NOT. GRAD_AUG2003_BUG ) THEN !
!
! ---------- Modern style
!
             IND_GRAD = (TIM - TGRAD(1))/(GRAD_INTERVAL/24.0D0) + 1
!
! ---------- NB: it may occur that the current time tag is a little bit later
! ---------- than the last time tag due to rounding
!
             IF ( IND_GRAD .GT. NUMGRAD(ISTA)-1 ) IND_GRAD = NUMGRAD(ISTA)-1
             FORWARD_FRACTION = (TIM - TGRAD(1))/(GRAD_INTERVAL/24.0D0) - &
     &                          (IND_GRAD-1)
             IF ( NP.EQ.2 ) FORWARD_FRACTION  = FORWARD_FRACTION  + &
     &                      (DT/8.64D10 - TAU_ACM/8.64D4)/(GRAD_INTERVAL/24.0D0)
             BACK_FRACTION = 1.0D0 - FORWARD_FRACTION
!
! ---------- Put derivatives of atmosphere gradients
!
! ---------- NB: Units!
! ---------- GRAD_INTERVAL is in hours
! ---------- GRAD is in days
!
             DERIV(NPAR_GRAD_STA(ISTA)+IND_GRAD*2-1,1)= GRAD(1)*BACK_FRACTION
             DERIV(NPAR_GRAD_STA(ISTA)+IND_GRAD*2  ,1)= GRAD(2)*BACK_FRACTION
             DERIV(NPAR_GRAD_STA(ISTA)+IND_GRAD*2+1,1)= GRAD(1)*FORWARD_FRACTION
             DERIV(NPAR_GRAD_STA(ISTA)+IND_GRAD*2+2,1)= GRAD(2)*FORWARD_FRACTION
!
             IF ( USE_RATE ) THEN
                DERIV(NPAR_GRAD_STA(ISTA)+IND_GRAD*2-1,2) = &
     &             -GRAD(1)/(GRAD_INTERVAL*3600.0D0) + GRADOT(1)*BACK_FRACTION
                DERIV(NPAR_GRAD_STA(ISTA)+IND_GRAD*2  ,2) = &
     &             -GRAD(2)/(GRAD_INTERVAL*3600.0D0) + GRADOT(2)*BACK_FRACTION
                DERIV(NPAR_GRAD_STA(ISTA)+IND_GRAD*2+1,2) = &
     &              GRAD(1)/(GRAD_INTERVAL*3600.0D0) + GRADOT(1)*FORWARD_FRACTION
                DERIV(NPAR_GRAD_STA(ISTA)+IND_GRAD*2+2,2) = &
     &              GRAD(2)/(GRAD_INTERVAL*3600.0D0) + GRADOT(2)*FORWARD_FRACTION
             END IF
!
             CALL F__PUT_IND ( NPAR_GRAD_STA(ISTA)+IND_GRAD*2-1, F__GLO, &
     &                         DERIV, PLACE, B3DOBJ )
             CALL F__PUT_IND ( NPAR_GRAD_STA(ISTA)+IND_GRAD*2,   F__GLO, &
     &                         DERIV, PLACE, B3DOBJ )
             CALL F__PUT_IND ( NPAR_GRAD_STA(ISTA)+IND_GRAD*2+1, F__GLO, &
     &                         DERIV, PLACE, B3DOBJ )
             CALL F__PUT_IND ( NPAR_GRAD_STA(ISTA)+IND_GRAD*2+2, F__GLO, &
     &                         DERIV, PLACE, B3DOBJ )
          ELSE
!
! ---------- This is the old, pre-AUG2003 style. Besides of sloppiness,
! ---------- slowness it has a bug: if the time tag esceeds the last epoch,
! ---------- it uses the index ofthe first epoch instead of the last epoch.
! ---------- This portion of code is preserved only for testing of Solve in
! ---------- August-September 2003 and later should be removed.
!
             LOOK = .TRUE.
             IATM=-1
             DO WHILE ( LOOK ) !running over atmosphere epochs
                IATM=IATM+1
                JATM=IATM
                IF ( IATM.EQ.(NUMGRAD(ISTA)-1) .OR. TIM.LT.TGRAD(JATM+1) ) THEN
!
! ------------------ The standard epoch has been found
!
                     ISTOP=IATM
                     LOOK=.FALSE.
                ENDIF
             ENDDO
!
! ---------- IATM tracks the epoch number for this station' epochs.
! ---------- JATM tracks to flag position in the LGRAD bit array and the
! ---------- epoch time positon in the TGRAD list.
!
             DO IATM = 1,NUMGRAD(ISTA) ! run over the epochs
                JATM = IATM
                IF ( KBIT( LGRAD, INT2(1) ) ) THEN ! flag is on
                   NCOUNT = NCOUNT + 1
!
! ---------------- This is an applicable epoch
!
                   IF ( ISTOP.EQ.0 ) THEN
                      DERIV(NCOUNT,1) = GRAD(1)
                      DERIV(NCOUNT,2) = GRADOT(1)
                      CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                      NCOUNT = NCOUNT+1
                      DERIV(NCOUNT,1) = GRAD(2)
                      DERIV(NCOUNT,2) = GRADOT(2)
                      CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                    ELSE IF ( IATM.EQ.ISTOP ) THEN
                      T1 = TIM - TGRAD(JATM)
                      IF ( NP.EQ.2 ) T1 = T1 + (DT/8.64D10 - TAU_ACM/8.64D4)
                      T1=T1*86400.0D0
                      T2 = (TGRAD(JATM+1) - TGRAD(JATM))*86400.0d0
                      DELT = (T2-T1)/T2
                      DERIV(NCOUNT,1) = GRAD(1)*DELT
                      IF ( USE_RATE ) THEN
                           DERIV(NCOUNT,2) = ( -GRAD(1)/T2 +GRADOT(1)*DELT )
                      END IF
                      CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                      NCOUNT = NCOUNT+1
                      DERIV(NCOUNT,1) = GRAD(2)*DELT
                      IF ( USE_RATE ) THEN
                           DERIV(NCOUNT,2) = ( -GRAD(2)/T2 + GRADOT(2)*DELT )
                      END IF
                      CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                    ELSE IF ( IATM.EQ.(ISTOP+1) ) THEN
                      T1 = TIM - TGRAD(JATM-1)
                      IF ( NP.EQ.2 ) T1 = T1 + (DT/8.64D10 - TAU_ACM/8.64D4)
                      T1=T1*86400.0D0
                      T2 = (TGRAD(JATM) - TGRAD(JATM-1))*86400.0D0
                      DELT = T1/T2
                      DERIV(NCOUNT,1) = GRAD(1)*DELT
                      IF ( USE_RATE ) THEN
                           DERIV(NCOUNT,2) = ( GRAD(1) +GRADOT(1)*T1 )/T2
                      END IF
                      CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
!C
                      NCOUNT = NCOUNT+1
                      DERIV(NCOUNT,1) = GRAD(2)*DELT
                      IF ( USE_RATE ) THEN
                           DERIV(NCOUNT,2) = ( GRAD(2) +GRADOT(2)*T1 )/T2
                      END IF
                      CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                    ELSE
                      NCOUNT = NCOUNT+1
                ENDIF
             ENDIF ! the flag is on
          ENDDO ! run over the epochs
         END IF ! GRAD_AUG2003_BUG
       ENDIF  ! gradients
 810   CONTINUE ! from the beginning of the cycle
      ENDDO !running over stations 1 and 2
!
! --- Set NCOUNT for radio sources
!
      NCOUNT = NSLAST
!
! --- Test source coordinate flags
!
      IF ( NSOURC .NE. 0 ) THEN !some sources are being estimated
!
! -------- Check whether we have to estimate right ascension and declination
! -------- of this source?
!
           DO IRADEC=1,2 !running over RA and DEC
              IF ( STAR_IND(ISTAR,IRADEC) .GT. 0 ) THEN ! flag is on
                   NCOUNT = NSLAST + STAR_IND(ISTAR,IRADEC)
                   DERIV(NCOUNT,1) = SP(IRADEC,1)
                   IF ( USE_RATE ) THEN
                        DERIV(NCOUNT,2) = SP(IRADEC,2)
                   END IF
                   CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
              END IF
           END DO
!
! -------- Check whether we have to estimate proper motion in right ascension
! -------- and declination of this source
!
           DO IRADEC=1,2 !running over RA and DEC
              IF ( PROP_IND(ISTAR,IRADEC) .GT. 0 ) THEN ! flag is on
                   NCOUNT = NSLAST + PROP_IND(ISTAR,IRADEC)
                   DERIV(NCOUNT,1) = SP(IRADEC,1)*TIM_PRM_SEC
                   IF ( USE_RATE ) THEN
                        DERIV(NCOUNT,2) = SP(IRADEC,2)*TIM_PRM_SEC
                   END IF
                   CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
              END IF
           END DO
      ENDIF
!
! --- Test to see if any remaining derivatives need to be processed
!
      NCOUNT = NSLAST+NSOURC
      IF ( FL_EHEO ) THEN ! Harmonic variations in the Earth rotation
!
! -------- Case of estimation of harmonic variations in Earth's rotation
!
           TIM_ARG = ( (FJD - J2000__JD) + FRACTC)*86400.0D0 - 32.184D0 
           TIM_SINCE_REF = TIM_ARG - &
     &                     ( (MJD_EHEO_REF - J2000__MJD)*86400.0D0 + &
     &                       (TAI_EHEO_REF - 43200.0D0) )
!
! -------- Get partial derivatives with respect to ERM
! -------- NB: Solve uses reverse sign for dTau/dUT1
!
           DER_EROT(1) =  ROTP(2,1)
           DER_EROT(2) =  ROTP(1,1)
           DER_EROT(3) = -ROTP(3,1)/UT1__TO__E3
!
           DO IHEO = 1, L_EHEO ! Running over HEO constituents
              ARG = (HEOSOL(IHEO)%ACCL*TIM_ARG/2.0D0 + &
     &               HEOSOL(IHEO)%FREQ)*TIM_ARG + HEOSOL(IHEO)%PHAS 
              IF ( HEOSOL(IHEO)%FL_EST(HEO__E1E2) ) THEN
!
! ---------------- Circular motion around E1/E2
!
                   NCOUNT = NCOUNT + 1
                   DERIV(NCOUNT,1) =   DER_EROT(1)*DCOS(ARG) &
     &                               + DER_EROT(2)*DSIN(ARG) 
                   CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
!
                   NCOUNT = NCOUNT + 1
                   DERIV(NCOUNT,1) =   DER_EROT(1)*DSIN(ARG) &
     &                               - DER_EROT(2)*DCOS(ARG) 
                   CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
              END IF
!
              IF ( HEOSOL(IHEO)%FL_EST(HEO__E3) ) THEN
!
! ---------------- E3 motion
!
                   NCOUNT = NCOUNT + 1
                   DERIV(NCOUNT,1) = DER_EROT(3)*DCOS(ARG)
                   CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
!
                   NCOUNT = NCOUNT + 1
                   IF ( DABS(HEOSOL(IHEO)%FREQ) > EHEO__FRQ_MIN ) THEN
                        DERIV(NCOUNT,1) = DER_EROT(3)*DSIN(ARG)
                      ELSE 
!
! --------------------- Special case of SIN amplitude for zero frequency for E3
! --------------------- This is the rate of change
!
                        DERIV(NCOUNT,1) = DER_EROT(3)*TIM_SINCE_REF
                   END IF
                   CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
              END IF
!
              IF ( HEOSOL(IHEO)%FL_EST_VEL(HEO__E1E2) ) THEN
!
! ---------------- Rate of change of circular motion around E1/E2
!
                   NCOUNT = NCOUNT + 1
                   DERIV(NCOUNT,1) = (   DER_EROT(1)*DCOS(ARG) &
     &                                 + DER_EROT(2)*DSIN(ARG) ) * TIM_SINCE_REF 
                   CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
!
                   NCOUNT = NCOUNT + 1
                   DERIV(NCOUNT,1) = (  DER_EROT(1)*DSIN(ARG) &
     &                                - DER_EROT(2)*DCOS(ARG) ) * TIM_SINCE_REF 
                   CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
              END IF
!
              IF ( HEOSOL(IHEO)%FL_EST_VEL(HEO__E3) ) THEN
!
! ---------------- Rate of change around E3
!
                   NCOUNT = NCOUNT + 1
                   DERIV(NCOUNT,1) = DER_EROT(3)*DCOS(ARG) * TIM_SINCE_REF 
                   CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
!
                   NCOUNT = NCOUNT + 1
                   DERIV(NCOUNT,1) = DER_EROT(3)*DSIN(ARG) * TIM_SINCE_REF 
                   CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
              END IF
           END DO
      END IF
!
      IF ( FL_EERM ) THEN ! Empricial Earth rotation model
!
! ------ Case of estimation of the Empirical Earth Rotation Model
!
         TIM_ARG = ( (FJD - J2000__JD) + FRACTC)*86400.0D0 - 32.184D0
!
! ------ Get patial derivatives with respect to ERM
! ------ NB: Solve uses reverse sign for dTau/dUT1
!
         DER_EROT(1) =  ROTP(2,1)
         DER_EROT(2) =  ROTP(1,1)
         DER_EROT(3) = -ROTP(3,1)/UT1__TO__E3
!
         DER_EROT_RATE(1) =  ROTP(2,2)
         DER_EROT_RATE(2) =  ROTP(1,2)
         DER_EROT_RATE(3) = -ROTP(3,2)/UT1__TO__E3
!
         DO IERM=1,3 ! over components of the ERM
            IF ( IND_EERM_NOD(IERM) > 0 ) THEN
               KNOT_PIVOT = IND_EERM_NOD(IERM)
               NERM_SKIP = 0
!
! ------------ Check for knot change during the session
!
               IF ( EERM_OVR(IERM) == 1 ) THEN
!
! ----------------- Simple case: one additional node
!
                    IF ( TIM_ARG > EERM%TIM(IND_EERM_NOD(IERM)+EERM_OVR(IERM),IERM) ) THEN
                         KNOT_PIVOT = KNOT_PIVOT + 1
                         NCOUNT = NCOUNT + 1 ! skip parameter before
                       ELSE
                         NERM_SKIP = 1 ! skip parameter after
                    END IF
                  ELSE IF ( EERM_OVR(IERM) > 1 ) THEN
!
! ----------------- General case: more than one additional knot
!
                    KNOT_EXTRA = IXMN8 ( EERM%NKNOTS(IERM), EERM%TIM(1,IERM), &
     &                                   TIM_ARG ) - KNOT_PIVOT
                    IF ( KNOT_EXTRA .EQ. -1 - KNOT_PIVOT ) KNOT_EXTRA = 0
                    IF ( KNOT_EXTRA .EQ. -2 - KNOT_PIVOT ) KNOT_EXTRA = EERM_OVR(IERM)
!
! ----------------- Correct the knot index
!
                    KNOT_PIVOT = KNOT_PIVOT + KNOT_EXTRA
!
! ----------------- Correct parameter index and the number of parameters which
! ----------------- should be skipped after
!
                    NCOUNT = NCOUNT + KNOT_EXTRA
                    NERM_SKIP = EERM_OVR(IERM) - KNOT_EXTRA
               END IF
!
               DO INOD = -EERM%DEGREE(IERM),0
!
! --------------- Compute time delay and delay rate partial derivatives wrt ERM
!
                  NCOUNT = NCOUNT + 1
                  DERIV(NCOUNT,1) = DER_EROT(IERM)* &
     &                                  BSPL_VAL ( EERM%NKNOTS(IERM), &
     &                                             EERM%TIM(1,IERM), &
     &                                             EERM%DEGREE(IERM), &
     &                                             KNOT_PIVOT+INOD, TIM_ARG )
                  IF ( USE_RATE ) THEN
                       DERIV(NCOUNT,2) = DER_EROT_RATE(IERM)* &
     &                                       BSPL_DER ( EERM%NKNOTS(IERM), &
     &                                                  EERM%TIM(1,IERM), &
     &                                                  EERM%DEGREE(IERM), &
     &                                                  KNOT_PIVOT+INOD, &
     &                                                  TIM_ARG )
                  END IF
                  CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
               END DO ! inod
               NCOUNT = NCOUNT + NERM_SKIP
            END IF ! ind_eerm_nod(ierm)
         END DO ! ierm
      END IF ! fl_errm
      IF ( IPSTP > 0 ) THEN ! More partial derivatives
!
! ====
! ==== BEGIN EARTH ORIENTATION SECTION
! ==== ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        DO IEOP = 1,2 ! 1 for pm and 2 for ut1
!
!       EOP_STYLE controls the EOP parameterization.
!
!         = EOP__POLY  --> 'Old Style' in which EOP is parameterized with a low order
!                polynomial spanning the entire session.  This is still the
!                routine style for runs not involving high frequency eop
!                estimation. This includes the common case of 'no eop parameterizton'.
!                If poly style, but no coefficients turned on then nothing estimated.
!
!         = EOP__RATES_AND_SEGS --> Unconstrained gobal rate and piecewise linear function. Offsets
!                at breaks estimated and constraints on the amount of change in adjacent offsets.
!
!         = EOP__SEGS_ONLY --> Same as immediately above, but no global rate/
!
!         = EOP__SINE --> Uconstrained global offset and rate, diurnal sine and cosine, and
!                semidiurnal sine and cosine. (Always 6 parameters.)
!
        IF ( EOP_STYLE(IEOP) .EQ. EOP__RATES_AND_SEGS .OR. &
     &       EOP_STYLE(IEOP) .EQ. EOP__SEGS_ONLY          ) THEN !one of the segmented styles
!
! --------- Find the eop segmentation epoch just prior the this obs
!
            IF ( UNF_EOP ) THEN
!
! -------------- Case of uniform intervals. NB! in uniform case the number of
! -------------- segments and the length of EOP interval may be different from
! -------------- non-uniform case.
!
                 N_EOP = NUM_EOP
                 EOP_INT = EOP_INTERVAL
                 DELTA_TIM = ( FJD - JDATE_EOP(1) ) + FRACTC
                 IF ( TIM .GT. JDATE_EOP(PLACE%EOP_SEG+1) ) THEN
                      PLACE%EOP_SEG = IDINT ( ( TIM - JDATE_EOP(1) )/ &
     &                                EOP_INTERVAL ) + 1
                      IF ( PLACE%EOP_SEG .GE. N_EOP ) THEN
!
! ------------------------ If this observation is a bit after the last epoch
! ------------------------ we'll play back. Such a situation is possible since
! ------------------------ it is allowed that the last observation of the
! ------------------------ session follows (a bit) after the last epoch
!
                           PLACE%EOP_SEG = PLACE%EOP_SEG - 1
                      END IF
                 END IF
              ELSE
!
! -------------- Non-uniform case
!
                 N_EOP   = NROT_A1(IEOP)
                 EOP_INT = ROT_INTERVAL(IEOP)
                 IF ( TIM .GT. (TROT_A1 + PLACE%EOP_SEG*EOP_INT) ) THEN
                      PLACE%EOP_SEG = PLACE%EOP_SEG + 1
                      IF ( PLACE%EOP_SEG .GE. N_EOP ) THEN
                           PLACE%EOP_SEG = PLACE%EOP_SEG - 1
                      END IF
                 END IF
            END IF
!
            DO IXYU = IXYU_START(IEOP),IXYU_STOP(IEOP) ! going over either x&y OR UT1
!
! ----------- Do the segment case global rate, if needed.
!
              IF (EOP_STYLE(IEOP) .EQ. EOP__RATES_AND_SEGS .AND. NROT > 0 ) THEN !global rate on
!
! --------------- Do the global rate
!
                  NCOUNT = NCOUNT+1
                  DERIV(NCOUNT,1) = ROTP(IXYU,1)*(TIM-TROT_A1)
                  IF ( USE_RATE ) THEN
                       DERIV(NCOUNT,2) = ROTP(IXYU,2)*(TIM-TROT_A1) + &
     &                                   ROTP(IXYU,1)/86400.D0
                  END IF
                  IF ( IXYU.EQ.3 ) THEN ! It's UT1, so flip the sense
                       DERIV(NCOUNT,1) = -DERIV(NCOUNT,1)
                       DERIV(NCOUNT,2) = -DERIV(NCOUNT,2)
                  ENDIF ! It's UT1, so flip the sense
!
                  CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
              ENDIF ! global rate on
!
! ----------- Segments!
!
              NCOUNT_EOP = NCOUNT + PLACE%EOP_SEG
!
! ----------- Calculate derivatives. To do it first calculate backward and
! ----------- forward fractions.
!
              IF ( UNF_EOP ) THEN
!
! -------------- Use array JDATE_EOP in uniform case.
!
                 FORWARD_FRACTION = DELTA_TIM/EOP_INT - (PLACE%EOP_SEG-1)
                 BACK_FRACTION    = 1.D0 - FORWARD_FRACTION
                ELSE
!
! -------------- non-uniform case
!
                 BACK_FRACTION    &
     &               =( (TROT_A1 + PLACE%EOP_SEG*EOP_INT)      - TIM ) /EOP_INT
                 FORWARD_FRACTION &
     &               =(  TIM - ((PLACE%EOP_SEG-1)*EOP_INT + TROT_A1) ) /EOP_INT
              END IF
!
! ----------- Derivatives for backward fraction
!
              DERIV(NCOUNT_EOP,1) = ROTP(IXYU,1)*BACK_FRACTION
              IF ( USE_RATE ) THEN
                   DERIV(NCOUNT_EOP,2) = ROTP(IXYU,2)* &
     &                   BACK_FRACTION-ROTP(IXYU,1)/(EOP_INT*86400.D0)
              END IF
              IF ( IXYU.EQ.3 ) THEN ! UT1, so we have to flip the sense.
                   DERIV(NCOUNT_EOP,1)= -DERIV(NCOUNT_EOP,1)
                   DERIV(NCOUNT_EOP,2)= -DERIV(NCOUNT_EOP,2)
              ENDIF
!
              CALL F__PUT_IND ( NCOUNT_EOP, F__CUS, DERIV, PLACE, B3DOBJ )
!
! ----------- Do the derivatives for the forward epoch
!
              NCOUNT_EOP = NCOUNT_EOP+1
              DERIV(NCOUNT_EOP,1) = ROTP(IXYU,1)*FORWARD_FRACTION
              IF ( USE_RATE ) THEN
                   DERIV(NCOUNT_EOP,2) = ROTP(IXYU,2)* &
     &                   FORWARD_FRACTION+ROTP(IXYU,1)/(EOP_INT*86400.D0)
              END IF
              IF ( IXYU.EQ.3 ) THEN ! UT1, so we have to flip the sense.
                   DERIV(NCOUNT_EOP,1)= -DERIV(NCOUNT_EOP,1)
                   DERIV(NCOUNT_EOP,2)= -DERIV(NCOUNT_EOP,2)
              ENDIF
              CALL F__PUT_IND ( NCOUNT_EOP, F__NES, DERIV, PLACE, B3DOBJ )
!
! ----------- Update NCOUNT to get past all the partials for this EOP component
!
              NCOUNT = NCOUNT + NROT_A1(IEOP)
            ENDDO ! Going over either x&y OR UT1
          ENDIF ! One of the segmented styles
!
          IF ( EOP_STYLE(IEOP) .EQ. EOP__SINE .AND. NROT > 0 ) THEN ! sine wave parameterization
              DO IXYU = IXYU_START(IEOP),IXYU_STOP(IEOP) ! Going over either x&y OR UT1
!
! -------------- Offset derivaives.
!
                 NCOUNT = NCOUNT + 1
                 DO I = 1,IRATE  !Derivatives of delay, rate wrt offset
                    DERIV(NCOUNT,I) = ROTP(IXYU,I)
                    IF ( IXYU.EQ.3 ) THEN
                         DERIV(NCOUNT,I)= -DERIV(NCOUNT,I)
                    END IF
                 END DO
                 CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
!
! -------------- Global rate derivatives.
!
                 NCOUNT = NCOUNT + 1
                 DO I = 1,IRATE  !Derivatives of delay, rate wrt global rate
                    DERIV(NCOUNT,I) = ROTP(IXYU,I) * (TIM - TROT_A1)
                    IF ( I .EQ. 2 ) then
                         DERIV(NCOUNT,2) = DERIV(NCOUNT,2)+ROTP(IXYU,1)/86400.D0
                    END IF
                    IF (IXYU.EQ.3) DERIV(NCOUNT,I)= -DERIV(NCOUNT,I)
                 END DO
                 CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
!
! -------------- Do diurnal sine wave, if it's being estimated.  Rather than
! -------------- estimate the amplitude and phase offset of the sine directly,
! -------------- treat the diurnal sine as the sum of a sine and cosine and
! -------------- estimate their amplitudes. Then do semi-diurnal.
!
                 IF ( KFIRST ) THEN
                      SAVFJD=FJD
                      KFIRST=.FALSE.
                 ENDIF
                 TRACE_CONVERT =(PI__NUM*1.0D12)/(180.0D0*3600.0D0*1.D3)
!
                 DO IDS = 1,2  ! Diurnal sine, then semi-diurnal sine
                    IF ( IDS .EQ. 1 ) DAYS_PER_CYCLE = 1.0D0
                    IF ( IDS .EQ. 2 ) DAYS_PER_CYCLE = 0.5D0
!
                    NCOUNT = NCOUNT + 1  ! Sine component
                    DO I = 1,IRATE ! Delay and rate
!
! -------------------- Use midnight rather than start of the experiment
! -------------------- for the starting epoch
!
                       TARG=(FJD-SAVFJD+FRACTC-1)*2.0D0 * PI__NUM /DAYS_PER_CYCLE
                       DERIV(NCOUNT,I) = ROTP(IXYU,I)*DSIN(TARG)
                       IF ( I .EQ. 2 ) DERIV(NCOUNT,2) =DERIV(NCOUNT,2)+ &
     &                      (2.0D0 * PI__NUM /DAYS_PER_CYCLE)*DCOS(TARG)* &
     &                      ROTP(IXYU,1)/86400.D0
                       IF (IXYU.EQ.3) DERIV(NCOUNT,I)= -DERIV(NCOUNT,I)
                    END DO
                    CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
!
                    NCOUNT = NCOUNT + 1  !cosine component
                    DO I = 1,IRATE !delay and rate
                       TARG=(FJD-SAVFJD+FRACTC-1)*2.0D0 * PI__NUM /DAYS_PER_CYCLE
                       DERIV(NCOUNT,I) = ROTP(IXYU,I)*DCOS(TARG)
                       IF ( I .EQ. 2 ) DERIV(NCOUNT,2)= DERIV(NCOUNT,2)+ &
     &                      (2.0D0 * PI__NUM /DAYS_PER_CYCLE)*(-DSIN(TARG))* &
     &                      ROTP(IXYU,1)/86400.d0
                       IF (IXYU.EQ.3) DERIV(NCOUNT,I)= -DERIV(NCOUNT,I)
                    END DO
                    CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                 END DO  !diurnal sine, then semi-diurnal sine
              END DO !loop over x&y or UT1
          ENDIF ! Sine wave parameterization
!
          IF ( EOP_STYLE(IEOP) .EQ. EOP__POLY .AND. NROT > 0 ) THEN ! Polynomial parameterization
!
! ---------- 'Old Style' (LOW POLYNOMIAL CASE) in which epochs are specified
! ---------- individually and low order polynomials are used.
!
             DO IXYU =IXYU_START(IEOP), IXYU_STOP(IEOP) ! Run over x&y or UT1
                LOOK = .TRUE.
                DO NRT = 1,NROT ! Running over the number of epochs
!
! ---------------- LOOK is set to true until the correct epoch for this obs
! ---------------- is found.
! ---------------- IOK is set to true only when the correct epoch for this obs
! ---------------- is being processed.
!
                   IOK = .FALSE.
                   IF ( LOOK .AND. (NRT.EQ.NROT .OR. TIM.LT.TROT(NRT+1)) ) THEN
!
! --------------------- this is the correct epoch for this obs
!
                        LOOK = .FALSE.
                        IOK   = .TRUE.
                        T = TIM - TROT(NRT)
                   ENDIF
!
! ---------------- Proccess the flags for this epoch regardless of whether this
! ---------------- is the correct epoch for this obs - NCOUNT must be correctly
! ---------------- incremented.  Note that to do the actual bit testing the
! ---------------- subroutine IROTT is used. See that routine in &SUTI2 for
! ---------------- further documentation.
!
                   DO IORD = 0,3 ! Running over 0th to 3rd order polynomials
                      IF ( IROTT( NRT, IXYU, INT2(IORD+1), LROT) .EQ. 1 ) THEN ! Flag is on
                           NCOUNT = NCOUNT+1
!
! ------------------------ If this is epoch for this obs, compute derivative
! ------------------------ Flip the sign of UT1 partial to UT1-TAI not TAI-UT1
!
                           IF ( IOK ) THEN ! it's estimated
                                DO I=1,IRATE
                                   DERIV(NCOUNT,I) = ROTP(IXYU,I)*T**(IORD)
                                   IF ( I .EQ. 2 .AND. T.NE.0 ) THEN
                                        DERIV(NCOUNT,2) = &
     &                                        DERIV(NCOUNT,2)+ROTP(IXYU,1)*IORD*T**(IORD-1)/ &
     &                                        86400.D0
                                   END IF
                                   IF ( IXYU.EQ.3 ) DERIV(NCOUNT,I)= -DERIV(NCOUNT,I)
                                ENDDO
                                CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, &
     &                                            B3DOBJ )
                           ENDIF !estimated
                      ENDIF ! flag on
                   ENDDO ! running over orders
                ENDDO ! running over epochs
             ENDDO ! running over x&y or ut1
          END IF ! Polynomial parameterization
        END DO ! run over polar motion,UT1 to determine parameterization style
!
! ===== END EOP SECTION
! ===== END EARTH ORIENTATION SECTION
!
! ===== BEGIN RELATIVITY SECTION
!
! ------ Test relativity flag
!
        IF ( LREL .NE. 0 ) THEN !relativity is estimated
             NCOUNT = NCOUNT + 1
             DO I=1,IRATE
                DERIV(NCOUNT,I) = RELP(I)
             ENDDO
             CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
        ENDIF
!
!   Test earth tide parameters
!   IF ITDGLB = 1 Then the parameters are site dependent
!   and must be sorted accordingly
!
!   WARNING: In January 1990 a more-than-10-year-old bug was
!   found. SOLVE internally keeps earth tide information
!   in the order l,h,lag.  CALC puts out the partials in the
!   order h, l, lag.  The CALC documetation which explains this
!   order was jumbled going to at least CALC-5 in 1981. The
!   CALC-7 documentation is cleaned up. The testing below on the
!   index J resolve this confusion. JWR
!
          IF ( ITDGLB .NE. 0 ) THEN !handle global case
               CALL FERR ( INT2(611), &
     &             "PARTL: bad news: Love numbers estimation "// &
     &             " is not supported any more since 2000.06.13. "// &
     &             "Use user partials for this purpose", INT2(0), INT2(0) )
          ENDIF
!
! ------- Test the precession constant parameter
!
          IF ( LPREC.NE.0) THEN ! Precession is estimated
               NCOUNT = NCOUNT +1
               DO I=1,IRATE
                  DERIV(NCOUNT,I) = PRCP(I)
               ENDDO
               CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
          ENDIF
!
! ------- Check for daily nutation estimate
!
          DO INUT=1,2 !running over dpse and deps
             IF ( KBITP ( LNUT(1), INUT ) ) THEN ! Parameter is estimated
                  NCOUNT = NCOUNT+1
                  DERIV(NCOUNT,1) = NUT_FCT(INUT)*NUTP(INUT,1)
                  IF ( USE_RATE ) THEN
                       DERIV(NCOUNT,2) = NUT_FCT(INUT)*NUTP(INUT,2)
                  END IF
                  CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
              ENDIF
          ENDDO
!
! ------- Handle nutation time series parials.
! ------- It would be meaningless to estimate both nutation offset and
! ------- time series parameters.  SETFL makes certain they are not both]
! ------- turned on.
! ------- Note that there is only one flag bit for each pair of in-phase
! ------- and out-of-phase coefficients. They are estimated in pairs.
!
          IF ( LNUT(2).NE.0  .OR.  LNUT(3).NE.0 ) THEN
               CALL FERR ( INT2(612), "PARTL: bad news: nutation model "// &
     &             "mapping is not supported after evening 2000.06.13", &
     &              INT2(0), INT2(0) )
          ENDIF
!
          IF ( NFLPSI .GT. 0  .OR. NFLEPS .GT. 0 )  THEN
               CALL FERR ( INT2(613), &
     &             "PARTL: bad news: nutation model mapping is not "// &
     &             "supported after evening 2000.06.13", INT2(0), INT2(0) )
          END IF
!
! ------- Test the baseline-dependent clocks
!
! ------- The two outer loops below run over the list of all possible
! ------- baseline for the stations in the master station list.
! ------- The array ICLOCK is a two dimensional bit array where each
! ------- bit corresponds to a baseline.  We must look over all baselines
! ------- and increment NCOUNT correctly.  Then compute the baseline dependent
! ------- for this observation, if neccessary.  Note that the rate derivatives
! ------- baseline dependent clocks are always zero.
!
          IF ( LOGBCL ) THEN ! There are baseline dependent clocks estimated
               IF ( BASCL_IND( ISITE(1), ISITE(2) ) .GT. 0 ) THEN
                    NCOUNT = NCOUNT + BASCL_IND( ISITE(1), ISITE(2) )
                    DERIV(NCOUNT,1) = 1.0D0
                    CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
               END IF
               IF ( BASCL_IND( ISITE(2), ISITE(1) ) .GT. 0 ) THEN
                    NCOUNT = NCOUNT + BASCL_IND( ISITE(2), ISITE(1) )
                    DERIV(NCOUNT,1) = -1.0D0
                    CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
               END IF
          ENDIF
!
          NCOUNT = NPARAM_AFTER_BASCL
          IF ( IOS_EST .NE. IOS__UNDF ) THEN
               IF ( .NOT. IS_R8_NAN(DOBS) .AND. .NOT. IS_R8_NAN(DOBSXS) ) THEN
                    DIF_DEL = (DOBS - DOBSXS)/1.0D6
                    IF ( DABS(DIF_DEL) > 1.D-6 ) THEN
                         DIF_DEL = 0.0D0
                    END IF
                  ELSE
                    DIF_DEL = 0.0D0
               END IF
          END IF
!
          IF ( IOS_EST == IOS__SES ) THEN
               NCOUNT = NCOUNT + 1
               DERIV(NCOUNT,1) = -DIF_DEL
               CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
             ELSE IF ( IOS_EST == IOS__STA ) THEN
               NCOUNT_BSL = NCOUNT
               KSTA = 0                
               DO 420 J2=1,NUMSTA
                  IF ( CHECK_STABIT ( INT2(J2) ) ) THEN
                       KSTA = KSTA + 1
                       IF ( ISITE(1) == J2 ) THEN
                            DERIV(NCOUNT_BSL + KSTA,1) = -DIF_DEL
                            CALL F__PUT_IND ( NCOUNT_BSL + KSTA, F__GLO, DERIV, PLACE, B3DOBJ )
                       END IF
                       IF ( ISITE(2) == J2 ) THEN
                            DERIV(NCOUNT_BSL + KSTA,1) = -DIF_DEL
                            CALL F__PUT_IND ( NCOUNT_BSL + KSTA, F__GLO, DERIV, PLACE, B3DOBJ )
                       END IF
                  END IF
 420           CONTINUE 
               NCOUNT = NCOUNT_BSL + KSTA
             ELSE IF ( IOS_EST == IOS__BAS ) THEN
               NCOUNT_BSL = NCOUNT
               KBAS = 0                
               DO 430 J3=1,NUMSTA
!
! --------------- Check STABIT bit fields to bypass deselected station
!
                  IF ( CHECK_STABIT ( INT2(J3) ) ) THEN
                       IP = J3 + 1
                       DO 440 J4=IP,NUMSTA
!
! ----------------------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
                          IF ( CHECK_STABIT ( INT2(J4) ) ) THEN
                               IF ( KBIT(ICLOCK(1,J3),INT2(J4)) .OR. KBIT(ICLOCK(1,J4),INT2(J3)) ) THEN
                                    KBAS = KBAS + 1
                                    IF ( ISITE(1) == J3 .AND. ISITE(2) == J4 ) THEN
                                         DERIV(NCOUNT_BSL+KBAS,1) = -DIF_DEL
                                         CALL F__PUT_IND ( NCOUNT_BSL+KBAS, F__GLO, DERIV, PLACE, B3DOBJ )
                                    END IF 
                                    IF ( ISITE(2) == J3 .AND. ISITE(1) == J4 ) THEN
                                         DERIV(NCOUNT_BSL+KBAS,1) = -DIF_DEL
                                         CALL F__PUT_IND ( NCOUNT_BSL+KBAS, F__GLO, DERIV, PLACE, B3DOBJ )
                                    END IF 
                               END IF
                          END IF
 440                   CONTINUE
                  END IF
 430           CONTINUE
               NCOUNT = NCOUNT_BSL + KBAS
          END IF
!
! ------- Handle tidal components for high frequency eop here
!
          IF ( KHFEOP.EQ.2 .OR. KHFEOP.EQ.3 ) THEN
               CALL TIDEPART(PART_UT1XY )
               DO I=1,NUM_SDE_UT1+NUM_SDE_XY
                  DO J = 1,2
                     NCOUNT = NCOUNT+1
                     DO K=1,2
                        DERIV(NCOUNT,K) = PART_UT1XY(K,J,I)
                     ENDDO
                     CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                  ENDDO
               ENDDO
          ENDIF
!
      IF ( SOU_ADM_FLAG .NE.  SOUADM__NO ) THEN
!
! ------ Count the number of parameters for source structure admittance
!
         DO J=1,NUMSTR ! running over the sources
!
! --------- Check, whether the J-th source was selected
!
!@              IF ( SUPMET == SUPMET__META ) THEN
!@                   FL_SOU_USE = BTEST ( SOU_USE(J), INT4(IDATYP) )
!@                ELSE 
                   FL_SOU_USE = KBIT ( ISRSEL(1), J ) 
!@              END IF
            IF ( FL_SOU_USE ) THEN
!
! -------------- Yes, it was selected
!
                 IF ( SOU_ADM_FLAG == SOUADM__GLB_ALL .OR. &
     &                SOU_ADM_FLAG == SOUADM__LCL_ALL      ) THEN
!
! ------------------- Admittance is computed for every sources.
!
                      NCOUNT = NCOUNT +1
                      DERIV(NCOUNT,1) = STRUC_DEL
                      CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                      GOTO 860
                    ELSE 
!
! ------------------- Admittance is computed for a list of sources
!
                      IF ( IND_SOU_ADM(1) > 0 ) THEN
!
! ------------------------ Search the J-th sources in the list
!
                           IP = LTM_DIF ( 1, IND_SOU_ADM(2)-IND_SOU_ADM(1)+1, &
     &                                    SRCSUP(IND_SOU_ADM(1)), &
     &                                    ISTRN_CHR(J) )
                           IF ( IP > 0 ) THEN
!
! ----------------------------- The J-th source was found in the list
!
                                IF ( SOU_ADM_FLAG == SOUADM__LCL_LIST_NO .OR. &
     &                               SOU_ADM_FLAG == SOUADM__GLB_LIST_NO ) THEN
!
                                     NCOUNT = NCOUNT +1
                                     IF ( J == ISTAR ) THEN
                                          DERIV(NCOUNT,1) = STRUC_DEL
                                          CALL F__PUT_IND ( NCOUNT, F__GLO, &
     &                                                      DERIV, PLACE, B3DOBJ )
                                     END IF
                                END IF
                              ELSE 
                                IF ( SOU_ADM_FLAG == SOUADM__LCL_LIST_YES .OR. &
     &                               SOU_ADM_FLAG == SOUADM__GLB_LIST_YES      ) THEN
!  
                                     NCOUNT = NCOUNT +1
                                     IF ( J == ISTAR ) THEN
                                          DERIV(NCOUNT,1) = STRUC_DEL
                                          CALL F__PUT_IND ( NCOUNT, F__GLO, &
     &                                                      DERIV, PLACE, B3DOBJ )
                                     END IF
                                END IF
                           END IF 
                         ELSE IF ( IND_SOU_ADM(1) == 0  .AND. &
     &                             ( SOU_ADM_FLAG == SOUADM__GLB_LIST_NO .OR. &
     &                               SOU_ADM_FLAG == SOUADM__LCL_LIST_NO ) ) THEN
                           NCOUNT = NCOUNT +1
                           IF ( J == ISTAR ) THEN
                                DERIV(NCOUNT,1) = STRUC_DEL
                                CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, &
     &                                            PLACE, B3DOBJ )
                           END IF
                      END IF ! Exception list
                    END IF ! Admittance flag
                 END IF ! fl_sou_use
             ENDDO ! sources
 860         CONTINUE 
          END IF ! Source admittance
!
! ------- Handle user-defined parameters (from USER_PARTIALS option)
!
      IF ( KUSER_PART  .AND.  ( UPT_FLAG .EQ. UPT__CMP ) .AND. &
     &     ( ARC_USER_PART > 0 .OR.  NUM_USER_PART > 0 )       ) THEN
               NPAR_NONUSER = NCOUNT
               DO 450 J5=1,SKIP_COUNT+1
                  IUER = -1
                  CALL RDBIN_ARRAY ( FILDES, 'I4', 2, I4_ARR, IEL, IUER )
                  IF ( IUER .NE. 0 ) THEN
                       CALL ERR_LOG ( 3281, -1, 'PARTL', 'Error '// &
     &                     'in reading the user partials file' )
                       RETURN
                  END IF
!
                  IOBS = I4_ARR(1)
                  NUPT = I4_ARR(2) ! Number of user parameters
                  IF ( NUPT .GT. 0 ) THEN
                       IUER = -1
                       CALL RDBIN_ARRAY ( FILDES, 'I4', NUPT, IND_PT, &
     &                                    IEL, IUER )
                       IF ( IUER .NE. 0 ) THEN
                            CALL ERR_LOG ( 3282, -1, 'PARTL', &
     &                          'Error in reading the user partials file' )
                            RETURN
                       END IF
!
                       IUER = -1
                       CALL RDBIN_ARRAY ( FILDES, 'R8', NUPT, &
     &                                    USER_DERIV, IEL, IUER )
                       IF ( IUER .NE. 0 ) THEN
                            CALL ERR_LOG ( 3283, -1, 'PARTL', &
     &                          'Error in reading the user partials file' )
                            RETURN
                       END IF
                     ELSE
                       CONTINUE 
                  END IF
 450           CONTINUE
!
               IF ( NUPT .GT. 0 ) THEN
                    DO 460 J6=1,NUPT
                       NCOUNT = NCOUNT+1
                       DERIV(NPAR_NONUSER+IND_PT(J6),1) = USER_DERIV(J6)
                       CALL F__PUT_IND ( NPAR_NONUSER+IND_PT(J6), &
     &                      F__GLO, DERIV, PLACE, B3DOBJ )
 460                CONTINUE
               END IF
               SKIP_COUNT = 0
            ELSE  IF ( KUSER_PART  .AND.  .NOT.( UPT_FLAG .EQ. UPT__CMP ) .AND. &
     &                 ARC_USER_PART > 0 .OR.  NUM_USER_PART > 0                ) THEN
!
! ------------ NB: records are twice shorter if NORATE_FLAG is .TRUE.
!
               IF ( NORATE_FLAG ) THEN
                    JBYTES = INT4(ARC_USER_PART)*8
                  ELSE
                    JBYTES = INT4(ARC_USER_PART)*16
               END IF
               JBLOCKS = (JBYTES+255)/256
!
! ------------ Reading USER-PARTIALS from the file
!
               DO WHILE ( SKIP_COUNT .GT. 0 )
                  CALL BIN_READ ( FNAME, FILDES, USER_DERIV, JBLOCKS )
                  SKIP_COUNT = SKIP_COUNT-1
               ENDDO
               CALL BIN_READ ( FNAME, FILDES, USER_DERIV, JBLOCKS )
!
               DO I=1,ARC_USER_PART
                  NCOUNT = NCOUNT+1
                  IF ( NORATE_FLAG  .OR.  UPT_FLAG .EQ. UPT__DEL ) THEN
                       ND = I
                       DERIV(NCOUNT,1) = USER_DERIV(ND)
                     ELSE
                       ND = (I-1)*2+1
                       DERIV(NCOUNT,1) = USER_DERIV(ND)
                       DERIV(NCOUNT,2) = USER_DERIV(ND+1)
                  END IF
!
                  IF ( DABS( USER_DERIV(ND) ) .GT. 1.D-30 ) THEN
!
! -------------------- Since we don't know location of zeroes in equation of
! -------------------- conditions for user partials we should use such a crude
! -------------------- trick
!
                       CALL F__PUT_IND ( NCOUNT, F__GLO, DERIV, PLACE, B3DOBJ )
                   END IF
               ENDDO
          ENDIF
      ENDIF  !  more derivatives to be processed.
!
      RETURN
      END  !#!  PARTL  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION  KBITP ( IARG1, IARG2 )
      INTEGER*2 IARG1, IARG2
      LOGICAL*2 KBITP
      KBITP = BTEST ( IARG1, IARG2-INT2(1) )
      RETURN
      END  !#!  KBITP  #!#
