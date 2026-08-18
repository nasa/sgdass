      SUBROUTINE SECND ( IFIRST, INUMBER, IFIRSTC, INUMBERC, IFIRST_EOP, &
     &                   INUMBER_EOP )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Generate atmosphere, clock and earth orientation values for
!     this data base, whether we intend to plot them or not.
!
!     Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
!     INPUT Variables:
      INTEGER*2 IFIRST(*),INUMBER(*),IFIRSTC(*),INUMBERC(*), &
     &          IFIRST_EOP(*),INUMBER_EOP(*)
!
!     IFIRST - First atmosphere parameter to use
!     INUMBER - Array of atmosphere parameters
!     IFIRSTC - First clock parameter to use
!     INUMBERC - Array of clock parameters
!     IFIRST_EOP - First earth orientation parameter to use
!     INUMBER_EOP - Array of earth orientation parameters
!
!     OUTPUT Variables: None
!
!     COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'oborg.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'mdlcm.i'
      INCLUDE 'hold_atm.i'
      INCLUDE 'glbc4.i'
!
!
!  .  LOCAL VARIABLES
      REAL*8    APP(2,2), AVG_ATM(4,MAX_ARC_STA)
      INTEGER*2 J
      INTEGER*4 IOS
      INTEGER*2 ITT(MAX_ARC_STA),ITTB(MAX_ARC_BSL)
      INTEGER*2 JCAVAL(MAX_ARC_STA), OBCAPL, MCAPL, JCAPPL(MAX_ARC_STA)
      INTEGER*2 JCAFFL(7,MAX_ARC_STA),NAMSTA,NFCAL
      CHARACTER*8 FCAL_NAMES(112)
      character*80 bufstr
      INTEGER*2 JSITI(MAX_ARC_STA),JSITN(4,MAX_ARC_STA)
      INTEGER*2 IY,IM,ID,IHR,IMIN,IDB,I
      INTEGER*4 IOBS, NOBS, IDBGN, IDEND
      INTEGER*2 NOGOOD, II,AX_TYPES(MAX_ARC_STA)
      LOGICAL*2 KBIT,TOL
      REAL*8    ET(2,MAX_ARC_BSL), BARO_CALS(MAX_ARC_STA), SE(MAX_ARC_STA), &
     &          SS(MAX_ARC_SRC)
      REAL*8 BARO_HEIGHTS(MAX_ARC_STA)
!
      LOGICAL*2 KSTAT(MAX_ARC_STA)
      REAL*8 CFJD, CFRACT, AX_OFFS(MAX_ARC_STA)
!
      REAL*8 DERIV(M_GPA,2), DERR_RAW, RERR_RAW, DPHER_RAW
      COMMON / DERCM / DERIV
      REAL*8 LATS(MAX_ARC_STA),HEIGHTS(MAX_ARC_STA), FJ
      INTEGER*4 iblas1
      REAL*8 CNVRT_EOP(3)
      LOGICAL*2 KEOSTAT
      INTEGER*2 IEOP
      CHARACTER*90 errstr
      INTEGER*4  I4P0
      CHARACTER  FNAME*(NAME_SIZE), STR*54, GET_VERSION*54
      integer*4 fildes
      integer*2 trimlen
      integer*2 num_model,ict
      character*200 buf200
      LOGICAL*4     DATYP_INQ
      COMMON /PARTFILE/ fname,fildes
!
      INCLUDE  'fast.i'
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
!
      DATA I4P0 /0/
      DATA KSTAT/MAX_ARC_STA*.FALSE./,CFJD/0.0D0/,CFRACT/0.0D0/
      INTEGER*2 INT2_ARG
!
! 4.  HISTORY
!  WHO   WHEN    WHAT
!  MWH   900416  Modified to accommodate Lanyi model
!  AEE   910515  Enhanced error messages written to the error file.
!  :91.08.01:jwr: UT1 adjustment converted to microseconds
!  :92.12.16:jlr: replaced 0J with I4P0
!  :93.12.22:mwh: Handle clock breaks appropriately
!  :94.12.09:jwr: Bug in clock break logic fixed. Entailed a major
!                 modification of clock model code. Documentation added.
!  :95.05.11:jwr: Evalution of John Gipson's hf eop model done with eop estimates.
!  :95.07.17:jwr: Better tracking of hfeop model status. Pole units
!                 changed to micro-arc-seconds
!  :95.07.17:jwr: Call to PARTL moved to after the call to ATMPART because
!                 the correct atm partial derivitive is set in ATMPART. (Sleeper.)
!  :95.08.09:kdb: Ability to deal with high frequency eop models other than hf926.
!                 Handle two hf eop models, one for application to the solution
!                 total estimates and one for plotting for comparison.
!                 (Previously one model was used for both functions.)
!  :95.08.31:kdb: Change avg_atm argument to socal (for addition of atmospheric
!                 turbulence rate and mapping function parameter error
!                 delay and rate constants to cres and proc socal calls.)
!   kdb  951207   Integer*4 number of observations.
!   pet  971104   Changed the format of reading EOPPxx file in according with
!                 changes in ADJST
!   pet  971124   Accomodated changes in the set of formal parameters for NCORT
!   pet  980203   Substituted hard-coded test of solution type by DATYP_INQ
!   pet  990421   Added a call of FLYBY_MAP_INIT
!   pet  1999.11.17   Updated the list of actual parameters for NCORT
!
!  Some notes on how the clock logic works.
!
!  Clocks with the continuous, piecewise continous method are modeled
!  as follows:
!
!  1) At the time of the first observation there is an initial
!  2nd order polynomial with no constraints.
!
!  2) Then there are epoch sat even intervals specified by the analysist.
!  At each epoch there is a constrained offset parameter. The size of
!  the constraint is set by the size of the clock rate constraint set
!  by the user and the length of the interval. For example, if the constraint
!  is 5e-14 then and interval is 30 minutes, then the offset constraint
!  is 5e-14 time s 30 minutes (taking into account units).
!  The clock model between the epoch consists of an linear interpolation
!  of the epoch values on either side plus the 1st and 2nd order terms
!  in the polynomial.
!
!  3) When a epoch break is requested then each interval bound by the epochs
!  is treated seperately as above. When an offset epoch is introduced an
!  additional epoch is introduced a fraction of a second earlier. Its
!  job is to act as the last epoch offset of the first interval.
!
!  There are 3 arrays which keep most of the information and which
!  work in parallel. The arrays have one entry each for each succesive
!  clock epoch.
!
!  1 - lclk(max_clk) - an array of single bit clock flags which is
!                      16 bits wide by max_clk long.
!
!                      Bits 1 to 12 flag whether a corresponding polynomial
!                      order is estimated or not.
!                      Bit 1 on = estimate clock offset    (0th order on)
!                      Bit 2 on = estimate clock drift     (1st order on)
!                      Bit 3 on = estimate frequency drift (2nd order on)
!                      Bit n on = estimate the (n-1)th order term
!
!                      Bit 13 on indicates that the clock model should be
!                      continous over this epoch, that is, no 'clock break'.
!
!  2 - iclsta(max_clk) - an array of single bit clock flags which is 16 bits
!                      wide by max_clk only.
!                      Bit(n) on = apply this epoch to station n.
!                      Bit(n) off = ignore this epoch for station n.
!
!  3 - fjdcl(max_clk)  - a real*8 array of clock epoch in the form of real
!                      number Julian dates.
!
!
!  Additional arrays work in conjuction with these arrays.
!
!  1 - numclk(max_arc_sta) - The number of clock epochs for each site organized
!                      by solve internal site numbers.
!
!  2 - iclstr(max_arc_sta) - an offset pointer for the three arrays above. For
!                      the nth site iclstr(n) points are the position in the
!                      above three arrays immediately before the position of
!                      the first array element which applys to station n.
!
!  3 - breaks_per_site(max_arc_sta) - tracks the number of unconstrained offsets
!                      fore each site. For a site with no breaks the value 1 for
!                      the intial offset.
!
!  4 - site_break_times(max_arc_sta,max_clk_parms) - The times of the breaks for
!                      each site.
!
!  5 - site_break_pointer(max_arc_sta,max_clk_params) - Contains the the number of
!                      the clock parameters (relative to the first) for each of
!                      of the entries in site_break_times.
!
!      For clock parameterizations set by batch and most parameterizations set
!      interactively since the late 1980's a unified list have been used.  In
!      that case numclk is the same for all sites and iclstr is 0 for all sites.
!      Only in very old parametriztions and parametrizations set interactively
!      in the 'THIS STATION ONLY' mode will there be different values for
!      different sites.
!
!     secnd program structure
!
      ICOUNT = 0
      iblas1=1
!
! --- Radians to milliarcseconds for x and y wobble
!
      CNVRT_EOP(1) =(180.0D0/PI__NUM) * 3600.0D0 * 1000.D0
      CNVRT_EOP(2) =(180.0D0/PI__NUM) * 3600.0D0 * 1000.D0
!
! --- Seconds to milliseconds for UT1
!
      CNVRT_EOP(3) =  1.0D3
      FJD_START    =  1.D7
      FJD_STOP     = -1.D7
!
      IF ( HFEOPF_CHR(1:1) .NE. ' ' .AND. HFEOPF_CHR(1:8) .NE. 'NONE    ' ) THEN
           HFEOP_SOL_FILE_NAME = HFEOPF_CHR(1:TRIMLEN(HFEOPF_CHR))
           HFEOP_SOL_FILE_NAMR = ' '
           HFEOP_SOL_FILE_NAMR = PRE_SAV_DIR(:PRE_SV_LEN)//'/'// &
     &                           HFEOP_SOL_FILE_NAME
!@           NUM_MODEL = 1
!@           CALL INIT_HF_EOP ( HFEOP_SOL_FILE_NAMR, NUM_MODEL )
           NUM_MODEL = 0
        ELSE
           HFEOP_SOL_FILE_NAME = 'NONE    '
           HFEOP_SOL_FILE_NAMR = 'NONE    '
      ENDIF
!
! --- An hfeop model is plotted over the eop estimated model
! --- for comparison.
! --- A default hfeop model is selected, but the user may override it and
! --- use an arbitrary model.  Initialize to plot the hfeop comparison model.
!
      HFEOP_CMP_FILE_NAMR = PRE_SAV_DIR(:PRE_SV_LEN)//'/'//HFEOP_CMP_FILE_NAME
!!     .  '/data1/solve_files/'//hfeop_cmp_file_name
!@      NUM_MODEL = 2
!@      CALL INIT_HF_EOP ( HFEOP_CMP_FILE_NAMR, NUM_MODEL )
      NUM_MODEL = 0
!
      MEAN_EO_OFF(1) = 0.D0
      MEAN_EO_OFF(2) = 0.D0
      MEAN_EO_OFF(3) = 0.D0
!
      IDBGN = 1
      DO IDB = 1, NDB
        IF(KBIT(IDCSEL,IDB)) THEN
!
! -------- Initialization of internal data structures used by FLYBY_MAP
!
           CALL FLYBY_MAP_INIT()
!
! -------- Read station names and status array
! -------- and set up a correspondence table between the stations
! -------- listed in NAMFIL (JSIT's) and those listed in PARFIL (ISIT's)
!
          CALL NCORT ( JSITN, JSITI, JCAPPL, NUMSTA, ITT, IDB, &
     &                 IDATYP, ITTB, ET, SE, SS, OBCAPL, MCAPL, JCAVAL, &
     &                 LATS, HEIGHTS, AX_TYPES, AX_OFFS, BARO_CALS, &
     &                 BARO_HEIGHTS, JCAFFL, FCAL_NAMES, NFCAL, NAMSTA, CALCV )
!
! ------- Set up the OBSFIL pointers
!
          IF ( IDB.EQ.1 ) IDBGN = 1
          IF ( IDB.GT.1 ) IDBGN = IDBEND(IDB-1)+1
          IDEND = IDBEND(IDB)
!
          STR = GET_VERSION ()
          DO NOBS = IDBGN,IDEND
!
! ---------- Generate atmosphere, clock and earth orientation values for
! ---------- this data base, whether we intend to plot them or not.
!
            IF ( ( NOBS .EQ. 1  .OR. NOBS .EQ. (NOBS/500)*500 ) .AND. &
     &             KSCREEN ) THEN
!
                 CALL SETCR_MN ( I4P0, I4P0 )
                 WRITE ( BUFSTR, 9010 ) NOBS
 9010            FORMAT ( I6 )
                 CALL ADDSTR_F ( STR(1:21)//'   Obs. '// BUFSTR(1:6) )
                 CALL REFRESH_MN()
            ENDIF
            IOBS = NOBS
!
            CALL USE_OBSFIL(IOBSFIL,IOBS,'R' )
            IF ( .NOT. ( TOL(CFJD,FJD,1.D-5)     .AND. &
     &                   TOL(CFRACT,FRACT,1.D-5)       ) ) THEN
! 
! -------------- This observation is at a brand new time
! 
                 DO I=1,NUMSTA
                    KSTAT(I)=.FALSE.
                 ENDDO
                 KEOSTAT = .FALSE.
                 CFJD=FJD
                 CFRACT=FRACT
            ENDIF
!
            IF ( .NOT. ( KSTAT(ISITE(1)) .AND. KSTAT(ISITE(2)) ) ) THEN
!
! ----------- Still need site 1 or site 2 for this time
!
              CALL FLYBY_MAP()
!
              CALL F__CLR_IND ( 0, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ )
!
! ----------- Add appropriate calibrations
!
              CALL ATMPART (ITT,ISITE,ISITN,ISTAR,VSTARC, &
     &                      AZ,ELEV,ATMPR,RELHU,TEMPC,LATS,HEIGHTS, &
     &                      AX_OFFS,AX_TYPES,BARO_CALS,BARO_HEIGHTS,IDB )
!
              CALL SOCAL (JCAPPL, JSITI, ITT, NOGOOD, ISITE, DT, RT, &
     &                    CALIBS, ICORR, GION, GIONSG, PHION, PHIONS, &
     &                    DERR, RERR, DPHER, ITTB, ET, CALIBB, OBCAPL, &
     &                    ISITN,ISTAR,VSTARC, &
     &                    AZ,ELEV,ATMPR,RELHU,TEMPC, &
     &                         DERR_RAW,RERR_RAW,DPHER_RAW,LATS,HEIGHTS, &
     &                    AX_OFFS,AX_TYPES,BARO_CALS,BARO_HEIGHTS, &
     &                    app,JCAFFL,NFCAL,FCAL_NAMES,NAMSTA,IDB, &
     &                         effreq,pheffreq,reffreq,effreq_xs,pheffreq_xs, &
     &                    axdif,istrn_chr(istar),source_weight_file, &
     &                    source_weights,avg_atm,keldep_noise )
!
!             If SOCAL returns with NOGOOD=1 (i.e., ionospheric correction not
!             good) then give the observation an unweight value of 8 so that
!             the statistics calculations will reflect only good data.  Note,
!             however, that this is only a temporary downweighting since MDLPL
!             does not write the OBSFIL back out.  This also causes the RESFIL
!             unweight flag to be set to 8 so that the ionosphere downweight
!             information will be passed to CNPLT.
!
              IF ( DATYP_INQ ( IDATYP, GROUP__DTP ) ) THEN
!
! ---------------- Group delay data
!
                   IF ( NOGOOD .EQ. 1 ) IUNW=8
                ELSE IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ---------------- Phase delay data
!
                   IF ( NOGOOD .EQ. 1 ) IUNWP=8
              END IF
!
              fj = fjd+fract
              call jd2tag(fj,iy,im,id,ihr,imin )
!
              ICOUNT = ICOUNT+1
              IF(ICOUNT .GT. MAX_OBS ) THEN
                WRITE(errstr, &
     &         '("Tried to save more than ",I5," atm and clock ", &
     &          "values. Quitting!")') MAX_OBS
                call ferr( INT2(247), errstr, INT2(0), INT2(0) )
                STOP "mdlpl/secnd: Too many values -1."
              Endif
!
              IF(.NOT.KSTAT(ISITE(1))) THEN
                SITE_ID(   ICOUNT) = ISITE(1)
                TIME_TAG(1,ICOUNT) = IY
                TIME_TAG(2,ICOUNT) = IM
                TIME_TAG(3,ICOUNT) = ID
                TIME_TAG(4,ICOUNT) = IHR
                TIME_TAG(5,ICOUNT) = IMIN
                Elevation( ICOUNT) = elev(1)
                saszdry(   icount) = sasstom_dry_zenith(1)
                saszwet(   icount) = sasstom_wet_zenith(1)
                press_h(   icount) = pressure          (1)
                tempc_h(   icount) = temperature       (1)
                humid_h(   icount) = humidity          (1)
                real_date( icount) = fjd-2400000.5d0+fract
                saszuse  ( icount) = sasstom_used(1)
!
! ------------- Get info for uniform horizontal plot scaling
!
                If(fj  .lt. fjd_start) then
                  Do II = 1,5
                    FIRST_TIME(II) = TIME_TAG(II,icount)
                  Enddo
                  FJD_START=FJ
                Endif
                if(fj .gt.fjd_stop) then
                  DO II = 1,5
                    LAST_TIME(II) = TIME_TAG(II,icount)
                  ENDDO
                  FJD_STOP=FJ
                endif
!
!
                KSTAT(ISITE(1))=.TRUE.
              ENDIF
!
              IF(.NOT.KSTAT(ISITE(2))) THEN
                ICOUNT = ICOUNT+1
                IF(ICOUNT .GT. MAX_OBS ) THEN
                   WRITE(errstr, &
     &            '("Tried to save more than ",I5," atm and clock values. Quitting!")') MAX_OBS
                  call ferr( INT2(248), errstr, INT2(0), INT2(0) )
                  STOP
                ENDIF
!
                SITE_ID(   ICOUNT) = ISITE(2)
                TIME_TAG(1,ICOUNT) = IY
                TIME_TAG(2,ICOUNT) = IM
                TIME_TAG(3,ICOUNT) = ID
                TIME_TAG(4,ICOUNT) = IHR
                TIME_TAG(5,ICOUNT) = IMIN
                Elevation( ICOUNT) = ELEV(2)
                saszdry(   icount) = sasstom_dry_zenith(2)
                saszwet(   icount) = sasstom_wet_zenith(2)
                press_h(   icount) = pressure          (2)
                tempc_h(   icount) = temperature       (2)
                humid_h(   icount) = humidity          (2)
                real_date( icount) = fjd-2400000.5d0+fract
                saszuse  ( icount) = sasstom_used(2)
!
                KSTAT(ISITE(2))=.TRUE.
              ENDIF !site 2 still needed
            END IF !site 1 or 2 still needed
!
          ENDDO !for all observations in this data base
!
!         BEGIN EOP SECTION
!         Do the eop logic, if neccessary
!
 1000     CONTINUE
          IF ( EO_CONTROL(1) .OR. &
     &         EO_CONTROL(2) .OR. &
     &         EO_CONTROL(2)      ) THEN
!
! --------- Complete the computation of the mean difference offset to
! --------- be removed from the comparison model totals.
!
! --------- Open the data file from ADJST and skip 3 records
!
            call use_eop_plot_file('O' )
            read(eopl_lu,'(a)') buf200
            read(eopl_lu,'(a)') buf200
            read(eopl_lu,'(a)') buf200
!
            ios = 0
            ieo_ct = 0
            do while(ios.eq.0 .and. ieo_ct.lt.max_pts) ! Read to EOF
              read(eopl_lu,'(a)',iostat=ios) buf200
              if(ios.eq.0) then !Value found
                ieo_ct = ieo_ct+1
                read(buf200, &
     &          '(f15.4,5i5,2(2f13.1,13x),f13.1,f13.1)')eop_date(ieo_ct),(eop_tag(i,ieo_ct),i=1,5), &
     &          eo_val(1,ieo_ct),eo_sig(1,ieo_ct), &
     &          eo_val(2,ieo_ct),eo_sig(2,ieo_ct), &
     &          eo_val(3,ieo_ct),eo_sig(3,ieo_ct)
!
!               Generate the high-frequency eop data for the model used
!               in the solution (but only if one was used).
!               Then generate the high-frequency eop data for the model
!               to be plotted for comparison.
!               Units are micro-seconds and micro-arc-seconds
                do ict = 1,2
                  if (ict.eq.2 .or. add_back_hfjmg) then
                    call return_hf_eop(eop_date(ieo_ct), &
     &                   model_val(3,ieo_ct,ict), &
     &                   model_val(1,ieo_ct,ict), &
     &                   model_val(2,ieo_ct,ict),ict )
                  else
                    do i = 1,3
                      model_val(i,ieo_ct,ict) = 0.d0
                    enddo
                  endif
                Enddo
!
!               The eo_val's from adjst always are total with the effect of hfeop.
                do ieop=1,3
                  mean_eo_off(ieop) =  mean_eo_off(ieop) - eo_val(ieop,ieo_ct)
                Enddo
              Endif !Value found
            Enddo ! Read to EOF
            call use_eop_plot_file('O' )
!
            do i = 1,3
              mean_eo_off(i) = mean_eo_off(i)/ieo_ct
            enddo
!
!           Eliminate the mean difference from the comparison model totals
!
            Do i = 1, ieo_ct
              Do j = 1, 3
                model_val(j,i,2) = model_val(j,i,2) - mean_eo_off(j)
              enddo
            enddo
          Endif ! Do eop
        ENDIF !this data base included
      ENDDO !for all data bases
!
      RETURN
      END
