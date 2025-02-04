      SUBROUTINE A1JST ( SCSIG, KSRC, OVRTRA_ATM, OVRTRA_CLK, OVRTRA_GRD, &
     &                   KCONS, SITES_ESTIMATED, MAT, M_SAV, L_SAV, &
     &                   ADR_SAV, VAL_SAV, TOTAL_SITEC, LBUF_LEN, LBUF, IPTR, &
     &                   PAGEWID, CNSTROBJ, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  A1JST PROGRAM SPECIFICATION
!
! 1.1 A1JST is the old ADJST and handles parameters thru sources only.
!     A2JST handles the rest.  A3JST is the epilogue of A1JST.
!
! 1.2 REFERENCES:
!
! 2.  A1JST INTERFACE
!
! 2.1 Parameter File
      INCLUDE    'astro_constants.i'
      INCLUDE    'solve.i'
      INTEGER*4   MAX_SEGMENTS
      INTEGER*4  MAX_ATM_I4, MAX_CLK_I4
      PARAMETER  ( MAX_ATM_I4 = MAX_ATM )
      PARAMETER  ( MAX_CLK_I4 = MAX_CLK )
      PARAMETER  ( MAX_SEGMENTS = MAX( MAX_ATM_I4, MAX_CLK_I4, MAX4_EOP) )
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4  M_SAV
      INTEGER*4  L_SAV
      ADDRESS__TYPE :: ADR_SAV(M_SAV)
      REAL*8     OVRTRA_ATM, OVRTRA_CLK, OVRTRA_GRD, VAL_SAV(M_SAV)
      REAL*8     TOTAL_SITEC(3,MAX_STA), SCSIG(*)
      LOGICAL*2  KSRC,KCONS
!
! KCONS - True if constraints are applied
! KSRC - True once any source coordinate parameters are printed
! OVRTRA_ATM - Overall sum of atmosphere constraint shares
! OVRTRA_CLK - Overall sum of clock constraint shares
! OVRTRA_GRD - Overall sum of gradients constraint shares
! SCSIG - Scaled sigmas
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
      INCLUDE 'buff4.i'
      INCLUDE 'buff2.i'
      INCLUDE 'cnstr.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'heo.i'
      INCLUDE 'flyby.i'
!
! 2.2 INPUT Variables:
!
      REAL*8 MAT(*)
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID
      CHARACTER   LBUF(LBUF_LEN)*120
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: adjst
!       CALLED SUBROUTINES: uen_vel,posmap1,posmap2,comp,errcmp,cndms
!
! 3.  LOCAL VARIABLES
!
!     ISCNT ( IN BUFF1 )will hold counters for BASFE indicating where
!     IN THE normal equations array the site coordinates are located.
!
      REAL*8 ATMSTA(MAX_ARC_STA),ATMRMS(MAX_ARC_STA),OVRRMS,OVRSTA
      REAL*8 GRDSTA(MAX_ARC_STA),GRDRMS(MAX_ARC_STA)
      REAL*8 CLKSTA(MAX_ARC_STA),CLKRMS(MAX_ARC_STA),OVRRMS_CLK
      REAL*8 OVRSTA_CLK, ATM_CUM, ATM_SQR_CUM, ATM_EPO_CUM, &
     &       ATM_AVG, ATM_EPO, TOT_ATM_RMS, ATM_RMS
      REAL*8 OVRSTA_GRD,OVRRMS_GRD,GRDTRA(MAX_ARC_STA)
      REAL*8 ATMTRA(MAX_ARC_STA),CLKTRA(MAX_ARC_STA),trace
      REAL*8 COR,XSIG,SSIG,CORR,XSSIG,SINCOS(2),FSTRC(2),full_clock_sig
      REAL*8 PVAL,AATM,FJD8,ATMFAC,APRIORI_XYZ(3)
      REAL*8 SIGMX(21), DUM(6), SCALE(6), FJD, XYZVEL(6,MAX_STA), SCERR
      REAL*8 CONST, CONST1, CONST2, TIME_INTERVAL1, TIME_INTERVAL2
      REAL*8 TIM1, TIM2, SIGSX(3), VS(3,MAX_PWC_EPS), VS2(3,MAX_STA)
      REAL*8 MM_PSEC
      COMMON / ADJ / VS2
! common block used to hold output for curses appears here and
! in a2jst, a3jst
      INTEGER*2 FIRST_LINE, LAST_LINE, LMINF, N, IALPHA, IOS, NATM
      INTEGER*4 IOS4
      CHARACTER ALPHABET*26, STARSYMB*1, MSD*1
      DATA ALPHABET /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      INTEGER*4 PAGELEN 
      INTEGER*2 NAMCP(4,2), NPX(6), MON, DAY, YR, ITIM
      INTEGER*2 ICMP(6), MSC(3), LABSC(2,2), OVRCNT_CLK
      INTEGER*2 ATMCNT(MAX_ARC_STA), CLKCNT(MAX_ARC_STA)
      INTEGER*2 OVRCNT, INEXT, XYZPRM(3,MAX_STA), KCLPREV
      INTEGER*2 OVRCNTX, CLKCNTX(MAX_ARC_STA), ATMCNTX(MAX_ARC_STA)
      INTEGER*2 GRDCNT(MAX_ARC_STA)
      INTEGER*2 OVRCNT_GRD
      INTEGER*2 OVRCNTX_GRD, GRDCNTX(MAX_ARC_STA)
      LOGICAL*2 KBIT, KBIT4, USESTA, KPRINT, PRINT_CONT, KEPOCHS
      INTEGER*2 KBITN, KBITN4
      LOGICAL*2 EQUAL, POLYONLY, NEED_HEAD, NEED_SPA, KREFSTA, KFIRST
      LOGICAL*2 SITES_ESTIMATED, VELSET
      CHARACTER   SOID(2)*6, stpt2(MAX_PWC_EPS)*6 
      CHARACTER   FBFNAM(12)*(NAME_SIZE), FNAME*(NAME_SIZE)
      CHARACTER   TYPEPR*8, TBLOUT*1, STR*255, STR1*32
      CHARACTER    NAMCP_CHR(2)*8
      EQUIVALENCE (NAMCP,NAMCP_CHR)
      INTEGER*2 I,IAORD,IATM,ICLCK,ID,IGOT1,IGOT2,IHR,IM,IMIN, &
     &   IOFCMP,ISTA,JSTA,ITIME,ITYPE,IXYZ,J,JATM,JCLOCK,iyr, &
     &   JSTR,K,KCLOCK,L,LC,LP,LPC,LS,M
      INTEGER*4  NPARM, NPARMC, NPARMV, NP
      INTEGER*4  ISTB, IHPE, ISPE
      INTEGER*8  N1, N2, N3, N3N1, P1N1, N1N1
      CHARACTER  CNAME*20, LCMP(3)*1, CDUM*4, PLATES(MAX_STA)*4 
      DATA       LCMP / 'X', 'Y', 'Z' /
      INTEGER*2  INT2_C, INT2_S
      PARAMETER  ( INT2_C = 2H C )
      PARAMETER  ( INT2_S = 2H S )
      CHARACTER*2 LS_CHR, LC_CHR
      EQUIVALENCE (LS,LS_CHR), (LS,LS_CHR)
      INTEGER*4 IX, IY, IDUM, JA, JS, JB
      INTEGER*2 LSTPT2(3,MAX_PWC_EPS), IOLD, OLDNP, NXTCLK, JCLOCKX
      INTEGER*2 JJ, II, NXTEP, SNUM, OVRCNTX_CLK, JZ
      INTEGER*2 ISTRUC_COPY(MAX_STRUC)
      INTEGER*4  NPREV, NNEXT, SIND(2), SIND_COO(MAX_SOU,2), SIND_PRP(MAX_SOU,2)
!
      REAL*8 ATMOS_VALUES(MAX_SEGMENTS,2*MAX_ARC_STA)
      REAL*8 ATMOS_SIGMAS(MAX_SEGMENTS,2*MAX_ARC_STA)
      REAL*8 CLOCK_VALUES(MAX_SEGMENTS,2*MAX_ARC_STA)
      REAL*8 CLOCK_SIGMAS(MAX_SEGMENTS,2*MAX_ARC_STA)
      REAL*8 TCLOCK_VALUES(MAX_SEGMENTS,2*MAX_ARC_STA)
      REAL*8 BCLOCK_VALUES(MAX_SEGMENTS,2*MAX_ARC_STA)
      REAL*8 TIME_LAPSED
      REAL*8 CL_SHF(0:31), CL_DRF(0:31), CL_SQU(0:31)
      REAL*8 CD(MAX_ARC_STA), CS(MAX_ARC_STA), BD(MAX_ARC_STA), TD(MAX_ARC_STA)
      INTEGER*4 NPARM_C1, NPARM_C2, IER, &
     &          IBRK, IT_BRK, MT_BRK, M_CNT, MM_CNT, L_CNT, &
     &          ICNT, IBG, IPAR, IEN, IP
      INTEGER*4  I_LEN, LINDEX
      PARAMETER  ( MT_BRK = MAX4_BRK*MAX_ARC_STA )
      PARAMETER  ( M_CNT  = MAX4_BRK*2 + 2       )
      PARAMETER  ( MM_CNT = (M_CNT*(M_CNT+1))/2  )
      REAL*8       JD_BRK(MT_BRK), IST_BRK(MT_BRK), TC_BRK(MT_BRK), &
     &                                              BC_BRK(MT_BRK), &
     &                                              BS_BRK(MT_BRK)
      REAL*8     FJD_1970, FJD_2030
      REAL*8     CLS_CUR, CLS_PRV, CLS_BRK, CLK_CNT(M_CNT), CLK_COV(M_CNT,M_CNT)
      REAL*8     MULT_VMV, VSITEC_SAVE(3,MAX_STA) 
      REAL*8     COREL_AAR, COREL_ADR, COREL_DRA, COREL_DDR, COREL_ARDR
      INTEGER*4  IPAR_CLK(M_CNT), ICLK_BRK(0:MAX4_BRK,0:MAX_CLK), IDEG, &
     &           INOD, IUER
      INTEGER*4  I61
      CHARACTER  OUTL*4096, OUTB*4096, SIGMA_LABEL*7, C_PRP(MAX_SOU)*8
!
      EQUIVALENCE ( LSTPT2(1,1), STPT2(1) )
      EQUIVALENCE (IDUM,CDUM)
      REAL*8      SCAMP, SCPHS
      REAL*8      CNV(2), FJD1, LJD1
      INTEGER*4  J1, J2, J3, J4, J5, J6, J11, J12, KZEN, IXC, L_PAR, L_PRP, &
     &           IBC_BRK, IBS_BRK
      REAL*8     JDATE_APR, JDATE_APR_BEG, ZENDEL, JAPR(MAX_OBS), JAPR_MIN, &
     &           ZDEL(MAX_OBS), COEF(MAX_OBS), WORK(MAX_OBS), SCALE_FACTOR, &
     &           FSPL8
      PARAMETER  ( JAPR_MIN = 1.D-6 )  ! in days
      INTEGER*4  IND_PRV, IND_CUR, IND_FTR, IND_PRP
      CHARACTER  UNIT*9, FULL_GRAD_STR*20, RELEASE_DATE*10, REVISION_DATE*10, &
     &           LET*2, C_PAR(M_GPA)*20
      REAL*8     VEC_UEN(3), XYZ_TO_UEN(3,3), JD_USE 
      REAL*8     RA_PRP(MAX_SOU), DEC_PRP(MAX_SOU), PRP_VAL(2)
      DATA MM_PSEC / 3.33564095198152 /
      LOGICAL*4  FL_STA_USED 
      LOGICAL*1  TITLE_ANON, FL_GLOBAL_L1
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, GET_VERSION*54
      LOGICAL*4, EXTERNAL :: CHECK_STABIT, DATYP_INQ, IS_R8_NAN
      INTEGER*4, EXTERNAL :: INDX4, ILEN, IXMN8, LTM_DIF
      INTEGER*8, EXTERNAL :: INDX8
!
      DATA CNV  / 15000.0D0, 1000.0D0 /
      DATA ICMP   /2HX ,2HY ,2HZ ,2HU ,2HE ,2HN /
      DATA NAMCP  /2HRT,2H. ,2HAS,2HC.,2HDE,2HC.,2H  ,2H  /
      DATA MSC    /2HT ,2HA ,2HM /
      DATA LABSC  /2HSI,2HN ,2HCO,2HS /
      DATA SOID / 'Sine  ', 'Cosine' /
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   JWR  880911  Logic for optional suppression of printing of
!                continued atmosphere rates and continued clock
!                rates added.
!
!   MWH  900620  Suppress printing of continued clock and atmosphere
!                rates even if full output is selected.
!
!   MWH  910524  Modify to support new clock/atmosphere parameterization scheme
!
!   jwr  921228  Logic added for cubic spline and ut1s.
!
!   mwh  940426  Support option to output station positions and sigmas for epoch
!                that minimizes sigmas
!
!   kdb  951006  Add an extra digit to the spoolfile global and arc section
!                ra and dec totals, adjustments and sigmas, and the
!                correlation.  Shift the North gradient output fields to the
!                left to make it easier to separate the North and East
!                gradient series.
!
!   kdb  960131  Install into the 32 site/i*4 obsfil counter version a
!                change made by Mark Hayes to print the Avg Atm.
!
!   DSM  960429  Corrected the calculation of gradient statistics:
!                rms,wrms, and share calculations
!
!   KDB  960502  Shift the gradient output fields again.  All gradient sigmas
!                will now line up with the other parameters' sigmas, and
!                the North and East adjustments will be offset slightly
!                centered on the other adjustment fields.
!
!   jmg  960610  For sites with episodic motion, don't estimate axis offset
!                for each epoch.
!
!   pet  970425  Added stuff for special simplified treatment of the case with
!                unifrom segments. Updated somewhere comments.
!
!   pet  970428  Added stuff for implementation new scheme for handling clock
!                breaks.
!
!   pet  971006  SEG_OUTPUT  from glbc4.i substituted obsolete variable
!                FULL_ADJUST_LIST from socom.i  Improved appearence of the
!                listing of segmented parameters
!
!   BA   971020  Corrected sign handling for declination output.
!
!   pet  971104  Added support of file CLOTxx for total clocks. Changed format
!                of CLOCxx and ATMOxx files (added more digits)
!
!   pet  971128  Changed MAX_SEGMENTS: set it in according to solve.i instead
!                of hard coded constant.
!
!   pet  971202  Added logic for bypassing deselected station
!
!   pet  971204  Added logic for splitting baseline deselection on phase delay
!                and group delay solution
!
!   pet  980119  Changed a bit the format for source declination to be
!                consistent with sp_comp and previous versions
!
!   pet  980123  Restored a piece of archaic code for calcualtion clock time
!                epoch for the case when clocks are not uniform for saving
!                comapatibilty with archaic SOLVE versions.
!
!   JMG  98JAN   Added the option to write sinex output.
!
!   pet  980204  Added printing condition number of the CGM in spool-file.
!
!   KDB  980223  Change interactive sinex output file name from SINEXxx to
!                SINXxx.
!
!   KDB  980223  Batch interface for sinex output.
!
!   pet  980313  Changed the strategy for calculation of clock function sigmas
!                used for clock plots for the case of uniform clocks: values
!                of only segmented component of clock function are calculated,
!                but sigmas correspond to total clock function. At the
!                same time "full_segmented_output" produces sigmas of the
!                parameters "segmented clocks". Their sigmas ARE NOT THE SAME
!                as sigmas of total clock function.
!
!    KDB 980414  In batch mode, compress sinex files.
!
!    pet 980508  Forced to print atmosphere rate when in no-batch mode
!
!    pet 980512  Corrected an error occurred in modifications of 980508
!
!    pet 980520  Added trap of inconsistency of parameterization for forward and
!                back solutions for the case when positions of some station(s)
!                were modeled as piece-wise linear finction in forward solution
!                but not in back solution
!
!    pet 980914  Fixed several bugs in printout of atmosphere parameters:
!                1) Previous version printed atmosphere scaled sigmas
!                   incorrectly (they printed unscaled sigmas instead)
!                2) Previous version put in spool file values for segmented
!                   atmosphere twice;
!                3) Previous version split line for atmopshere parameters at
!                   spool file
!
!    pet 980929  Increased number of digits for estimates and sigmas of right
!                ascension in intractive printout. Changed a bit format of
!                interactive printout of estimates and their uncertainties
!                for right ascension and declination of the sources
!
!    pet 990309  Moved reading user_buffer to ../adjst/adjst.f
!
!    pet 990312  Added bypass of clock section if NUMCLK=0
!                Added formal parameters M_SAV, L_SAV, ADR_SAV, VAL_SAV.
!                They keeps elements of covaraince matrix which are changed.
!                It allows to restore array MAT at the end of work adjst_do
!                and to make adjst_do re-enterable.
!
!    pet 990317  Changed the way of initialization of counters and
!                accumulators. Added a call of A1JST_INIT. Moved a call of
!                USE_SPOOL  from A1JST to ADJST_HEAD.
!
!    pet 990409  Replaced call of system with call of system2_sh in order to
!                fix SYSTEM-APR99 bug
!
!  pet 1999.05.11  Corrected a coding mistake previnting printing values of
!                  atmosphere path delay at the first epoch. Corrected another
!                  mistake: xssig ( the last coluumn) for atmosphere was
!                  printed incorrectly in batch mode.
!
!  pet 1999.05.28. Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                  eliminated common block adj_buf
!
!  pet 1999.11.04  Added support of file CLOBxx for clocks with breaks.
!                  File CLOBxx contains values of claock function plus clock
!                  shifts due to clock breaks.
!
!  pet 2000.01.31  Forced to print leading zeroes in dates, f.e "00/11/04 07:01"
!                  instead of " 0/11/ 4  7: 1"
!                  Added a delimiter before $$$$$$$ in CLOB-, CLOT- CLOC- files
!
!  pet 2000.02.01  Fixed a bug: formal uncertainties of clock function were
!                  computed incorrectly after clock breaks
!
!  2000.03.19 pet  Added support of value 'N' of UT1_RS and KEROT ( no zonal
!                  tide interpolation magic for mapping EOP)
!
!  2000.03.30 pet  Forced to print only the last part of the mapping file in the
!                  header if a mapping file is too lone (longr than 9 symbols)
!
!  2000.05.02 pet  Added support of variable APRIORI_ZENDAL. If APRIORI_ZENDAL
!                  is TRUE and SEG_OUTPUT is TRUE and BATCH is TRUE then
!                  a priroi zenith path delay at the epochs of linear spline
!                  is printed in spool_file. A priori path delay is computed
!                  for each observation in CRS and is written in file ATZExx .
!                  A1JST reads it and compute coefficients of interpolation
!                  cubic spline. Then it interpolates a priori zenith path
!                  delay on the epoch of the nodes of linear spline. A priori
!                  zenith path delay is printed at exactly the same epocs as
!                  the adjustments to wet path delay.
!
!  2000.07.17 pet  Increased space in each column of the output CLOxxx files
!                  in order to prevent overflow when printing clock function
!                  for the cases when clock had breaks about -+1 second
!
!  2000.08.18 pet  Fixed a bug: the previous version printed velocity
!                  substitution file always NONE, regardless whether velocity
!                  substitution file has been used or not.
!
!  2000.09.25 pet  Added support of mean troposphere gradients: if mapping file
!                  for mean troposphere gradients were supplied and therefore
!                  calibration for mean troposphere gradients has been applied
!                  then full adjustments of troposphere gradients are computed
!                  and put in the listing.
!
!  2001.01.11 pet  Changed the way how scaled factor or m-sigmas is computed.
!                  Renamed "Scaled sigma" --> "m-sgima" and renamed
!                  "Sigma" --> "a-sigma". forced a1jst to print true when
!                  it works in global mode: the last column contains "a-sigmas"
!
!  2001.01.18 pet  Corrected the code: the new version does not print
!                  adjustments of atmosphere gradients if KMINOUT flag is set
!                  (it corresponds to MINIMUM NO in $OUTPUT section of batch
!                  control file). But it still prints gradients constraints
!                  statistics.
!
!  2001.02.05 pet  Small correction: scale factor is set to 1.0 for global type
!                  of solution in current version. m-sigmas are computed only
!                  for independent solution since the session-dependent chi/ndg
!                  is not computed correctly in global solutions.
!
!  2001.04.25 pet  Added the check whcih prevents dividing on zero.
!
!  2001.12.05 pet  Added trap of internal control for catching a crazy value
!                  of source position
!
!  2002.03.05 pet  Changed offsets for adjustments of troposphere gradients:
!                  the previous version had additional offsets which caused
!                  misalignments. The new version puts adjustments and their
!                  formal uncertainties by the consistent way with other
!                  parameters.
!  2002.03.28 pet  Added call LISTING_OPTIONS -- subroutine which prints
!                  listing options
!  2002.04.08 pet  Moved call of WRITE_SINEX to ADJST_DO routine
!  2002.05.08 pet  The field "parameter index" was changed from I4 to I5
!                  in order to support solutions with more than 9999
!                  parameters. As a rasult all adjustments and their formal
!                  uncertainties were moved one character to the right edge
!                  of listing;
!                  2) Moved "Avg Atm" parameter two character to right
!                  3) Moved one character to right lines with UEN site positions
!  2002.05.23 pet  Added pringin a revision date just after Listing options
!                  (Request of Karen Baver)
!  2002.05.31 pet  Added parameter TOTAL_SITEC for carrying total site
!                  positions
!  2002.12.24 pet  Added logic which handles the case when the reference epoch
!                  for apriori site position and for the site position
!                  estimates are different
!  2003.08.07 pet  Replaced CNPLT with REPA
!  2003.11.05 pet  Removed call to ancient ib2as
!  2003.12.09 pet  Replaced fc_gwinw, fc_gwinsz  calls with GET_TERMSIZE
!  2004.03.16 pet  Fixed a bug in computing atmosphere constraints statistics
!                  in the case if only atmosphere time independent offset
!                  was computed
!  2004.04.17:jwr  Real8 variables were put in INT4() for printing since they
!                  are printed with I format specification
!                  logic more transparent.
!  2004.06.26 pet  Fixed a bug: the previous version did not print atmosphere
!                  path delay estimates in simulation mode
!  2004.10.26 pet  Fixed a bug introduced dyring porting in Linux: CLOCxx
!                  file was formed incorrectly.
!  2005.02.04 pet  Made ADJST to call OPTIN adter REPA ends
!  2005.03.02 pet  Added support of harmonics site position variations and 
!                  spline coefficients of site positions
!  2005.03.07 pet  Added call of two routines which generate extendend listing
!                  in the case of non-linear site positon estimation
!  2005.03.30 pet  Added restoration of spoiled covariance matrix and the &
!                  vector of estimates before call of SPE_POS
!  2005.07.21 pet  Added support of internal variable SEG_LISTING_STYLE. 
!                  SEG_LISTING_STYLE == SEG_PRE2005 is reserved for backward
!                  compatibiltiy. If SEG_LISTING_STYLE == SEG_POST2005, then
!                  for clock, atmospheric path delay in zenith direction
!                  and atmospheric gtradients the time tag epoch is printed 
!                  with accuracy one millisecond in ISO-compatible format.
!  2005.08.18 pet  Forced to print TAI time in time tag for parameters if &
!                  SEG_LISTING_STYLE == SEG_POST2005
!  2006.05.31 pet  Added support of printing adjustments of harmonic Earth
!                  orientation parameters
!  2019.04.03 pet  Updated logic to prevent crashes when clock parameters are insane
!  2023.03.13 pet  Added support of a priori proper motions. Changed format &
!                  of reported proper motions. 
!  2024.08.03 pet  Fixed a bug in computation of correlations related to proper motion
!
!-----
!
! 5.  A1JST PROGRAM STRUCTURE
!
!CCCC
!
!  STATEMENT FUNCTION THAT FIGURES IF WE SHOULD PRINT A PARAMETER
!
      KPRINT(NP) = ( KBIT4(ISTRUC_COPY,MAX(INT2(1),NP)) .EQV. KGLOBALS ) .OR. &
     &               .NOT. KBATCH .OR. &
     &             ( KBATCH  .AND.  ISLTY2 .EQ. 'I' )
!
      IF ( KBIT ( PRE_IP ( 3 ), INT2(12)) .AND. REWAY_VERBOSE ) THEN
           KSCREEN = .FALSE.
      END IF
!          ! But supress printout in silent REWAY mode
!
      IF ( ISCREEN == ISCREEN_NO ) KSCREEN = .FALSE. 
      JA = 3*M_GPA
      JB = 2*M_GPA
      JS =   M_GPA
!
! --- Initialization of some variables and arrays
!
      CALL A1JST_INIT ( MAX_ARC_STA, &
     &           ATMSTA, ATMRMS, ATMCNT, ATMCNTX, ATMTRA, &
     &           CLKSTA, CLKRMS, CLKCNT, CLKCNTX, CLKTRA, &
     &           GRDSTA, GRDRMS, GRDCNT, GRDCNTX, GRDTRA, &
     &           OVRSTA, OVRRMS, OVRCNT, OVRCNTX, &
     &           OVRTRA_ATM, OVRTRA_CLK, OVRTRA_GRD, &
     &           OVRSTA_CLK, OVRRMS_CLK, OVRCNT_CLK, OVRCNTX_CLK, &
     &           OVRSTA_GRD, OVRRMS_GRD, OVRCNT_GRD, OVRCNTX_GRD, &
     &           PRINT_CONT, JCLOCKX, NPREV, KFIRST )
!
      IF ( .NOT. KGLOBALS   .AND.  SOCOM_PLUS_FIRST .NE. SPL__DONE ) THEN
           WRITE ( 6, * ) 'A1JST: socom_plus has not been initialized'
           STOP 'ADJST: Abnormal termination'
      END IF
!
      CALL GETENVAR ( 'PSOLVE_GET_NAMES', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           CALL GET_NAMES ( C_PAR, INT2(20), M_GPA, L_PAR, TRUE__L2, FALSE__L2 )
           DO 510 J1=1,L_PAR
              WRITE ( 6, '(I6,1X,A)' ) J1, C_PAR(J1)
 510       CONTINUE 
     END IF
!
! --- Save array VSITEC since a1jst will spoil it
!
      CALL COPY_R8 ( 3*INT4(MAX_STA), VSITEC, VSITEC_SAVE )
!
! --- Set up glbfil
!
      CALL USE_GLBFIL   ( 'OR' )
      OLD_USER_PART = NUM_USER_PART
      CALL USE_GLBFIL_3 (  'R' )
      CALL USE_GLBFIL_4 (  'RC' )
      KBEEP = .FALSE.
      IF ( CRES_STYLE .EQ. CRES__PRE98 ) THEN
           TITLE_ANON = .TRUE.
         ELSE
           TITLE_ANON = .FALSE.
      END IF
!
! --- Open and read common, prfil, and nrmfil
!
      IF ( KGLOBALS ) THEN
           NEED_SPA = .FALSE.
      END IF
      NPARM = 0
      OLDNP = 0
      DO I=1,6
         NPX(I) = 0
      ENDDO
      SITES_ESTIMATED = .FALSE.
!
! --- Open clock and atmos files for passing info to MDLPL.
!
      IF ( .NOT. KBATCH ) THEN
           OPEN ( 36, FILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'CLOC'//PRE_LETRS, &
     &            STATUS='UNKNOWN', IOSTAT=IOS4 )
           OPEN ( 37, FILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'CLOT'//PRE_LETRS, &
     &            STATUS='UNKNOWN', IOSTAT=IOS4 )
           OPEN ( 38, FILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'CLOB'//PRE_LETRS, &
     &            STATUS='UNKNOWN', IOSTAT=IOS4 )
           OPEN ( 39, FILE = PRE_SCR_DIR(1:PRE_SD_LEN)//'ATMO'//PRE_LETRS, &
     &            STATUS='UNKNOWN', IOSTAT=IOS4 )
      ENDIF
!
! --- Initilaize the content of amosphere and clock values and sigmas by zeros
!
      CALL NOUT_R8 ( 2*MAX_SEGMENTS*INT4(MAX_ARC_STA),  ATMOS_VALUES )
      CALL NOUT_R8 ( 2*MAX_SEGMENTS*INT4(MAX_ARC_STA),  ATMOS_SIGMAS )
      CALL NOUT_R8 ( 2*MAX_SEGMENTS*INT4(MAX_ARC_STA),  CLOCK_VALUES )
      CALL NOUT_R8 ( 2*MAX_SEGMENTS*INT4(MAX_ARC_STA),  CLOCK_SIGMAS )
      CALL NOUT_R8 ( 2*MAX_SEGMENTS*INT4(MAX_ARC_STA), TCLOCK_VALUES )
      CALL NOUT_R8 ( 2*MAX_SEGMENTS*INT4(MAX_ARC_STA), BCLOCK_VALUES )
!
! --- Initialize iptr and set up top of lbuf
!
      IPTR=1
      PAGELEN=24
      PAGEWID=79
!
! --- Read saved CRES output from file to be used to scroll back display
!
      IF ( KSCREEN.AND. .NOT. KBATCH ) THEN
           FNAME = PRE_SCR_DIR(:PRE_SD_LEN)//'CRBF'//PRE_LETRS
           OPEN ( 34, FILE=FNAME, IOSTAT=IOS4 )
           IOS = IOS4
           CALL FERR ( IOS, "Opening CRES display buffer", INT2(0), INT2(0) )
           DO WHILE ( .TRUE. )
              READ ( 34, '(A)', END=50, IOSTAT=IOS4 ) LBUF(IPTR)
              IOS = IOS4
              CALL FERR ( IOS, "Reading CRES display buffer", INT2(0), &
     &             INT2(0) )
              IPTR=IPTR+1
           ENDDO
50         CONTINUE
           CLOSE ( 34, IOSTAT=IOS4 )
           IOS = IOS4
           CALL FERR ( IOS, "Closing CRES display buffer", INT2(0), INT2(0) )
      ENDIF
!
! --- The flag KGLOBALS indicates a run of ADJST to print the
! --- adjustments of the global parameters of a back solution
! --- In this case, ISTRUC is set to
! --- all true, since all the parameters being printed are
! --- global (see program GLOBL for origin of the globals)
! --- same story for JCAPPL
!
      FJD = SIT_EST_EPOCH
      CALL MDYJL ( MON, DAY, YR, ITIM, FJD )
      TYPEPR=' '
      DO J1=1,M_GPA
         CALL SBIT4 ( ISTRUC_COPY, J1, KBITN4(ISTRUC,J1) )
      ENDDO
!
! --- Handle the case of SKED simulations where the residuals are identically
! --- zero.  Set SCALE_FACTOR to 1.0 so that the scaled sigmas on the terminal
! --- are not zeroed out.
!
      IF ( CHISQR(3) .GT. 1.D-6 ) THEN
           SCALE_FACTOR = DSQRT ( CHISQR(3) )
         ELSE
           SCALE_FACTOR = 1.D0
      END IF
!
      IF ( KGLOBALS ) THEN
           TYPEPR='global'
           DO J1=1, M_GPA
              CALL SBIT4 ( ISTRUC_COPY, J1, INT2(1) )
           ENDDO
           DO I=1,MAX_ARC_STA*WORD_BITS
              CALL SBIT ( JCAPPL, I, INT2(1) )
           END DO
      END IF
      IF ( ISLTY2 .NE. 'I' ) THEN
           SCALE_FACTOR = 1.D0 ! a-sigma = m_sigma for global solution
      END IF
!
! --- Make scaled sigmas
!
      DO J1=1,NPARAM
         SCSIG(J1) = MAT(JS+J1) * SCALE_FACTOR
      END DO
!
      CALL SITPL ( ISITN, NUMSTA, PLATES )
!
! --- Flyby initialization
!
      IF ( .NOT. KGLOBALS ) THEN
          CALL FLYBY_APRIOR()
!
! ------- Loop over the 6 six possible FLYBY options.
!
         IF ( ( KSPOOL .AND. KFULLOUT )         .OR. &
     &        ( IARCNM .EQ. 1  .AND.  KMINOUT ) .OR. &
     &        ( KSCREEN .AND. KFULLOUT)              ) THEN
!
            CALL PUT9 ( STASUB, FBFNAM(1)  )  ! sites
            CALL PUT9 ( AXOSUB, FBFNAM(12) )  ! axis offsets
            CALL PUT9 ( SRCSUB, FBFNAM(2)  )  ! sources
            CALL PUT9 ( NUTSRS, FBFNAM(3)  )  ! nutation series
            CALL PUT9 ( NUTDLY, FBFNAM(4)  )  ! nutation daily values
            CALL PUT9 ( EOPDLY, FBFNAM(5)  )  ! eop series
!
            IF ( KEROT ) THEN ! there is eop mod file
                 IF ( FLYBY_INTERP     .EQ.1 )  FBFNAM(6) = 'Linear   '
                 IF ( FLYBY_INTERP     .EQ.3 )  FBFNAM(6) = 'Cubic    '
                 IF ( FLYBY_INTERP     .EQ.4 )  FBFNAM(6) = 'C. Spline'
              ELSE
                 IF ( INTERPOLATION_UT1.EQ.1) FBFNAM (6) = 'Linear   '
                 IF ( INTERPOLATION_UT1.EQ.3) FBFNAM (6) = 'Cubic    '
                 IF ( INTERPOLATION_UT1.EQ.4) FBFNAM (6) = 'C. Spline'
            ENDIF
            CALL PUT9 ( PLTMOD, FBFNAM(7) ) ! plate tectonic
            IF ( FBFNAM(7)(1:4) .EQ. 'NONE' ) THEN
!
! -------------- If plate techtonic model was not in use look for velocity
! -------------- substitution file
!
                 CALL PUT9 ( VELSUB, FBFNAM(7) ) ! site velocity
            ENDIF
            CALL PUT9 ( PLCALF, FBFNAM(8) ) ! pressure loading
            IF ( UT1_RS .EQ. 'R' ) THEN
                 FBFNAM(9) = 'UT1R     '
               ELSE IF ( UT1_RS .EQ. 'S' ) THEN
                 FBFNAM(9) = 'UT1S     '
               ELSE IF ( UT1_RS .EQ. 'N' ) THEN
                 FBFNAM(9) = 'NO_ZONAL '
               ELSE
                 STOP 'A1JST: UT1_RS not defined - must be R or S or N'
            ENDIF
!
            IF ( KEROT ) THEN
               IF ( UT1_RS_FLYBY .EQ. 'R' ) THEN
                    FBFNAM(10) = 'UT1R     '
                 ELSE IF ( UT1_RS_FLYBY .EQ. 'S' ) THEN
                    FBFNAM(10) = 'UT1S     '
                 ELSE IF ( UT1_RS_FLYBY .EQ. 'N' ) THEN
                    FBFNAM(10) = 'NO_ZONAL '
                 ELSE
                    STOP 'A1JST: UT1_RS_FLYBY not defined. Must be R or S or N.'
               ENDIF
             ELSE
               FBFNAM(10) = 'N/A      '
            ENDIF
!
! --------- Handle high frequency eop flyby model.
!
            IF ( STAT_HEO .EQ. HEO__READ ) THEN
                 STR = FINAM_HEO
                 IP = LINDEX ( STR, '/' )
                 IF ( IP .GT. 0 ) THEN
                      CALL CLRCH  ( STR(1:IP) )
                      CALL CHASHL ( STR       )
                 END IF
                 CALL PUT9 ( STR, FBFNAM(11) )
               ELSE 
                 CALL PUT9 ( HFEOPF, FBFNAM(11) )
            END IF
!
! ------- In the interactive mode pause to let the user see what CRES did,
! ------- but only if the EOP is set to the old polymomial mode.
! ------- (Otherwise the user might skip before some of the work for plotting
! ------- HF eop is done. Also secret test to suppress printing continued
! ------- clock and atm rates.
!
          IF ( KFULLOUT .AND. KSCREEN .AND.  .NOT. KBATCH ) then
               CALL GET_TERMSIZE ( PAGELEN, PAGEWID ) 
               PAGEWID = PAGEWID - 1
               if (pagelen.le.0) pagelen=24
               if (pagewid.lt.79) pagewid=79
               if (pagewid.gt.120) pagewid=120
               lminf=pagelen-6
               last_line=lminf+1
               first_line=last_line-lminf
               if (first_line.lt.1) first_line=1
               call start_mn()
100            continue
               CALL SETCR_MN ( 0, 0 )
               call addstr_f ( "ADJST Control: 'F'-Full Printout, "// &
     &                         "'Return'-Brief " )
               call addstr_f ( "Printout," )
               call nl_mn()
               call addstr_f ( "               'O'-OPTIN, 'P'-Plot residuals" )
               call addstr_f(", 'T'-Terminate" )
               call nl_mn()
               call reverse_on_mn()
               call addstr_f(" '-','+':Line up/down  '<','>':Page up/down  " )
               call addstr_f("'0'-'8' : Start n/8 of way through" )
               call reverse_off_mn()
               call nl_mn()
               iy=pagelen-1
               CALL SETSCRREG_MN ( 4, IY )
               goto 101
!
! ------------ If segemnent eop don't stop here bcause then JPM eop data file
! ------------ is still to be created.
!
99             CONTINUE 
               IF ( .NOT. (EOP_STYLE(1) .EQ. EOP__SEGS_ONLY .OR. &
     &                    EOP_STYLE(1) .EQ. EOP__RATES_AND_SEGS)) THEN
                    CALL SENKRS_MN(IX,IY,IDUM )
                  ELSE
                    CDUM(4:4) = ' '
               ENDIF
               if ( cdum(4:4) .eq. '+' ) then
                    if ( last_line.lt.iptr) then
                          last_line=last_line+1
                          call addstr_f(lbuf(last_line)(:pagewid) )
                          call nl_mn()
                    endif
                    goto 99
                 else if (cdum(4:4).eq.'-') then
                    if (first_line.gt.1) last_line=last_line-1
                    goto 101
                 else if (cdum(4:4).eq.'>') then
                    last_line = last_line+lminf
                    if (last_line.gt.iptr) last_line=iptr
                    goto 101
                 else if (cdum(4:4).eq.'<') then
                    last_line=last_line-lminf
                    if (last_line-lminf.lt.1) last_line=lminf+1
                    goto 101
                 else if (cdum(4:4).ge.'0'.and.cdum(4:4).le.'8') then
                    read(cdum(4:4),'(I1)') n
                   last_line=lminf+(n*(iptr-(lminf+1)))/8+1
                   goto 101
                 else
                   goto 102
               endif
101            continue
               first_line=last_line-lminf
               CALL SETCR_MN ( 0, 4 )
               call clrtobot_mn()
               do i=first_line,last_line
                 call addstr_f(lbuf(i)(:pagewid) )
                 call nl_mn()
               enddo
               GOTO 99
102            CONTINUE
               IF ( CDUM(4:4) .EQ. 'O' ) CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
               IF ( CDUM(4:4) .EQ. 'P' ) THEN
                    CALL RUN_PROG ( 'REPA',  'WAIT', INT2(0) )
                    CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
               END IF
               IF ( CDUM(4:4) .EQ. 'T' ) CALL RUN_PROG ( 'SLEND', 'PASS', INT2(0) )
               IF ( CDUM(4:4) .EQ. 'F' ) then
                    PRINT_CONT = .TRUE.
                  ELSE
                    PRINT_CONT = .FALSE.
               ENDIF
            ELSE IF(KBATCH) THEN
!
! ----------- MWH 910624  Don't print continued clocks and atmospheres even
! ----------- when minimum output is turned off
!
              PRINT_CONT=.FALSE.
          END IF
!
! ------- Write flyby status
!
          IF ( KSPOOL .AND. (KFULLOUT .OR. (IARCNM.EQ.1 .AND. KMINOUT)) ) THEN
!!               WRITE ( 23, 346 ) PRE_SAV_DIR(:PRE_SV_LEN)
!!               WRITE ( 23, 347 ) (FBFNAM(I)( 1:10),I=1,12)
!@               WRITE ( 23, '("Site Plate Map: ",A)') &
!@     &                 PRE_SAV_DIR(1:PRE_SV_LEN)//'/'//SITPL_FIL
!
! ------------ This is done in order to circumvent bug in HP FORTRAN90 2.5.1
! ------------ compiler
!
!!               WRITE ( 23, '("Site Plate Map: ",A,A,A)') &
!!     &                 PRE_SAV_DIR(1:PRE_SV_LEN), '/', SITPL_FIL
!!               WRITE ( 23, '(1X)' )
!!               IF ( FBFNAM(7)(1:5) .EQ. 'NUVEL' ) THEN
!!                    WRITE ( 23, '("Nuvel fixed plate: ",A)' ) NUVEL_FIXED
!!                    WRITE ( 23, '(1X)' )
!!               ENDIF
               WRITE ( 23, '(A)' ) ' '
          END IF
!
          IF ( KSCREEN .AND.KFULLOUT) THEN
            iptr=iptr+1
            WRITE(lbuf(iptr),3460)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
!
            iptr=iptr+1
            WRITE(lbuf(iptr),3460)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
!
            iptr=iptr+1
            WRITE(lbuf(iptr),3461)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
!
            iptr=iptr+1
            WRITE(lbuf(iptr),3460)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
!
            iptr=iptr+1
            WRITE(lbuf(iptr),3462) PRE_SAV_DIR(:PRE_SV_LEN)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
!
            iptr=iptr+1
            WRITE(lbuf(iptr),3460)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
!
            iptr=iptr+1
            WRITE(lbuf(iptr),3463)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
!
            iptr=iptr+1
            WRITE(lbuf(iptr),3464)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
!
            iptr=iptr+1
            WRITE(lbuf(iptr),3465)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
!
            iptr=iptr+1
            WRITE(lbuf(iptr),3466)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
!
            iptr=iptr+1
            WRITE(lbuf(iptr),3471)(FBFNAM(I)( 1:10),I=1,8)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
!
            iptr=iptr+1
            WRITE(lbuf(iptr),3460)
            call addstr_f(lbuf(iptr)(:pagewid) )
            call nl_mn()
          END IF
        ENDIF
      ENDIF
!
      CALL HOL2CHAR( ITBLOUT, INT2(1), INT2(1), TBLOUT )
!
  346 &
     &FORMAT(//,23X,'*** Flyby Status ***', &
     &//,20X,'directory: ',A, &
     &//,'Station   Source    Nutation  ', &
     &   'Nutation  Earth     Earth     ', &
     &   'Station   Pressure  EOP Intp. ', &
     &   'EOP Intp. High Freq Axis      ', &
     & /, &
     &   'Positions Positions Model     ', &
     &   'Time      Rotation  Rotation  ', &
     &   'Velocity  Loading   Smoothing ', &
     &   'Smoothing EOP       Offset    ', &
     & /, &
     &   '                              ', &
     &   'Series    Series    Interpol. ', &
     &   'Model               CALC      ', &
     &   'Mod File  Model     Mod File  ' &
     & /,12('---------',1x))
  347 FORMAT(12(A9,1x)/)
 3460 FORMAT(1X)
 3461 FORMAT(23X,'*** Flyby Status ***')
 3462 FORMAT(20X,'directory: ',A)
!
 3463 FORMAT(1X,'Station   ',1X,'Source    ',1X,'Nutation  ', &
     &       1X,'Nutation  ',1X,'Earth     ',1x,'Earth     ', &
     &       1x,'Station   ',1X,'Pressure  ')
!
 3464 FORMAT(1X,'Positions ',1X,'Positions ',1X,'Model     ', &
     &       1X,'Time      ',1X,'Rotation  ',1x,'Rotation  ', &
     &       1X,'Velocity  ',1x,'Loading   ')
!
 3465 FORMAT(1X,'          ',1X,'          ',1X,'          ', &
     &       1X,'Series    ',1X,'Series    ',1x,'Interpol. ', &
     &       1X,'Model     ')
!
 3466 FORMAT(8(1X,'----------'))
 3471 FORMAT(8(1X,A10))
!
! check whether we have only polynomials only for clocks
!
      IF ( .NOT. KGLOBALS ) THEN
           POLYONLY = .TRUE.
           DO I=1,NUMSTA
!
! ----------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
              IF ( .NOT. CHECK_STABIT ( I ) ) GOTO 810
!
              DO J=1,NUMCLK(I)
                 IF ( KBIT( LCLK(J), INT2(13)) .AND. KBIT(ICLSTA(1,J),I)) &
     &                THEN
                     POLYONLY = .FALSE.
                 ENDIF
              ENDDO
 810          CONTINUE
           ENDDO
      ENDIF
!
! --- Rotate velocities if we are in uen
!
      IF ( KBIT( IUEN, INT2(1) ) ) THEN !uen WAS ESTIMATED
           CALL UEN_VEL ( VSITEV, VSITEC, NUMSTA )
      ENDIF
!
! --- Print spool file header
!
      IF ( KBATCH .AND. KGLOBALS .AND.  CRES_STYLE .NE. CRES__PRE98 ) THEN
           FL_GLOBAL_L1 = .TRUE.
           WRITE ( 23, '(A)' ) ' -------------------------------------'// &
     &                         '--------------------------------------'
           CALL SOLUTION_IDENT ( CRL__ASC, 23, ' ', FL_GLOBAL_L1, TITLE_ANON )
           WRITE ( 23, '(A)' ) ' -------------------------------------'// &
     &                         '--------------------------------------'
!
! -------- Print information about listing status
!
           CALL LISTING_OPTIONS ( 23 )
!
! -------- Get the string with Solve version
!
           CALL ERR_PASS ( IUER, IER )
           CALL GET_SOLVE_VERSION ( RELEASE_DATE, REVISION_DATE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3911, IUER, 'A1JST', 'Error in an '// &
     &              'attempt to learn SOLVE release and revision dates' )
                RETURN
           END IF
!
! -------- Print the date of solve release and solve revision
!
           WRITE ( 23, '(A,A)' ) ' Solve_release:    ', RELEASE_DATE
           WRITE ( 23, '(A,A)' ) ' Solve_revision:   ', REVISION_DATE
           WRITE ( 23, '(A)' ) ' '
      END IF
!
! --- Print header for parameter adjustments
!
      IF ( KSPOOL ) THEN
           IF ( ISLTY2 .EQ. 'I' ) THEN
                SIGMA_LABEL = 'm-sigma'
              ELSE
                SIGMA_LABEL = 'a-sigma'
           ENDIF
           WRITE ( 23, '(A)' ) ' ------------------------------------------------'
           WRITE ( STR, 75 ) IRNCD, PRE_LETRS
   75      FORMAT(" Parameter adjustments for run ", I5, "-", I4, 1X, 'User=',A )
           CALL BLANK_TO_ZERO ( STR(32:41) )
           WRITE ( 23, '(A)' ) STR(1:49)
           WRITE ( 23, 85 ) 'a-sigma', SIGMA_LABEL
   85      FORMAT ( 40X, 'Parameter ', 14X, 'Adjustment', 14X, A, 14X, A )
      END IF
!
      IF ( KSCREEN ) THEN
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), '(A)' ) ' ------------------------------------------------'
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           IPTR=IPTR+1
           STR = GET_VERSION()
           WRITE ( LBUF(IPTR), 3076 ) IRNCD, PRE_LETRS, STR(1:I_LEN(STR))
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
!
           CALL NL_MN()
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), 3078 )
           CALL BLANK_TO_ZERO ( LBUF(IPTR)(32:41) )
           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
           CALL NL_MN()
 3075      FORMAT ( " Parameter adjustments for run ",I5,"-",I4, &
     &              1X,'User=',A,4X,'ADJST Ver. &dB',A, &
     &              /40X,'Parameter ',3X, 'Adjustment', 4X,'     m-sigma')
 3076      FORMAT ( " Parameter adjustments for run ",I5,"-",I4, &
     &              1X,'User=',A,4X,A)
 3078      FORMAT ( 40X,'Parameter ',3X, 'Adjustment',  4X,'     m-sigma')
      ENDIF
!
      IF ( KBATCH .AND. KGLOBALS ) THEN
           IF ( RCOND_CGM .GT. COND_WARNING .AND. COND_WARNING .GT. 1.0D0 ) THEN
                WRITE ( 23, 115 )
 115            FORMAT ( 'WARNING!!! Condition number of the CGM is too ', &
     &                   'large. Results may be corrupted. WARNING!!!' )
           END IF
!
! -------- Printing condition number of the CGM in spool file
!
           WRITE ( 23, "(/,' CGM condition number = ',1PE23.15/1X)" ) RCOND_CGM
      END IF
!
! --- Process the station parameters, including station coordiantes,
! --- antenna axis offsets, clock polynomial coefficients, and
! --- atmosphere parameters
!
      NSCNT1 = 0
      NSCNT1V = 0
      IT_BRK = 0
      DO ISTA=1,NUMSTA        !Run over the list of stations.
!
! ------ Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
         IF ( .NOT. CHECK_STABIT ( ISTA ) ) GOTO 820
!
! ------ See whether this is the reference station for clocks
!
         IF ( .NOT. KGLOBALS ) THEN
              KREFSTA = .TRUE.
              DO J=1, NUMCLK(ISTA)
                 IF ( KBIT(ICLSTA(1,J),ISTA) .AND. KBIT( LCLK(J), INT2(13))) &
     &                KREFSTA = .FALSE.
              ENDDO
         ENDIF
!
! ------ See if the station is in a selected baseline
!
         NEED_SPA = .FALSE.
         USESTA   = .FALSE.
         IF( ISTA .LE. MAX_ARC_STA  .AND.  .NOT. KGLOBALS ) THEN
             DO J = 1,MAX(NUMSTA,MAX_ARC_STA)
                IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------------ Phase dalay solution
!
                     IF ( KBIT(IBLSEL_P(1,ISTA),   J  ) .OR. &
     &                    KBIT(IBLSEL_P(1,J   ),ISTA )       ) USESTA = .TRUE.
                  ELSE
!
! ------------------ Group delay solution
!
                     IF ( KBIT(IBLSEL_G(1,ISTA),   J  ) .OR. &
     &                    KBIT(IBLSEL_G(1,J   ),ISTA )       ) USESTA = .TRUE.
                END IF
             END DO
           ELSE
             USESTA=.TRUE.
         ENDIF
!
! ------ Print the site coordinate parameters and write out type 7
! ------ archive records.
!
         IOFCMP=KBITN( IUEN, INT2(1) )*3
         NPARMC=NPARM
         DO IXYZ = 1,3  ! Run over X,Y,Z.
            IF ( KGLOBALS ) THEN
                 APRIORI_XYZ(IXYZ)=VSITEC(IXYZ,ISTA)
              ELSE
                 APRIORI_XYZ(IXYZ)=NVSITEC(IXYZ,ISTA)
            ENDIF
            IF ( PSITED(ISTA) .EQ. 0 ) THEN
                 SNUM = 0
              ELSE
                 SNUM = PWCNUM(1)-1
            ENDIF
!
            DO JJ = 1,SNUM+1
               STPT2(JJ) = '      '
               VS(IXYZ,JJ)=0.0D0
               IF ( KBIT ( LSITEC(1,IXYZ),ISTA) ) THEN ! The parameter is on!
                    SITES_ESTIMATED = .TRUE.
                    NPARM = NPARM + 1
                    NPX(IXYZ) = NPARM
                    VS(IXYZ,JJ) = MAT(JB+NPARM)
                    IF ( .NOT. KBIT( IUEN, INT2(1) ) ) THEN ! XYZ was estimated
!
! ---------------------- Total site position are
! ---------------------- 1) apriori site position    PLUS
! ---------------------- 2) {apriori velocity}*{difference in time epochs} PLUS
! ---------------------- 3) adjustments
!
                         IF ( KGLOBALS ) THEN
                              VSITEC(IXYZ,ISTA) = APRIORI_XYZ(IXYZ) + &
     &                              (SIT_EST_EPOCH - TIME0*YEAR__TO__DAY)* &
     &                               VSITEV(IXYZ,ISTA)/YEAR__TO__DAY + &
     &                               MAT(JB+NPARM)
                            ELSE
                              CALL OBSTM ( FJD1, LJD1 )
                              FJD1 = (FJD1+LJD1)/2.0D0
!
! ----------------------------- Looks like the old code applied velocities twice:
! ----------------------------- first in NVSITEC, second adding vsitev*tim_dif
!
!@                              VSITEC(IXYZ,ISTA) = APRIORI_XYZ(IXYZ)  + &
!@     &                              (FJD1 - VSITEC_TIM_REF_MJD)* &
!@     &                               VSITEV(IXYZ,ISTA)/YEAR__TO__DAY + &
!@     &                               MAT(JB+NPARM)
!
! --------------------------- New code as of 2024.11.10
!
                              VSITEC(IXYZ,ISTA) = VSITEC(IXYZ,ISTA) + MAT(JB+NPARM)
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!    write ( 23, * ) 'STA: ', ISITN_CHR(ISTA), ' IXYZ= ', IXYZ, ' Apr: ', APRIORI_XYZ(IXYZ), &       ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!     &              ' Dy: ', (FJD1 - VSITEC_TIM_REF_MJD)/YEAR__TO__DAY, ' VS= ', VSITEV(IXYZ,ISTA), ' ADJ= ', MAT(JB+NPARM), &
!     &              ' DS= ', (FJD1 - VSITEC_TIM_REF_MJD)*VSITEV(IXYZ,ISTA)/YEAR__TO__DAY, &
!     &              ' FJD1= ', FJD1, ' LJD1= ', LJD1, &
!     &              ' VSITEC_TIM_REF_MJD = ', VSITEC_TIM_REF_MJD  ! %%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                         END IF
                      ELSE ! UEN was estimated
!
! ---------------------- Rotate apriori velocity from XYZ to UEN
!
                         IF ( KGLOBALS ) THEN
                              CALL MAKE_XYZ_TO_UEN ( APRIORI_XYZ, XYZ_TO_UEN )
                              IER = -1
                              CALL MUL_MV_IV_V ( 3, 3, XYZ_TO_UEN,3, &
     &                             VSITEV(1,ISTA),3, VEC_UEN, IER )
                              VSITEC(IXYZ,ISTA) &
     &                              =(SIT_EST_EPOCH - TIME0*YEAR__TO__DAY)* &
     &                               VEC_UEN(IXYZ)/YEAR__TO__DAY + &
     &                               MAT(JB+NPARM)
                            ELSE
                              VSITEC(IXYZ,ISTA) = MAT(JB+NPARM)
                         ENDIF
                    ENDIF
                    IF ( USESTA ) THEN ! It is from a selected baseline!
!
! -------------------- If in batch, printing globals, and global parm,
! -------------------- then print it, else if not in batch, then print parm
!
                       IF ( KPRINT(NPARM) ) THEN
                          NEED_SPA = .TRUE.
                          STPT2(JJ) = 'Comp'
                          IF ( PSITED(ISTA) .NE. 0 ) THEN
                               FJD_1970 = 2451544.5D0 - 365.25*30.0D0
                               FJD_2030 = 2451544.5D0 + 365.25*50.0D0
!
! ---------------------------- Make check: is the sate of the JJ-th node of
! ---------------------------- piece-wise function in the range
! ---------------------------- [1970.0, 2050.0] year
!
                               IF (  PWCEP(JJ) .LT. FJD_1970  .OR. &
     &                               PWCEP(JJ) .GT. FJD_2030       ) THEN
                                  CALL CLRCH ( STR )
                                  CALL INCH  ( INT4(JJ), STR )
                                  CALL CLRCH ( STR1 )
                                  WRITE ( UNIT=STR1, FMT='(1PD15.7)' ) PWCEP(JJ)
                                  CALL CHASHl ( STR1 )
                                  CALL ERR_LOG ( 3912, IUER, 'A1JST', &
     &                                'Julian date for the '//STR(1:I_LEN(STR))// &
     &                                '-th epoch for '//'the station '// &
     &                                 ISITN_CHR(ISTA)// &
     &                                ' with positions modeled by piece-wise '// &
     &                                'linear function is '//STR1(1:I_LEN(STR1))// &
     &                                ' what is out of range 1970-2030 '// &
     &                                'years. One of the possible reasons: '// &
     &                                'inconsistency of global '// &
     &                                'parameterization in forward and back '// &
     &                                'solutions' )
                                  RETURN
                               END IF
                               CALL MDYJL ( IM, ID, IYR, ITIME, PWCEP(JJ) )
                               WRITE( UNIT=STPT2(JJ), FMT='(I2,I2,I2)' ) &
     &                                IYR, IM, ID
                               CALL BLANK_TO_ZERO ( STPT2(JJ) )
                          ENDIF
                          IF ( VSITED(ISTA) .NE. 0 ) THEN
                               CALL MDYJL ( IM, ID, IYR, ITIME, VSITED(ISTA) )
                               WRITE( UNIT=STPT2(JJ), FMT='(I2,I2,I2)' ) &
     &                                IYR, IM, ID
                               CALL BLANK_TO_ZERO ( STPT2(JJ) )
                          ENDIF
                          IF ( KSPOOL ) THEN
                             IF ( KFIRST ) THEN
                                 IF ( KGLOBALS) THEN
                                      FJD1 = SIT_EST_EPOCH
                                      CALL ERR_PASS ( IUER, IER )
                                      STR = JD_TO_DATE ( SIT_EST_EPOCH-32.184/86400.0D0, IER )
                                      IF ( IER .NE. 0 ) THEN
                                           WRITE ( 6, * ) ' SIT_EST_EPOCH = ', SIT_EST_EPOCH
                                           CALL ERR_LOG ( 3913, IUER, 'A1JST', 'Wrong date' ) 
                                           RETURN 
                                      END IF
                                    ELSE
                                      CALL OBSTM ( FJD1, LJD1 )
                                      FJD1 = (FJD1+LJD1)/2.0D0
                                      CALL ERR_PASS ( IUER, IER )
                                      STR = JD_TO_DATE ( FJD1-32.184/86400.0D0, IER )
                                      IF ( IER .NE. 0 ) THEN
                                           WRITE ( 6, * ) ' FJD1 = ', FJD1
                                           CALL ERR_LOG ( 3914, IUER, 'A1JST', 'Wrong date' ) 
                                           RETURN 
                                      END IF
                                  ENDIF
                                  PREPOCH  = FJD1
                                  PREPOCHV = FJD1
!
                                  WRITE ( 23, 1004 ) STR(1:19)
1004                              FORMAT ( " Station positions are for epoch: ", &
     &                                     A )
                                  KFIRST = .FALSE.
                             ENDIF
                             IF ( ILEN(LMONUMENTS(ISTA)) .EQ. 0 ) THEN
                                  CALL CLRCH ( LMONUMENTS(ISTA) )
                             END IF
                             WRITE(23,1000) NPARM,(ISITN(K,ISTA),K=1,4), &
     &                            (MONUMENTS(K,ISTA),K=1,2), PLATES(ISTA), &
     &                            ICMP(IXYZ+IOFCMP), STPT2(JJ), &
     &                            VSITEC(IXYZ,ISTA)*1000.D0, &
     &                            MAT(JB+NPARM)*1000.D0, MAT(JS+NPARM)*1000.D0, &
     &                            SCSIG(NPARM)*1000.D0, TYPEPR
 1000                             FORMAT ( I5,". ",4A2,1X,2A2,1X,A4,2X,A2,A6,2X, &
     &                                    F16.2,' mm   ', &
     &                                    F12.3,' mm    ', &
     &                                    F15.3,' mm    ', &
     &                                    F15.3,' mm    ',A6)
                          END IF
                          IF ( KSCREEN ) THEN
                             IF ( ILEN(MONUMENTS_CHR(ISTA)) == 0 ) THEN
                                  MONUMENTS_CHR(ISTA) = '9999'
                             END IF
                             IPTR=IPTR+1
                             CALL CLRCH ( LBUF(IPTR) )
                             WRITE( LBUF(IPTR), 3500 ) NPARM, &
     &                            (ISITN(K,ISTA),K=1,4), &
     &                            (MONUMENTS(K,ISTA),K=1,2), PLATES(ISTA), &
     &                            ICMP(IXYZ+IOFCMP), STPT2(JJ), &
     &                            VSITEC(IXYZ,ISTA)*1000.D0, &
     &                            MAT(JB+NPARM)*1000.D0, &
     &                            SCSIG(NPARM)*1000.D0
                             CALL ADDSTR_F ( LBUF(IPTR)(1:PAGEWID) )
                             CALL NL_MN()
 3500                        FORMAT( I5,". ",4A2,1X,2A2,1X,A4,2X,A2,A6,F14.2, &
     &                               ' mm  ',F9.2,' mm ',F9.2,'mm')
                          ENDIF
                       END IF
                    END IF  !It is from a selected baseline
!
! ----------------- Set up ISCNT for BASFE
!
                    NSCNT1 = NSCNT1 + 1
                    ISCNT(1,NSCNT1) = NPARM
                    ISCNT(2,NSCNT1) = ISTA
                    ISCNT(3,NSCNT1) = IXYZ
               END IF     !The parameter is on!
            ENDDO  ! JJ
            IF ( PSITED(ISTA) .NE. 0 ) VSITEC(IXYZ,ISTA) = APRIORI_XYZ(IXYZ)
         END DO  ! IXYZ Run over X,Y,Z
         IF ( NPARMC .NE. NPARM      .AND.( .NOT. KBIT( IUEN, INT2(1) ) ) .AND. &
     &        KPRINT(NPARM)          .AND.USESTA                       ) THEN
!
! ----------- Printing station coordinates in UEN system
!
              CALL COMP ( VS, NPARMC, ISTA, TYPEPR, STPT2, 'mm   ', LSITEC, &
     &                    APRIORI_XYZ, 'Comp', MAT, M_SAV, L_SAV, ADR_SAV, &
     &                    VAL_SAV, LBUF_LEN, LBUF, IPTR, PAGEWID )
         ENDIF
!
! --- Station velocity
!
      NPARMV = NPARM
      DO IXYZ = 1,3 ! Run over X,Y,Z.
         STPT2(1) = '      '
         XYZPRM(IXYZ,ISTA) = 0
         NPX(3+IXYZ) = 0
         VS(IXYZ,1)  = VSITEV(IXYZ,ISTA)
!
         IF (KBIT(LSITEV(1,IXYZ),ISTA)) THEN !The parameter is on!
            NPARM = NPARM + 1
            NPX(3+IXYZ) = NPARM
            XYZPRM(IXYZ,ISTA) = NPARM
            VS(IXYZ,1)     = VSITEV(IXYZ,ISTA) + MAT(JB+NPARM)
            VS2(IXYZ,ISTA) = VSITEV(IXYZ,ISTA) + MAT(JB+NPARM)
!
            IF ( USESTA ) THEN ! It is from a selected baseline!
!
! ------------ If in batch, printing globals, and global parm, then print it
! ------------ else if not in batch, then print parm
!
               IF ( KPRINT(NPARM) ) THEN
                     NEED_SPA = .TRUE.
                     STPT2(1) = '    '
                     IF ( VSITED(ISTA) .NE. 0 ) THEN
                          CALL MDYJL ( IM, ID, IYR, ITIME,VSITED(ISTA) )
                          WRITE( UNIT=STPT2(JJ), FMT='(I2,I2,I2)' ) IYR, IM, ID
                          CALL BLANK_TO_ZERO ( STPT2(JJ) )
                     ENDIF
                 IF (KSPOOL) THEN
                     IF ( ILEN(LMONUMENTS(ISTA)) .EQ. 0 ) THEN
                         CALL CLRCH ( LMONUMENTS(ISTA) )
                     END IF
                     WRITE ( 23, 1001 ) NPARM,(ISITN(K,ISTA),K=1,4), &
     &                     (MONUMENTS(K,ISTA),K=1,2), PLATES(ISTA), &
     &                     ICMP(IXYZ+IOFCMP), STPT2(1), &
     &                     VS(IXYZ,1)*1000.0, MAT(JB+NPARM)*1000.0, &
     &                     MAT(JS+NPARM)*1000.0, SCSIG(NPARM)*1000.0, &
     &                     TYPEPR
 1001                FORMAT (I5,". ",4A2,1X,2A2,1X,A4,2X,A2,'Velo ',A6,1X, &
     &                       F12.2,' mm/yr', &
     &                       F12.3,' mm/yr ', &
     &                       F15.3,' mm/yr ', &
     &                       F15.3,' mm/yr ',A6)
                 END IF
!
               IF ( KSCREEN ) THEN
                    IPTR=IPTR+1
                    WRITE ( LBUF(IPTR), 3501 ) NPARM,(ISITN(K,ISTA),K=1,4), &
     &                     ( MONUMENTS(K,ISTA),K=1,2), PLATES(ISTA), &
     &                     ICMP(IXYZ+IOFCMP), &
     &                     "V ",stpt2(1),VS(IXYZ,1)*1000.0, &
     &                     MAT(JB+NPARM)*1000.0, SCSIG(NPARM)*1000.0
                    CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                    CALL NL_MN()
 3501               FORMAT ( I5,". ",4A2,1X,2A2,1X,A4,2X,A2,A5,A2,F9.2, &
     &                      ' mm/yr ',F9.3,' mm/yr  ',F6.3,' mm/yr')
               ENDIF
             ENDIF
           ENDIF
!
           NSCNT1V = NSCNT1V + 1
           ISCNTV(1,NSCNT1V) = NPARM
           ISCNTV(2,NSCNT1V) = ISTA
           ISCNTV(3,NSCNT1V) = IXYZ
         END IF     !The parameter is on!
      END DO       ! Run over X,Y,Z
!
      N = 0
      DO I=1,3
         DO J=1,I
            N = N+1
            IF ( NPX(3+J) .EQ. 0 ) THEN
                 XYZVEL(N,ISTA) = 0.0D0
              ELSE
                 XYZVEL(N,ISTA) = MAT(JA+INDX4(NPX(3+J),NPX(3+I)))
            ENDIF
         ENDDO
      ENDDO
!
!***  Modified 910625 by MWH to get proper sigmas for station table
!***   when MINIMUM YES is specified
!
      IF ( KSPOOL .AND. USESTA .AND. KGLOBALS .AND. KPRINT(NPARM) ) THEN
           IF ( VSITED(ISTA).GT.0) THEN
                DO I = 1,ISTA-1
                   IF ( EQUAL( ISITN(1,ISTA), INT2(1), ISITN(1,I), INT2(1), &
     &                  INT2(8)) ) GOTO 700
                ENDDO
                I = ISTA
700             CONTINUE
!
                IOLD = I
                DO J=1,3
                   NPX(3+J) = XYZPRM(J,IOLD)
                ENDDO
           ENDIF
!
           DO I=1,6
              DO J=1,I
                 SIGMX(INDX4(J,I))=0
              ENDDO
           ENDDO
!
           DO I=1,6
              IF ( NPX(I) .NE. 0 ) THEN
                   DO J=1,I
                      IF ( NPX(J) .NE. 0 ) THEN
                           SIGMX(INDX4(J,I)) = MAT(JA+INDX4(NPX(J),NPX(I)))
                      ENDIF
                   ENDDO
              ENDIF
           ENDDO
!
           IF ( VSITED(ISTA) .GT. 0 ) THEN
                N = 0
                DO I=4,6
                   DO J=4,I
                      N = N+1
                      SIGMX(INDX4(J,I)) = XYZVEL(N,IOLD)
                   ENDDO
                ENDDO
           ENDIF
      ENDIF
!
      IF ( USESTA .AND. ( .NOT. KBIT( IUEN, INT2(1) ) ) .AND. KPRINT(NPARM)) &
     &     THEN
           IF ( (TBLOUT.EQ.'Y' .OR. MINIMIZE_SIGS ) .AND. KGLOBALS ) THEN
                CALL POSMAP1 ( APRIORI_XYZ, NPARMC, NPARMV, ISTA, TYPEPR, &
     &                         XYZPRM, XYZVEL, SIGMX, MAT )
           END IF
!
           IF ( NPARMV .NE. NPARM ) THEN
!
! ------------- Printing station velocities in UEN system
!
                CALL COMP ( VS, NPARMV, ISTA, TYPEPR, STPT2(1), 'mm/yr', &
     &                      LSITEV, APRIORI_XYZ, 'Velo', MAT, M_SAV, L_SAV, &
     &                      ADR_SAV, VAL_SAV, LBUF_LEN, LBUF, IPTR, PAGEWID )
           END IF
      ENDIF
!
      IF ( USESTA .AND. KGLOBALS .AND. KSPOOL ) THEN
           CALL ERRCMP ( VS, NPARMV, LSITEV, ISTA, MAT )
      END IF
      IF ( KSPOOL .AND. KFULLOUT .AND. USESTA .AND. KGLOBALS .AND. &
     &     KPRINT(NPARM) .AND. OLDNP.NE.NPARM ) THEN
!
           OLDNP=NPARM
           CALL NOUT_R8 ( 6, DUM )
           CALL SCALER  ( SIGMX, DUM, SCALE, 6 )
           CALL ERR_PASS ( IUER, IER )
           CALL ERR_PASS ( IUER, IER )
           STR = JD_TO_DATE ( SIT_EST_EPOCH-32.184/86400.0D0, IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) ' SIT_EST_EPOCH = ', SIT_EST_EPOCH
                CALL ERR_LOG ( 3915, IUER, 'A1JST', 'Wrong date' ) 
                RETURN 
           END IF
           WRITE ( 23, 1002 ) STR(1:19), (SIGMX(I),I=1,21)
1002       FORMAT ( 1X, "Correlations:     (Reference date for site ", &
     &                  "positions: ", A, ")" / &
     &              1X, "X Comp   ", F8.3/ &
     &              1X, "Y Comp   ", 2F8.3/ &
     &              1X, "Z Comp   ", 3F8.3/ &
     &              1X, "X Velo   ", 4F8.3/ &
     &              1X, "Y Velo   ", 5F8.3/ &
     &              1X, "Z Velo   ", 6F8.3 )
           WRITE ( 23, 1003 )
1003       format(12x,"X Comp  Y Comp  Z Comp  X Velo  Y Velo  Z Velo")
      ENDIF
!
      IF ( USESTA .AND. (.NOT.KBIT( IUEN, INT2(1) )) .AND. KPRINT(NPARM) ) THEN
           IF ( TBLOUT .EQ. 'Y' .AND. KGLOBALS ) THEN
                CALL POSMAP2 ( APRIORI_XYZ, NPARMC, NPARMV, ISTA, TYPEPR, &
     &               STPT2, INT2(1), LBUF_LEN, LBUF, IPTR, PAGEWID )
           END IF
!
           IF ( MINIMIZE_SIGS ) THEN
                VELSET = KBIT(LSITEV(1,1),ISTA)
                IF ( VSITED(ISTA) .NE. 0 ) CALL SBIT ( LSITEV(1,1), ISTA, &
     &               INT2(1) )
                CALL POSMAP2 ( APRIORI_XYZ, NPARMC, NPARMV, ISTA, TYPEPR, &
     &               STPT2, INT2(2), LBUF_LEN, LBUF, IPTR, PAGEWID )
                IF ( .NOT. VELSET ) CALL SBIT ( LSITEV(1,1), ISTA, INT2(0) )
           ENDIF
      ENDIF
!
! --- Diurnal radial sine and cosine
!
      IF(KBIT( IUEN, INT2(2) )) THEN
        DO I=1,2
          NPARM=NPARM+1
          IF ( KSPOOL ) THEN
               WRITE(23,1051) NPARM,(ISITN(K,ISTA),K=1,4), SOID(I), &
     &         MAT(JB+NPARM), MAT(JB+NPARM), MAT(JS+NPARM),SCSIG(NPARM)
 1051          FORMAT ( I5,". ",4A2,1X, "U Diurnal ",A6, 2X,4(F16.4, &
     &                  ' m    ') )
          ENDIF
          IF ( KSCREEN ) THEN
               IPTR=IPTR+1
               WRITE ( LBUF(IPTR), 1052 ) NPARM, (ISITN(K,ISTA),K=1,4), &
     &                 SOID(I), MAT(JB+NPARM), MAT(JB+NPARM), SCSIG(NPARM)
               CALL ADDSTR_F( LBUF(IPTR)(:PAGEWID) )
               CALL NL_MN()
 1052          FORMAT ( I5, ". ",4A2,1X, "U Diurnal ",A6, 1X,F14.3, &
     &                      ' m   ',F9.3,' m  ',F9.3,' m ')
          ENDIF
        ENDDO
      END IF
!
      IF ( FL_HPESOL ) THEN
!
! -------- Special trick in orer to overcome a crude mistake in desiging
! -------- Solve: if a station was modeled with eposidic motion(s), 
! -------- it enters the station list more than once.
!
           FL_STA_USED = .FALSE.
           IF ( ISTA > 1 ) THEN
                DO JSTA=1,ISTA-1
                   IF ( ISITN_CHR(JSTA) == ISITN_CHR(ISTA) ) FL_STA_USED = .TRUE.
                END DO
           END IF
!
           DO IHPE = 1,L_HPE ! Running over Harmonics
              DO IXYZ = 1,3 ! Running over X,Y,Z coordinates!
                 IF ( HPESOL(ISTA,IHPE)%FL_EST  .AND.  .NOT. FL_STA_USED ) THEN
                      NPARM = NPARM + 1
                      CALL CLRCH ( CNAME )
                      CNAME(1:8)   = ISITN_CHR(ISTA)
                      CNAME(10:10) = LCMP(IXYZ)
                      CNAME(11:12) = 'C '
                      CNAME(13:20) = HPESOL(ISTA,IHPE)%NAME
!
                      IF ( KGLOBALS .AND. KSPOOL ) THEN
                           WRITE ( 23, 1810 ) NPARM, CNAME,       &
     &                                        MAT(JB+NPARM)*1.D3, &
     &                                        MAT(JB+NPARM)*1.D3, &
     &                                        MAT(JS+NPARM)*1.D3, &
     &                                        SCSIG(NPARM)*1.D3
 1810                      FORMAT ( I5, ". " ,A20, 14X, 2(F13.3, ' mm '), &
     &                              2(5X,F13.3, ' mm '), '   global' )
                      ENDIF
!
                      IF ( KGLOBALS .AND. KSCREEN ) THEN
                           IPTR=IPTR+1
                           WRITE ( LBUF(IPTR), 1820 ) NPARM, CNAME, &
     &                             MAT(JB+NPARM)*1.D3, MAT(JB+NPARM)*1.D3, &
     &                             SCSIG(NPARM)*1.D3
                           CALL ADDSTR_F( LBUF(IPTR)(:PAGEWID) )
                           CALL NL_MN()
 1820                      FORMAT ( I5, ". ",A20, 1X,F14.3, ' mm  ',F9.3, &
     &                              ' m  ',F9.3,' m ')
                      ENDIF
!
                      NPARM = NPARM + 1
                      CNAME(11:12) = 'S '
                      IF ( KGLOBALS .AND. KSPOOL ) THEN
                           WRITE ( 23, 1810 ) NPARM, CNAME,       &
     &                                        MAT(JB+NPARM)*1.D3, &
     &                                        MAT(JB+NPARM)*1.D3, &
     &                                        MAT(JS+NPARM)*1.D3, &
     &                                        SCSIG(NPARM)*1.D3 
                      ENDIF
!
                      IF ( KGLOBALS .AND. KSCREEN ) THEN
                           IPTR=IPTR+1
                           WRITE ( LBUF(IPTR), 1820 ) NPARM, CNAME, &
     &                             MAT(JB+NPARM)*1.D3, MAT(JB+NPARM)*1.D3, &
     &                             SCSIG(NPARM)*1.D3
                           CALL ADDSTR_F( LBUF(IPTR)(:PAGEWID) )
                           CALL NL_MN()
                      ENDIF
                 ENDIF
              END DO 
           END DO
      END IF
!
      IF ( FL_SPESOL ) THEN
           DO ISPE = 1,L_SPE ! Running over stations with spline pos parameterization
              DO IXYZ = 1,3 ! Running over X,Y,Z coordinates!
                 IF ( SPESOL(ISPE)%IND_STA == ISTA  .AND.  KGLOBALS ) THEN
                      DO INOD = 1-SPESOL(ISPE)%DEGREE,SPESOL(ISPE)%L_NOD-1
                         IF ( SPESOL(ISPE)%USED(INOD) ) THEN
                              NPARM = NPARM + 1
                              CALL CLRCH ( CNAME )
                              CNAME(1:8)   = ISITN_CHR(ISTA)
                              CNAME(10:10) = LCMP(IXYZ)
                              CNAME(11:16) = 'BSPLN '
                              CALL INCH ( INOD, CNAME(17:20) )
                              CALL CHASHR     ( CNAME(17:20) )
                              IF ( KSPOOL ) THEN
                                   WRITE ( 23, 1810 ) NPARM, CNAME,       &
     &                                                MAT(JB+NPARM)*1.D3, &
     &                                                MAT(JB+NPARM)*1.D3, &
     &                                                MAT(JS+NPARM)*1.D3, &
     &                                                SCSIG(NPARM)*1.D3
                              ENDIF
!
                              IF ( KSCREEN ) THEN
                                   IPTR=IPTR+1
                                   WRITE ( LBUF(IPTR), 1820 ) NPARM, CNAME, &
     &                                     MAT(JB+NPARM)*1.D3, &
     &                                     MAT(JB+NPARM)*1.D3, &
     &                                     SCSIG(NPARM)*1.D3
                                   CALL ADDSTR_F( LBUF(IPTR)(:PAGEWID) )
                                   CALL NL_MN()
                              ENDIF
                         ENDIF
                      END DO
                    ELSE IF ( SPESOL(ISPE)%IND_STA == ISTA  .AND.  &
     &                        .NOT. KGLOBALS                       ) THEN
                      DO INOD = 1-SPESOL(ISPE)%DEGREE,SPESOL(ISPE)%L_NOD-1
                         IF ( SPESOL(ISPE)%USED(INOD) ) THEN
                              NPARM = NPARM + 1
                         END IF
                      END DO
                 END IF
              END DO
           END DO
       END IF
!
! ---- Print the axis offset parameter
!
       IF ( KBIT(LAXOF(1), ISTA) ) THEN
            IF ( ISTA .GT. 1 ) THEN
                 DO JSTA=1,ISTA-1
                    IF ( ISITN_CHR(ISTA) .EQ. ISITN_CHR(JSTA) ) GOTO 1101
                 ENDDO
            END IF
            NPARM=NPARM+1
            VAXOF(ISTA) = VAXOF(ISTA) + MAT(JB+NPARM)
            IF ( USESTA .AND. KPRINT(NPARM) ) THEN
!
                 IF ( KSPOOL ) THEN
                      WRITE(23,1100) NPARM,(ISITN(K,ISTA),K=1,4), &
     &                VAXOF(ISTA)*1D3, MAT(JB+NPARM)*1D3, MAT(JS+NPARM)*1D3, &
     &                SCSIG(NPARM)*1D3, TYPEPR
 1100                 FORMAT ( I5,". ",4A2,1X, "Axis Offset",10X, &
     &                         F16.2, &
     &                         " mm   "F12.3, &
     &                         " mm    "F15.3, &
     &                         " mm    "F15.3," mm    ",A6)
                 ENDIF
!
                 IF ( KSCREEN ) THEN
                      IPTR=IPTR+1
                      WRITE ( LBUF(IPTR), 3188) NPARM, (ISITN(K,ISTA),K=1,4), &
     &                   VAXOF(ISTA)*1D3, MAT(JB+NPARM)*1D3, SCSIG(NPARM)*1D3
                      CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                      CALL NL_MN()
 3188                 FORMAT ( I5, ". ", 4A2, 1X, "Axis Offset", 6X, F16.2, &
     &                             " mm  ", F9.2, " mm ", F9.2, " mm" )
                 END IF
            ENDIF
       ENDIF
1101   CONTINUE
!
! ---- Print the clock polynomial coefficient parameters
!
! ---- First zero out the arrays for passing info to MDLPL, if neccessary.
!
       IF ( .NOT. KBATCH ) THEN
            DO J1=1,NUM_CLO
               CLOCK_VALUES(J1,ISTA) = 0.D0
               CLOCK_SIGMAS(J1,ISTA) = 0.D0
            ENDDO
       ENDIF
!
       IF ( UNF_CLO  .AND.  ISTA .LE. MAX_ARC_STA  ) THEN
          IF ( NUMCLK(ISTA) .GT. 0  .AND. &
     &         KBIT(ICLSTA(1,1+ICLSTR(ISTA)), ISTA) ) THEN
!
! ========= Case of uniform segemted clock
!
! --------- Translate time tag of initial epoch
!
! --------- Cycle on global clock polinomial
!
            DO 410 J1=0,NUM_BRK(ISTA)
               IF ( J1 .EQ. 0 ) THEN
                    JD_USE = JDATE_CLO(1) 
                 ELSE
                    JD_USE = JDATE_BRK(J1,ISTA) 
               END IF
               CALL EPOC ( IM, ID, IYR, IHR, IMIN, JD_USE )
!
               CL_SHF(J1) = 0.0D0
               CL_DRF(J1) = 0.0D0
               CL_SQU(J1) = 0.0D0
!
               DO 420 J2=0,NPL_CLO
                  NPARM = NPARM + 1
                  IF ( USESTA ) THEN  ! This station is in a selected baseline
!
! ------------------ Storing index of the parameter
!
                     ICLK_BRK(J1,J2)=NPARM
!
! ------------------ Parameters scaling
!
                     MAT(JB+NPARM) = MAT(JB+NPARM)/8.64D4**J2
                     MAT(JS+NPARM) = MAT(JS+NPARM)/8.64D4**J2
                     SCSIG(NPARM)  = SCSIG(NPARM)/8.64D4**J2
                     PVAL          = MAT(JB+NPARM)
                     LET           = 'CL'
                     IF ( J1 .GT. 0 ) LET = 'BR'
!
                     IF ( J2 .EQ. 0 ) THEN
!
! ----------------------- Clock offset
!
                          CORR   = MAT(JB+NPARM)*1.D9
                          XSIG   = MAT(JS+NPARM)*1.D9
                          XSSIG  = SCSIG(NPARM)*1.D9
                          CL_SHF(J1) = MAT(JB+NPARM)
                          UNIT   = ' ns      '
                       ELSE IF ( J2 .EQ. 1 ) THEN
!
! ----------------------- Clock drift
!
                          CORR   = MAT(JB+NPARM)*1.D14
                          XSIG   = MAT(JS+NPARM)*1.D14
                          XSSIG  = SCSIG(NPARM)*1.D14
                          CL_DRF(J1) = MAT(JB+NPARM)
                          UNIT   = ' D-14    '
                          NPARM_C1 = NPARM
                       ELSE IF ( J2 .EQ. 2 ) THEN
!
! ----------------------- Frequency drift
!
                          CORR   = MAT(JB+NPARM)*1.D14*86400.D0
                          XSIG   = MAT(JS+NPARM)*1.D14*86400.D0
                          XSSIG  = SCSIG(NPARM)*1.D14*86400.D0
                          CL_SQU(J1) = MAT(JB+NPARM)
                          UNIT   = ' D-14/day'
                          NPARM_C2 = NPARM
                     END IF
!
! ------------------ Print them to spool file or on the screen
!
                     IF ( KSPOOL ) THEN
                          CALL CLRCH ( STR )
                          IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                               WRITE ( STR, 110 ) NPARM, (ISITN(L,ISTA), L=1,4), &
     &                                 LET, J2, IYR, IM, ID, IHR, IMIN, &
     &                                 CORR, UNIT, XSIG, UNIT, XSSIG, UNIT
 110                           FORMAT ( I5, ". ", 4A2, 1X, A2, I2, " ", &
     &                                  2(I2,'/'), I2, 1X, I2, ':', I2, &
     &                                  21X, (F15.3,A), 2(F13.3,A) )
!
! ---------------------------- Add leading zeroes
!
                               CALL BLANK_TO_ZERO ( STR(22:29) )
                               CALL BLANK_TO_ZERO ( STR(31:35) )
                             ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
                               CALL ERR_PASS ( IUER, IER )
                               WRITE ( STR, 113 ) NPARM, ISITN_CHR(ISTA),  &
     &                                 LET, J2, JD_TO_DATE ( JD_USE-32.184/86400.0D0, IER ), &
     &                                 CORR, UNIT, XSIG, UNIT, XSSIG, UNIT
 113                           FORMAT ( I5, ". ", A, 1X, A2, I2, 1X, &
     &                                  A, 12X, (F15.3,A), 2(F13.3,A) )
                               IF ( IER .NE. 0 ) THEN
                                    WRITE ( 6, * ) ' JD_USE = ', JD_USE 
                                    CALL ERR_LOG ( 3916, IUER, 'A1JST', 'Wrong date' ) 
                                    RETURN 
                               END IF
                          END IF
                          WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                     ENDIF
!
                     IF ( KSCREEN ) THEN
                          IPTR=IPTR+1
                          WRITE ( LBUF(IPTR),120 ) NPARM,(ISITN(L,ISTA),L=1,4), &
     &                            LET, J2, IYR, IM, ID, IHR, IMIN, CORR, UNIT, &
     &                         XSSIG, UNIT
 120                      FORMAT ( I5, ". ", 4A2, 1X, A2, I2, " ", &
     &                             2(I2,'/'), I2, 1X, I2, ':', I2, &
     &                             10X, F15.2, A, 4X, F6.2, A )
                          CALL BLANK_TO_ZERO ( LBUF(IPTR)(22:29) )
                          CALL BLANK_TO_ZERO ( LBUF(IPTR)(31:35) )
                          CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                          CALL NL_MN()
                     ENDIF
                  END IF
 420           CONTINUE
 410        CONTINUE
!
! --------- Segmented clocks
!
            CONST = DBLE(SCCNST(ISTA) ) *(CLO_INTERVAL*24.D0) * 3600.D-14
!
! --------- Set nparm on the index just before the first epoch of clock
!
            NPARM = NPARM - (NPL_CLO+1)*(NUM_BRK(ISTA) + 1)
            IBRK = 0
            DO 430 J3=1,NUM_CLO
               NPARM = NPARM + 1
!
! ------------ Tricky place: global clock polinomials are located between the
! ------------ first time epoch end the second
!
               IF ( J3 .EQ. 1 ) THEN
                    IND_PRV = NPARM
                    IND_CUR = NPARM
                    IND_FTR = NPARM + (NPL_CLO+1)*(NUM_BRK(ISTA) + 1)
                 ELSE IF ( J3 .EQ. 2 ) THEN
                    IND_PRV = NPARM -1
                    NPARM   = NPARM + (NPL_CLO+1)*(NUM_BRK(ISTA) + 1) -1
                    IND_CUR = NPARM
                    IND_FTR = NPARM + 1
                 ELSE
                    IND_PRV = NPARM - 1
                    IND_CUR = NPARM
                    IND_FTR = NPARM + 1
               END IF
!
! ------------ Calculation indeces for previous, current and future epoch
! ------------ for j3-th time epoch
!
               N1 = INDX8(IND_CUR,IND_CUR)
               N2 = INDX8(IND_PRV,IND_CUR)
               N3 = INDX8(IND_FTR,IND_CUR)
!
! ------------ Printout the clock adjustment if neccessary.
!
               CORR  = MAT(JB+IND_CUR)*1.D9
               XSIG  = MAT(JS+IND_CUR)*1.D9
               XSSIG = XSIG*SCALE_FACTOR
               J2 = 0
               UNIT  = ' ns      '
               IF ( .NOT. KBATCH  .AND.  J3 .LE. MAX_SEGMENTS ) THEN
!
! --------------- Calculate the value of total clocks as if no clock breaks have
! --------------- been detected
!
                  TIME_LAPSED = 86400.D0*(JDATE_CLO(J3) - JDATE_CLO(1) )
!
! --------------- ... without permanent shift since it is taken into account in
! --------------- segmented clocks
!
                  TCLOCK_VALUES(J3,ISTA) = CL_DRF(0) * TIME_LAPSED + &
     &                                     CL_SQU(0) * TIME_LAPSED**2
                  BCLOCK_VALUES(J3,ISTA) = 0.D0
!
                  L_CNT=1
                  IPAR_CLK(L_CNT) = IND_CUR
                  CLK_CNT(L_CNT)  = 1.D0
!
! --------------- NB: we start count degrees from 1: free term is taken into
! --------------- account by linear spline
!
                  DO IDEG=1,NPL_CLO
                     L_CNT = L_CNT + 1
                     IPAR_CLK(L_CNT) = ICLK_BRK(0,IDEG)
                     CLK_CNT(L_CNT)  = (TIME_LAPSED/86400.D0)**IDEG
                  END DO
!
! --------------- Calculate the index of the clock break for the J3-th epoch
!
                  IF ( IBRK .LT. NUM_BRK(ISTA)  .AND.  J3 .GT. 1 ) THEN
                     IF ( JDATE_CLO(J3) .GT. JDATE_BRK(IBRK+1,ISTA)  ) THEN
!
! --------------------- Clock break occurred just between the current clock
! --------------------- epoch and the previous one
!
                        IBRK = IBRK + 1
!
! --------------------- Calculate polynomial component of the clock function
! --------------------- to the epoch 1 sec before the clock break
!
                        IT_BRK = IT_BRK + 1
                        IST_BRK(IT_BRK) = ISTA
                        JD_BRK(IT_BRK)  = JDATE_BRK(IBRK,ISTA) - 1.D-5
                        TIME_LAPSED = 86400.D0*( JD_BRK(IT_BRK) - JDATE_CLO(1) )
                        TC_BRK(IT_BRK) = CL_DRF(0) * TIME_LAPSED + &
     &                                   CL_SQU(0) * TIME_LAPSED**2
                        BC_BRK(IT_BRK) = 0.D0
                        BS_BRK(IT_BRK) = 0.D0
!
                        IF ( IBRK .GT. 1 ) THEN
                             DO ICNT=1,IBRK-1
                                TIME_LAPSED = 86400.D0*(JD_BRK(IT_BRK) - &
     &                                                  JDATE_BRK(ICNT,ISTA) )
                                TC_BRK(IT_BRK) = TC_BRK(IT_BRK)             + &
     &                                           CL_SHF(ICNT)               + &
     &                                           CL_DRF(ICNT) * TIME_LAPSED + &
     &                                           CL_SQU(ICNT) * TIME_LAPSED**2
                                BC_BRK(IT_BRK) = BC_BRK(IT_BRK)             + &
     &                                           CL_SHF(ICNT)
                             ENDDO
                        END IF
!
! --------------------- Calculate piece-wise component of clock function to the
! --------------------- epoch  just 1 sec before the clock break
!
                        CLS_PRV  = MAT(JB+IND_PRV)
                        CLS_CUR  = MAT(JB+IND_CUR)
                        CLS_BRK  = ( CLS_CUR - CLS_PRV)* &
     &                             (JD_BRK(IT_BRK) - JDATE_CLO(J3-1))/ &
     &                             (JDATE_CLO(J3)  - JDATE_CLO(J3-1))
                        TC_BRK(IT_BRK)  = TC_BRK(IT_BRK) + CLS_BRK
                        BC_BRK(IT_BRK)  = BC_BRK(IT_BRK) + CLS_BRK
!
! --------------------- Calculate the clock function to the moment just 
! --------------------- 1.0 sec after the clock break
!
                        IT_BRK = IT_BRK + 1
                        IST_BRK(IT_BRK) = ISTA
                        JD_BRK(IT_BRK)  = JDATE_BRK(IBRK,ISTA) + 1.0D0/86400.0D0
                        TC_BRK(IT_BRK)  = TC_BRK(IT_BRK-1) + CL_SHF(IBRK)
                        BC_BRK(IT_BRK)  = BC_BRK(IT_BRK-1) + CL_SHF(IBRK)
                     END IF
                  END IF
!
                  IF ( IBRK .GT. 0 ) THEN
!
! -------------------- Take into account the influence of clock breaks at the
! -------------------- value of total clocks for the curent clock epoch
!
                       DO ICNT=1,IBRK
                          TIME_LAPSED = 86400.D0*(JDATE_CLO(J3) - &
     &                                            JDATE_BRK(ICNT,ISTA) )
                          TCLOCK_VALUES(J3,ISTA) = TCLOCK_VALUES(J3,ISTA)     + &
     &                                             CL_SHF(ICNT)               + &
     &                                             CL_DRF(ICNT) * TIME_LAPSED + &
     &                                             CL_SQU(ICNT) * TIME_LAPSED**2
                          BCLOCK_VALUES(J3,ISTA) = BCLOCK_VALUES(J3,ISTA) + &
     &                                             CL_SHF(ICNT)
!
! ----------------------- NB: we start count degrees from ZERO! Free term is
! ----------------------- taken into account in clok break
!
                          DO IDEG=0,NPL_CLO
                             L_CNT = L_CNT + 1
                             IPAR_CLK(L_CNT) = ICLK_BRK(ICNT,IDEG)
                             CLK_CNT(L_CNT)  = (TIME_LAPSED/86400.0)**IDEG
                          END DO
                       ENDDO
                  END IF
!
! --------------- Get the sigma of the whole clock model at this epoch.
! --------------- To do it we generated previously the array IPAR_CLK with
! --------------- indices of parameters entering the linear expression for
! --------------- calculation value of the total clock function at the current
! --------------- epoch: CLO = CLK_CNT(1)*EST(ipar_clk(1)) +
! ---------------              CLK_CNT(2)*EST(ipar_clk(2)) + ... +
! ---------------              CLK_CNT(L_CNT)*EST(ipar_clk(l_cnt))
! --------------- where CLK_CNT -- array of coefficients, EST -- array of
! --------------- extimates.
! ---------------
! --------------- Sigma**2 = clk_cnt(T) * COV * clk_cnt
! --------------- where COV is a submatrix from the full covariance matrix
! --------------- produced from covariance for the subvector EST(ipar_clk(1)),
! --------------- EST(ipar_clk(2)), EST(ipar_clk(l_cnt)),
! ---------------
! --------------- First create a covariance submatrix CLK_COV
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL CREATE_COVARIANCE_MATRIX ( L_CNT, IPAR_CLK, MAT, CLK_COV, &
     &                                            IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 3917, IUER, 'A1JST', 'Error in '// &
     &                     'creation of covariance matrix for clocks' )
                       RETURN
                  END IF
!
! --------------- Then execute multiplication CLK_COV * CLK_CNT (matrix by
! --------------- vector) and execute a dot produce between CLK_CNT and the
! --------------- result vector of the previous operation
!
                  FULL_CLOCK_SIG = DSQRT ( MULT_VMV ( L_CNT, CLK_CNT, CLK_COV, &
     &                                                CLK_CNT) )*1.D12
!
! --------------- Convert values of segmented component of clock finction and
! --------------- sigmas of total clock function to ps.
!
                  CLOCK_VALUES(J3,ISTA)  = CORR *1.D-9
                  CLOCK_SIGMAS(J3,ISTA)  = FULL_CLOCK_SIG *1.D-12
!
                  TCLOCK_VALUES(J3,ISTA) = TCLOCK_VALUES(J3,ISTA) + &  ! In sec
     &                                     CLOCK_VALUES(J3,ISTA)
                  BCLOCK_VALUES(J3,ISTA) = BCLOCK_VALUES(J3,ISTA) + &  ! In sec
     &                                     CLOCK_VALUES(J3,ISTA)
                ENDIF
!
                CALL EPOC ( IM, ID, IYR, IHR, IMIN, JDATE_CLO(J3) )
!
! ------------- Printing the estimates in the mode "SEG_OUTPUT" on
!
                IF ( SEG_OUTPUT  .AND.  KSPOOL .AND. J3.GT.1 ) THEN
                     LET = 'cl'
                     CALL CLRCH ( STR )
                     IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                          WRITE ( STR, 110 ) NPARM, (ISITN(L,ISTA), L=1,4), &
     &                                       LET, J2, IYR, IM, ID, IHR, IMIN, &
     &                                       CORR, UNIT, XSIG, UNIT, XSSIG, UNIT
!
! ----------------------- Add leading zeroes
!
                          CALL BLANK_TO_ZERO ( STR(22:29) )
                          CALL BLANK_TO_ZERO ( STR(31:35) )
                        ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
                          CALL ERR_PASS ( IUER, IER )
                          WRITE ( STR, 113 ) NPARM, ISITN_CHR(ISTA),  &
     &                            LET, J2, &
     &                            JD_TO_DATE ( JDATE_CLO(J3)-32.184/86400.0D0, IER ), &
     &                            CORR, UNIT, XSIG, UNIT, XSSIG, UNIT
                          IF ( IER .NE. 0 ) THEN
                               WRITE ( 6, * ) ' JDATE_CLO(J3) = ', JDATE_CLO(J3)
                               CALL ERR_LOG ( 3918, IUER, 'A1JST', 'Wrong date' ) 
                               RETURN 
                          END IF
                      END IF
                      WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                ENDIF
!
                IF ( SEG_OUTPUT  .AND.  KSCREEN .AND. J3.GT.1 ) THEN
                     IPTR=IPTR+1
                     LET = 'cl'
                     WRITE ( LBUF(IPTR),120 ) NPARM,(ISITN(L,ISTA),L=1,4), &
     &                       LET, J2, IYR, IM, ID, IHR, IMIN, CORR, UNIT, &
     &                       XSSIG, UNIT
                     CALL BLANK_TO_ZERO ( LBUF(IPTR)(22:29) )
                     CALL BLANK_TO_ZERO ( LBUF(IPTR)(31:35) )
                     CALL ADDSTR_F(LBUF(IPTR)(:PAGEWID) )
                     CALL NL_MN()
                ENDIF
!
! ------------- Calculation contribution to RMS of variation of the estimates
! ------------- of clock ( CLKRMS )
!
                IF ( J3 .GT. 1 ) THEN
                     CLKRMS(ISTA) = CLKRMS(ISTA) + &
     &                 ( ( MAT(JB+IND_CUR)*1.D9 - MAT(JB+IND_PRV)*1.D9) &
     &                   /( CLO_INTERVAL*24.D0 )                           )**2
                     CLKCNTX(ISTA) = CLKCNTX(ISTA) + 1
                ENDIF
!
! ------------- Calculation TRACE -- contribution to mathematical expectation
! ------------- of square of postfit residuals due to constraints imposed on
! ------------- clock rate
!
                IF ( J3 .EQ.1 ) THEN
                     TRACE = ( MAT(JA+N1) - MAT(JA+N3) ) /CONST**2
                   ELSE IF ( J3 .LT. NUM_CLO ) THEN
                     TRACE = ( 2*MAT(JA+N1) - MAT(JA+N2) - MAT(JA+N3) )/CONST**2
                  ELSE
                     TRACE = ( MAT(JA+N1) - MAT(JA+N2) ) /CONST**2
                ENDIF
                IF ( CONST .EQ. 0 ) THEN
                     TRACE = 0.D0
                ENDIF
!
               CLKTRA(ISTA) = CLKTRA(ISTA) + TRACE
               CLKCNT(ISTA) = CLKCNT(ISTA) + 1
 430        CONTINUE
!
! --------- Calculation CLKSTA -- scalled CLKRMS (ratio of a posteriori RMS
! --------- of clock function and apriori "sigma" )
!
            IF ( CONST .GT. 1.D-30 ) THEN
                 CLKSTA(ISTA) = CLKRMS(ISTA)/SCCNST(ISTA)**2
            END IF
          END IF
       END IF
!!
       IF ( .NOT. UNF_CLO   .AND.  ISTA .LE. MAX_ARC_STA ) THEN
         LS_CHR = '  '
         LC_CHR = '  '
!
! ------ Case of non-uniform clocks
!
         KEPOCHS=.FALSE.
         DO JCLOCK = 1,NUMCLK(ISTA)  !Run over the clocks for this station
            KCLOCK = JCLOCK + ICLSTR(ISTA)
            IF ( KBIT(ICLSTA(1,KCLOCK),ISTA) ) THEN
               KEPOCHS=.TRUE.
!!               CALL EPOC(IM,ID,IYR,IHR,IMIN,FJDCL(KCLOCK))
               FJD8 = .5+AINT(FJDCL(KCLOCK)-.5+2.D-5)
               CALL MDYJL(IM,ID,IYR,ITIME,FJD8 )
               FJD8=FJD8-2.D-5
               IHR = (FJDCL(KCLOCK)-FJD8) * 24.0D0 + .1D-7
               IMIN =((FJDCL(KCLOCK)-FJD8)-IHR/24.0D0)*1440.0D0+0.1D-7
               ICLCK = 0
               DO M=1,ICLMAX  !Run over the polynomial terms for this epoch
                  LS_CHR = '  ' ! Initialization
                  LC_CHR = '  '
                  IF ( KBIT(LCLK(KCLOCK),M) ) THEN ! Its turned on.
                     ICLCK = 1
                     NPARM = NPARM + 1
                     MAT(JB+NPARM) = MAT(JB+NPARM)/8.64D4**(M-1)
                     MAT(JS+NPARM) = MAT(JS+NPARM)/8.64D4**(M-1)
                     LP = M-1
                     SCSIG(NPARM) = SCSIG(NPARM)/8.64D4**LP
                     LPC = 2 - M
                     PVAL=MAT(JB+NPARM)
!
! ------------------ Check if this clock polynomial INCLUDED a continued
!                    SIN/COS estimation.
!
                     LS_CHR = '  '
                     LC_CHR = '  '
                     IF ( KBIT( LCLK(KCLOCK), INT2(14)) .AND. &
     &                    KBIT( LCLK(KCLOCK), INT2(15))       ) LS = INT2_S
                     IF ( KBIT( LCLK(KCLOCK), INT2(14)) .AND. &
     &                    KBIT( LCLK(KCLOCK), INT2(15))       ) LC = INT2_C
!
! ----------------- Change units and use special formats for clock offset,
! ----------------- rate, and frequency drift.
!
                    IF ( USESTA ) THEN  ! This station is in a selected baseline
                       IF (M .EQ.1 ) THEN    ! Handle Offset
                          CORR = MAT(JB+NPARM)*1.D9
                          XSIG = MAT(JS+NPARM)*1.D9
                          XSSIG = SCSIG(NPARM)*1.D9
                          IF (KSPOOL) THEN
                             IF (   .NOT. KBIT( LCLK(KCLOCK), INT2(13)) .OR. &
     &                              SEG_OUTPUT .OR. &
     &                            ( PRINT_CONT .AND. .NOT. KMINOUT) )THEN
                                  CALL CLRCH ( STR )
                                  IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                                       WRITE ( STR, 4201 ) NPARM,(ISITN(L,ISTA), &
     &                                    L=1,4), LP, IYR, IM, ID, IHR, IMIN, &
     &                                    CORR, XSIG, XSSIG, LS, LC
 4201                                  FORMAT ( I5,". ",4A2,1X,"CL",I2," ", &
     &                                          2(I2,'/'),I2,1X,I2,':',I2,1X, &
     &                                          22X,3(F13.3,' ns      '),2A2)
!
! -----------------------------------  Add leading zeroes
!
                                       CALL BLANK_TO_ZERO ( STR(22:29) )
                                       CALL BLANK_TO_ZERO ( STR(31:35) )
                                     ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
                                       LET = 'CL'
                                       UNIT   = ' ns      '
                                       CALL ERR_PASS ( IUER, IER )
                                       WRITE ( STR, 113 ) NPARM, &
     &                                       ISITN_CHR(ISTA), LET, LP, &
     &                                       JD_TO_DATE ( FJDCL(KCLOCK)-32.184/86400.0D0, IER ), &
     &                                       CORR, UNIT, XSIG, UNIT, XSSIG, UNIT
                                       IF ( IER .NE. 0 ) THEN
                                            WRITE ( 6, * ) ' FJDCL(KCLOCK) = ', FJDCL(KCLOCK)
                                            CALL ERR_LOG ( 3919, IUER, 'A1JST', 'Wrong date' ) 
                                            RETURN 
                                       END IF
                                  END IF
                                  WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                             ENDIF
                          ENDIF
                          IF ( KSCREEN ) THEN
                             IF ( .NOT. KBIT( LCLK(KCLOCK), INT2(13)) .OR. &
     &                            SEG_OUTPUT.OR. ( PRINT_CONT .AND. .NOT. &
     &                            KMINOUT)) THEN
!
                                  IPTR=IPTR+1
                                  CALL CLRCH ( LBUF(IPTR) )
                                  WRITE(lbuf(iptr),5201) NPARM,(ISITN(L,ISTA), &
     &                            L=1,4), LP, IYR, IM, ID, IHR, IMIN, &
     &                            CORR, XSSIG
                                  CALL BLANK_TO_ZERO ( LBUF(IPTR)(22:29) )
                                  CALL BLANK_TO_ZERO ( LBUF(IPTR)(31:35) )
                                  CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                                  CALL NL_MN()
 5201                             FORMAT ( I5, ". ",4A2,1X,"CL",I2," ", &
     &                                   2(I2,'/'),I2,1X,I2,':',I2, &
     &                                   10X,F15.3,' ns    ',F6.2,' ns')
                              ENDIF
                          ENDIF
                          IF ( .NOT.KBIT( LCLK(KCLOCK), INT2(13)) ) THEN
                               JCLOCKX = KCLOCK
                          ENDIF
                          IF ( KBIT( CONSTRAINT_BITS, INT2(3) ) .AND. .NOT. &
     &                         POLYONLY.AND. .NOT. OLD_CLOCKS .AND. .NOT. &
     &                         KREFSTA )THEN
                               NXTCLK = KCLOCK+1
                               do while (.not.kbit(iclsta(1,nxtclk),ista)  &
     &                                   .and.nxtclk.lt.numclk(ista))
                                   nxtclk = nxtclk+1
                               enddo
                            nxtep = nxtclk
                            do while (  (.not.kbit( lclk(nxtep), INT2(13)).or. &
     &                      .not.kbit(iclsta(1,nxtep),ista) ).and. &
     &                       nxtep.lt.max_clk)
                              nxtep=nxtep+1
                            enddo
                              time_interval1 = clock_interval
                              time_interval2 = clock_interval
                              if(jclock.gt.1) then
!
! ------------------------------ Trap for....
!
                                 CLOCK_VALUES(Kclock,ISTA) = 0.0d0
                                 CLOCK_SIGMAS(Kclock,ISTA) = 0.0d0
                                 TCLOCK_VALUES(Kclock,ISTA) = 0.0d0
                                 BCLOCK_VALUES(Kclock,ISTA) = 0.0d0
!
                                tim1=.5+AINT(FJDCL(Kclock)-.5+2D-5)
                                CALL MDYJL(IM,ID,IYr,ITIME,tim1 )
                                tim1=tim1-2D-5
                                IHR=(FJDCL(Kclock)-tim1)*24.0D0 + .1D-7
                                IMIN = ((FJDCL(Kclock)-tim1)- &
     &                             IHR/24.0D0)*1440.0D0+0.1D-7
                                tim1=ihr+imin/60.d0
                                tim2=.5+AINT(FJDCL(Kclprev)-.5+2D-5)
                                CALL MDYJL(IM,ID,IYr,ITIME,tim2 )
                                tim2=tim2-2D-5
                                IHR=(FJDCL(Kclprev)-tim2)*24.0D0 + .1D-7
                                IMIN=((FJDCL(Kclprev)-tim2)- &
     &                               IHR/24.0D0)*1440.0D0+0.1D-7
                                tim2=ihr+imin/60.d0
                                time_interval1=tim1-tim2
                                if(time_interval1.lt. &
     &                             0)time_interval1=time_interval1+24.d0
                              endif
                              kclprev = kclock
                              if(jclock.lt.numclk(ista)) then
                                tim1=.5+AINT(FJDCL(nxtep)-.5+2D-5)
                                CALL MDYJL(IM,ID,IYr,ITIME,tim1 )
                                tim1=tim1-2D-5
                                IHR=(FJDCL(nxtep)-tim1)*24.0D0 + .1D-7
                                IMIN = ((FJDCL(nxtep)-tim1)- &
     &                                 IHR/24.0D0)*1440.0D0+0.1D-7
                                tim1=ihr+imin/60.d0
                                tim2=.5+AINT(FJDCL(Kclock)-.5+2D-5)
                                CALL MDYJL(IM,ID,IYr,ITIME,tim2 )
                                tim2=tim2-2D-5
                                IHR=(FJDCL(Kclock)-tim2)*24.0D0 + .1D-7
                                IMIN=((FJDCL(Kclock)-tim2)- &
     &                               IHR/24.0D0)*1440.0D0+0.1D-7
                                tim2=ihr+imin/60.d0
                                time_interval2=tim1-tim2
                                if(time_interval2.lt. &
     &                             0)time_interval2=time_interval2+24.d0
                              endif
                              nnext = nparm+1
                              do ii=2,ICLMAX
                                if(kbit(lclk(kclock),ii)) nnext=nnext+1
                              enddo
                              n1 = indx8(nparm,nparm)
                              n2 = indx8(nprev,nparm)
                              n3 = indx8(nnext,nparm)
                              const = dble(sccnst(ista))
                              const1 = const*3600.d-14 * time_interval1
                              const2 = const*3600.d-14 * time_interval2
                              if(kclock.gt.jclockx)then
                                CLKRMS(ISTA)=CLKRMS(ISTA)+ &
     &                          ((mat(jb+NPARM)*1.d9-mat(jb+nprev)*1.d9)/ &
     &                           time_interval1)**2
                                clksta(ista)=clksta(ista)+ &
     &                           ((mat(jb+nparm)*1.d9-mat(jb+nprev)*1.d9)** &
     &                             2)/((sccnst(ista)*time_interval1)**2)
                                clkcntx(ista) = clkcntx(ista)+1
                              endif
                              nprev = nparm
                           if(kclock.eq.jclockx) then
                             trace = (mat(ja+n1)-mat(ja+n3))/(const2**2)
                           else if(kclock.lt.numclk(ista).and. &
     &                     kbit( lclk(nxtclk), INT2(13)).and.nxtep.eq.nxtclk) then
                             trace = (mat(ja+n1)- mat(ja+n2))/(const1**2) + &
     &                                (mat(ja+n1)-mat(ja+n3))/(const2**2)
                           else
                             trace = (mat(ja+n1)-mat(ja+n2))/(const1**2)
                           endif
                              clkTRA(ISTA)=clkTRA(ISTA)+trace
                              CLKCNT(ISTA)=CLKCNT(ISTA)+1
                           ENDIF !constraint logic
                         END IF  !Handle Offset
!
                         IF (M .EQ. 2) then !Handle Rate
!
                            CORR = mat(jb+NPARM)*1.D14
                            XSIG = mat(js+NPARM)*1.D14
                            XSSIG = SCSIG(NPARM)*1.D14
                            IF (KSPOOL) THEN
                               IF(.not.KBIT( LCLK(KCLOCK), INT2(13)) .or. &
     &                            SEG_OUTPUT.or.(PRINT_CONT .and. .not. &
     &                            KMINOUT))THEN
                                  CALL CLRCH ( STR )
                                  IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                                       LS_CHR = '  '
                                       LC_CHR = '  '
                                       WRITE(STR,4202) NPARM,(ISITN(L,ISTA), &
     &                                   L=1,4), LP, IYR, IM, ID, IHR, IMIN, &
     &                                  CORR, XSIG, XSSIG, LS, LC
4202                                    FORMAT ( I5,". ",4A2,1X,"CL",I2," ", &
     &                                   2(I2,'/'),I2,1X,I2,':',I2,1X, &
     &                                   22X,3(F13.3,' D-14    '),2A2)
                                       CALL BLANK_TO_ZERO ( STR(22:29) )
                                       CALL BLANK_TO_ZERO ( STR(31:35) )
                                     ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
                                       LET = 'CL'
                                       UNIT   = ' D-14    '
                                       CALL ERR_PASS ( IUER, IER )
                                       WRITE ( STR, 113 ) NPARM, &
     &                                       ISITN_CHR(ISTA), LET, LP, &
     &                                       JD_TO_DATE ( FJDCL(KCLOCK)-32.184/86400.0D0, IER ), &
     &                                       CORR, UNIT, XSIG, UNIT, XSSIG, UNIT
                                       IF ( IER .NE. 0 ) THEN
                                            WRITE ( 6, * ) ' FJDCL(KCLOCK) = ', FJDCL(KCLOCK)
                                            CALL ERR_LOG ( 3920, IUER, 'A1JST', 'Wrong date' ) 
                                            RETURN 
                                       END IF
                                  ENDIF
                                  WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                              ENDIF
                            ENDIF
                            IF ( ( KSCREEN .AND. .NOT. KBIT( LCLK(KCLOCK), &
     &                           INT2(13)) ).OR.(KSCREEN .and. PRINT_CONT)) THEN
!
                               LS_CHR = '  '
                               LC_CHR = '  '
                               iptr=iptr+1
                               WRITE(lbuf(iptr),5202)NPARM,(ISITN(L,ISTA), &
     &                               L=1,4), LP, IYr, IM, ID, IHR, IMIN, &
     &                               CORR, XSSIG
                               CALL BLANK_TO_ZERO ( LBUF(IPTR)(22:29) )
                               CALL BLANK_TO_ZERO ( LBUF(IPTR)(31:35) )
                               call addstr_f(lbuf(iptr)(:pagewid) )
                               call nl_mn()
 5202                          FORMAT ( I5,". ",4A2,1X,"CL",I2," ", &
     &                                2(I2,'/'),I2,1X,I2,':',I2, &
     &                                12X,F13.3,' D-14', 4X, F4.2,' D-14')
                            ENDIF
                            IF ( KBIT( CONSTRAINT_BITS, INT2(3) ) .AND. &
     &                           OLD_CLOCKS.AND. KBIT( LCLK(KCLOCK), &
     &                           INT2(13)) )THEN
!
                                 CLKRMS(ISTA) = CLKRMS(ISTA)+ &
     &                                          ( MAT(JB+NPARM)*1.D14 )**2
                                 CLKSTA(ISTA) = CLKSTA(ISTA)+ &
     &                                ( (MAT(JB+NPARM)*1.D14)/SCCNST(ISTA))**2
                                 CLKTRA(ISTA)=CLKTRA(ISTA)+ &
     &                                 ( (MAT(JS+NPARM)*1.d14)/SCCNST(ISTA))**2
                                 CLKCNT(ISTA)  = CLKCNT(ISTA)+1
                                 CLKCNTX(ISTA) = CLKCNTX(ISTA)+1
                            ENDIF
!
                         END IF !Handle rate
!
  360                    IF (M .EQ. 3) then !Handle frequency drift
!
                            CORR = MAT(JB+NPARM)*1.D14*86400.D0
                            XSIG = MAT(JS+NPARM)*1.D14*86400.D0
                            XSSIG = SCSIG(NPARM)*1.D14*86400.D0
                            IF ( KSPOOL ) THEN
                                 CALL CLRCH ( STR )
                                 IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                                      LS_CHR = '  '
                                      LC_CHR = '  '
                                      WRITE(STR,6201) NPARM,(ISITN(L,ISTA), &
     &                                   L=1,4), LP, IYR, IM, ID, IHR, IMIN, &
     &                                   CORR, XSIG, XSSIG, LS, LC
 6201                                 FORMAT ( I5, ". ",4A2,1X,"CL",I2," ", &
     &                                         2(I2,'/'),I2,1X,I2,':',I2,1X, &
     &                                         22X,3(F13.3,' D-14/day'),2A2)
                                      CALL BLANK_TO_ZERO ( STR(22:29) )
                                      CALL BLANK_TO_ZERO ( STR(31:35) )
                                    ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
                                      LET = 'CL'
                                      UNIT   = ' D-14/day'
                                      CALL ERR_PASS ( IUER, IER )
                                      WRITE ( STR, 113 ) NPARM, &
     &                                       ISITN_CHR(ISTA), LET, LP, &
     &                                       JD_TO_DATE ( FJDCL(KCLOCK)-32.184/86400.0D0, IER ), &
     &                                       CORR, UNIT, XSIG, UNIT, XSSIG, UNIT
                                       IF ( IER .NE. 0 ) THEN
                                            WRITE ( 6, * ) ' FJDCL(KCLOCK) = ', FJDCL(KCLOCK)
                                            CALL ERR_LOG ( 3921, IUER, 'A1JST', 'Wrong date' ) 
                                            RETURN 
                                       END IF
                                  ENDIF
                                  WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                            ENDIF
!
                            IF ( KSCREEN ) THEN
                                 IPTR=IPTR+1
                                 LS_CHR = '  '
                                 LC_CHR = '  '
                                 WRITE ( LBUF(IPTR), 6202) NPARM,(ISITN(L,ISTA), &
     &                                   L=1,4), LP, IYR, IM, ID, IHR, IMIN, &
     &                                   CORR, XSSIG
                                 CALL BLANK_TO_ZERO ( LBUF(IPTR)(22:29) )
                                 CALL BLANK_TO_ZERO ( LBUF(IPTR)(31:35) )
                                 CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                                 CALL NL_MN()
 6202                            FORMAT ( I5, ". ",4A2,1X,"CL",I2," ", &
     &                                    2(I2,'/'),I2,1X,I2,':',I2, &
     &                                   17X,F8.2,'-14/d', 4X, F4.2,'-14/d')
                            ENDIF
                         END IF !Handle frequency drift.
                         IF ( M .GT. 3 ) THEN !handle higher order terms
!
  370                       IF ( KSPOOL ) THEN
                                 LS_CHR = '  '
                                 LC_CHR = '  '
                                 WRITE(23,1200) NPARM,(ISITN(L,ISTA), L=1,4), &
     &                               LP, IYR,IM,ID,IHR,IMIN,mat(jb+NPARM),LPC, &
     &                               MAT(JS+NPARM),LPC,SCSIG(NPARM),LPC, &
     &                               LS, LC
 1200                          FORMAT ( I5,". ",4A2,1X,"CL",I2," ", &
     &                                  2(I2,'/'),I2,1X,I2,':',I2,23X, &
     &                                  3(7X,D9.3,' S**',I2,1X),2A2)
!    #                                  3(7X,D9.3,,' S**',I2,1X),2A2)
                            ENDIF
                            IF ( KSCREEN ) THEN
                                 IPTR = IPTR+1
                                 WRITE ( LBUF(IPTR), 3200) NPARM,(ISITN(L,ISTA), &
     &                                   L=1,4), LP, IYR, IM, ID, IHR, &
     &                                   IMIN, MAT(JB+NPARM), LPC, &
     &                                   SCSIG(NPARM), LPC, LS, LC
                                 CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                                 CALL NL_MN()
 3200                            FORMAT ( I5, ". ",4A2,1X,"CL",I2," ", &
     &                                    2(I2,'/'),I2,1X,I2,':',I2, &
     &                                    15X2(1X,E11.3, '@',I2),2A2)
                            ENDIF
                         END IF   !Handle higher order terms
                      END IF   !This station is in a selected baseline
                   END IF    !Its turned on
                END DO       !Run over the polynomial terms for this epoch
!
! ------------- If no terms estimated this is reference clock.
!
                IF ( ICLCK.EQ.0 .AND. .NOT.KBATCH .AND. .NOT. BMODE_CL ) THEN
!
                   IF ( KSCREEN ) THEN
                        IPTR=IPTR+1
                        WRITE ( LBUF(IPTR), 3277 ) (ISITN(L,ISTA),L=1,4), &
     &                        IYR, IM, ID, IHR, IMIN
                        CALL BLANK_TO_ZERO ( LBUF(IPTR)(22:29) )
                        CALL BLANK_TO_ZERO ( LBUF(IPTR)(31:34) )
                        CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                        CALL NL_MN()
 3277                   FORMAT(6X ,4A2," CLCK ",2(I2,"/"),I2,1X,I2,":", &
     &                         I2,' Reference')
                   ENDIF
                   IF ( KSPOOL .AND. .NOT.KBATCH ) THEN
                        CALL CLRCH ( STR )
                        WRITE ( STR, 3277 ) (ISITN(L,ISTA),L=1,4), &
     &                          IYR, IM, ID, IHR, IMIN
  327                   FORMAT(6X ,4A2," CLCK ",2(I2,"/"),I2,1X,I2,":", &
     &                        I2' Reference')
                        CALL BLANK_TO_ZERO ( STR(22:29) )
                        CALL BLANK_TO_ZERO ( STR(31:35) )
                        WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                   ENDIF
                END IF
!
! ------------- Check for sin and cos parameters and print out resutls.
!
                SINCOS(1)=0.D0
                SINCOS(2)=0.D0
                IGOT1=0
                IGOT2=0
!
! ------------- If continued diurnal from previous epoch skip on
!
                IF (.NOT. KBIT( LCLK(KCLOCK), INT2(14))) THEN !Not continued
                   DO M = 1, 2 !Run over sine and cosine
                      IF (KBIT( LCLK(KCLOCK), INT2(14+M))) then  !Its turned on
                         NPARM=NPARM+1
                         mat(jb+NPARM)=mat(jb+NPARM)*1.D9   !Change units to nanoseconds
                         mat(js+NPARM)=mat(js+NPARM)*1.D9
                         SCSIG(NPARM)=SCSIG(NPARM)*1.D9
                         SINCOS(M)=mat(jb+NPARM)
                         IF (M .EQ. 1) IGOT1=1
                         IF (M .EQ. 2) IGOT2=1
                         IF (USESTA) then !Its from a selected baseline
!
                            IF (KSCREEN) THEN
                                iptr=iptr+1
                                WRITE(lbuf(iptr),1210) NPARM,(ISITN(L,ISTA), &
     &                            L=1,4),(LABSC(L,M),L=1,2),IYr,IM,ID, &
     &                            IHR, IMIN, mat(jb+NPARM), SCSIG(NPARM)
                                call addstr_f(lbuf(iptr)(:pagewid) )
                                call nl_mn()
                            ENDIF
                            IF (KSPOOL) THEN
                               WRITE(23,1211)NPARM,(ISITN(L,ISTA), &
     &                         L=1,4),(LABSC(L,M),L=1,2),IYr,IM,ID, &
     &                         IHR,IMIN,mat(jb+NPARM),mat(js+NPARM), &
     &                         SCSIG(NPARM)
1210                           FORMAT ( I5, ". ", 4A2, 1X, 2A2, " ", &
     &                            2(I2,'/'),I2,1X,I2,':',I2,16X,F9.2, &
     &                            ' ns ',F9.2,' ns')
1211                           FORMAT ( I5, ". ", 4A2, 1X, 2A2, " ", &
     &                            2(I2,'/'),I2,1X,I2,':',I2,20X,F16.3, &
     &                                 ' ns     ',2X,F12.3 ,' ns     ', &
     &                            F14.3,' ns     ')
                            ENDIF
                         END IF !Its from a selected baseline
                      END IF !Its turned on
                   END DO !Run over sine and cosine
!
                   IF ( IGOT1 .NE.0 .AND. IGOT2 .NE. 0 .AND. KFULLOUT ) THEN !Both on
                        SCAMP=DSQRT(SINCOS(1)*SINCOS(1)+SINCOS(2)*SINCOS(2))
                        SCPHS=DATAN2(SINCOS(1), &
     &                           SINCOS(2))*24.D0/6.2831853072D0
!
! ------------------- Phase seems to be off by 6 hours ??????
!
                      SCPHS=SCPHS-6.D0
                      IF (SCPHS.LT.-12.D0) SCPHS=SCPHS+24.D0
                      IF (USESTA) then !Station is from selected b.l.
                         IF (KSCREEN) then
                        iptr=iptr+1
                        WRITE(lbuf(iptr),2177) SCAMP,SCPHS
                   call addstr_f(lbuf(iptr)(:pagewid) )
                   call nl_mn()
                      endif
                         IF (KSPOOL) WRITE(23,177) SCAMP,SCPHS
177                      FORMAT(58X," Amp ",F7.3," ns       Phase ", &
     &                          F6.3,' hours')
2177                     FORMAT(49X," Amp ",F5.2," ns Ph", &
     &                          F7.2,' hours')
                      END IF !Station is from selected b.l.
                   END IF !Both turned on
                END IF !Not continued
            ENDIF
         END DO !Run over the clocks for this station
!
! ------ If no clock epochs then is batch-mode reference clock
!
          IF ( KEPOCHS ) THEN
               IF ( KSCREEN ) THEN
                    iptr=iptr+1
                    WRITE(lbuf(iptr),3288)(ISITN(L,ISTA),L=1,4)
                    CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                    CALL NL_MN()
 3288               FORMAT(6X,4A2," CLCK ",12X," Batch Mode Reference")
              ENDIF
              IF ( KSPOOL.AND..NOT.KBATCH) THEN
                   WRITE(23,328)(ISITN(L,ISTA),L=1,4)
  328              FORMAT(6X,4A2," CLCK ",12X," Batch Mode Reference")
              ENDIF
          END IF
       END IF ! of clock section
!
! ---- Print atmosphere parameters
!
       ATM_CUM     = 0.D0
       ATM_SQR_CUM = 0.D0
       ATM_EPO_CUM = 0.D0
       NATM = 0
       IF ( UNF_ATM   .AND.  ISTA .LE. MAX_ARC_STA ) THEN
          IF ( NUMATM(ISTA) .GT. 0 ) THEN
!
! --------- Case of uniform atmosphere segments
!
            ATMFAC = 1.D12 ! scaling factor
            UNIT = 'ps       '
            CONST = DBLE(SACNST(ISTA) ) *(ATM_INTERVAL*24.D0) /ATMFAC
!
            IF ( APRIORI_ZENDEL .AND. SEG_OUTPUT ) THEN
!
! -------------- Apriori zenith delay was requested.
! -------------- Open the file with a priori zenith delay computed by CRES
!
                 FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'ATZE'//PRE_LETRS
                 OPEN ( UNIT=61, FILE=FNAME, STATUS='OLD', IOSTAT=IOS4 )
                 I61 = IOS4
                 IF ( I61 .NE. 0 ) THEN
                      CALL ERR_LOG ( 3922, IUER, 'A1JST', 'Error '// &
     &                    'during attempt to open file '//FNAME(1:I_LEN(FNAME))// &
     &                    ' with a priori zenith '//'path delay created by CRES' )
                      RETURN
                 END IF
!
! -------------- Skip the first line
!
                 READ ( UNIT=61, FMT='(1X)', IOSTAT=IOS4 )
                 KZEN = 0
!
! -------------- Now read the file. Our purpose is to extract lines relevant
! -------------- to the given station and to build two arrays: epoch, zenith
! -------------- path delay. Epoch is a time diffrerence between the current
! -------------- observation and the first observation at this station.
!
                 DO 440 J4=1,1024*1024*1024
!
! ----------------- Read a line
!
                    READ ( UNIT=61, FMT=117, IOSTAT=IOS4 ) STR, JDATE_APR, &
     &              ZENDEL
 117                FORMAT ( A8, 1X, F16.8, 1X, 1PD14.6 )
                    I61 = IOS4
                    IF ( I61 .EQ. -1 ) GOTO 844
                    IF ( I61 .NE.  0 ) THEN
                         WRITE ( 6, * ) ' J4=',J4,' J61=',I61
                         CALL ERR_LOG ( 3923, IUER, 'A1JST', &
     &                       'Error during attempt to read file '// &
     &                        FNAME(1:I_LEN(FNAME))//' with a priori zenith '// &
     &                       'path delay created by CRES' )
                         RETURN
                    END IF
                    IF ( STR(1:8) .EQ. ISITN_CHR(ISTA) ) THEN
!
! ---------------------- The line is realted to the station under investigation
!
                         IF ( KZEN .EQ. 0 ) THEN
                              JDATE_APR_BEG = JDATE_APR
                            ELSE
                         END IF
                         KZEN = KZEN + 1
!
! ---------------------- Add the point to the array
!
                         JAPR(KZEN) = JDATE_APR - JDATE_APR_BEG
                         ZDEL(KZEN) = ZENDEL
                         IF ( KZEN .GT. 1 ) THEN
!
! --------------------------- Tie epoch is the same as for the previous point?
!
                              IF ( JAPR(KZEN) - JAPR(KZEN-1) .LT. JAPR_MIN )THEN
!
! -------------------------------- Yes? Remove the last point!
!
                                   KZEN = KZEN-1
                              END IF
                        END IF
                    END IF
 440             CONTINUE
 844             CONTINUE
!
! -------------- Compute coefficients of natural interpolating spline
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL MAKE_SPLINE ( 1, KZEN, JAPR, ZDEL, 0.0D0, 0.D0,COEF, &
     &                WORK, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 3924, IUER, 'A1JST', &
     &                    'Error during attempt to comnpute coefficients '// &
     &                    'of cubic spline for interpolation of a priori '// &
     &                    'zenith path delay' )
                      RETURN
                 END IF
            END IF
            DO 450 J5=1,NUM_ATM
!
! ------------ Translate time tag of the J5-th epoch
!
               CALL EPOC ( IM, ID, IYR, IHR, IMIN, JDATE_ATM(J5) )
               IF ( USESTA ) THEN  ! This station is in a selected baseline
                    NPARM = NPARM+1
!
! ----------------- Scaling
!
                    MAT(JB+NPARM) = MAT(JB+NPARM) * ATMFAC
                    MAT(JS+NPARM) = MAT(JS+NPARM) * ATMFAC
!
! ----------------- Update accumulators for calculation of average atmosphere
!
                    ATM_CUM = ATM_CUM + MAT(JB+NPARM)
                    ATM_SQR_CUM = ATM_SQR_CUM + MAT(JB+NPARM)**2
                    ATM_EPO_CUM = ATM_EPO_CUM + JDATE_ATM(J5) 
                    NATM  = NATM+1
!
! ----------------- Store info for MDLPL, if neccessary
!
                    IF ( .NOT. KBATCH  .AND.  J5 .LE. MAX_SEGMENTS ) THEN
                         ATMOS_VALUES(J5,ISTA) = MAT(JB+NPARM)
                         ATMOS_SIGMAS(J5,ISTA) = MAT(JS+NPARM)
                    ENDIF
                    XSSIG = SCSIG(NPARM) * ATMFAC
!
! ----------------- Printing results in file or on the screen
!
                    IF ( KSPOOL ) THEN
                       IF ( APRIORI_ZENDEL .AND. SEG_OUTPUT ) THEN
!
! ------------------------- Compute value of cubic spline
!
                            IXC = IXMN8 ( KZEN, JAPR, &
     &                                    JDATE_ATM(J5)-JDATE_APR_BEG )
                            ZENDEL = FSPL8 ( JDATE_ATM(J5)-JDATE_APR_BEG, &
     &                                       KZEN, JAPR, ZDEL, IXC, COEF )
!
! ------------------------- ... and write down a priori zenith path delay
!
                            IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                                 WRITE ( STR, 1301 ) NPARM, (ISITN(L,ISTA),L=1,4), &
     &                                  'zd', 0, IYR, IM, ID, IHR, IMIN, &
     &                                  ZENDEL*1.D12,  UNIT(1:7), &
     &                                  MAT(JS+NPARM), UNIT(1:7), &
     &                                  XSSIG,         UNIT(1:7)
 1301                            FORMAT ( I5, ". ", 4A2, 1X, A, I2, 1X, &
     &                                    2(I2,'/'), I2, 1X, I2, ':', I2, &
     &                                    14X, '        ', F14.3, ' ', A, &
     &                                    2(F14.3,' ',A) )
                                 CALL BLANK_TO_ZERO ( STR(22:29) )
                                 CALL BLANK_TO_ZERO ( STR(31:35) )
                              ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
                                 CALL ERR_PASS ( IUER, IER )
                                 WRITE ( STR, 1302 ) NPARM, ISITN_CHR(ISTA), &
     &                                  'zd', 0, &
     &                                  JD_TO_DATE ( JDATE_ATM(J5)-32.184/86400.0D0, IER ), &
     &                                  ZENDEL*1.D12,  UNIT(1:7), &
     &                                  MAT(JS+NPARM), UNIT(1:7), &
     &                                  XSSIG,         UNIT(1:7)
                                 IF ( IER .NE. 0 ) THEN
                                      WRITE ( 6, * ) ' JDATE_ATM(J5) = ', JDATE_ATM(J5)
                                      CALL ERR_LOG ( 3925, IUER, 'A1JST', 'Wrong date' ) 
                                      RETURN 
                                 END IF
 1302                            FORMAT ( I5, ". ", A8, 1X, A, I2, 1X, &
     &                                A, 5X, '        ', F14.3, ' ', A, &
     &                                2(F14.3,' ',A) )
                            END IF
                            WRITE ( 23, '(A)' ) STR(1:75)
                       END IF
!
                       IF ( J5 .EQ. 1 .OR. &
     &                      ( PRINT_CONT .AND. .NOT. KMINOUT ) ) THEN
!
                            LET = 'AT'
                            CALL CLRCH ( STR )
                            IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                                 WRITE ( STR, 1301 ) NPARM, ( ISITN(L,ISTA),L=1,4 ), &
     &                                   LET, 0, IYR, IM, ID, IHR, IMIN, &
     &                                   MAT(JB+NPARM), UNIT(1:7), &
     &                                   MAT(JS+NPARM), UNIT(1:7), &
     &                                   XSSIG,         UNIT(1:7)
                                 CALL BLANK_TO_ZERO ( STR(22:29) )
                                 CALL BLANK_TO_ZERO ( STR(31:35) )
                               ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
                                 CALL ERR_PASS ( IUER, IER )
                                 WRITE ( STR, 1302 ) NPARM, ISITN_CHR(ISTA), &
     &                                   LET, 0, &
     &                                   JD_TO_DATE ( JDATE_ATM(J5)-32.184/86400.0D0, IER ), &
     &                                   MAT(JB+NPARM), UNIT(1:7), &
     &                                   MAT(JS+NPARM), UNIT(1:7), &
     &                                   XSSIG,         UNIT(1:7)
                                 IF ( IER .NE. 0 ) THEN
                                      WRITE ( 6, * ) ' JDATE_ATM(J5) = ', JDATE_ATM(J5)
                                      CALL ERR_LOG ( 3926, IUER, 'A1JST', 'Wrong date' ) 
                                      RETURN 
                                 END IF
                            END IF
                            WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                       ENDIF
                    ENDIF
!
                    IF ( KSCREEN ) THEN
                       IF ( J5 .EQ. 1  ) THEN
                          IPTR=IPTR+1
                          LET = 'AT'
                          WRITE ( LBUF(IPTR), 3301) NPARM, &
     &                          (ISITN(L,ISTA),L=1,4), LET, 0, IYR, IM, ID, &
     &                          IHR, IMIN, MAT(JB+NPARM), UNIT(1:5), &
     &                                     MAT(JS+NPARM), UNIT(1:5)
 3301                     FORMAT ( I5, ". ", 4A2, 1X, A, I2, 1X, &
     &                             2(I2,'/'), I2, 1X, I2, ':', I2, 10X, &
     &                             F12.2, ' ', A, F11.2, ' ', A )
                          CALL BLANK_TO_ZERO ( LBUF(IPTR)(22:29) )
                          CALL BLANK_TO_ZERO ( LBUF(IPTR)(31:35) )
                          CALL ADDSTR_F(LBUF(IPTR)(:PAGEWID) )
                          CALL NL_MN()
                       ENDIF
                       IF ( J5 .GT. 1  .AND.   ( SEG_OUTPUT .OR. &
     &                      ( PRINT_CONT .AND.  .NOT. KMINOUT) ) ) THEN
                          IPTR=IPTR+1
                          LET = 'at'
                          WRITE ( LBUF(IPTR), 3301) NPARM, &
     &                          (ISITN(L,ISTA),L=1,4), LET, 0, IYR, IM, ID, &
     &                          IHR, IMIN, MAT(JB+NPARM), UNIT(1:5), &
     &                                     MAT(JS+NPARM), UNIT(1:5)
                          CALL BLANK_TO_ZERO ( LBUF(IPTR)(22:29) )
                          CALL BLANK_TO_ZERO ( LBUF(IPTR)(31:35) )
                          CALL ADDSTR_F(LBUF(IPTR)(:PAGEWID) )
                          CALL NL_MN()
                       ENDIF
                    ENDIF
!
                    IF ( KSPOOL ) THEN
                       IF ( J5 .GT. 1   .AND.  ( SEG_OUTPUT .OR. &
     &                      ( PRINT_CONT  .AND.  .NOT. KMINOUT) ) ) THEN
                            CALL CLRCH ( STR )
                            LET = 'at'
                            IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                                 WRITE ( STR, 1301 ) NPARM, (ISITN(L,ISTA),L=1,4), &
     &                                   LET, 0, IYR, IM, ID, IHR, IMIN, &
     &                                   MAT(JB+NPARM), UNIT(1:7), &
     &                                   MAT(JS+NPARM), UNIT(1:7), &
     &                                   XSSIG,         UNIT(1:7)
                                 CALL BLANK_TO_ZERO ( STR(22:29) )
                                 CALL BLANK_TO_ZERO ( STR(31:35) )
                               ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
                                 CALL ERR_PASS ( IUER, IER )
                                 WRITE ( STR, 1302 ) NPARM, ISITN_CHR(ISTA), &
     &                                   LET, 0, &
     &                                   JD_TO_DATE ( JDATE_ATM(J5)-32.184/86400.0D0, IER ), &
     &                                   MAT(JB+NPARM), UNIT(1:7), &
     &                                   MAT(JS+NPARM), UNIT(1:7), &
     &                                   XSSIG,         UNIT(1:7)
                                 IF ( IER .NE. 0 ) THEN
                                      WRITE ( 6, * ) ' JDATE_ATM(J5) = ', JDATE_ATM(J5)
                                      CALL ERR_LOG ( 3927, IUER, 'A1JST', 'Wrong date' ) 
                                      RETURN 
                                 END IF
                            END IF
                            WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                       ENDIF
                    ENDIF
!
                    IF ( KBIT( CONSTRAINT_BITS, INT2(2) ) .AND. &
     &                   LATM(1,3) .NE. 0 ) THEN
                       N1 = INDX8( NPARM,NPARM)
                       N2 = INDX8( NPARM-1, NPARM)
                       N3 = INDX8( NPARM+1, NPARM)
!
! -------------------- Calculation contribution to RMS of variation of the
! -------------------- estimates of atmosphere ( ATMRMS )
!
                       IF ( J5 .GT. 1 ) THEN
                            ATMRMS(ISTA) = ATMRMS(ISTA) + &
     &                      ( ( MAT(JB+NPARM) - MAT(JB+NPARM-1))/ &
     &                        ( ATM_INTERVAL*24.D0              )  )**2
                            ATMCNTX(ISTA) = ATMCNTX(ISTA) + 1
                       ENDIF
!
! -------------------- Calculation TRACE -- contribution to mathematical
! -------------------- expectation of square of postfit residuals due to
! -------------------- constraints imposed on atmosphere rate
!
                       IF ( J5 .EQ.1 ) THEN
                            TRACE = ( MAT(JA+N1) - MAT(JA+N3) )/CONST**2
                          ELSE IF ( J5 .LT. NUM_ATM ) THEN
                            TRACE = ( 2*MAT(JA+N1) - MAT(JA+N2) - MAT(JA+N3) )/ &
     &                              CONST**2
                          ELSE
                            TRACE = ( MAT(JA+N1) - MAT(JA+N2) ) /CONST**2
                       ENDIF
                       IF ( CONST .EQ. 0 ) THEN
                            TRACE = 0
                       ENDIF
!
                       ATMTRA(ISTA) = ATMTRA(ISTA) + TRACE
                       ATMCNT(ISTA) = ATMCNT(ISTA) + 1
                    ENDIF
               ENDIF
 450        CONTINUE
            IF ( APRIORI_ZENDEL ) THEN
                 CLOSE ( UNIT=61 )
            END IF
!
! --------- Calculation ATMSTA -- scalled ATMRMS (ratio of a posteriori RMS
! --------- of atmopshere behavoir and its apriori "sigma" )
!
            IF ( KBIT( CONSTRAINT_BITS, INT2(2) ) .AND. &
     &           LATM(1,3).NE.0                   .AND. &
     &           SACNST(ISTA) .GT. 1.D-30               ) THEN
!
                 ATMSTA(ISTA) = ATMRMS(ISTA) / SACNST(ISTA)**2
            END IF
         END IF
       END IF
!
       IF ( .NOT. UNF_ATM  .AND.  ISTA .LE. MAX_ARC_STA ) THEN
         DO IATM = 1, NUMATM(ISTA) !Run over the atm epochs for this station
            JATM = IATM + IATSTR(ISTA)
            CALL EPOC(IM,ID,IYr,IHR,IMIN,TATM(JATM) )
            DO IAORD=0,1
            IF (KBIT(LATM(1,IAORD+1),JATM)) THEN !Its turned on
               NPARM = NPARM+1
               AATM=0.D0
               if ( IAORD .EQ. 0 ) AATM=VATM
               PVAL = AATM + mat(jb+NPARM)
               ATMFAC= 1.0D12
               unit = 'ps     '
               IF ( IAORD.EQ.1 ) THEN
                    ATMFAC = ATMFAC*3600.D0
                    UNIT = 'ps/hr  '
               end if
               PVAL = PVAL * ATMFAC
               MAT(JB+NPARM) = MAT(JB+NPARM) * ATMFAC
               NATM = NATM+1
               ATM_CUM = ATM_CUM + MAT(JB+NPARM)
               ATM_SQR_CUM = ATM_SQR_CUM + MAT(JB+NPARM)**2
               ATM_EPO_CUM = ATM_EPO_CUM + TATM(JATM)
               MAT(JS+NPARM) = MAT(JS+NPARM) * ATMFAC
               SCSIG(NPARM) = SCSIG(NPARM) * ATMFAC
               IF ( USESTA ) THEN !This station was from a selected b.l.
                   IF ( IAORD .EQ. 0  .OR.  .NOT. BMODE_AT ) THEN
                     IF (KSPOOL) THEN
                       IF ( .NOT. KBIT(LATM(1,3), JATM ) .OR. SEG_OUTPUT .OR. &
     &                      ( PRINT_CONT .AND. .NOT. KMINOUT ) ) THEN
                            IF ( LCHAO .GT. 0 ) THEN
                                 IF ( KBIT(JCAPPL(ISTA),LCHAO) .OR. &
     &                                SIMULATION_TEST               ) THEN
                                      WRITE ( 23, 1300 ) NPARM, &
     &                                        ISITN_CHR(ISTA), IAORD, &
     &                                        IYR, IM, ID, IHR, IMIN, &
     &                                        PVAL,          UNIT,  &
     &                                        MAT(JB+NPARM), UNIT,  &
     &                                        MAT(JS+NPARM), UNIT,  & 
     &                                        SCSIG(NPARM),  UNIT
 1300                                  FORMAT ( I5, ". ",           &
     &                                   A, 1X, "AT", I2, 1X,       &
     &                                   2(I2,'/'),I2,1X,I2,':',I2, &
     &                                    (F14.3,' ',A),            &
     &                                   3(F12.3,' ',A))
                                  END IF
                                ELSE 
                                  LET = 'AT'
                                  IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                                       WRITE ( STR, 1301 ) NPARM, &
     &                                    (ISITN(L,ISTA),L=1,4), LET, IAORD, &
     &                                    IYR, IM, ID, IHR, IMIN, MAT(JB+NPARM), &
     &                                    UNIT, MAT(JS+NPARM), UNIT, &
     &                                    SCSIG(NPARM), UNIT
                                       CALL BLANK_TO_ZERO ( STR(22:29) )
                                       CALL BLANK_TO_ZERO ( STR(31:35) )
                                     ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
                                       CALL ERR_PASS ( IUER, IER )
                                       WRITE ( STR, 1302 ) NPARM, ISITN_CHR(ISTA), &
     &                                     LET, 0, &
     &                                     JD_TO_DATE ( TATM(JATM)-32.184/86400.0D0, IER ), &
     &                                     MAT(JB+NPARM), UNIT(1:7), &
     &                                     MAT(JS+NPARM), UNIT(1:7), &
     &                                     XSSIG,         UNIT(1:7)
                                       IF ( IER .NE. 0 ) THEN
                                            WRITE ( 6, * ) ' TATM(JATM) = ', TATM(JATM)
                                            CALL ERR_LOG ( 3928, IUER, 'A1JST', 'Wrong date' ) 
                                            RETURN 
                                       END IF
                                  END IF
                                  WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                            END IF
                       ENDIF
                     ENDIF
!
                  IF ( KSCREEN ) THEN
                     IF ( .NOT. KBIT(LATM(1,3), JATM ) .OR. SEG_OUTPUT .OR. &
     &                    (PRINT_CONT .AND. .NOT. KMINOUT) ) THEN
                          IF ( LCHAO .GT. 0 ) THEN
                               IF ( KBIT(JCAPPL(ISTA), LCHAO) ) THEN
                                    IPTR=IPTR+1
                                    WRITE ( LBUF(IPTR), 3300 )  NPARM, &
     &                                    (ISITN(L,ISTA),L=1,4), &
     &                                    IAORD, IYR, IM, ID, IHR, IMIN, PVAL, &
     &                                    MAT(JB+NPARM), SCSIG(NPARM)
                                    CALL ADDSTR_F( LBUF(IPTR)(:PAGEWID) )
                                    CALL NL_MN()
 3300                               FORMAT ( I5,". ",4A2,1X,"AT",I2,1X, &
     &                                       2(I2,'/'),I2,1X,I2,':',I2,1X, &
     &                                       1X,F9.3,  ' ps ',2(1X,F9.2,' ps'))
                               END IF
                             ELSE
                               IPTR=IPTR+1
                               LET = 'AT'
                               WRITE ( LBUF(IPTR), 3301 )  NPARM, &
     &                                 (ISITN(L,ISTA),L=1,4), LET, IAORD, IYR, &
     &                             IM, ID, IHR, IMIN, MAT(JB+NPARM), UNIT(1:5), &
     &                             SCSIG(NPARM), UNIT(1:5)
                              CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                              CALL NL_MN()
                          END IF
                        ENDIF
                     END IF
                  END IF
!
                  IF ( IAORD .EQ. 0 ) THEN
                  IF(KBIT( CONSTRAINT_BITS, INT2(2) ).and.latm(1,3).ne.0) THEN
                        n1 = indx8( nparm, nparm)
                        n2 = indx8( nparm-1, nparm)
                        n3 = indx8( nparm+1, nparm)
                        const = dble(sacnst(ista))
                        atmos_interval =(tatm(iatstr(ista)+2)-tatm(iatstr(ista)+1))*24.d0
                        const = const*1.d-12 * atmos_interval
                     if(iatm.gt.1)then
                   ATMRMS(ISTA)=ATMRMS(ISTA)+ &
     &              ((mat(jb+NPARM)-mat(jb+nparm-1))/atmos_interval)**2
                   atmsta(ista)=atmsta(ista)+ &
     &               ((mat(jb+nparm)-mat(jb+nparm-1))** &
     &               2)/((sacnst(ista)*atmos_interval)**2)
                       atmcntx(ista) = atmcntx(ista)+1
                     endif
                    if(iatm.eq.1.and.numatm(ista).gt.1) then
                          trace = (mat(ja+n1)-mat(ja+n3))/(const**2)
                        else if(iatm.lt.numatm(ista)) then
                          trace = (2*mat(ja+n1)-mat(ja+n2)- &
     &                   mat(ja+n3))/(const**2)
                        else if(numatm(ista).gt.1) then
                          trace = (mat(ja+n1)-mat(ja+n2))/(const**2)
                        else 
                          trace = 0.0d0
                        endif
                        if (const.eq.0.0d0) then
                     trace = 0
                        endif
                    IF ( KBIT(LATM(1,IAORD+1),JATM) ) THEN
                         ATMTRA(ISTA)=ATMTRA(ISTA) + TRACE
                         ATMCNT(ISTA)=ATMCNT(ISTA)+1
                    ENDIF
                  ENDIF
                endif
                IF (IAORD .EQ. 1) THEN !handle rate
!
! --------------- Handle print suppression test
!
                  IF ( KSPOOL .AND. KFULLOUT .AND. PRINT_CONT ) THEN
                     IF (KBIT(JCAPPL(ISTA),LCHAO) .AND. &
     &                  (LCHAO .NE. 0)) THEN
                        WRITE ( 23, 1310 ) NPARM,(ISITN(L,ISTA),L=1,4), &
     &                                     IAORD, IYR, IM, ID, IHR, IMIN, &
     &                                     PVAL, MAT(JB+NPARM), &
     &                                     MAT(JS+NPARM), SCSIG(NPARM)
 1310                   FORMAT  ( I5, ". ", 4A2, 1X,"AT",I2,1X, &
     &                         2(I2,'/'),I2,1X,I2,':',I2, &
     &                         F14.3,' ps/h ',F14.3,' ps/h ', &
     &                         2(F14.3,' ps/h   '))
                     ELSE
                        WRITE(23,1311) NPARM,(ISITN(L,ISTA),L=1,4), &
     &                     IAORD,IYR,IM,ID,IHR,IMIN,MAT(JB+NPARM), &
     &                     MAT(JS+NPARM),SCSIG(NPARM)
 1311                   FORMAT ( I5,". ",4A2,1X,"AT",I2,1X, &
     &                         2(I2,'/'),I2,1X,I2,':',I2, &
     &                         14X,'        ',F14.3,' ps/h   ', &
     &                         2(F14.3,' ps/h   '))
                     END IF
                  END IF
!
!                 Handle  print suppression test.
!
                  IF(KSCREEN .and. PRINT_CONT) THEN
                     IF ((KBIT (JCAPPL(ISTA), LCHAO)).AND. &
     &                   (LCHAO .NE. 0)) THEN
                     iptr=iptr+1
                      WRITE(lbuf(iptr),3310)NPARM,(ISITN(L,ISTA),L=1,4), &
     &                     IAORD,IYr,IM,ID,IHR,IMIN,PVAL,mat(jb+NPARM), &
     &                     SCSIG(NPARM)
                   call addstr_f(lbuf(iptr)(:pagewid) )
                   call nl_mn()
 3310                   FORMAT ( I5,". ",4A2,1X,"AT",I2,1X, &
     &                         2(I2,'/'),I2,1X,I2,':',I2,1X, &
     &                         1X,F9.3,  ' ps/h ',2(1X,F7.2,' ps/h'))
                     ELSE
                          iptr=iptr+1
                    WRITE(lbuf(iptr),3311)  NPARM,(ISITN(L,ISTA),L=1,4), &
     &                        IAORD,IYr,IM,ID,IHR,IMIN,mat(jb+NPARM), &
     &                        SCSIG(NPARM)
                   call addstr_f(lbuf(iptr)(:pagewid) )
                   call nl_mn()
 3311                   FORMAT( I5, ". ",4A2,1X,"AT",I2,1X, &
     &                         2(I2,'/'),I2,1X,I2,':',I2,1X, &
     &                         1X,9X,  '      ',2(1X,F7.2,' ps/h'))
                     END IF
                  END IF
!
                  IF(KBIT( CONSTRAINT_BITS, INT2(2) )) THEN
                    ATMRMS(ISTA)=ATMRMS(ISTA)+mat(jb+NPARM)**2
                    ATMSTA(ISTA)=ATMSTA(ISTA) &
     &                      +(mat(jb+NPARM)/SACNST(ISTA))**2
                    ATMTRA(ISTA)=ATMTRA(ISTA) &
     &                      +(mat(js+NPARM)/SACNST(ISTA))**2
                    ATMCNT(ISTA)=ATMCNT(ISTA)+1
                    ATMCNTx(ISTA)=ATMCNTx(ISTA)+1
                  ENDIF
                 END IF !handle rate
               END IF !This station was from a selected b.l.
            END IF !Its turned on
         enddo
         END DO !Run over the atm epochs form this station
       END IF ! Atmosphere parameters section
       IF ( .NOT. KGLOBALS .AND. KSPOOL ) THEN
            IF ( NATM .GT. 0 ) THEN
                 ATM_AVG     = ATM_CUM/NATM
                 ATM_RMS     = DSQRT ( DABS(ATM_SQR_CUM/NATM - ATM_AVG**2)  )
                 TOT_ATM_RMS = DSQRT ( ATM_SQR_CUM/NATM )
                 ATM_EPO     = ATM_EPO_CUM/NATM
               ELSE
                 ATM_AVG = 0.0D0
                 ATM_RMS = 0.0D0
                 ATM_EPO_CUM = 0.0D0
                 TOT_ATM_RMS = 0.0D0
                 CALL OBSTM ( FJD1, LJD1 )
                 ATM_EPO = (FJD1 + LJD1)/2.0D0
            END IF
            IER = -1
            STR = JD_TO_DATE ( ATM_EPO, IER )
            STR(41:48) = ISITN_CHR(ISTA)
            CALL VTD_NAME_REPAIR ( STR(41:48) ) ! Repair the name: replace blanks inside the name wiht underscores
            WRITE ( 23, 1250 ) STR(41:48), STR(1:23), ATM_AVG, ATM_RMS, &
     &                         TOT_ATM_RMS, NAT(ISTA), &
     &                         TIM_ATM_END(ISTA) - TIM_ATM_BEG(ISTA), &
     &                         TZD_AVG(ISTA)*1.D12, TZD_RMS(ISTA)*1.D12, &
     &                         WZD_AVG(ISTA)*1.D12, WZD_RMS(ISTA)*1.D12, &
     &                         STM_AVG(ISTA), STM_RMS(ISTA), &
     &                         SPR_AVG(ISTA), SPR_RMS(ISTA) 
 1250       FORMAT ( 7X, A8, " Atm  ", A, 2X, "Avg: ", F9.3, " ps", 2X, &
     &                       " Rms: ", 2X, F9.3, " ps", 2X, &
     &                       " Tot_Rms: ", 2X, F9.3, " ps || Nat: ", I4, &
     &                       " Dura: ", F8.1, " sec ", &
     &                       " Tzd_avg: ", F6.1, " Tzd_rms: ", F6.1, &
     &                       " Wzd_avg: ", F6.1, " Wzd_rms: ", F6.1, " ps", &
     &                       " Stm_avg: ", F6.1, " Stm_rms: ", F6.1, " K ", &
     &                       " Spr_avg: ", F7.0, " Spr_rms: ", F7.0, " Pa"  )
       ENDIF
       IF ( ISTA .LE. MAX_ARC_STA ) THEN
!
! ------ Print gradient parameters
!
         DO IATM = 1, NUMGRAD(ISTA) !Run over the grad epochs for this station
            JATM = IATM
            CALL EPOC ( IM, ID, IYR, IHR, IMIN, TGRAD(JATM) )
            IF ( KBIT(LGRAD(1), JATM) ) THEN ! Its turned on
               NPARM = NPARM+1
               PVAL = MAT(JB+NPARM)
               IF ( USESTA) THEN ! This station was from a selected b.l.
                    CALL CLRCH ( STR )
                    CALL CLRCH ( FULL_GRAD_STR )
                    IF ( KMGR  .AND.  .NOT. KMINOUT ) THEN
                         WRITE ( UNIT=FULL_GRAD_STR, FMT='(F17.3," mm")') &
     &                           MGR_NORTH(ISTA)*1.D3+MAT(JB+NPARM)
                    END IF
!
                    IF ( KSPOOL  .AND.  .NOT. KMINOUT ) THEN
                         IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                              WRITE ( STR, 1350 ) NPARM, ISITN_CHR(ISTA), 'NG', &
     &                                IYR, IM, ID, IHR, IMIN, FULL_GRAD_STR, &
     &                                MAT(JB+NPARM), MAT(JS+NPARM), SCSIG(NPARM)
 1350                         FORMAT ( I5, ". ",A,1X, A, 1X, &
     &                                 2(I2,'/'),I2,1X,I2,':',I2, &
     &                                 A,'    ',F14.3,' mm     ', &
     &                                 2(F14.3,' mm     '))
                              CALL BLANK_TO_ZERO ( STR(20:27) )
                              CALL BLANK_TO_ZERO ( STR(29:33) )
                            ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
                              CALL ERR_PASS ( IUER, IER )
                              WRITE ( STR, 1355 ) NPARM, ISITN_CHR(ISTA), 'NG', &
     &                                JD_TO_DATE ( TGRAD(JATM)-32.184/86400.0D0, IER ), &
     &                                FULL_GRAD_STR(10:20), &
     &                                MAT(JB+NPARM), MAT(JS+NPARM), SCSIG(NPARM)
 1355                         FORMAT ( I5, ". ",A, 1X, A, 3X, &
     &                                 A, 2X, &
     &                                 A, 2X, F12.3,' mm     ', &
     &                                 2(F14.3,' mm     '))
                              IF ( IER .NE. 0 ) THEN
                                   WRITE ( 6, * ) ' TGRAD(JATM) = ', TGRAD(JATM)
                                   CALL ERR_LOG ( 3929, IUER, 'A1JST', 'Wrong date' ) 
                                   RETURN 
                              END IF
                         END IF
                         WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                    END IF
!
                    IF ( KSCREEN  .AND.  .NOT. KMINOUT ) THEN
                         CALL CLRCH ( FULL_GRAD_STR )
                         IF ( KMGR ) THEN
                              WRITE ( UNIT=FULL_GRAD_STR, FMT='(F9.2," mm")') &
     &                                MGR_NORTH(ISTA)*1.D3 + MAT(JB+NPARM)
                         END IF
!
                         IPTR=IPTR+1
                         WRITE ( LBUF(IPTR), 1351) NPARM, ISITN_CHR(ISTA), &
     &                           IYR, IM, ID, IHR, IMIN, FULL_GRAD_STR(1:12), &
     &                           MAT(JB+NPARM), SCSIG(NPARM)
                         CALL BLANK_TO_ZERO ( LBUF(IPTR)(20:27) )
                         CALL BLANK_TO_ZERO ( LBUF(IPTR)(29:33) )
                         CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                         CALL NL_MN()
 1351                    FORMAT ( I5, ". ", A, 1X, "NG", 1X, &
     &                           2(I2,'/'),I2,1X,I2,':',I2,1X, &
     &                           A, 2X,F9.2, ' mm', 5X, F9.2,' mm')
                    END IF
               ENDIF
               NPARM = NPARM+1
               PVAL  = MAT(JB+NPARM)
!
               IF ( USESTA ) THEN ! This station was from a selected b.l.
                    IF ( KSPOOL  .AND.  .NOT. KMINOUT ) THEN
                         CALL CLRCH ( STR )
                         CALL CLRCH ( FULL_GRAD_STR )
                         IF ( KMGR ) THEN
                              WRITE ( UNIT=FULL_GRAD_STR, &
     &                                FMT='(F17.3," mm")' )MGR_EAST(ISTA)*1.D3+MAT(JB+NPARM)
                         END IF
                         IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
                              WRITE ( STR, 1350 ) NPARM, ISITN_CHR(ISTA), 'EG', &
     &                               IYR, IM, ID, IHR, IMIN, FULL_GRAD_STR, &
     &                               MAT(JB+NPARM), MAT(JS+NPARM), SCSIG(NPARM)
                              CALL BLANK_TO_ZERO ( STR(20:27) )
                              CALL BLANK_TO_ZERO ( STR(29:33) )
                            ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
                              CALL ERR_PASS ( IUER, IER )
                              WRITE ( STR, 1355 ) NPARM, ISITN_CHR(ISTA), 'EG', &
     &                                JD_TO_DATE ( TGRAD(JATM)-32.184/86400.0D0, IER ), &
     &                                FULL_GRAD_STR(10:20), &
     &                                MAT(JB+NPARM), MAT(JS+NPARM), SCSIG(NPARM)
                              IF ( IER .NE. 0 ) THEN
                                   WRITE ( 6, * ) ' TGRAD(JATM) = ', TGRAD(JATM)
                                   CALL ERR_LOG ( 3930, IUER, 'A1JST', 'Wrong date' ) 
                                   RETURN 
                              END IF
                         END IF
                         WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                   END IF
!
                   IF ( KSCREEN   .AND.  .NOT. KMINOUT ) THEN
                        CALL CLRCH ( FULL_GRAD_STR )
                        IF ( KMGR ) THEN
                             WRITE ( UNIT=FULL_GRAD_STR, FMT='(F9.2," mm")') &
     &                               MGR_EAST(ISTA)*1.D3+MAT(JB+NPARM)
                        END IF
                        IPTR=IPTR+1
                        WRITE ( LBUF(IPTR), 1353)  NPARM, ISITN_CHR(ISTA), &
     &                          IYR, IM, ID, IHR, IMIN, FULL_GRAD_STR(1:12), &
     &                          MAT(JB+NPARM), SCSIG(NPARM)
                       CALL BLANK_TO_ZERO ( LBUF(IPTR)(20:27) )
                       CALL BLANK_TO_ZERO ( LBUF(IPTR)(29:33) )
                       CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                       CALL NL_MN()
 1353                  FORMAT ( I5, ". ", A, 1X, "EG", 1X, &
     &                          2(I2,'/'), I2, 1X, I2,':', I2, 1X, &
     &                          A, 2X, F9.2,' mm', 5X, F9.2,' mm')
                  END IF
!
                  IF ( KBIT( CONSTRAINT_BITS, INT2(8))  .AND.  &
     &                 LGRAD(1).NE. 0 ) THEN
                       N1 = INDX8( NPARM, NPARM )
                       N2 = INDX8( NPARM-2, NPARM)
                       N3 = INDX8( NPARM+2, NPARM)
                       N1N1 = INDX8( NPARM-1, NPARM-1 )
                       N3N1 = INDX8( NPARM-3, NPARM-1 )
                       P1N1 = INDX8( NPARM+1, NPARM-1 )
                       CONST1 = DBLE(GRADCONS(1))         ! constraint on initial offset
                       CONST2 = DBLE(GRADCONS(2))         ! constraint on rates
                       CONST2 = CONST2*GRAD_INTERVAL/24.0 ! since const2 is in mm/day
                       IF ( IATM .GT. 1 )THEN
                            GRDRMS(ISTA) = GRDRMS(ISTA)+ &
     &                        ( (MAT(JB+nparm) - MAT(JB+NPARM-2) )**2+ &
     &                          (MAT(JB+NPARM-1) - MAT(JB+NPARM-3) )**2) &
     &                          /(GRAD_INTERVAL/24.0)**2       ! to get mm/day
!
                            GRDSTA(ISTA)=GRDSTA(ISTA)+ &
     &                        ( ( MAT(JB+NPARM) - MAT(JB+NPARM-2) )**2 + &
     &                          ( MAT(JB+NPARM-1) - MAT(JB+NPARM-3) )** &
     &                          2)/(GRADCONS(2)*GRAD_INTERVAL/24.0)**2
                            GRDCNTX(ISTA) = GRDCNTX(ISTA)+2
                       ENDIF
!
! -------------------- Calculate gradient share using the total covariance
! -------------------- matrix C and the normal matrix Nc of the gradient
! -------------------- constraint ('observation') equations that constrain
! -------------------- the P gradient parameters
! -------------------- Share S = trace[Nc * C}/P
! -------------------- S*P is the number of degrees of freedom associated with
! -------------------- the constraint (The number of parameters is then
! -------------------- effectively reduced by this number)
!
                       IF ( IATM .EQ. 1 ) THEN
                           TRACE = ( MAT(JA+N1) - MAT(JA+N3) + MAT(JA+N1N1) - &
     &                               MAT(JA+P1N1) )/CONST2**2 + &
     &                             ( MAT(JA+N1) + MAT(JA+N1N1) )/(CONST1**2)
                          ELSE IF ( IATM .LT. NUMGRAD(ISTA) ) THEN
                           TRACE = ( 2*MAT(JA+N1) - MAT(JA+N2) - MAT(JA+N3) + &
     &                               2*MAT(JA+N1N1) - MAT(JA+N3N1) - &
     &                                 MAT(JA+P1N1) )/(CONST2**2)
                          ELSE
                           TRACE = ( MAT(JA+N1) - MAT(JA+N2) + MAT(JA+N1N1) - &
     &                               MAT(JA+N3N1) )/(CONST2**2)
                        ENDIF
                        IF ( CONST2 .EQ. 0 ) THEN
                             TRACE = 0.0
                        ENDIF
                        GRDTRA(ISTA) = GRDTRA(ISTA) + TRACE
                        GRDCNT(ISTA)=GRDCNT(ISTA)+2
                     ENDIF  !  kbit(constraint_bits,8)
                  ENDIF ! This station was from a selected b.l.
              ENDIF ! if ( kbit(lgrad(1), jatm) )
           ENDDO  !  do iatm
        END IF
!
! ----- Space list devices to separate stations
!
        IF ( KSCREEN ) THEN
             IPTR=IPTR+1
             WRITE ( LBUF(IPTR), "(1X)" )
             CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
             CALL NL_MN()
        ENDIF
        IF ( KSPOOL .AND. KFULLOUT .AND. NEED_SPA ) WRITE(23,"(1X)")
 820    CONTINUE
      END DO !Run over the list of stations
!
! --- Make the clock and atmos files for MDLPL, if neccessary.
!
      IF ( .NOT. KBATCH ) THEN
!
! ------ Writing piece-wise component of clock function
!
         WRITE ( 36, 210 )  ( ISITN_CHR(JZ),        JZ=1,NUMSTA )
         WRITE ( 36, 220 )  ( "     offset sigma    ", JZ=1,NUMSTA )
         WRITE ( 36, 220 )  ( "         ps    ps    ", JZ=1,NUMSTA )
 210     FORMAT ( ' yr mn dy hr min Julian Date      ', 256(5X,A8,5X) )
 220     FORMAT ( '                                  ', 256(A18) )
!
         DO J1=1,NUM_CLO
            CALL EPOC ( IM, ID, IYR, IHR, IMIN, JDATE_CLO(J1) )
            DO 4110 J11=1,NUMSTA
               IF ( IS_R8_NAN ( CLOCK_VALUES(J1,J11) ) .OR. &
     &              IS_R8_NAN ( CLOCK_VALUES(1,J11)  )      ) THEN
                    CD(J11) = 0.0D0
                 ELSE
                    CD(J11) = ( CLOCK_VALUES(J1,J11) - CLOCK_VALUES(1,J11) )*1.D12
                    IF ( CD(J11) >  1.D15 ) CD(J11) =  9.999999D15
                    IF ( CD(J11) < -1.D14 ) CD(J11) = -9.999999D14
               END IF 
               IF ( IS_R8_NAN ( CLOCK_SIGMAS(J1,J11) ) ) THEN
                    CS(J11) = 0.0D0
                  ELSE 
                    CS(J11) = CLOCK_SIGMAS(J1,J11)*1.D12 
                    IF ( CS(J11) >  1.D6 ) CS(J11) =  9.99999D6
               END IF
 4110       CONTINUE 
!            WRITE ( 36, '(5I3,F15.5,255(I15,I6))' ) IYR, IM, ID, IHR, IMIN, &
!     &              JDATE_CLO(J1), &
!     &            ( INT8( CLOCK_VALUES(J1,ISTA)*1.D12 - &
!     &                    CLOCK_VALUES(1,ISTA)*1.D12 ), &
!     &              INT8( CLOCK_SIGMAS(J1,ISTA)*1.D12 ), ISTA=1,NUMSTA )
            WRITE ( 36, '(5I3,F15.5,256(I15,I6))' ) IYR, IM, ID, IHR, IMIN, JDATE_CLO(J1), &
     &            ( INT8( CD(ISTA) ), INT8 ( CS(ISTA) ), ISTA=1,NUMSTA )
         ENDDO
         CLOSE ( UNIT=36 )
!
! ------ Writing total clock function
!
         WRITE ( 37, 210 )  ( ISITN_CHR(JZ),        JZ=1,NUMSTA )
         WRITE ( 37, 220 )  ( "     offset sigma    ", JZ=1,NUMSTA )
         WRITE ( 37, 220 )  ( "         ps    ps    ", JZ=1,NUMSTA )
!
! ------ Writing break clock function
!
         WRITE ( 38, 210 )  ( ISITN_CHR(JZ),        JZ=1,NUMSTA )
         WRITE ( 38, 220 )  ( "     offset sigma    ", JZ=1,NUMSTA )
         WRITE ( 38, 220 )  ( "         ps    ps    ", JZ=1,NUMSTA )
!
         DO J1=1,NUM_CLO
            IF ( J1 .GT. 1  .AND. IT_BRK .GT. 0 ) THEN
!
! -------------- Scans break-records to check: did a clock break occur between
! -------------- the J1-th and J1-1 -th epoch
!
                 DO J2=1,IT_BRK
                    IF ( JD_BRK(J2) .GT. JDATE_CLO(J1-1)  .AND. &
     &                   JD_BRK(J2) .LT. JDATE_CLO(J1)          ) THEN
!
! ---------------------- If the break occurred just between the current epoch
! ---------------------- and the previous one we prepeare the string which
! ---------------------- would contain the value of total clock function for
! ---------------------- the station the the clock break under consideration
! ---------------------- took place. For all other stations dollar sign will be
! ---------------------- printed
!
                         CALL EPOC ( IM, ID, IYR, IHR, IMIN, JD_BRK(J2) )
                         CALL CLRCH ( OUTL )
!
! ---------------------- Firstly make dollarization of the output line
!
                         WRITE ( UNIT=OUTL, FMT='(5I3,F15.5,256(A21))' ) IYR, IM, &
     &                           ID, IHR, IMIN, JD_BRK(J2), &
     &                           (  ' $$$$$$$$$$$ $$$$$$$$', ISTA=1,NUMSTA )
!
                         OUTB = OUTL
!
! ---------------------- Then print the value of total clock by overlapping it.
!
                         IBG = 31  + 21*(IST_BRK(J2)-1)
                         IEN = IBG + 20
                         WRITE ( UNIT=OUTL(IBG:IEN), FMT='(I15,I6)') &
     &                           NINT(( TC_BRK(J2) - TCLOCK_VALUES(1,ISTA) )*1.D12), &
     &                             0
!
! ---------------------- Formal uncertainty of "break clock" at the epoch
! ---------------------- of a break is obtained by linear interpolation of
! ---------------------- formal uncertainties between the epochs. Yeah, I know
! ---------------------- that it is not correct, though...
!
                         ISTB = IST_BRK(J2)
                         BS_BRK(J2) = CLOCK_SIGMAS(J1-1,ISTB) + &
     &                                ( CLOCK_SIGMAS(J1,ISTB) - &
     &                                  CLOCK_SIGMAS(J1-1,ISTB) )/ &
     &                                ( JDATE_CLO(J1) - JDATE_CLO(J1-1))* &
     &                                ( JD_BRK(J2) - JDATE_CLO(J1-1) )
                         IF ( DABS(BC_BRK(J2)*1.D12) .LT. 2.147483647D9 ) THEN
                              IBC_BRK = BC_BRK(J2)*1.D12
                            ELSE IF ( BC_BRK(J2)*1.D12 > 0.0D0 ) THEN
                              IBC_BRK =  2147483647
                            ELSE IF ( BC_BRK(J2)*1.D12 < 0.0D0 ) THEN
                              IBC_BRK = -2147483647
                         END IF
                         IF ( DABS(BS_BRK(J2)*1.D12) .LT. 2.147483647D9 ) THEN
                              IBS_BRK = BS_BRK(J2)*1.D12
                            ELSE IF ( BC_BRK(J2)*1.D12 > 0.0D0 ) THEN
                              IBS_BRK =  2147483647
                            ELSE IF ( BC_BRK(J2)*1.D12 < 0.0D0 ) THEN
                              IBS_BRK = -2147483647
                         END IF
                         WRITE ( UNIT=OUTB(IBG:IEN), FMT='(I15,I6)') &
     &                           IBC_BRK, IBS_BRK 
!
! ---------------------- And then write the line to the output file
!
                         OUTL(16:16) = 'B'
                         OUTB(16:16) = 'B'
                         WRITE ( 37, '(A)' ) OUTL(1:I_LEN(OUTL))
                         WRITE ( 38, '(A)' ) OUTB(1:I_LEN(OUTB))
                    END IF
                 END DO
            END IF
!
! --------- Now write date and total clock values for ordinary clock epochs
!
            CALL EPOC ( IM, ID, IYR, IHR, IMIN, JDATE_CLO(J1) )
            DO 4120 J12=1,NUMSTA
               IF ( IS_R8_NAN ( TCLOCK_VALUES(J1,J12) ) .OR. &
     &              IS_R8_NAN ( TCLOCK_VALUES(1,J12)  )      ) THEN
                    TD(J12) = 0.0D0
                 ELSE
                    TD(J12) = ( TCLOCK_VALUES(J1,J12) - TCLOCK_VALUES(1,J12) )*1.D12
                    IF ( TD(J12) >  1.D15 ) TD(J12) =  9.999999D15
                    IF ( TD(J12) < -1.D14 ) TD(J12) = -9.999999D14
               END IF 
               IF ( IS_R8_NAN ( BCLOCK_VALUES(J1,J12) ) .OR. &
     &              IS_R8_NAN ( BCLOCK_VALUES(1,J12)  )      ) THEN
                    BD(J12) = 0.0D0
                 ELSE
                    BD(J12) = ( BCLOCK_VALUES(J1,J12) - BCLOCK_VALUES(1,J12) )*1.D12
                    IF ( BD(J12) >  1.D15 ) BD(J12) =  9.999999D15
                    IF ( BD(J12) < -1.D14 ) BD(J12) = -9.999999D14
               END IF 
               IF ( IS_R8_NAN ( CLOCK_SIGMAS(J1,J12) ) ) THEN
                    CS(J12) = 0.0D0
                  ELSE 
                    CS(J12) = CLOCK_SIGMAS(J1,J12)*1.D12 
                    IF ( CS(J12) >  1.D6 ) CS(J12) =  9.99999D6
               END IF
 4120       CONTINUE 
!            WRITE ( 37, '(5I3,F15.5,236(I15,I6))' ) IYR, IM, ID, IHR, IMIN, &
!     &              JDATE_CLO(J1), &
!     &            ( INT8(TCLOCK_VALUES(J1,ISTA)*1.D12 - &
!     &              TCLOCK_VALUES(1,ISTA)*1.D12 ), &
!     &              0, ISTA=1,NUMSTA )
!            WRITE ( 38, '(5I3,F15.5,256(I15,I6))' ) IYR, IM, ID, IHR, IMIN, &
!     &              JDATE_CLO(J1), &
!     &            ( INT8(BCLOCK_VALUES(J1,ISTA)*1.D12 - &
!     &              BCLOCK_VALUES(1,ISTA)*1.D12 ), &
!     &              INT8(CLOCK_SIGMAS(J1,ISTA)*1.D12), ISTA=1,NUMSTA )
!!!
            WRITE ( 37, '(5I3,F15.5,256(I15,I6))' ) IYR, IM, ID, IHR, IMIN, &
     &              JDATE_CLO(J1), ( INT8(TD(ISTA)), 0, ISTA=1,NUMSTA )
            WRITE ( 38, '(5I3,F15.5,256(I15,I6))' ) IYR, IM, ID, IHR, IMIN, &
     &              JDATE_CLO(J1), ( INT8(BD(ISTA)), INT8(CS(ISTA)), ISTA=1,NUMSTA )
         ENDDO
!
         CLOSE ( UNIT=37 )
         CLOSE ( UNIT=38 )
!
         WRITE ( 39, 210 )  ( ISITN_CHR(JZ),        JZ=1,NUMSTA )
         WRITE ( 39, 220 )  ( "     offset sigma    ", JZ=1,NUMSTA )
         WRITE ( 39, 220 )  ( "         ps    ps    ", JZ=1,NUMSTA )
!
         DO J1=1,NUM_ATM
            CALL EPOC ( IM, ID, IYR, IHR, IMIN, JDATE_ATM(J1) )
            WRITE ( 39, '(5I3,F15.5,256(I15,I6))' ) IYR, IM, ID, IHR, IMIN, &
     &              JDATE_ATM(J1), &
     &            ( INT8(ATMOS_VALUES(J1,ISTA)), INT8(ATMOS_SIGMAS(J1,ISTA)), &
     &              ISTA=1,NUMSTA )
         ENDDO
         CLOSE ( UNIT=39 )
      ENDIF
!
! --- Compute the conversion constants for radians to time-seconds and
! --- radians to arcsconds
!
      FSTRC(1) = 8.64D4/(2.0D0*PI__NUM)
      FSTRC(2) = 1.296D6/(2.0D0*PI__NUM)
!
! --- Print source coordinate parameters
!
      KSRC=.FALSE.
      DO JSTR = 1,NUMSTR ! Run over the source list
         SIND(1)=0
         SIND(2)=0
         DO ITYPE = 1,2   ! Run over RA and DEC
            IF ( KBIT(LSTAR(1,ITYPE),JSTR) ) THEN ! Its turned on
               KSRC = .TRUE.
               NPARM = NPARM + 1
               SIND(ITYPE) = NPARM
               SIND_COO(JSTR,ITYPE) = NPARM
!
               VSTARC(ITYPE,JSTR) = VSTARC(ITYPE,JSTR) + MAT(JB+NPARM)
               PVAL = VSTARC(ITYPE,JSTR)
               PVAL = PVAL * FSTRC(ITYPE)
               IF ( ITYPE .EQ. 1  .AND. PVAL .LT. 0.D0 ) THEN
                    PVAL = PVAL + 86400.D0
               END IF
               MAT(JB+NPARM) = MAT(JB+NPARM) * FSTRC(ITYPE)
               MAT(JS+NPARM) = MAT(JS+NPARM) * FSTRC(ITYPE)
               SCSIG(NPARM)  = SCSIG(NPARM)  * FSTRC(ITYPE)
               IF ( DABS(PVAL) .GT. 1.D10 ) THEN
                    WRITE ( 6, * ) 'PVAL= ',PVAL
                    CALL ERR_PASS ( IUER, IER )
                    CALL ERR_LOG ( 3931, IER, 'A1JST', 'Error '// &
     &                  'of  internal control. Crazy position of source '// &
     &                  ISTRN_CHR(JSTR)//'. Nevertheless continue' )
                    PVAL = 1.D10
!@                  RETURN
               END IF
               CALL CNDMS ( MSD, ID, IM, PVAL )
!
! ------------ Convert the corrections, sigmas, and scaled sigmas to
! ------------ milliseconds of arc for additional print out.
! ------------ Keep in mind that the RA corrections are in time seconds and
! ------------ the dec corrections are in seconds of arc at this point.
!
               IF ( ITYPE .EQ. 1 ) THEN
!
! ----------------- Handle right ascension
!
                    COR = MAT(JB+NPARM)*15000.D0
                    XSIG= MAT(JS+NPARM)*15000.D0
                    SSIG= SCSIG(NPARM)*15000.D0
                    IF ( KPRINT(NPARM) ) THEN
                         IALPHA=MOD(JSTR-1,26)+1
                         STARSYMB=ALPHABET(IALPHA:IALPHA)
                         IF ( KBATCH ) STARSYMB = ' '
                         IF ( KSPOOL ) THEN
                              IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &                             SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                                   WRITE ( UNIT=STR, FMT=1400 ) NPARM, &
     &                                     ISTRN_CHR(JSTR), JNAME(JSTR), NAMCP_CHR(ITYPE), &
     &                                     ID, IM, PVAL, COR, XSIG, SSIG, TYPEPR
 1400                              FORMAT ( I5, ".", 2X, A, 2X, A, 2X, A, 1X, 2I3, 1X, &
     &                                      F11.8, 2X, 3(F11.4,' mas '), A6)
                                   CALL BLANK_TO_ZERO ( STR(41:42) ) ! insert leading
                                   CALL BLANK_TO_ZERO ( STR(44:45) ) ! zeroes if
                                   CALL BLANK_TO_ZERO ( STR(47:48) ) ! necessary
                                   WRITE ( UNIT=23, FMT='(A)' ) STR(1:I_LEN(STR))
                                 ELSE
                                   WRITE ( UNIT=STR, FMT=1500 ) NPARM, &
     &                                     ISTRN_CHR(JSTR), NAMCP_CHR(ITYPE), &
     &                                     ID, IM, PVAL, COR, XSIG, SSIG, TYPEPR
 1500                              FORMAT ( I5, ".",2X, A, 1X, A,9X,2I3,1X, &
     &                                      F11.8,2X,2(F10.4,1X,'m-asec ',5X), &
     &                                      (F10.4,1X,'m-asec ',4X),A6)
                                   CALL BLANK_TO_ZERO ( STR(36:37) ) ! insert leading
                                   CALL BLANK_TO_ZERO ( STR(39:40) ) ! zeroes if
                                   CALL BLANK_TO_ZERO ( STR(42:43) ) ! necessary
                                   WRITE ( UNIT=23, FMT='(A)' ) STR(1:I_LEN(STR))
                              END IF
                          END IF
!
                          IF ( KSCREEN ) THEN
                               IPTR=IPTR+1
                               IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &                              SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                                    WRITE ( LBUF(IPTR), 3400 ) NPARM, ISTRN_CHR(JSTR), &
     &                                      JNAME(JSTR), NAMCP_CHR(ITYPE), &
     &                                      ID, IM, PVAL, COR, XSIG
                                    CALL BLANK_TO_ZERO ( LBUF(IPTR)(41:42) ) ! insert leading
                                    CALL BLANK_TO_ZERO ( LBUF(IPTR)(44:45) ) ! zeroes if
                                    CALL BLANK_TO_ZERO ( LBUF(IPTR)(47:48) ) ! necessary
                                 ELSE
                                    WRITE ( LBUF(IPTR), 3400 ) NPARM, STARSYMB, &
     &                                      ISTRN_CHR(JSTR), NAMCP_CHR(ITYPE), &
     &                                      ID, IM, PVAL, COR, XSIG
!
                                    CALL BLANK_TO_ZERO ( LBUF(IPTR)(32:33) ) ! insert leading
                                    CALL BLANK_TO_ZERO ( LBUF(IPTR)(35:36) ) ! zeroes if
                                    CALL BLANK_TO_ZERO ( LBUF(IPTR)(38:39) ) ! necessary
                               END IF
 3400                          FORMAT ( I5, ".", 1X, A, 2X, A, 2X, A, 2X,2I3, &
     &                                   1X, F9.6, 2 (F9.2, ' mas ') )
 3410                          FORMAT ( I5, ".",A1,1X,4A2,1X,4A2, 5X,2I3, &
     &                                   1X,F9.6, 2(F9.2,3X,'mas  ') )
                               CALL ADDSTR_F(LBUF(IPTR)(:PAGEWID) )
                               CALL NL_MN()
                         ENDIF
                    ENDIF
               END IF ! Handle the RA's
!
               IF ( ITYPE .EQ. 2) THEN
!
! ----------------- Handle the declinations
!
                    COR  = MAT(JB+NPARM) * 1000.D0
                    XSIG = MAT(JS+NPARM) * 1000.D0
                    SSIG = SCSIG(NPARM) * 1000.D0
                    IF ( KPRINT(NPARM)) THEN
                         IALPHA=MOD(JSTR-1,26)+1
                         STARSYMB = ALPHABET(IALPHA:IALPHA)
                         IF (KBATCH) STARSYMB = ' '
                         IF ( KSPOOL) THEN
                              IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &                             SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                                   WRITE ( UNIT=STR, FMT=1401 ) NPARM, &
     &                                     ISTRN_CHR(JSTR), JNAME(JSTR), NAMCP_CHR(ITYPE), &
     &                                     MSD, ID, IM, PVAL, COR, XSIG, SSIG, TYPEPR
 1401                              FORMAT ( I5, ".", 2X, A, 2X, A, 2X, A, 1X, A1, I2, I3, 1X, &
     &                                      F11.8, 2X, 3(F11.4, ' mas '), A6 )
                                   CALL BLANK_TO_ZERO ( STR(40:42) ) ! insert leading
                                   CALL BLANK_TO_ZERO ( STR(44:45) ) ! zeroes if
                                   CALL BLANK_TO_ZERO ( STR(47:48) ) ! necessary
                                ELSE
                                   WRITE ( UNIT=STR, FMT=1501 ) NPARM, &
     &                                     ISTRN_CHR(JSTR), NAMCP_CHR(ITYPE), &
     &                                     MSD, ID, IM, PVAL, COR, XSIG, SSIG, TYPEPR
 1501                              FORMAT ( I5,".",2X,A,1X,A,9X,A1,I2,I3,1X, &
     &                                      F10.7,2X,3(F11.4,1X,'m-asec ',4X),A6 )
                                   CALL BLANK_TO_ZERO ( STR(36:37) ) ! insert leading
                                   CALL BLANK_TO_ZERO ( STR(39:40) ) ! zeroes if
                                   CALL BLANK_TO_ZERO ( STR(42:43) ) ! necessary
                              END IF
                              WRITE ( UNIT=23, FMT='(A)' ) STR(1:I_LEN(STR))
                         END IF
                         IF ( KSCREEN ) THEN
                              IPTR=IPTR+1
                              IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &                             SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                                   WRITE ( LBUF(IPTR), 3402 ) NPARM, &
     &                                     ISTRN_CHR(JSTR), JNAME(JSTR), NAMCP_CHR(ITYPE), &
     &                                     MSD, ID, IM, PVAL, COR, XSIG
                                   CALL BLANK_TO_ZERO ( LBUF(IPTR)(41:42) ) ! insert leading
                                   CALL BLANK_TO_ZERO ( LBUF(IPTR)(44:45) ) ! zeroes if
                                   CALL BLANK_TO_ZERO ( LBUF(IPTR)(47:48) ) ! necessary
                                 ELSE
                                   WRITE ( LBUF(IPTR), 3427 ) NPARM,  STARSYMB, &
     &                                     (ISTRN(L,JSTR),L=1,4), (NAMCP(L,ITYPE),L=1,4), &
     &                                     MSD, ID, IM, PVAL, COR, XSIG
                                   CALL BLANK_TO_ZERO ( LBUF(IPTR)(32:33) ) ! insert leading
                                   CALL BLANK_TO_ZERO ( LBUF(IPTR)(35:36) ) ! zeroes if
                                   CALL BLANK_TO_ZERO ( LBUF(IPTR)(38:39) ) ! necessary
                              END IF
                              CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                              CALL NL_MN()
 3402                         FORMAT ( I5, ".", 1X, A, 2X, A, 2X, A, 2X, A1, &
     &                                 I2, I3, 1X, F9.6, 2(F9.2, ' mas ') )
 3427                         FORMAT ( I5, ".", A1, 1X, 4A2, 1X, 4A2, 5X, A1, &
     &                                 I2, I3, 1X, F9.6, 2(F9.2,3X,'mas  ') )
                          ENDIF
                    ENDIF
                    IF ( SIND(1) .GT. 0   .AND.  SIND(2) .GT. 0 ) THEN
                         DO I=1,2
                            DO J=1,I
                               SIGSX(INDX4(J,I)) = MAT(JA+INDX8(SIND(J),SIND(I)))
                            ENDDO
                         ENDDO
!
                         CALL NOUT_R8 ( 2, DUM )
                         CALL SCALER  ( SIGSX, DUM, SCALE, 2 )
                    ENDIF
               END IF ! Handle the declinations
!
! ------------ Now print out the correction and the sigma below the seconds
! ------------ part of the parameter.
!
               IF ( KSPOOL .AND. .NOT. KBATCH ) THEN
                    WRITE(23,1402) MAT(JB+NPARM),SCSIG(NPARM)
 1402               FORMAT(16X,"  CORRECTION",12X,F10.7,4X,/, &
     &                     16X,'SCALED SIGMA',12X,F10.7,4X,/)
               END IF
!
               IF ( KSCREEN .AND. .NOT.KBATCH ) THEN
                    IF ( ITYPE .EQ. 1 ) THEN
                         UNIT = 'sec      '
                       ELSE IF ( ITYPE .EQ. 2 ) THEN
                         UNIT = 'arcsec   '
                    END IF
!
                    IPTR=IPTR+1
                    IF ( SCSIG(NPARM) .GT. 1.D-20 ) THEN
                         SCERR = DABS ( MAT(JB+NPARM) )/SCSIG(NPARM)
                       ELSE
                         SCERR = 0.0D0
                    END IF
                    IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &                   SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                         WRITE ( LBUF(IPTR), 3412 ) MAT(JB+NPARM), UNIT, SCERR
                       ELSE
                         WRITE ( LBUF(IPTR), 3432 ) MAT(JB+NPARM), UNIT, SCERR
                    END IF
                    CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                    CALL NL_MN()
!
                    IF ( SRC_LISTING_STYLE .NE. SRC_POST2021_SPOOL__FMT .AND. &
     &                   SRC_LISTING_STYLE .NE. SRC_POST2024_SPOOL__FMT       ) THEN
                         IPTR=IPTR+1
                         WRITE ( LBUF(IPTR), 3413 ) SCSIG(NPARM), UNIT
                         CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                         CALL NL_MN()
                    END IF
 3412               FORMAT ( 29X, "Correction", 7X, F9.6, 1X, A, 4X, F9.2, &
     &                            " sigmas" )
 3432               FORMAT ( 16X, "  Correction", 9X, F9.6, 1X, A,3X,F9.2, &
     &                            " sigmas" )
 3413               FORMAT ( 16X, 'Scaled Sigma', 9X, F9.6, 1X, A )
               ENDIF
               IF ( SIND(1).GT.0  .AND.  SIND(2).GT.0 ) THEN
                    IF ( KSPOOL .AND. KPRINT(NPARM) ) THEN
                         CALL ERR_PASS ( IUER, IER )
                         STR = JD_TO_DATE ( SOU_EST_EPOCH-32.184/86400.0D0, IER )
                         IF ( IER .NE. 0 ) THEN
                              WRITE ( 6, * ) ' SOU_EST_EPOCH = ', SOU_EST_EPOCH
                              CALL ERR_LOG ( 3932, IUER, 'A1JST', 'Wrong date' ) 
                              RETURN 
                         END IF
                         IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &                        SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                              WRITE  ( 23, 1091 ) ISTRN_CHR(JSTR), JNAME(JSTR), &
     &                                            SIGSX(2), STR(1:19)
 1091                         FORMAT ( 8X, A, 2X, A, 2X, "CORREL.", 3X, F7.4, 2X, &
     &                                       "Reference date: ", A )
                            ELSE
                              WRITE  ( 23, 1092 ) ISTRN_CHR(JSTR), SIGSX(2), &
     &                                            STR(1:19)
 1092                         FORMAT ( 8X, A," CORRELATION", 2X, F9.4, 2X, &
     &                                       "Reference date: ", A )
                         END IF
                    ENDIF
               ENDIF
            END IF ! Its turned on (estimated)
         END DO ! Run over RA and DEC
         IF ( KBATCH .AND. KSPOOL .AND. KPRINT(NPARM) .AND. &
     &        ( SIND(1).GT.0 .OR. SIND(2).GT.0 ) )  WRITE ( 23, '(1X)' )
      END DO ! Run over the source list
!
! --- Handle proper motions here
!
      IF ( KGLOBALS .AND. ( FL_VTD_GLB .OR.  FL_VTD_SES ) ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_GET_APR_PRP ( %VAL(VTD_ADR), L_PRP, C_PRP, RA_PRP, DEC_PRP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3933, IUER, 'A1JST', 'Error in an attempt '// &
     &              'to get apriori proper motions' )
                RETURN 
          END IF
      END IF
      DO JSTR = 1,NUMSTR ! Run over the source list
         SIND(1)=0
         SIND(2)=0
         DO ITYPE = 1,2   ! Run over RA and DEC
            IF ( KBIT(LPROP(1,ITYPE), JSTR) ) THEN ! Its turned on
                 NPARM = NPARM + 1
                 SIND_PRP(JSTR,ITYPE) = NPARM
                 SIND(ITYPE) = NPARM
!
! -------------- Convert from radians/sec to mas/year
!
                 MAT(JB+NPARM) = MAT(JB+NPARM) * FSTRC(ITYPE) * CNV(ITYPE) * YEAR__TO__SEC
                 MAT(JS+NPARM) = MAT(JS+NPARM) * FSTRC(ITYPE) * CNV(ITYPE) * YEAR__TO__SEC
                 SCSIG(NPARM)  = SCSIG(NPARM)  * FSTRC(ITYPE) * CNV(ITYPE) * YEAR__TO__SEC
                 PVAL = MAT(JB+NPARM)
                 XSIG = MAT(JS+NPARM)
                 SSIG = SCSIG(NPARM)
!
                 IF ( KPRINT(NPARM) ) THEN
                      IF ( KSPOOL ) THEN
                           IND_PRP = LTM_DIF ( 1, L_PRP, C_PRP, ISTRN_CHR(JSTR) )
                           IF ( IND_PRP > 0 ) THEN
                                IF ( ITYPE == 1 ) THEN
                                     PRP_VAL(ITYPE) = RA_PRP(IND_PRP) * RAD__TO__MAS * YEAR__TO__SEC
                                   ELSE IF ( ITYPE == 2 ) THEN
                                     PRP_VAL(ITYPE) = DEC_PRP(IND_PRP) * RAD__TO__MAS * YEAR__TO__SEC
                                END IF
                              ELSE
                                PRP_VAL  = 0.0D0
                           END IF
                           IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &                          SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                                WRITE ( 23, 1403 ) NPARM, ISTRN_CHR(JSTR), JNAME(JSTR), &
     &                                             NAMCP_CHR(ITYPE), &
     &                                             (PRP_VAL(ITYPE) + PVAL), &
     &                                             MAT(JB+NPARM), &
     &                                             XSIG, SSIG, TYPEPR
                              ELSE
                                WRITE ( 23, 1404 ) NPARM, ISTRN_CHR(JSTR), &
     &                                             NAMCP_CHR(ITYPE), &
     &                                             (PRP_VAL(ITYPE) + PVAL), &
     &                                             MAT(JB+NPARM), &
     &                                             XSIG, SSIG, TYPEPR
                           END IF
!
 1403                      FORMAT ( I5, ".  ", A, 2X, A, 2X, A, " rate   ", &
     &                              F10.4, 1X, "mas/yr", 3 (F10.4, ' mas/yr '), 2X, A6 )
 1404                      FORMAT ( I5, ".  ", A, 1X, A, " rate   ", &
     &                              F10.4, 1X, "mas/yr", 3(F10.4,2X,'mas/yr'), 2X, A6)
                     END IF
                     IF ( KSCREEN )  THEN
                          IPTR=IPTR+1
                          WRITE ( LBUF(IPTR), 3403 ) NPARM, &
     &                            (ISTRN(L,JSTR),L=1,4), &
     &                            ( NAMCP(L,ITYPE), L=1,4 ), PVAL, &
     &                            MAT(JB+NPARM), SSIG
                          CALL ADDSTR_F(LBUF(IPTR)(:PAGEWID) )
                          CALL NL_MN()
 3403                     FORMAT ( I5, ".  ", 4A2, 1X, 4A2, " rate", F13.4, &
     &                             1X, 'mas/yr', 2(F10.4,4X,'mas/yr') )
                     ENDIF
                  ENDIF
               END IF
         ENDDO
!
         IF ( SIND(1) .GT.0 .AND. SIND(2).GT.0 .AND. KPRINT(NPARM) ) THEN
              DO I=1,2
                 DO J=1,I
                    SIGSX(INDX4(J,I)) = MAT(JA+INDX8(SIND(J),SIND(I)))
                 ENDDO
              ENDDO
!
              CALL NOUT_R8 ( 2, DUM )
              CALL SCALER ( SIGSX, DUM, SCALE, 2 )
              IF ( KSPOOL .AND. KPRINT(NPARM) ) THEN
!
! ---------------- Compute components of the correlation matrix between source coordinates
! ---------------- aand propoer motions
!
                   IF ( DABS(MAT(JA+INDX8(SIND_PRP(JSTR,1),SIND_PRP(JSTR,1)))) > 1.D-48 .AND. &
     &                  DABS(MAT(JA+INDX8(SIND_PRP(JSTR,2),SIND_PRP(JSTR,2)))) > 1.D-48 .AND. &
     &                  DABS(MAT(JA+INDX8(SIND_COO(JSTR,1),SIND_COO(JSTR,1)))) > 1.D-48 .AND. &
     &                  DABS(MAT(JA+INDX8(SIND_COO(JSTR,2),SIND_COO(JSTR,2)))) > 1.D-48       ) THEN
                        COREL_AAR  = MAT(JA+INDX8(SIND_COO(JSTR,1),SIND_PRP(JSTR,1)))/ &
     &                               DSQRT ( MAT(JA+INDX8(SIND_COO(JSTR,1),SIND_COO(JSTR,1))) * &
     &                                       MAT(JA+INDX8(SIND_PRP(JSTR,1),SIND_PRP(JSTR,1)))   )
!
                        COREL_ADR  = MAT(JA+INDX8(SIND_COO(JSTR,1),SIND_PRP(JSTR,2)))/ &
     &                               DSQRT ( MAT(JA+INDX8(SIND_COO(JSTR,1),SIND_COO(JSTR,1))) * &
     &                                       MAT(JA+INDX8(SIND_PRP(JSTR,2),SIND_PRP(JSTR,2)))   )
!
                        COREL_DRA  = MAT(JA+INDX8(SIND_PRP(JSTR,2),SIND_COO(JSTR,1)))/ &
     &                               DSQRT ( MAT(JA+INDX8(SIND_PRP(JSTR,2),SIND_PRP(JSTR,2))) * &
     &                                       MAT(JA+INDX8(SIND_COO(JSTR,1),SIND_COO(JSTR,1)))   )
!
                        COREL_DDR  = MAT(JA+INDX8(SIND_PRP(JSTR,2),SIND_COO(JSTR,2)))/ &
     &                               DSQRT ( MAT(JA+INDX8(SIND_COO(JSTR,2),SIND_COO(JSTR,2))) * &
     &                                       MAT(JA+INDX8(SIND_PRP(JSTR,2),SIND_PRP(JSTR,2)))   )
!
                        COREL_ARDR = MAT(JA+INDX8(SIND_PRP(JSTR,1),SIND_PRP(JSTR,2)))/ &
     &                               DSQRT ( MAT(JA+INDX8(SIND_PRP(JSTR,1),SIND_PRP(JSTR,1))) * &
     &                                       MAT(JA+INDX8(SIND_PRP(JSTR,2),SIND_PRP(JSTR,2)))   )
!
!                         DO I=1,2
!                            DO J=1,I
!                               SIGSX(INDX4(J,I)) = MAT(JA+INDX8(SIND(J),SIND(I)))
!                            ENDDO
!                         ENDDO
!!
!                         CALL NOUT_R8 ( 2, DUM )
!                         CALL SCALER  ( SIGSX, DUM, SCALE, 2 )

!
                      ELSE 
                         COREL_AAR  = 0.0D0
                         COREL_ADR  = 0.0D0
                         COREL_DRA  = 0.0D0
                         COREL_DDR  = 0.0D0
                         COREL_ARDR = 0.0D0
                   END IF
!
                   IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT .OR. &
     &                  SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT      ) THEN
                        WRITE ( 23, 1093 ) ISTRN_CHR(JSTR), JNAME(JSTR), COREL_AAR, COREL_ADR, &
     &                                     COREL_DRA, COREL_DDR, COREL_ARDR
 1093                   FORMAT ( 8X, A, 2X, A, 2X, "PRP. CORREL.", '  AAR: ', F7.4, '  ADR: ', F7.4, &
     &                           '  DRA: ', F7.4, '  DDR: ', F7.4, '  ARDR: ', F7.4 )
                      ELSE
                        WRITE ( 23, 1094 ) ISTRN_CHR(JSTR), SIGSX(2)
 1094                   FORMAT ( 8X, A," PROPER MOTION CORRELATION", 2X, F9.4 )
                   END IF
              END IF
         ENDIF
!
         IF ( KBATCH  .AND.  KSPOOL  .AND.  (SIND(1).GT.0 .OR. SIND(2).GT.0) &
     &        .AND. KPRINT(NPARM) )  WRITE ( 23, '(1X)' )
      END DO ! Run over the source list for proper motions
!
      IF ( KGLOBALS ) THEN
           CALL GET_NAMES ( C_PAR, INT2(20), M_GPA, L_PAR, TRUE__L2, &
     &                      TRUE__L2 )

           CALL ERR_PASS ( IUER, IER )
           IF ( FL_VTD_GLB .OR.  FL_VTD_SES ) THEN
!
! ------------- VTD style of a priori
!
                CALL VTD_APR_SPOOL   ( 23, %VAL(VTD_ADR), IER )
              ELSE 
                CALL SOLVE_APR_SPOOL ( 23, %VAL(ADR_BSP), IER )
                L_PRP = 0
           END IF
!
           IF ( L_EHEO > 0 ) THEN
!
! ------------- Handle coefficients of the harmonic variations in Earth 
! ------------- orientation parameters
!
                CALL ERR_PASS ( IUER, IER )
                CALL EHEO_RES ( L_EHEO, %VAL(ADR_EHEO), &
     &                          MJD_EHEO_REF, TAI_EHEO_REF, L_PAR, C_PAR, &
     &                          MAT(JB+1), MAT(JA+1), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 3933, IUER, 'A1JST', 'Error in attempt '// &
     &                   'to print results of the harmonic model in Earth '// &
     &                   'orientation' )
                     RETURN
                END IF
           END IF
!
           IF ( L_EERM > 0 ) THEN
!
! ------------- Handle coefficients of the Empirical Earth Rotation Model here
!
! ------------- Print into the listing results of the Earth's rotation model
! ------------- estimation
!
                CALL ERR_PASS ( IUER, IER )
                CALL ERM_RES  ( %VAL(ADR_EERM), %VAL(VTD_ADR), L_PAR, C_PAR, &
     &                          MAT(JB+1), MAT(JA+1), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 3934, IUER, 'A1JST', 'Error in attempt '// &
     &                   'to print results of the Earth rotation model '// &
     &                   'estimation' )
                     RETURN
                END IF
           END IF
!
! -------- Restore VSITEC 
!
           CALL COPY_R8 ( 3*INT4(MAX_STA), VSITEC,      TOTAL_SITEC )
           CALL COPY_R8 ( 3*INT4(MAX_STA), VSITEC_SAVE, VSITEC      )
!
           IF ( L_SAV .GT. 0 ) THEN
!
! ------------- Restoration of some elements of covariance matrix which were
! ------------- changed during processing. The triciky point is that we have to
! ------------- restore elements in reverse order.
!
                DO 460 J6=L_SAV,1,-1
                   CALL COPY_V ( 1, VAL_SAV(J6), %VAL(ADR_SAV(J6)) )
 460            CONTINUE
                L_SAV = 0
           END IF
!
           IF ( L_HPE > 0       .AND. &
     &          ADR_HPE .NE. 0        ) THEN
!
! ------------- Aga. We have estimated harmonic site position variations.
! ------------- Let's call a special routine for putting more information
! ------------- about the estimates and their covariances into the spool file
!
                CALL HPESOL_CREATE ( %VAL(ADR_HPE) )
                CALL GET_NAMES ( C_PAR, INT2(20), M_GPA, L_PAR, TRUE__L2, &
     &                           TRUE__L2 )
!
                CALL ERR_PASS ( IUER, IER )
                CALL HPE_RES  ( L_HPE, %VAL(ADR_HPE), L_PAR, C_PAR, &
     &                          INT4(NUMSTA), MAT(JB+1), MAT(JA+1), 1.0D0, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 3935, IUER, 'A1JST', 'Error in attempt '// &
     &                   'to print results of harmoinic site position '// &
     &                   'estimation' )
                     RETURN
                END IF
           END IF
!
           IF ( L_SPE > 0      .AND. &
     &          ADR_SPE .NE. 0       ) THEN
!
! ------------- Aga. We have modeled position of some stations with spline.
! ------------- Let's call a special routine for putting more information
! ------------- about the estimates and their covariances into the spool file
!
                CALL GET_NAMES ( C_PAR, INT2(20), M_GPA, L_PAR, TRUE__L2, &
     &                           TRUE__L2 )
!
                CALL ERR_PASS ( IUER, IER )
                CALL SPE_RES  ( L_SPE, %VAL(ADR_SPE), L_PAR, C_PAR, MAT(JB+1), &
     &                          MAT(JA+1), 1.0D0, TIME0*YEAR__TO__DAY, &
     &                          SIT_EST_EPOCH, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 3936, IUER, 'A1JST', 'Error in attempt '// &
     &                   'to print results of site position estimation '// &
     &                   'modeled by a spline' )
                     RETURN
                END IF
           END IF
!
! -------- It is time to go home, Mr. Globals, isn't it?
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! === Atmosphere statistics
!
      NEED_HEAD = .TRUE.
      IF ( KBIT( CONSTRAINT_BITS, INT2(2) ) .AND. &
     &    .NOT. KBIT( PRE_IP(3), INT2(10) )       ) THEN
           DO I=1,MIN(NUMSTA,MAX_ARC_STA)
!
! ----------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
              IF ( .NOT. CHECK_STABIT ( I ) ) GOTO 830
!
              IF ( ATMCNT(I) .GT. 0 ) THEN
                   IF ( NEED_HEAD .AND. (.NOT. KGLOBALS .OR. .NOT.KBATCH) ) THEN
                        IF ( KSPOOL  ) WRITE(23,9945)
9945                    FORMAT(' Atmosphere Constraint Statistics')
                        IF ( KSCREEN ) THEN
                             IPTR=IPTR+1
                             WRITE ( LBUF(IPTR), 9945 )
                             CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                             CALL NL_MN()
                        ENDIF
                        NEED_HEAD = .FALSE.
                   ENDIF
!
                   OVRRMS     = OVRRMS+ATMRMS(I)
                   OVRSTA     = OVRSTA+ATMSTA(I)
                   OVRCNT     = OVRCNT+ATMCNT(I)
                   OVRCNTX    = OVRCNTX+ATMCNTX(I)
                   OVRTRA_ATM = OVRTRA_ATM+ATMTRA(I)
                   IF ( ABS(ATMCNTX(I)) .EQ. 0 ) THEN
                        ATMRMS(I)  = 0.0D0
                        ATMSTA(I)  = 0.0D0
                      ELSE 
                        ATMRMS(I)  = SQRT(ATMRMS(I)/ATMCNTX(I))
                        ATMSTA(I)  = SQRT(ATMSTA(I)/(ATMCNTX(I)))
                   END IF
              ENDIF
!
              IF ( KSPOOL .AND. ATMCNT(I).GT.0 ) THEN
                   WRITE ( 23, 9946) I, (ISITN(J,I),J=1,4), SACNST(I), &
     &                     ATMRMS(I), ATMSTA(I), ATMTRA(I)/ATMCNT(I), ATMCNT(I)
9946               FORMAT ( ' ', I2, '. ', 4A2, ' Input ', F6.2, ' ps/h RMS ', &
     &                      F6.2, ' ps/h NRMS ', F5.2, ' share ', F4.2, &
     &                      ' count ', I3 )
              ENDIF
!
              IF ( KSCREEN .AND. ATMCNT(I).GT.0 ) THEN
                   IPTR=IPTR+1
                   WRITE ( LBUF(IPTR), 9946 ) I, (ISITN(J,I),J=1,4), SACNST(I), &
     &                     ATMRMS(I), ATMSTA(I), ATMTRA(I)/ATMCNT(I), ATMCNT(I)
                   CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                   CALL NL_MN()
              ENDIF
 830          CONTINUE
        ENDDO
!
        IF ( OVRCNT .GT. 0 ) THEN
             IF ( OVRCNTX > 0 ) THEN
                  OVRRMS = SQRT ( OVRRMS/OVRCNTX )
                  OVRSTA = SQRT ( OVRSTA/OVRCNTX )
                ELSE
                  OVRRMS = 0.0D0
                  OVRSTA = 0.0D0
             END IF
             IF ( KSPOOL ) THEN
                  WRITE ( 23, 9947 ) OVRRMS, OVRSTA, OVRTRA_ATM/OVRCNT, OVRCNT
             END IF
9947         FORMAT ( 5X, 'Overall ','       ',6X,'      RMS ',F6.2, &
     &                ' ps/h NRMS ',F5.2,' share ',F4.2,' count ',I3,/)
9937         FORMAT ( 5X, 'Overall ','       ',6X,'      RMS ',F6.2, &
     &                    ' ps/h NRMS ',F5.2,' share ',F4.2,' count ',I3)
             IF ( KSCREEN ) THEN
                  IPTR=IPTR+1
                  WRITE ( LBUF(IPTR), 9937 ) OVRRMS, OVRSTA, &
     &                                       OVRTRA_ATM/OVRCNT, OVRCNT
                  CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                  CALL NL_MN()
             ENDIF
        ENDIF
      ENDIF
      IF ( NEED_HEAD .AND. KSPOOL .AND. .NOT. KGLOBALS ) WRITE(23,9948)
9948  FORMAT ( ' Atmosphere Constraint Statistics - NONE',/ )
!
! === Gradient statistics
!
      NEED_HEAD = .TRUE.
      IF ( KBIT( CONSTRAINT_BITS, INT2(8)) .AND. &
     &    .NOT. KBIT( PRE_IP(3), INT2(10) ) ) THEN
           DO I=1,MAX(NUMSTA,MAX_ARC_STA)
!
! ----------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
              IF ( .NOT. CHECK_STABIT ( I ) ) GOTO 840
!
              IF ( GRDCNT(I).GT.0 ) THEN
                 IF ( NEED_HEAD .AND. ( .NOT.KGLOBALS .OR. .NOT.KBATCH ) ) THEN
                      IF ( KSPOOL ) WRITE(23,9845)
                      IF ( KSCREEN ) THEN
                           IPTR=IPTR+1
                           WRITE ( LBUF(IPTR), 9845 )
                           CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                           CALL NL_MN()
                      ENDIF
9845                  FORMAT(' Gradient Constraint Statistics')
                      NEED_HEAD = .FALSE.
                 ENDIF
!
                 OVRRMS_GRD  = OVRRMS_GRD+GRDRMS(I)
                 OVRSTA_GRD  = OVRSTA_GRD+GRDSTA(I)
                 OVRCNT_GRD  = OVRCNT_GRD+GRDCNT(I)
                 OVRCNTX_GRD = OVRCNTX_GRD+GRDCNTX(I)
                 OVRTRA_GRD  = OVRTRA_GRD+GRDTRA(I)
                 GRDRMS(I)   = SQRT(GRDRMS(I)/GRDCNTX(I))
                 GRDSTA(I)   = SQRT(GRDSTA(I)/(GRDCNTX(I)))
            ENDIF
            IF ( KSPOOL .AND. GRDCNT(I) .GT. 0.0D0 ) THEN
                 WRITE ( 23, 9846) I, (ISITN(J,I),J=1,4), GRADCONS(2), &
     &                   GRDRMS(I), GRDSTA(I), GRDTRA(I)/GRDCNT(I), GRDCNT(I)
9846             FORMAT( ' ', I2, '. ', 4A2, ' Input ',F6.2,' mm/d RMS ',F6.2, &
     &                  ' mm/d NRMS ',F5.2,' share ',F4.2,' count ',I3)
            ENDIF
            IF ( KSCREEN .AND. GRDCNT(I).GT.0 ) THEN
                 IPTR=IPTR+1
                 WRITE ( LBUF(IPTR),9846) &
     &                   I, (ISITN(J,I),J=1,4), GRADCONS(2), GRDRMS(I), &
     &                   GRDSTA(I), GRDTRA(I)/GRDCNT(I), GRDCNT(I)
                 CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                 CALL NL_MN()
            ENDIF
 840        CONTINUE
        ENDDO
        IF ( OVRCNT_GRD.GT.0 ) THEN
             OVRRMS_GRD = SQRT(OVRRMS_GRD/OVRCNTX_GRD)
             OVRSTA_GRD = SQRT(OVRSTA_GRD/OVRCNTX_GRD)
             IF ( KSPOOL ) WRITE(23, &
     &            9847)OVRRMS_GRD, OVRSTA_GRD, OVRTRA_GRD/OVRCNT_GRD, OVRCNT_GRD
9847         FORMAT ( 5X, 'Overall ','       ',6X,'      RMS ',F6.2, &
     &                    ' mm/d NRMS ',F5.2,' share ',F4.2,' count ',I3,/)
9837         FORMAT ( 5X, 'Overall ','       ',6X,'      RMS ',F6.2, &
     &                    ' mm/d NRMS ',F5.2,' share ',F4.2,' count ',I3)
             IF ( KSCREEN ) THEN
                  IPTR=IPTR+1
                  WRITE ( LBUF(IPTR), 9837 ) OVRRMS_GRD, OVRSTA_GRD, &
     &                    OVRTRA_GRD/OVRCNT_GRD, OVRCNT_GRD
                  CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                  CALL NL_MN()
             ENDIF
        ENDIF
      ENDIF
!
! === Clock statistics
!
      NEED_HEAD = .TRUE.
      IF ( KBIT( CONSTRAINT_BITS, INT2(3) ) .AND. .NOT.KBIT( PRE_IP(3), &
     &     INT2(10)) ) THEN
         DO I=1,MAX(NUMSTA,MAX_ARC_STA)
!
! --------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
            IF ( .NOT. CHECK_STABIT ( I ) ) GOTO 850
!
            IF ( CLKCNT(I) .GT. 0 ) THEN
               IF ( NEED_HEAD .AND. (.NOT.KGLOBALS.OR..NOT.KBATCH) ) THEN
                  IF ( KSPOOL ) WRITE(23,9955)
                  IF ( KSCREEN ) THEN
                     IPTR=IPTR+1
                     WRITE ( LBUF(IPTR), 9955 )
                     CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                     CALL NL_MN()
                  ENDIF
9955              FORMAT(' Clock Constraint Statistics')
                  NEED_HEAD = .FALSE.
               ENDIF
!
               OVRRMS_CLK  = OVRRMS_CLK  + CLKRMS(I)
               OVRSTA_CLK  = OVRSTA_CLK  + CLKSTA(I)
               OVRCNT_CLK  = OVRCNT_CLK  + CLKCNT(I)
               OVRCNTX_CLK = OVRCNTX_CLK + CLKCNTX(I)
               OVRTRA_CLK  = OVRTRA_CLK  + CLKTRA(I)
               IF ( CLKCNTX(I) > 0 ) THEN
                    CLKRMS(I) = SQRT( CLKRMS(I)/CLKCNTX(I) )
                    CLKSTA(I) = SQRT( CLKSTA(I)/CLKCNTX(I) )
                  ELSE 
                    CLKRMS(I) = 0.0D0
                    CLKSTA(I) = 0.0D0
               END IF
               IF ( .NOT. OLD_CLOCKS ) THEN
                    CLKRMS(I) = CLKRMS(I) / 0.036d0
                    CLKSTA(I) = CLKSTA(I) / 0.036d0
               ENDIF
            ENDIF
            IF ( KSPOOL .AND. CLKCNT(I).GT.0 ) THEN
                 WRITE ( 23, 9956 ) I, (ISITN(J,I),J=1,4), SCCNST(I), &
     &                 CLKRMS(I), CLKSTA(I), CLKTRA(I)/CLKCNT(I), CLKCNT(I)
9956             FORMAT ( ' ', I2, '. ', 4A2, ' Input ', F6.2, ' D-14 RMS ', &
     &                    F6.2,' D-14 NRMS ',F5.2,' share ', F4.2,' count ',I3 )
            END IF
            IF ( KSCREEN .AND. CLKCNT(I).GT.0 ) THEN
                 IPTR=IPTR+1
                 WRITE ( LBUF(IPTR), 9956 ) I, (ISITN(J,I),J=1,4), SCCNST(I), &
     &                 CLKRMS(I), CLKSTA(I), CLKTRA(I)/CLKCNT(I), CLKCNT(I)
                 CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                 CALL NL_MN()
            ENDIF
 850        CONTINUE
         ENDDO
!
         IF ( OVRCNT_CLK .GT. 0 ) THEN
              OVRRMS_CLK=SQRT(OVRRMS_CLK/OVRCNTx_CLK)
              OVRSTA_CLK=SQRT(OVRSTA_CLK/OVRCNTx_CLK)
              IF ( .NOT. OLD_CLOCKS ) THEN
                   OVRRMS_CLK=OVRRMS_CLK / 0.036D0
                   OVRSTA_CLK=OVRSTA_CLK / 0.036D0
              ENDIF
!
              IF ( KSPOOL ) THEN
                   WRITE ( 23, 9957 ) OVRRMS_CLK, OVRSTA_CLK, &
     &                      OVRTRA_CLK/OVRCNT_CLK, OVRCNT_CLK
9957               FORMAT ( 5X, 'Overall ','       ',6X,'      RMS ', &
     &                 F6.2,' D-14 NRMS ',F5.2,' share ',F4.2,' count ',I3,/)
9967               FORMAT ( 5X, 'Overall ','       ',6X,'      RMS ', &
     &                 F6.2,' D-14 NRMS ',F5.2,' share ',F4.2,' count ',I3  )
              END IF
!
              IF ( KSCREEN ) THEN
                   IPTR = IPTR+1
                   WRITE ( LBUF(IPTR), 9967 ) OVRRMS_CLK, OVRSTA_CLK, &
     &                     OVRTRA_CLK/OVRCNT_CLK, OVRCNT_CLK
                   CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                   CALL NL_MN()
              ENDIF
          ENDIF
      ENDIF
      IF ( NEED_HEAD .AND. KSPOOL .AND. .NOT. KGLOBALS ) WRITE ( 23, 9958 )
9958  FORMAT(' Clock Constraint Statistics - NONE',/)
!
! --- Remember whether there were any constraint parameters for a3jst
!
      KCONS = OVRCNT .NE. 0  .OR.  OVRCNT_CLK .NE. 0
!
! --- Restore VSITEC and that's it
!
      CALL COPY_R8 ( 3*INT4(MAX_STA), VSITEC,      TOTAL_SITEC )
      CALL COPY_R8 ( 3*INT4(MAX_STA), VSITEC_SAVE, VSITEC      )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  A1JST  #!#
