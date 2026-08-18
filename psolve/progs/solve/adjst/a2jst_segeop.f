      SUBROUTINE A2JST_SEGEOP ( SCSIG, EOPTRACE, KCONS, MAT, NPARM, &
     &                          LBUF_LEN, LBUF, IPTR, PAGEWID )
      IMPLICIT NONE
!
!     A2JST handles the parameters after sources.  A1JST handles
!     stations and sources.
!
      integer*4 max_segs
      parameter (max_segs = 200)
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
      REAL*8      MAT(*)
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID
      CHARACTER   LBUF(LBUF_LEN)*120
!
! 2.3 OUTPUT Variables:
!
      REAL*8 SCSIG(*)
      REAL*8 EOPTRACE
      LOGICAL*2 KCONS
!
! EOPTRACE - sum of earth orientation constraint shares
! KCONS - True if constraints are applied
! SCSIG - Scaled sigmas
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'prfil.i'
!      include '../include/nrmcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
      INCLUDE 'heo.i'
      INCLUDE 'glbcm.i'
!
! 3.  LOCAL VARIABLES
!
      real*8 dut(max_segs),dxwob(max_segs),dywob(max_segs),dutd,dxwobd,dywobd
      real*8 shortp,xwob_rate,ywob_rate
      real*8 ut1_rate
      LOGICAL*2 KBIT, KSAVE, DO_EOP_SHARE, INTRP_RATES, &
     &  GOOD_RTOTAL, FULL_ROT, APRS_FOR_EPOCH
      INTEGER*2 ID,IHR,IM,IMIN,IP,IROTC,IROTT, &
     &  ITERM,IXYU,IY,J,KP,KQ,L,NPARM,NPARMR,NROTC, &
     &  IEOP,IXYU_START(2),IXYU_STOP(2),THIS_NROT, &
     &  IORD_START,IORD_STOP, imon, iday, iyr
      real*8 cnvrt(2)
      integer*2 hfiy,hfim,hfid,hfimin
      integer*2 i,idum2,ieop_count
      INTEGER*2 NUMDB, LDBNAM(5,15), IDBVER(15)
      character*30 ldbnam_chr(5)
      equivalence (ldbnam_chr,ldbnam)
      LOGICAL*2 EOP_PARM_ON,eop_on
      integer*4 nparm_global_rate, parm_list(2),nparm_covariance,ierr4
      real*8 cov_mat(2,2),partial_div(2),mult_vmv,total_sigma(3,max_segs)
!
      REAL*8 TROT_CUR,ROTAP_CUR(4)
      real*8 eop_time(max_segs),xyu_offset(3,max_segs),xyu_scsig(3,max_segs)
      real*8 xwob_val(max_segs)
      real*8 ywob_val(max_segs)
      real*8 ut1_val (max_segs)
      REAL*8 EOPSTA(3),EOPRMS(3),EOPTRA(3)
!
      REAL*8 DDUM,xyutot(3),xyutotp(3), xyutot_rate(3)
      real*8 return_adjustment, return_sigma, tim
      real*8 rate_adjustment, rate_sigma, scaled_rate_sigma
      real*8 offset_adjustment  (3,max_segs)
      real*8 offset_total       (3,max_segs)
      real*8 offset_sigma
      real*8 scaled_offset_sigma(3,max_segs)
      real*8 total_parm
!
      real*8 xp_apriori(2,max_segs)
      real*8 yp_apriori(2,max_segs)
      real*8 ut_apriori(2,max_segs)
      REAL*8 UT1_M_TDB
!
      character*5 short_tag(2,2)
      character*9  long_tag(2,2)
      character*8 eop_name(3)
      character*50 jmg_namr
      integer*2 num_model
!
      data ixyu_start /1,3/, ixyu_stop /2,3/
!
!     cnvrt(1) is radians to milliarcseconds
!     cnvrt(2) is seconds to milliseconds
      data cnvrt /206264806.d0, 1000.d0/
      data eop_name /'X Wobble','Y Wobble','UT1-TAI '/
      data short_tag /'masec',' msec', &
     &                'mas/d',' ms/d'/
!
      data  long_tag /'microasec','microsec ', &
     &                'microas/d','micros/d '/
      CHARACTER  STR*255
      INTEGER*4  IUER
      INTEGER*4  I_LEN
!
!
! 4.  HISTORY
!   WHO   WHEN    WHAT
!   DG    910701  Modified for new E.O. mod file mapping
!   KB    910816  Printing rate totals for original style of earth orientation
!                 parameterization (offsets and rates at individual epochs)
!   :94.01.14:jwr:Micro units introduced for adjstments and sigmas for eop offsets and
!                 rates.
!   :94.01.27:jwr:Missing test on kspool when writing hpeop info inserted.
!   :94.01.28:jwr:For no eop mod file case, modified code to interpolate apriori's
!                 rather than the contents of rotap.
!   jmg   960610  Remove holleriths.
!   pet   970610  Added capacity to write EOP adjustments to file
!                 in the case of EOP__RATES_AND_SEGS parameterization
!   pet   971104  Added capacity to write full adjustments list in according
!                 with SEG_OUTPUT variable. Changed format of EOPPxx file
!                 ( added more space)
!   pet   980923  Changed a format for printing values of EOP offsets: only
!                 tenths of microsec or microarcsec are to be printed.
!   pet   1999.05.28. Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                     eliminated common block adj_buf
!   pet   2000.01.31  Forced to print leading zeroes in dates, f.e
!                     "00/11/04 07:01" instead of " 0/11/ 4  7: 1"
!   pet   2002.05.08  The field "parameter index" was changed from I4 to I5
!                     in order to support solutions with more than 9999
!                     parameters. As a rasult all adjustments and their formal
!                     uncertainties were moved one character to the right edge
!                     of listing;
!   pet   2003.10.02  Added support of harmonic EOP variations in HEO format
!   pet   2006.01.19  Added support of ERM parameters
!
!
! --- Open the file for passing hfeop information
!
      CALL USE_EOP_PLOT_FILE ( 'O' )
!
! --- Get the parameter counter
!
      NPARM = NPARAM-IPSTP
      IF ( L_EERM > 0 ) THEN
!
! -------- Take into account ERM parameters
!
           DO I = 1,3
              IF ( IND_EERM_NOD(I) > 0 ) THEN
                   NPARM = NPARM + EERM%DEGREE(I) + 1 + EERM_OVR(I)
              END IF
           END DO
      END IF
!
!**   BEGIN EARTH ROTATION SECTION
!
!     Process the polar motion and UT1 parameters and write archive
!     archive scratch file records type 8.  Initialize IROTC and NROTC
!     for handling the earth rotation cross-correlations.
!
      DO IEOP = 1,2 !Run over wobble and ut1
!
! ------ Make a table of apriori eop information for all the epochs in the
! ------ segmented parameterization. Internally have the apriori's end up
! ------ in radians, seconds, radians/day, and seconds/day.
!
         IF ( NROT_A1(IEOP) .GT. MAX_SEGS ) THEN
              WRITE ( 6, * ) ' IEOP = ',IEOP,' NROT_A1(IEOP) =',NROT_A1(IEOP), &
     &               ' MAX_SEGS = ',MAX_SEGS
              STOP 'A2JST_NEOEOP: STOP 1'
         END IF
!
        DO I = 1,NROT_A1(IEOP)  ! Run over the number of epochs
!
! ------- Compute the JD of this epoch
!
          TIM = TROT_A1 + (I-1)*ROT_INTERVAL(IEOP)
!
! ------- If neccessary, get high-frequency corrections.
! ------- return_hfeop pm in radians and radians/sec, and UT
! ------- in seconds and seconds/day
!
          DXWOB(I) = 0.D0
          DYWOB(I) = 0.D0
          DUT(I)   = 0.D0
!
          DXWOBD   = 0.D0
          DYWOBD   = 0.D0
          DUTD     = 0.D0
!
! ------- Get the apriori totals. These routine returns values in the in
! ------- same units as  'return_hfeop'.
! ------- Convert to output units when forming the total apriori's.
!
          IF ( KEROT ) THEN ! flyby mapped values
               CALL INTRP_EOMOD ( TIM, UT1_VAL(I), SHORTP, XWOB_VAL(I), &
     &              YWOB_VAL(I), TRUE__L2, UT1_RATE, XWOB_RATE, YWOB_RATE, &
     &              IDUM2 )
             ELSE  ! standard values
               CALL INTRP_EOVR ( TIM, UT1_VAL(I), SHORTP, XWOB_VAL(I), &
     &              YWOB_VAL(I), TRUE__L2, UT1_RATE, XWOB_RATE, YWOB_RATE, &
     &              IDUM2 )
          ENDIF
!
! ------- Get apriori high-frequency EOP
!
          IF ( KHFEOP .GT. 0  ) THEN
               CALL RETURN_HFEOP ( TIM, DUT(I), DXWOB(I), DYWOB(I), DUTD, &
     &                             DXWOBD, DYWOBD )
             ELSE IF ( STAT_HEO .EQ. HEO__READ ) THEN
               IUER = -1
               UT1_M_TDB = -UT1_VAL(I) - 32.184D0
               CALL RETURN_HEO ( L_HEO, %VAL(ADR_HEO), TIM, HEO_EPOCH_SEC, &
     &                           UT1_M_TDB, DUT(I), DXWOB(I), DYWOB(I), DUTD, &
     &                           DXWOBD, DYWOBD, IUER )
               IF ( IUER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8341, IUER, 'A2JST_SEGEOP', &
     &                  'Error in computing harmonic EOP variations' )
                    CALL EXIT ( 1 )
               END IF
               IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
          END IF
!
! ------- Flip the sign of the ut1 interpolated values. Old Doug Robertson
! ------- problem.
!
          UT1_VAL(I) = -UT1_VAL(I)
          UT1_RATE   = -UT1_RATE
!
! ------- Convert the hf-eop value from internal to list units.
!
          DXWOB(I)  = DXWOB(I) *CNVRT(1)
          DYWOB(I)  = DYWOB(I) *CNVRT(1)
          DUT  (I)  = DUT  (I) *CNVRT(2)
          DXWOBD    = DXWOBD   *CNVRT(1)
          DYWOBD    = DYWOBD   *CNVRT(1)
          DUTD      = DUTD     *CNVRT(2)
!
! ------- Convert the hf-eop value from internal to list units
!
          XWOB_VAL(I) = XWOB_VAL(I)*CNVRT(1)
          YWOB_VAL(I) = YWOB_VAL(I)*CNVRT(1)
          UT1_VAL (I) = UT1_VAL (I)*CNVRT(2)
          XWOB_RATE   = XWOB_RATE  *CNVRT(1)
          YWOB_RATE   = YWOB_RATE  *CNVRT(1)
          UT1_RATE    = UT1_RATE   *CNVRT(2)
!
! ------- Convert the apriori (without hfeop) from internal to list units.
!
          XP_APRIORI(1,I) =  DXWOB(I)  + XWOB_VAL(I)
          YP_APRIORI(1,I) =  DYWOB(I)  + YWOB_VAL(I)
          UT_APRIORI(1,I) =  DUT  (I)  +  UT1_VAL(I)
          XP_APRIORI(2,I) =  DXWOBD    + XWOB_RATE
          YP_APRIORI(2,I) =  DYWOBD    + YWOB_RATE
          UT_APRIORI(2,I) =  DUTD      +  UT1_RATE
        ENDDO ! Run over the number of epochs
!
        IEOP_COUNT = 0
        DO IXYU = IXYU_START(IEOP), IXYU_STOP(IEOP) ! Do 2 for wobble, 1 for ut
!
! -------- Do the global rate parameter, if neccessary.
!
! -------- Write a blank line
!
           IPTR = IPTR+1
           CALL ADDSTR_F ( ' ' )
           CALL NL_MN()
!
           IF ( EOP_STYLE(IEOP) .EQ. EOP__RATES_AND_SEGS ) THEN ! Do global rate
!
! ------------- Bump NPARM and get the adjustments and sigmas for the global
! ------------- rate. Save the parameter number for later use to get the
! ------------- correct sigma for the plots. Convert units from radians/day
! ------------- to mas/day or seconds/day to millisec/day.
!
                NPARM = NPARM+1
                NPARM_GLOBAL_RATE = NPARM
                RATE_ADJUSTMENT   = RETURN_ADJUSTMENT(NPARM,MAT)*CNVRT(IEOP)
                RATE_SIGMA        = RETURN_SIGMA     (NPARM,MAT)*CNVRT(IEOP)
                SCALED_RATE_SIGMA =             SCSIG(NPARM    )*CNVRT(IEOP)
!
                CALL EPOC ( IMON, IDAY, IYR, IHR, IMIN, TROT_A1 )
                IF ( IXYU.EQ.1 ) THEN
                     TOTAL_PARM = XP_APRIORI(2,1) + RATE_ADJUSTMENT
                   ELSE IF ( IXYU.EQ.2 ) THEN
                     TOTAL_PARM = YP_APRIORI(2,1) + RATE_ADJUSTMENT
                   ELSE
                     TOTAL_PARM = UT_APRIORI(2,1) + RATE_ADJUSTMENT
                ENDIF
!
! ------------- NOTE!!: All the totals are in MILLI units.
! ------------- The adjustments, and sigma are in MICRO units.
! ------------- The micro conversion are done in the right statements.
!
                IF ( KSPOOL ) THEN
                     CALL CLRCH ( STR )
                     WRITE ( STR, 1600 ) NPARM, EOP_NAME(IXYU), '1', &
     &                       IYR, IMON, IDAY, IHR, IMIN, &
     &                       TOTAL_PARM,              SHORT_TAG(IEOP,2), &
     &                       RATE_ADJUSTMENT *1.D3,   LONG_TAG(IEOP,2), &
     &                       RATE_SIGMA *1.D3,        LONG_TAG(IEOP,2), &
     &                       SCALED_RATE_SIGMA *1.D3, LONG_TAG(IEOP,2)
!
 1600                FORMAT ( I5,". ", A8, 2X, A1, 2X, &
     &                        2(I2,'/'), I2, 1X, I2, ':', I2, &
     &                        F14.4, 1X, A5, 3(F11.2, 1X, A9) )
!
! ------------------ Add leading zeroes
!
                     CALL BLANK_TO_ZERO ( STR(21:28) )
                     CALL BLANK_TO_ZERO ( STR(30:34) )
                     WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
                ENDIF
                IF ( KSCREEN ) THEN
                     IPTR = IPTR+1
                     WRITE ( LBUF(IPTR),7600) &
     &                       NPARM, EOP_NAME(ixyu), '1', &
     &                       IYR, IMON, IDAY, IHR, IMIN, &
     &                       TOTAL_PARM, SHORT_TAG(IEOP,2), &
     &                       RATE_ADJUSTMENT*1.D3, LONG_TAG(IEOP,2), &
     &                       SCALED_RATE_SIGMA*1.D3, LONG_TAG(IEOP,2)
!
 7600                FORMAT ( I5, ". ", A8, 2X, A1, 2X, 2(I2,'/'), I2, 1X, &
     &                        I2, ':', I2, F12.4, 1X, A5, 2(F8.2, 1X,A9) )
!
                     CALL BLANK_TO_ZERO ( LBUF(IPTR)(21:28) )
                     CALL BLANK_TO_ZERO ( LBUF(IPTR)(30:34) )
                     CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                     CALL NL_MN()
                ENDIF
             ELSE ! No global rate
                RATE_ADJUSTMENT = 0.D0
          ENDIF ! Do global rates.
!
! ------- Do the offsets at the segment breaks.
!
          IF ( NROT_A1(IEOP) .GT. MAX_SEGS ) THEN
               WRITE ( 6, * ) ' IEOP = ',IEOP,' NROT_A1(IEOP) =',NROT_A1(IEOP), &
     &               ' MAX_SEGS = ',MAX_SEGS
               STOP 'A2JST_NEOEOP: STOP 2'
          END IF
          DO I = 1,NROT_A1(IEOP)  ! Run over the number of epochs
!
! ---------- Compute the JD of this epoch
!
             TIM = TROT_A1+(I-1)*ROT_INTERVAL(IEOP)
             CALL EPOC ( IMON, IDAY, IYR, IHR, IMIN, TIM )
             NPARM = NPARM +1
             OFFSET_ADJUSTMENT(IXYU,I) = &
     &                         RETURN_ADJUSTMENT(NPARM,MAT)*CNVRT(IEOP)
             OFFSET_SIGMA              = RETURN_SIGMA(NPARM,MAT)*CNVRT(IEOP)
             SCALED_OFFSET_SIGMA(IXYU,I) = SCSIG(NPARM)*CNVRT(IEOP)
!
! ---------- In the computation of the total parameter the effect of the global
! ---------- rate is included but when it is not turned on (the usual case)
! ---------- it is set to zero above.
!
             IF ( IXYU .EQ. 1 ) THEN
                  TOTAL_PARM = XP_APRIORI(1,I) + OFFSET_ADJUSTMENT(IXYU,I) + &
     &                                           RATE_ADJUSTMENT*(TIM-TROT_A1)
                ELSE IF ( IXYU.EQ.2 ) THEN
                  TOTAL_PARM = YP_APRIORI(1,I) + OFFSET_ADJUSTMENT(IXYU,I) + &
     &                                           RATE_ADJUSTMENT*(TIM-TROT_A1)
                ELSE
                  TOTAL_PARM = UT_APRIORI(1,I) + OFFSET_ADJUSTMENT(IXYU,I) + &
     &                                           RATE_ADJUSTMENT*(TIM-TROT_A1)
             ENDIF
             OFFSET_TOTAL(IXYU,I) = TOTAL_PARM
!
! ---------- Get the sigma of the total eop parm value from both the global
! ---------- rate and the the segment offset. Take into account the full
! ---------- covariance.
!
             IF ( EOP_STYLE(IEOP) .EQ. EOP__RATES_AND_SEGS ) THEN ! Do global rate
!
! ------------- If we have a global rate then we must compute the sigma on the
! ------------- sum of the integrated global rate and in individual offsets.
!
                NPARM_COVARIANCE = 2
                PARM_LIST(1) = NPARM_GLOBAL_RATE
                PARM_LIST(2) = NPARM
                CALL CREATE_COVARIANCE_MATRIX ( NPARM_COVARIANCE, PARM_LIST, &
     &                                          MAT, COV_MAT, IERR4 )
                PARTIAL_DIV(1) = TIM - TROT_A1
                PARTIAL_DIV(2) = 1
                TOTAL_SIGMA(IXYU,I) = ( (MULT_VMV(NPARM_COVARIANCE,PARTIAL_DIV, &
     &                                   COV_MAT,PARTIAL_DIV))**0.5)*CNVRT(IEOP)
              ELSE
                TOTAL_SIGMA(IXYU,I) = OFFSET_SIGMA
             ENDIF
!
             IF ( KSPOOL   .AND.   ( SEG_OUTPUT   .OR.   I .EQ. 1 ) ) THEN
!
! -------------- Write to spool file
!
                 WRITE ( STR, 1700 ) NPARM, EOP_NAME(IXYU), '0', &
     &                   IYR, IMON, IDAY, IHR, IMIN, &
     &                   TOTAL_PARM, SHORT_TAG(IEOP,1), &
     &                   OFFSET_ADJUSTMENT(IXYU,I)*1.D3, LONG_TAG(IEOP,1), &
     &                   OFFSET_SIGMA*1.D3, LONG_TAG(IEOP,1), &
     &                   SCALED_OFFSET_SIGMA(IXYU,I)*1.D3, LONG_TAG(IEOP,1)
 1700            FORMAT ( I5,". ", A8, 2X, A1, 2X, &
     &                    2(I2,'/'), I2, 1X, I2, ':', I2, &
     &                    F14.4, 1X, A5, 3(F11.1, 1X, A9) )
!
! -------------- Add leading zeroes
!
                 CALL BLANK_TO_ZERO ( STR(21:28) )
                 CALL BLANK_TO_ZERO ( STR(30:34) )
                 WRITE ( 23, '(A)' ) STR(1:I_LEN(STR))
             END IF
!
             IF ( KSCREEN  .AND.   ( SEG_OUTPUT   .OR.   I .EQ. 1 ) ) THEN
!
! ------------- Write to screen
!
                IPTR = IPTR+1
                WRITE ( LBUF(IPTR), 7700 ) NPARM, EOP_NAME(IXYU), '0', &
     &                  IYR, IMON, IDAY, IHR, IMIN, TOTAL_PARM, &
     &                  SHORT_TAG(IEOP,1), OFFSET_ADJUSTMENT(IXYU,I)*1.D3, &
     &                  LONG_TAG(IEOP,1),  SCALED_OFFSET_SIGMA(IXYU,I)*1.D3, &
     &                  LONG_TAG(IEOP,1)
 7700           FORMAT ( I5, ". ", A8, 2X, A1, 2X, 2(I2,'/'), I2, 1X, &
     &                   I2, ':', I2, F12.4, 1X, A5, 2(F8.1, 1X,A9) )
!
! ------------------ Add leading zeroes
!
                CALL BLANK_TO_ZERO ( LBUF(IPTR)(21:28) )
                CALL BLANK_TO_ZERO ( LBUF(IPTR)(30:34) )
                CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                CALL NL_MN()
             ENDIF  ! Write to screen on
!
! ---------- Save the eop data for this component for later writing
! ---------- to the plot file for model plot and for the eop analysis file.
! ---------- Test to make certain the time tags for all three componets
! ---------- coincide.
!
             IF ( IXYU.EQ.1 ) THEN
                  EOP_TIME(I) = TIM
                ELSE
                  IF ( ABS(EOP_TIME(I) - TIM ) .GT. 1.D-5 ) STOP 'A2JST_SEGEOP 3'
             ENDIF
!
          ENDDO  ! Run over the number of epochs
        ENDDO  ! do 2 for wobble, 1 for ut
      ENDDO  ! Run over wobble and ut1
!
! --- Write out the plot file information.
!
      WRITE ( EOPL_LU, '( &
     &"  Julian date    yr  mon  day   hr  min  |<-------------x pole----------><-----------------y pole---------><-------------------ut1----------->",/, &
     &"                                            val        sigma  hfmodel     val        sigma  hfmodel        val        sigma  hfmodel",/, &
     &"                                            micas      micas    micas     micas     micas     micas        micts      micts   micts ")')
!
      DO I=1,NROT_A1(1)
!
! ------ Get the hf eop calibrations (even if not used in apriori).
!
         CALL EPOC ( IMON, IDAY, IYR, IHR, IMIN, EOP_TIME(I) )
         WRITE ( EOPL_LU, '( F15.4, 5I5, 9F13.1 )' ) &
     &         EOP_TIME(I), IYR, IMON, IDAY, IHR, IMIN, &
     &         ( OFFSET_ADJUSTMENT(1,I) + DXWOB(I) )*1.D3, &
     &           TOTAL_SIGMA(1,I)*1.D3, &
     &           DXWOB(I)*1.D3, &
     &         ( OFFSET_ADJUSTMENT(2,I) + DYWOB(I))*1.D3, &
     &           TOTAL_SIGMA(2,I)*1.D3, &
     &           DYWOB(I)*1.D3, &
     &         ( OFFSET_ADJUSTMENT(3,I) + DUT(I) )*1.D3, &
     &           TOTAL_SIGMA(3,I)*1.D3, &
     &           DUT(I)*1.D3
      ENDDO
      CALL USE_EOP_PLOT_FILE ( 'C' )
!
! --- Put out offsets of EOP  in EOP-file if neccessary.
!
      IF ( IEOPL .NE. 0  .AND. ( EOP_STYLE(1) .EQ. EOP__SEGS_ONLY .OR. &
     &                           EOP_STYLE(1) .EQ. EOP__RATES_AND_SEGS ) ) THEN
!
! ------ Get the name of the data base.
!
         CALL DBPOX ( NUMDB, LDBNAM, IDBVER, IDBEND )
!
         CALL USE_SPLLK(PRE_SCR_DIR(:PRE_SD_LEN),EOPL_BASE,EOPL_LU,'O')
         CALL SYSTEM ( 'chmod o+rw,g+rw,u+rw '// &
     &                  PRE_SCR_DIR(:PRE_SD_LEN)//EOPL_BASE//PRE_LETRS//CHAR(0) )
         WRITE ( EOPL_LU, '("# ",a10," v. ",I5)' ) LDBNAM_CHR(1), IDBVER(1)
         WRITE ( EOPL_LU, '( &
     &  "#               | X-POLE                                     | Y-POLE                                    | UT1-TAI                                   ",/, &
     &  "#yr mn dy hr mn |    adjst      sigma    total   adj+spline  |   adjst      sigma    total   adj+spline  |   adjst      sigma    total   adjst+spline",/, &
     &  "#               | all mas                                    | all mas                                   | all ms                                    ")')
!
         DO I=1,NROT_A1(1)
            CALL EPOC ( IMON, IDAY, IYR, IHR, IMIN, EOP_TIME(I) )
            WRITE ( EOPL_LU, '(5I3,12F11.3)' ) &
     &              IYR, IMON, IDAY, IHR, IMIN, &
     &      OFFSET_ADJUSTMENT(1,I),TOTAL_SIGMA(1,I),OFFSET_TOTAL(1,I),OFFSET_ADJUSTMENT(1,I)+XWOB_VAL(I), &
     &      OFFSET_ADJUSTMENT(2,I),TOTAL_SIGMA(2,I),OFFSET_TOTAL(2,I),OFFSET_ADJUSTMENT(2,I)+YWOB_VAL(I), &
     &      OFFSET_ADJUSTMENT(3,I),TOTAL_SIGMA(3,I),OFFSET_TOTAL(3,I),OFFSET_ADJUSTMENT(3,I)+UT1_VAL (I)
         ENDDO
         CALL USE_SPLLK ( PRE_SCR_DIR(:PRE_SD_LEN),EOPL_BASE,EOPL_LU, 'C' )
      ENDIF
!
      RETURN
      END  !#!  A2JST_SEGEOP  #!#
