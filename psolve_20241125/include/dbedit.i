!
! >>>>> INCLUDE-BLOCK with descriptions of data structures used by dbedit
!
!    dbedit.i
!
!    Who   When        What
!    pet   2000.06.09  Created using pre-JUN2000 file frngs.i
!    pet   2000.06.16  Added SOB__STRU  and COB__STRU.
!    pet   2000.06.19  Added TOC__STRU
!    pet   2000.06.23  Added S2__MED
!    pet   2000.07.03  Re-made *mk4 stuff
!    pet   2000.07.04  Added LCODE RECMODE
!    pet   2000.07.14  Fixed the record type for OCCUPNUM
!    pet   2000.07.17  Added parameters UNIQUE__DBE, NOT_UNIQUIE__DBE
!    pet   2000.07.19  Added definition of TOC_UNIQUE__DBE
!    pet   2000.07.21  Added definition of REPLACE__DBE
!    pet   2001.06.14  Added SOB.SCA_NAM
!    pet   2001.07.09  Added lcode #SAMPLES
!    pet   2004.11.03  Added lcode BITSAMPL
!
      INTEGER*4    MAX_LC, MAX_BFL_BYTE, MAX_BFL_R8
      PARAMETER  ( MAX_LC       = 512   )
      PARAMETER  ( MAX_BFL_BYTE = 32768 )
      PARAMETER  ( MAX_BFL_R8   = MAX_BFL_BYTE/8 )
!
      INTEGER*2    L_MK3BUF_I2
      PARAMETER  ( L_MK3BUF_I2 = 768 )
!
      INTEGER*2    DBH__MED, MK3__MED, MK3DEC__MED, MK3END__MED, TAPE__MED, &
     &             MK4__MED, S2__MED, UNDEF__MED
      PARAMETER  ( DBH__MED    = 1 )
      PARAMETER  ( MK3__MED    = 2 )
      PARAMETER  ( MK3DEC__MED = 3 )
      PARAMETER  ( MK3END__MED = 4 )
      PARAMETER  ( TAPE__MED   = 5 )
      PARAMETER  ( MK4__MED    = 6 )
      PARAMETER  ( S2__MED     = 7 )
      PARAMETER  ( UNDEF__MED  = 0 )
!
      INTEGER*2    LC_DEF_1$, LC_DEF_2$
      TYPE      LC__STRU
          INTEGER*4  FIRST_FIELD
          INTEGER*4  DUMMY4
          CHARACTER  LC_NAME(MAX_LC)*8  ! Array of lcode names
          CHARACTER  LC_DESC(MAX_LC)*32 ! Array of lcode descriptions
          INTEGER*2  LC_ITOC(MAX_LC)    ! Array of indexes of TOC tables
          INTEGER*2  LC_DIMS(3,MAX_LC)  ! Array of dimensions
          INTEGER*2  LC_TYPE(MAX_LC)    ! Array of data types
          INTEGER*4  LC_ADDR(MAX_LC)    ! Array of relative addresses of the
!                                       ! the first elements of the item
          INTEGER*2  NLC                ! Total number of lcodes
          INTEGER*4  LBUF_BYTE          ! Length of the used part of the buffer
          BYTE       FILLER(4)          ! padding for alignment
!
! ------- Important: BUF should be aligned to have an address as a multiple 8
! ------- in order to avoid data misalighment.
!
          REAL*8     DUMMY
          BYTE       BUF(MAX_BFL_BYTE)  ! Buffer with data
          INTEGER*4  LAST_FIELD
      END TYPE  LC__STRU ! LC__STRU  /
!
! --- Table of Lcodes of Mark-3 correlator output
!     ===========================================
!
      INTEGER*2    NLC_MK3
      PARAMETER  ( NLC_MK3 = 77 )
      CHARACTER    LC_NAME_MK3(NLC_MK3)*8, &
     &             LC_DESC_MK3(NLC_MK3)*32
      INTEGER*2    TOC_MK3(NLC_MK3), DIM_MK3(3,NLC_MK3), TYP_MK3(NLC_MK3)
!
      DATA        (   LC_NAME_MK3(LC_DEF_1$), &
     &                LC_DESC_MK3(LC_DEF_1$), &
     &                TOC_MK3(LC_DEF_1$), &
     &              ( DIM_MK3(LC_DEF_2$,LC_DEF_1$), LC_DEF_2$=1,3 ), &
     &                TYP_MK3(LC_DEF_1$), &
     &                LC_DEF_1$=1,NLC_MK3 ) &
     &            / &
! ---|------------|-----------------------------------|---|---|---|---|----!---
!    |            |                                   |   |   |   |   |    !
!    |  Lcode     |  Lcode description                |toc|dim|dim|dim|typ ! N
!    |  name      |                                   |   | 1 | 2 | 3 |    !
! ---|------------|-----------------------------------|---|---|---|---|----!---
!    |            |                                   |   |   |   |   |    !
     &  '#CHANELS', 'No. of U-L pairs in integration.',  2,  1,  1,  1, 2, &  !  1
     &  'AMPBYFRQ', 'Amp,phs by chan(0-1)(-180to180).',  2,  2, 14,  1, 1, &  !  2
     &  'APCLOFST', 'Apriori clock offset microsec...',  3,  1,  1,  1, 1, &  !  3
     &  'AUTOEDIT', '1=Run resulted from AUTOEDIT....',  3,  1,  1,  1, 2, &  !  4
     &  'BASELINE', 'Ref and rem site names..........',  2,  4,  2,  1, 3, &  !  5
     &  'CALBYFRQ', 'PC amp,phs,frq by sta,channel...',  2,  3,  2, 14, 2, &  !  6
     &  'COHERCOR', 'Corr coeff (0 --> 1)............',  2,  1,  1,  1, 1, &  !  7
     &  'CORBASCD', 'Correlator baseline code (2 ch).',  3,  1,  1,  1, 3, &  !  8
     &  'CORBASNO', 'Correlator baseline number......',  3,  1,  1,  1, 2, &  !  9
     &  'CORELVER', 'COREL version number............',  3,  1,  1,  1, 2, &  !  10
     &  'CORRTYPE', 'Correlator type: MK3/MK4/K4 etc.',  1,  4,  1,  1, 3, &  !  11
     &  'DBEDITVE', 'Dbedit revision date YYYY MM DD ',  3,  3,  1,  1, 2, &  !  12
     &  'DEL OBSV', 'Observed delay us in 2 parts....',  2,  2,  1,  1, 1, &  !  13
     &  'DELOBSVM', 'Obs delay at central epoch us...',  2,  2,  1,  1, 1, &  !  14
     &  'DELRESID', 'Delay residual (sec)............',  2,  1,  1,  1, 1, &  !  15
     &  'DELSIGMA', 'Delay err (sec).................',  2,  1,  1,  1, 1, &  !  16
     &  'DELTAEPO', 'Epoch offset from center of run.',  2,  1,  1,  1, 1, &  !  17
     &  'DELTFLAG', 'Delay type, 1=grp, 2=phs........',  1,  1,  1,  1, 2, &  !  18
     &  'DELUFLAG', 'Delay unweight flag.............',  2,  1,  1,  1, 2, &  !  19
     &  'DISCARD ', 'Percent data discarded by FRNGE.',  3,  1,  1,  1, 1, &  !  20
     &  'DLYEPO+1', 'Phase delay at epoch+1 sec......',  3,  2,  1,  1, 1, &  !  21
     &  'DLYEPO-1', 'Phase delay at epoch-1 sec......',  3,  2,  1,  1, 1, &  !  22
     &  'EFF.DURA', 'Effective run duration sec......',  3,  1,  1,  1, 1, &  !  23
     &  'ERRORATE', 'Log err rate by sta, sb, channel',  3,  2,  2, 14, 2, &  !  24
     &  'EXPSERNO', 'Experiment Serial Number........',  1,  1,  1,  1, 2, &  !  25
     &  'FALSEDET', 'Prob of false det from FRNGE....',  3,  1,  1,  1, 1, &  !  26
     &  'FRNGERR ', 'FRNGE error flag 0=OK...........',  3,  1,  1,  1, 2, &  !  27
     &  'FRQGROUP', 'Frequency group code............',  3,  1,  1,  1, 3, &  !  28
     &  'GC PHASE', 'Tot phase ref to cen of Earth...',  2,  1,  1,  1, 1, &  !  29
     &  'GCRESPHS', 'Resid phs corrected to cen of E.',  3,  1,  1,  1, 1, &  !  30
     &  'GPDLAMBG', 'Group delay ambiguity (sec).....',  2,  1,  1,  1, 1, &  !  31
     &  'INCOH2  ', 'Incoh amp from FRNGE plot segs..',  3,  1,  1,  1, 1, &  !  32
     &  'INCOHAMP', 'Fr. amp from incoh int of chan..',  3,  1,  1,  1, 1, &  !  33
     &  'INDEXNUM', 'Corel index numbers by sb,freq..',  3,  2, 14,  1, 2, &  !  34
     &  'NO.OF AP', '# of AP by sideband and channel.',  2,  2, 14,  1, 2, &  !  35
     &  'OCCUPNUM', 'Site Occupation Number..........',  3,  4,  2,  1, 3, &  !  36
     &  'ORIGFILE', 'Original COREL file name........',  3,  3,  1,  1, 3, &  !  37
     &  'PHASECAL', 'PC rate by sta ( us per s)......',  2,  2,  1,  1, 1, &  !  38
     &  'PROC UTC', 'YDDD of COREL by sta,channel....',  3,  2, 14,  1, 2, &  !  39
     &  'QBFACTOR', 'Measure of uniformity of data...',  3,  1,  1,  1, 1, &  !  40
     &  'QUALCODE', 'FRNGE quality  index 0 -->  9...',  2,  1,  1,  1, 3, &  !  41
     &  'RAT OBSV', 'Obs rate (s per s)..............',  2,  1,  1,  1, 1, &  !  42
     &  'RATOBSVM', 'Obs rate at central epoch ......',  3,  1,  1,  1, 1, &  !  43
     &  'RATRESID', 'Rate resid sec per sec..........',  2,  1,  1,  1, 1, &  !  44
     &  'RATSIGMA', 'Rate err (sec per sec)..........',  2,  1,  1,  1, 1, &  !  45
     &  'RATUFLAG', 'Delay rate unweight flag........',  2,  1,  1,  1, 2, &  !  46
     &  'RECMODE ', 'Recoding mode...................',  1, 40,  1,  1, 3, &  !  47
     &  'RECSETUP', 'Samp rate(kHz),Frames/PP,PP/AP..',  2,  3,  1,  1, 2, &  !  48
     &  'RECTRACK', 'Trk table by sta,sideb,channel..',  3,  2,  2, 14, 2, &  !  49
     &  'REF FREQ', 'Freq to wh.phase is referred....',  2,  1,  1,  1, 1, &  !  50
     &  'REFCLKER', 'Ref sta clock epoch microsec....',  3,  1,  1,  1, 1, &  !  51
     &  'RFREQ   ', 'RF freq by channel (MHz)........',  2, 14,  1,  1, 1, &  !  52
     &  'RUN CODE', 'Run,code, e.g., "329-1300"......',  3,  4,  1,  1, 3, &  !  53
     &  'SB DELAY', 'Single band delay (microsec)....',  2,  1,  1,  1, 1, &  !  54
     &  'SB SIGMA', 'SB delay error microseconds.....',  2,  1,  1,  1, 1, &  !  55
     &  'SBRESID ', 'Single band delay residual......',  2,  1,  1,  1, 1, &  !  56
     &  'SEC TAG ', 'Seconds part of UTC TAG.........',  2,  1,  1,  1, 1, &  !  57
     &  'SNRATIO ', 'Signal to noise  ratio..........',  2,  1,  1,  1, 1, &  !  58
     &  'SRCHPAR ', 'FRNGE/Fourfit search parameters.',  2,  6,  1,  1, 1, &  !  59
     &  'STAR ID ', 'Radio source name...............',  2,  4,  1,  1, 3, &  !  60
     &  'STARELEV', 'Elev angles calc by COREL.......',  2,  2,  1,  1, 1, &  !  61
     &  'STARTSEC', 'Start time in sec past hour.....',  2,  1,  1,  1, 1, &  !  62
     &  'STOP SEC', 'Stop  time in sec past hour.....',  2,  1,  1,  1, 1, &  !  63
     &  'TAPEID  ', 'Raw data tape ID for ref and rem',  3,  4,  2,  1, 3, &  !  64
     &  'TAPQCODE', 'Tape quality code...............',  3,  3,  1,  1, 3, &  !  65
     &  'TOTPCENT', 'Tot phase at central epoch......',  3,  1,  1,  1, 1, &  !  66
     &  'TOTPHASE', 'Total phase degrees mod 360.....',  2,  1,  1,  1, 1, &  !  67
     &  'URVR    ', 'Rate derivatives mHz per asec...',  2,  2,  1,  1, 1, &  !  68
     &  'UTC TAG ', 'Epoch UTC YMDHM.................',  2,  5,  1,  1, 2, &  !  69
     &  'UTC TAG4', 'Epoch UTC YMDHMS (4 digit year).',  2,  6,  1,  1, 2, &  !  70
     &  'UTCM TAG', 'UTC at central epoch YMDHMS.....',  3,  5,  1,  1, 2, &  !  71
     &  'UVF/ASEC', 'U,V in FR per  arc sec..........',  2,  2,  1,  1, 1, &  !  72
     &  'VLB1FILE', 'Correlator file name............',  2,  3,  1,  1, 3, &  !  73
     &  'VLB1XTNT', 'corr. ext by sideb, channel.....',  2,  2, 14,  1, 2, &  !  74
     &  'VLB2 UTC', 'UTC of FRNGE proc YMDHMS........',  3,  6,  1,  1, 2, &  !  75
     &  'VLB2PRG ', 'FRNGE(YYMMDD), Fourfit(x.x) ver.',  3,  3,  1,  1, 3, &  !  76
     &  'VLB2XTNT', 'FRNGE extent number.............',  2,  1,  1,  1, 2 &   !  77
     &            /
!
! --- Table of Lcodes of Mark-4/Mark-5 correlator output
!     ==================================================
!
      INTEGER*2    NLC_MK4
      PARAMETER  ( NLC_MK4 = 94 )
      CHARACTER    LC_NAME_MK4(NLC_MK4)*8, &
     &             LC_DESC_MK4(NLC_MK4)*32
      INTEGER*2    TOC_MK4(NLC_MK4), DIM_MK4(3,NLC_MK4), TYP_MK4(NLC_MK4)
!
      DATA        (   LC_NAME_MK4(LC_DEF_1$), &
     &                LC_DESC_MK4(LC_DEF_1$), &
     &                TOC_MK4(LC_DEF_1$), &
     &              ( DIM_MK4(LC_DEF_2$,LC_DEF_1$), LC_DEF_2$=1,3 ), &
     &                TYP_MK4(LC_DEF_1$), &
     &                LC_DEF_1$=1,64 ) &
     &            / &
! ---|------------|-----------------------------------|---|---|---|---|----!---
!    |            |                                   |   |   |   |   |    !
!    |  Lcode     |  Lcode description                |toc|dim|dim|dim|typ ! N
!    |  name      |                                   |   | 1 | 2 | 3 |    !
! ---|------------|-----------------------------------|---|---|---|---|----!---
!    |            |                                   |   |   |   |   |    !
     &  '#CHANELS', 'No. of U-L pairs in integration.',  2,  1,  1,  1, 2, &  !   1
     &  '#SAMPLES', '# of samples by sideband and cha',  2,  2, 16,  1, 1, &  !   2
     &  'ABASACCE', 'Corel bas/apr accel (1/sec**2)..',  3,  1,  1,  1, 1, &  !   3
     &  'ABASDEL ', 'Corel bas/apr delay (sec).......',  3,  1,  1,  1, 1, &  !   4
     &  'ABASRATE', 'Corel bas/apr delay rate (s/s)..',  3,  1,  1,  1, 1, &  !   5
     &  'AMPBYFRQ', 'Amp(0-1), phs by chan(-180to180)',  2,  2, 16,  1, 1, &  !   6
     &  'APLENGTH', 'Length of accumul. period in sec',  1,  1,  1,  1, 1, &  !   7
     &  'BASELINE', 'Ref and rem site names..........',  2,  4,  2,  1, 3, &  !   8
     &  'BITSAMPL', 'Number of bits per sample.......',  2,  1,  1,  1, 2, &  !   9 
     &  'BBC IND ', 'Physical BBC number by channel..',  2,  2, 16,  1, 2, &  !  10
     &  'CALBYFRQ', 'PC amp,phs,frq by sta,channel...',  2,  3,  2, 16, 2, &  !  11
     &  'CHAN ID ', 'One-letter Fourfit channel ID...',  3, 16,  1,  1, 3, &  !  12
     &  'COHERCOR', 'Corr coeff (0 --> 1)............',  2,  1,  1,  1, 1, &  !  13
     &  'CORBASCD', 'Correlator baseline code (2 ch).',  3,  1,  1,  1, 3, &  !  14
     &  'CORCLOCK', 'Clock offset(sec)/rate(sec/sec).',  3,  2,  2,  1, 1, &  !  15
     &  'CORELVER', 'Correlator software version numb',  3,  1,  1,  1, 2, &  !  16
     &  'CORPLACE', 'Correlator name.................',  1, 16,  1,  1, 3, &  !  17
     &  'CORR UTC', 'UTC time tag of correlation.....',  3,  6,  1,  1, 2, &  !  18
     &  'CORRTYPE', 'Correlator type: MK3/MK4/K4 etc.',  1,  4,  1,  1, 3, &  !  19
     &  'CROOTFIL', 'Correlator root file name.......',  3,  8,  1,  1, 3, &  !  20
     &  'DBEDITVE', 'Dbedit revision date YYYY MM DD ',  3,  3,  1,  1, 2, &  !  21
     &  'DEL OBSV', 'Observed delay us in 2 parts....',  2,  2,  1,  1, 1, &  !  22
     &  'DELRESID', 'Delay residual (sec)............',  2,  1,  1,  1, 1, &  !  23
     &  'DELSIGMA', 'Delay err (sec).................',  2,  1,  1,  1, 1, &  !  24
     &  'DELTAEPO', 'Offset from center of scan (sec)',  2,  1,  1,  1, 1, &  !  25
     &  'DELUFLAG', 'Delay unweight flag.............',  2,  1,  1,  1, 2, &  !  26
     &  'DISCARD ', 'Percent data discarded by FRNGE.',  3,  1,  1,  1, 1, &  !  27
     &  'EFF.DURA', 'Effective run duration sec......',  3,  1,  1,  1, 1, &  !  28
     &  'ERRORATE', 'Log err rate by sta, sb, channel',  3,  2,  2, 16, 2, &  !  29
     &  'EXPCODE ', 'Experiment name.................',  1, 16,  1,  1, 3, &  !  30
     &  'EXPDESC ', 'Experiment description..........',  1, 40,  1,  1, 3, &  !  31
     &  'EXPSERNO', 'Experiment Serial Number........',  1,  1,  1,  1, 2, &  !  32
     &  'FALSEDET', 'Prob of false det from FRNGE....',  3,  1,  1,  1, 1, &  !  33
     &  'FOURF CF', 'Control file name for fourfit...',  1, 48,  1,  1, 3, &  !  34
     &  'FOURF CS', 'Command string used for fourfit.',  1, 64,  1,  1, 3, &  !  35
     &  'FOURFFIL', 'Fourfit output filename.........',  3,  8,  1,  1, 3, &  !  36
     &  'FOURFUTC', 'Fourfit processing time YMDHMS..',  3,  6,  1,  1, 2, &  !  37
     &  'FOURFVER', 'Fourfit version number..........',  3,  2,  1,  1, 2, &  !  38
     &  'FRNGERR ', 'Fourfit error flag blank=OK.....',  3,  1,  1,  1, 3, &  !  39
     &  'FRQGROUP', 'Frequency group code............',  3,  1,  1,  1, 3, &  !  40
     &  'GC MBD  ', 'Tot geocenter group delay (sec).',  3,  1,  1,  1, 1, &  !  41
     &  'GC PHASE', 'Tot phase ref to cen of Earth...',  2,  1,  1,  1, 1, &  !  42
     &  'GC RATE ', 'Tot geocenter delay rate (s/s)..',  3,  1,  1,  1, 1, &  !  43
     &  'GC SBD  ', 'Tot geocenter sbd delay (sec)...',  3,  1,  1,  1, 1, &  !  44
     &  'GCRESPHS', 'Resid phs corrected to cen of E.',  3,  1,  1,  1, 1, &  !  45
     &  'GPDLAMBG', 'Group delay ambiguity (sec).....',  2,  1,  1,  1, 1, &  !  46
     &  'GROBSDEL', 'Observed group delay in sec.....',  2,  1,  1,  1, 1, &  !  47
     &  'IDELAY  ', 'Corel instrumental delay (sec)..',  3,  2,  1,  1, 1, &  !  48
     &  'INCOH2  ', 'Incoh amp from FRNGE plot segs..',  3,  1,  1,  1, 1, &  !  49
     &  'INCOHAMP', 'Fr. amp from incoh int of chan..',  3,  1,  1,  1, 1, &  !  50
     &  'INDEXNUM', 'Corel index numbers by sb,freq..',  3,  2, 16,  1, 2, &  !  51
     &  'LO FREQ ', 'LO frequencies per cha/sta MHz..',  2,  2, 16,  1, 1, &  !  52
     &  'NLAGS   ', 'Num of lags used for correlation',  1,  1,  1,  1, 2, &  !  53
     &  'NO.OF AP', '# of AP by sideband and channel.',  2,  2, 16,  1, 2, &  !  54
     &  'PHASECAL', 'PC rate by sta ( us per s)......',  2,  2,  1,  1, 1, &  !  55
     &  'PHCALOFF', 'Phase cal offset (-18000/18000).',  2,  2, 16,  1, 2, &  !  56
     &  'PHCALSTS', 'Phase cal status: 0/1/2 ........',  2,  2,  1,  1, 2, &  !  57
     &  'PI NAME ', 'Agency/contact_person/PI name...',  1, 40,  1,  1, 3, &  !  58
     &  'POLARIZ ', 'Polarization per sta/chan R/L...',  2,  2, 16,  1, 3, &  !  59
     &  'QBFACTOR', 'Measure of uniformity of data...',  3,  1,  1,  1, 1, &  !  60
     &  'QUALCODE', 'FRNGE quality  index 0 --> 9....',  2,  1,  1,  1, 3, &  !  61
     &  'RAT OBSV', 'Observd delay rate (sec per sec)',  2,  1,  1,  1, 1, &  !  62
     &  'RATRESID', 'Rate resid (sec per sec)........',  2,  1,  1,  1, 1, &  !  63
     &  'RATSIGMA', 'Rate formal error(sec per sec)..',  2,  1,  1,  1, 1  &  !  64
     &  /
!
      DATA        (   LC_NAME_MK4(LC_DEF_1$), &
     &                LC_DESC_MK4(LC_DEF_1$), &
     &                TOC_MK4(LC_DEF_1$), &
     &              ( DIM_MK4(LC_DEF_2$,LC_DEF_1$), LC_DEF_2$=1,3 ), &
     &                TYP_MK4(LC_DEF_1$), &
     &                LC_DEF_1$=65,NLC_MK4 ) &
     &            / &
! ---|------------|-----------------------------------|---|---|---|---|----!---
!    |            |                                   |   |   |   |   |    !
!    |  Lcode     |  Lcode description                |toc|dim|dim|dim|typ ! N
!    |  name      |                                   |   | 1 | 2 | 3 |    !
! ---|------------|-----------------------------------|---|---|---|---|----!---
!    |            |                                   |   |   |   |   |    !
     &  'RATUFLAG', 'Delay rate unweight flag........',  2,  1,  1,  1, 2, &  !  65
     &  'RECMODE ', 'Recoding mode...................',  1, 40,  1,  1, 3, &  !  66
     &  'REF FREQ', 'Freq to which phase is referred.',  2,  1,  1,  1, 1, &  !  67
     &  'REFCLKER', 'Ref sta clock epoch microsec....',  3,  1,  1,  1, 1, &  !  68
     &  'RFREQ   ', 'RF freq by channel (MHz)........',  2, 16,  1,  1, 1, &  !  69
     &  'SAMPLRAT', 'Sample rate (Hz)................',  2,  1,  1,  1, 1, &  !  70
     &  'SB DELAY', 'Single band delay (microsec)....',  2,  1,  1,  1, 1, &  !  71
     &  'SB SIGMA', 'SB delay error microseconds.....',  2,  1,  1,  1, 1, &  !  72
     &  'SBRESID ', 'Single band delay residual......',  2,  1,  1,  1, 1, &  !  73
     &  'SCAN UTC', 'Nominal scan time YMDHMS........',  3,  6,  1,  1, 2, &  !  74
     &  'SCANNAME', 'Scan name ......................',  2,  5,  1,  1, 3, &  !  75
     &  'SEC TAG ', 'Seconds part of UTC TAG.........',  2,  1,  1,  1, 1, &  !  76
     &  'SITCODES', 'Two-letters site codes .........',  2,  2,  1,  1, 3, &  !  77
     &  'SNRATIO ', 'Signal to noise  ratio..........',  2,  1,  1,  1, 1, &  !  78
     &  'SRCHPAR ', 'FRNGE/Fourfit search parameters.',  2,  6,  1,  1, 1, &  !  79
     &  'STAR ID ', 'Radio source name...............',  2,  4,  1,  1, 3, &  !  80
     &  'STARELEV', 'Elev angles calc by COREL.......',  3,  2,  1,  1, 1, &  !  81
     &  'STARTOFF', 'Offset nominal start time (sec).',  3,  1,  1,  1, 2, &  !  82
     &  'STARTSEC', 'Start time in sec past hour.....',  2,  1,  1,  1, 1, &  !  83
     &  'STOP OFF', 'Offset nominal stop time (sec)..',  3,  1,  1,  1, 2, &  !  84
     &  'STOP SEC', 'Stop  time in sec past hour.....',  2,  1,  1,  1, 1, &  !  85
     &  'TAPEID  ', 'Raw data tape ID for ref and rem',  3,  4,  2,  1, 3, &  !  86
     &  'TAPQCODE', 'Tape quality code...............',  3,  3,  1,  1, 3, &  !  87
     &  'TOTPHASE', 'Total phase degrees mod 360.....',  2,  1,  1,  1, 1, &  !  88
     &  'URVR    ', 'Rate derivatives mHz per asec...',  2,  2,  1,  1, 1, &  !  89
     &  'UTC TAG ', 'Epoch UTC YMDHM.................',  2,  5,  1,  1, 2, &  !  90
     &  'UTC TAG4', 'Epoch UTC YMDHMS (4 digit year).',  2,  6,  1,  1, 2, &  !  91
     &  'UTCM TAG', 'UTC at central epoch YMDHMS.....',  3,  6,  1,  1, 2, &  !  92
     &  'UVF/ASEC', 'U,V in FR per  arc sec..........',  2,  2,  1,  1, 1, &  !  93
     &  'ZDELAY  ', 'Corel zenith atmos. delay (sec).',  3,  2,  1,  1, 1  &  !  94
     &            /
!
      TYPE      SOB__STRU
          REAL*8     TIME       ! Time
          REAL*8     PRIM_ELIM  ! Primary duplicate elim criterion
          REAL*8     SCND_ELIM  ! Secondary duplicate elim criterion
          INTEGER*4  IND_OBS    ! Index (current observation number)
          INTEGER*2  IND_BAS    ! Baseline code
          INTEGER*2  IND_SOU    ! Source code
          INTEGER*2  IFRQ       ! Frequency
          INTEGER*2  LQLC       ! Quality code
          INTEGER*2  PASS_FLAG  ! Pass/fail flag
          CHARACTER  SCA_NAM*16 ! The scan identifier
          CHARACTER  FRG_FIL*16 ! The scan identifier
      END TYPE  SOB__STRU ! SOB__STRU !
!
      TYPE      COB__STRU
          INTEGER*4   FIRST_FIELD
          REAL*8      USEC        ! The seconds part of the current observation time.
          REAL*8      PRIM_ELIM   ! The current primary elimination criterion.
          REAL*8      SCND_ELIM   ! The current secondary duplicate elimination criterion.
          REAL*8      DFRQ        ! The current frequency.
          INTEGER*2   IUTC(6)     ! The current observation time.
          INTEGER*2   LQLC        ! The current quality code.
          INTEGER*2   JCHAN       ! The current number of channels.
          CHARACTER   SOU_NAM*8   ! The current source.
          CHARACTER   BAS_NAM*16  ! The current baseline.
          CHARACTER   SCA_NAM*16  ! The scan identifier
          CHARACTER   FRG_FIL*16  ! The fringe file name
          INTEGER*4   LAST_FIELD
      END TYPE  COB__STRU ! COB__STRU !
!
      TYPE      TOC__STRU
          CHARACTER   LCODE*8
          INTEGER*2   IVER
          INTEGER*2   IDIM1
          INTEGER*2   IDIM2
          INTEGER*2   IDIM3
      END TYPE  TOC__STRU ! TOC__STRU !
!
      INTEGER*2  UNIQUE__DBE, NOT_UNIQUE__DBE, TOC_UNIQUE__DBE, REPLACE__DBE
      PARAMETER  ( UNIQUE__DBE     = 2310 )
      PARAMETER  ( NOT_UNIQUE__DBE =  717 )
      PARAMETER  ( TOC_UNIQUE__DBE =  719 )
      PARAMETER  ( REPLACE__DBE    =  721 )
!
!
! <<<<< end of  INCLUDE-BLOCK  dbedit.i
