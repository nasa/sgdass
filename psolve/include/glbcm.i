!@This is the start of file &GLBCM
!
!  Last update: 21-DEC-2002 13:24:35
!
!  NB: current length of STRUC is HARDCODED as MAX_STRUC and it corresponds
!      to MAX_PAR ( from solve.i ) 16384. In future we have to get rid from
!      this trap
!
!  92.01.13  MWH  Expanded ISTRUC to handle up to 2048 parameters
!  94.03.17  MWH  Expanded ISTRUC to handle up to 4096 parameters
!  95.12.04  KDB  Allow integer*4 number of observations
!  95.12.14  KDB  Rearranged variables so no skipped words in common.
!                 Created list of variable locations in common.
!  96.05.16  KDB  More documentation.
!  97.02.04  kdb  Site weighting feature. (Add weighting_type.)
!  97.03.19  KDB  New variable (sigma_type) to show current sigma type
!                   (pre- or post-fit).
!   kdb  970401  SPECIAL VERSION: KLUGE FOR CMA.  (REVERSE THE SIGN OF THE
!                EQE CONT CONTRIBUTION (CORRECTION TO THE EQUATION OF THE
!                EQUINOX TO FIX A CALC ERROR.)
!  97.06.10  kdb New bit variable, intracom_reway, to allow various calls
!                of reway to communicate with each other.
!                 Bit 1 - set - pause when listing info.
!                         cleared - don't pause
!  98.02.19  kdb  Fix igldum declaration (change from 51 words to 50).
!  98.02.20  kdb  Add variables for sinex output (ksinex,lsnxdir)
!  98.05.05  pet  Add parameter SIGMA_TYPE__DEF
!  98.05.05  pet  Add parameter SIGMA_TYPE__DEF
!  98.07.08  pet  Add logical variable ARCPE_WORKED, CRES_WORKED
!  98.07.22  pet  Add logical variable KBSL_CONST
!  99.01.05  pet  Add logical variable TRAIN_CGM
!  99.01.12  pet  Add integer variable NARCS -- total number of arcs
!  99.04.16  pet  Add SAVED_ARCNAME_MES
!  99.04.19  pet  Add LENCNT_SAVED, LENARC_SAVED
!  1999.10.07  pet  Added equivalent definition of SOLUID: SOLUID_CHR
!  1999.10.12  pet  Incresed MAX_STRUC from 256 to 1024 to reflect increasing
!                   MAX PAR from 4096 to 16384
!  2002.03.27  pet  Removed variables for sinex output (ksinex,lsnxdir)
!  2002.12.21  pet  Added variables SIT_EST_EPOCH   and  SOU_EST_EPOCH
!  2005.10.03  pet  Added variables USER_PROG_NAME  and  USER_PROG_BUFF
!  2011.06.01  pet  Increased MAX_STRUC from 1024 to 2000 for holding up 
!                   32000 parameters
!  2017.10.23  pet  Increased MAX_STRUC from 2000 to 12500 for holding up 
!                   100000 parameters and made it INTEGER*4
!  2023.03.12  pet  Added KPRP_CONST
!
!
!   Flags and their meanings:
!
!     INAMCG         - name of Input Cgm file
!     IOCGM          - output CGM control flag
!                      0 no output CGM
!                      1 put output CGM in user's scratch CGMFIL
!                      2 make a new permanent CGM
!     ICONT          - flag for continuing after forming CGM: yes(0)
!     ISOLU          - flag for solution type:  forward(0) or back(1)
!     ONAMCG         - name of last output CGM
!     SOLUID(30)     - 60 character Hollerith solution ID
!     IARCNM         - current arc number
!     ISTRUC(256)    - bit array in NRMFIL order reflecting the status
!                      of each parameter as arc(0) or global(1).
!     IARCS          - number of arc parms in an individual solution.
!     IGLBLS         - number of global parms in an individual solution.
!     ISLTY2         - type of solution pass F,B, or I
!     IIPASS         - number of  pass 1 or 2
!     PARCNM         - previous arc number
!     PONAMC         - previous output CGM
!     POCRCG         - previous output CGM cartridge reference
!     PSLTY2         - previous arc type of solution pass
!     PIPASS         - previous arc pass number
!     IPRGRC         - progress file record count
!     PPRGRC         - previous progress file record count
!     IPRGPS         - progress file position
!     PPRGPS         - previous progress file position
!     IARCRC         - arc record number.  Internal batch variable.
!     PARCRC         - previous arc record number.  Internal batch variable.
!     IARCPS         - arc position
!     PARCPS         - previous arc position
!     BUILT          - .TRUE. IF THE ARC IS FINISHED
!     COPIED         - .TRUE. IF RESTART DATA HAS BEEN COPIED
!     KMORED         - .TRUE. IF A RESTART IS POSSIBLE
!     STIME0         - elapsed time till this arc
!     STIMP0         - elapsed time till this pass
!     PTIME0         - elapsed time till previous arc
!     STIMP0         - elapsed time till this pass previous arc
!     SLAST          - TRUE IF THIS IS THE LAST ARC IN THIS PASS
!     PLAST          - TRUE IF PREVIOUS WAS THE LAST ARC IN THAT PASS
!     ISARRC         - position of last record in SARFxx
!     PSARRC         - previous position of last record in SARFxx
!     CWRMS(2)       - batch statistics information
!     CFACT(2)       -           "
!     CNPARAM        -           "                   (total # arc parms)
!     CNCSUM         -           "
!     CKCSUM         -           "
!     PWRMS(2)       - previous stat info as of last arc completed
!     PFACT(2)       -           "
!     PNPARAM        -           "
!     PNCSUM         -           "
!     PKCSUM         -           "
!     TGLBLS         - total number of global parms in this solution
!     PSPPOS         - previous spool file position number
!     ICOV           - type of i-arc parameter to correlate
!     JCOV           - type of j-arc parameter to correlate
!     I_ARC          - number of saved arcfile of i-arc:  for correlations
!     TARCS          - total # arcs processed :  left by BATCH at end of
!                      forward processing
!     I_ARCNAME      - ??
!     NRMFL_PARMS    - The number of parms to which the nrmfile is sized.
!     ITBLOUT        - table output control for ADJST
!     TIME0          - Reference date for plate motion model
!     SIT_EST_EPOCH  - Reference epoch for the estimates of station positions
!                      as Julian Date. It has sense only if both site position
!                      and site velocities are estiamted.
!     SOU_EST_EPOCH  - Reference epoch for the estimates of source positions
!                      as Julian Date. It has sense only if both source
!                      coordinates and proper motion are estimated
!     GLBSUP         - Number of suppressed global parameters
!     EOPLPOS        - previous position number for file of earth orientation
!                      adjustment plotting data
!     EOP_FACTOR     - From the control file $constraint section
!                      EARTH_ORIENTATION keyword's FACTOR phrase.
!                      Factors the sigmas when forming the coviarance matrix
!                      for the two endpoints in cov_eop.f
!     WEIGHTING_TYPE - The type of weighting used for the solution.
!                      There are two meanings, depending on whether the batch or
!                      interactive mode is being run.
!                      In batch mode, weighting_type gives the actual type of
!                      weighting:
!                         BL for baseline weighting,
!                         ST for site and
!                         DB for by arc weighting.
!                      In interactive mode, weighting_type is set to ?? to
!                         indicate that the program should determine the current
!                         weighting type.
!     SIGMA_TYPE     - type of sigmas - PR for pre-  or PS for post.
!
!     IREVCONT       - pointer to observation dependent contributions, in namfil
!                      order.  Points to the contribution whose sign should be
!                      reversed in socal.
!                      0 if no contribution should be reversed.
!     ARCPE_WORKED   - flag:
!                      .TRUE.  if ARCPE processed and saved in CGM (temporary
!                              or permanent) at least one arc.
!                      .FALSE. if no one arce has not been yet processed by
!                              ARCPE
!     CRES_WORKED    - flag:
!                      .TRUE.  if CRES processed at least one arc in back run
!                      .FALSE. if no one arc was processed by CRES
!     KBSL_CONST     - if TRUE then impose constraints on baseline dependent
!                      clocks
!     TRAIN_CGM      - flag:   the train mode for creation of CGM
!     NARCS          - total number of arcs in the run. -1 if unknown
!     SAVED_ARCNAME_MES -- the last saved arcname in the format:
!                          $yymmmddtt <vv>
!     LENCNT_SAVED      -- saved length of the control file (in lines)
!     LENARC_SAVED      -- saved length of the arc file (in lines)
!     UPT_FLAG          -- user program bookeeping flag.
!     IGLDUM(LEN_IGLDUM_WORDS) - filler to reach the boundary of the next
!                                 256-byte long block
!
      CHARACTER    SIGMA_TYPE__DEF*2
      PARAMETER  ( SIGMA_TYPE__DEF = 'PR' )
      INTEGER*2    MAX_STRUC          ! Should be MAX_PAR/16 where MAX_PAR from
      PARAMETER  ( MAX_STRUC = 6250 ) ! solve.i
      REAL*8       CWRMS(2), CFACT(2), PWRMS(2), PFACT(2)
      INTEGER*4 &
     &             IARCRC, PARCRC, &
     &             STIME0, STIMP0, PTIME0, PTIMP0, &
     &             ISARRC, PSARRC, &
     &             CNPARAM, CNCSUM, CKCSUM, &
     &             PNPARAM, PNCSUM, PKCSUM, &
     &             PSPPOS, EOPLPOS
      LOGICAL*2 &
     &             BUILT, COPIED, KMORED, SLAST, PLAST
      INTEGER*4    IARCNM, IARCS, IGLBLS, ISTRUC(MAX_STRUC)
      INTEGER*4    NRMFL_PARMS, TGLBLS, TARCS, GLBSUP
      INTEGER*2    I_ARC, &
     &             ISOLTYP, RECVR, &
     &             IOCGM, ICONT, ISOLU, &
     &             SOLUID(30), &
     &             IIPASS, PARCNM, PIPASS, &
     &             ITBLOUT, &
     &             IPRNT, LMNAM(5), REFSTA
      CHARACTER    LMNAM_CH*10
      EQUIVALENCE  ( LMNAM, LMNAM_CH ) 
      CHARACTER*128 &
     &             INAMCG,ONAMCG,PONAMC
      CHARACTER*128 &
     &             ARCDIR(3)
      CHARACTER*20 &
     &             ICOV, JCOV
      CHARACTER*128 &
     &             SAVAF
      CHARACTER*12 &
     &             I_ARCNAME
      CHARACTER*16 &
     &             SAVED_ARCNAME_MES
      CHARACTER*1 &
     &             ISLTY2,PSLTY2
      INTEGER*4 &
     &             PCVPOS
      LOGICAL*2 &
     &             KPERMARC,FS_FULL(3)
      REAL*8 &
     &             CHINMRG, CSHARE, PCSHARE, ARC_SHARE
      REAL*8 &
     &             SIT_EST_EPOCH, SOU_EST_EPOCH
      INTEGER*4 &
     &             CHIDNMG
      REAL*8 &
     &             TIME0
      CHARACTER*128 &
     &             OUTCGM
      REAL*8 &
     &             EOP_FACTOR
      integer*2 &
     &             MDORCTL
      LOGICAL*2 &
     &             REQREF, KUSER_PART, KGLOBONLY, KIONO, KUSER_CONST
      LOGICAL*2 &
     &             KSRC_CONST, KPRP_CONST, ALL_SIM_FLG
      CHARACTER*128 &
     &             USER_PART_PROG, USER_CONST_PROG, MERGCGM
      CHARACTER*50 RUN_STRING
      CHARACTER*4  NUVEL_FIXED
      INTEGER*4    LEN_IGLDUM_WORDS
      PARAMETER  ( LEN_IGLDUM_WORDS = 216 ) ! of INTEGER*2 words for a filler
      INTEGER*4    NUM_USER_PART, SKIP_COUNT, NUM_USER_GLOB, &
     &             OLD_USER_PART, &
     &             ARC_USER_PART, NARCS
      INTEGER*2   USER_PART_TYPE(31), IGLDUM(LEN_IGLDUM_WORDS)
      INTEGER*4    NRECD
      CHARACTER*2  WEIGHTING_TYPE, SIGMA_TYPE
      CHARACTER    USER_PROG_NAME*127
      CHARACTER    USER_PROG_BUFF*81
      INTEGER*2    IREVCONT, INTRACOM_REWAY
      INTEGER*2    LENCNT_SAVED, LENARC_SAVED
      LOGICAL*2    ARCPE_WORKED, CRES_WORKED, KBSL_CONST, TRAIN_CGM
      CHARACTER    SOLUID_CHR*60
      EQUIVALENCE  ( SOLUID, SOLUID_CHR )
!
      INTEGER*4    UPT_FLAG
      INTEGER*2    IGLBCM(JGLBCM_WORDS)
!
      COMMON /GLBCM/ &
!     REAL*8
     &             CWRMS, CFACT, PWRMS, PFACT, &
     &             CHINMRG, CSHARE,PCSHARE, ARC_SHARE, TIME0, &
     &             EOP_FACTOR, SIT_EST_EPOCH, SOU_EST_EPOCH, &
!     INTEGER*4
     &             IARCRC, PARCRC, &
     &             STIME0, STIMP0, PTIME0, PTIMP0, &
     &             ISARRC, PSARRC, &
     &             CNPARAM, CNCSUM, CKCSUM, &
     &             PNPARAM, PNCSUM, PKCSUM, &
     &             PSPPOS, NRECD, &
     &             CHIDNMG, EOPLPOS, PCVPOS, &
!     LOGICAL*2
     &             BUILT,COPIED,KMORED,SLAST,PLAST, &
!     INTEGER*2
     &             I_ARC, TARCS, &
     &             TGLBLS, &
     &             ISOLTYP, NRMFL_PARMS, RECVR, &
     &             IOCGM, ICONT, ISOLU, &
     &             SOLUID, IARCNM, ISTRUC, IARCS, IGLBLS, &
     &             IIPASS, PARCNM, PIPASS, &
     &             ITBLOUT, &
     &             IPRNT, LMNAM, &
!     CHARACTER*128
     &             INAMCG,ONAMCG,PONAMC, &
!     CHARACTER*128
     &             ARCDIR, &
!     CHARACTER*20
     &             ICOV, JCOV, &
!     CHARACTER*128
     &             SAVAF, &
!     CHARACTER*12
     &             I_ARCNAME, &
!     CHARACTER*1
     &             ISLTY2, PSLTY2, &
!     LOGICAL*2
     &             KPERMARC, FS_FULL, &
!     integer*2
     &             GLBSUP, &
!     character*128
     &             OUTCGM, &
!     INTEGER*2
     &             MDORCTL, &
!     logical*2
     &             REQREF, &
!     integer*2
     &             REFSTA, &
!     logical*2
     &             KUSER_PART, &
!     character*128
     &             USER_PART_PROG, &
!     INTEGER*2
     &             NUM_USER_PART, SKIP_COUNT, NUM_USER_GLOB, &
     &             USER_PART_TYPE, KGLOBONLY, OLD_USER_PART, RUN_STRING, &
     &             KIONO, ARC_USER_PART, KUSER_CONST, USER_CONST_PROG, &
     &             KSRC_CONST, KPRP_CONST, MERGCGM, NUVEL_FIXED, ALL_SIM_FLG, &
!     CHARACTER*2
     &             WEIGHTING_TYPE, sigma_type, &
!     INTEGER*2
     &             IREVCONT, INTRACOM_REWAY, &
!     logical*2
     &             ARCPE_WORKED, CRES_WORKED, KBSL_CONST, &
     &             TRAIN_CGM, &
!     INTEGER*2
     &             NARCS, &
!     CHARACTER*16
     &             SAVED_ARCNAME_MES, &
!     INTEGER*2
     &             LENCNT_SAVED, LENARC_SAVED, &
!     INTEGER*4
     &             UPT_FLAG, &
!
     &             USER_PROG_NAME, USER_PROG_BUFF, &
!     INTEGER*2
     &             IGLDUM
!
      EQUIVALENCE (IGLBCM,CWRMS)
      CHARACTER    SOLTYP_CH*1
      EQUIVALENCE  ( ISOLTYP, SOLTYP_CH )

