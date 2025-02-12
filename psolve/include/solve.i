! >>> This is the start of file solve.i
!
! Automatically generated on 2024.11.30_21:43:54 from psolve_templ.i
!     Last update:  2024.11.30_12:19:04
!
!     Contains parameters for all SOLVE programs
!
! NOTE FOR PROGRAMMERS:
!
!      Lines which starts from & are meta-definitions and they are tied with
!            local customization file.
!      The value of the parameter is an installation-specific.
!      Format of meta-commands:
!      a) &V&  <param_name> -- definition of the name of non-character type
!      b) &C0& <param_name> -- definition of the name of character type.
!                               No restrictions on the length of the line
!                               (but the line should not be empty)
!      c) &C<max_length>& <param_name> -- definition of the name of character
!                                         type. But the value should not have
!                                         length more than <max_length>
!                                         characters.
!      Actual values are are taken from the local customization file and
!      the output include file is generated.
!
!CCCCCCCCCCCCCCC
!
!     87-05-10  Created
!     87.08.04  Parameters related to GLBCM  added.
!     91.02.27  Expanded PARFIL to 116 blocks
!     91JUN06   Expanded GLBFIL to 70 blocks
!     92.01.13  Expand MAX_PAR to 1536, add one block to GLBFIL
!     92.01.13  AEE, Added paths for devices: 2648,2623,7470,9872,2686,
!               7475, and 2563.
!     92.01.13  AEE, Commented out CATALOG_CHAR stuff again; using those
!               from CATPARMS.FTNI.
!     95.03.22:jwr: Parameters added so that all parameters concerning the
!               plot_file (pltfl.i) would be computed from more primitive
!               parameters.
!     95.07.17:kdb: Fix declaration of jglbc4_blocks from 82 to 77.
!     95.07.27:kdb: Increase the size of MAX_SDC from 50 to 100.
!     95.09.28:kdb: Move max_num_met_values here and raise from 600 to 1500.
!     95.11.07:kdb: Remove obsolete flyby_files parameter
!                   Set flyby_defaults parameter to FDEF_STD
!     95.11.08:kdb: Rename FDEF_STD to flyby_mod_set.
!   kdb  951207  Integer*4 number of observations.
!   kdb  960131  Max_sup raised from 6000 to 20000.
!   kdb  960208  New parameter: max_sup_out (for mksup)
!   kdb  960311  Increase basic obsfil unit from 10 to 11 blocks.
!   kdb  960318  New parameter (noexper_user_list).
!   kdb  960506  New parameter (solve_post_filnam)
!   kdb  960610  Special version for jmg. Set max_par to 4096.
!   jmg  960730  changed so that plist block size was calculated correctly.
!   kdb  961112  Increase part_array dimension from 7 to 112 (which raises
!                jglbc4_blocks from 94 to 96).
!                New parameter (aval_part_file).
!   kdb  961125  Accommodate new batch suppression options,
!                no net rotation sources and
!                no net rotation and translation positions and velocities
!                (requires increase in glbc3 size from 54 to 55 blocks)
!   jmg  970303  Increase resfil size to 50 (pderr, prerr added).
!   pet  970317  Increase jglbc4_blocks from 96 to 97
!   pet  970421  Added constants MAX4_BRK, MAX4_EOP ( Type INTEGER*4 !! )
!   pet  970429  Added constants MAX4_HFE           ( Type INTEGER*4 !! )
!   pet  970624  Made conditional compilation of solve.i for GSFC site
!                dependent constant and for GIUB site dependent constants
!   pet  970810  Increase jglbc4_blocks from 97 to 98
!   kdb  970723  New parameter (solve_post_filnamb)
!   kdb  971024  New parameter (cnplt_config_file).
!                (Replaces cnplt/cnplt_xt.h's config_path parameter.)
!   pet  971105  Added constant MAX_ARCS
!   pet  971212  Removed constant NUTATION_DEFAULT and UT1_RATE_DEFAULT.
!                Environment variables ESTIMATE_NUTATION_FIRST,
!                ESTIMATE_UT1_RATE_FIRST, ESTIMATE_STATION_FIRST,
!                ESTIMATE_EOP_FIRST do the work in SDBH(blkcl) and GTSUP(gtsup)
!                which constants NUTATION_DEFAULT and UT1_RATE_DEFAULT did.
!                Default constant ESTIMATE_NUTATION_FIRST__DEF,
!                ESTIMATE_UT1_RATE_FIRST__DEF, ESTIMATE_STATION_FIRST__DEF,
!                ESTIMATE_EOP_FIRST__DEF  are migrating to glbc4.i
!  pet 98.02.03  Added constants xx__DTP and xx__TC, DATYP__ABR
!  pet 98.02.03  Added constants xx__SIGMA
!  pet 98.03.01  Added support of ITISCNR preferences
!  kdb 98.03.10  Move exper_cat to general.i
!  pet 98.03.25  Added parameter M_ACM -- number of stations with a priori
!                                         clock model applied
!  pet 98.03.31  Added support of MATERACGS preferences
!  pet 98.03.31  Added constant CENTER_ABR which keeps abbreviation of the
!                analysis center
!  pet 98.03.31  Added constants  SNX_WOB__SIGMA,  SNX_UT1__SIGMA,
!                SNX_NUT__SIGMA,  SNX_POS__SIGMA,  SNX_VEL__SIGMA
!  pet 98.04.15  Added constants  xxxx__SPS
!  pet 98.04.27  Added parameters OPP_SET1__BIT, OPP_SET2__BIT
!  pet 98.04.28  Added constants  xxxx__UAS
!  pet 98.04.30  Added constants  SUPMET_xxxxx
!  pet 98.05.01  Changed JRESREC from 50 to 52  2-bytes words
!  pet 98.05.10  Increased size of GLBC4 from 98 to 99 256-bytes blocks
!  pet 98.06.23  Changed some parameters related to ITISCNR site
!  pet 98.07.06  Added constants specifying singularity check action
!  pet 98.07.20  Increased JGLBC2_BLOCKS from  4 to  16 blocks
!  pet 98.07.22  Increased JGLBC4_BLOCKS from 99 to 100 blocks
!  pet 98.07.27  Added support of SUPMET__SNGBA (single band) method of
!                suppression
!  pet 98.08.08  Added support of USNO preferences
!  pvt 98.10.10  Added support of IRACNR preferences
!  pet 98.10.21  Added WPAC__SPS and URPH__SPS  bits definition
!  pet 98.11.21  Increased size of JGLBC2_BLOCKS from 16 to 32 blocks
!  pet 99.01.01  Added constants M_GPA  and  JGLBP_BLOCKS, JGLBP1_BLOCKS
!  kdb 99.02.10  Tighten source coordinate constraint sigma from 1.0d-3 to
!                1.0d-5.  This permits matrix inversion when normal matrix
!                zeroing and no net rotation sources are both used (as in the
!                TRF sinex submission).
!  kdb 99.03.24  Redirect output cgms to /box3.
!  pet 99.04.03  Added constants MAX_BSL
!  pet 99.04.05  Added constants STA__SHO, STA__NON
!  pet 99.04.07  Added constants CNI__LOC, CNI__GLO
!  pet 1999.05.16  Merged definitions for VALENCIA prepared by Karen Baver
!  pet 1999.05.28  Added constants MAX_USC and CRES_BUF_LEN2
!  pet 1999.06.08  Added constants TAU_ERR__BAD
!  kdb 1999.06.15  Add parameter values for GCCFA (dummy ones, since GCCFA does
!                  not want solve at this point).
!  pet 1999.07.26  Increased JRESREC_WORDS from 52 to 53
!  pet 1999.07.26  Added parameter MAX_SCA
!  pet 1999.09.20  Added preferences for LEIPZIG
!  pet 1999.09.23  Added optional compilation for CNPLT_CONFIG_FILE
!  pet 1999.09.28  Renamed cnplt_config to cnplt_config.big
!                  Added definition of a compiler directive BIG_SCREEN
!  pet 1999.09.29  Increased JGLBC4_BLOCKS  from 100 to 106 blocks
!  pet 1999.10.05  Added definitions of CRL__UND, CRL__ASC, CRL__BIN
!  pet 1999.10.11  Added definition of JPVERS_BLOCKS
!  pet 1999.10.12  Set MAX_PAR to 16384
!  pet 1999.10.13  Added definition of constants MAX_ARC_COREL, MAX_PAR_COREL
!                  and MAX_PRM
!  pet 1999.10.13  Changed value of ECC_DATA_FILE  from ECCDAT to
!                  ECCDAT.ecc
!  pet 1999.10.29  Changed values of some directories for GIUB
!  pet 1999.11.08  Added constants M_CLS, M_CLB, M_CLZ, M_CLM
!  pet 1999.11.17  Added constants MCL__GRX, MCL__PHX, MCL__RTX,
!                                  MCL__GRS, MCL__PHS, MCL__RTS
!  kdb 1999.12.23  Added parameter values for KASHIMA.
!  pet 2000.01.28  Consolidate recent changes
!  pet 2000.02.01  Added parameter values for MPIFR (Bonn).
!  pet 2000.02.09  Added definition of the directory PHD_DIR (for Phase Doctor)
!  pet 2000.03.27  Added constants definitions for EQUMEM
!  pet    2000.05.24  Moved installation-specific customization to local file
!  pet    2000.05.25  Added parameter CENTER_FULL_NAME (NB: the same parameter
!                     as sell as CENTER_ABR is defined in
!                     ../src/includes/catalog_parameters.i)
!  pet    2000.05.31  Added files fro revision date and release date
!  pet    2000.06.13  Raised MAX_SRC         from 768 to 1536
!                     Raised MAX_ARC_SRC     from 300 to  384
!                     Raised MAX_GRAD        from  22 to   64
!                     Raised JGLBC4_BLOCKS   from 106 to  117
!                     Raised JPARFIL_BLOCKS  from 256 to  330
!                     Reduced MAX_CAL        from  15 to   10
!                     Reduced MAX_CLZ        from  15 to    8
!                     Reduced JOBSREC_BLOCKS from  11 to   10
!  pet    2000.07.03  Added constants PHC__xxx which describe phase cal status
!  pet    2000.07.05  Added parameter MK5_ROOT
!  pet    2000.07.12  Added parameter CENTER_LABEL
!  pet    2000.07.13  Corrected value of PHC__MAN
!  pet    2000.07.20  Raised MAX_SRC         from 1536 to 4096
!                     Raised MAX_ARC_SRC     from  384 to  512
!                     as a result raised
!                            JGLBC4_BLOCKS   from  117 to  133
!                            JSOCOM_BLOCKS   from   40 to   46
!                            JPARFIL_BLOCKS  from  330 to  570
!  pet    2000.08.01  Raised JGLBC3_BLOCKS   from   55 to  171
!                     in according with change of MAX_SRC
!  pet    2000.09.19  Added definition of constant STA__SUC
!  pet    2000.10.04  Added constant SCRATCH_DATE
!  pet    2000.11.22  Increased  JGLBC4_BLOCKS  from 133 to 134 blocks
!  pet    2001.01.10  Added 11 constants __TS  for timescales
!  pet    2001.01.12  Added  4 constants __MET for metric names
!  pet    2001.03.01  Added definition for user partials bookkeeping
!                     UPF__UND, UPF__FUL, UPF__DEL, UPF__CMP constants
!  pet    2001.03.01  Increased GLBCM from 13 to 14 blocks
!  pet    2001.03.07  Raised MAX_NUM_MET_VALUES from 1500 to M_SCA
!  pet    2001.03.07  Raised MAX_SCA from 2048 to 3072
!  pet    2001.03.08  Added constrains CNI__MIX, CNI__ACI, CNI__DIR, CNI__UND
!  pet    2001.04.24  Added parameters MAX__FRQ, MIN__FRQ, IONFR_MODE__USEAP,
!                                      IONFR_MODE__NOUSEAP
!  pet    2001.04.27  Added parameter  XS__FRQ
!  pet    2001.05.01  Added parameter  TAU_ERR__TINY
!  pet    2001.06.15  Made parameters SUPCAT_FILE extarnally customaizable
!  pet    2001.09.05  Added parameters MAX4_WEIREC -- maximal number of
!                     records in weight file
!  pet    2002.03.27  Removed variables SNX_WOB__SIGMA,  SNX_UT1__SIGMA,
!                     SNX_NUT__SIGMA,  SNX_POS__SIGMA,  SNX_VEL__SIGMA
!  pet    2002.03.27  Increased JGLBC4_BLOCKS from 134 to 137 blocks
!  pet    2002.04.03  Added constants NAMSTA_FILE, NAMSOU_FILE
!  pet    2002.04.08  Added constants PI__NUM, MAS__TO__RAD, RAD__TO__MAS
!  pet    2002.04.08  Added constants OM__EAR
!  pet    2002.05.03  Increased JGLBC4_BLOCKS from 137 to 140 blocks
!  pet    2002.05.08  Added constant  RAD__TO__MSEC
!  pet    2002.05.10  Increased JGLBC4_BLOCKS from 140 to 141 blocks
!  pet    2002.05.17  Added constants YEAR__TO__SEC, YEAR__TO__DAY and
!                     MM__TO__SEC
!  pet    2002.09.25  Added constants CNI__ULC, CNI__UGL, CNS__LABEL
!  pet    2002.09.26  Increased sie of PARFIL block from 570 to 1074 blocks
!  pet    2002.09.27  Added constant M__POSVAR
!  pet    2002.09.27  Increased JGLBC4_BLOCKS from 141 to 146 blocks
!  pet    2002.09.30  Added constants J2000__JD, J2000__YR, JD2000__MJD
!  pet    2002.10.01  Added constant  ECC__LABEL
!  pet    2002.12.12  Increased JGLBC4_BLOCKS from 146 to 151 blocks
!  pet    2002.12.13  Added constants: PSV__HMD, PSV__TSR, PSV__CLS, PSV__LIN,
!                                      PSV__SPL, PSV__AVL, PSV__REQ, PSV__ALC,
!                                      PSV__REA
!  pet    2002.12.16  Added constants: M__HPSLEN, M_BDSLEN
!  pet    2002.12.17  Added constants: M__PSV, OVH__PSV, NEA__PSV
!  pet    2002.12.18  Added constants: M__BDS
!  pet    2002.12.21  Corrected the error in J2000__JD constant, oh, Goodness!
!  pet    2002.12.21  Increased constant JGLBCM_BLOCKS from 14 to 17
!  pet    2002.12.24  Added constants JYEAR__DAYS and SOLVE_REF_EPOCH__JD
!  pet    2002.12.24  Added temporarily ENDIAN__FMT constant
!  pet    2002.12.25  Added constant SPOOL__FMT
!  pet    2002.12.31  Added constants TRUE__L1,  TRUE__L2,  TRUE__L4,
!                                     FALSE__L1, FALSE__L2, FALSE__L4,
!  pet    2003.08.12  Added constants CRES__CURRENT, CRES__PRE98, CRES__PRE03
!  pet    2003.08.15  Moved MAX_FLYBY_EOP_VALUES from glbc4.i
!  pet    2003.09.02  Raised JGLBC3_BLOCKS   from   171 to  173
!  pet    2003.10.02  Added constatns NUT__UNDF, NUT__WAHR1980, NUT__IERS1996,
!                                     NUT__REN2000, NUT__MHB2000
!  pet    2004.03.09  Increased JGLBC4_BLOCKS from 151 to 152 blocks
!  pet    2004.06.29  Increased JGLBC2_BLOCKS from  32 to 251 blocks
!  pet    2004.09.09  Increased M__BDSLEN from 512 to 1024
!  pet    2004.11.18  Moved
!                       mathematical and astronomical constants to astro_constants.i
!                       nutation-related constants to heo.i
!                       position variation constants to harpos.i and bindisp.i
!  pet    2004.11.24  Increased JGLBC4_BLOCKS from 152 to 155 blocks
!  pet    2004.12.09  Added definitions of constants SNG_X__DTP, SNG_S__DTP,
!                     SNG_X__DTC, SNG_S__DTC, updated DATYP__ABR
!  pet    2004.12.09  Added parameter FREQ__S_DEFAILT
!  pet    2005.02.21  Added constants M__HPE, M__SPE, M__SPN, M__SPD
!  pet    2005.02.22  Added types HPE__TYPE, SPE__TYPE
!  pet    2005.02.24  Added types HPESOL__TYPE, SPESOL__TYPE
!  pet    2005.03.18  Added constants SRC_PRE2004_SPOOL__FMT,
!                     SRC_SHORT_SPOOL__FMT, SRC_LONG_SPOOL__FMT
!  pet    2005.09.03  Added parameter MAX__DELAY
!  pet    2005.11.24  Added definitions of OBS__2BN, DET__2BN, AVL__2BN, ION__2BN
!  pet    2005.12.05  Added definitions of SLV__MAX_SOLTYP
!  pet    2005.12.08  Added CAL__TYPE definition
!  pet    2005.12.10  Added constants ERM__MSPL, ERM__MKNOT
!  pet    2005.12.10  Increased JGLBC4_BLOCKS from 155 to 156 blocks
!  pet    2005.12.13  Increased MAX4_WEIREC from 65536 to 98304
!  pet    2006.01.24  Added defintion of ERM data structure
!  pet    2006.03.24  Increased JGLBC4_BLOCKS from 156 to 157 blocks
!  pet    2006.03.27  Raised MAX_SRC from 4096 to 8192
!  pet    2006.03.27  Increased JSOCOM from 46 to 58 blocks, increaed JPARFIL
!                     from 1074 to 1906 blocks
!  pet    2006.03.29  Increased JGLBC3_BLOCKS from 173 to 317 blocks
!  pet    2006.05.25  Added parameters related to estimation of the harmonic
!                     Earth orientation variations
!  pet    2006.06.25  Added deficintion of datastructure EHEC_TYPE
!  pet    2006.07.06  Increased parameter M__SPE from 16 to 64
!  pet    2006.11.08  Increased parameter JGLBC4_BLOCKS from 157 to 158
!  pet    2006.12.11  Increased parameter MAX_PAR from 20000 to 32000
!  pet    2007.03.15  Added data structure EHES for reciporocal weights of
!                     EHEO constraints
!  pet    2007.06.05  Added SUPMET__META constant
!  pet    2007.06.05  Added bits INIT__SPS and IOUS__SPS
!  pet    2007.06.06  Changed JRESREC from 52 to 60 2-bytes words
!  pet    2007.06.23  Added definition of OPP_DUAL__BIT
!  pet    2007.07.26  Increased parameter JGLBC3_BLOCKS from 173 to 2530
!  pet    2007.08.09  Added constants of the SOUADM family for source structure
!                     admittance estimation
!  pet    2007.10.09  Increased MAX_SUP from 20000 to 32000
!  pet    2007.10.25  Added definition of constants EDC__UNDF, EDC__CRE, 
!                     EDC__USE, EDC__REQ, EDC__ASC, EDC__BIN
!  pet    2007.10.26  Added definition of constants DCSP__UAS, DCFC__UAS 
!  pet    2007.11.05  Added definition of TPD constants and data structures 
!  pet    2007.11.09  Increased parameter JGLBC4_BLOCKS from 158 to 161
!  pet    2007.12.03  Added constaints RWT_EL_STA__CNB and RWT_SRC__CNB
!  pet    2007.12.03  Increased parameter JGLBC3_BLOCKS from 2802 to 2807
!  pet    2008.03.22  Added contstants NUT__PSE, NUT__XY
!  pet    2008.04.21  Increased JGLBC4_BLOCKS from 161 to 163 blocks
!  pet    2008.04.23  Added constants ATD__UNDF, ATD__USE, ATD__REQ
!                                     AOC__UNDF, AOC__USE, AOC__REQ
!  pet    2009.06.01  Added constant  SOLVE__RW_EL_MULT_GLOB 
!  pet    2010.02.06  Added constant  MAX_BND
!  pet    2010.02.07  Added constants defined in solve.lcl : 
!                     SOLVE_PS_VIEWER and SOLVE_GIF_VIEWER 
!  pet    2010.02.08  Reduced  MAX_CLM from 5 to 3 
!  pet    2010.04.09  Increased parameter MAX_SCA from 3072 to 8192
!  pet    2010.06.10  Added constant LSNR__SPS
!  pet    2010.10.23  Added derived type TCN
!  pet    2010.11.04  Added constants TCN__REGR, TCN__DEL
!  pet    2010.12.21  Increased constant JGLBCM_BLOCKS from 17 to 25
!  pet    2011.02.14  Increased MAX4_WEIREC from 98304 to 262144
!  pet    2011.09.29  Increased constant MAX_SRC from 8192 to 12288.
!                     As a result, JSOCOM_BLOCKS   increased from   58 to   68
!                                  JGLBC3_BLOCKS   increased from 2807 to 3081
!                                  JPARFIL_BLOCKS  increased from 1906 to 2738
!  pet    2012.03.12  Increased constant MAX_SRC from 12288 to 16384.
!                     Increased constant MAX_ARC_SRC from 512 to 1024
!                     As a result, JSOCOM_BLOCKS   increased from   68 to   78
!                                  JGLBC3_BLOCKS   increased from 3081 to 3867
!                                  JPARFIL_BLOCKS  increased from 2738 to 3570
!                                  JGLBC4_BLOCKS   increased from  163 to  227
!  pet    2015.10.23  Increased constant MAX4_BRK from 8 to 16
!  pet    2016.03.12  Increased constant MAX_SRC from 16384 to 32000
!                     As a result, JSOCOM_BLOCKS   increased from   78 to  116
!                                  JGLBC3_BLOCKS   increased from 3867 to 4911
!                                  JPARFIL_BLOCKS  increased from 3570 to 6742
!                                  JGLBC4_BLOCKS   increased from  163 to  227
!  pet    2016.08.10  Increased JGLBC2_BLOCKS from  251 to 257 blocks
!  pet    2016.12.25  Introduced new data type DUAL
!  pet    2017.07.15  Increased LSELAR and as a result
!                                  JGLBC2_BLOCKS   increased from  257 to 1025
!  pet    2017.10.14  Increased JGLBC4_BLOCKS                from  227 to 231
!  pet    2017.10.22  Increased MAX_PAR from 32000 to 32700
!  pet    2017.10.23  Added SOLVE_PIMA_DIR variable
!  pet    2017.10.23  Increased M_GPA from 32700 to 100000
!                     Increaed JGLBCM_BLOCKS from 25 to 73 blocks
!  pet    2017.11.27  Ranamed MK6_ROOT to PSOLVE_ROOT. Added PSOLVE_DIR variable
!  pet    2018.12.23  Added ERR_FUDGE_FACTOR__DEF variable
!  pet    2020.01.01  Increased M__SPE from 64 to 128
!  pet    2020.04.27  Added constants EDIT_STS, and EDIT_USE
!  pet    2020.07.14  Increased JGLBC4_BLOCKS                from  231 to 232
!  pet    2020.09.09  Increased the number of arc station from 32 to 256
!                     increased JPARFIL_BLOCKS  from 6742 to   6756
!                     increased JGLBC3_BLOCKS   from  491 to 104528
!                     increased JGLBC4_BLOCKS   from  232 to    337
!                     increased USED_PLTFL to 255*(256/2)
!  pet    2020.09.30  reduced   MAX_EROT_VALUES from 15 to 7
!  pet    2020.10.01  Added  PSOLVE__STACK_SIZE_IN_GIGABYTES definition
!  pet    2020.12.13  Added  SIMUL__STACK_SIZE_IN_GIGABYTES  definition
!  pet    2021.01.20  Increased MAX_SRC fromc 32000 to 32766
!  pet    2021.01.20  Increased PARFIL from   6766 to   6910
!  pet    2021.01.20  Increased GLBC3  from 104528 to 104580
!  pet    2021.01.20  Increased SOCOM  from    293 to    294
!  pet    2021.03.20  Increased JRESREC_WORDS from 60 to 70
!  pet    2021.06.01  Added parameter MAX_SOU and set it to 44444 (max total number of sources)
!                     Increased JPARFIL_BLOCKS  from 6910 to 9011
!  pet    2021.06.01  Increased JSARREC_WORDS from 21 to 26
!  pet    2021.09.07  Decreased MAX_SRC 32766 to 32752. Added MAX4_SRC and MAX4_STA
!  pet    2021.10.04  Increased MAX4_WEIREC from 256*1024 to 512*1024
!  pet    2021.12.31  Increased JGLBC4_BLOCKS from 337 to 338
!  pet    2021.12.31  Added LABEL__AOC, LABEL__ADDW, LABEL__EDIT, LABEL__DTEC
!  pet    2022.01.01  Renamed data type DUAL with data type FUSED
!  pet    2022.02.14  Added bit fields  DTD__STS, DTH__STS, DTL__STS, DTHL__STS
!  pet    2022.02.22  Increased MAS_ATM from 300 to 1600, increased SOCOM from 294 to 582
!  pet    2022.07.30  Added variable MAX4_BSL
!  pet    2022.08.21  Added constants IOS__SES, IOS__STA, IOS__BAS
!  pet    2022.12.06  Increased size of glb4 block
!  pet    2023.10.21  Increased size of MAX_ARCS from 16384 to 32000
!  pet    2023.12.22  Added SOLVE_GVF_DIR variable
!  pet    2023.12.22  Added SOLVE_VERSION variable
!  pet    2023.12.22  Increased JGLBC4_BLOCKS from 342 to 343 bocks
!  pet    2024.07.08  Added SRC_POST2021_SPOOL__FMT and SCA_STAT_MAX_GAP constants
!  pet    2024.07.08  Increased JSARREC_WORDS from 26 to 38
!  pet    2024.11.25  Added LABEL__SCAW
!  pet    2024.11.30  Added constatns ADDW__UNDF, ADDW__REPL, ADDW__QUAD, ADDW__MIN, ADDW__MAX
!
!=====================
!
      REAL*8     SOLVE_REF_EPOCH__JD
      PARAMETER  ( SOLVE_REF_EPOCH__JD  = 2451544.5D0 )  ! Solve reference epoch: 2000.01.01_00:00:00.0
!
!      PARAMETER  ( SOLVE_REF_EPOCH__JD  = 2444529.5D0 ) ! Old Solve reference epoch:
!                                    ! 1980.10.17_00:00:00 -- Solve used this
!                                    ! epoch as a default for site position
!
      LOGICAL*1    TRUE__L1,           FALSE__L1
      LOGICAL*2    TRUE__L2,           FALSE__L2
      LOGICAL*4    TRUE__L4,           FALSE__L4
      PARAMETER  ( TRUE__L1 = .TRUE.,  FALSE__L1 = .FALSE. )
      PARAMETER  ( TRUE__L2 = .TRUE.,  FALSE__L2 = .FALSE. )
      PARAMETER  ( TRUE__L4 = .TRUE.,  FALSE__L4 = .FALSE. )
!
      INTEGER*4    PSOLVE__STACK_SIZE_IN_GIGABYTES, SIMUL__STACK_SIZE_IN_GIGABYTES  
      PARAMETER  ( PSOLVE__STACK_SIZE_IN_GIGABYTES =  8 )
      PARAMETER  ( SIMUL__STACK_SIZE_IN_GIGABYTES  =  8 )
!
! --- File system constants
!
      INTEGER*2 INTS_WORDS, INTL_WORDS, REALS_WORDS, REALL_WORDS
      INTEGER*2 WORD_BYTES,WORD_BITS
      PARAMETER ( INTS_WORDS  = 1, &
     &            INTL_WORDS  = 2, &
     &            REALS_WORDS = 2, &
     &            REALL_WORDS = 4, &
     &            WORD_BYTES =  2, &
     &            WORD_BITS  = 16  )
      INTEGER*2 BLOCK_WORDS, NAME_SIZE, NAME_WORDS, NAME_BYTES, BLOCK_BYTES
      INTEGER*4 JMAX_TRANS_BLOCKS
      PARAMETER ( NAME_SIZE         = 63, &
     &            NAME_WORDS        = (NAME_SIZE+WORD_BYTES-1)/2, &
     &            NAME_BYTES        = NAME_WORDS*WORD_BYTES, &
     &            BLOCK_WORDS       = 128, &
     &            BLOCK_BYTES       = 256, &
     &            JMAX_TRANS_BLOCKS = 504 )
!
! --- MAX_PAR is put here so that rest of file has access to it.
!
      INTEGER*2   MAX_ARCS
!
! --- Maximal number of arcs estimated in one solution.
!
      PARAMETER ( MAX_ARCS = 32000 ) ! Local customization
!
      INTEGER*4    M_GPA           ! the same as MAX_PAR, but INTEGER*4
      PARAMETER  ( M_GPA = 100000 )
!!
! --- Parameters MAX_ARC_COREL and MAX_PAR_COREL for COREL only.
! --- COREL is obsolecent and may be removed in forther releases
!
      INTEGER*2 MAX_PAR_COREL, MAX_ARC_COREL ! for corel only!
      PARAMETER ( MAX_PAR_COREL = 4096 ) ! Local customization
      PARAMETER ( MAX_ARC_COREL = 3072 ) ! Local customization
      INTEGER*2    MAX_PAR_COREL_2
      PARAMETER  ( MAX_PAR_COREL_2 = 2*MAX_PAR_COREL )
!?      INTEGER*2   MAX_PRM ! For  BACK and COREL
!?      PARAMETER  ( MAX_PRM = 32000 ) !  ???
!
! --- PARFIL must be a whole number of blocks
! --- PARFIL is now 1074 block long.
!
      INTEGER*4 JPARFIL_BLOCKS,JPARFIL_WORDS,JPARFIL_BYTES
      PARAMETER ( JPARFIL_BLOCKS = 9009, &
     &            JPARFIL_WORDS  = JPARFIL_BLOCKS*BLOCK_WORDS, &
     &            JPARFIL_BYTES  = JPARFIL_WORDS*WORD_BYTES    )
!
! --- SOCOM must be a whole number of blocks
! --- SOCOM is now 40 blocks long
!
      INTEGER*4   JSOCOM_BLOCKS, JSOCOM_WORDS, JSOCOM_BYTES
      PARAMETER ( JSOCOM_BLOCKS = 582, &
     &            JSOCOM_WORDS  = JSOCOM_BLOCKS*BLOCK_WORDS, &
     &            JSOCOM_BYTES  = JSOCOM_WORDS*WORD_BYTES    )
!
! --- Plist must be a whole number of blocks. Now calculated at compile time.
!
! --- JPLIST_FILL is number of "Blank words" -1. (Have at least 1 blank word)
!
      INTEGER*4 JPLIST_BLOCKS,JPLIST_WORDS,JPLIST_BYTES,JPLIST_FILL
      PARAMETER ( &
     &     JPLIST_WORDS  = 5+10*M_GPA, &
     &     JPLIST_BLOCKS = (JPLIST_WORDS+BLOCK_WORDS-1)/BLOCK_WORDS, &
     &     JPLIST_FILL   = JPLIST_BLOCKS*BLOCK_WORDS-JPLIST_WORDS, &
     &     JPLIST_BYTES  = JPLIST_WORDS*WORD_BYTES                  )
!
! --- GLBFIL must be a whole number of blocks
! --- GLBFIL is now xxxxx blocks long = GLBCM(   108 blocks)
!                                     + GLBC2(  1025 blocks)
!                                     + GLBC3(104580 blocks)
!                                     + GLBC4(   342 blocks)
!
      INTEGER*4 JGLBFIL_BLOCKS, JGLBCM_BLOCKS, JGLBC2_BLOCKS, &
     &          JGLBC3_BLOCKS,  JGLBC4_BLOCKS, &
     &          JGLBCM_POS, JGLBC2_POS, JGLBC3_POS, JGLBC4_POS, &
     &          JGLBCM_WORDS, JGLBFIL_WORDS
      PARAMETER ( JGLBCM_BLOCKS  =    108, &
     &            JGLBC2_BLOCKS  =   1025, &
     &            JGLBC3_BLOCKS  = 104580, &
     &            JGLBC4_BLOCKS  =    343, &
     &            JGLBFIL_BLOCKS = JGLBCM_BLOCKS + JGLBC2_BLOCKS + &
     &                             JGLBC3_BLOCKS + JGLBC4_BLOCKS, &
     &           JGLBCM_POS    = 1, &
     &           JGLBC2_POS    = JGLBCM_POS + JGLBCM_BLOCKS, &
     &           JGLBC3_POS    = JGLBC2_POS + JGLBC2_BLOCKS, &
     &           JGLBC4_POS    = JGLBC3_POS + JGLBC3_BLOCKS, &
     &           JGLBCM_WORDS  = JGLBCM_BLOCKS*BLOCK_WORDS, &
     &           JGLBFIL_WORDS = JGLBFIL_BLOCKS*BLOCK_WORDS  )
!
      INTEGER*4  JGLBP_BLOCKS, JGLBP1_BLOCKS
      PARAMETER  ( JGLBP1_BLOCKS = 321 )
      PARAMETER  ( JGLBP_BLOCKS  = JGLBP1_BLOCKS + &
     &                             JSOCOM_BLOCKS + &
     &                             JPARFIL_BLOCKS  )
!
      INTEGER*4    JPVERS_BLOCKS
      PARAMETER  ( JPVERS_BLOCKS = 9 )
!
! --- Maximum parameters available
!
      INTEGER*4 JMAX_PAR
      INTEGER*8 JMAX_TRI
!
! --- Moved MAX_PAR definition to the top. JMG 96JUL30
!
      PARAMETER ( JMAX_PAR = M_GPA )
      PARAMETER ( JMAX_TRI = (INT8(JMAX_PAR)*INT8(JMAX_PAR+1))/2 )
!
! --- Size of NRMFIL, must be whole number of blocks
!
      INTEGER*4   JNRMFIL_BLOCKS
      INTEGER*8   JNRMFIL_ELEMS, JNRMFIL_WORDS
      PARAMETER ( JNRMFIL_ELEMS  = 3*JMAX_PAR+JMAX_TRI       )
      PARAMETER ( JNRMFIL_WORDS  = JNRMFIL_ELEMS*REALL_WORDS )
      PARAMETER ( JNRMFIL_BLOCKS = (JNRMFIL_WORDS+BLOCK_WORDS-1)/BLOCK_WORDS)
!
! --- New CGM directory information
! --- based on parfil_blocks and socom_blocks
!
      INTEGER*2 CGM_DIR_SOCOM,CGM_DIR_PARFIL,CGM_DIR_NRMFIL
      INTEGER*2 CGM_DIR_BLOCKS,CGM_DIR_PLIST
      INTEGER*4 MAX_CGM_BLOCKS
      PARAMETER ( CGM_DIR_BLOCKS = 1                               )
      PARAMETER ( CGM_DIR_SOCOM  = 1+CGM_DIR_BLOCKS                )
      PARAMETER ( CGM_DIR_PARFIL = CGM_DIR_SOCOM+JSOCOM_BLOCKS     )
      PARAMETER ( CGM_DIR_PLIST  = CGM_DIR_PARFIL+JPARFIL_BLOCKS   )
      PARAMETER ( CGM_DIR_NRMFIL = CGM_DIR_PLIST+JPLIST_BLOCKS     )
      PARAMETER ( MAX_CGM_BLOCKS = CGM_DIR_NRMFIL+JNRMFIL_BLOCKS-1 )
!
! --- Maximum number of stations and sources
! --- NB: Change of parameters below results in changes many other parameters,
! --- including parameters in the superfiles. As a rule of thumb you should
! --- re-make all superfiles after changes od MAX_xxx unless there are
! --- evindences that it is not necessary
!
      INTEGER*4 MAX_SOU
      INTEGER*2 MAX_STA, MAX_SRC, MAX_BSL
      INTEGER*2 MAX_ARC_STA, MAX_ARC_SRC, MAX_ARC_BSL
      INTEGER*2 MAX_STA_CMP, MAX_ATM, MAX_CLK, MAX_ROT, MAX_DBS, MAX_CLZ
      INTEGER*2 MAX_ESM, OLD_MAX_STA, OLD_MAX_STA_CMP, MAX_GRAD, MAX_CLM
      INTEGER*2 MAX_PWC_EPS, MAX_PWC_SITES, MAX_SDC, MAX_SUP
      INTEGER*2 MAX_CONT, MAX_CAL, MAX_ARC_BSL_WORDS
      INTEGER*4 MAX4_BRK, MAX4_EOP, MAX4_FRQ, MAX4_HFE, MAX4_SIT, MAX4_SRC, MAX4_BSL, MAX_BND
      PARAMETER ( MAX_STA=512,             & ! max total number of station
     &            MAX_STA_CMP=MAX_STA*3,   & ! max total number of st. components
     &            MAX_BSL=8192,            & ! max number of baselines (should be
     &            OLD_MAX_STA=128,                         & ! less 32766 !!
     &            OLD_MAX_STA_CMP=OLD_MAX_STA*3, &
     &            MAX_SOU=44444,     & ! max total number of sources
     &            MAX_SRC=32752,     & ! max total number of sources
     &            MAX_ARC_STA=256,   & ! max number of station at one session
     &            MAX_ARC_SRC=1024,  & ! max number of sources at one session
     &            MAX_ARC_BSL=(MAX_ARC_STA*(MAX_ARC_STA-1))/2, &
     &            MAX_ARC_BSL_WORDS=(MAX_ARC_BSL+15)/16, &
     &            MAX_ATM=1600,     & ! max number of atmosphere segments
     &            MAX_CLK=1600,     & ! max number of clock segments
     &            MAX_GRAD=64,      & ! max number of atmosphere gradient segments
     &            MAX_ROT=12,       & ! max number of (old!) rotation epoch
     &            MAX_ESM=240,      & ! episodic site motions
     &            MAX_PWC_EPS=128,  & ! number of station epochs
     &            MAX_PWC_SITES=32, &
     &            MAX_SDC=100,      &
     &            MAX_SUP=32000,    & ! number of superfiles in SUPCAT
     &            MAX_CONT=15,   & ! Maximal number of obs-dependent contributions
     &            MAX_CAL=10,    & ! Maximal number of station-dep calibrations
     &            MAX_CLZ=10,    & ! Maximal number of station-dep zen./calibrat.
     &            MAX_CLM=3,     & ! Maximal number of mode ecalibrations
     &            MAX4_EOP=100,  & ! Max number of segments for EOP
     &            MAX4_BRK=16,   & ! Max number of clock breaks for one station
     &            MAX4_HFE=128,  & ! Max number of epochs for interpolation of high
     &            MAX_DBS=15,    & ! Maximal number of databases in scratch files
     &            MAX4_FRQ=512,  & ! Maximal number of different frequencies
!                                  ! in one scan (16 for Mark-4, 512 for S2)
     &            MAX_BND=2      & ! Maximal number bands in one experiment
     &          )
      PARAMETER  ( MAX4_SIT = MAX_STA )
      PARAMETER  ( MAX4_SRC = MAX_SRC )
      PARAMETER  ( MAX4_BSL = MAX_BSL )
      INTEGER*4  M_SUP
      PARAMETER  ( M_SUP = MAX_SUP ) ! The same as MAX_SUP but INTEGER*4
!
! --- Max_sup_out = maximum number of entries allowed in the file that
! --- specifies where mksup may create new superfiles
!
      INTEGER*2   MAX_SUP_OUT
      PARAMETER ( MAX_SUP_OUT = 60 )
!
! --- Maximal number of allowed equations of user constraints
!
      INTEGER*2   MAX_USC
      PARAMETER ( MAX_USC = 512 )
!
! --- The following sets up all the parameters relative to the pltfl.i
!
! --- Set up on March 28, 1994 to be compatible with existing superfiles
! --- because 'pltfil' is saved in superfiles.
!
! --- Max_plots set equal to max_arc_bsl
!
! --- The logic below computes jpltfil_blocks from more primative information.
!
      INTEGER*2 MAX_PLOTS,USED_PLTFL,FREE_LEN_PLTFL
      INTEGER*4 JPLTFIL_WORDS,JPLTFIL_BLOCKS
!
      PARAMETER ( MAX_PLOTS = MAX_ARC_BSL  )
      PARAMETER ( USED_PLTFL = 255*(256/2) )
      PARAMETER ( JPLTFIL_BLOCKS  = (USED_PLTFL/BLOCK_WORDS)+1 )
      PARAMETER ( JPLTFIL_WORDS   = JPLTFIL_BLOCKS*BLOCK_WORDS )
      PARAMETER ( FREE_LEN_PLTFL  = JPLTFIL_WORDS - USED_PLTFL )
!
      INTEGER*2 STA_BIT_WORDS,SRC_BIT_WORDS,ARC_STA_BIT_WORDS
      INTEGER*2 ROT_BIT_WORDS,ATM_BIT_WORDS
      PARAMETER ( STA_BIT_WORDS    =(MAX_STA+WORD_BITS-1)/WORD_BITS,     &
     &            ARC_STA_BIT_WORDS=(MAX_ARC_STA+WORD_BITS-1)/WORD_BITS, &
     &            SRC_BIT_WORDS    =(MAX_SRC-1+WORD_BITS)/WORD_BITS,     &
     &            ROT_BIT_WORDS    =(MAX_ROT*4+WORD_BITS-1)/WORD_BITS,   &
     &            ATM_BIT_WORDS    =(MAX_ATM+WORD_BITS-1)/WORD_BITS      &
     &          )
!
! --- SOLVE_PROG_DIR: where type 6 files are stored
! --- SOLVE_CART:     where the scratch files are
!
      CHARACTER    SOLVE_VERSION*8
      CHARACTER    CGM_DIR*19
      CHARACTER    PSOLVE_ROOT*25
      CHARACTER    PSOLVE_DIR*6
      CHARACTER    SOLVE_PROG_DIR*18
      CHARACTER    SOLVE_WORK_DIR*20
      CHARACTER    SOLVE_SAVE_DIR*19
      CHARACTER    SOLVE_HELP_DIR*17
      CHARACTER    SOLVE_PIMA_DIR*9
      CHARACTER    SPOOL_DIR*21
      CHARACTER    SOLVE_GVF_DIR*7
      PARAMETER  ( SOLVE_VERSION   = "20241125" )
      PARAMETER  ( CGM_DIR         = "/scr/psolve/cgm_dir" )
      PARAMETER  ( PSOLVE_ROOT     = "/f1/progs/psolve_20241125" )
      PARAMETER  ( PSOLVE_DIR      = "/opt64" )
      PARAMETER  ( SOLVE_PROG_DIR  = "/opt64/psolve/bin/" )
      PARAMETER  ( SOLVE_WORK_DIR  = "/scr/psolve/work_dir" )
      PARAMETER  ( SOLVE_SAVE_DIR  = "/opt64/share/psolve" )
      PARAMETER  ( SOLVE_HELP_DIR  = "/opt64/psolve/doc" )
      PARAMETER  ( SOLVE_PIMA_DIR  = "undefined" )
      PARAMETER  ( SPOOL_DIR       = "/scr/psolve/spool_dir"   )
      PARAMETER  ( SOLVE_GVF_DIR   = "/l2/gvf" )
      CHARACTER    SCRATCH_DIR*4
      PARAMETER  ( SCRATCH_DIR = "/tmp" )
!
      CHARACTER    ECC_DATA_FILE*10
      CHARACTER    PRES_CAL_FILE*8
      CHARACTER    AVAL_FCAL_FILE*18
      CHARACTER    AVAL_PART_FILE*20
      PARAMETER  ( ECC_DATA_FILE  = 'ECCDAT.ecc'               )
      PARAMETER  ( PRES_CAL_FILE  = 'PRES_DAT'                 )
      PARAMETER  ( AVAL_FCAL_FILE = 'flyby_calibrations'       )
      PARAMETER  ( AVAL_PART_FILE = 'partial_calibrations'     )
!
! --- OBSFIL record length, maximum number of observations
!
      INTEGER*4   MAX_OBS
      PARAMETER ( MAX_OBS = 512*1024 ) ! 
!
! --- Maximal number of records in weights file
!
      INTEGER*4   MAX4_WEIREC
      PARAMETER ( MAX4_WEIREC = 512*1024 )
!
      INTEGER*4   MAX_SCA
      PARAMETER ( MAX_SCA = 3072 ) ! maximal number of scans in one session
!
      INTEGER*4   JOBSREC_BLOCKS, JOBSREC_WORDS, JOBSREC_BYTES
      PARAMETER ( JOBSREC_BLOCKS = 10, &
     &            JOBSREC_WORDS  = JOBSREC_BLOCKS*BLOCK_WORDS, &
     &            JOBSREC_BYTES  = JOBSREC_BLOCKS*BLOCK_BYTES )
!
! --- RESFIL record length
!
      INTEGER*4   JRESREC_WORDS
      PARAMETER ( JRESREC_WORDS = 70 )
!
! --- SARFIL record length in 2-byte words
!
      INTEGER*4   JSARREC_WORDS
      PARAMETER ( JSARREC_WORDS = 38 )
!
! --- NAMFIL processing intrinsics
!
      INTEGER*4   JNAMREC_WORDS, JNAMFIL_BLOCKS
      PARAMETER ( JNAMREC_WORDS  = 35, &
     &            JNAMFIL_BLOCKS = 50 )
      INTEGER*2   UNITNAM
      PARAMETER ( UNITNAM = 111 )
!
! --- LU (logical unit) on which data base catalog is located
!
      INTEGER*2   CATCART1
      PARAMETER ( CATCART1 = 25 )
!
! --- Path & lu for file to plot earth orientation adjustments
!
      CHARACTER*4 EOPL_BASE
      PARAMETER ( EOPL_BASE = 'EOPL' )
!
      INTEGER*2   EOPL_LU
      PARAMETER ( EOPL_LU = 24 )
!
! --- Set the maximum length of the polar motion and ut1 times series from
! --- the database.  Used in SDBH/BLKCL.
!
      INTEGER*2   MAX_EROT_VALUES
      PARAMETER ( MAX_EROT_VALUES = 7 )
!
      CHARACTER FLYBY_DEFAULTS*13
      PARAMETER ( FLYBY_DEFAULTS = 'flyby_mod_set' )
!
      CHARACTER*22 CWSP2393, CWSP2648, CWSP2623, CWSP7470, &
     &             CWSP9872, CWSP2686, CWSP7475, CWSP2563
      CHARACTER*5  LETOK_FILE
      CHARACTER*15 CWSP835
      INTEGER*2    LEN2393,LEN835,LEN2648,LEN2623,LEN7470,LEN9872, &
     &             LEN2686,LEN7475,LEN2563
      PARAMETER ( LETOK_FILE = 'letok')
      PARAMETER ( CWSP2393 = '/usr/local/bin/wsp2393')
      PARAMETER ( CWSP2648 = '/usr/local/bin/wsp2648')
      PARAMETER ( CWSP2623 = '/usr/local/bin/wsp2623')
      PARAMETER ( CWSP7470 = '/usr/local/bin/wsp7470')
      PARAMETER ( CWSP9872 = '/usr/local/bin/wsp9872')
      PARAMETER ( CWSP2686 = '/usr/local/bin/wsp2686')
      PARAMETER ( CWSP7475 = '/usr/local/bin/wsp7475')
      PARAMETER ( CWSP2563 = '/usr/local/bin/wsp2563')
      PARAMETER ( CWSP835 = '/mk3/bin/wsp835')
      PARAMETER ( LEN2393 = 22,LEN2648 = 22,LEN2623 = 22,LEN7470 = 22, &
     &            LEN9872 = 22,LEN2686 = 22,LEN7475 = 22,LEN2563 = 22, &
     &            LEN835 = 15 )
      CHARACTER*4  BASFE_FNAME
      PARAMETER  ( BASFE_FNAME= 'BASF' )
      CHARACTER*9  SITPL_FILE
      PARAMETER  ( SITPL_FILE = 'sitpl.dat' )
      CHARACTER*11 STATION_WEIGHT_FILE
      PARAMETER  ( STATION_WEIGHT_FILE = 'sta_wts_std' )
      CHARACTER*12 STATION_PICK_FILE
      PARAMETER  ( STATION_PICK_FILE= 'STATION_PICK' )
      CHARACTER*11 NUVEL_MOD, NUVEL_COV
      CHARACTER*8  NUVEL_WGT
      PARAMETER  ( NUVEL_MOD= 'nuvel.model')
      PARAMETER  ( NUVEL_COV= 'nuvel.covar')
      PARAMETER  ( NUVEL_WGT= 'nuvel.wt')
!
! --- Set maximum size for cres (and adjst) screen display buffer
!
      INTEGER*4    CRES_BUF_LEN, CRES_BUF_LEN2
      PARAMETER  ( CRES_BUF_LEN  = 100000 ) ! interactive SOLVE
      PARAMETER  ( CRES_BUF_LEN2 =   4096 ) ! batch SOLVE
!
! --- Set the maximum number of meteorological parameters.
! --- (Actually, this is really a
! --- limit on the number of met observations per site.)
!
      INTEGER*2   MAX_NUM_MET_VALUES
      PARAMETER ( MAX_NUM_MET_VALUES = MAX_SCA )
!
! --- People responsible for exper.cat are now mailed messages if a database
! --- being run through liptn is not in exper.cat.  The following parameter
! --- determines the recipients of the messages.  The proper format is a list
! --- of e-mail addresses separated by blanks.  This accomodates installations
! --- with e-mail address lengths other than the length of 3 considered
! --- standard by the GSFC VLBI group.
!
! --- Definition of database handler names
!
      INTEGER*4    DBH__UNDF, DBH__MK3, DBH__GVH
      PARAMETER  ( DBH__UNDF = 0    )
      PARAMETER  ( DBH__MK3  = 3001 )
      PARAMETER  ( DBH__GVH  = 3002 )
!
! --- Name the file whose existence will prevent solve, enter etc. from
! --- being run, because Solve code is being posted.
!
      CHARACTER*15 SOLVE_POST_FILNAM
      PARAMETER  ( SOLVE_POST_FILNAM = 'solve_post_file' )
!
! --- Name the file whose existence will prevent batch runs from
! --- being run, because Solve code is being posted.
!
      CHARACTER*21 SOLVE_POST_FILNAMB
      PARAMETER  ( SOLVE_POST_FILNAMB = 'solve_post_file_batch' )
!
! --- CNPLT_CONFIG_FILE -- Name of the file that sets Cnplt's X windows
! ---                      menus configuration
!
      CHARACTER CNPLT_CONFIG_FILE*16
      PARAMETER ( CNPLT_CONFIG_FILE = 'cnplt_config.big' ) ! Local customization
!
! --- CENTER_LABEL -- unofficial label of the analysis center used for
! ---                 installing CALC/SOLVE
! --- CENTER_ABR -- short 3-character abbreviation of the analysis center
! --- CENTER_FULL_NAME -- full name of the analysis center
!
      CHARACTER CENTER_LABEL*3
      CHARACTER CENTER_ABR*3
      CHARACTER CENTER_FULL_NAME*9
      PARAMETER  ( CENTER_LABEL     = "NAS" )
      PARAMETER  ( CENTER_ABR       = "NAS" )
      PARAMETER  ( CENTER_FULL_NAME = "NASA GSFC" )
!
! --- NAMSTA_FILE -- file name which keeps station codes, station name, etc.
! --- NAMSOU_FILE -- file name which keeps source names, source designators, etc
!
      CHARACTER    NAMSTA_FILE*12, NAMSRC_FILE*12
      PARAMETER  ( NAMSTA_FILE = 'ns-codes.txt' )
      PARAMETER  ( NAMSRC_FILE = 'source.names' )
!
      CHARACTER SOLVE_PS_VIEWER*16
      CHARACTER SOLVE_GIF_VIEWER*16
      CHARACTER SOLVE_STP_DIR*9
      PARAMETER  ( SOLVE_PS_VIEWER  = "/usr/bin/display" )
      PARAMETER  ( SOLVE_GIF_VIEWER = "/usr/bin/display" )
      PARAMETER  ( SOLVE_STP_DIR    = "/cont/stp" )
!
! --- Set filename for revision date and for release dates
!
      CHARACTER    RELEASE_FILE*12, REVISION_FILE*13
      PARAMETER  ( RELEASE_FILE  = 'RELEASE_DATE'  )
      PARAMETER  ( REVISION_FILE = 'REVISION_DATE' )
      INTEGER*4    SLV__MAX_SOLTYP      !  Maximum number of solution types
      PARAMETER  ( SLV__MAX_SOLTYP = 32 )
!
! --- Constants defining various types of solution
!
      INTEGER*2     GRPRAT__DTP, PHSRAT__DTP,  SNBRAT__DTP, &
     &              GRPONL__DTP, PHSONL__DTP,  SNBONL__DTP, &
     &              RATONL__DTP,  G_GXS__DTP,  PX_GXS__DTP, &
     &              PS_GXS__DTP,  PX_GX__DTP,   PX_GS__DTP, &
     &               PS_GX__DTP,  PS_GS__DTP,   P_PXS__DTP, &
     &               DELAY__DTP,   RATE__DTP,   GROUP__DTP, &
     &               PHASE__DTP,  MIXED__DTP,   SINGL__DTP, &
     &                  GX__DTP,     GS__DTP,      PX__DTP, &
     &                  PS__DTP,  SNG_X__DTP,   SNG_S__DTP, &
     &               XBAND__DTP,  SBAND__DTP,    COMB__DTP, &
     &               IOCAL__DTP,  FIRST__DTP,    LAST__DTP, &
     &               FUSED__DTP
!
      CHARACTER*21  GRPRAT__DTC, PHSRAT__DTC, SNBRAT__DTC, &
     &              GRPONL__DTC, PHSONL__DTC, SNBONL__DTC, &
     &              RATONL__DTC,  G_GXS__DTC, PX_GXS__DTC, &
     &              PS_GXS__DTC,  PX_GX__DTC,  PX_GS__DTC, &
     &                  GX__DTC,     GS__DTC,     PX__DTC, &
     &                  PS__DTC,  SNG_X__DTC,  SNG_S__DTC, &
     &               PS_GX__DTC,  PS_GS__DTC,  P_PXS__DTC, &
     &               FUSED__DTC
!
      PARAMETER ( GRPRAT__DTP = 0, GRPRAT__DTC='Group delay & rate  ' )
      PARAMETER ( PHSRAT__DTP = 1, PHSRAT__DTC='Phase delay & rate  ' )
      PARAMETER ( SNBRAT__DTP = 2, SNBRAT__DTC='N.Band delay & rate ' )
      PARAMETER ( GRPONL__DTP = 3, GRPONL__DTC='Group delay only    ' )
      PARAMETER ( PHSONL__DTP = 4, PHSONL__DTC='Phase delay only    ' )
      PARAMETER ( SNBONL__DTP = 5, SNBONL__DTC='N.Band delay only   ' )
      PARAMETER ( RATONL__DTP = 6, RATONL__DTC='Rate only           ' )
      PARAMETER (  G_GXS__DTP = 7,  G_GXS__DTC='G-Gxs combination   ' )
      PARAMETER ( PX_GXS__DTP = 8, PX_GXS__DTC='Px-Gxs combination  ' )
      PARAMETER ( PS_GXS__DTP = 9, PS_GXS__DTC='Ps-Gxs combination  ' )
      PARAMETER (  PX_GX__DTP = 10, PX_GX__DTC='Px-Gx combination   ' )
      PARAMETER (  PX_GS__DTP = 11, PX_GS__DTC='Px-Gs combination   ' )
      PARAMETER (  PS_GX__DTP = 12, PS_GX__DTC='Ps-Gx combination   ' )
      PARAMETER (  PS_GS__DTP = 13, PS_GS__DTC='Ps-Gs combination   ' )
      PARAMETER (  P_PXS__DTP = 14, P_PXS__DTC='P-Pxs combination   ' )
      PARAMETER (     GX__DTP = 15,    GX__DTC='Group delay X-band  ' )
      PARAMETER (     GS__DTP = 16,    GS__DTC='Group delay S-band  ' )
      PARAMETER (     PX__DTP = 17,    PX__DTC='Phase delay X-band  ' )
      PARAMETER (     PS__DTP = 18,    PS__DTC='Phase delay S-band  ' )
      PARAMETER (  SNG_X__DTP = 19, SNG_X__DTC='Single band X-band  ' )
      PARAMETER (  SNG_S__DTP = 20, SNG_S__DTC='Single band S-band  ' )
      PARAMETER (  FUSED__DTP = 21, FUSED__DTC='Fused group delays' )
      PARAMETER (  DELAY__DTP = 101                                   )
      PARAMETER (   RATE__DTP = 102                                   )
      PARAMETER (  GROUP__DTP = 103                                   )
      PARAMETER (  PHASE__DTP = 104                                   )
      PARAMETER (  MIXED__DTP = 105                                   )
      PARAMETER (  SINGL__DTP = 106                                   )
      PARAMETER (  XBAND__DTP = 201                                   )
      PARAMETER (  SBAND__DTP = 202                                   )
      PARAMETER (   COMB__DTP = 203                                   )
      PARAMETER (  IOCAL__DTP = 204                                   )
      PARAMETER (  FIRST__DTP = GRPRAT__DTP, LAST__DTP = FUSED__DTP   )
!
      INTEGER*2  IDATYP__DEF                    ! Default solution type which
      PARAMETER  ( IDATYP__DEF = GRPRAT__DTP )  ! is set when databse don't
!                                               ! have solution configuration
!                                               ! LCODE
      INTEGER*4    DATYP__LEN
      PARAMETER  ( DATYP__LEN = 6*(LAST__DTP-FIRST__DTP+1) )
!
! --- Abbreviations of solution types
!
      CHARACTER    DATYP__ABR*(DATYP__LEN)
      PARAMETER  ( DATYP__ABR = 'GRPRAT'// &
     &                          'PHSRAT'// &
     &                          'SNBRAT'// &
     &                          'GRPONL'// &
     &                          'PHSONL'// &
     &                          'SNBONL'// &
     &                          'RATONL'// &
     &                          'G_GXS '// &
     &                          'PX_GXS'// &
     &                          'PS_GXS'// &
     &                          'PX_GX '// &
     &                          'PX_GS '// &
     &                          'PS_GX '// &
     &                          'PS_GS '// &
     &                          'P_PXS '// &
     &                          'GX    '// &
     &                          'GS    '// &
     &                          'PX    '// &
     &                          'PS    '// &
     &                          'SNG_X '// &
     &                          'SNG_S '// &
     &                          'FUSED '   &
     &           )
!
! --- Constants defining "sigmas" of the constraints imposed by NORML, PROC
! --- ADJST
!
      REAL*8      LIN_STA__SIG__DEF,  BAS_CLK__SIG__DEF, &
     &            SRC_COO__SIG__DEF,  NNT_POS__SIG__DEF, &
     &            NNR_POS__SIG__DEF,  NNT_VEL__SIG__DEF, &
     &            NNR_VEL__SIG__DEF,  STA_WEA__SIG__DEF, &
     &            VEL_WEA__SIG__DEF,  VEL_DIR__SIG__DEF, &
     &            VEL_CMP__SIG__DEF,  VEL_SET__SIG__DEF, &
     &            STA_ORG__SIG__DEF,  VEL_ORG__SIG__DEF, &
     &            STA_TIE__SIG__DEF,  VEL_TIE__SIG__DEF, &
     &            RAS_ORG__SIG__DEF,  DCL_ORG__SIG__DEF, &
     &            NNR_SRC__SIG__DEF,  NNR_PRP__SIG__DEF, &
     &            NUT_CMP__SIG__DEF,  VEL_VER__SIG__DEF
!
      PARAMETER ( LIN_STA__SIG__DEF = 1.0D-5 )  ! Linear combination of station positions
      PARAMETER ( BAS_CLK__SIG__DEF = 1.0D-6 )  ! Baseline-dependent clocks
      PARAMETER ( SRC_COO__SIG__DEF = 1.0D-5 )  ! Source coordinates
      PARAMETER ( NNT_POS__SIG__DEF = 1.0D-4 )  ! No-net translation for station positions
      PARAMETER ( NNR_POS__SIG__DEF = 1.0D-4 )  ! No-net rotation station posit.
      PARAMETER ( NNT_VEL__SIG__DEF = 1.0D-4 )  ! No-net translation velocities
      PARAMETER ( NNR_VEL__SIG__DEF = 1.0D-4 )  ! No-net rotation velocities
      PARAMETER ( STA_WEA__SIG__DEF = 1.0D1  )  ! Station positions
      PARAMETER ( VEL_WEA__SIG__DEF = 1.0D-1 )  ! Velocities
      PARAMETER ( VEL_DIR__SIG__DEF = 1.0D-7 )  ! Velocity direction
      PARAMETER ( VEL_CMP__SIG__DEF = 1.0D-7 )  ! Velocity components
      PARAMETER ( STA_ORG__SIG__DEF = 1.0D-5 )  ! Station positions origin
      PARAMETER ( VEL_ORG__SIG__DEF = 1.0D-5 )  ! Velocity origin
      PARAMETER ( VEL_SET__SIG__DEF = 1.0D-5 )  ! Set of velicities
      PARAMETER ( STA_TIE__SIG__DEF = 1.0D-4 )  ! Station ties
      PARAMETER ( VEL_TIE__SIG__DEF = 1.0D-4 )  ! Velocity ties
      PARAMETER ( RAS_ORG__SIG__DEF = 4.8D-12 ) ! Right ascension origin
      PARAMETER ( DCL_ORG__SIG__DEF = 4.8D-12 ) ! Declination origin
      PARAMETER ( NNR_SRC__SIG__DEF = 1.0D-11 ) ! No-net rotation for sources
      PARAMETER ( NNR_PRP__SIG__DEF = 1.0D-11 ) ! No-net rotation for prop.motion
      PARAMETER ( NUT_CMP__SIG__DEF = 1.0D-10 ) ! Nutation components
      PARAMETER ( VEL_VER__SIG__DEF = 1.0D-4 )  ! Vertival velocity
!
      INTEGER*4    M_ACM
      PARAMETER  ( M_ACM = 4 ) ! Max number of station for a priori clock model
!
! --- Constants specifying suppression codes
!
      INTEGER*2     BQCX__SPS, BQCS__SPS, NOFX__SPS, NOFS__SPS, &
     &              CUEL__SPS, DSBS__SPS, DSSO__SPS, BWVR__SPS, &
     &              BPRN__SPS, GION__SPS, GIO1__SPS, GIO2__SPS, &
     &              GIO3__SPS, GIO4__SPS, PION__SPS, PIO1__SPS, &
     &              PIO2__SPS, PIO3__SPS, EXTS__SPS, FURE__SPS, &
     &              XAMB__SPS, SAMB__SPS, WPAS__SPS, IUNW__SPS, &
     &              SET1__SPS, SET2__SPS, GOOD__SPS, CBAD__SPS, &
     &              UNRC__SPS, DECM__SPS, IOUS__SPS, INIT__SPS, &
     &              LSNR__SPS
      PARAMETER  (  BQCX__SPS =  1 ) ! Bad quality code for X-band
      PARAMETER  (  BQCS__SPS =  2 ) ! Bad quality code for S-band
      PARAMETER  (  NOFX__SPS =  3 ) ! No fringes for X-band
      PARAMETER  (  NOFS__SPS =  4 ) ! No fringes for S-band
      PARAMETER  (  CUEL__SPS =  5 ) ! Observation made below elevation cut off limit
      PARAMETER  (  DSBS__SPS =  6 ) ! Observation at deselected baseline
      PARAMETER  (  DSSO__SPS =  7 ) ! Observation of deselected source
      PARAMETER  (  BWVR__SPS =  8 ) ! Bad WVR mask
      PARAMETER  (  BPRN__SPS =  9 ) ! No parangle correction available
      PARAMETER  (  GION__SPS = 10 ) ! GION calibration is not available *compatibility*
      PARAMETER  (  GIO1__SPS = 11 ) ! GION calibration is bad           *compatibility*
      PARAMETER  (  GIO2__SPS = 12 ) ! GION calibration is bad           *compatibility*
      PARAMETER  (  GIO3__SPS = 13 ) ! GION calibration is bad           *compatibility*
      PARAMETER  (  GIO4__SPS = 14 ) ! GION calibration is bad           *compatibility*
      PARAMETER  (  PION__SPS = 15 ) ! PION calibration is not available *compatibility*
      PARAMETER  (  PIO1__SPS = 16 ) ! PION calibration is bad           *compatibility*
      PARAMETER  (  PIO2__SPS = 17 ) ! PION calibration is bad           *compatibility*
      PARAMETER  (  PIO3__SPS = 18 ) ! PION calibration is bad           *compatibility*
      PARAMETER  (  EXTS__SPS = 18 ) ! Suppress by the flagging in the external file
      PARAMETER  (  LSNR__SPS = 19 ) ! Low SNR
      PARAMETER  (  FURE__SPS = 20 ) ! Recoverable for FUSED data type
      PARAMETER  (  XAMB__SPS = 21 ) ! X-band phase ambiguity not resolved
      PARAMETER  (  SAMB__SPS = 22 ) ! S-band phase ambiguity not resolved
      PARAMETER  (  IUNW__SPS = 23 ) ! IUNW code is not zero
      PARAMETER  (  WPAS__SPS = 24 ) ! Wrong phase delay ambiguity spacings
      PARAMETER  (  IOUS__SPS = 25 ) ! Ionospheric calibration is used if available
      PARAMETER  (  DECM__SPS = 26 ) ! Decimation is set on
      PARAMETER  (  INIT__SPS = 27 ) ! The bit field has been initialized
      PARAMETER  (  SET1__SPS = 28 ) ! Circumstnaces bits are set up
      PARAMETER  (  SET2__SPS = 29 ) ! Usage status bits are set up
      PARAMETER  (  GOOD__SPS = 30 ) ! Observation is marked as good
      PARAMETER  (  CBAD__SPS = 31 ) ! Observation is marked as conditionally
!                                    ! bad, but may become recoverable
      PARAMETER  (  UNRC__SPS = 32 ) ! Observation is marked as bad and
!                                    ! may never become recoverable
!
! --- Unquire codes of suppression status
!
      INTEGER*2     USED__SPS, RECO__SPS, URPH__SPS, MAXC__SPS
      PARAMETER  (  USED__SPS  = 129 ) ! Is observation used in solution?
      PARAMETER  (  RECO__SPS  = 130 ) ! Is observation recoverable?
      PARAMETER  (  URPH__SPS  = 131 ) ! Is observation unrecoverable for phase delay solution types?
      PARAMETER  (  MAXC__SPS  =  32 ) ! Total number of used bits in SPS-code
!
! --- Constants specifying user action for suppression
!
      INTEGER*2     GSUP__UAS, GOVV__UAS, PSUP__UAS, POVV__UAS, &
     &              INIT__UAS
      PARAMETER  (  GSUP__UAS = 1  ) ! Suppress good observation (group)
      PARAMETER  (  GOVV__UAS = 2  ) ! Restore bad observation (group)
      PARAMETER  (  PSUP__UAS = 3  ) ! Suppress good observation (phase)
      PARAMETER  (  POVV__UAS = 4  ) ! Restore bad observation (phase)
      PARAMETER  (  INIT__UAS =16  ) ! Flag: UACSUP has been initialized
!
! --- Constatns specifying status of additional parameters from the opposite
! --- band
!
      INTEGER*2    OPP_SET1__BIT, OPP_SET2__BIT, OPP_DUAL__BIT
      PARAMETER  ( OPP_SET1__BIT = 1  ) ! Status additional parameters
      PARAMETER  ( OPP_SET2__BIT = 2  ) ! Status additional parameters
      PARAMETER  ( OPP_DUAL__BIT = 3  ) ! Dual band status
!
! --- Constants specifying suppression strategy codes
!
      INTEGER*2    SUPMET__PRE98, SUPMET__PRE91, &
     &             SUPMET__COMB1, SUPMET__SNGBA, &
     &             SUPMET__META,  SUPMET__UND,   &
     &             SUPMET__DEF,   SUPMET__FIRST, SUPMET__LAST
      PARAMETER  ( SUPMET__PRE98 = 501 ) ! pre-98 method of observ. suppression
      PARAMETER  ( SUPMET__PRE91 = 502 ) ! pre-91 method of observ. suppression
      PARAMETER  ( SUPMET__COMB1 = 503 ) ! combination method of suppression
      PARAMETER  ( SUPMET__SNGBA = 504 ) ! single band method of suppression
      PARAMETER  ( SUPMET__META  = 505 ) ! advanced Meta-Solve strategey
      PARAMETER  ( SUPMET__UND   = -1  ) ! undefined method
      PARAMETER  ( SUPMET__DEF   = SUPMET__PRE98 ) ! Default suppression method
      PARAMETER  ( SUPMET__FIRST = SUPMET__PRE98 ) ! First available supmet_xxx
      PARAMETER  ( SUPMET__LAST  = SUPMET__META ) ! Last  supported supmet_xxx
!
! --- Constants specifying singularity check action
!
      INTEGER*4    SNGCHK_ACT__UNDF,  SNGCHK_ACT__NONE, &
     &             SNGCHK_ACT__WARN,  SNGCHK_ACT__REPR, &
     &             SNGCHK_ACT__STOP,  SNGCHK_ACT__SKIP, &
     &             SNGCHK_ACT__LSAL,  SNGCHK_ACT__LSIN
      PARAMETER  ( SNGCHK_ACT__UNDF = 0 ) ! Indefinfed action
      PARAMETER  ( SNGCHK_ACT__NONE = 1 ) ! Action: do nothing
      PARAMETER  ( SNGCHK_ACT__WARN = 2 ) ! Action: only warning
      PARAMETER  ( SNGCHK_ACT__REPR = 3 ) ! Action: warning and reparameterizat.
      PARAMETER  ( SNGCHK_ACT__STOP = 4 ) ! Action: warning and terminating
      PARAMETER  ( SNGCHK_ACT__SKIP = 5 ) ! Action: warning and skipping session
      PARAMETER  ( SNGCHK_ACT__LSAL = 5 ) ! Last action
      PARAMETER  ( SNGCHK_ACT__LSIN = 4 ) ! Last interactive action
!
! --- Constants specifying singularity check condition code
!
      INTEGER*4    SNGCHK_CMP__UNDF, SNGCHK_CMP__CONT, &
     &             SNGCHK_CMP__BACK, SNGCHK_CMP__STOP, &
     &             SNGCHK_CMP__SKIP, SNGCHK_CMP__FAIL
      PARAMETER  ( SNGCHK_CMP__UNDF = 0 ) ! Undefined condition
      PARAMETER  ( SNGCHK_CMP__CONT = 1 ) ! Condition: continue execution
      PARAMETER  ( SNGCHK_CMP__BACK = 2 ) ! Condition: repeat buiding normal eq.
      PARAMETER  ( SNGCHK_CMP__STOP = 3 ) ! Condition: stop execution
      PARAMETER  ( SNGCHK_CMP__SKIP = 4 ) ! Condition: bypass this session
      PARAMETER  ( SNGCHK_CMP__FAIL = 5 ) ! Condition: internal failure
!
! --- Constants specifying actions for displaying status of the processing
!
      INTEGER*4  STA__SHO, STA__NON, STA__BEG, STA__END, STA__INI, &
     &           STA__INT, STA__SUC
      PARAMETER  ( STA__SHO = 601 ) ! Status: show a line for monitoring
      PARAMETER  ( STA__NON = 602 ) ! Status: not to send a line to monitor
      PARAMETER  ( STA__BEG = 603 ) ! Status: begining
      PARAMETER  ( STA__END = 604 ) ! Status: end
      PARAMETER  ( STA__INI = 605 ) ! Status: initializing
      PARAMETER  ( STA__INT = 606 ) ! Status: interactive
      PARAMETER  ( STA__SUC = 607 ) ! Status: successfull completion
!
! --- Constants specifying mode of input/output of constraints
!
      INTEGER*4    CNI__LOC, CNI__GLO, CNI__MIX, CNI__UND, &
     &             CNI__ULC, CNI__UGL, CNI__ACI, CNI__DIR
      PARAMETER  ( CNI__LOC = 1601 ) ! Local  (file CSPRxx)
      PARAMETER  ( CNI__GLO = 1602 ) ! Global (file CSPGxx)
      PARAMETER  ( CNI__MIX = 1603 ) ! Mixed constraints
      PARAMETER  ( CNI__UND = 1604 ) ! Undefined constraints
      PARAMETER  ( CNI__ULC = 1605 ) ! Local  user constraints
      PARAMETER  ( CNI__UGL = 1606 ) ! Global user constraints
      PARAMETER  ( CNI__ACI = 1701 ) ! Add to the table of constraints
      PARAMETER  ( CNI__DIR = 1702 ) ! Direct applying constraints
!
! --- Constants specifying mode of correlations computations
!
      INTEGER*4    CRL__UND, CRL__ASC, CRL__BIN
      PARAMETER  ( CRL__UND =   0 ) ! Undefined
      PARAMETER  ( CRL__ASC = 901 ) ! ASCII format
      PARAMETER  ( CRL__BIN = 902 ) ! Binary format
!
      REAL*8       TAU_ERR__BAD, TAU_ERR__TINY
      PARAMETER  ( TAU_ERR__BAD  = 1.D-8  ) ! Bogus tau_err ( in sec )
      PARAMETER  ( TAU_ERR__TINY = 1.D-14 ) ! Bogus tau_err ( in sec )
!
      INTEGER*2    M_CLS, M_CLB, M_CLZ, M_CLM, MM_CLM
      PARAMETER  ( M_CLS = MAX_CAL  ) ! Number of used station-dependent calibrations
      PARAMETER  ( M_CLB = MAX_CONT ) ! Number of used observation-dependant calibr
      PARAMETER  ( M_CLZ = MAX_CLZ  ) ! Number of used zenith calibrations
      PARAMETER  ( M_CLM = MAX_CLM ) ! Number of used mode calibrations
      PARAMETER  ( MM_CLM= 15 ) ! Potential maximal number of mode calibrations
!
      INTEGER*4    MCL__GRX, MCL__PHX, MCL__RTX, &
     &             MCL__GRS, MCL__PHS, MCL__RTS
      PARAMETER  ( MCL__GRX = 1 )  !  Group delay X-band
      PARAMETER  ( MCL__PHX = 2 )  !  Phase delay X-band
      PARAMETER  ( MCL__RTX = 3 )  !  Delay rate  X-band
      PARAMETER  ( MCL__GRS = 4 )  !  Group delay S-band
      PARAMETER  ( MCL__PHS = 5 )  !  Phase delay S-band
      PARAMETER  ( MCL__RTS = 6 )  !  Delay rate  S-band
!
! --- Constants for EQUMEM
!
      INTEGER*4  EQM__UND, EQM__INI, EQM__DON
      PARAMETER  ( EQM__UND = -789 )
      PARAMETER  ( EQM__INI =  601 )
      PARAMETER  ( EQM__DON =  602 )
!
! --- Phase cal status
!
      INTEGER*2    PHC__UND, PHC__MSR, PHC__OFF, PHC__MAN, PHC__MIX
      PARAMETER  ( PHC__UND = 1601 ) ! Undefined phase calibration status
      PARAMETER  ( PHC__MSR = 1    ) ! Measured phase-cal has been applied
      PARAMETER  ( PHC__OFF = 2    ) ! Measured phase-cal + offset applied
      PARAMETER  ( PHC__MAN = 3    ) ! Manual phase calibration has been applied
      PARAMETER  ( PHC__MIX = 9    ) ! Mixed phase-cal: part measured, part manual
!
      CHARACTER*8  UTC__TS, UT1__TS, UT1R__TS, UT1S__TS,  TDB__TS, TDT__TS, &
     &             TAI__TS, TCG__TS, TCB__TS,  UNDEF__TS, NA__TS
      PARAMETER  ( UTC__TS   = 'UTC     ' )
      PARAMETER  ( UT1__TS   = 'UT1     ' )
      PARAMETER  ( UT1R__TS  = 'UT1R    ' )
      PARAMETER  ( UT1S__TS  = 'UT1S    ' )
      PARAMETER  ( TDB__TS   = 'TDB     ' )
      PARAMETER  ( TDT__TS   = 'TDT     ' )
      PARAMETER  ( TAI__TS   = 'TAI     ' )
      PARAMETER  ( TCG__TS   = 'TCG     ' )
      PARAMETER  ( TCB__TS   = 'TCB     ' )
      PARAMETER  ( UNDEF__TS = 'UNDEF   ' )
      PARAMETER  ( NA__TS    = 'N/A     ' )
!
! --- Definition of metric names
!
      CHARACTER  IERS92__MET*8, GRS__MET*8, TOPOCNTR__MET*8, NONE__MET*8
      PARAMETER  ( IERS92__MET   = 'IERS92  ' )
      PARAMETER  ( GRS__MET      = 'GRS     ' )
      PARAMETER  ( TOPOCNTR__MET = 'TOPOCNTR' )
      PARAMETER  ( NONE__MET     = 'NONE    ' )
!
! --- Definition of user partials bookeeping
!
      INTEGER*4  UPT__UND, UPT__FUL, UPT__DEL, UPT__CMP
      PARAMETER  ( UPT__UND = 1701 )  ! Undefined
      PARAMETER  ( UPT__FUL = 1702 )  ! Full
      PARAMETER  ( UPT__DEL = 1703 )  ! Delay only
      PARAMETER  ( UPT__CMP = 1704 )  ! Compressed
!
! --- Maximum and minimum sky frequencies for ionsphere contribution
! --- computation
!
      REAL*8     MAX__FRQ, MIN__FRQ, XS__FRQ
      PARAMETER  ( MIN__FRQ = 200.0D0   ) ! Minimal sky frequency (MHz)
      PARAMETER  ( MAX__FRQ = 90000.0D0 ) ! Maximal sky frequency (MHz)
      PARAMETER  ( XS__FRQ  = 4000.0D0  ) ! Boundary between X and S frequencies
!
! --- Modes of computing ionosphere frequencies
!
      INTEGER*4    IONFR_MODE__USEAP, IONFR_MODE__NOUSEAP
      PARAMETER  ( IONFR_MODE__USEAP   = 681 )
      PARAMETER  ( IONFR_MODE__NOUSEAP = 682 )
!
      CHARACTER  CNS__LABEL*32
      PARAMETER  ( CNS__LABEL = 'CNS-Solve  Version of 2002.09.25' )
!
      CHARACTER  ECC__LABEL*18
      PARAMETER  ( ECC__LABEL = '# ECC-FORMAT V 1.0' )
!
      CHARACTER  SPOOL__FMT*24
      PARAMETER  ( SPOOL__FMT = 'Revision date 2003.08.20' )
!
      INTEGER*2    CRES__CURRENT, CRES__PRE98, CRES__PRE03
      PARAMETER  ( CRES__CURRENT = 2001 )
      PARAMETER  ( CRES__PRE98   = 2002 )
      PARAMETER  ( CRES__PRE03   = 2003 )
!
      INTEGER*4    SRC_PRE2004_SPOOL__FMT, SRC_SHORT_SPOOL__FMT, &
    &              SRC_LONG_SPOOL__FMT, SRC_POST2021_SPOOL__FMT, &
    &              SRC_POST2024_SPOOL__FMT
      PARAMETER  ( SRC_PRE2004_SPOOL__FMT  = 16001 )
      PARAMETER  ( SRC_SHORT_SPOOL__FMT    = 16002 )
      PARAMETER  ( SRC_LONG_SPOOL__FMT     = 16003 )
      PARAMETER  ( SRC_POST2021_SPOOL__FMT = 16004 )
      PARAMETER  ( SRC_POST2024_SPOOL__FMT = 16005 )
!
      REAL*8     MAX__DELAY
      PARAMETER  ( MAX__DELAY = 0.5D0 ) ! maximum delay in sec
!
      INTEGER*2   MAX_FLYBY_EOP_VALUES
      PARAMETER ( MAX_FLYBY_EOP_VALUES   = 15 )
!
      REAL*8       FREQ__S_DEFAILT
      PARAMETER  ( FREQ__S_DEFAILT = 2.2D9 ) ! Default S-band frequency (Hz)
!
      INTEGER*4    M__HPE, M__SPE, M__SPN, M2__SPN, M__SPD, M__BSP, &
    &              N__COS, N__SIN, M__BSP_INT
      PARAMETER  ( M__HPE  =  16 ) ! max number of harmonic site estim. names
      PARAMETER  ( M__SPE  = 128 ) ! max number of strations for spline pos. est.
      PARAMETER  ( M__SPN  = 128 ) ! max number of epochs for spline pos estim.
      PARAMETER  ( M__BSP  = 128 ) ! max number of statsion with apriori B-spline
      PARAMETER  ( M2__SPN = 3*((M__SPN+2)*(M__SPN+3))/2 ) !
      PARAMETER  ( M__BSP_INT = 8 ) ! Number of knots for in-session BSP interpolation
      PARAMETER  ( M__SPD  =   3 ) ! max degree for spline pos estimation
      PARAMETER  ( N__COS  =   1 ) ! Index of the cosinus part of the harmonic
      PARAMETER  ( N__SIN  =   2 ) ! Index of the   sinus part of the harmonic
!
      INTEGER*4    OBS__2BN, DET__2BN, AVL__2BN, ION__2BN
      PARAMETER  ( OBS__2BN = 0 ) ! Second band was observed
      PARAMETER  ( DET__2BN = 1 ) ! Second band was detected
      PARAMETER  ( AVL__2BN = 2 ) ! Second band information is available
      PARAMETER  ( ION__2BN = 3 ) ! Ionospheric contribution from the 2nd band
!                                 ! is available
!
! --- Data structure for harmonic site position estimation
!
      TYPE HPE__TYPE
          SEQUENCE
          CHARACTER  NAME*8
          INTEGER*4  L_STA
          CHARACTER  C_STA(MAX_STA)*8
          REAL*8     PHASE
          REAL*8     FREQ
	  REAL*8     NNT_CNS_SIGMA
	  REAL*8     NNR_CNS_SIGMA
      END TYPE HPE__TYPE
!
! --- Auxilliary data structure in Solve for keeping track of harmonic site
! --- postition estimates
!
      TYPE      HPESOL__TYPE
          SEQUENCE
	  LOGICAL*4  FL_EST
	  CHARACTER  NAME*8
	  INTEGER*4  IND_EQU(3,2)
          REAL*8     PHASE
          REAL*8     FREQ
      END  TYPE HPESOL__TYPE
!
! --- Data structure for spline site position estimation
!
      TYPE SPE__TYPE
          SEQUENCE
          CHARACTER  STATION*8
          INTEGER*4  DEGREE
	  INTEGER*4  L_NOD
          INTEGER*4  MJD(M__SPN)
          REAL*8     TAI(M__SPN)
          INTEGER*4  MULT(M__SPN)
	  REAL*8     CNS_STA_SIGMA
	  REAL*8     CNS_VEL_SIGMA
          REAL*8     CNS_DER_SIGMA(0:M__SPD)
!
	  INTEGER*4  K_NOD
          REAL*8     TIM(1-M__SPD:M__SPN)
	  LOGICAL*1  USED(1-M__SPD:M__SPN)
      END TYPE SPE__TYPE
!
! --- Auxilliary data structure in Solve for keeping track of spline site
! --- postition estimates
!
      TYPE      SPESOL__TYPE
          SEQUENCE
	  INTEGER*4  IND_STA
	  INTEGER*4  DEGREE
	  INTEGER*4  L_NOD
	  LOGICAL*4  FL_CHECK_OVER  ! Flag: should the the node passover be checked
	  INTEGER*4  CORR_IND       ! Index correction
	  INTEGER*4  IND_NOD
	  INTEGER*4  IND_EQU(1-M__SPD:M__SPN,3)
          REAL*8     NOD_ARR(M__SPN)
	  LOGICAL*1  USED(1-M__SPD:M__SPN)
      END  TYPE SPESOL__TYPE
!
      TYPE      CAL__INFO__TYPE
	  INTEGER*4  CLASS       ! station/baseline
	  INTEGER*4  MODE        ! Delay/phs-grp-rat-band
	  INTEGER*4  RESERVED(2)
      END  TYPE CAL__INFO__TYPE
!
      TYPE      CAL__TYPE
	   CHARACTER  NAME*8
	   TYPE  ( CAL__INFO__TYPE ) :: INFO
	   INTEGER*4  APPLIED(MAX_ARC_STA)
      END  TYPE CAL__TYPE
!
      INTEGER*4    M__CAL
      PARAMETER  ( M__CAL = 64 ) !  maximal number of calibrations
      INTEGER*4  CAL__DEL, CAL__MOD
      PARAMETER  ( CAL__DEL = 901 ) ! Delay calibration
      PARAMETER  ( CAL__MOD = 902 ) ! Mode  calibration
!
      INTEGER*4    M__EHEO
      PARAMETER  ( M__EHEO = 8192 )
      INTEGER*4  HEO__E1E2, HEO__E3, HEO__COS, HEO__SIN
      INTEGER*4  EHC__REAL, EHC__IMAG
      PARAMETER  ( HEO__E1E2 = 1 )
      PARAMETER  ( HEO__E3   = 2 )
      PARAMETER  ( HEO__COS  = 1 )
      PARAMETER  ( HEO__SIN  = 2 )
      PARAMETER  ( EHC__REAL = 1 )
      PARAMETER  ( EHC__IMAG = 2 )
!
      REAL*8       EHEO__FRQ_MIN
      PARAMETER  ( EHEO__FRQ_MIN = 1.D-12 ) ! minimal by module frquency for EHEO
      TYPE       EHEO__TYPE
          SEQUENCE
	  CHARACTER  NAME*16
	  LOGICAL*1  FL_EST(2)
	  LOGICAL*1  FL_EST_VEL(2)
	  LOGICAL*1  FL_CNS(2)
	  LOGICAL*1  FL_CNS_VEL(2)
	  REAL*8     PHAS
	  REAL*8     FREQ
	  REAL*8     ACCL
	  REAL*8     AMPL(2,2)
	  REAL*8     EST(2,2)
	  REAL*8     SIG(2,2)
	  REAL*8     APR(2,2,2)
      END  TYPE  EHEO__TYPE
!
      TYPE       EHEC__TYPE
	  INTEGER*4  IND(2)
	  INTEGER*4  HEO_TYPE
	  REAL*8     AMP(2,2)
	  REAL*8     APR(2,2)
      END  TYPE  EHEC__TYPE
!
      INTEGER*4    EHEO__NUM_CNS
      PARAMETER  ( EHEO__NUM_CNS = 16 ) ! The number of EHEO constraints
      TYPE       EHES__TYPE
	  REAL*8     EHEO_VAL_E1E2_HAR
	  REAL*8     EHEO_VAL_E1E2_CROSS
	  REAL*8     EHEO_VAL_E1E2_SHIFT
	  REAL*8     EHEO_VAL_E1E2_DRIFT
	  REAL*8     EHEO_VAL_E3_HAR
	  REAL*8     EHEO_VAL_E3_CROSS
	  REAL*8     EHEO_VAL_E3_SHIFT
	  REAL*8     EHEO_VAL_E3_DRIFT
!
	  REAL*8     EHEO_ERM_E1E2_HAR
	  REAL*8     EHEO_ERM_E1E2_CROSS
	  REAL*8     EHEO_ERM_E1E2_SHIFT
	  REAL*8     EHEO_ERM_E1E2_DRIFT
	  REAL*8     EHEO_ERM_E3_HAR
	  REAL*8     EHEO_ERM_E3_CROSS
	  REAL*8     EHEO_ERM_E3_SHIFT
	  REAL*8     EHEO_ERM_E3_DRIFT
      END  TYPE  EHES__TYPE
!
      INTEGER*4   EDC__UNDF, EDC__CRE, EDC__USE, EDC__REQ, EDC__BIN, EDC__ASC
      PARAMETER ( EDC__UNDF = 0     )
      PARAMETER ( EDC__CRE = 21001 )
      PARAMETER ( EDC__USE = 21002 )
      PARAMETER ( EDC__REQ = 21003 )
      PARAMETER ( EDC__BIN = 101   )
      PARAMETER ( EDC__ASC = 102   )
!
      INTEGER*2    SOUADM__NO, SOUADM__GLB_ALL, SOUADM__GLB_LIST_NO, &
     &             SOUADM__GLB_LIST_YES, SOUADM__LCL_ALL, &
     &             SOUADM__LCL_LIST_NO, SOUADM__LCL_LIST_YES 
      PARAMETER  ( SOUADM__NO           = 0     ) ! Do not estimate
      PARAMETER  ( SOUADM__GLB_ALL      = 17001 ) ! Estimate globally all
      PARAMETER  ( SOUADM__GLB_LIST_NO  = 17002 ) ! Estimate globally a list except NO
      PARAMETER  ( SOUADM__GLB_LIST_YES = 17003 ) ! Estimate globally a list except YES
      PARAMETER  ( SOUADM__LCL_ALL      = 17004 ) ! Estimate locally  all
      PARAMETER  ( SOUADM__LCL_LIST_NO  = 17005 ) ! Estimate locally  a list except NO
      PARAMETER  ( SOUADM__LCL_LIST_YES = 17006 ) ! Estimate locally  a list except YES
!
      INTEGER*4    SOLVE__YES, SOLVE__NO
      PARAMETER  ( SOLVE__YES = 32001 )
      PARAMETER  ( SOLVE__NO  = 32002 )
      INTEGER*4    SOLVE__RW_EL_MULT_GLOB
      PARAMETER  ( SOLVE__RW_EL_MULT_GLOB = 33001 )
!
      INTEGER*4   TPD__UNDF, TPD__DBS, TPD__IGN, TPD__USE, TPD__UPD
      PARAMETER ( TPD__UNDF =     0 )
      PARAMETER ( TPD__DBS  = 22001 )
      PARAMETER ( TPD__IGN  = 22002 )
      PARAMETER ( TPD__USE  = 22003 )
      PARAMETER ( TPD__UPD  = 22004 )
!
      INTEGER*4   M__TPD_INIT
      PARAMETER ( M__TPD_INIT = 16 )
!
      INTEGER*4    TPD__LABEL_LEN
      PARAMETER  ( TPD__LABEL_LEN = 32 )
      CHARACTER  TPD__LABEL*(TPD__LABEL_LEN)
!?      PARAMETER  ( TPD__LABEL = 'TPD Format version of 2007.11.11' )
!?      PARAMETER  ( TPD__LABEL = 'TPD Format version of 2008.04.21' )
      PARAMETER  ( TPD__LABEL = 'TPD Format version of 2009.06.01' )
      INTEGER*4    TPD__NDER
      PARAMETER  ( TPD__NDER = 25 )
!
      TYPE       TPD_STA__TYPE
	 REAL*8     VSITEC(3)
	 REAL*8     VSITEV(3)
	 REAL*8     NVSITEC(3)
	 REAL*8     VAXOF
	 REAL*8     ECC_TRS(3)
	 CHARACTER  IVS_NAME*8
	 CHARACTER  CDP_NUMBER*4
	 CHARACTER  ECC_TYPE*2
	 CHARACTER  FILLER_1*2
      END TYPE   TPD_STA__TYPE
!
      TYPE       TPD_SOU__TYPE
	 CHARACTER  IVS_NAME*8
	 REAL*8     ALPHA
	 REAL*8     DELTA
      END  TYPE  TPD_SOU__TYPE
!
      TYPE       TPD_HEADER__TYPE
	  CHARACTER   LABEL*(TPD__LABEL_LEN)
	  CHARACTER   VTD_FILE*128
	  INTEGER*4   VTD_UNIX_DATE
	  CHARACTER   DB_NAME*16
	  INTEGER*4   NOBS
	  INTEGER*4   NSTA
	  INTEGER*4   NSOU
	  INTEGER*4   RATE_USE
	  REAL*8      TIME0
	  REAL*8      UTC_M_TAI
	  REAL*8      NUTPSI_AVE
	  REAL*8      NUTEPS_AVE
	  REAL*8      NUTPSI_DIF
	  REAL*8      NUTEPS_DIF
 	  REAL*8      NUT_XY_AVE
      END TYPE   TPD_HEADER__TYPE
!
      TYPE       TPD_PARAM__TYPE
	  REAL*8     AZIM(2)
	  REAL*8     ELEV(2)
          REAL*8     UT1_M_TAI 
          REAL*8     X_POLE    
          REAL*8     Y_POLE    
          REAL*8     UT1_RATE  
          REAL*8     XP_RATE   
          REAL*8     YP_RATE   
      END TYPE   TPD_PARAM__TYPE
!
      TYPE       TPD_DATA__TYPE
	  REAL*8     THEO
	  REAL*8     DER(TPD__NDER)
      END TYPE   TPD_DATA__TYPE
!
      TYPE       TPD__TYPE
	  TYPE   ( TPD_HEADER__TYPE )          :: HEADER
	  TYPE   ( TPD_STA__TYPE    ), POINTER :: STA(:)
	  TYPE   ( TPD_SOU__TYPE    ), POINTER :: SOU(:)
	  TYPE   ( TPD_PARAM__TYPE  ), POINTER :: PARAM(:)
	  TYPE   ( TPD_DATA__TYPE   ), POINTER :: DELAY(:)
	  TYPE   ( TPD_DATA__TYPE   ), POINTER :: RATE(:)
	  CHARACTER  FILE_NAME*128
      END TYPE   TPD__TYPE
!
      INTEGER*4   ATD__UNDF, ATD__NO, ATD__INSTANT, ATD__AVERAGE, &
                  ATD__LAGGED, ATD__USE, ATD__REQ
      PARAMETER ( ATD__UNDF    =     0 )
      PARAMETER ( ATD__NO      = 23000 )
      PARAMETER ( ATD__INSTANT = 23001 )
      PARAMETER ( ATD__AVERAGE = 23002 )
      PARAMETER ( ATD__LAGGED  = 23003 )
      PARAMETER ( ATD__USE     = 23004 )
      PARAMETER ( ATD__REQ     = 23005 )
!
      INTEGER*4   AOC__UNDF, AOC__USE, AOC__REQ
      PARAMETER ( AOC__UNDF =     0 )
      PARAMETER ( AOC__USE  = 24001 )
      PARAMETER ( AOC__REQ  = 24002 )
!
      INTEGER*4  NUT__PSE, NUT__XY
      PARAMETER  ( NUT__PSE  = 18001 ) !  Nutation Psi/Eps
      PARAMETER  ( NUT__XY   = 18002 ) !  Nutation X/Y
      INTEGER*4    RWT_EL_STA__CNB, RWT_SRC__CNB
      PARAMETER  ( RWT_EL_STA__CNB =  9 ) ! Bit of elevation-dep station constraints
      PARAMETER  ( RWT_SRC__CNB    =  9 ) ! Bit of source-dep    constraints
!
! --- Data structure for the tec noise
!
      INTEGER*4  M__TCN
      PARAMETER  ( M__TCN = 512 )
      CHARACTER    TCN__REGR*8, TCN__DEL*8
      PARAMETER  ( TCN__REGR = 'RMS_REGR' )
      PARAMETER  ( TCN__DEL  = 'RMS_DEL ' )
      TYPE     TCN__TYPE
          CHARACTER  MODE*8
          CHARACTER  STA_NAM(2)*8
	  REAL*8     FLOOR
	  REAL*8     SLOPE
	  REAL*8     FREQ
      END TYPE TCN__TYPE
!
      CHARACTER  SCRATCH_DATE*10                 ! The earliest date of compatible
      PARAMETER  ( SCRATCH_DATE = '2023.12.22' ) ! scratch files. Scratch files
!     !  created before this date are INCOMPATIBLE with current version of SOLVE
!
      INTEGER*2  IONOV__UNDF, IONOV__NONE, IONOV__LOADED, IONOV__GEN, IONOV__LOAD, IONOV__USE
      PARAMETER  ( IONOV__UNDF   = 0     )
      PARAMETER  ( IONOV__NONE   = 0     )
      PARAMETER  ( IONOV__LOADED = 7284  )
      PARAMETER  ( IONOV__GEN    = 19471 )
      PARAMETER  ( IONOV__LOAD   = 19472 )
      PARAMETER  ( IONOV__USE    = 19473 )
      REAL*8       ERR_FUDGE_FACTOR__DEF
      PARAMETER  ( ERR_FUDGE_FACTOR__DEF = 1.0 ) ! Default fudging factor for a priori errors
!
! --- Fringe type pararmeters
!
      INTEGER*4    UNDF__FTP, PHS_GRP_HOPS__FTP, PHS_GRP_DTEC_HOPS__FTP, &
     &             PHS_GRP_PIMA__FTP, PHS_GRP_DTEC_PIMA__FTP, PHS_GRP_ACL_PIMA__FTP
      PARAMETER  ( UNDF__FTP              =   0 )
      PARAMETER  ( PHS_GRP_HOPS__FTP      =   1 )
      PARAMETER  ( PHS_GRP_DTEC_HOPS__FTP =   2 )
      PARAMETER  ( PHS_GRP_PIMA__FTP      = 101 )
      PARAMETER  ( PHS_GRP_DTEC_PIMA__FTP = 102 )
      PARAMETER  ( PHS_GRP_ACL_PIMA__FTP  = 103 )
!
      CHARACTER    LABEL__AOC*68, LABEL__IONO*82, LABEL__EDIT*56, LABEL__ADDW*56, LABEL__SCAW*88, &
     &             LABEL__ADDW_V1*56, LABEL__DTEC*58, LABEL__EXT_ERR*52
      PARAMETER  ( LABEL__AOC     = '# External apriori observation file.  Format version of 2021.03.16' )
      PARAMETER  ( LABEL__IONO    = '# Ionospheric path delay from VLBI and GNSS TEC maps. Format version of 2022.12.06' )
      PARAMETER  ( LABEL__EDIT    = '# External suppression flags. Format version of 2020.07.15' )
      PARAMETER  ( LABEL__ADDW    = '# External additive weight. Format version of 2022.09.26'   )
      PARAMETER  ( LABEL__ADDW_V1 = '# External additive weight. Format version of 2020.07.14'   )
      PARAMETER  ( LABEL__SCAW    = '# Unformation about scans, wights, and suppression status. Format version of 2024.11.25 ' )
      PARAMETER  ( LABEL__DTEC    = '# External differential TEC.  Format version of 2022.06.28' )
      PARAMETER  ( LABEL__EXT_ERR = '# External error file.  Format version of 2023.02.09' )
!
      INTEGER*4    DTEC__NONE, DTEC__APPLY, DTEC__IMPORT
      PARAMETER  ( DTEC__NONE   =          0 )
      PARAMETER  ( DTEC__APPLY  = 1927841232 )
      PARAMETER  ( DTEC__IMPORT = 1471023489 )
!
      INTEGER*4    DTD__STS, DTH__STS, DTL__STS, DTHL__STS
      PARAMETER  ( DTD__STS  = 0 ) ! This bit iindicates that the bit field has been set
      PARAMETER  ( DTH__STS  = 1 ) ! The use of the upper band for dTEC
      PARAMETER  ( DTL__STS  = 2 ) ! The use of the lower band for dTEC
      PARAMETER  ( DTHL__STS = 3 ) ! The use of both the lower band the higher band for dTEC
      REAL*8       FREQ__SOLVE_MIN 
      PARAMETER  ( FREQ__SOLVE_MIN = 1.D6 )
!
      INTEGER*2    IOS__UNDF, IOS__SES, IOS__STA, IOS__BAS
      REAL*8       IOS__SIG_DEF
      PARAMETER  ( IOS__UNDF = 0     )
      PARAMETER  ( IOS__SES  = 19234 )
      PARAMETER  ( IOS__STA  = 28947 )
      PARAMETER  ( IOS__BAS  = 31034 )
      PARAMETER  ( IOS__SIG_DEF = 0.1D0 ) 
! 
      REAL*8       SCA_STAT_MAX_GAP
      PARAMETER  ( SCA_STAT_MAX_GAP = 300.0 ) ! Maximum gap within a scan for source statistics
      INTEGER*4    ADDW__UNDF, ADDW__REPL, ADDW__QUAD, ADDW__MIN, ADDW__MAX
      PARAMETER  ( ADDW__UNDF = 0         )
      PARAMETER  ( ADDW__REPL = 430429556 )
      PARAMETER  ( ADDW__QUAD = 582305252 )
      PARAMETER  ( ADDW__MIN  = 968234303 )
      PARAMETER  ( ADDW__MAX  = 832932305 )
!
! <<< This is the end of file solve.i
