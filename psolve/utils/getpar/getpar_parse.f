      SUBROUTINE GETPAR_PARSE ( FILSPL, &
     &           M_SES, M_SOU, M_STA, M_COMP, M_LSO, M_LST, M_BAS, M_TRP, &
     &           M_TRS, M_ERM, M_APR, MA_STA, MA_BAS, M_CNS, &
     &           NUT_USAGE, C_SOU, C_PRP, J_SOU, J_PRP, IND_SOU, IND_PRP,  &
     &           RA_VAL, RA_ERR, DL_VAL, DL_ERR, RAP_VAL, RAP_ERR, DLP_VAL, DLP_ERR, &
     &           OBU_SOU, OBT_SOU, SEU_SOU, SET_SOU, DAF_SOU, DAL_SOU, &
     &           LSO_NAME, LJO_NAME, LRA_VAL, LRA_ERR, LDL_VAL, LDL_ERR, LCR_VAL, &
     &           USO_VAL, TSO_VAL, &
     &           USC_LSO, TSC_LSO, &
     &           USC_SOU, TSC_SOU, &
     &           LST_NAME, L_ERM, &
     &           CL_VAL, CL_ERR, C_COO, CSTA_SRT, C_CRL, S_CRL, P_CRL, &
     &           C_VAL, C_ERR, C_VEL, CVEL_SRT, V_VAL, V_ERR, C_BAS, C_ERM, &
     &           OBU_STA, OBT_STA, SEU_STA, SET_STA, DAF_STA, DAL_STA, &
     &           XEOP_VAL, XEOP_ERR, XREOP_VAL, XREOP_ERR, &
     &           YEOP_VAL, YEOP_ERR, YREOP_VAL, YREOP_ERR, &
     &           UEOP_VAL, UEOP_ERR, &
     &           REOP_VAL, REOP_ERR, &
     &           QEOP_VAL, QEOP_ERR, &
     &           PEOP_VAL, PEOP_ERR, &
     &           EEOP_VAL, EEOP_ERR, &
     &           CEOP, &
     &           M_HEO, L_HEO, C_HEO, &
     &           M_NPV, L_NPV, C_NPV, &
     &           RMS_STR, RMS_VAL, RMS_IND, RMS_GLO_STR, &
     &           DBNAME, EXPNAME, USED, START, DURA, TAG, EPOCH, MJD_EOP, MJD_NUT, &
     &           LSO_SESIND, LST_SESIND, &
     &           N_LIN, N_SES, N_LSO, N_LST, L_SOU, L_PRP, L_COO, L_VEL, L_BAS, &
     &           L_TRP, IEXP_TRP, CSTA_TRP, MJD_TRP, ZEN_TRP, &
     &           ADJ_TRP, ERR_TRP, L_TRS, C_TRS, L_APR, C_APR, &
     &           SOL_ID, SOL_DATE, N_BAS, CN_BAS, KR_BAS, KU_BAS, C_NET, &
     &           L_CNS, C_CNS, MJD_STA_REF, TAI_STA_REF, &
     &           MJD_SOU_REF, TAI_SOU_REF, SOU_MID_EPOCH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GETPAR_PARSE  parses spool file of VLBI solution and      *
! *   extracts various information related to solution. GETPAR_PARSE     *
! *   can work with single-database, multi-database, independent or      *
! *   global (complete or back) solutions. Output arrays are in          *
! *   character from as they were read from the spool file.              *
! *                                                                      *
! *   NB: the output arrays contain a mixture of NON-STANDARD units.     *
! *   Pay special attention on units of each individual item.            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      FILSPL ( CHARACTER ) -- Filename with spool file from Solve.    *
! *       M_SES ( INTEGER*4 ) -- Maximal number of sessions which spool  *
! *                              file may contain.                       *
! *       M_SOU ( INTEGER*4 ) -- Maximal number of observed sources      *
! *                              (used or not used in solution).         *
! *       M_STA ( INTEGER*4 ) -- Maximal number of participated stations *
! *                              (used or not used in solution).         *
! *      M_COMP ( INTEGER*4 ) -- Maximal number of components for        *
! *                              station positions and velocities.       *
! *                              Must be 6.                              *
! *       M_LSO ( INTEGER*4 ) -- Maximal number of records for source    *
! *                              positions estimated as local parameters *
! *                              for each session.                       *
! *       M_LST ( INTEGER*4 ) -- Maximal number of records for station   *
! *                              positions estimated as local parameters *
! *                              for each session.                       *
! *       M_BAS ( INTEGER*4 ) -- Maximal number of the session-dependent *
! *                              estimates of baseline lengths for all   *
! *                              baselines and all sessions. Recommended *
! *                              values is M_SES*<average_number_of_     *
! *                              baselines_in_the_session>.              *
! *       M_TRP ( INTEGER*4 ) -- Maximal number of troposphere path      *
! *                              delay parameters.                       *
! *       M_TRS ( INTEGER*4 ) -- Maximal number of lines with            *
! *                              troposphere path delay statstics.       *
! *       M_ERM ( INTEGER*4 ) -- Maximal number of the records of the    *
! *                              Earth rotation model listing.           *
! *       M_APR ( INTEGER*4 ) -- Maximal number of lines for information *
! *                              about apriori models.                   *
! *      MA_STA ( INTEGER*4 ) -- Maximal number of stations in one       *
! *                              experiment.                             *
! *      MA_BAS ( INTEGER*4 ) -- Maximal number of baselines in one      *
! *                              experiment.                             *
! *       M_CNS ( INTEGER*4 ) -- Maximal number of lines with constraint *
! *                              information.                            *
! *   NUT_USAGE ( INTEGER*4 ) -- Code of the nutation offset usage:      *
! *                           1 -- nutation offsets wrt apriori nutation *
! *                           2 -- nutation offsets wrt IAU1980 series.  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *       C_SOU ( CHARACTER ) -- List of B-names of global sources which *
! *                              coordinates estimated as global         *
! *                              parameters. The list is sorted in the   *
! *                              order of appearance of source names in  *
! *                              the spool file.                         *
! *       C_PRP ( CHARACTER ) -- List of source which proper motions     *
! *                              have been estimated. The list is sorted *
! *                              in the order of appearance of source    *
! *                              names in the spool file.                *
! *       J_SOU ( CHARACTER ) -- List of J-names of global sources which *
! *                              coordinates estimated as global         *
! *                              parameters. The list is sorted in the   *
! *                              order of appearance of source names in  *
! *                              the spool file.                         *
! *       J_PRP ( CHARACTER ) -- List of J-names of the source which     *
! *                              proper motions have been estimated.     *
! *                              The list is sorted in the order of      *
! *                              appearance of source names in the spool *
! *                              file.                                   *
! *     IND_SOU ( INTEGER*4 ) -- Array of indexes of global source       *
! *                              names sorted in increasing their        *
! *                              right ascensions.                       *
! *     IND_PRP ( INTEGER*4 ) -- Array of indexes of the source names    *
! *                              which proper motions have been etimated *
! *                              sorted in increasing their right        *
! *                              ascensions.                             *
! *      RA_VAL ( CHARACTER ) -- Array of the estimates of right         *
! *                              ascensions of global sources. Format:   *
! *                              hh_mm_ss.ffffffff where hh -- hours,    *
! *                              mm -- minutes, ss -- seconds and        *
! *                              ffffffff -- fractions of seconds.       *
! *      RA_ERR ( CHARACTER ) -- Array of the estimates of formal        *
! *                              uncertainties of right ascensions of    *
! *                              the global sources. Format: F10.4,      *
! *                              Units: mas.                             *
! *      DL_VAL ( CHARACTER ) -- Array of the estimates of declinations  *
! *                              of global sources. Format:              *
! *                              ddd_mm_ss.fffffff where dd -- degrees,  *
! *                              mm -- minutes, ss -- seconds and        *
! *                              fffffff -- fractions of seconds.        *
! *      DL_ERR ( CHARACTER ) -- Array of the estimates of formal        *
! *                              uncertainties declination of the global *
! *                              sources. Format: F10.4, Units: mas.     *
! *     RAP_VAL ( CHARACTER ) -- Array of the estimates of right         *
! *                              ascensions rate of changes. Format:     *
! *                              F10.4, units: mas/yr.                   *
! *     RAP_ERR ( CHARACTER ) -- Array of the estimates of formal        *
! *                              uncertainties of right ascension rates  *
! *                              of change. Format: F10.4, Units: mas/yr *
! *     DLP_VAL ( CHARACTER ) -- Array of the estimates of declination   *
! *                              rates. Format: F10.4, units: mas/yr.    *
! *     DLP_ERR ( CHARACTER ) -- Array of the estimates of formal        *
! *                              uncertainties declination rates.        *
! *                              Format: F10.4, Units: mas/yr.           *
! *     OBU_SOU ( CHARACTER ) -- Array of the numbers of used            *
! *                              observations of the sources which       *
! *                              positions were estimated as global      *
! *                              parameters.                             *
! *     OBT_SOU ( CHARACTER ) -- Array of the total numbers of           *
! *                              observations of the sources which       *
! *                              positions were estimated as global      *
! *                              parameters.                             *
! *     SEU_SOU ( CHARACTER ) -- Array of the numbers of sessions in     *
! *                              which the source which positions were   *
! *                              estimated as global parameters has at   *
! *                              least one used observation.             *
! *     SET_SOU ( CHARACTER ) -- Array of the total numbers of sessions  *
! *                              in which the source which positions     *
! *                              were estimated as global parameters     *
! *                              participated (but may be not detected). *
! *     DAF_SOU ( CHARACTER ) -- Array of dates of the first used        *
! *                              observation of the sources which        *
! *                              positions were estimated as global      *
! *                              parameters.                             *
! *     DAL_SOU ( CHARACTER ) -- Array of dates of the last used         *
! *                              observation of the sources which        *
! *                              positions were estimated as global      *
! *                              parameters.                             *
! *    LSO_NAME ( CHARACTER ) -- List of local sources -- sources        *
! *                              with positions estimated as local       *
! *                              parameters. List is sorted in the order *
! *                              of appearance of source names in the    *
! *                              spool file.                             *
! *     LRA_VAL ( CHARACTER ) -- Array of the estimates of right         *
! *                              ascensions of local sources. Format:    *
! *                              hh_mm_ss.ffffffff where hh -- hours,    *
! *                              mm -- minutes, ss -- seconds and        *
! *                              ffffffff -- fractions of seconds.       *
! *     LRA_ERR ( CHARACTER ) -- Array of the estimates of formal        *
! *                              uncertainties of right ascensions of    *
! *                              the local sources. Format: F10.4,       *
! *                              Units: mas.                             *
! *     LDL_VAL ( CHARACTER ) -- Array of the estimates of declinations  *
! *                              of local sources. Format:               *
! *                              ddd_mm_ss.fffffff where dd -- degrees,  *
! *                              mm -- minutes, ss -- seconds and        *
! *                              fffffff -- fractions of seconds.        *
! *     LDL_ERR ( CHARACTER ) -- Array of the estimates of formal        *
! *                              uncertainties declination of the local  *
! *                              sources. Format: F10.4, Units: mas.     *
! *     LCR_VAL ( CHARACTER ) -- Array of the estimates of correlations  *
! *                              between right ascension and declination *
! *                              of local sources. Format: F7.4          *
! *     USO_VAL ( CHARACTER ) -- Array which contains the number of used *
! *                              observations of this particular source  *
! *                              in this particular session when its     *
! *                              coordinates were estimated as global    *
! *                              parameters. Dimension: N_LSO.           *
! *     TSO_VAL ( CHARACTER ) -- Array which contains the total number   *
! *                              of observations of this particular      *
! *                              source in this particular session when  *
! *                              its coordinates were estimated as       *
! *                              global parameters. Dimension: N_LSO.    *
! *     USC_LSO ( CHARACTER ) -- The number of used scans per source.    *
! *     TSC_LSO ( CHARACTER ) -- The total  of used scans per source.    *
! *     USC_SOU ( CHARACTER ) -- The number of used scans per source.    *
! *     TSC_SOU ( CHARACTER ) -- The total  of used scans per source.    *
! *    LST_NAME ( CHARACTER ) -- List of local station names -- stations *
! *                              with positions estimated as local       *
! *                              parameters. List is sorted in the order *
! *                              of appearance of station names in the   *
! *                              spool file.                             *
! *       L_ERM ( INTEGER*4 ) -- The number of records in the ERM        *
! *                              text array.                             *
! *      CL_VAL ( CHARACTER ) -- Array of the estimates of positions     *
! *                              of local stations. Dimension:           *
! *                              M_COMP,M_LST. Order of components:      *
! *                              X, Y, Z, U(p), E(ast), N(orth).         *
! *                              Format: F14.2 . Units: mm.              *
! *      CL_ERR ( CHARACTER ) -- Array of the estimates of formal        *
! *                              uncertainties of positions of local     *
! *                              stations. Dimension: M_COMP,M_LST.      *
! *                              Order of components: X, Y, Z,           *
! *                              U(p), E(ast), N(orth). Format: F10.3 .  *
! *                              Units: mm.                              *
! *       C_COO ( CHARACTER ) -- List of global station names - stations *
! *                              with positions estimated as global      *
! *                              parameters. List is sorted in the order *
! *                              of appearance of station names in the   *
! *                              spool file. Station name has length     *
! *                              15 symbols: 8 -character station        *
! *                              acronym, blank and a 6-letter epoch of  *
! *                              the station with episodic motion in     *
! *                              format YYMMDD. This field is blank for  *
! *                              stations without modeling episodic      *
! *                              motion.                                 *
! *    CSTA_SRT ( CHARACTER ) -- List of global station names - stations *
! *                              with positions estimated as global      *
! *                              parameters. List is sorted in the       *
! *                              alphabetic order of station names.      *
! *                              of appearance of station names in the   *
! *                              spool file. Station name has length     *
! *                              20 symbols: 8 -character station        *
! *                              acronym, blank and a 6-letter epoch of  *
! *                              the station with episodic motion in     *
! *                              format YYMMDD (This field is blank for  *
! *                              stations without modeling episodic      *
! *                              motion) and 5-character index of this   *
! *                              station name in the list C_COO.         *
! *       C_CRL ( CHARACTER ) -- Array which contains correlation        *
! *                              matrices in upper triangle              *
! *                              representation between the estimates:   *
! *                              X-pos, Y-pos, Z-pos, X-vel, Y-vel, Z-vel*
! *                              of global parameters. Dimension:        *
! *                              15,M_STA. Format: F5.3                  *
! *       S_CRL ( CHARACTER ) -- Array of correlations between the       *
! *                              estimates of right ascensions and       *
! *                              declinations of global sources.         *
! *                              Dimension: M_SOU. Format: F6.4          *
! *       P_CRL ( CHARACTER ) -- Array of correlations between the       *
! *                              estimates of proper motion in right     *
! *                              ascension and proper motion in          *
! *                              declination. Dimension: M_SOU.          *
! *                              Format: F6.4                            *
! *       C_VAL ( CHARACTER ) -- Array of the estimates of positions     *
! *                              of global stations. Dimension:          *
! *                              M_COMP,M_STA. Order of components:      *
! *                              X, Y, Z, U(p), E(ast), N(orth).         *
! *                              Format: F14.2 . Units: mm.              *
! *       C_ERR ( CHARACTER ) -- Array of the estimates of formal        *
! *                              uncertainties of positions of global    *
! *                              stations. Dimension: M_COMP,M_STA.      *
! *                              Order of components: X, Y, Z,           *
! *                              U(p), E(ast), N(orth). Format: F10.3 .  *
! *                              Units: mm.                              *
! *       C_VEL ( CHARACTER ) -- List of global station names whose      *
! *                              velocities were estimated. List is      *
! *                              sorted in the order of appearance of    *
! *                              station names in the spool file.        *
! *                              Station name are the 8-character        *
! *                              station acronyms.                       *
! *    CVEL_SRT ( CHARACTER ) -- List of global station names whose      *
! *                              velocities were estimated. List is      *
! *                              sorted in alphabetic order. These       *
! *                              station names have 13 symbols length:   *
! *                              the 8-character are station acronym and *
! *                              a 5-character index of this station     *
! *                              name in the list C_VEL.                 *
! *       V_VAL ( CHARACTER ) -- Array of the estimates of velocities    *
! *                              of global stations. Dimension:          *
! *                              M_COMP,M_STA. Order of components:      *
! *                              X, Y, Z, U(p), E(ast), N(orth).         *
! *                              Format: F8.2 . Units: mm/year.          *
! *       V_ERR ( CHARACTER ) -- Array of the estimates of formal        *
! *                              uncertainties of velocities of global   *
! *                              stations. Dimension: M_COMP,M_STA.      *
! *                              Order of components: X, Y, Z,           *
! *                              U(p), E(ast), N(orth). Format: F8.3 .   *
! *                              Units: mm/year.                         *
! *       C_BAS ( CHARACTER ) -- Text array of the listing of the        *
! *                              session-dependent baseline vector       *
! *                              estimates. Each line of the array       *
! *                              corresponds to the estimates of one     *
! *                              baseline vector for one session. Format:*
! *                              C_BAS(1:10) -- database name with       *
! *                                leading dollar-character;             *
! *                              C_BAS(13:15) -- database version;       *
! *                              C_BAS(25:34) -- data of the estimates   *
! *                                in years as YYYY.yyyyy, format F10.5; *
! *                              C_BAS(36:43) -- name of the first       *
! *                                station of the baseline;              *
! *                              C_BAS(45:52) -- name of the second      *
! *                                station of the baseline;              *
! *                              C_BAS(54:67) -- baseline length in mm,  *
! *                                format F14.2;                         *
! *                              C_BAS(68:73) -- formal uncertainty of   *
! *                                the baseline length in mm,            *
! *                                format F6.2;                          *
! *                              C_BAS(75:84) -- transversal component   *
! *                                of the baseline vector in mm,         *
! *                                format F10.2;                         *
! *                              C_BAS(85:92) -- formal uncertainty of   *
! *                                the transversal component of the      *
! *                                baseline vector in mm, format F8.2;   *
! *                              C_BAS(95:104) -- normal component       *
! *                                of the baseline vector in mm,         *
! *                                format F10.2;                         *
! *                              C_BAS(105:112) -- formal uncertainty of *
! *                                the transversal component of the      *
! *                                baseline vector in mm, format F8.2 .  *
! *       C_ERM ( CHARACTER ) -- Text array of the listing of the        *
! *                              parameter adjustments and elements of   *
! *                              the correlation matrix related to the   *
! *                              Earth's rotation model. Dimension:      *
! *                              M_ERM.                                  *
! *     OBU_STA ( CHARACTER ) -- Array of the numbers of used            *
! *                              observations of the stations which      *
! *                              positions were estimated as global      *
! *                              parameters.                             *
! *     OBT_STA ( CHARACTER ) -- Array of the total numbers of           *
! *                              observations of the stations which      *
! *                              positions were estimated as global      *
! *                              parameters.                             *
! *     SEU_STA ( CHARACTER ) -- Array of the numbers of sessions in     *
! *                              which the stations which positions were *
! *                              estimated as global parameters has at   *
! *                              least one used observation.             *
! *     SET_STA ( CHARACTER ) -- Array of the total numbers of sessions  *
! *                              in which the stations which positions   *
! *                              were estimated as global parameters     *
! *                              participated.                           *
! *     DAF_STA ( CHARACTER ) -- Array of dates of the first used        *
! *                              observation of the stations which       *
! *                              positions were estimated as global      *
! *                              parameters.                             *
! *     DAL_STA ( CHARACTER ) -- Array of dates of the last used         *
! *                              observation of the stations which       *
! *                              positions were estimated as global      *
! *                              parameters.                             *
! *    XEOP_VAL ( CHARACTER ) -- Array of the estimates of X pole        *
! *                              coordinates. Units: mas. Format: F11.4  *
! *    XEOP_ERR ( CHARACTER ) -- Array of the formal uncertainties of    *
! *                              the estimates of X pole coordinates.    *
! *                              Units: muas. Format: F10.2              *
! *   XREOP_VAL ( CHARACTER ) -- Array of the estimates of the rate of   *
! *                              X pole coordinates. Units: mas/day.     *
! *                              Format: F11.4                           *
! *   XREOP_ERR ( CHARACTER ) -- Array of the formal uncertainties of    *
! *                              the estimates of X pole coordinates     *
! *                              rate. Units: muas. Format: F10.2        *
! *    YEOP_VAL ( CHARACTER ) -- Array of the estimates of Y pole        *
! *                              coordinates. Units: mas. Format: F11.4  *
! *    YEOP_ERR ( CHARACTER ) -- Array of the formal uncertainties of    *
! *                              the estimates of Y pole coordinates.    *
! *                              Units: muas. Format: F10.2              *
! *   YREOP_VAL ( CHARACTER ) -- Array of the estimates of the rate of   *
! *                              Y pole coordinates. Units: mas/day.     *
! *                              Format: F11.4                           *
! *   YREOP_ERR ( CHARACTER ) -- Array of the formal uncertainties of    *
! *                              the estimates of Y pole coordinates     *
! *                              rate. Units: muas. Format: F10.2        *
! *    UEOP_VAL ( CHARACTER ) -- Array of the estimates of the UT1 angle *
! *                              for each session. Units: msec.          *
! *                              Format: F11.4                           *
! *    UEOP_ERR ( CHARACTER ) -- Array of the formal uncertainties of    *
! *                              the estimates of UT1. Units: musec.     *
! *                              Format F10.2 .                          *
! *    REOP_VAL ( CHARACTER ) -- Array of the estimates of the first     *
! *                              time derivative of UT1 angle for each   *
! *                              session. Units: msec/day.               *
! *                              Format: F11.4                           *
! *    REOP_ERR ( CHARACTER ) -- Array of the formal uncertainties of    *
! *                              the estimates of the first time         *
! *                              derivatives of UT1. Units: musec/day.   *
! *                              Format F10.2 .                          *
! *    QEOP_VAL ( CHARACTER ) -- Array of the estimates of the second    *
! *                              time derivative of UT1 angle for each   *
! *                              session. Units: msec/day**2.            *
! *                              Format: F11.4                           *
! *    QEOP_ERR ( CHARACTER ) -- Array of the formal uncertainties of    *
! *                              the estimates of the second time        *
! *                              derivatives of UT1. Units: musec/day**2 *
! *                              Format: F10.2                           *
! *    PEOP_VAL ( CHARACTER ) -- Array of the estimates of daily nutation*
! *                              in longitude offsets with respect to    *
! *                              IAU 1980 nutation model. Units: mas.    *
! *                              Format: F11.3                           *
! *    PEOP_ERR ( CHARACTER ) -- Array of the formal uncertainties of    *
! *                              the estimates of daily nutation in      *
! *                              longitude offsets with respect to the   *
! *                              IAU 1980 nutation model. Units: muas.   *
! *                              Format: F10.2                           *
! *    EEOP_VAL ( CHARACTER ) -- Array of the estimates of daily nutation*
! *                              in obliquity offsets with respect to    *
! *                              IAU 1980 nutation model. Units: mas.    *
! *                              Format: F11.3                           *
! *    EEOP_ERR ( CHARACTER ) -- Array of the formal uncertainties of    *
! *                              the estimates of daily nutation in      *
! *                              obliquity offsets with respect to the   *
! *                              IAU 1980 nutation model. Units: muas.   *
! *                              Format: F10.2                           *
! *        CEOP ( CHARACTER ) -- Array of the correlations between 8 EOP:*
! *                              X_pole, X'_pole, Y_pole, Y'_pole, Ut1,  *
! *                              Ut1', Psi, Epsilon for each session.    *
! *                              Correlation matrix is in the upper      *
! *                              triangle form. Dimension: 28,M_SES      *
! *                              Format: F6.4                            *
! *       M_HEO ( INTEGER*4 ) -- The maximal number of lines for output  *
! *                              of the Harmonic Earth Orientation       *
! *                              parameters.                             *
! *       L_HEO ( INTEGER*4 ) -- The actual number of lines in the       *
! *                              output of the Harmonic Earth            *
! *                              Orientation parameters.                 *
! *       C_HEO ( CHARACTER ) -- The text array of output of the         *
! *                              Harmonic Earth Orientation parameters   *
! *                              in HEO format. Dimension: L_HEO.        *
! *       M_NPV ( INTEGER*4 ) -- The maximal number of lines for output  *
! *                              of the non-linear site position         *
! *                              variations.                             *
! *       L_NPV ( INTEGER*4 ) -- The actual number of lines in the       *
! *                              of the non-linear site position         *
! *                              variations.                             *
! *       C_NPV ( CHARACTER ) -- The text array of output of the         *
! *                              non-linear site position variations,    *
! *                              either harmonic site position           *
! *                              variations or B-spline.                 *
! *     RMS_STR ( CHARACTER ) -- Array of the weighted root mean squares *
! *                              of residuals for each session.          *
! *                              RMS_STR(11:20) -- database name with    *
! *                                leading dollar-character;             *
! *                              RMS_STR(12:18) -- Number of             *
! *                                observations used in solution.        *
! *                                Format I7                             *
! *                              RMS_STR(20:32) -- wrms of delay in psec *
! *                                Format: F13.3 .                       *
! *     RMS_VAL ( REAL*8    ) -- Array of the weighted root mean squares *
! *                              of post-fit residuals in delay          *
! *                              (in psec).                              *
! *     RMS_IND ( REAL*8    ) -- Index of the arrays RMS_STR or RMS_VAL  *
! *                              sorted by decreasing postfit residuals. *
! * RMS_GLO_STR ( CHARACTER ) -- Line of the total weighted root mean    *
! *                              square of residuals (empty for          *
! *                              independent solutions) over all         *
! *                              observations. Format:                   *
! *                              RMS_GLO_STR(21:28) -- Number of         *
! *                                observations used in solution.        *
! *                                Format I7                             *
! *                              RMS_GLO_STR(31:45) -- wrms of delay in  *
! *                                psec. Format F15.3                    *
! *                              RMS_GLO_STR(47:59) -- wrms of delay     *
! *                                rate in psec/sec. Format F13.3 .      *
! *      DBNAME ( CHARACTER ) -- Array of database names for each        *
! *                              experiment. Database name includes      *
! *                              leading dollar-symbol. Dimension: M_SES.*
! *     EXPNAME ( CHARACTER ) -- Array of experiment names for each      *
! *                              experiment. Database name includes      *
! *                              leading dollar-symbol. Dimension: M_SES.*
! *        USED ( CHARACTER ) -- Array of number of used observations    *
! *                              for each session. Dimension: M_SES.     *
! *       START ( CHARACTER ) -- Array of the actual start time tag of   *
! *                              the experiment. Units: years from the   *
! *                              0-th year. Format: F10.5 .              *
! *                              Dimension: M_SES.                       *
! *        DURA ( CHARACTER ) -- Array of the actual experiment duration.*
! *                              Units: sec. Format: F10.3 .             *
! *         TAG ( CHARACTER ) -- EOP time tag. Format: "YY/MM/DD HH:MM", *
! *                              Length: 14. Dimension: M_SES.           *
! *       EPOCH ( CHARACTER ) -- Middle epoch of the actual duration of  *
! *                              the session. Units: years. Format: F10.5*
! *     MJD_EOP ( REAL*8    ) -- Array of MJD dates for UT1 pole         *
! *                              coordinates and their rate.             *
! *                              Units: days. Dimension: M_SES.          *
! *     MJD_NUT ( REAL*8    ) -- Array of MJD dates for nutation.        *
! *                              Units: days. Dimension: M_SES.          *
! *  LSO_SESIND ( INTEGER*4 ) -- Cross-index table from the array with   *
! *                              estimates of local sources to session   *
! *                              index, i.e LSO_SESIND(K) is the index   *
! *                              of the session in DBNAME array for the  *
! *                              K-th element of arrays LSO_NAME,        *
! *                              LRA_VAL, LRA_ERR, LDL_VAL, LDL_ERR etc. *
! *  LST_SESIND ( INTEGER*4 ) -- Cross-index table from the array with   *
! *                              estimates of local station position     *
! *                              to the session index, i.e LST_SESIND(K) *
! *                              is the index of the session in DBNAME   *
! *                              array for the K-th element of arrays    *
! *                              LST_NAME, CL_VAL, CL_ERR etc.           *
! *       N_LIN ( INTEGER*4 ) -- Number of lines in spool file.          *
! *       N_SES ( INTEGER*4 ) -- Number of sessions found in spool file. *
! *       N_LSO ( INTEGER*4 ) -- Number of records for source positions  *
! *                              estimated as local parameters for each  *
! *                              session.                                *
! *       N_LST ( INTEGER*4 ) -- Number of records for station positions *
! *                              estimated as local parameters for each  *
! *                              session.                                *
! *       L_SOU ( INTEGER*4 ) -- Number of observed sources estimated as *
! *                              global parameters.                      *
! *       L_PRP ( INTEGER*4 ) -- Number of sources which proper motions  *
! *                              have been estimated.                    *
! *       L_COO ( INTEGER*4 ) -- Number of stations whose positions were *
! *                              estimated as global parameters.         *
! *       L_VEL ( INTEGER*4 ) -- Number of stations whose velocities were*
! *                              estimated as global parameters.         *
! *       L_BAS ( INTEGER*4 ) -- Number of the session-dependent         *
! *                              estimates of baseline lengths for all   *
! *                              baselines and all sessions.             *
! *       L_EXP ( INTEGER*4 ) -- Number of troposphere parameters.       *
! *    IEXP_TRP ( INTEGER*4 ) -- Array of indices of the experiment name *
! *                              in the array DBNAME for each            *
! *                              troposphere parameter. Dimension: M_TRP *
! *    CSTA_TRP ( CHARACTER ) -- Array of statation names for each       *
! *                              troposphere parameter. Dimension: M_TRP *
! *     MJD_TRP ( REAL*8    ) -- Array of modified Julians dates for     *
! *                              troposphere parameter. Units: days.     *
! *                              Dimension: M_TRP.                       *
! *     ZEN_TRP ( REAL*8    ) -- Array of apriori troposphere zenith     *
! *                              delay path delays in psec.              *
! *                              Dimension: M_TRP.                       *
! *     ADJ_TRP ( REAL*8    ) -- Array of adjustments of troposphere     *
! *                              zenith path delays in psec.             *
! *                              Dimension: M_TRP.                       *
! *     ERR_TRP ( REAL*8    ) -- Array of formal uncertainties of        *
! *                              troposphere zenith path delays in psec. *
! *                              Dimension: M_TRP.                       *
! *       L_TRS ( INTEGER*4 ) -- The number of lines with tropospere     *
! *                              estimation statistics.                  *
! *       L_APR ( INTEGER*4 ) -- The number of lines for information     *
! *                              about apriori models.                   *
! *       C_APR ( CHARACTER ) -- Array with the information about the    *
! *                              apriori model. Dimension: L_APR.        *
! *       N_BAS ( INTEGER*4 ) -- Array of number of used baselines for   *
! *                              each experiment. Dimenstion: M_SES.     *
! *      CN_BAS ( CHARACTER ) -- Array of baseline name string for each  *
! *                              experiment. Dimension: M_SES.           *
! *                              If an experiment had M baselines,       *
! *                              then the string will have M words       *
! *                              separated by blank. Each word consists  *
! *                              of baseline name formed by two IVS      *
! *                              two-character long station names        *
! *                              separated by "/" character.             *
! *                              Example: there were three baselines     *
! *                              in the experiment:                      *
! *                              baseline 1: stations Ab and Cd;         *
! *                              baseline 2: stations Ab and Ef;         *
! *                              baseline 3: stations Cd and Ef.         *
! *                              Then the element of CN_BAS array for    *
! *                              this experiment will be                 *
! *                              "Ab/Cd Ab/Ef Cd/Ef"                     *
! *                              The order of baselines in the string    *
! *                              corresponds to the order of baselines   *
! *                              in the database.                        *
! *      KR_BAS ( INTEGER*4 ) -- Array of number of recoverable          *
! *                              observations per baseline, per          *
! *                              experiment.                             *
! *                              Dimension: KR_BAS(MA_BAS,M_SES).        *
! *      KU_BAS ( INTEGER*4 ) -- Array of number of used observations    *
! *                              per baseline, per experiment.           *
! *                              Dimension: KU_BAS(MA_BAS,M_SES).        *
! *       C_NET ( CHARACTER ) -- Array of the VLBI network participation *
! *                              per experiment. Dimension: M_SES.       *
! *                              Each element of the array consists of   *
! *                              a string of 2-letter long IVS station   *
! *                              names which participated in the         *
! *                              experiment and were used in the         *
! *                              solution. The station names are sorted  *
! *                              in alphabetic order.                    *
! *                              Example: there were three baselines     *
! *                              in the experiment:                      *
! *                              baseline 1: stations Ab and Cd;         *
! *                              baseline 2: stations Ab and Ef;         *
! *                              baseline 3: stations Cd and Ef.         *
! *                              Then the element of C_NET array for     *
! *                              this experiment will be                 *
! *                              "AbCdEf".                               *
! *       L_CNS ( INTEGER*4 ) -- The number of lines with constraint     *
! *                              information.                            *
! *       C_CNS ( CHARACTER ) -- Array of lines iwth constraint          *
! *                              information.                            *
! * MJD_STA_REF ( INTEGER*4 ) -- Reference epoch for station position    *
! *                              estimation (MJD part).                  *
! * TAI_STA_REF ( REAL*8    ) -- Reference epoch for station position    *
! *                              estimation (TAI part).                  *
! * MJD_SOT_REF ( INTEGER*4 ) -- Reference epoch for source position     *
! *                              estimation (MJD part).                  *
! * TAI_SOT_REF ( REAL*8    ) -- Reference epoch for source position     *
! *                              estimation (TAI part).                  *
! * SOU_MID_EPOCH ( CHARACTER ) -- The weighted mean epoch of            *
! *                                observations per source in Julian     *
! *                                years.                                *         
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
! *   Normally getpar terminates abnormally if a station in the spool    *
! *   file found which does not have a corresponding entry in the        *
! *   station ns-codes.txt file. A kludge environment variable           *
! *   GETPAR_NOSTA is supported. If set to "YES", than in the case if    *
! *   a spool file has a station without corresponding entry in          *
! *   ns-codes.txt, then getpar will translate that 8-character name to  *
! *   ?? . This trick is supported mainly for simulation runs.           *
! *                                                                      *
! *  ### 15-JUN-1999  GETPAR_PARSE  v16.6 (c) L. Petrov 22-MAY-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'param.i'
      INCLUDE   'solve.i'
      INTEGER*4  MX_SOU, MX_STA
      PARAMETER  ( MX_SOU = MAX_SOU )
      PARAMETER  ( MX_STA =     512 )
      INTEGER*4  M_SES, M_SOU, M_STA, M_COMP, M_LSO, M_LST, M_BAS, M_TRP, &
     &           M_TRS, M_ERM, M_HEO, M_NPV, M_APR, MA_STA, MA_BAS, M_CNS, &
     &           MJD_STA_REF, MJD_SOU_REF
      INTEGER*4  NUT_USAGE
      CHARACTER  FILSPL*(*), RMS_GLO_STR*(*)
      CHARACTER  C_SOU(M_SOU)*8, J_SOU(M_SOU)*10, &
     &           C_PRP(M_SOU)*8, J_PRP(M_SOU)*10, &
     &           RA_VAL(M_SOU)*17,  RA_ERR(M_SOU)*10, &
     &           DL_VAL(M_SOU)*17,  DL_ERR(M_SOU)*10, &
     &           RAP_VAL(M_SOU)*10, RAP_ERR(M_SOU)*10, &
     &           DLP_VAL(M_SOU)*10, DLP_ERR(M_SOU)*10, &
     &           LSO_NAME(M_LSO)*8, LJO_NAME(M_LSO)*10, &
     &                           LRA_VAL(M_LSO)*17, LRA_ERR(M_LSO)*10, &
     &                           LDL_VAL(M_LSO)*17, LDL_ERR(M_LSO)*10, &
     &                           LCR_VAL(M_LSO)*7, &
     &                           USO_VAL(M_LSO)*5,  TSO_VAL(M_LSO)*5, &
     &                           USC_LSO(M_LSO)*3,  TSC_LSO(M_LSO)*3, &
     &                           USC_SOU(M_LSO)*6,  TSC_SOU(M_LSO)*6, &
     &           LST_NAME(M_LST)*8, &
     &           CL_VAL(M_COMP,M_LST)*14, CL_ERR(M_COMP,M_LST)*10, &
     &           C_COO(M_STA)*15,         CSTA_SRT(M_STA)*20, &
     &           C_CRL(15,M_STA)*5,       S_CRL(M_SOU)*6, P_CRL(M_SOU)*69, &
     &           C_VAL(M_COMP,M_STA)*14,  C_ERR(M_COMP,M_STA)*10, &
     &           C_VEL(M_STA)*8,          CVEL_SRT(M_STA)*13, &
     &           V_VAL(M_COMP,M_STA)*8,   V_ERR(M_COMP,M_STA)*8, &
     &           C_BAS(M_BAS)*112,        C_LSO(M_SOU)*8, J_LSO(M_SOU)*10, &
     &           U_LSO(M_SOU)*5,          T_LSO(M_SOU)*5,  &
     &           SU_LSO(M_SOU)*3,         ST_LSO(M_SOU)*3, &
     &           SOL_ID*(*), SOL_DATE*(*), C_CNS(M_CNS)*(*)
      CHARACTER  OBU_SOU(M_SOU)*7,    OBT_SOU(M_SOU)*7, &
     &           SEU_SOU(M_SOU)*5,    SET_SOU(M_SOU)*5, &
     &           DAF_SOU(M_SOU)*10,   DAL_SOU(M_SOU)*10
      CHARACTER  OBU_STA(M_STA)*7,    OBT_STA(M_STA)*7, &
     &           SEU_STA(M_STA)*5,    SET_STA(M_STA)*5, &
     &           DAF_STA(M_STA)*10,   DAL_STA(M_STA)*10
      CHARACTER  XEOP_VAL(M_SES)*11,  XEOP_ERR(M_SES)*10, &
     &           XREOP_VAL(M_SES)*11, XREOP_ERR(M_SES)*10, &
     &           YEOP_VAL(M_SES)*11,  YEOP_ERR(M_SES)*10, &
     &           YREOP_VAL(M_SES)*11, YREOP_ERR(M_SES)*10, &
     &           UEOP_VAL(M_SES)*11,  UEOP_ERR(M_SES)*10, &
     &           REOP_VAL(M_SES)*11,  REOP_ERR(M_SES)*10, &
     &           QEOP_VAL(M_SES)*11,  QEOP_ERR(M_SES)*10, &
     &           PEOP_VAL(M_SES)*11,  PEOP_ERR(M_SES)*10, &
     &           EEOP_VAL(M_SES)*11,  EEOP_ERR(M_SES)*10, &
     &           CEOP(28,M_SES)*6,    RMS_STR(M_SES)*64, CSTA_TRP(M_TRP)*8, &
     &           C_ERM(M_ERM)*(*),    C_HEO(M_HEO)*(*), &
     &           C_APR(M_APR)*(*),    CN_BAS(M_SES)*(*), &
     &           C_NET(M_SES)*(*),    SOU_MID_EPOCH(M_SOU)*(*)
      CHARACTER  DBNAME(M_SES)*16, EXPNAME(M_SES)*8, USED(M_SES)*6, START(M_SES)*14, &
     &           DURA(M_SES)*10, TAG(M_SES)*14, EPOCH(M_SES)*10, &
     &           C_NPV(M_NPV)*128, STA_BAS(2)*8, COD_STA(MX_STA)*2
      REAL*8     RMS_VAL(M_SES), RMS_IND(M_SES), ZEN_TRP(M_TRP), &
     &           ADJ_TRP(M_TRP), ERR_TRP(M_TRP), TAI_STA_REF, TAI_SOU_REF
      INTEGER*4  IND_SOU(M_SOU), IND_PRP(M_SOU), LSO_SESIND(M_LSO), &
     &           LST_SESIND(M_LST), IEXP_TRP(M_TRP), MJD_VAL
      REAL*8     MJD_EOP(M_SES), MJD_NUT(M_SES), MJD_TRP(M_TRP)
      INTEGER*4  L_SOU, L_PRP, L_COO, L_VEL, L_BAS, L_TRP, L_TRS, &
     &           N_LIN, N_SES, N_LSO, N_LST, L_ERM, L_HEO, L_NPV, &
     &           L_APR, L_STA, K_STA, IUER
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*4  N_BAS(M_SES), KR_BAS(MA_BAS,M_SES), KU_BAS(MA_BAS,M_SES)
      LOGICAL*1  FL_NOMASTER
!
      CHARACTER  NAME*15, J_NAME*16, STR_NUM*32, STR*320, STR1*32, STR_LAST*160, &
     &           COMP*6, DBNAME_DATE*10, DBNAME_SUFFIX*2, DATE_CHR*19, &
     &           SPOOL_FMT_DATE*10, SOLVE_REV_DATE*10
      CHARACTER  MASTER_DIR*128, STA_NAME(M_STA)*8, STA_CODE(MX_STA)*2, &
     &           STA_DOME(MX_STA)*9, STA_DESC(MX_STA)*64, GETPAR_NOSTA_STR*80, &
     &           C_TRS(M_TRS)*(*)
      CHARACTER  CNO_BAS(MAX_ARC_BSL)*17, BAS_STR*17
      LOGICAL*4  LG, LE, LS, LB, FL_EPIS, FL_SST, FL_STA, FL_STU(MX_STA), &
     &           FL_GETPAR_NOSTA
      LOGICAL*4  LISTOPT_FL, FL_SEG_POST2005, FL_SRC_POST2021, FL_SRC_POST2024, &
     &           FL_ERM, FL_HEO, FL_NPV, FL_CNS, FL_EOP_ADJ_ONLY
      INTEGER*4  J0, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           IND_EOPCOR, IND_STACOR, &
     &           IB, IE, IP, IP1, IP2, IS, NR, IC, NCC, IL, IND_CUR, IND_LAST, &
     &           I_STA, I_SOU, IND_LSO, IND_LST, L_LSO, IVAL, MJD_I4, &
     &           ISRT_SOU(M_SOU), ISRT_PRP(M_SOU), &
     &           MINSIG_REC, STA_CDP(MX_STA), IO, L_NOB, L_CNS, IER
      INTEGER*4  NOBU_SOU(MX_SOU), NOBT_SOU(MX_SOU), NU_SOU, NT_SOU, &
     &           NSEU_SOU(MX_SOU), NSET_SOU(MX_SOU)
      INTEGER*4  NOBU_STA(MX_STA), NOBT_STA(MX_STA), NU_STA, NT_STA, &
     &           NSEU_STA(MX_STA), NSET_STA(MX_STA)
      REAL*8     START_R8, DURA_R8, YEAR_R8, UTC_R8, SEC_EOP, SEC_NUT
      INTEGER*4, EXTERNAL :: ADD_CLIST, FSTREAM, ILEN, I_LEN, LTM_DIF
      ADDRESS__TYPE, EXTERNAL :: OPENDIR 
      DATA       COMP / 'XYZUEN' /
!
      CALL GETENVAR ( 'GETPAR_NOMASTER', STR )
      IF ( STR(1:1) == 'Y' .OR. STR(1:1) == 'y' ) THEN
           FL_NOMASTER = .TRUE.
         ELSE
           FL_NOMASTER = .FALSE.
      END IF
      CALL GETENVAR ( 'GETPAR_EOP_ADJ_ONLY', STR )
      IF ( STR(1:1) == 'Y' .OR. STR(1:1) == 'y' ) THEN
           FL_EOP_ADJ_ONLY = .TRUE.
         ELSE
           FL_EOP_ADJ_ONLY = .FALSE.
      END IF
!
! --- Open spool file
!
      OPEN ( UNIT=11, FILE=FILSPL, STATUS='OLD', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 4911, IUER, 'GETPAR_PARSE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in opening spool file '//FILSPL )
           RETURN
      END IF
!
      CALL GETENVAR ( 'GETPAR_NOSTA', GETPAR_NOSTA_STR )
      CALL TRAN ( 11, GETPAR_NOSTA_STR, GETPAR_NOSTA_STR )
      IF ( GETPAR_NOSTA_STR(1:1) == 'Y' ) THEN
            FL_GETPAR_NOSTA = .TRUE.
          ELSE 
            FL_GETPAR_NOSTA = .FALSE.
      END IF
!
! --- Initialization
!
      CALL CLRCH ( RMS_GLO_STR )
      CALL NOUT_I4 ( MX_SOU, NOBU_SOU )
      CALL NOUT_I4 ( MX_SOU, NOBT_SOU )
      CALL NOUT_I4 ( MX_SOU, NSEU_SOU )
      CALL NOUT_I4 ( MX_SOU, NSET_SOU )
      CALL NOUT_I4 ( MX_STA, NOBU_STA )
      CALL NOUT_I4 ( MX_STA, NOBT_STA )
      CALL NOUT_I4 ( MX_STA, NSEU_STA )
      CALL NOUT_I4 ( MX_STA, NSET_STA )
      CALL NOUT_R8 ( M_SES,  MJD_EOP  )
      CALL NOUT_R8 ( M_SES,  MJD_NUT  )
      CALL NOUT_R8 ( M_TRP,  MJD_TRP  )
      CALL CLRCH   ( SOL_ID )
      CALL CLRCH   ( SOL_DATE )
      LG = .FALSE.
      LE = .FALSE.
      LS = .FALSE.
      LB = .FALSE.
      NR = 0
      IND_EOPCOR = -999
      IND_STACOR = -999
      IND_LSO = -999
      IND_LST = -999
      N_SES = 0
      N_LSO = 0
      N_LST = 0
      L_SOU = 0
      L_PRP = 0
      L_COO = 0
      L_VEL = 0
      L_BAS = 0
      L_TRP = 0
      L_ERM = 0
      L_HEO = 0
      I_STA = 0
      I_SOU = 0
      L_NPV = 0
      L_APR = 0
      L_TRS = 0
      L_CNS = 0
      MINSIG_REC = 0
      CALL CLRCH ( STR      )
      CALL CLRCH ( STR_LAST )
      FL_SEG_POST2005 = .FALSE.
      FL_SRC_POST2021 = .FALSE.
      FL_SRC_POST2024 = .FALSE.
!
      FL_SST = .FALSE.
      FL_STA = .FALSE.
      L_LSO  = 0
      DO 510 J0=1,M_STA
         CALL CLRCH ( DAF_STA(J0) )
         CALL CLRCH ( DAL_STA(J0) )
 510  CONTINUE
      MJD_STA_REF = -2147483647
      MJD_SOU_REF = -2147483647
      TAI_STA_REF = -1.D30
      TAI_SOU_REF = -1.D30
      USC_LSO = '  0'
      TSC_LSO = '  0'
      USC_SOU = '     0'
      TSC_SOU = '     0'
      SOU_MID_EPOCH = '   0.0  '
!
      CALL GETENVAR ( 'MASTER_DIR', MASTER_DIR )
      IF ( ILEN(MASTER_DIR) == 0 ) THEN
           MASTER_DIR = SOLVE_SAVE_DIR//'/master_dir'
           DIR_DESC = OPENDIR ( MASTER_DIR(1:I_LEN(MASTER_DIR))//CHAR(0) )
           IF ( DIR_DESC .LE. 0 ) THEN
                CALL ERR_LOG ( 4912, IUER, 'GETPAR_PARSE', 'MASTER_DIR '// &
     &              'directory file, '//MASTER_DIR(1:I_LEN(MASTER_DIR))// &
     &              ' has not been defined. Please correct this error and '// &
     &              'either re-compole VTD/Post-Solve or define '// &
     &              'environoment variable MASTER_DIR' )
                RETURN 
             ELSE 
               CALL CLOSEDIR ( %VAL(DIR_DESC) )
           END IF
         ELSE
           DIR_DESC = OPENDIR ( MASTER_DIR(1:I_LEN(MASTER_DIR))//CHAR(0) )
           IF ( DIR_DESC .LE. 0 ) THEN
                CALL ERR_LOG ( 4913, IUER, 'GETPAR_PARSE', 'Environment '// &
     &              'variable MASTER_DIR points to the directory '// &
     &               MASTER_DIR(1:I_LEN(MASTER_DIR))//' that does not '// &
     &               'exist. Please correct this error' )
                RETURN 
             ELSE 
               CALL CLOSEDIR ( %VAL(DIR_DESC) )
           END IF
      END IF
      IF ( MASTER_DIR(I_LEN(MASTER_DIR):I_LEN(MASTER_DIR)) .NE. '/' ) THEN
           MASTER_DIR(I_LEN(MASTER_DIR)+1:I_LEN(MASTER_DIR)+1) =    '/'
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GET_NS_TABLE ( MASTER_DIR, MX_STA, L_STA, STA_NAME, STA_CODE, &
     &                    STA_DOME, STA_CDP, STA_DESC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4914, IUER, 'GETPAR_PARSE', 'Error in getting '// &
     &         'the IVS table of station names and station codes' )
           RETURN 
      END IF
      STA_NAME(L_STA+1) = '??'
!
      FL_ERM = .FALSE.
      FL_HEO = .FALSE.
      FL_NPV = .FALSE.
      FL_CNS = .FALSE.
!
! --- Scan spool file
!
      DO 410 J1=1,1024*1024*1024
         STR_LAST = STR
!
! ------ Read a line and print a counter if needed
!
         READ ( UNIT=11, FMT='(A)', END=810 ) STR
!
         IF ( MOD(J1,10000) .EQ. 0 ) THEN
              WRITE ( 6, FMT='("    line ",I8,"   ",A$)' ) J1, CHAR(13)
              CALL FLUSH ( 6 )
         END IF
         NR = NR + 1
         IF ( INDEX ( STR, 'Beggining of global constraint section' ) > 0 ) THEN
              FL_CNS = .TRUE.
         END IF
         IF ( FL_CNS ) THEN
              L_CNS = L_CNS + 1
              C_CNS(L_CNS) = STR
         END IF
         IF ( INDEX ( STR, 'End of global constraint section' ) > 0 ) THEN
              FL_CNS = .FALSE.
         END IF
!
         IF ( ILEN(STR) .EQ. 0 ) GOTO 410
         IF ( STR(1:33) == ' Station positions are for epoch:' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( STR(35:53), MJD_STA_REF, TAI_STA_REF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4915, IUER, 'GETPAR_PARSE', 'Error in '// &
     &                 'parsing station position reference data from the '// &
     &                  TRIM(STR)//'th line of the spool file '// &
     &                  TRIM(FILSPL)//' -- '//STR )
                   RETURN 
              END IF
         END IF
!
         IF ( STR(31:37) == 'CORREL.' .AND. STR(50:64) == 'Reference date:' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( STR(66:84), MJD_SOU_REF, TAI_SOU_REF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4916, IUER, 'GETPAR_PARSE', 'Error in '// &
     &                 'parsing source position reference data from the '// &
     &                  TRIM(STR)//'th line of the spool file '// &
     &                  TRIM(FILSPL)//' -- '//STR )
                   RETURN 
              END IF
         END IF
!
! ------ Check: whether we crossed the section boundary:
! ------ global, local, statistics
!
         IF ( .NOT. LG .AND. .NOT. LE ) THEN
              IF ( STR(1:14) .EQ. ' Spool format:' ) THEN
                   IF ( STR(19:31) .EQ. 'Revision date' ) THEN
                        SPOOL_FMT_DATE = STR(33:42)
                      ELSE
                        SPOOL_FMT_DATE = '1999.10.01'
                   END IF
              END IF
              IF ( STR(1:15) == ' Solve_release:' ) THEN
                   L_APR = L_APR + 1
                   IF ( L_APR > M_APR ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( M_APR, STR )
                        CALL ERR_LOG ( 4917, IUER, 'GETPAR_PARSE', &
     &                      'Too many lines with description of the '// &
     &                      'apriori model: more than '//STR )
                        RETURN
                   END IF
                   C_APR(L_APR) = STR(2:16)//'   '//STR(17:)
                   CALL TRAN ( 11, C_APR(L_APR)(1:15), C_APR(L_APR)(1:15) )
              END IF
              IF ( STR(1:16) == ' Solve_revision:' ) THEN
                   L_APR = L_APR + 1
                   IF ( L_APR > M_APR ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( M_APR, STR )
                        CALL ERR_LOG ( 4918, IUER, 'GETPAR_PARSE', &
     &                      'Too many lines with description of the '// &
     &                      'apriori model: more than '//STR )
                        RETURN
                   END IF
                   C_APR(L_APR) = STR(2:16)//'   '//STR(17:)
                   CALL TRAN ( 11, C_APR(L_APR)(1:15), C_APR(L_APR)(1:15) )
                   SOLVE_REV_DATE = STR(20:29)
              END IF
              IF ( STR(1:10) .EQ. ' Data base' ) THEN
!
! ---------------- It is possible in INDEPENDENT solution
!
                   LE = .TRUE.
                   FL_SST = .FALSE.
              END IF
         END IF
         IF ( STR(1:3) .EQ. '1  ' .AND. STR(4:4) .NE. ' ' ) THEN
              LE = .FALSE.
            ELSE IF ( STR(1:4) .EQ. '1Ove' ) THEN
              LE = .FALSE.
              LS = .TRUE.
            ELSE IF ( STR(1:4) .EQ. '1Bas' ) THEN
              LE = .FALSE.
              LS = .FALSE.
         END IF
!
         IF ( .NOT. LG .AND. .NOT. LE ) THEN
              IF ( STR(1:13) == ' Solution ID:' ) THEN
                   SOL_ID = STR(19:)
              END IF
              IF ( STR(1:12) == ' Local time:' ) THEN
                   SOL_DATE = STR(19:)
              END IF
              IF ( STR(1:22) .EQ. ' Parameter adjustments' ) THEN
                   LG = .TRUE.
              END IF
         END IF
         IF ( LG .AND. .NOT. LE ) THEN
              IF ( STR(1:4) .EQ. '1Run' ) THEN
                   LG = .FALSE.
                   LE = .TRUE.
                   LB = .FALSE.
              END IF
         END IF
!
         IF ( .NOT. LISTOPT_FL ) THEN
              IF ( STR(1:16) .EQ. ' Listing_Options' ) LISTOPT_FL = .FALSE.
         END IF
         IF ( INDEX ( STR, 'SEG_STYLE POST2005' ) > 0 ) THEN
              FL_SEG_POST2005 = .TRUE.
         END IF
         IF ( INDEX ( STR, 'SRC_STAT POST2021' ) > 0 ) THEN
              FL_SRC_POST2021 = .TRUE.
         END IF
         IF ( INDEX ( STR, 'SRC_STAT POST2024' ) > 0 ) THEN
              FL_SRC_POST2024 = .TRUE.
         END IF
!
! ------ Analyse global section if we are in
!
         IF ( LG ) THEN
              IF ( STR(1:6) == '1  APR' ) THEN
                   L_APR = L_APR + 1
                   IF ( L_APR > M_APR ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( M_APR, STR )
                        CALL ERR_LOG ( 4919, IUER, 'GETPAR_PARSE', &
     &                      'Too many lines with description of the '// &
     &                      'apriori model: more than '//STR )
                        RETURN
                   END IF
                   C_APR(L_APR) = STR(9:)
              END IF
!
! ----------- Shift the line to right if no Listing_Option line was found
! ----------- (it means the the listing is oin PRE-MAY2002 format)
!
              IF ( .NOT. LISTOPT_FL ) THEN
                   IC = INDEX ( COMP, STR(27:27) )
                   IF ( STR(5:5)   .EQ. '.'                     ) STR = ' '//STR
                   IF ( STR(1:21)  .EQ. '      Nutation offset' ) STR = ' '//STR
                   IF ( STR(54:55) .EQ. 'mm'  .AND.  IC .GT. 0  ) STR = ' '//STR
                   IF ( STR(29:32) .EQ. 'Velo' .AND. &
     &                  STR(54:55) .EQ. 'mm'                    ) STR = ' '//STR
                   IF ( STR(17:27) .EQ. 'CORRELATION'           ) STR = ' '//STR
              END IF
!
              IF ( STR(1:15) .EQ. ' Minimum sigma:' ) THEN
                   MINSIG_REC = NR
              END IF
!
! ----------- Global station position
!
              IC = INDEX ( COMP, STR(28:28) )
              IF ( STR(55:56) .EQ. 'mm'    .AND. &
     &             STR(73:74) .EQ. 'mm'    .AND. &
     &             STR(95:96) .EQ. 'mm'    .AND. &
     &             IC .GT. 0               .AND. &
     &             STR(30:33) .NE. 'Velo'         ) THEN
!
! ---------------- Bypass the record which beong to "minimum sigma" block
!
                   IF ( STR(6:6) .NE. '.'      .AND. &
     &                 (NR-MINSIG_REC) .LE. 6        ) GOTO 410
!
! ---------------- Set station name. It may contain date of the episodic motion
! ---------------- at the right field
!
                   CALL CLRCH ( NAME )
                   NAME(1:8) = STR(8:15)
                   IF ( STR(30:33) .NE. 'Comp' ) THEN
                        NAME(9:15) = '_'//STR(30:35)
                        DO 420 J2=I_LEN(NAME(1:8)),15
                           IF ( NAME(J2:J2) .EQ. ' ' ) NAME(J2:J2) = '_'
 420                    CONTINUE
                   END IF
!
! ---------------- Try to find it in the list of station names...
!
                   IF ( L_COO .GT. 0 ) THEN
                        IP = LTM_DIF ( 1, L_COO, C_COO, NAME )
                     ELSE
                        IP = 0
                   END IF
!
! ---------------- ... and update station list if not found
!
                   IF ( IP .LE. 0 ) THEN
                        L_COO = L_COO + 1
                        C_COO(L_COO) = NAME
                        IP = L_COO
                   END IF
!
! ---------------- Get values and formal uncertainties of station position
! ---------------- estimates.
!
                   C_VAL(IC,IP) = STR(40:53)
                   C_ERR(IC,IP) = STR(84:93)
!
! ---------------- Add the index of this station name in the array C_COO to
! ---------------- the last field of CSTA_SRT
!
                   CALL CLRCH ( CSTA_SRT(IP) )
                   CSTA_SRT(IP)(1:15) = NAME
                   CALL INCH   ( IP, CSTA_SRT(IP)(16:20) )
                   CALL CHASHR (     CSTA_SRT(IP)(16:20) )
                   I_STA = IP
              END IF
!
              IF ( STR(1:28) .EQ. ' Correlations:     (Ref Date' .OR. &
     &             STR(1:34) .EQ. ' Correlations:     (Reference date' ) THEN
                   IND_STACOR = NR ! Set mark
              END IF
              NCC = NR - IND_STACOR
!
              IF ( NCC .GE. 2  .AND.  NCC .LE. 6  .AND.  I_STA .GT. 0 ) THEN
!
! ---------------- Gather lines with station/velocities correlations into
! ---------------- array C_CRL
!
                   IS = ((NCC-1)*(NCC-2))/2
                   DO 430 J3=1,NCC-1
                      IS = IS + 1
                      IF ( SPOOL_FMT_DATE .GE. '2003.08.20' ) THEN
                           IB = 13 + (J3-1)*8
                           IE = 18 + (J3-1)*8
                           C_CRL(IS,I_STA) = STR(IB:IB)//STR(IB+2:IE)
                         ELSE
                           IB = 14 + (J3-1)*8
                           IE = 18 + (J3-1)*8
                           C_CRL(IS,I_STA) = STR(IB:IE)
                      END IF
 430               CONTINUE
              END IF
!
! ----------- Global station velocity
!
              IC = INDEX ( COMP, STR(28:28) )
              IF ( STR(55:59) .EQ. 'mm/yr'  .AND. &
     &             STR(73:77) .EQ. 'mm/yr'  .AND. &
     &             STR(95:99) .EQ. 'mm/yr'  .AND. &
     &             IC .GT. 0                .AND. &
     &             STR(30:33) .EQ. 'Velo'         ) THEN
!
! ---------------- Station name
!
                   NAME = STR(8:15)
!
! ---------------- Search station name in the list C_VEL
!
                   IF ( L_VEL .GT. 0 ) THEN
                        IP = LTM_DIF ( 1, L_VEL, C_VEL, NAME )
                     ELSE
                        IP = 0
                   END IF
!
! ---------------- Add it to the list if not found
!
                   IF ( IP .LE. 0 ) THEN
                        L_VEL = L_VEL + 1
                        C_VEL(L_VEL) = NAME
                        IP = L_VEL
                   END IF
!
! ---------------- Get values of station velocity
!
                   V_VAL(IC,IP) = STR(46:53)
                   V_ERR(IC,IP) = STR(86:93)
                   IF ( V_ERR(IC,IP)(4:4) .EQ. ' ' ) V_ERR(IC,IP)(4:4) = '0'
!
! ---------------- Add the index of the station name in the list C_VEL to
! ---------------- the right field of CVEL_SRT
!
                   CALL CLRCH ( CVEL_SRT(IP) )
                   CVEL_SRT(IP)(1:8) = NAME
                   CALL INCH   ( IP, CVEL_SRT(IP)(9:13) )
                   CALL CHASHR (     CVEL_SRT(IP)(9:13) )
              END IF
!
! ----------- Global estimate of right ascension
!
              IF ( STR(6:6) .EQ. '.' .AND. &
     &             INDEX( STR, 'RT. ASC.' ) > 1 .AND. &
     &             INDEX( STR, ' rate '   ) < 1 .AND. &
     &             INDEX( STR, 'velocity' ) < 1       ) THEN
!
! ---------------- Seach for the source name in the array C_SOU
!
                   NAME = STR(9:16)
                   IF ( L_SOU .GT. 0 ) THEN
                        IP = LTM_DIF ( 1, L_SOU, C_SOU, NAME(1:8) )
                     ELSE
                        IP = 0
                   END IF
                   IF ( FL_SRC_POST2021 .OR. FL_SRC_POST2024 ) THEN
                        J_NAME = STR(19:28)
                      ELSE
                        CALL CLRCH ( J_NAME )
                   END IF
!
! ---------------- Add the source name to this array if not found
!
                   IF ( IP .LE. 0 ) THEN
                        L_SOU = L_SOU + 1
                        C_SOU(L_SOU) = NAME(1:8)
                        J_SOU(L_SOU) = J_NAME(1:10)
                        CALL CLRCH ( DAF_SOU(L_SOU) )
                        CALL CLRCH ( DAL_SOU(L_SOU) )
                        IP = L_SOU
                   END IF
!
! ---------------- Reformat estimates of right ascension and its formal
! ---------------- uncertainties to the slightly different format.
!
                   IF ( FL_SRC_POST2021 .OR. FL_SRC_POST2024 ) THEN
                        RA_VAL(IP) = STR(41:42)//'_'//STR(44:45)//'_'//STR(47:57)
                        CALL BLANK_TO_ZERO ( RA_VAL(IP) )
                        RA_ERR(IP) = STR(77:86)
                      ELSE
                        RA_VAL(IP) = STR(36:37)//'_'//STR(39:40)//'_'//STR(42:52)
                        CALL BLANK_TO_ZERO ( RA_VAL(IP) )
                        RA_ERR(IP) = STR(78:87)
                        IF ( RA_ERR(IP)(5:5) .EQ. ' ' ) RA_ERR(IP)(5:5) = '0'
                   END IF
                   CALL CHIN ( RA_VAL(IP)(1:2)//RA_VAL(IP)(4:5)// &
     &                         RA_VAL(IP)(7:8)//RA_VAL(IP)(10:12), ISRT_SOU(IP))
                   IND_SOU(IP) = IP
              END IF
!
! ----------- Global estimates of declination
!
              IF ( STR(6:6) .EQ. '.' .AND. &
     &             INDEX( STR, 'DEC. '    ) > 1 .AND. &
     &             INDEX( STR, ' rate '   ) < 1 .AND. &
     &             INDEX( STR, 'velocity' ) < 1       ) THEN
!
! ---------------- Search for the source name in the array C_SOU
!
                   NAME = STR(9:16)
                   IF ( L_SOU .GT. 0 ) THEN
                        IP = LTM_DIF ( 1, L_SOU, C_SOU, NAME(1:8) )
                     ELSE
                        IP = 0
                   END IF
                   IF ( FL_SRC_POST2021 .OR. FL_SRC_POST2024 ) THEN
                        J_NAME = STR(19:28)
                      ELSE
                        CALL CLRCH ( J_NAME )
                   END IF
!
! ---------------- Add it to the list if not found
!
                   IF ( IP .LE. 0 ) THEN
                        L_SOU = L_SOU + 1
                        C_SOU(L_SOU) = NAME(1:8)
                        J_SOU(L_SOU) = J_NAME 
                        IP = L_SOU
                        IND_SOU(IP) = IP
                        ISRT_SOU    = 0
                   END IF
!
! ---------------- Reformat estimates of right ascension and its formal
! ---------------- uncertainties to the slightly different format.
!
                   IF ( FL_SRC_POST2021 .OR. FL_SRC_POST2024 ) THEN
                        DL_VAL(IP) = STR(40:42)//'_'//STR(44:45)//'_'//STR(47:57)
                        CALL BLANK_TO_ZERO ( DL_VAL(IP) )
                        DL_ERR(IP) = STR(77:86)
                      ELSE
                        DL_VAL(IP) = STR(35:37)//'_'//STR(39:40)//'_'//STR(42:51)
                        CALL BLANK_TO_ZERO ( DL_VAL(IP) )
                        IF ( STR(89:94) == 'm-asec' ) THEN
                             DL_ERR(IP) = STR(78:87)
                           ELSE 
                             DL_ERR(IP) = STR(83:92)
                        END IF
                        IF ( DL_ERR(IP)(5:5) .EQ. ' ' ) DL_ERR(IP)(5:5) = '0'
                   END IF
                   I_SOU = IP
              END IF
!
              IF ( STR(18:28) .EQ. 'CORRELATION' ) THEN
!
! ---------------- Add corrlations
!
                   IF ( I_SOU .LE. 0 ) THEN
                        CALL ERR_LOG ( 4920, IUER, 'GETPAR_PARSE', 'I_SOU=0' )
                        RETURN
                   END IF
                   IF ( SPOOL_FMT_DATE .GE. '2003.08.20' ) THEN
                        S_CRL(I_SOU) = STR(33:39)
                     ELSE IF ( SPOOL_FMT_DATE .GE. '2002.12.24' ) THEN
                        S_CRL(I_SOU) = STR(34:34)//'0'//STR(35:39)
                      ELSE
                        S_CRL(I_SOU) = STR(77:77)//'0'//STR(78:82)
                   END IF
                   IF ( S_CRL(I_SOU)(3:3) .EQ. '*' ) THEN
                        S_CRL(I_SOU) = ' 0.9999'
                   END IF
                   IF ( S_CRL(I_SOU)(1:1) .EQ. '1' ) THEN
                        S_CRL(I_SOU) = ' 0.9999'
                   END IF
                ELSE IF ( STR(30:38) == ' CORREL. '       .AND. &
     &                    STR(50:64) == 'Reference date:'       ) THEN
                   S_CRL(I_SOU) = STR(41:47) 
                   IF ( S_CRL(I_SOU)(3:3) .EQ. '*' ) THEN
                        S_CRL(I_SOU) = ' 0.9999'
                   END IF
                   IF ( S_CRL(I_SOU)(1:1) .EQ. '1' ) THEN
                        S_CRL(I_SOU) = ' 0.9999'
                   END IF
              END IF
!
! ----------- Estimate of proper motion over right ascension
!
              IF ( STR(6:6) .EQ. '.'            .AND. &
     &             INDEX( STR, 'RT. ASC.' ) > 1 .AND. &
     &             INDEX( STR, ' rate '   ) > 1       ) THEN
                   NAME = STR(9:16)
                   CALL ERR_PASS  ( IUER, IER )
                   IP = ADD_CLIST ( M_SOU, L_PRP, C_PRP, NAME(1:8), IER )
!
                   J_PRP(IP) = STR(19:28)
                   RAP_VAL(IP) = STR(47:56)
                   RAP_ERR(IP) = STR(82:91)
                   IND_PRP(IP) = IP
                   I_SOU = LTM_DIF ( 1, L_SOU, C_SOU, NAME(1:8) )
                   CALL CHIN ( RA_VAL(I_SOU)(1:2)//RA_VAL(I_SOU)(4:5)// &
     &                         RA_VAL(I_SOU)(7:8)//RA_VAL(I_SOU)(10:12), ISRT_PRP(IP) )
              END IF
!
! ----------- Estimate of proper motion over declination 
!
              IF ( STR(6:6) .EQ. '.'            .AND. &
     &             INDEX( STR, 'DEC.    ' ) > 1 .AND. &
     &             INDEX( STR, ' rate '   ) > 1       ) THEN
                   NAME = STR(9:16)
                   CALL ERR_PASS  ( IUER, IER )
                   IP = ADD_CLIST ( M_SOU, L_PRP, C_PRP, NAME(1:8), IER )
!
                   J_PRP(IP) = STR(19:28)
                   DLP_VAL(IP) = STR(47:56)
                   DLP_ERR(IP) = STR(82:91)
              END IF
!
! ----------- Estimate of correlation between proper motion over declination and right ascension
!
              IF ( STR(31:42) == 'PRP. CORREL.'       ) THEN
                   CALL ERR_PASS  ( IUER, IER )
                   NAME = STR(9:16)
                   IP = ADD_CLIST ( M_SOU, L_PRP, C_PRP, NAME(1:8), IER )
                   P_CRL(IP) = STR(45:113)
              END IF             
!
              IF ( STR(1:15) == 'ERM_SECTION END' ) THEN
                   FL_ERM = .FALSE.
              END IF
              IF ( STR(1:22) == 'HEO  Output Begin: ==>' ) THEN
                   FL_HEO = .TRUE.
              END IF
              IF ( STR(1:20) == 'HEO  Output End: <==' ) THEN
                   FL_HEO = .FALSE.
              END IF
              IF ( STR(1:8) == 'L_ERM  0' ) THEN
                   FL_ERM = .FALSE.
              END IF
!
              IF ( FL_ERM ) THEN
                   L_ERM = L_ERM + 1
                   C_ERM(L_ERM) = STR
              END IF
              IF ( STR(1:17) == 'ERM_SECTION BEGIN' ) THEN
                   FL_ERM = .TRUE.
              END IF
!
              IF ( FL_HEO ) THEN
                   IF ( STR(1:22) == 'HEO  Output Begin: ==>' ) THEN
                        CONTINUE
                      ELSE
                        L_HEO = L_HEO + 1
                        IF ( L_HEO > M_HEO ) THEN
                             CALL CLRCH ( STR  )
                             CALL CLRCH ( STR1 )
                             CALL INCH  ( M_HEO, STR  )
                             CALL INCH  ( L_HEO, STR1 )
                             CALL ERR_LOG ( 4921, IUER, 'GETPAR_PARSE', &
     &                           'To many lines for HEO output: more than '// &
     &                           ' M_HEO: '//STR(1:I_LEN(STR))//' while '// &
     &                           'processing line '//STR1 )
                             RETURN
                        END IF
                        C_HEO(L_HEO) = STR
                   END IF
              END IF
!
! ----------- Check for non-linear site position variations,
! ----------- either harmonic site position variations, or
! ----------- B-spline position variations
!
              IF ( STR(1:6) == '2  SPE'  .OR.  STR(1:6) == '2  HPE' ) THEN
                   FL_NPV = .FALSE.
              END IF
              IF ( FL_NPV ) THEN
                   L_NPV = L_NPV + 1
                   IF ( L_NPV > M_NPV ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( M_NPV, STR )
                        CALL ERR_LOG ( 4922, IUER, 'GETPAR_PARSE', &
     &                      'Too small parameter M_NPV: '//STR )
                        RETURN
                   END IF
                   C_NPV(L_NPV) = STR
              END IF
              IF ( STR(1:6) == '1  SPE'  .OR.  STR(1:6) == 'L_HPE ' ) THEN
                   FL_NPV = .TRUE.
              END IF
         END IF
!
! ------ Let us analyze local section if we are in that section
!
         IF ( LE ) THEN
              IF ( STR(1:10) .EQ. ' Data base' ) THEN
                   FL_SST = .FALSE.
                   CALL INIT_LOGICAL ( MX_STA, FL_STU, .FALSE. )
!
! ---------------- New session.
!
                   N_SES = N_SES + 1
!
! ---------------- Initialization
!
                   CALL CLRCH ( XEOP_VAL(N_SES) )
                   CALL CLRCH ( YEOP_VAL(N_SES) )
                   CALL CLRCH ( XREOP_VAL(N_SES) )
                   CALL CLRCH ( YREOP_VAL(N_SES) )
                   CALL CLRCH ( UEOP_VAL(N_SES) )
                   CALL CLRCH ( REOP_VAL(N_SES) )
                   CALL CLRCH ( QEOP_VAL(N_SES) )
                   CALL CLRCH ( EEOP_VAL(N_SES) )
                   CALL CLRCH ( PEOP_VAL(N_SES) )
                   CALL CLRCH ( XEOP_ERR(N_SES) )
                   CALL CLRCH ( YEOP_ERR(N_SES) )
                   CALL CLRCH ( XREOP_ERR(N_SES) )
                   CALL CLRCH ( YREOP_ERR(N_SES) )
                   CALL CLRCH ( UEOP_ERR(N_SES) )
                   CALL CLRCH ( REOP_ERR(N_SES) )
                   CALL CLRCH ( QEOP_ERR(N_SES) )
                   CALL CLRCH ( EEOP_ERR(N_SES) )
                   CALL CLRCH ( PEOP_ERR(N_SES) )
!
                   CALL CLRCH ( START(N_SES) )
                   CALL CLRCH ( USED(N_SES)  )
                   CALL CLRCH ( DURA(N_SES)  )
                   CALL CLRCH ( TAG(N_SES)   )
                   CALL CLRCH ( RMS_STR(N_SES) )
                   L_NOB = 0
!
                   DO 440 J4=1,28
                      CEOP(J4,N_SES) = '      '
 440               CONTINUE
!
                   RMS_VAL(N_SES) = 0.0D0
                   RMS_IND(N_SES) = N_SES + 0.001
!
! ---------------- Get database name
!
                   CALL CLRCH ( DBNAME(N_SES) )
                   CALL CLRCH ( EXPNAME(N_SES) )
                   DBNAME(N_SES)  = STR(12:22)//'  '//STR(27:28)
                   EXPNAME(N_SES) = STR(31:38)
                   IF ( ILEN(EXPNAME(N_SES)) == 0 ) THEN
                        EXPNAME(N_SES) = DBNAME(N_SES)(3:10)
                   END IF
!
! ---------------- Extract the date from the database name
!
                   IER = -1
                   CALL PARSE_DBNAME ( DBNAME(N_SES)(1:10), DBNAME_DATE, &
     &                                 DBNAME_SUFFIX, IER )
              END IF
!
              IF ( STR(1:20) .EQ. ' Session started on:' ) THEN
                   START(N_SES) = STR(25:38)
              END IF
              IF ( STR(1:29) .EQ. ' Number of used observations:' ) THEN
                   USED(N_SES) = STR(55:60)
              END IF
              IF ( STR(1:18) .EQ. ' Nominal duration:' ) THEN
                   DURA(N_SES) = STR(24:33)
              END IF
              IF ( STR(1:17) .EQ. ' Actual duration:' ) THEN
                   DURA(N_SES) = STR(24:33)
              END IF
              IF ( STR(32:41) .EQ. 'deselected' ) THEN
!
! ---------------- Update the list of deselected baselines
!
                   L_NOB = L_NOB + 1
                   CNO_BAS(L_NOB) = STR(2:18)
              END IF
!
              IF ( STR(1:8) .EQ.  '   Delay' ) THEN
!
! ---------------- Reformat WRMS of the positfit residuals for this session
!
                   IF ( STR(33:33) .EQ. '.'  .AND.  STR(55:55) .EQ. '.' ) THEN
!
! --------------------- POST03 format
!
                        RMS_STR(N_SES) = DBNAME(N_SES)(1:10)//'  '// &
     &                                   STR(12:17)//' '//STR(24:39)
                        READ ( UNIT=STR(25:36), FMT='(F10.5)' ) RMS_VAL(N_SES)
                      ELSE
!
! --------------------- PRE03 format
!
                        RMS_STR(N_SES) = DBNAME(N_SES)(1:10)//'  '//STR(15:36)
                        CALL CLRCH ( STR_NUM )
                        STR_NUM = STR(21:33)
                        IF ( INDEX ( STR_NUM, '.' ) .EQ. 0 ) THEN
                             STR_NUM = STR_NUM(1:I_LEN(STR_NUM))//'.0'
                        END IF
                        READ ( UNIT=STR_NUM, FMT='(F4.0)', IOSTAT=IO ) &
     &                  RMS_VAL(N_SES)
                        IF ( IO .NE. 0 ) RMS_VAL(N_SES) = 0.0D0
                        IF ( STR(35:36) .EQ. 'ps' ) THEN
                             CONTINUE
                           ELSE IF ( STR(35:44) .EQ. 'NANOSECOND' ) THEN
                             RMS_VAL(N_SES) = RMS_VAL(N_SES)*1.D3
                           ELSE IF ( STR(35:43) .EQ. 'MICROSEC.' ) THEN
                             RMS_VAL(N_SES) = RMS_VAL(N_SES)*1.D6
                           ELSE IF ( STR(35:38) .EQ. 'SEC.' ) THEN
                             RMS_VAL(N_SES) = RMS_VAL(N_SES)*1.D12
                        END IF
                   END IF
                   RMS_VAL(N_SES) = -RMS_VAL(N_SES) ! Invert sign for sorting
              END IF
!
              IF ( STR(1:8) .EQ.  '   Rate ' ) THEN
                   RMS_STR(N_SES) = RMS_STR(N_SES)(1:36)//' '//STR(26:44)
              END IF
!
! ----------- Shift the line to right if no Listing_Option line was found
! ----------- (it means the the listing is oin PRE-MAY2002 format)
!
              IF ( .NOT. LISTOPT_FL ) THEN
                   IF ( STR(5:5)   .EQ. '.'                     ) STR = ' '//STR
                   IF ( STR(1:21)  .EQ. '      Nutation offset' ) STR = ' '//STR
                   IF ( STR(29:32) .EQ. 'Comp' .AND. &
     &                  STR(54:55) .EQ. 'mm'                    ) STR = ' '//STR
                   IF ( STR(17:27) .EQ. 'CORRELATION'           ) STR = ' '//STR
              END IF
!
              IF ( STR(17:20) .EQ. 'zd 0' .AND. &
     &             ( STR(24:24) .EQ. '/' .OR. STR(26:26) .EQ. '.' ) ) THEN
                   L_TRP = L_TRP + 1
                   IF ( L_TRP .GT. M_TRP ) THEN
                        CALL CLRCH ( STR_NUM )
                        CALL INCH  ( L_TRP, STR_NUM )
                        CALL ERR_LOG ( 4923, IUER, 'GETPAR_PARSE', 'The '// &
     &                      'number of troposphere parameters exceeded the '// &
     &                      'limit: '//STR_NUM )
                        RETURN
                   END IF
!
! ---------------- Transform the date
!
                   IF ( FL_SEG_POST2005 ) THEN
                        DATE_CHR = STR(22:44)
                      ELSE
                        DATE_CHR = '20'//STR(22:35)
                        CALL CHIN ( DATE_CHR(3:4), IVAL )
                        IF ( IVAL .GE. 70 ) THEN
                            DATE_CHR(1:2) = '19'
                        END IF
                        DATE_CHR(5:5) = '.'
                        DATE_CHR(8:8) = '.'
                        DATE_CHR(11:11) = '-'
                        DATE_CHR(17:19) = ':00'
                        CALL BLANK_TO_ZERO ( DATE_CHR )
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL DATE_TO_TIME ( DATE_CHR, MJD_I4, UTC_R8, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR_NUM )
                        CALL INCH  ( J1, STR_NUM )
                        CALL ERR_LOG ( 4924, IUER, 'GETPAR_PARSE', 'Error '// &
     &                      'in parsing line '//STR_NUM(1:I_LEN(STR_NUM))// &
     &                      ' -- wrong format of date: '//STR(22:35) )
                        RETURN
                   END IF
                   MJD_TRP(L_TRP) = MJD_I4 + UTC_R8/86400.0d0
!
                   CSTA_TRP(L_TRP) = STR(8:15)
                   IEXP_TRP(L_TRP) = N_SES
!
! ---------------- Read apriori troposphere zenith path delay
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL DFOR_MEN ( STR(58:71), ZEN_TRP(L_TRP), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR_NUM )
                        CALL INCH  ( J1, STR_NUM )
                        CALL ERR_LOG ( 4925, IUER, 'GETPAR_PARSE', 'Error '// &
     &                      'in reading apriori zenith path delay on line '// &
     &                       STR_NUM(1:I_LEN(STR_NUM))//' -- wrong format: '// &
     &                       STR(58:71) )
                        RETURN
                   END IF
              END IF
!
              IF ( STR(17:20) .EQ. 'AT 0' ) THEN
!
! ---------------- Now check whether we all segmented parameters were put
! ---------------- in listing
!
                   CALL CHIN ( STR(1:5),      IND_CUR )
                   CALL CHIN ( STR_LAST(1:5), IND_LAST )
                   IF ( IND_CUR .EQ. 1 ) THEN
!
! --------------------- M-m-m... perhaps
!
                        STR(17:20) = 'at 0'
                   END IF
                   IF ( IND_CUR .EQ. IND_LAST ) THEN
!
! --------------------- Oh, certainly. The previous line has the same parameter
! --------------------- index
!
                        STR(17:20) = 'at 0'
                   END IF
                   IF ( IND_CUR .EQ. IND_LAST+1 ) THEN
!
! --------------------- Yes, the previous line has parameter index less by 1.
!
                        STR(17:20) = 'at 0'
                   END IF
              END IF
!
              IF ( STR(17:24) .EQ. 'Avg Atm:' .AND. &
     &             STR_LAST(17:20) .EQ. 'AT 0'      ) THEN
!
! ---------------- Another way to avoid to learn whether egmented parameteres
! ---------------- were included in the listing is to check "Avg Atm:" line:
! ---------------- if it foloows "AT 0" line it means that no segmented
! ---------------- atmosphere parameters are in the listing.
!
                   IF ( L_TRP .GT. 0 ) L_TRP = L_TRP-1
              END IF
!
              IF ( STR(17:20) .EQ. 'at 0'  .AND.  &
     &             ( STR(24:24) .EQ. '/' .OR. STR(26:26) .EQ. '.' ) ) THEN
!
                   IF ( STR_LAST(17:20) .EQ. 'zd 0'  .AND. &
     &                  ( STR(24:24) .EQ. '/' .OR. STR(26:26) .EQ. '.' ) ) THEN
                        CONTINUE
                      ELSE
                        L_TRP = L_TRP + 1
!
                        IF ( L_TRP .GT. M_TRP ) THEN
                             CALL CLRCH ( STR_NUM )
                             CALL INCH  ( L_TRP, STR_NUM )
                             CALL ERR_LOG ( 4926, IUER, 'GETPAR_PARSE', 'The '// &
     &                           'number of troposphere parameters exceeded '// &
     &                           'the limit: '//STR_NUM )
                            RETURN
                        END IF
                        ZEN_TRP(L_TRP) = 0.0D0
!
! --------------------- Transform the date
!
                        IF ( FL_SEG_POST2005 ) THEN
                             DATE_CHR = STR(22:44)
                           ELSE
                             DATE_CHR = '20'//STR(22:35)
                             CALL CHIN ( DATE_CHR(3:4), IVAL )
                             IF ( IVAL .GE. 70 ) THEN
                                 DATE_CHR(1:2) = '19'
                             END IF
                             DATE_CHR(5:5) = '.'
                             DATE_CHR(8:8) = '.'
                             DATE_CHR(11:11) = '-'
                             DATE_CHR(17:19) = ':00'
                             CALL BLANK_TO_ZERO ( DATE_CHR )
                        END IF
!
                        CALL ERR_PASS ( IUER, IER )
                        CALL DATE_TO_TIME ( DATE_CHR, MJD_I4, UTC_R8, IER )
                        MJD_TRP(L_TRP) = MJD_I4 + UTC_R8/86400.0d0
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR_NUM )
                             CALL INCH  ( J1, STR_NUM )
                             CALL ERR_LOG ( 4927, IUER, 'GETPAR_PARSE', &
     &                           'Error in parsing line '// &
     &                            STR_NUM(1:I_LEN(STR_NUM))// &
     &                           ' -- wrong format of date: '//STR(22:35) )
                             RETURN
                        END IF
                        CSTA_TRP(L_TRP) = STR(8:15)
                        IEXP_TRP(L_TRP) = N_SES
                   END IF
!
! ---------------- Read adjustments of troposphere zenith path delay
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL DFOR_MEN ( STR(58:71), ADJ_TRP(L_TRP), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR_NUM )
                        CALL INCH  ( J1, STR_NUM )
                        CALL ERR_LOG ( 4928, IUER, 'GETPAR_PARSE', 'Error '// &
     &                      'in reading adjustemnts of zenith path delay on '// &
     &                      'line '//STR_NUM(1:I_LEN(STR_NUM))// &
     &                      ' -- wrong format: '//STR(58:71) )
                        RETURN
                   END IF
!
! ---------------- Read formal uncertainties of troposphere path delay
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL DFOR_MEN ( STR(84:93), ERR_TRP(L_TRP), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR_NUM )
                        CALL INCH  ( J1, STR_NUM )
                        CALL ERR_LOG ( 4929, IUER, 'GETPAR_PARSE', 'Error '// &
     &                      'in reading formal uncertainties of zenith path '// &
     &                      'delay on line '//STR_NUM(1:I_LEN(STR_NUM))// &
     &                      ' -- wrong format: '//STR(58:71) )
                        RETURN
                   END IF
              END IF
!
              IF ( STR(1:20) .EQ. ' Baseline Statistics' ) THEN
                   FL_SST = .FALSE.
!
! ---------------- We reached baseline statistics section. It is time
! ---------------- to reformat START_R8, DURA_R8, YEAR_R8, EPOCH_CH
!
                   IF ( ILEN(START(N_SES)) .EQ. 0 ) THEN
                        CALL ERR_LOG ( 4930, IUER, 'GETPAR_PARSE', 'No start '// &
     &                      'date was found. Spool file format is too old '// &
     &                      'and not supported by getpar' )
                        RETURN
                   END IF
!
                   IER = -1
                   CALL DFOR_MEN ( START(N_SES), START_R8, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4931, IUER, 'GETPAR_PARSE', 'Error in '// &
     &                      'attempt to decode start date '//START(N_SES) )
                        RETURN
                   END IF
!
                   CALL DFOR_MEN ( DURA(N_SES),  DURA_R8,  IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4932, IUER, 'GETPAR_PARSE', 'Error in '// &
     &                      'attempt to decode session duration '//DURA(N_SES) )
                        RETURN
                   END IF
!
                   YEAR_R8 = ( START_R8 - (J2000__JD - 0.5D0) + &
     &                         (DURA_R8/86400.0)/2.0D0 )/JYEAR__DAYS + 2000.0D0
                   WRITE ( EPOCH(N_SES), FMT='(0P,F10.5)' ) YEAR_R8
              END IF
              IF ( STR(1:27) .EQ. '      Baseline      # W.Obs' .OR. &
     &             STR(1:30) .EQ. '      Baseline      Numb.  Obs'   ) THEN
                   FL_STA = .TRUE.
                   CALL CLRCH ( C_NET(N_SES)  )
                   CALL CLRCH ( CN_BAS(N_SES) )
                   N_BAS(N_SES) = 0
              END IF
              IF ( FL_STA ) THEN
                   IF ( STR(101:108) == 'BAS_STAT' .OR. &
     &                ( STR(2:2)   .NE. ' ' .AND. &
     &                  STR(10:10) .EQ. '-' .AND. &
     &                  STR(23:23) .EQ. '/'       )    ) THEN
! 
                        IF ( FL_SRC_POST2021 .OR. FL_SRC_POST2024 ) THEN
                             CALL CHIN ( STR(19:24), NU_STA )
                             CALL CHIN ( STR(26:30), NT_STA )
                           ELSE
                             CALL CHIN ( STR(19:22), NU_STA )
                             CALL CHIN ( STR(24:27), NT_STA )
                        END IF
!
! --------------------- Get information about the first station of a baseline
!
                        IP1 = LTM_DIF ( 0, L_COO, C_COO, STR(2:9) )
                        IF ( IP1 .GT. 0 ) THEN
                             NOBU_STA(IP1) = NOBU_STA(IP1) + NU_STA
                             NOBT_STA(IP1) = NOBT_STA(IP1) + NT_STA
                             IF ( .NOT. FL_STU(IP1) ) THEN
                                  NSET_STA(IP1) = NSET_STA(IP1) + 1
                             END IF
                             IF ( NU_STA .GT. 0 ) THEN
                                  IF ( NSEU_STA(IP1) .EQ. 0 ) THEN
                                       DAF_STA(IP1)  = DBNAME_DATE
                                       DAL_STA(IP1)  = DBNAME_DATE
                                  END IF
                                  IF ( DBNAME_DATE .LT. DAF_STA(IP1) ) THEN
                                       DAF_STA(IP1)  = DBNAME_DATE
                                  END IF
                                  IF ( DBNAME_DATE .GT. DAL_STA(IP1) ) THEN
                                       DAL_STA(IP1)  = DBNAME_DATE
                                  END IF
                                  IF ( .NOT. FL_STU(IP1) ) THEN
                                       NSEU_STA(IP1) = NSEU_STA(IP1) + 1
                                  END IF
                             END IF
!
! -------------------------- Set a flag that this station in this database
! -------------------------- has been taken into account in order
! -------------------------- to prevent counting each baseline with this
! -------------------------- station as a new session
!
                             FL_STU(IP1) = .TRUE.
                        END IF
!
! --------------------- Get information about the second station of a baseline
!
                        IP2 = LTM_DIF ( 0, L_COO, C_COO, STR(11:18) )
                        IF ( IP2 .GT. 0 ) THEN
                             NOBU_STA(IP2) = NOBU_STA(IP2) + NU_STA
                             NOBT_STA(IP2) = NOBT_STA(IP2) + NT_STA
                             IF ( .NOT. FL_STU(IP2) ) THEN
                                  NSET_STA(IP2) = NSET_STA(IP2) + 1
                             END IF
                             IF ( NU_STA .GT. 0 ) THEN
                                  IF ( NSEU_STA(IP2) .EQ. 0 ) THEN
                                       DAF_STA(IP2)  = DBNAME_DATE
                                       DAL_STA(IP2)  = DBNAME_DATE
                                  END IF
                                  IF ( DBNAME_DATE .LT. DAF_STA(IP2) ) THEN
                                       DAF_STA(IP2)  = DBNAME_DATE
                                  END IF
                                  IF ( DBNAME_DATE .GT. DAL_STA(IP2) ) THEN
                                       DAL_STA(IP2)  = DBNAME_DATE
                                  END IF
!
                                  IF ( .NOT. FL_STU(IP2) ) THEN
                                       NSEU_STA(IP2) = NSEU_STA(IP2) + 1
                                  END IF
                             END IF
!
! -------------------------- Set a flag that this station in this database
! -------------------------- has been taken into account in order to prevent
! -------------------------- counting each baseline with this station as
! -------------------------- a new session
!
                             FL_STU(IP2) = .TRUE.
                        END IF
!
! --------------------- Collecting statiscis abnout the baseline
!
                        STA_BAS(1) = STR(2:9) 
                        STA_BAS(2) = STR(11:18) 
!
! --------------------- Repair station names (replace albnsk with underscores)
!
                        CALL VTD_NAME_REPAIR ( STA_BAS(1) )
                        CALL VTD_NAME_REPAIR ( STA_BAS(2) )
!
! --------------------- Get IVS 2-character long station codes
!
                        IP1 = LTM_DIF ( 1, L_STA, STA_NAME, STA_BAS(1) )
                        IF ( IP1 .LE. 0 ) THEN
                             IF ( FL_GETPAR_NOSTA  ) THEN
                                  IP1 = L_STA + 1
                                ELSE 
                                  IF ( FL_NOMASTER ) THEN
                                       IP1 = 1
                                     ELSE
                                       WRITE ( 6, * ) 'L_STA = ', L_STA
                                       CALL ERR_LOG ( 4933, IUER, 'GETPAR_PARSE', &
     &                                     'Cannot find station '//STA_BAS(1)// &
     &                                     ' in the IVS master file '// &
     &                                     ' ns-codes.txt, in the directory '// &
     &                                     MASTER_DIR(1:I_LEN(MASTER_DIR))// &
     &                                     ' If you run a simulation solution '// &
     &                                     'with bogus station name, consider '// &
     &                                     'setting a kludge envirnoment variable '// &
     &                                     'GETPAR_NOSTA to "YES" and try again.' )
                                       RETURN 
                                  END IF
                             END IF
                        END IF
                        IP2 = LTM_DIF ( 1, L_STA, STA_NAME, STA_BAS(2) )
                        IF ( IP2 .LE. 0 ) THEN
                             IF ( FL_GETPAR_NOSTA ) THEN
                                  IP2 = L_STA + 1
                                ELSE 
                                  IF ( FL_NOMASTER ) THEN
                                       IP2 = 1
                                     ELSE
                                       WRITE ( 6, * ) 'L_STA = ', L_STA
                                       CALL ERR_LOG ( 4934, IUER, 'GETPAR_PARSE', &
     &                                     'Cannot find station '//STA_BAS(2)// &
     &                                     ' in the IVS master file '// &
     &                                     ' ns-codes.txt, in the directory '// &
     &                                     MASTER_DIR(1:I_LEN(MASTER_DIR))// &
     &                                     ' If you run a simulation solution '// &
     &                                     'with bogus station name, consider '// &
     &                                     'setting a kludge envirnoment variable '// &
     &                                     'GETPAR_NOSTA to "YES" and try again.' )
                                       RETURN 
                                 END IF
                             END IF
                        END IF
!
! --------------------- Update the network configuration line
!
                        IF ( FL_NOMASTER ) THEN
                             C_NET(N_SES)(ILEN(C_NET(N_SES))+1:) = '??'
                           ELSE
                             IF ( INDEX ( C_NET(N_SES), STA_CODE(IP1) ) .LE. 0 ) THEN
                                  C_NET(N_SES)(ILEN(C_NET(N_SES))+1:) = STA_CODE(IP1) 
                             END IF
                             IF ( INDEX ( C_NET(N_SES), STA_CODE(IP2) ) .LE. 0 ) THEN
                                  C_NET(N_SES)(ILEN(C_NET(N_SES))+1:) = STA_CODE(IP2) 
                             END IF
                        END IF
!
! --------------------- Increment the baseline counter
!
                        N_BAS(N_SES) = N_BAS(N_SES) + 1
!
! --------------------- Update the baseline names string
!
                        IF ( FL_NOMASTER ) THEN
                             CN_BAS(N_SES)((N_BAS(N_SES)-1)*6+1:N_BAS(N_SES)*6) = &
     &                                    '??/??'
                           ELSE
                             CN_BAS(N_SES)((N_BAS(N_SES)-1)*6+1:N_BAS(N_SES)*6) = &
     &                                     STA_CODE(IP1)//'/'//STA_CODE(IP2)//' '
                        END IF
!
! --------------------- Extract the number of used and the number of 
! --------------------- recoverable observations
!
!                          call chin ( str(19:22), ku_bas(n_bas(n_ses),n_ses) )
!                          call chin ( str(24:27), kr_bas(n_bas(n_ses),n_ses) )
!
!@                         KU_BAS(N_BAS(N_SES),N_SES) = NU_STA
!@                         KR_BAS(N_BAS(N_SES),N_SES) = NT_STA
                   END IF
              END IF
!
              IF ( STR(1:14) .EQ. ' Not included:' ) THEN
                   FL_STA = .FALSE.
              END IF
!
              IF ( STR(1:18) .EQ. ' Source Statistics' ) THEN
                   K_STA = ILEN(C_NET(N_SES))/2
                   DO 450 J5=1,K_STA
                      COD_STA(J5) = C_NET(N_SES)((J5-1)*2+1:J5*2)
 450               CONTINUE 
!
                   CALL SORT_CH ( K_STA, COD_STA )
!
                   DO 460 J6=1,K_STA
                      C_NET(N_SES)((J6-1)*2+1:J6*2) = COD_STA(J6)
 460               CONTINUE 
!
                   FL_SST = .TRUE.
                   FL_STA = .FALSE.
                   L_LSO = 0
              END IF
              IF ( FL_STA ) THEN
                   IF ( STR(1:27) .EQ. ' Site Statistics (site_name' ) THEN
                        FL_STA = .FALSE.
                   END IF
              END IF
              IF ( FL_SST .OR. FL_STA ) THEN
                   IF ( STR(24:43) .EQ. '*** Flyby Status ***' ) THEN
                        FL_SST = .FALSE.
                        FL_STA = .FALSE.
                   END IF
              END IF
              IF ( FL_SST .OR. FL_STA ) THEN
                   IF ( STR(1:16) .EQ. 'Station   Source' ) THEN
                        FL_SST = .FALSE.
                        FL_STA = .FALSE.
                   END IF
              END IF
              IF ( FL_SST ) THEN
                   IF ( ( STR(6:6) .NE. ' ' .AND. STR(23:23) .EQ. '/') .OR. &
     &                    STR(1:9) .EQ. 'SRC_STAT:'                    .OR. &
     &                  ( STR(23:23) .EQ. '/' .AND. &
     &                    STR(28:28) .EQ. '/' .AND. &
     &                    STR(33:33) .EQ. '/' .AND. &
     &                    STR(43:43) .EQ. '.'       )   ) THEN
                        L_LSO = L_LSO + 1
                        IF ( STR(1:9) .EQ. 'SRC_STAT:' .AND. &
     &                       ( FL_SRC_POST2021 .OR. FL_SRC_POST2024 ) ) THEN
                             C_LSO(L_LSO) = STR(12:19)
                             J_LSO(L_LSO) = STR(22:31)
                             U_LSO(L_LSO) = STR(34:38)
                             T_LSO(L_LSO) = STR(46:50)
                          ELSE IF ( STR(1:9) .EQ. 'SRC_STAT:' .AND. &
     &                        .NOT. ( FL_SRC_POST2021 .OR. FL_SRC_POST2024 ) ) THEN
                             C_LSO(L_LSO) = STR(12:19)
                             U_LSO(L_LSO) = ' '//STR(22:25)
                             T_LSO(L_LSO) = ' '//STR(32:35)
                             CALL CLRCH ( J_LSO(L_LSO) )
                          ELSE IF ( STR(6:6) .NE. ' ' ) THEN
                             C_LSO(L_LSO) = STR(6:13)
                             U_LSO(L_LSO) = STR(19:22)
                             T_LSO(L_LSO) = STR(24:27)
                             CALL CLRCH ( J_LSO(L_LSO) )
                          ELSE
                             C_LSO(L_LSO) = STR(8:15)
                             U_LSO(L_LSO) = STR(19:22)
                             T_LSO(L_LSO) = STR(29:32)
                             CALL CLRCH ( J_LSO(L_LSO) )
                        END IF
                        CALL CHIN ( U_LSO(L_LSO), NU_SOU )
                        CALL CHIN ( T_LSO(L_LSO), NT_SOU )
                        IF ( STR(1:9) .EQ. 'SRC_STAT:' .AND. FL_SRC_POST2024 ) THEN
                             SU_LSO(L_LSO) = STR(65:67)
                             ST_LSO(L_LSO) = STR(69:71)
                             IF ( SU_LSO(L_LSO) == '***' ) SU_LSO(L_LSO) = '999'
                             IF ( ST_LSO(L_LSO) == '***' ) ST_LSO(L_LSO) = '999'
                          ELSE
                             SU_LSO(L_LSO) = '  0'
                             ST_LSO(L_LSO) = '  0'
                        END IF
!
                        IP = LTM_DIF ( 1, L_SOU, C_SOU, C_LSO(L_LSO) )
                        IF ( IP .GT. 0 ) THEN
                             J_SOU(IP)    = J_LSO(L_LSO)
                             NOBU_SOU(IP) = NOBU_SOU(IP) + NU_SOU
                             NOBT_SOU(IP) = NOBT_SOU(IP) + NT_SOU
                             NSET_SOU(IP) = NSET_SOU(IP) + 1
                             IF ( NU_SOU .GT. 0 ) THEN
                                  IF ( NSEU_SOU(IP) .EQ. 0 ) THEN
                                       DAF_SOU(IP)  = DBNAME_DATE
                                       DAL_SOU(IP)  = DBNAME_DATE
                                  END IF
                                  NSEU_SOU(IP) = NSEU_SOU(IP) + 1
                                  IF ( DBNAME_DATE .LT. DAF_SOU(IP) ) THEN
                                       DAF_SOU(IP)  = DBNAME_DATE
                                  END IF
                                  IF ( DBNAME_DATE .GT. DAL_SOU(IP) ) THEN
                                       DAL_SOU(IP)  = DBNAME_DATE
                                  END IF
                             END IF
                        END IF
                   END IF
              END IF
!
! ----------- Get EOP
!
            IF ( FL_SEG_POST2005 ) THEN
!
! -------------- Modern, post 2005.08.18 style of spool-files
!
                 IF ( STR(6:18) .EQ. '. X Wobble  0' ) THEN
                        XEOP_VAL(N_SES) = STR(47:57)
                        XEOP_ERR(N_SES) = STR(86:95)
                        TAG(N_SES) = '##############'
                        CALL DATE_TO_TIME  ( STR(22:44), MJD_VAL, SEC_EOP, IER )
                        MJD_EOP(N_SES) = MJD_VAL + SEC_EOP/86400.0D0
                        IF ( FL_EOP_ADJ_ONLY ) THEN
                             XEOP_VAL(N_SES) = STR(64:74)
                        END IF
                    ELSE IF ( STR(6:18) .EQ. '. X Wobble  1' ) THEN
                        XREOP_VAL(N_SES) = STR(46:57)
                        XREOP_ERR(N_SES) = STR(86:95)
                        TAG(N_SES) = '##############'
                        CALL DATE_TO_TIME  ( STR(22:44), MJD_VAL, SEC_EOP, IER )
                        MJD_EOP(N_SES) = MJD_VAL + SEC_EOP/86400.0D0
                        IF ( FL_EOP_ADJ_ONLY ) THEN
                             XREOP_VAL(N_SES) = STR(64:74)
                        END IF
                     ELSE IF ( STR(6:18) .EQ. '. Y Wobble  0' ) THEN
                        YEOP_VAL(N_SES) = STR(47:57)
                        YEOP_ERR(N_SES) = STR(86:95)
                        TAG(N_SES) = '##############'
                        CALL DATE_TO_TIME  ( STR(22:44), MJD_VAL, SEC_EOP, IER )
                        MJD_EOP(N_SES) = MJD_VAL + SEC_EOP/86400.0D0
                        IF ( FL_EOP_ADJ_ONLY ) THEN
                             YEOP_VAL(N_SES) = STR(64:74)
                        END IF
                     ELSE IF ( STR(6:18) .EQ. '. Y Wobble  1' ) THEN
                        YREOP_VAL(N_SES) = STR(46:57)
                        YREOP_ERR(N_SES) = STR(86:95)
                        TAG(N_SES) = '##############'
                        CALL DATE_TO_TIME  ( STR(22:44), MJD_VAL, SEC_EOP, IER )
                        MJD_EOP(N_SES) = MJD_VAL + SEC_EOP/86400.0D0
                        IF ( FL_EOP_ADJ_ONLY ) THEN
                             YREOP_VAL(N_SES) = STR(64:74)
                        END IF
                     ELSE IF ( STR(6:18) .EQ. '. UT1-TAI   0' ) THEN
                        UEOP_VAL(N_SES) = STR(47:57)
                        UEOP_ERR(N_SES) = STR(86:95)
                        TAG(N_SES) = '##############'
                        CALL DATE_TO_TIME  ( STR(22:44), MJD_VAL, SEC_EOP, IER )
                        MJD_EOP(N_SES) = MJD_VAL + SEC_EOP/86400.0D0
                        IF ( FL_EOP_ADJ_ONLY ) THEN
                             UEOP_VAL(N_SES) = STR(64:74)
                        END IF
                     ELSE IF ( STR(6:18) .EQ. '. UT1-TAI   1' ) THEN
                        REOP_VAL(N_SES) = STR(47:57)
                        REOP_ERR(N_SES) = STR(86:95)
                        TAG(N_SES) = '##############'
                        CALL DATE_TO_TIME  ( STR(22:44), MJD_VAL, SEC_EOP, IER )
                        MJD_EOP(N_SES) = MJD_VAL + SEC_EOP/86400.0D0
                        IF ( FL_EOP_ADJ_ONLY ) THEN
                             REOP_VAL(N_SES) = STR(64:74)
                        END IF
                     ELSE IF ( STR(6:18) .EQ. '. UT1-TAI   2' ) THEN
                        QEOP_VAL(N_SES) = STR(47:57)
                        QEOP_ERR(N_SES) = STR(86:95)
                        TAG(N_SES) = '##############'
                        CALL DATE_TO_TIME  ( STR(22:44), MJD_VAL, SEC_EOP, IER )
                        MJD_EOP(N_SES) = MJD_VAL + SEC_EOP/86400.0D0
                        IF ( FL_EOP_ADJ_ONLY ) THEN
                             QEOP_VAL(N_SES) = STR(64:74)
                        END IF
                     ELSE IF ( STR(6:40)  .EQ. '. Nutation DPSI wrt   apriori model'  .AND.  &
     &                         NUT_USAGE  .EQ. 1  ) THEN
                        PEOP_VAL(N_SES) = STR(66:74)
                        PEOP_ERR(N_SES) = STR(80:89)
                        CALL DATE_TO_TIME  ( STR(42:64), MJD_VAL, SEC_NUT, IER )
                        MJD_NUT(N_SES) = MJD_VAL + SEC_EOP/86400.0D0
                     ELSE IF ( STR(6:40)  .EQ. '. Nutation DEPS wrt   apriori model'  .AND.  &
     &                         NUT_USAGE  .EQ. 1  ) THEN
                        EEOP_VAL(N_SES) = STR(66:74)
                        EEOP_ERR(N_SES) = STR(80:89)
                        CALL DATE_TO_TIME  ( STR(42:64), MJD_VAL, SEC_NUT, IER )
                        MJD_NUT(N_SES) = MJD_VAL + SEC_EOP/86400.0D0
                     ELSE IF ( STR(6:20)  .EQ. '  Nutation DPSI' .AND. &
     &                         NUT_USAGE  .EQ. 2 ) THEN
!
! -------------------- Nutation wrt to a reference model
!
                        PEOP_VAL(N_SES) = STR(66:74)
                        PEOP_ERR(N_SES) = STR(80:89)
                        CALL DATE_TO_TIME  ( STR(42:64), MJD_VAL, SEC_NUT, IER )
                        MJD_NUT(N_SES) = MJD_VAL + SEC_EOP/86400.0D0
                     ELSE IF ( STR(6:20)  .EQ. '  Nutation DEPS' .AND. &
     &                         NUT_USAGE  .EQ. 2 ) THEN
!
! --------------------- Nutation wrt to a reference model
!
                        EEOP_VAL(N_SES) = STR(66:74)
                        EEOP_ERR(N_SES) = STR(80:89)
                        CALL DATE_TO_TIME  ( STR(42:64), MJD_VAL, SEC_NUT, IER )
                        MJD_NUT(N_SES) = MJD_VAL + SEC_EOP/86400.0D0
                 END IF
               ELSE
!
! -------------- Pre August 2005 listing format
!
                 IF ( STR(6:18) .EQ. '. X Wobble  0' ) THEN
                        XEOP_VAL(N_SES) = STR(39:49)
                        XEOP_ERR(N_SES) = STR(78:87)
                        TAG(N_SES) = STR(22:35)
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(1:8) )
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(10:14) )
                     ELSE IF ( STR(6:18) .EQ. '. X Wobble  1' ) THEN
                        XREOP_VAL(N_SES) = STR(39:49)
                        XREOP_ERR(N_SES) = STR(78:87)
                        TAG(N_SES) = STR(22:35)
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(1:8) )
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(10:14) )
                     ELSE IF ( STR(6:18) .EQ. '. Y Wobble  0' ) THEN
                        YEOP_VAL(N_SES) = STR(39:49)
                        YEOP_ERR(N_SES) = STR(78:87)
                        TAG(N_SES) = STR(22:35)
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(1:8) )
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(10:14) )
                     ELSE IF ( STR(6:18) .EQ. '. Y Wobble  1' ) THEN
                        YREOP_VAL(N_SES) = STR(39:49)
                        YREOP_ERR(N_SES) = STR(78:87)
                        TAG(N_SES) = STR(22:35)
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(1:8) )
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(10:14) )
                     ELSE IF ( STR(6:18) .EQ. '. UT1-TAI   0' ) THEN
                        UEOP_VAL(N_SES) = STR(39:49)
                        UEOP_ERR(N_SES) = STR(78:87)
                        TAG(N_SES) = STR(22:35)
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(1:8) )
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(10:14) )
                     ELSE IF ( STR(6:18) .EQ. '. UT1-TAI   1' ) THEN
                        REOP_VAL(N_SES) = STR(39:49)
                        REOP_ERR(N_SES) = STR(78:87)
                        TAG(N_SES) = STR(22:35)
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(1:8) )
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(10:14) )
                     ELSE IF ( STR(6:18) .EQ. '. UT1-TAI   2' ) THEN
                        QEOP_VAL(N_SES) = STR(56:66)
                        QEOP_ERR(N_SES) = STR(84:93)
                        TAG(N_SES) = STR(22:35)
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(1:8) )
                        CALL BLANK_TO_ZERO  ( TAG(N_SES)(10:14) )
                     ELSE IF ( STR(6:15)  .EQ. '. Nutation' .AND. &
     &                         STR(37:41) .EQ. '(Psi)'      .AND. &
     &                         NUT_USAGE  .EQ. 1                  ) THEN
                        PEOP_VAL(N_SES) = STR(52:62)
                        PEOP_ERR(N_SES) = STR(68:77)
                     ELSE IF ( STR(6:15)  .EQ. '. Nutation' .AND. &
     &                         STR(37:41) .EQ. '(Eps)'      .AND. &
     &                         NUT_USAGE  .EQ. 1                  ) THEN
                        EEOP_VAL(N_SES) = STR(52:62)
                        EEOP_ERR(N_SES) = STR(68:77)
                     ELSE IF ( STR(1:26)  .EQ. '       Nutation offset wrt' .AND. &
     &                         STR(43:47) .EQ. '(Psi)'                      .AND. &
     &                         NUT_USAGE  .EQ. 2                            ) THEN
!
! -------------------- Nutation wrt to a reference model
!
                        CALL CLRCH ( PEOP_VAL(N_SES) )
                        CALL CLRCH ( PEOP_ERR(N_SES) )
                        PEOP_VAL(N_SES) = STR(52:62)
                        PEOP_ERR(N_SES) = STR(68:77)
                     ELSE IF ( STR(1:26)  .EQ. '       Nutation offset wrt'  .AND. &
     &                         STR(43:47) .EQ. '(Eps)'                       .AND. &
     &                         NUT_USAGE  .EQ. 2                                   ) THEN
!
! --------------------- Nutation wrt to a reference model
!
                        CALL CLRCH ( EEOP_VAL(N_SES) )
                        CALL CLRCH ( EEOP_ERR(N_SES) )
                        EEOP_VAL(N_SES) = STR(52:62)
                        EEOP_ERR(N_SES) = STR(68:77)
                     ELSE IF ( STR(1:22)  .EQ. ' EOP epoch (TDT)  MJD:' ) THEN
!
! --------------------- Get EOP and nutation epoch
!
                        READ ( UNIT=STR(24:35), FMT='(F12.5)' ) MJD_EOP(N_SES)
                        READ ( UNIT=STR(66:77), FMT='(F12.5)' ) MJD_NUT(N_SES)
                 END IF
              END IF
!
              IF ( STR(1:18) .EQ. ' EOP Correlations:' ) THEN
                   IND_EOPCOR = NR ! mark a tag
              END IF
!
              NCC = NR - IND_EOPCOR
!
              IF ( NCC .GE. 2  .AND.  NCC .LE. 8 ) THEN
!
! ---------------- Get EOP correlations and put them in CEOP
!
                   IS = ((NCC-1)*(NCC-2))/2
                   DO 470 J7=1,NCC-1
                      IS = IS + 1
                      IF ( SPOOL_FMT_DATE .GE. '2003.08.20' ) THEN
                           IB = 14 + (J7-1)*8
                           IE = 20 + (J7-1)*8
                           CEOP(IS,N_SES) = STR(IB:IB)//STR(IB+2:IE)
                         ELSE
                           IB = 15 + (J7-1)*8
                           IE = 20 + (J7-1)*8
                           CEOP(IS,N_SES) = STR(IB:IE)
                      END IF
 470               CONTINUE
              END IF
!
! ----------- Get local source positions: right ascension
!
              IF ( STR(6:6) .EQ. '.' .AND. INDEX ( STR, 'RT. ASC.' ) > 1 ) THEN
                   IND_LSO = NR ! mark a tag
                   N_LSO = N_LSO + 1
!
                   IF ( N_LSO .GT. M_LSO ) THEN
                        WRITE ( 6, * ) ' M_LSO = ',M_LSO
                        CALL ERR_LOG ( 4935, IUER, 'GETPAR_PARSE', &
     &                                'M_LSO is too small' )
                        RETURN
                   END IF
!
! ---------------- Initialization
!
                   CALL CLRCH ( LRA_VAL(N_LSO) )
                   CALL CLRCH ( LRA_ERR(N_LSO) )
                   CALL CLRCH ( LDL_VAL(N_LSO) )
                   CALL CLRCH ( LDL_ERR(N_LSO) )
                   CALL CLRCH ( LCR_VAL(N_LSO) )
!
                   LSO_NAME(N_LSO)   = STR(9:16)
                   LSO_SESIND(N_LSO) = N_SES
!
! ---------------- Reformat right ascension
!        
                   IF ( FL_SRC_POST2021 .OR. FL_SRC_POST2024 ) THEN
                        LRA_VAL(N_LSO) = STR(41:42)//'_'//STR(44:45)//'_'//STR(47:57)
                        CALL BLANK_TO_ZERO ( LRA_VAL(N_LSO) )
                        LRA_ERR(N_LSO) = STR(77:86)
                        J_NAME = STR(19:28)
                      ELSE
                        LRA_VAL(N_LSO) = STR(36:37)//'_'//STR(39:40)//'_'//STR(42:52)
                        CALL BLANK_TO_ZERO ( LRA_VAL(N_LSO) )
                        LRA_ERR(N_LSO) = STR(78:87)
                        IF ( LRA_ERR(N_LSO)(5:5) .EQ. ' ' ) LRA_ERR(N_LSO)(5:5) = '0'
                        CALL CLRCH ( J_NAME )
                   END IF
                   IL = LTM_DIF ( 1, L_LSO, C_LSO, LSO_NAME(N_LSO) )
                   IF ( IL .LE. 0 ) THEN
                        USO_VAL(N_LSO) = '    '
                        TSO_VAL(N_LSO) = '    '
                        USC_LSO(N_LSO) = '   '
                        TSC_LSO(N_LSO) = '   '
                      ELSE
                        USO_VAL(N_LSO) = U_LSO(IL)
                        TSO_VAL(N_LSO) = T_LSO(IL)
                        USC_LSO(N_LSO) = SU_LSO(IL)
                        TSC_LSO(N_LSO) = ST_LSO(IL)
                   END IF
                   LJO_NAME(N_LSO) = J_NAME
              END IF
!
! ----------- Get local source positions: Declination
!
              IF ( NR .EQ. IND_LSO + 1 ) THEN
!
! ---------------- Reformat declination
!
                   IF ( FL_SRC_POST2021 .OR. FL_SRC_POST2024 ) THEN
                        LDL_VAL(N_LSO) = STR(40:42)//'_'//STR(44:45)//'_'//STR(47:57)
                        CALL BLANK_TO_ZERO ( LDL_VAL(N_LSO) )
                        LDL_ERR(N_LSO) = STR(77:86)
                      ELSE
                        LDL_VAL(N_LSO) = STR(35:37)//'_'//STR(39:40)//'_'//STR(42:51)
                        CALL BLANK_TO_ZERO ( LDL_VAL(N_LSO) )
                        IF ( STR(89:94) == 'm-asec' ) THEN
                             LDL_ERR(N_LSO) = STR(78:87)
                           ELSE 
                            LDL_ERR(N_LSO) = STR(83:92)
                        END IF
                        IF ( LDL_ERR(N_LSO)(5:5) .EQ. ' ' ) LDL_ERR(N_LSO)(5:5) = '0'
                   END IF
              END IF
!
! ----------- Get local source positions: correlation
!
              IF ( NR .EQ. IND_LSO + 2 ) THEN
!
! ---------------- Reformat correlation
!
                   IF ( FL_SRC_POST2021 .OR. FL_SRC_POST2024 ) THEN
                        LCR_VAL(N_LSO) = STR(41:47)
                      ELSE
                        IF ( SPOOL_FMT_DATE .GE. '2003.08.20' ) THEN
                             LCR_VAL(N_LSO) = STR(33:39)
                          ELSE IF ( SPOOL_FMT_DATE .GE. '2002.12.24' ) THEN
                             LCR_VAL(N_LSO) = STR(34:34)//'0'//STR(35:39)
                          ELSE
                             LCR_VAL(N_LSO) = STR(77:77)//'0'//STR(78:82)
                        END IF
                   END IF
!
                   IF ( LCR_VAL(N_LSO)(3:3) .EQ. '*' ) THEN
                        LCR_VAL(N_LSO) = ' 0.9999'
                   END IF
                   IF ( LCR_VAL(N_LSO)(1:1) .EQ. '1' ) THEN
                        LCR_VAL(N_LSO) = ' 0.9999'
                   END IF
              END IF
!
! ----------- Get local station position: Set station name and index
!
              CALL CHIN ( STR(30:35), IP )
!
! ----------- Special trick of the case when this station might have an
! ----------- episodic site motion. We check the field where the date of the
! ----------- motion may be located.
!
              FL_EPIS = .FALSE. ! no episodic motuion
              IF ( IP .GT. 0   .AND.  IP .LE. 991231 ) FL_EPIS = .TRUE. ! episodic
              IC = INDEX ( COMP, STR(28:28) )
!
              IF ( STR(55:56) .EQ. 'mm'     .AND. &
     &             STR(73:74) .EQ. 'mm'     .AND. &
     &             STR(95:96) .EQ. 'mm'     .AND. &
     &             STR(28:28) .EQ. 'X'      .AND. &
     &             ( STR(30:33) .EQ. 'Comp'  .OR.  FL_EPIS ) ) THEN
!
                   IND_LST = NR ! mark a tag
                   N_LST = N_LST + 1
                   IF ( N_LST .GT. M_LST ) THEN
                        WRITE ( 6, * ) ' M_LST = ',M_LST
                        CALL ERR_LOG ( 4936, IUER, 'GETPAR_PARSE', 'M_LST is '// &
     &                                'too small' )
                        RETURN
                   END IF
                   LST_NAME(N_LST)   = STR(8:15)
                   LST_SESIND(N_LST) = N_SES
!
! ---------------- Initialization
!
                   DO 480 J8=1,6
                      CALL CLRCH ( CL_VAL(J8,N_LST) )
                      CALL CLRCH ( CL_ERR(J8,N_LST) )
 480               CONTINUE
              END IF
!
! ----------- Now get local station position and its formal uncertainty:
!
              IF ( STR(55:56) .EQ. 'mm'     .AND. &
     &             STR(73:74) .EQ. 'mm'     .AND. &
     &             STR(95:96) .EQ. 'mm'     .AND. &
     &             IC .GT. 0                .AND. &
     &             NR-IND_LST .LE. 5        .AND. &
     &             NR-IND_LST .GE. 0        .AND. &
     &             ( STR(30:33) .EQ. 'Comp'  .OR.  FL_EPIS ) ) THEN
!
                   CL_VAL(IC,N_LST) = STR(40:54)
                   CL_ERR(IC,N_LST) = STR(84:93)
              END IF
!
              IF ( STR(51:74) .EQ. '(mm)      Sigma     (mm)' ) THEN
!
! ---------------- Pre 2001.01.17 format of the line with global baseline
! ---------------- lengths
!
                   LB = .TRUE.
                   GOTO 410
              END IF
              IF ( STR(48:85) .EQ. 'Vector mag  a-sigma   Length   a-sigma' ) THEN
!
! ---------------- Post 2001.01.17 format of the line with global baseline
! ---------------- lengths
!
                   LB = .TRUE.
                   GOTO 410
              END IF
              IF ( LB ) THEN
                   IF ( STR(1:1) .NE. ' ' ) THEN
                        LB = .FALSE.
                   END IF
                   IF ( STR(2:8) .EQ. 'Warning' ) THEN
                        LB = .FALSE.
                   END IF
              END IF
!
              IF ( LB .AND. STR(2:2) .NE. ' ' .AND. STR(22:23) .EQ. 'to' ) THEN
!
! ---------------- Baseline length. Get estimats of the components of baseline
! ---------------- vector. Check whether this baseline was deselected from
! ---------------- estimation
!
                   IF ( L_NOB > 0 ) THEN
                        CNO_BAS(L_NOB) = STR(2:18)
                        BAS_STR = STR(2:9)//'-'//STR(25:32)
                        IP = LTM_DIF ( 0, L_NOB, CNO_BAS, BAS_STR )
                      ELSE 
                        IP = 0
                   END IF
                   IF ( IP < 1 ) THEN
                        L_BAS = L_BAS + 1
                        CALL CLRCH ( C_BAS(L_BAS) )
                        C_BAS(L_BAS) = DBNAME(N_SES)//' EPOCH: '//EPOCH(N_SES)// &
     &                                 ' '//STR(2:9)//'/'//STR(25:32)//' '// &
     &                                 STR(66:85)//' '//STR(86:95)//' '// &
     &                                 STR(97:102)//' '//STR(104:120)
                   END IF
              END IF
         END IF
!
         IF ( STR(16:20) == ' Atm ' .AND. STR(47:50) == 'Avg:' ) THEN
              L_TRS = L_TRS + 1
              C_TRS(L_TRS) = 'TRP_STS:  '//DBNAME(N_SES)//' '//STR(8:)
         END IF
!
         IF ( LS ) THEN
!
! ----------- Analyze the overall summary section. Get overall wrms of postfit
! ----------- residuals
!
              IF ( STR(1:10) .EQ. '   Delay  ' ) THEN
                   RMS_GLO_STR = 'Global:    '//STR(14:21)//'    '// &
     &                            STR(22:37)//'  Chi/ndg = '//STR(58:68)
                   CALL BLANK_TO_ZERO ( STR(67:67) )
              END IF
         END IF
         IF ( STR(94:99) == 'GloSou' ) THEN
!
! ----------- Get statistics from the global source section
!
              I_SOU = LTM_DIF ( 1, L_SOU, C_SOU, STR(6:13) )
              IF ( I_SOU > 0 ) THEN
                   SOU_MID_EPOCH(I_SOU) = STR(84:91)
                   USC_SOU(I_SOU) = STR(68:73)
                   TSC_SOU(I_SOU) = STR(76:81)
              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
      N_LIN = J1-1
!
      IF ( L_SOU .GT. 0 ) THEN
           DO 4100 J10=1,L_SOU
              CALL INCH ( NOBU_SOU(J10), OBU_SOU(J10) )
              CALL INCH ( NOBT_SOU(J10), OBT_SOU(J10) )
              CALL INCH ( NSEU_SOU(J10), SEU_SOU(J10) )
              CALL INCH ( NSET_SOU(J10), SET_SOU(J10) )
              CALL CHASHR ( OBU_SOU(J10) )
              CALL CHASHR ( OBT_SOU(J10) )
              CALL CHASHR ( SEU_SOU(J10) )
              CALL CHASHR ( SET_SOU(J10) )
 4100      CONTINUE
      END IF
!
      IF ( L_COO .GT. 0 ) THEN
           DO 4110 J11=1,L_COO
              CALL INCH ( NOBU_STA(J11), OBU_STA(J11) )
              CALL INCH ( NOBT_STA(J11), OBT_STA(J11) )
              CALL INCH ( NSEU_STA(J11), SEU_STA(J11) )
              CALL INCH ( NSET_STA(J11), SET_STA(J11) )
              CALL CHASHR ( OBU_STA(J11) )
              CALL CHASHR ( OBT_STA(J11) )
              CALL CHASHR ( SEU_STA(J11) )
              CALL CHASHR ( SET_STA(J11) )
 4110      CONTINUE
      END IF
!
! --- Sort source names list
!
      IF ( L_SOU .GT. 0 ) THEN
           CALL SORT_I2 ( L_SOU, ISRT_SOU, IND_SOU )
      END IF
      IF ( L_PRP .GT. 0 ) THEN
           CALL SORT_I2 ( L_PRP, ISRT_PRP, IND_PRP )
      END IF
!
! --- Sort RMS vlaues in decreasing wrms values
!
      IF ( N_SES .GT. 0 ) THEN
           CALL SORT8 ( N_SES, RMS_VAL, RMS_IND )
      END IF
      CLOSE ( UNIT=11 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GETPAR_PARSE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INIT_LOGICAL ( M_ARR, LOG_ARR, LOG_VAL )
      IMPLICIT   NONE
      INTEGER*4  M_ARR, J1
      LOGICAL*4  LOG_ARR(M_ARR), LOG_VAL
!
      DO 410 J1=1,M_ARR
         LOG_ARR(J1) = LOG_VAL
 410  CONTINUE
!
      RETURN
      END  SUBROUTINE  INIT_LOGICAL  !#!#
