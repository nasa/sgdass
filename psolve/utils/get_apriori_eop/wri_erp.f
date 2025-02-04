      SUBROUTINE WRI_ERP ( FILOUT, EXTFMT, URLEXT, URLEXT2, FILERP, NF, &
     &                   JDF, XF_VAL, YF_VAL, UF_VAL, XF_ERR, YF_ERR, UF_ERR, &
     &                   CH_FLAG, FL_ROT, FL_EQUWEI, JD_BEGROT, JD_ENDROT, &
     &                   XP_SH_VAL, XP_DR_VAL, &
     &                   YP_SH_VAL, YP_DR_VAL, &
     &                   U1_SH_VAL, U1_DR_VAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WRI_ERP  writes down EOP series, pole coordinates and UT1 *
! *   in erp-format (used by Calc and Solve as an Earht orientation      *
! *   mod-file) into the file FILOUT. It also writes doen in the header  *
! *   comments some additional information: names of the original files, *
! *   parameters of linear trend removed from original series and so on. *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  FILOUT    ( CHARACTER ) -- Name of the local output eop file in     *
! *                             erp modfile-format.                      *
! *  EXTFMT    ( CHARACTER ) -- Format of the externl EOP file.          *
! *                             One of IERS_C04 or USNO_FINALS.          *
! *  URLEXT    ( CHARACTER ) -- URL of the external EOP file.            *
! *  URLEXT2   ( CHARACTER ) -- URL of the second external EOP file,     *
! *                             which if define will overwrite EOP       *
! *                             from the first file. This name can be    *
! *                             left blank if not specified in the       *
! *                             control file.                            *
! *  FILERP    ( CHARACTER ) -- Name of the local reference erp file.    *
! *   FILOUT ( CHARACTER ) -- EOP file name                              *
! *       NP ( INTEGER*4 ) -- Number of points of EOP series.            *
! *       JD ( REAL*8    ) -- Array of Julian dates for EOP. Units: days,*
! *                           dimension: NP.                             *
! *   XF_VAL ( REAL*8    ) -- Array of X pole coordinates. Units: rad,   *
! *                           dimension: NP.                             *
! *   YF_VAL ( REAL*8    ) -- Array of Y pole coordinates. Units: rad,   *
! *                           dimension: NP.                             *
! *   UF_VAL ( REAL*8    ) -- Array of UT1-UTC angles. Units: rad,       *
! *                           dimension: NP.                             *
! *   XF_ERR ( REAL*8    ) -- Array of formal uncertainties of X pole    *
! *                           coordinates. Units: rad, dimension: NP.    *
! *   YF_ERR ( REAL*8    ) -- Array of formal uncertainties of Y pole    *
! *                           coordinates. Units: rad, dimension: NP.    *
! *   UF_ERR ( REAL*8    ) -- Array of formal uncertainties of UT1-UTC   *
! *                           angles. Units: rad, dimension: NP.         *
! *  CH_FLAG ( CHARACTER ) -- Flag of the data:                          *
! *                           'I' -- data derived from observations;     *
! *                           'P' -- predicted data.                     *
! *  FL_ROT  ( LOGICAL*4 ) -- Flag. If .TRUE. then reference file should *
! *                           be read, linear regression should be       *
! *                           computed and parameters of linear trend    *
! *                           should be subtracted from the output EOP   *
! *                           file.                                      *
! *  FL_EQUWEI ( LOGICAL*4 ) -- Flag. If .TRUE. then equal weights are   *
! *                             to be used for computation of trend.     *
! *                             If not, then the weights produced by     *
! *                             sum in quadrature of the reported formal *
! *                             errors in external file and in the       *
! *                             reference file should be used.           *
! *  JD_BEGROT ( REAL*8    ) -- Julian date of the first epoch which     *
! *                             should be used for computation of linear *
! *                             regression.                              *
! *  JD_ENDROT ( REAL*8    ) -- Julian date of the first epoch which     *
! *                             should be used for computation of linear *
! *                             regression.                              *
! *  XP_SH_VAL ( REAL*8    ) -- Shift of the external series of X pole   *
! *                             coordinates wrt reference series on the  *
! *                             reference epoch J2000.0 in arcsec.       *
! *  XP_DR_VAL ( REAL*8    ) -- Drift of the external series of X pole   *
! *                             coordinates wrt reference series in      *
! *                             arcsec/year.                             *
! *  YP_SH_VAL ( REAL*8    ) -- Shift of the external series of Y pole   *
! *                             coordinates wrt reference series on the  *
! *                             reference epoch J2000.0 in arcsec.       *
! *  YP_DR_VAL ( REAL*8    ) -- Drift of the external series of Y pole   *
! *                             coordinates wrt reference series in      *
! *                             arcsec/year.                             *
! *  U1_SH_VAL ( REAL*8    ) -- Shift of the external series of UT1      *
! *                             angles wrt reference series on the       *
! *                             reference epoch J2000.0 in sec.          *
! *  U1_DR_VAL ( REAL*8    ) -- Drift of the external series of UT1      *
! *                             angles wrt reference series in sec/year. *
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
! *  ### 07-NOV-2000    WRI_ERP    v1.3 (c)  L. Petrov  25-AUG-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INTEGER*4  NF, IUER
      CHARACTER  FILOUT*(*), EXTFMT*(*), URLEXT*(*), URLEXT2*(*), &
     &           FILERP*(*), CH_FLAG(NF)*(*)
      LOGICAL*4  FL_ROT, FL_EQUWEI
      REAL*8     JDF(NF), JD_BEGROT, JD_ENDROT, &
     &           XF_VAL(NF), XF_ERR(NF), YF_VAL(NF), YF_ERR(NF), &
     &           UF_VAL(NF), UF_ERR(NF)
      REAL*8     XP_SH_VAL, XP_DR_VAL, &
     &           YP_SH_VAL, YP_DR_VAL, &
     &           U1_SH_VAL, U1_DR_VAL
      CHARACTER  STR*75, DATE_FT*23, DATE_LP*23, DATE_LR*23, DATE_X*23, &
     &           DB_E*10, CH_BEGROT*23, CH_ENDROT*23
      CHARACTER  JD_TO_DATE*23, GET_CDATE*19
      CHARACTER  EOP_FORMAT*15, RESERVED_HEADER*15, MON(12)*3
      DATA       EOP_FORMAT      / 'EOP-MOD Ver 2.0' /
      DATA       RESERVED_HEADER / '               ' /
      DATA       MON &
     &           / &
     &             'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
     &             'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' &
     &           /
      REAL*8     JDL
      INTEGER*4  LUN, IO, J1, J2, IMON_X
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN, ILEN
!
! --- Open output erp-file
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 8231, IUER, 'WRI_ERP', 'Error in attempt to open '// &
     &          'the output ERP file '//FILOUT(1:I_LEN(FILOUT))//'  IO='// &
     &           STR )
           RETURN
      END IF
!
! --- find the latest date when real ( not-predicted ) data wre used
!
      JDL = JDF(1)
      DO 410 J1=1,NF
         IF ( CH_FLAG(J1)(1:1) .EQ. 'I' ) JDL = JDF(J1)
 410  CONTINUE
!
! --- Write the first line of the output erp-file
!
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR, FMT=110, IOSTAT=IO ) EOP_FORMAT, JDF(1), 1.0, NF, &
     &        'UT1-TAI ', 'UNDEF    ', RESERVED_HEADER
 110  FORMAT( A15, 2X, F9.1, 2X, F4.1, 2X, I5, 2X, A8, 2X, A8, A15 )
      WRITE ( LUN, '(A)' ) STR
!
! --- Write header of mod file.
!
      DATE_FT = JD_TO_DATE ( JDF(1),  -3 )
      DATE_LP = JD_TO_DATE ( JDF(NF), -3 )
      DATE_LR = JD_TO_DATE ( JDL,     -3 )
!
      CALL CLRCH ( STR )
      IF ( EXTFMT(1:8) .EQ. 'IERS_C04' ) THEN
           STR = '# Earth orientation parameters series from IERS C04 series   '
         ELSE IF ( EXTFMT(1:11) .EQ. 'USNO_FINALS' ) THEN
           STR = '# Earth orientation parameters series from finals USNO series'
      END IF
      WRITE ( LUN, '(A)' ) STR
!
      CALL CLRCH ( STR )
      STR = '# Original data were taken from '//URLEXT
      WRITE ( LUN, '(A)' ) STR
!
      IF ( ILEN(URLEXT2) .GT. 0 ) THEN
           STR = '# and replaced with '//URLEXT2
           WRITE ( LUN, '(A)' ) STR
      END IF
!
      IF( FL_ROT ) THEN
          CALL CLRCH ( STR )
          STR = '# and then linear regerssion was subtracted from those series '
          WRITE ( LUN, '(A)' ) STR
!
          CALL CLRCH ( STR )
          STR = '# in order to eliminate shift and drift with respect to '// &
     &          'series '
          WRITE ( LUN, '(A)' ) STR
!
          CH_BEGROT = JD_TO_DATE ( JD_BEGROT, -3 )
          CH_ENDROT = JD_TO_DATE ( JD_ENDROT, -3 )
          CALL CLRCH ( STR )
          STR = '# Linear regression was computed on the interval [ '// &
     &           CH_BEGROT(1:10)//' , '//CH_ENDROT(1:10)//' ]'
          WRITE ( LUN, '(A)' ) STR
!
          CALL CLRCH ( STR )
          IF ( FL_EQUWEI ) THEN
               STR = '# Equal weights were used'
              ELSE
               STR = '# Weights from these files were used'
          END IF
          WRITE ( LUN, '(A)' ) STR
!
          CALL CLRCH ( STR )
          STR = '# '//FILERP
          WRITE ( LUN, '(A)' ) STR
!
          CALL CLRCH ( STR )
          STR = '# '
          WRITE ( LUN, '(A)' ) STR
!
          CALL CLRCH ( STR )
          WRITE ( UNIT=STR, FMT=120 ) 'X_pole:', &
     &                                 XP_SH_VAL*RAD__TO__MAS, 'mas', &
     &                                 XP_DR_VAL*RAD__TO__MAS, 'mas/year'
 120      FORMAT ( '# ',A,'  shift = ',F7.3,' ',A,'     drift = ',F7.3,' ',A, &
     &             '  at epoch J2000' )
          WRITE ( LUN, '(A)' ) STR
!
          CALL CLRCH ( STR )
          WRITE ( UNIT=STR, FMT=120 ) 'Y_pole:', &
     &                                 YP_SH_VAL*RAD__TO__MAS, 'mas', &
     &                                 YP_DR_VAL*RAD__TO__MAS, 'mas/year'
          WRITE ( LUN, '(A)' ) STR
!
          CALL CLRCH ( STR )
          WRITE ( UNIT=STR, FMT=120 ) 'UT1:   ', &
     &                                 U1_SH_VAL*RAD__TO__MAS, 'ms ', &
     &                                 U1_DR_VAL*RAD__TO__MAS, 'ms/year '
          WRITE ( LUN, '(A)' ) STR
      END IF
!
      CALL CLRCH ( STR )
      STR = '# '
      WRITE ( LUN, '(A)' ) STR
!
      CALL CLRCH ( STR )
      STR = '# File was created at '//CENTER_ABR//' ( '//CENTER_FULL_NAME// &
     &      ' ) '
      WRITE ( LUN, '(A)' ) STR
!
      CALL CLRCH ( STR )
      STR = '# File was created on '//GET_CDATE()
      WRITE ( LUN, '(A)' ) STR
!
      CALL CLRCH ( STR )
      STR = '# '
      WRITE ( LUN, '(A)' ) STR
!
      CALL CLRCH ( STR )
      STR = '# First date:                '//DATE_FT(1:10)
      WRITE ( LUN, '(A)' ) STR
!
      CALL CLRCH ( STR )
      STR = '# Last date with real data:  '//DATE_LR(1:10)
      WRITE ( LUN, '(A)' ) STR
!
      IF ( EXTFMT(1:11) .EQ. 'USNO_FINALS' ) THEN
           CALL CLRCH ( STR )
           STR = '# Last date with prediction: '//DATE_LP(1:10)
           WRITE ( LUN, '(A)' ) STR
      END IF
!
! --- Format date of the last session which is good for processing
!
      DATE_X = JD_TO_DATE ( JDF(NF)-8.D0, -3 )
      CALL BLANK_TO_ZERO ( DATE_X )
      CALL CHIN ( DATE_X(6:7),  IMON_X  )
      IF ( IMON_X .GE. 1  .AND.  IMON_X .LE. 12 ) THEN
           DB_E = '$'//DATE_X(3:4)//MON(IMON_X)//DATE_X(9:10)//'zz'
         ELSE
           DB_E = '$?????????'
      END IF
      CALL CLRCH ( STR )
      STR = '# Good for processing an experiment not after '//DB_E
      WRITE ( LUN, '(A)' ) STR
!
      CALL CLRCH ( STR )
      STR = '# '
      WRITE ( LUN, '(A)' ) STR
!
      WRITE ( LUN, '(A)' ) '#         Format descrpition                                               '
      WRITE ( LUN, '(A)' ) '#                                                                          '
      WRITE ( LUN, '(A)' ) '# File in EOP-MOD file consists of records of fixed length of 76 bytes     '
      WRITE ( LUN, '(A)' ) '#                                                                          '
      WRITE ( LUN, '(A)' ) '#   A record can be of three types                                         '
      WRITE ( LUN, '(A)' ) '# a) header record                                                         '
      WRITE ( LUN, '(A)' ) '# b) comment records                                                       '
      WRITE ( LUN, '(A)' ) '# c) data record                                                           '
      WRITE ( LUN, '(A)' ) '#                                                                          '
      WRITE ( LUN, '(A)' ) '# Header record:                                                           '
      WRITE ( LUN, '(A)' ) '#                                                                          '
      WRITE ( LUN, '(A)' ) '#   1:15 --  A15   magic                                                   '
      WRITE ( LUN, '(A)' ) '#  18:26 --  F9.1  Julian date in TAI of the first epoch                   '
      WRITE ( LUN, '(A)' ) '#  28:32 --  F5.1  Step in days                                            '
      WRITE ( LUN, '(A)' ) '#  35:39 --  I5    total number of records                                 '
      WRITE ( LUN, '(A)' ) '#                                                                          '
      WRITE ( LUN, '(A)' ) '# Comment record                                                           '
      WRITE ( LUN, '(A)' ) '#                                                                          '
      WRITE ( LUN, '(A)' ) '#   A record with starting # is considered as comment. Comment records     '
      WRITE ( LUN, '(A)' ) '# are allowed only after the header record and before any data record      '
      WRITE ( LUN, '(A)' ) '#                                                                          '
      WRITE ( LUN, '(A)' ) '# Data record                                                              '
      WRITE ( LUN, '(A)' ) '#                                                                          '
      WRITE ( LUN, '(A)' ) '#   1:9  --  F9.1  Julian date in days, in TAI                             '
      WRITE ( LUN, '(A)' ) '#  11:18 --  F7.4  X pole coordinate in 0.1 arcsec                         '
      WRITE ( LUN, '(A)' ) '#  19:25 --  F7.4  Y pole coordinate in 0.1 arcsec                         '
      WRITE ( LUN, '(A)' ) '#  27:35 --  I9    UT1-TAI in microseconds of time                         '
      WRITE ( LUN, '(A)' ) '#  37:42 --  F6.4  X pole uncertainty in 0.1 arcsec                        '
      WRITE ( LUN, '(A)' ) '#  44:49 --  F6.4  Y pole uncertainty in 0.1 arcsec                        '
      WRITE ( LUN, '(A)' ) '#  51:57 --  F7.0  UT1-TAI uncertainty in microseconds of time             '
      WRITE ( LUN, '(A)' ) '#  59:63 --  F5.3  correlation between X and Y pole coordinates (not used) '
      WRITE ( LUN, '(A)' ) '#  65:69 --  F5.3  correlation between X pole coordinate and UT1 (not used '
      WRITE ( LUN, '(A)' ) '#  71:75 --  F5.3  correlation between Y pole coordinate and UT1 (not used '
      WRITE ( LUN, '(A)' ) '#                                                                          '
!
      DO 420 J2=1,NF
         CALL CLRCH ( STR )
         WRITE ( UNIT=STR, FMT=130, IOSTAT=IO ) JDF(J2), &
     &           XF_VAL(J2)*10.D0*RAD__TO__ARCSEC, &
     &           YF_VAL(J2)*10.D0*RAD__TO__ARCSEC, &
     &           NINT ( UF_VAL(J2)*1.D6*RAD__TO__SEC + 0.01D0 ), &
     &           MAX( XF_ERR(J2)*10.0D0*RAD__TO__ARCSEC, 0.0001D0 ), &
     &           MAX( YF_ERR(J2)*10.0D0*RAD__TO__ARCSEC, 0.0001D0 ), &
     &           MAX( UF_ERR(J2)*1.0D6*RAD__TO__SEC,  1.0D0    )
 130     FORMAT ( F9.1, 1X, F7.4, 1X, F7.4, 1X, I9, 1X, F6.4, 1X, F6.4, &
     &            1X, F7.0, '  .000  .000  .000' )
!
         IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              CALL ERR_LOG ( 8232, IUER, 'WRI_ERP', 'Error in attempt to '// &
     &            'write the output ERP file '//FILOUT(1:I_LEN(FILOUT))// &
     &            '  IO='//STR )
              RETURN
         END IF
         WRITE ( LUN, '(A)', IOSTAT=IO ) STR
 420  CONTINUE
!
      CLOSE ( UNIT=LUN )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRI_ERP  #!#
