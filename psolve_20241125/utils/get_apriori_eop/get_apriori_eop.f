      PROGRAM    GET_APRIORI_EOP
! ************************************************************************
! *                                                                      *
! *   Program  GET_APRIORI_EOP  retrieves the file with apriori, either  *
! *   file finals.all from the United States Naval Observatory (USNO) or *
! *   IERS C04 from the International Earth Rotation Service using       *
! *   program wget for automatic files retrieval via ftp, (optionally)   *
! *   finds the differences between the retrieved EOP series and the EOP *
! *   series (in erp format) used by Solve for the Earth orientation     *
! *   mod-files, finds parameters of linear regression of the            *
! *   differences in UT1, X pole coordinates, Y pole coordinates, then   *
! *   (optionally) subtracts parameters of linear regression from the    *
! *   external EOP series of UT1 and polar motion. Finally, the          *
! *   subroutine finals_to_erp re-formats resulting EOP file to          *
! *   1) ut1pm.dat and to 2) erp-format for Calc and Solve for using it  *
! *   as an Earth orientation mod-file.                                  *
! *                                                                      *
! *   Program require parameter: configuration file.                     *
! *                                                                      *
! *   Configuration file contains records of three type:                 *
! *   1) comments: any line which beginning from ##                      *
! *   2) directives for get_priori_eop: the line which starts from       *
! *   # and which is not a comment line.                                 *
! *   Directive consists of three or more words separated by one or more *
! *   blanks.                                                            *
! *                                                                      *
! *   Word1 -- symbol # -- directive attribute                           *
! *   Word2 -- keyword                                                   *
! *   Word3,4...  value(s) of the keyword.                               *
! *                                                                      *
! *   Configuration file should contain definition of all keywords and   *
! *   all variables listed in the next subsection.                       *
! *                                                                      *
! *   Descriptions of directives of get_apriori_eop configuration file.  *
! *   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  *
! *                                                                      *
! *  EXTFMT:   Format of the external file. One of USNO_FINALS or        *
! *            IERS_C04                                                  *
! *  URLEXT:   URL of the external file which is to be browsed.          *
! *  URLEXT2:  URL of the second external file which is to be browsed.   *
! *            The external EOP series may be represented by two files.  *
! *            The second file overwrites the values defined in the      *
! *            first. The values defined only in the second file are     *
! *            kept untouched. THe length of the output file is          *
! *            determined by the first file. If the EOP should be        *
! *            represented with only one file, this field should be NONE.*
! *  FILEXT:   local name of the external file after it is browsed to    *
! *            the local machine.                                        *
! *  FILEXT2:  local name of the second external file after it is        *
! *            browsed to the local machine. If URLEXT2 is NONE, then    *
! *            FILEXT2 should be NONE as well.                           *
! *  FILERP:   File name of the reference erp file. This erp file should *
! *            be in erp modfile format.                                 *
! *  FILOUT:   local name of the output file in erp modfile format.      *
! *  FILUPM:   local name of the output file in binary ut1pm format.     *
! *            Program Calc, dbedit and apriori can use files in this    *
! *            format.                                                   *
! *  WGET_EXE: Filename with path of program wget. Program wget should   *
! *            be installed before calling get_apriori_eop.              *
! *  FL_ROT:   Flag whether to apply transformation of the external      *
! *            file. Value is one of TRUE or FALSE.                      *
! *  ROT_FROM: Date of the left boundary of the dates range for which    *
! *            computation of linear regression is done. The right       *
! *            boundary is the date of the last observation which has    *
! *            been reference used for deriving reference EOP series.    *
! *            Format: yyyy.dd.mm, for example 2000.01.29 for            *
! *            January 29, year 2000.                                    *
! *  WEIGHTS:  Flag: which weights should be used for computation of     *
! *            regression coefficients. One of EQUAL or IN are allowed.  *
! *            EQUAL means that all weighs are 1. IN means that the      *
! *            weight is 1/DSQRT ( sig_x**2 + sig_r**2 ) where sig_f     *
! *            stands for formal uncertainty of external EOP and sig_r   *
! *            stands for formal uncertainty of reference EOP.           *
! *                                                                      *
! * ### 02-NOV-2000  GET_APRIORI_EOP v4.0 (c) L. Petrov 01-APR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FILCNF*128, EXTFMT*128, FILEXT*128, FILERP*128, FILOUT*128, &
     &           URLEXT*128, FILEXT2*128, URLEXT2*128, FILUPM*128, WGET_EXE*128
      INTEGER*4  MP, IUER
      PARAMETER  ( MP = 16384 )
      REAL*8     XF_VAL(MP), YF_VAL(MP), UF_VAL(MP), PSIF_VAL(MP), EPSF_VAL(MP), &
     &           XF_ERR(MP), YF_ERR(MP), UF_ERR(MP), PSIF_ERR(MP), EPSF_ERR(MP), &
     &           XR_VAL(MP), YR_VAL(MP), UR_VAL(MP), &
     &           XR_ERR(MP), YR_ERR(MP), UR_ERR(MP), &
     &           JDF(MP), JDR(MP), JD_BEGROT, JD_ENDROT, &
     &           XP_SH_VAL, XP_DR_VAL, XP_SH_ERR, XP_DR_ERR, &
     &           YP_SH_VAL, YP_DR_VAL, YP_SH_ERR, YP_DR_ERR, &
     &           U1_SH_VAL, U1_DR_VAL, U1_SH_ERR, U1_DR_ERR, &
!
     &           JDF2(MP), XF_VAL2(MP), XF_ERR2(MP), YF_VAL2(MP), &
     &           YF_ERR2(MP), UF_VAL2(MP), UF_ERR2(MP), PSIF_VAL2(MP), &
     &           PSIF_ERR2(MP), EPSF_VAL2(MP), EPSF_ERR2(MP)
      LOGICAL*4  FL_ROT, FL_EQUWEI
      CHARACTER  CH_FLAG(MP)*3, CH_FLAG2(MP)*3
      INTEGER*4  IP1, J1, MJD_BEGROT, NF, NF2, NP_SHIFT, NR, NUMARG
#ifdef INTEL
      INTEGER*4, EXTERNAL :: IARGC
#endif
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      INCLUDE 'get_apriori_eop_version.i' ! Set revision date of the current version
!
! --- Initialization
!
      XP_SH_VAL = 0.0D0
      XP_DR_VAL = 0.0D0
      YP_SH_VAL = 0.0D0
      YP_DR_VAL = 0.0D0
      U1_SH_VAL = 0.0D0
      U1_DR_VAL = 0.0D0
!
! --- Get run-time argument -- name of the configuration file
!
      CALL CLRCH ( FILCNF   )
      NUMARG = IARGC ()
      IF ( NUMARG .GE. 1 ) THEN
           CALL GETARG ( 1, FILCNF )
        ELSE
           WRITE ( 6, 110 )
 110       FORMAT ( 1X,'Usage:  get_apriori_erp <configuration_file>' )
           CALL EXIT ( 127 )
      END IF
!
! --- Parse configuration file
!
      IUER  = -1
      CALL CONF_GAE ( FILCNF, EXTFMT, URLEXT, URLEXT2, FILEXT, FILEXT2, &
     &                FILERP, FILOUT, FILUPM, WGET_EXE, FL_ROT, FL_EQUWEI, &
     &                MJD_BEGROT, IUER )
      JD_BEGROT = 2400005.D0 + MJD_BEGROT
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 8201, -1, 'GET_APRIORI_EOP', 'Error in attempt to '// &
     &         'parse configuration file '//FILCNF )
           CALL EXIT ( 1 )
      END IF
!
! --- Get external EOP file via ftp by using wget
!
      IUER  = -1
      CALL GET_FTP_EOP ( URLEXT, FILEXT, WGET_EXE, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 8202, -1, 'GET_APRIORI_EOP', 'Error in '// &
     &         'attempt to retrieve original fils with EOP via ftp using '// &
     &         'program wget' )
           CALL EXIT ( 2 )
      END IF
!
      IF ( ILEN(URLEXT2) > 0 ) THEN
           IF ( ILEN(FILEXT2) == 0 ) THEN
                CALL ERR_LOG ( 8203, -1, 'GET_APRIORI_EOP', 'Error in '// &
     &              'configuration: URLEXT2 is specitied, but FILEXT2 '// &
     &              'is not specified' )
                CALL EXIT ( 3 )
           END IF
! 
           IUER  = -1
           CALL GET_FTP_EOP ( URLEXT2, FILEXT2, WGET_EXE, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8204, -1, 'GET_APRIORI_EOP', 'Error in '// &
     &              'attempt to retrieve original fils with EOP via ftp '// &
     &              'using program wget' )
                CALL EXIT ( 4 )
           END IF
      END IF
!
      IF ( EXTFMT(1:8) .EQ. 'IERS_C04' ) THEN
!
! -------- Parse iers c04 file
!
           IUER  = -1
           CALL RD_IERS_C04 ( FILEXT, MP, NF, JDF, XF_VAL, XF_ERR, YF_VAL, &
     &                        YF_ERR, UF_VAL, UF_ERR, PSIF_VAL, PSIF_ERR, &
     &                        EPSF_VAL, EPSF_ERR, CH_FLAG, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8205, -1, 'GET_APRIORI_EOP', 'Error in '// &
     &              'attempt to read original file eop file IERS C04' )
                CALL EXIT ( 5 )
           END IF
         ELSE IF ( EXTFMT(1:11) .EQ. 'USNO_FINALS' ) THEN
!
! -------- Parse finals.data file
!
           IUER  = -1
           CALL RD_FINALS ( FILEXT, MP, NF, JDF, XF_VAL, XF_ERR, YF_VAL, &
     &                      YF_ERR, UF_VAL, UF_ERR, PSIF_VAL, PSIF_ERR, &
     &                      EPSF_VAL, EPSF_ERR, CH_FLAG, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8206, -1, 'GET_APRIORI_EOP', 'Error in '// &
     &              'attempt to read original file with EOP from USNO '// &
     &               FILEXT )
                CALL EXIT ( 6 )
           END IF
!
           IF ( ILEN(FILEXT2) > 0 ) THEN
!
! ------------- Read the second USNO file in final format
!
                IUER  = -1
                CALL RD_FINALS ( FILEXT2, MP, NF2, JDF2, XF_VAL2, XF_ERR2, &
     &                           YF_VAL2, YF_ERR2, UF_VAL2, UF_ERR2, &
     &                           PSIF_VAL2, PSIF_ERR2, EPSF_VAL2, EPSF_ERR2, &
     &                           CH_FLAG2, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8207, -1, 'GET_APRIORI_EOP', 'Error in '// &
     &                   'attempt to read original file with EOP from USNO '// &
     &                    FILEXT )
                     CALL EXIT ( 7 )
                END IF
!
! ------------- Find the shift of the second file with respect to the
! ------------- first one
!
                NP_SHIFT = IDNINT ( JDF2(1) - JDF(1) )
!
! ------------- Replace the values of EOP with the values of the second file
!
                DO 410 J1=1,NF2
                   IP1 = J1 + NP_SHIFT
                   IF ( IP1 > NF ) GOTO 410
                   XF_VAL(IP1) = XF_VAL2(J1) 
                   XF_ERR(IP1) = XF_ERR2(J1) 
                   YF_VAL(IP1) = YF_VAL2(J1) 
                   YF_ERR(IP1) = YF_ERR2(J1) 
                   UF_VAL(IP1) = UF_VAL2(J1) 
                   UF_ERR(IP1) = UF_ERR2(J1) 
                   PSIF_VAL(IP1) = PSIF_VAL2(J1) 
                   PSIF_ERR(IP1) = PSIF_ERR2(J1) 
                   EPSF_VAL(IP1) = EPSF_VAL2(J1) 
                   EPSF_ERR(IP1) = EPSF_ERR2(J1) 
                   CH_FLAG(IP1)  = CH_FLAG2(J1)  
 410            CONTINUE 
           END IF
      END IF
!
      IF ( FL_ROT ) THEN
!
! -------- Read a benchmark ERP file and extract the series of pole coordinates
! -------- and UT1
!
           IUER = -1
           CALL RD_ERP ( FILERP, MP, NR, JDR, XR_VAL, YR_VAL, UR_VAL, &
     &                   XR_ERR, YR_ERR, UR_ERR, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8208, -1, 'GET_APRIORI_EOP', 'Error in '// &
     &              'attempt to read input erp-file ' )
                CALL EXIT ( 8 )
           END IF
!
! -------- Compute the differences between the external EOP series and the
! -------- benchmark EOP series
!
           IUER = -1
           CALL DIFF_ERP ( NR, JDR, XR_VAL, YR_VAL, UR_VAL, XR_ERR, YR_ERR, &
     &                     UR_ERR, &
     &             NF, JDF, XF_VAL, XF_ERR, YF_VAL, YF_ERR, UF_VAL, UF_ERR, &
     &             XP_SH_VAL, XP_DR_VAL, XP_SH_ERR, XP_DR_ERR, &
     &             YP_SH_VAL, YP_DR_VAL, YP_SH_ERR, YP_DR_ERR, &
     &             U1_SH_VAL, U1_DR_VAL, U1_SH_ERR, U1_DR_ERR, &
     &             CH_FLAG, JD_BEGROT, JD_ENDROT, FL_ROT, FL_EQUWEI, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8209, -1, 'GET_APRIORI_EOP', 'Error in '// &
     &              'attempt to compute differences between the external '// &
     &              'EOP series and the EOP series in the input erp-file' )
                CALL EXIT ( 9 )
           END IF
      END IF
!
! --- Write down erp-file
!
      IUER = -1
      CALL WRI_ERP ( FILOUT, EXTFMT, URLEXT, URLEXT2, FILERP, NF, JDF, XF_VAL, &
     &               YF_VAL, UF_VAL, XF_ERR, YF_ERR, UF_ERR, CH_FLAG, FL_ROT, &
     &               FL_EQUWEI, JD_BEGROT, JD_ENDROT, XP_SH_VAL, XP_DR_VAL, &
     &               YP_SH_VAL, YP_DR_VAL, U1_SH_VAL, U1_DR_VAL, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 8210, -1, 'GET_APRIORI_EOP', 'Error in attempt to '// &
     &         'write down the output eop modfile' )
           CALL EXIT ( 10 )
      END IF
!
! --- Write down ut1pm.dat file
!
      IUER = -1
      CALL WRI_UPM ( FILUPM, NF, JDF, XF_VAL, YF_VAL, UF_VAL, XF_ERR, YF_ERR, &
     &               UF_ERR, CH_FLAG, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 8211, -1, 'GET_APRIORI_EOP', 'Error in attempt to '// &
     &         'write down the output file '//FILUPM )
           CALL EXIT ( 11 )
      END IF
!
      END  !#!  GET_APRIORI_EOP  #!#
