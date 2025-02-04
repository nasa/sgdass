      SUBROUTINE VTD_APR_SPOOL ( LUN, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_APR_SPOOL 
! *                                                                      *
! * ### 08-NOV-2006  VTD_APR_SPOOL  v1.5 (c)  L. Petrov  10-JUL-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  LUN, IUER
      REAL*8     SOU_CAT_EPO 
      INTEGER*4  J1, J2, J3
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      WRITE ( LUN, '(A,A)' ) '1  APR  APRIORI_STYLE:       ', 'VTD'
      WRITE ( LUN, '(A,A)' ) '1  APR  CONFIG_FINAL:        ', VTD%CONF%CONFIG_FINAM(1:I_LEN(VTD%CONF%CONFIG_FINAM))
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_REVISION:        ', VTD__LABEL
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_STA_DEF:         ', &
     &               VTD%CONF%FINAM_STADESC(1:I_LEN(VTD%CONF%FINAM_STADESC))
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_FINAM_AEM:       ', &
     &               VTD%CONF%FINAM_AEM(1:I_LEN(VTD%CONF%FINAM_AEM))
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_FINAM_ERM:       ', &
     &               VTD%CONF%FINAM_ERM(1:I_LEN(VTD%CONF%FINAM_ERM))
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_FINAM_HEO:       ', &
     &               VTD%CONF%FINAM_HEO(1:I_LEN(VTD%CONF%FINAM_HEO))
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_FINAM_EOP:       ', &
     &               VTD%CONF%FINAM_EOP(1:I_LEN(VTD%CONF%FINAM_EOP))
      IF ( VTD%CONF%EOP_TIME_SCALE == VTD__TAI ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  EOP_TIME_SCALE:      ', &
     &               'TAI'
         ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__TDT ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  EOP_TIME_SCALE:      ', &
     &               'TDT'
         ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__TDB ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  EOP_TIME_SCALE:      ', &
     &               'TDB'
         ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__UTC ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  EOP_TIME_SCALE:      ', &
     &               'UTC'
         ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__UT1 ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  EOP_TIME_SCALE:      ', &
     &               'UT1'
         ELSE IF ( VTD%CONF%EOP_TIME_SCALE == VTD__UNDF ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  EOP_TIME_SCALE:      ', &
     &               'UNDEFINED'
         ELSE 
           WRITE ( LUN, '(A,A)' ) '1  APR  EOP_TIME_SCALE:      ', &
     &               'Unknown'
      END IF
!
      IF ( VTD%CONF%UZT_MODEL == UZT__DICKMAN1993 ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  UZT_MODEL:           ', &
     &             'DICKMAN1993'
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__DICKMAN_PRINCIPLE ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  UZT_MODEL:           ', &
     &             'DICKMAN_PRINCIPLE'
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__DICKMAN_SHORT ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  UZT_MODEL:           ', &
     &             'DICKMAN_SHORT'
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__RE2014 ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  UZT_MODEL:           ', &
     &             'RE2014'
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__RE2014_SHORT ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  UZT_MODEL:           ', &
     &             'RE2014_SHORT'
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__NONE ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  UZT_MODEL:           ', &
     &             'NONE'
         ELSE 
           WRITE ( LUN, '(A,A)' ) '1  APR  UZT_MODEL:           ', &
     &               'Unknown'
      END IF
!
      IF ( VTD%CONF%UZT_USE == UZT__ADD ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  UZT_MODEL:           ', &
     &             'ADD'
         ELSE IF ( VTD%CONF%UZT_USE == UZT__SUBTRACT ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  UZT_MODEL:           ', &
     &             'SUBTRACT'
         ELSE IF ( VTD%CONF%UZT_USE == UZT__NONE ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  UZT_MODEL:           ', &
     &             'NONE'
         ELSE 
           WRITE ( LUN, '(A,A)' ) '1  APR  UZT_MODEL:           ', &
     &             'Unknown'
      END IF
!
      IF ( VTD%CONF%PREC_EXP == PREC__LIESKE1977 ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  PRECESSION_EXPRESS.: ', &
     &             'LIESKE_1977'
        ELSE IF ( VTD%CONF%PREC_EXP == PREC__SIMON1994 ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  PRECESSION_EXPRESS.: ', &
     &             'SIMON_1994'
        ELSE IF ( VTD%CONF%PREC_EXP == PREC__IERS1996 ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  PRECESSION_EXPRESS.: ', &
     &             'IERS_1996'
        ELSE IF ( VTD%CONF%PREC_EXP == PREC__CAPITAINE2003 ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  PRECESSION_EXPRESS.: ', &
     &             'CAPITAINE_2003'
        ELSE IF ( VTD%CONF%PREC_EXP == PREC__CAPITAINE2005 ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  PRECESSION_EXPRESS.: ', &
     &             'CAPITAINE_2005'
        ELSE 
           WRITE ( LUN, '(A,A,1X,I12)' ) '1  APR  PRECESSION_EXPRESS.: ', &
     &             'Unknown', VTD%CONF%PREC_EXP 
      END IF
!
      IF ( VTD%CONF%NUT_EXP == NUT__WAHR1980 ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  NUTATION_EXPRESSION: ', &
     &             'WAHR_1980'
        ELSE IF ( VTD%CONF%NUT_EXP == NUT__IERS1996 ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  NUTATION_EXPRESSION: ', &
     &             'IERS_1996'
        ELSE IF ( VTD%CONF%NUT_EXP == NUT__REN2000  ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  NUTATION_EXPRESSION: ', &
     &             'REN_2000'
        ELSE IF ( VTD%CONF%NUT_EXP == NUT__MHB2000  ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  NUTATION_EXPRESSION: ', &
     &             'MHB_2000'
        ELSE IF ( VTD%CONF%NUT_EXP == NUT__MHB2000_TRANSF ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  NUTATION_EXPRESSION: ', &
     &             'MHB_2000_TRANSF'
        ELSE IF ( VTD%CONF%NUT_EXP == NUT__MHB2000_ADDON  ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  NUTATION_EXPRESSION: ', &
     &             'MHB_2000_ADDON'
        ELSE IF ( VTD%CONF%NUT_EXP == NUT__PETA           ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  NUTATION_EXPRESSION: ', &
     &             'PETA'
        ELSE IF ( VTD%CONF%NUT_EXP == NUT__PETB           ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  NUTATION_EXPRESSION: ', &
     &             'PETB'
        ELSE IF ( VTD%CONF%NUT_EXP == NUT__PETC           ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  NUTATION_EXPRESSION: ', &
     &             'PETC'
        ELSE 
           WRITE ( LUN, '(A,A,1X,I12)' ) '1  APR  NUTATION_EXPRESSION: ', &
     &             'Unknown', VTD%CONF%NUT_EXP 
      END IF
!
      IF ( VTD%CONF%NUT_GDS == NUT__GDS_YES ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  GEODESIC_NUTATION:   ', &
     &             'YES'
        ELSE IF ( VTD%CONF%NUT_GDS == NUT__GDS_NO ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  GEODESIC_NUTATION:   ', &
     &             'NO'
        ELSE 
           WRITE ( LUN, '(A,A)' ) '1  APR  GEODESIC_NUTATION:   ', &
     &             'Unknown'
      END IF
!
      IF ( VTD%CONF%EROT_COMPAT == VTD__NONE ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  EROT_COMPATIBILITY:  ', &
     &             'NONE'
        ELSE IF ( VTD%CONF%EROT_COMPAT == VTD__CALC10 ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  EROT_COMPATIBILITY:  ', &
     &             'CALC10'
        ELSE 
           WRITE ( LUN, '(A,A)' ) '1  APR  EROT_COMPATIBILITY:  ', &
     &             'Unknown'
      END IF
      WRITE ( LUN, '(A,A)' ) '1  APR  ANTENNA_THERMAL:     ', &
     &                           VTD%CONF%FINAM_ANTI(1:I_LEN(VTD%CONF%FINAM_ANTI))
      WRITE ( LUN, '(A,A)' ) '1  APR  ANTENNA_DEFORM:      ', &
     &                           VTD%CONF%FINAM_AGD(1:I_LEN(VTD%CONF%FINAM_AGD))
!
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_FINAM_STADESC:   ', &
     &               VTD%CONF%FINAM_STADESC(1:I_LEN(VTD%CONF%FINAM_STADESC))
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_FINAM_STACOO:    ', &
     &               VTD%CONF%FINAM_STACOO(1:I_LEN(VTD%CONF%FINAM_STACOO))
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_FINAM_STAVEL:    ', &
     &               VTD%CONF%FINAM_STAVEL(1:I_LEN(VTD%CONF%FINAM_STAVEL))
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_FINAM_STAECC:    ', &
     &               VTD%CONF%FINAM_STAECC(1:I_LEN(VTD%CONF%FINAM_STAECC))
      DO 410 J1=1,VTD__M_SCC
         IF ( VTD%CONF%FINAM_SOUCOO(J1) .NE. 'NONE' ) THEN
              WRITE ( LUN, '(A,I1,A,A)' ) '1  APR  VTD_FINAM_SOUCOO(', J1, '): ', &
     &                VTD%CONF%FINAM_SOUCOO(J1)(1:I_LEN(VTD%CONF%FINAM_SOUCOO(J1)))
         END IF
 410  CONTINUE 
!
      SOU_CAT_EPO = ((VTD%SOU(1)%MJD_REF - J2000__MJD) + VTD%SOU(1)%TAI_REF)/JYEAR__DAYS + &
     &              2000.0D0 
      WRITE ( LUN, '(A,F9.4)' ) '1  APR  SOUCAT_EPOCH:        ', SOU_CAT_EPO
!
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_FINAM_SOUPRP: ', &
     &               VTD%CONF%FINAM_SOUPRL_PRP(1:I_LEN(VTD%CONF%FINAM_SOUPRL_PRP))
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_FINAM_DE_EPH:    ', &
     &               VTD%CONF%FINAM_DE_EPH(1:I_LEN(VTD%CONF%FINAM_DE_EPH))
      WRITE ( LUN, '(A,A)' ) '1  APR  VTD_FINAM_LEAPSEC:   ', &
     &               VTD%CONF%FINAM_LEAPSEC(1:I_LEN(VTD%CONF%FINAM_LEAPSEC))
      DO 420 J2=1,VTD__M_PSF
         IF ( ILEN(VTD%CONF%POSVAR_FIL(J2)) > 0 .AND. &
     &        VTD%CONF%POSVAR_FIL(J2) .NE. 'NONE' ) THEN
!
              WRITE ( LUN, '(A,I2,A,A)' ) '1  APR  POSVAR_FIL(', J2, '):      ', &
     &                VTD%CONF%POSVAR_FIL(J2)(1:I_LEN(VTD%CONF%POSVAR_FIL(J2)))
         END IF
 420  CONTINUE 
!
      DO 430 J3=1,VTD__M_IOF
         IF ( ILEN(VTD%CONF%IONO_FILE(J3)) > 0 ) THEN
              WRITE ( LUN, '(A,I1,A,A)' ) '1  APR  VTD_FILE_IONO(', J3, '):    ', &
     &                     VTD%CONF%IONO_FILE(J3)(1:I_LEN(VTD%CONF%IONO_FILE(J3)))
           ELSE IF ( J3 == 1 ) THEN
              WRITE ( LUN, '(A,I1,A,A)' ) '1  APR  VTD_FILE_IONO(', J3, '):    ', 'NONE'
         END IF
 430  CONTINUE 
      WRITE ( LUN, '(A,F6.4)' ) '1  APR  IONOSPHERE_SCALE:    ', &
     &                          VTD%CONF%IONOSPHERE_SCALE
      IF ( VTD%CONF%GAL_ABR == VTD__YES ) THEN
           WRITE ( LUN, '(A,A)' ) '1  APR  GALACTIC_ABERR:      YES'
         ELSE
           WRITE ( LUN, '(A,A)' ) '1  APR  GALACTIC_ABERR:      NO'
      END IF
      WRITE ( LUN, '(A,A)' ) '1  APR  SOU_DEBIAS_MODEL:    ', &
     &                        VTD%CONF%SOU_DEBIAS_MODEL(1:I_LEN(VTD%CONF%SOU_DEBIAS_MODEL))
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_APR_SPOOL  !#!#
