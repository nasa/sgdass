!
! >> include-block for the a priori Earth Rotation Model
! >> L. Petrov  2006.10.31   v 1.4     2008.06.24_09:32:20
!
!    Derived type for the a priori Earth Orientation Model
!
!
	INTEGER*4  M_PRC__AEM
	INTEGER*4  M_NUT__AEM
	INTEGER*4  M_E3P__AEM
	INTEGER*4  M_E3H__AEM
	INTEGER*4  M_COM__AEM
	PARAMETER  ( M_PRC__AEM = 3    ) ! The hightest degree of precession parameters
	PARAMETER  ( M_NUT__AEM = 4096 ) ! The maximum number of terms in nutation
	PARAMETER  ( M_E3P__AEM = 2    ) ! The number of terms in E3 polynomial model
	PARAMETER  ( M_E3H__AEM = 32   ) ! The number of terms in E3 harmonic model
	PARAMETER  ( M_COM__AEM = 32   ) ! The number of lines with comments
	CHARACTER  AEM__LABEL*34
	PARAMETER  ( AEM__LABEL = 'AEM  Format version of 2006.12.06 ' )
!
	TYPE  AEM__TYPE
	   REAL*8     DZETA(0:M_PRC__AEM)
	   REAL*8     TETA(0:M_PRC__AEM)
	   REAL*8     Z(0:M_PRC__AEM)
	   REAL*8     EPS0(0:M_PRC__AEM)
	   REAL*8     CHI(0:M_PRC__AEM)
	   REAL*8     NUT_PHS(M_NUT__AEM)
	   REAL*8     NUT_FRQ(M_NUT__AEM)
	   REAL*8     NUT_ACC(M_NUT__AEM)
	   REAL*8     NUT_PSI_IN(M_NUT__AEM)
	   REAL*8     NUT_EPS_IN(M_NUT__AEM)
	   REAL*8     NUT_PSI_IN_RATE(M_NUT__AEM)
	   REAL*8     NUT_EPS_IN_RATE(M_NUT__AEM)
	   REAL*8     NUT_PSI_OUT(M_NUT__AEM)
	   REAL*8     NUT_EPS_OUT(M_NUT__AEM)
	   REAL*8     NUT_PSI_OUT_RATE(M_NUT__AEM)
	   REAL*8     NUT_EPS_OUT_RATE(M_NUT__AEM)
	   REAL*8     S0
	   REAL*8     OMEGA_N
	   REAL*8     E3_POL(0:M_E3P__AEM)
	   REAL*8     E3_FRQ(M_E3H__AEM)
	   REAL*8     E3_COS(M_E3H__AEM)
	   REAL*8     E3_SIN(M_E3H__AEM)
	   REAL*8     EEC_PHS(M_NUT__AEM)
	   REAL*8     EEC_FRQ(M_NUT__AEM)
	   REAL*8     EEC_SIN(M_NUT__AEM)
	   REAL*8     EEC_COS(M_NUT__AEM)
	   REAL*8     EEC_SEC
	   CHARACTER  MODEL_NAME*32
	   CHARACTER  MODEL_DATE*32
	   CHARACTER  MODEL_COMMENT(M_COM__AEM)*128
	   INTEGER*4  N_PRC
	   INTEGER*4  N_NUT
	   INTEGER*4  N_E3P
	   INTEGER*4  N_E3H
	   INTEGER*4  N_COM
	   INTEGER*4  N_EEC
	   INTEGER*4  STATUS
        END TYPE  AEM__TYPE
!
! << end of include block aem.i  for the the a priori Earth Rotation Model
!
