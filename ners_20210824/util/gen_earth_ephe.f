      PROGRAM    GEN_EARTH_EPHE
! ************************************************************************
! *                                                                      *
! *   Program GEN_EARTH_EPHE generates file with Earth's ephemerides.    *
! *   The file has spline coefficients for Earth's ephemerides in the    *
! *   baricentric coordinate system with step 1 day.                     *
! *                                                                      *
! *  ### 07-JUN-2018               v1.0 (c)  L. Petrov  07-JUN-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'de_eph.i'
      INTEGER*4  MP
      PARAMETER  ( MP = 32768 )
      TYPE     ( DE_EPH__TYPE ) :: DE_EPH
      CHARACTER  FILEPH*128, FILOUT*128, DATE_BEG*21, DATE_END*21
      INTEGER*4  MJD_BEG, MJD_END, MJD, NP, J1, J2, J3, LUN, IUER
      REAL*8     TAI_BEG, TAI_END, TAI, COO(3), VEL(3), ACC(3)
      REAL*8     COO_ARR(MP,3), TIM_ARR(MP), SPL_ARR(MP,3), TMP_ARR(MP)
!
      DATE_BEG = '1970_01_01:00:00:00.0'
      DATE_END = '2049_12_31:00:00:00.0'
      FILEPH   = '/progs/vtd_20171027/share/DE403_JPL.dat'
      FILOUT   = '/tmp/de403_earth_spline.dat'
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TAI_BEG, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END, MJD_END, TAI_END, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL READ_DE_EPH ( FILEPH, DE_EPH, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      NP = MJD_END - MJD_BEG + 1
      MJD = MJD_BEG
      TAI = 0.0D0
      DO 410 J1=1,NP
         IUER = -1
         CALL PLANETA_DE_EPH ( DE_EPH, MJD, TAI, 'EARTH', COO, VEL, ACC, IUER )
         IF ( IUER .NE. 0 ) CALL EXIT ( 1 ) 
         TIM_ARR(J1) = (MJD - J2000__MJD)*86400.0D0 + TAI
         COO_ARR(J1,1:3) = COO(1:3)
         MJD = MJD + 1
 410  CONTINUE 
      WRITE ( 6, * ) 'NP= ', NP
!
      DO 420 J2=1,3
         IUER = -1
         CALL MAKE_SPLINE ( 2, NP, TIM_ARR, COO_ARR(1,J2), 0.0D0, 0.0D0, &
     &                      SPL_ARR(1,J2), TMP_ARR, IUER )
 420  CONTINUE 
!
      LUN = 32
      IUER = -1
      CALL BINF_OPEN ( FILOUT, 'unknown', LUN, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL WRBIN_STRING ( LUN, 'Earth orbit from JPL ephemerides DE403', IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL WRBIN_RECORD ( LUN, 4, NP, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL WRBIN_RECORD ( LUN, 8*NP, TIM_ARR, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      DO 430 J3=1,3
         IUER = -1
         CALL WRBIN_RECORD ( LUN, 8*NP, COO_ARR(1,J3), IUER )
         IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         IUER = -1
         CALL WRBIN_RECORD ( LUN, 8*NP, SPL_ARR(1,J3), IUER )
         IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
 430  CONTINUE 
!      
      END  PROGRAM  GEN_EARTH_EPHE  !#!#
