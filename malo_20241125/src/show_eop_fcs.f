      PROGRAM    SHOW_EOP_FCS
! ************************************************************************
! *                                                                      *
! *   Program SHOW_EOP_FCS
! *                                                                      *
! *  ### 07-APR-2016  SHOW_EOP_FCS v1.2 (c)  L. Petrov  27-OCT-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER  FIL_FCS*128, INP_DATE*30, CPAR*8, STR*128, RES*32, STR_DATE*30
      REAL*8     TAI, TIM, EVEC(3,0:2), HEO_APS_VEC(3,0:2), &
     &           UTC_M_TAI, DPSI, DEPS, MAT_ROT(3,3,0:2), DPSI_RATE, DEPS_RATE
      REAL*8       LOD__TO__ER3
      PARAMETER  ( LOD__TO__ER3 =  1.00273781191135448E0*OM__EAR**2/PI2 )
      INTEGER*4  J1, J2, J3, MJD, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_HR_UTC_CDATE*29, &
     &                       TIM_TO_DATE*23
      REAL*8,    EXTERNAL :: GET_UTC
      INTEGER*4, EXTERNAL :: TIME
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage show_eop_fcs fcs_file date par'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_FCS  )
           CALL GETARG ( 2, INP_DATE )
           CALL GETARG ( 3, CPAR     )
      END IF
      CALL TRAN ( 12, CPAR, CPAR )
!
      IUER = 1
      CALL EOP_FCS_REA ( FIL_FCS, NERS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5701, IUER, 'SHOW_EOP_FCS', 'Failure in reading '// &
     &         'EOP forecast file '//FIL_FCS ) 
           CALL EXIT ( 1 )
      END IF
!
      CALL TRAN ( 11, INP_DATE, INP_DATE )
      IF ( INP_DATE == 'NOW' ) THEN
           INP_DATE = GET_HR_UTC_CDATE ()
      END IF
      IUER = -1
      CALL DATE_TO_TIME ( INP_DATE, MJD, TAI, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5702, IUER, 'SHOW_EOP_FCS', 'Wrong format '// &
     &         'of date '//INP_DATE(1:I_LEN(INP_DATE))//' -- '// &
     &         'YYYY.MM.DD_hh:mm:ss.sssssss was expected' )
           CALL EXIT ( 1 )
      END IF
      STR_DATE = MJDSEC_TO_DATE ( MJD, TAI, IUER ) 
      TIM = (MJD - J2000__MJD)*86400.0D0 + TAI
!      WRITE ( 6, * ) 'NJ                  = ', FCS%NJ
!      WRITE ( 6, * ) 'TIM                 = ', TIM
!      WRITE ( 6, * ) 'TIM_LAST_EOPS_A_ASS = ', FCS%TIM_LAST_EOPS_A_ASS
!      WRITE ( 6, * ) 'TIM_LAST_EOPS_A     = ', FCS%TIM_LAST_EOPS_A
!      WRITE ( 6, * ) 'TIM_LAST_EOPC_C     = ', FCS%TIM_LAST_EOPS_C
!      WRITE ( 6, * ) 'TIM_LAST_EOPS_I     = ', FCS%TIM_LAST_EOPS_I
!      WRITE ( 6, * ) 'TIM_LAST_EOPS_F     = ', FCS%TIM_LAST_EOPS_F
!      WRITE ( 6, * ) 'TIM_LAST_EOPS_R     = ', FCS%TIM_LAST_EOPS_R
!      WRITE ( 6, * ) 'TIM_LAST_EOPS_S     = ', FCS%TIM_LAST_EOPS_S
!      WRITE ( 6, * ) 'TIMC                = ', FCS%TIMC(1)
!      WRITE ( 6, * ) 'ARG_12              = ', FCS%ARG_12(1), FCS%ARG_12(FCS%NK_12)
!      WRITE ( 6, * ) 'ARG_3               = ', FCS%ARG_3(1),  FCS%ARG_3(FCS%NK_3)
!
      IF ( TIM < NERS%FCS%ARG_C(1) ) THEN
           MJD = J2000__MJD  + INT(NERS%FCS%ARG_C(1)/86400.0D0)
           TAI = NERS%FCS%ARG_C(1) - 86400.0D0*INT(NERS%FCS%ARG_C(1)/86400.0D0)
           STR = MJDSEC_TO_DATE ( MJD, TAI, IUER )
           WRITE ( 6, 210 ) INP_DATE(1:I_LEN(INP_DATE)), STR(1:21)
 210       FORMAT ( 'Requested date ', A, ' is too early. The earliest supported date is ', A )
           CALL EXIT ( 1 )
      END IF
      IF ( TIM > NERS%FCS%ARG_3(NERS%FCS%NK_3) ) THEN
           MJD = J2000__MJD  + INT(NERS%FCS%ARG_3(NERS%FCS%NK_3)/86400.0D0)
           TAI = NERS%FCS%ARG_3(NERS%FCS%NK_3) - &
     &           86400.0D0*INT(NERS%FCS%ARG_3(NERS%FCS%NK_3)/86400.0D0)
           STR = MJDSEC_TO_DATE ( MJD, TAI, IUER )
           WRITE ( 6, 220 ) INP_DATE(1:I_LEN(INP_DATE)), STR(1:21)
 220       FORMAT ( 'Requested date ', A, ' is too far in the future. The latest supported date is ', A )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL NERS_GET_EVEC ( NERS, TIM, EVEC, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5703, IUER, 'SHOW_EOP_FCS', 'Error in computing '// &
     &         'the vector of perturbation Earth orientation' ) 
           CALL EXIT ( 1 )
      END IF
!
      IF ( CPAR(1:1) == 'h'       .OR. &
     &     CPAR      == 'dpsi'    .OR. &
     &     CPAR      == 'deps'    .OR. &
     &     CPAR(1:7) == 'now_all' .OR. &
     &     CPAR(1:8) == 'now_html'     ) THEN
           IUER = -1
           CALL NERS_GET_HEO ( NERS, TIM, EVEC(3,0)/UT1__TO__E3, DPSI, DEPS, &
     &                         HEO_APS_VEC, DPSI_RATE, DEPS_RATE, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 5703, IUER, 'SHOW_EOP_NERS', 'Error in computing '// &
     &              'the vector of perturbation Earth orientation' ) 
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( CPAR(1:3) == 'mat' .OR. CPAR == 'now_all' .OR. CPAR == 'now_html' ) THEN
           IUER = -1
           CALL GET_NERS_MATROT ( TIM, NERS, MAT_ROT, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 5703, IUER, 'SHOW_EOP_NERS', 'Error in computing '// &
     &              'the rotatgion matrix for Earth orientation' ) 
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( CPAR == 'e1' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(1,0), 'rad'
         ELSE IF ( CPAR == 'e2' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(2,0), 'rad'
         ELSE IF ( CPAR == 'e3' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(3,0), 'rad'
         ELSE IF ( CPAR == 'e1r' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(1,1), 'rad/s'
         ELSE IF ( CPAR == 'e2r' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(2,1), 'rad/s'
         ELSE IF ( CPAR == 'e3r' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(3,1), 'rad/s'
         ELSE IF ( CPAR == 'e1rr' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(1,2), 'rad/s^2'
         ELSE IF ( CPAR == 'e2rr' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(2,2), 'rad/s^2'
         ELSE IF ( CPAR == 'e3rr' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(3,2), 'rad/s^2'
         ELSE IF ( CPAR == 'h1' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), HEO_APS_VEC(1,0), 'rad'
         ELSE IF ( CPAR == 'h2' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), HEO_APS_VEC(2,0), 'rad'
         ELSE IF ( CPAR == 'h3' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), HEO_APS_VEC(3,0), 'rad'
         ELSE IF ( CPAR == 'h1r' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), HEO_APS_VEC(1,1), 'rad/s'
         ELSE IF ( CPAR == 'h2r' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), HEO_APS_VEC(2,1), 'rad/s'
         ELSE IF ( CPAR == 'h3r' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), HEO_APS_VEC(3,1), 'rad/s'
         ELSE IF ( CPAR == 'h1rr' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), HEO_APS_VEC(1,2), 'rad/s^2'
         ELSE IF ( CPAR == 'h2rr' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), HEO_APS_VEC(2,2), 'rad/s^2'
         ELSE IF ( CPAR == 'h3rr' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), HEO_APS_VEC(3,2), 'rad/s^2'
         ELSE IF ( CPAR == 'ut1mtai' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F11.7,2x,a)'   ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(3,0)/UT1__TO__E3, 's'
         ELSE IF ( CPAR == 'ut1rat' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(3,1)/UT1__TO__E3*86400.0, 's/day'
         ELSE IF ( CPAR == 'lod' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(3,1)/LOD__TO__ER3, 's'
         ELSE IF ( CPAR == 'xpol' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F9.6,2x,a)'    ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(2,0)*RAD__TO__ARCSEC, 'arcsec'
         ELSE IF ( CPAR == 'ypol' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F9.6,2x,a)'    ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(1,0)*RAD__TO__ARCSEC, 'arcsec'
         ELSE IF ( CPAR == 'xpolr' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2X,A)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(2,1)*RAD__TO__ARCSEC*86400.0, 'arcsec/d'
         ELSE IF ( CPAR == 'ypolr' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2X,A)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), EVEC(1,1)*RAD__TO__ARCSEC*86400.0, 'arcsec/d'
         ELSE IF ( CPAR == 'deps' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F11.7,2X,A)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), DEPS*RAD__TO__ARCSEC, 'arcsec'
         ELSE IF ( CPAR == 'dpsi' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F11.7,2X,A)' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), DPSI*RAD__TO__ARCSEC, 'arcsec'
         ELSE IF ( CPAR == 'ser' ) THEN
           WRITE ( UNIT=6, FMT=110 ) &
     &                     STR_DATE(1:23), &
     &                     TIM_TO_DATE ( NERS%FCS%TAI_GEN, IUER ), &
     &                     EVEC(1:3,0), &
     &                     EVEC(1:3,1), &
     &                     EVEC(2,0)*RAD__TO__ARCSEC, &
     &                     EVEC(1,0)*RAD__TO__ARCSEC, &
     &                     EVEC(3,0)/UT1__TO__E3, &
     &                     EVEC(2,1)*RAD__TO__ARCSEC*86400.0, &
     &                     EVEC(1,1)*RAD__TO__ARCSEC*86400.0, &
     &                     EVEC(3,1)/UT1__TO__E3*86400.0, &
     &                     EVEC(3,1)/LOD__TO__ER3, &
     &                     NERS%FCS%EANG_MOD
 110       FORMAT ( A, ' || ', 2X, A, 2X, 3(1PD16.9,1X), 2X, 3(1PD16.9,1X),  &
     &                     2X, 3(1PD16.9,1X), 2X, 4(1PD16.9,1X), 2X, A, 1X, '|' )
         ELSE IF ( CPAR == 'utcmtai' ) THEN
           IUER = -1
           CALL EOP_NERS_UTCMTAI ( TIM, NERS, UTC_M_TAI, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 5703, IUER, 'SHOW_EOP_FCS', 'Error in extraction '// &
     &              'the UTC minus TAI function for the requested epoch' )
                CALL EXIT ( 1 )
           END IF
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F5.1,2x,A)'   ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), UTC_M_TAI, 's'
         ELSE IF ( CPAR == 'utc-tai' ) THEN
           IUER = -1
           CALL EOP_NERS_UTCMTAI ( TIM, NERS, UTC_M_TAI, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 5703, IUER, 'SHOW_EOP_FCS', 'Error in extraction '// &
     &              'the UTC minus TAI function for the requested epoch' )
                CALL EXIT ( 1 )
           END IF
           WRITE ( UNIT=6, FMT='(F5.1)'   ) UTC_M_TAI
         ELSE IF ( CPAR == 'mat' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 9(1PD20.12,1X))' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), MAT_ROT(1:3,1:3,0)
         ELSE IF ( CPAR == 'matr' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 9(1PD20.12,1X))' ) CPAR(1:I_LEN(CPAR)), &
     &                     STR_DATE(1:27), MAT_ROT(1:3,1:3,1)
         ELSE IF ( CPAR == 'matrr' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 9(1PD20.12,1X))' ) CPAR(1:I_LEN(CPAR)), STR_DATE(1:27), MAT_ROT(1:3,1:3,2)
         ELSE IF ( CPAR == 'start' ) THEN
           MJD = J2000__MJD  + INT(NERS%FCS%ARG_C(1)/86400.0D0)
           TAI = NERS%FCS%ARG_C(1) - 86400.0D0*INT(NERS%FCS%ARG_C(1)/86400.0D0)
           STR_DATE = MJDSEC_TO_DATE ( MJD, TAI, IUER ) 
           WRITE ( UNIT=6, FMT='(A)' ) STR_DATE(1:21)
         ELSE IF ( CPAR == 'stop' ) THEN
           MJD = J2000__MJD  + INT(NERS%FCS%TAI_LAST_EOPS_A/86400.0D0)
           TAI = NERS%FCS%TAI_LAST_EOPS_A - &
     &           86400.0D0*INT(NERS%FCS%TAI_LAST_EOPS_A/86400.0D0)
           STR_DATE = MJDSEC_TO_DATE ( MJD, TAI, IUER ) 
           WRITE ( UNIT=6, FMT='(A)' ) STR_DATE(1:21)
         ELSE IF ( CPAR == 'now_all' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) 'e1      ', &
     &                     STR_DATE(1:27), EVEC(1,0), 'rad'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) 'e2      ', &
     &                     STR_DATE(1:27), EVEC(2,0), 'rad'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) 'e3      ', &
     &                     STR_DATE(1:27), EVEC(3,0), 'rad'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) 'e1r     ', &
     &                     STR_DATE(1:27), EVEC(1,1), 'rad/s'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) 'e2r     ', &
     &                     STR_DATE(1:27), EVEC(2,1), 'rad/s'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) 'e3r     ', &
     &                     STR_DATE(1:27), EVEC(3,1), 'rad/s'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) 'e1rr    ', &
     &                     STR_DATE(1:27), EVEC(1,2), 'rad/s^2'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) 'e2rr    ', &
     &                     STR_DATE(1:27), EVEC(2,2), 'rad/s^2'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) 'e3rr    ', &
     &                     STR_DATE(1:27), EVEC(3,2), 'rad/s^2'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F11.7,2x,a)'   ) 'ut1mtai ', &
     &                     STR_DATE(1:27), EVEC(3,0)/UT1__TO__E3, 's'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) 'ut1rat  ', &
     &                     STR_DATE(1:27), EVEC(3,1)/UT1__TO__E3*86400.0, 's/day'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) 'lod     ', &
     &                     STR_DATE(1:27), EVEC(3,1)/LOD__TO__ER3, 's'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F9.6,2x,a)'    ) 'xpol    ', &
     &                     STR_DATE(1:27), EVEC(2,0)*RAD__TO__ARCSEC, 'arcsec'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F9.6,2x,a)'    ) 'ypol    ', &
     &                     STR_DATE(1:27), EVEC(1,0)*RAD__TO__ARCSEC, 'arcsec'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2X,A)' ) 'xpolr   ', &
     &                     STR_DATE(1:27), EVEC(2,1)*RAD__TO__ARCSEC*86400.0, 'arcsec/d'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2X,A)' ) 'ypolr   ', &
     &                     STR_DATE(1:27), EVEC(1,1)*RAD__TO__ARCSEC*86400.0, 'arcsec/d'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F11.7,2X,A)' ) 'deps    ', &
     &                     STR_DATE(1:27), DEPS*RAD__TO__ARCSEC, 'arcsec'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F11.7,2X,A)' ) 'dpsi    ', &
     &                     STR_DATE(1:27), DPSI*RAD__TO__ARCSEC, 'arcsec'
           IUER = -1
           CALL EOP_NERS_UTCMTAI ( TIM, NERS, UTC_M_TAI, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 5703, IUER, 'SHOW_EOP_FCS', 'Error in extraction '// &
     &              'the UTC minus TAI function for the requested epoch' )
                CALL EXIT ( 1 )
           END IF
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F5.1,2x,A)'      ) 'ut1mtai ', &
     &                     STR_DATE(1:27), UTC_M_TAI, 's'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 9(1PD20.12,1X))' ) 'mat     ', &
     &                     STR_DATE(1:27), MAT_ROT(1:3,1:3,0)
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 9(1PD20.12,1X))' ) 'matr    ', &
     &                     STR_DATE(1:27), MAT_ROT(1:3,1:3,1)
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 9(1PD20.12,1X))' ) 'matrr   ', &
     &                     STR_DATE(1:27), MAT_ROT(1:3,1:3,2)
         ELSE IF ( CPAR == 'now_html' ) THEN
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<TT>Parameter TAI time        '// &
     &                                                          '              Value            Dimension</TT>'
           IUER = -1
           CALL EOP_NERS_UTCMTAI ( TIM, NERS, UTC_M_TAI, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 5703, IUER, 'SHOW_EOP_FCS', 'Error in extraction '// &
     &              'the UTC minus TAI function for the requested epoch' )
                CALL EXIT ( 1 )
           END IF
!
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<A HREF="/ners/what_e1.html">e1</A><TT>      ', &
     &                     STR_DATE(1:27), EVEC(1,0), 'rad </TT>'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<A HREF="/ners/what_e2.html">e2</A><TT>      ', &
     &                     STR_DATE(1:27), EVEC(2,0), 'rad </TT>'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<A HREF="/ners/what_e3.html">e3</A><TT>      ', &
     &                     STR_DATE(1:27), EVEC(3,0), 'rad </TT>'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<A HREF="/ners/what_e1r.html">e1r</A><TT>     ', &
     &                     STR_DATE(1:27), EVEC(1,1), 'rad </TT>'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<A HREF="/ners/what_e2r.html">e2r</A><TT>     ', &
     &                     STR_DATE(1:27), EVEC(2,1), 'rad </TT>'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<A HREF="/ners/what_e3r.html">e3r</A><TT>     ', &
     &                     STR_DATE(1:27), EVEC(3,1), 'rad </TT>'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<A HREF="/ners/what_e1rr.html">e1rr</A><TT>    ', &
     &                     STR_DATE(1:27), EVEC(1,2), 'rad </TT>'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<A HREF="/ners/what_e2rr.html">e2rr</A><TT>    ', &
     &                     STR_DATE(1:27), EVEC(2,2), 'rad </TT>'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<A HREF="/ners/what_e3rr.html">e3rr</A><TT>    ', &
     &                     STR_DATE(1:27), EVEC(3,2), 'rad </TT>'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F11.7,2x,a)'   ) '<A HREF="/ners/what_utcmtai.html">utcmtai</A></TT> ', &
     &                     STR_DATE(1:27), UTC_M_TAI, '     s'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F11.7,2x,a)'   ) '<A HREF="/ners/what_ut1mtai.html">ut1mtai</A></TT> ', &
     &                     STR_DATE(1:27), EVEC(3,0)/UT1__TO__E3, '     s'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<A HREF="/ners/what_ut1rat.html">ut1rat</A></TT>  ', &
     &                     STR_DATE(1:27), EVEC(3,1)/UT1__TO__E3*86400.0, 's/day'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2x,a)' ) '<A HREF="/ners/what_lod.html">lod</A></TT>     ', &
     &                     STR_DATE(1:27), EVEC(3,1)/LOD__TO__ER3, 's'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F9.6,2x,a)'    ) '<A HREF="/ners/what_xpol.html">xpol</A></TT>    ', &
     &                     STR_DATE(1:27), EVEC(2,0)*RAD__TO__ARCSEC, '       arcsec'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F9.6,2x,a)'    ) '<A HREF="/ners/what_ypol.html">ypol</A></TT>    ', &
     &                     STR_DATE(1:27), EVEC(1,0)*RAD__TO__ARCSEC, '       arcsec'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2X,A)' ) '<A HREF="/ners/what_xpolr.html">xpolr</A></TT>   ', &
     &                     STR_DATE(1:27), EVEC(2,1)*RAD__TO__ARCSEC*86400.0, 'arcsec/d'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, 1PD16.9,2X,A)' ) '<A HREF="/ners/what_ypolr.html">ypolr</A></TT>   ', &
     &                     STR_DATE(1:27), EVEC(1,1)*RAD__TO__ARCSEC*86400.0, 'arcsec/d'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F11.7,2X,A)' ) '<A HREF="/ners/what_deps.html">deps</A></TT>    ', &
     &                     STR_DATE(1:27), DEPS*RAD__TO__ARCSEC, '     arcsec'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 2X, F11.7,2X,A)' ) '<A HREF="/ners/what_dpsi.html">dpsi</A></TT>    ', &
     &                     STR_DATE(1:27), DPSI*RAD__TO__ARCSEC, '     arcsec'
           WRITE ( UNIT=6, FMT='(A, 2X, A, 1X, 9(1PD20.12,1X))' ) '<A HREF="/ners/what_mat.html">mat</A></TT>     ', &
     &                     STR_DATE(1:27), MAT_ROT(1:3,1:3,0)
           WRITE ( UNIT=6, FMT='(A, 2X, A, 1X, 9(1PD20.12,1X))' ) '<A HREF="/ners/what_matr.html">matr</A></TT>    ', &
     &                     STR_DATE(1:27), MAT_ROT(1:3,1:3,1)
           WRITE ( UNIT=6, FMT='(A, 2X, A, 1X, 9(1PD20.12,1X))' ) '<A HREF="/ners/what_matrr.html">matrr</A></TT>   ', &
     &                     STR_DATE(1:27), MAT_ROT(1:3,1:3,2)
           IUER = -1
           CALL EOP_NERS_UTCMTAI ( TIM, NERS, UTC_M_TAI, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 5703, IUER, 'SHOW_EOP_FCS', 'Error in extraction '// &
     &              'the UTC minus TAI function for the requested epoch' )
                CALL EXIT ( 1 )
           END IF
         ELSE
           WRITE ( 6, 230 ) CPAR(1:I_LEN(CPAR))
 230       FORMAT ( 'Parameter ',A, ' is not supported' )
      END IF
!
      END  PROGRAM   SHOW_EOP_FCS  !#!#
