      PROGRAM    STA_ACP
! ************************************************************************
! *                                                                      *
! *   Program STA_ACP generates atmosphere chemical potential at the     *
! *   specified height above the station with given Carthesian           *
! *   coordinates or lat/lon/orth-heiight the air temperature at that    *
! *   height. It uses the following import datasets:                     *
! *                                                                      *
! *   1) 3D input field of atmosphere layer thickness;                   *
! *   2) 3D input field of air temperature;                              *
! *   3) 3D input field of specific humidity;                            *
! *   4) 2D input grid of the heights of grid point above                *
! *      the WGS84 reference ellipsoid;                                  *
! *   5) 2D output grid of the surface height avereged over the pixel    *
! *      area.                                                           *
! *                                                                      *
! *   Usage: sta_acp 
! *                                                                      *
! *   The results is written in heb-format.                              *
! *                                                                      *
! * ###  08-MAR-2014    STA_ACP   v1.0 (c)  L. Petrov  09-MAR-2014   ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( MALO__TYPE ) :: MAL
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_T, HEB_Q, HEB_G, &
     &                           HEB_OGH, HEB_ACP, HEB_ST, HEB_SPR, &
     &                           HEB_GEOID_BSPL
      CHARACTER  DIR_HEB*128, OBS_DATE*128, FIL_DATE*128, FIL_IGH*128, &
     &           FIL_OGH*128, FIL_OUT*128, DIR_ACP_OUT*128, &
     &           DIR_ST_OUT*128,  FIL_BSPL_HEB*128, STR*128, CAL_DATE*19, &
     &           LAT_STR*12, LON_STR*12
      CHARACTER  STA_ACP__LABEL*32
      REAL*8     COO(3), HEI_ABOVE_SUR, COO_HLP(3), ACP_VAL, HEI_GEOID, &
     &           PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC 
      PARAMETER  ( STA_ACP__LABEL = 'STA_ACP Vers  1.0  of 2014.03.09' )
      INTEGER*8  DIR_DESC
      INTEGER*4  ID, DEG, MODE, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR
      REAL*8,    EXTERNAL :: GET_GEOID 
!
      IF ( IARGC() < 10 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: sta_acp mode coo1 coo2 coo3 height heb-dir date '// &
     &                       'inp_grid_height output_grid_height fil_geoid_bspl'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, STR      )
           CALL CHIN   ( STR, MODE   )
           CALL GETARG ( 2, STR      )
           IF ( INDEX ( STR, '.' ) < 1 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) COO(1)
           CALL GETARG ( 3, STR      )
           IF ( INDEX ( STR, '.' ) < 1 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) COO(2)
           CALL GETARG ( 4, STR      )
           IF ( INDEX ( STR, '.' ) < 1 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) COO(3)
           CALL GETARG ( 5, STR      )
           IF ( INDEX ( STR, '.' ) < 1 ) STR = STR(1:I_LEN(STR))//'.'
           READ ( UNIT=STR(1:I_LEN(STR)), FMT='(F10.5)' ) HEI_ABOVE_SUR
           CALL GETARG ( 6, DIR_HEB  )
!
           CALL GETARG ( 7, OBS_DATE )
           CALL GETARG ( 8, FIL_IGH  )
           CALL GETARG ( 9, FIL_OGH  )
           CALL GETARG ( 10, FIL_BSPL_HEB )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( FIL_BSPL_HEB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6601, IUER, 'STA_ACP', 'Cannot find file with '// &
     &         'geoid heights expanded in B-spline basis '//FIL_BSPL_HEB )
           CALL EXIT ( 1 )
      END IF
!
      IUER = 0
      IF ( MODE == 1 ) THEN             ! XYZ
           CALL REF_ELL ( 1, COO, PHI_GCN, PHI_GDT, LAMBDA, H_ELL, &
     &                    RD, G_ACC )
           IUER = -1
           COO_HLP(1) = H_ELL - GET_GEOID ( PHI_GDT, LAMBDA, FIL_BSPL_HEB, &
     &                                      HEB_GEOID_BSPL, IUER )
           COO_HLP(2) = LAMBDA
           COO_HLP(3) = PHI_GDT
         ELSE IF ( MODE == 2 ) THEN
           COO_HLP(1:1) = COO(1:1)      ! H_geoid, L,P
           COO_HLP(2)   = COO(2)*DEG__TO__RAD
           COO_HLP(3)   = COO(3)*DEG__TO__RAD
         ELSE IF ( MODE == 3 ) THEN     ! H_ell,   L,P
           IUER = -1
           COO_HLP(1)   = COO_HLP(1) - GET_GEOID ( COO(3), COO(2), FIL_BSPL_HEB, &
     &                                             HEB_GEOID_BSPL, IUER )
           COO_HLP(2:3) = COO(2:3)
           COO_HLP(2)   = COO(2)*DEG__TO__RAD
           COO_HLP(3)   = COO(3)*DEG__TO__RAD
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6602, IUER, 'STA_ACP', 'Unsupported argument '// &
     &         'mode: '//STR(1:I_LEN(STR))//' -- only 1, 2, or 3 '// &
     &         'are supported' )
           CALL EXIT ( 1 )
      END IF
      IF ( COO_HLP(2) < 0.0D0 ) COO_HLP(2) = COO_HLP(2) + PI2
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6603, IUER, 'STA_ACP', 'Failure in an attempt '// &
     &         'to compute geoid height' )
           CALL EXIT ( 1 )
      END IF
!
      FIL_DATE = OBS_DATE(1:4)//OBS_DATE(6:7)//OBS_DATE(9:13)//OBS_DATE(15:16)
      IUER = -1
      CALL READ_DQT_HEB ( DIR_HEB, FIL_DATE, HEB_DELP, HEB_Q, HEB_T, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6601, IUER, 'STA_ACP', 'Error in an attempt '// &
     &                   'to read numerical weather model for date '//FIL_DATE )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( FIL_IGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           IUER = -1
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6602, IUER, 'STA_ACP', 'Cannot find input grid '// &
     &         'height file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
      IUER = -1
      CALL READ_HEB_HEADER ( FIL_IGH, HEB_G, IUER )
      IF ( IUER  .NE. 0 ) THEN
           IUER = -1
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6603, IUER, 'STA_ACP', 'Error in reading '// &
     &         'heb-file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB_DATA ( FIL_IGH, HEB_G, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6604, IUER, 'STA_ACP', 'Error in reading '// &
     &         'heb-file '//FIL_IGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_SHARE_FILE ( FIL_OGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           IUER = -1
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6605, IUER, 'STA_ACP', 'Cannot find output grid '// &
     &         'height file '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
      IUER = -1
      CALL READ_HEB_HEADER ( FIL_OGH, HEB_OGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           IUER = -1
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6606, IUER, 'STA_ACP', 'Error in reading '// &
     &         'heb-file '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB_DATA ( FIL_OGH, HEB_OGH, IUER )
      IF ( IUER  .NE. 0 ) THEN
           IUER = -1
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6607, IUER, 'STA_ACP', 'Error in reading '// &
     &         'heb-file '//FIL_OGH )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_COMP_ACP ( MALO__ACP_STA, HEI_ABOVE_SUR, HEB_DELP, HEB_T, HEB_Q, &
     &                     HEB_G, HEB_OGH, HEB_ACP, HEB_ST, HEB_SPR, COO_HLP, &
     &                     ACP_VAL, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6608, IUER, 'STA_ACP', 'Error in an attempt '// &
     &         'to compute surface atmospheric pressure' )
           CALL EXIT ( 1 )
      END IF
      IF ( COO_HLP(2) > 0.0D0 ) THEN
           CALL RG_TAT ( COO_HLP(2), 1, LON_STR, IUER )
         ELSE 
           CALL RG_TAT ( COO_HLP(2)-PI2, 1, LON_STR, IUER )
      END IF
      CALL RG_TAT ( COO_HLP(3), 1, LAT_STR, IUER )
      write ( 6, * ) ' coo_hlp= ', coo_hlp ! %%%
      write ( 6, * ) ' acp_val= ', acp_val ! %%%
      WRITE ( 6, 110 ) COO_HLP(1), LON_STR, LAT_STR, ACP_VAL
 110  FORMAT ( 'Hei_geoid: ', F8.1, ' Geod_lat: ', A, ' Long: ', A, ' ACP: ', F6.4 )
      END  PROGRAM  STA_ACP  !#!  
