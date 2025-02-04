      PROGRAM    GET_TRG_DENS
! ************************************************************************
! *                                                                      *
! *   Program  GET_TRG_DENS
! *                                                                      *
! *  ### 26-MAR-2018  GET_TRG_DENS v1.0 (c)  L. Petrov  30-MAR-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      CHARACTER  FIL_O3*128, FIL_CO2*128, FIL_D*128, FIL_T*128, FIL_Q*128, &
     &           FIL_NAT_OGH*128, FIL_FINE_OGH*128, FIL_OUT_TRG*128, COMPR*16, COMPR_COM*16, &
     &           STR*128
      CHARACTER  MODE*16
      TYPE     ( HEB__TYPE    ) :: HEB_DELP, HEB_T, HEB_Q, HEB_NAT_OGH, &
     &                             HEB_O3, HEB_CO2, HEB_FINE_OGH, &
     &                             HEB_TRG_3D, HEB_TRG_2D
      REAL*8     REF_HEI, CNS_DR2(2)
      LOGICAL*1  LEX
      INTEGER*4  IVRB, DIMO(3), J1, J2, J3, J4, ID, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      DIMO  = 0
      IVRB = 1
      COMPR = 'none'
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, '(A)' ) 'Usage get_trg_dens heb_o3 heb_nat_ogh mode heb_fine_ogh heb_trg [ivrb] [compr]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_O3  )
           CALL GETARG ( 2, FIL_NAT_OGH )
           CALL GETARG ( 3, MODE   )
           CALL GETARG ( 4, FIL_FINE_OGH )
           CALL GETARG ( 5, FIL_OUT_TRG )
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 6, STR   )
                CALL CHIN  ( STR, IVRB )
           END IF
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, COMPR )
           END IF
      END IF
      IF ( COMPR == 'none' ) THEN
           CONTINUE 
           CALL CLRCH ( COMPR_COM )
         ELSE IF ( COMPR == 'gzip' ) THEN
           COMPR_COM = 'gzip -1 -f '
         ELSE IF ( COMPR == 'bzip2' ) THEN
           COMPR_COM = 'bzip2 -9 -f '
         ELSE IF ( COMPR == 'pbzip2' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -f '
         ELSE IF ( COMPR == 'pbzip2_p1' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -p1 -f '
         ELSE IF ( COMPR == 'lbzip2' ) THEN
           COMPR_COM= 'lbzip2 -9 -f '
         ELSE IF ( COMPR == 'lbzip2_p1' ) THEN
           COMPR_COM= 'lbzip2 -9 -n1 -f '
         ELSE IF ( COMPR == 'lbzip2_1' ) THEN
           COMPR_COM= 'lbzip2 -1 -f '
         ELSE IF ( COMPR == 'lbzip2_1p1' ) THEN
           COMPR_COM= 'lbzip2 -1 -n1 -f '
         ELSE IF ( COMPR == 'lbzip2_2p1' ) THEN
           COMPR_COM = 'lbzip2 -2 -n1 -f '
         ELSE 
           CALL ERR_LOG ( 7101, -2, 'GEN_TRG_DENS', 'Unsupported '// &
     &         'compression method: '//TRIM(COMPR)//' . Supported methods: '// &
     &         ' none gzip bzip2 pbzip2 pbzip2_p1 lbzip lbzip2_p1 '// &
     &         'lbzip2 lbzip2_1 lbzip2_1p1 lbzip2_2p1' ) 
           CALL EXIT ( 1 )
      END IF
      IF ( MODE == 'd3' ) THEN
           REF_HEI = 0.0D0
         ELSE IF ( MODE(1:2) == 'h_' ) THEN
           IF ( ILEN(MODE) < 3 ) THEN
                CALL ERR_LOG ( 7102, -2, 'GEN_TRG_DENS', 'Unsupported mode '// &
     &               TRIM(MODE)//' . Supported modes: d3 and h1_mmmm' )
                CALL EXIT ( 1 )
           END IF
           READ ( UNIT=MODE(3:ILEN(MODE)), FMT=*, IOSTAT=IUER ) REF_HEI
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7103, -2, 'GEN_TRG_DENS', 'Unsupported mode '// &
     &               TRIM(MODE)//' a real numbmer should follow prefix h_' )
                CALL EXIT ( 1 )
           END IF
         ELSE
           CALL ERR_LOG ( 7104, -2, 'GEN_TRG_DENS', 'Unsupported mode '// &
     &          TRIM(MODE)//' . Supported modes: d3 and h1_mmmm' )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FIL_O3, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7105, -2, 'GEN_TRG_DENS', 'Cannot find input '// &
     &         'file with ozone mixing ratio '//FIL_O3 )
           RETURN 
      END IF
      ID = ILEN(FIL_O3)
      IF ( ID < 26 ) THEN
           CALL ERR_LOG ( 7106, -2, 'GEN_TRG_DENS', 'Wrong name (too short) '// &
     &         'of the input meteorological file '//FIL_D )
           RETURN 
      END IF
!
      IF ( FIL_O3(ID-7:ID) == '.heb.bz2' ) THEN
           FIL_CO2 = FIL_O3(1:ID-27)//'co2/co2_'//FIL_O3(ID-20:ID)
           FIL_D   = FIL_O3(1:ID-27)//'d/d_'//FIL_O3(ID-20:ID)
           FIL_Q   = FIL_O3(1:ID-27)//'q/q_'//FIL_O3(ID-20:ID)
           FIL_T   = FIL_O3(1:ID-27)//'t/t_'//FIL_O3(ID-20:ID)
        ELSE IF ( FIL_O3(ID-3:ID) == '.heb' ) THEN
           FIL_CO2 = FIL_O3(1:ID-24)//'co2/co2_'//FIL_O3(ID-20:ID)
           FIL_D   = FIL_O3(1:ID-24)//'d/d_'//FIL_O3(ID-20:ID)
           FIL_Q   = FIL_O3(1:ID-24)//'q/q_'//FIL_O3(ID-20:ID)
           FIL_T   = FIL_O3(1:ID-24)//'t/t_'//FIL_O3(ID-20:ID)
        ELSE 
           CALL ERR_LOG ( 7107, -2, 'GEN_TRG_DENS', 'Wrong extension of '// &
     &         'the input water vapor pressure file file '// &
     &          FIL_O3(1:I_LEN(FIL_O3))// &
     &         ' -- extensions .heb or .heb.bz2 were expected' )
           RETURN 
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_O3, HEB_O3, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7108, -2, 'GEN_TRG_DENS', 'Error in reading '// &
     &         'file '//TRIM(FIL_O3)//' with mixing ratio of ozone' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_CO2, HEB_CO2, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7109, -2, 'GEN_TRG_DENS', 'Error in reading '// &
     &         'file '//TRIM(FIL_CO2)//' with mixing ratio of carbon dioxide' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_D, HEB_DELP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7110, -2, 'GEN_TRG_DENS', 'Error in reading '// &
     &         'file '//TRIM(FIL_D)//' with atmospheric pressure thicnkess' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_T, HEB_T, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7111, -2, 'GEN_TRG_DENS', 'Error in reading '// &
     &         'file '//TRIM(FIL_T)//' with air termerature' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_Q, HEB_Q, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7112, -2, 'GEN_TRG_DENS', 'Error in reading '// &
     &         'file '//TRIM(FIL_Q)//' with specific humidity' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_NAT_OGH, HEB_NAT_OGH, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7113, -2, 'GEN_TRG_DENS', 'Error in reading '// &
     &         'file '//TRIM(FIL_NAT_OGH)//' with specific humidity' )
           CALL EXIT ( 1 )
      END IF

      IF ( MODE(1:2) == 'h_' ) THEN
           IUER = -1
           CALL READ_HEB ( FIL_FINE_OGH, HEB_FINE_OGH, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7114, -2, 'GEN_TRG_DENS', 'Error in reading '// &
     &              'file '//TRIM(FIL_FINE_OGH)//' with surface elevation '// &
     &              'height at the fine grid' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IUER = -1
      CALL GEN_TRG_DENS ( HEB_DELP, HEB_T, HEB_Q, HEB_NAT_OGH, HEB_O3, &
     &                   HEB_CO2, HEB_FINE_OGH, HEB_TRG_3D, HEB_TRG_2D, &
     &                   DIMO, MODE, REF_HEI, CNS_DR2, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7115, -2, 'GEN_TRG_DENS', 'Error in generating '// &
     &         'ozone density' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      IF ( MODE(1:2) == 'h_' ) THEN
           CALL WRITE_HEB ( HEB_TRG_2D, HEB_TRG_2D%VAL, FIL_OUT_TRG, IUER )
         ELSE
           CALL WRITE_HEB ( HEB_TRG_3D, HEB_TRG_3D%VAL, FIL_OUT_TRG, IUER )
      END IF
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7116, -2, 'GEN_TRG_DENS', 'Error in writing '// &
     &         'trace gas density into the output file '//FIL_OUT_TRG )
           CALL EXIT ( 1 )
      END IF
      END  PROGRAM   GET_TRG_DENS  !#!#
