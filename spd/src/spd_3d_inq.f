      PROGRAM    SPD_3D_INQ
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'astro_constants.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_3D_DEL
      CHARACTER  FIL_BSPL*128, STR*128
      INTEGER*4  IS, J1, J2, J3, IL, L_FIL, IVRB, MJD, IUER
      REAL*8     TIM
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, GET_FILE_FROM_DIR 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage spd_3d_inq file_bspl' 
           CALL EXIT ( 0 )
         ELSE
           CALL GETARG ( 1, FIL_BSPL )
      END IF
      IUER = -1
      CALL SPD_3D_BIN_READ_HEAD ( FIL_BSPL, SPD_3D_DEL, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7001, IUER, 'SPD_3D_INQ', 'Error in an attempt to '// &
     &         'read input file '//FIL_BSPL(1:I_LEN(FIL_BSPL))//' in BSPD format' )
           CALL EXIT ( 0 )
      END IF
      IUER = -1
      STR = MJDSEC_TO_DATE ( SPD_3D_DEL%TIM%MJD_BEG, SPD_3D_DEL%TIM%TAI_BEG, IUER )
      WRITE ( 6, * ) 'Start time: ', STR(1:21)
!
      IUER = -1
      STR = MJDSEC_TO_DATE ( SPD_3D_DEL%TIM%MJD_END, SPD_3D_DEL%TIM%TAI_END, IUER )
      WRITE ( 6, * ) 'End time:   ', STR(1:21)
!
      END  PROGRAM  SPD_3D_INQ  !#!#
