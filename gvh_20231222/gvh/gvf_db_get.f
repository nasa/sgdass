      SUBROUTINE GVF_DB_GET ( GVH, GVF_DB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVF_DB_GET
! *                                                                      *
! *  ### 14-OCT-2007   GVF_DB_GET  v1.2 (c)  L. Petrov  01-SEP-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'gvh.i'
      INCLUDE   'gvf_db.i'
      TYPE     ( GVF_DB__TYPE ) :: GVF_DB
      TYPE     ( GVH__STRU    ) :: GVH
      INTEGER*4  IUER
      INTEGER*4  M_CHR, M_ARR, MB, M_LCD
      PARAMETER  ( M_CHR = 8*MAX(GVF_DB__MSRC,GVF_DB__MSTA) )
      PARAMETER  ( M_ARR = 3*MAX(GVF_DB__MSRC,GVF_DB__MSTA) )
      PARAMETER  (    MB = 2 ) ! The number of bands
      PARAMETER  ( M_LCD = 512 ) 
      CHARACTER  STR*(M_CHR), C_LCD(M_LCD)*8, STR1*128
      REAL*8     ARR_R8(M_ARR)
      INTEGER*4  ARR_I4(M_ARR)
      REAL*4     ARR_R4(M_ARR)
      INTEGER*4  J1, J2, J3, J4, J5, IP, DIMS(2), L_STA, IND_SOU, &
     &           IND_SCA, IND_STA(2), MJD_UTC_OBS, LAST_SCA, L_LCD, IER
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF, LINDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IP = LINDEX ( GVH%FILENAME(1), '/' ) + 1
      GVF_DB%DB_NAME = GVH%FILENAME(1)(IP:IP+9)
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_GET_LCODE_LIST ( GVH, M_LCD, L_LCD, C_LCD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7601, IUER, 'GVF_DB_GET', 'Error in an attempt '// &
     &         'to read the LCODE list' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_STA', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%NUMB_STA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7602, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode NUMB_STA' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_OBS', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%NUMB_OBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7603, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode NUMB_OBS' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_SCA', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%NUMB_SCA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7604, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode NUMB_SCA' )
           RETURN
      END IF
!
!@      CALL ERR_PASS ( IUER, IER )
!@      CALL GVH_GLCODE ( GVH, 'APLENGTH', 0, 0, 8, DIMS(1), DIMS(2), &
!@     &                  GVF_DB%SES%APLENGTH, IER )
!@      IF ( IER .NE. 0 ) THEN
!@           CALL ERR_LOG ( 7605, IUER, 'GVF_DB_GET', 'Error in '// &
!@     &         'getting lcode APLENGTH' )
!@           RETURN
!@      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'BAND_NAM', 0, 0, 1*MB, DIMS(1), DIMS(2), &
     &                  %REF(GVF_DB%SES%BAND_NAM), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7606, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode BAND_NAM' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'CORPLACE', 0, 0, 32, DIMS(1), DIMS(2), &
     &                  %REF(GVF_DB%SES%CORPLACE), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7607, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode CORPLACE' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'COR_TYPE', 0, 0, 8, DIMS(1), DIMS(2), &
     &                  %REF(GVF_DB%SES%COR_TYPE), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7608, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode COR_TYPE' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'EXPSERNO', 0, 0, 2, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%EXPSERNO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7609, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode EXPSERNO' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'EXP_CODE', 0, 0, LEN(STR), &
     &                  DIMS(1), DIMS(2), %REF(GVF_DB%SES%EXP_CODE), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7610, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode EXP_CODE' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'EXP_NAME', 0, 0, LEN(STR), &
     &                  DIMS(1), DIMS(2), %REF(GVF_DB%SES%EXP_NAME), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7611, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode EXP_NAME' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'MK3_DBNM', 0, 0, LEN(STR), &
     &                  DIMS(1), DIMS(2), %REF(GVF_DB%SES%MK3_DBNM), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7612, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode MK3_DBNM' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_SOU', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%NUMB_SOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7613, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode NUMB_SCA' )
           RETURN
      END IF
!
      ALLOCATE ( GVF_DB%STA(GVF_DB%SES%NUMB_STA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( GVF_DB%SES%NUMB_STA*SIZEOF(GVF_DB%STA(1)), STR )
           CALL ERR_LOG ( 7614, IUER, 'GVF_DB_GET', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'needed for GVF_DB%STA array' )
           RETURN
      END IF
!
      ALLOCATE ( GVF_DB%SOU(GVF_DB%SES%NUMB_SOU), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( GVF_DB%SES%NUMB_SOU*SIZEOF(GVF_DB%SOU(1)), STR )
           CALL ERR_LOG ( 7615, IUER, 'GVF_DB_GET', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'needed for GVF_DB%SOU array' )
           RETURN
      END IF
!
      ALLOCATE ( GVF_DB%OBS(GVF_DB%SES%NUMB_OBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( GVF_DB%SES%NUMB_OBS*SIZEOF(GVF_DB%OBS(1)), STR )
           CALL ERR_LOG ( 7616, IUER, 'GVF_DB_GET', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'needed for GVF_DB%OBS array' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUM_BAND', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%NUM_BAND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7617, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode NUM_BAND' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUM_CHAN', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%NUM_CHAN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7618, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode NUM_CHAN' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUM_CHBN', 0, 0, 4*MB, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%NUM_CHBN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7619, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode NUM_CHBN' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'N_AVBAND', 0, 0, 4, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%N_AVBAND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7620, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode N_AVBAND' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'PI_NAME ', 0, 0, LEN(STR), DIMS(1), DIMS(2), &
     &                  %REF(GVF_DB%SES%PI_NAME), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7621, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode PI_NAME' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'REC_MODE', 0, 0, LEN(STR), DIMS(1), DIMS(2), &
     &                  %REF(GVF_DB%SES%REC_MODE), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7622, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode REC_MODE' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SKYFRQCH', 0, 0, 8*GVF_DB__MFRQ, &
     &                  DIMS(1), DIMS(2), GVF_DB%SES%SKYFRQCH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7623, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode SKYFRQCH' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'UTC_MTAI', 0, 0, 8, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%UTC_MTAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7624, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode UTC_MTAI' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'BITSAMPL', 0, 0, 2, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%BITSAMPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7625, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode BITSAMPL' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SAMPLRAT', 0, 0, 8, DIMS(1), DIMS(2), &
     &                  GVF_DB%SES%SAMPLRAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7626, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode SAMPLRAT' )
           RETURN
      END IF
!
!@      CALL ERR_PASS   ( IUER, IER )
!@      CALL GVH_GLCODE ( GVH, 'CABL_SGN', 0, 0, 2*GVF_DB__MSTA, &
!@     &                  DIMS(1), DIMS(2), GVF_DB%SES%CABL_SGN, IER )
!@      IF ( IER .NE. 0 ) THEN
!@           CALL ERR_LOG ( 7627, IUER, 'GVF_DB_GET', 'Error in '// &
!@     &         'getting lcode CABL_SGN' )
!@           RETURN
!@      END IF
!
!@      CALL ERR_PASS   ( IUER, IER )
!@      CALL GVH_GLCODE ( GVH, 'CAL_INFO', 0, 0, 4*GVF_DB__MSTA*GVF_DB__MCAL, &
!@     &                  DIMS(1), DIMS(2), GVF_DB%SES%CAL_INFO, IER )
!@      IF ( IER .NE. 0 ) THEN
!@           CALL ERR_LOG ( 7628, IUER, 'GVF_DB_GET', 'Error in '// &
!@     &         'getting lcode CAL_INFO' )
!@           RETURN
!@      END IF
!
!@      CALL ERR_PASS   ( IUER, IER )
!@      CALL GVH_GLCODE ( GVH, 'CAL_NAME', 0, 0, 8*GVF_DB__MSTA, &
!@     &                  DIMS(1), DIMS(2), GVF_DB%SES%CAL_NAME, IER )
!@      IF ( IER .NE. 0 ) THEN
!@           CALL ERR_LOG ( 7629, IUER, 'GVF_DB_GET', 'Error in '// &
!@     &         'getting lcode CAL_NAME' )
!@           RETURN
!@      END IF
!
!@      CALL ERR_PASS   ( IUER, IER )
!@      CALL GVH_GLCODE ( GVH, 'MEANCABL', 0, 0, 8*GVF_DB__MSTA, &
!@     &                  DIMS(1), DIMS(2), GVF_DB%SES%MEANCABL, IER )
!@      IF ( IER .NE. 0 ) THEN
!@           CALL ERR_LOG ( 7630, IUER, 'GVF_DB_GET', 'Error in '// &
!@     &         'getting lcode MEANCABL' )
!@           RETURN
!@      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'N_CALIB ', 0, 0, 2, &
     &                  DIMS(1), DIMS(2), GVF_DB%SES%N_CALIB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7631, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode N_CALIB' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SITNAMES', 0, 0, 8*GVF_DB%SES%NUMB_STA, &
     &                  DIMS(1), DIMS(2), %REF(STR), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7632, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode SIT_NAMES' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SIT_COOR', 0, 0, 3*8*GVF_DB%SES%NUMB_STA, &
     &                  DIMS(1), DIMS(2), ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7633, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode SIT_COOR' )
           RETURN
      END IF
!
      DO 410 J1=1,GVF_DB%SES%NUMB_STA
         GVF_DB%STA(J1)%SITNAMES = STR((J1-1)*8+1:(J1-1)*8+8)
         GVF_DB%STA(J1)%SIT_COOR(1) = ARR_R8((J1-1)*3+1)
         GVF_DB%STA(J1)%SIT_COOR(2) = ARR_R8((J1-1)*3+2)
         GVF_DB%STA(J1)%SIT_COOR(3) = ARR_R8((J1-1)*3+3)
 410  CONTINUE
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SRCNAMES', 0, 0, 8*GVF_DB%SES%NUMB_SOU, &
     &                  DIMS(1), DIMS(2), %REF(STR), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7634, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode SRCNAMES' )
           RETURN
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SOU_COOR', 0, 0, 2*8*GVF_DB%SES%NUMB_SOU, &
     &                  DIMS(1), DIMS(2), ARR_R8, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7635, IUER, 'GVF_DB_GET', 'Error in '// &
     &         'getting lcode SOU_COOR' )
           RETURN
      END IF
!
      DO 420 J2=1,GVF_DB%SES%NUMB_SOU
         GVF_DB%SOU(J2)%SRCNAMES = STR((J2-1)*8+1:(J2-1)*8+8)
         GVF_DB%SOU(J2)%SOU_COOR(1) = ARR_R8((J2-1)*2+1)
         GVF_DB%SOU(J2)%SOU_COOR(2) = ARR_R8((J2-1)*2+2)
 420  CONTINUE
!
      DO 430 J3=1,GVF_DB%SES%NUMB_OBS
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SOU_IND ', J3, 0, 4, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%SOU_IND, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7636, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode SOU_IND' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'STA_IND ', J3, 1, 2*4, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%STA_IND, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7637, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode STA_IND' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'MJD_OBS ', J3, 0, 4, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%MJD_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7638, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode MJD_OBS' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'UTC_OBS ', J3, 0, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%UTC_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7639, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode UTC_OBS' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'N_GRAMB ', J3, 0, 4*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%N_GRAMB, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7640, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode N_GRAMB' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'DEL_RATE', J3, 0, 8*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%DEL_RATE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7641, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode DEL_RATE' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'PHRATERR', J3, 0, 8*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%PHRATERR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7642, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode PHRATERR' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'GDAMBSP ', J3, 0, 8*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%GDAMBSP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7643, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode GDAMBSP' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'AUTO_SUP', J3, 1, 4, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%AUTO_SUP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7644, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode AUTO_SUP' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'USER_SUP', J3, 0, 4, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%USER_SUP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7645, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode USER_SUP' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'USER_REC', J3, 0, 4, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%USER_REC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7646, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode USER_REC' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'GR_DELAY', J3, 0, 8*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%GR_DELAY, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7647, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode GR_DELAY' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'GRDELERR', J3, 0, 8*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%GRDELERR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7648, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode GRDELERR' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SB_DELAY', J3, 0, 8*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%SB_DELAY, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7649, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode SB_DELAY' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SBDELERR', J3, 0, 8*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%SBDELERR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7650, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode SBDELERR' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'TOTPHASE', J3, 0, 8*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%TOTPHASE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7651, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode TOTPHASE' )
              RETURN
         END IF
!@         GVF_DB%OBS(J3)%TOTPHASE = GVF_DB%OBS(J3)%TOTPHASE*DEG__TO__RAD ! ???
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SNRATIO ', J3, 0, 8*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%SNRATIO, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7652, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode SNRATIO' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'FRN_AMPL', J3, 0, 8*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%FRN_AMPL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7653, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode FRN_AMPL' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SCANNAME', J3, 0, 16, DIMS(1), DIMS(2), &
     &                     %REF(GVF_DB%OBS(J3)%SCANNAME), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7654, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode SCANNAME' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'QUALCODE', J3, 0, 4, DIMS(1), DIMS(2), &
     &                     %REF(GVF_DB%OBS(J3)%QUALCODE), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7655, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode QUALCODE' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'ION_GDEL', J3, 0, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%ION_GDEL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7656, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode ION_GDEL' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'ION_GERR', J3, 0, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%ION_GERR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7657, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode ION_GERR' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'ION_PRAT', J3, 0, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%ION_PRAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7658, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode ION_PRAT' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'ION_RERR', J3, 0, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%ION_RERR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7659, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode ION_RERR' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'REF_FREQ', J3, 0, 8*MB, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%REF_FREQ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7660, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode REF_FREQ' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'AIR_TEMP', J3, 1, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%AIR_TEMP(1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7661, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode AIR_TEMP(1)' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SCAN_DUR', J3, 1, MB*8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%SCAN_DUR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7662, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode SCAN_DUR' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'AIR_TEMP', J3, 2, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%AIR_TEMP(2), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7663, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode AIR_TEMP(2)' )
              RETURN
         END IF
!
!@         CALL ERR_PASS   ( IUER, IER )
!@         CALL GVH_GLCODE ( GVH, 'CABL_DEL', J3, 1, 8, DIMS(1), DIMS(2), &
!@     &                     GVF_DB%OBS(J3)%CABL_DEL(1), IER )
!@         IF ( IER .NE. 0 ) THEN
!@              CALL ERR_LOG ( 7664, IUER, 'GVF_DB_GET', 'Error in '// &
!@     &            'getting lcode CABL_DEL(1)' )
!@              RETURN
!@         END IF
!
!@         CALL ERR_PASS   ( IUER, IER )
!@         CALL GVH_GLCODE ( GVH, 'CABL_DEL', J3, 2, 8, DIMS(1), DIMS(2), &
!@     &                     GVF_DB%OBS(J3)%CABL_DEL(2), IER )
!@         IF ( IER .NE. 0 ) THEN
!@              CALL ERR_LOG ( 7665, IUER, 'GVF_DB_GET', 'Error in '// &
!@     &            'getting lcode CABL_DEL(2)' )
!@              RETURN
!@         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'REL_HUMD', J3, 1, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%REL_HUMD(1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7666, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode REL_HUMD(1)' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'REL_HUMD', J3, 2, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%REL_HUMD(1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7667, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode REL_HUMD(1)' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'PIND_OBS', J3, 1, 4, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%PIND_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7667, IUER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode PIND_OBS' )
              RETURN
         END IF
!
         IER = 0
         CALL GVH_GLCODE ( GVH, 'AZIMUTH ', J3, 1, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%AZIMUTH(1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL ERR_LOG ( 7668, IER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode AZIMUTH(1)' )
!!              RETURN
              GVF_DB%OBS(J3)%AZIMUTH(1) = 0.0D0
         END IF
!
         IER = 0
         CALL GVH_GLCODE ( GVH, 'AZIMUTH ', J3, 2, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%AZIMUTH(2), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL ERR_LOG ( 7669, IER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode AZIMUTH(2)' )
!!              RETURN
              GVF_DB%OBS(J3)%AZIMUTH(2) = 0.0D0
         END IF
!
         IER = 0
         CALL GVH_GLCODE ( GVH, 'ELEV    ', J3, 1, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%ELEV(1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL ERR_LOG ( 7670, IER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode ELEV(1)' )
!!              RETURN
              GVF_DB%OBS(J3)%ELEV(1) = 0.0D0
         END IF
!
         IER = 0
         CALL GVH_GLCODE ( GVH, 'ELEV    ', J3, 2, 8, DIMS(1), DIMS(2), &
     &                     GVF_DB%OBS(J3)%ELEV(2), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL ERR_LOG ( 7671, IER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode ELEV(2)' )
              GVF_DB%OBS(J3)%ELEV(2) = 0.0D0
!!              RETURN
         END IF
!
         IER = 0
         CALL GVH_GLCODE ( GVH, 'DER_DEL ', J3, 0, GVF_DB__MDER*8, DIMS(1), &
     &                     DIMS(2), GVF_DB%OBS(J3)%DER_DEL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL ERR_LOG ( 7672, IER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode DER_DEL' )
              GVF_DB%OBS(J3)%DER_DEL = 0.0D0
!!              RETURN
         END IF
!
         IER = 0
         CALL GVH_GLCODE ( GVH, 'DER_RAT ', J3, 0, GVF_DB__MDER*8, DIMS(1), &
     &                     DIMS(2), GVF_DB%OBS(J3)%DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL ERR_LOG ( 7673, IER, 'GVF_DB_GET', 'Error in '// &
     &            'getting lcode DER_RAT' )
              GVF_DB%OBS(J3)%DER_RAT = 0.0D0
!!              RETURN
         END IF
!
         IF ( LTM_DIF ( 1, L_LCD, C_LCD, 'UV_COOR' ) > 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'UV_COOR ', J3, 0, 2*8, &
     &                          DIMS(1), DIMS(2), GVF_DB%OBS(J3)%UV_COOR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7674, IUER, 'GVF_DB_GET', 'Error in '// &
     &                 'getting lcode UV_COOR' )
                   RETURN
              END IF
            ELSE 
              GVF_DB%OBS(J3)%UV_COOR = 0.0D0
         END IF
!
         IF ( LTM_DIF ( 1, L_LCD, C_LCD, 'TSYS1   ' ) > 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'TSYS1   ', J3, 1, GVF_DB%SES%NUM_CHBN(1)*4, &
     &                          DIMS(1), DIMS(2), ARR_R4, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7675, IUER, 'GVF_DB_GET', 'Error in '// &
     &                 'getting lcode TSYS1' )
                   RETURN
              END IF
              GVF_DB%OBS(J3)%TSYS(1,1) = ARR_R4(1)
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'TSYS1   ', J3, 2, GVF_DB%SES%NUM_CHBN(1)*4, &
     &                          DIMS(1), DIMS(2), ARR_R4, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7676, IUER, 'GVF_DB_GET', 'Error in '// &
     &                 'getting lcode TSYS1' )
                   RETURN
              END IF
              GVF_DB%OBS(J3)%TSYS(2,1) = ARR_R4(1)
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, * ) 'gvf_db_get sta1 j3= ', int2(j3), ' arr_r4= ', arr_r4(1:gvf_db%ses%num_chbn(1)) ! %%%%%%%%
!  write ( 6, * ) 'gvf_db_get sta2 j3= ', int2(j3), ' arr_r4= ', arr_r4(1:gvf_db%ses%num_chbn(1)) ! %%%%%%%%
!            STR1 = MJDSEC_TO_DATE ( GVF_DB%OBS(J3)%MJD_OBS, &
!     &                              GVF_DB%OBS(J3)%UTC_OBS, -2 )
!            WRITE ( 6, 230 ) J3, &
!     &                       GVF_DB%STA(GVF_DB%OBS(J3)%STA_IND(1))%SITNAMES, &
!     &                       GVF_DB%STA(GVF_DB%OBS(J3)%STA_IND(2))%SITNAMES, &
!     &                       GVF_DB%SOU(GVF_DB%OBS(J3)%SOU_IND)%SRCNAMES, &
!     &                       STR1(1:21), &
!     &                       GVF_DB%OBS(J3)%TSYS(1,1), &
!     &                       GVF_DB%OBS(J3)%TSYS(2,1)
! 230        FORMAT ( I5, ' > ', A, 1X, A, 2X, A, 2X, A, 2X, ' Tsys: ', &
!     &                       F6.1, 1X F6.1 )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            ELSE 
              GVF_DB%OBS(J3)%TSYS(1,1) = 0.0D0
              GVF_DB%OBS(J3)%TSYS(2,1) = 0.0D0
         END IF
!
         IF ( LTM_DIF ( 1, L_LCD, C_LCD, 'TSYS2   ' ) > 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'TSYS2   ', J3, 1, GVF_DB%SES%NUM_CHBN(2)*4, &
     &                          DIMS(1), DIMS(2), ARR_R4, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7677, IUER, 'GVF_DB_GET', 'Error in '// &
     &                 'getting lcode TSYS2' )
                   RETURN
              END IF
              GVF_DB%OBS(J3)%TSYS(1,2) = ARR_R4(1)
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'TSYS2   ', J3, 2, GVF_DB%SES%NUM_CHBN(2)*4, &
     &                          DIMS(1), DIMS(2), ARR_R4, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7678, IUER, 'GVF_DB_GET', 'Error in '// &
     &                 'getting lcode TSYS2' )
                   RETURN
              END IF
              GVF_DB%OBS(J3)%TSYS(2,2) = ARR_R4(1)
            ELSE 
              GVF_DB%OBS(J3)%TSYS(1,2) = 0.0D0
              GVF_DB%OBS(J3)%TSYS(2,2) = 0.0D0
         END IF
         IF ( LTM_DIF ( 1, L_LCD, C_LCD, 'ANT_GAIN' ) > 0 ) THEN
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'ANT_GAIN', J3, 1, GVF_DB%SES%NUM_BAND*8, &
     &                          DIMS(1), DIMS(2), ARR_R4, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG ( 7679, IER, 'GVF_DB_GET', 'Error in '// &
     &                 'getting lcode ANT_GAIN' )
!!                   RETURN
                   ARR_R4 = 0.0D0
              END IF
              GVF_DB%OBS(J3)%GAIN(1:GVF_DB%SES%NUM_BAND,1) = ARR_R4(1:GVF_DB%SES%NUM_BAND)
!
              CALL ERR_PASS   ( IUER, IER )
              CALL GVH_GLCODE ( GVH, 'ANT_GAIN', J3, 2, GVF_DB%SES%NUM_BAND*8, &
     &                          DIMS(1), DIMS(2), ARR_R4, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG ( 7680, IER, 'GVF_DB_GET', 'Error in '// &
     &                 'getting lcode ANT_GAIN1' )
!!                   RETURN
                   ARR_R4 = 0.0D0
              END IF
              GVF_DB%OBS(J3)%GAIN(1:GVF_DB%SES%NUM_BAND,2) = ARR_R4(1:GVF_DB%SES%NUM_BAND)
            ELSE 
              GVF_DB%OBS(J3)%GAIN = 0.0D0
         END IF
 430  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVF_DB_GET  !#!
