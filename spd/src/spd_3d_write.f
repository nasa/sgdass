      SUBROUTINE SPD_3D_WRITE ( WRITE_MODE, IND_STA, SPD, OUT_PREF, &
     &                          N_MOD, MOD_TEXT, N_INP, INP_TEXT, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_WRITE 
! *                                                                      *
! *  ### 30-NOV-2008   SPD_3D_WRITE  v2.4 (c) L. Petrov 12-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'astro_constants.i'
      INTEGER*4  WRITE_MODE, IND_STA, IVRB, N_MOD, N_INP, IUER
      CHARACTER  OUT_PREF*(*), MOD_TEXT(N_MOD)*(*), INP_TEXT(N_INP)*(*)
      TYPE     ( SPD_3D__TYPE     ) :: SPD
      TYPE     ( SPD__ASCII__TYPE ) :: SAT
      CHARACTER  FILOUT*128, STR*256, DATE_STR*32, COM*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, &
     &           MJD, IOS, NR, IS, IER
      REAL*8     ARG_MIN, ARG_MAX, ARG, AZ, EL, DELS(2), UTC_M_TAI
      REAL*8     UTC, TAI
      LOGICAL*1, EXTERNAL :: IS_R4_NAN
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, SYSTEM
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, GET_CDATE*19, MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: DEL_ISA, INV_MAP_ISA 
!
      IF ( SPD%LEAPSEC%STATUS .NE. SPD__LOAD .AND. SPD%UTC_M_TAI_STATUS .NE. SPD__COMP ) THEN
!$OMP      CRITICAL (LOAD_LEAPSEC_BLOCK)
           CALL ERR_PASS ( IUER, IER )
           CALL SPD_LOAD_LEAPSEC ( SPD, IER )
!$OMP      END CRITICAL (LOAD_LEAPSEC_BLOCK)
           IF ( IER .NE. 0 ) THEN
!$OMP           CRITICAL (ERR_LOG_BLOCK)
                CALL ERR_LOG ( 5511, IUER, 'SPD_3D_WRITE', 'Failure to load '// &
     &              'and parse leap second file' )
!$OMP           END CRITICAL (ERR_LOG_BLOCK)
                RETURN 
           END IF
      END IF
!
      IF ( WRITE_MODE == SPD__WRITE_ASC ) THEN
           CONTINUE
         ELSE IF ( WRITE_MODE == SPD__WRITE_BIN ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL SPD_3D_BIN_WRITE ( IND_STA, SPD, OUT_PREF, &
     &                             N_MOD, MOD_TEXT, N_INP, INP_TEXT, &
     &                             IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5512, IUER, 'SPD_3D_WRITE', 'Failure in '// &
     &              'an attempt to write in the binary file ' )
                RETURN 
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- NB: the code below is only for the case when we write the file in 
! --- the ascii mode.
!
      CALL NOUT ( SIZEOF(SAT), SAT )
      SAT%NM = N_MOD
      SAT%NI = N_INP
      SAT%NS = SPD%NSTA
      SAT%NE = SPD%CONF%N_EL 
      SAT%NA = SPD%CONF%N_AZ
      SAT%NF = SPD%CONF%N_FRQ
      CALL CLRCH ( STR )
      STR(1:1) = 'N'
      CALL LIB$MOVC3 ( SIZEOF(SAT%NLINE), %REF(STR), SAT%NLINE )
      WRITE ( UNIT=SAT%NLINE%N_MOD, FMT='(I4)' ) SAT%NM 
      WRITE ( UNIT=SAT%NLINE%N_INP, FMT='(I4)' ) SAT%NI
      WRITE ( UNIT=SAT%NLINE%N_STA, FMT='(I7)' ) SAT%NS
      WRITE ( UNIT=SAT%NLINE%N_EL,  FMT='(I4)' ) SAT%NE
      WRITE ( UNIT=SAT%NLINE%N_AZ,  FMT='(I4)' ) SAT%NA
      WRITE ( UNIT=SAT%NLINE%N_FRQ, FMT='(I4)' ) SAT%NF
!
      ALLOCATE ( SAT%MLINE(SAT%NM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5514, IUER, 'SPD_3D_WRITE', 'Failure to '// &
     &         'allocate memory for SAT%MLINE' )
           RETURN 
      END IF
!
      IF ( SAT%NF > 0 ) THEN
           ALLOCATE ( SAT%FLINE(SAT%NF), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5515, IUER, 'SPD_3D_WRITE', 'Failure to '// &
     &              'allocate memory for SAT%FLINE' )
                RETURN 
           END IF
      END IF
!
      ALLOCATE ( SAT%ILINE(SAT%NI), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5516, IUER, 'SPD_3D_WRITE', 'Failure to '// &
     &         'allocate memory for SAT%ILINE' )
           RETURN 
      END IF
!
      ALLOCATE ( SAT%SLINE(SAT%NS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5517, IUER, 'SPD_3D_WRITE', 'Failure to '// &
     &         'allocate memory for SAT%SLINE' )
           RETURN 
      END IF
!
      ALLOCATE ( SAT%ELINE(SAT%NE), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5518, IUER, 'SPD_3D_WRITE', 'Failure to '// &
     &         'allocate memory for SAT%ELINE' )
           RETURN 
      END IF
!
      ALLOCATE ( SAT%ALINE(SAT%NA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5519, IUER, 'SPD_3D_WRITE', 'Failure to '// &
     &         'allocate memory for SAT%ALINE' )
           RETURN 
      END IF
!
      ALLOCATE ( SAT%PLINE(SAT%NS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5520, IUER, 'SPD_3D_WRITE', 'Failure to '// &
     &         'allocate memory for SAT%PLINE' )
           RETURN 
      END IF
!
      ALLOCATE ( SAT%DLINE(SAT%NE,SAT%NA,SAT%NS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5521, IUER, 'SPD_3D_WRITE', 'Failure to '// &
     &         'allocate memory for SAT%DLINE' )
           RETURN 
      END IF
!
      IF ( SAT%NF > 0 ) THEN
           ALLOCATE ( SAT%OLINE(SAT%NF,SAT%NE,SAT%NA,SAT%NS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5522, IUER, 'SPD_3D_WRITE', 'Failure to '// &
     &              'allocate memory for SAT%OLINE' )
                RETURN 
           END IF
      END IF
!
      DO 410 J1=1,SAT%NM 
         SAT%MLINE(J1)%CODE   = 'M'
         SAT%MLINE(J1)%FILL_1 = '  '
         WRITE ( UNIT=SAT%MLINE(J1)%IND_LINE, FMT='(I4)' ) J1
         SAT%MLINE(J1)%FILL_2 = '  '
         SAT%MLINE(J1)%TEXT   = MOD_TEXT(J1)
 410  CONTINUE 
!
      DO 420 J2=1,SAT%NI
         SAT%ILINE(J2)%CODE   = 'I'
         SAT%ILINE(J2)%FILL_1 = '  '
         WRITE ( UNIT=SAT%ILINE(J2)%IND_LINE, FMT='(I4)' ) J2
         SAT%ILINE(J2)%FILL_2 = '  '
         SAT%ILINE(J2)%TEXT   = INP_TEXT(J2)
 420  CONTINUE 
!
      SAT%ULINE%CODE   = 'U'
      SAT%ULINE%FILL_1 = '  '
      SAT%ULINE%FILL_2 = '  '
      SAT%ULINE%DEL1_CODE = SPD__TOT_STR 
      SAT%ULINE%DEL2_CODE = SPD__WAT_STR 
!
      IF ( SAT%NF > 0 ) THEN
           DO 430 J3=1,SAT%NF
              SAT%FLINE(J3)%CODE   = 'F'
              SAT%FLINE(J3)%FILL_1 = '  '
              WRITE ( UNIT=SAT%FLINE(J3)%FRQ_IND, FMT='(I4)'      ) J3
              SAT%FLINE(J3)%FILL_2 = '  '
              WRITE ( UNIT=SAT%FLINE(J3)%FRQ,     FMT='(1PD15.7)' ) SPD%CONF%FRQ_ARR(J3)
 430       CONTINUE 
      END IF
!
      DO 440 J4=1,SAT%NS
         SAT%SLINE(J4)%CODE   = 'S'
         SAT%SLINE(J4)%FILL_1 = ' '
	 WRITE ( UNIT=SAT%SLINE(J4)%STA_IND_STR, FMT='(I7)' ) J4
         SAT%SLINE(J4)%FILL_2 = '  '
	 SAT%SLINE(J4)%STA_NAME = SPD%STA(J4)%NAME
         SAT%SLINE(J4)%FILL_3 = '  '
	 WRITE ( UNIT=SAT%SLINE(J4)%X_COOR,    FMT='(F12.3)' ) SPD%STA(J4)%COO_CFS(1)
         SAT%SLINE(J4)%FILL_4 = '  '
	 WRITE ( UNIT=SAT%SLINE(J4)%Y_COOR,    FMT='(F12.3)' ) SPD%STA(J4)%COO_CFS(2)
         SAT%SLINE(J4)%FILL_5 = '  '
	 WRITE ( UNIT=SAT%SLINE(J4)%Z_COOR,    FMT='(F12.3)' ) SPD%STA(J4)%COO_CFS(3)
         SAT%SLINE(J4)%FILL_6 = '  '
	 WRITE ( UNIT=SAT%SLINE(J4)%LAT_GCN,   FMT='(F8.4)'  ) SPD%STA(J4)%LAT_GDT/DEG__TO__RAD
         SAT%SLINE(J4)%FILL_7 = '  '
	 WRITE ( UNIT=SAT%SLINE(J4)%LON,       FMT='(F8.4)'  ) SPD%STA(J4)%LON/DEG__TO__RAD
         SAT%SLINE(J4)%FILL_8 = '  '
	 WRITE ( UNIT=SAT%SLINE(J4)%HEI_ELL,   FMT='(F6.1)'  ) SPD%STA(J4)%HEI_ELL
         SAT%SLINE(J4)%FILL_9 = '  '
	 WRITE ( UNIT=SAT%SLINE(J4)%HEI_GEOID, FMT='(F6.1)'  ) SPD%STA(J4)%HEI_GEOID
 440  CONTINUE 
!
      DO 450 J5=1,SAT%NE
         SAT%ELINE(J5)%CODE   = 'E'
         SAT%ELINE(J5)%FILL_1 = '  '
         SAT%ELINE(J5)%FILL_2 = '  '
         WRITE ( UNIT=SAT%ELINE(J5)%ANG_IND, FMT='(I4)'    ) J5
         WRITE ( UNIT=SAT%ELINE(J5)%ANG,     FMT='(F10.6)' ) SPD%ELV%ELEV(J5)/DEG__TO__RAD
 450  CONTINUE 
!
      DO 460 J6=1,SAT%NA
         SAT%ALINE(J6)%CODE   = 'A'
         SAT%ALINE(J6)%FILL_1 = '  '
         SAT%ALINE(J6)%FILL_2 = '  '
         WRITE ( UNIT=SAT%ALINE(J6)%ANG_IND, FMT='(I4)'    ) J6
         WRITE ( UNIT=SAT%ALINE(J6)%ANG,     FMT='(F10.6)' ) SPD%AZM%AZIM(J6)/DEG__TO__RAD
 460  CONTINUE 
!
      SAT%TLINE%CODE   = 'T'
      SAT%TLINE%FILL_1 = '  '
      SAT%TLINE%FILL_2 = '  '
!
      CALL ERR_PASS ( IUER, IER )
      SAT%TLINE%DATE_STR = MJDSEC_TO_DATE ( SPD%MJD, SPD%TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5523, IUER, 'SPD_3D_WRITE', 'Trap of '// &
     &         'internal control: failure to convert the date' )
           RETURN 
      END IF
!
      IF ( SPD%UTC_M_TAI_STATUS == SPD__COMP ) THEN
           SPD%UTC = SPD%TAI - SPD%UTC_M_TAI
         ELSE 
           CALL ERR_PASS ( IUER, IER )
           CALL SPD_TAI_TO_UTC ( SPD, SPD%MJD, SPD%TAI, SPD%UTC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5524, IUER, 'SPD_3D_WRITE', 'Failure to '// &
     &              'compute value of UTC function on moment of time' )
                RETURN 
           END IF
      END IF
      WRITE ( UNIT=SAT%TLINE%UTC_M_TAI_STR, FMT='(F5.1)' ) SPD%UTC_M_TAI
!
      DO 480 J8=1,SAT%NS
         SAT%PLINE(J8)%CODE   = 'P'
         SAT%PLINE(J8)%FILL_1 = ' '
         WRITE ( UNIT=SAT%PLINE(J8)%STA_IND, FMT='(I7)' ) J8
         SAT%PLINE(J8)%FILL_2 = '  '
         WRITE ( UNIT=SAT%PLINE(J8)%PRES, FMT='(F8.1)' ) SPD%STA(J8)%SUR_PRS
         SAT%PLINE(J8)%FILL_3 = '  '
         WRITE ( UNIT=SAT%PLINE(J8)%WATER_VAPOR_PRES, FMT='(F8.2)' ) SPD%STA(J8)%SUR_PWP
         SAT%PLINE(J8)%FILL_4 = '  '
         WRITE ( UNIT=SAT%PLINE(J8)%TEMP, FMT='(F5.1)' ) SPD%STA(J8)%SUR_TEM 
         DO 490 J9=1,SAT%NA
            DO 4100 J10=1,SAT%NE
               SAT%DLINE(J10,J9,J8)%CODE = 'D'
               SAT%DLINE(J10,J9,J8)%FILL_1 = ' '
               WRITE ( UNIT=SAT%DLINE(J10,J9,J8)%STA_IND, FMT='(I7)' ) J8
               SAT%DLINE(J10,J9,J8)%FILL_2 = '  '
               WRITE ( UNIT=SAT%DLINE(J10,J9,J8)%EL_IND,  FMT='(I4)' ) J10
               SAT%DLINE(J10,J9,J8)%FILL_3 = '  '
               WRITE ( UNIT=SAT%DLINE(J10,J9,J8)%AZ_IND,  FMT='(I4)' ) J9
               SAT%DLINE(J10,J9,J8)%FILL_4 = '  '
               SAT%DLINE(J10,J9,J8)%FILL_5 = '  '
!
! ------------ Check whether we are attempting to write negative number.
! ------------ If yes, then flash it to zero.
!
               IF ( SPD%STA(J8)%DEL(J10,J9,SPD__TOT) < 0.0 ) THEN
                    SPD%STA(J8)%DEL(J10,J9,SPD__TOT) = 0.0
               END IF
               IF ( SPD%STA(J8)%DEL(J10,J9,SPD__WAT) < 0.0 ) THEN
                    SPD%STA(J8)%DEL(J10,J9,SPD__WAT) = 0.0
               END IF
               WRITE ( UNIT=SAT%DLINE(J10,J9,J8)%DEL1, FMT='(1PD12.6)' ) &
     &                 SPD%STA(J8)%DEL(J10,J9,SPD__TOT)
               WRITE ( UNIT=SAT%DLINE(J10,J9,J8)%DEL2, FMT='(1PD12.6)' ) &
     &                 SPD%STA(J8)%DEL(J10,J9,SPD__WAT)
               IF ( SAT%NF > 0 ) THEN
                    DO 4110 J11=1,SAT%NF
                       SAT%OLINE(J11,J10,J9,J8)%CODE = 'O'
                       SAT%OLINE(J11,J10,J9,J8)%FILL_1 = ' '
                       WRITE ( UNIT=SAT%OLINE(J11,J10,J9,J8)%STA_IND,  FMT='(I7)' ) J8
                       SAT%OLINE(J11,J10,J9,J8)%FILL_2 = '  '
                       WRITE ( UNIT=SAT%OLINE(J11,J10,J9,J8)%EL_IND,   FMT='(I4)' ) J10
                       SAT%OLINE(J11,J10,J9,J8)%FILL_3 = '  '
                       WRITE ( UNIT=SAT%OLINE(J11,J10,J9,J8)%AZ_IND,   FMT='(I4)' ) J9
                       SAT%OLINE(J11,J10,J9,J8)%FILL_4 = '  '
                       WRITE ( UNIT=SAT%OLINE(J11,J10,J9,J8)%FRQ_IND,  FMT='(I4)' ) J11
                       SAT%OLINE(J11,J10,J9,J8)%FILL_5 = '  '
                       WRITE ( UNIT=SAT%OLINE(J11,J10,J9,J8)%OPA,      FMT='(F6.4)' ) SPD%STA(J8)%OPA(J11,J10,J9) 
                       IF ( IS_R4_NAN(SPD%STA(J8)%OPA(J11,J10,J9)) ) THEN
                            WRITE ( 6, * ) 'J11= ', J11, ' J10 = ', J10, ' J9= ', J9
                            CALL ERR_LOG ( 5525, IUER, 'SPD_3D_WRITE', 'Opacity is NAN '// &
     &                          'for station '//SPD%STA(J8)%NAME )
                            RETURN 
                       END IF 
                       IF ( IS_R4_NAN(SPD%STA(J8)%TAT(J11,J10,J9)) ) THEN
                            WRITE ( 6, * ) 'J11= ', J11, ' J10 = ', J10, ' J9= ', J9
                            CALL ERR_LOG ( 5526, IUER, 'SPD_3D_WRITE', 'Tatm is NAN '// &
     &                          'for station '//SPD%STA(J8)%NAME )
                            RETURN 
                       END IF 
                       IF ( SPD%STA(J8)%OPA(J11,J10,J9) > 9.9999 ) THEN
                            WRITE ( UNIT=SAT%OLINE(J11,J10,J9,J8)%OPA, FMT='(F6.3)' ) SPD%STA(J8)%OPA(J11,J10,J9) 
                       END IF
                       IF ( SPD%STA(J8)%OPA(J11,J10,J9) > 99.999 ) THEN
                            WRITE ( UNIT=SAT%OLINE(J11,J10,J9,J8)%OPA, FMT='(F6.2)' ) SPD%STA(J8)%OPA(J11,J10,J9) 
                       END IF
                       IF ( SPD%STA(J8)%OPA(J11,J10,J9) > 999.99 ) THEN
                            WRITE ( UNIT=SAT%OLINE(J11,J10,J9,J8)%OPA, FMT='(F6.1)' ) SPD%STA(J8)%OPA(J11,J10,J9) 
                       END IF
                       SAT%OLINE(J11,J10,J9,J8)%FILL_6 = '  '
                       WRITE ( UNIT=SAT%OLINE(J11,J10,J9,J8)%TAT,      FMT='(F6.2)' ) SPD%STA(J8)%TAT(J11,J10,J9) 
 4110               CONTINUE 
               END IF
 4100       CONTINUE 
 490     CONTINUE 
 480  CONTINUE 
!
      FILOUT = OUT_PREF(1:I_LEN(OUT_PREF))// &
     &         SAT%TLINE%DATE_STR(1:4)// &
     &         SAT%TLINE%DATE_STR(6:7)// &
     &         SAT%TLINE%DATE_STR(9:10)//'_'// &
     &         SAT%TLINE%DATE_STR(12:13)// &
     &         SAT%TLINE%DATE_STR(15:16)// &
     &         '.spd'
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_ASCII_WRITE ( SPD, SAT, FILOUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5527, IUER, 'SPD_3D_WRITE', 'Failure to '// &
     &         'write the output file with slanted path delays '//FILOUT )
           RETURN 
      END IF
!
! --- Release dynamic memory
!
      CALL SAT_QUIT ( SAT )
!
      IF ( SPD%CONF%COMPR .NE. SPD__NO ) THEN
!
! -------- Run compressionof the output file
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) == 0 ) STR = '1'
           COM = TRIM(SPD%CONF%COMPR)//" -f -n "//TRIM(STR)//' '//FILOUT
           IS = SYSTEM ( TRIM(COM)//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 5528, IUER, 'SPD_3D_WRITE', 'Error in an '// &
     &              'attempt to compress ouput file '//TRIM(FILOUT)// &
     &              ' using command '//TRIM(COM) )
                RETURN 
           END IF
           IF ( IVRB > 0 ) THEN
                IF ( INDEX( SPD%CONF%COMPR, 'bzip2' ) > 0 ) THEN
                     WRITE ( 6, '(A)' ) 'SPD_3D: written file '//TRIM(FILOUT)//'.bz2'
                   ELSE IF ( INDEX ( SPD%CONF%COMPR, 'xz' ) > 0 ) THEN
                     WRITE ( 6, '(A)' ) 'SPD_3D: written file '//TRIM(FILOUT)//'.xz'
                   ELSE
                     WRITE ( 6, '(A)' ) 'SPD_3D: written file '//TRIM(FILOUT)//'.gz'
                END IF
           END IF
         ELSE
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, '(A)' ) 'SPD_3D: written file '//TRIM(FILOUT)
           END IF
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE SPD_3D_WRITE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_ASCII_WRITE ( SPD, SAT, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_ASCII_WRITE 
! *                                                                      *
! * ### 03-DEC-2008 SPD_3D_ASCII_WRITE v3.1 (c) L. Petrov 27-NOV-2015 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      CHARACTER  FILOUT*(*)
      TYPE     ( SPD__ASCII__TYPE ) :: SAT
      TYPE     ( SPD_3D__TYPE     ) :: SPD
      CHARACTER  STR*256
      INTEGER*4  IUER
      CHARACTER  BEG_FMT_LINE*80, END_FMT_LINE*80, BEG_DESC_LINE*80, END_DESC_LINE*80
      PARAMETER  ( BEG_FMT_LINE  = '#============================ Beginning of format description: =================' )
      PARAMETER  ( END_FMT_LINE  = '#============================ End of format description: =======================' )
      PARAMETER  ( BEG_DESC_LINE = '#============================ Beginning of data description: ===================' )
      PARAMETER  ( END_DESC_LINE = '#============================ End of data description: =========================' )
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      CHARACTER  SYSNAME*128, NODENAME*128, HARDWARE*128, DATE_STR*32
      LOGICAL*4  LEX
      INTEGER*4  LUN, LUN_FMT, LUN_DESC, IOS, J1, J2, J3, J4, J5, J6, J7, &
     &           J8, J9, J10, J11, J12, J13, J14, J15, J16, IER
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, GET_CDATE*19, GET_HR_CDATE*29
!
      CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
      IF ( STR(1:2) == '1 ' ) THEN
           DATE_STR = GET_HR_CDATE()
         ELSE
           CALL CLRCH ( DATE_STR )
      END IF
!
! --- Open the output file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5530, IUER, 'SPD_3D_ASCII_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in attempt to open the output file '// &
     &          FILOUT )
           RETURN
      END IF
!
! ----Write the label
!     
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) SPD__ASCII__LABEL 
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5531, IUER, 'SPD_3D_ASCII_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write in '// &
     &         'the output file '//FILOUT )
           RETURN
      END IF
!
! ----Write down the string with program name and program version
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '#'
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# Created by '// &
     &        SPD_3D_PROG__LABEL
!
! ----Get information about user name and system name
!
!#ifndef SPC
!      CALL GETINFO_USER   ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
!      CALL GETINFO_SYSTEM ( SYSNAME,   NODENAME,      HARDWARE       )
!#else
      USER_E_ADDRESS = " "
      USER_REALNAME  = " "
      USER_E_ADDRESS = " "
      NODENAME = "local computer"
!#endif
!
! ----Write information about user and system
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '#         run by '// &
     &        USER_REALNAME(1:I_LEN(USER_REALNAME))//' ( '// &
     &        USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))//' )'
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '#             on '// &
     &        NODENAME(1:I_LEN(NODENAME))//' at '//DATE_STR(1:19)//' local time'
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '#'
!
! ----Well, we have to put file with format description in the output file
!
      INQUIRE ( FILE=SPD%CONF%FIL_FMT, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5532, IUER, 'SPD_3D_ASCII_WRITE', 'File with '// &
     &         'format description '// &
     &          SPD%CONF%FIL_FMT(1:I_LEN(SPD%CONF%FIL_FMT))//' has '// &
     &         'not been found' )
           RETURN
      END IF
!
      LUN_FMT = GET_UNIT ()
      OPEN ( UNIT=LUN_FMT, FILE=SPD%CONF%FIL_FMT, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5533, IUER, 'SPD_3D_ASCII_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open file with '// &
     &         'format description '//SPD%CONF%FIL_FMT )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) BEG_FMT_LINE
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      DO 410 J1=1,1024*1024
!
! ------ Read line
!
         READ ( UNIT=LUN_FMT, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .EQ. -1 ) GOTO 810
         IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) ' J1=',J1
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              CALL ERR_LOG ( 5534, IUER, 'SPD_3D_ASCII_WRITE', 'Error '// &
     &             STR(1:I_LEN(STR))//' in an attempt to read file '// &
     &             'with format description '//SPD%CONF%FIL_FMT )
              RETURN
         END IF
!
! ------ Write line
!
         WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# '//STR(1:I_LEN(STR))
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              CALL ERR_LOG ( 5535, IUER, 'SPD_3D_ASCII_WRITE', 'Error '// &
     &             STR(1:I_LEN(STR))//' in writing in the output '// &
     &            'file '//SPD%CONF%FIL_FMT )
              RETURN
         END IF
 410  CONTINUE
 810  CONTINUE
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) END_FMT_LINE
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      CLOSE ( UNIT=LUN_FMT )
!
      LUN_DESC = GET_UNIT ()
      OPEN ( UNIT=LUN_DESC, FILE=SPD%CONF%FIL_DESC, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 5536, IUER, 'SPD_3D_ASCII_WRITE', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open file with '// &
     &         'format description '//SPD%CONF%FIL_DESC )
           RETURN
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) BEG_DESC_LINE
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      DO 420 J2=1,1024*1024
!
! ------ Read line
!
         READ ( UNIT=LUN_DESC, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .EQ. -1 ) GOTO 820
         IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) ' J2=',J2
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              CALL ERR_LOG ( 5537, IUER, 'SPD_3D_ASCII_WRITE', 'Error '// &
     &             STR(1:I_LEN(STR))//' in an attempt to read file '// &
     &             'with format description '//SPD%CONF%FIL_FMT )
              RETURN
         END IF
!
! ------ Write line
!
         WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) '# '//STR(1:I_LEN(STR))
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              CALL ERR_LOG ( 5538, IUER, 'SPD_3D_ASCII_WRITE', 'Error '// &
     &             STR(1:I_LEN(STR))//' in writing in the output '// &
     &            'file '//SPD%CONF%FIL_FMT )
              RETURN
         END IF
 420  CONTINUE
 820  CONTINUE
      CLOSE ( UNIT=LUN_DESC )
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) END_DESC_LINE
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
      CALL LIB$MOVC3 ( SIZEOF(SAT%NLINE), SAT%NLINE, %REF(STR) )
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%NLINE))
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
      DO 430 J3=1,SAT%NM
         CALL LIB$MOVC3 ( SIZEOF(SAT%MLINE(J3)), SAT%MLINE(J3), %REF(STR) )
         WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%MLINE(J3)))
 430  CONTINUE 
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!     
      DO 440 J4=1,SAT%NI
         CALL LIB$MOVC3 ( SIZEOF(SAT%ILINE(J4)), SAT%ILINE(J4), %REF(STR) )
         WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%ILINE(J4)))
 440  CONTINUE 
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      CALL LIB$MOVC3 ( SIZEOF(SAT%ULINE), SAT%ULINE, %REF(STR) )
      WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%ULINE))
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      CALL LIB$MOVC3 ( SIZEOF(SAT%TLINE), SAT%TLINE, %REF(STR) )
      WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%TLINE))
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!     
      IF ( SAT%NF > 0 ) THEN
           DO 450 J5=1,SAT%NF
              CALL CLRCH ( STR )
              CALL LIB$MOVC3 ( SIZEOF(SAT%FLINE(J5)), SAT%FLINE(J5), %REF(STR) )
              WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%FLINE(J5)))
 450       CONTINUE 
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      END IF
     WRITE ( UNIT=LUN, FMT='(A)' ) '#     Ind  Name        X-coord     Y-coord      Z-coord       Phi_gdt Long      H_ell  H_geoi'
      DO 460 J6=1,SAT%NS
         CALL LIB$MOVC3 ( SIZEOF(SAT%SLINE(J6)), SAT%SLINE(J6), %REF(STR) )
         WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%SLINE(J6)))
 460  CONTINUE 
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!     
      DO 470 J7=1,SAT%NE
         CALL LIB$MOVC3 ( SIZEOF(SAT%ELINE(J7)), SAT%ELINE(J7), %REF(STR) )
         WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%ELINE(J7)))
 470  CONTINUE 
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!     
      DO 480 J8=1,SAT%NA
         CALL LIB$MOVC3 ( SIZEOF(SAT%ALINE(J8)), SAT%ALINE(J8), %REF(STR) )
         WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%ALINE(J8)))
 480  CONTINUE 
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      DO 490 J9=1,SAT%NS
         CALL LIB$MOVC3 ( SIZEOF(SAT%PLINE(J9)), SAT%PLINE(J9), %REF(STR) )
         WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%PLINE(J9)))
 490  CONTINUE 
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      DO 4100 J10=1,SAT%NS
         DO 4110 J11=1,SAT%NA
            DO 4120 J12=1,SAT%NE
               CALL LIB$MOVC3 ( SIZEOF(SAT%DLINE(J12,J11,J10)), SAT%DLINE(J12,J11,J10), %REF(STR) )
               WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%DLINE(J12,J11,J10)))
 4120       CONTINUE 
 4110     CONTINUE 
 4100 CONTINUE 
!
      IF ( SAT%NF > 0 ) THEN
           DO 4130 J13=1,SAT%NS
              DO 4140 J14=1,SAT%NA
                 IF ( SPD%CONF%SOB_ALG == SOB__ALG_RTE_BENT_1AZ .AND. J14 .NE. 1 ) GOTO 4140
                 IF ( SPD%CONF%SOB_ALG == SOB__ALG_RTE_STRA_1AZ .AND. J14 .NE. 1 ) GOTO 4140
                 DO 4150 J15=1,SAT%NE
                    DO 4160 J16=1,SAT%NF
                       CALL LIB$MOVC3 ( SIZEOF(SAT%OLINE(J16,J15,J14,J13)), &
     &                                  SAT%OLINE(J16,J15,J14,J13), %REF(STR) )
                       WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:SIZEOF(SAT%OLINE(J16,J15,J14,J13)))
 4160               CONTINUE 
 4150            CONTINUE 
 4140         CONTINUE 
 4130       CONTINUE 
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) SPD__ASCII__LABEL 
      CLOSE ( UNIT=LUN ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   SPD_3D_ASCII_WRITE  !#!#
