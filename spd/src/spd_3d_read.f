      SUBROUTINE SPD_3D_READ ( FILIN, INP_FMT, SPD, M_MOD, L_MOD, &
     &                         MOD_TEXT, M_INP, L_INP, INP_TEXT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_READ reads the file in INP_FMT that contains the   *
! *   3D Slant Path Delays
! *                                                                      *
! * ###  06-JAN-2009   SPD_3D_READ  v4.2  (c) L. Petrov 11-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( SPD_3D__TYPE   ) :: SPD
      INTEGER*4  M_MOD, L_MOD, M_INP, L_INP, IUER
      CHARACTER  FILIN*(*), INP_FMT*(*), MOD_TEXT(M_MOD)*(*), &
     &           INP_TEXT(M_INP)*(*)
      CHARACTER  STR*128, STR1*128
      TYPE     ( SPD__ASCII__TYPE ) :: SAT
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           NRFR, IER
      REAL*8     DEL_MIN, DEL_MAX
      PARAMETER  ( DEL_MIN = 0.0D0  )
      PARAMETER  ( DEL_MAX = 1.D-6  )
      REAL*8,    EXTERNAL :: DEL_ISA 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL SPD_FREE ( SPD, 1 ) 
      CALL NOUT ( SIZEOF(SPD), SPD )
      CALL NOUT ( SIZEOF(SAT), SAT )
!
      IF ( INP_FMT == 'SPD_3D_ASCII' ) THEN
           CALL ERR_PASS ( IUER, IER )
	   SAT%PROC_O_MODE = SPD__YES
           CALL SPD_3D_READ_ASCII ( FILIN, SAT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5611, IUER, 'SPD_3D_READ', 'Failure in '// &
     &              'parsing the input file '//FILIN(1:I_LEN(FILIN))// &
     &              ' in the '//INP_FMT(1:I_LEN(INP_FMT))//' format' )
                RETURN 
           END IF 
           SPD%FILSPD = SAT%FILE
        ELSE IF ( INP_FMT == 'SPD_3D_ASCII_NO_O' ) THEN
           CALL ERR_PASS ( IUER, IER )
	   SAT%PROC_O_MODE = SPD__NO
           CALL SPD_3D_READ_ASCII ( FILIN, SAT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 5612, IUER, 'SPD_3D_READ', 'Failure in '// &
     &              'parsing the input file '//FILIN(1:I_LEN(FILIN))// &
     &              ' in the '//INP_FMT(1:I_LEN(INP_FMT))//' format' )
                RETURN 
           END IF 
           SPD%FILSPD = SAT%FILE
         ELSE 
           CALL ERR_LOG ( 5613, IUER, 'SPD_3D_READ', 'Unspoorted file '// &
     &         'format: '//INP_FMT(1:I_LEN(INP_FMT))//' -- supported '// &
     &         'formats: SPD_3D_ASCII' )
           RETURN 
      END IF 
!
      IF ( SAT%NM > M_MOD ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( M_MOD,  STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( SAT%NM, STR1 )
           CALL ERR_LOG ( 5614, IUER, 'SPD_3D_READ', 'Input parameter '// &
     &         'M_MOD '//STR(1:I_LEN(STR))//' is too small -- the mode '// &
     &         'description in the input 3D Slant Path Delay file '// &
     &          FILIN(1:I_LEN(FILIN))//' is greater: '//STR1 )
           CALL SAT_QUIT ( SAT )
           RETURN 
      END IF
!
      IF ( SAT%NI > M_MOD ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( M_INP,  STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( SAT%NI, STR1 )
           CALL ERR_LOG ( 5615, IUER, 'SPD_3D_READ', 'Input parameter '// &
     &         'M_MOD '//STR(1:I_LEN(STR))//' is too small -- the mode '// &
     &         'description in the input 3D Slant Path Delay file '// &
     &          FILIN(1:I_LEN(FILIN))//' is greater: '//STR1 )
           CALL SAT_QUIT ( SAT )
           RETURN 
      END IF
!
! --- Determine the number and the type of delays in the file
!
      SAT%ULINE%DEL1_CODE = SPD__TOT_STR 
      SAT%ULINE%DEL2_CODE = SPD__WAT_STR 

      IF ( SAT%ULINE%DEL1_CODE == SPD__TOT_STR .AND. &
     &     SAT%ULINE%DEL2_CODE == SPD__WAT_STR       ) THEN
           NRFR = 2
         ELSE 
           NRFR = -1
      END IF 
!
! --- Get the model description
!
      L_MOD = SAT%NM 
      DO 410 J1=1,SAT%NM 
         MOD_TEXT(J1) = SAT%MLINE(J1)%TEXT   
 410  CONTINUE 
!
! --- Get the information about the data set
!
      L_INP = SAT%NI
      DO 420 J2=1,SAT%NI
         INP_TEXT(J2) = SAT%ILINE(J2)%TEXT   
 420  CONTINUE 
!
      SPD%NSTA = SAT%NS
      SPD%CONF%N_EL = SAT%NE
      SPD%CONF%N_AZ = SAT%NA
      ALLOCATE ( SPD%STA(SPD%NSTA), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL IINCH ( SPD%NSTA*SIZEOF(SPD%STA(1)), STR )
           CALL ERR_LOG ( 5616, IUER, 'SPD_3D_READ', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for SPD%STA object' )
           CALL SAT_QUIT ( SAT )
           RETURN 
      END IF 
!
      DO 430 J3=1,SAT%NS
	 SPD%STA(J3)%NAME = SAT%SLINE(J3)%STA_NAME 
	 READ ( UNIT=SAT%SLINE(J3)%X_COOR,    FMT='(F12.3)' ) SPD%STA(J3)%COO_CFS(1)
	 READ ( UNIT=SAT%SLINE(J3)%Y_COOR,    FMT='(F12.3)' ) SPD%STA(J3)%COO_CFS(2)
	 READ ( UNIT=SAT%SLINE(J3)%Z_COOR,    FMT='(F12.3)' ) SPD%STA(J3)%COO_CFS(3)
	 READ ( UNIT=SAT%SLINE(J3)%LAT_GCN,   FMT='(F8.4)'  ) SPD%STA(J3)%LAT_GCN
	 READ ( UNIT=SAT%SLINE(J3)%LON,       FMT='(F8.4)'  ) SPD%STA(J3)%LON
	 READ ( UNIT=SAT%SLINE(J3)%HEI_ELL,   FMT='(F6.1)'  ) SPD%STA(J3)%HEI_ELL
	 READ ( UNIT=SAT%SLINE(J3)%HEI_GEOID, FMT='(F6.1)'  ) SPD%STA(J3)%HEI_GEOID
         SPD%STA(J3)%LAT_GCN = SPD%STA(J3)%LAT_GCN*DEG__TO__RAD
         SPD%STA(J3)%LON     = SPD%STA(J3)%LON*DEG__TO__RAD
 430  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( SAT%TLINE%DATE_STR, SPD%MJD, SPD%TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL ERR_LOG ( 5617, IUER, 'SPD_3D_READ', 'Failure to '// &
     &         'decode the date string '//SAT%TLINE%DATE_STR// &
     &         ' while parsing the input 3D Slant Path Delay file '// &
     &         FILIN )
           CALL SAT_QUIT ( SAT )
           CALL SPD_FREE ( SPD, 1 ) 
           RETURN 
      END IF
!
      READ ( UNIT=SAT%TLINE%UTC_M_TAI_STR, FMT='(F5.1)', IOSTAT=IER ) SPD%UTC_M_TAI
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL ERR_LOG ( 5618, IUER, 'SPD_3D_READ', 'Failure to '// &
     &         'decode the UTC_M__TAI string '//SAT%TLINE%DATE_STR// &
     &         ' while parsing the input 3D Slant Path Delay file '// &
     &         FILIN )
           CALL SAT_QUIT ( SAT )
           CALL SPD_FREE ( SPD, 1 ) 
           RETURN 
      END IF
      SPD%UTC = SPD%TAI - SPD%UTC_M_TAI
      SPD%UTC_M_TAI_STATUS = SPD__COMP
!
      DO 470 J7=1,SAT%NS
         ALLOCATE ( SPD%STA(J7)%DEL(SAT%NE,SAT%NA,SPD__MTYP), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( 4*SAT%NE,SAT%NA,SPD__MTYP, STR )
              CALL ERR_LOG ( 5619, IUER, 'SPD_3D_READ', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes for '// &
     &            'variable SPD%STA(J7)%DEL' )
              CALL SAT_QUIT ( SAT )
              RETURN 
         END IF
         IF ( INP_FMT .NE. 'SPD_3D_ASCII_NO_O' .AND. SAT%NF > 0 ) THEN
              ALLOCATE ( SPD%STA(J7)%OPA(SAT%NF,SAT%NE,SAT%NA), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*SAT%NF*SAT%NE,SAT%NA, STR )
                   CALL ERR_LOG ( 5620, IUER, 'SPD_3D_READ', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes for '// &
     &                  'variable SPD%STA(J7)%OPA' )
                   CALL SAT_QUIT ( SAT )
                   RETURN 
              END IF
!
              ALLOCATE ( SPD%STA(J7)%TAT(SAT%NF,SAT%NE,SAT%NA), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*SAT%NF*SAT%NE,SAT%NA, STR )
                   CALL ERR_LOG ( 5621, IUER, 'SPD_3D_READ', 'Failure to '// &
     &                  'allocate '//STR(1:I_LEN(STR))//' bytes for '// &
     &                  'variable SPD%STA(J7)%TAT' )
                   CALL SAT_QUIT ( SAT )
                   RETURN 
              END IF
         END IF
         READ ( UNIT=SAT%PLINE(J7)%PRES, FMT='(F8.1)' ) SPD%STA(J7)%SUR_PRS
         READ ( UNIT=SAT%PLINE(J7)%WATER_VAPOR_PRES, FMT='(F8.1)' ) SPD%STA(J7)%SUR_PWP
         READ ( UNIT=SAT%PLINE(J7)%TEMP, FMT='(F5.1)' ) SPD%STA(J7)%SUR_TEM
         DO 480 J8=1,SAT%NA
            DO 490 J9=1,SAT%NE
               READ ( UNIT=SAT%DLINE(J9,J8,J7)%DEL1, FMT='(1PD12.6)', IOSTAT=IER ) &
     &                SPD%STA(J7)%DEL(J9,J8,SPD__TOT)
               IF ( IER .NE. 0 ) THEN
                    WRITE ( 6, * ) ' '
                    WRITE ( 6, * ) 'J7=',J7, ' J8=',J8, ' J9= ',J9
                    CALL CLRCH ( STR )
                    STR = SAT%DLINE(J9,J8,J7)%DEL1
                    CALL ERR_LOG ( 5622, IUER, 'SPD_3D_READ', 'Error in decoding '// &
     &                  'the delay at line '//STR(1:I_LEN(STR))// &
     &                  ' of file '//FILIN )
                    RETURN 
               END IF
               READ ( UNIT=SAT%DLINE(J9,J8,J7)%DEL2, FMT='(1PD12.6)', IOSTAT=IER ) &
     &                SPD%STA(J7)%DEL(J9,J8,SPD__WAT)
               IF ( IER .NE. 0 ) THEN
                    WRITE ( 6, * ) ' '
                    WRITE ( 6, * ) 'J7=',J7, ' J8=',J8, ' J9= ',J9
                    CALL CLRCH ( STR )
                    STR = SAT%DLINE(J9,J8,J7)%DEL2
                    CALL ERR_LOG ( 5623, IUER, 'SPD_3D_READ', 'Error in decoding '// &
     &                  'the delay at line '//STR(1:I_LEN(STR))// &
     &                  ' of file '//FILIN )
                    RETURN 
               END IF
!
               IF ( INP_FMT .NE. 'SPD_3D_ASCII_NO_O' .AND. SAT%NF > 0 ) THEN
                    DO 4100 J10=1,SAT%NF
                       READ ( UNIT=SAT%OLINE(J10,J9,J8,J7)%OPA, FMT='(F6.1)', IOSTAT=IER ) &
     &                        SPD%STA(J7)%OPA(J10,J9,J8)
                       READ ( UNIT=SAT%OLINE(J10,J9,J8,J7)%TAT, FMT='(F6.1)', IOSTAT=IER ) &
     &                        SPD%STA(J7)%TAT(J10,J9,J8)
 4100               CONTINUE 
               END IF 
 490        CONTINUE 
 480     CONTINUE 
 470  CONTINUE 
!
      SPD%ELV%N_EL = SAT%NE
      IF ( ASSOCIATED(SPD%ELV%ELEV) ) THEN
           DEALLOCATE ( SPD%ELV%ELEV )
      END IF
      ALLOCATE ( SPD%ELV%ELEV(SPD%ELV%N_EL), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 4*SPD%ELV%N_EL, STR )
           CALL ERR_LOG ( 5624, IUER, 'SPD_3D_READ', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for SPD%ELV' )
           CALL SAT_QUIT ( SAT )
           CALL SPD_FREE ( SPD, 1 ) 
           RETURN 
      END IF
      IF ( ASSOCIATED(SPD%ELV%MAP) ) THEN
           DEALLOCATE ( SPD%ELV%MAP )
      END IF
      ALLOCATE ( SPD%ELV%MAP(SPD%ELV%N_EL), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 4*SPD%ELV%N_EL, STR )
           CALL ERR_LOG ( 5625, IUER, 'SPD_3D_READ', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for SPD%ELV' )
           CALL SAT_QUIT ( SAT )
           CALL SPD_FREE ( SPD, 1 ) 
           RETURN 
      END IF
!
      DO 4110 J11=1,SAT%NE
         READ ( UNIT=SAT%ELINE(J11)%ANG, FMT='(F10.6)' ) SPD%ELV%ELEV(J11)
         SPD%ELV%ELEV(J11) = SPD%ELV%ELEV(J11)*DEG__TO__RAD
         SPD%ELV%MAP(J11)  = DEL_ISA ( DBLE(SPD%ELV%ELEV(J11)) )/ &
     &                            DEL_ISA ( P2I )
 4110 CONTINUE 
!
      SPD%AZM%N_AZ = SAT%NA
      IF ( ASSOCIATED(SPD%AZM%AZIM) ) THEN
           DEALLOCATE ( SPD%AZM%AZIM )
      END IF
      ALLOCATE ( SPD%AZM%AZIM(SPD%AZM%N_AZ), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 4*SPD%AZM%N_AZ, STR )
           CALL ERR_LOG ( 5626, IUER, 'SPD_3D_READ', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for SPD%AZM' )
           CALL SAT_QUIT ( SAT )
           CALL SPD_FREE ( SPD, 1 ) 
           RETURN 
      END IF
!
      DO 4120 J12=1,SAT%NA
         READ ( UNIT=SAT%ALINE(J12)%ANG, FMT='(F10.1)' ) SPD%AZM%AZIM(J12)
         SPD%AZM%AZIM(J12) = SPD%AZM%AZIM(J12)*DEG__TO__RAD
 4120 CONTINUE 
!
      SPD%NFRQ = SAT%NF 
      IF ( INP_FMT .NE. 'SPD_3D_ASCII_NO_O' .AND. SAT%NF > 0 ) THEN
           IF ( ASSOCIATED ( SPD%FRQ ) ) THEN
                DEALLOCATE ( SPD%FRQ )
           END IF
           ALLOCATE ( SPD%FRQ(SPD%NFRQ), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR ) 
                CALL IINCH ( 8*SPD%NFRQ, STR )
                CALL ERR_LOG ( 5627, IUER, 'SPD_3D_READ', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for SPD%FRQ' )
                CALL SAT_QUIT ( SAT )
                CALL SPD_FREE ( SPD, 1 ) 
                RETURN 
           END IF
           DO 4130 J13=1,SAT%NF
              READ ( UNIT=SAT%FLINE(J13)%FRQ, FMT='(F15.5)' ) SPD%FRQ(J13)
 4130      CONTINUE 
      END IF
      CALL SAT_QUIT ( SAT )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE SPD_3D_READ  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_READ_ASCII ( FILIN, SAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_READ_ASCII 
! *                                                                      *
! * ### 06-JAN-2009 SPD_3D_READ_ASCII v2.4 (c) L. Petrov 11-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'astro_constants.i'
      INTEGER*4  IUER
      CHARACTER  FILIN*(*)
      TYPE     ( SPD__ASCII__TYPE     ) :: SAT
      TYPE     ( SPD__ASCII_ANG_LINE  ) :: ALINE_TMP
      TYPE     ( SPD__ASCII_DEL_LINE  ) :: DLINE_TMP
      TYPE     ( SPD__ASCII_FRQ_LINE  ) :: FLINE_TMP
      TYPE     ( SPD__ASCII_TEXT_LINE ) :: ILINE_TMP
      TYPE     ( SPD__ASCII_OPA_LINE  ) :: OLINE_TMP
      TYPE     ( SPD__ASCII_PLINE     ) :: PLINE_TMP
      TYPE     ( SPD__ASCII_PLINE_V2  ) :: PLINE_TMP_V2
      TYPE     ( SPD__ASCII_SLINE     ) :: SLINE_TMP
      CHARACTER  FILOUT*128, STR*128, FMT*3, COM*512, DIR_TMP*128, &
     &           FIL_USED*128, SAVED_PROC_O_MODE*4
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      INTEGER*4    M_ADD_LINE, MIND
      PARAMETER  ( M_ADD_LINE = 4*1024 )
      PARAMETER  ( MIND       =     64 )
      INTEGER*4  MJD, IOS, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           UNIX_DATE, NT, IS, ID, IL, MBUF, NBUF, IND_LINE, &
     &           IND_FRQ, IND_STA, IND_AZ, IND_EL, LIND, &
     &           IND(2,MIND), IER
      INTEGER*8  SIZE_I8
      REAL*8     AZ_STEP, ARG_STEP, ARG_MIN, ARG_MAX, ARG, AZ, EL, DELS(2)
      REAL*8     UTC, TAI
      LOGICAL*4  LEX, FL_TMP
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, FILE_INFO, LINDEX, SYSTEM
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, GET_CDATE*19, MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: DEL_ISA, INV_MAP_ISA 
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5641, IUER, 'SPD_3D_READ_ASCII', 'Cannot find input '// &
     &         'file with 3D Slant Path Delays: '//FILIN )
           RETURN 
      END IF
!
      IL = ILEN(FILIN)
      IF ( IL < 4 ) IL = 4
      IF ( FILIN(IL-3:IL) == '.bz2' ) THEN
!
! -------- The file is compressed. Let us find a directory where 
! -------- to decompress it
!
           DIR_TMP = '/dev/shm'
           IF ( .NOT. IS_DIR_EXIST ( DIR_TMP, STR ) ) THEN
                DIR_TMP = '/tmp'
           END IF
           ID = LINDEX ( FILIN, '/' ) + 1
           FIL_USED = TRIM(DIR_TMP)//'/'//FILIN(ID:)
!
! -------- Determine how many threads to use
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) == 0 ) STR = '1'
!
! -------- Run decompression
!
           COM = 'lbzip2 -n '//TRIM(STR)//' -fdc '//TRIM(FILIN)//' > '// &
     &            FIL_USED
           IS = SYSTEM ( TRIM(COM)//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 5642, IUER, 'SPD_3D_READ_ASCII', 'Error in an '// &
     &              'attempt to decompress input file '//TRIM(FILIN)// &
     &              ' using command '//TRIM(COM) )
                RETURN 
           END IF
           FL_TMP   = .TRUE.
         ELSE 
           FIL_USED = FILIN
           FL_TMP   = .FALSE.
      END IF
!
! --- Check the size of the input file
!
      IS = FILE_INFO ( FIL_USED(1:I_LEN(FIL_USED))//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR ) 
           CALL ERR_LOG ( 5643, IUER, 'SPD_3D_READ_ASCII', 'Failure in an '// &
     &         'attempt to access to the input file with 3D Slant '// &
     &         'Path Delays: '//FIL_USED(1:I_LEN(FIL_USED))//'  '//STR )
           RETURN 
      END IF
!
! --- Allocate memory for the buffer where to put contents 
! --- of the input file
!
      MBUF = SIZE_I8/INT8(SIZEOF(OLINE_TMP)-1) + M_ADD_LINE
      ALLOCATE ( BUF(MBUF), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MBUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 5644, IUER, 'SPD_3D_READ_ASCII', 'Failure in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory' )
           RETURN 
      END IF
!
! --- Read the input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_USED, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5645, IUER, 'SPD_3D_READ_ASCII', 'Failure in '// &
     &         'reading the input file with 3D Slant Path Delays '//FILIN )
           RETURN 
      END IF
      IF ( FL_TMP ) THEN
!
! -------- Removed temporary decompressed input file
!
           CALL UNLINK ( TRIM(FIL_USED)//CHAR(0) )
      END IF
!
      IF ( BUF(1)(1:LEN(SPD__ASCII__LABEL)) == SPD__ASCII__LABEL ) THEN
           FMT = 'CUR' 
         ELSE IF ( BUF(1)(1:LEN(SPD__ASCII__LABEL)) == SPD__ASCII__LABEL_1ST ) THEN
           FMT = '1ST'
         ELSE IF ( BUF(1)(1:LEN(SPD__ASCII__LABEL)) == SPD__ASCII__LABEL_2ND ) THEN
           FMT = '2ND'
         ELSE 
           CALL CLRCH  ( STR )
           CALL TRAN   ( 13, BUF(1), STR )
           CALL ERR_LOG ( 5646, IUER, 'SPD_3D_READ_ASCII', 'Wrong format '// &
     &         'of the input file with 3D Slant Path Delays '// &
     &          FILIN(1:I_LEN(FILIN))//' the first line is '// &
     &          STR(1:I_LEN(STR))//' while the format label '// &
     &          SPD__ASCII__LABEL//' was expected' )
           GOTO 811
      END IF
!
      IF ( BUF(NBUF)(1:LEN(SPD__ASCII__LABEL)) == BUF(1)(1:LEN(SPD__ASCII__LABEL)) ) THEN
           CONTINUE 
         ELSE 
           CALL CLRCH  ( STR )
           CALL TRAN   ( 13, BUF(1), STR )
           CALL ERR_LOG ( 5647, IUER, 'SPD_3D_READ_ASCII', 'The input file '// &
     &         'with 3D Slant Path Delays '//FILIN(1:I_LEN(FILIN))// &
     &         ' was read not to the end. The last line is not the '// &
     &         'same line as the first one' )
           GOTO 811
      END IF
!
      SAVED_PROC_O_MODE = SAT%PROC_O_MODE
      CALL NOUT ( SIZEOF(SAT), SAT )
      SAT%FILE = FILIN
      SAT%O_MODE = SPD__UNDF
      SAT%PROC_O_MODE = SAVED_PROC_O_MODE 
!
      DO 410 J1=2,NBUF-1
         IF ( BUF(J1)(1:1) == '#' ) THEN
              IF ( BUF(J1)(1:12) == '# Created by' ) THEN
                   SAT%LABEL = BUF(J1)(14:) 
              END IF
              GOTO 410
         END IF
         IF ( BUF(J1)(1:1) == ' ' ) GOTO 410
         IF ( BUF(J1)(1:1) == 'N' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(SAT%NLINE), %REF(BUF(J1)), SAT%NLINE )
              READ ( UNIT=SAT%NLINE%N_MOD, FMT='(I4)' ) SAT%NM 
              READ ( UNIT=SAT%NLINE%N_INP, FMT='(I4)' ) SAT%NI
              READ ( UNIT=SAT%NLINE%N_STA, FMT='(I7)' ) SAT%NS
              READ ( UNIT=SAT%NLINE%N_EL,  FMT='(I4)' ) SAT%NE
              READ ( UNIT=SAT%NLINE%N_AZ,  FMT='(I4)' ) SAT%NA
              IF ( FMT == 'CUR' ) THEN
                   READ ( UNIT=SAT%NLINE%N_FRQ,  FMT='(I4)' ) SAT%NF
                 ELSE 
                   SAT%NF = 0
              END IF
!
              ALLOCATE ( SAT%MLINE(SAT%NM), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5648, IUER, 'SPD_3D_READ_ASCII', 'Failure '// &
     &                 'to allocate memory for SAT%MLINE' )
                   RETURN 
              END IF
!        
              ALLOCATE ( SAT%ILINE(SAT%NI), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5649, IUER, 'SPD_3D_READ_ASCII', 'Failure '// &
     &                 'to allocate memory for SAT%MLINE' )
                   GOTO 811
              END IF
!        
              ALLOCATE ( SAT%SLINE(SAT%NS), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5650, IUER, 'SPD_3D_READ_ASCII', 'Failure '// &
     &                 'to allocate memory for SAT%SLINE' )
                   GOTO 811
              END IF
!        
              ALLOCATE ( SAT%ELINE(SAT%NE), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5651, IUER, 'SPD_3D_READ_ASCII', 'Failure '// &
     &                 'to allocate memory for SAT%ELINE' )
                   GOTO 811
              END IF
!        
              ALLOCATE ( SAT%ALINE(SAT%NA), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5652, IUER, 'SPD_3D_READ_ASCII', 'Failure '// &
     &                 'to allocate memory for SAT%ALINE' )
                   GOTO 811
              END IF
!        
              ALLOCATE ( SAT%PLINE(SAT%NS), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5653, IUER, 'SPD_3D_READ_ASCII', 'Failure '// &
     &                 'to allocate memory for SAT%PLINE' )
                   GOTO 811
              END IF
!        
              ALLOCATE ( SAT%DLINE(SAT%NE,SAT%NA,SAT%NS), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5654, IUER, 'SPD_3D_READ_ASCII', 'Failure '// &
     &                 'to allocate memory for SAT%DLINE' )
                   GOTO 811
              END IF
              IF ( SAT%NF > 0 ) THEN
                   ALLOCATE ( SAT%FLINE(SAT%NF), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5655, IUER, 'SPD_3D_READ_ASCII', 'Failure '// &
     &                      'to allocate memory for SAT%FLINE' )
                        GOTO 811
                   END IF
!
                   ALLOCATE ( SAT%OLINE(SAT%NF,SAT%NE,SAT%NA,SAT%NS), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5656, IUER, 'SPD_3D_READ_ASCII', 'Failure '// &
     &                      'to allocate memory for SAT%OLINE' )
                        GOTO 811
                   END IF
              END IF
            ELSE IF ( BUF(J1)(1:1) == 'M' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(ILINE_TMP), %REF(BUF(J1)), ILINE_TMP )
              READ ( UNIT=ILINE_TMP%IND_LINE, FMT='(I4)', IOSTAT=IER ) IND_LINE
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5657, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding index in line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file with 3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
              SAT%MLINE(IND_LINE) = ILINE_TMP
              IF ( SAT%PROC_O_MODE .NE. 'NO' ) THEN
                   IF ( INDEX ( SAT%MLINE(IND_LINE)%TEXT, 'Model for atmospheric' ) > 0 ) THEN
                        CALL EXWORD ( SAT%MLINE(IND_LINE)%TEXT, MIND, LIND, IND, CHAR(32), IER )
                        IF ( SAT%MLINE(IND_LINE)%TEXT(IND(1,LIND):IND(2,LIND)) == 'rte_bent_1az' ) THEN
                             SAT%NFA = 1
                             SAT%O_MODE = SPD__1AZ
                           ELSE IF ( SAT%MLINE(IND_LINE)%TEXT(IND(1,LIND):IND(2,LIND)) == 'rte_bent' ) THEN
                             SAT%NFA = SAT%NA 
                             SAT%O_MODE = SPD__NAZ
                        END IF
                   END IF
              END IF
            ELSE IF ( BUF(J1)(1:1) == 'I' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(ILINE_TMP), %REF(BUF(J1)), ILINE_TMP )
              READ ( UNIT=ILINE_TMP%IND_LINE, FMT='(I4)', IOSTAT=IER ) IND_LINE
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5658, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding index in line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file with 3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
              SAT%ILINE(IND_LINE) = ILINE_TMP
            ELSE IF ( BUF(J1)(1:1) == 'U' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(SAT%ULINE), %REF(BUF(J1)), SAT%ULINE )
            ELSE IF ( BUF(J1)(1:1) == 'T' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(SAT%TLINE), %REF(BUF(J1)), SAT%TLINE )
            ELSE IF ( BUF(J1)(1:1) == 'S' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(SLINE_TMP), %REF(BUF(J1)), SLINE_TMP )
              READ ( UNIT=SLINE_TMP%STA_IND_STR, FMT='(I7)', IOSTAT=IER ) IND_STA
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5659, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding index in line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file with 3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
              SAT%SLINE(IND_STA) = SLINE_TMP
            ELSE IF ( BUF(J1)(1:1) == 'E' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(ALINE_TMP), %REF(BUF(J1)), ALINE_TMP )
              READ ( UNIT=ALINE_TMP%ANG_IND, FMT='(I4)', IOSTAT=IER ) IND_EL
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5660, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding elevation index in line '// &
     &                  STR(1:I_LEN(STR))//' of the input file with '// &
     &                 '3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
              SAT%ELINE(IND_EL) = ALINE_TMP
            ELSE IF ( BUF(J1)(1:1) == 'A' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(ALINE_TMP), %REF(BUF(J1)), ALINE_TMP )
              READ ( UNIT=ALINE_TMP%ANG_IND, FMT='(I4)', IOSTAT=IER ) IND_AZ
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5661, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding azimuth index in line '// &
     &                  STR(1:I_LEN(STR))//' of the input file with '// &
     &                 '3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
              SAT%ALINE(IND_AZ) = ALINE_TMP
            ELSE IF ( ( FMT == 'CUR'  .AND.  BUF(J1)(1:1) == 'P' ) .OR.  &
     &                ( FMT == '1ST'  .AND.  BUF(J1)(1:1) == 'P' ) .OR.  &
     &                ( FMT == '2ND'  .AND.  BUF(J1)(1:1) == 'F' )       ) THEN
              IF ( FMT == 'CUR' ) THEN
                   CALL LIB$MOVC3 ( SIZEOF(PLINE_TMP), %REF(BUF(J1)), PLINE_TMP )
                   READ ( UNIT=PLINE_TMP%STA_IND, FMT='(I7)', IOSTAT=IER ) IND_STA
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 5662, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                      'in decoding index in line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file with 3D Slant Path Delays '//FILIN )
                        GOTO 811
                   END IF
                   SAT%PLINE(IND_STA) = PLINE_TMP
                ELSE IF ( FMT == '1ST' .OR.  FMT == '2ND' ) THEN
                   CALL LIB$MOVC3 ( SIZEOF(PLINE_TMP), %REF(BUF(J1)), PLINE_TMP_V2 )
                   READ ( UNIT=PLINE_TMP_V2%STA_IND, FMT='(I7)', IOSTAT=IER ) IND_STA
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 5663, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                      'in decoding index in line '//STR(1:I_LEN(STR))// &
     &                      ' of the input file with 3D Slant Path Delays '//FILIN )
                        GOTO 811
                   END IF
                   CALL CLRCH ( SAT%PLINE(IND_STA) )
                   SAT%PLINE(IND_STA)%CODE    = PLINE_TMP_V2%CODE
                   SAT%PLINE(IND_STA)%STA_IND = PLINE_TMP_V2%STA_IND
                   SAT%PLINE(IND_STA)%PRES    = PLINE_TMP_V2%PRES
                   SAT%PLINE(IND_STA)%TEMP    = PLINE_TMP_V2%TEMP
                   SAT%PLINE(IND_STA)%WATER_VAPOR_PRES = '    0.0'
              END IF
            ELSE IF ( FMT == 'CUR'  .AND.  BUF(J1)(1:1) == 'F' .OR. &
     &                FMT == '1ST'  .AND.  BUF(J1)(1:1) == 'F'      ) THEN
              CALL LIB$MOVC3 ( SIZEOF(FLINE_TMP), %REF(BUF(J1)), FLINE_TMP )
              READ ( UNIT=FLINE_TMP%FRQ_IND, FMT='(I4)', IOSTAT=IER ) IND_FRQ
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5664, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding index in line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file with 3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
              SAT%FLINE(IND_FRQ) = FLINE_TMP
            ELSE IF ( BUF(J1)(1:1) == 'D' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(DLINE_TMP), %REF(BUF(J1)), DLINE_TMP )
              READ ( UNIT=DLINE_TMP%STA_IND, FMT='(I7)', IOSTAT=IER ) IND_STA
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5665, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding station index in line '// &
     &                  STR(1:I_LEN(STR))//' of the input file with '// &
     &                 '3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
!
              READ ( UNIT=DLINE_TMP%EL_IND, FMT='(I4)', IOSTAT=IER ) IND_EL
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5666, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding elevation index in line '// &
     &                  STR(1:I_LEN(STR))//' of the input file with '// &
     &                 '3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
!
              READ ( UNIT=DLINE_TMP%AZ_IND, FMT='(I4)', IOSTAT=IER ) IND_AZ
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5667, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding azimuth index in line '// &
     &                  STR(1:I_LEN(STR))//' of the input file with '// &
     &                  '3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
              SAT%DLINE(IND_EL,IND_AZ,IND_STA) = DLINE_TMP
!
! ----------- NB: We bypass reading O-lines when SAT%PROC_O_MODE is 'NO'
!
            ELSE IF ( BUF(J1)(1:1) == 'O' .AND. SAT%PROC_O_MODE .NE. SPD__NO ) THEN
              SAT%PROC_O_MODE = SPD__YES
              CALL LIB$MOVC3 ( SIZEOF(OLINE_TMP), %REF(BUF(J1)), OLINE_TMP )
              READ ( UNIT=OLINE_TMP%STA_IND, FMT='(I7)', IOSTAT=IER ) IND_STA
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5668, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding station index in line '// &
     &                  STR(1:I_LEN(STR))//' of the input file with '// &
     &                 '3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
!
              READ ( UNIT=OLINE_TMP%EL_IND, FMT='(I4)', IOSTAT=IER ) IND_EL
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5669, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding elevation index in line '// &
     &                  STR(1:I_LEN(STR))//' of the input file with '// &
     &                 '3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
!
              READ ( UNIT=OLINE_TMP%AZ_IND, FMT='(I4)', IOSTAT=IER ) IND_AZ
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5670, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding azimuth index in line '// &
     &                  STR(1:I_LEN(STR))//' of the input file with '// &
     &                  '3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
!
              READ ( UNIT=OLINE_TMP%FRQ_IND, FMT='(I4)', IOSTAT=IER ) IND_FRQ
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5671, IUER, 'SPD_3D_READ_ASCII', 'Error '// &
     &                 'in decoding azimuth index in line '// &
     &                  STR(1:I_LEN(STR))//' of the input file with '// &
     &                  '3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
              SAT%OLINE(IND_FRQ,IND_EL,IND_AZ,IND_STA) = OLINE_TMP
         END IF
 410  CONTINUE 
 810  CONTINUE 
!      
      DEALLOCATE   ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN 
!
 811  CONTINUE 
!
! --- Clean the buffer
!
      DEALLOCATE    ( BUF )
!
      RETURN
      END  SUBROUTINE SPD_3D_READ_ASCII  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_READ_ASCII_ZEN ( FILIN, PAR, SAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_READ_ASCII 
! *                                                                      *
! * ### 06-JAN-2009 SPD_3D_READ_ASCII v2.5 (c) L. Petrov 15-FEB-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'astro_constants.i'
      INTEGER*4  IUER
      CHARACTER  FILIN*(*), PAR*(*)
      TYPE     ( SPD__ASCII__TYPE     ) :: SAT
      TYPE     ( SPD__ASCII_ANG_LINE  ) :: ALINE_TMP
      TYPE     ( SPD__ASCII_DEL_LINE  ) :: DLINE_TMP
      TYPE     ( SPD__ASCII_FRQ_LINE  ) :: FLINE_TMP
      TYPE     ( SPD__ASCII_TEXT_LINE ) :: ILINE_TMP
      TYPE     ( SPD__ASCII_OPA_LINE  ) :: OLINE_TMP
      TYPE     ( SPD__ASCII_PLINE     ) :: PLINE_TMP
      TYPE     ( SPD__ASCII_PLINE_V2  ) :: PLINE_TMP_V2
      TYPE     ( SPD__ASCII_SLINE     ) :: SLINE_TMP
      CHARACTER  FILOUT*128, STR*128, FMT*3, COM*128, DIR_TMP*128, &
     &           FIL_USED*256, SAVED_PROC_O_MODE*4, SUFFIX*8
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      INTEGER*4    M_ADD_LINE, MIND
      PARAMETER  ( M_ADD_LINE = 4*1024 )
      PARAMETER  ( MIND       =     64 )
      INTEGER*4  MJD, IOS, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           UNIX_DATE, NT, IS, ID, IL, MBUF, NBUF, IND_LINE, &
     &           IND_FRQ, IND_STA, IND_AZ, IND_EL, PID, LIND, &
     &           IND(2,MIND), IER
      INTEGER*8  SIZE_I8
      REAL*8     AZ_STEP, ARG_STEP, ARG_MIN, ARG_MAX, ARG, AZ, EL, DELS(2)
      REAL*8     UTC, TAI
      LOGICAL*4  LEX, FL_TMP
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
      INTEGER*4, EXTERNAL :: GETPID, ILEN, I_LEN, FILE_INFO, LINDEX, SYSTEM
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, GET_CDATE*19, MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: DEL_ISA, INV_MAP_ISA 
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5211, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Cannot find input '// &
     &         'file with 3D Slant Path Delays: '//FILIN )
           RETURN 
      END IF
!
      IL = ILEN(FILIN)
      IF ( IL < 4 ) IL = 4
      IF ( FILIN(IL-3:IL) == '.bz2' ) THEN
!
! -------- Get information about PID of this process
!
           PID = GETPID()
           WRITE ( UNIT=SUFFIX(1:8), FMT='(I8)' ) PID
           CALL BLANK_TO_ZERO ( SUFFIX(1:8) )
!
! -------- The file is compressed. Let us find a directory where 
! -------- to decompress it
!
           DIR_TMP = '/dev/shm'
           IF ( .NOT. IS_DIR_EXIST ( DIR_TMP, STR ) ) THEN
                DIR_TMP = '/tmp'
           END IF
           ID = LINDEX ( FILIN, '/' ) + 1
           FIL_USED = TRIM(DIR_TMP)//'/'//FILIN(ID:IL-4)//'__'//SUFFIX
!
! -------- Determine how many threads to use
!
           CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
           IF ( ILEN(STR) == 0 ) STR = '1'
!
! -------- Run decompression
!
           COM = 'lbzip2 -n '//TRIM(STR)//' -fdc '//TRIM(FILIN)//' > '// &
     &            FIL_USED
           IS = SYSTEM ( TRIM(COM)//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL ERR_LOG ( 5212, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error in an '// &
     &              'attempt to decompress input file '//TRIM(FILIN)// &
     &              'using command '//TRIM(COM) )
                RETURN 
           END IF
           FL_TMP   = .TRUE.
         ELSE 
           FIL_USED = FILIN
           FL_TMP   = .FALSE.
      END IF
!
! --- Check the size of the input file
!
      IS = FILE_INFO ( FIL_USED(1:I_LEN(FIL_USED))//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR ) 
           CALL ERR_LOG ( 5213, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure in an '// &
     &         'attempt to access to the input file with 3D Slant '// &
     &         'Path Delays: '//FIL_USED(1:I_LEN(FIL_USED))//'  '//STR )
           RETURN 
      END IF
!
! --- Allocate memory for the buffer where to put contents 
! --- of the input file
!
      MBUF = SIZE_I8/INT8(SIZEOF(OLINE_TMP)-1) + M_ADD_LINE
      ALLOCATE ( BUF(MBUF), STAT=IER ) 
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MBUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 5214, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory' )
           RETURN 
      END IF
!
! --- Read the input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_USED, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5215, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure in '// &
     &         'reading the input file with 3D Slant Path Delays '//FILIN )
           RETURN 
      END IF
      IF ( FL_TMP ) THEN
!
! -------- Removed temporary decompressed input file
!
           CALL UNLINK ( TRIM(FIL_USED)//CHAR(0) )
      END IF
!
      IF ( BUF(1)(1:LEN(SPD__ASCII__LABEL)) == SPD__ASCII__LABEL ) THEN
           FMT = 'CUR' 
         ELSE IF ( BUF(1)(1:LEN(SPD__ASCII__LABEL)) == SPD__ASCII__LABEL_1ST ) THEN
           FMT = '1ST'
         ELSE IF ( BUF(1)(1:LEN(SPD__ASCII__LABEL)) == SPD__ASCII__LABEL_2ND ) THEN
           FMT = '2ND'
         ELSE 
           CALL CLRCH  ( STR )
           CALL TRAN   ( 13, BUF(1), STR )
           CALL ERR_LOG ( 5216, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Wrong format '// &
     &         'of the input file with 3D Slant Path Delays '// &
     &          FILIN(1:I_LEN(FILIN))//' the first line is '// &
     &          STR(1:I_LEN(STR))//' while the format label '// &
     &          SPD__ASCII__LABEL//' was expected' )
           GOTO 811
      END IF
!
      IF ( BUF(NBUF)(1:LEN(SPD__ASCII__LABEL)) == BUF(1)(1:LEN(SPD__ASCII__LABEL)) ) THEN
           CONTINUE 
         ELSE 
           CALL CLRCH  ( STR )
           CALL TRAN   ( 13, BUF(1), STR )
           CALL ERR_LOG ( 5217, IUER, 'SPD_3D_READ_ASCII_ZEN', 'The input file '// &
     &         'with 3D Slant Path Delays '//FILIN(1:I_LEN(FILIN))// &
     &         ' was read not to the end. The last line is not the '// &
     &         'same line as the first one' )
           GOTO 811
      END IF
!
      SAVED_PROC_O_MODE = SAT%PROC_O_MODE
      CALL NOUT ( SIZEOF(SAT), SAT )
      SAT%FILE = FILIN
      SAT%O_MODE = SPD__UNDF
      SAT%PROC_O_MODE = SAVED_PROC_O_MODE 
!
      DO 410 J1=2,NBUF-1
         IF ( BUF(J1)(1:1) == '#' ) THEN
              IF ( BUF(J1)(1:12) == '# Created by' ) THEN
                   SAT%LABEL = BUF(J1)(14:) 
              END IF
              GOTO 410
         END IF
         IF ( BUF(J1)(1:1) == ' ' ) GOTO 410
         IF ( BUF(J1)(1:1) == 'N' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(SAT%NLINE), %REF(BUF(J1)), SAT%NLINE )
              READ ( UNIT=SAT%NLINE%N_MOD, FMT='(I4)' ) SAT%NM 
              READ ( UNIT=SAT%NLINE%N_INP, FMT='(I4)' ) SAT%NI
              READ ( UNIT=SAT%NLINE%N_STA, FMT='(I7)' ) SAT%NS
              READ ( UNIT=SAT%NLINE%N_EL,  FMT='(I4)' ) SAT%NE
              READ ( UNIT=SAT%NLINE%N_AZ,  FMT='(I4)' ) SAT%NA
              IF ( FMT == 'CUR' ) THEN
                   READ ( UNIT=SAT%NLINE%N_FRQ,  FMT='(I4)' ) SAT%NF
                 ELSE 
                   SAT%NF = 0
              END IF
!
              ALLOCATE ( SAT%MLINE(SAT%NM), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5218, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure '// &
     &                 'to allocate memory for SAT%MLINE' )
                   RETURN 
              END IF
!        
              ALLOCATE ( SAT%ILINE(SAT%NI), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5219, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure '// &
     &                 'to allocate memory for SAT%MLINE' )
                   GOTO 811
              END IF
!        
              ALLOCATE ( SAT%SLINE(SAT%NS), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5220, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure '// &
     &                 'to allocate memory for SAT%SLINE' )
                   GOTO 811
              END IF
!        
              ALLOCATE ( SAT%ELINE(SAT%NE), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5221, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure '// &
     &                 'to allocate memory for SAT%ELINE' )
                   GOTO 811
              END IF
!        
              ALLOCATE ( SAT%ALINE(SAT%NA), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5222, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure '// &
     &                 'to allocate memory for SAT%ALINE' )
                   GOTO 811
              END IF
!        
              ALLOCATE ( SAT%PLINE(SAT%NS), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5223, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure '// &
     &                 'to allocate memory for SAT%PLINE' )
                   GOTO 811
              END IF
!        
              ALLOCATE ( SAT%DLINE(SAT%NE,SAT%NA,SAT%NS), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5224, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure '// &
     &                 'to allocate memory for SAT%DLINE' )
                   GOTO 811
              END IF
              IF ( SAT%NF > 0 ) THEN
                   ALLOCATE ( SAT%FLINE(SAT%NF), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5225, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure '// &
     &                      'to allocate memory for SAT%FLINE' )
                        GOTO 811
                   END IF
!
                   ALLOCATE ( SAT%OLINE(SAT%NF,SAT%NE,SAT%NA,SAT%NS), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5226, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Failure '// &
     &                      'to allocate memory for SAT%OLINE' )
                        GOTO 811
                   END IF
              END IF
            ELSE IF ( BUF(J1)(1:1) == 'M' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(ILINE_TMP), %REF(BUF(J1)), ILINE_TMP )
              READ ( UNIT=ILINE_TMP%IND_LINE, FMT='(I4)', IOSTAT=IER ) IND_LINE
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5227, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                 'in decoding index in line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file with 3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
              SAT%MLINE(IND_LINE) = ILINE_TMP
              IF ( SAT%PROC_O_MODE .NE. 'NO' ) THEN
                   IF ( INDEX ( SAT%MLINE(IND_LINE)%TEXT, 'Model for atmospheric' ) > 0 ) THEN
                        CALL EXWORD ( SAT%MLINE(IND_LINE)%TEXT, MIND, LIND, IND, CHAR(32), IER )
                        IF ( SAT%MLINE(IND_LINE)%TEXT(IND(1,LIND):IND(2,LIND)) == 'rte_bent_1az' ) THEN
                             SAT%NFA = 1
                             SAT%O_MODE = SPD__1AZ
                           ELSE IF ( SAT%MLINE(IND_LINE)%TEXT(IND(1,LIND):IND(2,LIND)) == 'rte_bent' ) THEN
                             SAT%NFA = SAT%NA 
                             SAT%O_MODE = SPD__NAZ
                        END IF
                   END IF
              END IF
            ELSE IF ( BUF(J1)(1:1) == 'I' ) THEN
              CONTINUE 
            ELSE IF ( BUF(J1)(1:1) == 'U' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(SAT%ULINE), %REF(BUF(J1)), SAT%ULINE )
            ELSE IF ( BUF(J1)(1:1) == 'T' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(SAT%TLINE), %REF(BUF(J1)), SAT%TLINE )
            ELSE IF ( BUF(J1)(1:1) == 'S' ) THEN
              CALL LIB$MOVC3 ( SIZEOF(SLINE_TMP), %REF(BUF(J1)), SLINE_TMP )
              READ ( UNIT=SLINE_TMP%STA_IND_STR, FMT='(I7)', IOSTAT=IER ) IND_STA
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5228, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                 'in decoding index in line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file with 3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
              SAT%SLINE(IND_STA) = SLINE_TMP
            ELSE IF ( BUF(J1)(1:1) == 'E' ) THEN
              CONTINUE 
            ELSE IF ( BUF(J1)(1:1) == 'A' ) THEN
              CONTINUE 
            ELSE IF ( ( FMT == 'CUR'  .AND.  BUF(J1)(1:1) == 'P' ) .OR.  &
     &                ( FMT == '1ST'  .AND.  BUF(J1)(1:1) == 'P' ) .OR.  &
     &                ( FMT == '2ND'  .AND.  BUF(J1)(1:1) == 'F' )       ) THEN
              IF ( PAR == 'pres' .OR. &
     &             PAR == 'pwp'  .OR. &
     &             PAR == 'temp'      ) THEN
                   IF ( FMT == 'CUR' ) THEN
                        CALL LIB$MOVC3 ( SIZEOF(PLINE_TMP), %REF(BUF(J1)), PLINE_TMP )
                        READ ( UNIT=PLINE_TMP%STA_IND, FMT='(I7)', IOSTAT=IER ) IND_STA
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J1, STR )
                             CALL ERR_LOG ( 5229, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                           'in decoding index in line '//STR(1:I_LEN(STR))// &
     &                           ' of the input file with 3D Slant Path Delays '//FILIN )
                             GOTO 811
                        END IF
                        SAT%PLINE(IND_STA) = PLINE_TMP
                     ELSE IF ( FMT == '1ST' .OR.  FMT == '2ND' ) THEN
                        CALL LIB$MOVC3 ( SIZEOF(PLINE_TMP), %REF(BUF(J1)), PLINE_TMP_V2 )
                        READ ( UNIT=PLINE_TMP_V2%STA_IND, FMT='(I7)', IOSTAT=IER ) IND_STA
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J1, STR )
                             CALL ERR_LOG ( 5230, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                           'in decoding index in line '//STR(1:I_LEN(STR))// &
     &                           ' of the input file with 3D Slant Path Delays '//FILIN )
                             GOTO 811
                        END IF
                        CALL CLRCH ( SAT%PLINE(IND_STA) )
                        SAT%PLINE(IND_STA)%CODE    = PLINE_TMP_V2%CODE
                        SAT%PLINE(IND_STA)%STA_IND = PLINE_TMP_V2%STA_IND
                        SAT%PLINE(IND_STA)%PRES    = PLINE_TMP_V2%PRES
                        SAT%PLINE(IND_STA)%TEMP    = PLINE_TMP_V2%TEMP
                        SAT%PLINE(IND_STA)%WATER_VAPOR_PRES = '    0.0'
                   END IF
              END IF
            ELSE IF ( FMT == 'CUR'  .AND.  BUF(J1)(1:1) == 'F' .OR. &
     &                FMT == '1ST'  .AND.  BUF(J1)(1:1) == 'F'      ) THEN
              CALL LIB$MOVC3 ( SIZEOF(FLINE_TMP), %REF(BUF(J1)), FLINE_TMP )
              READ ( UNIT=FLINE_TMP%FRQ_IND, FMT='(I4)', IOSTAT=IER ) IND_FRQ
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5231, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                 'in decoding index in line '//STR(1:I_LEN(STR))// &
     &                 ' of the input file with 3D Slant Path Delays '//FILIN )
                   GOTO 811
              END IF
              SAT%FLINE(IND_FRQ) = FLINE_TMP
            ELSE IF ( BUF(J1)(1:1) == 'D' ) THEN
              IF ( BUF(J1)(10:21) ==  '     1     1' .AND. &
     &             ( PAR == 'delt' .OR. &
     &               PAR == 'del'  .OR. &
     &               PAR == 'delw' .OR. &
     &               PAR == 'deld'      )  ) THEN
!
                   CALL LIB$MOVC3 ( SIZEOF(DLINE_TMP), %REF(BUF(J1)), DLINE_TMP )
                   READ ( UNIT=DLINE_TMP%STA_IND, FMT='(I7)', IOSTAT=IER ) IND_STA
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 5232, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                      'in decoding station index in line '// &
     &                       STR(1:I_LEN(STR))//' of the input file with '// &
     &                      '3D Slant Path Delays '//FILIN )
                        GOTO 811
                   END IF
!
                   READ ( UNIT=DLINE_TMP%EL_IND, FMT='(I4)', IOSTAT=IER ) IND_EL
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 5233, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                      'in decoding elevation index in line '// &
     &                       STR(1:I_LEN(STR))//' of the input file with '// &
     &                      '3D Slant Path Delays '//FILIN )
                        GOTO 811
                   END IF
!
                   READ ( UNIT=DLINE_TMP%AZ_IND, FMT='(I4)', IOSTAT=IER ) IND_AZ
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 5234, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                      'in decoding azimuth index in line '// &
     &                       STR(1:I_LEN(STR))//' of the input file with '// &
     &                       '3D Slant Path Delays '//FILIN )
                        GOTO 811
                   END IF
                   SAT%DLINE(IND_EL,IND_AZ,IND_STA) = DLINE_TMP
              END IF
!
! ----------- NB: We bypass reading O-lines when SAT%PROC_O_MODE is 'NO'
!
            ELSE IF ( BUF(J1)(1:1) == 'O' .AND. SAT%PROC_O_MODE .NE. SPD__NO ) THEN
              IF ( BUF(J1)(10:21) ==  '     1     1' .AND. &
     &             ( PAR == 'opa'  .OR. &
     &               PAR == 'tatm'      ) ) THEN
                   SAT%PROC_O_MODE = SPD__YES
!
                   CALL LIB$MOVC3 ( SIZEOF(OLINE_TMP), %REF(BUF(J1)), OLINE_TMP )
                   READ ( UNIT=OLINE_TMP%STA_IND, FMT='(I7)', IOSTAT=IER ) IND_STA
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 5235, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                      'in decoding station index in line '// &
     &                       STR(1:I_LEN(STR))//' of the input file with '// &
     &                      '3D Slant Path Delays '//FILIN )
                        GOTO 811
                   END IF
!
                   READ ( UNIT=OLINE_TMP%EL_IND, FMT='(I4)', IOSTAT=IER ) IND_EL
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 5236, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                      'in decoding elevation index in line '// &
     &                       STR(1:I_LEN(STR))//' of the input file with '// &
     &                      '3D Slant Path Delays '//FILIN )
                        GOTO 811
                   END IF
!
                   READ ( UNIT=OLINE_TMP%AZ_IND, FMT='(I4)', IOSTAT=IER ) IND_AZ
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 5237, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                      'in decoding azimuth index in line '// &
     &                       STR(1:I_LEN(STR))//' of the input file with '// &
     &                       '3D Slant Path Delays '//FILIN )
                        GOTO 811
                   END IF
!
                   READ ( UNIT=OLINE_TMP%FRQ_IND, FMT='(I4)', IOSTAT=IER ) IND_FRQ
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 5238, IUER, 'SPD_3D_READ_ASCII_ZEN', 'Error '// &
     &                      'in decoding azimuth index in line '// &
     &                       STR(1:I_LEN(STR))//' of the input file with '// &
     &                       '3D Slant Path Delays '//FILIN )
                        GOTO 811
                   END IF
                   SAT%OLINE(IND_FRQ,IND_EL,IND_AZ,IND_STA) = OLINE_TMP
              END IF
         END IF
 410  CONTINUE 
 810  CONTINUE 
!      
      DEALLOCATE   ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN 
!
 811  CONTINUE 
!
! --- Clean the buffer
!
      DEALLOCATE    ( BUF )
!
      RETURN
      END  SUBROUTINE SPD_3D_READ_ASCII_ZEN  !#!  
