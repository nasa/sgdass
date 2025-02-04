      SUBROUTINE SPD_3D_BIN_READ_HEAD ( FILIN, SPD_3D_DEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_BIN_READ_HEAD 
! *                                                                      *
! * ## 18-FEB-2009 SPD_3D_BIN_READ_HEAD v2.0 (c) L. Petrov 12-JUL-2014 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      CHARACTER  FILIN*(*)
      TYPE       ( SPD_DEL__TYPE ) :: SPD_3D_DEL
      INTEGER*4  IUER 
      CHARACTER  STR*128
      LOGICAL*4  LEX
      INTEGER*4  J1, ARG_LEN, LUN, SEEK_SET, IER
      ADDRESS__TYPE :: IS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: LSEEK, READ
      REAL*8,    EXTERNAL :: DEL_ISA 
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
!
! --- Initialization
!
      CALL SPD_3D_DEL_INIT ( SPD_3D_DEL )
!
! --- Check whether the input file exists
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
!
! -------- File does not exist
!
           CALL ERR_LOG ( 6101, IUER, 'SPD_3D_BIN_READ_HEAD', 'Input file '// &
     &          FILIN(1:I_LEN(FILIN))//'  was not found' )
           RETURN 
      END IF
!
! --- Open the input file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6102, IUER, 'SPD_3D_BIN_READ_HEAD', 'Failure to '// &
     &         'open the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' for reading' )
           RETURN 
      END IF
!
      IS = READ ( %VAL(LUN), SPD_3D_DEL%LAB, %VAL(SIZEOF(SPD_3D_DEL%LAB)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6103, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading the input file '//FILIN )
           RETURN 
        ELSE IF ( IS < SIZEOF(SPD_3D_DEL%LAB) ) THEN
           CALL ERR_LOG ( 6104, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error in '// &
     &          'reading the input file '//FILIN(1:I_LEN(FILIN))//' -- not '// &
     &          'all bytes have been read' )
           RETURN 
      END IF     
!
      IF ( SPD_3D_DEL%LAB%FMT_LAB .NE. SPD_3D_BIN__LABEL ) THEN
           CALL CLRCH ( STR )
           CALL TRAN ( 13, SPD_3D_DEL%LAB%FMT_LAB, STR )
           CALL ERR_LOG ( 6105, IUER, 'SPD_3D_BIN_READ_HEAD', 'The input file '// &
     &         'has a wrong format: the label format is '// &
     &          STR(1:I_LEN(STR))//' while '//SPD_3D_BIN__LABEL//' was expected' )
           RETURN 
      END IF
!
      IS = LSEEK ( %VAL(LUN), %VAL(SPD_3D_DEL%LAB%OFF_TIM), %VAL(SEEK_SET) )
      IF ( IS .NE. SPD_3D_DEL%LAB%OFF_TIM ) THEN
           CALL CLRCH  ( STR )
           CALL INCH8  ( SPD_3D_DEL%LAB%OFF_TIM, STR )
           CALL ERR_LOG ( 6106, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error in '// &
     &          'an attempt to position the input file '// &
     &           FILIN(1:I_LEN(FILIN))//' to offset '//STR(1:I_LEN(STR))// &
     &          ' for sealing the TIM_REC section' )
           RETURN 
      END IF
!
      IS = READ ( %VAL(LUN), SPD_3D_DEL%TIM, %VAL(SPD_3D_DEL%LAB%LEN_TIM) )
      IF ( IS == -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6107, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading the TIM section of the '// &
     &          'input file '//FILIN )
           RETURN 
        ELSE IF ( IS < SPD_3D_DEL%LAB%LEN_TIM ) THEN
           CALL ERR_LOG ( 6108, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error in '// &
     &          'reading the input file '//FILIN(1:I_LEN(FILIN))//' -- not '// &
     &          'all bytes have been read' )
           RETURN 
      END IF     
!
      IS = READ ( %VAL(LUN), SPD_3D_DEL%STA, %VAL(SPD_3D_DEL%LAB%LEN_STA) )
      IF ( IS == -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6109, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading the STA section of the '// &
     &          'input file '//FILIN )
           RETURN 
        ELSE IF ( IS < SPD_3D_DEL%LAB%LEN_STA ) THEN
           CALL ERR_LOG ( 6110, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error in '// &
     &          'reading the input file '//FILIN(1:I_LEN(FILIN))//' -- not '// &
     &          'all bytes have been read' )
           RETURN 
      END IF     
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_READ_MOD_REC ( SPD_3D_DEL%MOD, LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6111, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error in '// &
     &          'reading the MOD section of the input file '//FILIN )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_READ_MET_REC ( SPD_3D_DEL%MET, LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6112, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error in '// &
     &          'reading the MET section of the input file '//FILIN )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_READ_ELV_REC ( SPD_3D_DEL%ELV, LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6113, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error in '// &
     &          'reading the ELV section of the input file '//FILIN )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( SPD_3D_DEL%MAP_ARR ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%MAP_ARR )
      END IF
!
      ALLOCATE ( SPD_3D_DEL%MAP_ARR(SPD_3D_DEL%ELV%N_EL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6114, IUER, 'SPD_3D_BIN_READ_HEAD', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array MAP_ARR' )
           RETURN 
      END IF
!
! --- Compute ISA mapping function that corresponds to elevation angles.
!
      DO 410 J1=1,SPD_3D_DEL%ELV%N_EL
         SPD_3D_DEL%MAP_ARR(J1) = DEL_ISA ( DBLE(SPD_3D_DEL%ELV%ELEV(J1)) )/ &
     &                            DEL_ISA ( P2I )
 410  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_READ_AZM_REC ( SPD_3D_DEL%AZM, LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6115, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error in '// &
     &          'reading the AZM section of the input file '//FILIN )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6116, IUER, 'SPD_3D_BIN_READ_HEAD', 'Error in '// &
     &          'attempt to close the input file '//FILIN )
           RETURN 
      END IF
!
      SPD_3D_DEL%STATUS = SPD__READ
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_BIN_READ_HEAD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_DEL_INIT ( SPD_3D_DEL )
! ************************************************************************
! *                                                                      *
! *   Aixilliary routine SPD_3D_DEL_INIT releases memory allocated in    *
! *   the SPD_3D object (if it was allocated) and initializes its        *
! *   internal fields.
! *                                                                      *
! *  ### 19-FEB-2009   SPD_3D_DEL  v1.2 (c)  L. Petrov  05-JAN-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_3D_DEL
      INTEGER*4  J1
!
      IF ( ASSOCIATED ( SPD_3D_DEL%MOD%TEXT ) ) DEALLOCATE ( SPD_3D_DEL%MOD%TEXT )
      IF ( ASSOCIATED ( SPD_3D_DEL%MET%TEXT ) ) DEALLOCATE ( SPD_3D_DEL%MET%TEXT )
      IF ( ASSOCIATED ( SPD_3D_DEL%ELV%ELEV ) ) DEALLOCATE ( SPD_3D_DEL%ELV%ELEV )
      IF ( ASSOCIATED ( SPD_3D_DEL%AZM%AZIM ) ) DEALLOCATE ( SPD_3D_DEL%AZM%AZIM )
      IF ( ASSOCIATED ( SPD_3D_DEL%RES ) ) THEN
           IF ( SPD_3D_DEL%NDEL > 0 ) THEN
                DO 410 J1=1,SPD_3D_DEL%NDEL
                   IF ( ASSOCIATED ( SPD_3D_DEL%RES(J1)%DEL ) ) THEN
                        DEALLOCATE ( SPD_3D_DEL%RES(J1)%DEL )
                   END IF
                   IF ( ASSOCIATED ( SPD_3D_DEL%RES(J1)%OPA ) ) THEN
                        DEALLOCATE ( SPD_3D_DEL%RES(J1)%OPA )
                   END IF
                   IF ( ASSOCIATED ( SPD_3D_DEL%RES(J1)%TAT ) ) THEN
                        DEALLOCATE ( SPD_3D_DEL%RES(J1)%TAT )
                   END IF
                   SPD_3D_DEL%RES(J1)%STATUS_DEL     = SPD__UNDF
                   SPD_3D_DEL%RES(J1)%STATUS_OPA_TAT = SPD__UNDF
 410            CONTINUE 
           END IF
           DEALLOCATE ( SPD_3D_DEL%RES )
      END IF
      IF ( ASSOCIATED ( SPD_3D_DEL%SUR_PRS ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%SUR_PRS )
      END IF
      IF ( ASSOCIATED ( SPD_3D_DEL%SUR_PWP ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%SUR_PWP )
      END IF
      IF ( ASSOCIATED ( SPD_3D_DEL%SUR_TEM ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%SUR_TEM )
      END IF
      IF ( ASSOCIATED ( SPD_3D_DEL%DELS ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%DELS )
      END IF
      IF ( ASSOCIATED ( SPD_3D_DEL%TIM_ARR ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%TIM_ARR )
      END IF
      IF ( ASSOCIATED ( SPD_3D_DEL%MAP_ARR ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%MAP_ARR )
      END IF
      IF ( ASSOCIATED ( SPD_3D_DEL%ZEN_DEL ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%ZEN_DEL )
      END IF
      CALL NOUT ( SIZEOF(SPD_3D_DEL), SPD_3D_DEL )
      SPD_3D_DEL%STATUS = SPD__UNDF
!
      RETURN
      END   SUBROUTINE  SPD_3D_DEL_INIT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_READ_MOD_REC ( MOD_REC, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Aauxilliary routine  SPD_3D_READ_MOD_REC  reads the MOD_REC        *
! *   section from the input file opened at the LUN unit.                *
! *                                                                      *
! * ## 19-FEB-2009 SPD_3D_READ_MOD_REC v1.0 (c) L. Petrov 18-FEB-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D_MOD__TYPE ) :: MOD_REC
      INTEGER*4  LUN, IUER
      CHARACTER  STR*128
      INTEGER*4  IER
      ADDRESS__TYPE :: IS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: READ
!
      IS = READ ( %VAL(LUN), MOD_REC%PREF, %VAL(LEN(MOD_REC%PREF)) )
      IF ( IS < LEN(MOD_REC%PREF)) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6121, IUER, 'SPD_3D_READ_MOD_REC', 'Error during '// &
     &         'READING the field PREF of the MOD_REC record' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), MOD_REC%N_RFR, %VAL(SIZEOF(MOD_REC%N_RFR)) )
      IF ( IS < SIZEOF(MOD_REC%N_RFR) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6122, IUER, 'SPD_3D_READ_MOD_REC', 'Error during '// &
     &         'READING the field N_RFR of the MOD_REC record' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), MOD_REC%SPD_TYPE, %VAL(SIZEOF(MOD_REC%SPD_TYPE)) )
      IF ( IS < SIZEOF(MOD_REC%SPD_TYPE)) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6123, IUER, 'SPD_3D_READ_MOD_REC', 'Error during '// &
     &         'reading the field N_LINES of the MOD_REC record' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), MOD_REC%N_LINES, %VAL(SIZEOF(MOD_REC%N_LINES)) )
      IF ( IS < SIZEOF(MOD_REC%N_LINES) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6124, IUER, 'SPD_3D_WRITE_MOD_REC', 'Error during '// &
     &         'reading the field MOD_REC%SPD_TYPE of the MOD_REC record' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), MOD_REC%LEN_TEXT, %VAL(SIZEOF(MOD_REC%LEN_TEXT)) )
      IF ( IS < SIZEOF(MOD_REC%LEN_TEXT) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6125, IUER, 'SPD_3D_READ_MOD_REC', 'Error during '// &
     &         'reading the field LEN_TEXT of the MOD_REC record' )
           RETURN
      END IF
!
      IF ( ASSOCIATED ( MOD_REC%TEXT ) ) DEALLOCATE ( MOD_REC%TEXT )
      ALLOCATE ( MOD_REC%TEXT(MOD_REC%LEN_TEXT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( MOD_REC%LEN_TEXT, STR )
           CALL ERR_LOG ( 6126, IUER, 'SPD_3D_READ_MOD_REC', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array MOD_REC%TEXT' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), MOD_REC%TEXT, %VAL(SIZEOF(MOD_REC%TEXT)) )
      IF ( IS < SIZEOF(MOD_REC%TEXT) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6127, IUER, 'SPD_3D_READ_MOD_REC', 'Error during '// &
     &         'reading the field TEXT of the MOD_REC record' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_READ_MOD_REC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_READ_MET_REC ( MET_REC, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Aauxilliary routine  SPD_3D_READ_MET_REC  reads the MET_REC        *
! *   section from the input file opened at the LUN unit.                *
! *                                                                      *
! * ## 19-FEB-2009 SPD_3D_READ_MET_REC v1.0 (c) L. Petrov 18-FEB-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D_MET__TYPE ) :: MET_REC
      INTEGER*4  LUN, IUER
      CHARACTER  STR*128
      INTEGER*4  IER
      ADDRESS__TYPE :: IS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: READ
!
      IS = READ ( %VAL(LUN), MET_REC%PREF, %VAL(LEN(MET_REC%PREF)) )
      IF ( IS < LEN(MET_REC%PREF)) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6131, IUER, 'SPD_3D_READ_MET_REC', 'Error during '// &
     &         'READING the field PREF of the MET_REC record' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), MET_REC%N_LINES, %VAL(SIZEOF(MET_REC%N_LINES)) )
      IF ( IS < SIZEOF(MET_REC%N_LINES) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6132, IUER, 'SPD_3D_READ_MET_REC', 'Error during '// &
     &         'reading the field N_LINES of the MET_REC record' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), MET_REC%LEN_TEXT, %VAL(SIZEOF(MET_REC%LEN_TEXT)) )
      IF ( IS < SIZEOF(MET_REC%LEN_TEXT) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6133, IUER, 'SPD_3D_READ_MET_REC', 'Error during '// &
     &         'reading the field LEN_TEXT of the MET_REC record' )
           RETURN
      END IF
!
      IF ( ASSOCIATED ( MET_REC%TEXT ) ) DEALLOCATE ( MET_REC%TEXT )
      ALLOCATE ( MET_REC%TEXT(MET_REC%LEN_TEXT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( MET_REC%LEN_TEXT, STR )
           CALL ERR_LOG ( 6134, IUER, 'SPD_3D_READ_MET_REC', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array MET_REC%TEXT' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), MET_REC%TEXT, %VAL(SIZEOF(MET_REC%TEXT)) )
      IF ( IS < SIZEOF(MET_REC%TEXT) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6135, IUER, 'SPD_3D_READ_MET_REC', 'Error during '// &
     &         'reading the field TEXT of the MET_REC record' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_READ_MET_REC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_READ_ELV_REC ( ELV_REC, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Aauxilliary routine  SPD_3D_READ_ELV_REC  reads the ELV_REC        *
! *   section from the input file opened at the LUN unit.                *
! *                                                                      *
! * ## 19-FEB-2009 SPD_3D_READ_ELV_REC v1.0 (c) L. Petrov 19-FEB-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D_ELV__TYPE ) :: ELV_REC
      CHARACTER  STR*128
      INTEGER*4  LUN, IUER
      INTEGER*4  IER
      ADDRESS__TYPE :: IS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: READ
!
      IS = READ ( %VAL(LUN), ELV_REC%PREF, %VAL(LEN(ELV_REC%PREF)) )
      IF ( IS < LEN(ELV_REC%PREF)) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6141, IUER, 'SPD_3D_READ_ELV_REC', 'Error during '// &
     &         'reading the field PREF of the ELV_REC record' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), ELV_REC%N_EL, %VAL(SIZEOF(ELV_REC%N_EL)) )
      IF ( IS < SIZEOF(ELV_REC%N_EL) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6142, IUER, 'SPD_3D_READ_ELV_REC', 'Error during '// &
     &         'reading the field N_EL of the ELV_REC record' )
           RETURN
      END IF
!
      IF ( ASSOCIATED ( ELV_REC%ELEV ) ) DEALLOCATE ( ELV_REC%ELEV )
      ALLOCATE ( ELV_REC%ELEV(ELV_REC%N_EL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) ' ELV_REC%N_EL = ', ELV_REC%N_EL
           CALL CLRCH  ( STR )
           CALL IINCH8 ( ELV_REC%N_EL*SIZEOF(ELV_REC%ELEV(1)), STR )
           CALL ERR_LOG ( 6143, IUER, 'SPD_3D_READ_ELV_REC', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array ELV_REC%ELEV' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), ELV_REC%ELEV, &
     &            %VAL(SIZEOF(ELV_REC%ELEV(1))*ELV_REC%N_EL) )
      IF ( IS < SIZEOF(ELV_REC%ELEV(1))*ELV_REC%N_EL ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6144, IUER, 'SPD_3D_READ_ELV_REC', 'Error during '// &
     &         'reading the field ELEV of the ELV_REC record' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_READ_ELV_REC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_READ_AZM_REC ( AZM_REC, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Aauxilliary routine  SPD_3D_READ_AZM_REC  reads the AZM_REC        *
! *   section from the input file opened at the LUN unit.                *
! *                                                                      *
! * ## 19-FEB-2009 SPD_3D_READ_AZM_REC v1.0 (c) L. Petrov 19-FEB-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D_AZM__TYPE ) :: AZM_REC
      CHARACTER  STR*128
      INTEGER*4  LUN, IUER
      INTEGER*4  IER
      ADDRESS__TYPE :: IS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: READ
!
      IS = READ ( %VAL(LUN), AZM_REC%PREF, %VAL(LEN(AZM_REC%PREF)) )
      IF ( IS < LEN(AZM_REC%PREF)) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6151, IUER, 'SPD_3D_READ_AZM_REC', 'Error during '// &
     &         'reading the field PREF of the AZM_REC record' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), AZM_REC%N_AZ, %VAL(SIZEOF(AZM_REC%N_AZ)) )
      IF ( IS < SIZEOF(AZM_REC%N_AZ) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6152, IUER, 'SPD_3D_READ_AZM_REC', 'Error during '// &
     &         'reading the field N_AZ of the AZM_REC record' )
           RETURN
      END IF
!
      IF ( ASSOCIATED ( AZM_REC%AZIM ) ) DEALLOCATE ( AZM_REC%AZIM )
      ALLOCATE ( AZM_REC%AZIM(AZM_REC%N_AZ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( AZM_REC%N_AZ*SIZEOF(AZM_REC%AZIM(1)), STR )
           CALL ERR_LOG ( 6153, IUER, 'SPD_3D_READ_MOD_REC', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array AZM_REC%AZIM' )
           RETURN
      END IF
!
      IS = READ ( %VAL(LUN), AZM_REC%AZIM, &
     &            %VAL(SIZEOF(AZM_REC%AZIM(1))*AZM_REC%N_AZ) )
      IF ( IS < SIZEOF(AZM_REC%AZIM(1))*AZM_REC%N_AZ ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6154, IUER, 'SPD_3D_READ_AZM_REC', 'Error during '// &
     &         'reading the field AZIM of the AZM_REC record' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_READ_AZM_REC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_BIN_READ_DEL ( FILIN, SPD_3D_DEL, IND_REC_BEG, &
     &                                 IND_REC_END, IND_BDI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_BIN_READ_DEL 
! *                                                                      *
! * ## 18-FEB-2009 SPD_3D_BIN_READ_DEL v1.1 (c) L. Petrov 2015.03.08  ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      CHARACTER  FILIN*(*)
      TYPE       ( SPD_DEL__TYPE ) :: SPD_3D_DEL
      INTEGER*4  IND_REC_BEG, IND_REC_END, IND_BDI, IUER
      TYPE       ( SPD_3D_RES__TYPE ) :: RES_REC
      CHARACTER  STR*128, STR1*128
      LOGICAL*4  LEX
      INTEGER*4  J1, J2, J3, J4, J5, IND_REC, LUN, SEEK_SET, ARG_LEN, IER
      ADDRESS__TYPE :: IS, REC_OFF_I8 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: LSEEK, READ
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
!
      IF ( SPD_3D_DEL%STATUS .NE. SPD__ALLO  .AND. &
     &     SPD_3D_DEL%STATUS .NE. SPD__LOAD  .AND. &
     &     SPD_3D_DEL%STATUS .NE. SPD__READ        ) THEN
           CALL ERR_LOG ( 6161, IUER, 'SPD_3D_BIN_READ_DEL', 'The header '// &
     &         'of the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' -- you need to call SPD_3D_BIN_READ_HEAD first' )
           RETURN 
      END IF
!
      IF ( IND_BDI > 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_BDI, STR )
           CALL ERR_LOG ( 6162, IUER, 'SPD_3D_BIN_READ_DEL', 'Wrong value '// &
     &         'of parameter IND_BDI: '//STR(1:I_LEN(STR))//' while '// &
     &         'an integer < 2 was expected' )
           RETURN 
      END IF
      IF ( IND_REC_BEG < 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_REC_BEG, STR )
           CALL ERR_LOG ( 6163, IUER, 'SPD_3D_BIN_READ_DEL', 'Wrong value '// &
     &         'of parameter IND_REC_BEG: '//STR(1:I_LEN(STR))//' while '// &
     &         'an integer > 0 was expected' )
           RETURN 
      END IF
!
      IF ( IND_REC_BEG > SPD_3D_DEL%LAB%TOT_NUM_DEL ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_REC_BEG, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( SPD_3D_DEL%LAB%TOT_NUM_DEL, STR1 )
           CALL ERR_LOG ( 6164, IUER, 'SPD_3D_BIN_READ_DEL', 'Wrong value '// &
     &         'of parameter IND_REC_BEG: '//STR(1:I_LEN(STR))//' while '// &
     &         'an integer =< '//STR1(1:I_LEN(STR1))//' was expected' )
           RETURN 
      END IF
!
      IF ( IND_REC_END < 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_REC_END, STR )
           CALL ERR_LOG ( 6165, IUER, 'SPD_3D_BIN_READ_DEL', 'Wrong value '// &
     &         'of parameter IND_REC_BEG: '//STR(1:I_LEN(STR))//' while '// &
     &         'an integer > 0 was expected' )
           RETURN 
      END IF
!
      IF ( IND_REC_END > SPD_3D_DEL%LAB%TOT_NUM_DEL ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_REC_END, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( SPD_3D_DEL%LAB%TOT_NUM_DEL, STR1 )
           CALL ERR_LOG ( 6166, IUER, 'SPD_3D_BIN_READ_DEL', 'Wrong value '// &
     &         'of parameter IND_REC_END: '//STR(1:I_LEN(STR))//' while '// &
     &         'an integer =< '//STR1(1:I_LEN(STR1))//' was expected' )
           RETURN 
      END IF
!
      IF ( IND_REC_END < IND_REC_BEG ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_REC_END, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( IND_REC_BEG, STR1 )
           CALL ERR_LOG ( 6167, IUER, 'SPD_3D_BIN_READ_DEL', 'Wrong value '// &
     &         'of parameter IND_REC_END: '//STR(1:I_LEN(STR))//' -- '// &
     &         'it should be => IND_REC_BEG '//STR1 )
           RETURN 
      END IF
!
! --- Check whether the input file exists
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
!
! -------- File does not exist
!
           CALL ERR_LOG ( 6168, IUER, 'SPD_3D_BIN_READ_DEL', 'Input file '// &
     &          FILIN(1:I_LEN(FILIN))//'  was not found' )
           RETURN 
      END IF
!
! --- Open the input file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6169, IUER, 'SPD_3D_BIN_READ_DEL', 'Failure to '// &
     &         'open the input file '//FILIN(1:I_LEN(FILIN))// &
     &         ' for reading' )
           RETURN 
      END IF
!
! --- Allocate memory for DEL
!
      IF ( ASSOCIATED ( SPD_3D_DEL%DELS ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%DELS )
      END IF
!
      SPD_3D_DEL%N_TIM = IND_REC_END - IND_REC_BEG + 1
!
      ALLOCATE ( SPD_3D_DEL%DELS(IND_BDI:SPD_3D_DEL%ELV%N_EL, &
     &                           IND_BDI:SPD_3D_DEL%AZM%N_AZ, &
     &                           IND_BDI:SPD_3D_DEL%N_TIM, &
     &                           1:SPD_3D_DEL%MOD%N_RFR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (SPD_3D_DEL%ELV%N_EL-IND_BDI+1)* &
     &                  (SPD_3D_DEL%AZM%N_AZ-IND_BDI+1)* &
     &                  (SPD_3D_DEL%N_TIM-IND_BDI+1)* &
     &                   SPD_3D_DEL%MOD%N_RFR* &
     &                   SIZEOF(SPD_3D_DEL%DELS(1,1,1,1)), STR )
           CALL ERR_LOG ( 6170, IUER, 'SPD_3D_BIN_READ_DEL', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for the delay sub-record' )
           RETURN 
      END IF
      CALL NOUT_R4 ( (SPD_3D_DEL%ELV%N_EL-IND_BDI+1)* &
     &               (SPD_3D_DEL%AZM%N_AZ-IND_BDI+1)* &
     &               (SPD_3D_DEL%N_TIM-IND_BDI+1)* &
     &                SPD_3D_DEL%MOD%N_RFR, &
     &                SPD_3D_DEL%DELS )
!
! --- Allocate memory for SUR_PRS
!
      IF ( ASSOCIATED ( SPD_3D_DEL%SUR_PRS ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%SUR_PRS  )
      END IF
      ALLOCATE ( SPD_3D_DEL%SUR_PRS(IND_BDI:SPD_3D_DEL%N_TIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (SPD_3D_DEL%N_TIM-IND_BDI+1)* &
     &                  SIZEOF(SPD_3D_DEL%SUR_PRS(1)), STR )
           CALL ERR_LOG ( 6171, IUER, 'SPD_3D_BIN_READ_DEL', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for the surface pressure sub-record' )
           DEALLOCATE ( SPD_3D_DEL%DELS )
           RETURN 
      END IF
      CALL NOUT_R4 ( (SPD_3D_DEL%N_TIM-IND_BDI+1), SPD_3D_DEL%SUR_PRS )
!
! --- Allocate memory for SUR_PWP
!
      IF ( ASSOCIATED ( SPD_3D_DEL%SUR_PWP ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%SUR_PWP  )
      END IF
      ALLOCATE ( SPD_3D_DEL%SUR_PWP(IND_BDI:SPD_3D_DEL%N_TIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (SPD_3D_DEL%N_TIM-IND_BDI+1)* &
     &                  SIZEOF(SPD_3D_DEL%SUR_PWP(1)), STR )
           CALL ERR_LOG ( 6172, IUER, 'SPD_3D_BIN_READ_DEL', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for the surface pressure sub-record' )
           DEALLOCATE ( SPD_3D_DEL%DELS )
           RETURN 
      END IF
      CALL NOUT_R4 ( (SPD_3D_DEL%N_TIM-IND_BDI+1), SPD_3D_DEL%SUR_PWP )
!
! --- Allocate memory for SUR_TEM
!
      IF ( ASSOCIATED ( SPD_3D_DEL%SUR_TEM ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%SUR_TEM )
      END IF
      ALLOCATE ( SPD_3D_DEL%SUR_TEM(IND_BDI:SPD_3D_DEL%N_TIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (SPD_3D_DEL%N_TIM-IND_BDI+1)* &
     &                  SIZEOF(SPD_3D_DEL%SUR_TEM(1)), STR )
           CALL ERR_LOG ( 6173, IUER, 'SPD_3D_BIN_READ_DEL', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for the surface air temperature sub-record' )
           DEALLOCATE ( SPD_3D_DEL%DELS )
           DEALLOCATE ( SPD_3D_DEL%SUR_PRS )
           DEALLOCATE ( SPD_3D_DEL%SUR_PWP )
           RETURN 
      END IF
      CALL NOUT_R4 ( (SPD_3D_DEL%N_TIM-IND_BDI+1), SPD_3D_DEL%SUR_TEM )
!
! --- Allocate memory for TIM_ARR
!
      IF ( ASSOCIATED ( SPD_3D_DEL%TIM_ARR ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%TIM_ARR )
      END IF
      ALLOCATE ( SPD_3D_DEL%TIM_ARR(SPD_3D_DEL%N_TIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SPD_3D_DEL%N_TIM*4, STR )
           CALL ERR_LOG ( 6174, IUER, 'SPD_3D_BIN_READ_DEL', 'Error in '// &
     &         'an attempt to allocated '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array TIM_ARR' )
           RETURN 
      END IF
      CALL NOUT_R4 ( SPD_3D_DEL%N_TIM, SPD_3D_DEL%TIM_ARR )
!
! --- Allocate memory for DEL
!
      ALLOCATE ( RES_REC%DEL(SPD_3D_DEL%ELV%N_EL,SPD_3D_DEL%AZM%N_AZ,SPD_3D_DEL%MOD%N_RFR), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SPD_3D_DEL%ELV%N_EL*SPD_3D_DEL%AZM%N_AZ*2* &
     &                  SIZEOF(RES_REC%DEL(1,1,1)), STR )
           CALL ERR_LOG ( 6175, IUER, 'SPD_3D_BIN_READ_DEL', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for the temporary delay record' )
           DEALLOCATE ( SPD_3D_DEL%DELS )
           DEALLOCATE ( SPD_3D_DEL%SUR_PRS )
           DEALLOCATE ( SPD_3D_DEL%SUR_PWP )
           DEALLOCATE ( SPD_3D_DEL%SUR_TEM )
           DEALLOCATE ( SPD_3D_DEL%TIM_ARR )
           RETURN 
      END IF
      RES_REC%STATUS_DEL = SPD__ALLO 
!
! --- Allocate memory for ZEN_DEL
!
      IF ( ASSOCIATED ( SPD_3D_DEL%ZEN_DEL ) ) THEN
           DEALLOCATE ( SPD_3D_DEL%ZEN_DEL )
      END IF
      ALLOCATE ( SPD_3D_DEL%ZEN_DEL(IND_BDI:SPD_3D_DEL%N_TIM,SPD_3D_DEL%MOD%N_RFR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (SPD_3D_DEL%N_TIM-IND_BDI+1)*SPD_3D_DEL%MOD%N_RFR*4, STR )
           CALL ERR_LOG ( 6176, IUER, 'SPD_3D_BIN_READ_DEL', 'Error in '// &
     &         'an attempt to allocated '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array ZEN_DEL' )
           RETURN 
      END IF
      CALL NOUT_R4 ( (SPD_3D_DEL%N_TIM-IND_BDI+1)*SPD_3D_DEL%MOD%N_RFR, &
     &                SPD_3D_DEL%ZEN_DEL )
!
! --- Position the file to the beginnif of the IND_REC_BEG record
!
      REC_OFF_I8 = SPD_3D_DEL%LAB%OFF_DEL + SPD_3D_DEL%LAB%LEN_DEL*(IND_REC_BEG-1)
      IS = LSEEK ( %VAL(LUN), %VAL(REC_OFF_I8), %VAL(SEEK_SET) )
      IF ( IS .NE. REC_OFF_I8 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL CLRCH   ( STR1 )
           CALL INCH    ( IND_REC_BEG, STR1 )
           CALL ERR_LOG ( 6177, IUER, 'SPD_3D_BIN_READ_DEL', 'Error in '// &
     &          'an attempt to position the input file '// &
     &           FILIN(1:I_LEN(FILIN))//' to the beginning of the '// &
     &           STR1(1:I_LEN(STR1))//'-th delay record: '//STR )
           DEALLOCATE ( SPD_3D_DEL%DELS )
           DEALLOCATE ( SPD_3D_DEL%SUR_PRS )
           DEALLOCATE ( SPD_3D_DEL%SUR_PWP )
           DEALLOCATE ( SPD_3D_DEL%SUR_TEM )
           DEALLOCATE ( RES_REC%DEL )
           DEALLOCATE ( SPD_3D_DEL%TIM_ARR )
           RETURN 
      END IF
!
      IND_REC = 0
      DO 420 J2=IND_REC_BEG,IND_REC_END
         IND_REC = IND_REC + 1
!
! ------ Update the time array
!
         IF ( IND_REC == 1 ) THEN
              SPD_3D_DEL%TIM_ARR(IND_REC) = 0.0
            ELSE 
              SPD_3D_DEL%TIM_ARR(IND_REC) = SPD_3D_DEL%TIM_ARR(IND_REC-1) + &
     &                                      SPD_3D_DEL%TIM%TIM_STEP
         END IF
!
         IS = READ ( %VAL(LUN), RES_REC%PREF, %VAL(LEN(RES_REC%PREF)) )
         IF ( IS < LEN(RES_REC%PREF) ) THEN
              CALL CLRCH   ( STR )
              CALL GERROR  ( STR )
              CALL ERR_LOG ( 6178, IUER, 'SPD_3D_BIN_READ_DEL', 'Error '// &
     &            'during reading the field PREF from the RES_REC record: '// &
     &             STR )
              DEALLOCATE ( SPD_3D_DEL%DELS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PRS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PWP )
              DEALLOCATE ( SPD_3D_DEL%SUR_TEM )
              DEALLOCATE ( RES_REC%DEL )
              DEALLOCATE ( SPD_3D_DEL%TIM_ARR )
              DEALLOCATE ( SPD_3D_DEL%ZEN_DEL )
              RETURN
         END IF
         IF ( RES_REC%PREF .NE. 'DEL_REC '  ) THEN
              CALL ERR_LOG ( 6179, IUER, 'SPD_3D_BIN_READ_DEL', 'Trap of '// &
     &            'internal control: DEL_REC prefix is not DEL_REC' )
              DEALLOCATE ( SPD_3D_DEL%DELS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PRS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PWP )
              DEALLOCATE ( SPD_3D_DEL%SUR_TEM )
              DEALLOCATE ( RES_REC%DEL )
              DEALLOCATE ( SPD_3D_DEL%TIM_ARR )
              DEALLOCATE ( SPD_3D_DEL%ZEN_DEL )
              RETURN 
         END IF
!
         IS = READ ( %VAL(LUN), RES_REC%SUR_PRS, &
     &               %VAL(SIZEOF(RES_REC%SUR_PRS)) )
         IF ( IS < SIZEOF(RES_REC%SUR_PRS) ) THEN
              CALL CLRCH   ( STR )
              CALL GERROR  ( STR )
              CALL ERR_LOG ( 6180, IUER, 'SPD_3D_BIN_READ_DEL', 'Error '// &
     &            'during reading the field SUR_PRS from the RES_REC '// &
     &            'record: '//STR )
              DEALLOCATE ( SPD_3D_DEL%DELS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PRS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PWP )
              DEALLOCATE ( SPD_3D_DEL%SUR_TEM )
              DEALLOCATE ( RES_REC%DEL )
              DEALLOCATE ( SPD_3D_DEL%TIM_ARR )
              DEALLOCATE ( SPD_3D_DEL%ZEN_DEL )
              RETURN
         END IF
!
         IS = READ ( %VAL(LUN), RES_REC%SUR_PWP, &
     &               %VAL(SIZEOF(RES_REC%SUR_PWP)) )
         IF ( IS < SIZEOF(RES_REC%SUR_PWP) ) THEN
              CALL CLRCH   ( STR )
              CALL GERROR  ( STR )
              CALL ERR_LOG ( 6181, IUER, 'SPD_3D_BIN_READ_DEL', 'Error '// &
     &            'during reading the field SUR_PWP from the RES_REC '// &
     &            'record: '//STR )
              DEALLOCATE ( SPD_3D_DEL%DELS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PRS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PWP )
              DEALLOCATE ( SPD_3D_DEL%SUR_TEM )
              DEALLOCATE ( RES_REC%DEL )
              DEALLOCATE ( SPD_3D_DEL%TIM_ARR )
              DEALLOCATE ( SPD_3D_DEL%ZEN_DEL )
              RETURN
         END IF
!
         IS = READ ( %VAL(LUN),  RES_REC%SUR_TEM, &
     &               %VAL(SIZEOF(RES_REC%SUR_TEM)) )
         IF ( IS < SIZEOF(RES_REC%SUR_TEM) ) THEN
              CALL CLRCH   ( STR )
              CALL GERROR  ( STR )
              CALL ERR_LOG ( 6182, IUER, 'SPD_3D_BIN_READ_DEL', 'Error '// &
     &            'during reading the field SUR_TEM from the DEL_REC '// &
     &            'record: '//STR )
              DEALLOCATE ( SPD_3D_DEL%DELS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PRS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PWP )
              DEALLOCATE ( SPD_3D_DEL%SUR_TEM )
              DEALLOCATE ( RES_REC%DEL )
              DEALLOCATE ( SPD_3D_DEL%TIM_ARR )
              DEALLOCATE ( SPD_3D_DEL%ZEN_DEL )
              RETURN
         END IF
!
         IS = READ ( %VAL(LUN),  RES_REC%DEL, &
     &               %VAL(SIZEOF(RES_REC%DEL(1,1,1))* &
     &                    SPD_3D_DEL%ELV%N_EL*SPD_3D_DEL%AZM%N_AZ*SPD_3D_DEL%MOD%N_RFR) )
         IF ( IS < SIZEOF(RES_REC%DEL(1,1,1))* &
     &             SPD_3D_DEL%ELV%N_EL*SPD_3D_DEL%AZM%N_AZ*SPD_3D_DEL%MOD%N_RFR ) THEN
              CALL CLRCH   ( STR )
              CALL GERROR  ( STR )
              CALL ERR_LOG ( 6179, IUER, 'SPD_3D_BIN_READ_DEL', 'Error '// &
     &            'during reading the field DEL from the DEL_REC record: '// &
     &             STR )
              DEALLOCATE ( SPD_3D_DEL%DELS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PRS )
              DEALLOCATE ( SPD_3D_DEL%SUR_PWP )
              DEALLOCATE ( SPD_3D_DEL%SUR_TEM )
              DEALLOCATE ( RES_REC%DEL )
              DEALLOCATE ( SPD_3D_DEL%TIM_ARR )
              DEALLOCATE ( SPD_3D_DEL%ZEN_DEL )
              RETURN
         END IF
!
         DO 430 J3=1,SPD_3D_DEL%AZM%N_AZ
            DO 440 J4=1,SPD_3D_DEL%ELV%N_EL
               SPD_3D_DEL%DELS(J4,J3,IND_REC,1) = RES_REC%DEL(J4,J3,1) 
               IF ( SPD_3D_DEL%MOD%N_RFR .GE. 2 ) THEN
                    SPD_3D_DEL%DELS(J4,J3,IND_REC,2) = RES_REC%DEL(J4,J3,2) 
               END IF
               IF ( SPD_3D_DEL%MOD%N_RFR .GE. 3 ) THEN
                    SPD_3D_DEL%DELS(J4,J3,IND_REC,3) = RES_REC%DEL(J4,J3,3) 
               END IF
 440        CONTINUE 
 430     CONTINUE 
         SPD_3D_DEL%ZEN_DEL(IND_REC,1) = RES_REC%DEL(1,1,1) 
         IF ( SPD_3D_DEL%MOD%N_RFR .GE. 2 ) THEN
              SPD_3D_DEL%ZEN_DEL(IND_REC,2) = RES_REC%DEL(1,1,2) 
         END IF
         IF ( SPD_3D_DEL%MOD%N_RFR .GE. 3 ) THEN
              SPD_3D_DEL%ZEN_DEL(IND_REC,3) = RES_REC%DEL(1,1,3) 
         END IF
         SPD_3D_DEL%SUR_PRS(IND_REC) = RES_REC%SUR_PRS
         SPD_3D_DEL%SUR_PWP(IND_REC) = RES_REC%SUR_PWP
         SPD_3D_DEL%SUR_TEM(IND_REC) = RES_REC%SUR_TEM
         RES_REC%STATUS_DEL = SPD__READ
 420  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6180, IUER, 'SPD_3D_BIN_READ_DEL', 'Error in '// &
     &          'attempt to close the input file '//FILIN )
           DEALLOCATE ( SPD_3D_DEL%DELS )
           DEALLOCATE ( SPD_3D_DEL%SUR_PRS )
           DEALLOCATE ( SPD_3D_DEL%SUR_PWP )
           DEALLOCATE ( SPD_3D_DEL%SUR_TEM )
           DEALLOCATE ( RES_REC%DEL )
           DEALLOCATE ( SPD_3D_DEL%TIM_ARR )
           DEALLOCATE ( SPD_3D_DEL%ZEN_DEL )
           RETURN 
      END IF
      DEALLOCATE ( RES_REC%DEL )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_BIN_READ_DEL  !#!#
