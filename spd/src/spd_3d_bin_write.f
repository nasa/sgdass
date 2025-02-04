      SUBROUTINE SPD_3D_BIN_WRITE ( IND_STA, SPD, FILOUT, &
     &                              N_MOD, MOD_TEXT, N_MET, MET_TEXT, &
     &                              IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_WRITE 
! *                                                                      *
! *  ### 30-NOV-2008   SPD_3D_WRITE  v1.4 (c) L. Petrov 05-JAN-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'astro_constants.i'
      INTEGER*4  IND_STA, IVRB, N_MOD, N_MET, IUER
      CHARACTER  FILOUT*(*), MOD_TEXT(N_MOD)*(*), MET_TEXT(N_MET)*(*)
      TYPE     ( SPD_3D__TYPE     ) :: SPD
!
      TYPE     ( SPD_3D_LAB__TYPE ) :: LAB_REC
      TYPE     ( SPD_3D_TIM__TYPE ) :: TIM_REC
      TYPE     ( SPD_3D_STA__TYPE ) :: STA_REC
      TYPE     ( SPD_3D_MOD__TYPE ) :: MOD_REC
      TYPE     ( SPD_3D_MET__TYPE ) :: MET_REC
      TYPE     ( SPD_3D_ELV__TYPE ) :: ELV_REC
      TYPE     ( SPD_3D_AZM__TYPE ) :: AZM_REC
      TYPE     ( SPD_3D_RES__TYPE ) :: RES_REC
      CHARACTER  STR*128, STR1*128
      INTEGER*4  MJD, IOS, J1, J2, J3, J4, J5, J6, J7, J8, J9, LUN, IS, NT, IER
      ADDRESS__TYPE :: IADR 
      LOGICAL*4  LEX
      REAL*8     ARG_MIN, ARG_MAX, ARG, AZ, EL, DELS(2)
      REAL*8     UTC, TAI
      CHARACTER, EXTERNAL :: JD_TO_DATE*23, GET_CDATE*19, MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: DEL_ISA, INV_MAP_ISA 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, UNLINK, WRITE 
!
! --- Initilization of the LABEL record
!
      CALL NOUT ( SIZEOF(LAB_REC), LAB_REC )
      LAB_REC%PREF        = 'LAB_REC '
      LAB_REC%LEN         = SIZEOF(LAB_REC)
      LAB_REC%FMT_LAB     = SPD_3D_BIN__LABEL
      LAB_REC%TOT_NUM_DEL = 1
!
! --- Initialization of TIME record
!
      LAB_REC%OFF_TIM  = LAB_REC%LEN
      TIM_REC%PREF     = 'TIM_REC '
      TIM_REC%NREC     = 1
      TIM_REC%MJD_BEG  = SPD%MJD
      TIM_REC%TAI_BEG  = SPD%TAI
      TIM_REC%MJD_END  = SPD%MJD
      TIM_REC%TAI_END  = SPD%TAI
      TIM_REC%TIM_STEP = 0.0D0
      LAB_REC%LEN_TIM  = SIZEOF(TIM_REC)
!
! --- Initialization of STATION record
!
      SPD%STA(IND_STA)%LAT_GDT = DATAN ( DTAN(SPD%STA(IND_STA)%LAT_GCN)/ &
     &                                   (1.D0 - SPD__FLAT_WGS84)**2.0D0 )
      LAB_REC%OFF_STA   = LAB_REC%OFF_TIM + LAB_REC%LEN_TIM
      STA_REC%PREF      = 'STA_REC '
      STA_REC%NAME      = SPD%STA(IND_STA)%NAME
      STA_REC%COO_CFS   = SPD%STA(IND_STA)%COO_CFS
      STA_REC%PHI_GCN   = SPD%STA(IND_STA)%LAT_GCN
      STA_REC%PHI_GDT   = SPD%STA(IND_STA)%LAT_GDT
      STA_REC%LON       = SPD%STA(IND_STA)%LON
      STA_REC%HEI_ELL   = SPD%STA(IND_STA)%HEI_ELL
      STA_REC%HEI_GEOID = SPD%STA(IND_STA)%HEI_GEOID
      LAB_REC%LEN_STA   = SIZEOF(STA_REC)
!
! --- Initialization of MODEL INFORMATION record
!
      LAB_REC%OFF_MOD     = LAB_REC%OFF_STA + LAB_REC%LEN_STA
      MOD_REC%PREF        = 'MOD_REC '
      MOD_REC%SPD_TYPE(1) = SPD__TOT_STR
      MOD_REC%SPD_TYPE(2) = SPD__WAT_STR
      MOD_REC%N_RFR       = SPD__MTYP
      MOD_REC%N_LINES     = N_MOD
!
! --- Compute the length of the information in bytes
!
      MOD_REC%LEN_TEXT = 0
      DO 410 J1=1,N_MOD
         MOD_REC%LEN_TEXT = MOD_REC%LEN_TEXT + I_LEN(MOD_TEXT(J1)) + 1 ! plus an extra byte for char(0)
 410  CONTINUE 
      ALLOCATE ( MOD_REC%TEXT(MOD_REC%LEN_TEXT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MOD_REC%LEN_TEXT, STR )
           CALL ERR_LOG ( 5712, IUER, ' SPD_3D_BIN_WRITE', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the temporary record with the model description text' )
           RETURN 
      END IF
!
! --- Fill the MOD_REC record with text
!
      IADR = LOC(MOD_REC%TEXT(1))
      DO 420 J2=1,N_MOD
         CALL LIB$MOVC3 ( I_LEN(MOD_TEXT(J2))+1, &
     &                    %REF(MOD_TEXT(J2)(1:I_LEN(MOD_TEXT(J2)))//CHAR(0)), &
     &                    %VAL(IADR) )
         IADR = IADR + I_LEN(MOD_TEXT(J2))+1
 420  CONTINUE 
      LAB_REC%LEN_MOD =   SIZEOF(MOD_REC%PREF)     + SIZEOF(MOD_REC%N_RFR)   &
                        + SIZEOF(MOD_REC%SPD_TYPE) + SIZEOF(MOD_REC%N_LINES) &
     &                  + SIZEOF(MOD_REC%LEN_TEXT) + MOD_REC%LEN_TEXT
!
! --- Initialization of METEOROLOGICAL DATA record
!
      LAB_REC%OFF_MET = LAB_REC%OFF_MOD + LAB_REC%LEN_MOD 
      MET_REC%PREF    = 'MET_REC '
      MET_REC%N_LINES = N_MET
!
! --- Compute the length of the information in bytes
!
      MET_REC%LEN_TEXT = 0
      DO 430 J3=1,N_MET
         MET_REC%LEN_TEXT = MET_REC%LEN_TEXT + I_LEN(MET_TEXT(J3)) + 1
 430  CONTINUE 
      ALLOCATE ( MET_REC%TEXT(MET_REC%LEN_TEXT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MET_REC%LEN_TEXT, STR )
           CALL ERR_LOG ( 5713, IUER, ' SPD_3D_BIN_WRITE', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the temporary record with the meteorologicla data '// &
     &         'description text' )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN 
      END IF
!
! --- Fill the MET_REC record with text
!
      IADR = LOC(MET_REC%TEXT(1))
      DO 440 J4=1,N_MET
         CALL LIB$MOVC3 ( I_LEN(MET_TEXT(J4))+1, &
     &                    %REF(MET_TEXT(J4)(1:I_LEN(MET_TEXT(J4)))//CHAR(0)), &
     &                    %VAL(IADR) )
         IADR = IADR + I_LEN(MET_TEXT(J4))+1
 440  CONTINUE 
      LAB_REC%LEN_MET = SIZEOF(MET_REC%PREF)     + SIZEOF(MET_REC%N_LINES) + &
     &                  SIZEOF(MET_REC%LEN_TEXT) + MET_REC%LEN_TEXT
!
! --- Initialization of ELEVATION DATA record
!
      LAB_REC%OFF_ELV  = LAB_REC%OFF_MET + LAB_REC%LEN_MET 
      ELV_REC%PREF = 'ELV_REC '
      ELV_REC%N_EL = SPD%CONF%N_EL 
      ALLOCATE ( ELV_REC%ELEV(ELV_REC%N_EL), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*ELV_REC%N_EL, STR )
           CALL ERR_LOG ( 5714, IUER, ' SPD_3D_BIN_WRITE', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the temporary record with the elevation array' )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN 
      END IF
!
! --- Fill the elevation record with values
!
      DO 450 J5=1,ELV_REC%N_EL
         ELV_REC%ELEV(J5) = SPD%ELV%ELEV(J5)
 450  CONTINUE 
      LAB_REC%LEN_ELV = SIZEOF(ELV_REC%PREF) + SIZEOF(ELV_REC%N_EL) + &
     &                  SIZEOF(ELV_REC%ELEV)
!
! --- Initialization of AZIMUTH DATA record
!
      LAB_REC%OFF_AZM  = LAB_REC%OFF_ELV + LAB_REC%LEN_ELV
      AZM_REC%PREF = 'AZM_REC '
      AZM_REC%N_AZ = SPD%CONF%N_AZ
      ALLOCATE ( AZM_REC%AZIM(AZM_REC%N_AZ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*AZM_REC%N_AZ, STR )
           CALL ERR_LOG ( 5715, IUER, ' SPD_3D_BIN_WRITE', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the temporary record with the azimuth array' )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN 
      END IF
!
! --- Fill the azimuth record with values
!
      DO 460 J6=1,AZM_REC%N_AZ
         AZM_REC%AZIM(J6) = SPD%AZM%AZIM(J6)
 460  CONTINUE 
      LAB_REC%LEN_AZM = SIZEOF(AZM_REC%PREF) + SIZEOF(AZM_REC%N_AZ) + &
     &                  SIZEOF(AZM_REC%AZIM)
!
! --- Initialization of DELAY record
!
      LAB_REC%OFF_DEL  = LAB_REC%OFF_AZM + LAB_REC%LEN_AZM 
      RES_REC%PREF = 'DEL_REC '
      ALLOCATE ( RES_REC%DEL(SPD%CONF%N_EL,SPD%CONF%N_AZ,MOD_REC%N_RFR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*SPD%CONF%N_EL*SPD%CONF%N_AZ, STR )
           CALL ERR_LOG ( 5716, IUER, ' SPD_3D_BIN_WRITE', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the temporary record with the slanted path delay array' )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN 
      END IF
!
! --- Compute the length of the delay record
!
      LAB_REC%LEN_DEL = SIZEOF(RES_REC%PREF)    + &
     &                  SIZEOF(RES_REC%SUR_PRS) + &
     &                  SIZEOF(RES_REC%SUR_PWP) + &
     &                  SIZEOF(RES_REC%SUR_TEM) + &
     &                  SIZEOF(RES_REC%DEL)
!
! --- Check whether the output file exists
!
      INQUIRE ( FILE=FILOUT, EXIST=LEX )
      IF ( LEX ) THEN
!
! -------- File exists. Then remove it
!
           IS = UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 5717, IUER, 'SPD_3D_BIN_WRITE', 'Error in '// &
     &              'an attempt to remove the exisiting output file '// &
     &              FILOUT(1:I_LEN(FILOUT))//'  '//STR )
                CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                             MET_REC, ELV_REC, AZM_REC, RES_REC )
                RETURN 
           END IF
      END IF
!
! --- Open the output file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILOUT, 'NEW', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5718, IUER, 'SPD_3D_BIN_WRITE', 'Failure to '// &
     &         'open the output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' for writing' )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN 
      END IF
!
! --- Write the LABEL record
!
      IS = WRITE ( %VAL(LUN), LAB_REC, %VAL(LAB_REC%LEN) )
      IF ( IS < LAB_REC%LEN ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5719, IUER, 'SPD_3D_BIN_WRITE', 'Error during '// &
     &         'writing the label record of the output file '//FILOUT )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN
      END IF
!
! --- Write the TIME record
!
      IS = WRITE ( %VAL(LUN), TIM_REC, %VAL(LAB_REC%LEN_TIM) )
      IF ( IS < LAB_REC%LEN_TIM ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5720, IUER, 'SPD_3D_BIN_WRITE', 'Error during '// &
     &         'writing the time record of the output file '//FILOUT )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN
      END IF
!
! --- Write the STATION record
!
      IS = WRITE ( %VAL(LUN), STA_REC, %VAL(LAB_REC%LEN_STA) )
      IF ( IS < LAB_REC%LEN_STA ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5721, IUER, 'SPD_3D_BIN_WRITE', 'Error during '// &
     &         'writing the station record of the output file '//FILOUT )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN
      END IF
!
! --- Write the MODEL INFORMATION record
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_WRITE_MOD_REC ( MOD_REC, LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5722, IUER, 'SPD_3D_BIN_WRITE', 'Error during '// &
     &         'writing the model record of the output file '//FILOUT )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN
      END IF
!
! --- Write the METEOROLOGICAL INFORMATION DATA record
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_WRITE_MET_REC ( MET_REC, LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5723, IUER, 'SPD_3D_BIN_WRITE', 'Error during '// &
     &         'writing the meteorological data information record of '// &
     &         'the output file '//FILOUT )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN
      END IF
!
! --- Write the ELEVATION record
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_WRITE_ELV_REC ( ELV_REC, LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5724, IUER, 'SPD_3D_BIN_WRITE', 'Error during '// &
     &         'writing the elevation record of '// &
     &         'the output file '//FILOUT )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN
      END IF
!
! --- Write the AZIMUTH record
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_WRITE_AZM_REC ( AZM_REC, LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5725, IUER, 'SPD_3D_BIN_WRITE', 'Error during '// &
     &         'writing the azimuth record of '// &
     &         'the output file '//FILOUT )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN
      END IF
!
      DO 470 J7=1,TIM_REC%NREC 
!
! ------ Prepare delay record
!
         RES_REC%SUR_PRS = SPD%STA(IND_STA)%SUR_PRS
         RES_REC%SUR_PWP = SPD%STA(IND_STA)%SUR_PWP
         RES_REC%SUR_TEM = SPD%STA(IND_STA)%SUR_TEM
         DO 480 J8=1,SPD%CONF%N_AZ
            DO 490 J9=1,SPD%CONF%N_EL
               RES_REC%DEL(J9,J8,SPD__TOT) = SPD%STA(IND_STA)%DEL(J9,J8,SPD__TOT)
               RES_REC%DEL(J9,J8,SPD__WAT) = SPD%STA(IND_STA)%DEL(J9,J8,SPD__WAT)
 490        CONTINUE 
 480     CONTINUE 
!
! ------ Write the DELAY record
!
         CALL ERR_PASS ( IUER, IER )
         CALL SPD_3D_WRITE_DEL_REC ( MOD_REC%N_RFR, ELV_REC, AZM_REC, &
     &                               RES_REC, LUN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH   ( STR )
              CALL GERROR  ( STR )
              CALL CLRCH   ( STR1 )
              CALL INCH    ( J7, STR )
              CALL ERR_LOG ( 5726, IUER, 'SPD_3D_BIN_WRITE', 'Error '// &
     &            'during writing the delay record '//STR1(1:I_LEN(STR1))// &
     &            ' of the output file '//FILOUT )
              CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                           MET_REC, ELV_REC, AZM_REC, RES_REC )
              RETURN
         END IF
 470  CONTINUE 
!
! --- Close the output file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5727, IUER, 'SPD_3D_BIN_WRITE', 'Error in '// &
     &         'an attempt to close the output file '//FILOUT )
           CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                        MET_REC, ELV_REC, AZM_REC, RES_REC )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      CALL FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                   MET_REC, ELV_REC, AZM_REC, RES_REC )
      RETURN
      END  SUBROUTINE  SPD_3D_BIN_WRITE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FREE_3D_BIN ( LAB_REC, TIM_REC, STA_REC, MOD_REC, &
     &                         MET_REC, ELV_REC, AZM_REC, RES_REC )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  FREE_3D_BIN  freezes dynamic memory allocated  *
! *   for reading/writing files with slanted path delay in binary format.*
! *                                                                      *
! *  ### 08-JAN-2009  FREE_3D_BIN  v1.0 (c)  L. Petrov  08-JAN-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D_LAB__TYPE ) :: LAB_REC
      TYPE     ( SPD_3D_TIM__TYPE ) :: TIM_REC
      TYPE     ( SPD_3D_STA__TYPE ) :: STA_REC
      TYPE     ( SPD_3D_MOD__TYPE ) :: MOD_REC
      TYPE     ( SPD_3D_MET__TYPE ) :: MET_REC
      TYPE     ( SPD_3D_ELV__TYPE ) :: ELV_REC
      TYPE     ( SPD_3D_AZM__TYPE ) :: AZM_REC
      TYPE     ( SPD_3D_RES__TYPE ) :: RES_REC
!
      IF ( ASSOCIATED ( MOD_REC%TEXT ) ) DEALLOCATE ( MOD_REC%TEXT )
      IF ( ASSOCIATED ( MET_REC%TEXT ) ) DEALLOCATE ( MET_REC%TEXT )
      IF ( ASSOCIATED ( ELV_REC%ELEV ) ) DEALLOCATE ( ELV_REC%ELEV )
      IF ( ASSOCIATED ( AZM_REC%AZIM ) ) DEALLOCATE ( AZM_REC%AZIM )
      IF ( ASSOCIATED ( RES_REC%DEL  ) ) DEALLOCATE ( RES_REC%DEL  )
!
      RETURN
      END  SUBROUTINE  FREE_3D_BIN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_WRITE_MOD_REC ( MOD_REC, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  SPD_3D_WRITE_MOD_REC  writes the MOD_REC       *
! *   section into the output file opened at the LUN unit.               *
! *                                                                      *
! * ## 18-FEB-2009 SPD_3D_WRITE_MOD_REC v1.0 (c) L. Petrov 18-FEB-2009 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D__TYPE     ) :: SPD
      TYPE     ( SPD_3D_MOD__TYPE ) :: MOD_REC
      CHARACTER  STR*128
      INTEGER*4  LUN, IUER
      INTEGER*4  IS
      INTEGER*4, EXTERNAL :: WRITE
!
      IS = WRITE ( %VAL(LUN), MOD_REC%PREF, %VAL(LEN(MOD_REC%PREF)) )
      IF ( IS < LEN(MOD_REC%PREF)) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5911, IUER, 'SPD_3D_WRITE_MOD_REC', 'Error during '// &
     &         'writing the field PREF of the MOD_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), MOD_REC%N_RFR, %VAL(SIZEOF(MOD_REC%N_RFR)) )
      IF ( IS < SIZEOF(MOD_REC%N_RFR) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5912, IUER, 'SPD_3D_WRITE_MOD_REC', 'Error during '// &
     &         'writing the field MOD_REC%N_RFR of the MOD_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), MOD_REC%SPD_TYPE, %VAL(SIZEOF(MOD_REC%SPD_TYPE)) )
      IF ( IS < SIZEOF(MOD_REC%SPD_TYPE)) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5913, IUER, 'SPD_3D_WRITE_MOD_REC', 'Error during '// &
     &         'writing the field MOD_REC%SPD_TYPE of the MOD_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), MOD_REC%N_LINES, %VAL(SIZEOF(MOD_REC%N_LINES)) )
      IF ( IS < SIZEOF(MOD_REC%N_LINES) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5914, IUER, 'SPD_3D_WRITE_MOD_REC', 'Error during '// &
     &         'writing the field N_LINES of the MOD_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), MOD_REC%LEN_TEXT, %VAL(SIZEOF(MOD_REC%LEN_TEXT)) )
      IF ( IS < SIZEOF(MOD_REC%LEN_TEXT) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5915, IUER, 'SPD_3D_WRITE_MOD_REC', 'Error during '// &
     &         'writing the field LEN_TEXT of the MOD_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), MOD_REC%TEXT, %VAL(SIZEOF(MOD_REC%TEXT)) )
      IF ( IS < SIZEOF(MOD_REC%TEXT) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5916, IUER, 'SPD_3D_WRITE_MOD_REC', 'Error during '// &
     &         'writing the field TEXT of the MOD_REC record' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_WRITE_MOD_REC  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_WRITE_MET_REC ( MET_REC, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  SPD_3D_WRITE_MET_REC  writes the MET_REC       *
! *   section into the output file opened at the LUN unit.               *
! *                                                                      *
! * ## 18-FEB-2009 SPD_3D_WRITE_MET_REC v1.0 (c) L. Petrov 18-FEB-2009 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D_MET__TYPE ) :: MET_REC
      CHARACTER  STR*128
      INTEGER*4  LUN, IUER
      INTEGER*4  IS
      INTEGER*4, EXTERNAL :: WRITE
!
      IS = WRITE ( %VAL(LUN), MET_REC%PREF, %VAL(LEN(MET_REC%PREF)) )
      IF ( IS < LEN(MET_REC%PREF)) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5921, IUER, 'SPD_3D_WRITE_MET_REC', 'Error during '// &
     &         'writing the field PREF of the MET_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), MET_REC%N_LINES, %VAL(SIZEOF(MET_REC%N_LINES)) )
      IF ( IS < SIZEOF(MET_REC%N_LINES) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5922, IUER, 'SPD_3D_WRITE_MET_REC', 'Error during '// &
     &         'writing the field N_LINES of the MET_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), MET_REC%LEN_TEXT, %VAL(SIZEOF(MET_REC%LEN_TEXT)) )
      IF ( IS < SIZEOF(MET_REC%LEN_TEXT) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5923, IUER, 'SPD_3D_WRITE_MET_REC', 'Error during '// &
     &         'writing the field LEN_TEXT of the MET_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), MET_REC%TEXT, %VAL(SIZEOF(MET_REC%TEXT)) )
      IF ( IS < SIZEOF(MET_REC%TEXT) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5924, IUER, 'SPD_3D_WRITE_MET_REC', 'Error during '// &
     &         'writing the field TEXT of the MET_REC record' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_WRITE_MET_REC  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_WRITE_ELV_REC ( ELV_REC, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Aauxilliary routine  SPD_3D_WRITE_ELV_REC  writes the ELV_REC      *
! *   section into the output file opened at the LUN unit.               *
! *                                                                      *
! * # 19-FEB-2009 SPD_3D_WRITE_ELV_REC v1.0 (c) L. Petrov 19-FEB-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D_ELV__TYPE ) :: ELV_REC
      CHARACTER  STR*128
      INTEGER*4  LUN, IUER
      INTEGER*4  IS
      INTEGER*4, EXTERNAL :: WRITE
!
      IS = WRITE ( %VAL(LUN), ELV_REC%PREF, %VAL(LEN(ELV_REC%PREF)) )
      IF ( IS < LEN(ELV_REC%PREF)) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5931, IUER, 'SPD_3D_WRITE_ELV_REC', 'Error '// &
     &         'during writing the field PREF of the ELV_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), ELV_REC%N_EL, %VAL(SIZEOF(ELV_REC%N_EL)) )
      IF ( IS < SIZEOF(ELV_REC%N_EL) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5932, IUER, 'SPD_3D_WRITE_ELV_REC', 'Error '// &
     &         'during writing the field N_EL of the ELV_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), ELV_REC%ELEV, &
     &             %VAL(SIZEOF(ELV_REC%ELEV(1))*ELV_REC%N_EL) )
      IF ( IS < SIZEOF(ELV_REC%ELEV(1))*ELV_REC%N_EL ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5933, IUER, 'SPD_3D_WRITE_ELV_REC', 'Error '// &
     &         'during writing the field ELEV of the ELV_REC record' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_WRITE_ELV_REC  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_WRITE_AZM_REC ( AZM_REC, LUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  SPD_3D_WRITE_AZM_REC  writes the AZM_REC       *
! *   section  into the output file opened at the LUN unit.              *
! *                                                                      *
! * # 18-FEB-2009 SPD_3D_WRITE_AZM_REC v1.0 (c) L. Petrov 18-FEB-2009 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D_AZM__TYPE ) :: AZM_REC
      CHARACTER  STR*128
      INTEGER*4  LUN, IUER
      INTEGER*4  IS
      INTEGER*4, EXTERNAL :: WRITE
!
      IS = WRITE ( %VAL(LUN), AZM_REC%PREF, %VAL(LEN(AZM_REC%PREF)) )
      IF ( IS < LEN(AZM_REC%PREF)) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5941, IUER, 'SPD_3D_WRITE_AZM_REC', 'Error '// &
     &         'during writing the field PREF of the AZM_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), AZM_REC%N_AZ, %VAL(SIZEOF(AZM_REC%N_AZ)) )
      IF ( IS < SIZEOF(AZM_REC%N_AZ) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5942, IUER, 'SPD_3D_WRITE_AZM_REC', 'Error '// &
     &         'during writing the field N_AZ of the AZM_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), AZM_REC%AZIM, &
     &             %VAL(SIZEOF(AZM_REC%AZIM(1))*AZM_REC%N_AZ) )
      IF ( IS < SIZEOF(AZM_REC%AZIM(1))*AZM_REC%N_AZ ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5943, IUER, 'SPD_3D_WRITE_AZM_REC', 'Error '// &
     &         'during writing the field ELEV of the AZM_REC record' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_WRITE_AZM_REC  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_WRITE_DEL_REC ( N_RFR, ELV_REC, AZM_REC, RES_REC, &
     &                                  LUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  SPD_3D_WRITE_DEL_REC  writes the RES_REC       *
! *   section into the output file opened at the LUN unit.               *
! *                                                                      *
! * # 18-FEB-2009 SPD_3D_WRITE_DEL_REC v1.2 (c) L. Petrov 05-JAN-2015 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D_ELV__TYPE ) :: ELV_REC
      TYPE     ( SPD_3D_AZM__TYPE ) :: AZM_REC
      TYPE     ( SPD_3D_RES__TYPE ) :: RES_REC
      CHARACTER  STR*128
      INTEGER*4  N_RFR, LUN, IUER
      INTEGER*4  IS
      INTEGER*4, EXTERNAL :: WRITE
!
      IS = WRITE ( %VAL(LUN), RES_REC%PREF, %VAL(LEN(RES_REC%PREF)) )
      IF ( IS < LEN(RES_REC%PREF)) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5951, IUER, 'SPD_3D_WRITE_DEL_REC', 'Error '// &
     &         'during writing the field PREF of the DEL_REC record: '//STR )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), RES_REC%SUR_PRS, %VAL(SIZEOF(RES_REC%SUR_PRS)) )
      IF ( IS < SIZEOF(RES_REC%SUR_PRS) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5952, IUER, 'SPD_3D_WRITE_DEL_REC', 'Error '// &
     &         'during writing the field SUR_PRS of the DEL_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), RES_REC%SUR_PWP, %VAL(SIZEOF(RES_REC%SUR_PWP)) )
      IF ( IS < SIZEOF(RES_REC%SUR_PRS) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5953, IUER, 'SPD_3D_WRITE_DEL_REC', 'Error '// &
     &         'during writing the field SUR_PRS of the DEL_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), RES_REC%SUR_TEM, %VAL(SIZEOF(RES_REC%SUR_TEM)) )
      IF ( IS < SIZEOF(RES_REC%SUR_TEM) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5954, IUER, 'SPD_3D_WRITE_DEL_REC', 'Error '// &
     &         'during writing the field SURF_TEMP of the DEL_REC record' )
           RETURN
      END IF
!
      IS = WRITE ( %VAL(LUN), RES_REC%DEL, &
     &             %VAL(SIZEOF(RES_REC%DEL(1,1,1))*ELV_REC%N_EL*AZM_REC%N_AZ*N_RFR) )
      IF ( IS < SIZEOF(RES_REC%DEL(1,1,1))*ELV_REC%N_EL*AZM_REC%N_AZ*N_RFR ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5955, IUER, 'SPD_3D_WRITE_DEL_REC', 'Error '// &
     &         'during writing the field DEL of the DEL_REC record' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_WRITE_DEL_REC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_3D_WRITE_DEL_ADD ( IND_DEL, IND_STA, SPD, FILOUT, &
     &                                  IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine  SPD_3D_WRITE_DEL_ADD  writes to the end of     *
! *   binary file with the 3D slanted path delay the section with delay  *
! *   for epoch IND_DEL.                                                 *
! *                                                                      *
! * ## 19-FEB-2009 SPD_3D_WRITE_DEL_ADD v1.2 (c) L. Petrov 05-JAN-2015 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      CHARACTER  FILOUT*(*)
      TYPE     ( SPD_3D__TYPE     ) :: SPD
      INTEGER*4  IND_DEL, IND_STA, IUER
      TYPE     ( SPD_3D_LAB__TYPE ) :: LAB_REC
      TYPE     ( SPD_3D_TIM__TYPE ) :: TIM_REC
      TYPE     ( SPD_3D_ELV__TYPE ) :: ELV_REC
      TYPE     ( SPD_3D_AZM__TYPE ) :: AZM_REC
      TYPE     ( SPD_3D_RES__TYPE ) :: RES_REC
      CHARACTER  STR*128, STR1*128, STR2*128
      LOGICAL*4  LEX
      INTEGER*4  LUN, J1, J2, J3, SEEK_SET, SEEK_CUR, SEEK_END, ARG_LEN, IER
      REAL*8     EPS_SEC
      PARAMETER  ( EPS_SEC = 2.D0 ) ! 2 sec tolerance limit in order to &
!                                   ! accommodate possible leap second
      ADDRESS__TYPE  :: IS
      CHARACTER,     EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4,     EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: LSEEK, READ, WRITE
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_CUR', SEEK_CUR, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_END', SEEK_END, ARG_LEN )
!
! --- Check whether the output file exists
!
      INQUIRE ( FILE=FILOUT, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
!
! -------- File does not exist
!
           CALL ERR_LOG ( 5961, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Output file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' was not found' )
           RETURN 
      END IF
!
! --- Open the output file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILOUT, 'UNKNOWN', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5962, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Failure to '// &
     &         'open the output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' for writing' )
           RETURN 
      END IF
!
! --- Read the LAB_REC section
!
      CALL NOUT ( SIZEOF(LAB_REC), LAB_REC )
      IS = READ ( %VAL(LUN), LAB_REC, %VAL(SIZEOF(LAB_REC)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5963, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading file '//FILOUT )
           RETURN 
        ELSE IF ( IS < SIZEOF(LAB_REC) ) THEN
           CALL ERR_LOG ( 5964, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &          'reading the LAB section of the file '// &
     &           FILOUT(1:I_LEN(FILOUT))//' -- not all bytes have been read' )
           RETURN 
      END IF     
!
      IF ( LAB_REC%FMT_LAB .NE. SPD_3D_BIN__LABEL ) THEN
           CALL CLRCH ( STR )
           CALL TRAN ( 13, LAB_REC%FMT_LAB, STR )
           CALL ERR_LOG ( 5965, IUER, 'SPD_3D_WRITE_DEL_ADD', 'The file '// &
     &         'has a wrong format: the label format is '// &
     &          STR(1:I_LEN(STR))//' while '//SPD_3D_BIN__LABEL// &
     &         ' was expected' )
           RETURN 
      END IF
!
      IF ( IND_DEL .NE. LAB_REC%TOT_NUM_DEL + 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IND_DEL, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( 1, STR1 )
           CALL CLRCH ( STR2 )
           CALL INCH  ( LAB_REC%TOT_NUM_DEL, STR2 )
           CALL ERR_LOG ( 5966, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Parameter '// &
     &         'IND_DEL has a wrong value: '//STR(1:I_LEN(STR))//' while '// &
     &         'it should be greater by '//STR1(1:I_LEN(STR1))//' of the '// &
     &         'TOT_NUM_DEL value '//STR2(1:I_LEN(STR2))// &
     &         ' from the delay file '//FILOUT )
           RETURN 
      END IF
!
! --- Position the file to the beginning of TIM_REC record
!
      IS = LSEEK ( %VAL(LUN), %VAL(LAB_REC%OFF_TIM), %VAL(SEEK_SET) )
      IF ( IS .NE. LAB_REC%OFF_TIM ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5967, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &          'an attempt to position the input file '// &
     &           FILOUT(1:I_LEN(FILOUT))//' to the beginning of TIM_REC: '// &
     &           STR )
           RETURN 
      END IF
!
      IS = READ ( %VAL(LUN), TIM_REC, %VAL(SIZEOF(TIM_REC)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5968, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in reading file '//FILOUT )
           RETURN 
        ELSE IF ( IS < SIZEOF(TIM_REC) ) THEN
           CALL ERR_LOG ( 5969, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &          'reading the TIM section of the file '// &
     &           FILOUT(1:I_LEN(FILOUT))//' -- not all bytes have been read' )
           RETURN 
      END IF     
!
! --- Position the file to the beginning of the ELV section
!
      IS = LSEEK ( %VAL(LUN), %VAL(LAB_REC%OFF_ELV), %VAL(SEEK_SET) )
      IF ( IS .NE.  LAB_REC%OFF_ELV ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5970, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &         'an attempt to position the input file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' to the beginning of ELV '// &
     &         'section: '//STR )
           RETURN 
      END IF
!
! --- Read the ELV section
!
      CALL NOUT ( SIZEOF(ELV_REC), ELV_REC )
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_READ_ELV_REC ( ELV_REC, LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5971, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &         'reading the ELV section of the input file '//FILOUT )
           RETURN 
      END IF
!
! --- Read the AZM section
!
      CALL NOUT ( SIZEOF(AZM_REC), AZM_REC )
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_READ_AZM_REC ( AZM_REC, LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5972, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &          'reading the AZM section of the input file '//FILOUT )
           RETURN 
      END IF
!
! --- Position the file to the end
!
      IS = LSEEK ( %VAL(LUN), %VAL(0), %VAL(SEEK_END) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5973, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &          'an attempt to position the input file '// &
     &           FILOUT(1:I_LEN(FILOUT))//' to the end: '//STR )
           RETURN 
      END IF
!
      LAB_REC%TOT_NUM_DEL = LAB_REC%TOT_NUM_DEL + 1
      IF ( TIM_REC%TIM_STEP < EPS_SEC ) THEN
           TIM_REC%TIM_STEP = (SPD%MJD - TIM_REC%MJD_END)*86400.0D0 + &
     &                         SPD%TAI - TIM_REC%TAI_END
      END IF
      IF ( DABS( SPD%MJD*86400.0D0 + SPD%TAI - &
     &     (TIM_REC%MJD_END*86400.0D0 +  TIM_REC%TAI_END + &
     &      TIM_REC%TIM_STEP) ) > EPS_SEC ) THEN
!
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL CLRCH ( STR2 )
           STR  = MJDSEC_TO_DATE ( SPD%MJD, SPD%TAI, -2 )
           STR1 = MJDSEC_TO_DATE ( TIM_REC%MJD_END,  TIM_REC%TAI_END,  -2 )
           WRITE ( UNIT=STR2, FMT='(1PD15.7)' ) TIM_REC%TIM_STEP
           WRITE ( 6, * ) 'DIF_ABS: ',DABS( SPD%MJD*86400.0D0 + SPD%TAI - &
     &                     (TIM_REC%MJD_END*86400.0D0 +  TIM_REC%TAI_END + &
     &                      TIM_REC%TIM_STEP) )
!
           CALL ERR_LOG ( 5974, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error '// &
     &         'during an attempt of prepare the delay record '// &
     &         'to the output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' -- its epoch '//STR(1:21)//' differs by more than '// &
     &         ' the step '//STR2(1:21)//' from the previously written '// &
     &         'epoch '//STR1 ) 
           RETURN
      END IF
!
      TIM_REC%MJD_END = SPD%MJD
      TIM_REC%TAI_END = SPD%TAI
!
      RES_REC%PREF = 'DEL_REC '
!
! --- Allocate memory for RES_REC that is to be written into the file
!
      ALLOCATE ( RES_REC%DEL(SPD%CONF%N_EL,SPD%CONF%N_AZ,SPD__MTYP), &
                 STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*SPD%CONF%N_EL*SPD%CONF%N_AZ, STR )
           CALL ERR_LOG ( 5975, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for the temporary record with the slanted path delay array' )
           RETURN 
      END IF
!
! --- Prepare delay record
!
      RES_REC%SUR_PRS = SPD%STA(IND_STA)%SUR_PRS
      RES_REC%SUR_PWP = SPD%STA(IND_STA)%SUR_PWP
      RES_REC%SUR_TEM = SPD%STA(IND_STA)%SUR_TEM
      DO 420 J2=1,SPD%CONF%N_AZ
         DO 430 J3=1,SPD%CONF%N_EL
            RES_REC%DEL(J3,J2,SPD__TOT) = SPD%STA(IND_STA)%DEL(J3,J2,SPD__TOT)
            RES_REC%DEL(J3,J2,SPD__WAT) = SPD%STA(IND_STA)%DEL(J3,J2,SPD__WAT)
 430     CONTINUE 
 420  CONTINUE 
!
! --- Write the DEL record
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_WRITE_DEL_REC ( SPD__MTYP, ELV_REC, AZM_REC, RES_REC, LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL CLRCH   ( STR1 )
           CALL INCH    ( J1, STR )
           CALL ERR_LOG ( 5976, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &         'during writing the delay record '//STR1(1:I_LEN(STR1))// &
     &         ' of the output file '//FILOUT )
           DEALLOCATE ( RES_REC%DEL )
           RETURN
      END IF
!
! --- Deallocate RES_REC
!
      DEALLOCATE ( RES_REC%DEL )
!
! --- Position the file to the beginning of LAB_REC record 
! --- (i.e. beginning of the file)
!
      IS = LSEEK ( %VAL(LUN), %VAL(0), %VAL(SEEK_SET) )
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5977, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &          'an attempt to position the input file '// &
     &           FILOUT(1:I_LEN(FILOUT))//' to the beginning of LAB_REC: '// &
     &           STR )
           RETURN 
      END IF
!
! --- Write the updated LAB section back 
!
      IS = WRITE ( %VAL(LUN), LAB_REC, %VAL(SIZEOF(LAB_REC)) )
      IF ( IS == -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5978, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error '// &
     &          STR(1:I_LEN(STR))//' in writing into file '//FILOUT )
           RETURN 
        ELSE IF ( IS < SIZEOF(LAB_REC) ) THEN
           CALL ERR_LOG ( 5979, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &          'writing the LAB section of the file '// &
     &           FILOUT(1:I_LEN(FILOUT))//' -- not all bytes have been '// &
     &          'written' )
           RETURN 
      END IF     
!
! --- Write the TIME record
!
      IS = WRITE ( %VAL(LUN), TIM_REC, %VAL(LAB_REC%LEN_TIM) )
      IF ( IS < LAB_REC%LEN_TIM ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5980, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error '// &
     &         'during writing the time record of the output file '//FILOUT )
           RETURN
      END IF
!
! --- Position the file to the end
!
      IS = LSEEK ( %VAL(LUN), %VAL(0), %VAL(SEEK_END) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 5981, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error '// &
     &         'an attempt to position the input file '// &
     &          FILOUT(1:I_LEN(FILOUT))//' to the end: '//STR )
           RETURN 
      END IF
!
! --- Close the output file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5982, IUER, 'SPD_3D_WRITE_DEL_ADD', 'Error in '// &
     &         'an attempt to close the output file '//FILOUT )
           RETURN 
      END IF
      IF ( ASSOCIATED ( ELV_REC%ELEV ) ) DEALLOCATE ( ELV_REC%ELEV )
      IF ( ASSOCIATED ( AZM_REC%AZIM ) ) DEALLOCATE ( AZM_REC%AZIM )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_WRITE_DEL_ADD  !#!#
