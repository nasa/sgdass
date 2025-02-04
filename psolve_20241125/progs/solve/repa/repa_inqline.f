      SUBROUTINE REPA_INQLINE ( REP, IND_BAS, IND_CLR, IND_PT, OUT )
! ************************************************************************
! *                                                                      *
! *   Function REPA_DF_INQLINE  builds the line OUT with information     *
! *   about the point at the baseline with index IND_BAS, point index    *
! *   IND_PT at the category IND_CLR.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *  IND_BAS ( INTEGER*4 ) -- baseline index.                            *
! *  IND_CLR ( INTEGER*4 ) -- Caterory index. One of                     *
! *                           REPA__I_GOO -- Good point.                 *
! *                           REPA__I_BAD -- Bad, but recoverable point. *
! *                           REPA__I_UNR -- Bad, unrecoverable point.   *
! *   IND_PT ( INTEGER*4 ) -- point index **within** the categroy.       *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      OUT ( CHARACTER ) -- Output string.                             *
! *                                                                      *
! * ### 07-DEC-2004   REPA_INQLINE   v1.1 (c) L. Petrov 01-OCT-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'astro_constants.i'
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  IND_BAS, IND_CLR, IND_PT
      CHARACTER  STR*32, STR1*32, UNIT*8
      CHARACTER  LINE(REPA__LINQ)*48, OUT*(*)
      REAL*8     IONDEL_VAL, IONDEL_ERR 
      CHARACTER  REG*5
      INTEGER*4  MIND
      PARAMETER  ( REG = CHAR(0)//CHAR(9)//CHAR(32)//',:' )
      PARAMETER  ( MIND =  32 )
      INTEGER*4  ICAL, ISTA, IND_OBS, IND_SOU, IND_CINQ, J1, J2, &
     &           LIND, IND(2,MIND), IER
      CHARACTER, EXTERNAL :: JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      CALL CLRCH ( OUT )
      DO 410 J1=1,REPA__LINQ
         CALL CLRCH ( LINE(J1) )
 410  CONTINUE 
!
! --- Fill the category mark
!
      IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, 'OBS_TYPE' )
      IF ( IND_CLR .EQ. REPA__I_GOO ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_GOO(IND_PT)
           LINE(IND_CINQ) = 'G'
        ELSE IF ( IND_CLR .EQ. REPA__I_BAD ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_BAD(IND_PT)
           LINE(IND_CINQ) = 'b'
        ELSE IF ( IND_CLR .EQ. REPA__I_UNR ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_UNR(IND_PT)
           LINE(IND_CINQ) = 'u'
      END IF
      IND_SOU = REP%OBS(IND_OBS)%IND_SOU
!
! --- Fill the observation index
!
      IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, 'OBS_INDEX' )
      CALL INCH   ( IND_OBS, LINE(IND_CINQ) )
      CALL CHASHL ( LINE(IND_CINQ) ) 
      LINE(IND_CINQ) = '#'//LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))
!
! --- Fill the baseline name
!
      IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, 'BAS_NAME' )
      LINE(IND_CINQ) = REP%LIS%C_BAS(IND_BAS)(1:8)//'/'// &
     &                 REP%LIS%C_BAS(IND_BAS)(9:16)
!
! --- Fill and source name
!
      IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, 'SOUR_NAME' )
      LINE(IND_CINQ) = REP%LIS%C_SOU(IND_SOU)
!
!@      CALL CLRCH ( UNIT )
!@      CALL CLRCH ( STR  )
!
!
! --- Fill the long time tag
!
      IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, 'TIME_LONG' )
      LINE(IND_CINQ) = JD_TO_DATE ( REP%OBS(IND_OBS)%FJD + REP%OBS(IND_OBS)%FRACT, -3 )
!
! --- Fill the short time tag
!
      IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, 'TIME_SHORT' )
      STR = JD_TO_DATE ( REP%OBS(IND_OBS)%FJD + REP%OBS(IND_OBS)%FRACT, -3 )
      CALL INCH ( IDINT(REP%OBS(IND_OBS)%FJD - REP%OBS(1)%FJD), STR(1:2) )
      CALL CHASHR ( STR(1:2) )
      CALL BLANK_TO_ZERO ( STR(1:2) )
      LINE(IND_CINQ) = STR(1:2)//'-'//STR(12:19)
!@
!@      OUT = OUT(1:I_LEN(OUT))//' '//REP__CH_ARG(REP%CNF%ARG_IND)
!@      IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Time    ' ) THEN
!@           STR = JD_TO_DATE ( REP%OBS(IND_OBS)%FJD + REP%OBS(IND_OBS)%FRACT, -3 )
!@         ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Del_Err ' ) THEN
!@           WRITE ( UNIT=STR, FMT='(1PD12.4)' ) REP%RES(IND_OBS)%ERR_DEL
!@           UNIT='sec'
!@         ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Elev_St1' ) THEN
!@           WRITE ( UNIT=STR, FMT='(F7.3)'   ) REP%OBS(IND_OBS)%EL(1)/DEG__TO__RAD
!@           UNIT='deg'
!@         ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Elev_St2' ) THEN
!@           WRITE ( UNIT=STR, FMT='(F7.3)'   ) REP%OBS(IND_OBS)%EL(2)/DEG__TO__RAD
!@           UNIT='deg'
!@         ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Azim_St1' ) THEN
!@           WRITE ( UNIT=STR, FMT='(F7.3)'   ) REP%OBS(IND_OBS)%AZ(1)/DEG__TO__RAD
!@           UNIT='deg'
!@         ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Azim_St2' ) THEN
!@           WRITE ( UNIT=STR, FMT='(F7.3)'   ) REP%OBS(IND_OBS)%AZ(2)/DEG__TO__RAD
!@           UNIT='deg'
!@         ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Temp_St1' ) THEN
!@           WRITE ( UNIT=STR, FMT='(F7.3)'   ) REP%OBS(IND_OBS)%AIR_TEMP(1) - 273.15D0
!@           UNIT='deg C'
!@         ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Temp_St2' ) THEN
!@           WRITE ( UNIT=STR, FMT='(F7.3)'   ) REP%OBS(IND_OBS)%AIR_TEMP(2) - 273.15D0
!@           UNIT='deg C'
!@      END IF
!@      OUT = OUT(1:I_LEN(OUT))//': '//STR
!@      OUT = OUT(1:I_LEN(OUT))//' '//UNIT
!
! --- Fill the observable
!
      IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, 'OBSERVABLE' )
      CALL CLRCH ( UNIT )
      CALL CHASHL ( STR  ) 
      CALL CHASHL ( STR1 ) 
      IF ( REP__CH_VAL(REP%CNF%VAL_IND)(1:3) == 'Cal' .OR. &
     &     REP__CH_VAL(REP%CNF%VAL_IND)(5:7) == '_St'      ) THEN
           LINE(IND_CINQ) = REP%CH_VAL(REP%CNF%VAL_IND)
         ELSE
           LINE(IND_CINQ) = REP__CH_VAL(REP%CNF%VAL_IND)
      END IF
      IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'Delay   ' ) THEN
           IF ( DABS(REP%RES(IND_OBS)%RES_DEL) .LT. 1.D-9 ) THEN
                WRITE ( UNIT=STR,  FMT='(F6.1)' ) REP%RES(IND_OBS)%RES_DEL*1.D12
                WRITE ( UNIT=STR1, FMT='(F9.1)' ) REP%RES(IND_OBS)%ERR_DEL*1.D12
                UNIT = 'ps'
              ELSE IF ( DABS(REP%RES(IND_OBS)%RES_DEL) .LT. 1.D-6 ) THEN
                WRITE ( UNIT=STR,  FMT='(F6.1)' ) REP%RES(IND_OBS)%RES_DEL*1.D9
                WRITE ( UNIT=STR1, FMT='(F9.1)' ) REP%RES(IND_OBS)%ERR_DEL*1.D9
                UNIT = 'ns'
              ELSE 
                WRITE ( UNIT=STR,  FMT='(1D10.3)' ) REP%RES(IND_OBS)%RES_DEL
                WRITE ( UNIT=STR1, FMT='(1D10.3)' ) REP%RES(IND_OBS)%ERR_DEL
                UNIT = 'sec'
           END IF
!
           CALL CHASHL ( STR  ) 
           CALL CHASHL ( STR1 ) 
!
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' -+ '//STR1
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' '//UNIT
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'Rate    ' ) THEN
           WRITE ( UNIT=STR,  FMT='(F9.3)' ) REP%RES(IND_OBS)%RES_RAT*1.D12
           WRITE ( UNIT=STR1, FMT='(F6.3)' ) REP%RES(IND_OBS)%ERR_RAT*1.D12
           UNIT = 'ps/sec'
           CALL CHASHL ( STR  ) 
           CALL CHASHL ( STR1 ) 
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' -+ '//STR1
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' '//UNIT
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'SNR_X   ' ) THEN
           WRITE ( UNIT=STR, FMT='(F9.3)'   ) REP%OBS(IND_OBS)%SNR_X
           CALL CHASHL ( STR ) 
           UNIT ='deg C'
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' '//UNIT
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'SNR_S   ' ) THEN
           WRITE ( UNIT=STR, FMT='(F9.3)'   ) REP%OBS(IND_OBS)%SNR_S
           CALL CHASHL ( STR ) 
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' '//UNIT
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'Temp_St1' ) THEN
           WRITE ( UNIT=STR, FMT='(F7.3)'   ) REP%OBS(IND_OBS)%AIR_TEMP(1) - 273.15D0
           CALL CHASHL ( STR ) 
           UNIT ='deg C'
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' '//UNIT
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'Temp_St2' ) THEN
           WRITE ( UNIT=STR, FMT='(F7.3)'   ) REP%OBS(IND_OBS)%AIR_TEMP(2) - 273.15D0
           CALL CHASHL ( STR ) 
           UNIT = 'deg C'
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' '//UNIT
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'Pres_St1' ) THEN
           WRITE ( UNIT=STR, FMT='(F8.3)'   ) REP%OBS(IND_OBS)%AIR_PRES(1)*0.01D0
           CALL CHASHL ( STR ) 
           UNIT = 'mbar'
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' '//UNIT
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'Pres_St2' ) THEN
           WRITE ( UNIT=STR, FMT='(F8.3)'   ) REP%OBS(IND_OBS)%AIR_PRES(2)*0.01D0
           CALL CHASHL ( STR ) 
           UNIT = 'mbar'
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' '//UNIT
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'GrIon_Dl' ) THEN
           CALL REPA_GET_IONO ( REP__CH_VAL(REP%CNF%VAL_IND), IND_OBS, REP, &
     &                          IONDEL_VAL, IONDEL_ERR )
           WRITE ( UNIT=STR,  FMT='(F12.3)' ) IONDEL_VAL*1.D9
           WRITE ( UNIT=STR1, FMT='(F12.3)' ) IONDEL_ERR*1.D9
           UNIT = 'ns'
!
           CALL CHASHL ( STR  ) 
           CALL CHASHL ( STR1 ) 
!
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' -+ '//STR1
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' '//UNIT
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'PhIon_Dl' ) THEN
           CALL REPA_GET_IONO ( REP__CH_VAL(REP%CNF%VAL_IND), IND_OBS, REP, &
     &                          IONDEL_VAL, IONDEL_ERR )
           WRITE ( UNIT=STR,  FMT='(F12.1)' ) IONDEL_VAL*1.D12
           WRITE ( UNIT=STR1, FMT='(F12.1)' ) IONDEL_ERR*1.D12
           UNIT = 'ps'
!
           CALL CHASHL ( STR  ) 
           CALL CHASHL ( STR1 ) 
!
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' -+ '//STR1
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' '//UNIT
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'SpAmb_Gx' ) THEN
           WRITE ( UNIT=STR, FMT='(F9.3)' ) REP%OBS(IND_OBS)%SPAMB_GR_X*1.D9
           CALL CHASHL ( STR  ) 
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'SpAmb_Gs' ) THEN
           WRITE ( UNIT=STR, FMT='(F9.3)' ) REP%OBS(IND_OBS)%SPAMB_GR_S*1.D9
           CALL CHASHL ( STR  ) 
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'SpAmb_Px' ) THEN
           WRITE ( UNIT=STR, FMT='(F9.3)' ) REP%OBS(IND_OBS)%SPAMB_PH_X*1.D12
           CALL CHASHL ( STR  ) 
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'SpAmb_Ps' ) THEN
           WRITE ( UNIT=STR, FMT='(F9.3)' ) REP%OBS(IND_OBS)%SPAMB_PH_S*1.D12
           CALL CHASHL ( STR  ) 
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'GpAmb_Gx' ) THEN
           WRITE ( UNIT=STR, FMT='(F9.3)' ) REP%OBS(IND_OBS)%NAMB_GR_X* &
     &                                      REP%OBS(IND_OBS)%SPAMB_GR_X*1.D9
           CALL CHASHL ( STR  ) 
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'GrAmb_Gs' ) THEN
           WRITE ( UNIT=STR, FMT='(F9.3)' ) REP%OBS(IND_OBS)%NAMB_GR_S* &
     &                                      REP%OBS(IND_OBS)%SPAMB_GR_S*1.D9
           CALL CHASHL ( STR  ) 
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR

         ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND)(1:3) == 'Cal' .OR. &
     &             REP__CH_VAL(REP%CNF%VAL_IND)(5:7) == '_St'      ) THEN
           CALL CHIN ( REP__CH_VAL(REP%CNF%VAL_IND)(4:4), ICAL )
           CALL CHIN ( REP__CH_VAL(REP%CNF%VAL_IND)(8:8), ISTA )
           WRITE ( UNIT=STR, FMT='(1PD12.4)' ) REP%OBS(IND_OBS)%SCAL(ICAL,ISTA)
           UNIT = 'sec'
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//': '//STR
           LINE(IND_CINQ) = LINE(IND_CINQ)(1:I_LEN(LINE(IND_CINQ)))//' '//UNIT
      END IF
!
! --- Fill the quality code
!
      IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, 'QUAL_CODE' )
      LINE(IND_CINQ) = 'QC: '//REP%OBS(IND_OBS)%QUAL_X//'/'// &
     &                         REP%OBS(IND_OBS)%QUAL_S
!
! --- Fill the SNR
!
      IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, 'SNR' )
      WRITE ( UNIT=STR, FMT='(F6.1)'   ) REP%OBS(IND_OBS)%SNR_X
      WRITE ( UNIT=STR1, FMT='(F6.1)'   ) REP%OBS(IND_OBS)%SNR_S
      LINE(IND_CINQ) = 'SNR: '//STR(1:6)//'/'//STR1(1:6)
!
! --- Fill the elevation
!
      IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, 'ELEV_DEG' )
      WRITE ( UNIT=STR,  FMT='(F6.3)'   ) REP%OBS(IND_OBS)%EL(1)/DEG__TO__RAD
      WRITE ( UNIT=STR1, FMT='(F6.3)'   ) REP%OBS(IND_OBS)%EL(2)/DEG__TO__RAD
      LINE(IND_CINQ) = 'Elev: '//STR(1:6)//'/'//STR1(1:6)
!
! --- Fill the Azimuth
!
      IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, 'AZIM_DEG' )
      WRITE ( UNIT=STR,  FMT='(F7.3)'   ) REP%OBS(IND_OBS)%AZ(1)/DEG__TO__RAD
      WRITE ( UNIT=STR1, FMT='(F7.3)'   ) REP%OBS(IND_OBS)%AZ(2)/DEG__TO__RAD
      LINE(IND_CINQ) = 'Az: '//STR(1:6)//'/'//STR1(1:6)
!
! --- Parse thje specification line into words
!
      IER = 0
      CALL EXWORD ( REP%CNF%INQUIRY_DATA, MIND, LIND, IND, REG, IER )
!
! --- Fill the output line with words
!
      DO 420 J2=1,LIND
         IND_CINQ = LTM_DIF ( 1, REPA__LINQ, REPA__CINQ, &
     &                        REP%CNF%INQUIRY_DATA(IND(1,J2):IND(2,J2)) )
         OUT = OUT(1:ILEN(OUT)+2)//LINE(IND_CINQ)
 420  CONTINUE 
      CALL CHASHL ( OUT )
      RETURN
      END  SUBROUTINE  REPA_INQLINE
