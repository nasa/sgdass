      SUBROUTINE REPA_SETDIAGI ( REP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  REPA_SETDIAGI 
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *        REP ( RECORD    ) -- Object which keeps internal parameters   *
! *                             for program REPA (REsiduals Plots and    *
! *                             Ambiguities).                            *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 03-DEC-2004  REPA_SETDIAGI  v1.4 (c)  L. Petrov  20-MAR-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i' 
      INCLUDE   'glbc4.i'
      INCLUDE   'diagi.i'
      INCLUDE   'repa.i' 
      TYPE     ( REP__TYPE   ) :: REP
      INTEGER*4  IUER
      CHARACTER  ZAG*128, UNIT*128
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, &
     &           J1, J2, J3, IER
      CHARACTER  ARG_UNITS*16, VAL_UNITS*16
      REAL*8     XMIN_GOO, XMAX_GOO, YMIN_GOO, YMAX_GOO, &
     &           XMIN_BAD, XMAX_BAD, YMIN_BAD, YMAX_BAD, &
     &           XMIN_UNR, XMAX_UNR, YMIN_UNR, YMAX_UNR
      INTEGER*4  MODES(MUSF)
      SAVE       MODES
      REAL*8     ARG_RANGE, VAL_RANGE 
      ADDRESS__TYPE :: LOC_EXT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN,                     &
     &                       REPA_DF_INIT,    REPA_DF_CHMOD,  &
     &                       REPA_DF_SETBOX,  REPA_DF_INQ,    &
     &                       REPA_DF_QUIT,    REPA_DF_SNGAMB, &
     &                       REPA_DF_SNGTGL,  REPA_DF_GRPAMB, &
     &                       REPA_DF_GRPTGL,  REPA_DF_UNDO,   &
     &                       REPA_DF_CONT,    REPA_DF_FP,     &
     &                       REPA_DF_FRIPLO
#ifdef HPUX  ! Actually, it is a bug in HP-UX Fortran compiler
      PARAMETER  ( DIAGI__PGDN = 221 ) ! PageDown key code
      PARAMETER  ( DIAGI__PGUP = 220 ) ! PageUp   key code
#else
      PARAMETER  ( DIAGI__PGDN = CHAR(221) ) ! PageDown key code
      PARAMETER  ( DIAGI__PGUP = CHAR(220) ) ! PageUp   key code
#endif
!
      IF ( .NOT. ASSOCIATED ( REP%PLT ) ) THEN
           CALL ERR_LOG ( 7771, IUER, 'REPA_SETDIAGI', 'Trap of internal '// &
     &         'control: pointer to REP%PLT is not associated' )
           RETURN 
      END IF
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4151, IUER, 'DIAGI_1', 'Error in setting '// &
     &         'default values for the plot' )
           RETURN
      END IF
!
! --- Cycle over all baselines.
!
      IF ( .NOT. ASSOCIATED ( REP%DIAGI ) ) ALLOCATE ( REP%DIAGI(REP%N_BAS) )
      DO 410 J1=1,REP%N_BAS
!
! ------ Initialize REP%DIAGI object which is used by Multi_DiaGi utility
!
         CALL NOUT ( SIZEOF(REP%DIAGI(J1)), REP%DIAGI(J1) )
!
! ------ Fill various fields of REP%DIAGI 
!
         REP%DIAGI(J1)%IDEV = IDEV
         REP%DIAGI(J1)%NCLR = 1
         IF ( REP%CNF%SHOW_CBAD ) REP%DIAGI(J1)%NCLR = 2
         IF ( REP%CNF%SHOW_UNRC ) REP%DIAGI(J1)%NCLR = 3
         REP%DIAGI(J1)%NPOI(1) = REP%PLT(J1)%N_GOO
         REP%DIAGI(J1)%NPOI(2) = REP%PLT(J1)%N_BAD
         REP%DIAGI(J1)%NPOI(3) = REP%PLT(J1)%N_UNR
!
         REP%DIAGI(J1)%ADR_X8(1) = LOC(REP%PLT(J1)%ARG_GOO(1))
         REP%DIAGI(J1)%ADR_Y8(1) = LOC(REP%PLT(J1)%VAL_GOO(1))
         REP%DIAGI(J1)%ADR_E8(1) = LOC(REP%PLT(J1)%ERR_GOO(1))
!
         REP%DIAGI(J1)%ADR_X8(2) = LOC(REP%PLT(J1)%ARG_BAD(1))
         REP%DIAGI(J1)%ADR_Y8(2) = LOC(REP%PLT(J1)%VAL_BAD(1))
         REP%DIAGI(J1)%ADR_E8(2) = LOC(REP%PLT(J1)%ERR_BAD(1))
!
         REP%DIAGI(J1)%ADR_X8(3) = LOC(REP%PLT(J1)%ARG_UNR(1))
         REP%DIAGI(J1)%ADR_Y8(3) = LOC(REP%PLT(J1)%VAL_UNR(1))
         REP%DIAGI(J1)%ADR_E8(3) = LOC(REP%PLT(J1)%ERR_UNR(1))
!
         REP%DIAGI(J1)%ICOL(1) = REP%CNF%GOOD_CLR
         REP%DIAGI(J1)%ICOL(2) = REP%CNF%BAD_CLR
         REP%DIAGI(J1)%ICOL(3) = REP%CNF%UNRC_CLR
!
         DO 420 J2=1,3 ! Cycle over colors
            REP%DIAGI(J1)%LER(J2) = .TRUE. 
            REP%DIAGI(J1)%IBST(J2) = 2
            REP%DIAGI(J1)%ILST(J2) = 1
            REP%DIAGI(J1)%IPST(J2) = 5
            REP%DIAGI(J1)%IWST(J2) = 1
            REP%DIAGI(J1)%IOST(J2) = 1
!
            IF ( J2 .EQ. 3 ) THEN
                 REP%DIAGI(J1)%LER(J2) = .FALSE.
                 REP%DIAGI(J1)%IBST(J2) = 0
                 REP%DIAGI(J1)%IPST(J2) = 3
            END IF
 420     CONTINUE 
         REP%DIAGI(J1)%ICLR = 1
!
! ------ Set bounding box
!
         IF ( REP%PLT(J1)%N_GOO .GT. 0 ) THEN
              CALL REPA_MINMAX ( REP%PLT(J1)%N_GOO,   REP%PLT(J1)%ARG_GOO, &
     &                           REP%PLT(J1)%VAL_GOO, REP%PLT(J1)%ERR_GOO, & 
     &                           XMIN_GOO, XMAX_GOO,  YMIN_GOO, YMAX_GOO   )
              REP%DIAGI(J1)%XMIN = XMIN_GOO
              REP%DIAGI(J1)%XMAX = XMAX_GOO
              REP%DIAGI(J1)%YMIN = YMIN_GOO
              REP%DIAGI(J1)%YMAX = YMAX_GOO
            ELSE
              REP%DIAGI(J1)%XMIN = 0.0
              REP%DIAGI(J1)%XMAX = 0.0 + 1.E-6
              REP%DIAGI(J1)%YMIN = 0.0
              REP%DIAGI(J1)%YMAX = 0.0 + 1.E-6
         END IF
!
         IF ( REP%CNF%SHOW_CBAD  .AND.  REP%PLT(J1)%N_BAD .GT. 0 ) THEN
!
! ----------- Adjust the bounding box taking into account conventionally 
! ----------- bad observations
!
              CALL REPA_MINMAX ( REP%PLT(J1)%N_BAD,   REP%PLT(J1)%ARG_BAD, &
     &                           REP%PLT(J1)%VAL_BAD, REP%PLT(J1)%ERR_BAD, &
     &                           XMIN_BAD, XMAX_BAD,  YMIN_BAD, YMAX_BAD   )
              REP%DIAGI(J1)%XMIN = MIN ( REP%DIAGI(J1)%XMIN, SNGL(XMIN_BAD) )
              REP%DIAGI(J1)%XMAX = MAX ( REP%DIAGI(J1)%XMAX, SNGL(XMAX_BAD) )
!
              IF ( REP%CNF%BOU_IND .GE. REPA__I_BAD ) THEN
                   REP%DIAGI(J1)%YMIN = MIN ( REP%DIAGI(J1)%YMIN, SNGL(YMIN_BAD) )
                   REP%DIAGI(J1)%YMAX = MAX ( REP%DIAGI(J1)%YMAX, SNGL(YMAX_BAD) )
              END IF
         END IF
!
         IF ( REP%CNF%SHOW_UNRC  .AND.  REP%PLT(J1)%N_UNR .GT. 0 ) THEN
!
! ----------- Adjust the bounding box taking into account 
! ----------- uncoverable observations
!
              CALL REPA_MINMAX ( REP%PLT(J1)%N_UNR,   REP%PLT(J1)%ARG_UNR, &
     &                           REP%PLT(J1)%VAL_UNR, REP%PLT(J1)%ERR_UNR, &
     &                           XMIN_UNR, XMAX_UNR,  YMIN_UNR, YMAX_UNR   )
              REP%DIAGI(J1)%XMIN = MIN ( REP%DIAGI(J1)%XMIN, SNGL(XMIN_UNR) )
              REP%DIAGI(J1)%XMAX = MAX ( REP%DIAGI(J1)%XMAX, SNGL(XMAX_UNR) )
!
              IF ( REP%CNF%BOU_IND .GE. REPA__I_UNR ) THEN
                   REP%DIAGI(J1)%YMIN = MIN ( REP%DIAGI(J1)%YMIN, SNGL(YMIN_UNR) )
                   REP%DIAGI(J1)%YMAX = MAX ( REP%DIAGI(J1)%YMAX, SNGL(YMAX_UNR) )
              END IF
         END IF
!
         IF ( REP%CNF%BOX_SYMMETRIC == 'YES ' ) THEN
              REP%DIAGI(J1)%YMIN = -MAX ( ABS(REP%DIAGI(J1)%YMIN), &
     &                                    ABS(REP%DIAGI(J1)%YMAX)  )
              REP%DIAGI(J1)%YMAX =  MAX ( ABS(REP%DIAGI(J1)%YMIN), &
     &                                    ABS(REP%DIAGI(J1)%YMAX)  )
         END IF
!
         REP%PLT(J1)%BOU_IND = REP%CNF%BOU_IND 
!
! ------ Make the bounding box for argument a little bit wider
!
         ARG_RANGE = REP%DIAGI(J1)%XMAX - REP%DIAGI(J1)%XMIN 
         REP%DIAGI(J1)%XMIN = REP%DIAGI(J1)%XMIN - REPA__OVR_ARG*ARG_RANGE
         REP%DIAGI(J1)%XMAX = REP%DIAGI(J1)%XMAX + REPA__OVR_ARG*ARG_RANGE
!
! ------ Determine appropriate scaling for the argument
!
         IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Time    ' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D0/3600.0D0
              ARG_UNITS = '(hours)'
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Del_Err ' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D0
              ARG_UNITS = '(sec)'
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Delay   ' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D3
              ARG_UNITS = '(msec   )'
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Rate    ' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D6
              ARG_UNITS = '(musec/s)'
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Elev_St1' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D0
              ARG_UNITS = '(degrees)'
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Elev_St2' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D0
              ARG_UNITS = '(degrees)'
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Azim_St1' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D0
              ARG_UNITS = '(degrees)'
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Azim_St2' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D0
              ARG_UNITS = '(degrees)'
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Feed_St1' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D0
              ARG_UNITS = '(rad)'
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Feed_St2' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D0
              ARG_UNITS = '(rad)'
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Temp_St1' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D0
              ARG_UNITS = '(degrees C)'
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) == 'Temp_St2' ) THEN
              REP%PLT(J1)%ARG_SCL = 1.0D0
              ARG_UNITS = '(degrees C)'
         END IF
!
! ------ Determine appropriate scaling for the values and errors
!
         IF ( REP__CH_VAL(REP%CNF%VAL_IND) == 'Delay   ' ) THEN
!
! ----------- Scaling for Delay
!
              VAL_RANGE = REP%DIAGI(J1)%YMAX - REP%DIAGI(J1)%YMIN 
              IF ( VAL_RANGE .LT. 1.D-9 ) THEN
                   REP%PLT(J1)%VAL_SCL = 1.D12
                   VAL_UNITS = '(psec)'
                 ELSE 
                   REP%PLT(J1)%VAL_SCL = 1.D9
                   VAL_UNITS = '(nsec)'
              END IF 
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'Rate    '      ) THEN
!
! ----------- Scaling for Delay Rate
!
              VAL_RANGE = REP%DIAGI(J1)%YMAX - REP%DIAGI(J1)%YMIN 
              IF ( VAL_RANGE .LT. 1.D-12 ) THEN
                   REP%PLT(J1)%VAL_SCL = 1.D15
                   VAL_UNITS = '(fs/sec)'
                 ELSE 
                   REP%PLT(J1)%VAL_SCL = 1.D12
                   VAL_UNITS = '(ps/sec)'
              END IF 
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'SNR_X   ' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D0
              VAL_UNITS = ' '
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'SNR_S   ' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D0
              VAL_UNITS = ' '
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'Temp_St1' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D0
              VAL_UNITS = '(deg C)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'Temp_St2' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D0
              VAL_UNITS = '(deg C)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'Pres_St1' ) THEN
              REP%PLT(J1)%VAL_SCL = 0.01D0
              VAL_UNITS = '(mbar)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'Pres_St2' ) THEN
              REP%PLT(J1)%VAL_SCL = 0.01D0
              VAL_UNITS = '(mbar)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'GrIon_Dl' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D9
              VAL_UNITS = '(ns)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'PhIon_Dl' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D9
              VAL_UNITS = '(ns)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'SpAmb_Gx' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D9
              VAL_UNITS = '(ns)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'SpAmb_Gs' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D9
              VAL_UNITS = '(ns)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'SpAmb_Px' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D12
              VAL_UNITS = '(ps)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'SpAmb_Ps' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D12
              VAL_UNITS = '(ps)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'GrAmb_Gx' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D9
              VAL_UNITS = '(ns)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'GrAmb_Gs' ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0D9
              VAL_UNITS = '(ns)'
            ELSE IF ( REP__CH_VAL(REP%CNF%VAL_IND)(1:3) .EQ. 'Cal' .AND. &
     &                REP__CH_VAL(REP%CNF%VAL_IND)(5:7) .EQ. '_St'       ) THEN
              REP%PLT(J1)%VAL_SCL = 1.0
              VAL_UNITS = '(sec)'
         END IF
!
! ------ Scaling arguments, values and errors
!
         DO 430 J3=1,3
            CALL REPA_PLOT_SCALE ( REP%DIAGI(J1)%NPOI(J3),         &
     &                             %VAL(REP%DIAGI(J1)%ADR_X8(J3)), & 
     &                             %VAL(REP%DIAGI(J1)%ADR_Y8(J3)), & 
     &                             %VAL(REP%DIAGI(J1)%ADR_E8(J3)), & 
     &                             REP%PLT(J1)%ARG_SCL, REP%PLT(J1)%VAL_SCL  )
 430     CONTINUE 
!
         REP%DIAGI(J1)%XMIN = REP%DIAGI(J1)%XMIN * REP%PLT(J1)%ARG_SCL
         REP%DIAGI(J1)%XMAX = REP%DIAGI(J1)%XMAX * REP%PLT(J1)%ARG_SCL
         REP%DIAGI(J1)%YMIN = REP%DIAGI(J1)%YMIN * REP%PLT(J1)%VAL_SCL
         REP%DIAGI(J1)%YMAX = REP%DIAGI(J1)%YMAX * REP%PLT(J1)%VAL_SCL
!
! ------ Adjst the range of the box in order to have  alootle bit extra space
!
         VAL_RANGE = REP%DIAGI(J1)%YMAX - REP%DIAGI(J1)%YMIN 
         REP%DIAGI(J1)%YMIN = REP%DIAGI(J1)%YMIN - REPA__OVR_ARG*VAL_RANGE
         REP%DIAGI(J1)%YMAX = REP%DIAGI(J1)%YMAX + REPA__OVR_ARG*VAL_RANGE
!
! ------ Set the plot title
!
         REP%DIAGI(J1)%ZAG = &
     &           REP%DBNAME_STR(1:I_LEN(REP%DBNAME_STR))//' '//                &
     &           REP%LIS%C_BAS(J1)(1:8)//'/'//REP%LIS%C_BAS(J1)(9:16)//' '//   &
     &       REP__CH_VAL(REP%CNF%VAL_IND)(1:I_LEN(REP__CH_VAL(REP%CNF%VAL_IND)))
         IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'Delay   ' .OR. &
     &        REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'Rate    '      ) THEN
              REP%DIAGI(J1)%ZAG = &
     &            REP%DIAGI(J1)%ZAG(1:I_LEN(REP%DIAGI(J1)%ZAG))//' '// &
     &            DATYP__ABR(1+REP%DATYP_I2*6:(REP%DATYP_I2+1)*6)//' '// &
     &            VAL_UNITS
            ELSE 
              REP%DIAGI(J1)%ZAG = &
     &            REP%DIAGI(J1)%ZAG(1:I_LEN(REP%DIAGI(J1)%ZAG))//' '// &
     &            VAL_UNITS
         END IF
!
! ------ Set the name of the plot short title
!
         REP%TITS(J1) = REP%LIS%C_BAS(J1)(1:8)//'/'//REP%LIS%C_BAS(J1)(9:16)
         CALL CLRCH ( REP%PREF(J1) )
!
! ------ Set units of the argument
!
         IF ( REP__CH_ARG(REP%CNF%ARG_IND) .EQ. 'Time    ' ) THEN
              REP%DIAGI(J1)%ARG_UNITS = 'Time '//ARG_UNITS
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) .EQ. 'Del_Err ' ) THEN
              REP%DIAGI(J1)%ARG_UNITS = 'Delay error '//ARG_UNITS
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) .EQ. 'Delay   ' ) THEN
              REP%DIAGI(J1)%ARG_UNITS = 'Delay '//ARG_UNITS
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) .EQ. 'Rate    ' ) THEN
              REP%DIAGI(J1)%ARG_UNITS = 'Delay rate '//ARG_UNITS
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) .EQ. 'Elev_St1' ) THEN
              REP%DIAGI(J1)%ARG_UNITS = 'Elevation at station #1 '//ARG_UNITS
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) .EQ. 'Elev_St2' ) THEN
              REP%DIAGI(J1)%ARG_UNITS = 'Elevation at station #2 '//ARG_UNITS
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) .EQ. 'Azim_St1' ) THEN
              REP%DIAGI(J1)%ARG_UNITS = 'Azimuth at station #1 '//ARG_UNITS
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) .EQ. 'Azim_St2' ) THEN
              REP%DIAGI(J1)%ARG_UNITS = 'Azimuth at station #2 '//ARG_UNITS
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) .EQ. 'Temp_St1' ) THEN
              REP%DIAGI(J1)%ARG_UNITS = 'Air temperatire st. #1 '//ARG_UNITS
            ELSE IF ( REP__CH_ARG(REP%CNF%ARG_IND) .EQ. 'Temp_St2' ) THEN
              REP%DIAGI(J1)%ARG_UNITS = 'Air temperature st. #2 '//ARG_UNITS
         END IF
!
! ------ Define the name of the hard-copy plot file
!
         REP%DIAGI(J1)%NAME   = '/tmp/'//REP%DBNAME_STR(1:10)
         REP%DIAGI(J1)%NAME   = REP%DIAGI(J1)%NAME(1:I_LEN(REP%DIAGI(J1)%NAME))// &
     &          '_'//REP%LIS%C_BAS(J1)(1:8)
         REP%DIAGI(J1)%NAME   = REP%DIAGI(J1)%NAME(1:I_LEN(REP%DIAGI(J1)%NAME))// &
     &          '.'//REP%LIS%C_BAS(J1)(9:16)
         REP%DIAGI(J1)%NAME   = REP%DIAGI(J1)%NAME(1:I_LEN(REP%DIAGI(J1)%NAME))// &
     &          '_'//REP__CH_VAL(REP%CNF%VAL_IND)
         REP%DIAGI(J1)%NAME   = REP%DIAGI(J1)%NAME(1:I_LEN(REP%DIAGI(J1)%NAME))// &
     &          '_'//REP__CH_ARG(REP%CNF%ARG_IND)
!
         REP%DIAGI(J1)%ITRM   = 0
         REP%DIAGI(J1)%IBATCH = 0
         REP%DIAGI(J1)%STATUS = DIA__DEF 
         CALL NOUT_I4 ( MUSF, MODES )
!
! ====== Section of DiaGi user function definitions
! ------ Here we define function name and the list of argiments
!
         REP%DIAGI(J1)%NUSER_FUNC       = 40 ! total number of user functions
         REP%DIAGI(J1)%UPDATE_USER_FUNC = 1
         REP%DIAGI(J1)%QUIT_USER_FUNC   = 0
!!
         REP%DIAGI(J1)%USER_FUNC(1)  = LOC_EXT(REPA_DF_INIT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,1) = 2 ! number of arguments of REPA_DF_INIT
         REP%DIAGI(J1)%USER_ARG(1,1) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,1) = LOC(REP)     
!!
         REP%DIAGI(J1)%USER_FUNC(2)  = LOC_EXT(REPA_DF_CHMOD) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,2) = 3 ! number of arguments of REPA_DF_CHMOD
         REP%DIAGI(J1)%USER_ARG(1,2) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,2) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,2) = LOC(MODES(2))
         MODES(2) = 1
         REP%DIAGI(J1)%USER_CHR(2)   = CHAR(27) ! Esc
!!
         REP%DIAGI(J1)%USER_FUNC(3)  = LOC_EXT(REPA_DF_CHMOD) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,3) = 3 ! number of arguments of REPA_DF_CHMOD
         REP%DIAGI(J1)%USER_ARG(1,3) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,3) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,3) = LOC(MODES(3))
         MODES(3) = 2
         REP%DIAGI(J1)%USER_CHR(3)   = CHAR(231) ! F1
!!
         REP%DIAGI(J1)%USER_FUNC(4)  = LOC_EXT(REPA_DF_CHMOD) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,4) = 3 ! number of arguments of REPA_DF_CHMOD
         REP%DIAGI(J1)%USER_ARG(1,4) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,4) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,4) = LOC(MODES(4))
         MODES(4) = 3
         REP%DIAGI(J1)%USER_CHR(4)   = CHAR(232) ! F2
!!
         REP%DIAGI(J1)%USER_FUNC(5)  = LOC_EXT(REPA_DF_CHMOD) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,5) = 3 ! number of arguments of REPA_DF_CHMOD
         REP%DIAGI(J1)%USER_ARG(1,5) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,5) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,5) = LOC(MODES(5))
         MODES(5) = 4
         REP%DIAGI(J1)%USER_CHR(5)   = CHAR(233) ! F3
!!
         REP%DIAGI(J1)%USER_FUNC(6)  = LOC_EXT(REPA_DF_SETBOX) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,6) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,6) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,6) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,6) = LOC(MODES(6))
         MODES(6) = 1
         REP%DIAGI(J1)%USER_CHR(6)   = CHAR(13)
!!
         REP%DIAGI(J1)%USER_FUNC(7)  = LOC_EXT(REPA_DF_QUIT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,7) = 3 ! number of arguments of REPA_DF_QUIT
         REP%DIAGI(J1)%USER_ARG(1,7) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,7) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,7) = LOC(MODES(7))
         MODES(7) = DIAGI__CONT
         REP%DIAGI(J1)%USER_CHR(7)   = 'X'
!!
         REP%DIAGI(J1)%USER_FUNC(8)  = LOC_EXT(REPA_DF_QUIT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,8) = 3 ! number of arguments of REPA_DF_QUIT
         REP%DIAGI(J1)%USER_ARG(1,8) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,8) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,8) = LOC(MODES(8))
         MODES(8) = DIAGI__CONT
         REP%DIAGI(J1)%USER_CHR(8)   = 'x'
!!
         REP%DIAGI(J1)%USER_FUNC(9)  = LOC_EXT(REPA_DF_QUIT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,9) = 3 ! number of arguments of REPA_DF_QUIIT
         REP%DIAGI(J1)%USER_ARG(1,9) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,9) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,9) = LOC(MODES(9))
         MODES(9) = DIAGI__CONT
         REP%DIAGI(J1)%USER_CHR(9)   = 'Q'
!!
         REP%DIAGI(J1)%USER_FUNC(10)  = LOC_EXT(REPA_DF_QUIT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,10) = 3 ! number of arguments of REPA_DF_QUIT
         REP%DIAGI(J1)%USER_ARG(1,10) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,10) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,10) = LOC(MODES(10))
         MODES(10) = DIAGI__CONT
         REP%DIAGI(J1)%USER_CHR(10)   = 'q'
!!
         REP%DIAGI(J1)%USER_FUNC(11)  = LOC_EXT(REPA_DF_QUIT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,11) = 3 ! number of arguments of REPA_DF_QUIT
         REP%DIAGI(J1)%USER_ARG(1,11) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,11) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,11) = LOC(MODES(11))
         MODES(11) = DIAGI__KEEP
         REP%DIAGI(J1)%USER_CHR(11)   = CHAR(222)
!!
         REP%DIAGI(J1)%USER_FUNC(12)  = LOC_EXT(REPA_DF_QUIT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,12) = 3 ! number of arguments of REPA_DF_QUIT
         REP%DIAGI(J1)%USER_ARG(1,12) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,12) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,12) = LOC(MODES(12))
         MODES(12) = DIAGI__QUIT
         REP%DIAGI(J1)%USER_CHR(12)   = CHAR(200) ! f10
!!
         REP%DIAGI(J1)%USER_FUNC(13)  = LOC_EXT(REPA_DF_INQ) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,13) = 2 ! number of arguments of REPA_DF_INQ
         REP%DIAGI(J1)%USER_ARG(1,13) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,13) = LOC(REP)     
         REP%DIAGI(J1)%USER_CHR(13)   = CHAR(1)
!!
         REP%DIAGI(J1)%USER_FUNC(14)  = LOC_EXT(REPA_DF_SNGAMB) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,14) = 3 ! number of arguments of REPA_DF_SNGAMB
         REP%DIAGI(J1)%USER_ARG(1,14) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,14) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,14) = LOC(MODES(14))
         MODES(14) = -1
         REP%DIAGI(J1)%USER_CHR(14)   = CHAR(0)
!!
         REP%DIAGI(J1)%USER_FUNC(15)  = LOC_EXT(REPA_DF_SNGAMB) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,15) = 3 ! number of arguments of REPA_DF_SNGAMB
         REP%DIAGI(J1)%USER_ARG(1,15) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,15) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,15) = LOC(MODES(15))
         MODES(15) = 1
         REP%DIAGI(J1)%USER_CHR(15)   = CHAR(0)
!!
         REP%DIAGI(J1)%USER_FUNC(16)  = LOC_EXT(REPA_DF_SNGTGL) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,16) = 2 ! number of arguments of REPA_DF_SNGTGL
         REP%DIAGI(J1)%USER_ARG(1,16) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,16) = LOC(REP)     
         REP%DIAGI(J1)%USER_CHR(16)   = CHAR(0)
!!
         REP%DIAGI(J1)%USER_FUNC(17)  = LOC_EXT(REPA_DF_SETBOX) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,17) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,17) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,17) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,17) = LOC(MODES(17))
         MODES(17) = 2
         REP%DIAGI(J1)%USER_CHR(17)   = CHAR(205)
!!
         REP%DIAGI(J1)%USER_FUNC(18)  = LOC_EXT(REPA_DF_SETBOX) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,18) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,18) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,18) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,18) = LOC(MODES(18))
         MODES(18) = 2
         REP%DIAGI(J1)%USER_CHR(18)   = CHAR(237)
!!
         REP%DIAGI(J1)%USER_FUNC(19)  = LOC_EXT(REPA_DF_SETBOX) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,19) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,19) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,19) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,19) = LOC(MODES(19))
         MODES(19) = 3
         REP%DIAGI(J1)%USER_CHR(19)   = 'M'
!!
         REP%DIAGI(J1)%USER_FUNC(20)  = LOC_EXT(REPA_DF_SETBOX) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,20) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,20) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,20) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,20) = LOC(MODES(20))
         MODES(20) = 2
         REP%DIAGI(J1)%USER_CHR(20)   = 'm'
!!
         REP%DIAGI(J1)%USER_FUNC(21)  = LOC_EXT(REPA_DF_GRPAMB) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,21) = 2 ! number of arguments of REPA_DF_GRPAMB
         REP%DIAGI(J1)%USER_ARG(1,21) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,21) = LOC(REP)     
         REP%DIAGI(J1)%USER_CHR(21)   = CHAR(0)
!!
         REP%DIAGI(J1)%USER_FUNC(22)  = LOC_EXT(REPA_DF_GRPTGL) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,22) = 3 ! number of arguments of REPA_DF_GRPTGL
         REP%DIAGI(J1)%USER_ARG(1,22) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,22) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,22) = LOC(MODES(22))
         MODES(22) = -1
         REP%DIAGI(J1)%USER_CHR(22)   = CHAR(0)
!!
         REP%DIAGI(J1)%USER_FUNC(23)  = LOC_EXT(REPA_DF_GRPTGL) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,23) = 3 ! number of arguments of REPA_DF_GRPTGL
         REP%DIAGI(J1)%USER_ARG(1,23) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,23) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,23) = LOC(MODES(23))
         MODES(23) = 1
         REP%DIAGI(J1)%USER_CHR(23)   = CHAR(0)
!!
         REP%DIAGI(J1)%USER_FUNC(24)  = LOC_EXT(REPA_DF_UNDO) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,24) = 2 ! number of arguments of REPA_DF_UNDO
         REP%DIAGI(J1)%USER_ARG(1,24) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,24) = LOC(REP)     
         REP%DIAGI(J1)%USER_CHR(24)   = CHAR(0)
!!
         REP%DIAGI(J1)%USER_FUNC(25)  = LOC_EXT(REPA_DF_CONT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,25) = 0 ! number of arguments of REPA_DF_CONT
         REP%DIAGI(J1)%USER_CHR(25)   = CHAR(0)
!!
         REP%DIAGI(J1)%USER_FUNC(26)  = LOC_EXT(REPA_DF_CONT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,26) = 0 ! number of arguments of REPA_DF_CONT
         REP%DIAGI(J1)%USER_CHR(26)   = CHAR(0)
!!
         REP%DIAGI(J1)%USER_FUNC(27)  = LOC_EXT(REPA_DF_CONT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,27) = 0 ! number of arguments of REPA_DF_CONT
         REP%DIAGI(J1)%USER_CHR(27)   = CHAR(0)
!!
         REP%DIAGI(J1)%USER_FUNC(28)  = LOC_EXT(REPA_DF_FP) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,28) = 3 ! number of arguments of REPA_DF_FP
         REP%DIAGI(J1)%USER_ARG(1,28) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,28) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,28) = LOC(MODES(28))
         MODES(28) = 1
         REP%DIAGI(J1)%USER_CHR(28)   = CHAR(0) ! CHAR(24)
!!
         REP%DIAGI(J1)%USER_FUNC(29)  = LOC_EXT(REPA_DF_FP) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,29) = 3 ! number of arguments of REPA_DF_FP
         REP%DIAGI(J1)%USER_ARG(1,29) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,29) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,29) = LOC(MODES(29))
         MODES(29) = 2
         REP%DIAGI(J1)%USER_CHR(29)   = CHAR(0) ! CHAR(216)
!!
         REP%DIAGI(J1)%USER_FUNC(30)  = LOC_EXT(REPA_DF_QUIT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,30) = 3 ! number of arguments of REPA_DF_QUIT
         REP%DIAGI(J1)%USER_ARG(1,30) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,30) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,30) = LOC(MODES(30))
         MODES(30) = DIAGI__CONT
         REP%DIAGI(J1)%USER_CHR(30)   = DIAGI__PGDN 
!!
         REP%DIAGI(J1)%USER_FUNC(31)  = LOC_EXT(REPA_DF_QUIT) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,31) = 3 ! number of arguments of REPA_DF_QUIT
         REP%DIAGI(J1)%USER_ARG(1,31) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,31) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,31) = LOC(MODES(31))
         MODES(31) = DIAGI__CONT
         REP%DIAGI(J1)%USER_CHR(31)   = DIAGI__PGUP 
!!
         REP%DIAGI(J1)%USER_FUNC(32)  = LOC_EXT(REPA_DF_SETBOX) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,32) = 3 ! number of arguments of REPA_DF_GOODBOX
         REP%DIAGI(J1)%USER_ARG(1,32) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,32) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,32) = LOC(MODES(32))
         MODES(32) = 4
         REP%DIAGI(J1)%USER_CHR(32)   = CHAR(14) ! Cntrl/N
!!
         REP%DIAGI(J1)%USER_FUNC(33)  = LOC_EXT(REPA_DF_SETBOX) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,33) = 3 ! number of arguments of REPA_DF_GOODBOX
         REP%DIAGI(J1)%USER_ARG(1,33) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,33) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,33) = LOC(MODES(33))
         MODES(33) = 5
         REP%DIAGI(J1)%USER_CHR(33)   = CHAR(238) ! Alt/Shft/N
!!
         REP%DIAGI(J1)%USER_FUNC(34)  = LOC_EXT(REPA_DF_SETBOX) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,34) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,34) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,34) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,34) = LOC(MODES(34))
         MODES(34) = 5
         REP%DIAGI(J1)%USER_CHR(34)   = CHAR(206) ! Alt/N
!!
         REP%DIAGI(J1)%USER_FUNC(35)  = LOC_EXT(REPA_DF_FRIPLO) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,35) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,35) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,35) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,35) = LOC(MODES(35))
         MODES(35) = 1
         REP%DIAGI(J1)%USER_CHR(35)   = CHAR(193) ! Alt/<Left_Mouse>
!!
         REP%DIAGI(J1)%USER_FUNC(36)  = LOC_EXT(REPA_DF_FRIPLO) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,36) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,36) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,36) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,36) = LOC(MODES(36))
         MODES(36) = 2
         REP%DIAGI(J1)%USER_CHR(36)   = CHAR(196) ! Alt/<Central_Mouse>
!!
         REP%DIAGI(J1)%USER_FUNC(37)  = LOC_EXT(REPA_DF_FRIPLO) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,37) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,37) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,37) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,37) = LOC(MODES(37))
         MODES(37) = 3
         REP%DIAGI(J1)%USER_CHR(37)   = CHAR(216) ! Alt/<Right_Mouse>
!!
         REP%DIAGI(J1)%USER_FUNC(38)  = LOC_EXT(REPA_DF_FRIPLO) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,38) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,38) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,38) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,38) = LOC(MODES(38))
         MODES(38) = 4
         REP%DIAGI(J1)%USER_CHR(38)   = CHAR(225) ! Shift/Alt/<Left_Mouse>
!!
         REP%DIAGI(J1)%USER_FUNC(39)  = LOC_EXT(REPA_DF_FRIPLO) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,39) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,39) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,39) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,39) = LOC(MODES(39))
         MODES(39) = 5
         REP%DIAGI(J1)%USER_CHR(39)   = CHAR(228) ! Shift/Alt/<Central_Mouse>
!!
         REP%DIAGI(J1)%USER_FUNC(40)  = LOC_EXT(REPA_DF_FRIPLO) ! Address of the entry point
         REP%DIAGI(J1)%USER_ARG(0,40) = 3 ! number of arguments of REPA_DF_SETBOX
         REP%DIAGI(J1)%USER_ARG(1,40) = LOC(REP%DIAGI(J1)) 
         REP%DIAGI(J1)%USER_ARG(2,40) = LOC(REP)     
         REP%DIAGI(J1)%USER_ARG(3,40) = LOC(MODES(40))
         MODES(40) = 6
         REP%DIAGI(J1)%USER_CHR(40)   = CHAR(248) ! Shift/Alt/<Right_Mouse>
 410  CONTINUE 
!
! --- Set the common title of the plot
!
      CALL CLRCH ( REP%TITLE )
      REP%TITLE = REP%DBNAME_STR(1:I_LEN(REP%DBNAME_STR))//' '// &
     &    REP__CH_VAL(REP%CNF%VAL_IND)(1:I_LEN(REP__CH_VAL(REP%CNF%VAL_IND)))
      IF ( REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'Delay   ' .OR. &
     &     REP__CH_VAL(REP%CNF%VAL_IND) .EQ. 'Rate    '      ) THEN
           REP%TITLE = REP%TITLE(1:I_LEN(REP%TITLE))//' '// &
     &                 DATYP__ABR(1+REP%DATYP_I2*6:(REP%DATYP_I2+1)*6)//' '// &
     &                 VAL_UNITS
         ELSE 
           REP%TITLE = REP%TITLE(1:I_LEN(REP%TITLE))//' '//VAL_UNITS
      END IF
!
! --- Initialize command stack
!
      REP%N_COM  = 0
!
      REP%STATUS = REPA__PLOT
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  REPA_SETDIAGI 
