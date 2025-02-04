      SUBROUTINE VTD_SET_POSVAR ( VTD, I_PSV, TIM_BEG_PSV, TIM_PSV, &
     &                            VAL_PSV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_SET_POSVAR  computes array of site position           *
! *   variations according to the I_PSV -th model specified in the       *
! *   controlfile for the time range around the nominal start and        *
! *   nominal stop  of the observing session. It returns the time epochs *
! *   of the displacements and 3-D displacement for all stations         *
! *   participated in the session.                                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *           VTD ( RECORD    ) -- the record which keeps variables      *
! *                                relevant to VTD                       *
! *         I_PSV ( INTEGER*4 ) -- Index of the position variation file  *
! *                                in the array POSVAR_FIL defined in    *
! *                                VTD.                                  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   TIM_BEG_PSV ( REAL*8    ) -- Date in TDB time scale in seconds     *
! *                                elapsed from the J2000.0 of the       *
! *                                first time epoch of the output array. *
! *                                It is set to the nominal session      *
! *                                start minus some amount of the        *
! *                                overhead time defined in OVH__PSV     *
! *                                variable from vtd.i                   *
! *       TIM_PSV ( REAL*8    ) -- Array of time epochs for the site     *
! *                                position variations as the time       *
! *                                elapsed from TIM_BEG_PSV.             *
! *                                Dimension: VTD__M_SDI.                *
! *                                Units: seconds.                       *
! *       VAL_PSV ( REAL*8    ) -- Three-dimensional array of position   *
! *                                variations of all stations            *
! *                                participated in this session          *
! *                                according to the I_PSV -th model of   *
! *                                site position variations defined in   *
! *                                the batch control file. Dimensions:   *
! *                                (VTD__M_SDI,3,VTD__M_STA).            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 17-DEC-2002  VTD_SET_POSVAR v2.0 (c) L. Petrov  23-JUN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  I_PSV, IUER
      REAL*8     TIM_BEG_PSV, TIM_PSV(VTD__M_SDI), &
     &           VAL_PSV(VTD__M_SDI,3,VTD__M_STA)
      REAL*8     STA_TRS_COO(3,VTD__M_STA)
      CHARACTER  STA_NAM(VTD__M_STA)*8, FILSUM*128, STR*64, STR1*64
      INTEGER*4  IS, J1, STAT_BLOCK(16), IER
      INTEGER*4, EXTERNAL :: I_LEN, FOR_STAT
!
      IF ( I_PSV .LT. 0  .OR.  I_PSV .GT. VTD__M_SDI ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I_PSV, STR )
           CALL ERR_LOG ( 2811, IUER, 'VTD_SET_POSVAR', 'Wrong value of '// &
     &         'parameter I_PSV: '//STR )
           RETURN
      END IF
!
      IF ( VTD%L_STA .GT. VTD__M_STA ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( VTD%L_STA, STR )
           CALL INCH  ( VTD__M_STA, STR1 )
           CALL ERR_LOG ( 2812, IUER, 'VTD_SET_POSVAR', 'Error of internal '// &
     &         'control: parameter VTD%L_STA: '//STR(1:I_LEN(STR))// &
     &         ' is too large: larger than the constant VTD__M_STA '// &
     &          STR1(1:I_LEN(STR1))//' defined in vtd.i' ) 
           RETURN 
      END IF
!
      DO 410 J1=1,VTD%L_STA
         STA_NAM(J1) = VTD%STA(J1)%IVS_NAME 
         CALL COPY_R8 ( 3, VTD%STA(J1)%COO_TRS(1,1), STA_TRS_COO(1,J1) )
 410  CONTINUE 
      IF ( VTD%CONF%POSVAR_MOD(I_PSV) .EQ. PSV__HMD ) THEN
!
! -------- Compute displacements of all sites in this sessions using the
! -------- model of harmonic displacements. Displacements will be computed at
! -------- the VTD__M_SDI epochs equally distribited over the range which is
! -------- of the slightly wider than the nominal time interval of the session.
!
           CALL ERR_PASS ( IUER, IER )
#ifdef GNU
           CALL VTD_SET_HARPOS ( VTD%MJD_BEG, VTD%TAI_BEG,              &
     &                           VTD%MJD_END, VTD%TAI_END,              &
     &                           VTD%L_STA, STA_NAM, STA_TRS_COO,       &
     &                           VTD%POSVAR(I_PSV)%RD_AREA,             &
     &                           VTD%POSVAR(I_PSV)%N_PSVHAR,            &
     &                           VTD%POSVAR(I_PSV)%N_PSVSTA,            &
     &                           %VAL(VTD%POSVAR(I_PSV)%ADR_NAMSIT),    &
     &                           %VAL(VTD%POSVAR(I_PSV)%ADR_STACOO),    &
     &                           %VAL(VTD%POSVAR(I_PSV)%ADR_HARVAL),    &
     &                           %VAL(VTD%POSVAR(I_PSV)%ADR_HARDSP), 8, &
     &                           VTD%CONF%POSVAR_USE(I_PSV),            &
     &                           LEN(VTD%CONF%POSVAR_FIL(I_PSV)),       &
     &                           VTD%CONF%POSVAR_FIL(I_PSV),            &
     &                           TIM_BEG_PSV, TIM_PSV, VAL_PSV,         &
     &                           VTD%CONF%FL_WARN, IER,                 &
     &                           %VAL(8), %VAL(8),                      &
     &                           %VAL(LEN(VTD%CONF%POSVAR_FIL(I_PSV))) )
#else
           CALL VTD_SET_HARPOS ( VTD%MJD_BEG, VTD%TAI_BEG,              &
     &                           VTD%MJD_END, VTD%TAI_END,              &
     &                           VTD%L_STA, STA_NAM, STA_TRS_COO,       &
     &                           VTD%POSVAR(I_PSV)%RD_AREA,             &
     &                           VTD%POSVAR(I_PSV)%N_PSVHAR,            &
     &                           VTD%POSVAR(I_PSV)%N_PSVSTA,            &
     &                           %VAL(VTD%POSVAR(I_PSV)%ADR_NAMSIT),    &
     &                           %VAL(VTD%POSVAR(I_PSV)%ADR_STACOO),    &
     &                           %VAL(VTD%POSVAR(I_PSV)%ADR_HARVAL),    &
     &                           %VAL(VTD%POSVAR(I_PSV)%ADR_HARDSP), 8, &
     &                           VTD%CONF%POSVAR_USE(I_PSV),            &
     &                           LEN(VTD%CONF%POSVAR_FIL(I_PSV)),       &
     &                           VTD%CONF%POSVAR_FIL(I_PSV),            &
     &                           TIM_BEG_PSV, TIM_PSV, VAL_PSV,         &
     &                           VTD%CONF%FL_WARN, IER )
#endif
         ELSE IF ( VTD%CONF%POSVAR_MOD(I_PSV) .EQ. PSV__TSR ) THEN
!
! -------- Build the name of the summary file
!
           FILSUM = VTD%CONF%POSVAR_FIL(I_PSV)(1:I_LEN(VTD%CONF%POSVAR_FIL(I_PSV)))// &
     &              '/'//SUMMARY_BDS_FILE
!
! -------- Learn information about the summary file, including date of last
! -------- modification
!
           IS = FOR_STAT ( FILSUM, STAT_BLOCK )
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 2813, IUER, 'VTD_SET_POSVAR', 'Error '// &
     &               STR(1:I_LEN(STR))//' in STAT of the file '//FILSUM )
                RETURN
           END IF
!
! -------- Check whether the summary file has been modifed since the time it
! -------- was read the first time?
!
           IF ( STAT_BLOCK(10) .GT. VTD%POSVAR(I_PSV)%TIM_PSVFIL ) THEN
!
! ------------- We learned that it was changed. Then we free dymanic memory
! ------------- allocated for the data structures used for keeping information
! ------------- from the summary file
!
                CALL FREE ( VTD%POSVAR(I_PSV)%MEM_ADR )
!
! ------------- And then re-load the summary file once again.
!
                CALL ERR_PASS ( IUER, IER )
                CALL VTD_LOAD_BINDISP ( VTD, I_PSV, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 2814, IUER, 'VTD_SET_POSVAR', 'Error '// &
     &                   'in an attempt to re-read and parse summary file '// &
     &                    FILSUM )
                     RETURN
                END IF
           END IF
!
! -------- Compute displacements of all sites in this sessions using the
! -------- time series of displacements. Displacements will be computed at
! -------- the VTD__M_SDI epochs equally distribited over the range which is
! -------- of the slightly wider than the nominal time interval of the session.
! -------- Displacements are computed at this grid using either linear or
! -------- spline interpolation depending on  POSVAR_INT(I_PSV)
!
           CALL ERR_PASS ( IUER, IER )
#ifdef GNU
           CALL VTD_SET_BINDISP ( &
     &          VTD%MJD_BEG, VTD%TAI_BEG, VTD%MJD_END, VTD%TAI_END, &
     &          VTD%L_STA, STA_NAM, STA_TRS_COO,                    &
     &          VTD%POSVAR(I_PSV)%RD_AREA,                          &
     &          VTD%POSVAR(I_PSV)%N_PSVSTA,                         &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_NAMSIT),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_STACOO),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSFIL),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSSAM),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSFMJ),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSFSC),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSLMJ),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSLSC),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSNSA),                 &
     &          VTD%POSVAR(I_PSV)%BDS_ENDIAN, 8,                    &
     &          128, VTD%CONF%POSVAR_FIL(I_PSV),                    &
     &          VTD%CONF%POSVAR_USE(I_PSV),                         &
     &          VTD%CONF%POSVAR_INT(I_PSV),                         &
     &          TIM_BEG_PSV, TIM_PSV, VAL_PSV,                      &
     &          VTD%CONF%FL_WARN, IER, &
     &          %VAL(8), %VAL(8), %VAL(8), %VAL(8), %VAL(128) )
#else
           CALL VTD_SET_BINDISP ( &
     &          VTD%MJD_BEG, VTD%TAI_BEG, VTD%MJD_END, VTD%TAI_END, &
     &          VTD%L_STA, STA_NAM, STA_TRS_COO,                    &
     &          VTD%POSVAR(I_PSV)%RD_AREA,                          &
     &          VTD%POSVAR(I_PSV)%N_PSVSTA,                         &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_NAMSIT),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_STACOO),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSFIL),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSSAM),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSFMJ),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSFSC),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSLMJ),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSLSC),                 &
     &          %VAL(VTD%POSVAR(I_PSV)%ADR_BDSNSA),                 &
     &          VTD%POSVAR(I_PSV)%BDS_ENDIAN, 8,                    &
     &          128, VTD%CONF%POSVAR_FIL(I_PSV),                    &
     &          VTD%CONF%POSVAR_USE(I_PSV),                         &
     &          VTD%CONF%POSVAR_INT(I_PSV),                         &
     &          TIM_BEG_PSV, TIM_PSV, VAL_PSV,                      &
     &          VTD%CONF%FL_WARN, IER )
#endif
         ELSE IF ( VTD%CONF%POSVAR_MOD(I_PSV) .EQ. PSV__BSP ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_SET_BSPPOS ( VTD%MJD_BEG, VTD%TAI_BEG,              &
     &                           VTD%MJD_END, VTD%TAI_END,              &
     &                           VTD%L_STA, STA_NAM, STA_TRS_COO,       &
     &                           VTD%POSVAR(I_PSV)%RD_AREA,             &
     &                           VTD%POSVAR(I_PSV)%N_PSVSTA,            &
     &                           %VAL(VTD%POSVAR(I_PSV)%ADR_NAMSIT), 8, &
     &                           %VAL(VTD%POSVAR(I_PSV)%ADR_STACOO),    &
     &                           VTD%POSVAR(I_PSV)%BSP,                 &
     &                           VTD%CONF%POSVAR_USE(I_PSV),            &
     &                           LEN(VTD%CONF%POSVAR_FIL(I_PSV)),       &
     &                           VTD%CONF%POSVAR_FIL(I_PSV),            &
     &                           TIM_BEG_PSV, TIM_PSV, VAL_PSV,         &
     &                           VTD%CONF%FL_WARN, IER )
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( I_PSV, STR )
           CALL ERR_LOG ( 2815, IUER, 'VTD_SET_POSVAR', 'Wrong value of '// &
     &         'parameter I_PSV: '//STR )
           RETURN
      END IF
!
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2816, IUER, 'VTD_SET_POSVAR', 'Error in an '// &
     &         'attempt to compute position variations at the nodes of the '// &
     &         'interpolation range' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  VTD_SET_POSVAR  #!#
