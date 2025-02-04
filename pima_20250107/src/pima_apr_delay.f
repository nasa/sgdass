      SUBROUTINE PIMA_APR_DELAY ( PIM, VTD, IND_OBS, IND_BND, MODE_TIM, &
     &                            MODE_REF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_APR_DELAY  computes apriori path delay               *
! *   for observation with index  IND_OBS  at band with index IND_BND.   *
! *                                                                      *
! *  ### 08-JUL-2009  PIMA_APR_DELAY  v3.7 (c) L. Petrov 12-MAY-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      TYPE     ( VTD__TYPE  ) :: VTD
      INTEGER*4  IND_OBS, IND_BND, IUER
      CHARACTER  MODE_TIM*(*), MODE_REF*(*)
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      REAL*8     TIM_ARG, TIM_VAL, TIM_POL, TIM_EPS, TIM_TOL, DEL(2), &
     &           PHS(2), RAT(2), DEL_1ST, TIM_BEG_1ST, TIM_BEG_2ND, &
     &           ATM_DEL_APR(2), TIM_STP, PIMA_VAR_GR_RAT_ERR_SCALE
      CHARACTER  STR*128, STR1*128, STR2*128, ENV_RDV78*32
      INTEGER*4  M__DEG 
      PARAMETER  ( M__DEG = 6 )
      PARAMETER  ( TIM_EPS = 2.D-6 )
      PARAMETER  ( TIM_TOL = 1.D-3 )
      PARAMETER  ( TIM_STP = 8.0D0 )
      REAL*8     TIM_DIF, CLO_OFF_APR(2), CLO_RAT_APR(2)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, J20, FRG_IND, UV_IND, &
     &           IND_STA(2), IND_MOD(2), ISGN, IND_1ST, IND_2ND, &
     &           M_ITR, PIMA_PRAT_IND, NSPL(2), IER
      REAL*8     VAL_APR, VAL_THR, TIM_ARR(PIM__MUV,2), DEL_ARR(PIM__MUV,2), &
     &           DEL_VAL, RAT_VAL, WEI_ARR(PIM__MUV), &
     &           DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), &
     &           DEL_LEG(0:M__DEG,2), ERR_LEG(0:M__DEG), DEL0, TIM_1ST_AP, SOF
      LOGICAL*1  FL_INCLUDE_CLO, FL_CONT(2)
      LOGICAL*1  FL_NO_APR_CLO, FL_NO_APR_CLO_RATE, FL_ONLY_THEO, FL_MKDB_NOSORT
      PARAMETER  ( M_ITR = 3 )
!!      REAL*8    x1(8192), x2(8192) ! %%%%%%%%%
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REAL*8,    EXTERNAL :: LEGENDRE_POL, LEGENDRE_DER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      PIMA_PRAT_IND = 1
      FL_INCLUDE_CLO = .TRUE.  ! default: include clock model
      FL_NO_APR_CLO = .FALSE.
      FL_NO_APR_CLO_RATE = .FALSE.
      PIMA_VAR_GR_RAT_ERR_SCALE = 1.0
!
      CALL GETENVAR ( 'PIMAVAR_APR_RDV78', ENV_RDV78 )
      CALL GETENVAR ( 'PIMAVAR_PRAT_IND', STR   )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, PIMA_PRAT_IND )
      END IF
      CALL GETENVAR ( 'PIMAVAR_NO_APR_CLO', STR   )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_NO_APR_CLO = .TRUE.
      END IF
      CALL GETENVAR ( 'PIMAVAR_NO_APR_CLO_RATE', STR   )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_NO_APR_CLO_RATE = .TRUE.
      END IF
      CALL GETENVAR ( 'PIMAVAR_NO_APR_CLO_INCLUDE', STR   )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_INCLUDE_CLO = .FALSE.
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_GR_RAT_ERR_SCALE', STR   )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F12.5)' ) PIMA_VAR_GR_RAT_ERR_SCALE
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_TOTAL_ONLY_THEO', STR   )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_ONLY_THEO = .TRUE.
         ELSE 
           FL_ONLY_THEO = .FALSE.
      END IF
!
      FL_MKDB_NOSORT = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_MKDB_NOSORT', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR(1:3) == 'YES' ) FL_MKDB_NOSORT = .TRUE.
!
! --- Initialization
!
      PIM%OBS(IND_OBS)%TOT_MB_DEL(1:PIM__MFRA,IND_BND) = -1.0
      PIM%OBS(IND_OBS)%TOT_PH_RAT(1:PIM__MFRA,IND_BND) = -1.0
      PIM%OBS(IND_OBS)%TOT_PHS(1:PIM__MFRA,IND_BND)    = -1.0
      PIM%OBS(IND_OBS)%TOT_PHS_GC(1:PIM__MFRA,IND_BND) = -1.0
      PIM%OBS(IND_OBS)%TOT_SB_DEL(IND_BND) = -1.0
      PIM%OBS(IND_OBS)%TOT_GR_RAT(IND_BND) = -1.0
      PIM%OBS(IND_OBS)%APR_PHS(IND_BND)    = -1.0
      PIM%OBS(IND_OBS)%APR_RAT(IND_BND)    = -1.0
      PIM%OBS(IND_OBS)%APR_GR_DEL(IND_BND) = -1.0
      PIM%OBS(IND_OBS)%CLO_OFFSET_APR(1,IND_BND) = 0.0D0
      PIM%OBS(IND_OBS)%CLO_OFFSET_APR(2,IND_BND) = 0.0D0
      PIM%OBS(IND_OBS)%CLO_RATE_APR(1,IND_BND)   = 0.0D0
      PIM%OBS(IND_OBS)%CLO_RATE_APR(2,IND_BND)   = 0.0D0
!
      IF ( PIM%CONF%FRG_USE == PIMA__COMBINE ) THEN
!
! -------- If the frequency group is combined, we search for the first 
! -------- frequency group that is not empty, i.e. has accumulation
! -------- periods
!
           DO 4200 J20=PIM%CONF%FRG_LIST(1),PIM%CONF%FRG_LIST(2)
              FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(J20)
              IF ( FRG_IND == 0 ) GOTO 4200
              UV_IND = PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND)
              IF ( UV_IND == 0 ) GOTO 4200
              GOTO 8200
 4200      CONTINUE 
 8200      CONTINUE 
         ELSE
!
! -------- Normal case: the frequency group index is fixed
!
           FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
      END IF
      IF ( FRG_IND == 0 ) THEN
!
! -------- FRG_IND is a pathological case: ther are no data in the 
! -------- current frequency group. In order to prevent a crash, let
! -------- us set default frequency group (1)
!
           FRG_IND = 1
      END IF
!
! --- Get the time argument
!
      IF ( PIM%NFRG == 1 ) THEN
           TIM_1ST_AP = PIM%TIM_R8(PIM%OBS(IND_OBS)%TIM_BEG_IND)
         ELSE
           TIM_1ST_AP = PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND )
      END IF
!
      IF ( MODE_TIM == 'OBS_BEG' ) THEN
!
! -------- Beginning of the scan
!
           TIM_ARG = TIM_1ST_AP
         ELSE IF ( MODE_TIM == 'OBS_SRT' ) THEN
!
! -------- Scan Reference Time
!
           IF ( DABS(PIM%OBS(IND_OBS)%SRT_OFFSET) < PIM__MSCL ) THEN
!
! ------------- i.e. SRT_OFFSET is within a maximum scan length
!
                TIM_ARG = TIM_1ST_AP + PIM%OBS(IND_OBS)%SRT_OFFSET
              ELSE
!
! ------------- This should not normally occur
!
                TIM_ARG = TIM_1ST_AP
           END IF
         ELSE
           CALL ERR_LOG ( 7782, IUER, 'PIMA_APR_DELAY', 'Unknown time mode '// &
     &          MODE_TIM(1:I_LEN(MODE_TIM))//' -- one of OBS_BEG or '// &
     &         'OBS_SRT were expected' )
           RETURN
      END IF
!
! --- SOF -- offset of scan reference time with respect to the fringe refrence time
!
      SOF= ( TIM_1ST_AP                + PIM%OBS(IND_OBS)%SRT_OFFSET )- &
     &     ( PIM%OBS(IND_OBS)%TIM_BEG  + PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND) )
!
! --- Compute apriori path delay on the specified moment of time
!
      IND_STA = 0 
      IND_MOD = 0
      DO 410 J1=1,2
         IND_STA(J1) = PIM%OBS(IND_OBS)%STA_IND(J1)
         IND_MOD(J1) = 0
         FL_CONT(J1) = .TRUE.
         IF ( PIM%OBS(IND_OBS)%MOD_IND_BEG(J1) > 0 .AND. &
     &        PIM%OBS(IND_OBS)%MOD_IND_END(J1) > 0       ) THEN
!
! ----------- Search for the index of the array with polynomial coefficients
!
              DO 420 J2=PIM%OBS(IND_OBS)%MOD_IND_BEG(J1),PIM%OBS(IND_OBS)%MOD_IND_END(J1)
                 IF ( J2 > PIM%OBS(IND_OBS)%MOD_IND_BEG(J1) ) THEN
                      IF ( ( PIM%STA(IND_STA(J1))%MOD(J2)%TIM_BEG -  &
     &                       PIM%STA(IND_STA(J1))%MOD(J2-1)%TIM_END  ) > TIM_TOL ) THEN
!
! ------------------------ There is an interval of time when the interferometric model is 
! ------------------------ not defined. Mark this scan as "not continuos"
!
                           FL_CONT(J1) = .FALSE.
                      END IF
                 END IF
                 IF ( TIM_ARG > PIM%STA(IND_STA(J1))%MOD(J2)%TIM_BEG - TIM_EPS .AND. &
     &                TIM_ARG < PIM%STA(IND_STA(J1))%MOD(J2)%TIM_END + TIM_EPS       ) THEN
                      IND_MOD(J1) = J2
                      GOTO 820
                 END IF
 420          CONTINUE
 820          CONTINUE
         END IF
!
         IF ( IND_MOD(J1) == 0 .AND. FL_CONT(J1)                                                      .AND. &
     &        .NOT. ( TIM_ARG .GE. &
     &                    PIM%STA(IND_STA(J1))%MOD(PIM%OBS(IND_OBS)%MOD_IND_BEG(J1))%TIM_BEG          .AND. &
     &                TIM_ARG .LE. PIM%STA(IND_STA(J1))%MOD(PIM%OBS(IND_OBS)%MOD_IND_END(J1))%TIM_END     ) &
     &        ) THEN
!
! ----------- If we get to this point, we did not find the index of the
! ----------- block with polynomial coefficients.  
!
! ----------- Let us prepare the error message
!
              CALL CLRCH ( STR )
              CALL CLRCH ( STR1 )
              CALL INCH  ( IND_OBS, STR )
              STR2 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%OBS(IND_OBS)%TIM_BEG + TIM_ARG, -2 )
              WRITE ( UNIT=STR1, FMT='(F14.8)', IOSTAT=IER ) TIM_ARG
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 10 ) THEN
                   DO 430 J3=1,PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP))
                      WRITE ( 6, '("Epc: ", I5, "  UV_IND: ", I9)' ) J3, &
     &                        PIM%OBS(IND_OBS)%UV_IND(J3,1)
 430               CONTINUE
!
                   WRITE ( 6, * ) 'TIM_ARG     = ', TIM_ARG
                   WRITE ( 6, * ) 'TIM_MOD_BEG = ', PIM%STA(IND_STA(J1))%MOD(PIM%OBS(IND_OBS)%MOD_IND_BEG(J1))%TIM_BEG
                   WRITE ( 6, * ) 'TIM_MOD_END = ', PIM%STA(IND_STA(J1))%MOD(PIM%OBS(IND_OBS)%MOD_IND_END(J1))%TIM_END
                   WRITE ( 6, * ) INT2(J1), ' staion ', PIM%C_STA(IND_STA(J1)),  &
     &                            ' IND_BND: ', INT2(IND_BND),      &
     &                            ' MOD_IND_BEG/END: ',             &
     &                            PIM%OBS(IND_OBS)%MOD_IND_BEG(J1), &
     &                            PIM%OBS(IND_OBS)%MOD_IND_END(J1), &
     &                            ' SRT_OFFSET: ', PIM%OBS(IND_OBS)%SRT_OFFSET, &
     &                            ' FRT_OFFSET: ', PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND)
                   DO 440 J4=PIM%OBS(IND_OBS)%MOD_IND_BEG(J1),PIM%OBS(IND_OBS)%MOD_IND_END(J1)
                      STR  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                   PIM%STA(IND_STA(J1))%MOD(J4)%TIM_BEG, -2 )
                      STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                   PIM%STA(IND_STA(J1))%MOD(J4)%TIM_END, -2 )
                      WRITE ( 6, * ) 'OBS: ', INT2(IND_OBS), &
     &                               ' J1 = ',INT2(J1), ' J4= ', INT2(J4), &
     &                               ' STA: ', PIM%C_STA(IND_STA(J1))
                      WRITE ( 6, * ) ' TB = ', PIM%STA(IND_STA(J1))%MOD(J4)%TIM_BEG, &
     &                                         STR(1:22)
                      WRITE ( 6, * ) ' TE = ', PIM%STA(IND_STA(J1))%MOD(J4)%TIM_END, &
     &                                         STR1(1:22)
 440              CONTINUE
                  WRITE ( 6, '(" Fringe status: ", B16)' ) PIM%OBS(IND_OBS)%FRI_STS(IND_BND)
              END IF
!
              IF ( IBSET ( PIM%OBS(IND_OBS)%FRI_STS(IND_BND), FAI__PIM ) == 1 ) THEN
!
! ---------------- There was a failure in fringing process for this observation.
! ---------------- Let us through this observation away and not worry about
! ---------------- any more
!
                   WRITE ( 6, * ) 'Cannot find apriori polynomial model for' // &
     &                            ' observation '//STR(1:I_LEN(STR))// &
     &                            ' epoch '//STR1(1:I_LEN(STR1))// &
     &                            ' station '//PIM%C_STA(IND_STA(J1))// &
     &                            ' moment: '//STR2(1:23)
!
                   PIM%OBS(IND_OBS)%APR_PHS(IND_BND)          = 0.0D0
                   PIM%OBS(IND_OBS)%APR_GR_DEL(IND_BND)       = 0.0D0
                   PIM%OBS(IND_OBS)%APR_RAT(IND_BND)          = 0.0D0
                   PIM%OBS(IND_OBS)%CLO_OFFSET_APR(1,IND_BND) = 0.0D0
                   PIM%OBS(IND_OBS)%CLO_OFFSET_APR(2,IND_BND) = 0.0D0
                   PIM%OBS(IND_OBS)%CLO_RATE_APR(1,IND_BND)   = 0.0D0
                   PIM%OBS(IND_OBS)%CLO_RATE_APR(2,IND_BND)   = 0.0D0
                   PIM%OBS(IND_OBS)%TOT_SB_DEL(IND_BND) = 0.0D0
                   PIM%OBS(IND_OBS)%TOT_GR_RAT(IND_BND) = 0.0D0
!
                   PIM%OBS(IND_OBS)%TOT_MB_DEL(1:PIM__MFRA,IND_BND) = 0.0D0
                   PIM%OBS(IND_OBS)%TOT_PH_RAT(1:PIM__MFRA,IND_BND) = 0.0D0
!
                   PIM%OBS(IND_OBS)%TOT_PHS(1:PIM__MFRA,IND_BND)    = 0.0D0
                   PIM%OBS(IND_OBS)%TOT_PHS_GC(1:PIM__MFRA,IND_BND) = 0.0D0
!
                   CALL ERR_LOG ( 0, IUER )
                   RETURN
                 ELSE
                   CALL CLRCH   ( STR )
                   CALL INCH    ( IND_OBS, STR )
                   WRITE ( 6, * ) 'TIM_ARG= ', TIM_ARG
                   CALL ERR_LOG ( 7784, IUER, 'PIMA_APR_DELAY', 'Cannot find apriori '// &
     &                 'model polynomial for observation '//STR(1:I_LEN(STR))// &
     &                 ' epoch '//STR1(1:I_LEN(STR1))//' station '// &
     &                  PIM%C_STA(IND_STA(J1))//'  moment: '//STR2 )
                   RETURN
              END IF
         END IF
!
         IF ( .NOT. FL_CONT(J1) ) THEN
!
! ----------- Case of the interferometric model that is not defined in 
! ----------- a continuous interval within a scan.
! ----------- To handle this case, we compute theoretical path delay using 
! ----------- VTD at a grid where the interferometriv is defined.
! ----------- We form a diffrences Calc minus VTD and compute coefficients
! ----------- of the Legendre polynomials that fit these differences.
! ----------- 
! ----------- Why? Actually, the goal is extrapolation or interpolation.
! ----------- The accuracy of original Calc polynomlas is not sufficient
! ----------- for precise extra- or interpolation. However, the differences
! ----------- VTD-Calc are much slower variating function. Therefore,
! ----------- we reduce the errors of extra- or interpolation if we 
! ----------- first compute path delay at the time epoch of the interest
! ----------- with VTD and then subtract the extra- interpolated value
! ----------- VTD - Calc
!
              OBS_TYP%PLRZ       = 'RR'     
              OBS_TYP%FRQ_REF(1) = PIM%REF_FREQ
              OBS_TYP%N_BND      = 1
              OBS_TYP%DELAY_TYPE = VTD__ML__DTP
              OBS_TYP%FRQ_ION_EFF(1) = PIM%REF_FREQ
              OBS_TYP%STATUS      = VTD__BND 
              OBS_TYP%DELAY_TYPE  = VTD__PL__DTP
!
! ----------- First define the grid within a scan with step TIM_STP that 
! ----------- has interferometric model and compute the interferometric
! ----------- model using Calc polynomials
!
              NSPL(J1) = 0 
              DO 450 J5=1,PIM__MUV
                 TIM_VAL = PIM%STA(IND_STA(J1))%MOD(PIM%OBS(IND_OBS)%MOD_IND_BEG(J1))%TIM_BEG + &
     &                    (J5-1)*TIM_STP
                 IND_MOD(J1) = 0
!
! -------------- Check, whether there is an interferometric model for TIM_VAL
!
                 DO 460 J6=PIM%OBS(IND_OBS)%MOD_IND_BEG(J1),PIM%OBS(IND_OBS)%MOD_IND_END(J1)
                    IF ( TIM_VAL > PIM%STA(IND_STA(J1))%MOD(J6)%TIM_BEG - TIM_EPS .AND. &
     &                   TIM_VAL < PIM%STA(IND_STA(J1))%MOD(J6)%TIM_END + TIM_EPS       ) THEN
!
! ---------------------- Very well: TIM_VAL is within the IND_MOD(J1) interval of
! ---------------------- the interferometric model
!
                         IND_MOD(J1) = J6
                         GOTO 860
                    END IF
 460             CONTINUE
 860             CONTINUE 
                 IF ( IND_MOD(J1) > 0 ) THEN
!
! ------------------- Since the interferometric model is defined for this interval,
! ------------------- let us compute the value of path delay 
!
                      TIM_POL = TIM_VAL - PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%TIM_BEG
                      VAL_APR = PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%GDEL_POL(PIM__MDPL,1)
                      DO 470 J7=PIM__MDPL-1,0,-1
                         VAL_APR = PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%GDEL_POL(J7,1) + &
     &                             VAL_APR*TIM_POL
 470                  CONTINUE
!
! ------------------- Augment array TIM_ARR/DEL_ARR
!
                      NSPL(J1) = NSPL(J1) + 1
                      TIM_ARR(NSPL(J1),J1) = (J5-1)*TIM_STP
                      DEL_ARR(NSPL(J1),J1) = VAL_APR
                 END IF
 450          CONTINUE
!
! ----------- Now compute path delay using VTD and put in 
! ----------- DEL_ARR the differences VTD - Calc
!
              DO 480 J8=1,NSPL(J1)
                 CALL ERR_PASS ( IUER, IER )
                 CALL VTD_DELAY ( PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND),    &
     &                'GEOCENTR',  PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1)), &
     &                PIM%MJD_0, PIM%TAI_0 + PIM%STA(IND_STA(J1))%MOD(PIM%OBS(IND_OBS)%MOD_IND_BEG(J1))%TIM_BEG + &
     &                           TIM_ARR(J8,J1), &
     &                OBS_TYP, VTD, DEL_VAL, RAT_VAL, &
     &                DER_DEL, DER_RAT, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_OBS, STR )
                      CALL ERR_LOG ( 7785, IUER, 'PIMA_APR_DELAY', 'Error in '// &
     &                    'an attempt to compute theoretical path delay for '// &
     &                    'observation '//STR )
                      RETURN 
                 END IF
                 DEL_ARR(J8,J1) = DEL_ARR(J8,J1) - DEL_VAL
                 WEI_ARR(J8) = 1.0D0
 480         CONTINUE 
!
! ---------- Finally, we compute coefficients of Legendre polynomials that
! ---------- fit the differences
!
             CALL NOUT_R8 ( M__DEG+1, DEL_LEG(0:M__DEG,J1)  )
             CALL ERR_PASS ( IUER, IER )
             CALL LEGENDRE_REGR ( NSPL(J1), TIM_ARR(1,J1), DEL_ARR(1,J1), &
     &                            WEI_ARR, MIN(NSPL(J1)-1, M__DEG), DEL_LEG(0,J1), ERR_LEG, IER )
             IF ( IER .NE. 0 ) THEN
                  write  ( 6, * ) ' nspl= ', nspl(j1), ' m__deg= ', m__deg ! %%%%
                  CALL CLRCH ( STR )
                  CALL INCH  ( IND_OBS, STR )
                  CALL ERR_LOG ( 7786, IUER, 'PIMA_APR_DELAY', 'Error in '// &
     &                'an attempt to compute coefficients of Legendre '// &
     &                'polynomials for differences VTD - Calc for '// &
     &                'observation '//STR )
                  RETURN 
             END IF
         END IF
!
! ------ Continue normal workflow
!
         IF ( PIM%STA(IND_STA(J1))%L_MDC > 0 ) THEN
              IF ( FL_NO_APR_CLO_RATE ) THEN
                   PIM%STA(IND_STA(J1))%MDC%CLOCK_RATE = 0.0
              END IF
              IF ( FL_NO_APR_CLO ) THEN
                   PIM%STA(IND_STA(J1))%MDC%CLOCK_OFFSET = 0.0
                   PIM%STA(IND_STA(J1))%MDC%CLOCK_RATE = 0.0
              END IF
              TIM_DIF = 1.0D8
!
! ----------- Compute clock function and atmosphere path delay added by Calc
!
              DO 4110 J11=1,PIM%STA(IND_STA(J1))%L_MDC
                 IF ( DABS ( PIM%STA(IND_STA(J1))%MDC%TIME_CEN(J11) - TIM_ARG ) < TIM_DIF ) THEN
                      TIM_DIF = DABS ( TIM_ARG - PIM%STA(IND_STA(J1))%MDC%TIME_CEN(J11) )
                      ATM_DEL_APR(J1) = PIM%STA(IND_STA(J1))%MDC%ATMO_DELAY(J11)*20.0D0 ! ????
!
                      CLO_OFF_APR(J1) = PIM%STA(IND_STA(J1))%MDC%CLOCK_OFFSET(J11) + &
     &                                  ( TIM_ARG - PIM%STA(IND_STA(J1))%MDC%TIME_CEN(J11) )* &
     &                                  PIM%STA(IND_STA(J1))%MDC%CLOCK_RATE(J11) 
                      CLO_RAT_APR(J1) = PIM%STA(IND_STA(J1))%MDC%CLOCK_RATE(J11) 
                 END IF
 4110          CONTINUE
            ELSE IF (  PIM%STA(IND_STA(J1))%MDC%CLO_MODEL_STATUS == PIMA__MDC_GLO_INCLUDED ) THEN
               CONTINUE 
               TIM_DIF = TIM_ARG - ( (PIM%STA(IND_STA(J1))%MDC%MJD_REF - PIM%MJD_0)*86400.0D0 + &
     &                               (PIM%STA(IND_STA(J1))%MDC%TAI_REF - PIM%TAI_0) )
               CLO_OFF_APR(J1) = PIM%STA(IND_STA(J1))%MDC%CLO_OFFS + &
     &                           PIM%STA(IND_STA(J1))%MDC%CLO_RATE*TIM_DIF
               CLO_RAT_APR(J1) = PIM%STA(IND_STA(J1))%MDC%CLO_RATE
            ELSE
               CLO_OFF_APR(J1) = 0.0D0
               CLO_RAT_APR(J1) = 0.0D0
         END IF
!
         IF ( ENV_RDV78 == 'YES' ) THEN
              IF ( TIM_ARG > 7886.D0 ) THEN
                   IF ( PIM%STA(IND_STA(J1))%IVS_NAME == 'FD-VLBA ' ) THEN
                        CLO_OFF_APR(J1) = CLO_OFF_APR(J1) + 2.379151446880734E-008
                       ELSE IF ( PIM%STA(IND_STA(J1))%IVS_NAME == 'KP-VLBA ' ) THEN
                        CLO_OFF_APR(J1) = CLO_OFF_APR(J1) + 8.372572361831168E-009
                      ELSE IF ( PIM%STA(IND_STA(J1))%IVS_NAME == 'MK-VLBA ' ) THEN
                        CLO_OFF_APR(J1) = CLO_OFF_APR(J1) - 4.216053442447995E-008
                      ELSE IF ( PIM%STA(IND_STA(J1))%IVS_NAME == 'PIETOWN ' ) THEN
                        CLO_OFF_APR(J1) = CLO_OFF_APR(J1) + 2.639986270257185E-008
                   END IF
              END IF
         END IF
         ATM_DEL_APR(J1) = 0.0D0 ! ATM_DEL_APR from VLBA is wrong 
!
         IF ( FL_CONT(J1) ) THEN
!
! ----------- Compute path delay using polynomials from the 
! ----------- interferometric model
!
              TIM_POL  = TIM_ARG - PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%TIM_BEG
              DEL(J1)  = PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%GDEL_POL(PIM__MDPL,1)
              RAT(J1)  = PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%PRAT_POL(PIM__MDPL,PIMA_PRAT_IND)
              PHS(J1)  = PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%PDEL_POL(PIM__MDPL,PIMA_PRAT_IND)
              DO 4120 J12=PIM__MDPL-1,0,-1
                 DEL(J1) = PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%GDEL_POL(J12,1) + &
     &                     DEL(J1)*TIM_POL
                 RAT(J1) = PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%PRAT_POL(J12,PIMA_PRAT_IND) + &
     &                     RAT(J1)*TIM_POL
                 PHS(J1) = PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%PDEL_POL(J12,PIMA_PRAT_IND) + &
     &                     PHS(J1)*TIM_POL
 4120         CONTINUE
!
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 10  ) THEN
                   CALL CLRCH ( STR )
                   STR  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_ARG, -2 )
                   WRITE ( 6, 205 ) STR(1:26), PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1)), TIM_POL, &
     &                              PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%GDEL_POL(0:PIM__MDPL,1), &
     &                              PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%PRAT_POL(0:PIM__MDPL,PIMA_PRAT_IND)
 205               FORMAT ( 'PIMA_APR_DELAY ',A, 1X, ' Sta: ', A, &
     &                     ' TIM_POL: ', F12.6, &
     &                     ' GDEL_POL: ', 7(1PD19.12, 1X), &
     &                     ' PRAT_POL: ', 7(1PD19.12, 1X)  )
              END IF
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                   CALL CLRCH ( STR )
                   STR  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_ARG, -2 )
                   WRITE ( 6, 210 ) STR(1:26), PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                              J1, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1)), &
     &                              DEL(J1), RAT(J1), FL_CONT(J1), IND_MOD(J1), &
     &                              PIM%C_SOU(PIM%STA(IND_STA(J1))%MOD(J2)%SOU_IND), &
     &                              PIM%STA(IND_STA(J1))%MOD(IND_MOD(J1))%SCANNAME
 210              FORMAT ( 'PIMA_APR_DELAY ',A, 1X, ' Sou: ', A, &
     &                     ' Sta_ind: ', I1, ' Sta: ', A, &
     &                     ' Geoc_apr_delay: ', 1PD20.12, &
     &                     ' Geoc_apr_rate: ', 1PD20.12, ' FL_CONT: ', L1, &
     &                     ' Ind_mod: ', I6, ' Mod_sou: ', A, ' Scan: ', A  )
              END IF                
            ELSE 
!
! ----------- Compute path delay using VTD and subtraction 
! ----------- the difference VTD - Calc that is produced by interpolation
! ----------- or extrapolation of Legendre polynomials
!
              TIM_POL = TIM_ARG - PIM%STA(IND_STA(J1))%MOD(PIM%OBS(IND_OBS)%MOD_IND_BEG(J1))%TIM_BEG
              DEL(J1) = 0.0D0
              RAT(J1) = 0.0D0
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_DELAY ( PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND),    &
     &            'GEOCENTR',  PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1)), &
     &             PIM%MJD_0, PIM%TAI_0 + TIM_ARG, &
     &             OBS_TYP, VTD, DEL(J1), RAT(J1), &
     &             DER_DEL, DER_RAT, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IND_OBS, STR )
                   CALL ERR_LOG ( 7787, IUER, 'PIMA_APR_DELAY', 'Error in '// &
     &                 'an attempt to compute theoretical path delay for '// &
     &                 'observation '//STR )
                   RETURN 
              END IF
              DO 4130 J13=0,M__DEG
                 DEL(J1) = DEL(J1) + DEL_LEG(J13,J1)* LEGENDRE_POL ( J13, &
     &                                 TIM_ARR(1,J1), TIM_ARR(NSPL(J1),J1), &
     &                                 TIM_POL )
                 RAT(J1) = RAT(J1) + DEL_LEG(J13,J1)* LEGENDRE_DER ( J13, &
     &                                 TIM_ARR(1,J1), TIM_ARR(NSPL(J1),J1), &
     &                                 TIM_POL )
 4130         CONTINUE
              PHS = DEL
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                   CALL CLRCH ( STR )
                   STR  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_ARG, -2 )
                   WRITE ( 6, 210 ) STR(1:26), PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND), &
     &                              J1, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1)), &
     &                              DEL(J1), RAT(J1), FL_CONT(J1)
              END IF                
         END IF
 410  CONTINUE
! 
      PIM%OBS(IND_OBS)%APR_GC_RAT(1,IND_BND) = RAT(1)
      PIM%OBS(IND_OBS)%APR_GC_RAT(2,IND_BND) = RAT(2)
      PIM%OBS(IND_OBS)%APR_GC_DEL(1,IND_BND) = DEL(1)
      PIM%OBS(IND_OBS)%APR_GC_DEL(2,IND_BND) = DEL(2)
!
! --- Re-adjust apriori phase for change of the frequency.
! --- Interferometric model computes fringe phase for the file
! --- reference frequency, that may not be necessarily the frequency
! --- that was used as a reference during fringing process. The
! --- coefficients of the polynomials for apriori phases and rates
! --- have been divied by the file referecne frequency. Now we have to
! --- scale them to the reference frequency of the band.
!
! --- We also multiply the phase by pi2 in order to express it in radians,
! --- since the phase polynomals in the interferometric model give
! --- the apriori phase in phase turns
!
      PIM%OBS(IND_OBS)%APR_GC_PHS(1,IND_BND) = PHS(1)*PI2*PIM%OBS(IND_OBS)%REF_FREQ(IND_BND)
      PIM%OBS(IND_OBS)%APR_GC_PHS(2,IND_BND) = PHS(2)*PI2*PIM%OBS(IND_OBS)%REF_FREQ(IND_BND)
!
! --- Resolve 2pi ambiguities
!
      PIM%OBS(IND_OBS)%APR_GC_PHS(1,IND_BND) = PIM%OBS(IND_OBS)%APR_GC_PHS(1,IND_BND) - &
     &                 PI2*IDNINT ( PIM%OBS(IND_OBS)%APR_GC_PHS(1,IND_BND)/PI2 )
      PIM%OBS(IND_OBS)%APR_GC_PHS(2,IND_BND) = PIM%OBS(IND_OBS)%APR_GC_PHS(2,IND_BND) - &
     &                 PI2*IDNINT ( PIM%OBS(IND_OBS)%APR_GC_PHS(2,IND_BND)/PI2 )
!
! --- Determine which station is the first, and therefore, should be taken
! --- as a reference.
!
      IF ( IND_STA(1) < IND_STA(2) ) THEN
           IND_1ST =  1
           IND_2ND =  2
           ISGN    =  1
         ELSE
           IND_1ST =  2
           IND_2ND =  1
           ISGN    = -1
      END IF
      IF ( FL_ONLY_THEO ) THEN
           ISGN    =  0
           ISGN    =  0
      END IF
      IF ( FL_MKDB_NOSORT ) THEN
           IND_1ST =  1
           IND_2ND =  2
           ISGN    =  1
      END IF
!
      IF ( MODE_REF == 'GEOCENTER' .OR. MODE_REF == 'GEOCENTR' ) THEN
           CONTINUE
         ELSE IF ( MODE_REF == '1ST_STA' .AND. PIM%CORR_NAME == 'RASFX' ) THEN
           CONTINUE 
         ELSE IF ( MODE_REF == '1ST_STA' ) THEN
!
! -------- Adjust apriori delay for the placement of the time tag:
! -------- transform delay from the geocenter referenced to the
! -------- 1st station referenced, i.e. get delay at the moment of arrival
! -------- of the wavefront to the first station
!
           DO 4140 J14=1,M_ITR
              IF ( FL_CONT(IND_1ST) ) THEN
                   TIM_BEG_1ST = PIM%STA(IND_STA(IND_1ST))%MOD(IND_MOD(IND_1ST))%TIM_BEG
                 ELSE 
                   TIM_BEG_1ST = PIM%STA(IND_STA(IND_1ST))%MOD(PIM%OBS(IND_OBS)%MOD_IND_BEG(IND_1ST))%TIM_BEG
              END IF
              IF ( FL_CONT(IND_2ND) ) THEN
                   TIM_BEG_2ND = PIM%STA(IND_STA(IND_2ND))%MOD(IND_MOD(IND_2ND))%TIM_BEG
                 ELSE 
                   TIM_BEG_2ND = PIM%STA(IND_STA(IND_2ND))%MOD(PIM%OBS(IND_OBS)%MOD_IND_BEG(IND_2ND))%TIM_BEG
              END IF
!
! ----------- Now the argument of the polynomial is the moment of
! ----------- wavefront arrival to the reference station of the baseline
! ----------- NB: We add the apriori clock delay, not subtact.
! ----------- This is because FITS-IDI file keeps this delay 
! ----------- with the opposite sign
!
              TIM_POL =  TIM_ARG     &
     &                 - TIM_BEG_1ST &
     &                 - ( DEL(IND_1ST) + CLO_OFF_APR(IND_1ST) - ATM_DEL_APR(IND_1ST) )
              IF ( FL_CONT(IND_1ST) ) THEN
!
! ---------------- Re-compute path delay for the first station using polynomials 
! ---------------- from the inteferometric model
!
                   DEL(IND_1ST) = PIM%STA(IND_STA(IND_1ST))%MOD(IND_MOD(IND_1ST))%GDEL_POL(PIM__MDPL,1)
                   RAT(IND_1ST) = PIM%STA(IND_STA(IND_1ST))%MOD(IND_MOD(IND_1ST))%PRAT_POL(PIM__MDPL,PIMA_PRAT_IND)
                   PHS(IND_1ST) = PIM%STA(IND_STA(IND_1ST))%MOD(IND_MOD(IND_1ST))%PDEL_POL(PIM__MDPL,PIMA_PRAT_IND)
                   DO 4150 J15=PIM__MDPL-1,0,-1
                      DEL(IND_1ST) = PIM%STA(IND_STA(IND_1ST))%MOD(IND_MOD(IND_1ST))%GDEL_POL(J15,1) &
     &                               + DEL(IND_1ST)*TIM_POL
                      RAT(IND_1ST) = PIM%STA(IND_STA(IND_1ST))%MOD(IND_MOD(IND_1ST))%PRAT_POL(J15,PIMA_PRAT_IND) &
     &                               + RAT(IND_1ST)*TIM_POL
                      PHS(IND_1ST) = PIM%STA(IND_STA(IND_1ST))%MOD(IND_MOD(IND_1ST))%PDEL_POL(J15,PIMA_PRAT_IND) &
     &                               + PHS(IND_1ST)*TIM_POL
 4150              CONTINUE
                 ELSE 
!
! ---------------- Re-compute path delay for the first station using VTD
! ---------------- and applying the differences VTD - Calc produced by
! ---------------- interpolation
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL VTD_DELAY ( PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND),    &
     &                 'GEOCENTR',  PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(IND_1ST)), &
     &                  PIM%MJD_0, PIM%TAI_0 + TIM_ARG &
     &                      - ( DEL(IND_1ST) + CLO_OFF_APR(IND_1ST) - ATM_DEL_APR(IND_1ST) ), &
     &                  OBS_TYP, VTD, DEL(IND_1ST), RAT(IND_1ST), &
     &                  DER_DEL, DER_RAT, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( IND_OBS, STR )
                        CALL ERR_LOG ( 7788, IUER, 'PIMA_APR_DELAY', 'Error in '// &
     &                      'an attempt to compute theoretical path delay for '// &
     &                      'observation '//STR )
                        RETURN 
                   END IF
!
                   DO 4160 J16=0,M__DEG
                      DEL(IND_1ST) = DEL(IND_1ST) + DEL_LEG(J16,IND_1ST)* LEGENDRE_POL ( J16, &
     &                                    TIM_ARR(1,IND_1ST), TIM_ARR(NSPL(IND_1ST),IND_1ST), &
     &                                    TIM_POL )
                      RAT(IND_1ST) = RAT(IND_1ST) + DEL_LEG(J16,IND_1ST)* LEGENDRE_DER ( J16, &
     &                                    TIM_ARR(1,IND_1ST), TIM_ARR(NSPL(IND_1ST),IND_1ST), &
     &                                    TIM_POL )
 4160              CONTINUE 
                   PHS(IND_1ST) = DEL(IND_1ST)
              END IF
 4140      CONTINUE
!
! -------- Now update delay for the second station for update TIM_POL
!
           TIM_POL =   TIM_ARG     &
     &               - TIM_BEG_2ND &
     &               - ( DEL(IND_1ST) + CLO_OFF_APR(IND_1ST) - ATM_DEL_APR(IND_1ST) )
!@   write ( 6, * ) 'PAD-660 ', del(ind_2nd), del(ind_1st), clo_off_apr(ind_2nd) - clo_off_apr(ind_1st) ! %%%%%%%%%%
           IF ( FL_CONT(IND_2ND) .AND. PIM%CORR_NAME .NE. 'RASFX' ) THEN 
!
! ------------- Re-compute path delay for the second station using polynomials 
! ------------- from the inteferometric model
!
                DEL(IND_2ND) = PIM%STA(IND_STA(IND_2ND))%MOD(IND_MOD(IND_2ND))%GDEL_POL(PIM__MDPL,1)
                RAT(IND_2ND) = PIM%STA(IND_STA(IND_2ND))%MOD(IND_MOD(IND_2ND))%PRAT_POL(PIM__MDPL,PIMA_PRAT_IND)
                PHS(IND_2ND) = PIM%STA(IND_STA(IND_2ND))%MOD(IND_MOD(IND_2ND))%PDEL_POL(PIM__MDPL,PIMA_PRAT_IND)
                DO 4170 J17=PIM__MDPL-1,0,-1
                   DEL(IND_2ND) = PIM%STA(IND_STA(IND_2ND))%MOD(IND_MOD(IND_2ND))%GDEL_POL(J17,1) &
     &                            + DEL(IND_2ND)*TIM_POL
                   RAT(IND_2ND) = PIM%STA(IND_STA(IND_2ND))%MOD(IND_MOD(IND_2ND))%PRAT_POL(J17,PIMA_PRAT_IND) &
     &                            + RAT(IND_2ND)*TIM_POL
                   PHS(IND_2ND) = PIM%STA(IND_STA(IND_2ND))%MOD(IND_MOD(IND_2ND))%PDEL_POL(J17,PIMA_PRAT_IND) &
     &                            + PHS(IND_2ND)*TIM_POL
 4170           CONTINUE
              ELSE
!
! ------------- Re-compute path delay for the second station using VTD
! ------------- and applying the differences VTD - Calc produced by
! ------------- interpolation
!
                CALL ERR_PASS ( IUER, IER )
                CALL VTD_DELAY ( PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND),    &
     &               'GEOCENTR',  PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(IND_2ND)), &
     &               PIM%MJD_0, PIM%TAI_0 + TIM_ARG &
     &                   - ( DEL(IND_1ST) + CLO_OFF_APR(IND_1ST) - ATM_DEL_APR(IND_1ST) ), &
     &               OBS_TYP, VTD, DEL(IND_2ND), RAT(IND_2ND), &
     &               DER_DEL, DER_RAT, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IND_OBS, STR )
                     CALL ERR_LOG ( 7789, IUER, 'PIMA_APR_DELAY', 'Error in '// &
     &                   'an attempt to compute theoretical path delay for '// &
     &                   'observation '//STR )
                     RETURN 
                END IF
                DO 4180 J18=0,M__DEG
                   DEL(IND_2ND) = DEL(IND_2ND) + DEL_LEG(J18,IND_2ND)* LEGENDRE_POL ( J18, &
     &                                TIM_ARR(1,IND_2ND), TIM_ARR(NSPL(IND_2ND),IND_2ND), &
     &                                TIM_POL )
                   RAT(IND_2ND) = RAT(IND_2ND) + DEL_LEG(J18,IND_2ND)* LEGENDRE_DER ( J18, &
     &                                TIM_ARR(1,IND_2ND), TIM_ARR(NSPL(IND_2ND),IND_2ND), &
     &                                TIM_POL )
 4180           CONTINUE 
                PHS(IND_2ND) = DEL(IND_2ND)
           END IF
        ELSE
           CALL ERR_LOG ( 7790, IUER, 'PIMA_APR_DELAY', 'Unknown reference '// &
     &         'mode '//MODE_REF(1:I_LEN(MODE_REF))//' -- one of '// &
     &         'GEOCENTER or 1ST_STA were expected' )
           RETURN
      END IF
!
! --- Compute apriori group delay and delay rate at the moment of
! --- the wavefront arrival at the reference station
! --- NB: The apriori clock offset is ADDED to the apriori delay
! ---     the apriori clock rate   is ADDED to the apriori rate
!
      IF ( FL_INCLUDE_CLO ) THEN
           PIM%OBS(IND_OBS)%APR_GR_DEL(IND_BND) = DEL(IND_2ND) - DEL(IND_1ST) &
     &          + ( CLO_OFF_APR(IND_2ND) - CLO_OFF_APR(IND_1ST) )
           PIM%OBS(IND_OBS)%APR_RAT(IND_BND)    = RAT(IND_2ND) - RAT(IND_1ST) &
     &          + ( CLO_RAT_APR(IND_2ND) - CLO_RAT_APR(IND_1ST) )
!@   write ( 6, * ) 'PAD-724 ', del(ind_2nd), del(ind_1st), clo_off_apr(ind_2nd) - clo_off_apr(ind_1st) ! %%%%%%%%%%
        ELSE 
           PIM%OBS(IND_OBS)%APR_GR_DEL(IND_BND) = DEL(IND_2ND) - DEL(IND_1ST)
           PIM%OBS(IND_OBS)%APR_RAT(IND_BND)    = RAT(IND_2ND) - RAT(IND_1ST)
      END IF
      IF ( MODE_REF == '1ST_STA' .AND. PIM%CORR_NAME == 'RASFX' ) THEN
           PIM%OBS(IND_OBS)%APR_GR_DEL(IND_BND) = (DEL(IND_1ST) - DEL(IND_2ND))*(1.D0 - RAT(IND_2ND)) &
     &          + ( CLO_OFF_APR(IND_1ST) - CLO_OFF_APR(IND_2ND) )
           PIM%OBS(IND_OBS)%APR_RAT(IND_BND)    = (RAT(IND_1ST) - RAT(IND_2ND)) &
     &          + ( CLO_RAT_APR(IND_1ST) - CLO_RAT_APR(IND_2ND) )
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 11 ) THEN
           WRITE ( 6, 220 ) STR(1:26), &
     &                      CLO_OFF_APR(IND_2ND) - CLO_OFF_APR(IND_1ST), CLO_RAT_APR(IND_1ST) - CLO_RAT_APR(IND_2ND) 
 220       FORMAT ( 'PIMA_APR_DELAY ',A, 2X, 'Clock_offset: ', 1PD15.8,' Clock_rate: ', 1PD15.8 )
      END IF
!
! --- Store apriori clock offsets and rates with correct sign
!
      PIM%OBS(IND_OBS)%CLO_OFFSET_APR(1,IND_BND) = -CLO_OFF_APR(1)
      PIM%OBS(IND_OBS)%CLO_OFFSET_APR(2,IND_BND) = -CLO_OFF_APR(2)
      PIM%OBS(IND_OBS)%CLO_RATE_APR(1,IND_BND)   = -CLO_RAT_APR(1)
      PIM%OBS(IND_OBS)%CLO_RATE_APR(2,IND_BND)   = -CLO_RAT_APR(2)
!
! --- Re-adjust the apriori phase and resolve 2pi ambiguity
!
      IF ( FL_INCLUDE_CLO ) THEN
           PIM%OBS(IND_OBS)%APR_PHS(IND_BND) = ( (PHS(IND_2ND) - PHS(IND_1ST)) &
     &                    + ( CLO_OFF_APR(IND_2ND) - CLO_OFF_APR(IND_1ST) ) )* &
     &                      PI2*PIM%OBS(IND_OBS)%REF_FREQ(IND_BND)
         ELSE 
           PIM%OBS(IND_OBS)%APR_PHS(IND_BND) = (PHS(IND_2ND) - PHS(IND_1ST))* &
     &                      PI2*PIM%OBS(IND_OBS)%REF_FREQ(IND_BND)
      END IF
      PIM%OBS(IND_OBS)%APR_PHS(IND_BND) = PIM%OBS(IND_OBS)%APR_PHS(IND_BND) - &
     &                         PI2*IDNINT ( PIM%OBS(IND_OBS)%APR_PHS(IND_BND)/PI2 )
!
! --- Now compute total observables for this band
!
      DO 4190 J19=1,PIM__MFRA
         PIM%OBS(IND_OBS)%TOT_MB_DEL(J19,IND_BND)  = &
     &                  PIM%OBS(IND_OBS)%APR_GR_DEL(IND_BND) &
     &                + ISGN*PIM%OBS(IND_OBS)%RES_MB_DEL(J19,IND_BND) &
     &                + ISGN*PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND)* SOF
         IF ( PIM%CORR_NAME == 'RASFX' ) THEN
              PIM%OBS(IND_OBS)%TOT_MB_DEL(J19,IND_BND)  = &
     &                      PIM%OBS(IND_OBS)%APR_GR_DEL(IND_BND) &
     &                    - ISGN*PIM%OBS(IND_OBS)%RES_MB_DEL(J19,IND_BND) &
     &                    - ISGN*PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND)* SOF
         END IF
         PIM%OBS(IND_OBS)%TOT_PH_RAT(J19,IND_BND) = &
     &                ( PIM%OBS(IND_OBS)%APR_RAT(IND_BND) &
     &                  + ISGN*PIM%OBS(IND_OBS)%RES_PH_RAT(J19,IND_BND) &
     &                )*(1.0D0 - RAT(IND_1ST))
         IF ( PIM%CORR_NAME == 'RASFX' ) THEN
              PIM%OBS(IND_OBS)%TOT_PH_RAT(J19,IND_BND) = &
     &                ( PIM%OBS(IND_OBS)%APR_RAT(IND_BND)    &
     &                  + ISGN*PIM%OBS(IND_OBS)%RES_PH_RAT(J19,IND_BND) &
     &                )*(1.0D0 - RAT(IND_2ND))
         END IF
!
! ------ Compute the total phase.
! ------ We deal with two sorts of phases:
! ------ a) 1st station referenced;
! ------ b) "geocenter" referenced;
!
         PIM%OBS(IND_OBS)%TOT_PHS(J19,IND_BND)   = &
     &                  PIM%OBS(IND_OBS)%APR_PHS(IND_BND) &
     &                - ISGN*PIM%OBS(IND_OBS)%RES_PHS(J19,IND_BND) &
     &                + ISGN*PIM%OBS(IND_OBS)%RES_PH_RAT(J19,IND_BND)* SOF * &
     &                  PI2*PIM%OBS(IND_OBS)%REF_FREQ(IND_BND)
         PIM%OBS(IND_OBS)%TOT_PHS_GC(J19,IND_BND) = &
     &                  ISGN*( PIM%OBS(IND_OBS)%APR_GC_PHS(IND_2ND,IND_BND) - &
     &                         PIM%OBS(IND_OBS)%APR_GC_PHS(IND_1ST,IND_BND) &
     &                       ) &
     &                - ISGN*PIM%OBS(IND_OBS)%RES_PHS(J19,IND_BND) &
     &                + ISGN*PIM%OBS(IND_OBS)%RES_PH_RAT(J19,IND_BND)* SOF * &
     &                  PI2*PIM%OBS(IND_OBS)%REF_FREQ(IND_BND)
!
! ------ Resolve 2pi ambiguitues of total phases
!
         PIM%OBS(IND_OBS)%TOT_PHS(J19,IND_BND)    = &
     &                 PIM%OBS(IND_OBS)%TOT_PHS(J19,IND_BND) - &
     &                 PI2*IDNINT ( PIM%OBS(IND_OBS)%TOT_PHS(J19,IND_BND)/PI2 )
         PIM%OBS(IND_OBS)%TOT_PHS_GC(J19,IND_BND) = &
     &                 PIM%OBS(IND_OBS)%TOT_PHS_GC(J19,IND_BND) - &
     &                 PI2*IDNINT ( PIM%OBS(IND_OBS)%TOT_PHS_GC(J19,IND_BND)/PI2 )
         PIM%OBS(IND_OBS)%MB_DEL_ERR(J19,IND_BND) = DSQRT ( &
     &        PIM%OBS(IND_OBS)%MB_DEL_ERR(J19,IND_BND)**2 + &
     &        2.0D0*PIM%OBS(IND_OBS)%COV_GR_GD(IND_BND)* SOF + &
     &        PIMA_VAR_GR_RAT_ERR_SCALE*PIM%OBS(IND_OBS)%GR_RAT_ERR(IND_BND)**2* SOF**2 &
     &        )
!
        PIM%OBS(IND_OBS)%PH_DEL_ERR(J19,IND_BND) = DSQRT ( &
     &        PIM%OBS(IND_OBS)%PH_DEL_ERR(J19,IND_BND)**2 + &
     &        2.0D0*PIM%OBS(IND_OBS)%COV_PR_PH(IND_BND)* SOF + &
     &        PIM%OBS(IND_OBS)%PH_RAT_ERR(J19,IND_BND)**2* SOF**2 &
     &       )
 4190 CONTINUE 
!
      PIM%OBS(IND_OBS)%TOT_SB_DEL(IND_BND)  = &
     &                 PIM%OBS(IND_OBS)%APR_GR_DEL(IND_BND) +   &
     &                 ISGN*PIM%OBS(IND_OBS)%RES_SB_DEL(IND_BND)
      PIM%OBS(IND_OBS)%TOT_GR_RAT(IND_BND) = &
     &                ( PIM%OBS(IND_OBS)%APR_RAT(IND_BND) +   &
     &                  ISGN*PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) &
     &                )*(1.0D0 - RAT(IND_1ST))
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_APR_DELAY  !#!#
