      SUBROUTINE ERM_RES ( ERM, VTD, L_PAR, C_PAR, EST_VEC, COV_MAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine ERME_RES  puts in the spool file opened at the logical     *
! *   unit 23 the ERM-section with information about results of          *
! *   estimation of Earth Rotation Model.                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *          ERM ( ERM__TYPE ) -- Derived object defined in              *
! *                               $PSOLVE_ROOT/include/solve.i which     *
! *                               keeps information about estimation     *
! *                               of the Earth Rotation Model.           *
! *        L_PAR ( INTEGER*4 ) -- The total number of global parameter.  *
! *        C_PAR ( INTEGER*4 ) -- The list of global parameters.         *
! * EST_VEC ( REAL*8    ) -- Vector of estimates. Dimension: L_PAR.      *
! * COV_MAT ( REAL*8    ) -- Unscaled covariance matrix in packed upper  *
! *                          triangular representation. Dimension: L_PAR.*
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *  ### 19-JAN-2006     ERM_RES   v2.4 (c)  L. Petrov  01-JAN-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'vtd.i'
      INCLUDE   'erm.i'
      TYPE      ( ERM__TYPE ) ERM
      TYPE      ( VTD__TYPE ) VTD
      INTEGER*4  L_PAR, IUER
      REAL*8     EST_VEC(*), COV_MAT(*)
      CHARACTER  C_PAR(L_PAR)*(*)
      CHARACTER  PAR_NAM*20, PAR_NAM1*20, PAR_NAM2*20, NOD_DATE*30, &
     &           VTD_CONF_FILE*128, C_STA_1*8, C_SOU_1*8,UZT_MODEL_STR*64, &
     &           UZT_USE_STR*64, STR1*32, STR2*32, STR3*32, CNS_UNIT(0:2)*7
      DATA       CNS_UNIT / 'rad    ', 'rad/s  ', 'rad/s^2' /
      INTEGER*4  J1, J2, J3, J4, J5, IND_PAR(1-ERM__MSPL:M_GPA,3), &
     &           IPAR, MJD_NODE, ERM_MJD_BEG, ERM_MJD_END, ERM_MJD_OBS, IDAY, IVRB, IER
      REAL*8     ERM_TAI_BEG, ERM_TAI_END, ERM_TAI_OBS, TIM_END, ERM_APR(3)
      INTEGER*8  IND11, IND12, IND22
      REAL*8     SEC_NODE, CORR
      REAL*8     COVMAT_EPS, TIM__OVR 
      PARAMETER  ( COVMAT_EPS = 1.D-30  )
      PARAMETER  ( TIM__OVR   = 300.0D0 ) ! Overshot time before the 1st and after the last knot
      INTEGER*4  I, J
      INTEGER*8  LOCS
      LOCS(I,J) = INT8(min(I,J)) +(INT8(max(I,J))*INT8(max(I,J)-1))/2
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( ERM%NKNOTS(1) == 0  .AND.  &
     &     ERM%NKNOTS(2) == 0  .AND.  &
     &     ERM%NKNOTS(3) == 0         ) THEN
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( L_PAR .LT. 1  .OR.  L_PAR .GT. M_GPA ) THEN
           WRITE ( 6, * ) 'L_PAR  = ', L_PAR, ' M_GPA = ', M_GPA
           CALL ERR_LOG ( 7681, IUER, 'ERM_RES', 'Parameter L_PAR is '// &
     &         'out of range [1, M_GPA]' )
           RETURN
      END IF
!
! --- Cycle over components: E1, E2, E3
!
      ERM_MJD_BEG = ERM%MJD_BEG 
      ERM_TAI_BEG = ERM%TAI_BEG - TIM__OVR 
      IDAY = ERM_TAI_BEG/86400.D0
      IF ( ERM_TAI_BEG < 0.0D0 ) IDAY = IDAY - 1
      IF ( IDAY < 0 ) THEN
           ERM_MJD_BEG = ERM_MJD_BEG - IDAY
           ERM_TAI_BEG = ERM_TAI_BEG + IDAY*86400.0D0
      END IF
      TIM_END = 0.0D0
      IF ( ERM%NKNOTS(1) > 0 ) TIM_END = MAX ( TIM_END, ERM%TIM(ERM%NKNOTS(1),1) )
      IF ( ERM%NKNOTS(2) > 0 ) TIM_END = MAX ( TIM_END, ERM%TIM(ERM%NKNOTS(2),2) )
      IF ( ERM%NKNOTS(3) > 0 ) TIM_END = MAX ( TIM_END, ERM%TIM(ERM%NKNOTS(3),3) )
      ERM_MJD_END = J2000__MJD
      ERM_TAI_END = 43200.0D0 + TIM_END + TIM__OVR 
      IDAY = ERM_TAI_END/86400.0D0
      IF ( IDAY > 0 ) THEN
           ERM_MJD_END = ERM_MJD_END + IDAY
           ERM_TAI_END = ERM_TAI_END - IDAY*86400.0D0
      END IF
!
! --- Re-load VTD in order to update start and stop date of the entire ERM date range
!
      C_STA_1 = VTD%STA(1)%IVS_NAME
      C_SOU_1 = VTD%SOU(1)%IVS_NAME
      VTD_CONF_FILE = VTD%CONF%CONFIG_FINAM
      IVRB = VTD%CONF%IVRB 
!
! --- First quit VTD
!
      IER = 0
      CALL VTD_QUIT ( VTD, IER )
!
! --- Then init VTD
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7682, IUER, 'ERM_RES', 'Error in an attempt to '// &
     &         'initialize VTD oibject' )
           RETURN 
      END IF
!
! --- The load the configuration file
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_CONF ( VTD_CONF_FILE, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7683, IUER, 'ERM_RES', 'Error in an attempt '// &
     &         'to read configuration file '//VTD_CONF_FILE )
           RETURN 
      END IF
!
! --- Disable computat ion of position variations, ionospheric  contribution,
! --- and slant path delay, since they are date-specifi and are interpolated
! --- for a shrot period of time
!
      DO 510 J1=1,VTD__M_PSF
         VTD%CONF%POSVAR_MOD(J1) = VTD__UNDF
         CALL CLRCH ( VTD%CONF%POSVAR_FIL(J1) )
 510  CONTINUE 
      DO 520 J2=1,VTD__M_IOF
         CALL CLRCH ( VTD%CONF%IONO_FILE(J2) )
 520  CONTINUE 
      DO 530 J3=1,VTD__M_EPD
         CALL CLRCH ( VTD%CONF%DIR_EPD(J3) )
 530  CONTINUE 
      VTD%CONF%FINAM_STAECC = 'NONE'
      CALL CLRCH ( VTD%CONF%IONO_MODEL )
!
! --- Load one station and one source for being able to use VTD
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL VTD_LOAD  ( VTD, 1, C_STA_1, 1, C_SOU_1, &
     &                 ERM_MJD_BEG, ERM_TAI_BEG, ERM_MJD_END, ERM_TAI_END, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7684, IUER, 'ERM_RES', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           RETURN
      END IF
!
      IF ( VTD%CONF%UZT_MODEL == UZT__NONE ) THEN
           UZT_MODEL_STR = 'NONE'
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__DICKMAN1993 ) THEN
           UZT_MODEL_STR = 'DICKMAN1993'
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__DICKMAN_PRINCIPLE ) THEN
           UZT_MODEL_STR = 'DICKMAN_PRINCIPLE'
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__DICKMAN_SHORT     ) THEN
           UZT_MODEL_STR = 'DICKMAN_SHORT'
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__RE2014            ) THEN
           UZT_MODEL_STR = 'RE2014'
         ELSE IF ( VTD%CONF%UZT_MODEL == UZT__RE2014_SHORT      ) THEN
           UZT_MODEL_STR = 'RE2014_SHORT'
         ELSE
           UZT_MODEL_STR = 'NONE'
      END IF 
!
      IF ( VTD%CONF%UZT_USE == UZT__NONE ) THEN
           UZT_USE_STR = 'NONE'
        ELSE IF ( VTD%CONF%UZT_USE == UZT__ADD ) THEN
           UZT_USE_STR = 'ADD'
        ELSE IF ( VTD%CONF%UZT_USE == UZT__SUBTRACT ) THEN
           UZT_USE_STR = 'SUBTRACT'
        ELSE IF ( VTD%CONF%UZT_USE == UZT__INTERPOLATE ) THEN
           UZT_USE_STR = 'INTERPOLATE'
        ELSE
           UZT_USE_STR = 'Uknwown'
      END IF
!
      WRITE  ( 23, 210 ) ERM_FMT__LABEL, TRIM(VTD%CONF%FINAM_EOP), &
     &                   TRIM(UZT_MODEL_STR), TRIM(UZT_USE_STR)
 210  FORMAT ( 'ERM_SECTION BEGIN'/ &
     &          A/ &
     &         'ERM APR  File:  ', A/ &
     &         'ERM UZM  Model: ', A/ &
     &         'ERM UZU  Use:   ', A  )
      DO 410 J1=1,3
         IPAR = 0
         WRITE  ( 23, 220 ) J1, ERM%DEGREE(J1), ERM%NKNOTS(J1), &
     &                      ERM%TIME_EST_SPAN(J1), ERM%TIME_CNS_SPAN(J1)
 220     FORMAT ( 'ERM DIM  Icmp: ', I1, ' Ncmp: ', I1, ' Nknots: ', I5, &
     &            ' Est_Step: ', F10.2, ' Cns_Step: ', F10.2 )
         CALL ERR_PASS ( IUER, IER ) 
         STR1 = MJDSEC_TO_DATE ( ERM%MJD_BEG, ERM%TAI_BEG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7685, IUER, 'ERM_RES', &
     &                   'Wrong ERM%MJD_BEG, ERM%TAI_BEG' )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER ) 
         STR2 = MJDSEC_TO_DATE ( ERM%MJD_END, ERM%TAI_END, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7686, IUER, 'ERM_RES', &
     &                   'Wrong ERM%MJD_END, ERM%TAI_END' )
              RETURN
         END IF
         WRITE  ( 23, 230 ) 'Beg', ERM%MJD_BEG, ERM%TAI_BEG, STR1(1:19)
         WRITE  ( 23, 230 ) 'End', ERM%MJD_END, ERM%TAI_END, STR2(1:19)
 230     FORMAT ( 'ERM TIM  ',A,':  ', I5, 1X, F7.1, 1X, A )
!
         DO 420 J2=0,2
            IF ( ERM%CNS_DER_SIGMA(J2,J1) > 0.0D0 ) THEN
                 WRITE  ( 23, 240 ) J1, J2, ERM%CNS_DER_SIGMA(J2,J1), CNS_UNIT(J2)
 240             FORMAT ( 'ERM CNS  Icmp: ', I1, ' Cnstr der  ', I1, ' Sigma: ', 1PD12.5, 1X, A )
            END IF
 420     CONTINUE 
!
         IF ( ERM%MJD_BEG_RANGE_CNS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER ) 
              STR1 = MJDSEC_TO_DATE ( ERM%MJD_BEG_RANGE_CNS, ERM%TAI_BEG_RANGE_CNS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7687, IUER, 'ERM_RES', &
     &                        'Wrong ERM%MJD_BEG_RANGE_CNS, ERM%TAI_BEG_RANGE_CNS' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER ) 
              STR2 = MJDSEC_TO_DATE ( ERM%MJD_END_RANGE_CNS, ERM%TAI_END_RANGE_CNS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7688, IUER, 'ERM_RES', &
     &                        'Wrong ERM%MJD_END_RANGE_CNS, ERM%TAI_END_RANGE_CNS' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER ) 
              STR3 = MJDSEC_TO_DATE ( ERM%MJD_REF_CNS,       ERM%TAI_REF_CNS,       IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7689, IUER, 'ERM_RES', &
     &                        'Wrong ERM%MJD_REF_CNS, ERM%TAI_REF_CNS' )
                   RETURN
              END IF
              WRITE  ( 23, 250 ) 'Beg', ERM%MJD_BEG_RANGE_CNS, ERM%TAI_BEG_RANGE_CNS, STR1(1:19)
              WRITE  ( 23, 250 ) 'End', ERM%MJD_END_RANGE_CNS, ERM%TAI_END_RANGE_CNS, STR2(1:19)
              WRITE  ( 23, 250 ) 'Ref', ERM%MJD_REF_CNS,       ERM%TAI_REF_CNS,       STR3(1:19)
 250          FORMAT ( 'ERM TIM  Trend ',A,': ', I5, 1X, F7.1, 1X, A )
         END IF
         IF ( ERM%CNS_MEAN_SIGMA(J1) > 0.0D0 ) THEN
              WRITE  ( 23, 260 ) J1, 'MEAN', ERM%CNS_MEAN_SIGMA(J1), ERM%CNS_MEAN_RTP(J1), &
     &                           CNS_UNIT(0)
 260          FORMAT ( 'ERM CNS  Icmp: ', I1, ' Cnstr   ', A, ' Sigma: ', 1PD12.5, 1X, &
     &                 ' Rhs: ', 1PD14.7, 1X, A )
         END IF
         IF ( ERM%CNS_RATE_SIGMA(J1) > 0.0D0 ) THEN
              WRITE  ( 23, 260 ) J1, 'RATE', ERM%CNS_RATE_SIGMA(J1), ERM%CNS_RATE_RTP(J1), &
     &                           CNS_UNIT(1)
         END IF
!
         CALL CLRCH ( PAR_NAM )
         DO 430 J3=1-ERM%DEGREE(J1),ERM%NKNOTS(J1)
            IF ( J3 .LE. 0 ) THEN
                 CALL ERR_PASS ( IUER, IER ) 
                 NOD_DATE = MJDSEC_TO_DATE ( ERM%MJD_BEG, ERM%TAI_BEG, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7690, IUER, 'ERM_RES', &
     &                   'Wrong ERM%MJD_BEG, ERM%TAI_BEG' )
                      RETURN
                 END IF
                 ERM_MJD_OBS = ERM%MJD_BEG 
                 ERM_TAI_OBS = ERM%TAI_BEG 
               ELSE
                 CALL ERR_PASS ( IUER, IER ) 
                 NOD_DATE = JD_TO_DATE ( ERM%TIM(J3,J1)/86400.0D0 + &
     &                                   J2000__JD, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7691, IUER, 'ERM_RES', 'Wrong ERM%TIM' )
                      RETURN
                 END IF
                 ERM_MJD_OBS = J2000__MJD 
                 ERM_TAI_OBS = 43200.0D0 + ERM%TIM(J3,J1)
                 IDAY = ERM_TAI_OBS/86400.0D0
                 IF ( IDAY > 0 ) THEN
                      ERM_MJD_OBS = ERM_MJD_OBS + IDAY
                      ERM_TAI_OBS = ERM_TAI_OBS - IDAY*86400.0D0
                 END IF
            END IF
!
! --------- Compute apriori EOP on the epoch of the knot
!
            IER = 0
            CALL VTD_MOMENT ( VTD%SOU(1)%IVS_NAME, ERM_MJD_OBS, ERM_TAI_OBS, VTD, IER )
            IF ( IER .EQ. 0 ) THEN
!
! -------------- Transform the EOP to radians. NB the EOP order
!
                 ERM_APR(1) = VTD%MOM%YPL
                 ERM_APR(2) = VTD%MOM%XPL
                 ERM_APR(3) = VTD%MOM%UT1_M_TAI*UT1__TO__E3
               ELSE
!
! -------------- It may happend we compute the EOP in future and no apriori EOP
! -------------- was specified
!
                 ERM_APR = 0.0D0
            END IF
!
            WRITE ( UNIT=PAR_NAM, FMT='("ERM ",I1,4X,I5,5X)' ) J1, J3
            IF ( J3 < ERM%NKNOTS(J1) ) THEN
                 IND_PAR(J3,J1) = LTM_DIF ( 1, L_PAR, C_PAR, PAR_NAM )
                 IF ( IND_PAR(J3,J1) > 0 ) THEN
!
! ------------------- Aga. We found it. Print the estimate
!
                      WRITE ( 23, 270 ) PAR_NAM(1:14), NOD_DATE(1:21), &
     &                                  ERM_APR(J1), EST_VEC(IND_PAR(J3,J1)), &
     &                        DSQRT( COV_MAT(LOCS(IND_PAR(J3,J1),IND_PAR(J3,J1))) )
 270                  FORMAT ( 'ERM EST  Par:  ', A, '  Epc: ', A, ' Apr: ', 1PD16.9, &
     &                            ' Adj: ',1PD16.9, ' Err: ', 1PD11.5 )
                    ELSE
                      CALL ERR_LOG ( 7692, IUER, 'ERM_RES', 'Trap of '// &
     &                              'internal control: ERM parameters '//PAR_NAM// &
     &                              ' was not found in the parameters list' )
                      RETURN
                 END IF
              ELSE
                 WRITE ( 23, 270 ) PAR_NAM(1:14), NOD_DATE(1:21), &
     &                             ERM_APR(J1), 0.0D0, 0.0D0
            END IF
 430     CONTINUE
!
         IF ( ERM%DEGREE(J1) > 0 ) THEN
              DO 440 J4=1-ERM%DEGREE(J1),ERM%NKNOTS(J1)-1
                 DO 450 J5=J4+1,J4+ERM%DEGREE(J1)
                    IF ( J5 > ERM%NKNOTS(J1)-1 ) GOTO 450
                    WRITE ( UNIT=PAR_NAM1, FMT='("ERM ",I1,4X,I5,5X)' ) J1, J4
                    WRITE ( UNIT=PAR_NAM2, FMT='("ERM ",I1,4X,I5,5X)' ) J1, J5
!
                    IND11 = LOCS(IND_PAR(J4,J1),IND_PAR(J4,J1))
                    IND12 = LOCS(IND_PAR(J4,J1),IND_PAR(J5,J1))
                    IND22 = LOCS(IND_PAR(J5,J1),IND_PAR(J5,J1))
                    IF ( COV_MAT(IND11) > COVMAT_EPS .AND. &
     &                   COV_MAT(IND22) > COVMAT_EPS       ) THEN
                         CORR  = COV_MAT(IND12)/ &
     &                      ( DSQRT( COV_MAT(IND11) ) * DSQRT( COV_MAT(IND22) ) )
!
                      ELSE
                         CORR = 0.0D0
                    END IF
                    WRITE  ( 23, 280 ) PAR_NAM1(1:14), PAR_NAM2(1:14), CORR
 280                FORMAT ( 'ERM COV  Par1: ', A, '  Par2: ', A, &
     &                          ' Corr: ', F8.5 )
 450             CONTINUE
 440          CONTINUE
         END IF
 410  CONTINUE
      WRITE  ( 23, 290 ) 
 290  FORMAT ( 'ERM_SECTION END' )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ERM_RES !#!
