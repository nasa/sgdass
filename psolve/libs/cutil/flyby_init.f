      SUBROUTINE FLYBY_INIT ( STA, SRC, NUTS, NUTD, EOPD, PLTMO, TIME0X, SVEL, &
     &                        STA2, SRC2, VEL2, PLTMO2, PLATE_SCALE, AXO, &
     &                        ECCMAP, MGRMAP, METRIC_MAP )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  FLYBY_INIT PROGRAM SPECIFICATION
!
! 1.1 Set up the parameter mapping system.  Subroutines SSTAP, SSOUC,
!     GNUTS, GNUTD, SEROT, STECT and others are called to open and to read
!     the substitution files.
!
! 1.2 REFERENCES:
!
! 2.  FLYBY_INIT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STA, SRC, NUTS, NUTD, EOPD, PLTMO, SVEL, STA2, VEL2, &
     &              SRC2, PLTMO2, AXO, ECCMAP, MGRMAP, METRIC_MAP
      REAL*8        TIME0X, PLATE_SCALE, FJDCT_END_BSP
!
! EOPD - Name of earth orientation substitution file
! NUTD - Name of daily nutation substitution file
! NUTS - Name of nutation series substitution file
! SRC - Name of source position substitution file
! STA - Name of station position substitution file
! PLTMO - Name of plate motion model
! SVEL - Name of station velocity substitution file
! time0x - site ref date parameter
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc4.i'
      INCLUDE 'fbcom.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'oborg.i'
      INCLUDE 'flyby.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: /batch/domapp
!       CALLED SUBROUTINES: sstap,ssouc,gnuts,gnutd,serot,stect,svelp
!
! 3.  LOCAL VARIABLES
!
      REAL*8        VSUBXYZ(3,MAX_STA)
      CHARACTER     FILE_NAME*128, VELOCITY_FILE_NAME*128, STR*32
      LOGICAL*2     KFBDSP, EQUAL
      LOGICAL*4     MGR_WARNING
      INTEGER*2     J, I, TRIMLEN
      INTEGER*4     J1, J2, J3, J4, J5, J6, J7, NOBS_STA(MAX_ARC_STA), M_ELM
      REAL*8        JFIRST_OBS, JLAST_OBS, WORK_ARR(M__PSV), FJD_BEG, FJD_END
      REAL*8        VALTMP_PSV(M__PSV,3,MAX_ARC_STA)
      DATA KFBDSP / .FALSE. /
      INTEGER*4,    EXTERNAL :: LTM_DIF
      INTEGER*4     IUER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! FILE_NAME - File name for subroutine calls
! I,J - Loop indices
! KFBDSP - True if flyby information is to be displayed
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  900620  Add site ref date parm to  call to STECT
!   AEE  910307  Added STATION VELOCITY section and made it so that
!                either Station Velocity or Plate_Model can be used
!   DG   910701  Changed E.O. differences to E.O. values from E.O. mod
!                file and changed call to SEROT
!   :93.12.20:jwr: Change at the time that spline fitting was introduced.
!                Variable 'max_flyby_eop_values' introduced.
!   PET  980087  Correected a bug: date of the first observation was taken
!                from TATM1 (first atmosphere epoch) for call GNUTD in the
!                previous version. It caused abend when TATM was not set up.
!                Added correct call of OBSTM for getting the date of the
!                first observation.
!   PET  990103  Changed the logic of determination of path to the modification
!                files: if the first cahracter of the file is "/" then a file
!                name is interpreted as a file name with path. Overwise a path
!                name to the SAVE directory is put before it.
!   PET  990413  Deleted a line which updated vaxof -- it caused a bug in some
!                cases.
!   PET  1999.05.06  Added call of FLYBY_MAP_INIT for initialization of
!                    a data structure described in ../include/flyby.i
!   PET  1999.10.15  Added suuport of a new argument ECCMAP -- mapping
!                    eccentricity file.
!   PET  2000.09.25  Added suuport of a new argument MGRMAP -- mapping
!                    mean gradient file.
!   PET  2001.01.12  Added suuport of a new argument METRIC_MAP
!   PET  2002.12.17  Added support of new mapping: mapping of position
!                    variations defined in the external model files
!   PET  2002.12.27  Fixed the bug in support of mapping of site position
!                    vatiations: the previous version applied only the last
!                    model ignoring all others if more than one position
!                    variations model was specified.
!   JWR  2004.05.03  Sleeping bug affecting character substrings in numerous
!                    places fixed.
!   PET  2004.10.11  Changed interface to GAXOF
!
! 5.  FLYBY_INIT PROGRAM STRUCTURE
!
! --- Initialization
!
      DO I=1,MAX_ARC_STA
         SUBAX(I) = 0.d0
         AXDIF(I) = 0.d0
         DO J=1,3
            SITDIF(J,I)=0.0D0
            NVSITEC(J,I)=VSITEC(J,I)
            NVSITEV(J,I)=VSITEV(J,I)
         END DO
      END DO
      NUMAXOF = 0
!
      DO I=1,MAX_ARC_SRC
         DO J=1,2
            STRDIF(J,I)=0.0D0
            NVSTARC(J,I)=VSTARC(J,I)
         END DO
      END DO
!
      DO I=1,4
         DPRIN(I)=0.0D0
         DDECA(I)=0.0D0
         DANNU(I)=0.0D0
         DSEMA(I)=0.0D0
         D122D(I)=0.0D0
         DSEMM(I)=0.0D0
      END DO
!
      DPREC=0.0D0
      NVPREC=VPREC
!
      DO I=1,3
         DNUT(I)=5.0D0*I
         DDPSI(I)=0.0D0
         DDEPS(I)=0.0D0
      END DO
!
      DO I=1,MAX_FLYBY_EOP_VALUES
         UT1PTV(I)=0.0D0
         WOBXXV(I)=0.0D0
         WOBYYV(I)=0.0D0
         WBXSIG(I)=0.0D0
         WBYSIG(I)=0.0D0
         UT1SIG(I)=0.0D0
         WXYCOR(I)=0.0D0
         WXUCOR(I)=0.0D0
         WYUCOR(I)=0.0D0
      END DO
!
      FJDCT_BEG_PSV = 0.0D0
      CALL NOUT_R8 ( M__PSV, TIM_PSV )
      M_ELM = M__PSV*MAX_ARC_STA*3
      CALL NOUT_R8 ( M_ELM, VALLIN_PSV )
      CALL NOUT_R8 ( M_ELM, VALSPL_PSV )
      CALL NOUT_R8 ( M_ELM, COESPL_PSV )
!
      FL_PSV_LIN = .FALSE.
      FL_PSV_SPL = .FALSE.
!
      KSTAP = .FALSE.
      KAXOP = .FALSE.
      KSOUC = .FALSE.
      KNUTS = .FALSE.
      KNUTD = .FALSE.
      KEROT = .FALSE.
      KSVEL = .FALSE.
      KSTAM = .FALSE.
      KECC  = .FALSE.
      KMGR  = .FALSE.
      KMET  = .FALSE.
!
! --- Get the time of the first and the last observation (Julian date)
!
      CALL OBSTM ( JFIRST_OBS, JLAST_OBS )
!
      CALL USE_PARFIL ( 'ORC' )
!
! --- Axis offsets
!
      IF ( AXO(1:4) .NE. 'NONE'  .AND.  AXO(1:1) .NE. ' ' ) THEN
           IF ( AXO(1:1) .EQ. '/' ) THEN
                FILE_NAME = AXO
              ELSE
                FILE_NAME = PRE_SAV_DIR(:PRE_SV_LEN)//AXO
           END IF
           IUER = -1
           CALL GAXOF ( FILE_NAME, NUMAXOF, LAXNAM_CHR, SUBAX, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL FERR ( INT2(666), &
     &              'FLYBY_INIT: error in reading global antenna axis '// &
     &              'offset file '//FILE_NAME, INT2(0), INT2(0) )
                WRITE ( 6, * ) 'Abnormal termination'
                CALL EXIT ( 1 )
           END IF
!
! -------- Pick up mod file axis offset values, if any
!
           IF ( NUMAXOF .GT. 0 ) THEN
                DO I=1,NUMSTA
                   DO J=1,NUMAXOF
                      IF ( ISITN_CHR(I) .EQ. LAXNAM_CHR(J) ) THEN
                           AXDIF(I) = (SUBAX(J)/1000.D0) - VAXOF(I)
                      ENDIF
                   ENDDO
                ENDDO
           ENDIF
!
           KAXOP = .TRUE.
      END IF
!
! --- Station positions
!
      STAMOD2 = STA2
      IF ( STA(1:4) .NE. 'NONE'   .AND.  STA(1:1) .NE. ' ' ) THEN
           IF ( STA(1:1) .EQ. '/' ) THEN
                FILE_NAME = STA
              ELSE
                FILE_NAME = PRE_SAV_DIR(:PRE_SV_LEN)//STA
           END IF
           STAMOD1 = STA
           CALL SSTAP ( LSINAM, SUBXYZ, SITDIF, FILE_NAME, KFBDSP, NVSITEC )
           KSTAP = .TRUE.
           KSTAM = .TRUE.
      END IF
!
! --- Source positions
!
      SRCMOD2 = SRC2
      IF ( SRC(1:4) .NE. 'NONE'  .AND.  SRC(1:1) .NE. ' ' ) THEN
           IF ( SRC(1:1) .EQ. '/' ) THEN
                FILE_NAME = SRC
              ELSE
                FILE_NAME = PRE_SAV_DIR(:PRE_SV_LEN)//SRC
           END IF
!
           SRCMOD1 = SRC
           CALL SSOUC ( LSONAM, SUBRD, STRDIF, FILE_NAME, KFBDSP, NVSTARC )
           KSOUC = .TRUE.
      END IF
!
! --- Nutation model (series)
!
      IF ( NUTS(1:4) .NE. 'NONE'  .AND.  NUTS(1:1) .NE. ' ' ) THEN
           IF ( NUTS(1:1) .EQ. '/' ) THEN
                FILE_NAME = NUTS
              ELSE
                FILE_NAME = PRE_SAV_DIR(:PRE_SV_LEN)//NUTS
           END IF
           CALL GNUTS ( DPRIN, DDECA, DANNU, DSEMA, D122D, DSEMM, DPREC, &
     &                  FILE_NAME, KFBDSP, VPREC, NVPREC )
           KNUTS = .TRUE.
      END IF
!
! --- Daily nutation values
!
      IF ( NUTD(1:4) .NE. 'NONE'  .AND.  NUTD(1:1) .NE. ' ' ) THEN
           IF ( NUTD(1:1) .EQ. '/' ) THEN
                FILE_NAME = NUTD
              ELSE
                FILE_NAME = PRE_SAV_DIR(:PRE_SV_LEN)//NUTD
           END IF
           CALL GNUTD ( JFIRST_OBS, DNUT, DDPSI, DDEPS, FILE_NAME, KFBDSP, &
     &                  LNSIG, OBSIG, LNOBCOR )
           KNUTD = .TRUE.
      END IF
!
! --- Earth orientation
!
      IF ( EOPD(1:4) .NE. 'NONE'   .AND.  EOPD(1:1) .NE. ' ' ) THEN
           IF ( EOPD(1:1) .EQ. '/' ) THEN
                FILE_NAME = EOPD
              ELSE
                FILE_NAME = PRE_SAV_DIR(:PRE_SV_LEN)//EOPD
           END IF
           CALL SEROT ( FILE_NAME, KFBDSP )
           KEROT = .TRUE.
      END IF
!
! --- Plate motion model
!
      PLTMOD2 = PLTMO2
      VELMOD2 = VEL2
      IF ( PLTMO2(1:4) .NE. 'NONE'  .AND. &
           PLTMO2(1:1) .NE. ' '     .AND. &
           PLTMO2(1:1) .NE. CHAR(0)       ) THEN
           PLTMOD1 = PLTMO
           CALL STECT ( SITDIF, PLTMO, KFBDSP, NVSITEV, TIME0X, &
     &                  NVSITEC, PLATE_SCALE )
           KSVEL = .TRUE.
           KSTAM = .TRUE.
      ENDIF
!
! --- Station velocity
!
      IF ( SVEL(1:4) .NE. 'NONE'  .AND.  SVEL(1:1) .NE. ' ' ) THEN
           CALL CLRCH ( VELOCITY_FILE_NAME )
           IF ( SVEL(1:1) .EQ. '/' ) THEN
                VELOCITY_FILE_NAME = SVEL
              ELSE
                VELOCITY_FILE_NAME = PRE_SAV_DIR(:PRE_SV_LEN)//SVEL
           END IF
!
           VELMOD1=SVEL
           CALL SVELP ( LSINAM, VSUBXYZ, SITDIF, VELOCITY_FILE_NAME, KFBDSP, &
     &                  NVSITEV, TIME0X, NVSITEC )
           KSVEL = .TRUE.
           KSTAM = .TRUE.
      END IF
!
      IF ( ECCMAP(1:1) .NE. ' '  .AND.  ECCMAP(1:4) .NE. 'NONE' ) THEN
!
! -------- Put to flyby.i old values of eccentricity vector and new values
! -------- from mapping file
!
           IF ( ECCMAP(1:1) .EQ. '/' ) THEN
                FILE_NAME = ECCMAP
              ELSE
                FILE_NAME = PRE_SAV_DIR(:PRE_SV_LEN)//ECCMAP
           END IF
!
           IUER = -1
           CALL GECC ( ECCMAP, JFIRST_OBS, JLAST_OBS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL FERR ( INT2(670), &
     &              'FLYBY_INIT: error in mapping eccentricity file', &
     &               INT2(0), INT2(0) )
                WRITE ( 6, * ) 'Abnormal termination'
                CALL EXIT ( 1 )
           END IF
           KECC  = .TRUE.
      END IF
!
      IF ( MGRMAP(1:1) .NE. ' '  .AND.  MGRMAP(1:4) .NE. 'NONE' ) THEN
!
! -------- Put to flyby.i mean gradients
!
           IF ( MGRMAP(1:1) .EQ. '/' ) THEN
                FILE_NAME = MGRMAP
              ELSE
                FILE_NAME = PRE_SAV_DIR(:PRE_SV_LEN)//MGRMAP
           END IF
!
           MGR_WARNING = G_WARNING
           IUER = -1
           CALL GMGR ( MGRMAP, MGR_WARNING, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL FERR ( INT2(680), 'FLYBY_INIT: error in applying mean '// &
     &              'gradient file', INT2(0), INT2(0) )
                WRITE ( 6, * ) 'Abnormal termination'
               CALL EXIT ( 1 )
           END IF
           KMGR  = .TRUE.
      END IF
!
      IF ( METRIC_MAP .EQ. IERS92__MET    .OR. &
     &     METRIC_MAP .EQ. GRS__MET       .OR. &
     &     METRIC_MAP .EQ. TOPOCNTR__MET       ) THEN
!
           CALL GMET ( METRIC_MAP )
           KMET  = .TRUE.
         ELSE IF ( METRIC_MAP .EQ. NONE__MET ) THEN
           KMET  = .FALSE.
         ELSE IF ( METRIC_MAP(1:1) .EQ. ' '  ) THEN
           KMET  = .FALSE.
         ELSE
           CALL FERR ( INT2(690), 'FLYBY_INIT: Wrong value of METRIC_MAP: '// &
     &          METRIC_MAP, INT2(0), INT2(0) )
           WRITE ( 6, * ) 'Abnormal termination'
           CALL EXIT ( 1 )
      END IF
!
      IF ( N_POSVAR .GT. 0 ) THEN
           DO 410 J1=1,N_POSVAR
              CALL NOUT_R8 ( M_ELM, VALTMP_PSV )
!
! ----------- Compute position variations due to the J1 -th model at the
! ----------- sequence of time epoch TIM_PSV and put the displacements in
! ----------- temporary array VALTMP_PSV
!
              IUER = -1
              CALL MAP_POSVAR ( J1, FJDCT_BEG_PSV, TIM_PSV, VALTMP_PSV, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL FERR ( INT2(700), &
     &                 'FLYBY_INIT: Error in an attempt to '// &
     &                 'compute interpolation polynomial for position '// &
     &                 'variations defined in the external file '//POSVAR_FIL(J1), &
     &                  INT2(0), INT2(0) )
                   WRITE ( 6, * ) 'Abnormal termination'
                   CALL EXIT ( 1 )
              END IF
!
! ----------- Now add displacements due to the J1 -th model to the array of
! ----------- total displacements. There are two arrays for this purpose:
! ----------- one for displacements for linear interpolation and another
! ----------- array for spline interpolation
!
              IF ( POSVAR_INT(J1) .EQ. PSV__LIN ) THEN
                   FL_PSV_LIN = .TRUE.
                   CALL ADD_VV ( M_ELM, VALLIN_PSV, VALTMP_PSV )
                 ELSE IF ( POSVAR_INT(J1) .EQ. PSV__SPL ) THEN
                   FL_PSV_SPL = .TRUE.
                   CALL ADD_VV ( M_ELM, VALSPL_PSV, VALTMP_PSV )
                 ELSE
                   WRITE ( 6, * ) ' J1=',J1,' POSVAR_INT(J1) = ',POSVAR_INT(J1)
                   CALL FERR ( INT2(702), &
     &                 'FLYBY_INIT: Trap of internal control: '// &
     &                 'unknown interpolation mode for external file '// &
     &                  POSVAR_FIL(J1), INT2(0), INT2(0) )
                   WRITE ( 6, * ) 'Abnormal termination'
                   CALL EXIT ( 1 )
              END IF
 410       CONTINUE
!
           IF ( FL_PSV_SPL ) THEN
!
! ------------- For each station for each component compute coefficients of
! ------------- interpolating spline. This interpolating will interpolate
! ------------- displacements of all models for which spline interpolation
! ------------- was specified, except the models for which linear
! ------------- interpolation was specified
!
                DO 420 J2=1,3
                   DO 430 J3=1,INT4(NUMSTA)
                      IUER = -1
                      CALL MAKE_SPLINE ( 3, M__PSV, TIM_PSV, &
     &                                   VALSPL_PSV(1,J2,J3), 0.0D0, 0.0D0, &
     &                                   COESPL_PSV(1,J2,J3), WORK_ARR, IUER )
                      IF ( IUER .NE. 0 ) THEN
                           WRITE ( 6, * ) ' TIM_PSV = ',TIM_PSV
                           CALL ERR_LOG ( 2611, -1, 'FLYBY_INIT', &
     &                         'Error in an attempt to compute coefficients '// &
     &                         'of cubic spline for interpolation of site '// &
     &                         'position variations from external files' )
                           WRITE ( 6, * ) 'Abnormal termination'
                           CALL EXIT ( 1 )
                      END IF
 430               CONTINUE
 420            CONTINUE
           END IF
      END IF
!
      DO 440 J4=1,INT4(NUMSTA)
         STAUSE_BSP(J4) = .FALSE.
 440  CONTINUE
!
      CALL NOUT_R8 ( M__BSP_INT, TIM_BSP )
      CALL NOUT_R8 ( M__BSP_INT*3*INT4(MAX_ARC_STA), VALSPL_BSP )
      CALL NOUT_R8 ( M__BSP_INT*3*INT4(MAX_ARC_STA), COESPL_BSP )
!
      IF ( L_BSP > 0  .AND.  ADR_BSP .NE. 0 ) THEN
!
! -------- Get nominal start ( FJD_BEG ) and nominal stop ( FJD_END ) time epoch
!
           CALL OBSTM ( FJD_BEG, FJD_END )
!
! -------- Compute the first epoch of the time interval for which
! -------- displacements modeled with expansion with the B-spline
! -------- is to be computed
!
           FJDCT_BEG_BSP = FJD_BEG - OVH__PSV/86400.0D0
           FJDCT_END_BSP = FJD_END + OVH__PSV/86400.0D0
!
           DO 450 J5=1,L_BSP
              IUER = -1
              CALL RESPLINE_BSP ( FJDCT_BEG_BSP, FJDCT_END_BSP, L_BSP, J5, &
     &                            %VAL(ADR_BSP), INT4(NUMSTA), ISITN_CHR, &
     &                            TIM_BSP, VALSPL_BSP, COESPL_BSP, &
     &                            STAUSE_BSP, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J5, STR )
                   CALL ERR_LOG ( 2612, -1, 'FLYBY_INIT', &
     &                      'Error in an attempt to compute coefficients '// &
     &                      'of cubic spline for interpolation '// &
     &                      ' position variations modeled '// &
     &                      'with an expansion with the B-spline basis '// &
     &                      'with different knots' )
                   WRITE ( 6, * ) 'Abnormal termination'
                   CALL EXIT ( 1 )
              END IF
 450       CONTINUE
      END IF
!
      IF ( DBNAME_CH(1:1) == '$'  .AND.  ATD_USE == ATD__AVERAGE ) THEN
!
! -------- Compute average temperature
!
           CALL NOUT_R8 ( INT4(MAX_ARC_STA), TEM_AVR  )
           CALL NOUT_I4 ( INT4(MAX_ARC_STA), NOBS_STA )
           CALL ACS_OBSFIL ( 'O' )
           DO 460 J6=1,IDBEND(INT2(1))
              CALL USE_OBSFIL ( IOBSFIL, J6, 'R' )
              TEM_AVR(ISITE(1)) = TEM_AVR(ISITE(1)) + 1
              TEM_AVR(ISITE(2)) = TEM_AVR(ISITE(2)) + 1
              NOBS_STA(ISITE(1)) = NOBS_STA(ISITE(1)) + 1
              NOBS_STA(ISITE(2)) = NOBS_STA(ISITE(2)) + 1
 460       CONTINUE 
           CALL ACS_OBSFIL ( 'C' )
!
           DO 470 J7=1,NUMSTA
              IF ( NOBS_STA(J7) > 0 ) THEN
                   TEM_AVR(J7) = TEM_AVR(J7)/NOBS_STA(J7) + 273.15D0
                 ELSE 
                   TEM_AVR(J7) = 0.D0
              END IF
 470       CONTINUE 
     END IF
!
! --- And at last initialization of internal data structure described in
! --- flyby.i  Interpolation routines use checks status: initialized
! --- or not, and if not, then they put there initial values. It is important
! --- to clear these data structure BEFORE processing the first observation
! --- of the database
!
      CALL FLYBY_MAP_INIT ( )
!
      RETURN
      END  !#!  FLYBY_INIT  #!#
