      SUBROUTINE SPD_3D_COMP ( SPD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_COMP  computes the atmospheric path delay in the   *
! *   3D atmosphere by solving a system of two differential equations of *
! *   wave propagation that in the form of Euler equations of the        *
! *   variational problem that minimizes the propagation time through    *
! *   the continuous heterogeneous media.                                *
! *                                                                      *
! *   SPD_3D_COMP perform computation for each epoch, each station and   *
! *   each element of the 2D azimuthal-elevation grid. This 2D grid is   *
! *   equidistant over azimuth, but not equidistant over elevation.      *
! *   Instead, it equidistant over a ISO_MAP function of elevation in    *
! *   the range [ISO_MAP(+90_deg), ISO_MAP(el_min)].                     *
! *                                                                      *
! *   The atmospheric path delay is computed for one or two components   *
! *   among a) total path delay; b) hydrostatic path delay;              *
! *   c) non-hydrostatic path delay in accordance to                     *
! *   SPD%CONF%SPLIT_MODE parameter.                                     *
! *                                                                      *
! *   In addition to computing to path delay, the atmospheric pressure   *
! *   and air temperature for the station reference point are computed   *
! *   as well.                                                           *
! *                                                                      *
! *   Computation is parallelized over stations using OpenMP.            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     IVRB ( INTEGER*4      ) -- Verbosity parameter.                  *
! *                                IVRB == 0 -- silent mode;             *
! *                                IVRB == 1 -- terse progress messages  *
! *                                             are printed to stdout.   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * SPD ( MET_GRID__TYPE ) -- Object with data structure of the          *
! *                                4D field of meteorological parameters.*
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 27-NOV-2008   SPD_3D_COMP  v3.2 (c)  L. Petrov  19-NOV-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  IUER
      TYPE     ( SPD_3D__TYPE ) :: SPD
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, DIMS(3), INDS(3), IVRB, IER
      REAL*8     ARGS(3), ARGS_MIN(3), EPS
      PARAMETER  ( EPS = 1.0E-6 )
      REAL*8     AZ_STEP, ARG_STEP, ARG_MIN, ARG_MAX, ARG, AZ, EL, &
     &           DEL(SPD__MTYP), OPA(SPD__M_FRQ), TAT(SPD__M_FRQ), &
     &           VEC_GROUND_HLP(3)
      CHARACTER  NUM_THR_STR*12, STR*128
#ifdef GNU
      INTEGER*4  NTHR,    NTHR_SAVED
#else
      ADDRESS__TYPE NTHR, NTHR_SAVED
#endif
      LOGICAL*4  FL_PWT, FL_ERROR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4 
      REAL*8,    EXTERNAL :: VAL_3D_BSPL
      REAL*8,    EXTERNAL :: DEL_ISA, INV_MAP_ISA 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
#ifdef GNU
      INTEGER*4, EXTERNAL     :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
#else
      ADDRESS__TYPE, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
#endif
!
#ifndef SERIAL
!
      NTHR_SAVED = OMP_GET_MAX_THREADS()
      IF ( OMP_IN_PARALLEL() ) THEN
!
! -------- Do serial if we are already in the parallel region
!
           NTHR = 1
         ELSE
           CALL GETENVAR ( 'OMP_NUM_THREADS', NUM_THR_STR )
           IF ( ILEN(NUM_THR_STR) == 0 ) THEN
                NTHR = 1
              ELSE 
                CALL CHIN ( NUM_THR_STR, NTHR )
           END IF
           CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
           NTHR = OMP_GET_MAX_THREADS()
      END IF
#endif
      IF ( IVRB == 6  .OR.  IVRB == 7 ) NTHR = 1
!
! --- Set the min, max and step for the argument of the elevation grid -- 
! --- the ISO_MAP mapping function
!
      IF ( SPD%CONF%N_EL == 1 ) THEN
           ARG_MIN  = 1.0D0
           ARG_STEP = 0.0D0
         ELSE 
           ARG_MIN  = 1.D0
           ARG_MAX  = DEL_ISA( SPD__DEL_MIN_DEG*DEG__TO__RAD)/DEL_ISA(P2I) 
           ARG_STEP = ( ARG_MAX - ARG_MIN )/(SPD%CONF%N_EL - 1)
      END IF
      IF ( SPD%NLON > 1024 ) THEN
           FL_PWT = .FALSE.
         ELSE
           FL_PWT = .TRUE.
      END IF
!
! --- Set the min, max and step for the argument of the azimuthal grid 
!
      IF ( SPD%CONF%N_AZ == 1 ) THEN
           AZ_STEP = 0.0D0
         ELSE 
           AZ_STEP = PI2/(SPD%CONF%N_AZ - 1)
      END IF
!
      DIMS(1) = SPD%NLEV
      DIMS(2) = SPD%NLON
      DIMS(3) = SPD%NLAT
!
      ARGS_MIN(1) = SPD%LEV(1)
      ARGS_MIN(2) = SPD%LON(1)
      ARGS_MIN(3) = SPD%LAT(1)
!
      ALLOCATE ( SPD%AZM%AZIM(SPD%CONF%N_AZ) ) 
      ALLOCATE ( SPD%ELV%ELEV(SPD%CONF%N_EL) ) 
!
! --- Fill arrays of azimuths and elevations
!
      DO 410 J1=1,SPD%CONF%N_AZ
         SPD%AZM%AZIM(J1) = (J1-1)*AZ_STEP
 410  CONTINUE 
      DO 420 J2=1,SPD%CONF%N_EL
         IF ( J2 == 1 ) THEN
              SPD%ELV%ELEV(J2) = P2I
            ELSE 
              ARG = ARG_MIN + ARG_STEP*(J2-1)
              SPD%ELV%ELEV(J2) = INV_MAP_ISA ( ARG )
         END IF
 420  CONTINUE 
      IF ( IVRB .GE. 5 ) THEN
           WRITE ( 6, * ) '  SPD_3D_COMP  147' ; CALL FLUSH ( 6 )
      END IF
!
! --- Cycle over time
!
      IF ( IVRB > 1 ) THEN
           STR = MJDSEC_TO_DATE ( SPD%MJD, SPD%UTC, IER )
           WRITE ( 6, '(A)' ) 'Compute path delays on '//STR(1:19)
      END IF
!
      FL_ERROR = .FALSE.
!
! --- Cycle over stations
!
      IF ( SPD%CONF%TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( %VAL(0) )
      END IF
#ifndef SERIAL
!$OMP PARALLEL DO IF ( NTHR > 1 ), DEFAULT(NONE), &
!$OMP&   PRIVATE ( J4, J5, J6, J7, EL, AZ, DEL, OPA, TAT, &
!$OMP&             INDS, ARGS, VEC_GROUND_HLP, IER ), &
!$OMP&   SHARED ( SPD, DIMS, IVRB, FL_PWT, FL_ERROR, IUER ), &
!$OMP&   SCHEDULE ( STATIC )
#endif
      DO 440 J4=1,SPD%NSTA 
         IF ( ( IVRB == 3 .OR. IVRB == 4 )  .AND.  .NOT.  FL_ERROR ) THEN
!$OMP         CRITICAL
              WRITE ( 6, '(A,I6,A,A)' ) 'Compute path delay for the ',J4, &
     &                    ')th station ', SPD%STA(J4)%NAME
              CALL FLUSH ( 6 )
!$OMP         END CRITICAL
         END IF
!
! ------ Cycle over elements of the azimuthal grid
!
         DO 450 J5=1,SPD%CONF%N_AZ
            AZ = SPD%AZM%AZIM(J5) 
!
! --------- Cycle over elements of the elevation grid
!
            DO 460 J6=1,SPD%CONF%N_EL
               EL = SPD%ELV%ELEV(J6) 
!$OMP          FLUSH (FL_ERROR)
               IF ( .NOT. FL_ERROR ) THEN
!
! ----------------- Compute the slant path delay and (optionally) atmospheric opacity
! ----------------- and atmosphere brightness temperature
!
                    CALL ERR_PASS   ( IUER, IER )
                    CALL SPD_3D_DEL ( SPD, J4, EL, AZ, DEL, OPA, TAT, IVRB, IER )
                    IF ( IER .NE. 0 ) THEN
!$OMP                    CRITICAL
                         WRITE ( 6, * ) 'EL: ', EL, ' AZ = ', AZ
                         CALL ERR_LOG ( 5331, IUER, 'SPD_3D_COMP', &
     &                       'Failure in computing atmosphere path '// &
     &                       'delay for station '//SPD%STA(J4)%NAME )
                         FL_ERROR = .TRUE.
!$OMP                    END CRITICAL
                    END IF
!
                    SPD%STA(J4)%DEL(J6,J5,1:SPD__MTYP) = DEL(1:SPD__MTYP)
                    IF ( SPD%NFRQ > 0 ) THEN
                         DO 470 J7=1,SPD%NFRQ
                            SPD%STA(J4)%OPA(J7,J6,J5) = OPA(J7)
                            SPD%STA(J4)%TAT(J7,J6,J5) = TAT(J7)
 470                     CONTINUE 
                    END IF
               END IF
 860           CONTINUE 
 460        CONTINUE 
 450     CONTINUE 
!
!$OMP    FLUSH (FL_ERROR)
         IF ( .NOT. FL_ERROR ) THEN
!
! ----------- Compute surface atmospheric pressure and 
! ----------- surface air temperature for the J4-th station
!
! ----------- Get HLP coordinates for the station ground point
!
              CALL XYZ_TO_HLP ( SPD, SPD%STA(J4)%COO_CFS, VEC_GROUND_HLP )
!
              CALL ERR_PASS ( IUER, IER )
              CALL SPD_GET_INDS ( SPD, VEC_GROUND_HLP, ARGS, INDS, DIMS, IER )
              IF ( IER .NE. 0 ) THEN
!$OMP              CRITICAL
                   CALL ERR_LOG ( 5312, IUER, 'SPD_3D_COMP', 'Trap of '// &
     &                 'internal control in an attempt to compute '// &
     &                 'the pivotal index' )
                   FL_ERROR = .TRUE.
!$OMP              END CRITICAL
              END IF
!
!$OMP         FLUSH (FL_ERROR)
              IF ( FL_PWT .AND. .NOT. FL_ERROR ) THEN
!
! ---------------- Interpolate surface atmospheric pressure
!
                   SPD%STA(J4)%SUR_PRS = &
     &                 VAL_3D_BSPL ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                SPD%LEV(1), SPD%LON(1), SPD%LAT(1), SPD%SPR_3D )
!
! ---------------- Interpolate surface partial pressure of warter pressure
!
                   SPD%STA(J4)%SUR_PWP = &
     &                 VAL_3D_BSPL ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                SPD%LEV(1), SPD%LON(1), SPD%LAT(1), SPD%SPW_3D )
!          
! ---------------- Interpolate surface air temperature
!
                   SPD%STA(J4)%SUR_TEM = &
     &                 VAL_3D_BSPL ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                                SPD%LEV(1), SPD%LON(1), SPD%LAT(1), SPD%STM_3D )
              END IF
         END IF
 440  CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
#endif
      IF ( FL_ERROR ) RETURN 
#ifndef SERIAL
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR_SAVED) )
#endif
      IF ( SPD%CONF%TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Computation of path delays: '//STR(1:I_LEN(STR)-5)
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_COMP  !#!  
