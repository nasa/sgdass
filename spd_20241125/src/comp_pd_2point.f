#define SERIAL
      SUBROUTINE  COMP_PD_2POINT ( ISTL, NMOD, N_DEL, SPD_4D, SPD_2P, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine COMP_PD_2POINT 
! *                                                                      *
! * ### 07-NOV-2014  COMP_PD_2POINT  v1.2 (c) L. Petrov  28-DEC-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'spd.i'
      INTEGER*4  IFMT, ISTL, NMOD, N_DEL, IUER
      TYPE     ( SPD_4D__TYPE ) :: SPD_4D
      TYPE     ( SPD_2P__TYPE ) :: SPD_2P(N_DEL)
      REAL*8     POI_VEC_XYZ(3), POI_VEC_HLP(3), DIST
      REAL*4     ARGS(4), XI(SPD__MLEV), REFR(1-SPD__MDEG:SPD__MLEV,SPD__MREF)
      REAL*8     HEI_TOA
      PARAMETER  ( HEI_TOA = 80000.0D0 )
      LOGICAL*1  FL_ERROR
      INTEGER*4  INDS(4), NTHR, NTHR_SAVED, J1, J2, J3, J4, IER 
      CHARACTER  NUM_THR_STR*12, STR*128
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REAL*4,    EXTERNAL :: VAL_4D_BSPLE3_R4, SPL4_INT
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
#ifdef GNU
      INTEGER*4, EXTERNAL     :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
#else
      ADDRESS__TYPE, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
#endif
!
      FL_ERROR = .FALSE.
      DIST = 0.0
#ifndef SERIAL
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
!$OMP PARALLEL DO IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J1, J2, J3, J4, POI_VEC_XYZ, POI_VEC_HLP, XI, REFR, &
!$OMP&             INDS, ARGS, DIST, IER ), &
!$OMP&   SCHEDULE ( STATIC )
#endif
      DO 410 J1=1,N_DEL
         IF ( FL_ERROR ) GOTO 410
         IF ( ISTL == SPD__SAT ) THEN
              DIST = DSQRT ( (SPD_2P(J1)%COO_EMI(1) - SPD_2P(J1)%COO_REC(1))**2 + &
     &                       (SPD_2P(J1)%COO_EMI(2) - SPD_2P(J1)%COO_REC(2))**2 + &
     &                       (SPD_2P(J1)%COO_EMI(3) - SPD_2P(J1)%COO_REC(3))**2   )
         END IF
         DO 420 J2=1,SPD__MLEV
            IF ( ISTL == SPD__2P ) THEN
                 POI_VEC_XYZ = SPD_2P(J1)%COO_REC + &
     &                 (J2-1)*(SPD_2P(J1)%COO_EMI - SPD_2P(J1)%COO_REC)/(SPD__MLEV-1)
               ELSE IF ( ISTL == SPD__SAT ) THEN
                 POI_VEC_XYZ = SPD_2P(J1)%COO_REC + &
     &                 (J2-1)*(SPD_2P(J1)%COO_EMI - SPD_2P(J1)%COO_REC)/(SPD__MLEV-1)* &
     &                 HEI_TOA/DIST
            END IF
            CALL XYZ_TO_HLP ( SPD_4D, POI_VEC_XYZ, POI_VEC_HLP )
            CALL ERR_PASS ( IUER, IER )
            CALL SPD_4D_GET_INDS ( SPD_4D, SPD_2P(J1)%MJD, SPD_2P(J1)%TAI, &
     &                             POI_VEC_HLP, ARGS, INDS, IER )
            IF ( IER .NE. 0 ) THEN
!$OMP            CRITICAL
                 DIST = DSQRT ( (SPD_2P(J1)%COO_EMI(1) - SPD_2P(J1)%COO_REC(1))**2 + &
     &                          (SPD_2P(J1)%COO_EMI(2) - SPD_2P(J1)%COO_REC(2))**2 + &
     &                          (SPD_2P(J1)%COO_EMI(3) - SPD_2P(J1)%COO_REC(3))**2   )
                 WRITE ( 6, * ) ' J1= ', J1, ' J2= ', J2, ' ISTL= ', ISTL
                 WRITE ( 6, * ) ' POI_VEC_XYZ= ', POI_VEC_XYZ
                 WRITE ( 6, * ) ' POI_VEC_HLP= ', POI_VEC_HLP
                 WRITE ( 6, * ) ' COO_REC= ', SPD_2P(J1)%COO_REC
                 WRITE ( 6, * ) ' COO_EMI= ', SPD_2P(J1)%COO_EMI
                 WRITE ( 6, * ) ' DIST= ', DIST, ' HEI_TOA= ', HEI_TOA
                 CALL CLRCH (     STR )
                 CALL INCH  ( J1, STR )
                 CALL ERR_LOG ( 6181, IUER, 'COMP_PD_2POINT', 'Error in '// &
     &               'computing indices for interpolation for point # '// &
     &                STR )
                 FL_ERROR = .TRUE.
!$OMP            END CRITICAL
                 GOTO 410
            END IF
!
            XI(J2) = DSQRT ( (POI_VEC_XYZ(1)-SPD_2P(J1)%COO_REC(1))**2 + &
     &                       (POI_VEC_XYZ(2)-SPD_2P(J1)%COO_REC(2))**2 + &
     &                       (POI_VEC_XYZ(3)-SPD_2P(J1)%COO_REC(3))**2   )
            DO 430 J3=1,NMOD
               REFR(J2,J3) = VAL_4D_BSPLE3_R4 ( ARGS, SPD_4D%DIMS, INDS, &
     &                           SPD_4D%LEV, SPD_4D%LON, SPD_4D%LAT, &
     &                           SPD_4D%TIM, &
     &                           SPD_4D%REFR(1-SPD__MDEG:SPD_4D%DIMS(1),1-SPD__MDEG:SPD_4D%DIMS(2),1-SPD__MDEG:SPD_4D%DIMS(3),1-SPD__MDEG:SPD_4D%DIMS(4),J3) )
 430        CONTINUE
 420     CONTINUE 
!
! ------ Expand DST_TOT_R4 and DST_WAT_R4 into B-spline basis
!
         SPD_2P(J1)%DEL      = 0.0D0
         SPD_2P(J1)%DEL_RDER = 0.0D0
         SPD_2P(J1)%DEL_EDER = 0.0D0
         DO 440 J4=1,NMOD
            CALL ERR_PASS ( IUER, IER )
            CALL BSPL4_1D_CMP ( SPD__MDEG, 0, SPD__MLEV, XI, &
     &                          REFR(1-SPD__MDEG:SPD__MLEV,J4), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH (     STR )
                 CALL INCH  ( J1, STR )
!$OMP            CRITICAL
                 CALL ERR_LOG ( 6182, IUER, 'COMP_PD_2POINT', 'Error in '// &
     &               'computing expansion of the refractivity along the '// &
     &               'straight line into B-spline basis' )
                 FL_ERROR = .TRUE.
!$OMP            END CRITICAL
            END IF
!
            SPD_2P(J1)%DEL(J4) = SPL4_INT ( SPD__MLEV, XI, SPD__MDEG, &
     &                                      REFR(1-SPD__MDEG:SPD__MLEV,J4), XI(SPD__MLEV) )
            SPD_2P(J1)%DEL_RDER(J4) = REFR(1,J4)
            IF ( ISTL == SPD__2P ) THEN
                 SPD_2P(J1)%DEL_EDER(J4) = REFR(SPD__MLEV,J4)
               ELSE 
                 SPD_2P(J1)%DEL_EDER(J4) = 0.0D0
            END IF
 440     CONTINUE 
 410  CONTINUE 
#ifndef SERIAL
!$OMP END PARALLEL DO
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR_SAVED) )
#endif
      IF ( FL_ERROR ) RETURN 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COMP_PD_2POINT  !#!  
