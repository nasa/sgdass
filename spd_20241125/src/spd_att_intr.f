      SUBROUTINE SPD_ATT_INTR ( NTHR, SPD, MP, MW, MT, P_ARG, PW_ARG, &
     &                          TEM_ARG, BSPL_ATT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_ATT_INTR 
! *                                                                      *
! *  ### 19-SEP-2014  SPD_ATT_INTR  v1.1 (c)  L. Petrov  02-JAN-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      INTEGER*4  NTHR, MP, MW, MT, IUER
      REAL*4     P_ARG(1-SPD__MDEG:MP+SPD__MDEG), &
     &           PW_ARG(1-SPD__MDEG:MW+SPD__MDEG), &
     &           TEM_ARG(1-SPD__MDEG:MT+SPD__MDEG)
      REAL*4     EPS
      PARAMETER  ( EPS = 1.0E-5 )
      REAL*4     BSPL_ATT(1-SPD__MDEG:MP,1-SPD__MDEG:MW,1-SPD__MDEG:MT,SPD%NFRQ)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, DIMS(3), IER
      LOGICAL*1  FL_EXIT
      REAL*8,    EXTERNAL :: ATT_ITU_R13 
!
      DIMS(1) = MP
      DIMS(2) = MW
      DIMS(3) = MT
!
! --- Compute arrays with arguments
!
      DO 410 J1=1,MP
         P_ARG(J1) = LOG(SPD__P_MIN) + (J1-1)*(LOG(SPD__P_MAX) - LOG(SPD__P_MIN))/(MP-1)
 410  CONTINUE 
!
      DO 420 J2=1,MW
         PW_ARG(J2) = LOG(SPD__PW_MIN) + (J2-1)*(LOG(SPD__PW_MAX) - LOG(SPD__PW_MIN))/(MW-1)
 420  CONTINUE 
!
      DO 430 J3=1,MT
         TEM_ARG(J3) = SPD__TEM_MIN + (J3-1)*(SPD__TEM_MAX - SPD__TEM_MIN)/(MT-1)
 430  CONTINUE 
!
! --- Extend the knot sequency
!
      CALL BSPLE3_EXTEND_R4 ( MP, P_ARG )
      CALL BSPLE3_EXTEND_R4 ( MW, PW_ARG )
      CALL BSPLE3_EXTEND_R4 ( MT, TEM_ARG )
!
! --- Compute attenuation and put it in BSPL_ATT array
!
      DO 440 J4=1,SPD%NFRQ
!$OMP    PARALLEL DO & ! IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J5, J6, J7 ), &
!$OMP&             SCHEDULE ( STATIC )
         DO 450 J5=1,MT
            DO 460 J6=1,MW
               DO 470 J7=1,MP
                  BSPL_ATT(J7,J6,J5,J4) = ATT_ITU_R13 ( DBLE(EXP(P_ARG(J7))), &
     &                                        DBLE(EXP(PW_ARG(J6))), DBLE(TEM_ARG(J5)), &
     &                                        SPD%CONF%FRQ_ARR(J4) )
 470           CONTINUE 
 460        CONTINUE 
 450     CONTINUE 
!$OMP    END PARALLEL DO
 440  CONTINUE 
!
! --- Perform B-spline expansion
!
      FL_EXIT = .FALSE.
!$OMP    PARALLEL DO & ! IF ( NTHR > 1 ), &
!$OMP&   PRIVATE ( J8, IER  )
      DO 480 J8=1,SPD%NFRQ
         IF ( FL_EXIT ) GOTO 480
         CALL ERR_PASS ( IUER, IER )
         CALL BSPL4_3D_CMP ( SPD__MDEG, 0, DIMS, P_ARG(1), PW_ARG(1), TEM_ARG(1), &
     &                       BSPL_ATT(1-SPD__MDEG:MP,1-SPD__MDEG:MW,1-SPD__MDEG:MT,J8), IER )
         IF ( IER .NE. 0 ) THEN
!$OMP         CRITICAL
              CALL ERR_LOG ( 4821, IUER, 'SPD_ATT_INTR', 'Failure in '// &
     &            'an attempt to compute coefficients of the 3D '// &
     &            'interpolating spline for atmosphee attenuation' )
              FL_EXIT = .TRUE.
!$OMP         END CRITICAL
         END IF
 480  CONTINUE 
!$OMP    END PARALLEL DO
!
      IF ( .NOT. FL_EXIT ) THEN
           CALL ERR_LOG ( 0, IUER )
      END IF
      RETURN
      END  SUBROUTINE  SPD_ATT_INTR  !#!#
