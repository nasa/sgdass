      SUBROUTINE SPE_CNST ( CNS_TYPE, L_PAR, C_PAR, SPE, B3DOBJ, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPE_CNST ... was reserved for the future 25-FEB-2005       *
! *                                                                      *
! *  ### 29-MAY-2023    SPE_CNST  v1.0 (c)  L. Petrov   30-MAY-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'prfil.i'
      INCLUDE   'fast.i'
      INCLUDE   'cnstr.i'
      INTEGER*4  L_PAR, IUER
      TYPE ( B3D__STRU   ) :: B3DOBJ
      TYPE ( CNSTR__STRU ) :: CNSTROBJ
      TYPE ( SPE__TYPE   ) :: SPE(L_SPE)
      INTEGER*4    M_CON
      PARAMETER  ( M_CON = 6  )
      LOGICAL*4  GLOBAL_FLAG
      CHARACTER  CNS_TYPE*(*), C_PAR(L_PAR)*(*)
      CHARACTER  CNS_ABR*8, STA_NAM*8, CNS_NAME*32, CNS_UNIT*8, &
     &           C_STA(MAX4_SIT,0:M__SPD)*8, OUT*4096
      REAL*8     SIG(M__SPE), CONS_EQU(M_GPA,M__SPN+M__SPD), NORM, TIM_ARG
      CHARACTER  C_CMP(3)*6
      DATA       C_CMP &
     &           / &
     &             'XBSPLN', & ! 1
     &             'YBSPLN', & ! 2
     &             'ZBSPLN'  & ! 3
     &           / 
      LOGICAL*1  FL_DEBUG
      DATA       GLOBAL_FLAG / .TRUE. /
      REAL*8     SIG__MIN, SPAN__MIN
      PARAMETER  ( SIG__MIN  =     1.0D-30 )
      PARAMETER  ( SPAN__MIN = 50000.0D0   )
      INTEGER*4    M_STP
      PARAMETER  ( M_STP = 6 ) ! the number of steps within a span between consecutive knots
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, IR, IE, KNOT, IPAR_KNOT1, IEQU, &
     &           I_STA, L_STA(0:M__SPD), IER
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF
      REAL*8,    EXTERNAL :: BSPL_VAL, BSPL_DER, BSPL_DR2
!
      FL_DEBUG = .FALSE.
!
      L_STA = 0
      DO 410 J1=1,L_SPE
         DO 420 J2=0,M__SPD
            IF ( SPE(J1)%CNS_DER_SIGMA(J2) > SIG__MIN ) THEN
                 IF ( FL_DEBUG ) THEN
                      WRITE  ( 6, 210 ) J1, SPE(J1)%STATION, C_CMP(J2), J2, &
     &                                  SPE(J1)%CNS_DER_SIGMA(J2),  SPESOL(J1)%L_NOD
 210                  FORMAT ( 'SPE_CNST J1= ', I2, ' Sta: ', A, ' Cmp: ', A , ' Der: ', I1, &
     &                         ' Sig: ', 1PD12.4, ' L_nod: ', I2 )
                 END IF
                 DO 430 J3=1,3 ! Over X, Y and Z components of coordinates
                    CONS_EQU = 0.0D0
                    IEQU     = 0
                    DO 440 J4=1,SPESOL(J1)%L_NOD-1
                       IF ( FL_DEBUG .AND. J3 == 1 ) THEN
                            WRITE  ( 6, 220 ) J4, SPE(J1)%TIM(J4), SPE(J1)%MULT(J4)
 220                        FORMAT ( 'SPE_CNST I_nod= ', I4, ' Tim: ', F12.0, ' Mult: ', I1 )
                       END IF
!
                       IF ( SPE(J1)%TIM(J4+1) - SPE(J1)%TIM(J4) > SPAN__MIN ) THEN
                            I_STA = ADD_CLIST ( MAX4_SIT, L_STA(J2), C_STA(1,J2), SPE(J1)%STATION, IER )
                            IEQU = IEQU + 1
                            IPAR_KNOT1 = -1
                            DO 450 J5=1,L_PAR
                               IF ( C_PAR(J5)(10:15) .EQ. C_CMP(J3) ) THEN
!
! --------------------------------- Check the station
!
                                    IF ( C_PAR(J5)(1:8) == SPE(J1)%STATION ) THEN
!
! -------------------------------------- Decode the knot index
!
                                         CALL CHIN ( C_PAR(J5)(17:20), KNOT )
                                         IF ( KNOT == 1 ) THEN
                                              IPAR_KNOT1 = J5
                                         END IF
                                    END IF
                               END IF
 450                        CONTINUE 
!
                            IF ( IPAR_KNOT1 < 1 ) THEN
                                 WRITE ( 6, * ) 'knot: ', J4
                                 CALL ERR_LOG ( 8761, IUER, 'SPE_CNST', 'Trap of internal '// &
     &                               'control cannot find spline parameter for station '// &
     &                                SPE(J1)%STATION//' component '//C_CMP(J3) )
                                 RETURN 
                            END IF
!
                            DO 460 J6=1,M_STP
                               TIM_ARG = SPE(J1)%TIM(J4) + (J6-1)*(SPE(J1)%TIM(J4+1) - SPE(J1)%TIM(J4))/(M_STP-1)
                               IF ( J6 == 1     ) TIM_ARG = TIM_ARG + SPAN__MIN
                               IF ( J6 == M_STP ) TIM_ARG = TIM_ARG - SPAN__MIN
                               IF ( FL_DEBUG .AND. J3 == 1 ) THEN
                                    WRITE  ( 6, 230 ) J6, TIM_ARG
 230                                FORMAT ( 'SPE_CNST J6= ', I2, '  Tim_arg: ', F12.0 )
                               END IF
!
                               DO 470 J7=1-SPE(J1)%DEGREE,SPESOL(J1)%L_NOD-1
                                  IF ( J2 == 0 ) THEN
                                       CONS_EQU(IPAR_KNOT1-1+J7,IEQU) = &
     &                                          BSPL_VAL ( SPESOL(J1)%L_NOD, SPESOL(J1)%NOD_ARR, &
     &                                                     SPESOL(J1)%DEGREE, J7, TIM_ARG )
                                       CNS_UNIT = 'meter'
                                    ELSE IF ( J2 == 1 ) THEN
                                       CONS_EQU(IPAR_KNOT1-1+J7,IEQU) = &
     &                                          BSPL_DER ( SPESOL(J1)%L_NOD, SPESOL(J1)%NOD_ARR, &
     &                                                     SPESOL(J1)%DEGREE, J7, TIM_ARG )
                                       CNS_UNIT = 'm/yr'
                                    ELSE IF ( J2 == 2 ) THEN
                                       CONS_EQU(IPAR_KNOT1-1+J7,IEQU) = &
     &                                          BSPL_DR2 ( SPESOL(J1)%L_NOD, SPESOL(J1)%NOD_ARR, &
     &                                                     SPESOL(J1)%DEGREE, J7, TIM_ARG )
                                       CNS_UNIT = 'm/yr^2'
                                  END IF
                                  IF ( FL_DEBUG .AND. J3 == 1 .AND. CONS_EQU(IPAR_KNOT1-1+J7,IEQU) .NE. 0.0D0 ) THEN
                                       WRITE  ( 6, 240 ) j2, J7, IEQU, IPAR_KNOT1-1+J7, &
     &                                                   C_PAR(IPAR_KNOT1-1+J7), &
     &                                                   CONS_EQU(IPAR_KNOT1-1+J7,IEQU)
 240                                   FORMAT ( 'SPE_CNST Deg: ', i1, ' J7= ', I2, ' Iequ: ', I4, &
     &                                           ' Ipar: ', I6, ' C_par: ', A, ' equ: ', 1PD12.4 )
                                  END IF
 470                           CONTINUE 
 460                        CONTINUE 
                       END IF
 440                CONTINUE 
!
                    CNS_ABR(1:2) = 'SD'
                    CALL INCH ( J2, CNS_ABR(3:3) )
                    CNS_ABR(4:4) = '_'
                    CNS_ABR(5:5) = C_CMP(J3)(1:1)
                    CALL INCH      ( J1, CNS_ABR(6:8) )
                    CALL CHASHR        ( CNS_ABR(6:8) )
                    CALL BLANK_TO_ZERO ( CNS_ABR(6:8) )
!!   write ( 6, * ) ' j1/j2/j3= ', j1, j2, j3, ' CNS_ABR = ', CNS_ABR ! %%%%%%%%%%%%%%%
                    CNS_NAME = 'B-spline der '//CNS_ABR(3:3)//' '//CNS_ABR(5:5)//' '//SPE(J1)%STATION
!
! ----------------- Normalize the equation of constraints
!
                    DO 480 J8=1,IEQU
!
! -------------------- Insert information about constraint, name, description,
! -------------------- abbreviation, right hand side, reciprocal weight (sigma), type
! 
                       CALL ERR_PASS ( IUER, IER )
                       CALL ADDCNS_NAM ( CNS_ABR, J8, CNS_NAME, CNS_UNIT, &
     &                                   0.0D0, SPE(J1)%CNS_DER_SIGMA(J2), GLOBAL_FLAG, &
     &                                   CNSTROBJ, IER )
                       IF ( IER .NE. 0 ) THEN
                            CALL ERR_LOG ( 8762, IUER, 'SPE_CNST', 'Error in '// &
     &                          'an attempt to put information about '// &
     &                          'constraint on Bspline '//CNS_ABR(5:5)//' derivative '// &
     &                          'for '//SPE(J1)%STATION//' into CNSTROBJ' )
                            RETURN
                       END IF
!
                       CALL NORM_VEC ( L_PAR, CONS_EQU(1,J8), NORM )
                       DO 490 J9=1,L_PAR
                          IF ( CONS_EQU(J9,J8) .NE. 0.0D0 ) THEN
                               CALL ERR_PASS ( IUER, IER )
                               CALL ADDCNS_EQU ( CNS_ABR, J8, J9, CONS_EQU(J9,J8), &
     &                                           GLOBAL_FLAG, CNSTROBJ, IER )
                               IF ( IER .NE. 0 ) THEN
                                    CALL ERR_LOG ( 8763, IUER, 'SPE_CNST', &
     &                                  'Failure in putting a coefficient of an '// &
     &                                  'equation of Constraint on Bspline and '// &
     &                                  'pos. for '//SPE(J1)%STATION//' into CNSTROBJ' )
                                    RETURN
                               END IF
                          END IF
 490                   CONTINUE 
 480                CONTINUE 
 430             CONTINUE 
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( FL_DEBUG ) THEN
           WRITE ( 6, '(A)' ) 'SPE_CNST Finished'
           CALL FLUSH  ( 6 ) 
      END IF
      DO 4100 J10=0,M__SPD
         IF ( L_STA(J10) .GT. 0 ) THEN
!
! ----------- Sorting the list of stations
!
              CALL SORT_CH ( L_STA(J10), C_STA(1,J10) )
!
! ----------- Split the list into a line
!
              CALL CLRCH   ( OUT )
              CALL LIST_TO_LINE ( L_STA(J10), C_STA(1,J10), ' ', OUT )
              IR = L_STA(J10)/8
              IF ( L_STA(J10) > IR*8 ) IR = IR + 1
              WRITE ( 23, 110 ) L_STA(J10), J10
 110          FORMAT ( 'SPE_CNST: ', I3, &
     &                 ' stations participated in constraints to B-spline ', I1, '-derivative' )
              DO 4110 J11=1,IR
                 IF ( J11 < IR ) THEN
                      IE = 72
                    ELSE
                      IE = (L_STA(J10) - (IR-1)*8)*9
                 END IF
                 CALL WRITE_LONG ( 23, 72, OUT )
 4110         CONTINUE 
              WRITE ( 23, '(A)' ) ' '
         END IF
 4100 CONTINUE 
!
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPE_CNST  !#!#
