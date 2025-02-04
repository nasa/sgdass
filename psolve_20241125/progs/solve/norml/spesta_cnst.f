      SUBROUTINE SPESTA_CNST ( CNS_TYPE, L_PAR, C_PAR, SPE, CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPESTA_CNST imposes decorrelation constraints on           *
! *   coefficients of the splines which parameterize non-linear site     *
! *   motion in such a manner that a) the integral of the spline will be *
! *   zero (decorrelation with global site position) or 2) the first     *
! *   moment of the motion modeled by a spline is zero (decorrelation    *
! *   with global site velocity).                                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      CNS_TYPE ( CHARACTER ) -- Constraint type. One of               *
! *                                'SPE_STA' -- 3 equations of the       *
! *                                 integral over the site position      *
! *                                 evolution modeled by a spline being  *
! *                                 zero for X, Y and Z coordinate       *
! *                                 components.                          *
! *                                                                      *
! *                                 OR                                   *
! *                                                                      *
! *                                'SPE_VEL' -- 3 equations of the       *
! *                                 first moment of the site position    *
! *                                 evolution modeled by a spline being  *
! *                                 zero for X, Y and Z coordinate       *
! *                                 components.                          *
! * FL_NN_LISTING ( LOGICAL*4 ) -- If .TRUE. then the list of stations   *
! *                                participated in constraint equations  *
! *                                will be put in the spool-file.        *
! *         L_PAR ( INTEGER*4 ) -- The total number of global parameter. *
! *         C_PAR ( INTEGER*4 ) -- The list of global parameters.        *
! *           SPE ( RECORD    ) -- Array of records defined in           *
! *                                $PSOLVE_ROOT/include/solve.i which    *
! *                                keeps information about spline        *
! *                                parameterization of some stations     *
! *                                position variations estimation.       *
! *                                Dimension: L_SPE.                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    CNSTROBJ  ( RECORD    ) -- Object whcih accumulates information   *
! *                               about constraints: names, coefficient  *
! *                               of constraint equations, etc.          *
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
! *  ### 25-FEB-2005  SPESTA_CNST  v2.1 (c)  L. Petrov  29-MAY-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'prfil.i'
      INCLUDE   'fast.i'
      INCLUDE   'cnstr.i'
      INTEGER*4  L_PAR, IUER
      TYPE ( CNSTR__STRU ) :: CNSTROBJ
      TYPE ( SPE__TYPE   ) :: SPE(L_SPE)
      INTEGER*4    M_CON
      PARAMETER  ( M_CON = 3 )
      LOGICAL*4  GLOBAL_FLAG
      LOGICAL*4  FL_CNS
      CHARACTER  CNS_TYPE*(*), C_PAR(L_PAR)*(*)
      CHARACTER  CNS_ABR*8, STA_NAM*8, CNS_NAME*32, CNS_UNIT*8, C_STA(MAX4_SIT,0:1)*8, &
     &           STR*32, OUT*4096
      REAL*8     CONS_EQU(M_GPA,M_CON), CONS_RHS(M_CON), NORM, SIG, SIGS(0:1), &
     &           TIM_SPAN, TIM_VEL_SEC 
      CHARACTER  C_CMP(M_CON)*6
      LOGICAL*1  FL_DEBUG
      REAL*8     SIG__MIN
      PARAMETER  ( SIG__MIN = 1.D-30 )
      DATA       C_CMP &
     &           / &
     &             'XBSPLN', & ! 1
     &             'YBSPLN', & ! 2
     &             'ZBSPLN'  & ! 3
     &           / 
      DATA       GLOBAL_FLAG / .TRUE. /
      INTEGER*4  J1, J2, J3, J4, J5, J6, L_STA(0:1), I_STA, INOD, IE, IR, IER 
      INTEGER*4   INT4
      REAL*8,    EXTERNAL :: BSPL_INT1_FULL, BSPL_MOM1_FULL 
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF
!
      FL_DEBUG = .FALSE.  ! Flag whether to run this program in the debugging mode
!
! --- Time epoch for site position estimation as time in sec from J2000.0
!
      TIM_VEL_SEC = (SIT_EST_EPOCH-J2000__JD)*86400.0D0
!
! --- First run over the stations which position evolution is modeled by
! --- estimation of the coefficient of the B-spline basis expansion 
!
      L_STA = 0
      DO 410 J1=1,L_SPE
         STA_NAM  = SPE(J1)%STATION
         TIM_SPAN = SPE(J1)%TIM(SPE(J1)%L_NOD) - SPE(J1)%TIM(1)
         DO 420 J2=1,3 ! Over X, Y and Z components of coordinates
!
! --------- Zeroing out equation of conditions
!
            CALL NOUT_R8 ( M_GPA, CONS_EQU(1,J2) )
            CONS_RHS(J2) = 0.D0
            SIGS         = 0.0D0
            FL_CNS = .FALSE.
            DO 430 J3=1,L_PAR
!
! ------------ Check the component name
!
               IF ( C_PAR(J3)(10:15) .EQ. C_CMP(J2) ) THEN
!
! ----------------- Check the station
!
                    IF ( C_PAR(J3)(1:8) == STA_NAM ) THEN
!
! ---------------------- Decode the node index
!
                         CALL CHIN ( C_PAR(J3)(17:20), INOD )
                         IF ( CNS_TYPE == 'SPE_POS' ) THEN
                              SIG     = SPE(J1)%CNS_STA_SIGMA
                              SIGS(0) = SPE(J1)%CNS_STA_SIGMA
                              IF ( SIGS(0) > SIG__MIN ) THEN
                                   FL_CNS = .TRUE.
                                   I_STA = ADD_CLIST ( MAX4_SIT, L_STA(0), C_STA(1,0), &
     &                                                 SPE(J1)%STATION, IER )
                              END IF
!
! --------------------------- Compute the integral of the spline over 
! --------------------------- the interval of arguments ARG(INOD), ARG(INOD+1), 
! --------------------------- ... ARG(INOD + DEGREE + 1)
!
                              CONS_EQU(J3,J2) = &
     &                                BSPL_INT1_FULL ( SPESOL(J1)%L_NOD, &
     &                                                 SPESOL(J1)%NOD_ARR, &
     &                                                 SPESOL(J1)%DEGREE, INOD )/TIM_SPAN
                              IF ( FL_DEBUG .AND. FL_CNS ) THEN
                                   WRITE  ( 6, 210 ) 'STA', SPE(J1)%STATION, C_CMP(J2), INOD, J3, &
     &                                                C_PAR(J3), CONS_EQU(J3,J2)
 210                               FORMAT ( 'SPESTA_CNST ', A, 1X , A, ' Cmp: ', A1, &
     &                                      ' Inod: ', I3, ' Iequ: ', I6, ' C_par: ', A, ' Equ: ', 1PD12.4 )
                              END IF
                              CNS_ABR(1:5) = 'SPPS_'
                            ELSE IF ( CNS_TYPE == 'SPE_VEL' ) THEN
                              SIG     = SPE(J1)%CNS_VEL_SIGMA
                              SIGS(1) = SPE(J1)%CNS_VEL_SIGMA
                              IF ( SIGS(1) > SIG__MIN ) THEN
                                   FL_CNS = .TRUE.
                                   I_STA = ADD_CLIST ( MAX4_SIT, L_STA(1), C_STA(1,1), &
     &                                                 SPE(J1)%STATION, IER )
                              END IF
!
! --------------------------- Compute the first central moment of the spline 
! --------------------------- over the interval of arguments 
! --------------------------- ARG(INOD), ARG(INOD+1), ... ARG(INOD + DEGREE + 1)
!
                              CONS_EQU(J3,J2) = &
     &                             BSPL_MOM1_FULL ( SPESOL(J1)%L_NOD, &
     &                                              SPESOL(J1)%NOD_ARR, &
     &                                              SPESOL(J1)%DEGREE, INOD )/TIM_SPAN**2
                              IF ( FL_DEBUG .AND. FL_CNS ) THEN
                                   WRITE  ( 6, 210 ) 'VEL', SPE(J1)%STATION, C_CMP(J2), INOD, J3, &
     &                                               C_PAR(J3), CONS_EQU(J3,J2)
                              END IF
                              CNS_ABR(1:5) = 'SPVL_'
                            ELSE 
                              CALL ERR_LOG ( 8741, IUER, 'SPESTA_CNST', &
     &                            'Unsupported constraint type: '//CNS_TYPE )
                              RETURN 
                         END IF
                    END IF
               END IF
 430        CONTINUE 
!
            IF ( FL_CNS ) THEN
                 CALL INCH      ( J1, CNS_ABR(6:8) )
                 CALL CHASHR        ( CNS_ABR(6:8) )
                 CALL BLANK_TO_ZERO ( CNS_ABR(6:8) )
!
! -------------- Insert information about constraint, name, description,
! -------------- abbreviation, right hand side, reciprocal weight (sigma), type
!
                 IF ( CNS_TYPE == 'SPE_POS' ) THEN
                      CNS_NAME = 'B-spline and pos. '//SPE(J1)%STATION
                      CNS_UNIT = 'meter'
                    ELSE IF ( CNS_TYPE == 'SPE_VEL' ) THEN
                      CNS_NAME = 'B-spline and vel. '//SPE(J1)%STATION
                      CNS_UNIT = 'm/yr'
                 END IF
                 CALL ERR_PASS ( IUER, IER )
                 CALL ADDCNS_NAM ( CNS_ABR, J2, CNS_NAME, CNS_UNIT, &
     &                             CONS_RHS(J2), SIG, GLOBAL_FLAG, &
     &                             CNSTROBJ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8562, IUER, 'SPESTA_CNST', 'Error in '// &
     &                     'an attempt to put information about '// &
     &                     'Constraint on Bspline and pos. for '// &
     &                     SPE(J1)%STATION//' into CNSTROBJ' )
                      RETURN
                 END IF
!
! -------------- Normalize the equation of constraints
!
                 CALL NORM_VEC ( L_PAR, CONS_EQU(1,J2), NORM )
!
! -------------- Insert the coefficicent of constraint equation into
! -------------- the CNSTROBJ object
!
                 DO 440 J4=1,L_PAR
                    IF ( CONS_EQU(J4,J2) .NE. 0.0D0 ) THEN
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, 220 ) cns_abr, j2, j4, c_par(j4), cons_equ(j4,j2)                ! %%%
!  write ( 6, 220 ) cns_abr, j2, j4, c_par(j4), sig                            ! %%%
! 220  format ( A,') Cmp: ',I2,' Par: ',I5, ' par: >>',A,'<<  CNS: ',1PD15.7 ) ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                         CALL ERR_PASS ( IUER, IER )
                         CALL ADDCNS_EQU ( CNS_ABR, J2, J4, CONS_EQU(J4,J2), &
     &                                     GLOBAL_FLAG, CNSTROBJ, IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8563, IUER, 'SPESTA_CNST', &
     &                            'Failure in putting a coefficient of an '// &
     &                            'equation of Constraint on Bspline and '// &
     &                            'pos. for '//SPE(J1)%STATION//' into CNSTROBJ' )
                              RETURN
                        END IF
                    END IF
 440             CONTINUE
            END IF
 420     CONTINUE 
         IF ( FL_DEBUG ) THEN
              IF ( FL_CNS ) THEN
                   WRITE  ( 6, '(A)' ) ' '
                ELSE 
                   WRITE  ( 6, '(A)' ) 'SPESTA_CNST Station '//SPE(J1)%STATION//' is skipped'
                   WRITE  ( 6, '(A)' ) ' '
              END IF
         END IF
         IF ( FL_CNS .AND. FAST_DBG .EQ. F__PRI ) THEN
              WRITE ( 6, * ) ' '//CNS_ABR//'      n_cnstr = ', cnstrobj%n_ecnst
         END IF
 410  CONTINUE 
!
      DO 450 J5=0,1
         IF ( L_STA(J5) .GT. 0 ) THEN
!
! ----------- Sorting the list of stations
!
              CALL SORT_CH ( L_STA(J5), C_STA(1,J5) )
!
! ----------- Split the list into a line
!
              CALL CLRCH   ( OUT )
              CALL LIST_TO_LINE ( L_STA(J5), C_STA(1,J5), ' ', OUT )
              IR = L_STA(J5)/8
              IF ( L_STA(J5) > IR*8 ) IR = IR + 1
              IF ( J5 == 0 ) THEN
                   WRITE ( 23, 110 ) L_STA(J5), 'average', SIGS(J5)
                 ELSE IF ( J5 == 1 ) THEN
                   WRITE ( 23, 110 ) L_STA(J5), 'trend  ', SIGS(J5)
              END IF
 110          FORMAT ( 'SPESTA_CNST: ', I3, &
     &                 ' stations participated in constraints to B-spline ', A/ &
     &                 'SPESTA_CNST sigma: ', 1PD14.6 ) 
              DO 460 J6=1,IR
                 IF ( J6 < IR ) THEN
                      IE = 72
                    ELSE
                      IE = (L_STA(J5) - (IR-1)*8)*9
                 END IF
                 CALL WRITE_LONG ( 23, 72, OUT )
 460          CONTINUE 
              WRITE ( 23, '(A)' ) ' '
         END IF
 450  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPESTA_CNST  !#!#
