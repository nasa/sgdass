!
         IF ( ERM%CNS_MEAN_SIGMA(J3) > 0.0D0 ) THEN
!
! ----------- Impose constraint on the mean value of the ERM
!
              CNS_ABR = 'ERM_E%_M' 
              CALL INCH ( J3, CNS_ABR(6:6) )
!
! ----------- Set the range of knots for applying the constraint
!
              KNOT_BEG = IDNINT ( (ERM%MJD_BEG_MEAN_CNS(J3) - ERM%MJD_BEG)*86400.0D0 + &
     &                            (ERM%TAI_BEG_MEAN_CNS(J3) - ERM%TAI_BEG) )/ &
     &                             ERM%TIME_SPAN(J3) + 1
              KNOT_END = IDNINT ( (ERM%MJD_END_MEAN_CNS(J3) - ERM%MJD_BEG)*86400.0D0 + &
     &                            (ERM%TAI_END_MEAN_CNS(J3) - ERM%TAI_BEG) )/ &
     &                             ERM%TIME_SPAN(J3) + 1
   write ( 6, * ) 'ERM_CNST_MEAN-182 j3= ', j3 ! %%%%
!
! ----------- Fudge factors for applying on the right hand side of the contraint
! ----------- when KNOT_BEG is at the begnning of the interval of the 
! ----------- constraint. The first three numbers were found from computational
! ----------- test and are probably related to integrals over a portion of a spline
! ----------- NB: these numbers are probably incorrect if the knots are not equi-distant
!
              IF ( KNOT_BEG == 1  ) THEN
                   FUDGE_RAT = 0.749770046D0
                 ELSE IF ( KNOT_BEG == 2 ) THEN
                   FUDGE_RAT = 0.6248732714D0
                 ELSE IF ( KNOT_BEG == 3 ) THEN
                   FUDGE_RAT = 0.5416321547D0
                 ELSE
                   FUDGE_RAT = 0.5D0
              END IF
!
! ----------- Set the right-hand side of the constraint tasking into account constraints
! ----------- on rate with a non-zero right-hand size
!
              MEAN_RHS = ERM%CNS_MEAN_RTP(J3) 
#ifdef rate_alg_ends
              MEAN_RHS = ERM%CNS_MEAN_RTP(J3) + FUDGE_RAT*ERM%DEGREE(J3)*ERM%TIME_SPAN(J3)*ERM%CNS_RATE_RTP(J3)
#endif
#ifdef rate_alg_mom
              MEAN_RHS = ERM%CNS_MEAN_RTP(J3) + FUDGE_RAT*ERM%DEGREE(J3)*ERM%TIME_SPAN(J3)*ERM%CNS_RATE_RTP(J3) ! *ERM%DEGREE(J3)
#endif
#ifdef mid
              MEAN_RHS = ERM%CNS_MEAN_RTP(J3) + FUDGE_RAT*ERM%DEGREE(J3)*ERM%TIME_SPAN(J3)*ERM%CNS_RATE_RTP(J3)
#endif
!
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( CNS_ABR, 1, 'ERM mean constraint', 'rad', &
     &                          MEAN_RHS, ERM%CNS_MEAN_SIGMA(J3), .TRUE., CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8526, IUER, 'ERM_CNST', 'Error in '// &
     &                 'an attempt to put information about '// &
     &                  CNS_ABR//' constraints into CNSTROBJ' )
                   RETURN
              END IF
!
   write ( 6, * ) 'ERM_CNST_MEAN-217 j3= ', j3, ' mean_rhs= ', mean_rhs ! %%%%
!
! ----------- Form constraint equations in a form of integral over the B-spline
! ----------- at the whole interval of the constraint 
!
              DO 470 J7=KNOT_BEG,KNOT_END-1
!
! -------------- Compute the fudge factor for the constraint in a case if the 
! -------------- constraint start date is wihtin 1-3 knots of the ERM start date
!
                 FUDGE_INT = 0.0D0
                 IF ( KNOT_BEG == 1 ) THEN
                      FUDGE_INT = 6.0D0*ERM%TIME_SPAN(J3)/(ERM%DEGREE(J3)+1)
                    ELSE IF ( KNOT_BEG == 2 ) THEN
                      FUDGE_INT = 3.0D0*ERM%TIME_SPAN(J3)/(ERM%DEGREE(J3)+1)
                    ELSE IF ( KNOT_BEG == 3 ) THEN
                      FUDGE_INT = 1.0D0*ERM%TIME_SPAN(J3)/(ERM%DEGREE(J3)+1)
                 END IF
                 FINT = (ERM%TIM(J7+1,J3) - ERM%TIM(J7-ERM%DEGREE(J3),J3))/(ERM%DEGREE(J3)+1)/ &
     &                  (ERM%TIM(KNOT_END,J3) - ERM%TIM(KNOT_BEG,J3) - FUDGE_INT )
!
! -------------- Search for the parameter in the parameter list
!
                 IND_EQU = J7
                 IND_PAR = IND_PARS ( IND_EQU, J3 )
                 IF ( IND_PAR .LE. 0 ) THEN
                      CALL ERR_LOG ( 8527, IUER, 'ERM_CNST', 'Trap of '// &
     &                    'internal control: failure to find paramater '// &
     &                     PAR_NAME//' while imposing '//CNS_ABR//' constraint' )
                      RETURN
                 END IF
!
! -------------- Add constraint equation to the list
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL ADDCNS_EQU ( CNS_ABR, 1, IND_PAR, FINT, &
     &                             .TRUE., CNSTROBJ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8528, IUER, 'ERM_CNST', 'Failure '// &
     &                    'in putting a coefficient of an equation '// &
     &                    'of '//CNS_ABR//' constraint' )
                      RETURN
                 END IF
   write ( 6, * ) 'ERM_CNST_MEAN-270 j3= ', j3, ' ind_par= ', ind_par, c_par(ind_par), ' fint= ', fint ; !  %%%%%%%%%%%
 470          CONTINUE 
         END IF
!
         IF ( ERM%CNS_RATE_SIGMA(J3) > 0.0D0 ) THEN
   write ( 6, * ) 'ERM_CNST_RATE-275 j3= ', j3, ' rhs= ', erm%cns_rate_rtp(j3) ! %%%%
!
! ----------- Setting contstaint on global rate of the ERM
!
#ifdef rate_alg_ends
              CNS_ABR = 'ERM_E%_R' 
              CALL INCH ( J3, CNS_ABR(6:6) )
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( CNS_ABR, 1, 'ERM rate constraint', 'rad/s', &
     &                          ERM%CNS_RATE_RTP(J3), ERM%CNS_RATE_SIGMA(J3), &
     &                          .TRUE., CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8529, IUER, 'ERM_CNST', 'Error in '// &
     &                 'an attempt to put information about '// &
     &                  CNS_ABR//' constraints into CNSTROBJ' )
                   RETURN
              END IF
!
              KNOT_BEG = IDNINT ( (ERM%MJD_BEG_RATE_CNS(J3) - ERM%MJD_BEG)*86400.0D0 + &
     &                            (ERM%TAI_BEG_RATE_CNS(J3) - ERM%TAI_BEG) )/ &
     &                             ERM%TIME_SPAN(J3) + 1
              KNOT_END = IDNINT ( (ERM%MJD_END_RATE_CNS(J3) - ERM%MJD_BEG)*86400.0D0 + &
     &                            (ERM%TAI_END_RATE_CNS(J3) - ERM%TAI_BEG) )/ &
     &                             ERM%TIME_SPAN(J3) + 1
!
! ----------- First, apply rate contraint for the left point of the constraint interval
!
              IND_EQU = KNOT_BEG
              TIM_ARG = ERM%TIM(IND_EQU,J3) + TIM_EPS
              DO 490 J9=-ERM%DEGREE(J3),0
!
! -------------- Search for the parameter in the parameter list
!
                 IND_PAR = IND_PARS ( IND_EQU+J9, J3 )
                 IF ( IND_PAR .LE. 0 ) THEN
                      CALL ERR_LOG ( 8530, IUER, 'ERM_CNST', 'Trap of '// &
     &                    'internal control: failure to find paramater '// &
     &                     PAR_NAME//' while imposing '//CNS_ABR//' constraint' )
                      RETURN
                 END IF
                 COEF_EQU = -BSPL_VAL ( ERM%NKNOTS(J3), ERM%TIM(1,J3), &
     &                                  ERM%DEGREE(J3), IND_EQU+J9, TIM_ARG)/ &
     &                                 (ERM%TIM(KNOT_END,J3) - ERM%TIM(KNOT_BEG,J3))
                 CALL ERR_PASS ( IUER, IER )
                 CALL ADDCNS_EQU ( CNS_ABR, 1, IND_PAR, COEF_EQU, &
     &                             .TRUE., CNSTROBJ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8531, IUER, 'ERM_CNST', 'Failure '// &
     &                    'in putting a coefficient of an equation '// &
     &                    'of '//CNS_ABR//' constraint' )
                      RETURN
                 END IF
   write ( 6, * ) 'ERM_CNST_RATE-326 j3= ', j3, ' ind_par= ', ind_par, c_par(ind_par), ' coef_equ= ', coef_equ ; !  %%%%%%%%%%%
 490          CONTINUE 
!
! ----------- Second, apply rate contraint for the right point of the constraint interval
!
              IND_EQU = KNOT_END
              TIM_ARG = ERM%TIM(IND_EQU,J3) - TIM_EPS
              DO 4100 J10=-ERM%DEGREE(J3),0
!
! -------------- Search for the parameter in the parameter list
!
                 IND_PAR = IND_PARS ( IND_EQU+J10, J3 )
                 IF ( IND_PAR .LE. 0 ) THEN
                      CALL ERR_LOG ( 8530, IUER, 'ERM_CNST', 'Trap of '// &
     &                    'internal control: failure to find paramater '// &
     &                     PAR_NAME )
                      RETURN
                 END IF
                 COEF_EQU = BSPL_VAL ( ERM%NKNOTS(J3), ERM%TIM(1,J3), &
     &                                 ERM%DEGREE(J3), IND_EQU+J10, TIM_ARG )/ &
     &                                 (ERM%TIM(KNOT_END,J3) - ERM%TIM(KNOT_BEG,J3))
                 CALL ERR_PASS ( IUER, IER )
                 CALL ADDCNS_EQU ( CNS_ABR, 1, IND_PAR, COEF_EQU, &
     &                             .TRUE., CNSTROBJ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 8532, IUER, 'ERM_CNST', 'Failure '// &
     &                    'in putting a coefficient of an equation '// &
     &                    'of '//CNS_ABR//' constraint' )
                      RETURN
                 END IF
   write ( 6, * ) 'ERM_CNST_RATE-356 j3= ', j3, ' ind_par= ', ind_par, c_par(ind_par), ' coef_equ= ', coef_equ ; !  %%%%%%%%%%%
 4100         CONTINUE 
#endif
#ifdef rate_alg_mom
              CNS_ABR = 'ERM_E%_R' 
              CALL INCH ( J3, CNS_ABR(6:6) )
              CALL ERR_PASS ( IUER, IER )

              CALL ADDCNS_NAM ( CNS_ABR, 1, 'ERM rate constraint', 'rad/s', &
     &                          ERM%CNS_RATE_RTP(J3)*ERM%DEGREE(J3), ERM%CNS_RATE_SIGMA(J3), &
     &                          .TRUE., CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8529, IUER, 'ERM_CNST', 'Error in '// &
     &                 'an attempt to put information about '// &
     &                  CNS_ABR//' constraints into CNSTROBJ' )
                   RETURN
              END IF
!
              KNOT_BEG = IDNINT ( (ERM%MJD_BEG_RATE_CNS(J3) - ERM%MJD_BEG)*86400.0D0 + &
     &                            (ERM%TAI_BEG_RATE_CNS(J3) - ERM%TAI_BEG) )/ &
     &                             ERM%TIME_SPAN(J3) + 1
              KNOT_END = IDNINT ( (ERM%MJD_END_RATE_CNS(J3) - ERM%MJD_BEG)*86400.0D0 + &
     &                            (ERM%TAI_END_RATE_CNS(J3) - ERM%TAI_BEG) )/ &
     &                             ERM%TIME_SPAN(J3) + 1
!
! ----------- Form constraint equations in a form of integral over the B-spline
! ----------- at the whole interval of the constraint 
!
              DO 490 J9=KNOT_BEG,KNOT_END-1
!
! -------------- Compute the fudge factor for the constraint in a case if the 
! -------------- constraint start date is within 1-3 knots of the ERM start date
!
                 FUDGE_INT = 0.0D0
                 IF ( KNOT_BEG == 1 ) THEN
                      FUDGE_INT = 6.0D0*ERM%TIME_SPAN(J3)/(ERM%DEGREE(J3)+1)
                    ELSE IF ( KNOT_BEG == 2 ) THEN
                      FUDGE_INT = 3.0D0*ERM%TIME_SPAN(J3)/(ERM%DEGREE(J3)+1)
                    ELSE IF ( KNOT_BEG == 3 ) THEN
                      FUDGE_INT = 1.0D0*ERM%TIME_SPAN(J3)/(ERM%DEGREE(J3)+1)
                 END IF
                 FINT = (ERM%TIM(J9+1,J3) - ERM%TIM(J9-ERM%DEGREE(J3),J3))
                 IND_EQU = J9
                 IF ( IND_EQU < ERM%NKNOTS(J3)-1 ) THEN
                      TIM_ARG = ERM%TIM(IND_EQU,J3) + TIM_EPS
                    ELSE IF ( J9 == KNOT_END - 1 ) THEN
                      TIM_ARG = ERM%TIM(IND_EQU,J3) - TIM_EPS
                    ELSE
                      TIM_ARG = ERM%TIM(IND_EQU,J3)
                 END IF
                 DO 4100 J10=-ERM%DEGREE(J3),0
!
! ----------------- Search for the parameter in the parameter list
!
                    IND_PAR = IND_PARS ( IND_EQU+J10, J3 )
                    IF ( IND_PAR .LE. 0 ) THEN
                         CALL ERR_LOG ( 8527, IUER, 'ERM_CNST', 'Trap of '// &
     &                       'internal control: failure to find paramater '// &
     &                        PAR_NAME//' while imposing '//CNS_ABR//' constraint' )
                         RETURN
                    END IF
!
! ----------------- Compute constraint equation
!
                    COEF_EQU = BSPL_DER ( ERM%NKNOTS(J3), ERM%TIM(1,J3), &
     &                                    ERM%DEGREE(J3), IND_EQU+J10, TIM_ARG )/ &
     &                                    (KNOT_END-KNOT_BEG)
                    IF ( DABS(COEF_EQU) > 1.D-19 ) THEN
!
! ---------------------- Add constraint equation to the list
!
                         CALL ERR_PASS ( IUER, IER )
                         CALL ADDCNS_EQU ( CNS_ABR, 1, IND_PAR, COEF_EQU, &
     &                                     .TRUE., CNSTROBJ, IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8528, IUER, 'ERM_CNST', 'Failure '// &
     &                            'in putting a coefficient of an equation '// &
     &                            'of '//CNS_ABR//' constraint' )
                              RETURN
                         END IF
   write ( 6, * ) 'ERM_CNST_RATE-443 j3= ', int2(j3), ' ind_par= ', ind_par, c_par(ind_par), ' coef_equ= ', coef_equ,  cns_abr ; call flush ( 6 ) ! %%%
                    END IF
 4100         CONTINUE 
 490          CONTINUE 
#endif
#ifdef rate_growth
              CNS_ABR = 'ERM_E%_R' 
              CALL INCH ( J3, CNS_ABR(6:6) )
              TIM_MID   = (ERM%TIM(KNOT_END,J3) + ERM%TIM(KNOT_BEG,J3))/2.0D0
              TIM_RANGE = (ERM%TIM(KNOT_END,J3) - ERM%TIM(KNOT_BEG,J3))
              FUDGE_RAT =  0.0155067187409005D0 ! when kd=905 
              FUDGE_RAT =  1.1896500707286111D0 ! when kd=632
              FUDGE_RAT =  1.6865700970796464D0 ! when kd=448
              FUDGE_RAT =  3.4467241686909471D0 ! when kd=342
              FUDGE_RAT =  5.3525015510154645   ! when kd=272 
              FUDGE_RAT =  0.0D0
              RATE_RHS = ERM%CNS_RATE_RTP(J3) + FUDGE_RAT*ERM%DEGREE(J3)*ERM%CNS_MEAN_RTP(J3)/TIM_RANGE
              CALL ERR_PASS ( IUER, IER )
              CALL ADDCNS_NAM ( CNS_ABR, 1, 'ERM rate constraint', 'rad/s', &
     &                          RATE_RHS, ERM%CNS_RATE_SIGMA(J3), &
     &                          .TRUE., CNSTROBJ, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 8529, IUER, 'ERM_CNST', 'Error in '// &
     &                 'an attempt to put information about '// &
     &                  CNS_ABR//' constraints into CNSTROBJ' )
                   RETURN
              END IF
!
              KNOT_BEG = IDNINT ( (ERM%MJD_BEG_RATE_CNS(J3) - ERM%MJD_BEG)*86400.0D0 + &
     &                            (ERM%TAI_BEG_RATE_CNS(J3) - ERM%TAI_BEG) )/ &
     &                             ERM%TIME_SPAN(J3) + 1
              KNOT_END = IDNINT ( (ERM%MJD_END_RATE_CNS(J3) - ERM%MJD_BEG)*86400.0D0 + &
     &                            (ERM%TAI_END_RATE_CNS(J3) - ERM%TAI_BEG) )/ &
     &                             ERM%TIME_SPAN(J3) + 1
              KNOT_MID  = (KNOT_END - KNOT_BEG)/2.0D0
!
! ----------- Form constraint equations in a form of integral over the B-spline
! ----------- at the whole interval of the constraint 
!
              DO 490 J9=KNOT_BEG,KNOT_END-1
                 IND_EQU = J9
                 IF ( IND_EQU < ERM%NKNOTS(J3)-1 ) THEN
                      TIM_ARG = ERM%TIM(IND_EQU,J3) + TIM_EPS
                    ELSE IF ( J9 == KNOT_END - 1 ) THEN
                      TIM_ARG = ERM%TIM(IND_EQU,J3) - TIM_EPS
                    ELSE
                      TIM_ARG = ERM%TIM(IND_EQU,J3)
                 END IF
                 DO 4100 J10=-ERM%DEGREE(J3),0
!
! ----------------- Search for the parameter in the parameter list
!
                    IND_PAR = IND_PARS ( IND_EQU+J10, J3 )
                    IF ( IND_PAR .LE. 0 ) THEN
                         CALL ERR_LOG ( 8527, IUER, 'ERM_CNST', 'Trap of '// &
     &                       'internal control: failure to find paramater '// &
     &                        PAR_NAME//' while imposing '//CNS_ABR//' constraint' )
                         RETURN
                    END IF
!
! ----------------- Compute constraint equation
!
                    COEF_EQU = BSPL_VAL ( ERM%NKNOTS(J3), ERM%TIM(1,J3), &
     &                                    ERM%DEGREE(J3), IND_EQU+J10, TIM_ARG )* &
     &                         J9/DBLE(KNOT_END - KNOT_BEG)**3/ERM%TIME_SPAN(J3)* &
     &                         12.0D0*ERM%NKNOTS(J3)/DBLE(ERM%NKNOTS(J3)-ERM%DEGREE(J3))
#ifdef mid
                    COEF_EQU = BSPL_VAL ( ERM%NKNOTS(J3), ERM%TIM(1,J3), &
     &                                    ERM%DEGREE(J3), IND_EQU+J10, TIM_ARG )* &
     &                         (J9 - KNOT_MID)/DBLE(KNOT_END - KNOT_BEG)**3/ERM%TIME_SPAN(J3)* &
     &                         12.0D0*ERM%NKNOTS(J3)/DBLE(ERM%NKNOTS(J3)-ERM%DEGREE(J3))
#endif
!
                    IF ( DABS(COEF_EQU) > 1.D-24 ) THEN
!
! ---------------------- Add constraint equation to the list
!
                         CALL ERR_PASS ( IUER, IER )
                         CALL ADDCNS_EQU ( CNS_ABR, 1, IND_PAR, COEF_EQU, &
     &                                     .TRUE., CNSTROBJ, IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 8528, IUER, 'ERM_CNST', 'Failure '// &
     &                            'in putting a coefficient of an equation '// &
     &                            'of '//CNS_ABR//' constraint' )
                              RETURN
                         END IF
   write ( 6, * ) 'ERM_CNST_RATE-514 j3= ', int2(j3), ' ind_par= ', ind_par, c_par(ind_par), ' coef_equ= ', coef_equ,  cns_abr ; call flush ( 6 ) ! %%%
                    END IF
 4100         CONTINUE 
 490          CONTINUE 
#endif
         END IF
   write ( 6, * ) 'TIM_MID= ', TIM_MID, ' TIM_RANGE= ', TIM_RANGE, ' KNO = ', KNOT_END - KNOT_BEG, ' RATE_RHS= ', RATE_RHS
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ERM_CNST  !#!#
