      SUBROUTINE SOB_COMP ( NX, XI, ATT, TEM, OPA, TAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SOB_COMP computes atmosphere opacity (i.e. optical depth)  *
! *   and atmosphere brightness temperature along the trajectory XI.     *
! *                                                                      *
! *   NB: For reasons which are not understood, SOB_COMP should be       *
! *       compiled with optimizations turned off.                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   NX ( INTEGER*4  ) -- The number of knots of the B-spline expansion *
! *                        of refractivity along XI axis.                *
! *   XI ( REAL*8     ) -- Array of knots along the XI axis. The first   *
! *                        point corresponds to the receiver. The last   *
! *                        point corresponds to the top of the           *
! *                        atmosphere in the direction to the emitter,   *
! *                        undisturbed by refraction. Dimension: NX.     *
! *                        Units: m.                                     *
! *  ATT ( REAL*8     ) -- Array of atmosphere attenuation (absorption)  *
! *                        at a specific frequency. Dimension: NX.       *
! *                        Units: 1/m. Att=1 means the electromagnetic   *
! *                        wave is attenuated by the factor of 2.7183    *
! *                        for propagation through 1 meter of the media. *
! *  TEM ( REAL*8     ) -- Array of atmosphere air temperature.          *
! *                        Dimension: NX. Units: K.                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  OPA ( REAL*8     ) -- Array of atmosphere opacity for the distance  *
! *                        XI(1) to XI(NX) /from the ground station to   *
! *                        the top of the atmosphere/. Opacity 1 means   *
! *                        the ratio of power at the top of the          *
! *                        atmosphere to the power at the ground is      *
! *                        2.7183. Units: dimensionless.                 *
! *  TAT ( REAL*8     ) -- Brightness temperature of the atmosphere      *
! *                        at the ground. Unit: K.                       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *  ### 12-SEP-2014    SOB_COMP   v1.1 (c)  L. Petrov  02-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INTEGER*4  NX, IUER
      REAL*8     XI(NX), ATT(NX), TEM(NX), OPA, TAT 
      REAL*8     ATT_SPL(1-SPD__MDEG:SPD__MLEV), &
     &           FUN_SPL(1-SPD__MDEG:SPD__MLEV), OPA_FUN(SPD__MLEV)
      REAL*8     XI_FUN(SPD__MLEV)
      REAL*8     OPA_MIN
      PARAMETER  ( OPA_MIN = 1.D-12 )
      LOGICAL*1  FL_TEST
      INTEGER*4  J1, J2, J3, IND, INDF, IER
      LOGICAL*4, EXTERNAL :: IS_R8_NAN 
      REAL*8,    EXTERNAL :: VAL_BSPLJ 
!
      FL_TEST = .FALSE.
!
! --- Fill array ATT_SPL and FUN_SPL
!
      DO 410 J1=1,NX
         IF ( FL_TEST ) THEN
              WRITE ( 6, * ) 'J1= ', INT2(J1), ' XI= ', SNGL(XI(J1)), &
     &                       ' ATT= ', SNGL(ATT(J1))
         END IF
         ATT_SPL(J1) = ATT(J1)
 410  CONTINUE 
!
! --- Expand attenuation into the B-spline basis
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL_1D_CMP ( SPD__MDEG, 0, NX, XI, ATT_SPL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5591, IUER, 'SOB_COMP', 'Error in '// &
     &         'an attempt to compute expansion of the atmosphere '// &
     &         'attenuation into B-spline basis' )
           RETURN 
      END IF
!
! --- Compute array of OPA_FUN (optical depth) and 
! --- FUN_SPL -- product of air temperature and accumulated attenuation
!
! --- NB: integration of the radiative transfer equation is performed from
! --- the top of the atmosphere down to the station. Therefore, we 
! --- re-index arrays
!
      INDF = 0
      IND  = 1
      OPA_FUN(IND) = 2.0D0*OPA_MIN
      FUN_SPL(IND) = TEM(NX)*DEXP(-OPA_FUN(IND))
      IF ( FL_TEST ) THEN
           WRITE ( 6, * ) ' TEMP_TOA: ', tem(nx), ' FUN_SPL= ', FUN_SPL(IND)
      END IF
      DO 420 J2=NX-1,1,-1 ! NB: cycle runs backward
!
! ------ Compute optical depth between node J2 and the top of the atmosphere
! ------ OPA_FUN is the opacity that runs from TOA to the ground
!
         IND = IND + 1
         OPA_FUN(IND) = VAL_BSPLJ ( XI(J2), SPD__MDEG, NX, J2, XI, ATT_SPL )
         XI_FUN(IND) = XI(J2)
!
         IF ( IS_R8_NAN ( OPA_FUN(IND) ) ) THEN
              WRITE ( 6, * ) 'IND = ', IND, ' J2= ', J2, ' NX= ', NX
              WRITE ( 6, * ) 'XI      = ', XI
              WRITE ( 6, * ) 'ATT     = ', ATT
              WRITE ( 6, * ) 'ATT_SPL = ', ATT_SPL
              WRITE ( 6, * ) 'OPA_FUN = ', OPA_FUN
              CALL ERR_LOG ( 5593, IUER, 'SOB_COMP', 'Trap of internal '// &
     &            'control: OPA is NaN' )
              RETURN 
         END IF
!
! ------ Check whether opacity is small
! ------ This is done to avoid an out-of-order sequence due to rounding errors
!
         IF ( OPA_FUN(IND) > SPD__OPA_MIN ) THEN
              IF ( INDF == 0 ) INDF = IND
         END IF
!
! ------ Compute the function under integral
!
         FUN_SPL(IND) = TEM(J2)*DEXP(-OPA_FUN(IND))
         IF ( FL_TEST ) THEN
              WRITE ( 6, * ) 'SOB J2= ', INT2(J2), ' IND= ', INT2(J2), &
     &                       ' XI= ', SNGL(XI(J2)), ' OPA= ', SNGL(OPA_FUN(IND)), &
     &                       ' FU= ', SNGL(FUN_SPL(IND)), ' TEM= ', TEM(IND)
         END IF
 420  CONTINUE 
 820  CONTINUE 
      IF ( FL_TEST ) THEN
           WRITE ( 6, * ) 'INDF= ', INDF
           CALL DIAGI_1 ( NX-INDF+1, XI_FUN(INDF), OPA_FUN(INDF), IER )
      END IF
!
! --- Sort arrays OPA_FUN/FUN_SPL. Due to roundig errors array OPA_FUN
! --- may not be an increasing sequence. Check and correct this.
!
      CALL SORT8 ( NX-INDF+1, OPA_FUN(INDF), FUN_SPL(INDF) )
      DO 430 J3=INDF+1,NX-INDF
!
! ------ Difference in OPA_FUN increment should be no less than OPA_MIN
!
         IF ( OPA_FUN(J3) - OPA_FUN(J3-1) < OPA_MIN )  THEN
              OPA_FUN(J3) = OPA_FUN(J3-1) + OPA_MIN
         END IF
 430  CONTINUE
      IF ( FL_TEST ) THEN
           CALL DIAGI_1 ( nx-indf+1, OPA_FUN(INDF), FUN_SPL(INDF), IER )
      END IF
!
! --- We compute the B-spline expansion for a section of arrays OPA_FUN, FUN_SPL
! --- starting from index IND
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL_1D_CMP ( SPD__MDEG, 0, NX-INDF+1, OPA_FUN(INDF), FUN_SPL(INDF-SPD__MDEG), &
     &                   IER )
      IF ( IER .NE. 0 ) THEN
           write ( 6, * ) ' nx = ', nx, ' indf= ', indf
           do 520 j2=1,nx
              write ( 6, 210 ) j2, xi(j2), att(j2), tem(j2), opa_fun(j2)
 210          format ( 'i= ',i3, ' xi/tem/opa/fun= ', 5(1pd22.15,1x) )
 520       continue 
           write ( 6, * ) ' ier= ', ier, ' iuer= ', iuer 
           CALL ERR_LOG ( 5594, IUER, 'SOB_COMP', 'Error in '// &
     &         'an attempt to compute expansion of the function that '// &
     &         'is the product of accumulated atmosphere opacity and '// &
     &         'thermodynamic temperature into B-spline basis' )
           RETURN 
      END IF
!
! --- Compute opacity at the ground by integrating of attenuation along axis XI
!
      OPA  = VAL_BSPLJ ( XI, SPD__MDEG, NX, 1, XI, ATT_SPL )
!
! --- Finally, get the atmosphere brightness temperature at the ground
!
      IF ( FL_TEST ) THEN
           WRITE ( 6, * ) 'OPA= ', SNGL(OPA), ' CMB= ', SNGL(SPD__TEM_CMB*DEXP(-OPA)), &
     &                    ' VTAT= ', SNGL( VAL_BSPLJ ( OPA_FUN(INDF), SPD__MDEG, NX-INDF+1, 1, OPA_FUN(INDF), FUN_SPL(INDF-SPD__MDEG) ) )
      END IF
      TAT = SPD__TEM_CMB*DEXP(-OPA) + &
     &      VAL_BSPLJ ( OPA_FUN(INDF), SPD__MDEG, NX-INDF+1, 1, OPA_FUN(INDF), FUN_SPL(INDF-SPD__MDEG) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SOB_COMP  !#!  
