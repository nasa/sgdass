      SUBROUTINE VTD_POSVAR_INIT ( VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_POSVAR_INIT  reads position variation files specified *
! *   in the internal fields of the object VTD, parses them, computes    *
! *   interpolation poynomials and stores coefficients in the fields of  *
! *   the VTD object.                                                    *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
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
! * ### 29-JAN-2004  VTD_POSVAR_INIT v1.0 (c) L. Petrov  29-JAN-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  I_STA, IUER
      REAL*8     OVR__SMP
      PARAMETER  ( OVR__SMP = 0.15 )
      REAL*8     TIM_BEG_PSV, TIM_PSV(M__PSV), &
     &           VAL_PSV(VTD__M_SDI,3,VTD__M_STA), WORK_ARR(VTD__M_SDI), &
     &           D1, DN
      INTEGER*4  J1, J2, J3, J4, IER
      EXTERNAL   ILEN
      INTEGER*4  ILEN
!
! --- Cycle over the maximal allowed number of position variation models
!
      DO 410 J1=1,VTD__M_PSF
         IF ( ILEN(VTD%CONF%POSVAR_FIL(J1)) .GT. 0 ) THEN
!
! ----------- Aga. the position variation file was set up.
! ----------- Get an arrays of position variations at the epochs during 
! ----------- the session
!
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_SET_POSVAR ( VTD, J1, TIM_BEG_PSV, TIM_PSV, VAL_PSV, &
     &                              IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2191, IUER, 'VTD_SETRANGE', 'Error in '// &
     &                 'an attempt to initialize position variations '//    &
     &                 'determined in the file '//VTD%CONF%POSVAR_FIL(J1) )
                   RETURN
              END IF
!
! ----------- Now make interpolating polynomials for each station
!
              DO 420 J2=1,VTD%L_STA
!
! -------------- Copy array of time epochs and postion variations in to 
! -------------- appropriate slots of VTD
!
                 CALL COPY_R8 ( VTD__M_SDI, TIM_PSV, VTD%STA(J2)%PSV_TIM(1,J1) )
                 CALL COPY_R8 ( VTD__M_SDI*3, VAL_PSV(1,1,J2), &
     &                                        VTD%STA(J2)%PSV_POS(1,1,J1) )
                 VTD%STA(J2)%PSV_TIM_BEG = TIM_BEG_PSV 
                 IF ( VTD%CONF%POSVAR_INT(J1) == PSV__SPL ) THEN
!
! ------------------- Cubic spline interpolation was ordered. Compute sets of
! ------------------- spline coefficients for each component of the displacement
!
                      DO 430 J3=1,3
                         CALL ERR_PASS ( IUER, IER )
                         CALL MAKE_SPLINE ( 3, VTD__M_SDI,                   &
     &                                 VTD%STA(J2)%PSV_TIM(1,J1),            &
     &                                 VTD%STA(J2)%PSV_POS(1,J3,J1),         &
     &                                 D1, DN, VTD%STA(J2)%PSV_SPL(1,J3,J1), &
     &                                 WORK_ARR, IER )
                         IF ( IER .NE. 0 ) THEN
                              CALL ERR_LOG ( 2192, IUER, 'VTD_POSVAR_INIT',  &
     &                            'Error in an attempt to compute '//        &
     &                            'coefficient of a spline for '//           &
     &                            'interpolation variations of station '//   &
     &                             VTD%STA(J2)%IVS_NAME//' for the '//       &
     &                            'model determined in the file '//          &
     &                             VTD%CONF%POSVAR_FIL(J1) )
                              RETURN
                         END IF
 430                  CONTINUE 
                 END IF
 420          CONTINUE 
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE VTD_POSVAR_INIT
