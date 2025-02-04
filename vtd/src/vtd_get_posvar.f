      SUBROUTINE VTD_GET_POSVAR ( VTD, I_STA, I_PSV, PSD_TRS, PSV_TRS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_GET_POSVAR  computes array of site displacements      *
! *   according to the I_PSV -th model of position variations specified  *
! *   in the control file for the time range around the nominal start    *
! *   and nominal stop  of the observing session. It returns the time    *
! *   epochs of the displacements and 3-D displacement for all stations  *
! *   participated in the session as well as the vector of velocity.     *
! *   It is assumed that the model has been previously loaded in VTD.    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! *     I_STA ( INTEGER*4 ) -- Index of the station in the VTD station   *
! *                            list.                                     *
! *     I_PSV ( INTEGER*4 ) -- Index of the position variation file in   *
! *                            the array POSVAR_FIL defined in VTD.      *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   PSD_TRS ( REAL*8    ) -- Vector of site displacements according to *
! *                            the I_PSV -th model of position           *
! *                            variations in the terrestrial reference   *
! *                            system. Dimension: 3. Units: meters.      *
! *   PSV_TRS ( REAL*8    ) -- Vector of site velocity in the            *
! *                            terrestrial reference system according    *
! *                            to the  I_PSV -th model of position       *
! *                            variations. Dimension: 3.                 *
! *                            Units: meter/sec.                         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 17-DEC-2002  VTD_GET_POSVAR v2.0 (c) L. Petrov  24-JUN-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  I_PSV, I_STA, IUER
      REAL*8     PSD_TRS(3), PSV_TRS(3)
      REAL*8     TIM_MOM, TIM_PSV
      CHARACTER  STR*80, STR1*80
      INTEGER*4  J1, IBEG, IEND, NODE, NN, IER
      REAL*8,    EXTERNAL :: FLIN8, FSPL8, DSPL8
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8 
!
! --- Initialization
!
      CALL NOUT_R8 ( 3, PSD_TRS )
      CALL NOUT_R8 ( 3, PSV_TRS )
!
! --- Check validity of input parameters
!
      IF ( I_PSV .LT. 0  .OR.  I_PSV .GT. VTD__M_PSF ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( I_PSV, STR )
           CALL ERR_LOG ( 2811, IUER, 'VTD_GET_POSVAR', 'Wrong value of '// &
     &         'parameter I_PSV: '//STR )
           RETURN
      END IF
!
      IF ( I_STA .LE. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( I_STA, STR )
           CALL ERR_LOG ( 2812, IUER, 'VTD_GET_POSVAR', 'Error of internal '// &
     &         'control: parameter I_STA is wrong: '//STR )
           RETURN 
      END IF
!
      IF ( I_STA .GT. VTD__M_STA ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( I_STA, STR )
           CALL INCH  ( VTD__M_STA, STR1 )
           CALL ERR_LOG ( 2813, IUER, 'VTD_GET_POSVAR', 'Error of internal '// &
     &         'control: parameter I_STA: '//STR(1:I_LEN(STR))// &
     &         ' is too large: larger than the constant VTD__M_STA '// &
     &          STR1(1:I_LEN(STR1))//' defined in vtd.i' ) 
           RETURN 
      END IF
!
      IF ( ILEN(VTD%CONF%POSVAR_FIL(I_PSV)) .EQ. 0 ) THEN
!
! -------- Nothing to do
!
           CONTINUE 
         ELSE 
!
! -------- Compute TIM_MOM -- moment of tim of observations in seconds 
! --------                    elapsed from the J2000.0 epoch
! -------- Compute TIM_PSV -- time in seconds elapsed from the first node
! --------                    for which a portion of position variations 
! --------                    time series is provided
!
           TIM_MOM = ( VTD%MOM%MJD - J2000__MJD)*86400.0D0 + &
     &               ( VTD%MOM%TAI - 43200.0D0 )
           TIM_PSV = TIM_MOM - VTD%STA(I_STA)%PSV_TIM_BEG(I_PSV)
           NODE = IXMN8 ( M__PSV, VTD%STA(I_STA)%PSV_TIM(1,I_PSV), TIM_PSV )
           IF ( NODE .LT. 1  ) THEN
                WRITE ( 6, * ) 'NODE    = ', NODE
                WRITE ( 6, * ) 'MJD     = ', VTD%MOM%MJD, ' TAI=', VTD%MOM%TAI
                WRITE ( 6, * ) 'Moment date: '//MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI, IER )
                WRITE ( 6, * ) 'TIM_MOM = ', TIM_MOM
                WRITE ( 6, * ) 'TIM_PSV = ', TIM_PSV
                WRITE ( 6, * ) 'PSV_TIM = ', &
     &                ( VTD%STA(I_STA)%PSV_TIM(NN,I_PSV), NN=1,VTD__M_SDI )
                CALL ERR_LOG ( 2814, IUER, 'VTD_GET_POSVAR', 'Trap of '// &
     &               'internal control: a node beyond interpolation range '// &
     &               ' was found' )
                RETURN 
           END IF
!
           DO 410 J1=1,3 ! Three components
              IF ( VTD%CONF%POSVAR_INT(I_PSV) == PSV__CLS ) THEN
!
! ---------------- Interpolation method: to pick the closest point
!
                   PSD_TRS(J1) = VTD%STA(I_STA)%PSV_POS(1,J1,I_PSV) 
                   IBEG = NODE
                   IEND = NODE + 1
                   IF ( IEND .GT. M__PSV ) THEN
!
! --------------------- The last point was at the right edge of the interval
!
                        IBEG = M__PSV - 1
                        IEND = M__PSV
                   END IF
!
                   IF ( DABS( VTD%STA(I_STA)%PSV_TIM(IBEG,I_PSV) - TIM_PSV) < &
     &                  DABS( VTD%STA(I_STA)%PSV_TIM(IEND,I_PSV) - TIM_PSV) )THEN
                        PSV_TRS(J1) = VTD%STA(I_STA)%PSV_POS(IBEG,J1,I_PSV) 
                      ELSE 
                        PSV_TRS(J1) = VTD%STA(I_STA)%PSV_POS(IEND,J1,I_PSV) 
                   END IF
                   PSV_TRS(J1) = ( VTD%STA(I_STA)%PSV_POS(IEND,J1,I_PSV) -   &
     &                             VTD%STA(I_STA)%PSV_POS(IBEG,J1,I_PSV) )/  &
     &                           ( VTD%STA(I_STA)%PSV_TIM(IEND,I_PSV) -      &
     &                             VTD%STA(I_STA)%PSV_TIM(IBEG,I_PSV) )
                 ELSE IF ( VTD%CONF%POSVAR_INT(I_PSV) == PSV__LIN ) THEN
!
! ---------------- Linear interpolation
!
                   PSD_TRS(J1) = FLIN8 ( TIM_PSV, M__PSV,                    &
     &                                   VTD%STA(I_STA)%PSV_TIM(1,I_PSV),    &
     &                                   VTD%STA(I_STA)%PSV_POS(1,J1,I_PSV), &
     &                                   NODE )
                   IBEG = NODE
                   IEND = NODE + 1
                   IF ( IEND .GT. M__PSV ) THEN
!
! --------------------- The last point was at the right edge of the interval
!
                        IBEG = M__PSV - 1
                        IEND = M__PSV
                   END IF
                   PSV_TRS(J1) = ( VTD%STA(I_STA)%PSV_POS(IEND,J1,I_PSV) -   &
     &                             VTD%STA(I_STA)%PSV_POS(IBEG,J1,I_PSV) )/  &
     &                           ( VTD%STA(I_STA)%PSV_TIM(IEND,I_PSV) -      &
     &                             VTD%STA(I_STA)%PSV_TIM(IBEG,I_PSV) )
                 ELSE IF ( VTD%CONF%POSVAR_INT(I_PSV) == PSV__SPL ) THEN
!
! ---------------- Spline interpolation
!
                   PSD_TRS(J1) = FSPL8 ( TIM_PSV, M__PSV,                    &
     &                                   VTD%STA(I_STA)%PSV_TIM(1,I_PSV),    &
     &                                   VTD%STA(I_STA)%PSV_POS(1,J1,I_PSV), &
     &                                   NODE,                               &
     &                                   VTD%STA(I_STA)%PSV_SPL(1,J1,I_PSV)  )
                   PSV_TRS(J1) = DSPL8 ( TIM_PSV, M__PSV,                    &
     &                                   VTD%STA(I_STA)%PSV_TIM(1,I_PSV),    &
     &                                   VTD%STA(I_STA)%PSV_POS(1,J1,I_PSV), &
     &                                   NODE,                               &
     &                                   VTD%STA(I_STA)%PSV_SPL(1,J1,I_PSV)  )
              END IF
  410      CONTINUE 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_GET_POSVAR 
