      FUNCTION VTD_REL_ORB ( VTD, STA_ORB, MJD_BEG, TCG_BEG, MJD_END, &
     &                       TCG_END, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_REL_ORB computes the time dilation for a VLBI station  *
! *   which is on the orbit. The dilation is computed for interval       *
! *   of geocentric time coordinates MJD_BEG/TCG_BEG, MJD_END/TCG_END.   *
! *   It does this computation by evaluating the integral using          *
! *   coefficients of spline interpolation which were computed by        *
! *   routine VTD_LOAD_NZO.                                              *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     VTD ( VTD__TYPE ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
! * STA_ORB ( CHARACTER ) -- Name of the orbiting station.               *
! *                          The station should be in the input          *
! *                          catalogue defined in the control file.      *
! * MJD_BEG ( INTEGER*4 ) -- Modified Julian date of the midnight of     *
! *                          the beginning of the interval.              *
! * TCG_BEG ( REAL*8    ) -- Geocentric time coordinate for beginning of *
! *                          the interval.                               *
! * MJD_END ( INTEGER*4 ) -- Modified Julian date of the midnight of     *
! *                          the end of the interval.                    *
! * TCG_END ( REAL*8    ) -- Geocentric time coordinate for end of       *
! *                          the interval.                               *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! *  ### 11-OCT-2012  VTD_REL_ORB  v1.0 (c)  L. Petrov  11-OCT-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      REAL*8     VTD_REL_ORB
      INTEGER*4  MJD_BEG, MJD_END, IUER
      CHARACTER  STA_ORB*(*)
      CHARACTER  STR*128
      REAL*8     TCG_BEG, TCG_END
!
      INTEGER*4  ISTA_ORB, KNOT_BEG, KNOT_END, J1, J2, IND_NZO, IER
      REAL*8     TARG_BEG, TARG_END
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: SPL_INT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8
!
      IF ( VTD%L_NZO == 0 ) THEN
           CALL ERR_LOG ( 3271, IUER, 'VTD_REL_ORB', 'No position of '//  &
     &         'any orbiting object was loaded. Cannot get position '// &
     &         'for orbiting antenna '//STA_ORB )
           RETURN
      END IF
!
! --- Find indices of the ground station
!
      ISTA_ORB= 0
      DO 410 J1=1,VTD%L_STA
         IF ( STA_ORB == VTD%STA(J1)%IVS_NAME ) ISTA_ORB = J1
 410  CONTINUE 
      IF ( ISTA_ORB == 0 ) THEN
           CALL ERR_LOG ( 3272, IUER, 'VTD_REL_ORB', 'Cannot find '// &
     &         'orbit station '//STA_ORB(1:I_LEN(STA_ORB))// &
     &         ' -- it was not defined early when VTD_LOAD_NZO '// &
     &         ' was called' )
           RETURN 
      END IF
      IF ( VTD%STA(ISTA_ORB)%STA_TYP .NE. VTD__OR ) THEN
           CALL ERR_LOG ( 3273, IUER, 'VTD_REL_ORB', 'Station '// &
     &          STA_ORB(1:I_LEN(STA_ORB))//' is not defined as an '// &
     &         'orbiting element of the interferometer' )
           RETURN 
      END IF
!
      IND_NZO = 0
      DO 420 J2=1,VTD%L_NZO
         IF ( VTD%NZO(J2)%NAME == STA_ORB ) THEN
              IND_NZO = J2
         END IF
 420  CONTINUE
      IF ( IND_NZO == 0 ) THEN
           CALL ERR_LOG ( 3274, IUER, 'VTD_REL_ORB', 'Orbigin VLBI '// &
     &         'station '//STA_ORB//' was not defined' )
           RETURN
      END IF
!
      TARG_BEG = (MJD_BEG - VTD%NZO(IND_NZO)%MJD_BEG)*86400.0D0 + &
     &           (TCG_BEG - VTD%NZO(IND_NZO)%TIM_BEG)
      TARG_END = (MJD_END - VTD%NZO(IND_NZO)%MJD_BEG)*86400.0D0 + &
     &           (TCG_END - VTD%NZO(IND_NZO)%TIM_BEG)
!
      VTD_REL_ORB = SPL_INT ( VTD%NZO(IND_NZO)%NOD_SPL, VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                        VTD%NZO(IND_NZO)%DEG_SPL, VTD%NZO(IND_NZO)%SPL_RLT_ARR, &
     &                        TARG_END ) - &
     &              SPL_INT ( VTD%NZO(IND_NZO)%NOD_SPL, VTD%NZO(IND_NZO)%TIM_ARR(1), &
     &                        VTD%NZO(IND_NZO)%DEG_SPL, VTD%NZO(IND_NZO)%SPL_RLT_ARR, &
     &                        TARG_BEG )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  VTD_REL_ORB  !#!#
