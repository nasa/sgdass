      SUBROUTINE VTD_ANT_GRAVITY ( VTD, ISTA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_ANT_GRAVITY  computes the contribution of the antenna  *
! *   gravity deformation to the path delay. It is considered that the   *
! *   focus length change due to antenna gravity deformation as          *
! *   a function of elevation has already been computed, stored in the   *
! *   the file, loaded to VTD mad coefficients of the cubic spline has   *
! *   already been loaded. This routine finds the cubic spline that      *
! *   corresponds to this station, and if finds, it computes the value   *
! *   of the cubic spline that corresponds to this elevation and stores  *
! *   it in VTD%AGR%ANT_GRAVITY_DEL.                                     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      ISTA ( INTEGER*4 ) -- Station index in internal VTD data        *
! *                            structures.                               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 23-APR-2008  VTD_ANT_GRAVITY  v1.1 (c) L. Petrov 30-JUL-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      INTEGER*4  ISTA, IUER
      INTEGER*4  IND_STA, IXP
      REAL*8,    EXTERNAL :: FSPL8 
      INTEGER*4, EXTERNAL :: IXMN8, LTM_DIF
!
      IF ( VTD%STATUS_AGD .NE. VTD__YES ) THEN
           CALL ERR_LOG ( 2471, IUER, 'VTD_ANT_GRAVITY', 'Trap of internal '// &
     &         'control: Antenna gravity deformation file has not been loaded' )
           RETURN 
      END IF
!
      VTD%STA(ISTA)%ANT_GRAVITY_DEL = 0.0D0
      IND_STA = 0
      IF ( VTD%AGD%N_ANT > 0 ) THEN
           IND_STA = LTM_DIF ( 1, VTD%AGD%N_ANT, VTD%AGD%STA_NAM, &
                               VTD%STA(ISTA)%IVS_NAME )
      END IF
      IF ( IND_STA == 0 ) THEN
!
! -------- Nothing to do
!
           VTD%STA(ISTA)%ANT_GRAVITY_DEL = 0.0D0
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IXP = IXMN8 ( VTD%AGD%INFO(IND_STA)%N_POI, VTD%AGD%INFO(IND_STA)%ELEV, &
     &              VTD%STA(ISTA)%ELEV )
!
      VTD%STA(ISTA)%ANT_GRAVITY_DEL = FSPL8 ( VTD%STA(ISTA)%ELEV, &
     &                                  VTD%AGD%INFO(IND_STA)%N_POI, &
     &                                  VTD%AGD%INFO(IND_STA)%ELEV, &
     &                                  VTD%AGD%INFO(IND_STA)%FOC_LEN, IXP, &
     &                                  VTD%AGD%INFO(IND_STA)%FOC_SPL )/VTD__C
!@      IF ( VTD%AGD%INFO(IND_STA)%FOCUS_TYPE == AGD__FO_SECN ) THEN
!@           VTD%STA(ISTA)%ANT_GRAVITY_DEL = 2.0D0 * &
!@     &                                     VTD%STA(ISTA)%ANT_GRAVITY_DEL
!@      END IF
!
      IF ( VTD%CONF%IVRB .GE. 2 ) THEN
           WRITE ( 6, 110 ) VTD%STA(ISTA)%IVS_NAME, &
     &                      VTD%STA(ISTA)%ELEV/DEG__TO__RAD, &
     &                      VTD%STA(ISTA)%ANT_GRAVITY_DEL*1.D12
 110       FORMAT ( 'ANT_GRAVITY: ',A, ' Elev: ',F8.3,' deg  Delay: ', &
     &               F6.2, ' ps ' )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  !#!#  VTD_ANT_GRAVITY  #!#
