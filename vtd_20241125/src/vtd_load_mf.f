      SUBROUTINE VTD_LOAD_MF ( VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_LOAD_MF
! *                                                                      *
! *  ### 19-AUG-2014   VTD_LOAD_MF  1.1 (c)  L. Petrov  26-AUG-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  IUER 
      CHARACTER  STR*128
      REAL*8     EL_MIN, EL_MAX
      INTEGER*4  L_NOD
      PARAMETER  ( EL_MIN = 2.0D0*DEG__TO__RAD )
      PARAMETER  ( EL_MAX = P2I )
      PARAMETER  ( L_NOD  = 24  )
      INTEGER*4  IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( VTD%CONF%ATM_PARTIAL_TYPE == VTD__GL ) THEN
           VTD%MF%STATUS = VTD__INIT
           VTD%MF%L_NOD = L_NOD
           VTD%MF%MF_NAME = SPD__GL_STR
!
           IF ( ASSOCIATED ( VTD%MF%EL_ARG ) ) THEN
                DEALLOCATE ( VTD%MF%EL_ARG )
           END IF
           ALLOCATE ( VTD%MF%EL_ARG(1:VTD%MF%L_NOD), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG( 2441, IUER, 'VTD_LOAD_MF', 'Error in an '// &
     &              'attempt to allocate memory for array VTD%MF%EL_ARG' )
                RETURN 
           END IF
!
           IF ( ASSOCIATED ( VTD%MF%MF_SPL ) ) THEN
                DEALLOCATE ( VTD%MF%MF_SPL )
           END IF
           ALLOCATE ( VTD%MF%MF_SPL(1-VTD__M_SPD:VTD%MF%L_NOD), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG( 2442, IUER, 'VTD_LOAD_MF', 'Error in an '// &
     &              'attempt to allocate memory for array VTD%MF%MF_SPL' )
                RETURN 
           END IF
!
           IF ( ASSOCIATED ( VTD%MF%MF_ARG ) ) THEN
                DEALLOCATE ( VTD%MF%MF_ARG )
           END IF
           ALLOCATE ( VTD%MF%MF_ARG(1:VTD%MF%L_NOD), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG( 2443, IUER, 'VTD_LOAD_MF', 'Error in an '// &
     &              'attempt to allocate memory for array VTD%MF%MF_ARG' )
                RETURN 
           END IF
!
           IF ( ASSOCIATED ( VTD%MF%EL_SPL ) ) THEN
                DEALLOCATE ( VTD%MF%EL_SPL )
           END IF
           ALLOCATE ( VTD%MF%EL_SPL(1-VTD__M_SPD:VTD%MF%L_NOD), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG( 2444, IUER, 'VTD_LOAD_MF', 'Error in an '// &
     &              'attempt to allocate memory for array VTD%MF%EL_SPL' )
                RETURN 
           END IF
!
           VTD%MF%STATUS = VTD__ALLC
!
           CALL ERR_PASS ( IUER, IER )
           CALL SPD_GAUSSIAN_LAYER_MF ( VTD%CONF%ATM_PARTIAL_PAR(1), &
     &                             VTD%CONF%ATM_PARTIAL_PAR(2), &
     &                             EL_MIN, EL_MAX, VTD__M_SPD, VTD%MF%L_NOD, &
     &                             VTD%MF%EL_ARG, VTD%MF%MF_SPL, &
     &                             VTD%MF%MF_ARG, VTD%MF%EL_SPL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG( 2445, IUER, 'VTD_LOAD_MF', 'Error in an '// &
     &              'attempt to compute the coefficients of the B-spline '// &
     &              'expansion of mapping function for the Gaussian layer '// &
     &              'model' )
                RETURN 
           END IF
           VTD%MF%STATUS = VTD__LOAD
         ELSE 
           CALL INCH ( VTD%CONF%ATM_PARTIAL_TYPE, STR )
           CALL ERR_LOG( 2446, IUER, 'VTD_LOAD_MF', 'Trap of internal '// &
     &         'control: ampping function model '//STR(1:I_LEN(STR))// &
     &         ' model is not supported' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_MF  !#!#
