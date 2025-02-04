      SUBROUTINE FMTGPS_TIME_FILTER_RAW ( ANC, IND_TAG, NP, TIM, FMGT,  &
     &                                    FMPT, IUER )
!
! *******************************************************************************************
! *                                                                                         *
! *   Routine  FMTGPS_TIME_FILTER_RAW                                                       *
! *   N.B: This routine does not remove outliers/spikes, those are removed when averaging   *
! *                                                                                         *
! *   INPUT:                                                                                *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }          *
! *                                                                                         *
! *          IND_TAG       =  Time Tag Index                      { INT*4 }                 *
! *                                                                                         *
! *          IUER          =  Error Handler                       { INT*4, OPT }            *
! *                           If IUER=0 no error message will be printed,  even in          *
! *                           the event of an error. However, for other possible            *
! *                           values, i.e. IUER=-1,-2, & -3, the error message will         *
! *                           print to screen. For the latter case, i.e., IUER = -3,        *
! *                           after printing the program will terminate.                    *
! *                           Default, IUER = -1                                            *
! *                                                                                         *
! *   OUTPUT:                                                                               *
! *          NP            =  Filtered Number of points           { INT*4 }                 *
! *                                                                                         *
! *          TIM           =  Filtered Time array                 { REAL*8 }  (NPx1)  [s]   *
! *                                                                                         *
! *          FMGT          =  Filtered Formater minus GPS array   { REAL*8 }  (NPx1)  [s]   *
! *                                                                                         *
! *          FMPT          =  Filtered Formater minus PPS array   { REAL*8 }  (NPx1)  [s]   *
! *                                                                                         *
! * ###  31-JUL-2023   FMTGPS_TIME_FILTER_RAW        v1.0 (c)  N. Habana  31-JUL-2023   ### *
! *                                                                                         *
! *******************************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC
      INTEGER*4  IND_TAG, NP, IUER
      REAL*8     TIM(ANC__MEPC), FMGT(ANC__MEPC), FMPT(ANC__MEPC)
      INTEGER*4  NS, KB, KE, IP, KP
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      REAL*8     DEL_MIN, DELT1, DELT2
      PARAMETER  ( DEL_MIN = 1.D-6 )

!     
! --- Initial Values
!
      NP = 0                    ! No. of filtered points
      TIM      = 0.D0
      FMGT     = 0.D0
      FMPT     = 0.D0
!
! --- Filter all points, and emerge with the clean raw data 
!
      DO 410 J1 = 1, ANC%NUM_GPS
!
! ------ Eliminate NaN values, and filter out points
!
         IF ( .NOT. IS_R8_NAN ( ANC%GPS(J1)%TIM )            .AND.      &
     &        .NOT. IS_R8_NAN ( ANC%GPS(J1)%FMG(IND_TAG) )   .AND.      &
              .NOT. IS_R8_NAN ( ANC%GPS(J1)%FMP(IND_TAG) )) THEN
!
! --------- Collect only values that are not fillers
! ---------            
!
            DELT1 = ABS( ANC%GPS(J1)%FMG(IND_TAG) - ANC__FILLER_R8 )
            DELT2 = ABS( ANC%GPS(J1)%FMP(IND_TAG) - ANC__FILLER_R8 )
            IF ( (DELT1 < DEL_MIN) .OR. ( DELT2 < DEL_MIN) ) GOTO 410
!
! --------- Store values for the filtered points
!
            NP = NP + 1                                                 ! Number of points update
            TIM(NP)    =  ANC%GPS(J1)%TIM                               ! 
            FMGT(NP)   =  ANC%GPS(J1)%FMG(IND_TAG)
            FMPT(NP)   =  ANC%GPS(J1)%FMP(IND_TAG)
         END IF
 410  CONTINUE
!
!###!      DO J1 = 1, 10   
!###!         DO J2 = 1, 4
!###!            PRINT *, "%%%%%%", J1, J2, ANC%GPS(J1)%TIM , ANC%GPS(J1)%FMG(J2) !, ANC%GPS(J1)%FMP(J2) ! "%%%%% FMTGPS_FILTER - 6
!###!         END DO
!###!      END DO
      CALL ERR_LOG ( 0, IUER )



      

!#      IUER = -1
  !#    CALL DIAGI_1 (NP, TIM, FMGT, IUER)

      
      RETURN
      END SUBROUTINE FMTGPS_TIME_FILTER_RAW !#!1
!
! ---------------------------------------------------------------------------------------
!
