      SUBROUTINE STP_OBS_ERR ( VEX, STP, IND_STA_STP, SOU_NAM, SNR, &
     &                         N_IFS, IND_IFS, BAND_MODE, TAU_GR_ERR, IUER )
! ***************************************************************************
! *                                                                         *
! *   Routine STP_GR_ERR computes the uncertainty group delay as the        *
! *   uncertainty of the linear trend over a given no. of IF.               *
! *                                                                         *
! *   INPUT:                                                                *
! *         VEX          =  VEX Object                   { Derived Type }   *
! *                                                                         *
! *         STP_STA      =  STP Object (per station)     { Derived Type }   *
! *                                                                         *
! *         IND_STA_STP  =  Station Indices in STP       { INT } (2x1)      *
! *                                                                         *
! *         SOU_NAM      =  Source Name                  { CHAR }           *
! *                                                                         *
! *         SNR          =  Signal-to-Noise Ratio        { REAL }           *
! *                         This is computed over the given band.           *
! *                                                                         *
! *         IUER         =  Error Handler                { INT, OPT }       *
! *                         If IUER=0 no error message will be printed,     *
! *                         even in the event of an error. However, for     *
! *                         other possible values, i.e. IUER=-1,-2, & -3,   *
! *                         the error message will print to screen. For     *
! *                         the latter case, i.e. IUER=-3, after printing   *
! *                         the program will terminate.                     *
! *                         Default, IUER = -1                              *
! *                                                                         *
! *   OUTPUT:                                                               *
! *         TAU_GR_ERR       =  Group delay                  { REAL } [s]   *
! *                                                                         *
! *   ###  01-NOV-2020  STP_GR_ERR v3.0 (c)  N. Habana  08-DEC-2020  ###    *
! *                                                                         *
! ***************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'astro_constants.i'
      INCLUDE    'vex.i'
      INCLUDE    'stp.i'
      TYPE     ( STP__TYPE ) :: STP
      TYPE     ( VEX_TYPE  ) :: VEX
      INTEGER*4  IUER
      INTEGER*4  IND_STA_STP(2), N_IFS(4), IND_IFS(VEX__MCHA,4) 
      REAL*8     TAU_GR_ERR(4)
      CHARACTER  SOU_NAM*(*), BAND_MODE*(*)
      REAL*8     SNR(4), PHASE_UNCERTAINTY, FRQ
      INTEGER*4  J0, J1, J2, J3, J4, J5
      INTEGER*4  FL_STA(2), FRQ_IDX1, FRQ_IDX2
      CHARACTER  BAND_ID*1
      INTEGER*4  N_IFS1, N_IFS2, IJOB
      REAL*8     SUM_ANG_FRQ_SQ(4), SUM_ANG_FRQ(4), ERR(4)
      REAL*8     A1, A2, A3, A4, A5
      INTEGER*4, EXTERNAL :: VEX_STA_ID_IDX
      INTEGER*4, EXTERNAL :: LINDEX, IFIND_PL
!
! --- Check if the stations you want are part of the Vex File
!
      FL_STA(1) = VEX_STA_ID_IDX( VEX,STP%STA(IND_STA_STP(1))%SHORT_NAME )
      FL_STA(2) = VEX_STA_ID_IDX( VEX,STP%STA(IND_STA_STP(2))%SHORT_NAME )
!
! --- If station one is not part of Vex File
!      
      IF ( FL_STA(1) == 0 ) THEN
           CALL ERR_LOG ( 2001, IUER, 'STP_OBS_ERR',                      &
     &          STP%STA(IND_STA_STP(1))%NAME//' is not part of '//      &
     &          'VEX object '//TRIM(VEX%EXPER_NAME) )
         RETURN
      END IF
!
      IF ( FL_STA(2) == 0 ) THEN
           CALL ERR_LOG ( 2002, IUER, 'STP_OBS_ERR',                       &
     &          STP%STA(IND_STA_STP(2))%NAME//' is not part of'//       &
     &          'VEX object '//TRIM(VEX%EXPER_NAME) ) 
         RETURN
      END IF
!
! --- Find the Frequency each station is on.
!
      DO 120 J0 = 1, VEX%N_FRQ
         DO 130 J1 = 1, VEX%FRQ(J0)%N_STA
            IF ( VEX%FRQ(J0)%SITE_ID(J1) ==                             &
     &           STP%STA(IND_STA_STP(1))%SHORT_NAME ) THEN
               FRQ_IDX1 = J0
            ELSE IF ( VEX%FRQ(J0)%SITE_ID(J1) ==                        &
     &                STP%STA(IND_STA_STP(2))%SHORT_NAME ) THEN
               FRQ_IDX2 = J0
            END IF
 130     CONTINUE
 120  CONTINUE
!
! --- Initialize the accumulators
!
      SUM_ANG_FRQ     = 0.D0 
      SUM_ANG_FRQ_SQ  = 0.D0 
!
      DO 410 J1=1,4
         IF ( N_IFS(J1) > 0 ) THEN
              DO 420 J2=1,N_IFS(J1)
                 FRQ = VEX%FRQ(FRQ_IDX1)%SKY_FRQ(IND_IFS(J2,J1))
!
! -------------- Sum the angular sky frequencies
!
!                SUM_ANG_FRQ(J1)    = SUM_ANG_FRQ(J1) + FRQ
                 SUM_ANG_FRQ_SQ(J1) = SUM_ANG_FRQ_SQ(J1) + FRQ**2
 420          CONTINUE 
!
              IF ( SNR(J1) > STP__SNR_MIN ) THEN
                   TAU_GR_ERR(J1) = DSQRT ( P2I/SNR(J1)**2 + P2I/STP__SNR_FLOOR**2 )/  &
                                   (1.D-12 + PI2*DSQRT( SUM_ANG_FRQ_SQ(J1)/N_IFS(J1) - &
     &                                                  SUM_ANG_FRQ(J1)/N_IFS(J1)**2  ))
                 ELSE
                   TAU_GR_ERR(J1) = 0.0D0
              END IF
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE  STP_OBS_ERR   !#!
