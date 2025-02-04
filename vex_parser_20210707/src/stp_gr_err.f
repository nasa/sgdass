      SUBROUTINE STP_OBS_ERR ( VEX, STP, IND_STA_STP,                   &
     &                         SOU_NAM, FREQ, SNR, TAU_GR, IUER )
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
! *         FREQ         =  Frequency                    { REAL }           *
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
! *         TAU_GR       =  Group delay                  { REAL } [s]       *
! *                                                                         *
! *   ###  01-NOV-2020  STP_GR_ERR v3.0 (c)  N. Habana  08-DEC-2020  ###    *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE    'vex.i'
      INCLUDE    'stp.i'
      INCLUDE    'astro_constants.i'
      TYPE ( STP__TYPE ) :: STP
      TYPE ( VEX_TYPE ) :: VEX
      INTEGER*4  IUER
      INTEGER*4  IND_STA_STP(2)
      REAL*8     FREQ, TAU_PH, TAU_GR
      CHARACTER  SOU_NAM*16
      REAL*8     SNR, PHASE_UNCERTAINTY
      INTEGER*4  J0, J1, J2, J3, J4, J5
      INTEGER*4  FL_STA(2), FRQ_IDX1, FRQ_IDX2
      CHARACTER  BAND_ID*1
      INTEGER*4  N_IFS1, N_IFS2, IJOB
      REAL*8     SUM_ANG_FRQ_SQ, SUM_ANG_FRQ
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
! --- Find the Freq. Band of our Range, and count the no. of IF's 
!     for each station.
!
      IF ( (FREQ .GE. SBND__RNG(1)) .AND. (FREQ .LE. SBND__RNG(2)) ) THEN
         BAND_ID =  'S'
         N_IFS1  =  VEX%FRQ(FRQ_IDX1)%N_SX_CHA(1)
         N_IFS2  =  VEX%FRQ(FRQ_IDX2)%N_SX_CHA(1)
      ELSEIF ( (FREQ .GE. XBND__RNG(1)) .AND. (FREQ .LE. XBND__RNG(2)) ) THEN
         BAND_ID =  'X'
         N_IFS1  =  VEX%FRQ(FRQ_IDX1)%N_SX_CHA(2)
         N_IFS2  =  VEX%FRQ(FRQ_IDX2)%N_SX_CHA(2)
      END IF
!
! --- Compute the phase uncertainty at each IF
!
      IF ( SNR .GT. 10.D0 ) THEN 
         TAU_PH =  DSQRT(REAL(N_IFS1, 8))/SNR
      ELSE
         TAU_PH = 1.D0/SNR
      END IF
!
! --- Preallocate sums
!
      SUM_ANG_FRQ     = 0.D0 
      SUM_ANG_FRQ_SQ  = 0.D0 
!
      IF ( BAND_ID .EQ. 'S' ) THEN
         DO 210 J1 = 1, N_IFS1
!
! --------- Sum the angular sky frequencies
!
            SUM_ANG_FRQ = SUM_ANG_FRQ + VEX%FRQ(FRQ_IDX1)%              &
     &           SKY_FRQ(VEX%FRQ(FRQ_IDX1)%IDX_SX_CHA(1,J1))
!
            SUM_ANG_FRQ_SQ = SUM_ANG_FRQ_SQ + VEX%FRQ(FRQ_IDX1)%        &
     &           SKY_FRQ(VEX%FRQ(FRQ_IDX1)%IDX_SX_CHA(1,J1))**2

 210     CONTINUE
      ELSE
         DO 220 J1 = 1, N_IFS1
!
! --------- Sum the angular sky frequencies
!
            SUM_ANG_FRQ = SUM_ANG_FRQ + VEX%FRQ(FRQ_IDX1)%              &
     &           SKY_FRQ(VEX%FRQ(FRQ_IDX1)%IDX_SX_CHA(2,J1))
!
            SUM_ANG_FRQ_SQ = SUM_ANG_FRQ_SQ + VEX%FRQ(FRQ_IDX1)%        &
     &           SKY_FRQ(VEX%FRQ(FRQ_IDX1)%IDX_SX_CHA(2,J1))**2
!
 220     CONTINUE 
      END IF
!
      TAU_GR = 1.D0/(SNR*(1.D-12 + PI2*DSQRT( (SUM_ANG_FRQ_SQ/N_IFS1) -   &
     &         (SUM_ANG_FRQ/N_IFS1)**2 )))
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE  STP_OBS_ERR   !#!
