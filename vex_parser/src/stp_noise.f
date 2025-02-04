      SUBROUTINE STP_NOISE ( FIL_VEX, FIL_STP, FREQ, KAPPA, NOISE, IUER)
!
! ***************************************************************************
! *                                                                         *
! *   Routine STP_NOISE computes the station noise given the loss factor    *
! *   due to a non-ractangular bandpass shape, and a Vex file to be used    *
! *   to compute the number of recorded samples over the band used.         *
! *                                                                         *
! *   N.B: - It is assumed that all sampling is at 2 bit quantization,      *
! *          therefore the quantization loss factor is kept constant.       *
! *        - For now, the Loss factor due to non-rectangular bandpass       *
! *          shape, is input as 0.9. Later we will include in the STP       *
! *          file.                                                          *
! *                                                                         *
! *   INPUT:                                                                *
! *            FIL_VEX   =  VEX File                    { CHAR }            *
! *                                                                         *
! *            FIL_STP   =  STP File                    { CHAR }            *
! *                                                                         *
! *            STA_ID    =  Station ID                  { CHAR }            *
! *                                                                         *
! *            FREQ      =  Frequency                   { REAL }            *
! *                                                                         *
! *            KAPPA     =  Loss factor                 { REAL }            *
! *                         (due to non-rectangular bandpass shape)         *
! *                                                                         *
! *            IUER      =  Error Handler               { INT, OPT }        *
! *                         If IUER=0 no error message will be printed,     *
! *                         even in the event of an error. However, for     *
! *                         other possible values, i.e. IUER=-1,-2, & -3,   *
! *                         the error message will print to screen. For     *
! *                         the latter case, i.e. IUER=-3, after printing   *
! *                         the program will terminate.                     *
! *                         Default, IUER = -1                              *
! *                                                                         *
! *   OUTPUT:                                                               *
! *            NOISE     =  Noise                       { REAL }            *
! *                                                                         *
! *   ###  31-OCT-2020  STP_NOISE   v1.0 (c)  N. Habana  31-OCT-2020  ###   *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE 
      INCLUDE   'vex.i'
      INCLUDE   'stp.i'
      TYPE ( STP__TYPE ) :: STP
      TYPE ( VEX_TYPE ) :: VEX
      CHARACTER  FIL_VEX*(*), FIL_STP*(*)
      INTEGER*4  IUER,ISDR
      REAL*8     FREQ, KAPPA, NOISE
      REAL*8     ETA, BT
      PARAMETER  ( ETA = 0.8825D0 )
      INTEGER*4  J0, J1, J2, J3, J4, J5
      INTEGER*4  FL_STA, FRQ_IDX
      INTEGER*4  N_STA, N_SOU, N_SCA, N_FRQ, N_IFS
      CHARACTER  BAND_ID*2
      INTEGER*4  NO_OF_IFS
!
! --- Parse the Vex File and STP file
!
      CALL STP_PARSER ( STP, FIL_STP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 2002, IUER, 'STP_NOISE',                      &
     &          'Error in parsing stp file '//FIL_STP )                  
           RETURN 
      END IF
! ---
      ISDR = 2
      CALL VEX_PARSER ( VEX, FIL_VEX, ISDR, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 2001, IUER, 'STP_NOISE',                      &
     &          'Error in parsing vex file '//FIL_VEX )                  
           RETURN 
      END IF
!
! --- Count the number of stations and freq.
!
      CALL VEX_COUNT (N_STA, N_SOU, N_SCA, N_FRQ, N_IFS, FIL_VEX, IUER)
!
! --- Check if the station you want is part of the Vex File
!
      FL_STA = 0
      DO 110 J0 = 1, VEX%N_STA
         IF ( VEX%STA(J0)%SITE_ID == STP%SHORT_NAME ) THEN
            FL_STA = 1
            GO TO 115
         END IF
 110  CONTINUE

!
! --- If the station is not part of Vex File
!      
      IF ( FL_STA == 0 ) THEN
         CALL ERR_LOG ( 2003, IUER, 'STP_NOISE',                        &
     &          STP%NAME//' is not part of '//FIL_VEX )
         RETURN
      END IF
! ---
 115  CONTINUE
!
! --- Find which Freq. this station is on.
!
      DO 120 J0 = 1, N_FRQ
         DO 130 J1 = 1, VEX%FRQ(J0)%N_STA
            IF ( VEX%FRQ(J0)%SITE_ID(J1) .EQ. STP%SHORT_NAME ) THEN
               FRQ_IDX = J0
               GO TO 125
            END IF
 130     CONTINUE
 120  CONTINUE
! ---
 125  CONTINUE
!
! --- Find the Freq. Band of our range, and count the no. of IF's
!
      IF ( (FREQ .GE. 2.1D9) .AND. (FREQ .LE. 2.4D9) ) THEN
         BAND_ID = 'S'
         NO_OF_IFS = VEX%FRQ(FRQ_IDX)%N_SX_CHA(1)
      ELSEIF ( (FREQ .GE. 7.9D9) .AND. (FREQ .LE. 9.1D9) ) THEN
         BAND_ID = 'X'
         NO_OF_IFS = VEX%FRQ(FRQ_IDX)%N_SX_CHA(2)
      END IF
!
! --- Compute the number of recorded samples over the band used, BT.
!     BT == (Sampling rate at one IF) x (no. of IFs in that Band)
!
      BT = VEX%FRQ(FRQ_IDX)%SAMPLE_RATE*NO_OF_IFS
!
! --- Compute the Noise
!
      NOISE = 1.D0/(ETA*DSQRT(2.D0*KAPPA*BT))
!
      RETURN
      END SUBROUTINE
