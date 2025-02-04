      SUBROUTINE  ATP_GEPM ( ANC, STA_NAM, T_ACCUM, GEPM_OVR,           &
     &                       TIME_THRESH, DIFF_THRESH, MAX_COUNT, IUER )
!
! ***************************************************************************
! *                                                                         *
! *   Routine PIMA_GEPM                                                     *
! *                                                                         *
! *   Automatically generatee phase calibration mask.                       *
! *                                                                         *
! *   INPUT:                                                                *
! *       ATP        =  ATP structure.                     { DERIVED TYPE } *
! *                                                                         *
! *       STA_NAM    = Station Name                        { CHAR }         *
! *                    Acepts station names (IVS convention) or "ALL" for   *
! *                    consideration of all stations in PIM                 *
! *                                                                         *
! *       T_ACCUM     = Target time for averaging          { REAL*8 }       *
!                       e.g. pcal may be sampled at 1 sample/second and we  *
!                       want to average the pcal data such that it is       *
!                       sampled at  1 sample per T_ACCUM seconds. The data  *
!                       is commonly irregularly sampled, though, so the     *
!                       sample rate is tuned via averaging such that T_ACCUM*
!                       is as close to the average time between samples as  *
!                       possible.                                           *
! *                                                                         *
! *       GEPM_OVR    = Overwrite request                  { LOGICAL*4 }    *
! *                     Should GEPM overwrite the existing Phase            *
! *                     calibration mask file?                              *
! *                                                                         *
! *       TIME_THRESH = threshold fraction for flagging    { REAL*8 }       *
!                       is the threshold fraction of epochs at which a tone *
!                       is flagged as spurious before it is deactivated,    *
!                       e.g. for TIME_THRESH = 0.1, if the tone is flagged  *
!                       as spurious in 10% of all epochs, it is deactivated *
! *                                                                         *
! *       DIFF_THRESH =                                    { REAL*8 }       *
!                       is the threshold distance on the complex plane      *
!                       between the spline and data curve that results      *
!                       in the tone being flagged at the epoch.             *
! *                                                                         *
! *       MAX_COUNT   =  Maximum number of ???             { INT*4 }        *
! *             ????-Confirm with Joe-?????
! *                                                                         *
! *        IUER       =  Error Handler                     { INT*4, OPT }   *
! *                      If IUER=0 no error message will be printed, even   *
! *                      in the event of an error. However, for other       *
! *                      possible values, i.e. IUER=-1,-2, & -3, the error  *
! *                      message will print to screen. For the latter       *
! *                      case, i.e., IUER = -3, after printing the program  *
! *                      will terminate.                                    *
! *                      Default, IUER = -1                                 *
! *                                                                         *
! *   OUTPUT:        ???-????                                               *
! *        PIM    =  Updated PIM structure                 { DERIVED TYPE } *
! *        exp_band_pcal_rms.txt - A file indicating the health of the      *
! *                                phase calibration tones in the form of   *
! *                                the root-mean-square phase jitter in the *
! *                                time domain and the frequency domain.    *    
! *                                                                         *
! *        exp_band_pcal_report.gen - This report file shows the specific   *
! *                                   tones that have been masked out as    *
! *                                   well as the reason for their masking  *
! *                                   outreport file                        *
! *                                                                         *
! *  ### 21-JUL-2023  PIMA_GEPM      v1.0 (c)  N. HABANA   21-JUL-2023 ###  *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT 
      IMPLICIT   NONE
      INCLUDE   'atp.i'
      INCLUDE    'diagi.i'
      TYPE ( ANC__TYP   ) :: ANC
      TYPE (DIAGI_STRU) :: DIA(2)






      END SUBROUTINE !#! ATP_GEPM
