!
! >>>>> INCLUDE-BLOCK with descriptions data structures
!       calibrations/contributions
!
! --- cals.i  01-SEP-97   v2.0  (c)  L. Petrov  --  18-NOV-99 13:24:01
!
! --- Constants
!
      INTEGER*4    MAXC_STA
      PARAMETER  ( MAXC_STA   = 32 ) ! maximal number of stations
      INTEGER*4  MC_CONT, MC_SCAL, MC_ZENC, MC_MCAL
      PARAMETER  ( MC_CONT = 15 ) ! maximal number of observation-dep contrib.
      PARAMETER  ( MC_SCAL = 10 ) ! maximal number of station-dep contributions
      PARAMETER  ( MC_ZENC = 10 ) ! maximal number of zenith calibrations
      PARAMETER  ( MC_MCAL =  5 ) ! maximal number of mode calibrations
      INTEGER*4    CAL__UNF, CAL__DONE
      PARAMETER  ( CAL__UNF  = -1  ) ! flag: CAL structure is undefined
      PARAMETER  ( CAL__DONE = 701 ) ! flag: CAL structure is defined
!
      TYPE      CALS_STRU
!
          INTEGER*4  L_STA    ! Number of stations
!
          INTEGER*4  L_SCAL   ! Number of station dependent calibrations
          INTEGER*4  L_ZENC   ! Number of mapping functions
          INTEGER*4  L_CONT   ! Number of observation-dep contributions
          INTEGER*4  L_MCAL   ! Number of mode calibrations
!
          CHARACTER  STANAM(MAXC_STA)*8 ! The list if station names
!
! ------- Names of calibrations
!
          CHARACTER  SCAL(MC_SCAL)*8 ! Station dependent calibration
          CHARACTER  ZENC(MC_ZENC)*8 ! Zenith calibration
          CHARACTER  CONT(MC_CONT)*8 ! Obs-dependent calibrations
          CHARACTER  MCAL(MC_MCAL)*8 ! Mode calibration
!
! ------- Lcodes asociated to calibrations
!
          CHARACTER  SCAL_LCODE(MC_SCAL)*8 ! Station dependent calibration
          CHARACTER  CONT_LCODE(MC_CONT)*8 ! Obs-dependent calibrations
          CHARACTER  MCAL_LCODE(MC_MCAL)*8 ! Mode calibration
!
! ------- Avialability status of calibrations:
! ------- .TRUE. -- available, .FALSE. -- not available
!
          LOGICAL*1  SCAL_AVL(MC_SCAL,MAXC_STA) ! Station dependent cal
          LOGICAL*1  ZENC_AVL(MC_ZENC,MAXC_STA) ! Zenith calibration
          LOGICAL*1  CONT_AVL(MC_CONT)          ! Obs-dependent calibrations
          LOGICAL*1  MCAL_AVL(MC_MCAL)          ! Mode calibration
!
! ------- Application status of calibrations
! ------- .TRUE. -- applied in computation of o-c, .FALSE. -- not applied
!
          LOGICAL*1  SCAL_APL(MC_SCAL,MAXC_STA) ! Station dependent cal
          LOGICAL*1  ZENC_APL(MC_ZENC,MAXC_STA) ! Zenith calibration
          LOGICAL*1  CONT_APL(MC_CONT)          ! Obs-dependent cal
          LOGICAL*1  MCAL_APL(MC_MCAL)          ! Mode calibration
!
! ------- Ionosphere calibration status
!
          LOGICAL*1  GION_AVL(MAXC_STA)  ! Availability of group ionosphere cal.
          LOGICAL*1  PION_AVL(MAXC_STA)  ! Availability of phase ionosphere cal.
!
          LOGICAL*1  GION_APL(MAXC_STA)  ! Apply status of group ionosphere cal.
          LOGICAL*1  PION_APL(MAXC_STA)  ! Apply status of phase ionosphere cal.
!
          INTEGER*2  NZCAL2_SAVE
!
! ------- Status of this data structure
!
          INTEGER*4  STATUS
      END TYPE  CALS_STRU  !  CALS_STRU  !
!
! <<<<< end of INCLUDE-BLOCK  cals.i
!
