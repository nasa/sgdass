      SUBROUTINE  STP_SIGNAL ( SCORR, SEFD_1, SEFD_2, SIGNAL, IUER )
!
! ************************************************************************************
! *                                                                                  *
! *   Routine STP_SIGNAL computes the signal between two stations, given  their      *
! *   their correlated flux density, and respective SEFDs.                           *
! *                                                                                  *
! *   INPUT:                                                                         *
! *           SCORR   =  Correlated Flux Density                   { REAL } [Jy]     *
! *                                                                                  *
! *           SEFD_1  =  System Equivalent Flux Density at Sta 1   { REAL } [Jy]     *
! *                                                                                  *
! *           SEFD_2  =  System Equivalent Flux Density at Sta 2   { REAL } [Jy]     *
! *                                                                                  *
! *           IUER    =  Error Handler                             { INT, OPT }      *
! *                      If IUER=0 no error message will be printed, even in the     *
! *                      event of an error. However, for  other possible values,     *
! *                      i.e. IUER=-1,-2, & -3, the error message will print to      *
! *                      screen. For the latter case, i.e. IUER=-3, after            *
! *                      printing the program will terminate.                        *        
! *                      Default, IUER = -1                                          *
! *                                                                                  *
! *   OUTPUT:                                                                        *
! *           SIGNAL  =  Computed signal                           { REAL }          *
! *                                                                                  *
! *   ###    29-OCT-2020  STP_SIGNAL    v1.0 (c)   N. Habana    29-OCT-2020   ###    *
! *                                                                                  *
! ************************************************************************************
!
      IMPLICIT    NONE
      INTEGER*4   IUER
      REAL*8      SCORR, SEFD_1, SEFD_2, SIGNAL
!
! --- Compute the signal
!
      SIGNAL = SCORR/DSQRT(SEFD_1*SEFD_2)

      RETURN
      END SUBROUTINE
