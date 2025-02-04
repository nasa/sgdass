      SUBROUTINE INATM(ISITE,TEPOC,JATM)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  INATM PROGRAM SPECIFICATION
!
! 1.1 Insert atmosphere parameter epochs
!
! 1.2 REFERENCES:
!
! 2.  INATM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISITE
      REAL*8 TEPOC
!
! ISITE - Site number in the PARFIL list
! TEPOC - Complete Julian Date of epoch
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 JATM
!
! JATM - The number of the new epoch relative to the entire list
!        of atmosphere epochs.  JATM is -1 if atmosphere epoch
!        is already in the list or the atmosphere list is full
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
      CHARACTER*70 errstr
      CHARACTER*80 bufstr
      integer*2 i,j,kbitn
!
! 4.  HISTORY
!   who   when   what
!   JWR  860617  Replaced COMPC structures with FTN&& structures
!   KDB  860623  Fixed bug, for 2nd,3rd database etc.
!                IATSTR(NUMSTA) had the wrong value (0)
!   MK   861105  Added atmosphere rate
!   AEE  910515  Enhanced error messages written to the error file.
!
! 5.  INATM PROGRAM STRUCTURE
!
!
!     Make certain there is room in the atmosphere epochs list.
!
      IF  ((NUMATM(NUMSTA)+IATSTR(NUMSTA)) .GE. &
     &MAX_ATM)THEN  !too many epochs
        JATM=-1
 1000   FORMAT(" More than ",I4," clock epochs - too many")
        WRITE(errstr,1000) MAX_ATM
          call ferr( INT2(194), errstr, INT2(0), INT2(0) )
          RETURN
          END IF  !too many epochs
!
!
!     Make certain it is not already in the list
!
      JATM=-1
      IF  (NUMATM(ISITE).GT. &
     &0)THEN  !this site has at least one epoch
        DO  I=IATSTR(ISITE)+1,IATSTR(ISITE)+NUMATM(ISITE)
          IF(ABS(TATM(I)-TEPOC).LT.0.1D0/86400.0D0) RETURN
        END DO
      END IF  !this site has at least one epoch
!
!     Determine the position of the new epoch.
!     The correct position is just before the first first epoch after
!     this epoch or at the end of the list for this station.
!
!
      JATM = IATSTR(ISITE)+1
      DO WHILE ( (TEPOC .GT.  TATM(JATM)                  ) .AND. &
     &         (JATM  .LE. (IATSTR(ISITE)+NUMATM(ISITE)))      )
        JATM = JATM+1
      END DO
!
!     JATM is the location of the new atmosphere epoch
!     Move the epochs and the flags for the succeeding epochs
!
      DO  I=IATSTR(NUMSTA)+NUMATM(NUMSTA),JATM,-1
!
!       clear a hole by moving from the top down
!
        TATM(I+1) = TATM(I)
        CALL SBIT( LATM(1,1), INT2(I+1), KBITN(LATM(1,1),I) )
        CALL SBIT( LATM(1,2), INT2(I+1), KBITN(LATM(1,2),I) )
        CALL SBIT( LATM(1,3), INT2(I+1), KBITN(LATM(1,3),I) )
      END DO  !clear a hole by moving from the top down
!
!     Insert the new epoch
!
      TATM(JATM) = TEPOC
      CALL SBIT( LATM(1,1), JATM, INT2(0) )
      CALL SBIT( LATM(1,2), JATM, INT2(0) )
      CALL SBIT( LATM(1,3), JATM, INT2(0) )
!
!     Increment the counters for the succeeding stations
!     First handle the start counter
!
      IF  (ISITE.LT. &
     &MAX_ARC_STA)THEN  !not the last, so do it
        DO  J = ISITE+1, MAX_ARC_STA
          IATSTR(J) = IATSTR(J)+1
        END DO
      END IF  !not the last, so do it
!
!     Increment the asmosphere counter for this station
!
      NUMATM(ISITE) = NUMATM(ISITE) + 1
!
      RETURN
      END
