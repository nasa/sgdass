      SUBROUTINE INCLK ( ISITE, TJD, KLOC, KTEST )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  INCLK PROGRAM SPECIFICATION
!
! 1.1 Insert clock polynomial epochs in the SOLVE clock
!     solution list.
!
! 1.2 REFERENCES:
!
! 2.  INCLK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISITE
      LOGICAL*2 KTEST
      REAL*8 TJD
!
! ISITE - The site number of interest
! KTEST - If FALSE, don't check for epoch already in list
! TJD - The Julian date of the new epoch
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 KLOC
!
! KLOC - The number of the new epoch relative to the entire list
!        of clock epochs. KLOC is -1 if the clock epoch is already
!        in the list
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
      CHARACTER  ERRSTR*70, BUFSTR*80
      REAL*8    CENTIMIN
      INTEGER*2 I, J, K
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  860513  Fixed bug in logic to see if clock is already in list
!   JWR  860617  Replaced COMPC structures with FTN77 structures
!   KDB  Fixed bug -- for 2nd,3rd databases,etc.
!        ICLSTR(NUMSTA) had the wrong value (0)
!   MWH  910402  Fixed epoch insertion bug causing lost parameters
!   AEE  910515  Enhanced error messages written to the error file.
!   MWH  910723  Allow inserting epochs in batch_mode clocks
!   PET  971027  Enhanced comments. Spesification "offest" for the epoch
!                following justy after the clock break added.
!
! 5.  INCLK PROGRAM STRUCTURE
!
!C
!
! --- Make certain the list of epochs is not already full.
!
      CENTIMIN = 1.0D0/(100.0*60.0*24.0)
      IF ( ICLSTR(NUMSTA) + NUMCLK(NUMSTA) .GE. MAX_CLK ) THEN
!
! -------- List already full
!
           KLOC = -1
 1000      FORMAT ( " More than ",I4," clock epochs" )
           WRITE ( ERRSTR, 1000 ) MAX_CLK
           CALL FERR ( INT2(195), ERRSTR, INT2(0), INT2(0) )
           write ( 6, * ) iclstr(kloc) ! In order to deliberatry stop the process
           RETURN
      END IF ! List already full
!
! --- Make certain the epoch is not already in the list.
!
      KLOC = -1
      IF ( NUMCLK(ISITE) .GT.0 .AND. KTEST ) THEN
!
! -------- This site has at least one epoch
!
           DO I = ICLSTR(ISITE)+1,ICLSTR(ISITE)+NUMCLK(ISITE)
              IF(ABS(TJD-FJDCL(I)).LT.0.1D0/86400.0D0) RETURN
           END DO
      END IF  !this site has at least one epoch
!
! --- Determine the position of the new epoch.
! --- The correct position will be immediately before the first epoch
! --- which is after this epoch. However, if there is no such epoch
! --- it goes at the end of the list
!
      KLOC = ICLSTR(ISITE) + 1
      DO WHILE ( TJD .GT. FJDCL(KLOC)-0.1/86400.0D0   .AND. &
     &          KLOC .LE. ICLSTR(ISITE)+NUMCLK(ISITE)       )
         KLOC = KLOC+1
      END DO
!
! --- Move the epochs and the flags for the succeeding polys
!
      DO K = ICLSTR(NUMSTA)+NUMCLK(NUMSTA),KLOC,-1
!
! ------ Clear a hole
!
         FJDCL(K+1) = FJDCL(K)
         LCLK( K+1) = LCLK(K)
         DO I=1,2
            ICLSTA(I,K+1) = ICLSTA(I,K)
         ENDDO
      END DO  !clear a hole
!
! --- Insert the new parameters
!
      FJDCL(KLOC) = TJD
      LCLK( KLOC) = 0
      DO I=1,2
         ICLSTA(I,KLOC)=0
      ENDDO
      CALL SBIT ( ICLSTA(1,KLOC), ISITE, INT2(1) )
!
! --- Increment the clock counter for this station
!
      NUMCLK(ISITE) = NUMCLK(ISITE) + 1
!
      IF ( BMODE_CL ) THEN
           DO K = ICLSTR(ISITE)+NUMCLK(ISITE), KLOC, -1
!
! ----------- Clear a hole
!
              FJDCL(K+1) = FJDCL(K)
              LCLK( K+1) = LCLK(K)
              DO I=1,2
                 ICLSTA(I,K+1) = ICLSTA(I,K)
              ENDDO
           END DO ! clear a hole
!
! -------- Insert the new parameters
!
           FJDCL(KLOC) = TJD - CENTIMIN
           LCLK( KLOC) = 0
           DO I=1,2
              ICLSTA(i,KLOC)=0
           ENDDO
!
           CALL SBIT ( ICLSTA(1,KLOC), ISITE, INT2(1) )
!
! -------- Set bit of continuity for the current epoch
!
           CALL SBIT ( LCLK(KLOC), INT2(13), INT2(1) )
!
! -------- ... And offset bit for the next epoch (just after the clock break)
! -------- added by L. Perov 27-OCT-97
!
           CALL SBIT ( LCLK(KLOC+1), INT2(1), INT2(1) )
!
! -------- Increment the clock counter for this station
!
           NUMCLK(ISITE) = NUMCLK(ISITE) + 1
      ENDIF
!
! --- Increment the counters for other stations
!
      IF ( BMODE_CL ) THEN
           DO J=1,NUMSTA
              NUMCLK(J) = NUMCLK(ISITE)
           ENDDO
        ELSE
           IF ( ISITE.LT.MAX_ARC_STA) THEN
!
! ------------- This is not the last site
!
                DO J = ISITE+1,MAX_ARC_STA
                   ICLSTR(J) = ICLSTR(J)+1
                END DO
            END IF ! This is not the last site
      ENDIF
!
      RETURN
      END  !#!  INCLK  #!#
