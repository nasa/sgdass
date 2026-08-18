      SUBROUTINE QSITPL(QSITN,NUMSTA,PLATES)
      IMPLICIT NONE
!
! 1.  SITPL PROGRAM SPECIFICATION
!
! 1.1 Find tectonic plate for each site.
!
! 1.2 REFERENCES:
!
! 2.  SITPL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NUMSTA
      character*8 QSITN(*)
!
! QSITN - Array containing site names
! NUMSTA - Number of sites to be looked up
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*4 PLATES(*)
!
! PLATES - Plate names corresponding to sites in ISITN
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 MAX_SITES
      PARAMETER (MAX_SITES = 500)
!
!      character*20 buf
      CHARACTER*4 PLNM(MAX_SITES)
      CHARACTER*8 QSTN(MAX_SITES)
      INTEGER*2 I,J,ITOTAL
      CHARACTER*290 errstr
      character*50 fname
      character*80 qrstr
!      EQUIVALENCE (KSTN,KSTN_CHR)
!
! I,J - Loop indices
! ITOTAL - Number of sites in look-up table
! KSTN,KSTN_CHAR - Table of site names
! PLNM - Table of plate names corresponding to sites in KSTN
!
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   DSC   931102 cloned from sitpl to eliminate yucky integer-characeter
!                equivalences
!   KDB   950307 Stop calling ferr from here - gsnoop does not set up
!                properly to open the ERRF file.
!   KDB   951016 Fix to access proper sitpl.dat (via parameter, not hard
!                coded path).
!   kdb   971112 move solve.i from /data15 to /data18
!   kdb   990422 relative include reference, now that gsnoop has joined the
!                standard source directory tree.
!
! 5.  SITPL PROGRAM STRUCTURE
!
!     Open and read list of sites/plates
!
      itotal = 0
      fname=SOLVE_SAVE_DIR//SITPL_FILE
!      fname=PRE_SAV_DIR(:PRE_SV_LEN)//SITPL_FILE
      open(97,file=fname)
      do while (.TRUE.)
        itotal = itotal+1
        read(97,'(a8,3x,a4)',END=50) qstn(itotal),plnm(itotal)
      enddo
50    close(97)
      itotal = itotal-1
!
!     Find plate name for each site
!
      DO I=1,NUMSTA
        PLATES(I)='    '
        DO  J=1,ITOTAL
          IF(QSITN(I) .EQ. QSTN(J)) PLATES(I)=PLNM(J)
        end do
!
        IF(PLATES(I).EQ.'    ') then
          WRITE(qrstr,'( &
     &  "  Site ",A," missing in list of sites and their tectonic", &
     &  "  plates. ")') &
     &    QSITN(I)
          call as2nl(qrstr)
          WRITE(qrstr,'("Please add to the list in ",A)') &
     &      SOLVE_SAVE_DIR//SITPL_FILE
          call as2nl(qrstr)
        Endif
!
      end do   ! I
      RETURN
      END
