      SUBROUTINE DVELP(LSINAM,NSITE,VSUBXYZ,JNSTA,JSITN, &
     &                 time0x,SITDIF,FBY_WARNING,NVSITEV,nvsitec)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DVELP PROGRAM SPECIFICATION
!
! 1.1 Calculate site velocity position differences
!
! 1.2 REFERENCES:
!
! 2.  DVELP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NSITE,JNSTA,LSINAM(4,*)
      INTEGER*2 JSITN(4,*)
      LOGICAL*2 FBY_WARNING
      REAL*8    VSUBXYZ(3,*), TIME0X
!
! FBY_WARNING - True if we want message when no match found
! JSITN - Names of stations in this arc
! JNSTA - Number of stations in this arc
! LSINAM - Station names for alternate positions
! NSITE - Number of stations with alternate positions
! VSUBXYZ - Alternate velocity positions
! TIME0X - Site ref date parameter
!
! 2.3 OUTPUT Variables:
!
      REAL*8 SITDIF(3,*),NVSITEV(3,*),NVSITEC(3,*)
!
! SITDIF - Site coordinate differences
! NVSITEV - Site velocity coordinates from mod file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbc2.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: svelp
!       CALLED SUBROUTINES: none
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I, K, ITEST, J, TRIMLEN
      LOGICAL*2 PAUSE_FLYBY,equal,starc
      REAL*8    DT_YEAR, T0, FJDOBS, LJDOBS
      CHARACTER errstr*80, BUFSTR*80
      character*1 styp
!
! I,J,K - Loop indices
! ITEST - Flag used for 'no match' condition
! PAUSE_FLYBY - True if we are to pause and warn of missing sites
!
! 4.  HISTORY
!   WHO   WHEN      WHAT
!   AEE   910314    First version
!   AEE   910515    Enhanced error messages written to the error file.
!   MWH   930713    Use fjdobs from obstm instead of tatm for start time
!   pet   06-APR-99  Added support of FLUBY_WARNING for BATCH mode also
!   pet   2002.09.30 Fixed  a bug: the previous version comuted station at the
!                    epoch a day before of the nominal start of the session
!
! 5.  DVELP PROGRAM STRUCTURE
!
      PAUSE_FLYBY = .FALSE.
      T0 = TIME0X
!
      CALL OBSTM ( FJDOBS, LJDOBS )
      DT_YEAR = (FJDOBS + LJDOBS)/2.0/YEAR__TO__DAY - T0
!
! --- Generate site velocities and differences
!
      IF ( JNSTA .GT. MAX_ARC_STA ) THEN
942        FORMAT ( 'Too many stations, only',I3,' per session allowed.')
           WRITE ( ERRSTR, 942 ) MAX_ARC_STA
           CALL FERR ( INT2(168), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
      DO  I=1,JNSTA
!
!     checking the table read from the data base.
!
          ITEST = 0
          J = 0
          DO WHILE (ITEST.EQ.0 .AND. J.LT.NSITE)
!
!     running over the list of alternate postions
!
              J=J+1
                IF  (JSITN(1,I).EQ.LSINAM(1,J) .AND. &
     &             JSITN(2,I).EQ.LSINAM(2,J) .AND. &
     &             JSITN(3,I).EQ.LSINAM(3,J) .AND. &
     &             JSITN(4,I).EQ.LSINAM(4,J)) &
     &          THEN  !match found
!                   Update velocity for each site (change from mm/yr to m/yr):
                   NVSITEV(1,I)=NVSITEV(1,I) + VSUBXYZ(1,J)/1000
                   NVSITEV(2,I)=NVSITEV(2,I) + VSUBXYZ(2,J)/1000
                   NVSITEV(3,I)=NVSITEV(3,I) + VSUBXYZ(3,J)/1000
!                   Update site position differences:
                   SITDIF(1,I) = SITDIF(1,I) +(VSUBXYZ(1,J)/1000)*DT_YEAR
                   SITDIF(2,I) = SITDIF(2,I) +(VSUBXYZ(2,J)/1000)*DT_YEAR
                   SITDIF(3,I) = SITDIF(3,I) +(VSUBXYZ(3,J)/1000)*DT_YEAR
                  starc = .not.kcsta
                  do k=1,nacsta
                    if (equal( iselar(iacsta+(k-1)*4), INT2(1), jsitn(1,i), &
     &               INT2(1), INT2(8)))then
                      starc = .not.starc
                    endif
                  enddo
                  call hol2char( isoltyp, INT2(1), INT2(1), styp )
                  if (kbatch.and.styp.eq.'I') starc=.TRUE.
                 if (.not.kbatch.or.starc) then
                  nvsitec(1,I) =nvsitec(1,I) +(VSUBXYZ(1,J)/1000)*DT_YEAR
                  nvsitec(2,I) =nvsitec(2,I) +(VSUBXYZ(2,J)/1000)*DT_YEAR
                  nvsitec(3,I) =nvsitec(3,I) +(VSUBXYZ(3,J)/1000)*DT_YEAR
                 endif
!
                   ITEST = 1
                END IF  !match found
              END DO  !running over the list of sites
!
              IF ( ITEST .EQ. 0 ) THEN ! no match found
                   IF ( FBY_WARNING ) THEN
                        WRITE  ( BUFSTR, 1080 ) (JSITN(K,I),K=1,4)
 1080                   FORMAT ( 'Warning: (DVELP) ', 4A2, &
     &                           ' not in velocity flyby file' )
!
                        IF ( .NOT. KBATCH ) THEN
                             CALL ADDSTR_F ( BUFSTR )
                             CALL NL_MN()
                             CALL REFRESH_MN()
                             PAUSE_FLYBY = .TRUE.
                           ELSE
                             WRITE (  6, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
                             WRITE ( 23, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
                        ENDIF
                   END IF  ! No match found
              END IF  ! Itest
          END DO  ! Checking the table read from the data base.
!
      IF ( PAUSE_FLYBY ) THEN
           CALL FERR ( INT2(149), '(DVELP) Flyby warning', INT2(0), INT2(0) )
      END IF
      RETURN
      END  !#!  DVELP  #!#
