      SUBROUTINE get_bl_wts(bl_wts,idb,idbeg,idend,ksflag)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'solve.i'
      INCLUDE 'prfil.i'
      INCLUDE 'socom.i'
!
! modifications
!
! kdb 970318 Change pause to ferr2 call.
! pet 980203 Substituted hard-coded test of solution type by DATYP_INQ
! JMG Modified to also get:
!  1.)  idb     -- which database are we using.
!  3.)  idbeg  -- Where to start to reading in residuals.
!  4.)  idend -- Where to end reading in residuals.
!  2.)  ksflag  -- ARe we using S band?
!
! pet 2000.07.31  -- changed type ITEMP from INTEGER*2 to INTEGER*4, otherwise
!                    REWAY died in attempt to process the session with
!                    more than 32768 observations.
!
! get re-weight constants from namfil
!
! input:
!     idatyp   data type from socom
!
! output:
!    bl_wts( )     error constants for first baselines in NAMFIL for idatyp
!          (1)     delay constant, picoseconds
!          (2)     rate constant, femtoseconds/second
!                  actually, get   CONSTANTS^2 * sign(constants)
!
!     lbasel       station names at either end of baseline
!
!
      character*72 jbuf
      character*8  Lbasl(2)
      INTEGER*2    IDB, IERR
      INTEGER*4    IOS
      INTEGER*2    i, ibl, num_baselines
      double precision bl_wts(2,*)
      DOUBLE precision ee(4)
      integer*2 trimlen
      character*79 errbuf
      logical*2 kbit
      integer*4 idbeg,idend
      INTEGER*2 ixors, idbver
      character*10 ldbnam
      integer*4 itemp
      logical*2 ksflag
      integer*2 ioff
      character*1 lflag
      LOGICAL*4   DATYP_INQ
!
! pick group (i=1) or phase (i=3)
!
      call opennamfil()
!
      idb=0
      do i=1,ndb
        IF(KBIT(IDBSEL,I)) then
! have a databse in the solution. Did we generate residuals?
          IF(.not.KBIT(IDCSEL,i)) then
            call ferr( INT2(243), "MYWAY: Did not generate residuals!", &
     &           INT2(0), INT2(0) )
            call end_prog()
          endif
          if(idb .ne. 0) then
            call ferr( INT2(243), &
     &          "MYWAY: Only 1 databse should be on in solution", INT2(0), &
     &           INT2(0) )
            call end_prog()
          endif
          idb=i
        endif
      end do
      if(idb .eq. 0) then              !No database found!
        call ferr2( INT2(-1), 'MYWAY: No databases in solution!', INT2(0), &
     &       INT2(0), INT2(2) )
        call end_prog()
      endif
!
!
! beginning and ending points of residuals.
! Above we found that we that we had only 1 database in the solution.
! Here we find out where the residuals of this database are.
!
      if(idb .eq. 1) then
        idbeg=1                        !Simplest case. First database.
      else
        idbeg=idbend(idb-1)+1
      endif
      idend=idbend(idb)
!
!
      call getcard( idb, 'INIT', INT2(1), jbuf, ierr )
      if(ierr .ne. 0) then
          WRITE(errbuf, &
     &      "('MYWAY: Unexpected IERR from GETCARD (INIT) ',I5)") IERR
          call addstr_f(errbuf )
          call nl_mn()
          call ferr( INT2(243), errbuf, INT2(0), INT2(0) )
      End If
!
!
      READ ( JBUF, 910, IOSTAT=IOS ) LDBNAM, IDBVER, ITEMP, LFLAG
      CALL FERR ( INT2(IOS), 'Reading NAMFIL INIT card: "'//JBUF//'"', INT2(0), &
     &     INT2(0) )
910   FORMAT(10X,A10,I4,11X,I11,1X,A1)
      KSFLAG = LFLAG .EQ. "S"
!
! --- Now read in the re-weights
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
           IOFF = 3                  ! Phase delay weights
        ELSE
           IOFF = 1                  ! Group delay weights
      ENDIF
      ibl=0
      call getcard( idb, 'REWT', INT2(1), jbuf, ierr )
      do while (ierr .eq. 0)
!
! ----- Parse the card.
!
        read (jbuf,'(5x,a8,1x,a8,4f10.2,8x)',IOSTAT=ios) Lbasl, ee
        call ferr ( INT2(ios), "decoding NAMFIL REWT card", INT2(0), INT2(0) )
        ibl=ibl+1
        bl_wts(1,ibl) = ee(ioff)
        bl_wts(2,ibl) = ee(ioff+1)
!
! ----- ... and read the next one.
!
        call getcard( idb, 'REWT', INT2(0), jbuf, ierr )
      enddo
!
      if(ierr.ne.1) then
        write(errbuf,"('error ',i5, &
     &    ' reading NAMFIL REWT card in get_bl_wts')") ierr
        call ferr2( INT2(9103), errbuf(1:trimlen(errbuf)), INT2(0), INT2(0), &
     &       INT2(2) )
      endif
      num_baselines= (numsta*(numsta-1))/2
      IF(ibl .NE. num_baselines) then
        write ( 6, * ) ' ierr=',ierr 
        write(errbuf, &
     &    "('get_wts: Read ',i5, ' expected ', i5)")ibl,num_baselines
        call ferr2( INT2(9104), errbuf(1:trimlen(errbuf)), INT2(0), INT2(0), &
     &       INT2(2) )
      endif
!
!
      call closenamfil()
      return
      end
