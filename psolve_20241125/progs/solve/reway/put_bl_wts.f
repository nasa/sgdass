      SUBROUTINE put_bl_wts(bl_wts,idb)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INCLUDE 'solve.i'
      INCLUDE 'prfil.i'
      INCLUDE 'socom.i'
!
      real*8 bl_wts(2,*)
!
! kdb 970318 Change pause to ferr2 call.
! kdb 971211 Better error handling for cases when an overly large weight or
!            a "NaN" weight is generated.  Instead of writing it to the namfil
!            REWT card and letting solve trudge uselessly on only to abort
!            later in proc, flag the error now (and do it with a clearer
!            error message than proc would give).
! pet 980203 Substituted hard-coded test of solution type by DATYP_INQ
!
! put re-weight constants in NAMFIL
!
! input:
!     idatyp      data type from socom
!     constant( ) error constants to put in NAMFIL for idatyp
!             (1) delay constant, picoseconds
!             (2) rate constant, femtoseconds/second
!
      double precision ee(4),ee_check(4)
      character*72 jbuf
      character*8  Lbasl(2)
      INTEGER*2 IOFF, IERR, IBASE, NUM_BASELINES
      INTEGER*4 IOS
      LOGICAL*2 kfirst
      character*79 errbuf
      integer*2 trimlen
      data kfirst/.true./
      integer*2 idb
      LOGICAL*4 DATYP_INQ
!
! determine group (i=1) or phase (i=3) constants to put
!
      call opennamfil()
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
           IOFF = 3         ! Phase delay weights
        ELSE
           IOFF = 1         ! Group delay weights
      ENDIF
!
!
10    continue
      call getcard( idb, 'INIT', INT2(1), jbuf, ierr )
      if(ierr .ne. 0) then
          WRITE(errbuf, &
     & "('MYWAY(put_bl_wts): Unexpected IERR from GETCARD (INIT) ',I5)") &
     &    IERR
          call addstr_f(errbuf )
          call nl_mn()
          call ferr( INT2(243), errbuf, INT2(0), INT2(0) )
      End If
!
      ibase=0
      ierr = 0
      call getcard( idb, 'REWT', INT2(1), jbuf, ierr )
      do while (ierr .eq. 0)
        read (jbuf,'(5x,a8,1x,a8,4f10.2,8x)',IOSTAT=ios) Lbasl, ee
          call ferr ( INT2(ios), "Reading NAMFIL REWT card", INT2(0), INT2(0) )
        ibase=ibase+1
        ee(ioff)  =bl_wts(1,ibase)
        ee(ioff+1)=bl_wts(2,ibase)
        IF ( EE(IOFF)   .GT. 100000.0 ) EE(IOFF)   = 100000.0
        IF ( EE(IOFF+1) .GT. 100000.0 ) EE(IOFF+1) = 100000.0
        write (jbuf,'("REWT ",a8,"-",a8,4f10.2)') Lbasl, ee
        read(jbuf,"(22x,4f10.2)",iostat=ios) ee_check
        ierr  = ios
        if ( ierr.ne.0) then
             write ( 6, * ) ' ee=',ee
             call ferr2 ( ierr, "put_bl_wts: generated bad NAMFIL vals: "// &
     &            jbuf, INT2(0), INT2(0), INT2(0) )
        endif
! use re-write feature (4) of putcard
        call putcard( idb, 'REWT', INT2(4), jbuf, ierr )
        if(ierr.ne.0)call ferr2( ierr, 'put_bl_wts error writing NAMFIL', &
     &     INT2(0), INT2(0), INT2(2) )
        call getcard( idb, 'REWT', INT2(0), jbuf, ierr )
      enddo
!
!
      num_baselines= (numsta*(numsta-1))/2
      IF(ibase .NE. num_baselines) then
         write(errbuf, &
     &     '("put_bl_wts: Read ",i5," expected ",i5)')ibase,num_baselines
         call ferr2( INT2(9105), errbuf(1:trimlen(errbuf)), INT2(0), INT2(0), &
     &        INT2(2) )
      endif
!
!
      if(ierr.ne.1) then
         write(errbuf,'("error ",i5, &
     &     " reading NAMFIL in put_bl_wts ")') ierr
         call ferr2( INT2(9106), errbuf(1:trimlen(errbuf)), INT2(0), INT2(0), &
     &        INT2(2) )
      endif
      call closenamfil()
      return
      end
