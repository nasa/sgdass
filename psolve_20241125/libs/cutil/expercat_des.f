      SUBROUTINE EXPERCAT_DES(CKYNM,EXPNAM,KERR)
      implicit NONE
      INCLUDE 'general.i'
!
!     purpose: checks the experiment catalog for the input database and
!              returns its descriptor or an error return
!
!     input variables
!
!     ckynm - input database
!
      character*10 ckynm
!
!     output variables
!
!     expnam - output description
!     kerr - error return
!        -2 - error opening experiment catalog
!        -1 - database not in experiment catalog
!         0 - success
!         1 - database is in the experiment catalog, but it has a blank
!             description
!
      character*32 expnam
      integer*2 kerr
!
!     created by  kdb 960318
!
!     Local variables
!
      character*9 simsf,cursf
      INTEGER*4  IERR
      integer*2 trimlen
      character*80 inbuf
      logical*2 exp_found
!
      expnam = ' '
      kerr = -999
      open(13,file=EXPER_CAT,iostat=ierr,err=25,status='old', &
     &        access='sequential',form='formatted')
 25   if (ierr.ne.0) then
        kerr = -2
      else
        exp_found = .false.
        do while (.TRUE.)
          read(13,'(A)',END=50) inbuf
          simsf = inbuf(48:56)
          cursf = inbuf(2:10)
          if (cursf.eq.ckynm(2:10).or.simsf.eq.ckynm(2:10)) then
            expnam = inbuf(14:44)
            exp_found = .true.
            goto 50
          endif
        enddo
50      continue
        close(13)
        if (exp_found) then
          if (trimlen(expnam).eq.0) then
!           The experiment is in the catalog, but the description
!           is blank
            kerr = 1
          else
!           Valid description found
            kerr = 0
          endif
        else
!         Experiment not in experiment catalog
          kerr = -1
        endif
      endif
!
      return
      END
