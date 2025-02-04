      SUBROUTINE cplstat (iyr, imn, idy, kcpls)
!
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
      INCLUDE "gsnoop_com.i"
!
      integer*4     ios
      integer*2     mep(mcpl)
      integer*2     i, j, icpl
!      integer*2     trimlen, len
      integer*2     iyr, imn, idy
!
      LOGICAL*2 kcpls
!
      character*8   qsub
      character*80  qstr
!
      integer*2 icol_adj
      real*8 fjldy
!
      save mep
!
!     modified
!     980806  kdb Add cpls_nepochs (for outputting continuous piecewise
!                 linear sites to iers_solep_<>.
!     001026  kdb Range checking has now been enabled and has found an error
!                 in the logic.  If the solution has a cpl site, but the user
!                 doesn't request that gsnoop treat this site as cpl, the
!                 site's information will be put in an internal array in an
!                 illegal element (the first element past the last legal one).
!
!                 A good fix would treat the cpl site as cpl without the user
!                 having to request it.  But right now a quick fix is needed.
!                 If the user doesn't designate a cpl site as cpl, he wants to
!                 ignore it, so gsnoop stores the information during this
!                 subroutine call, then ignores the information for the rest of
!                 the run.  So all that is needed is to store the unwanted
!                 information in a legal element.  So for a quick fix, gsnoop
!                 will now store it in the final legal element.
!     020515  kdb The parameter index in solve spoolfile adjustment lines is
!                 being enlarged from a four character field to five
!                 characters.  Update to read the new format.
!     030109  kdb Change from separate changeover variables to an array.
!                 Add julian date changeover array to common.
!
!     Initialize
!
      qsub = 'cplstat'
!
      icol_adj = 0
      if (spool_solve_revision_date_jd .ge. &
     &  changeover_jd(1))icol_adj = 1
!
!     Find right index for entries.
!
      icpl = 1
      do while (( site_names(nsite)(1:8) .ne. &
     &                                  qcpl_stat(icpl)).and.(icpl .le. mcpl-1))
          icpl = icpl + 1
      end do
      qmcpl(icpl) = site_names(nsite)(9:12)  !get monument number
      mep(icpl) = mep(icpl) + 1
      cpls_nepochs(icpl) = 1  !gsnoop_com.i variable
!
!     Copy X component information from first X position into cpls arrays
!     then start reading new info.
!
      icpls_date(icpl,1,mep(icpl)) = iyr
      icpls_date(icpl,2,mep(icpl)) = imn
      icpls_date(icpl,3,mep(icpl)) = idy
      cpls_xyz(icpl,1,mep(icpl))   = xyz(1,nsite)
      cpls_xyz_sig(icpl,1,mep(icpl)) = xyz_sig(1,nsite)
!
!     If cbuf is Y info we have first type of entry in spoolfile.
!     If cbuf is X info we have second type of entry in spoolfile.
!
      if (cbuf(26+icol_adj:28+icol_adj) .eq. " Y ") then
!
          Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &                    iostat=ios)cpls_xyz(icpl,2,mep(icpl))
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &                iostat=ios)cpls_xyz_sig(icpl,2,mep(icpl))
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!         Get z information
          Read(40,'(A)',END=910) CBUF
          Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &                    iostat=ios)cpls_xyz(icpl,3,mep(icpl))
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &            iostat=ios)cpls_xyz_sig(icpl,3,mep(icpl))
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!         Get the uen adjustments
!
!         Get u information
          Read(40,'(A)',END=910) CBUF
          Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &                    iostat=ios)cpls_uen(icpl,1,mep(icpl))
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &                iostat=ios)cpls_uen_sig(icpl,1,mep(icpl))
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!         Get e information
          Read(40,'(A)',END=910) CBUF
          Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &                    iostat=ios)cpls_uen(icpl,2,mep(icpl))
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &                iostat=ios)cpls_uen_sig(icpl,2,mep(icpl))
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
!
!         Get n information
          Read(40,'(A)',END=910) CBUF
          Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)', &
     &                    iostat=ios)cpls_uen(icpl,3,mep(icpl))
          If(ios.ne.0) call quitter(qsub,cbuf,int2(ios))
          Read(CBUF(83+icol_adj:92+icol_adj),'(f10.3)', &
     &                iostat=ios)cpls_uen_sig(icpl,3,mep(icpl))
!
!
      else
!
!
!
!     Get x information
      j = 2   !  current cbuf corresponds to the second position
      do while (cbuf(26+icol_adj:28+icol_adj) .eq. " X " )
        Read(CBUF(29+icol_adj:34+icol_adj), &
     &      '(3i2)')(icpls_date(icpl,i,j),i=1,3)
        cpls_nepochs(icpl) = j  !gsnoop_com.i variable
        Read(CBUF(38+icol_adj:52+icol_adj),'(f15.2)') cpls_xyz(icpl,1,j)
        Read(CBUF(83+icol_adj:92+icol_adj), &
     &    '(f10.3)')cpls_xyz_sig(icpl,1,j)
        j = j + 1
        READ(40,'(A)',END=910) CBUF
      end do
!
!     Get y information starting with the first position
      j = 1
      do while (cbuf(26+icol_adj:28+icol_adj) .eq. " Y " )
        Read(CBUF(38+icol_adj:52+icol_adj), &
     &    '(f15.2)')cpls_xyz(icpl,2,j)
        Read(CBUF(83+icol_adj:92+icol_adj), &
     &    '(f10.3)')cpls_xyz_sig(icpl,2,j)
        j = j + 1
        READ(40,'(A)',END=910) CBUF
      end do
!
!     Get z information starting with the first position
      j = 1
      do while (cbuf(26+icol_adj:28+icol_adj) .eq. " Z " )
        Read(CBUF(38+icol_adj:52+icol_adj), &
     &    '(f15.2)')cpls_xyz(icpl,3,j)
        Read(CBUF(83+icol_adj:92+icol_adj), &
     &    '(f10.3)')cpls_xyz_sig(icpl,3,j)
        j = j + 1
        READ(40,'(A)',END=910) CBUF
      end do
!
!     write out results
!
!      do j = 1, mstat
!       write (qbuffer, "(120x)")
!       write (qbuffer(1:), "(3(i2,x),2x,3(f15.1,x,f7.1,2x))")
!     &        (icpls_date(icpl,i,j), i = 1,3),
!     &        (cpls_xyz(icpl,i,j), cpls_xyz_sig(icpl,i,j),i=1,3)
!       len = trimlen (qbuffer)
!       write (58, "(a)") qbuffer(1:len)
!      end do
!
!     Get the uen adjustments
!
!     Get u information
      j = 1
      do while (cbuf(26+icol_adj:28+icol_adj) .eq. " U " )
        Read(CBUF(38+icol_adj:52+icol_adj), &
     &    '(f15.2)')cpls_uen(icpl,1,j)
        Read(CBUF(83+icol_adj:92+icol_adj), &
     &    '(f10.3)')cpls_uen_sig(icpl,1,j)
        j = j + 1
        READ(40,'(A)',END=910) CBUF
      end do
!
!     Get e information
      j = 1
      do while (cbuf(26+icol_adj:28+icol_adj) .eq. " E " )
        Read(CBUF(38+icol_adj:52+icol_adj), &
     &    '(f15.2)')cpls_uen(icpl,2,j)
        Read(CBUF(83+icol_adj:92+icol_adj), &
     &    '(f10.3)')cpls_uen_sig(icpl,2,j)
        j = j + 1
        READ(40,'(A)',END=910) CBUF
      end do
!
!     Get n information
      j = 1
      do while (cbuf(26:28) .eq. " N " )
        Read(CBUF(38+icol_adj:52+icol_adj), &
     &    '(f15.2)')cpls_uen(icpl,3,j)
        Read(CBUF(83+icol_adj:92+icol_adj), &
     &    '(f10.3)')cpls_uen_sig(icpl,3,j)
        j = j + 1
        READ(40,'(A)',END=910) CBUF
      end do
!
!     write out results
!
!      do j = 1, mstat
!       write (qbuffer, "(120x)")
!       write (qbuffer(1:), "(3(i2,x),2x,3(f9.1,x,f7.1,2x))")
!     &        (icpls_date(icpl,i,j), i = 1,3),
!     &        (cpls_uen(icpl,i,j), cpls_uen_sig(icpl,i,j),i=1,3)
!       len = trimlen (qbuffer)
!       write (59, "(a)") qbuffer(1:len)
!      end do
!
!
      end if
      return
!
 910  qstr = "EOF found in cplstat"
      call asnl(qstr )
      call getstr_f(qstr )
      return
!
      end
