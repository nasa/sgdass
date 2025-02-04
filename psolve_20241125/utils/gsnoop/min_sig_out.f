!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
      Subroutine min_sig_out
!
!     Creates tables of xyz posiitons and uen adjustments of stations at
!     the epoch at which their
!     sigmas are minimal.
!
!     Cloned 94 09 07 DS Caprette HSTX from global_info
!
!     Added new file to output rss of minimum x,y,z position sigmas.
!     K. Baver 6/27/95
!
      IMPLICIT NONE
      INCLUDE 'gsnoop_com.i'
!
      integer*2     i, j
!
!
      Character*79  qstr
      integer*2 ilpt,ict
      real*8 rss_sig
!
!
      write (qstr , "('Doing minimum sigma tables')")
      call asnl(qstr)
!
!     Remember that the minimum sigma tables are directly indexed
!
 1001 format ("Station   mon  YY MM DD")
 1002 format (t15,"X",t23,"Xsig",t41"Y",t49"Ysig",t67,"Z",t75,"Zsig")
 1003 format (t15,"U",t23,"Usig",t41"E",t49"Esig",t67,"N",t75,"Nsig")
!
 1004 format (/,a8,x,a4,x,3(x,a2))
 1005 format (3(f15.2,x,f8.3,2x),a8,x,a4,3(x,a2))
 1006 format ("  Long       Lat        Rss    Xsig   Ysig   Zsig  ")
 1007 format (2(f9.3,1x),2x,4(f6.1,1x),1x,3(a2,1x),1x,a8)
!
!
        write (79, 1001)
        write (79, 1002)
        write (79, 1003)
        write (95, 1006)
!
      do i = 1, nmin
        write (79, 1004) min_site(i)(1:8),min_site(i)(9:12), &
     &    min_site(i)(13:14), min_site(i)(15:16), min_site(i)(17:18)
!
        read (min_site(i)(19:23), "(i5)") j   !get index
!
        write (79, 1005) xyz_min(1,j),xyz_min_sig(1,j), &
     &    xyz_min(2,j),xyz_min_sig(2,j), xyz_min(3,j),xyz_min_sig(3,j), &
     &    min_site(i)(1:8),min_site(i)(9:12), &
     &    min_site(i)(13:14), min_site(i)(15:16), min_site(i)(17:18)
!
        write (79, 1005) uen_min(1,j),uen_min_sig(1,j), &
     &    uen_min(2,j),uen_min_sig(2,j), uen_min(3,j),uen_min_sig(3,j), &
     &    min_site(i)(1:8),min_site(i)(9:12), &
     &    min_site(i)(13:14), min_site(i)(15:16), min_site(i)(17:18)
!
        rss_sig = xyz_min_sig(1,j) * xyz_min_sig(1,j) + &
     &            xyz_min_sig(2,j) * xyz_min_sig(2,j) + &
     &            xyz_min_sig(3,j) * xyz_min_sig(3,j)
        rss_sig = dsqrt(rss_sig)
!
        ilpt = 0
        do ict = 1,nsite
          if (min_site(i)(1:8) .eq. site_names(ict)(1:8)) then
            read(site_names(ict)(19:23),'(i5)') ilpt
          endif
        end do
!
!       For each site, print:
!
!        longitude
!        latitude
!        rss of the xyz position sigmas for the epoch where the sigmas are at
!           their minimum
!        the sigmas themselves
!        the epoch at which they reach a minimum
!        the site name
!
!
        write (95, 1007) &
     &    site_lon(ilpt),site_phi(ilpt),rss_sig, &
     &    xyz_min_sig(1,j),xyz_min_sig(2,j),xyz_min_sig(3,j), &
     &    min_site(i)(13:14), min_site(i)(15:16), min_site(i)(17:18), &
     &    min_site(i)(1:8)
!
      end do
!
!
!
!
!
      return
      end
