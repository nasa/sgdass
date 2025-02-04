      SUBROUTINE solmat(cvrf_path,ilen,ap_choice,covs_wanted, &
     &                  covs_found,ncovparms)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     written by mwh 3/95 as a standalone program
!
!     purpose: produces the solution/matrix_estimate L cova
!     section of the 1995 style iers site submission
!     (as either apriori or non-apriori)
!
!     input arguments:
!
!     cvrf_path,ilen - path to input solve covariance file
!     ap_choice - determines whether the apriori or non-apriori version of
!                 the file should be generated
!                 (note that this option just affects the wording of the
!                  begining and ending
!                  delimiter lines and the name of the file to which
!                  the information is
!                  written.  The type of data produced is determined by a
!                  SOLVE option which writes one of the two types of
!                  information to the covariance file which is read by
!                  this subroutine.)
!     covs_wanted - bit array of types of parameters for which covariances
!                   are desired
!
      character*63 cvrf_path
      integer*2 ilen
      logical*2 ap_choice
      integer*2 covs_wanted
!
!     output arguments:
!     covs_found - bit array of types of parameters for which covariances
!              were actually generated
!     ncovparms - number of parameters for which covariances were printed
      integer*2 covs_found
      integer*2 ncovparms
!
!     modifications
!
!     kdb 4/10/95 incorporated into gsnoop.  Print out header.
!                 Print out apriori version
!     kdb 4/19/95 add implicit none.
!     kdb 7/1/96  Originally handled CVRFxx files from a solution with
!                 the $OUTPUT section COVARIANCES keyword set to CGM STA.
!                 Now handles more general CVRFxx file formats. (One problem
!                 was a varying number of header lines.  Also, there may be
!                 a blank line between the parameter list and the parameter
!                 matrix.  Also originally only handled station parameters.
!                 Now handles eop parameters (but offsets only.
!                 Essentially the current change only guarantees to
!                 support site and eop parameters from a solution with
!                 COVARIANCES set to BY_ARC ALL).)
!     kdb 7/18/96 Add new files to produce the sinex header line and
!                 the input/history section. (This subroutine
!                 must pass up the number of parameters in the sinex solution
!                 (which equals the number for which covariances were
!                 generated).
!     kdb 7/24/96 Decrease output field from e20.14 to e19.13 for consistency
!                 with sinex .05 version.
!     kdb 7/26/96 Suppress output lines that only contain values of zero.
!     kdb 7/30/98 Upgrade for sinex 1.00.
!
!
!
      character*128 buf,buf1
      real*8 vals(3000),sig(3000)
      integer*2 i,j,k,first,last,npar,nvals
      integer*2 id1,id2,nlines,n
      integer*2 outlu
      integer*2 ict,nwants,parmtype,iwant
      real*8 vwants(3000)
      integer*2 parms_wanted(1+(3000-1)/16)
      logical*2 kbit,loop
      integer*2 trimlen
      integer*2 iparm
      logical*2 all_zeros,this_zero
!
      covs_found = 0
!
      if (ap_choice) then
        outlu = 91
        write(outlu,'("+SOLUTION/MATRIX_APRIORI L COVA")')
      else
        outlu = 89
        write(outlu,'("+SOLUTION/MATRIX_ESTIMATE L COVA")')
      end if
      write(outlu,'("*PARA1 PARA2  ____PARA2+0_________  ", &
     &                        "____PARA2+1_________  ", &
     &                        "____PARA2+2_________ ")')
!
      rewind(87)
!out  open(87,file=cvrf_path(1:ilen))
      read(87,*)id1,id2,npar
!     Find blank line before first parameter line
      loop = .true.
      do while (loop)
        read (87,'(A)') buf
        if (trimlen(buf).eq.0) loop = .false.
      end do
!
!     In a little bit, we'll output the parameter matrix from the CVRFxx file,
!     but first we must read the CVRFxx file's parameter list.  This will
!     help us to determine which parameters we actually want.  (For example,
!     all parameters may be represented, but we may only want site and eop
!     ones.)  Also we need to save the sigmas, which are located in the
!     input parameter list, but not the input matrix.
!
      do i=1,npar
        read(87,'(A30,E21.16,A)')buf,sig(i),buf1
        call parminfo(buf,covs_wanted,parmtype,iwant )
        call sbit(parms_wanted,i,iwant )
        if (iwant.eq.1) call sbit( covs_found, parmtype, INT2(1) )
      enddo
!     There may be a blank line between the parameter list and parameter
!     matrix.  Position to the next line, hoping it's the blank.
!     If it's actually the first line of the matrix, then just back up.
      read(87,'(A)') buf
      if (trimlen(buf).ne.0) backspace(87)
!
!     Now read in the parameter matrix from the CVRFxx file, writing it out
!     to the sinex output file.  The values are 5 to a line in the input file,
!     but 3 to a line in the output file.
!     Don't forget to weed out unwanted parameter types.
!
      iparm = 0
      do i=1,npar
!       Read the values for the current parameter into an array.
!       Even if we don't want it, we need to get past it to the next desired
!       parameter.
        nvals = i-1
        nlines = (nvals+4)/5
        do j=1,nlines
          first = (j-1)*5+1
          last = min(j*int2(5),nvals)
          if (j.eq.1) then
            read(87,*)n,(vals(k),k=first,last)
          else
            read(87,*)(vals(k),k=first,last)
          endif
        enddo
!       Append the sigma for the parameter itself.
        vals(i) = sig(i)
!       Skip printing this parameter if it's not wanted.
        if (kbit(parms_wanted,i)) then
          iparm = iparm + 1
!         If the parameter is wanted,
!         squeeze the array to remove unwanted parameters.
          nwants = 0
          do ict = 1, i
            if (kbit(parms_wanted,ict)) then
              nwants = nwants + 1
              vwants(nwants) = vals(ict)
            endif
          enddo
!         Now print the squeezed array, 3 values per line.
!         But don't bother printing lines that only contain values of zero.
          nlines = (nwants+2)/3
          do j=1,nlines
            first = (j-1)*3+1
            last = min(j*int2(3),nwants)
            all_zeros = .true.
            do k = first,last
              if (dabs(vwants(k)).lt.0.00000000000005D0) then
                this_zero = .true.
              else
                this_zero = .false.
              endif
              if (.not.this_zero) all_zeros = .false.
            enddo
            if (.not. &
     &        all_zeros)write(outlu,'(2I6,3(2X,E20.14))')iparm,first, &
     &        (vwants(k),k=first,last)
          enddo
        endif
      enddo
100   continue
      close(87)
      if (ap_choice) then
        write(outlu,'("-SOLUTION/MATRIX_APRIORI L COVA")')
      else
        write(outlu,'("-SOLUTION/MATRIX_ESTIMATE L COVA")')
      end if
      ncovparms = iparm
      end
