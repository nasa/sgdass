      SUBROUTINE snx_head_hst(sol_start,sol_end,nsinparms,cons_override, &
     &   covs_wanted,output_file_time, &
     &   center_abr_use,center_full_name_use)
!
!     WRITES OUT THE HEADER LINE OF A SINEX 0.05 SOLUTION.
!     ALSO WRITES THE INPUT/HISTORY SECTION
!
!     written 7/18/96 by kdb
!
!     modifications
!
!     980730 KDB upgrade to handle Sinex 1.00
!     001101 KDB Use solve.i parameters for center identification in
!                sinex_header_<> and sinex_inhist_<> files.
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Input variables:
!
!     sol_start, sol_end - epoch of first and last arcs in the solution
!     nsinparms - number of parameters in the sinex file
!     cons_override - overall constraint level for solution
!                     (-1 for individual levels)
!     covs_wanted - bit array of parameters for which covariances were
!                   generated.  Used to determine parameter types in the
!                   sinex file.
!     output_file_time - creation time of output files
!     center_abr_use  - center abbreviation
!     center_full_name_use  - full center identifier
!
!
      character*12 sol_start,sol_end,output_file_time
      integer*2 nsinparms,cons_override,covs_wanted
      character*(*) center_abr_use,center_full_name_use
!
!     Output variables: none
!
!
!     local variables
!
      integer*2 ipt
      character*1 conlev
      character*79 qstr
      character*8 parmbuf
      logical*2 kbit
      character*4 sinex_version
      character*3 file_agency,data_agency
      character*1 technique
      character*77 cbuffer
      integer*2 trimlen
      save conlev
      data conlev /'-'/
!
!     I. set up the information
!
!     Solicit an overall constraint level if individual levels were used.
!
      if (conlev.eq.'-') then
        if (cons_override.ne.-1) then
          write(conlev,"(i1)") cons_override
        else
          write(qstr,"('It appears this solution had ', &
     &     'individual constraint levels.')")
          call asnl(qstr )
          write(qstr,"('Give me an overall level ', &
     &      'for the sinex header line.')")
          call asnl(qstr )
          write(qstr,"('Choices are: ')")
          call asnl(qstr )
          write(qstr,"('0 = fixed/tight ')")
          call asnl(qstr )
          write(qstr,"('1 = significant ')")
          call asnl(qstr )
          write(qstr,"('2 = unconstrained ')")
          call asnl(qstr )
          call getstr_f(conlev )
        endif
      endif
!
!     Set up the buffer indicating the parameter types estimated.
!
      parmbuf = ' '
      ipt = 0
      if (kbit( covs_wanted, INT2(1) )) then  !site position parms
        ipt = ipt + 2
        parmbuf(ipt:ipt) = 'X'
      endif
      if (kbit( covs_wanted, INT2(2) )) then  !site velocity parms
        ipt = ipt + 2
        parmbuf(ipt:ipt) = 'V'
      endif
      if (kbit( covs_wanted, INT2(3) ).or.kbit( covs_wanted, INT2(4) )) then !eop parms
        ipt = ipt + 2
        parmbuf(ipt:ipt) = 'E'
      endif
!
!     II. write the files
!
!     The overall sinex header line is nearly identical to the line that
!     forms the body of the input/history section.  Set up a common buffer
!     now.
!
      sinex_version = '1.00'
      file_agency = center_abr_use !originator of sinex file
      data_agency = center_abr_use !originator of data
      technique = 'R'
      write(cbuffer,"('%=SNX',1X,A4,1X,A3,1X,A12,1X,A3, &
     &     1X,A12,1X,A12,1X,A1,1X,I5.5,1X,A1,A8)") &
     &     sinex_version,file_agency, &
     &     output_file_time,data_agency,sol_start,sol_end, &
     &     technique,nsinparms,conlev,parmbuf
!     Write the buffer to the sinex header file.
      write(101,"(A)") cbuffer
!     Write the input history section.
      write(102,"('+INPUT/HISTORY')")
      write(102, &
     &  "('*Note: ',A,' = ',A)")center_abr_use, &
     &  center_full_name_use(1:trimlen(center_full_name_use))
      write(102,"('*_Version_ Cre __Creation__ Own _Data_start_ ', &
     &            '_Data_end___ T Param S Type_')")
      write(102,"(A)") cbuffer
      write(102,"('-INPUT/HISTORY')")
!
      return
      end
