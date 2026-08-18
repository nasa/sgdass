        PROGRAM CHPART
        IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 MAX_PROGS, IERR, NUM_PART_TOT, IPROG, IPART
      INTEGER*2 IOBS, IOFF, NUM_PROG
      REAL*4    CPARFILE, FNAME
!-----END of imp added lines.
!
!
! Written by JMGipson.  August 2, 1996.
! This routine allows you to chain several userpartial programs together.
! The full paths of the userpartials files are in
!  $WORK_DIR/CPARxx
! where xx are the users initials.
!
! 1. The program first opens this file to read the user partials.
! 2.a It then schedules each in turn.
!   b. After each one is run, it renames the PARTxx file to PARTxx_nn
!     where nn is a two digit number.
! 3. Finally it reads all of these, and writes out the completed file.
!
        INCLUDE 'solve.i'
        INCLUDE 'prfil.i'
        INCLUDE 'precm.i'
        INCLUDE 'socom.i'
        integer*4 num_partials
        integer*2 trimlen
!
        character*22 lnames(m_gpa)
        real*8 part(2,m_gpa)
        logical*4 kexist
        integer*4 nobs
      LOGICAL*2 kdebug,kcheck_debug
        character*63 filnam,luserpnam,lpartnam
        character*200 lstring
        character*2 lind
      INTEGER*2 num_part
        integer*2 il1,il2
!
! maximum number of user partial programs we can handle.
        parameter (max_progs=100)
      INTEGER*2 num_progs
        character*63 luserpart(max_progs),ldum
! how many part per program?
      INTEGER*2 num_parts_per_prog(max_progs)
        integer*4 fil_out,jbytes,jblocks_out
        integer*4 fil_in(max_progs),jblocks_in(max_progs)
        character*63 lpartnam_in(max_progs)
        INTEGER*2 system2_sh
!
!   pet  990409  Replaced call of system with call of system2_sh in order to
!                fix SYSTEM-APR99 bug
!***********************************************************************
      KCHECK_DEBUG = 0
      kdebug=kcheck_debug
!*******************************************************
!
!     open precm.i common
      call PRE_PROG()          !
      INCLUDE 'chpart_version.i' ! Set revision date of the current version
!     open prfil.i
      call USE_PARFIL('ORC' )
!     open common blocks in socom.i
      call USE_COMMON('ORC' )
!
!
!
!     Open Chain file and read in userprograms.
!
      filnam = PRE_SCR_DIR(:PRE_SD_LEN)//'CPAR'//PRE_LETRS
      inquire(file=filnam,exist=Kexist)
      if(.not. kexist) then
         writE(*,*) "Aborting because file not found: ",CPARFILE
         stop
      endif
!
!
      open(66,file=filnam)
      num_progs=0
5     continue
      read(66,'(a)',end=10) ldum
      if(ldum(1:1) .eq. " ") goto 10
      if(ldum(1:1) .eq. "*" .or. ldum(1:1) .eq. "$") goto 5
      num_progs=num_progs+1
      luserpart(num_progs)=ldum
      goto 5
!
!
10    continue
      close(66)
!
!
! If only 1 user prog, don't do anything fancy. Run it and exit.
      if(num_progs .eq. 1) then
        call run_prog( luserpart(1), 'WAIT', INT2(1) )
        goto 500
      endif
!
!
! If more than 1, erase files of the form USRPxx_nn
      FILNAM = PRE_SCR_DIR(:PRE_SD_LEN)//'PART'//PRE_LETRS//'_*'
! remove old scratch files.  I think this speeds up some stuff.
      lstring="rm "//FILNAM
      il1=trimlen(lstring)
      il1=il1-2
      il2=2*il1+10
      ierr=system2_sh(lstring(1:il1))
!
!
!
      LUSERPNAM = PRE_SCR_DIR(:PRE_SD_LEN)//'USRP'//PRE_LETRS
      LPARTNAM  = PRE_SCR_DIR(:PRE_SD_LEN)//'PART'//PRE_LETRS
!
!
! now execute programs one at a time
      num_part_tot=0
      do iprog=1,num_progs
        call run_prog( luserpart(iprog), 'WAIT', INT2(1) )
! read in luserpnam file. (Partial names.)
        open(66,file=luserpnam)
        read(66,*) num_part
        do ipart=1,num_part
           read(66,'(a)') lnames(ipart+num_part_tot)
        end do
        close(66)
        num_part_tot=num_part_tot+num_part
        num_parts_per_prog(iprog)=num_part
        write(lind,'(i2.2)') iprog
        lpartnam_in(iprog) = &
     &      PRE_SCR_DIR(:PRE_SD_LEN)//'PART'//PRE_LETRS//'_'//lind
        lstring="mv "//lpartnam(1:il1)//" "//lpartnam_in(iprog)(1:il1)
        ierr=system2_sh(lstring(1:il2))
      end do
!
!
! done executing all the part.  Now combine the results and write them out.
! first out the USRPxx file
!
      open(66,file=luserpnam)
      write(66,'(i6)') num_part_tot
      do ipart=1,num_part_tot
         write(66,'(a)') lnames(ipart)
      end do
      close(66)
!
!
! now get ready for the PARTxx file
      jbytes = 8*2*num_part_tot
      jblocks_out = (jbytes+255)/256
      call bin_create8(lpartnam,fil_out, int8(0) )
!
!
! open up all the of the "PARTxx_nn" files
      do iprog=1,num_progs
        call bin_open(lpartnam_in(iprog),fil_in(iprog),'O' )
        jbytes = 8*2*num_parts_per_prog(iprog)
        jblocks_in(iprog) = (jbytes+255)/256
      end do
!
!
      do iobs=1,numobs
         ioff=1
         do iprog=1,num_progs
            call bin_read(lpartnam_in(iprog),fil_in(iprog),part(1,ioff), &
     &           jblocks_in(iprog) )
         ioff=ioff+num_parts_per_prog(iprog)
         end do
         call bin_write(lpartnam,fil_out,part,jblocks_out )
      end do
!
!
! now close all the "PARTxx_nn" Files
      call bin_close(fname,fil_out )
      do iprog=1,num_progs
         call bin_close(lpartnam_in(iprog),fil_in(iprog) )
      end do
!
!
!
!
500   continue
      call end_prog()
!
!
!      if(kdebug) close(13)
!
        STOP
        END
