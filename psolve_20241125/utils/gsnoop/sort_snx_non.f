      SUBROUTINE sort_snx_non(upath,ftag,sort_choice)
!
!     Sorts the iers_siteid_<> and iers_solep_<> files in the manner
!     specified by sort_choice.
!
!     written 4/26/96 by kdb
!
!     modifications
!
!     960802 kdb Fix error introduced when the length of the iers_siteid_<>
!                lines changed.  The field for sorting by longitude has shifted
!                by one column.
!
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Input variables:
!
!     upath, ftag - define the path to the iers_siteid_<> and
!                   iers_solep_<> files for this gsnoop run.
!     sort_choice - sorting criterion.  Initially one option:
!                     L - sort by longitude
!
      character*78 upath
      character*6 ftag
      character*1 sort_choice
!
!     Output variables: none
!
!     local variables
!
      integer*2 trimlen,ilpath,iltag,iolen1,iolu1,iolen2,iolu2, &
     &          tmplen,tmplu,SYSTEM,iclose,ict,sort_len, &
     &          istart1,istop1,istart2,istop2,errlen, &
     &          iotlu1,iotlu2,iotlen1,iotlen2,ttmplen,ttmplu
      integer*4 ierr, icounti, icounto
      logical*2 loop,kbit
      character*9 err_act
      character*11 sort_key
      character*63 inout_file1,inout_file2,tmp_file,err_file
      character*68 inoutt_file1,inoutt_file2,ttmp_file
      character*60 err_cond
      character*80 header1(4),header2(4),cbuffer1,cbuffer2,qstr
      character*180 cbufferb
      character*255 unix_com
!
!
!
!     Programming structure:
!
!       The site entries in the iers_siteid_ and iers_solep_ files are
!       currently in the same order,
!       but only one (iers_site_id_<>) has the data used for sorting.
!       So read both files
!       in, write them into a temporary file which concatenates analogous
!       lines and prepends the sort_key,  then sort that file, and write
!       the sorted lines back out to the iers_siteid_ and iers_solep_ files.
!
!     I. Concatenate the input files into the temporary file with the sort key
!        prepended.
!
      ilpath = trimlen(upath)
      iltag = trimlen(ftag)
!
      inout_file1 = upath(1:ilpath)//'iers_siteid_'//ftag(1:iltag)
      iolen1 = trimlen(inout_file1)
      iolu1 = 86
!
      inout_file2 = upath(1:ilpath)//'iers_solep_'//ftag(1:iltag)
      iolen2 = trimlen(inout_file2)
      iolu2 = 85
!
      inoutt_file1 = upath(1:ilpath)//'iers_siteid_'// &
     &      ftag(1:iltag)//'_tmp'
      iotlen1 = trimlen(inoutt_file1)
      iotlu1 = 86
!
      inoutt_file2 = upath(1:ilpath)//'iers_solep_'// &
     &     ftag(1:iltag)//'_tmp'
      iotlen2 = trimlen(inoutt_file2)
      iotlu2 = 85
!
      tmp_file = &
     &  upath(1:ilpath)//'iers_sort_snx_non_'//ftag(1:iltag)
      tmplen = trimlen(tmp_file)
      tmplu = 87
!
      ttmp_file = &
     &  upath(1:ilpath)//'iers_sort_snx_non_'//ftag(1:iltag)//'_tmp'
      ttmplen = trimlen(ttmp_file)
      ttmplu = 87
!
!     First rename the input iers_siteid_<> and iers_solep_<> files to
!     temporary names.
!
      unix_com = 'mv '//inout_file1(1:iolen1)//' '// &
     &                  inoutt_file1(1:iotlen1)//char(0)
      IERR = SYSTEM(unix_com)
      IF (IERR.NE.0) GO TO 210
!
      unix_com = 'mv '//inout_file2(1:iolen2)//' '// &
     &                  inoutt_file2(1:iotlen2)//char(0)
      IERR = SYSTEM(unix_com)
      IF (IERR.NE.0) GO TO 210
!
!     open all three files
!
      iclose = 0
      err_act = ' opening '
      err_file = inoutt_file1(1:iotlen1)
      err_cond = 'for input pass'
      open(iotlu1,file=inoutt_file1(1:iotlen1),iostat=ierr, &
     &     err=110,status='old', &
     &     access='sequential',form='formatted')
      call sbit( iclose, INT2(1), INT2(1) )
!
      err_act = 'opening'
      err_file = inoutt_file2(1:iotlen2)
      err_cond = 'for input pass'
      open(iotlu2,file=inoutt_file2(1:iotlen2),iostat=ierr, &
     &     err=110,status='old', &
     &     access='sequential',form='formatted')
      call sbit( iclose, INT2(2), INT2(1) )
!
      err_act = 'opening'
      err_file = tmp_file(1:tmplen)
      err_cond = 'for input pass'
      open(tmplu,file=tmp_file(1:tmplen),iostat=ierr, &
     &     err=110,status='unknown', &
     &     access='sequential',form='formatted')
      call sbit( iclose, INT2(3), INT2(1) )
!
!     Read in the files.
!
!     The top lines of the input files are headers records which should be
!     kept out of the sorting process.  Just save them for later in arrays.
!
!     Skip headers
!
      do ict = 1,4
        READ(iotlu1,'(A)',IOSTAT=IERR,ERR=130,END=130) HEADER1(ICT)
        READ(iotlu2,'(A)',IOSTAT=IERR,ERR=130,END=130) HEADER2(ICT)
      enddo
      go to 140
 130  continue
      err_cond = 'Error reading headers into an array'
      go to 120
!
!     Read the input files to concatenate lines and prepend the sort key.
!
 140  ICOUNTI = 0
      LOOP = .TRUE.
      DO WHILE (LOOP)
!       Read the next input line
        READ(iotlu1,'(A)',IOSTAT=IERR,ERR=150,END=160) CBUFFER1
        READ(iotlu2,'(A)',IOSTAT=IERR,ERR=150,END=165) CBUFFER2
        IF (CBUFFER1(1:1).EQ.'-'.OR. &
     &      CBUFFER2(1:1).EQ.'-') THEN
          IF (CBUFFER1(1:8).EQ.'-SITE/ID'.AND. &
     &        CBUFFER2(1:16).EQ.'-SOLUTION/EPOCHS') THEN
            GO TO 180
          ELSE
            GO TO 170
          ENDIF
        ENDIF
        ICOUNTI = ICOUNTI + 1
!       Extract the sort data and fill in the blanks to form a sort key.
        IF (SORT_CHOICE.EQ.'L') THEN !sort by longitude
          READ(CBUFFER1(45:55),"(A11)") sort_key
!         Fill in missing leading zeroes in the degrees field for an accurate
!         numeric sort.
          DO ICT = 1,3
            IF (sort_key(ICT:ICT) .EQ. &
     &        ' ')sort_key(ICT:ICT) = '0'
          END DO
!         Fill in the blanks between the data fields to squeeze the
!         degree, minute and seconds fields into one field.
          sort_key(4:4) = '_'
          sort_key(7:7) = '_'
          SORT_LEN = 11
        END IF
!       Write out a concatenated line, with the sort key prepended.
        err_act = ' writing '
        err_file = tmp_file(1:tmplen)
        err_cond = 'for input pass'
        WRITE(tmplu,"(A,A,A)",IOSTAT=IERR, &
     &    ERR=110)SORT_KEY(1:SORT_LEN),CBUFFER1,CBUFFER2
      END DO
 150  err_cond = 'error reading an input file for input pass'
      go to 120
 160  err_cond = 'premature end of input file 1 (need -site/id record)'
      go to 120
 165  err_cond = 'premature end of input file 2'
      go to 120
 170  err_cond = 'trailer record found for only one of the input files'
      go to 120
 180  CONTINUE
      CLOSE(IOTLU1)
      CLOSE(IOTLU2)
      CLOSE(tmplu)
      iclose = 0
!
!     II. Now sort the temporary concatenated file.
!
      unix_com = 'sort '//tmp_file(1:tmplen)//' > '// &
     &                    ttmp_file(1:ttmplen)//char(0)
      IERR = SYSTEM(unix_com)
      IF (IERR.NE.0) GO TO 210
!
!     III. Now read the sorted temporary concatenated file back in, splitting
!     the lines and writing them out to the final files.
!
!     open the files
!
      err_act = ' opening '
      err_file = inout_file1(1:iolen1)
      err_cond = 'for output pass'
      open(iolu1,file=inout_file1(1:iolen1),iostat=ierr, &
     &     err=110,status='new', &
     &     access='sequential',form='formatted')
      call sbit( iclose, INT2(1), INT2(1) )
!
      err_act = ' opening '
      err_file = inout_file2(1:iolen2)
      err_cond = 'for output pass'
      open(iolu2,file=inout_file2(1:iolen2),iostat=ierr, &
     &     err=110,status='new', &
     &     access='sequential',form='formatted')
      call sbit( iclose, INT2(2), INT2(1) )
!
      err_act = ' opening '
      err_file = ttmp_file(1:ttmplen)
      err_cond = 'for output pass'
      open(ttmplu,file=ttmp_file(1:ttmplen),iostat=ierr, &
     &     err=110,status='old', &
     &     access='sequential',form='formatted')
      call sbit( iclose, INT2(3), INT2(1) )
!
!     Write out headers which were saved earlier.
!
      err_act = ' writing '
      err_cond = 'in output head'
      do ict = 1,4
        err_file = inout_file1(1:iolen1)
        WRITE(iolu1,'(A)',IOSTAT=IERR,ERR=110) HEADER1(ICT)
        err_file = inout_file2(1:iolen2)
        WRITE(iolu2,'(A)',IOSTAT=IERR,ERR=110) HEADER2(ICT)
      enddo
!
!     Loop over the temporary file, writing each line to the two output files.
!
      ICOUNTO = 0
      LOOP = .TRUE.
      DO WHILE (LOOP)
!       Read the next input line
        err_act = ' reading '
        err_file = ttmp_file(1:ttmplen)
        err_cond = 'in output body'
        READ(ttmplu,'(A)',IOSTAT=IERR,ERR=110,END=350) CBUFFERB
        ICOUNTO = ICOUNTO + 1
!       split the line and write each part to the appropriate file
        ISTART1 = SORT_LEN + 1
        ISTOP1 =  SORT_LEN + 76
        ISTART2 = SORT_LEN + 80 + 1
        ISTOP2 =  SORT_LEN + 80 + 54
        err_act = ' writing '
        err_file = inout_file1(1:iolen1)
        err_cond = 'in output body'
        WRITE(iolu1,"(A)",IOSTAT=IERR,ERR=110) CBUFFERB(ISTART1:ISTOP1)
        err_act = ' writing '
        err_file = inout_file2(1:iolen2)
        err_cond = 'in output body'
        WRITE(iolu2,"(A)",IOSTAT=IERR,ERR=110) CBUFFERB(ISTART2:ISTOP2)
      END DO
 350  CONTINUE
!     Write out the tails of the files.
      err_act = ' writing '
      err_file = inout_file1(1:iolen1)
      err_cond = 'in output tail'
      WRITE(iolu1,"('-SITE/ID')",IOSTAT=IERR,ERR=110)
      err_act = ' writing '
      err_file = inout_file2(1:iolen2)
      err_cond = 'in output tail'
      WRITE(iolu2,"('-SOLUTION/EPOCHS')",IOSTAT=IERR,ERR=110)
      CLOSE(IOLU1)
      CLOSE(IOLU2)
      CLOSE(ttmplu)
!
      IF (ICOUNTI.NE.ICOUNTO) THEN
        write(qstr,"('Difference in number of lines between ', &
     &     'original and sorted files.')")
        call asnl(qstr )
        write(qstr, &
     &    "('Original was ',i10,'.  Sorted is ',i10',.')")icounti, icounto
        call asnl(qstr )
        call return_to_continue()
      ENDIF
!
      RETURN
!
!     Error blocks - print various error messages.
!
 110  CONTINUE
      write(qstr,"('sort_snx_non error ',i5,A)") ierr,err_act
      call asnl(qstr )
      errlen = trimlen(err_file)
      write(qstr,"(A)") err_file(1:errlen)
      call asnl(qstr )
      write(qstr,"(A)")  err_cond
      call asnl(qstr )
      go to 999
 120  write(qstr,"(A)")  err_cond
      call asnl(qstr )
      write(qstr,"('input files are: ')")
      call asnl(qstr )
      write(qstr,"(A)") inoutt_file1(1:iotlen1)
      call asnl(qstr )
      write(qstr,"(A)") inoutt_file2(1:iotlen2)
      call asnl(qstr )
      go to 999
 210  write(qstr,"('unix system error  ',i5)") ierr
      call asnl(qstr )
      write(qstr,"('unix command was: ')")
      call asnl(qstr )
      errlen = trimlen(unix_com)
      write(qstr,"(A)") unix_com(1:errlen)
      call asnl(qstr )
      go to 999
!
!     common error processing
!
 999  call return_to_continue
      if (kbit( iclose, INT2(1) )) close(86)
      if (kbit( iclose, INT2(2) )) close(85)
      if (kbit( iclose, INT2(3) )) close(87)
!
      return
      end
