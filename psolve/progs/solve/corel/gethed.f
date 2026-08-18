      SUBROUTINE GETHED(KEND,ROWNAM,COLNAM,ROWS,COLS,ROWNUM,COLNUM)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER*(*) ROWNAM,COLNAM
      INTEGER*2 ROWS,COLS,ROWNUM,COLNUM
      LOGICAL*2 KEND
!
      INCLUDE 'corpar.i'
      INCLUDE 'corcom.i'
!
!     modifications
!
!     kdb 960514 Added documentation.
!
      CHARACTER*80 STRING,TOKEN
      INTEGER*2    DECIMALTOINT, TRIMLEN, IERR, N
      INTEGER*4 IOS
!
!     Gethed reads the two "header" records for a given arc from the input
!     CVRFxx file.
!     These take the form:
!
!      rownum rows rownam
!      colnum cols colnam   date
!
!     These values are written out by back/out_lst.f, and are actually
!     out_lst.f variables:
!
!         (iarcnm  jtot jname
!          iarcnm  jtot jname    datestr)  where
!         iarcnm= the number of the current arc
!         jtot = the number of covariance parameters applicable to the current
!            arc.  That is, the control file's $OUTPUT section's COVARIANCES
!            keyword requests certain global and/or arc parameters for which
!            covariances should be generated, and jtot shows the number that
!            apply to the current arc.
!         jname = the name of the current arc file.
!     Comment added by kdb, 5/14/96.
!
      IF(KEND) RETURN
!
100   continue
      READ(LUIN,'(1X,A)',END=99,IOSTAT=IOS) STRING
      CALL FERR ( INT2(IOS), "Reading covariance file", INT2(0), INT2(0) )
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      ROWNUM=DECIMALTOINT(TOKEN,IERR)
      IF(IERR.NE.0) PAUSE 'ERROR DECODING ROWARC'
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      ROWS=DECIMALTOINT(TOKEN,IERR)
      IF(IERR.NE.0) PAUSE 'ERROR DECODING ROWS'
!
      CALL SPLITSTRING(STRING,ROWNAM,STRING )
      IF(ROWNAM.EQ.' ') PAUSE 'EMPTY ROWNAM'
!
      READ(LUIN,'(1X,A)',END=99,IOSTAT=IOS) STRING
      CALL FERR ( INT2(IOS), "Reading covariance file", INT2(0), INT2(0) )
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      COLNUM=DECIMALTOINT(TOKEN,IERR)
      IF(IERR.NE.0) PAUSE 'ERROR DECODING COLARC'
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      COLS=DECIMALTOINT(TOKEN,IERR)
      IF(IERR.NE.0) PAUSE 'ERROR DECODING COLS'
!
      CALL SPLITSTRING(STRING,COLNAM,STRING )
      IF(COLNAM.EQ.' ') PAUSE 'EMPTY ROWNAM'
!
      date = '       '
      n = index(string,'Date')
      if (n.gt.0) then
        date = string(n:trimlen(string))
      else
        READ ( LUIN, '(A)', END=99, IOSTAT=IOS ) STRING
        CALL FERR ( INT2(IOS), "Reading covariance file", INT2(0), INT2(0) )
        n = index(string,'Date')
        date = string(n:trimlen(string))
      endif
      READ ( LUIN, '(A)', END=99, IOSTAT=IOS ) STRING
      CALL FERR ( INT2(IOS), "Reading covariance file", INT2(0), INT2(0) )
      IF ( ROWS .EQ. 0  .AND.  COLS .EQ. 0 ) GOTO 100
      RETURN
!
99    CONTINUE
      KEND=.TRUE.
!
      RETURN
      END
