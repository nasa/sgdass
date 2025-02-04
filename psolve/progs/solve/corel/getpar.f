      SUBROUTINE GETPAR ( KEND, NAM, SIG, ADJ, ROWS, TOT, KREF )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
      INTEGER*2 ROWS
      CHARACTER*20 NAM(ROWS)
      LOGICAL*2 KEND
      REAL*8 SIG(ROWS),ADJ(ROWS),tot(rows)
!
      INCLUDE 'corpar.i'
      INCLUDE 'corcom.i'
!
      INTEGER*2 I,is,irow
      INTEGER*4  IOS
      character*100 buf(3)
      LOGICAL*2 kref
!
      IF(KEND) RETURN
!
      if (date.ne.' ') then
        READ ( LUIN, 9912, END=99, IOSTAT=IOS ) (BUF(I),I=1,3)
        CALL FERR ( INT2(IOS), "Reading parameter list", INT2(0), INT2(0) )
        read(buf(1),'(I7)',IOSTAT=ios) irow
        CALL FERR ( INT2(IOS), "Reading parameter list", INT2(0), INT2(0) )
        if (irow.eq.0) then
          do i=1,3
            READ ( BUF(I), 9911, IOSTAT=IOS ) NAM(I+ROWS), TOT(I+ROWS)
            CALL FERR ( INT2(IOS), "Reading parameter list", INT2(0), INT2(0) )
          enddo
          is=1
          kref=.TRUE.
        else
          do i=1,3
            READ ( BUF(I), 9910, IOSTAT=IOS ) NAM(I), SIG(I), ADJ(I), TOT(I)
            CALL FERR ( INT2(IOS), "Reading parameter list", INT2(0), INT2(0) )
          enddo
          is=4
          kref=.FALSE.
        endif
      endif
      READ ( LUIN, 9910, END=99, IOSTAT=IOS ) (NAM(I),SIG(I),ADJ(I), &
     &                                         TOT(I), I=IS, ROWS )
      CALL FERR ( INT2(IOS), "Reading parameter list", INT2(0), INT2(0) )
9910  FORMAT(7X,A20,1X,3D23.16)
9911  FORMAT(7X,A20,47X,D23.16)
9912  FORMAT(A)
      RETURN
!
99    CONTINUE
      KEND=.TRUE.
      RETURN
      END  !#!  GETPAR  #!#
