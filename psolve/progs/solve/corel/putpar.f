      SUBROUTINE PUTPAR ( NAM, SIG, ADJ, ROWS, TOT, KREF )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 ROWS
      CHARACTER*20 NAM(ROWS)
      logical*2 kref
      REAL*8 SIG(ROWS),ADJ(ROWS),tot(rows)
!
      INCLUDE 'corpar.i'
      INCLUDE 'corcom.i'
!
      INTEGER*2  I
      INTEGER*4  IOS
!
      IF ( KREF ) THEN
           WRITE ( LUOUT, 9911, IOSTAT=IOS ) ( NAM(I), TOT(I), I=ROWS+1,ROWS+3)
           CALL FERR ( INT2(IOS), "Writing paramter list", INT2(0), INT2(0) )
      ENDIF
      IF ( DATE .NE. ' ' ) THEN
           WRITE ( LUOUT, 9910, IOSTAT=IOS ) ( I, NAM(I), SIG(I), ADJ(I), &
     &                                         TOT(I), I=1,ROWS )
           CALL FERR ( INT2(IOS), "Writing paramter list", INT2(0), INT2(0) )
!9910      FORMAT(" ",I5,1X,A20,1X,F9.4,1X,2F14.4)
!9911      FORMAT("     0",1X,A20,25X,F14.4)
9910       FORMAT(" ",I5,1X,A20,1X,3D23.16)
9911       FORMAT("     0",1X,A20,47X,D23.16)
         ELSE
           WRITE ( LUOUT, 9912, IOSTAT=IOS ) ( I, NAM(I), SIG(I), &
     &                                         ADJ(I), I=1,ROWS )
           CALL FERR ( INT2(IOS), "Writing paramter list", INT2(0), INT2(0) )
9912       FORMAT(" ",I5,1X,A20,1X,1PD9.3,1X,1PD23.16)
      ENDIF
!
      RETURN
      END  !#!  PUTPAR  #!#
