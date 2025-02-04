      SUBROUTINE LINFT(X,Y,SIGMY,NPTS,A,SIGMA,B,SIGMB,R,ADDED_SIGMY)
!
!     Good old LINFT from plx.
!     :90.11.05:jwr: Ported from PLX on the A900.
!     :91.01.15:jwr: Test for zero sigma added.
!     :92.07.26:jwr: Dimensions changed from 1 to * for array checking.
!     :93.10.06:dsc: ported here from snoop.  Modified to fit y= ax + b
!                    rather than y = bx +a
!
!     A is the slope
!     B is the intercept
!
      IMPLICIT NONE
      REAL*8 SUM,SUMX,SUMY,SUMX2,SUMXY,SUMY2,ADDED_SIGMY
      REAL*8 XI,YI,WEIGH,DELTA,VARNC,C,R
      REAL*8 X(*),Y(*),SIGMY(*),A,SIGMA,B,SIGMB
      INTEGER*2 MODE,I,NPTS,j
!
11    SUM=0.
      SUMX=0.
      SUMY=0.
      SUMX2=0.
      SUMXY=0.
      SUMY2=0.
!
21    DO 50 I=1,NPTS
        XI=X(I)
        YI=Y(I)
        WEIGH=(SIGMY(I)**2+ADDED_SIGMY**2)
        If(WEIGH.le. 1.d-25) Then
          Write(6,'( &
     &    " Zero weight in LINFT!",/, &
     &    " Point #",I5," X = ",f15.5," Y =",f20.5)') I,X(I),Y(I)
          pause
          write(6,'(//)')
          do j=1,npts
            write(6,'(i5,2f20.5)') j,x(j),y(j)
          enddo
          stop
        Endif
        WEIGH=1.D0/WEIGH
41      SUM=SUM+WEIGH
        SUMX=SUMX+WEIGH*XI
        SUMY=SUMY+WEIGH*YI
        SUMX2=SUMX2+WEIGH*XI*XI
        SUMXY=SUMXY+WEIGH*XI*YI
        SUMY2=SUMY2+WEIGH*YI*YI
50    CONTINUE
!
51    DELTA=SUM*SUMX2-SUMX*SUMX
      B=(SUMX2*SUMY-SUMX*SUMXY)/DELTA
53    A=(SUMXY*SUM-SUMX*SUMY)/DELTA
!
      VARNC=1.D0
67    SIGMB=DSQRT(VARNC*SUMX2/DELTA)
68    SIGMA=DSQRT(VARNC*SUM/DELTA)
71    R=(SUM*SUMXY-SUMX*SUMY)/DSQRT(DELTA*(SUM*SUMY2-SUMY*SUMY))
!
!     WRITE(6,'("INTERCEPT",F20.3,"+-",F10.3,"   "/
!    .          "SLOPE    ",F10.3,"+-",F10.3,"   /x_unit")')
!    .B, SIGMB, A, SIGMA
!
      RETURN
      END
