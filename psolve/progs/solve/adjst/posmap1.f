      SUBROUTINE POSMAP1(VSC,NPARMC,NPARMV,ISTA,TYPEPR,xyzprm,xyzvel, &
     &    sigmx,mat)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  POSMAP1 PROGRAM SPECIFICATION
!
! 1.1 MAPP POSITIONS AND VELOCITIES FOR XYZ COORDINATES TO OTHER YEARS
!  ON INPUT WE ASSUME:
!
!    VSITEV(1...3,ISTA) = IS FINAL ADJUSTED VALUE FOR THE VELOCITIES
!    A(...), B(...)  STILL CONTAINS THE INVERTED NORMAL EQUATIONS AND
!                    ADJUSTMENTS, WITH COORDINATE AND VELOCITY PARAMETERS
!                    AND covariances FOR STATION ISTA UNMODIFIED
!    LSITEV, LSITEC, ISITN STILL VALID FOR THIS SOLUTION
!
! 1.2 REFERENCES:
!
! 2.  POSMAP1 INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) TYPEPR
      REAL*8 VSC(3),sigmx(21),mat(*)
      INTEGER*2 NPARMC,NPARMV,ISTA,xyzprm(3,*),xyzvel(6,*)
!
!    VSC    - A PRIORI STATION COORDIANTES FOR THIS STATIONS
!    NPARMC - PARMETER NUMBER BEFORE FIRST STATION COORDIANTE PARAMETER
!             FOR THIS STATION
!    NPARMV - PARMETER NUMBER BEFORE FIRST STATION VELOCITY   PARAMETER
!             FOR THIS STATION
!    ISTA   - STATION NUMBER OF THIS STATION
!    TYPEPR - FOR OUTPUT CONTROL
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'prfil.i'
!      INCLUDE '../include/nrmcm.i'
      INCLUDE 'dmapp.i'
      REAL*8 ADJ(6),SIGM(21),AMAP(6,6),RES(6,6),RES2(21),ADJ2(6)
      REAL*8 SIGT(3),ADJT(3),DUM(6),SCALE(6)
      real*8 vs2(3,MAX_STA)
      COMMON/TIMMAP/ADJ,SIGM,AMAP,RES,RES2,ADJ2,DUM,SCALE,ADJT,SIGT
      COMMON/adj/ vs2
!
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: a1jst
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,K,L,NP(6),NPARM,ICMP(3),IYEAR,iold,n,xr(6)
      INTEGER*4 INDX4,iblas0,iblas1,iblas7,nblas,iblas14,jb
      LOGICAL*2 KBIT,equal
      REAL*8 z,onez
      REAL*8 FJLDY,DELTAT,REF,TU
!
      DATA ICMP/2HX ,2HY ,2HZ /
!
! 4.  HISTORY
!  WHO   WHEN    WHAT
!
! 5.  POSMAP1 PROGRAM STRUCTURE
!
      jb = 2*M_GPA
      NPARM=NPARMC
!
!  INITIALIZE
!
      DO I=1,6
        ADJ(I)=0.0D0
        DO J=1,I
          SIGM(INDX4(J,I))=0.0D0
        ENDDO
        NP(I)=0
      ENDDO
!
! FIND COORDINATE PARAMETERS AND ADJUSTMENT VALUES
!
      DO I=1,3
        IF(KBIT(LSITEC(1,I),ISTA)) THEN
           NPARM=NPARM+1
           NP(1+2*(I-1))=NPARM
        ENDIF
        if (nparm.gt.0) then
          ADJ(1+2*(I-1))=mat(jb+NPARM)
        endif
      ENDDO
!
! FIND VELOCITY PARMETERS AND TOTAL VALUES
!
      DO I=1,3
        xr(1+2*(i-1))=i
        xr(2*i)=3+i
        IF(KBIT(LSITEV(1,I),ISTA)) THEN
           NPARM=NPARM+1
           NP(2*I)=NPARM
        ENDIF
        ADJ(2*I)=VS2(I,ISTA)
      ENDDO
        do i = 1,ista-1
          if(equal( isitn(1,ista), INT2(1), isitn(1,i), INT2(1), INT2(8))) &
     &       go to 700
        enddo
        i = ista
700     continue
        iold = i
        do i=1,3
          if (kbit(lsitev(1,i),iold)) then
            np(2*i) = xyzprm(i,iold)
          endif
!         adj(2*i) = vsitev(i,iold)
          adj(2*i) = vs2(i,iold)
        enddo
!
!  GET COVARIANCES
!
!
      do i=1,6
        do j=1,i
          sigm(indx4(j,i))=sigmx(indx4(xr(j),xr(i)))
        enddo
      enddo
      RETURN
      END
