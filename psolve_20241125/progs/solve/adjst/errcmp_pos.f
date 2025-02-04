      SUBROUTINE ERRCMP_POS(VS,NPARMV,LSITEV,ISTA,mat)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ERRCMP_POS PROGRAM SPECIFICATION
!
! 1.1  RECOVER ERROR ELLIPSES FOR UEN POSITIONS
!
! 1.2 REFERENCES:
!
! 2.  ERRCMP_POS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 NPARMV
      INTEGER*2 LSITEV(STA_BIT_WORDS,3),ISTA
      REAL*8 VS(3),mat(*)
!
! ISTA - Station number
! LSITEV - Station position flag
! NPARMV - Parameter number immediately before first one we are to use
! VS - Component positions
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!      include '../include/nrmcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: a1jst
!       CALLED SUBROUTINES: jacobi, eigsrt
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  ICMP(3),I,J,KBITN,NC,NROT,IA(3)
      INTEGER*4  NPARM
      INTEGER*8, EXTERNAL :: INDX8
      INTEGER*8  IAIIAJ
      INTEGER*4  JA, JB
      LOGICAL*2  KBIT
      REAL*8 ALCL(3,3),D(3),V(3,3),BT(3),Z(3),AZD,AZM,ELD,MAG,RD2DG
      real*8 dx(2),vx(2,2),btx(2),zx(2),alclx(2,2)
      REAL*8 BLCL(3),DATAN2Z,VSLCL(3),thorvec,ahorvec
      REAL*8 DAZDE,DAZDN,DVDE,DVDN,CEE,CNE,CNN,SAZ,SV
!
      DATA ICMP/3,1,2/,RD2DG/57.29577951D0/
!
! 4.  HISTORY
!  WHO   WHEN    WHAT
!
! 5.  ERRCMP_POS PROGRAM STRUCTURE
!
! Check to make sure that at least two components were estimated
!
      NC=KBITN(LSITEV(1,1),ISTA)+KBITN(LSITEV(1,2),ISTA)+ &
     &   KBITN(LSITEV(1,3),ISTA)
      IF(NC.LT.2) RETURN
      ja = 3*M_GPA
      jb = 2*M_GPA
!
! Initialize some arrays
!
      DO I=1,3
        BLCL(I)=0.0D0
        VSLCL(I)=0.0D0
        IA(I)=0
        DO J=1,3
          ALCL(J,I)=0.0D0
        ENDDO
      ENDDO
!
! Extract the data that we'll need
!
      NPARM=NPARMV
      DO I=1,3
        IF(KBIT(LSITEV(1,I),ISTA)) THEN
          NPARM=NPARM+1
          BLCL(ICMP(I))=mat(jb+NPARM)
          IA(ICMP(I))=NPARM
        ENDIF
        VSLCL(ICMP(I))=VS(I)
      ENDDO
!
!  ICMP IS USED TO MAP BETWEEN UEN VELOCITIES IN SOLVE AND ENU VELOCITIES HERE
!       CAN BE GOTTEN RID WHEN THE VELOCITIES ARE ESTIMATED ENU
!
      DO I=1,3
        IF(IA(I).NE.0) THEN
          DO J=1,3
            IF(IA(J).NE.0) THEN
              IAIIAJ=INDX8(IA(I),IA(J))
              ALCL(J,I)=mat(ja+IAIIAJ)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
!
      CEE=ALCL(1,1)
      CNE=ALCL(2,1)
      CNN=ALCL(2,2)
!
! Copy the covariance matrix, omitting up components, for horizontal
!
      do i=1,2
        do j=1,2
          alclx(i,j) = alcl(i,j)
        enddo
      enddo
!
! Total vector case
!
!@      CALL JACOBI( ALCL, INT2(3), INT2(3), D, V, NROT, BT, Z )
!@      CALL EIGSRT( D, V, INT2(3), INT2(3) )
      CALL LAPACK_JACOBI( ALCL, INT2(3), INT2(3), D, V, NROT, BT, Z )
!
! Horizontal vector case
!
!@      CALL JACOBI ( ALCLX, int2(2), int2(2), DX, VX, NROT, BTX, ZX )
!@      CALL EIGSRT ( DX, VX, int2(2), int2(2) )
      CALL LAPACK_JACOBI ( ALCLX, int2(2), int2(2), DX, VX, NROT, BTX, ZX )
!
! Calculate direction and magnitude of total position vector
!
      AZD=DATAN2Z(VSLCL(1),VSLCL(2))*RD2DG
      IF(AZD.LT.0.0D0) AZD=AZD+360.0D0
      AZM=DSQRT(VSLCL(1)**2+VSLCL(2)**2)
      ELD=DATAN2Z(VSLCL(3),AZM)*RD2DG
      MAG=DSQRT(AZM**2+VSLCL(3)**2)
!      WRITE(23,9908) AZD,ELD,MAG*1000.0D0
9908  FORMAT(30X,' Total Position Vector:      ', &
     & 'Azimuth = ',F6.1,' Elevation = ',F5.1,' Position = ',F10.3, &
     & ' mm/yr')
!
! Now do the horizontal component of the total position vector
!
      IF(VSLCL(1)**2+VSLCL(2)**2.GT.1D-8) THEN
        thorvec = sqrt(vslcl(1)**2+vslcl(2)**2)
        DAZDE= VSLCL(2)/(VSLCL(1)**2+VSLCL(2)**2)   ! N/(E**2+N**2)
        DAZDN=-VSLCL(1)/(VSLCL(1)**2+VSLCL(2)**2)   !-E/(E**2+N**2)
        DVDE=VSLCL(1)/SQRT(VSLCL(1)**2+VSLCL(2)**2) !E/SQRT(E**2+N**2)
        DVDN=VSLCL(2)/SQRT(VSLCL(1)**2+VSLCL(2)**2) !N/SQRT(E**2+N**2)
        SAZ=SQRT(DAZDE*DAZDE*CEE+DAZDN*DAZDN*CNN+2.0D0*DAZDE*DAZDN*CNE)
        SV =SQRT(DVDE *DVDE *CEE+DVDN *DVDN *CNN+2.0D0*DVDE *DVDN *CNE)
!        write(23,9921) azd,thorvec*1000.d0
9921    FORMAT(30X,' Total Horizontal Vector:    ', &
     &  "Azimuth = ",F6.1,18X," Position = ",F10.3," mm")
!        WRITE(23,9922) SAZ*RD2DG,SV*1000.0D0
9922    FORMAT(30X,' Total Horizontal Sigma:     ', &
     &  "Azimuth = ",F6.1,18X," Position = ",F10.3," mm")
      ENDIF
!
! Now calculate the adjustment position vector
!
      AZD=DATAN2Z(BLCL(1),BLCL(2))*RD2DG
      IF(AZD.LT.0.0D0) AZD=AZD+360.0D0
      AZM=DSQRT(BLCL(1)**2+BLCL(2)**2)
      ELD=DATAN2Z(BLCL(3),AZM)*RD2DG
      MAG=DSQRT(AZM**2+BLCL(3)**2)
      WRITE(23,9909) AZD,ELD,MAG*1000.0D0
9909  FORMAT(25X,' Adjustment Position Vector:      ', &
     & 'Azimuth = ',F6.1,' Elevation = ',F5.1,' Position = ',F10.3, &
     & ' mm')
!
! Now do the horizontal component of the adjustment position vector
!
      IF(BLCL(1)**2+BLCL(2)**2.GT.1D-8) THEN
        ahorvec = sqrt(blcl(1)**2+blcl(2)**2)
        DAZDE= BLCL(2)/(BLCL(1)**2+BLCL(2)**2)   ! N/(E**2+N**2)
        DAZDN=-BLCL(1)/(BLCL(1)**2+BLCL(2)**2)   !-E/(E**2+N**2)
        DVDE=BLCL(1)/SQRT(BLCL(1)**2+BLCL(2)**2) !E/SQRT(E**2+N**2)
        DVDN=BLCL(2)/SQRT(BLCL(1)**2+BLCL(2)**2) !N/SQRT(E**2+N**2)
        SAZ=SQRT(DAZDE*DAZDE*CEE+DAZDN*DAZDN*CNN+2.0D0*DAZDE*DAZDN*CNE)
        SV =SQRT(DVDE *DVDE *CEE+DVDN *DVDN *CNN+2.0D0*DVDE *DVDN *CNE)
        write(23,9923) azd,ahorvec*1000.d0
9923    FORMAT(17X,'Position Adjustment Horizontal Vector:    ', &
     &  "Azimuth = ",F6.1,18X," Position = ",F10.3," mm")
        WRITE(23,9920) SAZ*RD2DG,SV*1000.0D0
9920    FORMAT(17X,'Position Adjustment Horizontal Sigma:     ', &
     &  "Azimuth = ",F6.1,18X," Position = ",F10.3," mm")
      ENDIF
!
! Finally determine and write the axes of the error ellipsoid
!
      DO I=1,NC
        IF(DSQRT(MAX(0.0D0,D(I)))*1000.0D0.GE..0005) THEN
          AZD=DATAN2Z(V(1,I),V(2,I))*RD2DG
          IF(AZD.LT.0.0D0) AZD=AZD+360.0D0
          AZM=DSQRT(V(1,I)**2+V(2,I)**2)
          ELD=DATAN2Z(V(3,I),AZM)*RD2DG
          WRITE(23,9910) AZD,ELD,DSQRT(D(I))*1000.0D0
9910      FORMAT(28X,'Position Error Ellipsoid Axis: ', &
     & 'Azimuth = ',F6.1,' Elevation = ',F5.1,' Sigma    = ',F10.3, &
     & ' mm')
        ENDIF
      ENDDO
!
! Calculate and write horizontal error ellipsoid
!
      DO I=1,2
        IF(DSQRT(MAX(0.0D0,Dx(I)))*1000.0D0.GE..0005) THEN
          AZD=DATAN2Z(Vx(1,I),Vx(2,I))*RD2DG
          IF(AZD.LT.0.0D0) AZD=AZD+360.0D0
          ELD=0.0d0
          WRITE(23,9911) AZD,ELD,DSQRT(Dx(I))*1000.0D0
9911      FORMAT(19X,'Position Horizontal Error Ellipse Axis: ', &
     & 'Azimuth = ',F6.1,' Elevation = ',F5.1,' Sigma    = ',F10.3, &
     & ' mm')
        ENDIF
      ENDDO
      RETURN
      END
