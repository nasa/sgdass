      SUBROUTINE ERRCMP ( VS, NPARMV, LSITEV, ISTA, MAT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ERRCMP PROGRAM SPECIFICATION
!
! 1.1  RECOVER ERROR ELLIPSES FOR UEN VELOCITIES
!
! 1.2 REFERENCES:
!
! 2.  ERRCMP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 NPARMV
      INTEGER*2 LSITEV(STA_BIT_WORDS,3),ISTA
      REAL*8    VS(3), MAT(*)
!
! ISTA - Station number
! LSITEV - Station velocity flag
! NPARMV - Parameter number immediately before first one we are to use
! VS - Component velocities
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
      INTEGER*2  ICMP(3),I,J,KBITN,NC,NROT
      INTEGER*4  IA(3), NPARM
      INTEGER*8  IAIIAJ
      INTEGER*8, EXTERNAL :: INDX8
      INTEGER*4  JA, JB
      LOGICAL*2 KBIT
      REAL*8 ALCL(3,3),D(3),V(3,3),BT(3),Z(3),AZD,AZM,ELD,MAG,RD2DG
      real*8 dx(2),vx(2,2),btx(2),zx(2),alclx(2,2)
      REAL*8 BLCL(3), DATAN2Z, VSLCL(3), THORVEC, AHORVEC, SAZ2, SV2
      REAL*8 DAZDE,DAZDN,DVDE,DVDN,CEE,CNE,CNN,SAZ,SV
!
      DATA ICMP / 3, 1, 2 /, RD2DG / 57.29577951D0 /
!
! 4.  HISTORY
!  WHO   WHEN        WHAT
!  ???   ????        ????
!  pet   2001.12.13  Tried to imporve comments in this can of worms
!
! 5.  ERRCMP PROGRAM STRUCTURE
!
! Check to make sure that at least two components were estimated
!
      NC=KBITN(LSITEV(1,1),ISTA)+KBITN(LSITEV(1,2),ISTA)+ &
     &   KBITN(LSITEV(1,3),ISTA)
      IF(NC.LT.2) RETURN
      JA = 3*M_GPA
      JB = 2*M_GPA
!
! --- Initialize some arrays
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
! --- Extract the data that we'll need
!
      NPARM=NPARMV
      DO I=1,3
         IF ( KBIT ( LSITEV(1,I), ISTA ) ) THEN
              NPARM=NPARM+1
              BLCL(ICMP(I)) = MAT(JB+NPARM)
              IA(ICMP(I)) = NPARM
         ENDIF
         VSLCL(ICMP(I))=VS(I)
      ENDDO
!
! --- ICMP is used to map between uen velocities in solve and enu velocities
! --- here can be gotten rid when the velocities are estimated ENU
!
      DO I=1,3
         IF ( IA(I) .NE. 0 ) THEN
              DO J=1,3
                 IF ( IA(J) .NE. 0 ) THEN
                      IAIIAJ = INDX8 ( IA(I), IA(J) )
                      ALCL(J,I) = MAT(JA+IAIIAJ)
                 ENDIF
              ENDDO
         ENDIF
      ENDDO
!
      CEE=ALCL(1,1)
      CNE=ALCL(2,1)
      CNN=ALCL(2,2)
!
! --- Copy the covariance matrix, omitting up components, for horizontal
!
      DO I=1,2
         DO J=1,2
            ALCLX(I,J) = ALCL(I,J)
         ENDDO
      ENDDO
!
! --- Total vector case
!
!@      CALL JACOBI ( ALCL, INT2(3), INT2(3), D, V, NROT, BT, Z )
!@      CALL EIGSRT ( D, V, INT2(3), INT2(3) )
      CALL LAPACK_JACOBI ( ALCL, INT2(3), INT2(3), D, V, NROT, BT, Z )
!
! --- Horizontal vector case
!
!@      CALL OLD_JACOBI ( ALCLX, INT2(2), INT2(2), DX, VX, NROT, BTX, ZX )
!@      CALL OLD_EIGSRT ( DX, VX, INT2(2), INT2(2) )
      CALL LAPACK_JACOBI ( ALCLX, INT2(3), INT2(3), DX, VX, NROT, BTX, ZX )
!
! --- Calculate direction and magnitude of total velocity vector
!
      AZD = DATAN2Z(VSLCL(1),VSLCL(2))*RD2DG
      IF ( AZD .LT. 0.0D0 ) AZD=AZD+360.0D0
      AZM = DSQRT ( VSLCL(1)**2 + VSLCL(2)**2 )
      ELD = DATAN2Z ( VSLCL(3), AZM )*RD2DG
      MAG = DSQRT ( AZM**2 + VSLCL(3)**2 )
      WRITE ( 23, 9908 ) AZD, ELD, MAG*1000.0D0
9908  FORMAT(30X,' Total Velocity Vector:      ', &
     & 'Azimuth = ',F6.1,' Elevation = ',F5.1,' Velocity = ',F10.3, &
     & ' mm/yr')
!
! Now do the horizontal component of the total velocity vector
!
      IF ( VSLCL(1)**2 + VSLCL(2)**2 .GT. 1D-8 ) THEN
           THORVEC = SQRT ( VSLCL(1)**2 + VSLCL(2)**2 )
           DAZDE= VSLCL(2)/(VSLCL(1)**2+VSLCL(2)**2)   !  N/(E**2+N**2)
           DAZDN=-VSLCL(1)/(VSLCL(1)**2+VSLCL(2)**2)   ! -E/(E**2+N**2)
           DVDE=VSLCL(1)/SQRT(VSLCL(1)**2+VSLCL(2)**2) !  E/SQRT(E**2+N**2)
           DVDN=VSLCL(2)/SQRT(VSLCL(1)**2+VSLCL(2)**2) !  N/SQRT(E**2+N**2)
           SAZ = SQRT ( DAZDE*DAZDE*CEE+DAZDN*DAZDN*CNN + &
     &     2.0D0*DAZDE*DAZDN*CNE )
           IF ( DVDE*DVDE*CEE + DVDN*DVDN*CNN + 2.0D0*DVDE*DVDN*CNE < 0.0D0 ) THEN
                SV = -1.0D0
              ELSE
                SV  = SQRT ( DVDE*DVDE*CEE + DVDN*DVDN*CNN + 2.0D0*DVDE*DVDN*CNE )
           END IF
           WRITE ( 23, 9921)  AZD, THORVEC*1000.D0
9921       FORMAT(30X,' Total Horizontal Vector:    ', &
     &                "Azimuth = ",F6.1,18X," Velocity = ",F10.3," mm/yr")
           WRITE ( 23, 9922 ) SAZ*RD2DG, SV*1000.0D0
9922       FORMAT(30X,' Total Horizontal Sigma:     ', &
     &                "Azimuth = ",F6.1,18X," Velocity = ",F10.3," mm/yr")
      ENDIF
!
! Now calculate the adjustment velocity vector
!
      AZD = DATAN2Z ( BLCL(1), BLCL(2) )*RD2DG
      IF ( AZD .LT. 0.0D0 ) AZD = AZD + 360.0D0
      AZM = DSQRT ( BLCL(1)**2 + BLCL(2)**2 )
      ELD = DATAN2Z ( BLCL(3), AZM )*RD2DG
      MAG = DSQRT ( AZM**2 + BLCL(3)**2 )
      WRITE ( 23, 9909 ) AZD, ELD, MAG*1000.0D0
9909  FORMAT(25X,' Adjustment Velocity Vector:      ', &
     & 'Azimuth = ',F6.1,' Elevation = ',F5.1,' Velocity = ',F10.3, &
     & ' mm/yr')
!
! --- Now do the horizontal component of the adjustment velocity vector
!
      IF ( BLCL(1)**2 + BLCL(2)**2 .GT. 1.0D-8 ) THEN
           AHORVEC = SQRT ( BLCL(1)**2 + BLCL(2)**2 )
           DAZDE= BLCL(2)/(BLCL(1)**2+BLCL(2)**2)         ! N/(E**2+N**2)
           DAZDN=-BLCL(1)/(BLCL(1)**2+BLCL(2)**2)         !-E/(E**2+N**2)
           DVDE = BLCL(1)/SQRT ( BLCL(1)**2 + BLCL(2)**2) !E/SQRT(E**2+N**2)
           DVDN = BLCL(2)/SQRT ( BLCL(1)**2 + BLCL(2)**2) !N/SQRT(E**2+N**2)
           SAZ2 = DAZDE*DAZDE*CEE + DAZDN*DAZDN*CNN + 2.0D0*DAZDE*DAZDN*CNE
           SV2  = DVDE *DVDE *CEE + DVDN *DVDN *CNN + 2.0D0*DVDE *DVDN *CNE
           IF ( SAZ2 .LT. 0.0D0 ) THEN
                WRITE ( 6, * ) ' SAZ2=',SAZ2
                CALL FERR ( INT2(1431), &
     &              'ADJST(errcmp) Error in computation '// &
     &              'of error ellipse: square root from negative '//'number', &
     &               INT2(0), INT2(0) )
                STOP 'ADJST: abnormal termination'
           END IF
           IF ( SV2 .LT. 0.0D0 ) THEN
                WRITE ( 6, * ) ' SV2=',SV2
                CALL FERR ( INT2(1432), &
     &              'ADJST(errcmp) Error in computation '// &
     &              'of error ellipse: square root from negative '//'number', &
     &               INT2(0), INT2(0) )
                STOP 'ADJST: abnormal termination'
           END IF
!
           SAZ=SQRT ( DAZDE*DAZDE*CEE + DAZDN*DAZDN*CNN + &
     &     2.0D0*DAZDE*DAZDN*CNE )
           SV =SQRT ( DVDE *DVDE *CEE + DVDN *DVDN *CNN + 2.0D0*DVDE *DVDN &
     &     *CNE )
           WRITE ( 23, 9923 ) AZD, AHORVEC*1000.D0
9923       FORMAT(25X,' Adjustment Horizontal Vector:    ', &
     &                "Azimuth = ",F6.1,18X," Velocity = ",F10.3," mm/yr")
           WRITE ( 23, 9920 ) SAZ*RD2DG, SV*1000.0D0
9920       FORMAT(25X,' Adjustment Horizontal Sigma:     ', &
     &                "Azimuth = ",F6.1,18X," Velocity = ",F10.3," mm/yr")
      ENDIF
!
! --- Finally determine and write the axes of the error ellipsoid
!
      DO I=1,NC
         IF ( DSQRT ( MAX(0.0D0,D(I)))*1000.0D0 .GE. 0.0005D0 ) THEN
              AZD=DATAN2Z(V(1,I),V(2,I))*RD2DG
              IF ( AZD .LT. 0.0D0 ) AZD=AZD+360.0D0
              AZM = DSQRT ( V(1,I)**2 + V(2,I)**2 )
              ELD = DATAN2Z ( V(3,I), AZM )*RD2DG
              WRITE ( 23, 9910 ) AZD, ELD, DSQRT(D(I))*1000.0D0
9910          FORMAT(36X,' Error Ellipsoid Axis: Azimuth = ',F6.1, &
     &                   ' Elevation = ',F5.1,' Sigma    = ',F10.3,' mm/yr')
         ENDIF
      ENDDO
!
! --- Calculate and write horizontal error ellipsoid
!
      DO I=1,2
         IF ( DSQRT ( MAX(0.0D0,DX(I)))*1000.0D0 .GE. 0.0005D0 ) THEN
              AZD=DATAN2Z ( VX(1,I), VX(2,I) )*RD2DG
              IF ( AZD .LT. 0.0D0 ) AZD=AZD+360.0D0
              ELD=0.0D0
              WRITE ( 23, 9911 ) AZD, ELD, DSQRT(DX(I))*1000.0D0
9911          FORMAT(27X,' Horizontal Error Ellipse Axis: Azimuth = ', F6.1, &
     &                   ' Elevation = ',F5.1,' Sigma    = ',F10.3,' mm/yr')
         ENDIF
      ENDDO
!
      RETURN
      END  !#!  ERRCMP   #!#
