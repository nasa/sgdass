      SUBROUTINE STECT ( SITDIF, LNAME, KFBDSP, NVSITEV, TIME0X, NVSITEC, &
     &                   PLATE_SCALE )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PROGRAM SPECIFICATION
!
! 1.1 Calculate site displacements using tectonic plate model
!       AM0-2 or NUVEL.
!
! 1.2 REFERENCES:
!
! 2.  STECT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) LNAME
      REAL*8 time0x,plate_scale
      LOGICAL*2 KFBDSP
!
! LNAME - Name of station mod file
! TIME0X - Site ref date parameter
! KFBDSP - True if flyby info is to be displayed
!
! 2.3 OUTPUT Variables:
!
      REAL*8 SITDIF(3,*),NVSITEV(3,*),nvsitec(3,*)
!
! SIDDIF - Site coordinate differences
! NVSITEV - SItee velocities after substitutions
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc2.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: flyby_init
!       CALLED SUBROUTINES: sitpl,absmo,absmo_nuvel
!
! 3.  LOCAL VARIABLES
!
      REAL*8 T,T0,XPM,YPM,ZPM
      logical*2 kbit,starc,equal
      CHARACTER*4 PLATES(MAX_ARC_STA)
      character*80 bufstr
      INTEGER*2 I,j
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  900620  Use site motion reference date provided in control file
!                (If none provided use default - 1980 Oct 17  00UT)
!
! 5. STECT PROGRAM STRUCTURE
!
      IF ( LNAME(1:5) .EQ. 'AM0-2' .OR. &
     &     LNAME(1:5) .EQ. 'NUVEL'      ) THEN
         ELSE
           CALL FERR ( INT2(219), 'STECT: Unknown plate motion model: '// &
     &                 LNAME, INT2(0), INT2(0) )
      END IF
!
      IF(KFBDSP) THEN
        IF(KSPOOL)WRITE(23,5514)lname
        IF(KSCREEN.and.kbit( pre_ip(2), INT2(6))) then
          WRITE(bufstr ,5514)lname
          call addstr_f(bufstr )
          call nl_mn()
        endif
 5514 FORMAT(   " Station velocities from model ",A10," applied")
      END IF
!
      T0 = TIME0X
      T  = TATM(1)/365.25D0
      CALL SITPL(ISITN,NUMSTA,PLATES )
      DO I=1,NUMSTA
         IF ( LNAME(1:5) .EQ. 'AM0-2' ) THEN
              CALL ABSMO ( PLATES(I), T0, (VSITEC(1,I)), (VSITEC(2,I)), &
     &                     (VSITEC(3,I)), T, XPM, YPM, ZPM, PLATE_SCALE )
            ELSE IF ( LNAME(1:5) .EQ. 'NUVEL' ) THEN
              IF ( LNAME(1:8) .EQ. 'NUVEL-1A' ) THEN
                   PLATE_SCALE = 0.9562D0
              ENDIF
              CALL ABSMO_NUVEL ( PLATES(I), T0, VSITEC(1,I), VSITEC(2,I), &
     &                           VSITEC(3,I), T, XPM, YPM, ZPM, PLATE_SCALE, &
     &                           NUVEL_FIXED )
         ENDIF
         SITDIF(1,I)= SITDIF(1,I)+ XPM-VSITEC(1,I)
         SITDIF(2,I)= SITDIF(2,I)+ YPM-VSITEC(2,I)
         SITDIF(3,I)= SITDIF(3,I)+ ZPM-VSITEC(3,I)
         STARC = .NOT. KCSTA
         DO J=1,NACSTA
            IF ( EQUAL( ISELAR(IACSTA+(J-1)*4), INT2(1), ISITN(1,I), INT2(1), &
     &           INT2(8)) ) THEN
                 STARC = .NOT.STARC
            ENDIF
         ENDDO
         IF ( .NOT. KBATCH .OR. STARC ) THEN
              NVSITEC(1,i)=NVSITEC(1,I) + XPM-VSITEC(1,I)
              NVSITEC(2,i)=NVSITEC(2,I) + YPM-VSITEC(2,I)
              NVSITEC(3,i)=NVSITEC(3,I) + ZPM-VSITEC(3,I)
         ENDIF
!
         NVSITEV(1,I) = NVSITEV(1,I) + (XPM-VSITEC(1,I))/(T-T0)
         NVSITEV(2,I) = NVSITEV(2,I) + (YPM-VSITEC(2,I))/(T-T0)
         NVSITEV(3,I) = NVSITEV(3,I) + (ZPM-VSITEC(3,I))/(T-T0)
      END DO
!
      RETURN
      END  !#!  STECT  #!#
