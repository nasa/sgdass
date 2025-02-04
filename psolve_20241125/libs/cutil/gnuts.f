      SUBROUTINE GNUTS ( DPRIN, DDECA, DANNU, DSEMA, D122D, DSEMM, DPREC, &
     &                   LNAME, KFBDSP, VPREC, NVPREC )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GNUTS PROGRAM SPECIFICATION
!
! 1.1 Read a nutation parameter value changes file.
!
! 1.2 REFERENCES:
!
! 2.  GNUTS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) LNAME
      LOGICAL*2 KFBDSP
      REAL*8 VPREC
!
! KFBDSP - True if flyby info is to be displayed
! LNAME - File name
! VPREC - Precession constant a priori
!
! 2.3 OUTPUT Variables:
!
      REAL*8 DPRIN(4),DDECA(4),DANNU(4),DSEMA(4),D122D(4), &
     &       DSEMM(4),DPREC,NVPREC
!
! DANNU - Nutation annual term difference
! DDECA - Nutation 9.3 year term difference
! DPREC - Nutation precession term difference
! DPRIN - Nutation principal term difference
! DSEMA - Nutation semiannual term difference
! DSEMM - Nutation 13.7 day term difference
! D122D - Nutation 122.7 day term difference
! NVPREC - Precession term from mapping file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 JNAM(3)
      INTEGER*4  IOS
      INTEGER*2 I,NNUTS,IDUM,JJ,IE
      logical*2 kbit
      REAL*8 TWOPI,CONV,DXXXX(4)
      CHARACTER*80 CBUF,bufstr
      CHARACTER*100 errstr
      DATA TWOPI /.6283185307179587D+1/
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  881109  Added status info to OPEN
!   AEE  910515  Enhanced error messages written to the error file.
!
! 5.  GNUTS PROGRAM STRUCTURE
!
    1 CONTINUE
      OPEN ( 40, FILE=LNAME, STATUS='OLD', IOSTAT=IOS )
!
      IF(IOS.NE.0) THEN
        WRITE(errstr,"('Failure in opening nutation mod file in gnuts', &
     &     A15)") LNAME
        call ferr( INT2(186), errstr, INT2(0), INT2(0) )
        GOTO 1
      ENDIF
!
      IF(KFBDSP) THEN
      IF(KSPOOL) WRITE(23,5514) LNAME
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
           WRITE ( BUFSTR, 5514 ) LNAME
           CALL    ADDSTR_F ( BUFSTR )
           CALL NL_MN()
      ENDIF
 5514 FORMAT(" Nutation model parameter value changes from file ",A63)
      END IF
!
! --- Get the list of nutation parameter value changes
!
      DPREC=VPREC*10000.0D0
      NNUTS = 0
      CBUF=' '
      DO WHILE (CBUF(1:2).NE.'//')
!
! ------ reading in the nutation model parameter offset values.
!
         READ(40,'(A)',END=9590, IOSTAT= IOS ) CBUF
         CALL FERR ( INT2(IOS), "(GNUTS) Reading nutation mod file "//LNAME, &
     &               INT2(0), INT2(0) )
         IF ( .NOT. ( CBUF(1:2).EQ.'//' .OR. CBUF(1:1).EQ.'$') )THEN  !good card
              NNUTS = NNUTS+1
              IF (NNUTS.GT.7 )THEN  !error exit because of too many cards
              WRITE(errstr,113)
 113          FORMAT ( " More than 7 nutation parameters. Terminating "// &
     &                 "substitution")
              CALL FERR ( INT2(187), ERRSTR, INT2(0), INT2(0) )
          END IF  !error exit because of too many cards
          IE=4
          IF(NNUTS.EQ.7) IE=1
          READ ( CBUF, 102, IOSTAT=IOS) (DXXXX(I),I=1,IE)
          IF ( IOS .NE. 0 ) CALL FERR ( INT2(IOS), "(GNUTS) Reading nutation "// &
     &        "model file "//LNAME, INT2(0), INT2(0) )
  102     FORMAT(10X,4F13.6)
!
          IF(NNUTS.EQ.1)THEN
            DO I=1,4
              DPRIN(I)=DXXXX(I)
            END DO
          ELSE IF(NNUTS.EQ.2)THEN
            DO I=1,4
              DDECA(I)=DXXXX(I)
            END DO
          ELSE IF(NNUTS.EQ.3)THEN
            DO I=1,4
              DANNU(I)=DXXXX(I)
            END DO
          ELSE IF(NNUTS.EQ.4)THEN
            DO I=1,4
              DSEMA(I)=DXXXX(I)
            END DO
          ELSE IF(NNUTS.EQ.5)THEN
            DO I=1,4
              D122D(I)=DXXXX(I)
            END DO
          ELSE IF(NNUTS.EQ.6)THEN
            DO I=1,4
              DSEMM(I)=DXXXX(I)
            END DO
          ELSE IF(NNUTS.EQ.7) THEN
            DPREC=DXXXX(1)*10000.0
          END IF
!
        END IF  !good        card found
      END DO  !reading the source positions
!
 9590 CONTINUE
!
! --- Change units from 0.0001 arcsec to radian
!
      CONV=(360.D0/TWOPI)*3600.D0*10000.D0
      DO I=1,4
          DPRIN(I)=DPRIN(I)/CONV
          DDECA(I)=DDECA(I)/CONV
          DANNU(I)=DANNU(I)/CONV
          DSEMA(I)=DSEMA(I)/CONV
          D122D(I)=D122D(I)/CONV
          DSEMM(I)=DSEMM(I)/CONV
      ENDDO
!
! --- Precession constant a priori is stored in arcsecond/century
! --- But was .0001 arcsec in mapping file
! --- Precession constant partial is radians/century
!
      NVPREC=DPREC/10000.0D0
      DPREC=(NVPREC-VPREC)*10000.D0/CONV
!
! --- Close the substitute file.
!
      CLOSE(40)
      RETURN
      END  !#!  GNUTS  #!#
