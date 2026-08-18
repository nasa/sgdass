      SUBROUTINE PLOD_CORR ( PLOD_FILE )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! --- This routine reads a calibration file to get values to
! --- use for atmospheric pressure correction at each station.
!
!     HISTORY:
!     * written by D.MacMillan 11/23/92
!     MWH   6/1/93      for use within SOLVE
!     pet  2000.07.27   Allowed PLOD_FILE be the absolute filename (with path)
!
      CHARACTER   PLOD_FILE*(*)
      INCLUDE 'solve.i'
      INCLUDE 'prfil.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
!
      REAL*8     ATMIN(200), AVPRES(200)  ! Pressure dependence (mm/mb) each site
      REAL*8     DEFATM, DEFPRES
      INTEGER*2  JSITE(4,200), I, J, K, NUMSITES
      INTEGER*4  IERR
      LOGICAL*2 EQUAL
      CHARACTER BUFSTR*120, TOKEN*20, FINAM*128
!
      KPLODCAL = .FALSE.
      IF ( PLOD_FILE .EQ. 'NONE' .OR. &
     &     PLOD_FILE .EQ. ' '        ) THEN
           RETURN
      END IF
!
      CALL CHAR2HOL ( PLOD_FILE, PLCALF, INT2(1), NAME_SIZE )
      DEFATM  =   0.D0
      DEFPRES = 980.D0
!
! --- Set filename
!
      CALL CLRCH ( FINAM )
      IF ( PLOD_FILE(1:1) .EQ. '/' ) THEN
           FINAM = PLOD_FILE
         ELSE
           FINAM = PRE_SAV_DIR(:PRE_SV_LEN)//PLOD_FILE
      END IF
!
      OPEN ( 64, FILE=FINAM, STATUS='OLD', IOSTAT=IERR )
      IF ( IERR .NE. 0) THEN
           CALL FERR ( INT2(IERR), '(PLOD_CORR) opening pressure loading file '// &
     &          FINAM, INT2(0), INT2(0) )
      END IF
      NUMSITES = 0
      J = 1
      DO WHILE ( .TRUE. )                 ! read value for each site
         READ ( 64, '(A)', END=10, IOSTAT=IERR ) BUFSTR
         IF ( IERR .NE. 0 )  THEN
              CALL FERR ( INT2(IERR), 'PLOD_CORR Error in reading pressure '// &
     &            'loading file '//FINAM, INT2(0), INT2(0) )
         END IF
         IF ( BUFSTR(1:1) .NE. '*' ) THEN
              READ ( BUFSTR(1:8), '(4A2)',IOSTAT=IERR ) (JSITE(I,J),I=1,4)
              IF ( IERR .NE. 0 ) CALL FERR ( INT2(IERR), "1. Reading plod file "// &
     &             FINAM, INT2(0), INT2(0) )
              CALL SPLITSTRING ( BUFSTR(9:), TOKEN, BUFSTR )
              READ ( TOKEN, *, IOSTAT=IERR ) ATMIN(J)
              IF ( IERR .NE. 0 ) CALL FERR ( INT2(IERR), "2. Reading plod file "// &
     &             FINAM, INT2(0), INT2(0) )
              CALL SPLITSTRING ( BUFSTR, TOKEN, BUFSTR )
              READ ( TOKEN, *, IOSTAT=IERR ) AVPRES(J)
              IF ( IERR .NE. 0 ) CALL FERR ( INT2(IERR), "3. Reading plod file "// &
     &             FINAM, INT2(0), INT2(0) )
              NUMSITES = NUMSITES+1
              IF ( EQUAL ( JSITE(1,J), INT2(1), 8HDEFAULT , INT2(1), INT2(8))) &
     &              THEN
                   DEFATM=ATMIN(J)
                   DEFPRES=AVPRES(J)
              ENDIF
              J = J+1
         ENDIF
      END DO
10    CONTINUE
      CLOSE ( 64, IOSTAT=IERR )
      CALL FERR ( INT2(IERR), "PLOD_CORR: Closing "//FINAM, INT2(0), INT2(0) )
!
      DO K = 1,NUMSTA
!
! ------ Get pressure dependence for each station in experiment
! ------ from list of input values
!
         SITPLD(K) = DEFATM
         REFPRES(K) = DEFPRES
         DO J=1,NUMSITES
            IF ( ( JSITE(1,J) .EQ. ISITN(1,K) ) .AND. &
     &           ( JSITE(2,J) .EQ. ISITN(2,K) ) .AND. &
     &           ( JSITE(3,J) .EQ. ISITN(3,k) ) .AND. &
     &           ( JSITE(4,J) .EQ. ISITN(4,k) )       ) THEN
                   SITPLD(K) = ATMIN(J)
                   REFPRES(K) = AVPRES(J)
            END IF
         END DO
      END DO
      KPLODCAL = .TRUE.
!
      RETURN
      END  !#!  PLOD_CORR  #!#
