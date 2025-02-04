      SUBROUTINE GSOUC ( LSONAM, NSTAR, SUBRD, LNAME, KFBDSP )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GSOUC PROGRAM SPECIFICATION
!
! 1.1 Read the source position substitution file.
!
! 1.2 REFERENCES:
!
! 2.   GSOUC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) LNAME
      LOGICAL*2 KFBDSP
!
! KFBDSP - True if flyby info is to be displayed
! LNAME - Name of the source mapping file
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 NSTAR, LSONAM(4,*)
      REAL*8    SUBRD(2,*)
!
! LSONAM - Array of source names
! NSTAR - Number of source positions read from mapping file
! SUBRD - Source positions read from mapping file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 JNAM(3),INAM(4)
      INTEGER*4 IOS
      INTEGER*2 I,ISIGN,IDCDG,IDCMN,IRMN,IRHR,ITEST,JJ
      REAL*8    RASC, DECSC, TWOPI
      LOGICAL*2 KBIT
      CHARACTER CBUF*80, BUFSTR*200, ERRSTR*200
      DATA TWOPI /.6283185307179587D+1/
!
! 4.  HISTORY
!   WHO  WHEN   WHAT
!   JWR 881109  Added status info to OPEN
!   AEE 910515  Enhanced error messages written to the error file.
!   BA  980225  Allowing for leading negative sign at 0 degrees
!               declination.
!   BA  980302  Allowing for negative sign _anywhere_ in right
!               ascension or declination fields.
!
! 5.  GSOUC PROGRAM STRUCTURE
!
 910  CONTINUE
      OPEN ( 40, FILE=LNAME, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( ERRSTR, &
     &        "('(GSOUC) Failure in opening source mapping file ', A) " ) LNAME
           CALL FERR ( INT2(188), ERRSTR, INT2(0), INT2(0) )
           GOTO 910
      ENDIF
!
! --- Display flyby information if appropriate
!
      IF ( KFBDSP )THEN
           IF ( KSPOOL ) WRITE(23,5514) LNAME
           IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
                WRITE  ( BUFSTR, 5514 ) LNAME
 5514           FORMAT ( " Alternate source coordinates from file ", A )
                CALL ADDSTR_F ( BUFSTR(1:79) )
                CALL NL_MN()
           ENDIF
      END IF
!
! --- Get the list of source positions.
!
      NSTAR = 0
      CBUF  = ' '
      DO WHILE (CBUF(1:1).NE.'//')
!
! ------ Reading in the source positions
!
         READ ( 40, '(A)', END=9590, IOSTAT=IOS ) CBUF
         CALL FERR ( INT2(IOS), "(GSOUC) Error in reading source mod file", INT2(0), &
     &        INT2(0) )
         IF  ( .NOT. (CBUF(1:2).EQ.'//' .OR. CBUF(1:1).EQ.'$' ) ) THEN
               NSTAR = NSTAR+1
               IF ( NSTAR .GT. MAX_SRC ) THEN
!
! ----------------- Error exit because of too many sources
!
                    WRITE ( ERRSTR, 117 ) MAX_SRC, LNAME
 117                FORMAT ( "(GSOUC) More than ",I7," substitute sources ", &
     &                       "in file ",A, " Terminating  substitution" )
                    CALL FERR ( INT2(189), ERRSTR, INT2(0), INT2(0) )
               END IF ! Error exit because of too many sources
!
               READ ( CBUF, 102, IOSTAT=IOS ) ITEST, INAM, IRHR, IRMN, RASC, &
     &                IDCDG, IDCMN, DECSC
               IF ( IOS .NE. 0 ) THEN
                    WRITE ( 6, '(A)' ) CBUF
                    CALL FERR ( INT2(IOS), "(GSOUC) Error in reading source "// &
     &                  "mod file "//LNAME, INT2(0), INT2(0) )
               END IF
  102          FORMAT ( A2, 2X, 4A2, 1X, I3, I3, D12.5, 3X, I3, I3, D12.4 )
               IF ( INDEX ( CBUF(14:31), '-' ) .NE. 0 ) THEN
                    ISIGN = -1
                  ELSE
                    ISIGN = 1
               END IF
               IRHR = ABS(IRHR)
               IRMN = ABS(IRMN)
               RASC = ABS(RASC)
               SUBRD(1,NSTAR) = (IRHR/24.D0 + IRMN/1440.D0 + RASC/86400.D0)* &
     &                           TWOPI*ISIGN
               IF ( SUBRD(1,NSTAR) .LT. 0.D0 ) THEN
                    SUBRD(1,NSTAR)=SUBRD(1,NSTAR)+TWOPI
               END IF
!
               IF ( INDEX ( CBUF(35:52), '-' ) .NE. 0 ) THEN
                    ISIGN = -1
                  ELSE
                    ISIGN = 1
               END IF
               IDCDG = ABS(IDCDG)
               IDCMN = ABS(IDCMN)
               DECSC = ABS(DECSC)
               SUBRD(2,NSTAR)=(((DECSC/60.D0+IDCMN)/60.D0+IDCDG)/360.D0)* &
     &                        TWOPI*ISIGN
               DO I=1,4
                  LSONAM(I,NSTAR) = INAM(I)
               END DO
         END IF  ! Good source card found
      END DO  ! Reading the source positions
!
! --- Close the substitute file.
!
 9590 CONTINUE
      CLOSE ( UNIT=40 )
      RETURN
      END  !#!  GSOUC  #!#
