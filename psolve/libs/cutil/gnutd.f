      SUBROUTINE GNUTD ( JDATE_FIRST, DNUT, DDPSI, DDEPS, LNAME, KFBDSP, LNSIG, &
     &                   OBSIG, LNOBCOR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GNUTD PROGRAM SPECIFICATION
!
! 1.1 Reads a nutation 'daily offset' values file.
!
! 1.2 REFERENCES:
!
! 2.  GNUTD INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      LOGICAL*2 KFBDSP
      REAL*8    JDATE_FIRST
      CHARACTER*(NAME_SIZE) LNAME
      INTEGER*2  TRIMLEN
!
! KFBDSP      - True if flyby info is to be displayed
! LNAME       - Name of nutation daily mapping file
! JDATE_FIRST - Julian date of first atmosphere epoch
!
! 2.3 OUTPUT Variables:
!
      REAL*8 DNUT(5),DDPSI(5),DDEPS(5),lnsig(5),obsig(5),lnobcor(5)
!
! DDEPS - Obliquity offset
! DDPSI - Longitude offset
! DNUT - Julian date
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
      REAL*8 TWOPI,CONV,FSTJD,INC,LGTSM,OBQSM,XC
      logical*2 kbit
      INTEGER*2 NUM, I, IDAY
      INTEGER*4  IOS
      CHARACTER    ERRSTR*255, BUFSTR*80
      PARAMETER  ( TWOPI = 2.0D0*PI__NUM )
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   jwr  881109  Added status info to OPEN
!   AEE  910515  Enhanced error messages written to the error file.
!   PET  980807  Improved error messages
!
! 5.  GNUTD PROGRAM STRUCTURE
!
!
1     CONTINUE
      OPEN ( 40, IOSTAT=IOS, FILE=LNAME, ACCESS='DIRECT', STATUS='OLD', &
     &       FORM='FORMATTED', RECL=75 )
      IF ( IOS.NE.0) THEN
           WRITE ( ERRSTR, "('Failure in opening nutation daily mapping file', &
     &                        A)" ) LNAME
           CALL FERR ( INT2(184), ERRSTR, INT2(0), INT2(0) )
          GOTO 1
      ENDIF
!
      IF ( KFBDSP ) THEN
           IF ( KSPOOL ) WRITE(23,5514) LNAME
           IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
                WRITE ( BUFSTR ,5514) LNAME
                CALL ADDSTR_F ( BUFSTR )
                CALL NL_MN()
          ENDIF
 5514     FORMAT ( " Nutation daily offset values from file ",A )
      END IF
!
! --- Get the list of nutation daily offsets
!
      READ ( 40, FMT=102, REC=1, IOSTAT=IOS ) FSTJD, INC, NUM
      IF ( IOS .NE. 0 ) GOTO 900
 102  FORMAT ( F9.1, 1X, F4.1, 1X, I5 )
      IDAY=((JDATE_FIRST-FSTJD)/INC)+2
      IF ( JDATE_FIRST .LT. FSTJD .OR. IDAY+3 .GT.NUM ) GOTO 1000
      DO I=1,5
         READ ( 40, FMT=103, REC=IDAY, IOSTAT=IOS ) DNUT(I), DDPSI(I), &
     &         LNSIG(I), DDEPS(I), OBSIG(I), LNOBCOR(I)
         IF ( IOS .NE. 0 ) GOTO 900
         IDAY = IDAY+1
 103     FORMAT(F9.1,1X,5(F9.3,1X))
      ENDDO
!
! --- CHANGE UNITS FORM 0.001 ARCSEC TO RADIAN
!
      CONV=(360.D0/TWOPI)*3600.D0*1000.D0
      DO I=1,5
          DDPSI(I)=DDPSI(I)/CONV
          DDEPS(I)=DDEPS(I)/CONV
          LNSIG(i)=LNSIG(I)/CONV
          OBSIG(i)=OBSIG(I)/CONV
      ENDDO
!
! --- CLOSE THE SUBSTITUTE FILE.
!
      CLOSE(40)
      RETURN
!
! --- Handle reading error here
!
 900  CONTINUE
      CALL FERR ( INT2(IOS), "Reading nutation mod file "//LNAME, INT2(0), &
     &            INT2(0) )
      CLOSE ( 40 )
      RETURN
!
! *** TRAP EOF CONDITION
!
 1000 CONTINUE
 1001 FORMAT ( 'Processing', a63 )
      WRITE  ( ERRSTR, 1003)  JDATE_FIRST, LNAME(1:TRIMLEN(LNAME)), &
     &         FSTJD, FSTJD+INC*FLOAT(NUM)
 1003 FORMAT ( ' Date ',F12.4,' is out of range of nutation daily offset file ', &
     &         A, ' : [', F10.2, ', ', F10.2, ']' )
      CALL FERR ( INT2(185), ERRSTR, INT2(0), INT2(0) )
!!      PAUSE 'GNUTD IS PAUSED PLEASE CORRECT DATA'
      CLOSE ( 40 )
      RETURN
      END  !#!  GNUTD  #!#
