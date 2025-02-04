      SUBROUTINE GSTAP ( LSINAM, NSITE, SUBXYZ, LNAME, KFBDSP )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GSTAP PROGRAM SPECIFICATION
!
! 1.1 Read the site position substitution file.
!
! 1.2 REFERENCES:
!
! 2.  GSTAP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      LOGICAL*2 KFBDSP
      CHARACTER*(*) LNAME
!
! KFBDSP - True if flyby info is to be displayed
! LNAME - Name of the site mapping file
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 LSINAM(4,*), NSITE
      REAL*8    SUBXYZ(3,*)
!
! LSINAM - Array of station names
! NSITE - Number of station positions read from mapping file
! SUBXYZ - Station positions read from mapping file
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
      INTEGER*2 I, ITEST, INAM(4)
      INTEGER*4 IOS
      LOGICAL*2 KBIT
      CHARACTER CBUF*80, BUFSTR*80, ERRSTR*133, STA_NAM*8
      EQUIVALENCE ( STA_NAM, INAM )
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  881109  Added status info to OPEN
!   AEE  910515  Enhanced error messages written to the error file.
!   BA   930712  Error messages improved.  Stop added.
!   PET  2001.09.27  Made source code readable
!
! 5.  GSTAP PROGRAM STRUCTURE
!
      OPEN ( UNIT=40, FILE=LNAME, IOSTAT=IOS, STATUS='OLD' )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) ' IOS =',IOS
           ERRSTR = 'GSTAP: Failure in opening site flyby/mod file: '//LNAME
           CALL FERR ( INT2(190), ERRSTR, INT2(0), INT2(0) )
           STOP 'Abnormal termination'
      ENDIF
!
! --- Display station info if appropriate
!
      IF ( KFBDSP ) THEN
           IF ( KSPOOL ) WRITE ( 23, '(A)' ) "Alternate station coordinates "// &
     &                                       "from file "//LNAME
           IF ( KSCREEN  .AND.  KBIT( PRE_IP(2), INT2(6)) ) THEN
                BUFSTR = "Alternate station coordinates from file "//LNAME
                CALL ADDSTR_F ( BUFSTR )
                CALL NL_MN()
           ENDIF
      END IF
!
! --- Read in the substitute site position list.
!
      NSITE = 0
      CBUF=' '
      DO WHILE ( CBUF(1:2) .NE. '//' )
!
! ------ reading in the sites
!
         READ ( UNIT=40, FMT='(A)', END=9590, IOSTAT=IOS ) CBUF
         IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) ' NSITE=', NSITE ! %%
              CALL FERR ( INT2(IOS), "GSTAP Reading station mod file", &
     &                    INT2(0), INT2(0) )
         END IF
         IF ( .NOT. ( CBUF(1:2) .EQ. '//'  .OR.  CBUF(1:1).EQ.'$') ) THEN
              NSITE = NSITE+1
              IF ( NSITE .GT. MAX_STA ) THEN ! error exit
                   WRITE ( ERRSTR, 124 )  MAX_STA
 124               FORMAT ( "GSTAP more than ",I7," substitute sites. ", &
     &                      "terminating substitution" )
                   CALL FERR ( INT2(191), ERRSTR, INT2(0), INT2(0) )
              END IF  ! error exit
              READ ( UNIT=CBUF, FMT=106, IOSTAT=IOS ) ITEST, INAM, &
     &              ( SUBXYZ(I,NSITE), I=1,3 )
              IF ( IOS .NE. 0 ) THEN
                   CALL FERR ( INT2(IOS), "GSTAP Reading station "// &
     &                 "mod file. Line: "//CBUF, INT2(0), INT2(0) )
              END IF
!
  106         FORMAT ( A2, 2X, 4A2, D15.11, 1X, D15.11, 1X, D15.11 )
              DO I=1,4
                 LSINAM(I,NSITE) = INAM(I)
              END DO
         END IF  !good site card found
      END DO  !reading in the sites
!
! --- Close the substitute file.
!
 9590 CONTINUE
      CLOSE ( UNIT=40 )
      RETURN
      END  !#!  GSTAP  #!#
