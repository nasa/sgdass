      SUBROUTINE GVELP ( LSINAM, NSITE, VSUBXYZ, VELOCITY_FILE_NAME, &
     &                   KFBDSP, TIME0X )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GVELP PROGRAM SPECIFICATION
!
! 1.1 Read the velocity position substitution file.
!
! 1.2 REFERENCES:
!
! 2.  GVELP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      LOGICAL*2  KFBDSP
      CHARACTER  VELOCITY_FILE_NAME*(*), LOCAL_NAME*60 
!
! KFBDSP - True if flyby info is to be displayed
! LNAME - Name of the station velocity mapping file
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 LSINAM(4,*),NSITE
      REAL*8    VSUBXYZ(3,*), TIME0X
!
! LSINAM - Array of station names
! NSITE - Number of station positions read from mapping file
! VSUBXYZ - Station velocity positions read from mapping file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: svelp
!       CALLED SUBROUTINES: none
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 INAM(4), I, ITEST, YR, MO, DAY
      INTEGER*4 IOS
      LOGICAL*2 KBIT
      CHARACTER CBUF*80, BUFSTR*80, ERRSTR*255
      REAL*8    FJLDY
!
! 4.  HISTORY
!   WHO   WHEN      WHAT
!   AEE   910314    First version
!   AEE   910515    Enhanced error messages written to the error file.
!   pet   2000.05.08 Enhanced error messages and imroved comments
!   pet   2000.05.10 Added support of YYYY.DD.MM format in the firsst line of
!                    the velocity substitution file
!   pet   2000.11.28 Fixed a bug in error message.
!
! 5.  GVELP PROGRAM STRUCTURE
!
      EXTERNAL   I_LEN
      INTEGER*4  I_LEN
1     CONTINUE
      OPEN ( 40, FILE=VELOCITY_FILE_NAME(1:I_LEN(VELOCITY_FILE_NAME)), &
     &           IOSTAT=IOS, STATUS='OLD' )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) ' IOS=',IOS
           ERRSTR = 'GVELP: Failure to open velocity mapping file '// &
     &               VELOCITY_FILE_NAME
           CALL FERR ( INT2(192), ERRSTR(1:I_LEN(ERRSTR)), INT2(0), INT2(0) )
           STOP 'GVELP: Abnormal termination'
      ENDIF
!
! --- Display station info if appropriate
!
      IF ( KFBDSP ) THEN
           IF ( KSPOOL ) WRITE ( 23, 5514 ) LOCAL_NAME
           IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
                WRITE ( BUFSTR, 5514 ) LOCAL_NAME
                CALL ADDSTR_F ( BUFSTR )
                CALL NL_MN()
           ENDIF
 5514      FORMAT(   " Alternate velocity coordinates from file ",A )
      END IF
!
! --- Read in the substitute site velocity position list.
!
      NSITE = 0
      CBUF=' '
      READ ( 40, '(A)', END=9590, IOSTAT=IOS ) CBUF
      CALL FERR ( INT2(IOS), "gvelp(1): Reading velocity mod file", INT2(0), &
     &     INT2(0) )
      DO WHILE ( CBUF(1:1) .EQ. '$' )
         READ ( 40, '(A)', END=9590, IOSTAT=IOS ) CBUF
         CALL FERR ( INT2(IOS), "gvelp(2): Reading velocity mod file", INT2(0), &
     &        INT2(0) )
      ENDDO
!
! --- First try yymmdd format
!
      READ ( CBUF, '(3I2)', IOSTAT=IOS ) YR, MO, DAY
      IF ( IOS .NE. 0 ) THEN
!
! -------- Then try YYYY.MM.DD format
!
           READ ( CBUF, '(I4,1X,I2,1X,I2)', IOSTAT=IOS ) YR, MO, DAY
      END IF
      IF ( IOS .NE. 0 ) THEN
           CALL FERR ( INT2(IOS), 'gvelp(3): Reading the first line '// &
     &         ' of velocity mod file: '//VELOCITY_FILE_NAME// &
     &         ' YYYY.MM.DD data was expected but the line is >'//CBUF// &
     &         '< was found instead of', INT2(0), INT2(0) )
      END IF
      TIME0X = FJLDY ( MO, DAY, YR )/YEAR__TO__DAY
      DO WHILE (CBUF(1:2).NE.'//')
!
! ------ Reading in the sites
!
         READ ( 40, '(A)', END=9590, IOSTAT=IOS ) CBUF
         CALL FERR ( INT2(IOS), "gvelp(4): Reading velocity mod file", INT2(0), &
     &        INT2(0) )
         IF ( .NOT. (CBUF(1:2).EQ.'//' .OR. CBUF(1:1).EQ.'$') ) THEN
              NSITE = NSITE+1
              IF ( NSITE .GT. MAX_STA ) THEN ! error exit
                   WRITE ( ERRSTR, 114 )  MAX_STA
 114               FORMAT ( "GVELP: more than ",I7," substitute sites. ", &
     &                    "Terminating substitution.")
                   CALL FERR ( INT2(193), ERRSTR, INT2(0), INT2(0) )
              END IF ! error exit
              READ ( CBUF, 106, IOSTAT=IOS) ITEST, INAM, &
     &               (VSUBXYZ(I,NSITE),I=1,3)
              IF ( IOS .NE. 0 ) THEN 
                   CALL FERR ( INT2(IOS), "gvelp(5): Reading velocity "// &
     &                 "mod file. Line: "//CBUF, INT2(0), INT2(0) )
              END IF
  106         FORMAT(A2,2X,4A2,1X,D15.11,1X,D15.11,1X,D15.11)
              DO  I=1,4
                 LSINAM(I,NSITE) = INAM(I)
              END DO
        END IF  !good site card found
      END DO  !reading in the sites
!
! --- CLOSE THE SUBSTITUTE FILE.
!
 9590 CONTINUE
      CLOSE ( 40 )
      RETURN
      END  !#!  GVELP  #!#
