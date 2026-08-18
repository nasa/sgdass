      SUBROUTINE DB2SFNM ( KEY, VER, FILE_NAME )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DB2SFNM PROGRAM SPECIFICATION
!
! 1.1 Construct a superfile name for the specified database
!
! 1.2 REFERENCES:
!
! 2.  DB2SFNM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 VER
      CHARACTER KEY*(*)
!
! KEY - Experiment key: xYYMMDDab, leading non-numeric x optional
! VER - Database version number
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) FILE_NAME
!
! FILE_NAME - Superfile name: YYMMDDab_Vvvv
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*6 CVER,INTTODECIMAL
      INTEGER*2 IL,TRIMLEN,IST
!
! CVER - Character representation of version number
! IL - String lengths for checking formats
! IST - Position of first numeric character of key
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   pet   2000.09.06  Corrected a bug: it didn't work correctly when the
!                     key was an empty line
!
! 5.  DB2SFNM PROGRAM STRUCTURE
!
      FILE_NAME='YYMMMDD___S000'
      IL = TRIMLEN ( KEY(1:9) )
      IF ( IL .EQ. 0 ) THEN
           CALL FERR ( INT2(154), 'DB2FILE: database file name is empty', &
     &          INT2(0), INT2(0) )
      END IF
!
      IST=1
      IF ( INDEX ( '0123456789', KEY(1:1) ) .EQ. 0 ) IST=2
      IF ( IST+8 .LE. LEN(KEY) ) THEN
           IL = TRIMLEN ( KEY ( IST:IST+8) )
         ELSE IF ( LEN(KEY) .GE. IST ) THEN
           IL = TRIMLEN ( KEY ( IST:LEN(KEY) ) )
         ELSE
           IL = 0
      END IF
      IF ( IL .LT. 7 ) THEN
           CALL FERR ( INT2(155), &
     &         'DB2FILE: incorrect format in database name: '//KEY, INT2(0), &
     &          INT2(0) )
           STOP 'GTSUP(db2sfnm)  Abnormal termination'
      END IF
      FILE_NAME(1:IL)=KEY(IST:IST+IL-1)
!
      CVER=INTTODECIMAL(VER)
      IL=TRIMLEN(CVER)
      IF ( IL .GT. 3 ) THEN
           WRITE ( 6, * ) 'DB2SFNM: version number too high (more than 99): '// &
     &                     CVER
           IL = -1
      END IF
      FILE_NAME(15-IL:14)=CVER(1:IL)
!
      RETURN
      END  !#!  DB2SFNM  #!#
