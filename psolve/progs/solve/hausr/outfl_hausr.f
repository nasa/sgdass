      SUBROUTINE OUTFL_HAUSR ( TTD )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OUTFL_HAUSR PROGRAM SPECIFICATION
!
! 1.1 Open or close outfile
!
! 1.2 REFERENCES:
!
! 2.  OUTFL_HAUSR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) TTD
!
! TTD -  'O' : open
!        'C' : close
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: hausr
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 TTD_IDX, I
      INTEGER*4 IOS
      CHARACTER OUT_FILE*63
!
! 4.  HISTORY
!  WHO  WHEN   WHAT
!
! 5.  OUTFL_HAUSR PROGRAM STRUCTURE
!
      I=1
      TTD_IDX=LEN(TTD)
      IF(TTD_IDX) 20,20,10
 10   CONTINUE
      IF ( TTD(I:I) .EQ. 'O' ) THEN
           OUT_FILE=PRE_SCR_DIR(1:PRE_SD_LEN)//'PRGF'//PRE_LETRS
           CALL  FTN_OPEN( INT2(23), OUT_FILE, 'A' )
         ELSE IF(TTD(I:I).EQ.'C') THEN
           ENDFILE ( 23 )
           CLOSE ( 23, IOSTAT=IOS )
           CALL FERR ( INT2(IOS), ' Closing '//OUT_FILE, INT2(0), INT2(0) )
      ENDIF
      I=I+1
      IF(I-TTD_IDX) 10,10,20
 20   CONTINUE
      RETURN
      END  !#!  OUTFL_HAUSR  #!#
