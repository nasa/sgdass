      SUBROUTINE OUTFL_BACK (TTD, RSTRT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OUTFL_BACK PROGRAM SPECIFICATION
!
! 1.1 Open or close a file
!
! 1.2 REFERENCES:
!
! 2.  OUTFL_BACK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) TTD
      INTEGER*4  IOS
      LOGICAL*2 RSTRT
!
! RSTRT - True if we are restarting
! TTD - O for open and C for close
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: cvrnc
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 TTD_IDX,I,IERR
      INTEGER*4 POS_BEG,POS_END
      CHARACTER*(NAME_SIZE) OUT_FILE
      SAVE POS_BEG,POS_END
!
! 4.  HISTORY
!   WHO   WHEN  WHAT
!
! 5.  OUTFL_BACK PROGRAM STRUCTURE
!
      OUT_FILE=PRE_SCR_DIR(1:PRE_SD_LEN)//'CVRF'//PRE_LETRS
      I=1
      TTD_IDX=LEN(TTD)
      IF(TTD_IDX) 20,20,10
 10   CONTINUE
      IF(TTD(I:I).EQ.'O') THEN
       IF(RSTRT) THEN
         CALL FTN_OPEN( INT2(88), OUT_FILE, ' ' )
         ENDFILE 88
         REWIND 88
       ELSE
        CALL FTN_OPEN( INT2(88), OUT_FILE, 'A' )
       ENDIF
      ELSE IF(TTD(I:I).EQ.'C') THEN
       ENDFILE 88
       CLOSE ( 88, IOSTAT=IOS )
       CALL FERR ( INT2(IOS), ' Closing '//OUT_FILE, INT2(0), INT2(0) )
      ENDIF
      I=I+1
      IF(I-TTD_IDX) 10,10,20
 20   CONTINUE
!
      RETURN
      END  !#!  OUTFL_BACK  #!#
