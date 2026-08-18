      SUBROUTINE MDFIL ( ISITN, VSITEC, NUMSTA, IX, IY, LBUF_LEN, LBUF, &
     &                   IPTR, PAGEWID )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  MDFIL PROGRAM SPECIFICATION
!
! 1.1 Write adjusted site positions to user's mod file.
!
! 1.2 REFERENCES:
!
! 2.  MDFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISITN(4,*),NUMSTA,IX,IY
      REAL*8    VSITEC(3,*)
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID
      CHARACTER   LBUF(LBUF_LEN)*120
!
! IX,IY - Current cursor position
! ISITN - Site names
! NUMSTA - Number of sites
! VSITEC - Adjusted site positions
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: a3jst
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*(NAME_SIZE) FNAME
      INTEGER*2 I,J,LETRS
      INTEGER*4  IOS
!
! FNAME - Name of mod file
! I,J - Loop inices
! IOS - IOSTAT return from OPEN
! LETRS - User ID letters
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   1999.05.28. Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                     eliminated common block adj_buf
!
! 5.  MDFIL PROGRAM STRUCTURE
!
! Construct file name and open the file
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'*MOD'//PRE_LETRS
      OPEN(88,FILE=FNAME,IOSTAT=IOS,ERR=700)
      CALL FERR ( INT2(IOS), "Opening site mod file", INT2(0), INT2(0) )
!
      DO 500 I = 1,NUMSTA
!
! Write out the adjusted site positions
!
        WRITE(88,9001, &
     &    IOSTAT=ios)(ISITN(J,I),J=1,4),(VSITEC(J,I),J=1,3)
        CALL FERR ( INT2(IOS), "Opening site mod file", INT2(0), INT2(0) )
 9001   FORMAT("  ",2X,4A2,3(1X,F15.5))
!
  500   CONTINUE
!
        WRITE(88,9000,IOSTAT=ios)
        CALL FERR ( INT2(IOS), "Opening site mod file", INT2(0), INT2(0) )
 9000 FORMAT("//")
        CLOSE(88,IOSTAT=ios)
        CALL FERR ( INT2(IOS), "Opening site mod file", INT2(0), INT2(0) )
!
        GOTO 1000
!
! We arrive here if there was an error opening the mod file
!
  700   CONTINUE
        iptr=iptr+1
        WRITE ( lbuf(iptr), 9100 ) LETRS, IOS
        call addstr_f(lbuf(iptr)(:pagewid) )
        call nl_mn()
 9100   FORMAT("ADJST - ERROR OPENING *MOD",A2," = ",I4)
!
 1000 RETURN
      END
