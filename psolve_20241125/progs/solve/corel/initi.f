      SUBROUTINE INITI()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      INCLUDE 'corpar.i'
      INCLUDE 'corcom.i'
      INCLUDE 'precm.i'
!
      CHARACTER*63 INFILE,OUTFILE
      INTEGER*2 IERR
      INTEGER*4  IOS
      logical*4 kexist
!
      luop=6
      luin=24
      LUOUT=25
!
      INFILE =PRE_SCR_DIR(1:PRE_SD_LEN)//'CVRF'//PRE_LETRS
      OUTFILE=PRE_SCR_DIR(1:PRE_SD_LEN)//'CORL'//PRE_LETRS
!
      OPEN ( LUIN, FILE=INFILE, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL FERR ( INT2(IOS), 'Opening input file', INT2(0), INT2(0) )
      ENDIF
      inquire(FILE=outfile,EXIST=kexist)
      if (kexist) call bin_unlink(outfile,ios)
      OPEN ( LUOUT, FILE=OUTFILE, STATUS='NEW', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL FERR ( INT2(IOS), 'Opening output file', INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END  !#!  INITI  #!#!
