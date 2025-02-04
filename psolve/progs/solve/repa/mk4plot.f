      SUBROUTINE MK4_PLOT ( FRINGE_FILE, SOLVE_PS_VIEWER, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MK4_PLOT extracts the fringe plot from the Mark4 fringe    *
! *   file FRINGE_FILE, decodes it and invokes the viewer                *
! *   SOLVE_PS_VIEWER.                                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     FRINGE_FILE ( CHARACTER ) -- Name of the fringe file.            *
! * SOLVE_PS_VIEWER ( CHARACTER ) -- Name of the program which displays  *
! *                                  the file. The viewer should support *
! *                                  the first argument "-" and read     *
! *                                  the postscript file from the pipe   *
! *                                  (standard input).                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *   
! *  ### 28-JAN-2005    MK4_PLOT   v1.0 (c)  L. Petrov  28-JAN-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IUER
      CHARACTER  SOLVE_PS_VIEWER*(*), FRINGE_FILE*(*)
      INTEGER*4  MRCT, MCP_RATIO
      PARAMETER  ( MRCT = 13, MCP_RATIO = 32 )
      CHARACTER  WORK*32768, STR*32, STR1*128
      CHARACTER, ALLOCATABLE :: BUF_FILE(:)*1, BUF_PLOT(:)*1
      LOGICAL*4  LEX
      INTEGER*4  SIZE_FILE, SIZE_PLOT, IC, LUN, ISH, IP, IR, IPL, ILN, IOS, &
     &           REC_SIZE, ITYPE, IVERSION, UNIX_DATE, J1, IS, PID, PIP(0:1)
      INTEGER*8  SIZE_I8
      INTEGER*4, EXTERNAL :: COMPRESS_DECOMPRESS, EXECL, FILE_INFO, FORK, &
     &           I_LEN, ILEN, PIPE, READ, SIZE_DFIO, WRITE 
!
! --- Learn whether the fringe plot file exists
!
      INQUIRE ( FILE=FRINGE_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2541, IUER, 'MK4_PLOT', 'File '// &
     &                    FRINGE_FILE(1:I_LEN(FRINGE_FILE))//' was not found' )
           RETURN 
      END IF
!
! --- Learn the size of the fringe plot file
!
      IS = FILE_INFO ( FRINGE_FILE(1:I_LEN(FRINGE_FILE))//CHAR(0), UNIX_DATE, &
                       SIZE_I8 )
      IF ( SIZE_I8 .LE. 0 ) THEN
           CALL ERR_LOG ( 2542, IUER, 'MK4_PLOT', 'File has zero length' )
           RETURN 
      END IF
      SIZE_FILE = SIZE_I8
      SIZE_PLOT = MCP_RATIO*SIZE_FILE
!
! --- Allocate memory for buffers
!
      ALLOCATE ( BUF_FILE(SIZE_FILE), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL IINCH ( SIZE_FILE, STR )
           CALL ERR_LOG ( 2543, IUER, 'MK4_PLOT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
      ALLOCATE ( BUF_PLOT(SIZE_PLOT), STAT=IOS  )
      IF ( IOS .NE. 0 ) THEN
           CALL IINCH ( SIZE_PLOT, STR )
           CALL ERR_LOG ( 2544, IUER, 'MK4_PLOT', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN 
      END IF
!
! --- Open the fringe plot file
!
      IUER = -1
      CALL BINF_OPEN ( FRINGE_FILE, 'OLD', LUN, IUER )
!
! --- Read its contents to the BIF_FILE buffer
!
      IUER = -1
      IC = READ ( %VAL(LUN), %REF(BUF_FILE), %VAL(SIZE_FILE) )
!
      CALL BINF_CLOSE ( LUN, IUER )
      IF ( IC .NE. SIZE_FILE ) THEN
           WRITE ( 6, * ) ' IC=',IC,' SIZE_FILE=',SIZE_FILE
           CALL ERR_LOG ( 2543, IUER, 'MK4_PLOT', 'Error in reading file '// &
     &                    FRINGE_FILE )
           DEALLOCATE ( BUF_FILE )
           DEALLOCATE ( BUF_PLOT )
           RETURN
      END IF
!
! --- Now scan the file and try to find the record 221 with fringe plot
!
      ISH=0
      DO 410 J1=1,SIZE_FILE
!
! ------ Get the first 5 characters of the file
!
         CALL LIB$MOVC3 ( 5, %REF(BUF_FILE(ISH+1)), STR )
         CALL CHIN ( STR(1:3), ITYPE    )
         CALL CHIN ( STR(4:5), IVERSION )
!
! ------ Determine the record size
!
!???         REC_SIZE = SIZE_DFIO ( ITYPE, IVERSION )
         rec_size = -1
!
         IF ( REC_SIZE .LE. 0 ) THEN
              WRITE ( 6, * ) ' j1=',j1,' ish=', ish, ' itype=',itype, &
     &               ' iversion=',iversion,' rec_size=',rec_size
              CALL ERR_LOG ( 2544, IUER, 'MK4_PLOT', 'Zero REC_SIZE' )
              DEALLOCATE ( BUF_FILE )
              DEALLOCATE ( BUF_PLOT )
              RETURN 
         END IF
         IF ( STR(1:3) .EQ. '221' ) THEN
!
! ----------- Ura! We have found the record which keeps comressed finge plot.
! ----------- Lets learn the size ofthe compressed fringe plot (ILN)
!
              CALL LIB$MOVC3 ( 4, %REF(BUF_FILE(ISH+9:ISH+12)), ILN )
#ifdef LITTLE_ENDIAN
              CALL ENDIAN_CNV_I4 ( ILN )
#endif
!
! ----------- Decompress the firnge file and write it in BUF_PLOT
!
!@              IR = COMPRESS_DECOMPRESS ( %REF(WORK), %REF(BUF_FILE(ISH+13:)), &
!@     &                                   %VAL(ILN), %REF(BUF_PLOT), IPL )
              WRITE ( 6, * ) 'Mark-4 plots are not supported any more'
              call exit ( 6 )
!
! ----------- Peek the first 23 and the last 5 bytes
!
              CALL LIB$MOVC3 ( 23, %REF(BUF_PLOT(1)),     STR )
              CALL LIB$MOVC3 (  5, %REF(BUF_PLOT(IPL-5)), STR1 )
!
! ----------- Check whether it is a valid PostScript file or garbge
!
              IF ( STR(1:23) .EQ. '%!PS-Adobe-3.0 EPSF-3.0'  .AND. &
     &             STR1(1:5) .EQ. '%%EOF' ) THEN
!
! ---------------- Make a pipe
!
                   IS = PIPE ( PIP )
!
! ---------------- Fork the porcess
!
                   PID = FORK()
                   IF ( PID == 0 ) THEN
!
! ===================== This is the child context
!
!
! --------------------- Close the stadnard input
!
                        CALL CLOSE ( %VAL(0) )
!
! --------------------- Duplive the standard input file descriptor
!
                        CALL DUP   ( %VAL(PIP(0)) ) 
!
! --------------------- Close both end of the pipe
!
                        CALL CLOSE ( %VAL(PIP(0)) )
                        CALL CLOSE ( %VAL(PIP(1)) )
!
! --------------------- Spawn the PostScropt viewer process
!
                        IS  = EXECL ( %REF('/bin/csh'//CHAR(0)), &
     &                                %REF('csh'//CHAR(0)), &
     &                                %REF('-c'//CHAR(0)), &
!@     &                                %REF('/mk5/bin/GhosT -'//CHAR(0)), &
     &                                %REF( SOLVE_PS_VIEWER(1:I_LEN(SOLVE_PS_VIEWER))//' -'//CHAR(0) ), &
     &                                %VAL(0) )
                        CALL EXIT ( 0 )
                   ENDIF
!
! ================ This is parent process context
!
! ---------------- Close the read end of the pipe
!
!
                   CALL CLOSE ( %VAL(PIP(0)) )
!
! ---------------- Write the uncompressed fringe plot to the viewer process
!
                   IS = WRITE ( %VAL(PIP(1)), BUF_PLOT, %VAL(IPL) )
!
! ---------------- Close the write end of thepipe
!
                   CALL CLOSE ( %VAL(PIP(1)) )
!
! ---------------- Wait fopr finishing the child process
!
                   CALL WAITPID ( %VAL(PID), %VAL(0), %VAL(0) )
!
! ---------------- Dealloacate buffers and return
!
                   DEALLOCATE ( BUF_FILE )
                   DEALLOCATE ( BUF_PLOT )
                   CALL ERR_LOG ( 0, IUER )
                   RETURN 
                ELSE
                   CALL ERR_LOG ( 2545, IUER, 'MK4_PLOT', 'Error in '// &
     &                 'decoding the fringe plot' )
                   DEALLOCATE ( BUF_FILE )
                   DEALLOCATE ( BUF_PLOT )
                   RETURN 
              END IF
         END IF
         ISH = ISH + REC_SIZE
 410  CONTINUE
!
      DEALLOCATE ( BUF_FILE )
      DEALLOCATE ( BUF_PLOT )
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  MK4_PLOT 
!
! ------------------------------------------------------------------------
!
      FUNCTION GET_MK4DATE ( DAT )
! ************************************************************************
! *                                                                      *
! *   Routine GET_MK4DATE transtorms the date from MK4 representation to *
! *   Solve format.                                                      *
! *                                                                      *
! *  ### 28-JAN-2005  GET_MK4DATE  v1.0 (c)  L. Petrov  28-JAN-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  GET_MK4DATE*26
      INTEGER*1  DAT(12)
!
      INTEGER*2 IYEA, IDAY, IHOU, IMIN
      CHARACTER DATE_CHR*26, JD_TO_DATE*26
      REAL*4    RSEC
      INTEGER*4 MJD, J1
      REAL*8    SEC, JD
!
      CALL LIB$MOVC3 ( 2, DAT(1), IYEA )
      CALL LIB$MOVC3 ( 2, DAT(3), IDAY )
      CALL LIB$MOVC3 ( 2, DAT(5), IHOU )
      CALL LIB$MOVC3 ( 2, DAT(7), IMIN )
      CALL LIB$MOVC3 ( 4, DAT(9), RSEC )
!
      WRITE ( UNIT=DATE_CHR, FMT='(I4,".",I2,".",I2,"-",I2,":",I2,":",F9.6)' ) &
     &        IYEA, 1, 1, IHOU, IMIN, RSEC
      CALL BLANK_TO_ZERO ( DATE_CHR )
      CALL DATE_TO_TIME  ( DATE_CHR, MJD, SEC, -3 )
      JD = ( 2400000.5D0 + ( SEC + 1.D-3 )/86400.0D0 ) + MJD + IDAY - 1
      GET_MK4DATE = JD_TO_DATE ( JD, -3 )
      GET_MK4DATE(21:26) = DATE_CHR(21:26)
!
! --- This cycle
!
      DO 410 J1=26,22,-1
         IF ( GET_MK4DATE(J1:J1) .EQ. '0' ) THEN
              GET_MK4DATE(J1:J1) = ' '
            ELSE
              GOTO 810
         END IF
 410  CONTINUE
 810  CONTINUE
      RETURN
      END  !#!  GET_MK4DATE  #!#
