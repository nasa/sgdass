        SUBROUTINE ERR_LOG ( NERR, IUER, PROG, MES )
! ************************************************************************
! *                                                                      *
! *     Routine ERR_LOG  is a universal errors handler.                  *
! *   It the input value of IUER = -1,-2 or -3 then ERR_LOG works in     *
! *   verbose mode, otherwise it works in silent mode.                   *
! *                                                                      *
! *     If NERR=0, it means that no errors occurred. In that case        *
! *   ERR_LOG checks whether IUER is accessible for writing. It yes,     *
! *   it assigns IUER value 0 and returns.                               *
! *                                                                      *
! *     If NERR is not 0, it means that the error occurred. ERR_LOG      *
! *   formats the line with error message. If input value of IUER = -1,  *
! *   -2 or -3, it prints the error message in screen. If input value    *
! *   of is not -1, -2 or -3 then it does not print the message. Then    *
! *   ERR_LOG checks input value of IUER. If the unput value of IUER is  *
! *   not -2 and not -3, assigns the output value of IUER to NERR and    *
! *   returns. If the input value is -2, IUER is not changed. If the     *
! *   input value if IUER =-3, then if there was no error, IUER is not   *
! *   changed. But if there was an error, then upon printing the error   *
! *   message, the system unwinds stack of calls and aborts.             *
! *                                                                      *
! *     ERR_LOG formats the error message in following way:              *
! *   three dollars (hint that errors cost $), two blanks, IUER=xxxx     *
! *   (where xxxx is the value of NERR), one blank, pppppp -- value of   *
! *   argument PROG_NAME, then blank, double quote, message supplied     *
! *   by the argument MES, double quote, blank three dollar characters.  *
! *   example:                                                           *
! *                                                                      *
! *   $$$  IUER=xxxx ppppppp "mmmmmmmmmmmmmmm" $$$                       *
! *                                                                      *
! *   The portion of the error message "IUER=xxxx ppppppp" is printed    *
! *   in inverse mode and "mmmmmmmmmmmmm" message is printed by red      *
! *   color if the terminal can support inversion and colors.            *
! *                                                                      *
! *   If the total length of the formatted message exceeds the current   *
! *   terminal length it will be split by word boundaries and printed at *
! *   several line. The maximal length of the error message is 1024      *
! *   characters.                                                        *
! *                                                                      *
! *   The last error message formatted by err_log is kept in the         *
! *   internal butter and can be retrieved by ERR_PEEK.                  *
! *                                                                      *
! *   Comments:                                                          *
! *                                                                      *
! *   hpterm in HP-UX has a known bug: it crashes if it cannot allocate  *
! *   colors. It may occur if all available colors have been grabbed     *
! *   by another applications. Therefore, invoking ERR_LOG can result    *
! *   in disappearing the screen. If the environment variable SET_COLOR  *
! *   is set to "NO", then ERR_LOG will print "mmmmm" string with        *
! *   normal foreground color and hpterm will survive.                   *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT )   -- Input: mode.                         *
! *                      IUER=0  -- silent mode: to format the error     *
! *                                  message but not print it.           *
! *                      IUER=-1 -- verbose mode: to format the error    *
! *                                 message and print it in the screen   *
! *                                 Output: Error code.                  *
! *                      IUER=-2 -- Verbose mode, but IUER is not        *
! *                                 modified.                            *
! *                      IUER=-3 -- Verbose mode, but IUER is not        *
! *                                 modified, and in the case of error   *
! *                                 ( NERR is not 0 ), the processes is  *
! *                                 aborted and the stack is unwinded.   *
! *                                                                      *
! * _________________________ Input parameters:  _______________________ *
! *                                                                      *
! *     NERR ( INTEGER*4 ) -- Error code                                 *
! *     PROG ( CHARACTER, OPT ) -- Subroutine name where the error has   *
! *                                occurred.                             *
! *      MES ( CHARACTER, OPT ) -- Additional message describing the     *
! *                                error (no more than 1024 characters)  *
! *                                                                      *
! *  ###  28-AUG-1991    ERR_LOG   v5.1 (c) L. Petrov  19-SEP-2009  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NERR, IUER
      CHARACTER, OPTIONAL :: PROG*(*), MES*(*)
      CHARACTER  STR1*132, STR2*1024, OUT*1024, ERROR_BUFFER*1024
      COMMON   / ERROR__COMMON / ERROR_BUFFER
      INTEGER*4  LSCR, LOUT, IBG, IB, J1, IR, IE, IE_NEW, LR, IARR(2), IER
      LOGICAL*4  FL_OUT_TERM
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS, PROBE_WRITE_ADDRESS
#ifdef GNU
      LOGICAL*4, INTRINSIC :: ISATTY
      INTRINSIC  FLUSH
#else
      LOGICAL*4, EXTERNAL :: FUNC_ISATTY
#endif
!
      IF ( NERR .EQ. 0 ) THEN
!
! -------- Error code is zero. Check, whether it is accessible for writing.
! -------- If yes, then assign IUER value 0
!
           IF ( IUER .NE. -2  .AND.  IUER .NE. -3 ) THEN
                IUER = 0
           END IF
           RETURN
      END IF
!
! ----- Initialization
!
        CALL CLRCH ( ERROR_BUFFER )
        CALL CLRCH ( STR1 )
        CALL CLRCH ( STR2 )
!
! ----- Put there the name of the subroutine and the message itself
!
        IF ( LOC(PROG) .NE. 0 ) STR1=PROG
        IF ( LOC(MES)  .NE. 0 ) STR2=MES
!
        CALL CLRCH ( OUT )
        OUT(1:)=' IUER='
        CALL INCH ( NERR, OUT(ILEN(OUT)+1:) )
!
! ----- Prepare the error buffer string in plain ASCII format
!
        ERROR_BUFFER = OUT(1:I_LEN(OUT))//' '//STR1(1:I_LEN(STR1))//' "'// &
     &                 STR2(1:I_LEN(STR2))//'"'
!
! ----- Check whether IUER is acessible wor reading
!
        IF ( PROBE_READ_ADDRESS(IUER) ) THEN
!
! ---------- Yes, it is acessible for reading. 
! ---------- Is the input value of IUER = 0?
!
             IF ( IUER .EQ. 0 ) THEN
!
! --------------- Yes it is zero. Then return
!
                  IUER = NERR
                  RETURN
             END IF
        END IF
        WRITE ( 6, '(A,$)' ) '$$$ '
        WRITE ( 6, '(A,$)' ) OUT(1:I_LEN(OUT))
        WRITE ( 6, '(A,$)' ) ' '
!
! ----- Printout on the screen the name of the module where an error occured
!
        WRITE ( 6, '(A,$)' ) STR1(1:I_LEN(STR1))
        WRITE ( 6, '(A,$)' ) '  '
        IBG = 7 + ILEN(OUT) + ILEN(STR1) ! offset of the cusor position
        LSCR = 80
!
! ----- Forming string with message
!
        OUT='"'//STR2(1:I_LEN(STR2))//'"'
        LOUT = ILEN(OUT)  ! their length without trailing blanks
!
        IB=1
        LR=LOUT
!
! ----- Printing it on the screen
!
        DO 410 J1=1,999
           IR = LSCR - IBG  ! acceptable lenght of one line on the screen
           IE = IB+IR  ! the last position of the symbol to be print
!
! -------- Adjusting the last position (moving back if needed)
!
           IF ( IE .GE. LOUT ) THEN
                IE=LOUT
             ELSE
!
! ------------  Seek the last blank symbol on the line.
!
                IE_NEW = LINDEX ( OUT(IB:IE), ' ' ) + IB-1
                IF ( IE_NEW .GT. IB ) IE=IE_NEW  !  moving the right boundary
           END IF
           WRITE ( 6, '(A,$)' ) OUT(IB:IE) ! printing
           IB = IE + 1
           IF ( IB .GT. LOUT ) GOTO 810
           WRITE ( 6, '(A,$)' ) CHAR(10) ! New line
           IBG = 1
 410    CONTINUE
 810    CONTINUE
        WRITE ( 6, '(A,$)' ) ' $$$'
        WRITE ( 6, '(A,$)' ) CHAR(10) ! New line
!
! ----- Flashing the output device
!
        CALL FLUSH ( 6 )
!
! ----- IF IUER is accessible for writing then put there value of NERR
!
        IF ( IUER == -2 ) THEN
             CONTINUE 
           ELSE IF ( IUER == -3 ) THEN
!
! ---------- Deliberate crash in or order to cause stack unwinding neeeded for
! ---------- diagnositc
!
             IBG = 3
             IB = IARR(IBG) 
           ELSE
             IUER = NERR
        END IF
!
        RETURN
        END  SUBROUTINE  ERR_LOG  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ERR_PASS ( IUER, IER )
! ************************************************************************
! *                                                                      *
! *   Auxillary souroutine ERR_PASS assigns  velus of IER to the         *
! *   varaible IUER in the case if IUER is accessible for writing.       *
! *                                                                      *
! *  ###  28-AUG-1991   ERR_PASS   v3.0 (c)  L. Petrov  07-MAY-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IUER, IER
      LOGICAL*4  PROBE_READ_ADDRESS
      IF ( PROBE_READ_ADDRESS(IUER) ) THEN
           IER = IUER
         ELSE
           IER = -1
      END IF
      RETURN
      END  !#!  IUER  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ERR_PEEK ( STR )
! ************************************************************************
! *                                                                      *
! *     Routine ERR_PEEK returns the line with the latest error message  *
! *   returned by ERR_LOG. IF ERR_LOG was not called at all or the last  *
! *   time was called as ERR_LOG ( 0, IUER ), what means that no error   *
! *   message was to be generated, then ERR_PEEK returns empty string.   *
! *                                                                      *
! *  ### 12-JUN-2002    ERR_PEEK   v1.0 (c)  L. Petrov  12-JUN-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  STR*(*), ERROR_BUFFER*1024
      COMMON   / ERROR__COMMON / ERROR_BUFFER
      EXTERNAL   ERROR_BLKDAT
!
      IF ( LOC(STR) .NE. 0 ) THEN
           CALL CLRCH ( STR )
           IF ( ERROR_BUFFER(1:3) .EQ. '???' ) THEN
                CALL CLRCH ( ERROR_BUFFER )
           END IF
           STR = ERROR_BUFFER
      END IF
!
      RETURN
      END  !#!  ERR_PEEK  #!#
!
! ------------------------------------------------------------------------
!
      BLOCK DATA ERROR_BLKDAT
! ************************************************************************
! *                                                                      *
! *   Block data program unit which intializes ERROR_COMMON common       *
! *   blocks. It sets only the first three fields pf ERROR_BUFFER --     *
! *   that is enough to tell to ERR_PEEK routine that ERROR_BUFFER was   *
! *   not yet properly initialized.                                      *
! *                                                                      *
! *  ### 12-JUN-2002  ERROR_BLKDAT  v1.0 (c)  L. Petrov 12-JUN-2002 ###  *
! *                                                                      *
! ************************************************************************
      CHARACTER  ERROR_BUFFER*1024
      COMMON   / ERROR__COMMON / ERROR_BUFFER
      DATA       ERROR_BUFFER(1:3) / '???' /
      END   !#!  ERROR_BLKDAT  #!#
