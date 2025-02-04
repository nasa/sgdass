        SUBROUTINE INANG_TAT ( DA, IH, IM, RS, IERR )
! ************************************************************************
! *                                                                      *
! *     Auxiliary routine INANG_TAT extracts digital values of subfields *
! *   from the line in form HHH_MM_SS.FFFF                               *
! *                   where                                              *
! *               HHH  --  1-3 digit characters;                         *
! *                "_" --  subfields delimiter;                          *
! *                MM  --  1-2 digit characters;                         *
! *           SS.FFFF  --  number which is less than 100.0               *
! *                                                                      *
! *     Comment line in form HH:MM:SS.FFFFF is also permitted.           *
! *                                                                      *
! * ______________________ Input parameters: ___________________________ *
! *                                                                      *
! *       DA ( CHARACTER )  --  Character input line.                    *
! *                                                                      *
! * _____________________ Output parameters: ___________________________ *
! *                                                                      *
! *       IH ( INTEGER*4 )  -- Integer number -- value of HHH subfield.  *
! *       IM ( INTEGER*4 )  -- Integer number -- value of  MM subfield.  *
! *       RS ( REAL*8    )  -- Read*8 number -- vlaue of SS.SSSSSS field *
! *     IERR ( INTEGER*4 )  -- Completion status:                        *
! *                         IERR=0  --  Normlal compleion.               *
! *                         IERR=1  --  Error: wrong format.             *
! *                         IERR=2  --  Error: empty line.               *
! *                                                                      *
! *                                                                      *
! *  ### 17-APR-1991   INANG_TAT   v1.0 (c)  L. Petrov  11-SEP-2001 ###  *
! *                                                                      *
! ************************************************************************
        CHARACTER DA*(*), SZ*1, SZ1*1
        REAL*8 RS
        PARAMETER (  SZ='_' )  !  Primary delimiter character
        PARAMETER ( SZ1=':' )  !  Additional delimiter character
        INTEGER*4, EXTERNAL :: ILEN
!
        IH=0
        IM=0
        RS=0.0D0
!
! ----- Check: is the line empty?
!
        L=ILEN(DA)
        IF ( L.EQ.0 ) THEN
             IERR=2
             RETURN
        END IF
!
! ----- Search for the first character which is not blank
!
        DO 410 J1=1,L
           IF ( DA(J1:J1).NE.' ' ) GOTO 810
  410   CONTINUE
  810   IB=J1
!
! ----- Extraction the first field
!
        IF ( DA(IB:IB).EQ.'-'  ) IB=IB+1
        IE = IB + INDEX ( DA(IB:L), SZ ) - 1  !  Position of delimiter
        IF ( IE .EQ. IB-1 ) IE = IB + INDEX ( DA(IB:L), SZ1 ) -1
        IF ( IE.EQ.IB .OR. IE.EQ.L ) THEN
!
! ---------- Delimiter is the first or the lst character of the line
!
             IERR=1
             RETURN
        END IF
!
! ----- Decoding the first subfield
!
        IF ( IE.EQ.0 ) THEN
!
! ---------- The line contains only the first subfield
!
             IH=0
             CALL CHIN ( DA(IB:L), IH  )
             RETURN
        END IF
        IF ( IE .LT. IB ) THEN
             IERR = 1
             RETURN
        END IF
        CALL CHIN ( DA(IB:(IE-1)), IH )
        IF ( IH .LT. 0   .OR.  IH .GT. 900 ) THEN
             IERR = 3
             RETURN
        END IF
!
! ----- Decoding the second subfield
!
        IB = IE + 1
        IE = IB + INDEX ( DA(IB:L), SZ ) - 1  !  The next delimeter
        IF ( IE .EQ. IB-1 ) IE = IB + INDEX ( DA(IB:L), SZ1 ) - 1
!
        IF ( IE.LT.IB ) THEN
!
! ---------- There are minutes
!
             CALL CHIN ( DA(IB:L), IM )
             RETURN
        END IF
!
! ----- Minutes decoding
!
        CALL CHIN ( DA(IB:(IE-1)), IM )
        IF ( IM .LT. 0   .OR.  IM .GT. 100 ) THEN
             IERR = 4
             RETURN
        END IF
!
! ----- Extraction of seconds and their parts
!
        IB=IE+1
        IF ( IB.GE.L ) RETURN
        IE=L
        IF ( IE-IB.GT.19 ) IE=IB+19
!
! ----- Extraction of seconds
!
        CALL DFOR_MEN ( DA(IB:IE), RS, IERR )
        RETURN
        END  !#!  INANG_TAT  #!#
