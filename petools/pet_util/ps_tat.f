        SUBROUTINE PS_TAT ( ISIGN, IG, IM, RS, ISS, DA, IERR )
! ************************************************************************
! *                                                                      *
! *     Auxiliary Routine  PS_TAT  reads the integer number of hours or  *
! *   degrees, integer number of minutes, real number for seconds and    *
! *   builds the output line which represents the angle in the format    *
! *   GGG_MM_SS.FFFFF  of the angle in the format.                       *
! *                                                                      *
! *        GGG  --    3 characters of degrees or hours.                  *
! *         MM  --    2 characters for minutes.                          *
! *         SS  --    2 characters for integer part of seconds.          *
! *       FFFF  --  0-ISS characters for fractional part of seconds      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      ISIGN  ( INTEGER*4 )  --  Signs of the angle: 1 or -1           *
! *         IG  ( INTEGER*4 )  --  Degrees or hours.                     *
! *         IM  ( INTEGER*4 )  --  minutes.                              *
! *         RS  ( REAL*8 )     --  seconds and their parts.              *
! *        ISS  ( INTEGER*4 )  --  the nuber of signs in fractional part *
! *                                of seconds.                           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *         DA  ( CHARACTER* ) --  Output line.                          *
! *       IERR  ( INTEGER*4  ) --  Error parameter.                      *
! *                                                                      *
! *  ### 26-JUL-1993     PS_TAT    v1.0 (c)  L. Petrov  17-SEP-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  ISIGN, IG, IM, ISS, IERR
      REAL*8     RS
      INTEGER*4  IST, LB, LE, LEN_DA, ISD, ILF, IER
      INTEGER*4, EXTERNAL :: ILEN
      CHARACTER DA*(*), SZ*1, FMT*8
!
!!        CALL VER$ARG ( 7 )
        SZ='_'     !  delimiter character
        IERR=0
        LEN_DA=LEN(DA)  !  הלימב ףפעןכי
        IF ( LEN_DA .LT. 10 ) THEN
             IERR=1
             RETURN
        END IF
!
! ----- Clean the output line
!
        CALL CLRCH ( DA )
!
! ----- Transforming degrees (hours)
!
        LB=1
        LE=3
        CALL INCH ( IABS(IG), DA(LB:LE) )
        IF ( DA(LB:LB) .EQ. '*' ) THEN
             IERR=2
             RETURN
        END IF
        CALL CHASHR ( DA(LB:LE) )
        CALL BLANK_TO_ZERO ( DA(LB:LE) )
        IF ( IABS(IG).LT.100 .AND. ISIGN.EQ. 1 ) DA(LB:LB)=' '
        IF ( IABS(IG).LT.100 .AND. ISIGN.EQ.-1 ) DA(LB:LB)='-'
        IF ( IABS(IG).GE.100 .AND. ISIGN.EQ.-1 ) THEN
             DA='-'//DA
             LE=LE+1
        END IF
!
! ----- Put the delimiter beteen degres (hours) and minutes
!
        DA(LE+1:LE+1)=SZ
        LE=LE+1
!
! ----- Transforming minutes
!
        LB=LE+1
        LE=LB+1
        CALL INCH ( IM, DA(LB:LE) )
        IF ( DA(LB:LB) .EQ. '*' ) THEN
             IERR=3
             RETURN
        END IF
        CALL CHASHR ( DA(LB:LE) )
        CALL BLANK_TO_ZERO   ( DA(LB:LE) )
!
! ----- Put delimiter between minutes and seconds
!
        DA(LE+1:LE+1)=SZ
        LE=LE+1
!
! ----- Transforing seconds
!
        ISD=ISS
        IF ( ISS.LE.0  ) ISD=0
        IF ( ISS.GE.15 ) ISD=15
!
! ----- Building the FMT  -- the line of the format transformation
!
        IST=ISD+3
        IF ( LE+IST .GT. LEN_DA ) THEN
!
! ---------- Format adjustment
!
             IST=LEN_DA-LE
             ISD=IST-3
        END IF
!
        CALL CLRCH ( FMT )
        FMT(1:2)='(F'
        CALL INCH ( IST, FMT(3:) )
        ILF=ILEN(FMT)
        FMT(ILF+1:)='.'
        CALL INCH ( ISD, FMT(ILF+2:) )
        ILF=ILEN(FMT)
        FMT(ILF+1:)=')'
!
! ----- TRansform seconds
!
        LB=LE+1
        LE=LB+IST-1
        WRITE ( UNIT=DA(LB:LE), FMT=FMT, IOSTAT=IER ) RS
        IF ( IER.NE.0 ) THEN
             IERR=4
             RETURN
        END IF
!
! ----- Replaces blanks ot zeroes
!
        CALL BLANK_TO_ZERO ( DA(LB:LE) )
!
! ----- Removing the decimal points and follwowing characters in the case
! ----- when we don't need seconds
!
        IF ( ISS.LE.0 ) CALL CLRCH ( DA(LB+2:) )
!
        RETURN
        END  !#!  PS_TAT  #!#
