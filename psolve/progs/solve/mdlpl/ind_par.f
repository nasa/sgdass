      SUBROUTINE IND_PAR(IPARM,IWDS,NPARM,IKEY1,IKEY2,IC,IST,INUM,IW)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  IND_PAR PROGRAM SPECIFICATION
!
! 1.1 Index parameters
!
! 1.2 REFERENCES:
!
! 2.  IND_PAR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IWDS,NPARM,IPARM(IWDS,NPARM),IKEY1(IWDS),IKEY2(IWDS)
      INTEGER*2 IC
!
! IPARM - Array of parameters
! IWDS - Number of words per parameter
! NPARM - Total number of parameters
! IKEY1,IKEY2 - Clock or atmosphere
! IC - 9
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IST,INUM,IW(*)
!
! IST - Start (index) of clock/atmosphere parameter
! INUM - Clock/atmosphere parameter
! IW - clock, atmosphere, or eop parameter?
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: first
!       CALLED SUBROUTINES: kchek2
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J
      LOGICAL*2 KCHEK2
      character*1 ch
!
! 4.  HISTORY
!  WHO  WHEN   WHAT
!
! 5.  IND_PAR PROGRAM STRUCTURE
!
      IST=1
      INUM=0
      DO I=1,NPARM
        IF(KCHEK2(IKEY1,IPARM(1,I),IC)) THEN
          IW(I)=1
          call hol2char( iparm(1,i), INT2(10), INT2(10), ch )
          if (ch.eq.'0') iw(i) = 2
        ELSE IF(KCHEK2(IKEY2,IPARM(1,I),IC)) THEN
          IW(I)=2
        ELSE
           IW(I)=0
        ENDIF
        IF(IW(I).NE.0) THEN
          IST=I
          INUM=I
          DO J=I+1,NPARM
            IF(KCHEK2(IKEY1,IPARM(1,J),IC)) THEN
              IW(J)=1
              call hol2char( iparm(1,j), INT2(10), INT2(10), ch )
              if (ch.eq.'0') iw(j) = 2
              INUM=J
            ELSE IF(KCHEK2(IKEY2,IPARM(1,J),IC)) THEN
              IW(J)=2
              INUM=J
            ELSE
              GO TO 10
            ENDIF
          ENDDO
          GO TO 10
        ENDIF
      ENDDO
10    CONTINUE
      DO I=INUM+1,NPARM
        IW(I)=0
      ENDDO
      INUM=INUM-IST+1
!
      RETURN
      END
