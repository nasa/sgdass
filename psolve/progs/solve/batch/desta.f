      SUBROUTINE DESTA ( ISTA )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DESTA PROGRAM SPECIFICATION
!
! 1.1 Delete parameters associated with station ISTA.
!
! 1.2 REFERENCES:
!
! 2.  DESTA INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISTA
!
! ISTA - Station number
!
! 2.3 OUTPUT Variables:
!
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ddata
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 J,JATM,JCLOCK,IORD,K,JTYPE
      LOGICAL*2 KBIT
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DESTA PROGRAM STRUCTURE
!
! Delete position and velocity parameters
!
      DO J=1,3
         CALL SBIT( LSITEC(1,J), ISTA, INT2(0) )
         CALL SBIT( LSITEV(1,J), ISTA, INT2(0) )
      ENDDO
!
! --- Delete axis offset parameter
!
      CALL SBIT( LAXOF, ISTA, INT2(0) )
!
! --- Delete atmosphere parameters
!
      IF ( .NOT. BMODE_AT ) THEN
           DO JATM = IATSTR(ISTA)+1,IATSTR(ISTA)+NUMATM(ISTA)
              DO IORD=0,1
                 CALL SBIT( LATM(1,IORD+1), JATM, INT2(0) )
              ENDDO
           ENDDO
      ENDIF
!
! --- Delete clock parameters
!
      DO JCLOCK = 1,NUMCLK(ISTA)
         K=JCLOCK + ICLSTR(ISTA)
!
! ------ Check to make sure we have a clock offset to delete
!
         CALL SBIT ( ICLSTA(1,K), ISTA, INT2(0) )
      ENDDO
!
      IF ( LOGBCL ) THEN
           DO J=1,NUMSTA
              CALL SBIT( ICLOCK(1,ISTA), J, INT2(0) )
              CALL SBIT( ICLOCK(1,J), ISTA, INT2(0) )
           ENDDO
      ENDIF
!
      RETURN
      END  !#!  DESTA  #!#
