      SUBROUTINE SETCL ( NEPOC, REFSTA, NOFST, INDAY )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SETCL PROGRAM SPECIFICATION
!
! 1.1 Set up clock epochs in automatic mode.
!
! 1.2 REFERENCES:
!
! 2.  SETCL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NEPOC,REFSTA,NOFST
      REAL*8  INDAY
!
! INDAY - Interval between epochs, in days
! NEPOC - Number of clock epochs
! NOFST - Number of clock offset epochs (clock breaks)
! REFSTA - Site number of reference station
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: auto_intrvl
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,K,RATES,KLOC_IN,KLOC_OUT,KLOC,BAD_APPLES
      REAL*8 NEWEP,CENTIMIN
!
! BAD_APPLES -
! CENTIMIN - One centiminute expressed as fraction of a day
! I,J - Loop indices
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  910524  Modify for new parameterization scheme
!
! 5.  SETCL PROGRAM STRUCTURE
!
      CENTIMIN=1.0D0/(100.0*60.0*24.0)
!
! --- Add ONE continued offset epoc immediately before each clock break
!
      DO I=2,2*(NOFST)-1,2
         NEWEP=FJDCL(I)-centimin
         DO J=NOFST+(I-1)/2,I,-1
             LCLK(J+1)  = LCLK(J)
             FJDCL(J+1) = FJDCL(J)
             DO K=1,2
                ICLSTA(K,J+1) = ICLSTA(K,J)
             ENDDO
         ENDDO
         LCLK(I)=0
         CALL SBIT ( LCLK(I), INT2(1), INT2(1) )
         CALL SBIT ( LCLK(I), INT2(13), INT2(1) )
         FJDCL(I)=NEWEP
         CALL SBIT ( ICLSTA(1,I), REFSTA, INT2(0) )
      ENDDO
      NOFST=2*NOFST-1
!
! --- Add continued rates to list, keying on the continued rate epoc after
! --- the earliest offset added to LCLK in OFSTS: this will always be
! --- FJDCL(2), since the earliest first offset was chosen to apply to all
! --- stations
!
      BAD_APPLES=0
      KLOC_IN=1
      DO I=1,NEPOC-1
         NEWEP=FJDCL(1)+(I*INDAY)
!
         CALL AUTO_INTRVL ( REFSTA, NEWEP, KLOC_IN, KLOC_OUT, INT2(NOFST+I-1- &
     &        BAD_APPLES) )
         IF ( KLOC_OUT .NE. -1 ) THEN
              CALL SBIT ( LCLK(KLOC_OUT), INT2(1), INT2(1) )
              CALL SBIT ( LCLK(KLOC_OUT), INT2(13), INT2(1) )
              DO J=1,2
                 ICLSTA ( J, KLOC_OUT )=0
              ENDDO
              DO J=1,NUMSTA
                 CALL SBIT ( ICLSTA(1,KLOC_OUT), J, INT2(1) )
              ENDDO
              CALL SBIT ( ICLSTA (1,KLOC_OUT), REFSTA, INT2(0) )
              KLOC_IN = KLOC_OUT
            ELSE
              BAD_APPLES=BAD_APPLES+1
          ENDIF
      ENDDO
      DO I=1,NUMSTA
         ICLSTR(I)=0
         NUMCLK(I)=NOFST+NEPOC-1-BAD_APPLES
      ENDDO
!
      RETURN
      END  !#!  SETCL  #!#
