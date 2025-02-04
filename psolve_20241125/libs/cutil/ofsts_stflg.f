      SUBROUTINE OFSTS_STFLG ( NOFST, REFSTA, FJDOBS, MODE )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OFSTS_STFLG PROGRAM SPECIFICATION
!
! 1.1
!
! 1.2 REFERENCES:
!
! 2.  OFSTS_STFLG INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 REFSTA
      REAL*8 FJDOBS
      CHARACTER*(*) MODE
!
! FJDOBS - Julian date for the observation
! MODE -
! REFSTA - Reference station
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 NOFST
!
! NOFST -
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: hsort
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I, J, K, STA, JCLOCK
      REAL*8    EARLY, DATE1, DATE2
      INTEGER*2 TICLSTA(ARC_STA_BIT_WORDS,MAX_CLK), &
     &          TLCLK(MAX_CLK), INDX(MAX_CLK)
      REAL*8    TFJDCL(MAX_CLK), SORTRA(MAX_CLK), MINUTE
      LOGICAL*2 KBIT
      INTEGER*4  I4P2
      DATA       I4P2 / 2 /
!
! DATE1,DATE2 -
! EARLY -
! I,J,K -
! JCLOK -
! INDX -
! MINUTE -
! STA -
! SORTRA -
! TFJDCL -
! TICLSTA -
! TLCLK -
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   PET   22-AUG-97  Added test of global variable SETFL_MDEG for taking
!                    decision whether to turn on bit for global 2-nd order
!                    polynomial
!
! 5.  OFSTS_STFLG PROGRAM STRUCTURE
!
! --- Initialize temp arrays
!
      MINUTE=1.D0/(60.0*24.0)
      DO I=1,MAX_CLK
          TLCLK(I)=0
          TFJDCL(I)=0.0
          DO J=1,2
             TICLSTA(j,I)=0
          ENDDO
          SORTRA(I)=0.0
          INDX(I)=0
      ENDDO
!
! --- Find earliest offset first epoc, set up as first element in ICLSTA, ect.
!
      EARLY=FJDCL(ICLSTR(1)+1)
      STA=1
      DO I=2,NUMSTA
          DATE1=FJDCL(ICLSTR(I)+1)
          IF(DATE1.LT.EARLY) THEN
              EARLY=DATE1
              STA=I
          ENDIF
      ENDDO
      CALL SBIT ( TLCLK(1), INT2(1), INT2(1) )
      CALL SBIT ( TLCLK(1), INT2(2), INT2(1) )
      IF ( SETFL_MDEG .EQ. I4P2 ) CALL SBIT ( TLCLK(1), INT2(3), INT2(1) )
      TFJDCL(1) = FJDCL(ICLSTR(STA)+1)
      IF ( TFJDCL(1) .LE. 0.0 ) TFJDCL(1)=FJDOBS-MINUTE
!
! --- Loop thru looking for refsta, offset epocs
!
      NOFST=1
      DO I=1,NUMSTA
         DO J=2,NUMCLK(I)
            JCLOCK=ICLSTR(I)+J
            IF ( KBIT( LCLK(JCLOCK), INT2(1) ) .AND. KBIT(ICLSTA(1,JCLOCK), &
     &           I).AND. .NOT.KBIT( LCLK(JCLOCK), INT2(13)) ) THEN
                 IF ( MODE(1:1) .EQ. 'A' ) THEN
                      NOFST=NOFST+1
                      CALL SBIT ( TLCLK(NOFST), INT2(1), INT2(1) )
                      CALL SBIT ( TLCLK(NOFST), INT2(2), INT2(1) )
                      IF ( SETFL_MDEG .EQ. I4P2 )CALL SBIT ( TLCLK(NOFST), &
     &                     INT2(3), INT2(1) )
                      TFJDCL(NOFST)=FJDCL(ICLSTR(I)+J)
                      CALL SBIT ( TICLSTA(1,NOFST), I, INT2(1) )
                  ENDIF
            ENDIF
         ENDDO
!
         IF ( REFSTA .EQ. 0 ) THEN
              JCLOCK = ICLSTR(I)+1
              IF ( .NOT. KBIT( LCLK(JCLOCK), INT2(1) )     .OR..NOT. &
     &             KBIT(ICLSTA(1,JCLOCK),I)      ) REFSTA=I
         ENDIF
      ENDDO
!
! --- Test for a refsta
!
      IF ( REFSTA .EQ. 0 ) CALL FERR ( INT2(204), '(AUTC): no refsta in '// &
     &    'auto clocks', INT2(0), INT2(0) )
      DO I=1,NUMSTA
         CALL SBIT ( TICLSTA(1,1), I, INT2(1) )
      ENDDO
      CALL SBIT( TICLSTA(1,1), REFSTA, INT2(0) )
!
! --- Set up and sort the FJDCL array in time order:  use INDX array
! --- to move around elements of LCLK, FJDCL arrays
!
      IF ( MODE(1:1) .EQ. 'A' ) THEN
           DO I=1,NOFST
              SORTRA(I)=TFJDCL(I)
              INDX(I)=I
           ENDDO
           CALL HSORT(SORTRA,INDX,NOFST )
!
! -------- Move temp arrays to common
!
           DO I=1,NOFST
              FJDCL(I)=TFJDCL(INDX(I))
              LCLK(I)=TLCLK(INDX(I))
              DO J=1,2
                 ICLSTA(J,I)=TICLSTA(J,INDX(I))
              ENDDO
          ENDDO
      ENDIF
!
      FJDCL(1)=TFJDCL(1)
      LCLK(1)=TLCLK(1)
      DO I=1,2
         ICLSTA(i,1)=TICLSTA(i,1)
      ENDDO
!
      RETURN
      END  !#!  OFSTS_STFLG  #!#
