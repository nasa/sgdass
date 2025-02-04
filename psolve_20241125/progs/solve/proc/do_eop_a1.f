      SUBROUTINE DO_EOP_A1 ( A, B3DOBJ, B1B3DOBJ )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DO_EOP_A1 PROGRAM SPECIFICATION
!
! 1.1 Apply new style earth orientation constraints (applies to ratio).
!
! 1.2 REFERENCES:
!
! 2.  DO_EOP_A1 INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8 A(*)
!
!     A - Normal equation matrix
!
! 2.3 OUTPUT Variables:
!
!     A - Modified normal equation matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*8 INDX8, POS
      INTEGER*4 I, J, ITYP, IROT, IORD, IROTT, THIS_START, THIS_NUM, &
     &          ISTART, ISTOP
      REAL*8    CONSTRAINT, SIGMA
      LOGICAL*2 KBIT
!C
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      INTEGER*4  FAST_MODE, FAST_DBG
      ADDRESS__TYPE :: IAD_ELEM
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      ADDRESS__TYPE, EXTERNAL :: FULL_B3D, FULL_B1B3D
!
!     Transformation INT2 --> INT4
!
! 4.  HISTORY
!   WHO  WHEN    WHAT
!   pet  970117  Added support of B3D parametrization
!   pet  970226  Added support of B1B3D parametrization
!
! 5.  DO_EOP_A1 PROGRAM STRUCTURE
!
!CCCCC
      THIS_START = NPARAM - IPSTP + 1 ! This is where the eo parms start
!
! --- For x-wobble, then y-wobble, then UT1, if the user wants to estimate
! --- individual rates associated with epochs, apply constraints to the
! --- A matrix elements that represent those rates. If the user is not
! --- estimating individual rates, must still do the logic to locate
! --- the set of x-wobble (y-wobble, UT1) parameters
! --- within the A matrix, since the next set of parameters will work from
! --- that position.
!
      DO I = 1,3 ! Running over X-WOBBLE, Y-WOBBLE, UT1
         IF ( I .LE. 2 ) THEN ! X-WOBBLE AND Y-WOBBLE have same constraints
              ITYP = 1
           ELSE
              ITYP = 2
         END IF
!
! ------ 1. Find out how many parameters this earth orientation parameter has..
!
         IF ( EOP_STYLE(ITYP) .EQ. 1) THEN
!
! ----------- ...At least two - one for offset and one for global rate
!
              THIS_NUM = 2
!
! ----------- One for each individual rate selected.
!
              IF ( KBIT( EOPA1_CHOICE(ITYP), INT2(1) ) ) THEN
                   THIS_NUM = THIS_NUM + NROT_A1(ITYP)
!
! ---------------- If rate breaks, then subtract off the global rate parameter
! ---------------- (MWH 920528)
!
                   THIS_NUM = THIS_NUM - 1
              ENDIF
!
! ----------- Two for diurnal sine. (Estimate the amplitudes of the sine
! ----------- and cosine which, when added together, form the diurnal sine.)
!
              IF ( KBIT( EOPA1_CHOICE(ITYP), INT2(2) ) ) THIS_NUM = THIS_NUM +2
!
! ----------- Same for semi-diurnal sine.
!
              IF (KBIT( EOPA1_CHOICE(ITYP), INT2(3) )) THIS_NUM = THIS_NUM +2
            ELSE
              THIS_NUM = 0
              DO IROT = 1,NROT
                 DO IORD = 1,4
                   IF ( IROTT(IROT,I,IORD,LROT) .EQ. 1 ) THIS_NUM = THIS_NUM +1
                 END DO
              END DO
         END IF
!
! ------ 2. Apply the constraints, if the user is estimating the individual
! ------    rates for this type of earth orientation parameter.
!
         IF ( EOP_STYLE(ITYP) .EQ. 1  .AND. KBIT( CONSTRAINT_BITS, INT2(ITYP+ &
     &        3)) ) THEN
!
! ----------- Calculate the constraint
!
              SIGMA=DBLE(SEOCNST(ITYP))
!
! ----------- For X and Y wobble, convert sigma from mas/day to rad/day
! ----------- For UT1, convert from ms/day to  (time) sec/day
!
              IF ( ITYP .EQ. 1 ) THEN
                   SIGMA=(SIGMA*PI__NUM)/(1000.0D0*3600.0D0*180.0D0)
                ELSE
                   SIGMA=SIGMA/1000.0D0
              END IF
!
              CONSTRAINT=1.D0/(SIGMA**2)
!
! ----------- Add the constraint to the proper A matrix elements (the rate
! ----------- break elements on the diagonal)
!
              ISTART = THIS_START + 1
              ISTOP  = ISTART + NROT_A1(ITYP) -1
              DO J = ISTART, ISTOP
                 IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD   .OR. &
     &                FAST_MODE .EQ. F__B1D     ) THEN
!
! ------------------- FULL case
!
                      POS = INDX8(J,J)
                      A(POS) = A(POS) + CONSTRAINT
                   ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ------------------- B3D case
!
                      IAD_ELEM = FULL_B3D ( B3DOBJ, J, J,%VAL(0), &
     &                                      %VAL(0), %VAL(0), %VAL(0) )
                      CALL R8_UPDATE ( IAD_ELEM, CONSTRAINT )
                   ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------------- B1B3D case
!
                      IAD_ELEM = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, J, &
     &                                        J, %VAL(0), %VAL(0), &
     &                                        %VAL(0), %VAL(0) )
                      CALL R8_UPDATE ( IAD_ELEM, CONSTRAINT )
                 END IF
              ENDDO
         END IF !constraints on for this parameter
!
         THIS_START = THIS_START + THIS_NUM
      END DO
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' do_eop_a1     fast_mode = ',fast_mode
      END IF
      RETURN
      END  !#!  DO_EOP_A1  #!#
