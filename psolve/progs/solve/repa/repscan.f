      SUBROUTINE REPSCAN ( IBOBSM, M, Y, AMB, FLAG, FCLICK )
!
! ************************************************************************
! *                                                                      *
! *  REPSCAN corrects an input pseudo click value for ambiguity shifting *
! *  if FLAG>0 by scanning an area around FCLICK input value which       *
! *  depends on the value of FLAG. For FLAG=0 the routine finds the best *
! *  pseudo click value by scanning the whole area of the input value    *
! *  field Y.                                                            *
! *                                                                      *                                                                    *
! *  called subroutines: REPAMB8                                         *
! *  calling routine: REPA                                               *
! *                                                                      *
! *  ### 04-JUNE-2003  REPSCAN            V. Thorandt  04-JUNE-2003 ###  *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT   NONE
!
      INTEGER*4   IBOBSM           ! copy of BOBS_MAX (repa.i)
      INTEGER*4   M                ! # of values
      REAL*8      Y(M)             ! array of values
      REAL*8      AMB              ! ambiguity step
      REAL*8      MIN8             ! minimum values
      REAL*8      MAX8             ! maximum values
      REAL*8      FCLICK           ! click value (in/output depending on FLAG)
      REAL*8      FCLICK_IN        ! copy of input value of FCLICK
      INTEGER*4   J1, J2           ! cycle variables
      REAL*8      DIF(IBOBSM)      ! max.-min. as FCLICK values (Dimensionierung auf M aendern)
      REAL*8      VAL8_T(M)        ! temp. array
      INTEGER*4   ISTEPS           ! # of amb. steps
      INTEGER*4   FLAG             ! if >0 --> search in [FCLICK-(FLAG/100)*AMB,FCLICK+(FLAG/100)*AMB]
! with given FCLICK to correct FCLICK;
! else find FCLICK by scanning all observations
      REAL*8      BORD_L           ! lower border for searching
      REAL*8      BORD_U           ! upper border for searching
      REAL*8      DELT             ! temp. value
!
!C    write(6,*) 'REPSCAN: M=',M
!---- find the best FCLICK value
      IF ( FLAG .LE. 0 ) THEN
         DO J1=1,M                                                 ! try all Y values
            DO J2=1,M
               CALL REPAMB8 ( Y(J2), Y(J1), AMB, ISTEPS )          ! find # of amb. steps for Y(J1) click
               VAL8_T(J2) = Y(J2) + (ISTEPS * AMB)                 ! fill temp. array VAL8_T
            END DO
            MIN8 = VAL8_T(1)                                       ! initialize minimum of values
            MAX8 = VAL8_T(1)                                       ! initialize maximum of values
            DO J2=1,M
               IF ( VAL8_T(J2) .LT. MIN8 ) MIN8 = VAL8_T(J2)       ! find minimum of values
               IF ( VAL8_T(J2) .GT. MAX8 ) MAX8 = VAL8_T(J2)       ! find maximum of values
            END DO
            DIF(J1) = MAX8 - MIN8                                  ! greatest difference for Y(J1)click
         END DO
         MIN8 = DIF(1)                                             ! initialize minimum of differences
         FCLICK = Y(1)                                             ! initialize output click value
         DO J1=1,M                                                 ! find the minimum differences
            IF ( DIF(J1) .LT. MIN8 ) THEN
               MIN8 = DIF(J1)
               FCLICK = Y(J1)
            END IF
         END DO
      ELSE
         FCLICK_IN = FCLICK
!C       write(6,*) 'REPSCAN: FCLICK[in]=',FCLICK
         BORD_L = FCLICK - FLAG*AMB/100.0D0                        ! lower border for searching
         BORD_U = FCLICK + FLAG*AMB/100.0D0                        ! upper border for searching
         DELT = (BORD_U - BORD_L)/10.0D0
         DO J1=1,11                                                ! try 11 steps in FLAG-area
            FCLICK = BORD_L + (J1-1)*DELT                          ! set current FCLICK
            DO J2=1,M
               CALL REPAMB8 ( Y(J2), FCLICK, AMB, ISTEPS )         ! find # of amb. steps for current FCLICK
               VAL8_T(J2) = Y(J2) + (ISTEPS * AMB)                 ! fill temp. array VAL8_T
!C             write(6,*) 'ISTEPS[',J1,']',ISTEPS
            END DO
            MIN8 = VAL8_T(1)                                       ! initialize minimum of values
            MAX8 = VAL8_T(1)                                       ! initialize maximum of values
            DO J2=1,M
               IF ( VAL8_T(J2) .LT. MIN8 ) THEN
                  MIN8 = VAL8_T(J2)                                ! find minimum of values
               END IF
               IF ( VAL8_T(J2) .GT. MAX8 ) THEN
                  MAX8 = VAL8_T(J2)                                ! find maximum of values
               END IF
            END DO
            DIF(J1) = MAX8 - MIN8                                  ! greatest difference for current FCLICK
!C          write(6,*) 'DIF(',J1,')=',DIF(J1)
         END DO
         FCLICK = BORD_L + 5*DELT                                  ! initialize pseudo click by input value
         MIN8 = DIF(6)                                             ! initialize minimum of differences
         DO J1=1,11                                                ! find the minimum differences
            IF ( DIF(J1) .LT. MIN8 ) THEN
               MIN8 = DIF(J1)
!C             write(6,*) 'J1, MIN8=',J1,' ',MIN8
               FCLICK = BORD_L + (J1-1)*DELT                       ! set pseudo click value
            END IF
         END DO
!C       write(6,*) 'REPSCAN: FCLICK[out]=',FCLICK
      END IF
!
      RETURN
      END  !#!  REPSCAN  #!#
