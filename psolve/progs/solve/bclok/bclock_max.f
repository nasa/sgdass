      SUBROUTINE BCLOCK_MAX ( IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  BCLOCK_MAX  resets flags of estimation of baseline        *
! *   dependent clocks. It finds such a maximal combination of baselines *
! *   dependent clocks which doesn't make normal matrix singular.        *
! *                                                                      *
! *   Old setup of baseline-dependent clocks is lost.                    *
! *                                                                      *
! *   BCLOCK_MAX doesn't know whether the baselines selected in          *
! *   solution are actually used or not (there was at least one          *
! *   observation at these baselines). If it is not true then setup      *
! *   which  BLOCK_MAX  bids may lead to singularity of the normal       *
! *   matrix.                                                            *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  20-JUL-98   BCLOCK_MAX   v1.2  (c)  L. Petrov 22-AUG-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'obser.i'
!
      INTEGER*4  IUER
      INTEGER*4  L_STA, LIS_STA(MO_STA), L_BAS, LIS_BAS(MO_BAS), &
     &           L_TRI, LIS_TRI(3,MO_TRI)
      INTEGER*4  IER, J1, J2, J3
      LOGICAL*2  KBIT
      LOGICAL*4  CHECK_STABIT, DATYP_INQ
      INTEGER*4  NSTBA
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! --- Make a station list
!
      L_STA = 0
      DO 410 J1=1,INT4(NUMSTA)
         IF ( CHECK_STABIT ( INT2(J1) ) ) THEN
              CALL ADD_LIS ( MO_STA, L_STA, LIS_STA, J1, -3 )
         END IF
 410  CONTINUE
!
! --- Sorting it in increasing order
!
      CALL SORT_I ( L_STA, LIS_STA )
!
! --- Make a baseline list
!
      L_BAS = 0
      DO 420 J2=1,INT4(NUMSTA)-1
         IF ( .NOT. CHECK_STABIT ( INT2(J2) ) ) GOTO 420
         DO 430 J3=J2+1,INT4(NUMSTA)
            IF ( .NOT. CHECK_STABIT ( INT2(J3) ) ) GOTO 430
!
            IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                 IF ( KBIT (IBLSEL_P(1,INT2(J2)),INT2(J3)) .OR. &
     &                KBIT (IBLSEL_P(1,INT2(J3)),INT2(J2))      ) THEN
!
                      CALL ADD_LIS ( MO_BAS, L_BAS, LIS_BAS, NSTBA(J2,J3), -3 )
                 END IF
              ELSE
                 IF ( KBIT (IBLSEL_G(1,INT2(J3)),INT2(J2)) .OR. &
     &                KBIT (IBLSEL_G(1,INT2(J2)),INT2(J3))      ) THEN
!
                      CALL ADD_LIS ( MO_BAS, L_BAS, LIS_BAS, NSTBA(J2,J3), -3 )
                 END IF
            END IF
 430     CONTINUE
 420  CONTINUE
!
! --- Soriting it in increasing of baseline modules
!
      CALL SORT_IA ( L_BAS, LIS_BAS )
!
! --- Making  a closed triangle list
!
      CALL ERR_PASS ( IUER, IER )
      CALL TRI_GRP  ( L_STA, LIS_STA, L_BAS, LIS_BAS, MO_TRI, L_TRI, LIS_TRI, &
     &                IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4011, IUER, 'BCLOCK_MAX', 'Error during '// &
     &         'computation of the list of linearly independent closed '// &
     &         'trinagles' )
           RETURN
      END IF
!
! --- Setting flags of estimation of baseline dependent clocks
!
      CALL ERR_PASS   ( IUER, IER )
      CALL SET_BASCLK ( L_TRI, LIS_TRI, L_BAS, LIS_BAS, L_STA, LIS_STA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4012, IUER, 'BCLOCK_MAX', 'Error during '// &
     &         'setting up estimation flag for baseline dependent '// &
     &         'clocks' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  BCLOCK_MAX  #!#
