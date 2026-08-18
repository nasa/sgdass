      SUBROUTINE DO_CLK ( FAST_MODE, FAST_DBG, WHO_STA, LPARM, IPARMS, A, &
     &                    B3DOBJ, B1B3DOBJ )
      IMPLICIT NONE
!
! 1.  DO_CLK PROGRAM SPECIFICATION
!
! 1.1 Apply clock constraints as requested.
!
! 1.2 REFERENCES:
!
! 2.  DO_CLK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4   IPARMS
      REAL*8      A(*)
      CHARACTER   LPARM(M_GPA)*(*), WHO_STA(*)*8
!
! A - Normal equation matrix
! IPARM - Array of parameter names
! IPARMS - Number of parameters
! WHO_STA - Array of station names
!
! 2.3 OUTPUT Variables:
!
! A - Normal equation matrix with constraints added
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: cnstr
!       CALLED SUBROUTINES: add_clk
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2    IERR, HR, MIN
      INTEGER*4    CPARM(M_GPA), END_CPARM, I, J, K
      INTEGER*4    CHK_STRT, CHK_END, ICHK, INIT_CLK(MAX_ARC_STA,3)
      INTEGER*4    N4, IUER
      INTEGER*8    POS1
      INTEGER*8    INDX8
      CHARACTER*20 CLK_PARM*13,DUMRA
      REAL*8       CONSTRAINT(MAX_CLK), SIGMA, TIME_INTERVAL, TIM8(MAX_CLK)
!C
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      CHARACTER  TYP*1
      INTEGER*4  FAST_MODE, FAST_DBG, NBL, IR, IC, FULL_B3D, FULL_B1B3D, &
     &           IAD_DIAG, IAD_ELEM, J1
      REAL*8     VAL_DIAG, VAL_ELEM, EPS
      PARAMETER  ( EPS = 1.D-12 )
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: CHECK_STABIT
!C
!
! 4.  HISTORY
!   WHO  WHEN    WHAT
!   MWH  910524  Modify to accomodate new parameterization scheme
!   jmg  960610  Remove holleriths.
!   pet  970117  Added support of B3D parametrization
!   pet  970226  Added support of B1B3D parametrization
!   pet  971203  Added logic for bypassing deselected station
!
! 5.  DO_CLK PROGRAM STRUCTURE
!
!   Set up the constraint:  it is hard-wired here . . .
!
!CC
      DO I=1,NUMSTA
!
! ------ Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
         IF ( CHECK_STABIT ( I ) ) THEN
            SIGMA = DBLE(SCCNST(I))
            SIGMA = (SIGMA*1.D-14)*3600.d0
            CALL CINDEX_PARM ( 'CL1', CPARM, LPARM, IPARMS, END_CPARM, &
     &                          DUMRA, .FALSE., WHO_STA(I) )
            DO J=1,3
               INIT_CLK(I,J) = CPARM(1)+J-1
               IF ( END_CPARM.EQ.0 ) INIT_CLK(I,J) = 0
            ENDDO
!
            DO J=1,END_CPARM
               READ ( LPARM(CPARM(J))(17:18), '(I2)' ) HR
               READ ( LPARM(CPARM(J))(19:20), '(I2)' ) MIN
               TIM8(J) = HR + MIN/60.D0
            ENDDO
            CONSTRAINT(1) = 0.D0
            CHK_STRT = 1
            DO J=2,END_CPARM
               TIME_INTERVAL = TIM8(J)-TIM8(J-1)
               IF ( TIME_INTERVAL .LE. 0 ) TIME_INTERVAL = TIME_INTERVAL +24.D0
               CONSTRAINT(J) = 1.D0/( (SIGMA*TIME_INTERVAL)**2 )
               IF ( LPARM(CPARM(J))(9:10) .EQ. 'C0' ) THEN
                    CONSTRAINT(J) = 0.D0
                    CHK_END = J-1
                    DO ICHK=CHK_STRT,CHK_END
                       IF ( FAST_MODE .EQ. F__NONE .OR. &
     &                      FAST_MODE .EQ. F__PRD  .OR. &
     &                      FAST_MODE .EQ. F__B1D       ) THEN
!
! ------------------------- FULL case
!
                            POS1=INDX8(CPARM(ICHK),CPARM(ICHK))
                            IF ( A(POS1) .GT. EPS ) GOTO 100
                         ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ------------------------- B3D case
!
                            IAD_DIAG = FULL_B3D ( B3DOBJ, CPARM(ICHK), &
     &                                            CPARM(ICHK), TYP, NBL, IR, IC )
                            IF ( IAD_DIAG .LE. 0 ) THEN
                                 WRITE ( 6, * ) ' iad_diag=',iad_diag
                                 WRITE ( 6, * ) ' b3dobj%ad_b0 = ',b3dobj%ad_b0
                                 IUER = -1
                                 CALL ERR_LOG ( 3431, IUER, 'DO_CLK', &
     &                               'Internal error' )
                                 RETURN
                            END IF
                            CALL LIB$MOVC3 ( 8, %VAL(IAD_DIAG), VAL_DIAG )
                            IF ( VAL_DIAG .GT. EPS ) GOTO 100
                         ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------------------- B1B3D case
!
                            IAD_DIAG = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, &
     &                                              CPARM(ICHK), CPARM(ICHK), &
     &                                              TYP, NBL, IR, IC )
                            IF ( IAD_DIAG .LE. 0 ) THEN
                                 WRITE ( 6, * ) ' iad_diag=',iad_diag
                                 IUER = -1
                                 CALL ERR_LOG ( 3432, IUER, 'DO_CLK', &
     &                               'Internal error' )
                                 RETURN
                            END IF
                            CALL LIB$MOVC3 ( 8, %VAL(IAD_DIAG), VAL_DIAG )
                            IF ( VAL_DIAG .GT. EPS ) GOTO 100
                       END IF
                    ENDDO  ! ICHK
                    DO ICHK=CHK_STRT,CHK_END
                       CONSTRAINT(ICHK) = 0.D0
                    ENDDO
!
100                 CONTINUE
                    CHK_STRT=J
               ENDIF
            ENDDO
!
            DO J=CHK_STRT,END_CPARM
               IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD  .OR. &
     &              FAST_MODE .EQ. F__B1D     ) THEN
!
! ----------------- FULL case
!
                    POS1 = INDX8(CPARM(J),CPARM(J))
                    IF ( A(POS1) .GT. EPS ) GOTO 200
                 ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ----------------- B3D case
!
                    IAD_DIAG = FULL_B3D ( B3DOBJ, CPARM(J), CPARM(J), TYP, NBL, IR, IC )
                    IF ( IAD_DIAG .LE. 0 ) THEN
                         WRITE ( 6, * ) ' iad_diag=',iad_diag
                         WRITE ( 6, * ) ' b3dobj%ad_b0 = ',b3dobj%ad_b0
                         IUER = -1
                         CALL ERR_LOG ( 3433, IUER, 'DO_CLK', 'Internal error' )
                         RETURN
                    END IF
!
                    CALL LIB$MOVC3 ( 8, %VAL(IAD_DIAG), VAL_DIAG )
                    IF ( VAL_DIAG .GT. EPS ) GOTO 200
                 ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ----------------- B1B3D case
!
                    IAD_DIAG = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, CPARM(J), &
     &                                      CPARM(J), TYP, NBL, IR, IC )
                    IF ( IAD_DIAG .LE. 0 ) THEN
                         WRITE ( 6, * ) ' iad_diag=',iad_diag
                         IUER = -1
                         CALL ERR_LOG ( 3434, IUER, 'DO_CLK', 'Internal error' )
                         RETURN
                    END IF
                    CALL LIB$MOVC3 ( 8, %VAL(IAD_DIAG), VAL_DIAG )
                    IF ( VAL_DIAG .GT. EPS ) GOTO 200
               END IF
            ENDDO
!
            DO J=CHK_STRT,END_CPARM
               CONSTRAINT(J) = 0.D0
            ENDDO
!
  200       CONTINUE
!
! --------- Add the constraint
!
            CALL ADD_CLK ( FAST_MODE, CONSTRAINT, CPARM, END_CPARM, A, &
     &                     B3DOBJ, B1B3DOBJ )
         END IF
      ENDDO
!C
      CALL USE_GLBFIL ('ORC')
!C
      IF ( KBATCH  .AND.  .NOT. REQREF  .AND.  REFSTA .EQ. 0 ) THEN
           IF ( FAST_DBG .EQ. F__PRI ) THEN
                WRITE ( 6, * ) ' do_clk +      fast_mode = ',fast_mode
           END IF
!
! -------- The case when clocks for ALL stations are estimated.
!
           DO I=1,3
              DO J=1,NUMSTA
!
! -------------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
                  IF ( CHECK_STABIT ( J ) ) THEN
                       DO K=1,J
                          IF ( INIT_CLK(J,I).NE.0 .AND. INIT_CLK(K,I).NE.0 )THEN
                               IF ( FAST_MODE .EQ. F__NONE  .OR. &
     &                              FAST_MODE .EQ. F__PRD   .OR. &
     &                              FAST_MODE .EQ. F__B1D        ) THEN
!
! --------------------------------- FULL case
!
                                    POS1 = INDX8(INIT_CLK(J,I),INIT_CLK(K,I))
                                    A(POS1)=A(POS1) + 1.D0/1.D-12**2
                                  ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! --------------------------------- B3D case
!
                                    IAD_ELEM = FULL_B3D ( B3DOBJ, &
     &                                         INIT_CLK(J,I), INIT_CLK(K,I), &
     &                                         TYP, NBL, IR, IC )
                                    IF ( IAD_ELEM .LE. 0 ) THEN
                                         WRITE ( 6, * ) ' iad_elem=',iad_elem
                                         IUER = -1
                                         CALL ERR_LOG ( 3435, IUER, 'DO_CLK', &
     &                                                 'Internal error' )
                                         RETURN
                                    END IF
                                    CALL R8_UPDATE ( IAD_ELEM, 1.D0/1.D-12**2 )
                                  ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! --------------------------------- B1B3D case
!
                                    IAD_ELEM = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, &
     &                                              INIT_CLK(J,I), INIT_CLK(K,I), &
     &                                              TYP, NBL, IR, IC )
                                    IF ( IAD_ELEM .LE. 0 ) THEN
                                         WRITE ( 6, * ) ' iad_elem=',iad_elem
                                         IUER = -1
                                         CALL ERR_LOG ( 3436, IUER, 'DO_CLK', &
     &                                                 'Internal error' )
                                         RETURN
                                    END IF
                                    CALL R8_UPDATE ( IAD_ELEM, 1.D0/1.D-12**2 )
                               END IF
                          END IF
                       ENDDO
                  END IF
              ENDDO
           ENDDO
      ENDIF
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' do_clk        fast_mode = ',fast_mode
      END IF
      RETURN
      END  !#!  DO_CLK  #!#
