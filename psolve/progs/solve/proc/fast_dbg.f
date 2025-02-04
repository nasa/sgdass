      SUBROUTINE B3D_FULL ( B3DOBJ, TYP, NBS, IR, IC, IROW, ICOL )
! ************************************************************************
! *                                                                      *
! *   Routine  B3D_FULL  finds the address of the element in full matrix *
! *   which corresponds to an element of B3D set of submatrices.         *
! *                                                                      *
! *  ###   13-Jan-97   B3D_FULL    v1.0  (c)  L. Petrov  13-Jan-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  IROW, ICOL, IR, IC, NBS
      CHARACTER  TYP*(*)
      INTEGER*4  ISWAP
      INTEGER*4  N, G, S, J1, NP, IER
!
      N = B3DOBJ%NBS
      G = B3DOBJ%N_GLO
      S = B3DOBJ%SB
!
      IROW = -1
      ICOL = -1
!
! --- Verify the admittance of the address
!
      IF (   IR .LE. 0   .OR.   IC .LE. 0                     ) RETURN
      IF (  NBS .LT. 0   .OR.   NBS .GT. N                    ) RETURN
      IF (  TYP .EQ. 'D' .AND.  NBS .LE. 1                    ) RETURN
      IF ( (TYP .EQ. 'B' .OR. TYP .EQ. 'C') .AND.  NBS .LE. 0 ) RETURN
!
      NP = B3DOBJ%N_GLO + B3DOBJ%N_SGM
!
! --- Search among all elements of full matric the correspndence
!
      DO 410 J1=1,NP
         IF ( TYP .EQ. 'B' ) THEN
              IF ( B3DOBJ%BLO(J1) .EQ. NBS .AND. &
     &             B3DOBJ%PL(J1)  .EQ. IR         ) THEN
                 ICOL = J1
              END IF
              IF ( B3DOBJ%BLO(J1) .EQ. 0   .AND. &
     &             B3DOBJ%PL(J1)  .EQ. IC         ) THEN
                 IROW = J1
              END IF
            ELSE IF ( TYP .EQ. 'C' ) THEN
              IF ( B3DOBJ%BLO(J1) .EQ. NBS .AND. &
     &             B3DOBJ%PL(J1)  .EQ. IR         ) THEN
                 IROW = J1
              END IF
              IF ( B3DOBJ%BLO(J1) .EQ. NBS .AND. &
     &             B3DOBJ%PL(J1)  .EQ. IC         ) THEN
                 ICOL = J1
              END IF
            ELSE IF ( TYP .EQ. 'D' ) THEN
              IF ( B3DOBJ%BLO(J1) .EQ. NBS .AND. &
     &             B3DOBJ%PL(J1)  .EQ. IR         ) THEN
                 ICOL = J1
              END IF
              IF ( B3DOBJ%BLO(J1) .EQ. NBS-1 .AND. &
     &             B3DOBJ%PL(J1)  .EQ. IC         ) THEN
                 IROW = J1
              END IF
            ELSE IF ( TYP .EQ. 'G' ) THEN
              IF ( B3DOBJ%BLO(J1) .EQ. 0   .AND. &
     &             B3DOBJ%PL(J1)  .EQ. IR         ) THEN
                 IROW = J1
              END IF
              IF ( B3DOBJ%BLO(J1) .EQ. 0   .AND. &
     &             B3DOBJ%PL(J1)  .EQ. IC         ) THEN
                 ICOL = J1
              END IF
         END IF
 410  CONTINUE
!
! --- Swapping of the elements
!
      IF ( IROW .GT. ICOL ) THEN
           ISWAP = IROW
           IROW  = ICOL
           ICOL  = ISWAP
      END IF
!
      RETURN
      END  !#!  B3D_FULL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BEFULL_B3D ( B3DOBJ, MAT, VEC, DSP, SCL )
! ************************************************************************
! *                                                                      *
! *   Routine  BEFULL_B3D  expand normal submatrices and subvector       *
! *   enclosed in object B3DOBJ  to full matrix and full vectors. It is  *
! *   assumed that matrix MAT was zeroed earlier. Only those elements of *
! *   MAT which correspond elements B3D are being changed.               *
! *                                                                      *
! *  ###  14-JAN-97   BEFULL_B3D   v2.1  (c)  L. Petrov  22-Jan-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      REAL*8     MAT(*), VEC(*), DSP(*), SCL(*)
!
      INTEGER*4  IR, IC, NBS
      CHARACTER  TYP*1
      INTEGER*4  J1, J2, LA, NP
      ADDRESS__TYPE :: IAD_MAT, IAD_VEC, IAD_DSP, IAD_SCL
      ADDRESS__TYPE, EXTERNAL :: FULL_B3D
!
      NP = B3DOBJ%N_GLO + B3DOBJ%N_SGM  !  Total numbers of parameters
!
      LA = 0
      DO 410 J1=1,NP
         DO 420 J2=1,J1
!
! --------- Calculating address of the element for full matrix (LA) and
! --------- the address of the element from B3DOBJ (IAD_MAT) which corresponds
! --------- to it
!
            LA  = LA + 1
            IAD_MAT = FULL_B3D ( B3DOBJ, J1, J2, TYP, NBS, IR, IC )
            IF ( IAD_MAT .GT. 0 ) THEN
!
! -------------- In the case that there is such element in B3D submatrices --
! -------------- moving an elelment.
!
                 CALL LIB$MOVC3 ( 8, %VAL(IAD_MAT), MAT(LA) )
            END IF
 420     CONTINUE
!
! ------ Calculation address of the element of normal vector for B3D case
!
         IF ( NBS .EQ. 0 ) THEN
              IAD_VEC = B3DOBJ%AD_E0 + 8*(IR-1)
              IAD_DSP = B3DOBJ%AD_Z0 + 8*(IR-1)
              IAD_SCL = B3DOBJ%AD_U0 + 8*(IR-1)
           ELSE IF ( NBS .LT. B3DOBJ%NBS ) THEN
              IAD_VEC = B3DOBJ%AD_ES(NBS) + 8*(IR-1)
              IAD_DSP = B3DOBJ%AD_ZS(NBS) + 8*(IR-1)
              IAD_SCL = B3DOBJ%AD_US(NBS) + 8*(IR-1)
           ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
              IAD_VEC = B3DOBJ%AD_ESX + 8*(IR-1)
              IAD_DSP = B3DOBJ%AD_ZSX + 8*(IR-1)
              IAD_SCL = B3DOBJ%AD_USX + 8*(IR-1)
         END IF
!
! ------ Moving an element of the vector
!
         CALL LIB$MOVC3 ( 8, %VAL(IAD_VEC), VEC(J1) )
         CALL LIB$MOVC3 ( 8, %VAL(IAD_DSP), DSP(J1) )
         CALL LIB$MOVC3 ( 8, %VAL(IAD_SCL), SCL(J1) )
 410  CONTINUE
!
      RETURN
      END  !#!  BEFULL_B3D  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SEE_B3D ( B3DOBJ, IPAR )
! ************************************************************************
! *                                                                      *
! *   Routine  SEE_B3D  visualize set of B3DOBJ matrices/vectors         *
! *   before/after solution.                                             *
! *                                                                      *
! *  ###  09-Jan-97     SEE_B3D    v2.0  (c)  L. Petrov  22-Jan-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  ITP, K, G, GA, S, SA, SX, SXA, IPAR, IER
!
      G   = B3DOBJ%N_GLO
      GA  = (G*(G+1))/2
      S   = B3DOBJ%SB
      SX  = B3DOBJ%SX
      SA  = (S*(S+1))/2
      SXA = (S*(S+1))/2
  910 CONTINUE
         IF ( IPAR .EQ. 1 ) WRITE ( 6, 110 )  B3DOBJ%NBS
         IF ( IPAR .EQ. 2 ) WRITE ( 6, 120 )  B3DOBJ%NBS
 110     FORMAT ( '  SEE_B3D.  Enter (0-B0, 1-B, 2-C, 3-D, 4-Z0, 5-ZS)', &
     &            '  and   K (1,',I3,')  >> '$ )
 120     FORMAT ( '  SEE_B3D.  Enter (0-B0, 1-B, 2-C, 3-D, 4-E0, 5-ES)', &
     &            '  and   K (1,',I3,')  >> '$ )
         READ ( 5, * )  ITP, K
         IF ( K .GT. B3DOBJ%NBS ) THEN
              WRITE ( 6, * ) ' Wrong   K!'
              GOTO 910
         END IF
         IF ( ITP.EQ.0 ) THEN
              CALL MATVIEW ( 3, G, G, %VAL(B3DOBJ%AD_B0), 'Block B0', &
     &                       '()', 1, 1, %VAL(0) )
            ELSE IF ( ITP.EQ.1 ) THEN
              IF ( K .LT. B3DOBJ%NBS ) THEN
                   CALL MATVIEW ( 1, S, G, %VAL(B3DOBJ%AD_B(K)), &
     &                  'Block B(K)', '()', 1, 1, IER )
                 ELSE IF ( K .EQ. B3DOBJ%NBS ) THEN
                   CALL MATVIEW ( 1, SX, G, %VAL(B3DOBJ%AD_BX), &
     &                  'Block BX', '()', 1, 1, IER )
              END IF
            ELSE IF ( ITP.EQ.2 ) THEN
              IF ( K .LT. B3DOBJ%NBS ) THEN
                   CALL MATVIEW ( 3, S, S, %VAL(B3DOBJ%AD_C(K)), &
     &                 'Block C(K)', '()', 1, 1, IER )
                 ELSE IF ( K .EQ. B3DOBJ%NBS ) THEN
                   CALL MATVIEW ( 3, SX, SX, %VAL(B3DOBJ%AD_CX), &
     &                 'Block CX', '()', 1, 1, -1  )
              END IF
            ELSE IF ( ITP.EQ.3 ) THEN
              IF ( K .LT. B3DOBJ%NBS ) THEN
                   CALL MATVIEW ( 1, S, S, %VAL(B3DOBJ%AD_D(K)), &
     &                  'Block D(K)', '()', 1, 1, IER )
                 ELSE IF ( K .EQ. B3DOBJ%NBS ) THEN
                   CALL MATVIEW ( 1, SX, S, %VAL(B3DOBJ%AD_DX), &
     &                  'Block DX', '()', 1, 1, -1  )
              END IF
            ELSE IF ( ITP.EQ.4 ) THEN
              IF ( IPAR .EQ. 1 ) THEN
                   CALL MATVIEW( 1, G, 1, %VAL(B3DOBJ%AD_Z0), &
     &                 'Vector Z0', '()', 1, 1, IER )
               ELSE IF ( IPAR .EQ. 2 ) THEN
                   CALL MATVIEW( 1, G, 1, %VAL(B3DOBJ%AD_E0), &
     &                 'Vector E0', '()', 1, 1, IER )
              END IF
            ELSE IF ( ITP.EQ.5 ) THEN
              IF ( IPAR .EQ. 1 ) THEN
                   IF ( K .LT. B3DOBJ%NBS ) THEN
                        CALL MATVIEW ( 1, S, 1, %VAL(B3DOBJ%AD_ZS(K)), &
     &                      'Vector ZS(K)', '()', 1, 1, IER )
                      ELSE IF ( K .EQ. B3DOBJ%NBS ) THEN
                        CALL MATVIEW ( 1, SX, 1, %VAL(B3DOBJ%AD_ZSX), &
     &                      'Vector ZSX', '()', 1, 1, IER )
                   END IF
                 ELSE IF ( IPAR .EQ. 2 ) THEN
                   IF ( K .LT. B3DOBJ%NBS ) THEN
                        CALL MATVIEW ( 1, S, 1, %VAL(B3DOBJ%AD_ES(K)), &
     &                      'Vector ES(K)', '()', 1, 1, IER )
                      ELSE IF ( K .EQ. B3DOBJ%NBS ) THEN
                        CALL MATVIEW ( 1, SX, 1, %VAL(B3DOBJ%AD_ESX), &
     &                      'Vector ESX', '()', 1, 1, IER )
                   END IF
              END IF
            ELSE IF ( ITP .LT. 0 ) THEN
              GOTO 810
          END IF
         GOTO 910
 810  CONTINUE
      RETURN
      END  !#!  SEE_B3D  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SEE_B1B3D ( B3DOBJ, B1B3DOBJ, IPAR )
! ************************************************************************
! *                                                                      *
! *   Routine  SEE_B1B3D  visualize set of B3DOBJ matrices/vectors       *
! *   before/after solution.                                             *
! *                                                                      *
! *  ###  26-FEB-97   SEE_B1B3D    v1.1  (c)  L. Petrov  13-MAR-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( PLACE__STRU ) ::     PLACE
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      INTEGER*4  ITP, K, G, GA, L, LA, S, SC,  IPAR, IER
!
      G   = B3DOBJ%N_GLO
      GA  = (G*(G+1))/2
      L   = B3DOBJ%N_LOC
      LA  = (L*(L+1))/2
      S   = B3DOBJ%SB
  910 CONTINUE
         WRITE ( 6, * ) 'SEE_B1B3D'
         IF ( IPAR .EQ. 1 ) WRITE ( 6, 110 )  B3DOBJ%NBS
         IF ( IPAR .EQ. 2 ) WRITE ( 6, 120 )  B3DOBJ%NBS
 110     FORMAT ( ' Enter (0-W00, 1-WI0, 2-WIJ, 3-BI0, 4-BIJ', &
     &            ' 5-CIJ, 6-DIJ, 7-ZI0, 8-ZIJ)'/ &
     &            ' and   K (1,',I3,')  >> '$ )
 120     FORMAT ( ' Enter (0-W00, 1-WI0, 2-WIJ, 3-BI0, 4-BIJ', &
     &            ' 5-CIJ, 6-DIJ, 7-EI0, 8-EIJ)'/ &
     &            ' and   K (1,',I3,')  >> '$ )
         READ ( 5, * )  ITP, K
         IF ( K .GT. B3DOBJ%NBS ) THEN
              WRITE ( 6, * ) ' Wrong   K!'
              GOTO 910
         END IF
!
         SC=B3DOBJ%SB
         IF ( K .EQ. B3DOBJ%NBS ) SC = B3DOBJ%SX
!
         IF ( ITP.EQ.0 ) THEN
              CALL MATVIEW ( 3, G,  G,  %VAL(B1B3DOBJ%AD_W00), 'Block W00', &
     &                       '()',  1, 1, IER )
            ELSE IF ( ITP.EQ.1 ) THEN
              CALL MATVIEW ( 1, L,  G,  %VAL(B1B3DOBJ%AD_WI0), &
     &                      'Block WI0', '()', 1, 1, IER )
            ELSE IF ( ITP.EQ.2 ) THEN
              CALL MATVIEW ( 1, SC, G,  %VAL(B1B3DOBJ%AD_WIJ(K)), &
     &                 'Block WIJ(K)', '()', 1, 1, IER )
            ELSE IF ( ITP.EQ.3 ) THEN
              CALL MATVIEW ( 3, L,  L,  %VAL(B1B3DOBJ%AD_BI0), &
     &                  'Block BI0', '()', 1, 1, IER )
            ELSE IF ( ITP.EQ.4 ) THEN
              CALL MATVIEW ( 1, SC, L,  %VAL(B1B3DOBJ%AD_BIJ(K)), &
     &                  'Block BIJ(K)', '()', 1, 1, IER )
            ELSE IF ( ITP.EQ.5 ) THEN
              CALL MATVIEW ( 3, SC, SC, %VAL(B1B3DOBJ%AD_CIJ(K)), &
     &                  'Block CIJ(K)', '()', 1, 1, IER )
            ELSE IF ( ITP.EQ.6 ) THEN
              CALL MATVIEW ( 1, SC, S,  %VAL(B1B3DOBJ%AD_DIJ(K)), &
     &                  'Block DIJ(K)', '()', 1, 1, IER )
            ELSE IF ( ITP.EQ.7 ) THEN
              IF ( IPAR .EQ. 1 ) THEN
                   CALL MATVIEW ( 1, L, 1,  %VAL(B1B3DOBJ%AD_ZI0), &
     &                 'Block ZI0', '()', 1, 1, IER )
                ELSE IF ( IPAR .EQ. 2 ) THEN
                   CALL MATVIEW ( 1, L, 1,  %VAL(B1B3DOBJ%AD_EI0), &
     &                 'Block EI0', '()', 1, 1, IER )
              END IF
            ELSE IF ( ITP.EQ.8 ) THEN
              IF ( IPAR .EQ. 1 ) THEN
                   CALL MATVIEW ( 1, SC, 1, %VAL(B1B3DOBJ%AD_ZIJ(K)), &
     &                 'Block ZIJ(K)', '()', 1, 1, IER )
                ELSE IF ( IPAR .EQ. 2 ) THEN
                   CALL MATVIEW ( 1, SC, 1, %VAL(B1B3DOBJ%AD_EIJ(K)), &
     &                 'Block EIJ(K)', '()', 1, 1, IER )
              END IF
            ELSE IF ( ITP .LT. 0 ) THEN
              GOTO 810
          END IF
         GOTO 910
 810  CONTINUE
      RETURN
      END  !#!  SEE_B1B3D  #!#
