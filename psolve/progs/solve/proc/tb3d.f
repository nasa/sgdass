      PROGRAM    TB3D
! ************************************************************************
! *                                                                      *
! *   Testing utility for impplementation B3D algorithm in SOLVE.        *
! *                                                                      *
! *  ###  31-JAN-1997     TB3D     v2.0  (c)  L. Petrov 08-SEP-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      CHARACTER  FINAM*80, FINAM_B3D*80, FINAM_FULL*80, FINAM_VEC*80, &
     &           ZAG*80, FORM*20
      INTEGER*4  IUER, M, MM, NV, NH, IV, IH, IER, IP, MATYP, MR, MD, MDD
      PARAMETER  ( M=700, MM=M*M )
      PARAMETER  ( MD = 20000, MDD = (MD*(MD+1))/2 )
      REAL*4     T1, T2
      REAL*8     A(MM), B(M), A2(MM), B2(M), D2(M), SC(M)
      REAL*8,    ALLOCATABLE :: AD(:)
      INTEGER*4  IROW, ICOL, IR, IC, IR_BLO, IR_PLA, IC_BLO, IC_PLA, NBS
      INTEGER*4  ISWAP, N, S, G, NP, J1, J2, J3, IDGT, &
     &           SB, SX
      CHARACTER  TYP*1
      INTEGER*4  IPART, I, J
      INTEGER*8  LOCS
      REAL*8     RCOND, VAL_B3D, VAL_FULL, ADIF, EPS, EPS_N, EPS_C, THR, &
     &           GETEL_B3D
      PARAMETER  ( EPS_N = 1.D-12 )
      PARAMETER  ( EPS_C = 1.D-6  )
      PARAMETER  ( THR   = 1.D-30 )
      ADDRESS__TYPE :: IAD
      IPART(I,J) = I/J + MIN( MOD(I,J), 1 )
      LOCS(I,J) = INT8(min(I,J)) + ( INT8(max(I,J))*INT8(max(I,J)-1))/2
      ADDRESS__TYPE, EXTERNAL :: FULL_B3D
!
      CALL CLRCH ( FINAM )
!
!  0 -- TO SEE FULL NORMAL MATRIX
!  1 -- TO SEE B3D  NORMAL MATRIX/VECTOR
!  2 -- TO SEE TRANSFORMATION B3D SUMBATRICES       --->  FULL MATRIX
!  3 -- TO SEE TRANSFORMATION FULL MATRIX           --->  B3D SUMBATRICES
!  4 -- TO COMPARE NORMAL MATRIX/VECTOR B3D        <--->  FULL
!  5 -- TO COMPARE COVARIANCS MATRIX/ESTIMATES B3D <--->  FULL
!  6 -- TO SEE B3D  COV   MATRIX / VECTOR OF B3D ESTIMATES
!
 900  CONTINUE
      WRITE ( 6, 110 )
 110  FORMAT ( 1X,'   TB3D. (Test B3D implementation in SOLVE) '/ &
     &         1X,'   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '/ &
     &            ' 0 -- Look at full normal matrix '/ &
     &            ' 1 -- Look at B3D  normal matrix/vector '/ &
     &            ' 2 -- Look at transformation B3D sumbatrices   ---> ', &
     &                 ' full matrix'/ &
     &            ' 3 -- Look at transformation full matrix       ---> ', &
     &                 ' B3D sumbatrices '/ &
     &            ' 4 -- Compare normal matrix/vector B3D        <---> ', &
     &                 ' full '/ &
     &            ' 5 -- Compare covariancs matrix/estimates B3D ', &
     &                 '<--->  full'/ &
     &            ' 6 -- look at B3D  cov matrix / vector of B3D estimates'/ &
     &            ' Enter  (0/1/2/3/4/5/6)  >>  '$ )
      READ ( UNIT=5, ERR=900, FMT=* ) IP
      IF ( IP .EQ. 0  ) THEN
           FINAM = '/tmp/nor_full.mat'
           ALLOCATE ( AD(MDD), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) 'Failure to allocate dynamic memory'
                CALL EXIT ( 1   )
           END IF
           IER=-1
           CALL MATVIEW_R ( MDD, FINAM, MATYP, NV, NH, AD, ZAG, FORM, &
     &                      IV, IH, IER )
           IF ( IER .NE. 0 ) CALL EXIT ( 1 )
           IER=-1
           CALL MATVIEW ( MATYP, NV, NH, AD, ZAG, FORM, 1, 1, IER )
           IF ( IER .NE. 0 ) CALL EXIT ( 1 )
!
           FINAM = '/tmp/nor_full.vec'
           IER=-1
           CALL MATVIEW_R ( MD, FINAM, MATYP, NV, NH, A, ZAG, FORM, &
     &                      IV, IH, IER )
           IF ( IER .NE. 0 ) CALL EXIT ( 1 )
           IER=-1
           CALL MATVIEW ( MATYP, NV, NH, A, ZAG, FORM, 1, 1, IER )
           IF ( IER .NE. 0 ) CALL EXIT ( 1 )
           DEALLOCATE ( AD )
        ELSE IF ( IP .EQ. 1  .OR.  IP .EQ. 6 ) THEN
           IF ( IP .EQ. 1 ) FINAM = '/tmp/nor_b3d.bin'
           IF ( IP .EQ. 6 ) FINAM = '/tmp/cov_b3d.bin'
           CALL TIM_INIT()
           IUER=-1
           CALL RDNOR_B3D ( FINAM, B3DOBJ, IUER )
           CALL TIM_TP ( 1, T1, T2 )
           WRITE ( 6, 120 )
 120       FORMAT ( 1X,'--------------------------------------' )
           IF ( IP .EQ. 1 ) CALL SEE_B3D ( B3DOBJ, 1 )
           IF ( IP .EQ. 6 ) CALL SEE_B3D ( B3DOBJ, 2 )
        ELSE IF ( IP .EQ. 2 ) THEN
           FINAM = '/tmp/nor_b3d.bin'
           IUER=-1
           CALL RDNOR_B3D ( FINAM, B3DOBJ, IUER )
           N = B3DOBJ%NBS
           G = B3DOBJ%N_GLO
           S = B3DOBJ%SB
 910       CONTINUE
           WRITE ( 6, 130 )  G, N, S
 130       FORMAT ( 1X,'   G=',I3,' N=',I3,' S=',I3, &
     &                 '  Element in Full matrix: IROW, ICOL  ? '$ )
           READ ( 5, * )  IROW, ICOL
           IAD = FULL_B3D ( B3DOBJ, IROW, ICOL, TYP, NBS, IR, IC )
!
           WRITE ( 6, 140 )  IROW, ICOL, TYP, NBS, IR, IC
 140       FORMAT ( 1X,' Full:  IROW=',I4,' ICOL=',I4, &
     &                 '  --->  ',A,'(',I3,')  IR=',I3,' IC=',I3 )
           GOTO 910
        ELSE IF ( IP .EQ. 3 ) THEN
           FINAM = '/tmp/nor_b3d.bin'
           IUER=-1
           CALL RDNOR_B3D ( FINAM, B3DOBJ, IUER )
           N = B3DOBJ%NBS
           G = B3DOBJ%N_GLO
           S = B3DOBJ%SB
 920       CONTINUE
           WRITE ( 6, 150 )  G, N, S
 150       FORMAT ( 1X,'   G=',I3,' N=',I3,' S=',I3, &
     &            '  El. in B3D matrix: TYP  NBS, IR, IC  ? '$ )
           READ ( UNIT=5, FMT='(A1,I4,I4,I4)' ) TYP, NBS, IR, IC
           CALL B3D_FULL ( B3DOBJ, TYP, NBS, IR, IC, IROW, ICOL )
           IF ( IR .LE. 0 ) THEN
              WRITE ( 6, 180 )  TYP, NBS, IR, IC
 180          FORMAT ( 1X,' TYP  >>',A,'<< NBS=',I4,' IR=',I4,' IC=',I4 )
              WRITE ( 6, * ) ' Too small IR'
              GOTO 920
           END IF
           IF ( IC .LE. 0 ) THEN
              WRITE ( 6, 180 )  TYP, NBS, IR, IC
              WRITE ( 6, * ) ' Too small IC'
              GOTO 920
           END IF
           IF ( IROW .EQ. -1 ) THEN
              WRITE ( 6, 180 )  TYP, NBS, IR, IC
              WRITE ( 6, * ) ' IR/NBS are uncorrect'
              GOTO 920
           END IF
           IF ( ICOL .EQ. -1 ) THEN
              WRITE ( 6, 180 )  TYP, NBS, IR, IC
              WRITE ( 6, * ) ' IC/NBS are uncorrect'
              GOTO 920
           END IF
!
           WRITE ( 6, 160 )  TYP, NBS, IR, IC, IROW, ICOL
 160       FORMAT ( 1X,' B3D:  ',A,'(',I3,')  IR=',I3,' IC=',I3,'   ', &
     &                 '  --->   IROW=',I4,' ICOL=',I4 )
           GOTO 920
        ELSE IF ( IP .EQ. 4  .OR.  IP .EQ. 5 ) THEN
           IF ( IP .EQ. 4 ) THEN
                FINAM_B3D = '/tmp/nor_b3d.bin'
                FINAM_FULL= '/tmp/nor_full.mat'
                FINAM_VEC = '/tmp/nor_full.vec'
                EPS       = EPS_N
                WRITE ( 6, * ) 'NORMAL MATRICES and VECTORS'
             ELSE IF ( IP .EQ. 5 ) THEN
                FINAM_B3D = '/tmp/cov_b3d.bin'
                FINAM_FULL= '/tmp/cov_full.mat'
                FINAM_VEC = '/tmp/cov_full.vec'
                EPS       = EPS_C
                WRITE ( 6, * ) 'COVARIANCE MATRICES and VECTORS'
           END IF
           IUER=-1
           CALL RDNOR_B3D ( FINAM_B3D, B3DOBJ, IUER )
           IER=-1
           CALL MATVIEW_R ( M*M, FINAM_FULL, MATYP, NV, NH, A, ZAG, &
     &                      FORM, IV, IH, IER )
!!           CALL BEFULL_B3D ( B3DOBJ, A2, B2, D2, SC )
           CALL EXPAND_B3D ( B3DOBJ, A2, B2, D2, SC )
!
           N = B3DOBJ%NBS
           G = B3DOBJ%N_GLO
           SB = B3DOBJ%SB
           SX = B3DOBJ%SX
           NP = G + (N-1)*SB + SX
           DO 410 J1=1,NP
              DO 420 J2=1,J1
                 IAD = FULL_B3D ( B3DOBJ, J1, J2, TYP, NBS, IR, IC )
                 IF ( IR .EQ. -1 ) GOTO 420
                 CALL B3D_FULL ( B3DOBJ, TYP, NBS, IR, IC, IROW, ICOL )
                 IF ( ICOL .NE. J1 .OR. IROW .NE. J2 ) THEN
                      WRITE ( 6, * ) 'ERROR in B3D_FULL, FULL_B3D:  j1=',j1, &
     &                ' j2=',j2
                      WRITE ( 6, 160 )  TYP, NBS, IR, IC, IROW, ICOL
                 END IF
!!                 VAL_B3D = GETEL_B3D ( B3DOBJ, TYP, NBS, IR, IC )
!!                   CALL LIB$MOVC3 ( 8, %VAL(IAD), VAL_B3D )
!!                          type *,' irow=',irow,' icol=',icol  ! %%%%%%%%%%%%%
                 VAL_B3D = A2( LOCS(IROW,ICOL) )  ! test BEFULL_B3D
                 VAL_FULL=  A( LOCS(IROW,ICOL) )
                 ADIF = ABS ( VAL_FULL - VAL_B3D )
                 IF ( ( DABS(VAL_B3D) .GT. THR .AND. &
     &                  ADIF/DABS(VAL_B3D) .GT. EPS ) .OR. &
     &                ( DABS(VAL_B3D) .LT. EPS .AND. &
     &                  ADIF .GT. EPS ) ) THEN
                    IF ( DABS(VAL_B3D) .GT. THR ) THEN
                         IDGT = - IDINT ( DLOG10( ADIF/DABS(VAL_B3D) ) )+1
!!                         IDGT=88
                      ELSE
                       IDGT = 99
                    END IF
                    WRITE ( 6, 170 )  TYP, NBS, IR, IC, IROW, ICOL, VAL_B3D, &
     &                        VAL_FULL, IDGT
 170                FORMAT ( 1X,' ###:  ',A,'(',I3,')  IR=',I3, &
     &                    ' IC=',I3,'   ','  --->   IROW=',I4, &
     &                    ' ICOL=',I4/4X,' VAL_B3D = ',1PD22.15, &
     &                    ' VAL_FULL= ',1PD22.15,'  [',I2,']' )
                END IF
  420         CONTINUE
  410      CONTINUE
!
           IER=-1
           CALL MATVIEW_R ( M*M, FINAM_VEC, MATYP, NV, NH, A, ZAG, &
     &                      FORM, IV, IH, IER )
           DO 430 J3=1,NP
              IAD = FULL_B3D ( B3DOBJ, J3, J3, TYP, NBS, IR, IC )
              IF ( IR .EQ. -1 ) GOTO 430
              IF ( NBS .EQ. 0 ) THEN
                   IF ( IP .EQ. 4 ) THEN
                        IAD = B3DOBJ%AD_Z0 + 8*(IR-1)
                     ELSE IF ( IP .EQ. 5 ) THEN
                        IAD = B3DOBJ%AD_E0 + 8*(IR-1)
                   END IF
                ELSE IF ( NBS .LT. B3DOBJ%NBS ) THEN
                   IF ( IP .EQ. 4 ) THEN
                        IAD = B3DOBJ%AD_ZS(NBS) + 8*(IR-1)
                     ELSE IF ( IP .EQ. 5 ) THEN
                        IAD = B3DOBJ%AD_ES(NBS) + 8*(IR-1)
                   END IF
                ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                   IF ( IP .EQ. 4 ) THEN
                        IAD = B3DOBJ%AD_ZSX + 8*(IR-1)
                     ELSE IF ( IP .EQ. 5 ) THEN
                        IAD = B3DOBJ%AD_ESX + 8*(IR-1)
                   END IF
              END IF
              CALL LIB$MOVC3 ( 8, %VAL(IAD), VAL_B3D )
!!                 type *,' j3=',j3,' b2(j3)=',b2(j3), ' a(j3)=',a(j3) ! %%%%
!!                  VAL_B3D = B2(J3) ! Test of BEFULL_B3D  !!!!!!!!!!!!!
              IF ( IP .EQ. 4 ) THEN
                   VAL_B3D = D2(J3)
                ELSE IF ( IP .EQ. 5 ) THEN
                   VAL_B3D = B2(J3)
              END IF
              VAL_FULL= A(J3)
              ADIF = ABS ( VAL_FULL - VAL_B3D )
              IF ( ( DABS(VAL_B3D) .GT. THR .AND. &
     &               ADIF/DABS(VAL_B3D) .GT. EPS ) .OR. &
     &             ( DABS(VAL_B3D) .LT. EPS .AND. ADIF .GT. EPS ) ) THEN
                 IF ( DABS(VAL_B3D) .GT. THR ) THEN
                      IDGT = - IDINT ( DLOG10( ADIF/DABS(VAL_B3D) ) )+1
                    ELSE
                      IDGT = 99
                 END IF
                 WRITE ( 6, 190 )  NBS, IR, VAL_B3D, VAL_FULL, IDGT
 190             FORMAT ( 1X,'@@@: Z',I3,'(',I3,') ', &
     &                    ' B3D = ',1PD22.15,' FULL= ',1PD22.15, &
     &                    ' [',I2,']' )
              END IF
 430       CONTINUE
      END IF
      END  !#!  TB3D #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   GETEL_B3D ( B3DOBJ, TYP, NBS, IR, IC )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      CHARACTER  TYP*(*)
      REAL*8     GETEL_B3D
      INTEGER*4  NBS, IR, IC, I, J
      ADDRESS__TYPE :: ADR, LA
      INTEGER*8  LOCS
      LOCS(I,J) = INT8(min(I,J)) + INT8(max(I,J)*INT8(max(I,J)-1))/2
!
      IF ( TYP .EQ. 'G' ) THEN
           LA = LOCS( IR, IC ) - 1
           CALL LIB$MOVC3(8, %VAL(B3DOBJ%AD_B0 + 8*LA), GETEL_B3D )
      END IF
      IF ( TYP .EQ. 'B' ) THEN
           IF ( NBS .LT. B3DOBJ%NBS ) THEN
                ADR = B3DOBJ%AD_B(NBS) + 8*( (IC-1)*B3DOBJ%SB + IR -1 )
             ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                ADR = B3DOBJ%AD_BX     + 8*( (IC-1)*B3DOBJ%SX + IR -1 )
           END IF
           CALL LIB$MOVC3(8, %VAL(ADR), GETEL_B3D )
      END IF
      IF ( TYP .EQ. 'C' ) THEN
           LA = LOCS( IR, IC ) -1
           IF ( NBS .LT. B3DOBJ%NBS ) THEN
                ADR = B3DOBJ%AD_C(NBS) + 8*LA
             ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                ADR = B3DOBJ%AD_CX     + 8*LA
           END IF
           CALL LIB$MOVC3(8, %VAL(ADR), GETEL_B3D )
      END IF
      IF ( TYP .EQ. 'D' ) THEN
           IF ( NBS .LT. B3DOBJ%NBS ) THEN
                ADR = B3DOBJ%AD_D(NBS) + 8* ( (IC-1)*B3DOBJ%SB + IR -1 )
             ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                ADR = B3DOBJ%AD_DX     + 8* ( (IC-1)*B3DOBJ%SX + IR -1 )
           END IF
           CALL LIB$MOVC3(8, %VAL(ADR), GETEL_B3D )
      END IF
!
      RETURN
      END  !#!  GETEL_B3D  #!#
