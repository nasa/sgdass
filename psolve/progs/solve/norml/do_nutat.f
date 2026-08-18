      SUBROUTINE DO_NUTAT ( A, IXATS, NPARM, KNUT, KFLPSI, KFLEPS, KIDPNUT, &
     &                      KNDPNUT, KNFLPSI, KNFLEPS, INUT, NUT_CMP_SIGMA, &
     &                      CNSTROBJ )
!CCCC
!  pet 980206 Declared the sigma of the constrain in solve.i instead of
!             hard-coded value. Write down information about the type
!             of applied constraint.
!  pet 980722 Made NUT_CMP_SIGMA formal parameter instead of named
!             constatnt in solve.i block.
!CCCC
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INTEGER*4 NPARM, IXATS(NPARM)
      INTEGER*2 KNUT(3),KFLPSI(14),KFLEPS(14),KIDPNUT(7),KNDPNUT, &
     &          KNFLPSI,KNFLEPS,INUT(2,2,116)
      REAL*8 A(*)
!
      INCLUDE 'socom.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'cnstr.i'
!
      CHARACTER*1 COMP,PHASE
      CHARACTER*2 LOCAL
      INTEGER*4 I,J,IOFF,I1,I2
      INTEGER*8 INDX8
      LOGICAL*2 KBIT
      REAL*8 SINEPS
      DATA SINEPS/0.39777716D0/
      REAL*8      NUT_CMP_SIGMA
      LOGICAL*4   TRUE_L4
      PARAMETER ( TRUE_L4 = .TRUE. )
      TYPE ( CNSTR__STRU ) ::    CNSTROBJ
!
      DO I=1,NPARM
         IF ( IXATS(I).EQ.0 ) THEN
              IOFF=I-1
              GOTO 10
         ENDIF
      ENDDO
      RETURN
10    CONTINUE
!
      DO I=1,116
         CALL HOL2CHAR( NUTSUP(I), INT2(1), INT2(2), LOCAL )
         COMP=LOCAL(1:1)
         PHASE=LOCAL(2:2)
         IF ( COMP.EQ.'T' ) THEN
            IF ( PHASE.EQ.'B' .OR. PHASE.EQ.'I' ) THEN
               IF ( INUT(1,1,I).NE.0 .AND. INUT(1,2,I).NE.0 ) THEN
!
! --------------- Add information about the type of the constraint applied
!
                  CALL ADD_TYCNS ( 'NUT_CMP', 'Nutation component', &
     &                'rad', NUT_CMP_SIGMA, TRUE_L4, CNSTROBJ )
!
! --------------- Apply constraint
!
                  I1=IOFF+INUT(1,1,I)
                  I2=IOFF+INUT(1,2,I)
                  A(INDX8(I1,I1)) = A(INDX8(I1,I1))+(SINEPS**2)/NUT_CMP_SIGMA**2
                  A(INDX8(I1,I2)) = A(INDX8(I1,I2))+(-SINEPS)/NUT_CMP_SIGMA**2
                  A(INDX8(I2,I2)) = A(INDX8(I2,I2))+(1.0D0)/NUT_CMP_SIGMA**2
                ELSE
                  WRITE(*,*) I
!                 PAUSE 'NUTATION IN PHASE CANNOT BE TIED'
                  call ferr( INT2(104), 'NUTATION IN PHASE CANNOT BE TIED', &
     &                 INT2(0), INT2(0) )
               ENDIF
            ENDIF
!
            IF ( PHASE.EQ.'B' .OR. PHASE.EQ.'O')  THEN
               IF ( INUT(2,1,I).NE.0 .AND. INUT(2,2,I).NE.0 ) THEN
!
! --------------- Add information about the type of the constraint applied
!
                  CALL ADD_TYCNS ( 'NUT_CMP', 'Nutation component', &
     &                'rad', NUT_CMP_SIGMA, TRUE_L4, CNSTROBJ )
!
! --------------- Apply constraint
!
                  I1=IOFF+INUT(2,1,I)
                  I2=IOFF+INUT(2,2,I)
                  A(INDX8(I1,I1))=A(INDX8(I1,I1)) + (SINEPS**2)/NUT_CMP_SIGMA**2
                  A(INDX8(I1,I2))=A(INDX8(I1,I2)) + (-SINEPS)/NUT_CMP_SIGMA**2
                  A(INDX8(I2,I2))=A(INDX8(I2,I2)) + (1.0D0)/NUT_CMP_SIGMA**2
                ELSE
                  WRITE(*,*) I
!                 PAUSE 'NUTATION OUT OF PHASE CANNOT BE TIED'
                  call ferr( INT2(105), &
     &                'NUTATION OUT OF PHASE CANNOT BE TIED', INT2(0), INT2(0) )
               ENDIF
            ENDIF
         ENDIF
      ENDDO
!
      DO I=1,3
         LNUT(I)=KNUT(I)
      ENDDO
!
      DO I=1,14
         FLPSI(I)=KFLPSI(I)
         FLEPS(I)=KFLEPS(I)
      ENDDO
      DO I=1,7
         IDPNUT(I)=KIDPNUT(I)
      ENDDO
      NFLPSI=KNFLPSI
      NFLEPS=KNFLEPS
      NDPNUT=KNDPNUT
!
      RETURN
      END  !#!  DO_NUTAT  #!#
