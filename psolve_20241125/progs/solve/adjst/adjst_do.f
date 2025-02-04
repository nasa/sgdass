      SUBROUTINE ADJST_DO ( NP, ARR, LBUF_LEN, LBUF, IPTR, PAGEWID, &
     &                      CNSTROBJ, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'precm.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'fast.i'
      INCLUDE   'cnstr.i'
!
      INTEGER*4   NP
      REAL*8      ARR(*)
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID, IUER
      CHARACTER   LBUF(LBUF_LEN)*120
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
!
      LOGICAL*2 KSRC, KCONS, SITES_ESTIMATED
      REAL*8    SCSIG(M_GPA), OVRTRA_ATM, OVRTRA_CLK, OVRTRA_GRD, EOPTRACE
      COMMON  / SCLSIG / SCSIG
      INTEGER*4 M_SAV, L_SAV
      PARAMETER ( M_SAV = M_GPA*6 )
      ADDRESS__TYPE :: ADR_SAV(M_SAV)
      INTEGER*4  J1, IER
      REAL*8     ARR_VEC1(M_GPA), ARR_VEC2(M_GPA), ARR_VEC3(M_GPA), &
     &           VAL_SAV(M_SAV), TOTAL_SITEC(3,MAX_STA)
      REAL*8     APR_VAL(N__SNX), APR_SIG(N__SNX), EST_VAL(N__SNX)
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 4.  HISTORY
!   WHO  WHEN       WHAT
!   pet  09-MAR-99  Created using former ../adjst/adjst.f
!   pet  12-MAR-99  Added saving/restoring array ARR. Upon completion of work
!                   array ARR is restored to its original state. It makes
!                   ADJST_DO reenterable.
!   pet  05-APR-99  Added call of TIM_GET
!   pet  1999.05.28 Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments
!   pet  2002.03.19 Added parameters CNSTROBJ, IUER
!   pet  2002.04.08 Moved call of WRITE_SINEX from A1JST routine
!   pet  2002.05.31 Added parameter TOTAL_SITEC for carrying total site
!                   positions
!   pet  2007.10.31 Fixed a long-standing bug related to WRITE_SINEX: &
!                   socom_est status should be restored when WRITE_SINEX
!                   is processing global solution, because the following 
!                   processing of the next database need a refresh
!
!CCC
!
! --- Save three starting vectors located in area ARR since further
! --- procedure may spoil it
!
      CALL COPY_V ( M_GPA, ARR(1),         ARR_VEC1 )
      CALL COPY_V ( M_GPA, ARR(1+M_GPA),   ARR_VEC2 )
      CALL COPY_V ( M_GPA, ARR(1+2*M_GPA), ARR_VEC3 )
!
! --- Initialization
!
      EOPTRACE   = 0.0
      KCONS      = .FALSE.
      NPARAM     = NP
      CALL NOUT_R8 ( N__SNX, APR_VAL )
      CALL NOUT_R8 ( N__SNX, APR_SIG )
      CALL NOUT_R8 ( N__SNX, EST_VAL )
!
! --- Set timer
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_INIT()
      END IF
!
! --- Handle parameters up through sources
!
      L_SAV = 0
      CALL ERR_PASS ( IUER, IER )
      CALL A1JST ( SCSIG, KSRC, OVRTRA_ATM, OVRTRA_CLK, OVRTRA_GRD, KCONS, &
     &     SITES_ESTIMATED, ARR, M_SAV, L_SAV, ADR_SAV, VAL_SAV, TOTAL_SITEC, &
     &     LBUF_LEN, LBUF, IPTR, PAGEWID, CNSTROBJ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3741, IUER, 'ADJST_DO', 'Errors in A1JST' )
           RETURN
      END IF
!
! --- Handle paramters after sources
!
      CALL ERR_PASS ( IUER, IER )
      CALL A2JST ( SCSIG, EOPTRACE, KCONS, ARR, LBUF_LEN, LBUF, IPTR, &
     &             PAGEWID, APR_VAL, APR_SIG, EST_VAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3742, IUER, 'ADJST_DO', 'Errors in A2JST' )
           RETURN
      END IF
!
! --- Restore three starting vectors located in areas ARR since above
! --- procedure may spoiled it
!
      CALL COPY_V ( M_GPA, ARR_VEC1, ARR(1)         )
      CALL COPY_V ( M_GPA, ARR_VEC2, ARR(1+M_GPA)   )
      CALL COPY_V ( M_GPA, ARR_VEC3, ARR(1+2*M_GPA) )
!
      IF ( L_SAV .GT. 0 ) THEN
!
! -------- Restoration of some elements of covariance matrix which were
! -------- changed during processing. The triciky point is that we have to
! -------- restore elements in reverse order.
!
           DO 410 J1=L_SAV,1,-1
              CALL COPY_V ( 1, VAL_SAV(J1), %VAL(ADR_SAV(J1)) )
 410       CONTINUE
      END IF
!
! --- Do final cleanup
!
      CALL ERR_PASS ( IUER, IER )
      CALL A3JST ( KSRC, OVRTRA_ATM, OVRTRA_CLK, EOPTRACE, KCONS, &
     &             SITES_ESTIMATED, OVRTRA_GRD, ARR, TOTAL_SITEC, LBUF_LEN, &
     &             LBUF, IPTR, PAGEWID, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3743, IUER, 'ADJST_DO', 'Errors in A3JST' )
           RETURN
      END IF
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_GET ( 'ADJST' )
      END IF
!
      IF ( FL_SINEX_MAKE ) THEN
!
! -------- Generate listing in SINEX format
!
           IF ( FAST_DBG .EQ. F__APP .OR.  FAST_DBG .EQ. F__PRI ) THEN
               IF ( FL_SINEX_GLO ) WRITE ( 6, * ) '  ADJST: started write_sinex'
           END IF
!
           CALL SOCOM_EXT()
           CALL ERR_PASS ( IUER, IER )
           CALL WRITE_SINEX ( KGLOBALS, CNSTROBJ, ARR(3*M_GPA+1), &
     &                        ARR(2*M_GPA+1), ARR(1*M_GPA+1), APR_VAL, &
     &                        APR_SIG, EST_VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3744, IUER, 'ADJST_DO', 'Error in '// &
     &              'generating listing in SINEX format' )
                RETURN
           END IF
           IF ( FAST_DBG .EQ. F__APP .OR.  FAST_DBG .EQ. F__PRI ) THEN
                IF ( FL_SINEX_GLO ) WRITE ( 6, * ) '  ADJST: ended write_sinex'
           END IF
           IF ( KGLOBALS ) SOCOM_PLUS_FIRST = SPL__UNDF
      ENDIF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ADJST_DO  #!#
