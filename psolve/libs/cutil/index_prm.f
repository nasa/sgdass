      SUBROUTINE INDEX_PRM(PARMTYP,PARMIDX,NPARM,NPARAMS,IWDS,END_IDX, &
     &                     WHO,KWHO,STA)
      IMPLICIT NONE
!
      INCLUDE 'solve.i'
!
      INTEGER*2 IWDS, NPARM(IWDS,NPARAMS)
      INTEGER*4 NPARAMS
      CHARACTER*(*) PARMTYP,STA
      LOGICAL*2 KWHO
!
      INTEGER*4 PARMIDX(M_GPA),END_IDX
      CHARACTER*20 WHO(*)
!
      INTEGER*4 I, J, K
      INTEGER*2  R,LINE,IDUM(10),IARCS,IGLBLS,LENGTH, &
     &    TrimLen,INDEX
      INTEGER*4  INUM
      CHARACTER*20 PARM
      EQUIVALENCE (IDUM(1),PARM)
!
!      DO I=1,M_GPA
!          PARMIDX(I)=0
!      ENDDO
!      I=0
!
!   decide on what to do by looking at what's in PARMTYP
!
! First handle station parameters
!
      IF(PARMTYP(1:3) .eq. 'STA') THEN
          I = 1
          Do K = 1, NPARAMS
              Do R = 1, IWDS
                  IDUM(R) = NPARM(R,K)
              End do
              IF ( PARM(12:20).EQ. 'COMPONENT' ) THEN
                   PARMIDX(I) = K
                   I = I + 1
              END IF
!
              CALL CHIN ( PARM(11:16), INUM )
              IF ( PARM(17:20) .EQ. '-COO'       .AND. &
     &             ( PARM(10:10) .EQ. 'X' .OR. &
     &               PARM(10:10) .EQ. 'Y' .OR. &
     &               PARM(10:10) .EQ. 'Z'     )  .AND. &
     &               INUM .GT. 0  .AND.  INUM .LT. 999999 ) THEN
!
                  PARMIDX(I) = K
                  I = I + 1
             END IF
          End do
!
!
!
      Else IF(PARMTYP(1:3).EQ.'STX') THEN
          I=1
          DO K=1,NPARAMS
              DO R=1,IWDS
                 IDUM(R)=NPARM(R,K)
              ENDDO
              IF(INDEX(PARM,STA(1:8)//' X').NE.0) then
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=PARM
                I=I+1
              ENDIF
          ENDDO
!
! Handle continuous station position parameters (Y)
!
      Else IF(PARMTYP(1:3).EQ.'STY') THEN
          I=1
          DO K=1,NPARAMS
              DO R=1,IWDS
                 IDUM(R)=NPARM(R,K)
              ENDDO
              IF(INDEX(PARM,STA(1:8)//' Y').NE.0) then
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=PARM
                I=I+1
              ENDIF
          ENDDO
!
! Handle continuous station position parameters (Z)
!
      Else IF(PARMTYP(1:3).EQ.'STZ') THEN
          I=1
          DO K=1,NPARAMS
              DO R=1,IWDS
                 IDUM(R)=NPARM(R,K)
              ENDDO
              IF(INDEX(PARM,STA(1:8)//' Z').NE.0) then
                PARMIDX(I)=K
                IF(KWHO) WHO(I)=PARM
                I=I+1
              ENDIF
          ENDDO
!
      End if
!      END_IDX = I - 1
      RETURN
      END
