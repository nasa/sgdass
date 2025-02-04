      SUBROUTINE MALO_WAVE_TABLE ( MALO, SDS_NAME, L_FRQ, IND_FRQ, IND_MOD, &
     &                             IND_HEB, NUM_CMP, L_CMP, IX_CMP, IX_FRQ, &
     &                             CMPL_AMP_FRQ, FRQ_STR, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_WAVE_TABLE 
! *                                                                      *
! * ### 23-MAY-2014  MALO_WAVE_TABLE  v3.0 (c) L. Petrov 02-JUN-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  L_FRQ, IND_MOD, IND_FRQ(MALO__MWAV), IND_HEB(MALO__MWAV), &
     &           NUM_CMP(MALO__MWAV), L_CMP, IX_FRQ(MALO__MWAV), &
     &           IX_CMP(MALO__MWAV), IVRB, IUER
      CHARACTER  SDS_NAME*(*), FRQ_STR*(*)
      COMPLEX*16 CMPL_AMP_FRQ(MALO__MWAV)
      INTEGER*4  M_BUF, MIND
      PARAMETER  ( M_BUF = MALO__MWAV*8 )
      PARAMETER  ( MIND  = 64 )
      CHARACTER  BUF(M_BUF)*256, STR*256, STR1*256, WAV_HEB(MALO__MWAV)*4, &
     &           WAVE_FRQ(MALO__MWAV)*4, REG*4, WAVE_NAME*4, C_CMP(2)*1
      PARAMETER  ( REG = CHAR(0)//CHAR(9)//CHAR(32)//',' )
      DATA       C_CMP / 'c', 's' /
      INTEGER*4  N_BUF, J1, J2, J3, J4, J5, J6, J7, J8, IND_KWO_LAST, &
     &           LIND, IND(2,MIND), L_WAV, IND_3, &
     &           IND_HEB_MAIN, IND_FRQ_MAIN, IND_KWN, IND_KWO, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, MULTI_INDEX
!
      IF ( MALO%CONF%MODEL_USE == MALO__OCEAN ) THEN
           IF ( IVRB .GE. 5 ) THEN
                WRITE ( 6, * ) 'SDS_NAME = ', TRIM(SDS_NAME)
           END IF
!
           IF ( SDS_NAME(I_LEN(SDS_NAME):I_LEN(SDS_NAME)) .NE. ',' ) THEN
                SDS_NAME = SDS_NAME(1:I_LEN(SDS_NAME))//','
           END IF
           L_FRQ = 0
           FRQ_STR = '|'
           IND_KWO_LAST = 0
           DO 410 J1=1,MOT
              IND_KWO = MULTI_INDEX ( J1, SDS_NAME, ',' )
              IF ( IND_KWO < 1 ) GOTO 810
              IF ( IND_KWO_LAST < 1 ) THEN
                   DO 420 J2=IND_KWO,1,-1
                      IF ( SDS_NAME(J2:J2) == ' ' ) THEN
                           IND_KWO_LAST = J2
                           GOTO 820
                      END IF
 420               CONTINUE 
 820               CONTINUE  
              END IF
!
              CALL CLRCH ( WAVE_NAME )
              WAVE_NAME = SDS_NAME(IND_KWO_LAST+1:IND_KWO-1) 
              CALL TRAN ( 11, WAVE_NAME, WAVE_NAME )
              L_FRQ = L_FRQ + 1                 
              IND_FRQ(L_FRQ) = LTM_DIF ( 1, MOT, OTID_WAV, WAVE_NAME )
              IF ( IND_FRQ(L_FRQ) < 1 ) THEN
                   IF ( IVRB .GE. 2 ) THEN
                        WRITE ( 6, '(A)' ) 'Warning: Unknown wage '//WAVE_NAME
                   END IF
                   L_FRQ = L_FRQ - 1
                   IND_KWO_LAST = IND_KWO
                   GOTO 410
              END IF
              NUM_CMP(L_FRQ)  = 2
              WAVE_FRQ(L_FRQ) = OTID_WAV(IND_FRQ(L_FRQ))
              CMPL_AMP_FRQ(L_FRQ) = (1.0D0, 0.0D0)
              IND_HEB(L_FRQ) = J1
              IND_HEB_MAIN = J1
              IND_FRQ_MAIN = IND_FRQ(L_FRQ)
              IF ( ILEN(WAVE_NAME) < 4 ) THEN
                   DO 430 J3=1,2
                      WAVE_NAME = SDS_NAME(IND_KWO_LAST+1:IND_KWO-1)
                      IF ( J3 == 1 ) THEN
                           WAVE_NAME = WAVE_NAME(1:I_LEN(WAVE_NAME))//'-'
                         ELSE IF ( J3 == 2 ) THEN
                           WAVE_NAME = WAVE_NAME(1:I_LEN(WAVE_NAME))//'+'
                      END IF
                      CALL TRAN ( 11, WAVE_NAME, WAVE_NAME )
!
                      IND_FRQ(L_FRQ+1) = LTM_DIF ( 1, MOT, OTID_WAV, WAVE_NAME )
                      IF ( IND_FRQ(L_FRQ+1) > 0 ) THEN
                           L_FRQ = L_FRQ + 1
                           CMPL_AMP_FRQ(L_FRQ) = &
     &                          CMPLX(OTID_AMP(IND_FRQ(L_FRQ))*DCOS(OTID_PHS(IND_FRQ(L_FRQ))), &
     &                                OTID_AMP(IND_FRQ(L_FRQ))*DSIN(OTID_PHS(IND_FRQ(L_FRQ))))/ &
     &                          CMPLX(OTID_AMP(IND_FRQ_MAIN)*DCOS(OTID_PHS(IND_FRQ_MAIN)), &
     &                                OTID_AMP(IND_FRQ_MAIN)*DSIN(OTID_PHS(IND_FRQ_MAIN)))
                           WAVE_FRQ(L_FRQ) = OTID_WAV(IND_FRQ(L_FRQ))
                           NUM_CMP(L_FRQ)  = 2
                           IND_HEB(L_FRQ)  = IND_HEB_MAIN
                      END IF 
 430               CONTINUE 
              END IF
              IND_KWO_LAST = IND_KWO
 410       CONTINUE 
 810       CONTINUE 
         ELSE
!
! -------- Land
!
           IF ( IND_MOD < 1 .OR. IND_MOD > MALO__MFS ) THEN
                CALL CLRCH ( STR  ) 
                CALL CLRCH ( STR1 ) 
                CALL INCH  ( IND_MOD,   STR  )
                CALL INCH  ( MALO__MFS, STR1 )
                CALL ERR_LOG ( 5415, IUER, 'MALO_WAVE_TABLE', 'Trap '// &
     &              'of internal control: parameter IND_MOD '//TRIM(STR)// &
     &              ' is out of range [1, '//TRIM(STR1)//']' )
                RETURN 
           END IF
!
           L_FRQ = 0
           DO 450 J5=1,MALO_HFS(IND_MOD)
              IF ( MALO_WFS(J5,IND_MOD)(6:8) == 'cos' ) THEN
                   IND_KWN = LTM_DIF ( 1, MALO__KWAV, MALO__KWS, MALO_WFS(J5,IND_MOD) )
                   IF ( IND_KWN < 1 ) THEN
                        CALL ERR_LOG ( 5416, IUER, 'MALO_WAVE_TABLE', 'Trap '// &
     &                      'of internal control: cannot find primary wave for '// &
     &                      'wave '//MALO_WFS(J5,IND_MOD) )
                        RETURN 
                   END IF
                   L_FRQ = L_FRQ + 1                 
                   IND_FRQ(L_FRQ) = LTM_DIF ( 1, MOT, OTID_WAV, MALO__KWN(IND_KWN) )
                   NUM_CMP(L_FRQ)  = 1
                   WAVE_FRQ(L_FRQ) = OTID_WAV(IND_FRQ(L_FRQ))
                   CMPL_AMP_FRQ(L_FRQ) = (1.0D0, 0.0D0)
                   IND_HEB(L_FRQ) = L_FRQ
                 ELSE IF ( MALO_WFS(J5,IND_MOD)(6:8) == 'sin' ) THEN
                   NUM_CMP(L_FRQ)  = 2
              END IF
 450       CONTINUE 
      END IF
!
      L_CMP = 0
      FRQ_STR = '|'
      DO 460 J6=1,L_FRQ
         L_CMP = L_CMP + 1
         IX_CMP(L_CMP) = 1
         IX_FRQ(L_CMP) = J6
         IF ( NUM_CMP(J6) == 2 ) THEN
              L_CMP = L_CMP + 1
              IX_CMP(L_CMP) = 2
              IX_FRQ(L_CMP) = J6
         END IF
         DO 470 J7=1,NUM_CMP(J6)
            FRQ_STR = FRQ_STR(1:I_LEN(FRQ_STR))//WAVE_FRQ(J6)// &
     &                C_CMP(J7)//'|'
 470     CONTINUE 
!
         IF ( IVRB .GE. 5 ) THEN
              WRITE ( 6, 110 ) J6, WAVE_FRQ(J6), IND_HEB(J6), IND_FRQ(J6), &
     &                         NUM_CMP(J6), CMPL_AMP_FRQ(J6)
 110          FORMAT ( I3, 2X, A, 2X, I4, 2X, I4, 2X, I1, 2X, &
     &                ' Amp: ', F9.6, 1X, F9.6 )
         END IF
 460  CONTINUE 
      IF ( IVRB .GE. 5 ) THEN
           DO 480 J8=1,L_CMP
              WRITE  ( 6, 120 ) J8, IX_FRQ(J8), IX_CMP(J8)
 120          FORMAT ( 'Cmp: ', I3, ' IX_frq: ', I3, ' IX_cmp: ', I1 )
 480       CONTINUE 
           WRITE ( 6, '("LEN(FRQ_STR)= ", I4)' ) ILEN(FRQ_STR)
           WRITE ( 6, '("FRQ_STR= ", A)' ) TRIM(FRQ_STR)
      END IF 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_WAVE_TABLE  !#!#
