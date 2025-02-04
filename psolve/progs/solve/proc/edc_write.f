      SUBROUTINE EDC_WRITE ( EDC, EDC_DIR, EDC_PAR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EDC_WRITE
! *                                                                      *
! *  ### 25-OCT-2007   EDC_WRITE   v1.0 (c)  L. Petrov  25-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'edc.i'
      TYPE     ( EDC__TYPE     ) :: EDC
      CHARACTER  EDC_DIR*(*)
      INTEGER*4  EDC_PAR, IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = MAX_OBS + MAX_ARC_STA + MAX_ARC_SRC + 512 )
      CHARACTER  FILOUT*128, STR*128
      CHARACTER*80, ALLOCATABLE :: OUT(:)
      LOGICAL*4  LEX
      INTEGER*4  J1, J2, J3, J4, IS, LUN, NR, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, WRITE
!
      IF ( EDC_PAR == EDC__BIN ) THEN
           IF ( EDC%HEA%DB_NAME(1:1) == '$' ) THEN
                FILOUT = EDC_DIR(1:I_LEN(EDC_DIR))//'/'// &
     &                   EDC%HEA%DB_NAME(2:I_LEN(EDC%HEA%DB_NAME))//'.edb'
              ELSE 
                FILOUT = EDC_DIR(1:I_LEN(EDC_DIR))//'/'// &
     &              EDC%HEA%DB_NAME(1:I_LEN(EDC%HEA%DB_NAME))//'.edb'
           END IF
!
           INQUIRE ( FILE=FILOUT, EXIST=LEX )
           IF ( LEX ) THEN
                CALL UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
           END IF
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL BINF_OPEN ( FILOUT, 'NEW', LUN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7631, IUER, 'EDC_WRITE', 'Error in an '// &
     &              'attempt to open external decimation output file '// &
     &               FILOUT )
                RETURN 
           END IF
!
           STR = EDC__BF_LABEL
#ifdef BIG_ENDIAN
           STR(34:35) = EDC__BE 
#endif
#ifdef LITTLE_ENDIAN
           STR(34:35) = EDC__LE 
#endif
!
           IS = WRITE ( %VAL(LUN), %REF(STR(1:LEN(EDC__BF_LABEL))), &
     &                  %VAL(LEN(EDC__BF_LABEL)) )
           IF ( IS .NE. LEN(EDC__BF_LABEL) ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 7632, IUER, 'EDC_WRITE', 'Error during '// &
     &              'writing the file format label record in the output '// &
     &              'external decimation file '//FILOUT(1:ILEN(FILOUT))// &
     &              ' --- '//STR )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL WRBIN_RECORD ( LUN, SIZEOF(EDC%HEA), EDC%HEA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 7633, IUER, 'EDC_WRITE', 'Error during '// &
     &              'writing the header record in the output '// &
     &              'external decimation file '//FILOUT(1:ILEN(FILOUT))// &
     &              ' --- '//STR )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL WRBIN_RECORD ( LUN, EDC%HEA%N_STA*SIZEOF(EDC%C_STA(1)), &
     &                         EDC%C_STA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 7634, IUER, 'EDC_WRITE', 'Error during '// &
     &              'writing the station record in the output '// &
     &              'external decimation file '//FILOUT(1:ILEN(FILOUT))// &
     &              ' --- '//STR )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL WRBIN_RECORD ( LUN, EDC%HEA%N_SOU*SIZEOF(EDC%C_SOU(1)), &
     &                         EDC%C_SOU, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 7635, IUER, 'EDC_WRITE', 'Error during '// &
     &              'writing the source record in the output '// &
     &              'external decimation file '//FILOUT(1:ILEN(FILOUT))// &
     &              ' --- '//STR )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL WRBIN_RECORD ( LUN, EDC%HEA%N_OBS*SIZEOF(EDC%OBS(1)), &
     &                         EDC%OBS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 7636, IUER, 'EDC_WRITE', 'Error during '// &
     &              'writing the observation record in the output '// &
     &              'external decimation file '//FILOUT(1:ILEN(FILOUT))// &
     &              ' --- '//STR )
                RETURN
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL BINF_CLOSE ( LUN,  IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 7637, IUER, 'EDC_WRITE', 'Error in '// &
     &              'an attempt to close the output external decimation '// &
     &              'file '//FILOUT(1:ILEN(FILOUT))//' --- '//STR )
                RETURN
           END IF
         ELSE 
!
! -------- Writing ASCII file
!
           IF ( EDC%HEA%DB_NAME(1:1) == '$' ) THEN
                FILOUT = EDC_DIR(1:I_LEN(EDC_DIR))//'/'// &
     &                   EDC%HEA%DB_NAME(2:I_LEN(EDC%HEA%DB_NAME))//'.eda'
              ELSE 
                FILOUT = EDC_DIR(1:I_LEN(EDC_DIR))//'/'// &
     &                   EDC%HEA%DB_NAME(1:I_LEN(EDC%HEA%DB_NAME))//'.eda'
           END IF
!
! -------- Allocate buffer
!
           ALLOCATE ( OUT(MBUF), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MBUF*SIZEOF(OUT(1)), STR )
                CALL ERR_LOG ( 7638, IUER, 'EDC_WRITE', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for a temporary text buffer' )
                RETURN 
           END IF
!
           CALL NOUT ( MBUF*SIZEOF(OUT(1)), OUT )
!
           NR = 1; OUT(NR) = EDC__AF_LABEL
           NR = 2; OUT(NR) = '#'
           NR = 3; OUT(NR) = 'DBN: '//EDC%HEA%DB_NAME
           NR = 4; OUT(NR) = 'PRC: '//EDC%HEA%PRC_NAME
           CALL ERR_PASS ( IUER, IER )
           STR = MJDSEC_TO_DATE ( EDC%HEA%MJD_CRE, EDC%HEA%TIM_CRE + 0.001D0, &
     &                            IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) ' EDC%HEA%MJD_CRE = ', EDC%HEA%MJD_CRE, &
     &                         ' EDC%HEA%TIM_CRE = ', EDC%HEA%TIM_CRE 
                CALL ERR_LOG ( 7639, IUER, 'EDC_WRITE', 'Trap of internal '// &
     &              'control: wrong values of EDC%HEA%MJD_CRE, '// &
     &              'EDC%HEA%TIM_CRE' )
                RETURN 
           END IF
           NR = 5; OUT(NR) = 'DAT_CRE: '//STR(1:21)
           NR = 6; WRITE ( OUT(NR), '(A,I6)' ) 'N_OBS: ', EDC%HEA%N_OBS
           NR = 7; WRITE ( OUT(NR), '(A,I6)' ) 'N_SCA: ', EDC%HEA%N_SCA
           NR = 8; WRITE ( OUT(NR), '(A,I6)' ) 'N_STA: ', EDC%HEA%N_STA
           NR = 9; WRITE ( OUT(NR), '(A,I6)' ) 'N_SOU: ', EDC%HEA%N_SOU
           CALL ERR_PASS ( IUER, IER )
           STR = MJDSEC_TO_DATE ( EDC%HEA%MJD_SES, EDC%HEA%TAI_SES + 0.001D0, &
     &                            IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) ' EDC%HEA%MJD_SES = ', EDC%HEA%MJD_SES, &
     &                         ' EDC%HEA%TAI_SES = ', EDC%HEA%TAI_SES 
                CALL ERR_LOG ( 7640, IUER, 'EDC_WRITE', 'Trap of internal '// &
     &              'control: wrong values of EDC%HEA%MJD_CRE, '// &
     &              'EDC%HEA%TIM_CRE' )
                RETURN 
           END IF
!
           NR = 10; OUT(NR) = 'DAT_SES: '//STR(1:21)
           NR = 11; OUT(NR) = '#'
!
           DO 410 J1=1,EDC%HEA%N_STA
              NR = NR+1; WRITE ( OUT(NR), '(A,I4,2X,A)' ) 'C_STA: ', J1, &
     &                                                    EDC%C_STA(J1)
 410       CONTINUE 
           NR = NR+1; OUT(NR) = '#'
!
           DO 420 J2=1,EDC%HEA%N_SOU
              NR = NR+1; WRITE ( OUT(NR), '(A,I4,2X,A)' ) 'C_SOU: ', J2, &
     &                                                     EDC%C_SOU(J2)
 420       CONTINUE 
           NR = NR+1; OUT(NR) = '#'
!
           DO 430 J3=1,EDC%HEA%N_OBS
              CALL ERR_PASS ( IUER, IER )
              STR = MJDSEC_TO_DATE ( EDC%HEA%MJD_SES, EDC%HEA%TAI_SES + &
     &                               EDC%OBS(J3)%TIM_OBS + 0.001D0, &
     &                               IER )
              IF ( IER .NE. 0 ) THEN
                   WRITE ( 6, * ) ' Obs: ', J3, &
     &                            ' EDC%HEA%MJD_SES = ', EDC%HEA%MJD_SES, &
     &                            ' EDC%HEA%TAI_SES = ', EDC%HEA%TAI_SES, &
     &                            ' TIM_OBS = ', EDC%OBS(J3)%TIM_OBS
                   CALL ERR_LOG ( 7641, IUER, 'EDC_WRITE', 'Trap of '// &
     &                 'internal control: failure in date transform for '// &
     &                 ' the '//STR(1:I_LEN(STR))//'-th observation' )
                   RETURN 
              END IF
              NR = NR + 1
              WRITE ( UNIT=OUT(NR), FMT=110 ) J3, STR(1:21), &
     &                              EDC%C_STA(EDC%OBS(J3)%IND_STA(1)), &
     &                              EDC%C_STA(EDC%OBS(J3)%IND_STA(2)), &
     &                              EDC%C_SOU(EDC%OBS(J3)%IND_SOU), &
     &                              EDC%OBS(J3)%SUP_STS, &
     &                              EDC%OBS(J3)%DCM_STS
 110          FORMAT ( 'Obs: ',I6,2X,A,2X,A,1X,A,2X,A,2X,I4,2X,I4 )
 430       CONTINUE 
           NR = NR+1; OUT(NR) = '#'
           NR = NR+1; OUT(NR) = EDC__AF_LABEL
!
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT  ( NR, OUT, FILOUT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7642, IUER, 'EDC_WRITE', 'Error in an '// &
     &              'attempt to write output external decimation file '// &
     &              'in ascii format '//FILOUT )
                RETURN 
           END IF
!
           DEALLOCATE ( OUT )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EDC_WRITE  !#!#
