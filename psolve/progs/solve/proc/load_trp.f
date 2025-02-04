      SUBROUTINE LOAD_TRP ( TRP_USE, TRP_DIR, DBNAME_CH, N_FIL_TRP, &
     &                      TRP_FIL_BUF, STS_TRP_FIL, TRP_SES_BUF, &
     &                      STS_TRP_SES, TRP, STS_TRP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine LOAD_TRP
! *                                                                      *
! *  ### 08-FEB-2008    LOAD_TRP   v1.0 (c)  L. Petrov  08-FEB-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'trp.i'
      INTEGER*4  TRP_USE, N_FIL_TRP, STS_TRP_FIL, STS_TRP_SES, &
     &           STS_TRP, IUER
      CHARACTER  TRP_FIL_BUF(N_FIL_TRP)*128, TRP_SES_BUF(N_FIL_TRP)*10
      CHARACTER  TRP_DIR*128, DBNAME_CH*10, TRP_FUDGE_STR*128
      TYPE      ( TRP__TYPE ) :: TRP
      LOGICAL*4  LEX
      CHARACTER  FINAM*128
      INTEGER*4  J1, J2, IND, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IFIND_SORT_CH
!
!@  write ( 6,* ) 'load_trp 23 dbname_ch = ', dbname_ch ! %%%%%%
      IF ( ASSOCIATED(TRP%C_SCA) ) DEALLOCATE ( TRP%C_SCA )
      IF ( ASSOCIATED(TRP%STA)   ) DEALLOCATE ( TRP%STA   )
      IF ( ASSOCIATED(TRP%SCA)   ) THEN
           IF ( TRP%N_SCA > 0 ) THEN
                DO 410 J1=1,TRP%N_SCA
                   IF ( ASSOCIATED ( TRP%SCA(J1)%DAT ) ) THEN
                        DEALLOCATE ( TRP%SCA(J1)%DAT )
                   END IF
                   IF ( ASSOCIATED ( TRP%SCA(J1)%IND_STA ) ) THEN
                        DEALLOCATE ( TRP%SCA(J1)%IND_STA )
                   END IF
 410            CONTINUE
           END IF
           DEALLOCATE ( TRP%SCA   )
      END IF
      STS_TRP = ALLO__TRP
!
      IF ( ILEN(TRP_DIR) == 0 ) THEN
           CALL ERR_LOG ( 8241, IUER, 'LOAD_TRP', 'Trap of internal '// &
     &         'control: TRP_DIR variable is empty' )
           RETURN
      END IF
!
      FINAM = TRP_DIR(1:I_LEN(TRP_DIR))//'/'//DBNAME_CH(1:I_LEN(DBNAME_CH))//'.trp'
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
            FINAM = TRP_DIR(1:I_LEN(TRP_DIR))//'/'//DBNAME_CH(1:I_LEN(DBNAME_CH))//'.spm'
      END IF
!
      IF ( .NOT. LEX ) THEN
           IF ( STS_TRP_FIL .NE. LOAD__TRP ) THEN
                CALL ERR_LOG ( 8242, IUER, 'LOAD_TRP', 'Trap of internal '// &
     &              'control: the directory TRP_DIR was not read into the '// &
     &              'buffer' )
                RETURN
           END IF
           IF ( STS_TRP_SES .NE. LOAD__TRP ) THEN
                CALL ERR_LOG ( 8243, IUER, 'LOAD_TRP', 'Trap of internal '// &
     &              'control: the list of VLBI names that correspond to '// &
     &              'the files with external tropospheric path delay was '// &
     &              'not found' )
                RETURN
           END IF
!
           IND = IFIND_SORT_CH ( N_FIL_TRP, TRP_SES_BUF, DBNAME_CH )
           IF ( IND < 1 ) THEN
                IF ( TRP_USE == REQ__TRP ) THEN
                     CALL ERR_LOG ( 8244, IUER, 'LOAD_TRP', 'Cannot find '// &
     &                   'external file with tropospheric path delay model '// &
     &                   'for the database '//DBNAME_CH )
                     RETURN
                   ELSE
                     CALL ERR_LOG ( 0, IUER )
                     RETURN
                END IF
           END IF
!
! -------- Check whether the file exists
!
           FINAM = TRP_FIL_BUF(IND)
           INQUIRE ( FILE=FINAM, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 8245, IUER, 'LOAD_TRP', 'Cannot find '// &
     &              'the external file with atmospheric path delay '// &
     &               FINAM(1:I_LEN(FINAM))//' that corresponds to '// &
     &              'the database '//DBNAME_CH )
                RETURN
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_TRP ( FINAM, TRP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8246, IUER, 'LOAD_TRP', 'Error in attempt to '// &
     &         'read and parse the external file with atmospheric path '// &
     &         'delay '//FINAM )
           RETURN
      END IF
!
      CALL GETENVAR ( 'TRP_FUDGE', TRP_FUDGE_STR )
      IF ( ILEN(TRP_FUDGE_STR) > 0 ) THEN
           CALL ERR_PASS  ( IUER, IER )
           CALL TRP_FUDGE ( TRP, TRP_FUDGE_STR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8247, IUER, 'LOAD_TRP', 'Error in attempt '// &
     &              'to apply station-depednedn fudge factor to the '// &
     &              'troposphere path delay' )
           RETURN
      END IF
      END IF
!
      STS_TRP = LOAD__TRP
!@  write ( 6,* ) 'load_trp 103  STS_TRP = ', STS_TRP 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE LOAD_TRP  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TRP_FUDGE ( TRP, FUDGE_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine TRP_FUDGE 
! *                                                                      *
! *  ### 08-APR-2008   TRP_FUDGE   v1.0 (c)  L. Petrov  08-APR-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'trp.i'
      TYPE      ( TRP__TYPE ) :: TRP
      CHARACTER  FUDGE_FILE*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 1024 )
      CHARACTER  BUF(MBUF)*128
      CHARACTER  C_STA(MBUF)*8
      REAL*8     FUDGE(MBUF)
      INTEGER*4  J1, J2, J3, J4, J5, NBUF, IER
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FUDGE_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8261, IUER, 'TRP_FUDGE', 'Error in an attempt '// &
     &         'to read the file with trp fudge factors '//FUDGE_FILE )
           RETURN 
      END IF
!
      CALL NOUT_R8 ( MBUF, FUDGE )
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         DO 420 J2=1,TRP%N_STA
            IF ( BUF(J1)(1:8) == TRP%STA(J2)%NAME ) THEN
                 READ ( UNIT=BUF(J1)(42:50), FMT='(F9.5)' ) FUDGE(J2)
!@                 READ ( UNIT=BUF(J1)(67:75), FMT='(F)' ) FUDGE(J2)
!@  write ( 6, * ) 'trp_fudge j2= ', j2, ' name: ', TRP%STA(J2)%NAME, ' fudge = ', fudge(j2) ! %%%%%%%%
!@                 FUDGE(J2) = -0.004D0
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      DO 430 J3=1,TRP%N_SCA
         DO 440 J4=1,TRP%SCA(J3)%L_STA
!@            TRP%SCA(J3)%DAT(J4)%DEL_TOT_SLANT = TRP%SCA(J3)%DAT(J4)%DEL_TOT_SLANT* &
!@!@     &            (1.0D0 - 1.8D-3) 
!@     &            (1.0D0 - 5.4D-3) 
!@            TRP%SCA(J3)%DAT(J4)%DEL_TOT_SLANT = &
!@     &            TRP%SCA(J3)%DAT(J4)%DEL_TOT_SLANT*(1.0D0 - 1.8D-3)
 440     CONTINUE 
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TRP_FUDGE  !#!#
