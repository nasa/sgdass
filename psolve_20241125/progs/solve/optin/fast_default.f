      SUBROUTINE FAST_DEFAULT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Initialisation FAST parameters for  interactide SOLVE.             *
! *                                                                      *
! *  ###  08-AUG-97  FAST_DEFAULT  v1.0  (c)  L. Petrov  08-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'fast.i'
      INTEGER*4  IUER, J1, J2, ILEN, I_LEN
      CHARACTER  STR*20
!
! --- Initial setup of FAST_MODE
!
      FAST_MODE = FAST_MODE__DEF
      FAST_DBG  = FAST_DBG__DEF
      FAST_COV  = FAST_COV__DEF_I
!
! --- Examining FAST_MODE
!
      CALL CLRCH  (              STR )
      CALL GETENVAR ( 'FAST_MODE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           FAST_MODE = F__UND
           DO 410 J1=1,FMI_VAR
              IF ( STR(1:3) .EQ. FM_ABR(J1) ) FAST_MODE = FM_VAL(J1)
 410       CONTINUE
           IF ( FAST_MODE .EQ. F__UND ) THEN
                CALL ERR_LOG ( 6811, IUER, 'FAST_DEFAULT', 'Environment '// &
     &              'variable FAST_MODE has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining FAST_DBG
!
      CALL CLRCH  (             STR )
      CALL GETENVAR ( 'FAST_DBG', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           FAST_DBG = F__UND
           DO 420 J2=1,FDI_VAR
              IF ( STR(1:3) .EQ. FD_ABR(J2) ) FAST_DBG = FD_VAL(J2)
 420       CONTINUE
           IF ( FAST_DBG .EQ. F__UND ) THEN
                CALL ERR_LOG ( 6812, IUER, 'FAST_DEFAULT', 'Environment '// &
     &              'variable FAST_DBG has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
      FAST_MODE_GLO = FAST_MODE
      FAST_DBG_GLO  = FAST_DBG
      FAST_COV_GLO  = FAST_COV
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  FAST_DEFAULT  #!#
