      SUBROUTINE GVH_SKIP_BIN ( FILDES, SKIP_BYTES, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine GVH_SKIP_BIN 
! *                                                                      *
! *  ### 04-NOV-2005  GVH_SKIP_BIN  v1.0 (c) L. Petrov  04-NOV-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  FILDES, SKIP_BYTES, IUER
      INTEGER*4  VAL_CNST, LEN_CNST, IS
      CHARACTER  STR*64, STR1*32
      INTEGER*4, EXTERNAL ::  I_LEN, LSEEK
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_CUR', VAL_CNST, LEN_CNST ) 
      IF ( SKIP_BYTES > 0 ) THEN
           IS = LSEEK ( %VAL(FILDES), %VAL(SKIP_BYTES), %VAL(VAL_CNST) )
           IF ( IS .EQ. SKIP_BYTES-1 ) THEN
                CALL GERROR ( STR  )
                CALL CLRCH  ( STR1 )
                CALL INCH   ( SKIP_BYTES, STR1 )
                CALL ERR_LOG ( 4271, IUER, 'GVH_SKIP_BIN', 'Error in skippig '// &
          &          STR1(1:I_LEN(STR1))//' bytes: '//STR )
                RETURN 
           END IF
      END IF
!     
!@         write ( 6, * ) ' skipped: ', skip_bytes, ' is=',is ! %%%
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVH_SKIP_BIN  !#!#
