      PROGRAM BATCH_MAIN
! ************************************************************************
! *                                                                      *
! *   Main program for calling BATCH. There are many options due to      *
! *   historic reasons. There are two major use cases:                   *
! *   
! *   a) BATCH 1 0 0 0 INITIALS  control_file -- start solution anew.    *
! *   b) BATCH 0 0 0 0 INITIALS  control_file -- restart interrupted     *
! *      solution if it is possible. If restarting solution is not       *
! *      possible, the solution is started from the very beginning.      *
! *                                                                      *
! *  ### 30-APR-2020  BATCH_MAIN   v2.0 (c)  L. Petrov  01-OCT-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'ba2cm.i'
      INCLUDE   'precm.i'
      INTEGER*4  START_ANEW, ASK_RESTART
      CHARACTER  STR*54, INIT_LETS*2
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      CALL PRE_PROG()
      STRING = ' '
      IF ( IARGC() .GE. 6 ) THEN
           PRE_IP = 0
           CALL GETARG ( 1,   STR )
           CALL CHIN   ( STR, START_ANEW )
           CALL GETARG ( 2,   STR )
           CALL CHIN   ( STR, ASK_RESTART )
           CALL GETARG ( 5, INIT_LETS )
           CALL TRAN ( 11, INIT_LETS, INIT_LETS )
           CALL GETARG ( 6, STRING )
!
           IF ( START_ANEW == 1 ) THEN
!
! ------------- Do not try to recover, even if it is possible
!
                CALL SBIT ( PRE_IP(2), INT2(8), INT2(1) )
              ELSE 
!
! ------------- Try to recover, if it is possible
!
                CALL SBIT ( PRE_IP(2), INT2(8), INT2(0)  )
           END IF
!
           IF ( ASK_RESTART == 1 ) THEN
!
! ------------- Ask user, whether he wants to recover or not
!
                CALL SBIT ( PRE_IP(2), INT2(7), INT2(0) )
              ELSE 
!
! ------------- Do not ask user about recovery: just do it if it is possible
!
                CALL SBIT ( PRE_IP(2), INT2(7), INT2(1)  )
           END IF
           CALL SBIT ( PRE_IP(2), INT2(6), INT2(0) ) ! Set bit 6 of word 2 to 0
!                                                    ! to indicate background
           CALL SBIT ( PRE_IP(3), INT2(1), INT2(1)  )
           STRING = 'RU,BAT'//INIT_LETS//','//INIT_LETS//',0,1,1,'//TRIM(STRING)//','
           CALL MAKE_PIPES()
         ELSE
           CALL USE_BUFFER  ( ISTRNG, INT2(64), 'ORC' )
      END IF
      CALL BATCH()
!
! --- Print farewell message and putting a message to the
! --- status and timing files
!
      CALL DBG_END()
      CALL REMOVE_SOLVE_LOCK()  
      CALL END_PROG()
!
      END PROGRAM BATCH_MAIN
