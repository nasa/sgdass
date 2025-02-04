      SUBROUTINE UNPACK_RMPAR ( IP )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  UNPACK_RMPAR PROGRAM SPECIFICATION
!
! 1.1 Unpack the RMPAR variables into the appropriate logical flags.
!
! 1.2 REFERENCES:
!
! 2.  UNPACK_RMPAR INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IP(5)
!
! IP - The RMPAR variables
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE   'precm.i'
      LOGICAL*2 KOPEN
      COMMON/BUFFCM/KOPEN
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
!
      character*64 cbuf
      integer*2 ibuf(32)
      equivalence (ibuf,cbuf)
      INTEGER*2 I,TRIMLEN
      INTEGER*4 IERR
      LOGICAL*2 KBIT
      INTEGER*2 INT2_ARG
!
! I - Loop index
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pet   2000.07.05  Added caring of PRE_ROOT_xxx and PRE_SOL_xfx stuff
!   pet   2001.12.13  Added caring of PRE_SPL_NAM and PRE_SPL_LEN
!   aem   2003.08.25  Fixed a typo in name of the spool file for the case when
!                     SPOOL_DIR environment variable has not been defined
!
! 5.  UNPACK_RMPAR PROGRAM STRUCTURE
!
      DO I=1,5
         PRE_IP(I)=IP(I)
      ENDDO
!
      PRE_FIRST=PRE_IP(1)
      PRE_ILETRS=PRE_IP(5)
!
!  RMPAR WORD 2 BITS ARE:
!
!   1: TEST VERSION
!   2: SPOOLING
!   3: CLASS I/O BUFFER WAITING
!   4: SCHEDULED USING 'PASS'
!
      PRE_INTERA=PRE_IP(2)
      KTESTV=KBIT( PRE_INTERA, INT2(1) )
      KSPOOL=KBIT( PRE_INTERA, INT2(2) )
!
      PRE_IBATCH=PRE_IP(3)
      KBATCH=      KBIT( PRE_IBATCH, INT2(1) )
      KGLBBSL=     KBIT( PRE_IBATCH, INT2(7))
      KLCLBSL=     KBIT( PRE_IBATCH, INT2(3) )
      KGLOBALS=    KBIT( PRE_IBATCH, INT2(10))
      KBACKSL=     KBIT( PRE_IBATCH, INT2(6))
      KSCREEN=.NOT.KBIT( PRE_IBATCH, INT2(4) )
      KMINOUT=     KBIT( PRE_IBATCH, INT2(2) )
      KPOSELL=     KBIT( PRE_IBATCH, INT2(13))
      KFULLOUT=.NOT.KMINOUT
!
! --- Get WORK_DIR
!
      IERR=FC_GETENV ( PTR_CH ( 'PSOLVE_WORK_DIR'//CHAR(0) ), PTR_NC(IBUF) )
      IF ( IERR .GT. 0 ) THEN
           PRE_SCR_DIR=CBUF(1:IERR)
         ELSE
           PRE_SCR_DIR=SOLVE_WORK_DIR
      ENDIF
      PRE_SD_LEN=TRIMLEN(PRE_SCR_DIR)
      IF ( PRE_SCR_DIR(PRE_SD_LEN:PRE_SD_LEN) .NE. '/' ) THEN
           PRE_SD_LEN=PRE_SD_LEN+1
           PRE_SCR_DIR(PRE_SD_LEN:PRE_SD_LEN) = '/'
      ENDIF
!
! --- Get SAVE_DIR
!
      IERR=FC_GETENV ( PTR_CH ( 'PSOLVE_SAVE_DIR'//CHAR(0) ), PTR_NC(IBUF) )
      IF ( IERR .GT. 0 ) THEN
           PRE_SAV_DIR=CBUF(1:IERR)
         ELSE
           PRE_SAV_DIR=SOLVE_SAVE_DIR
      ENDIF
      PRE_SV_LEN=TRIMLEN(PRE_SAV_DIR)
      IF ( PRE_SAV_DIR(PRE_SV_LEN:PRE_SV_LEN) .NE. '/' ) THEN
           PRE_SV_LEN=PRE_SV_LEN+1
           PRE_SAV_DIR(PRE_SV_LEN:PRE_SV_LEN) = '/'
      ENDIF
!
! --- Get PSOLVE_ROOT direrctory
!
      IERR=FC_GETENV ( PTR_CH ( 'PSOLVE_ROOT'//CHAR(0) ), PTR_NC(IBUF) )
      IF ( IERR .GT. 0 ) THEN
           PRE_ROOT_DIR = CBUF(1:IERR)
         ELSE
           PRE_ROOT_DIR = PSOLVE_ROOT
      ENDIF
      PRE_ROOT_LEN = TRIMLEN ( PRE_ROOT_DIR )
      IF ( PRE_ROOT_DIR(PRE_ROOT_LEN:PRE_ROOT_LEN) .NE. '/' ) THEN
           PRE_ROOT_LEN=PRE_ROOT_LEN+1
           PRE_ROOT_DIR(PRE_ROOT_LEN:PRE_ROOT_LEN) = '/'
      ENDIF
!
! --- Get SOLVE_DIR direrctory
!
      IERR=FC_GETENV ( PTR_CH ( 'PSOLVE_DIR'//CHAR(0) ), PTR_NC(IBUF) )
      IF ( IERR .GT. 0 ) THEN
           PRE_SOL_DIR = CBUF(1:IERR)//'/bin'
         ELSE
           PRE_SOL_DIR = SOLVE_PROG_DIR
      ENDIF
      PRE_SOL_LEN = TRIMLEN ( PRE_SOL_DIR )
      IF ( PRE_SOL_DIR(PRE_SOL_LEN:PRE_SOL_LEN) .NE. '/' ) THEN
           PRE_SOL_LEN=PRE_SOL_LEN+1
           PRE_SOL_DIR(PRE_SOL_LEN:PRE_SOL_LEN) = '/'
      ENDIF
!
! --- Get SPOOL_DIR direrctory
!
      IERR=FC_GETENV ( PTR_CH ( 'PSOLVE_SPOOL_DIR'//CHAR(0) ), PTR_NC(IBUF) )
      IF ( IERR .GT. 0 ) THEN
           PRE_SPL_NAM = CBUF(1:IERR)
         ELSE
           PRE_SPL_NAM = SPOOL_DIR
      ENDIF
      PRE_SPL_LEN = TRIMLEN ( PRE_SPL_NAM )
      IF ( PRE_SPL_NAM(PRE_SPL_LEN:PRE_SPL_LEN) .NE. '/' ) THEN
           PRE_SPL_LEN=PRE_SPL_LEN+1
           PRE_SPL_NAM(PRE_SPL_LEN:PRE_SPL_LEN) = '/'
      ENDIF
      PRE_SPL_NAM(PRE_SPL_LEN+1:PRE_SPL_LEN+4) = 'SPLF'
      PRE_SPL_NAM(PRE_SPL_LEN+5:PRE_SPL_LEN+6) = PRE_LETRS
      PRE_SPL_LEN = PRE_SPL_LEN + 6
!
      IERR=FC_GETENV ( PTR_CH ( 'SOLVE_DBH'//CHAR(0) ), PTR_NC(IBUF) )
      IF ( IERR .GT. 0 ) THEN
           CALL TRAN ( 11, CBUF(1:IERR), CBUF(1:IERR) )
           IF ( CBUF(1:IERR) .EQ. 'DBH__GVH' .OR. &
     &          CBUF(1:IERR) .EQ. 'GVH'           ) THEN
                DBH_PROG = DBH__GVH
              ELSE 
                DBH_PROG = DBH__MK3
           END IF          
         ELSE
           DBH_PROG = DBH__MK3
      END IF
!
      TEST_FIELD = 0
!
      RETURN
      END  !#!  UNPACK_RMPAR  #!#
