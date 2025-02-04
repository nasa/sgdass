      FUNCTION   GET_MEMRSS ()
! ************************************************************************
! *                                                                      *
! *   Function GET_MEMRSS
! *                                                                      *
! *  ### 16-JUN-2011  GET_MEMRSS  v1.0 (c)  L. Petrov  16-JUN-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*8  GET_MEMRSS
      INTEGER*4  MIND, MW_IND
      PARAMETER  ( MIND   = 128 )
      PARAMETER  ( MW_IND =  24 )
      CHARACTER  DEL*3
      PARAMETER  ( DEL = CHAR(0)//CHAR(32)//CHAR(9) )
      CHARACTER  PROC_FINAM*16, STR*256
      INTEGER*4  LIND, IND(2,MIND), PID, NP, IUER
      INTEGER*4, EXTERNAL :: GETPID, I_LEN
!
#if defined LINUX
      PID = GETPID()
      CALL INCH ( PID, PROC_FINAM )
      PROC_FINAM = '/proc/'//PROC_FINAM(1:I_LEN(PROC_FINAM))//'/stat'
!
      IUER = -1
      CALL RD_TEXT ( PROC_FINAM, 1, STR, NP, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      CALL EXWORD ( STR, MIND, LIND, IND, DEL, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      IF ( LIND < MW_IND ) THEN
           CALL ERR_LOG ( 6666, -2, 'GET_MEMRSS', 'Trap of internal '// &
     &         'control: the contents of '//PROC_FINAM// &
     &         ' is too short: less than 24 words' )
           CALL EXIT ( 1 )
      END IF
!
      CALL CHIN8 ( STR(IND(1,MW_IND):IND(2,MW_IND)), GET_MEMRSS )
      GET_MEMRSS = GET_MEMRSS*4096
#else 
      CALL ERR_LOG ( 7777, -2, 'GET_MEMRSS', 'Routine GET_MEMRSS '// &
     &    'is not implemented for your operating system' )
      CALL EXIT ( 1 )
#endif
      RETURN
      END  FUNCTION  GET_MEMRSS  !#!  
