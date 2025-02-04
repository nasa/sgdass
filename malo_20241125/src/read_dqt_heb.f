      SUBROUTINE READ_DQT_HEB ( HEB_DIR, DATE_HEB, HEB_DELP, HEB_Q, &
     &                          HEB_T, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_HEB_DQT 
! *                                                                      *
! *  ### 15-FEB-2013  READ_HEB_DQT  v1.1 (c)  L. Petrov 07-FEB-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_DELP, HEB_Q, HEB_T
      CHARACTER  HEB_DIR*(*), DATE_HEB*(*)
      CHARACTER  FINAM_TEMP*128, FINAM_DELP*128, FINAM_Q*128, FINAM_T*128
      CHARACTER  FIL_TEMP*128,   FIL_DELP*128,   FIL_Q*128,   FIL_T*128
      CHARACTER  TMP_DIR*128, COMPR_COM*128, TEST_STR*8
      INTEGER*4  IUER
      CHARACTER  INTERNET_HOSTNAME*128, STR*128
      LOGICAL*1  LEX
      INTEGER*4  IS, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, SYSTEM
!
      TEST_STR = 'none'         
!          
      CALL GETINFO_HOST ( INTERNET_HOSTNAME )
      IF ( INTERNET_HOSTNAME(1:8)  == 'astrogeo'                   .OR. &
     &     INTERNET_HOSTNAME(1:13) == 'earthrotation'              .OR. &
     &     INTERNET_HOSTNAME(1:5)  == 'terra'                      .OR. &
     &     INTERNET_HOSTNAME(1:26) == 'gs61a-sagitta.ndc.nasa.gov' .OR. &
     &     INTERNET_HOSTNAME(1:24) == 'gs61a-crux.gsfc.nasa.gov'   .OR. &
     &     INTERNET_HOSTNAME(1:14) == 'gs61a-geodev-a'                  ) THEN
           TMP_DIR = '/dev/shm'
         ELSE 
           TMP_DIR = '/tmp'
      END IF
!
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF
!
! --- Read DELP-file
!
      FINAM_DELP = HEB_DIR(1:I_LEN(HEB_DIR))//'/'//DATE_HEB(1:4)// &
     &            '/d/d_'//DATE_HEB(1:I_LEN(DATE_HEB))//'.heb.bz2'
      FIL_DELP   = TMP_DIR(1:I_LEN(TMP_DIR))//'/d_'// &
     &             DATE_HEB(1:I_LEN(DATE_HEB))//'.heb'
!
! --- Uncomppress command
!
      INQUIRE ( FILE=FINAM_DELP, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           FINAM_DELP = HEB_DIR(1:I_LEN(HEB_DIR))//'/'//DATE_HEB(1:4)// &
     &                 '/d/d_'//DATE_HEB(1:I_LEN(DATE_HEB))//'.heb'
      END IF
      IF ( INDEX( FINAM_DELP, '.bz2' ) > 0 ) THEN
           COMPR_COM = 'lbzip2 -n1 -dfc '//FINAM_DELP(1:I_LEN(FINAM_DELP))// &
     &                 ' > '//FIL_DELP
         ELSE 
           COMPR_COM = 'cp '//FINAM_DELP(1:I_LEN(FINAM_DELP))//' '//FIL_DELP
      END IF
!
      IS = SYSTEM ( COMPR_COM(1:I_LEN(COMPR_COM))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           WRITE ( 6, * ) 'COMPR_COM: '//COMPR_COM(1:I_LEN(COMPR_COM))
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4681, IUER, 'READ_DQT_HEB', 'Error in an attempt '// &
     &         'to decompress file '//FINAM_DELP(1:I_LEN(FINAM_DELP))// &
     &         ' to '//FIL_DELP(1:I_LEN(FIL_DELP))//' -- '//STR )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB_HEADER ( FIL_DELP, HEB_DELP, IER )
      IF ( IER  .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4682, IUER, 'READ_DQT_HEB', 'Error in reading '// &
     &         'heb-file '//FIL_DELP )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB_DATA ( FIL_DELP, HEB_DELP, IER )
      IF ( IER  .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4683, IUER, 'READ_DQT_HEB', 'Error in reading '// &
     &         'heb-file '//FIL_DELP )
           RETURN 
      END IF
!
      IS = UNLINK ( FIL_DELP(1:I_LEN(FIL_DELP))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4684, IUER, 'READ_DQT_HEB', 'Error in an attempt '// &
     &         'to remove temporary heb-file '//FIL_DELP(1:I_LEN(FIL_DELP))// &
     &         ' -- '//STR )
           RETURN 
      END IF
!
! --- Read Q-file
!
      FINAM_Q = HEB_DIR(1:I_LEN(HEB_DIR))//'/'//DATE_HEB(1:4)// &
     &          '/q/q_'//DATE_HEB(1:I_LEN(DATE_HEB))//'.heb.bz2'
      FIL_Q   = TMP_DIR(1:I_LEN(TMP_DIR))//'/q_'// &
     &          DATE_HEB(1:I_LEN(DATE_HEB))//'.heb'
      INQUIRE ( FILE=FINAM_Q, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           FINAM_Q = HEB_DIR(1:I_LEN(HEB_DIR))//'/'//DATE_HEB(1:4)// &
     &                 '/q/q_'//DATE_HEB(1:I_LEN(DATE_HEB))//'.heb'
      END IF
      IF ( INDEX( FINAM_Q, '.bz2' ) > 0 ) THEN
            COMPR_COM = 'lbzip2 -n1 -dfc '//FINAM_Q(1:I_LEN(FINAM_Q))// &
     &            ' > '//FIL_Q
         ELSE 
           COMPR_COM = 'cp '//FINAM_Q(1:I_LEN(FINAM_Q))//' '//FIL_Q
      END IF
      IS = SYSTEM ( COMPR_COM(1:I_LEN(COMPR_COM))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           WRITE ( 6, * ) 'COMPR_COM: '//COMPR_COM(1:I_LEN(COMPR_COM))
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4685, IUER, 'READ_DQT_HEB', 'Error in an attempt '// &
     &         'to decompress file '//FINAM_Q(1:I_LEN(FINAM_Q))// &
     &         ' to '//FIL_Q(1:I_LEN(FIL_Q))//' -- '//STR )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB_HEADER ( FIL_Q, HEB_Q, IER )
      IF ( IER  .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4686, IUER, 'READ_DQT_HEB', 'Error in reading '// &
     &         'heb-file '//FIL_Q )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB_DATA ( FIL_Q, HEB_Q, IER )
      IF ( IER  .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4687, IUER, 'READ_DQT_HEB', 'Error in reading '// &
     &         'heb-file '//FIL_Q )
           RETURN 
      END IF
!
      IS = UNLINK ( FIL_Q(1:I_LEN(FIL_Q))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4688, IUER, 'READ_DQT_HEB', 'Error in an attempt '// &
     &         'to remove temporary heb-file '//FIL_Q(1:I_LEN(FIL_Q))// &
     &         ' -- '//STR )
           RETURN 
      END IF
!
! --- Read T-file
!
      FINAM_T = HEB_DIR(1:I_LEN(HEB_DIR))//'/'//DATE_HEB(1:4)// &
     &          '/t/t_'//DATE_HEB(1:I_LEN(DATE_HEB))//'.heb.bz2'
      FIL_T   = TMP_DIR(1:I_LEN(TMP_DIR))//'/t_'// &
     &          DATE_HEB(1:I_LEN(DATE_HEB))//'.heb'
      INQUIRE ( FILE=FINAM_T, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           FINAM_T = HEB_DIR(1:I_LEN(HEB_DIR))//'/'//DATE_HEB(1:4)// &
     &                 '/t/t_'//DATE_HEB(1:I_LEN(DATE_HEB))//'.heb'
      END IF
      IF ( INDEX( FINAM_T, '.bz2' ) > 0 ) THEN
            COMPR_COM = 'lbzip2 -n1 -dfc '//FINAM_T(1:I_LEN(FINAM_T))// &
     &            ' > '//FIL_T
         ELSE 
            COMPR_COM = 'lbzip2 -n1 -dfc '//FINAM_T(1:I_LEN(FINAM_T))// &
     &                  ' > '//FIL_T
      END IF
      IS = SYSTEM ( COMPR_COM(1:I_LEN(COMPR_COM))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4689, IUER, 'READ_DQT_HEB', 'Error in an attempt '// &
     &         'to decompress file '//FINAM_T(1:I_LEN(FINAM_T))// &
     &         ' to '//FIL_T(1:I_LEN(FIL_T))//' -- '//STR )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB_HEADER ( FIL_T, HEB_T, IER )
      IF ( IER  .NE. 0 ) THEN
           WRITE ( 6, * ) 'COMPR_COM: '//COMPR_COM(1:I_LEN(COMPR_COM))
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4690, IUER, 'READ_DQT_HEB', 'Error in reading '// &
     &         'heb-file '//FIL_T )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB_DATA ( FIL_T, HEB_T, IER )
      IF ( IER  .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4691, IUER, 'READ_DQT_HEB', 'Error in reading '// &
     &         'heb-file '//FIL_T )
           RETURN 
      END IF
!
      IS = UNLINK ( FIL_T(1:I_LEN(FIL_T))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4692, IUER, 'READ_DQT_HEB', 'Error in an attempt '// &
     &         'to remove temporary heb-file '//FIL_T(1:I_LEN(FIL_T))// &
     &         ' -- '//STR )
           RETURN 
      END IF
!
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR ) 
           WRITE ( 6, '(A)' ) 'Read and uncompress DQT-files:    '//STR(1:I_LEN(STR)-5)
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_DQT_HEB  !#!  
