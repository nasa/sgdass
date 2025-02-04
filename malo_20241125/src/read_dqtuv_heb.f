      SUBROUTINE READ_DQTUV_HEB ( HEB_FILE, HEB_D, HEB_Q, HEB_T, HEB_U, &
     &                            HEB_V, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_HEB_DQTUV 
! *                                                                      *
! *  ### 04-NOV-2014 READ_HEB_DQTUV v1.1 (c)  L. Petrov 05-AUG-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_D, HEB_Q, HEB_T, HEB_U, HEB_V
      CHARACTER  HEB_FILE*(*)
      INTEGER*4  IUER
      CHARACTER  FIL_D*128, FIL_Q*128, FIL_T*128, FIL_U*128, FIL_V*128
      CHARACTER  STR*128, TEST_STR*128
      INTEGER*4  IS, IL, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      TEST_STR = 'none'         
!
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( %VAL(0) ) 
      END IF
!
! --- Read DELP-file with pressure thinkness
!
      IL = ILEN(HEB_FILE)
      IF ( HEB_FILE(IL-3:IL) == '.bz2' ) THEN
           IF ( HEB_FILE(IL-21:IL-21) == '+' ) THEN
                FIL_D = HEB_FILE(1:IL-38)//'/d/d_'//HEB_FILE(IL-32:IL-8)//'.heb.bz2'
              ELSE
                FIL_D = HEB_FILE(1:IL-25)//'/d/d_'//HEB_FILE(IL-20:IL-8)//'.heb.bz2'
           END IF
         ELSE 
           FIL_D = HEB_FILE(1:IL-21)//'/d/d_'//HEB_FILE(IL-16:IL-4)//'.heb.bz2'
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FIL_D, HEB_D, IER )
      IF ( IER  .NE. 0 ) THEN
           CALL ERR_LOG ( 4631, IUER, 'READ_DQTUV_HEB', 'Error in reading '// &
     &         'pressure thickness heb-file '//FIL_D )
           RETURN 
      END IF
!
! --- Read Q-file with specific humidity
!
      IF ( HEB_FILE(IL-3:IL) == '.bz2' ) THEN
           IF ( HEB_FILE(IL-21:IL-21) == '+' ) THEN
                FIL_Q = HEB_FILE(1:IL-38)//'/q/q_'//HEB_FILE(IL-32:IL-8)//'.heb.bz2'
              ELSE
                FIL_Q = HEB_FILE(1:IL-25)//'/q/q_'//HEB_FILE(IL-20:IL-8)//'.heb.bz2'
           END IF
         ELSE 
           FIL_Q = HEB_FILE(1:IL-21)//'/q/q_'//HEB_FILE(IL-16:IL-4)//'.heb.bz2'
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FIL_Q, HEB_Q, IER )
      IF ( IER  .NE. 0 ) THEN
           CALL ERR_LOG ( 4632, IUER, 'READ_DQTUV_HEB', 'Error in reading '// &
     &         'specifi humidity heb-file '//FIL_Q )
           RETURN 
      END IF
!
! --- Read T-file with air temperature
!
      IF ( HEB_FILE(IL-3:IL) == '.bz2' ) THEN
           IF ( HEB_FILE(IL-21:IL-21) == '+' ) THEN
                FIL_T = HEB_FILE(1:IL-38)//'/t/t_'//HEB_FILE(IL-32:IL-8)//'.heb.bz2'
              ELSE
                FIL_T = HEB_FILE(1:IL-25)//'/t/t_'//HEB_FILE(IL-20:IL-8)//'.heb.bz2'
           END IF
         ELSE 
           FIL_T = HEB_FILE(1:IL-21)//'/t/t_'//HEB_FILE(IL-16:IL-4)//'.heb.bz2'
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FIL_T, HEB_T, IER )
      IF ( IER  .NE. 0 ) THEN
           CALL ERR_LOG ( 4633, IUER, 'READ_DQTUV_HEB', 'Error in reading '// &
     &         'air temperature heb-file '//FIL_T )
           RETURN 
      END IF
!
! --- Read U-file with U-wind (eastward) speed
!
      IF ( HEB_FILE(IL-3:IL) == '.bz2' ) THEN
           IF ( HEB_FILE(IL-21:IL-21) == '+' ) THEN
                FIL_U = HEB_FILE(1:IL-38)//'/u/u_'//HEB_FILE(IL-32:IL-8)//'.heb.bz2'
              ELSE
                FIL_U = HEB_FILE(1:IL-25)//'/u/u_'//HEB_FILE(IL-20:IL-8)//'.heb.bz2'
           END IF
         ELSE 
           FIL_U = HEB_FILE(1:IL-21)//'/u/u_'//HEB_FILE(IL-16:IL-4)//'.heb.bz2'
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FIL_U, HEB_U, IER )
      IF ( IER  .NE. 0 ) THEN
           CALL ERR_LOG ( 4634, IUER, 'READ_DQTUV_HEB', 'Error in reading '// &
     &         'u(eastward) wind speed '//FIL_U )
           RETURN 
      END IF
!
! --- Read V-file with V-wind (northward) speed
!
      IF ( HEB_FILE(IL-3:IL) == '.bz2' ) THEN
           IF ( HEB_FILE(IL-21:IL-21) == '+' ) THEN
                FIL_V = HEB_FILE(1:IL-38)//'/v/v_'//HEB_FILE(IL-32:IL-8)//'.heb.bz2'
              ELSE
                FIL_V = HEB_FILE(1:IL-25)//'/v/v_'//HEB_FILE(IL-20:IL-8)//'.heb.bz2'
           END IF
         ELSE 
           FIL_V = HEB_FILE(1:IL-21)//'/v/v_'//HEB_FILE(IL-16:IL-4)//'.heb.bz2'
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FIL_V, HEB_V, IER )
      IF ( IER  .NE. 0 ) THEN
           CALL ERR_LOG ( 4635, IUER, 'READ_DQTUV_HEB', 'Error in reading '// &
     &         'v(northward) wind speed '//FIL_V )
           RETURN 
      END IF
!
      IF ( TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR ) 
           WRITE ( 6, '(A)' ) 'Read and uncompress DQTUV-files:  '//STR(1:I_LEN(STR)-5)
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN 
      END  SUBROUTINE  READ_DQTUV_HEB  !#!  
