      SUBROUTINE DCM_LISTS ( DCM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine DCM_LISTS 
! *                                                                      *
! *  ### 26-OCT-2007   DCM_LISTS   v1.0 (c)  L. Petrov  26-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'edc.i'
      TYPE     ( DCM__TYPE     ) :: DCM
      INTEGER*4  IUER
      CHARACTER  BUF(DCM__M_OBJ)*128
      INTEGER*4  J1, J2, NBUF, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      DCM%L_INC = 0
      DCM%L_EXC = 0
      DCM%L_OBJ = 0
      IF ( DCM%OBJECT == DCM__ALL ) THEN
           DCM%L_INC = 1 
           DCM%L_EXC = 0
           CALL ERR_LOG ( 0, IUER )
           RETURN 
         ELSE IF ( DCM%OBJECT == DCM__SOU ) THEN
           CONTINUE 
         ELSE IF ( DCM%OBJECT == DCM__STA ) THEN
           CONTINUE 
         ELSE IF ( DCM%OBJECT == DCM__BAS ) THEN
           CONTINUE 
         ELSE 
           CALL ERR_LOG ( 6361, IUER, 'DCM_LISTS', 'Trap of internal '// &
     &         'control: unsupported parameter DCM%OBJECT: '//DCM%OBJECT )
           RETURN 
      END IF
!
      IF ( ILEN(DCM%FIL_INC) == 0 ) THEN
           CALL ERR_LOG ( 6362, IUER, 'DCM_LISTS', 'TRap of internal '// &
     &         'control: no file INCLUDE list for object type '// &
     &          DCM%OBJECT//' was specified' )
           RETURN 
      END IF
!
! --- Read the include-name file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( DCM%FIL_INC, DCM__M_OBJ, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6363, IUER, 'DCM_LISTS', 'Error reading '// &
     &         'include-file of objects '//DCM%FIL_INC )
           RETURN 
      END IF
!
! --- Parse the include file
!
      DO 410 J1=1,NBUF
         CALL CHASHL ( BUF(J1) ) 
         IF ( BUF(J1)(1:1) == ' ' ) GOTO 410
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         DCM%L_INC = DCM%L_INC + 1
         IF ( DCM%OBJECT == DCM__SOU ) THEN
              DCM%NAM_INC(DCM%L_INC) = BUF(J1)(1:8)
           ELSE IF ( DCM%OBJECT == DCM__STA ) THEN
              DCM%NAM_INC(DCM%L_INC) = BUF(J1)(1:8)
              CALL VTD_NAME_REPAIR ( DCM%NAM_INC(DCM%L_INC) )
            ELSE IF ( DCM%OBJECT == DCM__BAS ) THEN
              DCM%NAM_INC(DCM%L_INC)(1:8) = BUF(J1)(1:8)
              CALL CLRCH  ( BUF(J1)(1:8) )
              CALL CHASHL ( BUF(J1)      )
              DCM%NAM_INC(DCM%L_INC)(9:16) = BUF(J1)(1:8)
              CALL VTD_NAME_REPAIR ( DCM%NAM_INC(DCM%L_INC)(1:8)  )
              CALL VTD_NAME_REPAIR ( DCM%NAM_INC(DCM%L_INC)(9:16) )
         END IF
 410  CONTINUE 
!
      IF ( ILEN(DCM%FIL_EXC) == 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Read the exclude-name file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( DCM%FIL_EXC, DCM__M_OBJ, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6364, IUER, 'DCM_LISTS', 'Error reading '// &
     &         'include-file of objects '//DCM%FIL_EXC )
           RETURN 
      END IF
!
! --- Parse the exclude-file
!
      DO 420 J2=1,NBUF
         CALL CHASHL ( BUF(J2) ) 
         IF ( BUF(J2)(1:1) == ' ' ) GOTO 420
         IF ( BUF(J2)(1:1) == '#' ) GOTO 420
         DCM%L_EXC = DCM%L_EXC + 1
         IF ( DCM%OBJECT == DCM__SOU ) THEN
              DCM%NAM_EXC(DCM%L_EXC) = BUF(J2)(1:8)
           ELSE IF ( DCM%OBJECT == DCM__STA ) THEN
              DCM%NAM_EXC(DCM%L_EXC) = BUF(J2)(1:8)
              CALL VTD_NAME_REPAIR ( DCM%NAM_EXC(DCM%L_EXC) )
            ELSE IF ( DCM%OBJECT == DCM__BAS ) THEN
              DCM%NAM_EXC(DCM%L_EXC)(1:8) = BUF(J2)(1:8)
              CALL CLRCH  ( BUF(J2)(1:8) )
              CALL CHASHL ( BUF(J2)      )
              DCM%NAM_EXC(DCM%L_EXC)(9:16) = BUF(J2)(1:8)
              CALL VTD_NAME_REPAIR ( DCM%NAM_EXC(DCM%L_EXC)(1:8)  )
              CALL VTD_NAME_REPAIR ( DCM%NAM_EXC(DCM%L_EXC)(9:16) )
         END IF
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE DCM_LISTS  !#!#
