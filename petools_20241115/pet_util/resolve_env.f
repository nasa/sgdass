      SUBROUTINE RESOLVE_ENV ( STR, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine RESOLVE_ENV resolves environment variables in the          *
! *   form ${ENV_NAME} and replaces their definitions with values.       *
! *   Nested definitions are allowed.                                    *
! *                                                                      *
! *  ### 03-MAR-2019   RESOLVE_ENV  v1.0 (c)  L. Petrov 03-MAR-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  STR*(*)
      INTEGER*4  IUER
      CHARACTER  VAL_VAR*128
      INTEGER*4  J1, IB, IE
      INTEGER*4, EXTERNAL :: ILEN
!
      DO 410 J1=1,LEN(STR)
!
! ------ Search for the environment variable definition header
!
         IB = INDEX ( STR, '${' )
         IF ( IB > 0 ) THEN
!
! ----------- Search for the environment variable definition trailer
!
              IE =  INDEX ( STR(IB+1:), '}' ) + IB
              IF ( IE == IB ) THEN
                   CALL ERR_LOG ( 5211, IUER, 'RESOLVE_ENV', 'There is no '// &
     &                 'matching } after ${' )
                   RETURN 
              END IF
              IF ( IE == IB+2 ) THEN
!
! ---------------- Empy defintion
!
                   IF ( IE+1 > LEN(STR) ) THEN
                        STR = STR(1:IB-1)
                      ELSE
                        STR = STR(1:IB-1)//STR(IE+1:)
                   END IF
                 ELSE
!
! ---------------- Resolve the environment variable
!
                   CALL CLRCH ( VAL_VAR )
                   CALL GETENVAR ( STR(IB+2:IE-1), VAL_VAR )
                   IF ( ILEN(VAL_VAR) == 0 ) THEN
                        CALL ERR_LOG ( 5212, IUER, 'RESOLVE_ENV', 'Undefined environment '// &
     &                      'variable '//STR(IB+2:IE-1) )
                        RETURN 
                   END IF
                   IF ( IE+1 > LEN(STR) ) THEN
                        STR = STR(1:IB-1)//STR(IE+1:)//TRIM(VAL_VAR)
                      ELSE
                        STR = STR(1:IB-1)//TRIM(VAL_VAR)//STR(IE+1:)
                   END IF
              END IF
            ELSE
              GOTO 810
         END IF
 410  CONTINUE 
 810  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  RESOLVE_ENV  !#!  
