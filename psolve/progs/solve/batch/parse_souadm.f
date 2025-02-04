      SUBROUTINE PARSE_SOUADM ( STRING, SOU_ADM_FLAG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_SOUADM 
! *                                                                      *
! *  ### 09-AUG-2007  PARSE_SOUADM  v1.0 (c) L. Petrov  09-AUG-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'glbc3.i'
      CHARACTER  STRING*(*)
      INTEGER*2  SOU_ADM_FLAG
      INTEGER*4  IUER
!
      CHARACTER  TOKEN*32
      LOGICAL*4  FL_GLO, FL_LOC, FL_YES, FL_NO
      INTEGER*4  J1, IP
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN == '\' ) THEN
           CALL CFREAD ( STRING )
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
         ELSE IF ( TOKEN(1:1) == '*' ) THEN
           CALL CFREAD ( STRING )
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
      END IF            
!
      CALL TRAN ( 11, TOKEN, TOKEN )
      FL_LOC = .FALSE.
      FL_GLO = .FALSE.
      FL_YES = .FALSE.
      FL_NO  = .FALSE.
!
      IF ( TOKEN == 'NO' ) THEN
           SOU_ADM_FLAG = SOUADM__NO
           CALL ERR_LOG ( 0, IUER )
           RETURN 
         ELSE IF ( TOKEN == 'GLOBL'  .OR.  TOKEN == 'GLOBAL' ) THEN
           FL_GLO = .TRUE.
         ELSE IF ( TOKEN == 'LOCAL' ) THEN
           FL_LOC = .TRUE.
         ELSE IF ( TOKEN == '' ) THEN
           CALL ERR_LOG ( 8311, IUER, 'PARSE_SOUADM', 'No value was found '// &
     &         'during parsing the STRUCTURE_ADMITTANCE keyword: '// &
     &         ' -- GLOBL or LOCAL were expected' )
           RETURN 
         ELSE 
           CALL ERR_LOG ( 8312, IUER, 'PARSE_SOUADM', 'Error in parsing '// &
     &         'STRUCTURE_ADMITTANCE keyword: unsupported value '// &
     &          TOKEN(1:I_LEN(TOKEN))//' -- GLOBL or LOCAL were expected' )
           RETURN 
      END IF
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN == '\' ) THEN
           CALL CFREAD ( STRING )
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
         ELSE IF ( TOKEN(1:1) == '*' ) THEN
           CALL CFREAD ( STRING )
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
      END IF            
!
      CALL TRAN ( 11, TOKEN, TOKEN )
      IF ( TOKEN == 'YES' ) THEN
           FL_YES = .TRUE.
         ELSE IF ( TOKEN == 'NO' ) THEN
           FL_NO  = .TRUE.
         ELSE IF ( TOKEN == '' ) THEN
           CALL ERR_LOG ( 8313, IUER, 'PARSE_SOUADM', 'No 2nd value was '// &
     &         'found during parsing the STRUCTURE_ADMITTANCE keyword: '// &
     &         ' -- GLOBL or LOCAL were expected' )
           RETURN
         ELSE 
           CALL ERR_LOG ( 8314, IUER, 'PARSE_SOUADM', 'Error in parsing '// &
     &         'STRUCTURE_ADMITTANCE keyword: unsupported 2nd value '// &
     &          TOKEN(1:I_LEN(TOKEN))//' -- NO or YES were expected' )
           RETURN 
      END IF
!      
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN == '\' ) THEN
           CALL CFREAD ( STRING )
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
      END IF            
      CALL TRAN ( 11, TOKEN, TOKEN )
      IF ( TOKEN == 'EXCEPT' ) THEN
            IF ( FL_YES ) THEN
                 IF ( FL_GLO ) THEN
                      SOU_ADM_FLAG = SOUADM__GLB_LIST_YES
                    ELSE IF ( FL_LOC ) THEN
                      SOU_ADM_FLAG = SOUADM__LCL_LIST_YES
                 END IF
               ELSE IF ( FL_NO ) THEN
                 IF ( FL_GLO ) THEN
                      SOU_ADM_FLAG = SOUADM__GLB_LIST_NO
                    ELSE IF ( FL_LOC ) THEN
                      SOU_ADM_FLAG = SOUADM__LCL_LIST_NO
                 END IF
            END IF
            DO 410 J1=1,MAX_SRC
               CALL SPLITSTRING ( STRING, TOKEN, STRING )
               IF ( TOKEN == '\' ) THEN
                    CALL CFREAD ( STRING )
                    CALL SPLITSTRING ( STRING, TOKEN, STRING )
                  ELSE IF ( TOKEN(1:1) == '*' ) THEN
                    CALL CFREAD ( STRING )
                    CALL SPLITSTRING ( STRING, TOKEN, STRING )
               END IF            
!
               IF ( IND_SOU_ADM(1) > 0 ) THEN
!
! ----------------- Search, whether the source has already been defined
!
                    IP = LTM_DIF ( 0, IND_SOU_ADM(2)-IND_SOU_ADM(2)+1, &
     &                             SRCSUP(IND_SOU_ADM(1)), TOKEN )
                    IF ( IP == 0 ) THEN
!
! ---------------------- No? Add it tot the list
!
                         IND_SOU_ADM(2) = IND_SOU_ADM(2) + 1
                         SRCSUP(IND_SOU_ADM(2)) = TOKEN
                         ISRCSP = ISRCSP + 1
                    END IF
                  ELSE 
!
! ----------------- Add the first source to the list
!
                    IND_SOU_ADM(1) = ISRCSP + 1
                    IND_SOU_ADM(2) = ISRCSP + 1
                    SRCSUP(IND_SOU_ADM(2)) = TOKEN
                    ISRCSP = ISRCSP + 1
               END IF
 410        CONTINUE 
!
          ELSE IF ( TOKEN == '' ) THEN
            IF ( FL_YES ) THEN
                 IF ( FL_GLO ) THEN
                      SOU_ADM_FLAG = SOUADM__GLB_LIST_NO
                    ELSE IF ( FL_LOC ) THEN
                      SOU_ADM_FLAG = SOUADM__LCL_LIST_NO
                 END IF
               ELSE IF ( FL_NO ) THEN
                 SOU_ADM_FLAG = SOUADM__NO
            END IF
          ELSE IF ( TOKEN == 'ALL' ) THEN
            IF ( FL_YES ) THEN
                 IF ( FL_GLO ) THEN
                      SOU_ADM_FLAG = SOUADM__GLB_ALL 
                    ELSE IF ( FL_LOC ) THEN
                      SOU_ADM_FLAG = SOUADM__LCL_ALL 
                 END IF
               ELSE IF ( FL_NO ) THEN
                 SOU_ADM_FLAG = SOUADM__NO
            END IF
          ELSE 
            CALL ERR_LOG ( 8314, IUER, 'PARSE_SOUADM', 'Error in parsing '// &
     &          'STRUCTURE_ADMITTANCE keyword: unsupported 3nd value '// &
     &          TOKEN(1:I_LEN(TOKEN))//' -- ALL or EXCEPT were expected' )
            RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_SOUADM  !#!#
