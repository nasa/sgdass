      SUBROUTINE MAKE_FILENAME ( ENV_NAME, FILE_IN, FILE_OUT )
      CHARACTER  ENV_NAME*(*), FILE_IN*(*), FILE_OUT*(*)
      CHARACTER  STR*255
!
      DO 410 J1=1,LEN(FILE_OUT)
         FILE_OUT(J1:J1) = ' '
 410  CONTINUE
      ILOUT = 0
      IF ( LEN(ENV_NAME) .GT. 0 ) THEN
           CALL GETENVAR ( ENV_NAME, FILE_OUT )
           ILOUT = INDEX  ( FILE_OUT, ' ' ) -1
      END IF
!
      DO 420 J2=1,LEN(STR)
         STR(J2:J2) = ' '
 420  CONTINUE
      IF ( FILE_IN(1:1) .EQ. '$' ) THEN
           IF ( STR(1:1) .EQ. ' ' ) STR = FILE_IN
        ELSE
           STR = FILE_IN
      END IF
!
      IF ( ILOUT .GT. 0 ) THEN
           IF ( FILE_OUT(ILOUT:ILOUT) .EQ. '/'  .AND. STR(1:1) .EQ. '/' ) THEN
                FILE_OUT = FILE_OUT(1:ILOUT)//STR(2:)
             ELSE IF (FILE_OUT(ILOUT:ILOUT).EQ.'/' .AND. STR(1:1).NE.'/' ) THEN
                FILE_OUT = FILE_OUT(1:ILOUT)//STR(1:)
             ELSE IF (FILE_OUT(ILOUT:ILOUT).NE.'/' .AND. STR(1:1).EQ.'/' ) THEN
                FILE_OUT = FILE_OUT(1:ILOUT)//STR(1:)
             ELSE IF (FILE_OUT(ILOUT:ILOUT).NE.'/' .AND. STR(1:1).NE.'/' ) THEN
                FILE_OUT = FILE_OUT(1:ILOUT)//'/'//STR(1:)
           END IF
        ELSE
           FILE_OUT = STR
      END IF
!
      RETURN
      END  !#!  MAKE_FILENAME  #!#
