      SUBROUTINE PETOOLS_VERS ( VERS_STR )
      CHARACTER  VERS_STR*(*)
      INCLUDE   'petools_local.i'
      VERS_STR = PETOOLS__LABEL
      RETURN
      END  SUBROUTINE PETOOLS_VERS
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PETOOLS_INQ ( INQ, ANS )
      IMPLICIT   NONE 
      INCLUDE   'petools_local.i'
      CHARACTER  INQ*(*), ANS*(*)
!
      IF ( INQ == 'version'   .OR. &
     &     INQ == '--version'      ) THEN
           ANS = PETOOLS__VERSION
        ELSE IF ( INQ == 'prefix'   .OR.  &
     &            INQ == '--prefix'       ) THEN
           ANS = PETOOLS__PREFIX
        ELSE IF ( INQ == 'label'   .OR.  &
     &            INQ == '--label'       ) THEN
           ANS = PETOOLS__LABEL
        ELSE IF ( INQ == 'bindir'   .OR.  &
     &            INQ == '--bindir'       ) THEN
           ANS = PETOOLS__PREFIX//'/bin'
        ELSE IF ( INQ == 'root'   .OR.  &
     &            INQ == '--root'       ) THEN
           ANS = PETOOLS__ROOT
        ELSE IF ( INQ == 'doc'   .OR.  &
     &            INQ == '--doc'       ) THEN
           ANS = PETOOLS__DOC
      END IF
      RETURN
      END  SUBROUTINE  PETOOLS_INQ   !#!  
