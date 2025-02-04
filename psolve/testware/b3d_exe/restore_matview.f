      PROGRAM    RESTORE_MATVIEW
! ************************************************************************
! *                                                                      *
! *   Program RESTORE_MATVIEW
! *                                                                      *
! * ### 03-AUG-2006  RESTORE_MATVIEW  v1.0 (c) L. Petrov 03-AUG-2006 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MATYP, MV, MH, IV, IH, IUER
      REAL*8,    POINTER :: MAT(:)
      CHARACTER  ZAG*128, FORM*128, FINAM*128
      INTEGER*4, EXTERNAL :: IARGC
      INTERFACE
         SUBROUTINE MATVIEW_RA ( FINAM, MATYP, MV, MH, MAT, ZAG, FORM, IV, IH, &
     &                           IUER )
            CHARACTER    FINAM*(*), ZAG*(*), FORM*(*)
            INTEGER*4    MATYP, MV, MH, IV, IH, IUER
            REAL*8,      POINTER :: MAT(:)
         END SUBROUTINE MATVIEW_RA 
      END INTERFACE
!
      IF ( IARGC () < 1 ) THEN
           WRITE ( 6, * ) 'Usage: restore_matview {filename}'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FINAM ) 
      END IF
!
      NULLIFY ( MAT )
!
      IUER = -1
      CALL MATVIEW_RA ( FINAM, MATYP, MV, MH, MAT, ZAG, FORM, IV, IH, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4701, -2, 'RESTORE_MATVIEW', 'Error in an '// &
     &                   'to attempt read the matrix from file '//FINAM )
           CALL EXIT ( 0 )
      END IF
!
      IUER = -1
      CALL MATVIEW ( MATYP, MV, MH, MAT, ZAG, FORM, IV, IH, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4702, -2, 'RESTORE_MATVIEW', 'Error in an '// &
     &                   'to attempt display the matrix from file '//FINAM )
           CALL EXIT ( 0 )
      END IF
!
      DEALLOCATE(MAT)
      END  !#!  
