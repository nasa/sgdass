      PROGRAM    DIAGI_KEY
! ************************************************************************
! *                                                                      *
! *   Program DIAGI_KEY askes in an indefinite loop a user to hit a key. *
! *   It returns the code of the key as it is passed to PGPLOT.          *
! *                                                                      *
! *  ### 21-APR-2003   DIAGI_KEY   v1.0 (c)  L. Petrov  21-APR-2003 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*4  PGOPEN
      REAL*4     XP, YP
      CHARACTER  CH*1, STR*8
      INTEGER*4  IUER, ID_XW
      IUER = -1
!
      ID_XW = PGOPEN ( '/XS' )
      IF ( ID_XW .LE. 0 ) THEN
           CALL ERR_LOG ( 6201, IUER, 'DIAGI_KEY', 'Error in openning '// &
     &         'the graphic device pgplot /XS' )
           CALL EXIT ( 1 )
      END IF
!
      CALL PGSVP  (  0.0, 1.0,  0.0, 1.0 )
      CALL PGSWIN (  0.0, 1.0,  0.0, 1.0 )
      CALL PGSCR  ( 0, 216./255., 216./255., 216./255. ) ! whitish background
      CALL PGSCR  ( 1, 0.0, 0.0, 0.0 ) ! pure black foreground
      CALL PGSLW  ( 8   )
      CALL PGSCH  ( 3.0 )
      CALL PGERAS
!
 910  CONTINUE
         XP = 0.5
         YP = 0.7
         CALL PGPTXT ( XP, YP, 0.0, 0.5, 'Hit a key, please' )
         XP = 0.5
         YP = 0.5
         CALL PGCURS ( XP, YP, CH )
         WRITE ( 6, * ) ' Cursor position: XP=',XP,' YP=', YP 
         CALL PGERAS
         XP = 0.5
         YP = 0.3
         CALL INCH ( ICHAR(CH), STR )
         CALL PGPTXT ( XP, YP, 0.0, 0.5, 'Code: '//STR(1:I_LEN(STR)) )
         GOTO 910
      END  PROGRAM  DIAGI_KEY
