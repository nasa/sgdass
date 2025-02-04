      SUBROUTINE DIAGI_SAV ( DIAGI_S, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_SAV  writes current status of the plot to the       *
! *   output file (filename is detemined by constants DIAGI_OUT and      *
! *   SAV_DIAGI). THis file then may be read by DIAGI_RES and plot may   *
! *   be restored.                                                       *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *      DIAGI_S ( RECORD    ) -- Data structure which keeps DiaGI       *
! *                               internal parameters.                   *
! *                                                                      *
! *  ###  21-MAY-98    DIAGI_SAV   v1.4  (c)  L. Petrov 13-JUL-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      CHARACTER  FINAM*80, STR*20, MES*80, CH*1
      REAL*4     XC, YC
      INTEGER*4  IUER, LUN, NBT, IDEV_SAVE, IBATCH_SAVE, J1, IER
      INTEGER*4  I_LEN
!
! --- Save IDEV and IBATCH
!
      IDEV_SAVE   = DIAGI_S%IDEV 
      IBATCH_SAVE = DIAGI_S%IBATCH 
      IF ( DIAGI_S%IDEV > 4 ) THEN
           DIAGI_S%IDEV = 2
      END IF
      DIAGI_S%IBATCH = 0
!
! --- Constructing output file name
!
      CALL CLRCH ( FINAM )
      IF ( IBATCH_SAVE == 0 ) THEN
           FINAM = DIAGI_OUT//SAV_DIAGI
         ELSE 
           FINAM = DIAGI_S%NAME      
      END IF
!
! --- Openning output file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'UNKNOWN', LUN, IER )
      IF ( IER > 0 ) THEN
           CALL ERR_LOG ( 4161, IUER, 'DIAGI_SAV', 'Error during attempt to '// &
     &                   'open output file '//FINAM )
           GOTO 810
      END IF
!
! --- Writing DiaGI data structure
!
      NBT = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
      CALL ERR_PASS     ( IUER, IER )
      CALL WRBIN_RECORD ( LUN, NBT, DIAGI_S, IER )
      IF ( IER > 0 ) THEN
           CALL ERR_LOG ( 4162, IUER, 'DIAGI_SAV', 'Error during writing '// &
     &                   'DiaGI data structure to the file '//FINAM )
           GOTO 810
      END IF
!
      DO 410 J1=1,DIAGI_S%NCLR
!
! ------ Writing arguments
!
         CALL ERR_PASS    ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'R8', DIAGI_S%NPOI(J1), &
     &                      %VAL(DIAGI_S%ADR_X8(J1)), IER )
         IF ( IER > 0 ) THEN
              CALL CLRCH   ( STR )
              CALL INCH    ( J1, STR )
              CALL ERR_LOG ( 4163, IUER, 'DIAGI_SAV', 'Error during writing '// &
     &                       'array of arguments for the '//STR(1:I_LEN(STR))// &
     &                       '-th color to the file '//FINAM )
              GOTO 810
         END IF
!
! ------ Writing values
!
         CALL ERR_PASS    ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'R8', DIAGI_S%NPOI(J1), &
     &                      %VAL(DIAGI_S%ADR_Y8(J1)), IER )
         IF ( IER > 0 ) THEN
              CALL CLRCH   ( STR )
              CALL INCH    ( J1, STR )
              CALL ERR_LOG ( 4164, IUER, 'DIAGI_SAV', 'Error during writing '// &
     &                       'array of values for the '//STR(1:I_LEN(STR))// &
     &                       '-th color to the file '//FINAM )
              GOTO 810
         END IF
         IF ( DIAGI_S%LER(J1) ) THEN
!
! ----------- Writing errors
!
              CALL ERR_PASS    ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', DIAGI_S%NPOI(J1), &
     &                           %VAL(DIAGI_S%ADR_E8(J1)), IER )
              IF ( IER > 0 ) THEN
                   CALL CLRCH   ( STR )
                   CALL INCH    ( J1, STR )
                   CALL ERR_LOG ( 4165, IUER, 'DIAGI_SAV', 'Error during '// &
     &                 'writing array of errors for the '//STR(1:I_LEN(STR))// &
     &                 '-th color to the file '//FINAM )
                   GOTO 810
              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
      CALL ERR_PASS    ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER > 0 ) THEN
           CALL ERR_LOG ( 4166, IUER, 'DIAGI_SAV', 'Failure in an '// &
     &                 'attempt to close file '//FINAM )
           GOTO 810
      END IF
!
      CALL PGSAVE ! 1
!
! --- Deleting previous window
!
      CALL PGERAS
      CALL PGSLW   ( 8   )
!
! --- Preparing message which will be printed in graphic window
!
      CALL CLRCH ( MES )
      IF ( IER .EQ. 0  .OR. IER .EQ. -2 ) THEN
           CALL PGSCI   ( 1 )
           MES = 'Plot has been saved in file '//FINAM
           CALL PGSCH   ( 2.0 )
           CALL ERR_LOG ( 0, IUER )
         ELSE
           CALL PGSCI   ( 2   )
           MES = 'Plot has not been saved since error occured'
           CALL PGSCH   ( 2.0 )
           CALL ERR_LOG ( 1, IUER )
      END IF
!
! --- Setting new world coodrinates
!
      CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
      IF ( IBATCH_SAVE == 0 ) THEN
!
! -------- Printing the message
!
           XC = 0.50
           YC = 0.66
           CALL PGPTXT  ( XC, YC, 0.0, 0.5, MES(1:I_LEN(MES)) )
           CALL PGUNSA ! !
!
! -------- Waiting for user reaction
!
           CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
      END IF
      DIAGI_S%IDEV = IDEV_SAVE   
      DIAGI_S%IBATCH = IBATCH_SAVE 
!
      RETURN
      END  !#!  DIAGI_SAV  #!#
