      SUBROUTINE DIAGI_VIE ( DIAGI_S )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_VIE  puts informational message at the gfraphic     *
! *   window and prints the table of the plotting values in MATView mode *
! *   on the text window from which DiaGi was activated.                 *
! *   The first column of the output is the arguments list, the scoen is *
! *   the list of corresponding values and the third is the lists of     *
! *   errors  of the function to be polotted (if specified).             *
! *                                                                      *
! *   It also write down the tabel of arguments, values [, erors] of the *
! *   current color to ASCII table to the file /tmp/diagi.tab .          *
! *                                                                      *
! *   It returns control to the calling routine when user terminates     *
! *   MATView.                                                           *
! *                                                                      *
! *  ###  11-DEC-97    DIAGI_VIE   v1.3  (c)  L. Petrov 31-DEC-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  ICLR, NPOI, IER, I99, J1
      ADDRESS__TYPE :: MEM_LEN, MEM_ADR, MAT_ADR
      INTEGER*4  I_LEN
      LOGICAL*4  LER
      REAL*4     XC, YC
      CHARACTER  ZAG*80, MES*80, FINAM*160
      REAL*8     VAL, ARG, ERR
!
      ICLR = DIAGI_S%ICLR
      LER  = DIAGI_S%LER(ICLR)
      NPOI = DIAGI_S%NPOI(ICLR)
!
      CALL CLRCH ( MES )
      MES = 'Activate text window to look at table of ploting values'
      CALL PGSAVE ! 1
!
      CALL PGSCI   ( 1    )
      CALL PGSCH   ( 2.0  )
      CALL PGSLW   ( 6    )
!
! --- Deleting previous window
!
      CALL PGERAS
!
! --- Setting new world coodrinates
!
      CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
!
! --- Printing the prompt
!
      XC = 0.50
      YC = 0.66
      CALL PGPTXT  ( XC, YC, 0.0, 0.5, MES(1:I_LEN(MES)) )
      CALL PGUNSA ! !
!
! --- Grabbing dynamic memore to form matrix of the plotting values
!
      IER = -1
#ifdef ADR_32BIT
      CALL GRAB_MEM ( IER, MEM_LEN,  MEM_ADR, 1, &
     &                     8*NPOI*3, MAT_ADR     )
#else
      CALL GRAB_MEM ( IER, MEM_LEN,        MEM_ADR, 1, &
     &                     INT8(8*NPOI*3), MAT_ADR     )
#endif
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4171, -1, 'DIAGI_VIE', 'Error in attempt to '// &
     &         'grab additional dynamic memory for looking matrix of the '// &
     &         'plot valuues' )
           RETURN
      END IF
!
! --- Copying arguments, values and (if specified) errors to the matrix
!
      CALL COPY_V ( NPOI, %VAL(DIAGI_S%ADR_X8(ICLR)), %VAL(MAT_ADR) )
      CALL COPY_V ( NPOI, %VAL(DIAGI_S%ADR_Y8(ICLR)), %VAL(MAT_ADR + 8*NPOI) )
      IF ( LER ) THEN
           CALL COPY_V ( NPOI, %VAL(DIAGI_S%ADR_E8(ICLR)), &
     &                         %VAL(MAT_ADR + 8*2*NPOI) )
      END IF
!
! --- Calling MATView
!
      IER = -1
      CALL CLRCH ( ZAG )
      IF ( LER ) THEN
          ZAG = 'Arguments, values and errors for color '
          CALL INCH ( ICLR, ZAG(I_LEN(ZAG)+2:) )
          CALL MATVIEW ( 1, NPOI, 3, %VAL(MAT_ADR), ZAG, '(1PG15.7)', 1, 1, IER)
        ELSE
          ZAG = 'Arguments and values for color '
          CALL INCH ( ICLR, ZAG(I_LEN(ZAG)+2:) )
          CALL MATVIEW ( 1, NPOI, 2, %VAL(MAT_ADR), ZAG, '(1PG15.7)', 1, 1, IER)
      END IF
!
! --- Openning the file for the output table of the argument, value [and error]
! --- of the current color
!
      CALL CLRCH ( FINAM )
      FINAM = '/tmp/diagi'//TAB_DIAGI
      OPEN ( UNIT=99, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=I99 )
      IF ( I99 .NE. 0 ) THEN
           CALL ERR_LOG ( 4181, -1, 'DIAGI_VIE', 'Error during attempt to '// &
     &         'open file '//FINAM(1:I_LEN(FINAM))//' at the unit 99' )
           GOTO 810
      END IF
!
      DO 410 J1=1,NPOI
!
! ------ Exstraction argument, values [and error] of the J1-th point of the
! ------ current color to ARG, VAL, ERR
!
         CALL LIB$MOVC3 ( 8, %VAL(DIAGI_S%ADR_X8(ICLR) + 8*(J1-1) ), ARG )
         CALL LIB$MOVC3 ( 8, %VAL(DIAGI_S%ADR_Y8(ICLR) + 8*(J1-1) ), VAL )
         IF ( LER ) THEN
              CALL LIB$MOVC3 ( 8, %VAL(DIAGI_S%ADR_E8(ICLR) + 8*(J1-1) ), ERR )
           ELSE
              ERR = 0.0D0
         END IF
!
! ------ Writing them to the file
!
         WRITE  ( 99, 110, IOSTAT=I99 ) ARG, VAL, ERR
 110     FORMAT ( 1PD22.15, 1X, 1PD22.15, 1X, 1PD22.15 )
         IF ( I99 .NE. 0 ) THEN
              CALL ERR_LOG ( 4181, -1, 'DIAGI_VIE', 'Error during attempt to '// &
     &            'write table of the values for the current color to the '// &
     &            'file '//FINAM(1:I_LEN(FINAM))//' at the unit 99' )
              CLOSE ( UNIT=99 )
              GOTO 810
         END IF
 410  CONTINUE
      CLOSE ( UNIT=99 )
      WRITE ( 6, * ) ' Tables of values for the current color written to '// &
     &         FINAM(1:I_LEN(FINAM))
 810  CONTINUE
      WRITE ( 6, * ) ' Now activate pgplot window to continue looking graphic ouput'
!
! --- Freeing dynamic memory
!
      CALL FREE_MEM ( MEM_ADR )
!
      RETURN
      END  !#!  DIAGI_VIE  #!#
