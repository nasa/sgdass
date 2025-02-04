      SUBROUTINE REDOC_SYN ( MBUF, LBUFIN, BUFIN, LBUFOUT, BUFOUT, &
     &           L_SECT, LIN_SECT, COL_SECT, ROW_HDSE, COL_HDSE, LEV_SECT, &
     &           L_META, LIN_META, COL_META, ROW_HDME, COL_HDME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  REDOC_SYN  transforms the array of text strings BUFIN     *
! *   from rdc-format to .syn format. The output is put in the array of  *
! *   strings BUFOUT.                                                    *
! *                                                                      *
! *   File in .syn format contains section titles and shortened items.   *
! *                                                                      *
! *  ###  17-MAY-2000   REDOC_SYN   v1.0 (c)  L. Petrov  25-JUL-2000 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MBUF, LBUFIN, LBUFOUT, IUER
      CHARACTER  BUFIN(LBUFIN)*(*), BUFOUT(MBUF)*128
      INTEGER*4  L_SECT, L_META
      INTEGER*4  LIN_SECT(2,L_SECT), COL_SECT(2,L_SECT), LEV_SECT(L_SECT)
      INTEGER*4  LIN_META(2,L_META), COL_META(2,L_META)
      INTEGER*4  ROW_HDSE(L_SECT),   ROW_HDME(L_META), &
     &           COL_HDSE(2,L_SECT), COL_HDME(2,L_META)
      INTEGER*4  M_SECT, M_META
      PARAMETER  ( M_SECT = 256, M_META = 16 )
      CHARACTER  BLANK*128, FILLER*80, DATE_STR*128, SYNOPSIS_STR*128
      INTEGER*4  IP, IP1, IP2, J1, J2, J3, J4, J5, J6, J7, J8
      LOGICAL*4  FL_COPY
      BYTE       TAB_BLANK(0:255), VAL_BLANK
      INTEGER*4  ILEN, IFIND_PL, I_LEN
!
      CALL CLRCH ( BLANK )
      CALL NOUT ( 256, TAB_BLANK )
      VAL_BLANK     = 1
      TAB_BLANK(32) = VAL_BLANK
      CALL REPEAT ( '~', 128, FILLER )
      CALL CLRCH ( DATE_STR )
      CALL CLRCH ( SYNOPSIS_STR )
!
! --- Looking for date
!
      DO 410 J1=1,L_META
         IF ( BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ. &
     &   'Synopsis' ) THEN
              DO 420 J2=LIN_META(1,J1),LIN_META(2,J1)
                 IF ( ILEN(BUFIN(J2)) .GT. 0 ) THEN
!
! ------------------- We found!
!
                      SYNOPSIS_STR = BUFIN(J2)(1:I_LEN(BUFIN(J2)))
                      CALL CHASHL ( SYNOPSIS_STR )
                 END IF
 420          CONTINUE
         END IF
!
         IF ( BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ. 'Date' ) &
     &   THEN
              DO 430 J3=LIN_META(1,J1),LIN_META(2,J1)
                 IF ( ILEN(BUFIN(J3)) .GT. 0 ) THEN
!
! ------------------- We found!
!
                      DATE_STR = BUFIN(J3)(1:I_LEN(BUFIN(J3)))
                      CALL CHASHL ( DATE_STR )
                 END IF
 430          CONTINUE
         END IF
 410  CONTINUE
!
! --- Set the title of the document
!
      LBUFOUT=1
      BUFOUT(LBUFOUT) = BLANK(1:16)//'Synopsis of '// &
     &                  SYNOPSIS_STR(1:I_LEN(SYNOPSIS_STR))// &
     &                  ' ( '//DATE_STR(1:I_LEN(DATE_STR))//' )'
      IP = ILEN(BUFOUT(LBUFOUT))
!
      LBUFOUT=LBUFOUT+1
      BUFOUT(LBUFOUT) = FILLER(1:IP)
      BUFOUT(LBUFOUT)(1:16) = BLANK(1:16)
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
!
! --- Scan the document
!
      FL_COPY = .FALSE.
      DO 440 J4=ROW_HDSE(1),LBUFIN
!
! ------ This line is the line with section title
!
         IP = IFIND_PL ( L_SECT, ROW_HDSE(1), J4 )
         IF ( IP .GT. 0 ) THEN
              IF ( BUFIN(ROW_HDSE(IP))(COL_HDSE(1,IP):COL_HDSE(1,IP)) .EQ. &
     &            '$' ) THEN
!
! --------------- It is the main section. Add it.
!
                  LBUFOUT = LBUFOUT + 1
                  BUFOUT(LBUFOUT) = &
     &               BUFIN(ROW_HDSE(IP))(COL_HDSE(1,IP):COL_HDSE(2,IP))
              END IF
            ELSE
!
! ----------- Search for the beginning the section with summary
!
              IP1 = INDEX ( BUFIN(J4), '#(' )
              IF ( IP1 .GT. 0 ) THEN
                   FL_COPY = .TRUE.
                   GOTO 440
              END IF
!
! ----------- Search for the end of the section with summary
!
              IP2 = INDEX ( BUFIN(J4), '#)' )
              IF ( IP2 .GT. 0 ) THEN
                   FL_COPY = .FALSE.
                   GOTO 440
              END IF
!
              IF ( FL_COPY ) THEN
!
! --------------- Add sumary items
!
                  LBUFOUT = LBUFOUT + 1
                  BUFOUT(LBUFOUT) = BUFIN(J4)
              END IF
         END IF
 440  CONTINUE
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
!
! --- Add notation
!
      DO 450 J5=1,L_META
         IF ( BUFIN(ROW_HDME(J5))(COL_HDME(1,J5):COL_HDME(2,J5)) .EQ.'Notation') &
     &   THEN
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = 'Notation:'
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = '~~~~~~~~~'
!
              DO 460 J6=LIN_META(1,J5),LIN_META(2,J5)
                 LBUFOUT = LBUFOUT + 1
                 BUFOUT(LBUFOUT) = BUFIN(J6)(1:I_LEN(BUFIN(J6)))
 460          CONTINUE
         END IF
 450  CONTINUE
!
! --- Add contact
!
      DO 470 J7=1,L_META
         IF ( BUFIN(ROW_HDME(J7))(COL_HDME(1,J7):COL_HDME(2,J7)) .EQ.'Contact' ) &
     &   THEN
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = ' '
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = 'Contact:'
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = '~~~~~~~~'
!
              DO 480 J8=LIN_META(1,J7),LIN_META(2,J7)
                 LBUFOUT = LBUFOUT + 1
                 BUFOUT(LBUFOUT) = BUFIN(J8)(1:I_LEN(BUFIN(J8)))
 480          CONTINUE
         END IF
 470  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REDOC_SYN  #!#
