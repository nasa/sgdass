      SUBROUTINE REDOC_TXT ( MBUF, LBUFIN, BUFIN, LBUFOUT, BUFOUT, &
     &           L_SECT, LIN_SECT, COL_SECT, ROW_HDSE, COL_HDSE, LEV_SECT, &
     &           L_META, LIN_META, COL_META, ROW_HDME, COL_HDME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  REDOC_TXT transforms the array of test strings BUFIN from *
! *   rdc-format to .txt format. The output is put in the array of       *
! *   strings BUFOUT.                                                    *
! *                                                                      *
! *  ### 16-MAY-2000   REDOC_TXT   v1.0 (c)  L. Petrov  17-MAY-2000 ###  *
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
      CHARACTER  BLANK*128, FILLER*128, STR*80, &
     &           STR1*8, STR2*8, STR3*8, STR4*8, LEV_CHAR*4, &
     &           TIT_SECT(M_SECT)*128, NUM_SECT(M_SECT)*128
      INTEGER*4  IP, IP1, IP2, ID, ICNT(4), IN, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9
      LOGICAL*4  FL_NOBLANK
      BYTE       TAB_BLANK(0:255), VAL_BLANK
      INTEGER*4  ILEN, IFIND_PL, I_LEN
!
      LEV_CHAR = '=~- '
      CALL CLRCH ( BLANK )
      CALL NOUT ( 256, TAB_BLANK )
      VAL_BLANK     = 1
      TAB_BLANK(32) = VAL_BLANK
!
      LBUFOUT = 0
      CALL NOUT ( LEN(BUFOUT(1))*MBUF, %REF(BUFOUT) )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         type *,' l_meta=',l_meta
!      do 510 j1=1,l_meta
!         type *, lin_meta(1,j1), lin_meta(2,j1),
!     #           col_meta(1,j1), col_meta(2,j1),
!     #           row_hdme(j1),
!     #           col_hdme(1,j1), col_hdme(2,j1)
! 510  continue
!c
!         type *,' l_sect=',l_sect
!      do 520 j1=1,l_sect
!         type *, lin_sect(1,j1), lin_sect(2,j1),
!     #           col_sect(1,j1), col_sect(2,j1),
!     #           lev_sect(j1),   row_hdse(j1),
!     #           col_hdse(1,j1), col_hdse(2,j1)
! 520  continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! --- Scan different META-tags
!
      DO 410 J1=1,L_META
         IF ( BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ. 'Title' ) &
     &   THEN
!
! ----------- Well. Title META
!
              FL_NOBLANK = .FALSE.
              DO 420 J2=LIN_META(1,J1),LIN_META(2,J1)
                 IF ( ILEN(BUFIN(J2)) .GT. 0 ) FL_NOBLANK = .TRUE.
                 IF ( FL_NOBLANK ) THEN
!
! ------------------- Centering
!
                      IP = (80 - ILEN(BUFIN(J2)))/2
                      LBUFOUT = LBUFOUT + 1
!
! ------------------- Add title
!
                      IF ( IP .GT. 1 ) THEN
                           BUFOUT(LBUFOUT) = BLANK(1:IP)// &
     &                                       BUFIN(J2)(1:I_LEN(BUFIN(J2)))
                         ELSE
                           BUFOUT(LBUFOUT) = BUFIN(J2)(1:I_LEN(BUFIN(J2)))
                      END IF
                 END IF
 420          CONTINUE
         END IF
!
         IF ( BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ. 'Author' &
     &        .OR. &
     &        BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ. 'Authors') &
     &   THEN
!
! ----------- Author meta
!
              DO 430 J3=LIN_META(1,J1),LIN_META(2,J1)
                 LBUFOUT = LBUFOUT + 1
                 BUFOUT(LBUFOUT) = BLANK(1:32)//BUFIN(J3)(1:I_LEN(BUFIN(J3)))
 430          CONTINUE
         END IF
!
         IF ( BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ. 'Date' ) &
     &   THEN
!
! ----------- Date META
!
              DO 440 J4=LIN_META(1,J1),LIN_META(2,J1)
                 LBUFOUT = LBUFOUT + 1
                 BUFOUT(LBUFOUT) = BLANK(1:32)//BUFIN(J4)(1:I_LEN(BUFIN(J4)))
 440          CONTINUE
         END IF
!
         IF ( BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ. 'Contact') &
     &   THEN
!
! ----------- Contact META
!
              DO 450 J5=LIN_META(1,J1),LIN_META(2,J1)
                 LBUFOUT = LBUFOUT + 1
                 BUFOUT(LBUFOUT) = BUFIN(J5)(1:I_LEN(BUFIN(J5)))
 450          CONTINUE
         END IF
!
         IF ( BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ.'Abstract') &
     &   THEN
!
! ----------- Abstract META
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = ' '
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = BLANK(1:32)//'Abstract'
              DO 460 J6=LIN_META(1,J1),LIN_META(2,J1)
                 LBUFOUT = LBUFOUT + 1
                 BUFOUT(LBUFOUT) = BUFIN(J6)(1:I_LEN(BUFIN(J6)))
 460          CONTINUE
         END IF
 410  CONTINUE
!
! --- Add table of content
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = BLANK(1:32)//'Table of contents:'
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
!
! --- Making table of contents
!
      ICNT(1) = 0
      DO 470 J7=1,L_SECT
         CALL CLRCH ( STR )
!
! ------ Set section numbering in according to the section level
!
         IF ( LEV_SECT(J7) .EQ. 1 ) THEN
              ICNT(1) = ICNT(1) + 1
              ICNT(2) = 0
              CALL INCH  ( ICNT(1), STR1 )
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = ' '
              NUM_SECT(J7) = STR1(1:I_LEN(STR1))
           ELSE IF ( LEV_SECT(J7) .EQ. 2 ) THEN
              ICNT(2) = ICNT(2) + 1
              ICNT(3) = 0
              CALL INCH  ( ICNT(1), STR1 )
              CALL INCH  ( ICNT(2), STR2 )
              NUM_SECT(J7) = BLANK(1:3)// &
     &                       STR1(1:I_LEN(STR1))//'.'//STR2(1:I_LEN(STR2))
           ELSE IF ( LEV_SECT(J7) .EQ. 2 ) THEN
              ICNT(3) = ICNT(3) + 1
              ICNT(4) = 0
              CALL INCH  ( ICNT(1), STR1 )
              CALL INCH  ( ICNT(2), STR2 )
              CALL INCH  ( ICNT(3), STR3 )
              NUM_SECT(J7) = BLANK(1:6)// &
     &                       STR1(1:I_LEN(STR1))//'.'//STR2(1:I_LEN(STR2))// &
     &                  '.'//STR3(1:I_LEN(STR3))
           ELSE IF ( LEV_SECT(J7) .EQ. 2 ) THEN
              ICNT(4) = ICNT(4) + 1
              CALL INCH  ( ICNT(1), STR1 )
              CALL INCH  ( ICNT(2), STR2 )
              CALL INCH  ( ICNT(3), STR3 )
              CALL INCH  ( ICNT(4), STR4 )
              NUM_SECT(J7) = BLANK(1:9)// &
     &                       STR1(1:I_LEN(STR1))//'.'//STR2(1:I_LEN(STR2))// &
     &                  '.'//STR3(1:I_LEN(STR3))//'.'//STR4(1:I_LEN(STR4))
         END IF
!
! ------ Set title of the sectiuon
!
         IP = I_LEN(NUM_SECT(J7))
         TIT_SECT(J7) = BUFIN(ROW_HDSE(J7))(COL_HDSE(1,J7):COL_HDSE(2,J7))
!
         ID = 18 - 1 - IP
         LBUFOUT = LBUFOUT + 1
         IF ( ID .GT. 0 ) THEN
              CALL REPEAT ( '.', 128, FILLER )
              BUFOUT(LBUFOUT) = NUM_SECT(J7)(1:IP)//' '//FILLER(1:ID)//' '// &
     &                BUFIN(ROW_HDSE(J7))(COL_HDSE(1,J7):COL_HDSE(2,J7))
            ELSE
              BUFOUT(LBUFOUT) = NUM_SECT(J7)(1:IP)//' '//TIT_SECT(J7)
         END IF
!
         IF ( LEV_SECT(J7) .EQ. 1 ) THEN
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = ' '
         END IF
 470  CONTINUE
!
! --- TRailing lines
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
!
      CALL REPEAT ( '_', 128, STR )
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = STR
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
!
! --- Scan all lones of the text
!
      DO 480 J8=ROW_HDSE(1),LBUFIN
         IP = IFIND_PL ( L_SECT, ROW_HDSE(1), J8 )
         IF ( IP .GT. 0 ) THEN
!
! ----------- This line is the line with section name
!
              ID = LEV_SECT(IP)*8
              STR = NUM_SECT(IP)(1:I_LEN(NUM_SECT(IP)))//' '//TIT_SECT(IP)
              CALL CHASHL ( STR )
!
! ----------- Add the line
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = BLANK(1:ID)//STR(1:I_LEN(STR))
!
! ----------- And add underlining
!
              CALL REPEAT ( LEV_CHAR(LEV_SECT(IP):LEV_SECT(IP)), 128, FILLER )
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = BLANK(1:ID)//FILLER(1:I_LEN(STR))
            ELSE
!
! ----------- Remove #( and #) from text
!
              IP1 = INDEX ( BUFIN(J8), '#(' )
              IP2 = INDEX ( BUFIN(J8), '#)' )
              IF ( IP1 .EQ. 1 ) THEN
                   BUFIN(J8)(1:) = BUFIN(J8)(3:)
                 ELSE IF ( IP1 .GT. 1 ) THEN
                   BUFIN(J8) = BUFIN(J8)(1:IP1-1)//BUFIN(J8)(1:IP1+2)
              END IF
!
              IF ( IP2 .EQ. 1 ) THEN
                   BUFIN(J8)(1:) = BUFIN(J8)(3:)
                 ELSE IF ( IP2 .GT. 1 ) THEN
                   BUFIN(J8) = BUFIN(J8)(1:IP2-1)//BUFIN(J8)(1:IP2+2)
              END IF
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = BUFIN(J8)(1:I_LEN(BUFIN(J8)))
         END IF
 480  CONTINUE
!
! --- Remove <B> and </B>
!
      DO 490 J9=1,LBUFOUT
         IF ( ILEN(BUFOUT(J9)) .EQ. 0 ) GOTO 490
!
 910     CONTINUE
         IN = INDEX ( BUFOUT(J9), '<B>' )
         IF ( IN .EQ. 1 ) THEN
              BUFOUT(J9) = BUFOUT(J9)(4:)
              GOTO 910
            ELSE IF ( IN .GT. 1 ) THEN
              IF ( IN .EQ. LEN(BUFOUT(J9)) ) THEN
                   BUFOUT(J9) = BUFOUT(J9)(1:IN-1)//'   '
                 ELSE
                   BUFOUT(J9) = BUFOUT(J9)(1:IN-1)//BUFOUT(J9)(IN+3:)//'   '
              END IF
              GOTO 910
         END IF
!
         IN = INDEX ( BUFOUT(J9), '</B>' )
         IF ( IN .EQ. 1 ) THEN
              BUFOUT(J9) = BUFOUT(J9)(5:)
              GOTO 910
            ELSE IF ( IN .GT. 1 ) THEN
              IF ( IN .EQ. LEN(BUFOUT(J9)) ) THEN
                   BUFOUT(J9) = BUFOUT(J9)(1:IN-1)//'    '
                 ELSE
                   BUFOUT(J9) = BUFOUT(J9)(1:IN-1)//BUFOUT(J9)(IN+4:)//'    '
              END IF
              GOTO 910
         END IF
 490  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REDOC_TXT  #!#
