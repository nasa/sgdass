      SUBROUTINE REDOC_HTML ( MBUF, LBUFIN, BUFIN, LBUFOUT, BUFOUT, &
     &           L_SECT, LIN_SECT, COL_SECT, ROW_HDSE, COL_HDSE, LEV_SECT, &
     &           L_META, LIN_META, COL_META, ROW_HDME, COL_HDME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  REDOC_HTML transforms the array of test strings BUFIN     *
! *   from rdc-format to .html format. The output is put in the array of *
! *   strings BUFOUT.                                                    *
! *                                                                      *
! *  ### 16-MAY-2000   REDOC_HTML   v1.0 (c)  L. Petrov  17-MAY-2000 ### *
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
      CHARACTER  BLANK*128, STR*128, DATE_STR*128, REF_SECT*128, &
     &           STR1*8, STR2*8, STR3*8, STR4*8, LEV_CHAR*4, &
     &           TIT_SECT(M_SECT)*128, NUM_SECT(M_SECT)*128
      INTEGER*4  IP, IP1, IP2, ICNT(4), IN, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           J10
      BYTE       TAB_BLANK(0:255), VAL_BLANK
      INTEGER*4  ILEN, IFIND_PL, I_LEN
!
      LEV_CHAR = '=~- '
      CALL CLRCH ( BLANK )
      CALL NOUT ( 256, TAB_BLANK )
      VAL_BLANK     = 1
      TAB_BLANK(32) = VAL_BLANK
!
! --- Clear entire output buffer
!
      LBUFOUT = 0
      CALL NOUT ( LEN(BUFOUT(1))*MBUF, %REF(BUFOUT) )
!
! --- Write the header of the html-document
!
      LBUFOUT=LBUFOUT+1
      BUFOUT(LBUFOUT) = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
!
      LBUFOUT=LBUFOUT+1
      BUFOUT(LBUFOUT) = '<HTML lang="en">'
!
      LBUFOUT=LBUFOUT+1
      BUFOUT(LBUFOUT) = '<HEAD>'
!
      LBUFOUT=LBUFOUT+1
      BUFOUT(LBUFOUT) = '     <META http-equiv="Content-Type" '// &
     &                  'content="text/html; charset=iso-8859-1">'
!
      LBUFOUT=LBUFOUT+1
      BUFOUT(LBUFOUT) = '     <META NAME="GENERATOR" CONTENT="redoc" >'
!
! --- Search for title
!
      DO 410 J1=1,L_META
         IF ( BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ. 'Title' ) &
     &   THEN
              DO 420 J2=LIN_META(1,J1),LIN_META(2,J1)
                 IF ( ILEN(BUFIN(J2)) .GT. 0 ) THEN
                      LBUFOUT=LBUFOUT+1
                      BUFOUT(LBUFOUT) = '    <TITLE> '// &
     &                               BUFIN(J2)(1:I_LEN(BUFIN(J2)))//' </TITLE>'
!
                      LBUFOUT=LBUFOUT+1
                      BUFOUT(LBUFOUT) = '</HEAD>'
!
                      LBUFOUT=LBUFOUT+1
                      BUFOUT(LBUFOUT) = '<BODY>'
!
                      LBUFOUT=LBUFOUT+1
                      BUFOUT(LBUFOUT) = '    <CENTER><B><BIG> '// &
     &                                  BUFIN(J2)(1:I_LEN(BUFIN(J2)))// &
     &                                  ' </BIG></B></CENTER>'
                 END IF
 420          CONTINUE
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = '<HR SIZE="6">'
         END IF
!
! ------ Search for author(s)
!
         IF ( BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ. 'Author' &
     &        .OR. &
     &        BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ. 'Authors') &
     &   THEN
              DO 430 J3=LIN_META(1,J1),LIN_META(2,J1)
                 LBUFOUT = LBUFOUT + 1
                 BUFOUT(LBUFOUT) = '    <CENTER><I> '// &
     &                             BUFIN(J3)(1:I_LEN(BUFIN(J3)))// &
     &                             ' </I></CENTER>'
 430          CONTINUE
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = '<P>'
         END IF
!
! ------ Search for date
!
         IF ( BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ. 'Date' ) &
     &   THEN
              DO 440 J4=LIN_META(1,J1),LIN_META(2,J1)
                 IF ( ILEN(BUFIN(J4)) .GT. 0 ) THEN
!
! ------------------- We extract and save the date
!
                      CALL CLRCH ( DATE_STR )
                      DATE_STR = BUFIN(J4)(1:I_LEN(BUFIN(J4)))
                 END IF
 440          CONTINUE
         END IF
!
! ------ Search for abstract
!
         IF ( BUFIN(ROW_HDME(J1))(COL_HDME(1,J1):COL_HDME(2,J1)) .EQ.'Abstract') &
     &   THEN
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = '<CENTER><I><BIG>'//' Abstract: '// &
     &                          '</CENTER></I></BIG>'
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = ' '
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = '<P><I><UL><FONT SIZE=2>'
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = ' '
              DO 450 J5=LIN_META(1,J1),LIN_META(2,J1)
                 LBUFOUT = LBUFOUT + 1
                 BUFOUT(LBUFOUT) = BUFIN(J5)(1:I_LEN(BUFIN(J5)))
 450          CONTINUE
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = ' '
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = '</FONT></I></UL><P>'
         END IF
 410  CONTINUE
!
! --- Make a haeader for table of contents
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '<CENTER><BIG><I>'//' Table of contents: '// &
     &                  '</CENTER></BIG></I>'
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '<DL>'
!
! --- Making table of contents
!
      ICNT(1) = 0
      DO 460 J6=1,L_SECT
         CALL CLRCH ( STR )
!
! ------ Build the numbering of the section
!
         IF ( LEV_SECT(J6) .EQ. 1 ) THEN
              ICNT(1) = ICNT(1) + 1
              ICNT(2) = 0
              CALL INCH  ( ICNT(1), STR1 )
              NUM_SECT(J6) = STR1(1:I_LEN(STR1))
           ELSE IF ( LEV_SECT(J6) .EQ. 2 ) THEN
              ICNT(2) = ICNT(2) + 1
              ICNT(3) = 0
              CALL INCH  ( ICNT(1), STR1 )
              CALL INCH  ( ICNT(2), STR2 )
              NUM_SECT(J6) = STR1(1:I_LEN(STR1))//'.'//STR2(1:I_LEN(STR2))
           ELSE IF ( LEV_SECT(J6) .EQ. 2 ) THEN
              ICNT(3) = ICNT(3) + 1
              ICNT(4) = 0
              CALL INCH  ( ICNT(1), STR1 )
              CALL INCH  ( ICNT(2), STR2 )
              CALL INCH  ( ICNT(3), STR3 )
              NUM_SECT(J6) = STR1(1:I_LEN(STR1))//'.'//STR2(1:I_LEN(STR2))// &
     &                  '.'//STR3(1:I_LEN(STR3))
           ELSE IF ( LEV_SECT(J6) .EQ. 2 ) THEN
              ICNT(4) = ICNT(4) + 1
              CALL INCH  ( ICNT(1), STR1 )
              CALL INCH  ( ICNT(2), STR2 )
              CALL INCH  ( ICNT(3), STR3 )
              CALL INCH  ( ICNT(4), STR4 )
              NUM_SECT(J6) = STR1(1:I_LEN(STR1))//'.'//STR2(1:I_LEN(STR2))// &
     &                  '.'//STR3(1:I_LEN(STR3))//'.'//STR4(1:I_LEN(STR4))
         END IF
!
! ------ Build title of the section and a link tag
!
         IP = I_LEN(NUM_SECT(J6))
         TIT_SECT(J6) = BUFIN(ROW_HDSE(J6))(COL_HDSE(1,J6):COL_HDSE(2,J6))
!
         CALL CLRCH ( REF_SECT )
         REF_SECT = '<A HREF="#section'//NUM_SECT(J6)(1:IP)//'">'
!
! ------ Add the item to the table of contents
!
         LBUFOUT = LBUFOUT + 1
         IF ( LEV_SECT(J6) .EQ. 1 ) THEN
              BUFOUT(LBUFOUT) = '<P><DD> '// &
     &                NUM_SECT(J6)(1:IP)//' &nbsp; '// &
     &                REF_SECT(1:I_LEN(REF_SECT))//' '// &
     &                BUFIN(ROW_HDSE(J6))(COL_HDSE(1,J6):COL_HDSE(2,J6))// &
     &                ' </A> </DD><P>'
            ELSE IF ( LEV_SECT(J6) .EQ. 2 ) THEN
              BUFOUT(LBUFOUT) = '<DD> <DL><DD> '// &
     &                NUM_SECT(J6)(1:IP)//' &nbsp; '// &
     &                REF_SECT(1:I_LEN(REF_SECT))//' '// &
     &                BUFIN(ROW_HDSE(J6))(COL_HDSE(1,J6):COL_HDSE(2,J6))// &
     &                ' </A> </DD> </DL><DD>'
            ELSE IF ( LEV_SECT(J6) .EQ. 3 ) THEN
              BUFOUT(LBUFOUT) = '<DD> <DL><DD><DL><DD> '// &
     &                NUM_SECT(J6)(1:IP)//' &nbsp; '// &
     &                REF_SECT(1:I_LEN(REF_SECT))//' '// &
     &                BUFIN(ROW_HDSE(J6))(COL_HDSE(1,J6):COL_HDSE(2,J6))// &
     &                ' </A> </DD> </DL></DD></DL><DD>'
         END IF
 460  CONTINUE
!
! --- TRailin items of the table of contents
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '</DL> '
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '<HR SIZE="2">'
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '<LISTING>'
!
! --- San the text
!
      DO 470 J7=ROW_HDSE(1),LBUFIN
         IP = IFIND_PL ( L_SECT, ROW_HDSE(1), J7 )
         IF ( IP .GT. 0 ) THEN
!
! ----------- Aga! This line is the line with section title
! ----------- Switch font to proportional
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = '</FONT></LISTING><CENTER><BIG><B>'
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = '<P>'
!
! ----------- Add the title name with an anchor for a link tag
!
              STR = '<A NAME="section'//NUM_SECT(IP)(1:I_LEN(NUM_SECT(IP)))// &
     &              '"> </A> '// &
     &              NUM_SECT(IP)(1:I_LEN(NUM_SECT(IP)))//' &nbsp; '// &
     &              TIT_SECT(IP)
              CALL CHASHL ( STR )
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = STR(1:I_LEN(STR))
!
! ----------- Swith font to fixed
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = '</B></BIG></CENTER><LISTING> <FONT SIZE=4>'
            ELSE
 910          CONTINUE
!
! ----------- Replace "<" with "&lt;"  and  ">" with "&gt;"
!
              IP1 = INDEX ( BUFIN(J7), '<' )
              IF ( IP1 .EQ. 1 ) THEN
                   BUFIN(J7)(1:) = '&lt;'//BUFIN(J7)(2:)
                   GOTO 910
                 ELSE IF ( IP1 .GT. 1 ) THEN
                   BUFIN(J7) = BUFIN(J7)(1:IP1-1)//'&lt;'//BUFIN(J7)(IP1+1:)
                   GOTO 910
              END IF
!
              IP2 = INDEX ( BUFIN(J7), '>' )
              IF ( IP2 .EQ. 1 ) THEN
                   BUFIN(J7)(1:) = '&gt;'//BUFIN(J7)(2:)
                   GOTO 910
                 ELSE IF ( IP2 .GT. 1 ) THEN
                   BUFIN(J7) = BUFIN(J7)(1:IP2-1)//'&gt;'//BUFIN(J7)(IP2+1:)
                   GOTO 910
              END IF
!
! ----------- Remove #( and #) from text
! ----------- Set font color just after #( and restor font color just after
! ----------- #)
!
              IP1 = INDEX ( BUFIN(J7), '#(' )
              IF ( IP1 .EQ. 1 ) THEN
                   BUFIN(J7)(1:) = '<B><FONT COLOR="30B030">'//BUFIN(J7)(3:)
                 ELSE IF ( IP1 .GT. 1 ) THEN
                   BUFIN(J7) = '<B><FONT COLOR="30B030">'// &
     &                          BUFIN(J7)(1:IP1-1)//BUFIN(J7)(IP1+2:)
              END IF
!
              IP2 = INDEX ( BUFIN(J7), '#)' )
              IF ( IP2 .EQ. 1 ) THEN
                   BUFIN(J7)(1:) = '</B></FONT>'//BUFIN(J7)(3:)
                 ELSE IF ( IP2 .GT. 1 ) THEN
                   BUFIN(J7) = '</B></FONT>'// &
     &                         BUFIN(J7)(1:IP2-1)//BUFIN(J7)(IP2+2:)
              END IF
!
              LBUFOUT = LBUFOUT + 1
              BUFOUT(LBUFOUT) = BUFIN(J7)(1:I_LEN(BUFIN(J7)))
         END IF
 470  CONTINUE
!
! --- Add trailing lines
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '</FONT></LISTING>'
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '<P>'
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '<HR SIZE="6">'
!
! --- Search for contact info
!
      DO 480 J8=1,L_META
         IF ( BUFIN(ROW_HDME(J8))(COL_HDME(1,J8):COL_HDME(2,J8)) .EQ.'Contact' ) &
     &   THEN
              DO 490 J9=LIN_META(1,J8),LIN_META(2,J8)
                 LBUFOUT = LBUFOUT + 1
                 BUFOUT(LBUFOUT) = BUFIN(J9)(1:I_LEN(BUFIN(J9)))//' <BR>'
 490          CONTINUE
         END IF
 480  CONTINUE
!
! --- Replace &lt;B&gt; with <B> and &lt;/B&gt; with </B>
!
      DO 4100 J10=1,LBUFOUT
         IF ( ILEN(BUFOUT(J10)) .EQ. 0 ) GOTO 4100
!
 920     CONTINUE
         IN = INDEX ( BUFOUT(J10), '&lt;B&gt;' )
         IF ( IN .EQ. 1 ) THEN
              BUFOUT(J10) = '<B>'//BUFOUT(J10)(10:)
              GOTO 920
            ELSE IF ( IN .GT. 1 ) THEN
              IF ( IN .EQ. LEN(BUFOUT(J10)) ) THEN
                   BUFOUT(J10) = BUFOUT(J10)(1:IN-1)//'<B>      '
                 ELSE
                   BUFOUT(J10) = BUFOUT(J10)(1:IN-1)//'<B>'// &
     &                           BUFOUT(J10)(IN+9:)//'      '
              END IF
              GOTO 920
         END IF
!
         IN = INDEX ( BUFOUT(J10), '&lt;/B&gt;' )
         IF ( IN .EQ. 1 ) THEN
              BUFOUT(J10) = '<B>'//BUFOUT(J10)(11:)
              GOTO 920
            ELSE IF ( IN .GT. 1 ) THEN
              IF ( IN .EQ. LEN(BUFOUT(J10)) ) THEN
                   BUFOUT(J10) = BUFOUT(J10)(1:IN-1)//'</B>      '
                 ELSE
                   BUFOUT(J10) = BUFOUT(J10)(1:IN-1)//'</B>'// &
     &                           BUFOUT(J10)(IN+10:)//'      '
              END IF
              GOTO 920
         END IF
 4100  CONTINUE
!
! --- Add the date
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '<EM>'
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '     Last update: '//DATE_STR(1:I_LEN(DATE_STR))
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '</EM>'
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = ' '
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '</BODY>'
!
      LBUFOUT = LBUFOUT + 1
      BUFOUT(LBUFOUT) = '</HTML>'
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REDOC_HTML  #!#
