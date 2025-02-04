      SUBROUTINE REDOC_PARSE ( MBUF, LBUFIN, BUFIN, &
     &       M_SECT, L_SECT, LIN_SECT, COL_SECT, ROW_HDSE, COL_HDSE, LEV_SECT, &
     &       M_META, L_META, LIN_META, COL_META, ROW_HDME, COL_HDME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  REDOC_PARSE parses the array of text strings BUFIN and    *
! *   extracts section and meta tags.                                    *
! *                                                                      *
! *  ### 16-MAY-2000   REDOC_PARSE  v1.0 (c)  L. Petrov 17-MAY-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MBUF, LBUFIN, IUER
      CHARACTER  BUFIN(LBUFIN)*(*)
      INTEGER*4  M_SECT, M_META, L_SECT, L_META
      INTEGER*4  LIN_SECT(2,M_SECT), COL_SECT(2,M_SECT), LEV_SECT(M_SECT)
      INTEGER*4  LIN_META(2,M_META), COL_META(2,M_META)
      INTEGER*4  ROW_HDSE(M_SECT),   ROW_HDME(M_META), &
     &           COL_HDSE(2,M_SECT), COL_HDME(2,M_META)
      CHARACTER  STR_LIN*32, BLANK*128
      INTEGER*4  IP1, IP2, IP3, IP4, KP1, KP2, J1
      LOGICAL*4  FL_SECT, FL_META
      BYTE       TAB_BLANK(0:255), VAL_BLANK
      INTEGER*4  I_LEN, LIB$SPANC
!
      CALL CLRCH ( BLANK )
      CALL NOUT ( 256, TAB_BLANK )
      VAL_BLANK     = 1
      TAB_BLANK(32) = VAL_BLANK
!
      L_SECT = 0
      L_META = 0
      FL_SECT = .FALSE.
      FL_META = .FALSE.
!
      DO 410 J1=1,LBUFIN
         CALL CLRCH (     STR_LIN )
         CALL INCH  ( J1, STR_LIN )
!
! ------ Search for attributes of new section and meta-tags
!
         IP1 = INDEX ( BUFIN(J1), '#/' )
         IP2 = INDEX ( BUFIN(J1), '#\' )
         IP3 = INDEX ( BUFIN(J1), '#{' )
         IP4 = INDEX ( BUFIN(J1), '#}' )
!
         IF ( IP1 .GT. 0  .OR.  IP2 .GT. 2  .OR. &
     &        IP3 .GT. 0  .OR.  IP4 .GT. 0       ) THEN
              IF ( FL_SECT ) THEN
                   LIN_SECT(2,L_SECT) = J1-1
                   COL_SECT(2,L_SECT) = I_LEN(BUFIN(J1-1))
                   FL_SECT = .FALSE.
              END IF
              IF ( FL_META ) THEN
                   LIN_META(2,L_META) = J1-1
                   COL_META(2,L_META) = I_LEN(BUFIN(J1-1))
                   FL_META = .FALSE.
              END IF
         END IF
!
         IF ( IP1 .GT. 0 ) THEN
!
! ----------- It was a new section
!
              FL_SECT = .TRUE.
              L_SECT = L_SECT + 1
              LEV_SECT(L_SECT) = 1
!
! ----------- Determine a section level
!
              IF ( INDEX ( BUFIN(J1), '##/' ) .GT. 0 ) LEV_SECT(L_SECT) = 2
              IF ( INDEX ( BUFIN(J1), '###/' ) .GT. 0 ) LEV_SECT(L_SECT) = 3
              IF ( INDEX ( BUFIN(J1), '####/' ) .GT. 0 ) LEV_SECT(L_SECT) = 4
!
! ----------- Look for the beginning the section name
!
              KP1 = LIB$SPANC ( BUFIN(J1)(IP1+2:), TAB_BLANK, VAL_BLANK ) +IP1+1
              IF ( IP2 .EQ. 0 ) THEN
                   CALL ERR_LOG ( 7211, IUER, 'REDOC_PARSE', 'Tag #\ was not '// &
     &                 'found in processing the line '//STR_LIN )
                   RETURN
              END IF
!
! ----------- Look for the end of section name
!
              KP2 = I_LEN( BUFIN(J1)(1:IP2-LEV_SECT(L_SECT)) )
              ROW_HDSE(  L_SECT) = J1
              COL_HDSE(1,L_SECT) = KP1
              COL_HDSE(2,L_SECT) = KP2
              LIN_SECT(1,L_SECT) = J1+1
              COL_SECT(1,L_SECT) = 1
         END IF
!
         IF ( IP3 .GT. 0 ) THEN
!
! ----------- This is a meta tag
!
              FL_META = .TRUE.
              L_META  = L_META + 1
!
! ----------- Look for the beginning of the meta-name
!
              KP1 = LIB$SPANC ( BUFIN(J1)(IP3+2:), TAB_BLANK, VAL_BLANK ) + IP3+1
              IF ( IP4 .EQ. 0 ) THEN
                   CALL ERR_LOG ( 7212, IUER, 'REDOC_PARSE', 'Tag #} was not '// &
     &                 'found in processing the line '//STR_LIN )
                   RETURN
              END IF
!
! ----------- look for the end of meta-name
!
              KP2 = I_LEN( BUFIN(J1)(1:IP4-1) )
              ROW_HDME(  L_META) = J1
              COL_HDME(1,L_META) = KP1
              COL_HDME(2,L_META) = KP2
              LIN_META(1,L_META) = J1+1
              COL_META(1,L_META) = 1
         END IF
 410  CONTINUE
!
! --- Close sevtion or meta array
!
      IF ( FL_SECT ) THEN
           LIN_SECT(2,L_SECT) = J1-1
           COL_SECT(2,L_SECT) = I_LEN(BUFIN(J1-1))
           FL_SECT = .FALSE.
      END IF
      IF ( FL_META ) THEN
           LIN_META(2,L_META) = J1-1
           COL_META(2,L_META) = I_LEN(BUFIN(J1-1))
           FL_META = .FALSE.
      END IF
!
      IF ( L_SECT .EQ. 0 ) THEN
           CALL ERR_LOG ( 7213, IUER, 'REDOC_PARSE', 'No section was found '// &
     &         'during parsing the input file' )
           RETURN
      END IF
!
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
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!
