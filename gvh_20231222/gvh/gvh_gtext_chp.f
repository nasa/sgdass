      SUBROUTINE GVH_GTEXT_CHP ( GVH, ISEG, N_CHP, M_LIN, L_LIN, TITLE, &
     &                           BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_GTEXT_CHP gets the title and contents of the N_CHP-th *
! *   text chapter of the ISEG-th segment of the GVH internal data       *
! *   structure.                                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      GVH ( GVH__STRU ) -- Data structure which keeps internal        *
! *                           information related to the database of     *
! *                           an astro/geo VLBI experiment.              *
! *     ISEG ( INTEGER*4 ) -- Index of the segment where the text        *
! *                           section will be sought. The index should   *
! *                           be in the range of available segments.     *
! *    N_CHP ( INTEGER*4 ) -- Index of the chapeter. The index should    *
! *                           be in the range [1, GVH%TEXT(ISEG)%NTIT ]. *
! *    M_LIN ( INTEGER*4 ) -- The maximal number of lines of the chapter *
! *                           contents.                                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    TITLE ( CHARACTER ) -- Title of the segment.                      *
! *    L_LIN ( INTEGER*4 ) -- The number of lines in the buffer with     *
! *                           contents of the chapter.                   *
! *      BUF ( CHARACTER ) -- Buffer with the chapter contents.          *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 21-NOV-2001  GVH_GTEXT_CHP  v1.1 (c) L. Petrov 22-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU ) ::  GVH
      INTEGER*4  ISEG, N_CHP, M_LIN, L_LIN, IUER
      CHARACTER  TITLE*(*), BUF(M_LIN)*(*)
      CHARACTER  STR*32, STR1*32, STR2*32
      INTEGER*4  ILN_CHA, LN, ILN, IP, J1, IER
      ADDRESS__TYPE :: IAD_CHA, IAD_END
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LIB$SCANC 
      INTEGER*1  TAB(0:255), TAB_VAL
!
! --- Intialization of the delimiters table
!
      CALL NOUT ( 256, TAB )
      TAB_VAL = 1
      TAB(10) = TAB_VAL ! Carriage return -- record delimiter
      TAB(26) = TAB_VAL ! chapter delimiter
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4081, IUER, 'GVH_GTEXT_CHP', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      IF ( ISEG .LE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL ERR_LOG ( 4082, IUER, 'GVH_GTEXT_CHP', 'Wrong parameter '// &
     &         'ISEG: '//STR )
           RETURN
      END IF
!
      IF ( ISEG .GT. GVH%NSEG ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( GVH%NSEG, STR1 )
           CALL ERR_LOG ( 4083, IUER, 'GVH_GTEXT_CHP', 'Wrong parameter '// &
     &         'ISEG: '//STR(1:I_LEN(STR))//' -- it exceeds the total '// &
     &         'numer of segments GVH%NSEG: '//STR1 )
           RETURN
      END IF
!
      IF ( N_CHP .LE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( N_CHP, STR )
           CALL ERR_LOG ( 4084, IUER, 'GVH_GTEXT_CHP', 'Parameter N_CHP '// &
     &         'is too small: '//STR )
           RETURN
      END IF
!
      IF ( N_CHP > GVH%TEXT(ISEG)%NTIT ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( N_CHP, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( GVH%TEXT(ISEG)%NTIT, STR1 )
           CALL ERR_LOG ( 4085, IUER, 'GVH_GTEXT_CHP', 'The indes of '// &
     &         'the requrested chapters in text section exceed the total '// &
     &         'number of chapters: '//STR )
           RETURN
      END IF
!
! --- Extract chapter's title
!
      LN = MIN ( GVH%TEXT(ISEG)%TITLE_LEN(N_CHP)-1, LEN(TITLE) )
      CALL MEMCPY ( %REF(TITLE), %VAL(GVH%TEXT(ISEG)%TITLE_ADR(N_CHP)), &
     &              %VAL(LN) )
!
      IAD_CHA = GVH%TEXT(ISEG)%BODY_ADR(N_CHP)
      ILN_CHA = GVH%TEXT(ISEG)%BODY_LEN(N_CHP)
      IAD_END = GVH%TEXT(ISEG)%BODY_ADR(N_CHP) + ILN_CHA-1
!
! --- Cycle on records in the chapter
!
      L_LIN = 0
      IF ( ILN_CHA == 0 ) THEN
           CALL ERR_LOG ( 0, IUER, ' ', ' ' )
           RETURN
      END IF
      DO 410 J1=1,1024*1024*1024
         L_LIN = L_LIN + 1
         IF ( L_LIN > M_LIN ) THEN
              L_LIN = M_LIN
              CALL CLRCH ( STR )
              CALL INCH  ( M_LIN, STR )
              CALL CLRCH ( STR1 ) 
              CALL INCH  ( GVH%NSEG, STR1 )
              CALL CLRCH ( STR2 ) 
              CALL INCH  ( ISEG, STR2 )
              CALL ERR_LOG ( 4086, IUER, 'GVH_GTEXT_CHP', 'Too small '// &
     &            'parameter M_LIN: '//STR(1:I_LEN(STR))//' the total '// &
     &            'number of records in chapeter '//STR1(1:I_LEN(STR1))// &
     &            ' of segment '//STR2(1:I_LEN(STR2))//' is greater' )
              RETURN 
         END IF
         CALL CLRCH ( BUF(L_LIN) )
!
! ------ Seach for delimiter
!
         IP = LIB$SCANC ( %VAL(IAD_CHA), TAB, TAB_VAL, %VAL(ILN_CHA) )
!
         ILN = MIN ( IP-1, ILN_CHA, LEN(BUF(L_LIN)) )
         IF ( ILN > 0 ) THEN
!
! ----------- Copy the next line of the chapter's body into BUF
!
              CALL MEMCPY ( %REF(BUF(L_LIN)), %VAL(IAD_CHA), %VAL(ILN) )
         END IF
         IAD_CHA = IAD_CHA + IP
         ILN_CHA = ILN_CHA - IP
         IF ( IAD_CHA .GE. IAD_END ) GOTO 810
 410  CONTINUE
 810  CONTINUE
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_GTEXT_CHP  !#!#
