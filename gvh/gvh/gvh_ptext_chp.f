      SUBROUTINE GVH_PTEXT_CHP ( GVH, ISEG, TITLE, NLINES, BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_PTEXT_CHP  puts the text chapter with title TITLE and *
! *   the text buffer BUF which consists of NLINES into the next chapter *
! *   of the ISEG-th segment of GVH internal data structure.             *
! *   The trailing blanks in BUF are ignored. The text and the title     *
! *   should not contain characters with decimal codes 10 and 26.        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ISEG ( INTEGER*4 ) -- Index of the segment where the preamble    *
! *                           section will be added. The index should    *
! *                           be in the range of available segments.     *
! *    TITLE ( CHARACTER ) -- Title of the segment.                      *
! *   NLINES ( INTEGER*4 ) -- The number of lines in the buffer with     *
! *                           contents of the chapter.                   *
! *      BUF ( CHARACTER ) -- Buffer with contents of the chapter.       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     GVH ( GVH__STRU      ) -- Data structure which keeps internal    *
! *                               information related to the database of *
! *                               an astro/geo VLBI experiment.          *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 21-NOV-2001  GVH_PTEXT_CHP  v2.0 (c) L. Petrov 18-NOV-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU ) ::  GVH
      INTEGER*4  ISEG, NLINES, IUER
      CHARACTER  TITLE*(*), BUF(NLINES)*(*)
      CHARACTER  STR*32, STR1*32
      INTEGER*4  J1, J2, NT, NLEN, ILN, IER
      ADDRESS__TYPE  IADR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4031, IUER, 'GVH_PTEXT_CHP', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
! --- Check validity of ISEG argument
!
      IF ( ISEG .LE.  0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL ERR_LOG ( 4032, IUER, 'GVH_PTEXT_CHP', 'Wrong argument '// &
     &         'ISEG '//STR )
           RETURN
      END IF
!
      IF ( ISEG .GT. GVH__MSEG ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ISEG, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( GVH__MSEG, STR1 )
           CALL ERR_LOG ( 4033, IUER, 'GVH_PTEXT_CHP', 'Wrong argument '// &
     &         'ISEG: '//STR(1:I_LEN(STR))//' -- it exceeds the '// &
     &         'maximal number of segments GVH__MSEG: '//STR1 )
           RETURN
      END IF
!
      GVH%TEXT(ISEG)%NTIT = GVH%TEXT(ISEG)%NTIT + 1
      IF ( GVH%TEXT(ISEG)%NTIT .GT. GVH__MTIT ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( GVH__MTIT, STR )
           CALL ERR_LOG ( 4034, IUER, 'GVH_PTEXT_CHP', 'The number of '// &
     &         'chapters in text section exceed the current limit '// &
     &         'GVH__MTIT: '//STR )
           RETURN
      END IF
      NT = GVH%TEXT(ISEG)%NTIT
!
! --- Learn the length of the buffer of the body of text chapter
!
      NLEN = 1
      DO 410 J1=1,NLINES
         NLEN = NLEN + ILEN(BUF(J1)) + 1
 410  CONTINUE
!
! --- Allocate dynamic memory for keeping the title
!
      GVH%TEXT(ISEG)%TITLE_LEN(NT) = LEN(TITLE)+1
      IER = IUER
      CALL GVH_ALLOCATE ( GVH, GVH%TEXT(ISEG)%TITLE_LEN(NT), &
     &                         GVH%TEXT(ISEG)%TITLE_ADR(NT), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4035, IUER, 'GVH_PTEXT_CHP', 'The error in '// &
     &         'attempt to allocate dynamic memory for a title' )
           RETURN
      END IF
!
! --- Copy the title
!
      CALL MEMCPY ( %VAL(GVH%TEXT(ISEG)%TITLE_ADR(NT)), %REF(TITLE), &
     &              %VAL(GVH%TEXT(ISEG)%TITLE_LEN(NT)-1)  )
!
! --- Putting chapter delimiter
!
      CALL MEMCPY ( %VAL( GVH%TEXT(ISEG)%TITLE_ADR(NT)+ &
     &                    GVH%TEXT(ISEG)%TITLE_LEN(NT)-1 ), &
     &                    %REF(GVH__CHAPTER_DEL), %VAL(1) )
!
! --- Allocate dynamic memory for keeping the body
!
      GVH%TEXT(ISEG)%BODY_LEN(NT) = NLEN
      IER = IUER
      CALL GVH_ALLOCATE ( GVH, GVH%TEXT(ISEG)%BODY_LEN(NT), &
     &                         GVH%TEXT(ISEG)%BODY_ADR(NT), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4036, IUER, 'GVH_PTEXT_CHP', 'The error in '// &
     &         'attempt to allocate dynamic memory for the body of the '// &
     &         'text buffer' )
           RETURN
      END IF
!
! --- Copying content of the buffer
!
      IADR = GVH%TEXT(ISEG)%BODY_ADR(NT)
      DO 420 J2=1,NLINES
         ILN = ILEN(BUF(J2))
         IF ( ILN .GT. 0 ) THEN
!
! ----------- Copy the line without trailing blanks
!
              CALL MEMCPY ( %VAL(IADR), %REF(BUF(J2)), %VAL(ILN) )
              IADR = IADR + ILN
         END IF
!
! ------ Putting record delimiter
!
         CALL MEMCPY ( %VAL(IADR), %REF(GVH__RECORD_DEL), %VAL(1) )
         IADR = IADR + 1
 420  CONTINUE
!
! --- Putting chapter delimiter
!
      CALL MEMCPY ( %VAL(IADR), %REF(GVH__CHAPTER_DEL), %VAL(1) )
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_PTEXT_CHP  !#!#
