      SUBROUTINE GVH_PTEXT_BUF ( GVH, TITLE, NLINES, BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVH_PTEXT puts
! *                                                                      *
! *  ### 21-NOV-2001  GVH_PTEXT_BUF  v1.0 (c) L. Petrov 25-NOV-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      TYPE ( GVH__STRU ) ::  GVH
      INTEGER*4   NLINES, IUER
      CHARACTER  TITLE*(*), BUF(NLINES)*(*)
      CHARACTER  CHAPTER_DEL*1, RECORD_DEL*1, STR*32
      PARAMETER  ( CHAPTER_DEL = CHAR(26) )
      PARAMETER  ( RECORD_DEL  = CHAR(10) )
      INTEGER*4  J1, J2, NT, NLEN, ILN, IER
      ADDRESS__TYPE  IADR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( GVH%STATUS .NE. GVH__INITIALIZED ) THEN
           CALL ERR_LOG ( 4031, IUER, 'GVH_PTEXT_BUF', 'The GVH data '// &
     &         'structure was not initialized. Please, use gvh_init first' )
           RETURN
      END IF
!
      GVH%TEXT(GVH%SEG)%NTIT = GVH%TEXT(GVH%SEG)%NTIT + 1
      IF ( GVH%TEXT(GVH%SEG)%NTIT .GT. GVH__MTIT ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( GVH__MTIT, STR )
           CALL ERR_LOG ( 4032, IUER, 'GVH_PTEXT_BUF', 'The number of '// &
     &         'chapters in text section exceed the current limit '// &
     &         'GVH__MTIT: '//STR )
           RETURN
      END IF
      NT = GVH%TEXT(GVH%SEG)%NTIT
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
      GVH%TEXT(GVH%SEG)%TITLE_LEN(NT) = LEN(TITLE)+1
      IER = IUER
      CALL GVH_ALLOCATE ( GVH, GVH%TEXT(GVH%SEG)%TITLE_LEN(NT), &
     &                         GVH%TEXT(GVH%SEG)%TITLE_ADR(NT), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4033, IUER, 'GVH_PTEXT_BUF', 'The error in attempt '// &
     &         'to allocate dynamic memory for a title' )
           RETURN
      END IF
!
! --- Copy the title
!
      CALL MEMCPY ( %VAL(GVH%TEXT(GVH%SEG)%TITLE_ADR(NT)), %REF(TITLE), &
     &              %VAL(GVH%TEXT(GVH%SEG)%TITLE_LEN(NT)-1)  )
!
! --- Putting chapter delimiter
!
      CALL MEMCPY ( %VAL( GVH%TEXT(GVH%SEG)%TITLE_ADR(NT)+ &
     &                    GVH%TEXT(GVH%SEG)%TITLE_LEN(NT)-1 ), &
     &                    %REF(CHAPTER_DEL), %VAL(1) )
!
! --- Allocate dynamic memory for keeping the body
!
      GVH%TEXT(GVH%SEG)%BODY_LEN(NT) = NLEN
      IER = IUER
      CALL GVH_ALLOCATE ( GVH, GVH%TEXT(GVH%SEG)%BODY_LEN(NT), &
     &                         GVH%TEXT(GVH%SEG)%BODY_ADR(NT), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4034, IUER, 'GVH_PTEXT_BUF', 'The error in attempt '// &
     &         'to allocate dynamic memory for the body of the text buffer' )
           RETURN
      END IF
!
! --- Copying content of the buffer
!
      IADR = GVH%TEXT(GVH%SEG)%BODY_ADR(NT)
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
         CALL MEMCPY ( %VAL(IADR), %REF(RECORD_DEL), %VAL(1) )
         IADR = IADR + 1
 420  CONTINUE
!
! --- Putting chapter delimiter
!
      CALL MEMCPY ( %VAL(IADR), %REF(CHAPTER_DEL), %VAL(1) )
!
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  GVH_PTEXT_BUF  !#!#
