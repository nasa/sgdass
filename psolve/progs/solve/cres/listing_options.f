      SUBROUTINE LISTING_OPTIONS ( LUN )
! ************************************************************************
! *                                                                      *
! *   Routine LISTING_OPTIONS writes in the file opened at channel LUN   *
! *   information about the listying status.                             *
! *                                                                      *
! *  ### 28-MAR-2002 LISTING_OPTIONS v1.1 (c) L. Petrov 12-AUG-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  LUN
      LOGICAL*2  KBIT
      CHARACTER  OUT*160
      INTEGER*4  I_LEN
!
      OUT = ' Listing_Options:'
      IF ( CRES_STYLE == CRES__PRE98 ) THEN
           OUT(I_LEN(OUT)+3:) = 'CRES_EMULATION PRE98'
         ELSE IF ( CRES_STYLE == CRES__PRE03 ) THEN
           OUT(I_LEN(OUT)+3:) = 'CRES_EMULATION PRE03'
         ELSE
           OUT(I_LEN(OUT)+3:) = 'CRES_EMULATION NO'
      END IF
!
      IF ( KBIT(PRE_IP(3),INT2(3)) ) THEN
           OUT(I_LEN(OUT)+3:) = 'BASELINES YES'
         ELSE
           OUT(I_LEN(OUT)+3:) = 'BASELINES NO'
      END IF
!
      IF ( KBIT(PRE_IP(3),INT2(2)) ) THEN
           OUT(I_LEN(OUT)+3:) = 'MINIMUM YES'
         ELSE
           OUT(I_LEN(OUT)+3:) = 'MINIMUM NO'
      END IF
      WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
!
      OUT = ' Listing_Options:'
      IF ( MAPPED_EOP_OUTPUT ) THEN
           OUT(I_LEN(OUT)+3:) = 'MAPPED_EOP_OUTPUT YES'
         ELSE
           OUT(I_LEN(OUT)+3:) = 'MAPPED_EOP_OUTPUT NO'
      END IF
!
      IF ( SEG_OUTPUT ) THEN
           OUT(I_LEN(OUT)+3:) = 'SEG_OUTPUT YES'
         ELSE
           OUT(I_LEN(OUT)+3:) = 'SEG_OUTPUT NO'
      END IF
!
      IF ( APRIORI_ZENDEL ) THEN
           OUT(I_LEN(OUT)+3:) = 'APRIORI_ZENDEL YES'
         ELSE
           OUT(I_LEN(OUT)+3:) = 'APRIORI_ZENDEL NO'
      END IF
      WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
!
      OUT = ' Listing_Options:'
      IF ( FL_NRD_TABLE ) THEN
           OUT(I_LEN(OUT)+3:) = 'NRD_TABLE YES'
         ELSE
           OUT(I_LEN(OUT)+3:) = 'NRD_TABLE NO'
      END IF
!
      IF ( FL_CHI_TABLE ) THEN
           OUT(I_LEN(OUT)+3:) = 'CHI_TABLE YES'
         ELSE
           OUT(I_LEN(OUT)+3:) = 'CHI_TABLE NO'
      END IF
!
      IF ( SRC_LISTING_STYLE == SRC_SHORT_SPOOL__FMT ) THEN
           OUT(I_LEN(OUT)+3:) = 'SRC_STAT SHORT'
         ELSE IF ( SRC_LISTING_STYLE == SRC_LONG_SPOOL__FMT ) THEN
           OUT(I_LEN(OUT)+3:) = 'SRC_STAT LONG'
         ELSE IF ( SRC_LISTING_STYLE == SRC_POST2021_SPOOL__FMT ) THEN
           OUT(I_LEN(OUT)+3:) = 'SRC_STAT POST2021'
         ELSE IF ( SRC_LISTING_STYLE == SRC_POST2024_SPOOL__FMT ) THEN
           OUT(I_LEN(OUT)+3:) = 'SRC_STAT POST2024'
         ELSE 
           OUT(I_LEN(OUT)+3:) = 'SRC_STAT PRE2004'
      END IF
!
      IF ( SEG_LISTING_STYLE == SEG_PRE2005_SPOOL__FMT ) THEN
           OUT(I_LEN(OUT)+3:) = 'SEG_STYLE PRE2005'
         ELSE IF ( SEG_LISTING_STYLE == SEG_POST2005_SPOOL__FMT ) THEN
           OUT(I_LEN(OUT)+3:) = 'SEG_STYLE POST2005'
         ELSE 
           OUT(I_LEN(OUT)+3:) = 'SEG_STYLE PRE2005'
      END IF
!
      WRITE ( LUN, '(A)' ) OUT(1:I_LEN(OUT))
!
      RETURN
      END  !#!  LISTING_OPTIONS  #!#
