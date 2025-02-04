      SUBROUTINE SUPSTAT_UPD ( ISTAR, ISITE, SUPSTAT, UACSUP, AUTO_SUP )
! ************************************************************************
! *                                                                      *
! *   Routine  SUPSTAT_UPD  analyzes circumstances of the observation,   *
! *   global settings of the solution and updates status of the          *
! *   observation under consideration in according with status of the    *
! *   source, station, baseline.                                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    ISTAR ( INTEGER*2 ) -- Source code.                               *
! *    ISITE ( INTEGER*2 ) -- Array (dim=2) of station code of the       *
! *                           baseline under consideration.              *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  SUPSTAT ( INTEGER*2 ) -- 32-bits field of suppression status        *
! *                           which is set automatically in according    *
! *                           with circumstances of the observation and  *
! *                           global settings (such as quality code      *
! *                           limit, applying ionosphere calibration     *
! *                           etc. )                                     *
! * AUTO_SUP ( INTEGER*2 ) -- 32-bits field of suppression status        *
! *                           for the post-2005 suppression scheme.      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   UACSUP ( INTEGER*2 ) -- 16-bits field of user action for           *
! *                           suppression.                               *
! *                                                                      *
! *  ###  12-JUL-98   SUPSTAT_UPD  v2.0  (c)  L. Petrov 07-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'socom.i'
      INTEGER*2  ISTAR, ISITE(2), SUPSTAT(2), UACSUP
      INTEGER*4  AUTO_SUP
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      LOGICAL*4, EXTERNAL :: DATYP_INQ
      LOGICAL*2, EXTERNAL :: KBIT
!
! --- Test the baseline selection flag
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- Phase delay solution type
!
           IF ( KBIT ( IBLSEL_P(1,ISITE(1)),ISITE(2) ) ) THEN
                CALL SBIT ( SUPSTAT, DSBS__SPS, INT2(0) )
                IF ( SUPMET == SUPMET__META ) THEN
                     AUTO_SUP = IBCLR ( AUTO_SUP, INT4(DSBS__SPS) )
                END IF
              ELSE
                CALL SBIT ( SUPSTAT, DSBS__SPS, INT2(1) )
                IF ( SUPMET == SUPMET__META ) THEN
                     AUTO_SUP = IBSET ( AUTO_SUP, INT4(DSBS__SPS) )
                END IF
           END IF
         ELSE
!
! -------- Group delay (or rate only) solution type
!
           IF ( KBIT ( IBLSEL_G(1,ISITE(1)),ISITE(2) ) ) THEN
                CALL SBIT ( SUPSTAT, DSBS__SPS, INT2(0) )
                IF ( SUPMET == SUPMET__META ) THEN
                     AUTO_SUP = IBCLR ( AUTO_SUP, INT4(DSBS__SPS) )
                END IF
              ELSE
                CALL SBIT ( SUPSTAT, DSBS__SPS, INT2(1) )
                IF ( SUPMET == SUPMET__META ) THEN
                     AUTO_SUP = IBSET ( AUTO_SUP, INT4(DSBS__SPS) )
                END IF
           END IF
      END IF
!
! --- Test of the source selection flag
!
      IF ( KBIT(ISRSEL(1),ISTAR) ) THEN
           CALL SBIT ( SUPSTAT, DSSO__SPS, INT2(0) )
           IF ( SUPMET == SUPMET__META ) THEN
                AUTO_SUP = IBCLR ( AUTO_SUP, INT4(DSSO__SPS) )
           END IF
         ELSE
           CALL SBIT ( SUPSTAT, DSSO__SPS, INT2(1) )
           IF ( SUPMET == SUPMET__META ) THEN
                AUTO_SUP = IBSET ( AUTO_SUP, INT4(DSSO__SPS) )
           END IF
      END IF
!
! --- Setting usage status bits
!
      IF ( SUPMET .NE. SUPMET__META ) THEN
           CALL SUPUSE_SET ( SUPSTAT )
      END IF
!
      RETURN
      END  SUBROUTINE  SUPSTAT_UPD  !#!#
