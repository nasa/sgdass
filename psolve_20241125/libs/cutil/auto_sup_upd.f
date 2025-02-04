      SUBROUTINE AUTO_SUP_UPD ( ISITE, ISTAR, ELEV, AUTO_SUP )
! ************************************************************************
! *                                                                      *
! *   Routine  AUTO_SUP_SET  analyzes circumstances of the observation   *
! *   and global settings of the solution. Then it sets suppression      *
! *   status of the observation under consideration in according with    *
! *   this status, user action of suppression and suppression method     *
! *   SUPMET.                                                            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *    ISITE ( INTEGER*2 ) -- Array (dim=2) of station code of the       *
! *                           baseline under consideration.              *
! *    ISTAR ( INTEGER*2 ) -- Source code.                               *
! *     ELEV ( REAL*8    ) -- Array (dim=2) of elevation angles for      *
! *                           both stations.                             *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * AUTO_SUP ( INTEGER*4 ) -- 32-bits field of suppression status        *
! *                           which is set automatically in according    *
! *                           with circumstances of the observation and  *
! *                           global settings (such as quality code      *
! *                           limit, applying ionosphere calibration     *
! *                           etc. )                                     *
! *                                                                      *
! *  ### 26-JUN-2007  AUTO_SUP_UPD  v1.1 (c) L. Petrov  10-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'socom.i'
      INTEGER*4  AUTO_SUP
      INTEGER*2  ISITE(2), ISTAR
      INTEGER*2  IARG_I2
      INTEGER*4  INT4
      REAL*8     ELEV(2)
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: DATYP_INQ
      INT4(IARG_I2) = INT(IARG_I2,KIND=4)
!
! --- Test of elevation cutoff limit
!
      AUTO_SUP = IBCLR ( AUTO_SUP, INT4(CUEL__SPS) )
      IF ( ELEV(1) .LT. ELVCUT(ISITE(1)) ) THEN
           AUTO_SUP = IBSET ( AUTO_SUP, INT4(CUEL__SPS) )
      END IF
      IF ( ELEV(2) .LT. ELVCUT(ISITE(2)) ) THEN
           AUTO_SUP = IBSET ( AUTO_SUP, INT4(CUEL__SPS) )
      END IF
!
! --- Test the baseline selection flag
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- Phase delay solution type
!
           IF ( KBIT ( IBLSEL_P(1,ISITE(1)),ISITE(2) ) ) THEN
                AUTO_SUP = IBCLR ( AUTO_SUP, INT4(DSBS__SPS) )
              ELSE
                AUTO_SUP = IBSET ( AUTO_SUP, INT4(DSBS__SPS) )
           END IF
         ELSE
!
! -------- Group delay (or rate only) solution type
!
           IF ( KBIT ( IBLSEL_G(1,ISITE(1)),ISITE(2) ) ) THEN
                AUTO_SUP = IBCLR ( AUTO_SUP, INT4(DSBS__SPS) )
              ELSE
                AUTO_SUP = IBSET ( AUTO_SUP, INT4(DSBS__SPS) )
           END IF
      END IF
!
! --- Test the source selection flag
!
      IF ( KBIT(ISRSEL(1),ISTAR) ) THEN
           AUTO_SUP = IBCLR ( AUTO_SUP, INT4(DSSO__SPS) )
         ELSE
           AUTO_SUP = IBSET ( AUTO_SUP, INT4(DSSO__SPS) )
      END IF
!
      RETURN
      END  SUBROUTINE   AUTO_SUP_UPD  !#!#
