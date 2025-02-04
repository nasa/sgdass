      SUBROUTINE WRI_UPM ( FILOUT, NF, JDF, XF_VAL, YF_VAL, UF_VAL, XF_ERR, &
     &                     YF_ERR, UF_ERR, CH_FLAG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WRI_UPM writes down EOP series, pole coordinates and UT1  *
! *   in ut1pm-format used by Calc and Apriori into the binary file      *
! *   FILOUT.                                                            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   FILOUT ( CHARACTER ) -- EOP file name                              *
! *       NP ( INTEGER*4 ) -- Number of points of EOP series.            *
! *       JD ( REAL*8    ) -- Array of Julian dates for EOP. Units: days,*
! *                           dimension: NP.                             *
! *   XF_VAL ( REAL*8    ) -- Array of X pole coordinates. Units: arcsec,*
! *                           dimension: NP.                             *
! *   YF_VAL ( REAL*8    ) -- Array of Y pole coordinates. Units: arcsec,*
! *                           dimension: NP.                             *
! *   UF_VAL ( REAL*8    ) -- Array of UT1-UTC angles. Units: sec of     *
! *                           time, dimension: NP.                       *
! *   XF_ERR ( REAL*8    ) -- Array of formal uncertainties of X pole    *
! *                           coordinates. Units: arcsec, dimension: NP. *
! *   YF_ERR ( REAL*8    ) -- Array of formal uncertainties of Y pole    *
! *                           coordinates. Units: arcsec, dimension: NP. *
! *   UF_ERR ( REAL*8    ) -- Array of formal uncertainties of UT1-UTC   *
! *                           angles. Units: sec of time, dimension: NP. *
! *  CH_FLAG ( CHARACTER ) -- Flag of the data:                          *
! *                           'I' -- data derived from observations;     *
! *                           'P' -- predicted data.                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *  ### 07-NOV-2000    WRI_UPM    v1.1 (c)  L. Petrov  09-JUL-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INTEGER*4  NF, IUER
      CHARACTER  FILOUT*(*), CH_FLAG(NF)*(*)
      REAL*8     JDF(NF), XF_VAL(NF), XF_ERR(NF), YF_VAL(NF), YF_ERR(NF), &
     &           UF_VAL(NF), UF_ERR(NF)
      INTEGER*4  LUN, IO, J1
      CHARACTER  STR*75, GET_CDATE*19
      LOGICAL*4  LEX
      TYPE      UT1PM_D__STRU
          REAL*8     DATE_EARLY
          REAL*8     DATE_LAST
          REAL*8     DATE_UPD1
          REAL*8     DATE_UPD2
          REAL*8     DATE_UPD3
          INTEGER*2  COUNTER_UPD
          INTEGER*2  NUMREC
          CHARACTER  SER_CODE*4
          REAL*8     SPAN
          INTEGER*2  TIDAL_FLAG
          CHARACTER  FILL_1*6
      END TYPE  UT1PM_D__STRU
!
      TYPE      UT1PM_H__STRU
          CHARACTER  CREATE_HISTORY*40
          CHARACTER  UPDATE_HISTORY*16
          CHARACTER  FILL_2*8
      END TYPE  UT1PM_H__STRU
!
      TYPE      UT1PM_V__STRU
          REAL*8     MJD
          CHARACTER  ENTRY_TYPE*2
          INTEGER*2  UPDATE_COUNTER
          CHARACTER  SERIES_CODE*4
          REAL*8     XPL_VAL
          REAL*8     XPL_ERR
          REAL*8     YPL_VAL
          REAL*8     YPL_ERR
          REAL*8     UT1_VAL
          REAL*8     UT1_ERR
      END TYPE  UT1PM_V__STRU  !  UT1PV__STRU  !
      TYPE ( UT1PM_D__STRU ) ::  UT1PM_D
      TYPE ( UT1PM_H__STRU ) ::  UT1PM_H
      TYPE ( UT1PM_V__STRU ) ::  UT1PM_V
      INTEGER*4  I_LEN, GET_UNIT
!
! --- Check: does the output file exists. If yes, then remove it
!
      INQUIRE ( FILE=FILOUT, EXIST=LEX )
      IF ( LEX ) THEN
           CALL UNLINK ( FILOUT(1:I_LEN(FILOUT))//CHAR(0) )
      END IF
!
! --- Open output ut1pm-file
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='NEW', RECL=64, ACCESS='DIRECT', &
     &       FORM='UNFORMATTED', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 8241, IUER, 'WRI_UPM', 'Error in attempt to open '// &
     &          'the output ut1pm file '//FILOUT(1:I_LEN(FILOUT))//'  IO='// &
     &           STR )
           RETURN
      END IF
!
! --- Prepare the first header record
!
      UT1PM_D%DATE_EARLY  = JDF(1)  - 2400000.5D0
      UT1PM_D%DATE_LAST   = JDF(NF) - 2400000.5D0
      UT1PM_D%DATE_UPD1   = -1.0D0
      UT1PM_D%DATE_UPD2   = -1.0D0
      UT1PM_D%DATE_UPD3   = -1.0D0
      UT1PM_D%COUNTER_UPD = -1
      UT1PM_D%NUMREC      = NF + 2
      UT1PM_D%SER_CODE    = '----'
      UT1PM_D%SPAN        = JDF(2) - JDF(1)
      IF ( UT1PM_D%SPAN .LT. 1.D0 ) UT1PM_D%SPAN = 1.D0
      UT1PM_D%TIDAL_FLAG  =  1
      CALL CLRCH ( UT1PM_D%FILL_1 )
!
! --- Write the first header record
!
      WRITE ( UNIT=LUN, REC=1, IOSTAT=IO ) UT1PM_D
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 8242, IUER, 'WRI_UPM', 'Error in attempt to '// &
     &            'write the output ERP file '//FILOUT(1:I_LEN(FILOUT))// &
     &            '  IO='//STR )
           RETURN
      END IF
!
! --- Prepare the second header record
!
      CALL CLRCH ( UT1PM_H%CREATE_HISTORY )
      UT1PM_H%CREATE_HISTORY = 'Created by finals_to_erp '
      CALL CLRCH ( UT1PM_H%UPDATE_HISTORY )
      UT1PM_H%UPDATE_HISTORY= GET_CDATE()
      CALL CLRCH ( UT1PM_H%FILL_2 )
      WRITE ( UNIT=LUN, REC=2, IOSTAT=IO ) UT1PM_H
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 8243, IUER, 'WRI_UPM', 'Error in attempt to '// &
     &         'write the output ERP file '//FILOUT(1:I_LEN(FILOUT))// &
     &         '  IO='//STR )
           RETURN
      END IF
!
! --- Cycle of the elements of the EOP array
!
      DO 410 J1=1,NF
         UT1PM_V%MJD = JDF(J1) - 2400000.5D0
         IF ( CH_FLAG(J1)(1:1) .EQ. 'I' ) THEN
              UT1PM_V%ENTRY_TYPE = 'FV'
            ELSE
              UT1PM_V%ENTRY_TYPE = 'EX'
         END IF
         UT1PM_V%UPDATE_COUNTER = -1
         UT1PM_V%SERIES_CODE = '----'
!
! ------ Units transformation
!
         UT1PM_V%XPL_VAL = XF_VAL(J1)*1.0D4
         UT1PM_V%XPL_ERR = XF_ERR(J1)*1.0D4
         UT1PM_V%YPL_VAL = YF_VAL(J1)*1.0D4
         UT1PM_V%YPL_ERR = YF_ERR(J1)*1.0D4
         UT1PM_V%UT1_VAL = UF_VAL(J1)
         UT1PM_V%UT1_ERR = UF_ERR(J1)
         WRITE ( UNIT=LUN, REC=J1+2, IOSTAT=IO ) UT1PM_V
!
         IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              CALL ERR_LOG ( 8244, IUER, 'WRI_UPM', 'Error in attempt to '// &
     &            'write the output ERP file '//FILOUT(1:I_LEN(FILOUT))// &
     &            '  IO='//STR )
              RETURN
         END IF
!!         WRITE ( LUN, '(A)', IOSTAT=IO ) STR
 410  CONTINUE
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRI_UPM  #!#
