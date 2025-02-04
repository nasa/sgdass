      SUBROUTINE MDLPL_DIAGI ( L_PTS, TIME_ARR, VAL_ARR, SIG_ARR, MODU_ARR, &
     &                         MODC_ARR, I_TYP, DBNAME, STA_NAM, &
     &                         PREF_NAME, DIAGI_S, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MDLPL_DIAGI  full fields of the data structure DIAGI for  *
! *   plotting the estimates of segmented parameters. MDLPL_DIAGI only   *
! *   fills the slots of the data structure, but doen't display the plot.*
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      L_PTS ( INTEGER*4 ) -- Number of extracted values (number of    *
! *                             time epochs).                            *
! *   TIME_ARR ( REAL*8    ) -- Array of arguments: time epochs in hours.*
! *                             Dimension: L_PTS.                        *
! *    VAL_ARR ( REAL*8    ) -- Array of values. Units: psec for delay,  *
! *                             microseconds of time for UT1,            *
! *                             microarcseconds for pole coordinates.    *
! *                             Dimension: L_PTS.                        *
! *    SIG_ARR ( REAL*8    ) -- Array of formal uncertainties. Units are *
! *                             the same as for VAL_ARR.                 *
! *                             Dimension: L_PTS.                        *
! *   MODU_ARR ( REAL*8    ) -- Array of a priori values of EOP (zero    *
! *                             for other types). Value of a priori EOP  *
! *                             applyed before estimation plus           *
! *                             a constant shift found to make           *
! *                             the difference between estimates and     *
! *                             a priori value at the moment of the      *
! *                             first epoch to be zero.                  *
! *   MODC_ARR ( REAL*8    ) -- Array of model values of EOP (zero for   *
! *                             other types). It is ued for comparison   *
! *                             purpose only. A constant shift is        *
! *                             applied to all mopdel values in order to *
! *                             make the difference between estimates    *
! *                             and a priori value at the moment of the  *
! *                             first epoch to be zero.                  *
! *      I_TYP ( INTEGER*4 ) -- Type of plotting values. The following   *
! *                             types are supported (defined in mdlcm.i) *
! *                          ICLP_TYP -- "Piese-wise clock function"     *
! *                          ICLT_TYP -- "Total clock function"          *
! *                          IATP_TYP -- "Piese-wise atmosphere path     *
! *                                       delay"                         *
! *                          IXPL_TYP -- "Piese-wise X pole coordinate"  *
! *                          IYPL_TYP -- "Piese-wise Y pole coordinate"  *
! *                          IUT1_TYP -- "Piese-wise UT1 arguments"      *
! *    STA_NAM ( CHARACTER ) -- Station name.                            *
! *     DBNAME ( CHARACTER ) -- String with database name and version    *
! *                             number.                                  *
! *  PREF_NAME ( CHARACTER ) -- Prefix string with pathname which will   *
! *                             be prepend before the second part of     *
! *                             filename of hardcopies of the plots.     *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    DIAGI_S ( RECORD    ) -- Data structure with plot parameters for  *
! *                             DiaGI interface. Data structure is ready *
! *                             for displaying the plot.                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *         IUER ( INTEGER*4, OPT ) -- Universal error handler.          *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  03-NOV-1997  MDLPL_DIAGI  v1.5  (c)  L. Petrov 06-APR-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'diagi_local.i'
      INCLUDE    'diagi.i'
      INCLUDE    'mdlcm.i'
      INTEGER*4  L_PTS, I_TYP, IUER
      REAL*8     TIME_ARR(L_PTS), VAL_ARR(L_PTS), SIG_ARR(L_PTS), &
     &           MODU_ARR(L_PTS),  MODC_ARR(L_PTS)
      CHARACTER  STA_NAM*(*), DBNAME*(*), PREF_NAME*(*)
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      REAL*8     VAL_MIN, VAL_MAX, VAL_SPR, TIME_SPAN, FILL
      INTEGER*4  J1, IPR
      CHARACTER  ZAG*128, STA_NAM_LOW*8, STR*32
      INTEGER*4  LEN_DIAGI
      PARAMETER  ( FILL = 0.02 )
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Learh the length of DiaGI object
!
      LEN_DIAGI = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
!
! --- Transform the station name to the symbols of lower registr
!
      CALL TRAN ( 12, STA_NAM, STA_NAM_LOW )
      IPR = ILEN(PREF_NAME)
!
! --- Search for minimal and maximal value
!
      DO 410 J1=1,L_PTS
         IF ( J1 .EQ. 1 ) THEN
              VAL_MIN = VAL_ARR(1) - SIG_ARR(1)
              VAL_MAX = VAL_ARR(1) + SIG_ARR(1)
           ELSE
              IF ( VAL_ARR(J1)-SIG_ARR(J1) .LT. VAL_MIN ) VAL_MIN = VAL_ARR(J1)- &
     &                                                              SIG_ARR(J1)
              IF ( VAL_ARR(J1)+SIG_ARR(J1) .GT. VAL_MAX ) VAL_MAX = VAL_ARR(J1)+ &
     &                                                              SIG_ARR(J1)
         END IF
 410  CONTINUE
!
! --- Take special care for empty plots
!
      VAL_SPR = VAL_MAX - VAL_MIN
      VAL_MIN = VAL_MIN - FILL*VAL_SPR
      VAL_MAX = VAL_MAX + FILL*VAL_SPR
      IF ( VAL_SPR .LT. 1.D-30 ) THEN
           VAL_MIN = -1.0
           VAL_MAX =  1.0
           TIME_ARR(2) = TIME_ARR(L_PTS)
           L_PTS   =  2
      END IF
!
! --- Setting up the values of the DIAGI internal data structure for the further
! --- plotting
!
      CALL NOUT  ( LEN_DIAGI, DIAGI_S )
!
! --- Set the screen size
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'DIAGI_SCREEN', STR )
      CALL TRAN   ( 11, STR, STR )
      DIAGI_S%IDEV = IXS__DEF
      IF ( STR(1:4) .EQ. 'TINY' ) THEN
            DIAGI_S%IDEV = 1
          ELSE IF ( STR(1:5) .EQ. 'SMALL' ) THEN
            DIAGI_S%IDEV = 2
          ELSE IF ( STR(1:3) .EQ. 'BIG' ) THEN
            DIAGI_S%IDEV = 3
          ELSE IF ( STR(1:4) .EQ. 'HUGE' ) THEN
            DIAGI_S%IDEV = 4
          ELSE IF ( STR(1:4) .EQ. 'VAST' ) THEN
            DIAGI_S%IDEV = 5
      END IF
!
      DIAGI_S%ILST(1)   = 2
      DIAGI_S%ICLR      = 1
      CALL CLRCH ( ZAG                )
      CALL CLRCH ( DIAGI_S%NAME       )
      CALL CLRCH ( DIAGI_S%ARG_UNITS  )
      DIAGI_S%NPOI(1)   = L_PTS
      DIAGI_S%NPOI(2)   = L_PTS
      DIAGI_S%NPOI(3)   = L_PTS
      DIAGI_S%ADR_X8(1) = LOC(TIME_ARR)
      DIAGI_S%ADR_Y8(1) = LOC(VAL_ARR)
      DIAGI_S%ADR_E8(1) = LOC(SIG_ARR)
      DIAGI_S%ADR_X8(2) = LOC(TIME_ARR)
      DIAGI_S%ADR_Y8(2) = LOC(MODU_ARR)
      DIAGI_S%ADR_E8(2) = 0
      DIAGI_S%ADR_X8(3) = LOC(TIME_ARR)
      DIAGI_S%ADR_Y8(3) = LOC(MODC_ARR)
      DIAGI_S%ADR_E8(3) = 0
      DIAGI_S%LER(1)    = .TRUE.
      DIAGI_S%LER(2)    = .FALSE.
      DIAGI_S%LER(3)    = .FALSE.
!
! --- Set up a title of the plot
!
      IF ( I_TYP .EQ. ICLP_TYP ) THEN
!
! -------- Segmented clock type
!
           DIAGI_S%NCLR    = 1
           DIAGI_S%ICOL(1) = 2
           DIAGI_S%ZAG     = DBNAME(1:I_LEN(DBNAME))//'  '//STA_NAM//'  '// &
     &                     'Piece-wise clock function (psec)'
           DIAGI_S%NAME    = PREF_NAME(1:IPR)//'clo_'// &
     &                       STA_NAM_LOW(1:I_LEN(STA_NAM_LOW))
           DIAGI_S%ARG_UNITS = 'Time (hours)'
           DIAGI_S%IPST(1) = 3
           DIAGI_S%IWST(1) = 3
        ELSE IF ( I_TYP .EQ. ICLT_TYP ) THEN
!
! -------- Total clocks
!
           DIAGI_S%NCLR    = 1
           DIAGI_S%ICOL(1) = 8
           DIAGI_S%ZAG     = DBNAME(1:I_LEN(DBNAME))//'  '//STA_NAM//'  '// &
     &                     'Total clock function (nsec)'
           DIAGI_S%NAME    = PREF_NAME(1:IPR)//'tcl_'// &
     &                       STA_NAM_LOW(1:I_LEN(STA_NAM_LOW))
           DIAGI_S%ARG_UNITS = 'Time (hours)'
           DIAGI_S%IPST(1) = 2
           DIAGI_S%IWST(1) = 2
        ELSE IF ( I_TYP .EQ. IATP_TYP ) THEN
!
! -------- Segmented atmosphere path delay
!
           DIAGI_S%NCLR    = 1
           DIAGI_S%ICOL(1) = 3
           DIAGI_S%ZAG     = DBNAME(1:I_LEN(DBNAME))//'  '//STA_NAM//'  '// &
     &                     'Piece-wise atmosphere path delay (psec)'
           DIAGI_S%NAME    = PREF_NAME(1:IPR)//'atm_'// &
     &                       STA_NAM_LOW(1:I_LEN(STA_NAM_LOW))
           DIAGI_S%ARG_UNITS = 'Time (hours)'
           DIAGI_S%IPST(1) = 3
           DIAGI_S%IWST(1) = 3
        ELSE IF ( I_TYP .EQ. IXPL_TYP ) THEN
!
! -------- X pole coordinates
!
           DIAGI_S%NCLR    = 3
           DIAGI_S%ICOL(1) = 1
           DIAGI_S%ZAG     = DBNAME(1:I_LEN(DBNAME))//'  '// &
     &                     'Piece-wise X pole coordinates (microarcs)'
           DIAGI_S%NAME    = PREF_NAME(1:IPR)//'eop_xpl'
           DIAGI_S%ARG_UNITS = 'Time (hours)'
           DIAGI_S%IPST(1) = 3
           DIAGI_S%IWST(1) = 3
        ELSE IF ( I_TYP .EQ. IYPL_TYP ) THEN
!
! -------- Y pole coordinates
!
           DIAGI_S%NCLR    = 3
           DIAGI_S%ICOL(1) = 1
           DIAGI_S%ZAG     = DBNAME(1:I_LEN(DBNAME))//'  '// &
     &                     'Piece-wise Y pole coordinates (microarcs)'
           DIAGI_S%NAME    = PREF_NAME(1:IPR)//'eop_ypl'
           DIAGI_S%ARG_UNITS = 'Time (hours)'
           DIAGI_S%IPST(1) = 3
           DIAGI_S%IWST(1) = 3
        ELSE IF ( I_TYP .EQ. IUT1_TYP ) THEN
!
! -------- UT1
!
           DIAGI_S%NCLR    = 3
           DIAGI_S%ICOL(1) = 1
           DIAGI_S%ZAG     = DBNAME(1:I_LEN(DBNAME))//'  '// &
     &                     'Piece-wise UT1 adjustments (microsec)'
           DIAGI_S%NAME    = PREF_NAME(1:IPR)//'eop_ut1'
           DIAGI_S%ARG_UNITS = 'Time (hours)'
           DIAGI_S%IPST(1) = 3
           DIAGI_S%IWST(1) = 3
      END IF
!
      DIAGI_S%ICOL(2) = 12
      DIAGI_S%IPST(2) = 1
      DIAGI_S%IWST(2) = 2
      DIAGI_S%IBST(2) = 0
      DIAGI_S%ILST(2) = 3
!
      DIAGI_S%ICOL(3) = 13
      DIAGI_S%IPST(3) = 1
      DIAGI_S%IWST(3) = 1
      DIAGI_S%IBST(3) = 0
      DIAGI_S%ILST(3) = 3
!
      DIAGI_S%IBST(1) = 4
      DIAGI_S%ILST(1) = 2
      DIAGI_S%ICLR    = 1
!
      TIME_SPAN = ( TIME_ARR(L_PTS) - TIME_ARR(1) )
      DIAGI_S%XMIN    = TIME_ARR(1)     - FILL*TIME_SPAN
      DIAGI_S%XMAX    = TIME_ARR(L_PTS) + FILL*TIME_SPAN
!
      DIAGI_S%YMIN    = VAL_MIN
      DIAGI_S%YMAX    = VAL_MAX
      DIAGI_S%ITRM    = 0
      DIAGI_S%IBATCH  = 0
      DIAGI_S%STATUS  = DIA__DEF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MDLPL_DIAGI  #!#
