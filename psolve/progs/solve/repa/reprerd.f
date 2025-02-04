       SUBROUTINE REPRERD ( FILE_NAME, IOBS )
!
!C
!C    PURPOSE:           read record IOBS of residual file RESFxx
!C
!C    INPUT:             RESFxx (direct access file)
!C
!C    PARAMETERS:
!C
!C    CHARACTER   FILE_NAME    (in)     filename including path
!C    INTEGER*4   IOBS         (in)     # of records in RESFxx rep. of observation
!C
!C    RECORD (106 Byte, equivalence to IRESCM):
!C
!C    REAL*8      RDOC         (out)     delay residual
!C    REAL*8      RROC         (out)     rate residual
!C    REAL*8      RDERR        (out)     delay error
!C    REAL*8      RRERR        (out)     rate  error
!C    REAL*8      PDERR        (out)     delay post_fit error
!C    REAL*8      PRERR        (out)     rate  post_fit error
!C    REAL*8      RFJD         (out)     Julian date of midnight prior (or at) this obs. (days)
!C    REAL*8      RFRCT        (out)     UTC fraction of day for this obs. (days)
!C    REAL*8      RELEV        (out)     source elevation station 1
!C    REAL*8      RFAMB        (out)     Group delay ambiguity (s)
!C    INTEGER*2   IRSITE       (out)     Solve intermal site number station 1
!C    INTEGER*2   IRSTAR       (out)     Solve internal source number
!C    INTEGER*2   IRUNW        (out)     suppression status
!C    INTEGER*2   NAMB         (out)     group delay ambiguity counter
!C    INTEGER*2   IRPNTR       (out)     pointer for difference processing (obsolete)
!C    INTEGER*2   SUPSTAT_RES  (out)     suppression status
!C    INTEGER*2   UACSUP_RES   (out)     user action for suppression
!C
!C    called subroutines:
!C    none
!C
!C    calling routine:
!C    REPA
!C
!C    02-12-11
!C    Gerald Engelhardt
!C
      INCLUDE 'solve.i'                               ! CALC/SOLVE - include
      INCLUDE 'resfl.i'                               ! RESFxx     - include
      INTEGER*4 IOBS, GET_UNIT, LUN
      CHARACTER*(*) FILE_NAME
!
! --- open RESFxx
!
      LUN = GET_UNIT()
!
      OPEN( UNIT=LUN, FILE=FILE_NAME, ACCESS = 'DIRECT', RECL = 2 * JRESREC_WORDS )
!
! --- get buffer
!
      READ ( LUN, REC = IOBS ) IRESCM
!
      CLOSE ( LUN )
!
      RETURN
      END
