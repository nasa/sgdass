       SUBROUTINE REPOBRD ( FILE_NAME, IOBS, REC_OBSF, JOBSREC_WORDS )
!
!C
!C    PURPOSE:        read scratch file OBSFGxx to get oborg.i parameters
!C
!C    INPUT:          OBSFxx (direct access file)
!C
!C    PARAMETERS:
!C
!C    FILE_NAME (in)  file name including path
!C
!C    IOBS (in)       number of record resp. observation
!C
!C    oborg.i:        COMMON /OBORG/ FJD,FRACT,...,ELEV,AZ,...,DERR,DERR_S,
!C                                   RERR,RERR_S,...,SNR,SNR_S,TEMPC,ATMPR,
!C                                   RELHU,...,UACSUP,SUPSTAT,...,ISITE,ISTAR,
!C                                   ...,LQUAL,LQUAL_S,ICORR,...
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
!
      INTEGER*4       IOBS, GET_UNIT, LUN
      INTEGER*4       JOBSREC_WORDS                 ! set in solve.i
      CHARACTER*(*)   FILE_NAME
      INTEGER*2       REC_OBSF( JOBSREC_WORDS )     ! OBSFxx record
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILE_NAME, ACCESS='DIRECT', RECL = 2 * JOBSREC_WORDS )
      READ ( LUN, REC=IOBS ) REC_OBSF
!
      CLOSE(LUN)
!
      END
