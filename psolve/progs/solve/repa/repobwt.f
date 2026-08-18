       SUBROUTINE REPOBWT ( FILE_NAME, IOBS, REC_OBSF, JOBSREC_WORDS )
!
!C
!C    PURPOSE:        write scratch file OBSFxx
!C
!C    PARAMETERS:
!C
!C    FILE_NAME (in)  file name including path
!C
!C    INTEGER*4       IOBS            number of record resp. observation
!C
!C    oborg.i:        COMMON /OBORG/ FJD,FRACT,...,ELEV,AZ,...,DERR,DERR_S,
!C                                   RERR,RERR_S,...,SNR,SNR_S,TEMPC,ATMPR,
!C                                   RELHU,...,UACSUP,SUPSTAT,...,ISITE,ISTAR,
!C                                   ...,LQUAL,LQUAL_S,ICORR,...
!C
!C    02-12-11
!C    Gerald Engelhardt
!
      INTEGER*4       IOBS, GET_UNIT, LUN
      INTEGER*4       JOBSREC_WORDS                 ! set in solve.i
      CHARACTER*(*)   FILE_NAME
      INTEGER*2       REC_OBSF( JOBSREC_WORDS )     ! OBSFxx record
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILE_NAME, ACCESS='DIRECT', RECL = 2 * JOBSREC_WORDS )
!
      WRITE ( LUN, REC=IOBS ) REC_OBSF
!
      CLOSE(LUN)
!
      RETURN
      END
