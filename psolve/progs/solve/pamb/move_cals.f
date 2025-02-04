      SUBROUTINE MOVE_CALS ( NOBS_FROM, IDBF_FROM, NOBS_TO, IDBF_TO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MOVE_CALS  moves values of a) station dependent           *
! *   calibrations; b) zenith troposphere calibrations; c) baseline      *
! *   dependent calibration from the database with index IDBF_FROM       *
! *   to the observation of the database with index IRBF_TO.             *
! *                                                                      *
! *   It assumed that the databases IDBF_FROM and IDBF_TO are for the    *
! *   same session.                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * NOBS_FROM ( INTEGER*4 ) -- Number of observations in the database    *
! *                            "from" which is a donor of calibrations.  *
! * IDBF_FROM ( INTEGER*4 ) -- Index of the database in the scratch file.*
! *                            This database is a donor of calibrations  *
! *   NOBS_TO ( INTEGER*4 ) -- Number of observations in the database    *
! *                            "to" which is an acceptor of calibrations *
! *   IDBF_TO ( INTEGER*4 ) -- Index of the database in the scratch file.*
! *                            This database is an acceptor of           *
! *                            calibrations.                             *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                          Input: switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will be put on     *
! *                                 stdout.                              *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! *  ###  13-NOV-97    MOVE_CALS   v1.0  (c)  L. Petrov  13-NOV-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'oborg.i'
      INTEGER*4  NOBS_FROM, IDBF_FROM, NOBS_TO, IDBF_TO, IUER
!
      INTEGER*4  J1, J2, IB, IM, IREC
      INTEGER*2  ISITE1_FROM, ISITE2_FROM
      REAL*8     FJD_FROM, FRACTC_FROM
      REAL*8     CALIBS_FROM(2,2,15), CALIBZ_FROM(2,2,15), CALIBB_FROM(2,15), &
     &           TEMPC1_FROM, TEMPC2_FROM, &
     &           ATMPR1_FROM, ATMPR2_FROM, &
     &           RELHU1_FROM, RELHU2_FROM
!
      IB = 1
      IM = 0
!
! --- Open scratch file
!
      CALL ACS_OBSFIL ( 'O' )
      DO 410 J1=1,NOBS_FROM
!
! ------ Read the IREC-th record of scratch file of the database "from"
!
         IREC = (IDBF_FROM-1)+J1
         CALL USE_OBSFIL ( IOBSFIL, IREC, 'R' )
!
! ------ Store parameters of the observation
!
         FJD_FROM    = FJD
         FRACTC_FROM = FRACTC
         ISITE1_FROM = ISITE(1)
         ISITE2_FROM = ISITE(2)
!
! ------ Store calibrations
!
         CALL COPY_V ( 2*2*15, CALIBS, CALIBS_FROM )
         CALL COPY_V ( 2*2*15, CALIBZ, CALIBZ_FROM )
         CALL COPY_V (   2*15, CALIBB, CALIBB_FROM )
         TEMPC1_FROM = TEMPC(1)
         TEMPC2_FROM = TEMPC(2)
         ATMPR1_FROM = ATMPR(1)
         ATMPR2_FROM = ATMPR(2)
         RELHU1_FROM = RELHU(1)
         RELHU2_FROM = RELHU(2)
!
! ------ Reading database IDBF_TO
!
         DO 420 J2=IB,NOBS_TO
!
! --------- Reading the IREC-th record
!
            IREC = (IDBF_TO-1)+J2
            CALL USE_OBSFIL ( IOBSFIL, IREC, 'R' )
            IF ( DABS(FJD-FJD_FROM)       .LT. 1.D-10  .AND. &
     &           DABS(FRACTC_FROM-FRACTC) .LT. 1.D-10  .AND. &
     &           ISITE1_FROM .EQ. ISITE(1)             .AND. &
     &           ISITE2_FROM .EQ. ISITE(2)                    ) THEN
!
! -------------- We matched observations of two databases
!
                 IB = J2 + 1
                 IM = IM + 1
!
                 CALL COPY_V ( 2*2*15, CALIBS_FROM, CALIBS )
                 CALL COPY_V ( 2*2*15, CALIBZ_FROM, CALIBZ )
                 CALL COPY_V (   2*15, CALIBB_FROM, CALIBB )
                 TEMPC(1) = TEMPC1_FROM
                 TEMPC(2) = TEMPC2_FROM
                 ATMPR(1) = ATMPR1_FROM
                 ATMPR(2) = ATMPR2_FROM
                 RELHU(1) = RELHU1_FROM
                 RELHU(2) = RELHU2_FROM
!
! -------------- Writing down the updated record
!
                 CALL USE_OBSFIL ( IOBSFIL, IREC, 'W' )
                 GOTO 410
            END IF
 420     CONTINUE
 410  CONTINUE
      CALL ACS_OBSFIL ( 'C' )
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  MOVE_CALS  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION  CALS_IDEN ( CALS1, CALS2 )
! ************************************************************************
! *                                                                      *
! *   Logical function  CALS_IDEN  makes comparison of two CALS data     *
! *   structures which holds calibrations and their status. If the       *
! *   data structures CALS1 and CALS2 are identical then                 *
! *   CALS_IDEN = .TRUE.                                                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * CALS1 ( RECORD ) -- Data strucutre which contains information about  *
! *                     calibration status.                              *
! * CALS2 ( RECORD ) -- Data strucutre which contains information about  *
! *                     calibration status.                              *
! *                                                                      *
! *  ###  14-NOV-97    CALS_IDEN   v2.0  (c)  L. Petrov  18-NOV-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT  NONE
      LOGICAL*4 CALS_IDEN
      INCLUDE   'cals.i'
      TYPE ( CALS_STRU ) ::  CALS1, CALS2
      INTEGER*4  J1, J2, J3, J4, J5
!
      CALS_IDEN = .FALSE.
      IF ( CALS1%STATUS .NE. CAL__DONE    ) RETURN
      IF ( CALS2%STATUS .NE. CAL__DONE    ) RETURN
      IF ( CALS1%L_STA  .NE. CALS2%L_STA  ) RETURN
      IF ( CALS1%L_SCAL .NE. CALS2%L_SCAL ) RETURN
      IF ( CALS1%L_ZENC .NE. CALS2%L_ZENC ) RETURN
      IF ( CALS1%L_CONT .NE. CALS2%L_CONT ) RETURN
      IF ( CALS1%L_MCAL .NE. CALS2%L_MCAL ) RETURN
!
      DO 410 J1=1,CALS1%L_STA
         IF ( CALS1%STANAM(J1)   .NE.   CALS2%STANAM(J1)   ) RETURN
         IF ( CALS1%GION_AVL(J1) .NEQV. CALS2%GION_AVL(J1) ) RETURN
         IF ( CALS1%GION_APL(J1) .NEQV. CALS2%GION_APL(J1) ) RETURN
         IF ( CALS1%PION_AVL(J1) .NEQV. CALS2%PION_AVL(J1) ) RETURN
         IF ( CALS1%PION_APL(J1) .NEQV. CALS2%PION_APL(J1) ) RETURN
         DO 420 J2=1,CALS1%L_SCAL
            IF ( CALS1%SCAL_AVL(J2,J1) .NEQV. CALS2%SCAL_AVL(J2,J1) ) RETURN
            IF ( CALS1%SCAL_APL(J2,J1) .NEQV. CALS2%SCAL_APL(J2,J1) ) RETURN
            IF ( CALS1%SCAL(J2)        .NE.   CALS2%SCAL(J2)        ) RETURN
 420     CONTINUE
!
         DO 430 J3=1,CALS1%L_ZENC
            IF ( CALS1%ZENC_APL(J3,J1) .NEQV. CALS2%ZENC_APL(J3,J1) ) RETURN
            IF ( CALS1%ZENC(J3)        .NE.   CALS2%ZENC(J3)        ) RETURN
 430     CONTINUE
 410  CONTINUE
!
      DO 440 J4=1,CALS1%L_CONT
         IF ( CALS1%CONT(J4)       .NE.   CALS2%CONT(J4)       ) RETURN
         IF ( CALS1%CONT_LCODE(J4) .NE.   CALS2%CONT_LCODE(J4) ) RETURN
         IF ( CALS1%CONT_AVL(J4)   .NEQV. CALS2%CONT_AVL(J4)   ) RETURN
         IF ( CALS1%CONT_APL(J4)   .NEQV. CALS2%CONT_APL(J4)   ) RETURN
 440  CONTINUE
!
      IF ( CALS1%L_MCAL .GT. 0 ) THEN
           DO 450 J5=1,CALS1%L_MCAL
              IF ( CALS1%MCAL(J5)       .NE. CALS2%MCAL(J5)       ) RETURN
              IF ( CALS1%MCAL_LCODE(J5) .NE. CALS2%MCAL_LCODE(J5) ) RETURN
              IF ( CALS1%MCAL_AVL(J5)   .NEQV. CALS2%MCAL_AVL(J5)   ) RETURN
              IF ( CALS1%MCAL_APL(J5)   .NEQV. CALS2%MCAL_APL(J5)   ) RETURN
 450       CONTINUE
      END IF
!
      CALS_IDEN = .TRUE.
!
      RETURN
      END  !#!  CALS_IDEN  #!#
