      FUNCTION   READ64 ( LUN, ARR, NS )
! ************************************************************************
! *                                                                      *
! *   Routine READ64 reads a big chunk of data in the file opened at     *
! *   logical unit LUN. The chunk can be greater than 2G.b               *
! *                                                                      *
! *  ### 05-JUL-2013    READ64     v2.0 (c)  L. Petrov  28-APR-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*8  READ64 
      INTEGER*4  LUN
      INTEGER*1  ARR(*)
      INTEGER*8  NS, KS, IS, RB, RR, NC, LC, KC
      INTEGER*4  J1, J2
      INTEGER*4, EXTERNAL :: READ
!
#ifdef LINUX
      KS = NS
      RB = 0
      DO 410 J1=1,32*1024
         IS = READ ( %VAL(LUN), ARR(RB+1), %VAL(KS) )
         IF ( IS .EQ. -1 ) THEN
              READ64 = IS
              RETURN 
           ELSE IF ( IS .EQ. 0 ) THEN
              READ64 = RB
              RETURN 
           ELSE IF ( IS == KS ) THEN
              RB = RB + IS
              READ64 = RB
              RETURN
           ELSE IF ( IS .NE. KS ) THEN
              RB = RB + IS
              KS = KS - IS
         END IF
 410  CONTINUE 
      READ64 = -1
#endif
#ifdef DARWIN
      LC = 2000*1000*1000  ! maximum buffer lentgh
      NC = NS/LC           
      IF ( NC*LC < NS ) NC = NC + 1  ! Number of buffers to write
      RB = 0    ! Number of bytes to write
      RR = NS   ! Remaining bytes to write
      DO 410 J1=1,NC
!
! ------ KS -- The buffer length that will be used for this
! ------       chunk. May be less thant LC if this chunk the last 
!
         KS = MIN(LC,RR)
         DO 420 J2=1,32*1024
!
! --------- Wrte the chunk
!
            IS = READ ( %VAL(LUN), ARR(RB+1), %VAL(KS) )
            IF ( IS .EQ. -1 ) THEN
!
! -------------- Ahh, error
!
                 READ64 = IS
                 RETURN 
              ELSE IF ( IS .EQ. 0 ) THEN
!
! -------------- Nothing is written. If this is hte last chunk,
! -------------- good buy
!
                 IF ( J1 == NC ) THEN
                      READ64 = RB
                      RETURN 
                    ELSE
                      RB = RB + KS
                      RR = RR - KS
                 END IF
              ELSE IF ( IS == KS ) THEN
!
! -------------- We read the chunk to the end
!
                 RB = RB + IS
                 RR = RR - IS
                 IF ( J1 == NC ) THEN
!
! ------------------- If this is the last chunk, work is finished
!
                      READ64 = RB
                      RETURN
                    ELSE 
                      GOTO 820
                 END IF 
              ELSE IF ( IS .NE. KS ) THEN
!
! -------------- The chunk was only partly written. 
!
                 RB = RB + IS
                 RR = RR - IS
                 KS = KS - IS
            END IF
 420     CONTINUE 
 820     CONTINUE 
 410  CONTINUE 
      READ64 = -1
#endif
      RETURN
      END  FUNCTION   READ64  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   WRITE64 ( LUN, ARR, NS )
! ************************************************************************
! *                                                                      *
! *   Routine WRITE64 writes a big chunk of data in the file opened at   *
! *   logical unit LUN. The chunk can be greater than 2Gb.               *
! *                                                                      *
! *  ### 05-JUL-2013    WRITE64    v2.0 (c)  L. Petrov  28-APR-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*8  WRITE64 
      INTEGER*4  LUN
      INTEGER*1  ARR(*)
      INTEGER*8  NS, KS, IS, WB, WR, NC, LC, KC
      INTEGER*4  J1, J2
      INTEGER*4, EXTERNAL :: WRITE
!
#ifdef LINUX
      KS = NS
      WB = 0
      DO 410 J1=1,32*1024
         IS = WRITE ( %VAL(LUN), ARR(WB+1), %VAL(KS) )
         IF ( IS .EQ. -1 ) THEN
              WRITE64 = IS
              RETURN 
           ELSE IF ( IS .EQ. 0 ) THEN
              WRITE64 = WB
              RETURN 
           ELSE IF ( IS == KS ) THEN
              WB = WB + IS
              WRITE64 = WB
              RETURN
           ELSE IF ( IS .NE. KS ) THEN
              WB = WB + IS
              KS = KS - IS
         END IF
 410  CONTINUE 
      WRITE64 = -1
#endif
#ifdef DARWIN
      LC = 2000*1000*1000  ! maximum buffer lentgh
      NC = NS/LC           
      IF ( NC*LC < NS ) NC = NC + 1  ! Number of buffers to write
      WB = 0    ! Number of bytes to write
      WR = NS   ! Remaining bytes to write
      DO 410 J1=1,NC
!
! ------ KS -- The buffer length that will be used for this
! ------       chunk. May be less thant LC if this chunk the last 
!
         KS = MIN(LC,WR)
         DO 420 J2=1,32*1024
!
! --------- Wrte the chunk
!
            IS = WRITE ( %VAL(LUN), ARR(WB+1), %VAL(KS) )
            IF ( IS .EQ. -1 ) THEN
!
! -------------- Ahh, error
!
                 WRITE64 = IS
                 RETURN 
              ELSE IF ( IS .EQ. 0 ) THEN
!
! -------------- Nothing is written. If this is hte last chunk,
! -------------- good buy
!
                 IF ( J1 == NC ) THEN
                      WRITE64 = WB
                      RETURN 
                    ELSE
                      WB = WB + KS
                      WR = WR - KS
                 END IF
              ELSE IF ( IS == KS ) THEN
!
! -------------- We read the chunk to the end
!
                 WB = WB + IS
                 WR = WR - IS
                 IF ( J1 == NC ) THEN
!
! ------------------- If this is the last chunk, work is finished
!
                      WRITE64 = WB
                      RETURN
                    ELSE 
                      GOTO 820
                 END IF 
              ELSE IF ( IS .NE. KS ) THEN
!
! -------------- The chunk was only partly written. 
!
                 WB = WB + IS
                 WR = WR - IS
                 KS = KS - IS
            END IF
 420     CONTINUE 
 820     CONTINUE 
 410  CONTINUE 
      WRITE64 = -1
#endif
      RETURN
      END  FUNCTION   WRITE64  !#!#
