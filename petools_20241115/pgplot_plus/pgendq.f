C*PGEND -- close all open graphics devices
C%void cpgendq(void);
C+
      SUBROUTINE PGENDQ
C
C Close and release any open graphics devices. All devices must be
C closed by calling either PGCLOS (for each device) or PGEND before
C the program terminates. If a device is not closed properly, some
C or all of the graphical output may be lost.
C
C This routine is similar to PGEND except it does never send prompt
C before closing device. 
C
C
C Arguments: none
C--
C 15-MAY-98   Leonid Petrov new routine
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER I
C
      DO 10 I=1,PGMAXD
         IF (PGDEVS(I).EQ.1) THEN
            CALL PGSLCT(I)
            CALL PGCLOQ
         END IF
 10   CONTINUE
      END
