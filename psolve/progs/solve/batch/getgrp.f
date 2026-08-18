      SUBROUTINE GETGRP(CNTCAL,STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GETGRP PROGRAM SPECIFICATION
!
! 1.1 Get a calibration group.
!
! 1.2 REFERENCES:
!
! 2.  GETGRP INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
      INTEGER*2 CNTCAL
!
! CNTCAL - Current calibration frame number
! STRING - String containing a line from the CALIBRATIONS GROUP section
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gcalib
!       CALLED SUBROUTINES: newfrm,gtcalst,addstr
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*8 TOKEN
      INTEGER*2 LENGTH,CFREAD,IDUM
      LOGICAL*2 KPICK,CFEOF,KDEF,KSTAT
      CHARACTER*1 BSLASH
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215  replaced '\' with BSLASH
!
! 5.  GETGRP PROGRAM STRUCTURE
!
! Initialize some flags
!
      KDEF=.FALSE.
      KPICK=.FALSE.
      KSTAT=.FALSE.
!
! Pull off the first token from STRING
!
      CALL SPLITSTRING(STRING,TOKEN,STRING )
      DO WHILE (TOKEN.NE.' ')
!
! If end of line continuation character, read next line
!
        IF(TOKEN.EQ.BSLASH) THEN
          LENGTH=CFREAD(STRING)
          IF(STRING(1:1).EQ.'$'.OR.CFEOF(IDUM)) THEN
            CALL FERR( INT2(13010), 'ILLEGAL CONTINUATION LINE '// &
     &           STRING(1:10), INT2(0), INT2(0) )
          ENDIF
!
! PICK keyword
!
        ELSE IF(TOKEN.EQ.'PICK') THEN
          IF(KPICK) CALL FERR( INT2(13020), 'PICK USED TWICE', INT2(0), &
     &       INT2(0) )
          CALL NEWFRM(CNTCAL )
          KPICK=.TRUE.
!
! PICK keyword
!
        ELSE IF(TOKEN.EQ.'ELSE') THEN
          IF(.NOT.KPICK) THEN
            CALL FERR( INT2(13030), 'ELSE BEFORE PICK', INT2(0), INT2(0) )
          ELSE IF(KDEF) THEN
            CALL FERR( INT2(13040), 'ELSE AFTER DEFAULT', INT2(0), INT2(0) )
          ENDIF
          CALL NEWFRM(CNTCAL )
!
! DEFAULT keyword
!
        ELSE IF(TOKEN.EQ.'DEFAULT') THEN
          IF(KDEF ) THEN
            CALL FERR( INT2(13050), 'DEFAULT USED TWICE', INT2(0), INT2(0) )
          ELSE IF (KPICK) THEN
            CALL ADDSTR(CNTCAL,TOKEN )
            KDEF=.TRUE.
          ELSE
            CALL FERR( INT2(13060), 'DEFAULT BEFORE PICK', INT2(0), INT2(0) )
          ENDIF
!
! STATIONS keyword
!
        ELSE IF(TOKEN.EQ.'STATIONS') THEN
          KSTAT=.TRUE.
          CALL NEWFRM(CNTCAL )
          CALL GTCALST(CNTCAL,STRING )
        ELSE
          CALL ADDSTR(CNTCAL,TOKEN )
        ENDIF
        CALL SPLITSTRING(STRING,TOKEN,STRING )
      ENDDO
!
      IF(.NOT.KSTAT) THEN
        CALL NEWFRM(CNTCAL )
        CALL GTCALST(CNTCAL,STRING )
      ENDIF
!
! Error if there is no PICK clause
!
      IF(.NOT.KPICK) CALL FERR( INT2(13070), 'NO PICK CLAUSE', INT2(0), &
     &   INT2(0) )
      RETURN
      END
