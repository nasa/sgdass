      SUBROUTINE GPART ( KPART )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GPART PROGRAM SPECIFICATION
!
! 1.1 Parse partials section.
!
! 1.2 REFERENCES:
!
! 2.  GPART INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
      logical*2 kpart
!
! 2.3 OUTPUT Variables:
!
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
!      include '../include/calcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrlfl
!       CALLED SUBROUTINES: addstr,gtcalst,newfrm,newsec
!
! 3.  LOCAL VARIABLES
!
      CHARACTER STRING*256, TOKEN*256, TOKEN0*256
      INTEGER*2 LENGTH,CFREAD,TRIMLEN,I,IDUM,CNTCAL
      LOGICAL*2 KDEF,CFEOF,KGRP,KION,KAVAL,KUSE,KSET
      INTEGER*2 IUNIT,LIMIT,NUM_PARTIAL,IPARTDEF,KERR,ICT
      CHARACTER*63 DIR_PART
      CHARACTER*63 FILE_PART
      CHARACTER*8 PARTIAL_TEMP(112)
      CHARACTER*128 ERRSTR
!
!      DATA KDEF/.FALSE./,KGRP/.FALSE./,KION/.FALSE./,KAVAL/.FALSE./
!      DATA KUSE/.FALSE./,KSET/.FALSE./
!
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920704 First version. Last partial = ON from batch control file
!                is applied. if none is turned on, CHWTPART is used as default.
!   kdb   961112 No longer hard code partial names.  (Use save directory file.)
!                No longer hard coded default partial.  (Get from the file.)
!                Set num_part, since setting part_array.
!   kdb   980324 Fix error - $partials error parsing aborted due to erroneous
!                FERR call.
!   pet   2000.05.11  Added support of a keyword SET
!
! 5.  GPART PROGRAM STRUCTURE
!     Get the partials and default (the one to turn on)  from a file.
!
      iunit = 302
      limit = 112
      dir_part = pre_sav_dir(:pre_sv_len)
      file_part = AVAL_PART_FILE//char(0)
      call atmavl_n(iunit,limit,dir_part,file_part, &
     &              partial_temp,num_partial,ipartdef,kerr )
      if (kerr.ne.0 .or. num_partial .eq. 0) then
        if (kerr.ne.0) then
          errstr = 'batch gpart error:  from atmavl_n'
          call ferr( kerr, errstr, INT2(0), INT2(0) )
        else
          errstr = 'batch gpart error: part cal avail file empty = '// &
     &       dir_part(1:trimlen(dir_part))//'/'//file_part
          call ferr( INT2(401), errstr, INT2(0), INT2(0) )
        end if
      endif
      call use_glbfil_4('OR' )
!
      do ict = 1,num_partial
        part_array(ict)= partial_temp(ict)
      enddo
      part_applied= ipartdef ! set default partial
      num_part = num_partial
!
!
! Read the first record in the PARTIALS section
!
      IF ( KPART ) THEN
           LENGTH=CFREAD(STRING)
           DO WHILE(STRING(1:1).EQ.' '.AND..NOT.CFEOF(IDUM))
              DO WHILE(TRIMLEN(STRING).GT.0)
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .EQ. 'SET' .OR. TOKEN .EQ. 'set' ) THEN
!
! ------------------- Skip token SET
!
                      CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 END IF
                 TOKEN0= TOKEN
!
                 CALL SPLITSTRING ( STRING, TOKEN, STRING )
                 IF ( TOKEN .NE. 'ON' .AND. TOKEN .NE. 'OFF' .AND. &
     &                TOKEN .NE. ' ' ) THEN
                      CALL FERR ( INT2(11050), &
     &                    'GPART(BATCH) Illegal qualifier '// &
     &                    'after the keyword PARTIALS: '//TOKEN, INT2(0), INT2(0) )
                 ENDIF
!
                 IF ( TOKEN .EQ. 'ON' .OR. TOKEN(1:1) .EQ. ' ' ) THEN
                      DO ICT = 1,NUM_PARTIAL
                         IF ( TOKEN0(1:8) .EQ. PART_ARRAY(ICT) ) THEN
                              PART_APPLIED=ICT
                         END IF
!@  write ( 6, * ) ' num_partial: ', num_partial, ' ict= ',ict,' part_array =', part_array(ict) ! %%% ! %%%
                      ENDDO
                 ENDIF
!
                 IF ( TOKEN .EQ. 'OFF' ) THEN
                      DO ICT = 1,NUM_PARTIAL
                         IF ( TOKEN0(1:8) .EQ. PART_ARRAY(ICT) .AND. &
     &                        PART_APPLIED .EQ. ICT ) THEN
                              IF ( ICT .NE. IPARTDEF ) THEN
                                   PART_APPLIED = IPARTDEF
                                ELSE
                                   PART_APPLIED = 0
                              ENDIF
                         ENDIF
                     ENDDO
                 ENDIF
              ENDDO
              LENGTH=CFREAD(STRING)
           ENDDO
!
! -------- Now that this section is finished, what now?
!
           CALL CFUNRD(LENGTH,STRING )
      ENDIF
      CALL USE_GLBFIL_4 ( 'WC' )
!@  write ( 6, * ) ' part_applied: ', part_applied
!
      RETURN
      END  !#!  GPART  #!#
