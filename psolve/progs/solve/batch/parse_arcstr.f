      SUBROUTINE PARSE_ARCSTR ( STRING, KPERMARC, B_ARCDIR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PARSE_ARCSTR PROGRAM SPECIFICATION
!
! 1.1 Parse string containing $SETUP section arc_files keyword information.
!
! 1.2 REFERENCES:
!
! 2.  PARSE_ARCSTR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER STRING*256
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 KPERMARC
      CHARACTER B_ARCDIR(3)*(*)
!
! B_ARCDIR - Directories in which to write arc files
! KPERMARC - True if arc files are to be permanently saved
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
! 3.  LOCAL VARIABLES
!
      CHARACTER  TOKEN*128
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*2  I, JCT, INT2_ARG
      LOGICAL*4  LEX
      INTEGER*4,     EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: OPENDIR 
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb  951215  Created due to creation of the arc file environment variable
!                feature.
!                (The algorithm was originally embedded in batch/gsetup.f.)
!                Also fix pre-existing error
!                (inability to handle the specification of 0 0 0 or
!                /directory1/ 0 0) and remove a little extraneous code.
!   pet  970612  Made test of existance of directory files. If directory
!                specified in ARC_FILES doesn't exist then error message is
!                generated.
!   pet  2004.05.19  Replaced INQUIRE statments with a system call OPENDIR, &
!                    since some Fortran implementations, f.e. Intel Fortran,
!                    does not consider directory as a valid file.
!   pet  2019.09.03  Changed type of  DIR_DISC and OPENDIR to INTEGER*8
!
!
! 5.  PARSE_ARCSTR PROGRAM STRUCTURE
!
!  EXTERNAL FUNCTIONS
!
      KPERMARC=.FALSE.
      DO I=1,3
         CALL SPLITSTRING(STRING,TOKEN,STRING )
         IF ( I.EQ.1 .AND. TOKEN.EQ.'SAVE' ) THEN
              KPERMARC=.TRUE.
              CALL SPLITSTRING(STRING,TOKEN,STRING )
           ELSE IF ( I.EQ.1 .AND. TOKEN.EQ.'NONE' ) THEN
              DO JCT = 1,3
                 CALL CLRCH ( B_ARCDIR(JCT) ) ! To fill it by blanks
              ENDDO
              GOTO 20
         ENDIF
         CALL CLRCH ( B_ARCDIR(I) )
         B_ARCDIR(I) = TOKEN
!
! ------ 0 (or 0000) instead of the file name means blank for ARC_FILES syntax
!
         IF ( B_ARCDIR(I)(1:1) .EQ. '0'  .AND. &
     &        B_ARCDIR(I)(I_LEN(B_ARCDIR(I)):I_LEN(B_ARCDIR(I))) .EQ. '0') &
     &        CALL CLRCH ( B_ARCDIR(I) )
!
         IF ( ILEN(B_ARCDIR(I)) .GT. 0 ) THEN
!
! --------- Test: does this directory really exist
!
            DIR_DESC = OPENDIR ( B_ARCDIR(I)(1:I_LEN(B_ARCDIR(I)))//CHAR(0) )
            IF ( DIR_DESC .EQ. 0 ) THEN
                 CALL ERR_LOG ( 8201, -2, 'PARSE_ARSTR', "Token -"// &
     &                B_ARCDIR(I)(1:I_LEN(B_ARCDIR(I)))//"- in the list of "// &
     &               "directories for ARC_FILES is not an existing "// &
     &               "directory. Please correct batch file and rerun SOLVE." )
                 CALL FERR ( INT2(8201), 'Error in specification of '// &
     &             'directory-name for ARC_FILES', INT2(0), INT2(0) )
                 STOP 'BATCH  Abnormal termination'
               ELSE
                 CALL CLOSEDIR ( %VAL(DIR_DESC) )
            END IF
         END IF
         IF ( TOKEN .EQ. '0' ) THEN
              B_ARCDIR(I)=' '
         ENDIF
      ENDDO
20    CONTINUE
      RETURN
      END  !#!  PARSE_ARCSTR  #!#
