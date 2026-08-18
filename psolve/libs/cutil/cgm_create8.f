      SUBROUTINE CGM_CREATE8 ( IDIRECT, FNAME, FILDES, NP, STRING, KERR )
      IMPLICIT NONE
!
! 1.  CGM_CREATE PROGRAM SPECIFICATION
!
! 1.1 Create CGM file
!
! 1.2 REFERENCES:
!
! 2.  CGM_CREATE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 NP
      CHARACTER*(*) FNAME,STRING
!
! FNAME - Name of file to create
! NP - Number of parameters
! STRING - 'M'= make a new file; 'U' means update if file exists
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 FILDES
      INTEGER*2 IDIRECT(*), KERR
!
! FILDES - File descriptor of the created file
! IDIRECT - Array written to file
! KERR - Error return; 1 = illegal command string; 2 = file already exists,
!                      3 = error in creation
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: file_report,bin_exist,bin_create,file_error,use_file
!
! 3.  LOCAL VARIABLES
!
      CHARACTER ME*10
      INTEGER*2 I
      INTEGER*4 MAT_E, JRND_BLOCKS, LENGTH
      LOGICAL*2 KEXIST
      INTEGER*8 LEN8_BYTES, LEN8_BLOCKS
!
      DATA ME/'CGM_CREATE'/
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! I - Loop index
! KEXIST - True if file FNAME already exists
! LENGTH - Length of file to be created
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CGM_CREATE PROGRAM STRUCTURE
!
!
1     CONTINUE
!
! --- Check for illegal string
!
      KERR = 0
      IF ( LEN(STRING) .NE. 1  .OR. INDEX ( 'MU', STRING ) .EQ. 0 ) THEN
           KERR = 1
           RETURN
         ELSE
           CALL BIN_EXIST ( FNAME, KEXIST )
!
! -------- Make sure file doesn't exist if we're going to create it new
!
           IF ( KEXIST .AND. STRING .EQ. 'M' ) THEN
                KERR = 2
                RETURN
              ELSE IF ( STRING .EQ. 'M'  .OR.  .NOT. KEXIST ) THEN
!
! ------------- Create the file if that's what's appropriate
!
                LEN8_BYTES  = 8*(3*M_GPA + INT8(NP)*INT8(NP+1)/2)
                LEN8_BLOCKS = (LEN8_BYTES + 255)/256
                IDIRECT(1)  = CGM_DIR_SOCOM
                IDIRECT(2)  = CGM_DIR_PARFIL
                IDIRECT(3)  = CGM_DIR_NRMFIL
                IDIRECT(4)  = -1
                IDIRECT(5)  = CGM_DIR_PLIST
                CALL MEMCPY ( IDIRECT(6), M_GPA, %VAL(4) )
                CALL MEMCPY ( IDIRECT(8), NP,      %VAL(4) )
                IDIRECT(10) = 2017
!
                DO I=11,BLOCK_WORDS
                   IDIRECT(I)=0
                ENDDO
                CALL BIN_CREATE8 ( FNAME, FILDES, LEN8_BLOCKS )
                IF ( FILDES .LT. 0 ) THEN
                     WRITE ( 6, * ) 'CGMM_CREATE: LENGTH=',LENGTH, &
     &                              ' BIN_CREATE returned: ',FILDES
                     KERR = 3
                     RETURN
                ENDIF
!
! ------------- Write the first 256 blick
!
                CALL USE_FILE ( FNAME, FILDES, IDIRECT, 1, 1, 'W' )
              ELSE
!
! ------------- If file exists, open for update, reading the first 256-bytes
! ------------- block to extract an array IDIRECT
!
                CALL USE_FILE ( FNAME, FILDES, IDIRECT, 1, 1, 'OR' )
           ENDIF
      ENDIF
!
      RETURN
      END  !#!  CGM_CREATE8  #!#
