      SUBROUTINE SORT_INTO_FL(FILE_NAME,NUM_ADD,BUFFER_ADD, &
     &                        ISORT_FIELD,IGETUN,PRINT_ERR,KERR)
!
!     SORT_INTO_FL
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SORT_INTO_FL PROGRAM SPECIFICATION
!
! 1.1.   Adds the contents of the input array to the input file as a set
!        of new records (one per array element),
!        then optionally sorts the old and new records
!        together on the given field to produce a sorted file.
!
! 1.2.   REStRICTIONS -
!
! 1.3.   REFERENCES - none
!
! 2.  SORT_INTO_FL INTERFACE
!
! 2.1.   CALLING SEQUENCE:
!
!
!     CALL SORT_INTO_FL(FILE_NAME,NUM_ADD,BUFFER_ADD,
!    .                        ISORT_FIELD,IGETUN,PRINT_ERR,KERR)
!     INPUT VARIABLES:
!
!     FILE_NAME - name of input file
!     NUM_ADD - number of records to add
!     BUFFER_ADD - ARRAY OF records to add
!     ISORT_FIELD - > 0 field on which all records should be sorted.
!                   other values skip sorting
!     IGETUN - Specifies file lu
!                > 0 to use that lu
!                other values get an lu using getunit
!     PRINT_ERR - true to print error messages
!
      CHARACTER*(*) FILE_NAME, BUFFER_ADD(*)
      INTEGER*2 NUM_ADD,ISORT_FIELD,IGETUN
      LOGICAL*2 PRINT_ERR
!
!     OUTPUT VARIABLES:
!
!     KERR - error return
!        0 - for full success
!                       (input file was updated with input array and all
!                        interim files were cleaned up)
!        >0 - warning - (input file was updated, but one or more interim files
!                        were not cleaned up)
!        <0 - error -   (input file not updated)
!
      INTEGER*2 KERR
!
! 2.2.   COMMON BLOCKS USED: none
!
! 2.3.   DATA BASE ACCESSES: none
!
! 2.4.   EXTERNAL INPUT/OUTPUT: none
!
! 2.5.   SUBROUTINE INTERFACE:
!
!     CALLING SUBROUTINES: utility
!
!     CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 LUO,GETUNIT,IUNIT,ILENF,ILENA,ILENC,TRIMLEN,IERR,ICT, &
     &          ILENS,ILENU,IPT_LB,SYSTEM,ILENB
      CHARACTER*5 CSORT
      CHARACTER*255 FILE_NAME_ADD,FILE_NAME_CAT,FILE_NAME_SORT, &
     &              FILE_NAME_SOURCE,SYSCOM,BUFFER_THIS
      INTEGER*4 IERR4
!
! 4.  CONSTANTS USED:
!
! 5.  INITIALIZED VARIABLES:
!
! 6.  PROGRAMMER: K. Baver 5/26/98
!
!     LAST MODIFIED:
!
!     PROGRAM STRUCTURE
!
      KERR = 0
      IF (NUM_ADD.EQ.0) RETURN
      LUO = 6
      ILENF = TRIMLEN(FILE_NAME)
!
!     Place the new records in a separate file
!
      IF (IGETUN.LE.0) THEN
        IUNIT = GETUNIT()
      ELSE
        IUNIT = IGETUN
      ENDIF
!
      FILE_NAME_ADD = FILE_NAME(1:ILENF)//'_add'
      ILENA = TRIMLEN(FILE_NAME_ADD)
      OPEN(IUNIT,FILE=FILE_NAME_ADD(1:ILENA),IOSTAT=IERR4,ERR=50, &
     &     STATUS='NEW',ACCESS='SEQUENTIAL',FORM='FORMATTED')
 50   IF (IERR4.NE.0) THEN
        IF &
     &    (PRINT_ERR)WRITE(LUO,"('SORT_INTO_FL ERROR *1* -- ERROR ',I5, &
     &                ' OPENING FILE OF NEW RECORDS TO BE ADDED')") &
     &   IERR4
        KERR = -1
        GO TO 900
      ENDIF
!
      ICT = 1
      DO WHILE (ICT .LE. NUM_ADD .AND. KERR.EQ.0)
        BUFFER_THIS = BUFFER_ADD(ICT)
        ILENB = TRIMLEN(BUFFER_THIS)
        WRITE(IUNIT,"(A)",IOSTAT=IERR4, &
     &    ERR=100)BUFFER_THIS(1:ILENB)
 100    IF (IERR4.NE.0) THEN
          KERR = -2
        ELSE
          ICT = ICT + 1
        ENDIF
      ENDDO
      CLOSE(IUNIT)
      IF (KERR.EQ.-2) THEN
        IF &
     &    (PRINT_ERR)WRITE(LUO,"('SORT_INTO_FL ERROR *2* -- ERROR ',I5, &
     &                ' WRITING TO FILE OF NEW RECORDS TO BE ADDED')") &
     &   IERR4
        GO TO 800
      ENDIF
!
!     Concatenate the old and new records.
!
      FILE_NAME_CAT = FILE_NAME(1:ILENF)//'_cat'
      ILENC = TRIMLEN(FILE_NAME_CAT)
      SYSCOM = &
     &   'cat '//FILE_NAME(1:ILENF)//' '//FILE_NAME_ADD(1:ILENA)// &
     &      ' > '//FILE_NAME_CAT(1:ILENC)
      CALL ZTERM(SYSCOM,IERR4 )
      IF (IERR4.NE.0) THEN
        IF &
     &    (PRINT_ERR)WRITE(LUO,"('SORT_INTO_FL ERROR *3* -- ERROR ',I5, &
     &                ' ZTERMING COMMAND TO CONCATENATE ',/, &
     &                'FILES OF OLD AND NEW RECORDS')") &
     &   IERR4
        KERR = -3
        GO TO 800
      ENDIF
!
      IERR4 = system(SYSCOM)
      IF (IERR4.NE.0) THEN
        IF &
     &    (PRINT_ERR)WRITE(LUO,"('SORT_INTO_FL ERROR *4* -- ERROR ',I5, &
     &                ' CONCATENATING ',/, &
     &                'FILES OF OLD AND NEW RECORDS')") &
     &   IERR4
        KERR = -4
        GO TO 800
      ENDIF
!
!     Sort the file on the input field.
!
      IF (ISORT_FIELD .GT. 0) THEN
        FILE_NAME_SORT = FILE_NAME(1:ILENF)//'_sort'
        ILENS = TRIMLEN(FILE_NAME_SORT)
        WRITE(CSORT,"(I5)") ISORT_FIELD
        DO ICT = 1,5
          IF (CSORT(ICT:ICT).EQ.' ') IPT_LB = ICT
        ENDDO
        CSORT(IPT_LB-1:IPT_LB) = '-k'
        SYSCOM = &
     &     'sort '//CSORT//' '//FILE_NAME_CAT(1:ILENC)// &
     &     ' >  '//FILE_NAME_SORT(1:ILENS)
        CALL ZTERM(SYSCOM,IERR4 )
        IF (IERR4.NE.0) THEN
          IF &
     &      (PRINT_ERR)WRITE(LUO,"('SORT_INTO_FL ERROR *5* -- ERROR ',I5, &
     &                  ' ZTERMING COMMAND TO SORT ', &
     &                  'CONCATENATED FILE')") &
     &     IERR4
          KERR = -5
          GO TO 700
        ENDIF
!
        IERR4 = system(SYSCOM)
        IF (IERR4.NE.0) THEN
          IF &
     &      (PRINT_ERR)WRITE(LUO,"('SORT_INTO_FL ERROR *6* -- ERROR ',I5, &
     &                  ' SORTING CONCATENATED FILE')") &
     &     IERR4
          KERR = -6
          GO TO 700
        ENDIF
      ENDIF
!
!     Now place the final file in the original file name...
!
      IF (ISORT_FIELD .GT. 0) THEN
        FILE_NAME_SOURCE = FILE_NAME_SORT
      ELSE
        FILE_NAME_SOURCE = FILE_NAME_CAT
      ENDIF
      ILENU = TRIMLEN(FILE_NAME_SOURCE)
!
      SYSCOM = &
     &     'mv '//FILE_NAME_SOURCE(1:ILENU)//' '// &
     &            FILE_NAME(1:ILENF)
      CALL ZTERM(SYSCOM,IERR4 )
      IF (IERR4.NE.0) THEN
        IF &
     &    (PRINT_ERR)WRITE(LUO,"('SORT_INTO_FL ERROR *7* -- ERROR ',I5, &
     &                ' ZTERMING COMMAND TO MOVE OUTPUT FILE ',/, &
     &                'TO ORIGINAL FILE NAME')") &
     &   IERR4
        KERR = -7
        IF (ISORT_FIELD .GT. 0) THEN
          GO TO 600
        ELSE
          GO TO 700
        ENDIF
      ENDIF
!
      IERR4 = system(SYSCOM)
      IF (IERR4.NE.0) THEN
        IF &
     &    (PRINT_ERR)WRITE(LUO,"('SORT_INTO_FL ERROR *8* -- ERROR ',I5, &
     &                ' MOVING OUTPUT FILE TO ORIGINAL FILE NAME')") &
     &   IERR4
        KERR = -8
        IF (ISORT_FIELD .GT. 0) THEN
          GO TO 600
        ELSE
          GO TO 700
        ENDIF
      ENDIF
!
      IF (ISORT_FIELD .GT. 0) THEN
!       The sort file was just removed, so don't try to clean it up.
        GO TO 700
      ELSE
!       The cat file was just removed, so don't try to clean it up.
        GO TO 800
      ENDIF
!
!     ...and clean up any interim files.
!
 600  IF (ISORT_FIELD .GT. 0) THEN
        OPEN(IUNIT,FILE=FILE_NAME_SORT(1:ILENS),IOSTAT=IERR4,ERR=610, &
     &         STATUS='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL')
 610    IF (IERR4.NE.0) THEN
          IF (KERR.GE.0) THEN
            CALL SBIT( KERR, INT2(3), INT2(1) )
            IF &
     &        (PRINT_ERR)WRITE(LUO,"('SORT_INTO_FL WARNING -- FAILURE TO ', &
     &                  ' DELETE SORT FILE')")
          ENDIF
        ELSE
          CLOSE(IUNIT,STATUS='DELETE')
        ENDIF
      ENDIF
!
 700  OPEN(IUNIT,FILE=FILE_NAME_CAT(1:ILENC),IOSTAT=IERR4,ERR=710, &
     &         STATUS='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL')
 710  IF (IERR4.NE.0) THEN
        IF (KERR.GE.0) THEN
          CALL SBIT( KERR, INT2(2), INT2(1) )
          IF &
     &      (PRINT_ERR)WRITE(LUO,"('SORT_INTO_FL WARNING -- FAILURE TO ', &
     &                ' DELETE CONCATENATION FILE')")
        ENDIF
      ELSE
        CLOSE(IUNIT,STATUS='DELETE')
      ENDIF
!
 800  OPEN(IUNIT,FILE=FILE_NAME_ADD(1:ILENA),IOSTAT=IERR4,ERR=810, &
     &     STATUS='OLD',FORM='FORMATTED',ACCESS='SEQUENTIAL')
 810  IF (IERR4.NE.0) THEN
        IF (KERR.GE.0) THEN
          CALL SBIT( KERR, INT2(1), INT2(1) )
          IF &
     &      (PRINT_ERR)WRITE(LUO,"('SORT_INTO_FL WARNING -- FAILURE TO ', &
     &                ' DELETE FILE OF NEW RECORDS TO BE ADDED')")
        ENDIF
      ELSE
        CLOSE(IUNIT,STATUS='DELETE')
      ENDIF
!
 900  RETURN
      END
