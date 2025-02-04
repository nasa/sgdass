      FUNCTION   DIAGI_PRN ( NAME, DEVICE_PRN, IDEV, IPRN )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_PRN  bids user to choose the plotting device for    *
! *   making hard copy.                                                  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *        NAME ( CHARACTER ) -- Plot name.                              *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  DEVICE_PRN ( CHARACTER ) -- Hard copy "device" in PGPLOT format     *
! *                              ( something like "plot.file/GIF" )      *
! *        IDEV ( INTEGER*4 ) -- Index of a chosen plotting device in    *
! *                              DEVS array (defined in diagi.i ).       *
! *        IPRN ( INTEGER*4 ) -- Code of further operation:              *
! *                              IPRN=0 -- create the file with hard     *
! *                                        copy of current plot.         *
! *                              IPRN=1 -- create the file with hard     *
! *                                        copy of current plot and then *
! *                                        try to print it using the     *
! *                                        command defined in environment*
! *                                        variable DIAGI_PRICOM.        *
! *                              IPRN=2 -- file with hardcopy cannot     *
! *                                        be opened.                    *
! *                                                                      *
! *  ###  14-OCT-1997  DIAGI_PRN   v1.62 (c)  L. Petrov  23-JAN-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      INTEGER*4  DIAGI_PRN
      CHARACTER  NAME*(*), DEVICE_PRN*(*), MES*128, CH*1, STR*80, FINAM*128
      INTEGER*4  IPRN, IDEV, ICHO, J1, J2, LUN, IO, NN, MPRN
      LOGICAL*4  LOP
      INTEGER*4  I_LEN, DIAGI_INBOX
      REAL*4     XC, YC, SIZ, CINT
      PARAMETER  ( MPRN = 9   )
      PARAMETER  ( CINT = 0.5 )
      TYPE ( DIAGI_BOXES ) ::  BOX(MPRN)
      CHARACTER  MES_MODE(MPRN)*16, MSZ_MODE(MPRN)*10
      INTEGER*4  IDEV_MODE(MPRN), ISIZ_MODE(MPRN), IPRN_MODE(MPRN)
      REAL*4     ANG_MODE(MPRN), BAN_MODE(MPRN)
      INTEGER*4, EXTERNAL :: ILEN
!
! --- MES_MODE  -- message to be printed at the dialogue box
! --- MSZ_MODE  -- message with proposed size of the plot
! --- IDEV_MODE -- device type as specified in diagi.i
! --- ISIZ_MODE -- mode of size of the box
! --- IPRN_MODE -- Code of the operation (see above for IPRN)
! --- ANG_MODE  -- Angle (in degrees) of the turn of the text in the box
! --- BAN_MODE  -- print or not print banner of the type (1 -- print)
!
      DATA       ( MES_MODE(NN), MSZ_MODE(NN), IDEV_MODE(NN), ISIZ_MODE(NN), &
     &             IPRN_MODE(NN), ANG_MODE(NN), BAN_MODE(NN), NN=1,MPRN ) &
     &     / &
     &           'PS -> file      ', '175x235 mm',   7,   7,   0,  90.,  0, &
     &           'PS -> file      ', '165x90  mm',   8,   8,   0,   0.,  1, &
     &           'PS -> file      ', '75x45 mm  ',   9,   9,   0,   0.,  0, &
     &           'PS -> printer   ', '175x235 mm',   7,   7,   1,  90.,  0, &
     &           'PS -> printer   ', '165x90 mm ',   8,   8,   1,   0.,  1, &
     &           'PS -> printer   ', '75x45 mm  ',   9,   9,   1,   0.,  0, &
     &           'GIF -> file     ', '175x235 mm',  10,   7,   0,  90.,  0, &
     &           'GIF -> file     ', '165x90 mm ',  11,   8,   0,   0.,  1, &
     &           'GIF -> file     ', '75x45 mm  ',  12,   9,   0,   0.,  0 &
     &     /
!
      CALL CLRCH ( MES )
      MES = 'Select print mode'
      CALL PGSAVE ! 1
!
! --- Deleting previous window
!
      CALL PGERAS
!
! --- Setting new world coodrinates
!
      CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
!
      CALL PGSCI   ( 1   )
      CALL PGSCH   ( 2.5 )
      CALL PGSLW   ( 8   )
!
! --- Printing the prompt
!
      XC = 0.50
      YC = 0.66
      CALL PGPTXT  ( XC, YC, 0.5, 0.5, MES(1:I_LEN(MES)) )
      XC  = 0.50
      YC  = 0.33
!
      CALL PGSAVE ! 2
      SIZ = 1.0/( MPRN + CINT*(MPRN+1) )
      CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
      DO 410 J1=1,MPRN
!
! ------ Specifing the corner of the boxes
!
         BOX(J1)%XLB = J1*SIZ*CINT + (J1-1)*SIZ
         BOX(J1)%YLB = YC-SIZ*1.2
         BOX(J1)%XTU = BOX(J1)%XLB+SIZ
         BOX(J1)%YTU = YC+SIZ*1.2
         CALL PGSAVE ! 3A
!
! ------ Printing the box with Print style. Firstly as filled rectangular
!
         CALL PGSFS  ( 1 )
         CALL PGSCI  ( 3 )
         CALL PGRECT ( BOX(J1)%XLB, BOX(J1)%XTU, BOX(J1)%YLB, BOX(J1)%YTU )
!
! ------ ... then as outlined rectangular
!
         CALL PGSFS  ( 2 )
         CALL PGSCI  ( 1 )
         IF ( INDEX ( MES_MODE(J1), 'printer' ) .GT. 0 ) THEN
              CALL PGSLW  ( 5 )
            ELSE
              CALL PGSLW  ( 1 )
         END IF
         CALL PGRECT ( BOX(J1)%XLB, BOX(J1)%XTU, BOX(J1)%YLB, BOX(J1)%YTU )
!
         CALL PGUNSA ! 3A
!
! ------ Printing explaining text at the bottom ...
!
         CALL PGSAVE ! 3B
         CALL CLRCH  ( STR )
         CALL INCH   ( J1, STR )
         CALL PGSCF  ( 3 )
         CALL PGSCH  ( 1.2 )
         IF ( INDEX ( MES_MODE(J1), 'printer' ) .GT. 0 ) THEN
              CALL PGSLW  ( 5 )
            ELSE
              CALL PGSLW  ( 1 )
         END IF
         CALL PGPTXT ( BOX(J1)%XLB+SIZ/2., BOX(J1)%YLB-SIZ/2., 0.5, 0.5, &
     &                 'PRN='//STR(1:1) )
!
! ------ ... and at the top of the icon
!
         IF ( BAN_MODE(J1) .EQ. 1 ) THEN
              CALL PGSCF ( 2 )
              IF ( INDEX ( MES_MODE(J1), 'printer' ) .GT. 0 ) THEN
                   CALL PGSLW  ( 10  )
                   CALL PGSCH  ( 2.2 )
                ELSE
                   CALL PGSLW  ( 5   )
                   CALL PGSCH  ( 2.8 )
              END IF
              CALL PGPTXT ( BOX(J1)%XLB+SIZ/2., BOX(J1)%YLB+SIZ*3.0, 0.5, 0.5, &
     &                 MES_MODE(J1)(1:I_LEN(MES_MODE(J1))) )
         END IF
         CALL PGSLW ( 1 )
         CALL PGSCF ( 1 )
!
! ------ Temporarily making the current icon the entire working space
!
         CALL PGSVP  ( BOX(J1)%XLB, BOX(J1)%XTU, BOX(J1)%YLB, BOX(J1)%YTU )
         CALL PGSWIN ( 0.0, 210.0, 0.0, 297.0 )
!
         CALL PGSFS  ( 2    )
         CALL PGSCH  ( 0.65 )
!
! ------ Printing the rectangular to be proportianal to the proposed ouput
! ------ and putting the text with the size of the proposed output
!
         IF ( ANG_MODE(J1) .EQ. 0.0 ) THEN
!
! ----------- Portrait mode
!
              CALL PGRECT ( XLEFTS(ISIZ_MODE(J1)), XRIGHTS(ISIZ_MODE(J1)), &
     &                      YBOTS(ISIZ_MODE(J1)), YTOPS(ISIZ_MODE(J1)) )
              CALL PGPTXT ( 105., YBOTS(ISIZ_MODE(J1))+YTOPS(ISIZ_MODE(J1)), &
     &                      ANG_MODE(J1), 0.5, &
     &                      MSZ_MODE(J1)(1:I_LEN(MSZ_MODE(J1))) )
            ELSE
!
! ----------- Landscape mode
!
              CALL PGRECT ( YBOTS(ISIZ_MODE(J1)), YTOPS(ISIZ_MODE(J1)), &
     &                      XLEFTS(ISIZ_MODE(J1)), XRIGHTS(ISIZ_MODE(J1)) )
              CALL PGPTXT ( 105.0, 150., ANG_MODE(J1), 0.5, &
     &                      MSZ_MODE(J1)(1:I_LEN(MSZ_MODE(J1))) )
         END IF
!
! ------ Restoring world coordinate space
!
         CALL PGSVP   ( 0.0, 1.0, 0.0, 1.0 )
         CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
         CALL PGUNSA ! 3B
 410  CONTINUE
      IDEV = 0
      IPRN = 0
      XC  = 0.5
      YC  = 0.5
      CALL CLRCH ( DEVICE_PRN )
!
! --- Waiting for user reaction
!
      CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
!
      CALL PGUNSA ! 2
      CALL PGUNSA ! 1
!
! --- Determine: which box has been specified
!
      ICHO = 0
      CALL CHIN ( CH, ICHO )
!
      IF ( ICHO .GE. 1  .AND.  ICHO .LE. MPRN ) THEN
           CONTINUE
         ELSE
           ICHO = 0
           IF ( CH .NE. 'X' ) THEN
                ICHO = DIAGI_INBOX ( MPRN, BOX, XC, YC )
           END IF
      END IF
!
      IF ( ICHO .GT. 0 ) THEN
!
! -------- If some box has been specified we determine device type,
! -------- device name and print mode
!
           IDEV = IDEV_MODE(ICHO)
           IPRN = IPRN_MODE(ICHO)
!
! -------- Form a "device name"
!
           CALL CLRCH ( FINAM )
           IF ( ILEN(NAME) .EQ. 0 ) THEN
                FINAM = DIAGI_OUT
              ELSE
                FINAM = NAME
           END IF
!
           IF ( INDEX ( DEVS(IDEV), 'PS' ) .NE. 0 ) THEN
!
! ------------- PostScript. Let's  avoid double extension
!
                IF ( ILEN(FINAM) .GT. 3 ) THEN
                     IF ( FINAM(ILEN(FINAM)-2:ILEN(FINAM)) .NE. PS_DIAGI ) THEN
                          FINAM = FINAM(1:I_LEN(FINAM))//PS_DIAGI
                     END IF
                   ELSE
                     FINAM = FINAM(1:I_LEN(FINAM))//PS_DIAGI
                END IF
             ELSE IF ( INDEX ( DEVS(IDEV), 'GIF' ) .NE. 0 ) THEN
!
! ------------- Gif-format. Let's  avoid double extension
!
                IF ( ILEN(FINAM) .GT. 4 ) THEN
                     IF ( FINAM(ILEN(FINAM)-3:ILEN(FINAM)) .NE. GIF_DIAGI ) THEN
                          FINAM = FINAM(1:I_LEN(FINAM))//GIF_DIAGI
                     END IF
                   ELSE
                     FINAM = FINAM(1:I_LEN(FINAM))//GIF_DIAGI
                END IF
             ELSE
                FINAM = FINAM(1:I_LEN(FINAM))//OUT_DIAGI
           END IF
!
           DEVICE_PRN = FINAM(1:I_LEN(FINAM))//DEVS(IDEV)
!
! -------- Seach the first free logical unit
!
           DO 420 J2=20,99
              INQUIRE ( UNIT=J2, OPENED=LOP )
              IF ( .NOT. LOP ) THEN
                   LUN =J2
                   GOTO 820
              END IF
 420       CONTINUE
 820       CONTINUE
!
! -------- Try to open file
!
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IO )
           IF ( IO .EQ. 0 ) THEN
                CLOSE ( UNIT=LUN )
              ELSE
!
! ------------- Failure!
!
                IPRN = 2
                write ( 6, * ) 'diagi_prn: lun = ', lun,' io = ', io
                write ( 6, * ) 'finam  >>'//finam(1:i_len(finam))//'<< '
                CALL CLRCH ( DEVICE_PRN )
                DEVICE_PRN = FINAM(1:I_LEN(FINAM))
           END IF
      END IF
!
      DIAGI_PRN  = IDEV
!
      RETURN
      END  !#!  DIAGI_PRN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DIAGI_PPR ( DEVICE_PRN, ICODE, IPRN )
! ************************************************************************
! *                                                                      *
! *   Routine  DIAGI_PPR  informs user about success or failure of       *
! *   printing hard copy of the plot.                                    *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  DEVICE_PRN ( CHARACTER ) -- Hard copy "device" in PGPLOT format     *
! *                              ( something like "plot.file/GIF" )      *
! *       ICODE ( INTEGER*4 ) -- Code of success. ICODE >= 0 -- success, *
! *                              ICODE < 0 -- failure.                   *
! *        IPRN ( INTEGER*4 ) -- Code of further operation:              *
! *                              IPRN=0 -- create the file with hard     *
! *                                        copy of current plot.         *
! *                              IPRN=1 -- create the file with hard     *
! *                                        copy of current plot and then *
! *                                        try to print it using the     *
! *                                        command defined in environment*
! *                                        variable DIAGI_PRICOM.        *
! *                                                                      *
! *  ###  14-OCT-97    DIAGI_PPR   v1.2  (c)  L. Petrov  23-JUL-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'diagi.i'
      CHARACTER  DEVICE_PRN*(*), CH*1, MES*128, MES2*128
      REAL*4     XC, YC
      INTEGER*4  ICODE, IPRN, IL, IL2
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      CALL PGSAVE ! 1
!
! --- Deleting previous window
!
      CALL PGERAS
!
! --- Setting new world coodrinates
!
      CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
!
      CALL PGSCH   ( 1.6 )
      CALL PGSLW   ( 5   )
      XC = 0.50
      YC = 0.50
!
! --- Preparing the text
!
      CALL CLRCH ( MES )
      CALL CLRCH ( MES2 )
      IF ( ICODE .GE. 0  .AND. IPRN  .EQ. 1 ) THEN
           CALL PGSCI   ( 1   )
           MES = 'Plot has been sent to printer'
        ELSE IF ( ICODE .GE. 0  .AND. IPRN  .EQ. 0 ) THEN
           CALL PGSCI   ( 1   )
           IF ( ILEN(DEVICE_PRN) .EQ. 0 ) THEN
                MES  = 'Plot has been written'
             ELSE IF ( I_LEN(DEVICE_PRN) .LT. 16 ) THEN
                MES  = 'Plot has been written to file '//DEVICE_PRN
             ELSE
                MES  = 'Plot has been written to file '
                MES2 = DEVICE_PRN
           END IF
        ELSE IF ( ICODE .LT. 0  .AND. IPRN  .EQ. 0 ) THEN
           CALL PGSCI   ( 2   )
           IF ( I_LEN(DEVICE_PRN) .LT. 16 ) THEN
                MES = 'Error occured in writing the output to file '//DEVICE_PRN
              ELSE
                MES = 'Error occured in writing the output to file '
                MES2= DEVICE_PRN
           END IF
      END IF
!
! --- Cutting away the suffix with device type
!
      IL = LINDEX ( MES, '/' ) - 1
      IF ( IL .LE. 0 ) IL = I_LEN(MES)
      IL2 = LINDEX ( MES2, '/' ) - 1
      IF ( IL2 .LE. 0 ) IL2 = I_LEN(MES2)
!
! --- Printing the text
!
      CALL PGPTXT ( XC, YC,     0.0, 0.5, MES(1:IL) )
      CALL PGPTXT ( XC, YC-0.1, 0.0, 0.5, MES2(1:IL2) )
      CALL PGBAND ( 0, 0, XC, YC, XC, YC, CH )
      CALL PGUNSA ! 1
!
      RETURN
      END  !#!  DIAGI_PPR  #!#
