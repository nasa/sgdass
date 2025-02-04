      SUBROUTINE FIND_IMAGE_NOISE ( MAP )
! ************************************************************************
! *                                                                      *
! *   Routine FIND_IMAGE_NOISE deterines the noise rms of the image.     *
! *   It computes rms in 4 cornes of the image , each of 1/4 x 1/4 of    *
! *   the image size, discards the corner with the largest rms noise     *
! *   and then average rms over remainging three corners. Then it        *
! *   iteratively removes points with dseviation of more than 4 sigma    *
! *   and re-adjust the rms of the noise level, untill no outliers       *
! *   remains.                                                           *
! *                                                                      *
! * ### 26-JAN-2007 FIND_IMAGE_NOISE  v1.1 (c) L. Petrov 23-NOV-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'diagi.i'
      INCLUDE    'sou_map.i'
      TYPE     ( SOUMAP__TYPE ) :: MAP
      REAL*8     NSIG
      PARAMETER  ( NSIG = 4.0 )
      REAL*8     NOI_QUA(4), NOI_RMS
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, IND_BAD_QUA, NEL(4), &
     &           NEL_TOT, IB1, IE1, IB2, IE2, IND, LOUT, NTER
      LOGICAL*1, ALLOCATABLE :: FL_OUT(:,:)
      LOGICAL*4  FL_DISCARD 
      INTEGER*4, EXTERNAL :: MAX_LIST_R8 
!
      ALLOCATE ( FL_OUT(MAP%DIM1,MAP%DIM2) )
      FL_OUT = .FALSE.
!
! --- Cycle over corners
!
      DO 410 J1=1,4
         IF ( J1 == 1 ) THEN
              IB1 = 1
              IE1 = MAP%DIM1/4
              IB2 = 1
              IE2 = MAP%DIM2/4
            ELSE IF ( J1 == 2 ) THEN
              IB1 = 1
              IE1 = MAP%DIM1/4
              IB2 = 3*MAP%DIM2/4
              IE2 = MAP%DIM2
            ELSE IF ( J1 == 3 ) THEN
              IB1 = 3*MAP%DIM1/4
              IE1 = MAP%DIM1
              IB2 = 1
              IE2 = MAP%DIM2/4
            ELSE IF ( J1 == 4 ) THEN
              IB1 = 3*MAP%DIM1/4
              IE1 = MAP%DIM1
              IB2 = 3*MAP%DIM2/4
              IE2 = MAP%DIM2
         END IF
!
! ------ Compute the sum of squares and the number of used elements
!
         NEL(J1) = 0
         NOI_QUA(J1) = 0.0D0
         DO 420 J2=IB2,IE2
            DO 430 J3=IB1,IE1 
               NOI_QUA(J1) = NOI_QUA(J1) + MAP%IMAGE(J3,J2)**2
               NEL(J1) = NEL(J1) + 1
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
! --- Find the quadrant with the maximum RMS
!
      IND_BAD_QUA = MAX_LIST_R8 ( 4, NOI_QUA )
!
! --- Compute the total noise level
!
      NOI_RMS = 0.0D0
      NEL_TOT = 0
      DO 440 J4=1,4
         IF ( IND_BAD_QUA == J4 ) GOTO 440
         NOI_RMS = NOI_RMS + NOI_QUA(J4)
         NEL_TOT = NEL_TOT + NEL(J4)
 440  CONTINUE 
!
! --- Now run iterations of removing outliers for the case when image features
! --- appeared in the corners
!
      NTER = NEL_TOT/2
      LOUT = 0
      DO 450 J5=1,NTER
         DO 460 J6=1,4
            IF ( IND_BAD_QUA == J6 ) GOTO 460
            IF ( J6 == 1 ) THEN
                 IB1 = 1
                 IE1 = MAP%DIM1/4
                 IB2 = 1
                 IE2 = MAP%DIM2/4
               ELSE IF ( J6 == 2 ) THEN
                 IB1 = 1
                 IE1 = MAP%DIM1/4
                 IB2 = 3*MAP%DIM2/4
                 IE2 = MAP%DIM2
               ELSE IF ( J6 == 3 ) THEN
                 IB1 = 3*MAP%DIM1/4
                 IE1 = MAP%DIM1
                 IB2 = 1
                 IE2 = MAP%DIM2/4
               ELSE IF ( J6 == 4 ) THEN
                 IB1 = 3*MAP%DIM1/4
                 IE1 = MAP%DIM1
                 IB2 = 3*MAP%DIM2/4
                 IE2 = MAP%DIM2
            END IF
!
! --------- Search for the outlier
!
            FL_DISCARD = .FALSE.
            DO 470 J7=IB2,IE2 
               DO 480 J8=IB1,IE1
                  IF ( FL_OUT(J8,J7) ) GOTO 480
                  IF ( MAP%IMAGE(J8,J7)**2 > NSIG**2*(NOI_RMS/NEL_TOT) ) THEN
!
! -------------------- We found the outlier
!
                       NOI_RMS = NOI_RMS - MAP%IMAGE(J8,J7)**2 
                       NEL_TOT = NEL_TOT - 1
                       LOUT = LOUT + 1
                       FL_OUT(J8,J7) = .TRUE.
                       FL_DISCARD = .TRUE.
                  END IF
 480           CONTINUE 
 470        CONTINUE 
 460     CONTINUE 
!
! ------ Not outliers? End of work
!
         IF ( .NOT. FL_DISCARD ) GOTO 850
 450  CONTINUE 
 850  CONTINUE 
!
      MAP%NOISE = DSQRT ( NOI_RMS/NEL_TOT )
      DEALLOCATE ( FL_OUT )
!
      RETURN
      END  SUBROUTINE  FIND_IMAGE_NOISE  !#!  
