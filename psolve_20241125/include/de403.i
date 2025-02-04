!
! >>>>> Include block for epehermirdes DE403
! >>>>> 2004.01.24   (c)  L. Petrov  v 1.0 07-JUL-2004 13:37:24
!
      INTEGER*4  DE403__RECLEN, DE403__RECLEN_R8, DE403__NREC
      PARAMETER  ( DE403__RECLEN = 8144, DE403__RECLEN_R8 = DE403__RECLEN/8 )
      PARAMETER  ( DE403__NREC =  1024 )
      TYPE       DE403__TYPE
            REAL*8      BUF(DE403__RECLEN_R8,DE403__NREC)
            CHARACTER   TIT(3)*84    
            CHARACTER   HEA_NAM(400)*6  
            REAL*8      HEA_VAL(400) 
            REAL*8      DATE_BEG_JD
            REAL*8      DATE_END_JD
            REAL*8      STEP_DAY
            INTEGER*4   NCON
            INTEGER*4   NUMDE      
            REAL*8      AU         
            REAL*8      EMRAT      
            INTEGER*4   IPT(3,13)
            INTEGER*4   STATUS
      END TYPE   DE403__TYPE
!
      TYPE       DE403_HEA__TYPE
            CHARACTER   TIT(3)*84    !  252
            CHARACTER   CNAM(400)*6  ! 2400
            INTEGER*1   ARR(5492)    ! 5492
      END TYPE   DE403_HEA__TYPE
      INTEGER*4  DE403__LOADED
      PARAMETER  ( DE403__LOADED = 1 ) 
!
! >>>>> End of include block for epehermirdes DE403
!
