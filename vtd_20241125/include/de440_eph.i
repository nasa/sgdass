!
! >>>>> Include block for epehermirdes DE403
! >>>>> 2004.01.24   (c)  L. Petrov  v 1.3  21-SEP-2004 19:50:00
!
      INTEGER*4  DE440__RECLEN, DE440__RECSH, DE440__RECLEN_R8, DE440__NREC
      INTEGER*4  DE440__MFILL, DE440__MHEAD, DE440__MTOES, DE440__MPAR
      PARAMETER  ( DE440__RECLEN = 8144, DE440__RECLEN_R8 = DE440__RECLEN/8 )
      PARAMETER  ( DE440__RECSH  = 7184 )
      PARAMETER  ( DE440__MHEAD  =  645 )
      PARAMETER  ( DE440__MTOES  =  400 )
      PARAMETER  ( DE440__MFILL  = 4022 )
      PARAMETER  ( DE440__NREC   = 1145 )
      PARAMETER  ( DE440__MPAR   =   15 )
      TYPE       DE440__TYPE
            REAL*8      BUF(DE440__RECLEN_R8,DE440__NREC)
            CHARACTER   TIT(3)*84    
            CHARACTER   HEA_NAM(DE440__MHEAD)*6  
            REAL*8      HEA_VAL(DE440__MHEAD) 
            REAL*8      DATE_BEG_JD
            REAL*8      DATE_END_JD
            REAL*8      STEP_DAY
            INTEGER*4   NCON
            INTEGER*4   NUMDE      
            REAL*8      AU         
            REAL*8      EMRAT      
            INTEGER*4   IPT(3,15)
            INTEGER*4   STATUS
      END TYPE   DE440__TYPE
!
!         WRITE(12,REC=1,IOSTAT=OUT) TTL,(CNAM(I),I=1,OLDMAX),SS,NCON,AU, &
!     &              EMRAT,IPT,NUMDE,LPT,(CNAM(J),J=K,NCON),RPT,TPT
!
      TYPE       DE440_HEA__TYPE
            CHARACTER   TIT(3)*84             !  252
            CHARACTER   CNAM(DE440__MTOES)*6  ! 2400
            INTEGER*1   ARR(DE440__MFILL)     ! 5460
      END TYPE   DE440_HEA__TYPE
      INTEGER*4  DE440__LOADED
      PARAMETER  ( DE440__LOADED = 1 ) 
      CHARACTER    DE440_EPH__LABEL*35
      PARAMETER  ( DE440_EPH__LABEL = 'JPL Planetary Ephemeris DE440/LE440' )

!
! >>>>> End of include block for epehermirdes DE440
!
