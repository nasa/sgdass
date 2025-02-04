!!DatBuf
!
!     Common space for database data buffer, the remapping array,
!     and new (after 99.12) items to be output to database.
!
!     Programmers:
!       Gregg Cooke        90.02.08  Creation.
!
!       Darin Miller       91.11.14  Added documentation
!                                    and Removed ARCHIVE LCODE
!                          91.12.13  Added 'LISTNAME', and 'lisend'.
!       Brent Archinal     92.04.08  "ntot" explicitly typed i*4.
!       Brent Archinal     95.02.16  MAXDBUF increased from 500 to 512
!                                    due to increased number of Calc 8
!                                    lcodes (per D. Gordon e-mail of
!                                    94.11.22).
!       Brent Archinal     95.06.05  MAXDBUF increased from 512 to 530
!                                    per GSFC version of 95.05.22.
!       D. Gordon/GSFC     97.08.06  MAXDBUF increased from 530 to 560
!       Brent Archinal
!       Brent Archinal   1999.12.22  Directly adding new items to be
!                        to          output to database (rather than
!                        2000.01.18  adding them to the more complicated
!                                    jdbuf, etc. arrays).
!       L. Petrov        19-JUL-2003 Removed initialization of NTNEW
!
!     Parameters:
!
!     MAXDBUF is the size of the common in R*8.
!     MAXREMAP is the size of the remapping array.  This value is
!     dependent on the size of the standard lcode list and should
!     only be changed when lcodes are added or deleted from that list.
!
      integer*2 MAXDBUF, MAXREMAP
      parameter (MAXDBUF  = 560, &
     &           MAXREMAP = 79)
!
!     Specifications:
!
!     JDBUF  -- The data buffer is equivalenced seven ways to Sunday for
!               a good reason: the data type of the buffer used to GET
!               information from the database must agree with the data
!               type of the GET call.
!     REMAP  -- The remapping array contains a list of indices referencing
!               where data goes in the data buffer when the data comes from
!               a source other than a database.
!     NTOT   -- This is the effective size of the data buffer in words.
!     SCRDCB -- The unit number of the scratch data file, where each
!               observation gets stored until it is ready to be placed
!               into an output database.
!     LISEND  - Unit # of listing file.
!     ntnew   - Length in words of "new" items in common block, beyond
!               those specified by "ntot".
!
!           In order to delete a LCODE the files "initl.f",
!      "dbedit_datbuf.i", and "remap_ddbuf.f" must be modified.
!           In the file dbedit_datbuf.i the parameter "MAXREMAP"
!      must correspond to the number of LCODES in total.
!           In "initl.f" the variables "STDNx" and "STDTx" are
!      modified to correspond to the number of LCODE data types,
!      and number of LCODES per table of contents respectively.
!      The toc LCODE entry in "clcdtxt" is deleted. The respective
!      LCODE position in the "ltoc" array is deleted, and the
!      remap block is modified. The remap elements are start addresses
!      of each of the LCODE values. Each element past the deleted
!      LCODE element must be modified, paying attention to the original
!      spacing of the addresses.
!           In "remap_ddbuf.f" the element of output buffer(s) jdbuf,and ddbuf
!      that is assigned to the deleted LCODE is removed, and any remap
!      indices greater than the deleted remap index is decremented
!      by one in order for the correct mapping.
!
      integer*2 jdbuf(MAXDBUF*4), remap(MAXREMAP), scrdcb, lisend
      integer*4 ldbuf(MAXDBUF*2), ntot
      real*8 ddbuf(MAXDBUF)
!
      real*4 sutctg                  !  2 words
      integer*2 ntnew, utctg4(6)     !  7 words
      character scnnam_ch*32         ! 16 words
!                                      25 total words -> ntnew value
!
      equivalence (jdbuf,ldbuf,ddbuf)
!
!     Common Space:
!
      COMMON / DATBUF / JDBUF, REMAP, NTOT, SCRDCB, LISEND, &
     &                  SUTCTG, NTNEW, UTCTG4, SCNNAM_CH
