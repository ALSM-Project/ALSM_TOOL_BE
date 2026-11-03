      *                      /*    TOKUISAKI  MASTER 300  */            VFI00010
      *                      /*    DATE  88/06/21   V-007 */            VFI00020
           02  TMS-KEY.                                                 VFI00030
               03  TMS-TOKCD.                                           VFI00040
                   04  TMS-TKCD1  PIC  X(4).                            VFI00050
                   04  TMS-TKCD2  PIC  X(3).                            VFI00060
           02  TMS-TOKME       PIC  X(25).                              VFI00070
           02  TMS-STNCD.                                               VFI00080
               03  TMS-STCD1   PIC  X(1).                               VFI00090
               03  TMS-STCD2   PIC  X(2).                               VFI00100
           02  TMS-CKUKG.                                               VFI00110
               03  TMS-CKUNO   PIC  X(3).                               VFI00120
               03  TMS-CKUKA   PIC  X(1).                               VFI00130
               03  TMS-CKUCK   PIC  X(1).                               VFI00140
           02  TMS-CKUR1     REDEFINES  TMS-CKUKG.                      VFI00150
               03  TMS-CKR11   PIC  X(4).                               VFI00160
               03  FILLER      PIC  X(1).                               VFI00170
           02  TMS-CKUR2     REDEFINES  TMS-CKUR1.                      VFI00180
               03  FILLER      PIC  X(3).                               VFI00190
               03  TMS-CKR22   PIC  X(2).                               VFI00200
           02  TMS-SNRKM.                                               VFI00210
               03  TMS-SRKM    PIC  X(1)     OCCURS 35.                 VFI00210
           02  TMS-SEYOS       PIC  9(1).                               VFI00210
           02  TMS-SEBUS       PIC  9(1).                               VFI00210
           02  TMS-YUTOS       PIC  9(1).                               VFI00210
           02  TMS-NOSEK       PIC  9(1).                               VFI00210
           02  TMS-SIHENG      PIC  9(8).                               VFI00210
           02  TMS-LSIMEB      PIC  X(2).                               VFI00210
           02  TMS-BOGIE       PIC S9(7)     COMP-3.                    VFI00210
           02  TMS-CYUSI       PIC  9(1).                               VFI00210
           02  TMS-BRIC        PIC  X(5).                               VFI00210
           02  TMS-LET         PIC  X(1).                               VFI00210
           02  TMS-MANG        PIC  X(2).
           02  TMS-SKNRK       PIC  X(1).                               VFI00220
           02  TMS-SIMEB       PIC  9(2).                               VFI00230
           02  TMS-HNRKB       PIC  X(1).                               VFI00240
           02  TMS-SKUCD.                                               VFI00250
               03  TMS-SUCD1   PIC  X(4).
               03  TMS-SUCD2   PIC  X(3).
           02  TMS-LSEBUS      PIC  X(1).                               VFI00260
           02  TMS-MOTCP       PIC  9(1).                               VFI00270
           02  TMS-INKAN       PIC  9(1).                               VFI00280
           02  TMS-KEIYK       PIC  9(1).                               VFI00290
           02  TMS-TKONU       PIC S9(6)     COMP-3.                    VFI00300
           02  TMS-UMPFL       PIC  X(1).                               VFI00310
           02  TMS-MKHNO.                                               VFI00320
               03  TMS-MAKHN   PIC  X(1)     OCCURS 100.                VFI00330
           02  TMS-HENYM       PIC  9(6).                               VFI00340
           02  TMS-QCKUK.                                               VFI00350
               03  TMS-QCKNO   PIC  X(3).                               VFI00360
               03  TMS-QCK22.
                   04  TMS-QCKKA   PIC  X(1).                           VFI00370
                   04  TMS-QCKCK   PIC  X(1).                           VFI00380
           02  TMS-TOKKA       PIC  X(30).
           02  TMS-QSTCD       PIC  X(3).
           02  TMS-BED         PIC  X(5).
           02  TMS-ZASOS       PIC  X(1).
           02  TMS-OYAKO       PIC  X(1).
           02  TMS-OUMSC       PIC  X(01).
           02  TMS-OUMSE       PIC  X(01).
           02  TMS-OGHPF       PIC  X(01).
           02  TMS-OGKKS       PIC  X(01).
           02  TMS-OGKKM       PIC  X(01).
           02  FILLER          PIC  X(22).                              VFI00390
