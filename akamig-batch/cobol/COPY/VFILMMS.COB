      *                      /*    SHIRESAKI  MASTER      */            VFI00010
      *                      /*    DATE  87/02/16   V-006 */            VFI00020
      *                      /*    LENGTH  => 100         */            VFI00020
           02  MMS-KEY.                                                 VFI00030
               03  MMS-SHIMC   PIC  X(4).                               VFI00040
               03  MMS-SITEK   PIC  X(1).                               VFI00050
           02  MMS-SHIMM       PIC  X(8).                               VFI00060
           02  MMS-SHIKJ       PIC  X(12).                              VFI00060
           02  MMS-SHIKK.                                               VFI00060
               03  MMS-SHIKKU  PIC  X(10).                              VFI00060
               03  MMS-SHIKKD  PIC  X(10).                              VFI00060
           02  MMS-SIMEB       PIC  9(2).                               VFI00070
           02  MMS-KSANM       PIC  9(2).                               VFI00080
           02  MMS-SHIKB       PIC  X(1).                               VFI00090
           02  MMS-MAKKB       PIC  X(1).                               VFI00100
           02  MMS-SITEN       PIC  X(1).                               VFI00110
           02  MMS-HJYOH       PIC  9(1).                               VFI00120
           02  MMS-MKETC       PIC  9(1).                               VFI00130
           02  MMS-HCUYB       PIC  9(1).                               VFI00140
           02  MMS-TEIKI       PIC  9(2).                               VFI00150
           02  MMS-ARAHH       PIC S9(2)V9(2) COMP-3.                   VFI00160
           02  MMS-ARAHT       PIC S9(2)V9(2) COMP-3.                   VFI00170
           02  FILLER          PIC  X(3).                               VFI00180
           02  MMS-SPRKB       PIC  X(1).                               VFI00210
           02  MMS-SPRJB       PIC  X(1).                               VFI00220
           02  MMS-YAKEK       PIC  X(1).                               VFI00230
           02  MMS-ARAHR.
               03  MMS-ARAG1   PIC S9(02)V9 COMP-3.
               03  MMS-ARAG2   PIC S9(02)V9 COMP-3.
               03  MMS-ARAG3   PIC S9(02)V9 COMP-3.
               03  MMS-ARAG4   PIC S9(02)V9 COMP-3.
               03  MMS-ARAG5   PIC S9(02)V9 COMP-3.
               03  MMS-ARAG6   PIC S9(02)V9 COMP-3.
               03  MMS-ARAG7   PIC S9(02)V9 COMP-3.
           02  FILLER          PIC  X(17).                              VFI00240
