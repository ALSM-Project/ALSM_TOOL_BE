      *                      /*    NOHINSEIKYU MASTER 100 */            VFI00010
      *                      /*    DATE  86/07/07   V-001 */            VFI00020
           02  NSM-KEY.                                                 VFI00030
               03  NSM-TOKCD.                                           VFI00040
                   04  NSM-TKCD1  PIC  X(4).                            VFI00050
                   04  NSM-TKCD2  PIC  X(3).                            VFI00060
               03  NSM-NOSCD      PIC  9(2).                            VFI00070
           02  NSM-STNCD.                                               VFI00080
               03  NSM-STCD1      PIC  X(1).                            VFI00090
               03  NSM-STCD2      PIC  X(2).                            VFI00100
           02  NSM-NOSME          PIC  X(25).                           VFI00110
           02  NSM-NOSKA          PIC  X(30).                           VFI00110
           02  NSM-ALTGT.                                               VFI00110
               03  NSM-ALTYY      PIC  9(4).                            VFI00120
               03  NSM-ALTMM      PIC  9(2).                            VFI00120
               03  NSM-ALTDD      PIC  9(2).                            VFI00120
           02  FILLER             PIC  X(25).                           VFI00170
