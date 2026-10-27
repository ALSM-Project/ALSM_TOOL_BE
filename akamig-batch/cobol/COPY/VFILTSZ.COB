      *                      /*  ( TBLFILE )  SOSIKIZU TABLE   */       VFI00010
      *                      /*  DATE  88/08/03   V-002        */       VFI00020
           02  TSZ-KEY.                                                 VFI00030
               03  TSZ-RECID       PIC  X(02).                          VFI00040
               03  TSZ-RECNO       PIC  X(04).                          VFI00050
           02  TSZ-SOSIKI.                                              VFI00060
               03  TSZ-SYSNO       PIC  X(02).                          VFI00070
               03  TSZ-BASID.                                           VFI00080
                   04  TSZ-SITENKG  PIC X(01).                          VFI00090
                   04  TSZ-SOKOKG   PIC X(01).                          VFI00100
               03  TSZ-SYOKAKG     PIC  X(01).                          VFI00110
               03  TSZ-SITENNO     PIC  9(03).                          VFI00120
               03  TSZ-SOKONO      PIC  9(03).                          VFI00130
               03  TSZ-SYOKANO     PIC  9(03).                          VFI00140
               03  TSZ-SITEN       PIC  X(10).                          VFI00150
               03  TSZ-SYOKA       PIC  X(08).                          VFI00160
               03  TSZ-SOKME       PIC  X(08).                          VFI00160
           02  FILLER          PIC  X(34).                              VFI00170
