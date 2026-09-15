      *                        /*    PARM  FOR  VSBA0410      */        VPA00010
      *                        /*    LENGTH  =  201           */        VPA00020
      *                        /*    DATE  85/06/14   V-001   */        VPA00020
           02  P41-HNBCD       PIC  X(1).                               VPA00030
           02  P41-DATA.                                                VPA00030
               03  P41-DATA1   PIC  X(1)  OCCURS  200.                  VPA00030
           02  FILLER   REDEFINES  P41-DATA.                            VPA00030
               03  P41-DATA2   PIC  X(2)  OCCURS  100.                  VPA00030
