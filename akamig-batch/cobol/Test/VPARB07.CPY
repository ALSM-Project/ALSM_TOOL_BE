      *                        /*    PARM  FOR  BSBA0070      */        VPA00010
      *                        /*    DATE  83/05/28   V-001   */        VPA00020
           02  P07-DATE        PIC  9(6).                               VPA00030
           02  P07-DATEX   REDEFINES  P07-DATE.                         VPA00040
               03  P07-YY      PIC  9(2).                               VPA00050
               03  P07-MM      PIC  9(2).                               VPA00060
               03  P07-DD      PIC  9(2).                               VPA00070
           02  FILLER      REDEFINES  P07-DATE.                         VPA00080
               03  P07-YYMM    PIC  9(4).                               VPA00090
               03  FILLER      PIC  X(2).                               VPA00100
