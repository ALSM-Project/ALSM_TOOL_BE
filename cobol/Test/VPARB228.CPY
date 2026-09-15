      *                        /*    PARM  FOR  VSBA2280      */        VPA00010
      *                        /*    DATE  98/03/23   V-001   */        VPA00020
           02  P228-DATE       PIC  9(8).                               VPA00030
           02  P228-DATEX  REDEFINES  P228-DATE.                        VPA00040
               03  P228-YY     PIC  9(4).                               VPA00050
               03  P228-MM     PIC  9(2).                               VPA00060
               03  P228-DD     PIC  9(2).                               VPA00070
           02  FILLER      REDEFINES  P228-DATE.                        VPA00080
               03  P228-YYMM   PIC  9(6).                               VPA00090
               03  FILLER      PIC  X(2).                               VPA00100
