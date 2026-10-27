      *                        /*    PARM  FOR  VSBA2270      */        VPA00010
      *                        /*    DATE  98/03/17   V-001   */        VPA00020
           02  P227-DATE       PIC  9(8).                               VPA00030
           02  P227-DATEX  REDEFINES  P227-DATE.                        VPA00040
               03  P227-YY     PIC  9(4).                               VPA00050
               03  P227-MM     PIC  9(2).                               VPA00060
               03  P227-DD     PIC  9(2).                               VPA00070
           02  FILLER      REDEFINES  P227-DATE.                        VPA00080
               03  P227-YYMM   PIC  9(6).                               VPA00090
               03  FILLER      PIC  X(2).                               VPA00100
           02  P227-SIGN       PIC  9(1).                               VPA00110
           02  P227-LOADS      PIC  9(1).                               VPA00120
           02  P227-CALENX.                                             VPA00130
               03  P227-CALEN                 OCCURS 24.                VPA00140
                   04  P227-TYYMM   PIC 9(6).                           VPA00150
                   04  P227-TDDX.                                       VPA00160
                       05  P227-TDD PIC 9(1)  OCCURS 31.                VPA00170
