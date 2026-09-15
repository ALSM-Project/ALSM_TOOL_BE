      *                        /*    PARM  FOR  VSBA0180      */        VPA00010
      *                        /*    DATE  98/03/18   V-001   */        VPA00020
           02  P232-DATE       PIC  9(8).                               VPA00030
           02  P232-DATEX  REDEFINES  P232-DATE.                        VPA00040
               03  P232-YY     PIC  9(4).                               VPA00050
               03  P232-MM     PIC  9(2).                               VPA00060
               03  P232-DD     PIC  9(2).                               VPA00070
           02  FILLER      REDEFINES  P232-DATE.                        VPA00080
               03  P232-YYMM   PIC  9(6).                               VPA00090
               03  FILLER      PIC  X(2).                               VPA00100
           02  P232-LOADS      PIC  9(1).                               VPA00110
           02  P232-CALENX.                                             VPA00120
               03  P232-CALEN                 OCCURS 24.                VPA00130
                   04  P232-TYYMM   PIC 9(6).                           VPA00140
                   04  P232-TDDX.                                       VPA00150
                       05  P232-TDD PIC 9(1)  OCCURS 31.                VPA00160
