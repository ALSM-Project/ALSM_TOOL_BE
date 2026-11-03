      *                        /*    PARM  FOR  VSBA2290      */        VPA00010
      *                        /*    DATE  98/03/18   V-001   */        VPA00020
           02  P229-DATE       PIC  9(8).                               VPA00030
           02  P229-DATEX  REDEFINES  P229-DATE.                        VPA00040
               03  P229-YY     PIC  9(4).                               VPA00050
               03  P229-MM     PIC  9(2).                               VPA00060
               03  P229-DD     PIC  9(2).                               VPA00070
           02  FILLER      REDEFINES  P229-DATE.                        VPA00080
               03  P229-YYMM   PIC  9(6).                               VPA00090
               03  FILLER      PIC  X(2).                               VPA00100
           02  P229-YOKU       PIC  9(2).                               VPA00110
           02  P229-LOADS      PIC  9(1).                               VPA00120
           02  P229-CALENX.                                             VPA00130
               03  P229-CALEN                 OCCURS 24.                VPA00140
                   04  P229-TYYMM   PIC 9(6).                           VPA00150
                   04  P229-TDDX.                                       VPA00160
                       05  P229-TDD PIC 9(1)  OCCURS 31.                VPA00170
