      *                        /*    PARM  FOR  VSBA1260      */        VPA00010
      *                        /*    DATE  91/10/10   V-001   */        VPA00020
      *                        /*    LENGTH  900                        VPA00020
           02  PC6-KUBUN            PIC  X(01).                         VPA00030
           02  PC6-EGYNS            PIC  9(02).                         VPA00030
           02  PC6-HZUK1.                                               VPA00030
               03  PC6-HZSYM        PIC  9(06).                         VPA00040
               03  PC6-HZDD         PIC  9(02).                         VPA00050
           02  PC6-LDFLG            PIC  X(01).                         VPA00070
           02  PC6-CDTBL.                                               VPA00120
               03  PC6-CALEN                    OCCURS 24.              VPA00130
                   04  PC6-CTSYM    PIC  9(06).                         VPA00140
                   04  PC6-CTDDX.                                       VPA00150
                       05  PC6-CTDD PIC  9(01)  OCCURS 31.              VPA00160
