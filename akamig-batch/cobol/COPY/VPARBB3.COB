      *                        /*    PARM  FOR  VSBA1130      */        VPA00010
      *                        /*    DATE  91/10/09   V-001   */        VPA00020
      *                        /*    LENGTH  898                        VPA00020
           02  PB3-SYMD             PIC  9(08).                         VPA00030
           02  PB3-SYMDR    REDEFINES PB3-SYMD.                         VPA00070
               03  PB3-SYMDSS       PIC  9(02).                         VPA00040
               03  PB3-SYMDYY       PIC  9(02).                         VPA00050
               03  PB3-SYMDMM       PIC  9(02).                         VPA00050
               03  PB3-SYMDDD       PIC  9(02).                         VPA00060
           02  FILLER       REDEFINES PB3-SYMD.                         VPA00070
               03  PB3-SYM          PIC  X(06).                         VPA00080
               03  FILLER           PIC  X(02).                         VPA00090
           02  PB3-KYJHS            PIC  9(01).                         VPA00070
           02  PB3-LDFLG            PIC  9(01).                         VPA00070
           02  PB3-CDTBL.                                               VPA00120
               03  PB3-CALEN                   OCCURS 24.               VPA00130
                   04  PB3-CTSYM    PIC  9(06).                         VPA00140
                   04  PB3-CTDDX.                                       VPA00150
                       05  PB3-CTDD PIC  9(01) OCCURS 31.               VPA00160
