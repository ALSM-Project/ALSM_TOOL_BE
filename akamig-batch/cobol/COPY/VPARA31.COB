      *                        /*    PARM  FOR  VSAA0310      */        BPA00010
      *                        /*    DATE  85/01/25   V-001   */        BPA00020
      *                        /*    LENGTH  6                */        BPA00020
           02  C31-HNBCD         PIC  X(1).                             BPA00030
           02  C31-TRMID.                                               BPA00080
             03  C31-BASID.
               04  C31-STNCD     PIC  X(1).
               04  C31-SOKCD     PIC  X(1).
             03  FILLER          PIC  X(2).
           02  C31-RTNCD         PIC  X(1).                             BPA00030
