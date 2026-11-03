      *                        /*    PARM  FOR  VSBA0430      */        00010000
      *                        /*    DATE  86/02/26   V-002   */        00020000
           02  P43-LPDAT           PIC X(160).                          00030000
           02  P43-DATA.                                                00040000
               03  FILLER OCCURS 20 TIMES.                              00050000
                   04  P43-HNBCD   PIC X(01).                           00060000
                   04  P43-START   PIC 9(03).                           00070000
                   04  P43-LENGH   PIC 9(03).                           00080000
           02  P43-DATAL           PIC 9(03).                           00090000
           02  P43-DATAM           PIC X(196).                          00100000
           02  P43-FILLER          PIC X(29).                           00110000
