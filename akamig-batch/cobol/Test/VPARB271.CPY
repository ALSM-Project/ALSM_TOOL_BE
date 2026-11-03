      *                        /*    PARM  FOR  VSBA2710      */
      *                        /*    DATE  98/10/20   V-001   */
      *                        /*    LENGTH   34              */
           02  P271-SYMDO          PIC  9(6).
           02  P271-SYMDOX    REDEFINES  P271-SYMDO.
               03  P271-SS         PIC  9(2).
               03  P271-YY         PIC  9(2).
               03  P271-MM         PIC  9(2).
           02  P271-SKDCD          PIC  X(4).
           02  P271-KZHBI          PIC  9(8).
           02  P271-KZHBIX    REDEFINES  P271-KZHBI.
               03  P271-KZSS       PIC  9(2).
               03  P271-KZYMD.
                   04  P271-KZYY   PIC  9(2).
                   04  P271-KZMM   PIC  9(2).
                   04  P271-KZDD   PIC  9(2).
           02  P271-IDDBI          PIC  9(8).
           02  P271-IDDBIX    REDEFINES  P271-IDDBI.
               03  P271-IDSS       PIC  9(2).
               03  P271-IDYMD.
                   04  P271-IDYY   PIC  9(2).
                   04  P271-IDMM   PIC  9(2).
                   04  P271-IDDD   PIC  9(2).
           02  P271-KJSBI          PIC  9(8).
           02  P271-KJSBIX    REDEFINES  P271-KJSBI.
               03  P271-KJSS       PIC  9(2).
               03  P271-KJYMD.
                   04  P271-KJYY   PIC  9(2).
                   04  P271-KJMM   PIC  9(2).
                   04  P271-KJDD   PIC  9(2).
