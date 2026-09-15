      *                        /*    PARM  FOR  VSBA3310      */
      *                        /*    DATE  04/05/31   V-001   */
      *                        /*    LENGTH   18              */
           02  P331-SYMDO          PIC  9(6).
           02  P331-SYMDOX    REDEFINES  P331-SYMDO.
               03  P331-SS         PIC  9(2).
               03  P331-YY         PIC  9(2).
               03  P331-MM         PIC  9(2).
           02  P331-SKDCD          PIC  X(4).
           02  P331-KZHBI          PIC  9(8).
           02  P331-KZHBIX    REDEFINES  P331-KZHBI.
               03  P331-KZSS       PIC  9(2).
               03  P331-KZYMD.
                   04  P331-KZYY   PIC  9(2).
                   04  P331-KZMM   PIC  9(2).
                   04  P331-KZDD   PIC  9(2).
