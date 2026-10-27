      *                        /*    PARM  FOR  VSBA2720      */
      *                        /*    DATE  98/10/20   V-001   */
      *                        /*    LENGTH   8               */
           02  P272-SYMD           PIC  9(8).
           02  P272-SYMDX    REDEFINES  P272-SYMD.
               03  P272-SS         PIC  9(2).
               03  P272-YMD.
                   04  P272-YY     PIC  9(2).
                   04  P272-MM     PIC  9(2).
                   04  P272-DD     PIC  9(2).
