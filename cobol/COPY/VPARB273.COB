      *                        /*    PARM  FOR  VSBA2730      */
      *                        /*    DATE  98/10/20   V-001   */
      *                        /*    LENGTH   13              */
           02  P273-SYMD           PIC  9(8).
           02  P273-SYMDX    REDEFINES  P273-SYMD.
               03  P273-SS         PIC  9(2).
               03  P273-YMD.
                   04  P273-YY     PIC  9(2).
                   04  P273-MM     PIC  9(2).
                   04  P273-DD     PIC  9(2).
           02  P273-SIMEB          PIC  9(2).
           02  P273-NISSU          PIC  9(3).
