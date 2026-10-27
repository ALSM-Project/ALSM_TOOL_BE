      *                        /*    PARM  FOR  VSBA1320      */
      *                        /*    DATE  98/04/09   V-001   */
      *                        /*    LENGTH = 14              */
           02  P132-DATE           PIC  9(08).
           02  P132-DATEX    REDEFINES  P132-DATE.
               03  P132-YY1        PIC  9(04).
               03  P132-MM1        PIC  9(02).
               03  P132-DD1        PIC  9(02).
           02  P132-GATUDO         PIC  9(06).
           02  P132-GATUDOX  REDEFINES  P132-GATUDO.
               03  P132-YY2        PIC  9(04).
               03  P132-MM2        PIC  9(02).
