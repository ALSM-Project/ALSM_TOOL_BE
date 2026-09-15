      *                        /*    PARM  FOR  VSBA1250      */
      *                        /*    DATE  91/10/03   V-001   */
      *                        /*    LENGTH   14              */
           02  PC5-YMDS8           PIC  9(8).
           02  PC5-RYMDS8    REDEFINES  PC5-YMDS8.
               03  PC5-SSS8        PIC  9(2).
               03  PC5-YMD8        PIC  9(6).
               03  PC5-RYMD8 REDEFINES  PC5-YMD8.
                   04  PC5-YYS8    PIC  9(2).
                   04  PC5-MMS8    PIC  9(2).
                   04  PC5-DDS8    PIC  9(2).
           02  PC5-YMDW6           PIC  9(6).
           02  PC5-RYMDW6    REDEFINES  PC5-YMDW6.
               03  PC5-YYW6        PIC  9(2).
               03  PC5-MMW6        PIC  9(2).
               03  PC5-DDW6        PIC  9(2).
