      *                        /*    PARM  FOR  VSBA1200      */
      *                        /*    DATE  91/10/02   V-001   */
      *                        /*    LENGTH   8               */
           02  PC0-YMDS8           PIC  9(8).
           02  PC0-RYMDS8    REDEFINES  PC0-YMDS8.
               03  PC0-SSS8        PIC  9(2).
               03  PC0-YMD8        PIC  9(6).
               03  PC0-RYMD8 REDEFINES  PC0-YMD8.
                   04  PC0-YYS8    PIC  9(2).
                   04  PC0-MMS8    PIC  9(2).
                   04  PC0-DDS8    PIC  9(2).
