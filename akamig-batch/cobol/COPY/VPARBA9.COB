      *                        /*    PARM  FOR  VSBA1090      */
      *                        /*    DATE  91/09/24   V-001   */
           02  PA9-YMDS6           PIC  9(6).
           02  PA9-RYMDS6    REDEFINES  PA9-YMDS6.
               03  PA9-YYS6        PIC  9(2).
               03  PA9-MMS6        PIC  9(2).
               03  PA9-DDS6        PIC  9(2).
           02  PA9-YMDS8           PIC  9(8).
           02  PA9-RYMDS8    REDEFINES  PA9-YMDS8.
               03  PA9-SSS8        PIC  9(2).
               03  PA9-YMD8        PIC  9(6).
               03  PA9-RYMD8 REDEFINES  PA9-YMD8.
                   04  PA9-YYS8    PIC  9(2).
                   04  PA9-MMS8    PIC  9(2).
                   04  PA9-DDS8    PIC  9(2).
