      *                        /*  FURIKOMI TESURYO MASTER        */
      *                        /*  LENGTH = 57                    */
      *                        /*  DATE  17/11/13   V-001         */
           02 KEY1.
              03 TOKCD                      PIC  X(07).
              03 NOSCD                      PIC  X(02).
              03 DATKB                      PIC  X(01).
              03 SEQ                        PIC  9(03).
           02 KJNGKB                        PIC  X(01).
           02 TESRY                         PIC S9(05) COMP-3.
           02 CKNKJ.
              03 NYKHI.
                 04 NYHSS                   PIC  9(02).
                 04 NYHYMD.
                    05 NYHYY                PIC  9(02).
                    05 NYHMM                PIC  9(02).
                    05 NYHDD                PIC  9(02).
              03 NYKGK                      PIC S9(11) COMP-3.
           02 FRMKFG                        PIC  X(01).
           02 MRMSJ                         PIC  X(01).
           02 SJPST                         PIC S9(02)V9 COMP-3.
           02 KSYTGB                        PIC S9(11) COMP-3.
           02 TORKB.
              03 TRKSE                      PIC  9(02).
              03 TKYMD.
                 04 TRKYY                   PIC  9(02).
                 04 TRKMM                   PIC  9(02).
                 04 TRKDD                   PIC  9(02).
           02 HENKB.
              03 HENSE                      PIC  9(02).
              03 HNYMD.
                 04 HENYY                   PIC  9(02).
                 04 HENMM                   PIC  9(02).
                 04 HENDD                   PIC  9(02).
