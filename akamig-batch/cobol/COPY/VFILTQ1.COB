           02 TQ1-KYOKM.
              03 TQ1-JTERM              PIC X(04).
              03 TQ1-KTERM.
                 04 TQ1-BUKG            PIC X(01).
                 04 TQ1-SOKOK           PIC X(01).
                 04 FILLER              PIC X(02).
              03 TQ1-SYMDS-X.
                 04 TQ1-SSES            PIC 9(02).
                 04 TQ1-SNTHS           PIC 9(06).
                 04 TQ1-RSNTHS  REDEFINES TQ1-SNTHS.
                    05 TQ1-SYYS         PIC 9(02).
                    05 TQ1-SMMS         PIC 9(02).
                    05 TQ1-SDDS         PIC 9(02).
              03 TQ1-SYMDS      REDEFINES TQ1-SYMDS-X
                                        PIC 9(08).
              03 TQ1-SYMDW-X.
                 04 TQ1-SYYW            PIC 9(02).
                 04 TQ1-SMMW            PIC 9(02).
                 04 TQ1-SDDW            PIC 9(02).
              03 TQ1-SYMDW      REDEFINES TQ1-SYMDW-X
                                        PIC 9(06).
              03 TQ1-GYMDS-X.
                 04 TQ1-GSES            PIC 9(02).
                 04 TQ1-GNTHS           PIC 9(06).
                 04 TQ1-RGNTHS  REDEFINES TQ1-GNTHS.
                    05 TQ1-GYYS         PIC 9(02).
                    05 TQ1-GMMS         PIC 9(02).
                    05 TQ1-GDDS         PIC 9(02).
              03 TQ1-GYMDS      REDEFINES TQ1-GYMDS-X
                                        PIC 9(08).
              03 TQ1-GYMDW-X.
                 04 TQ1-GYYW            PIC 9(02).
                 04 TQ1-GMMW            PIC 9(02).
                 04 TQ1-GDDW            PIC 9(02).
              03 TQ1-GYMDW      REDEFINES TQ1-GYMDW-X
                                        PIC 9(06).
              03 TQ1-KGHSX.
                 04 TQ1-KGHSS           PIC 9(02).
                 04 TQ1-KYMDS           PIC 9(06).
                 04 TQ1-KYMDSR  REDEFINES TQ1-KYMDS.
                    05 TQ1-KGHSY        PIC 9(02).
                    05 TQ1-KGHSM        PIC 9(02).
                    05 TQ1-KGHSD        PIC 9(02).
              03 TQ1-KGHSR      REDEFINES TQ1-KGHSX
                                        PIC 9(08).
              03 TQ1-KGHWX.
                 04 TQ1-KGHWY           PIC 9(02).
                 04 TQ1-KGHWM           PIC 9(02).
                 04 TQ1-KGHWD           PIC 9(02).
              03 TQ1-KGHWR      REDEFINES TQ1-KGHWX
                                        PIC 9(06).
              03 TQ1-SYZKU.
                 04 TQ1-SZBNS           PIC X(03).
                 04 TQ1-SZKNO           PIC X(03).
              03 TQ1-PASWD              PIC X(04).
              03 FILLER                 PIC X(04).
