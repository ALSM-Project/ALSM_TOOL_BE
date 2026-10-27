      *                               /*    KAISHU JYOUHOU MASTER    */
      *                               /*    DATE  00/08/08   V-002   */
      *                               /*    LENGTH = 311             */
           02 KEY1.
              03 TOKCD                 PIC  X(07).
              03 NOSCD                 PIC  X(02).
           02 KSSBT                    PIC  X(02).
           02 KDTKCD                   PIC  X(07).
           02 KSYTB                    PIC  X(02).
           02 KSJK                     PIC  X(01).
           02 SITTS                    PIC  9(02).
           02 PSTKS                    PIC S9(02)V9 COMP-3.
           02 TGKKS                    PIC S9(11)   COMP-3.
           02 TGTKJ                    PIC  9(03).
           02 FKTSHS                   PIC  X(01).
           02 SKNBHS                   PIC  X(01).
           02 RYSHS                    PIC  X(01).
           02 FGHCD1                   PIC  X(04).
           02 FGHCD2                   PIC  X(04).
           02 SKDKCD                   PIC  X(04).
           02 YKNSB                    PIC  X(01).
           02 KZABG                    PIC  X(07).
           02 SMKGKA.
              03 SMGCDA                PIC  X(04).
              03 SMSTCA                PIC  X(03).
              03 FKMIRA                PIC  X(48).
           02 SMKGKB.
              03 SMGCDB                PIC  X(04).
              03 SMSTCB                PIC  X(03).
              03 FKMIRB                PIC  X(48).
           02 SMKGKC.
              03 SMGCDC                PIC  X(04).
              03 SMSTCC                PIC  X(03).
              03 FKMIRC                PIC  X(48).
           02 FKMICD                   PIC  X(10).
           02 KFTKB                    PIC  X(01).
           02 TOYMD.
              03 TOSSD                 PIC  9(02).
              03 TOYMDD.
                 04 TOYYD              PIC  9(02).
                 04 TOMMD              PIC  9(02).
                 04 TODDD              PIC  9(02).
           02 HEKYMD.
              03 HKSS                  PIC  9(02).
              03 HYMD.
                 04 HKYY               PIC  9(02).
                 04 HKMM               PIC  9(02).
                 04 HKDD               PIC  9(02).
           02 KFSSB                    PIC  X(01).
           02 KFSHDT.
              03 KFSHSS                PIC  9(02).
              03 KFHYMD.
                 04 KFSHYY             PIC  9(02).
                 04 KFSHMM             PIC  9(02).
                 04 KFSHDD             PIC  9(02).
           02 SKFKSB                   PIC  X(01).
           02 SKFKYB.
              03 SKFKSS                PIC  9(02).
              03 SKFYMD.
                 04 SKFKYY             PIC  9(02).
                 04 SKFKMM             PIC  9(02).
                 04 SKFKDD             PIC  9(02).
           02 KSSJK.
              03 TEIGK                 PIC S9(11)   COMP-3.
              03 KKASI.
                 04 KKASS              PIC  9(02).
                 04 KKAYM.
                    05 KKAYY           PIC  9(02).
                    05 KKAMM           PIC  9(02).
              03 KKIGN.
                 04 KKISS              PIC  9(02).
                 04 KKIYM.
                    05 KKIYY           PIC  9(02).
                    05 KKIMM           PIC  9(02).
              03 KEISN1                PIC  X(01).
              03 KEISN2                PIC  X(01).
              03 SJTRB.
                 04 SJRSS              PIC  9(02).
                 04 SJRYMD.
                    05 SJRYY           PIC  9(02).
                    05 SJRMM           PIC  9(02).
                    05 SJRDD           PIC  9(02).
              03 SJHNB.
                 04 SJNSS              PIC  9(02).
                 04 SJNYMD.
                    05 SJNYY           PIC  9(02).
                    05 SJNMM           PIC  9(02).
                    05 SJNDD           PIC  9(02).
              03 SJHSB.
                 04 SJHSS              PIC  9(02).
                 04 SJHYMD.
                    05 SJHYY           PIC  9(02).
                    05 SJHMM           PIC  9(02).
                    05 SJHDD           PIC  9(02).
