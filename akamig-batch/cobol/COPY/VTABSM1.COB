      *               /*    SHIN SHOUHIN MASTER (HONSHA ZOKUSEI)   */
      *               /*    DATE  02/10/31   V-003                 */
      *               /*    LENGTH = 609                           */
           02 KEY1.
              03 SYOCD                 PIC  X(09).
           02 KEY2.
              03 TSYCD                 PIC  X(09).
           02 KEY3.
              03 SYOMH                 PIC  X(25).
           02 SYOMK                    PIC  X(10).
           02 SYOMY                    PIC  X(10).
           02 MAKCD                    PIC  9(04).
           02 GOJUN                    PIC  X(06).
           02 SBMMK                    PIC  X(02).
           02 HOSOT                    PIC S9(06)V9(1) COMP-3.
           02 KANSN                    PIC S9(02)V9(1) COMP-3.
           02 YAYMD.
              03 YAYY                  PIC  9(04).
              03 YAMM                  PIC  9(02).
              03 YADD                  PIC  9(02).
           02 TYAKN                    PIC S9(07)V9(2) COMP-3.
           02 TYAKO                    PIC S9(07)V9(2) COMP-3.
           02 KCYGM                    PIC  X(01).
           02 HOUKB.
              03 HOUK1                 PIC  X(01).
              03 HOUK2                 PIC  X(01).
              03 HOUK3                 PIC  X(01).
              03 HOUK4                 PIC  X(01).
              03 HOUK5                 PIC  X(01).
              03 HOUK6                 PIC  X(01).
              03 HOUK7                 PIC  X(01).
              03 HOUK8                 PIC  X(01).
              03 HOUK9                 PIC  X(01).
              03 HOUK10                PIC  X(01).
           02 TORIA.
              03 TORI1                 PIC  X(01).
              03 TORI2                 PIC  X(01).
              03 TORI3                 PIC  X(01).
              03 TORI4                 PIC  X(01).
              03 TORI5                 PIC  X(01).
              03 TORI6                 PIC  X(01).
              03 TORI7                 PIC  X(01).
              03 TORI8                 PIC  X(01).
           02 GIGYO                    PIC  X(01).
           02 SMKUH                    PIC  X(01).
           02 KKSCD                    PIC  X(09).
           02 SHIKN                    PIC S9(09)      COMP-3.
           02 SHIKO                    PIC S9(09)      COMP-3.
           02 KONPO                    PIC S9(05)      COMP-3.
           02 TOYMD.
              03 TOYY                  PIC  9(04).
              03 TOMM                  PIC  9(02).
              03 TODD                  PIC  9(02).
           02 HEYMD.
              03 HEYY                  PIC  9(04).
              03 HEMM                  PIC  9(02).
              03 HEDD                  PIC  9(02).
           02 HAYMD.
              03 HAYY                  PIC  9(04).
              03 HAMM                  PIC  9(02).
              03 HADD                  PIC  9(02).
           02 SHMKK                    PIC  X(25).
           02 SKYKK                    PIC  X(20).
           02 HTYMD.
              03 HTYY                  PIC  9(04).
              03 HTMM                  PIC  9(02).
              03 HTDD                  PIC  9(02).
           02 KSSCD                    PIC  X(12).
           02 ANAYK                    PIC  X(05).
           02 SYAKO                    PIC  X(04).
           02 JANCD                    PIC  X(13).
           02 CUYMD.
              03 CUYY                  PIC  9(04).
              03 CUMM                  PIC  9(02).
              03 CUDD                  PIC  9(02).
           02 JDKBN                    PIC  X(01).
           02 JCHFLG                   PIC  X(01).
           02 HKRCD                    PIC  9(04).
           02 SHICD                    PIC  9(04).
           02 SIHKB                    PIC  X(01).
           02 JSHKB                    PIC  X(01).
           02 URBKB                    PIC  X(02).
           02 TTKBN                    PIC  X(01).
           02 ZAIKT                    PIC S9(11)V9(2) COMP-3.
           02 SHZRN                    PIC S9(02)V9(1) COMP-3.
           02 SHZRO                    PIC S9(02)V9(1) COMP-3.
           02 SZRYMD.
              03 SZSS                  PIC  9(02).
              03 SZYMD.
                 04 SZYY               PIC  9(02).
                 04 SZMM               PIC  9(02).
                 04 SZDD               PIC  9(02).
           02 SHKYMD.
              03 SKSS                  PIC  9(02).
              03 SKHYMD.
                 04 SKYY               PIC  9(02).
                 04 SKMM               PIC  9(02).
                 04 SKDD               PIC  9(02).
           02 ARAKN                    PIC S9(09)      COMP-3.
           02 ARAKO                    PIC S9(09)      COMP-3.
           02 AKHKB.
              03 AKSS                  PIC  9(02).
              03 AKYMD.
                 04 AKYY               PIC  9(02).
                 04 AKMM               PIC  9(02).
                 04 AKDD               PIC  9(02).
           02 NONYGN                   PIC S9(09)      COMP-3.
           02 NONYGO                   PIC S9(09)      COMP-3.
           02 NGHKB.
              03 NGSS                  PIC  9(02).
              03 NGYMD.
                 04 NGYY               PIC  9(02).
                 04 NGMM               PIC  9(02).
                 04 NGDD               PIC  9(02).
           02 SMSTN                    PIC S9(09)      COMP-3.
           02 SMSTO                    PIC S9(09)      COMP-3.
           02 SMHYMD.
              03 SMSS                  PIC  9(02).
              03 SMYMD.
                 04 SMYY               PIC  9(02).
                 04 SMMM               PIC  9(02).
                 04 SMDD               PIC  9(02).
           02 IMSCD                    PIC  X(09).
           02 IMSGN                    PIC  X(18).
           02 IMSKD.
              03 IMSHCD                PIC  X(05).
              03 IMSSCD                PIC  X(05).
           02 IMSHN                    PIC  X(18).
           02 IMSKB                    PIC  X(01).
           02 ARAHR.
              03 ARAH1                 PIC S9(02)V9(1) COMP-3.
              03 ARAH2                 PIC S9(02)V9(1) COMP-3.
              03 ARAH3                 PIC S9(02)V9(1) COMP-3.
              03 ARAH4                 PIC S9(02)V9(1) COMP-3.
           02 ZHAKB                    PIC  X(01).
           02 KHHMK                    PIC  X(01).
           02 SHYKN                    PIC S9(09)      COMP-3.
           02 SHYKO                    PIC S9(09)      COMP-3.
           02 SHHYMD.
              03 SHYSS                 PIC  9(02).
              03 SHYYMD.
                 04 SHYYY              PIC  9(02).
                 04 SHYMM              PIC  9(02).
                 04 SHYDD              PIC  9(02).
           02 ZNCHN                    PIC  X(01).
           02 ZCTNGP.
              03 ZCTSS                 PIC  9(02).
              03 ZCTYMD.
                 04 ZCTYY              PIC  9(02).
                 04 ZCTMM              PIC  9(02).
                 04 ZCTDD              PIC  9(02).
           02 ZNDNGP.
              03 ZCDSS                 PIC  9(02).
              03 ZCDYMD.
                 04 ZCDYY              PIC  9(02).
                 04 ZCDMM              PIC  9(02).
                 04 ZCDDD              PIC  9(02).
           02 NSEKB                    PIC  X(01).
           02 KAKTO                    PIC  X(01).
           02 SYAKON.
              03 YKCD1                 PIC  X(02).
              03 YKCD2                 PIC  X(02).
              03 YKCD3                 PIC  X(02).
           02 KOHKB                    PIC  X(01).
           02 IPMEJ                    PIC  X(70).
           02 IPMEK                    PIC  X(50).
           02 KKYBN                    PIC S9(09)      COMP-3.
           02 KKYBO                    PIC S9(09)      COMP-3.
           02 KYBSYMD.
              03 KYBSS                 PIC  9(02).
              03 KYBYMD.
                 04 KYBYY              PIC  9(02).
                 04 KYBMM              PIC  9(02).
                 04 KYBDD              PIC  9(02).
           02 YOBI01                   PIC  X(02).
           02 YOBI02                   PIC  X(02).
