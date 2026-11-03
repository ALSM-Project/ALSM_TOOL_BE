./ ADD NAME=VBKA4150  01.03 20150728 20190621 105140 00776 00769 MCSK012
       ID  DIVISION.
       PROGRAM-ID.     'VBKA4150'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : 10 SYSTEM                     *
      *              <> SUBSYSTEM      : K.KAI                         *
      *              <> PG.OUTLINE     : SEIKYUU-YOU TORIHIKI MEISAI   *
      *                                     DATA SAKUSEI               *
      *                 * INPUT FILE   : BU-SEIKYUU-SYO UCHIWAKE DATA  *
      *                                                         (DWKD) *
      *                                : TORIHIKI MEISAI DATA   (DWKA) *
      *                                : TOKUISAKI MASTER    (TMSFILE) *
      *                 * OUTPUT FILE  : TANPYOU-YOU TOUGETSU TORIHIKI *
      *                                     TORIKESHI DATA      (DWKB) *
      *                                : TANPYOU-YOU TORIHIKI MEISAI   *
      *                                     DATA                (DWKG) *
      *                                : SEIKYUU-SYO UCHIWAKE DATA     *
      *                                                         (DWKE) *
      *                                : ICHIRANSEI-YOU TOUGETSU       *
      *                                     SEIKYUU HANBAI DATA (DWKF) *
      *                                : ICHIRANSEI-YOU TOUGETSU       *
      *                                     SEIKYUU TORIKESHI DATA     *
      *                                                         (DWKC) *
      *                                                                *
      *              <> AUTHOR         : CSK-M.KATOH                   *
      *              <> DATE           : 95/06/05                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) K-203              Õ›∫≥   : 96/10/25  CSK-K.ITO           *
      * (02) K-232              Õ›∫≥   : 97/02/15  CSK-M.KATOH         *
      * (03) K-261              Õ›∫≥   : 97/10/02  CSK-H.YOSHIMOTO     *
      * (04) K-275C             Õ›∫≥   : 98/04/30  CSK-R.TAKAYANAGI    *
      * (05) K-568              Õ›∫≥   : 03/07/02  CSK-M.YOSHIDA       *
      * (05) H-010  T-6336      Õ›∫≥   : 07/09/10  CSK-Y.YAMADA        *
      * (06) K-1549             Õ›∫≥   : 13/02/06  CSK-N.EGUCHI        *
      * (07) K-1758           ÿ∫› ﬂ≤Ÿ  : 15/07/09  CSK-K.KOYAMA        *
      * (08) K-1780             Õ›∫≥   : 15/07/28  CSK-Y.YOSHIGA       *
      * (09) K-2163             Õ›∫≥   : 19/06/21  CSK-M.MATSUSHIMA    *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKCFILE ASSIGN    TO S-DWKC.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
           SELECT WKEFILE ASSIGN    TO S-DWKE.
           SELECT WKFFILE ASSIGN    TO S-DWKF.
           SELECT WKGFILE ASSIGN    TO S-DWKG.
      *    SELECT TMSFILE ASSIGN    TO TMSFILE
      *                   ORGANIZATION INDEXED
      *                   ACCESS MODE  RANDOM
      *                   RECORD KEY   KEY-TMS
      *                   FILE STATUS  S-CODE.
       DATA DIVISION.
       FILE SECTION.
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(260).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(250).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(180).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(280).
       FD  WKEFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKE-REC.
           02 FILLER                        PIC  X(80).
       FD  WKFFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKF-REC.
           02 FILLER                        PIC  X(180).
       FD  WKGFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKG-REC.
           02 FILLER                        PIC  X(250).
      *FD  TMSFILE
      *    LABEL RECORD STANDARD.
      *01  TMS-REC.
      *    02 KEY-TMS                       PIC  X(07).
      *    02 FILLER                        PIC  X(293).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STRAGE SECTION                                      *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
      *01  TMS-R.
      *    COPY VFILTMS SUPPRESS.
       01  P04-PARM.
           COPY VPARB04 SUPPRESS.
       01  P05-PARM.
           COPY VPARB05 SUPPRESS.
       01  WKA-R.
           02 WKA-BUKGO                     PIC  X(01).
           02 WKA-SOKOK                     PIC  X(01).
           02 WKA-DNPNO.
              03 WKA-DNPN1                  PIC  X(01).
              03 WKA-DNPN2                  PIC  X(06).
           02 WKA-TORIK.
              03 WKA-TURIK                  PIC  X(01).
              03 WKA-THAIK                  PIC  X(01).
           02 WKA-CKUKG.
              03 WKA-CKUKA                  PIC  X(03).
              03 WKA-CKUCK                  PIC  X(02).
           02 WKA-TOKCD.
              03 WKA-TKCD1                  PIC  X(04).
              03 WKA-TKCD2                  PIC  X(03).
           02 WKA-SSOKO                     PIC  X(01).
           02 WKA-SCKUK                     PIC  X(05).
           02 WKA-SKYCD                     PIC  X(07).
           02 WKA-SNSCD                     PIC  X(02).
           02 WKA-HAYMD                     PIC  9(08).
           02 WKA-LINNO                     PIC  9(01).
           02 WKA-SYOCD                     PIC  X(09).
           02 WKA-SURYO                     PIC S9(05) COMP-3.
           02 WKA-TANKA                     PIC S9(08) COMP-3.
           02 WKA-NTANK                     PIC S9(08) COMP-3.
           02 WKA-HDNPN                     PIC  X(07).
           02 WKA-HDNPL                     PIC  9(01).
           02 WKA-HDNPD                     PIC  9(08).
           02 WKA-HDNPT                     PIC  X(02).
           02 WKA-URIDA                     PIC  9(08).
           02 WKA-ZHYOS                     PIC  X(01).
           02 WKA-HKHYO                     PIC  X(01).
           02 WKA-GIGYD                     PIC  9(08).
           02 WKA-SBMKG                     PIC  X(02).
           02 WKA-SYOME.
              03 WKA-SYOMH                  PIC  X(25).
              03 WKA-SYOMK                  PIC  X(10).
              03 WKA-SYOMY                  PIC  X(10).
           02 WKA-SHIMC                     PIC  X(04).
           02 WKA-SHIMM                     PIC  X(08).
           02 WKA-TSIME                     PIC  9(02).
           02 WKA-SEKMM                     PIC  9(02).
           02 WKA-TSYCD                     PIC  X(09).
           02 WKA-NOSCD                     PIC  X(02).
           02 WKA-TSFKB                     PIC  X(01).
           02 WKA-THAKB                     PIC  X(01).
           02 WKA-ADNPN                     PIC  X(07).
           02 WKA-ADNPL                     PIC  9(01).
           02 WKA-ADNPD                     PIC  9(08).
           02 WKA-ADNPT                     PIC  X(02).
           02 WKA-TOKME                     PIC  X(25).
           02 WKA-TKSYM                     PIC  9(06).
           02 WKA-OSOKO                     PIC  X(01).
           02 WKA-ZHAKB                     PIC  X(01).
           02 WKA-SSIMD                     PIC  9(08).
           02 WKA-KETKB                     PIC  X(01).
           02 WKA-SHZEI                     PIC S9(11) COMP-3.
           02 WKA-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKA-DSHZFG                    PIC  X(01).
           02 WKA-KGZKB                     PIC  X(01).
           02 FILLER                        PIC  X(09).
       01  WKB-R.
           02 WKB-BUKGO                     PIC  X(01).
           02 WKB-SOKKG                     PIC  X(01).
           02 WKB-DNPNO.
              03 WKB-DNPN1                  PIC  X(01).
              03 WKB-DNPN2                  PIC  X(06).
           02 WKB-TORIK.
              03 WKB-TURIK                  PIC  X(01).
              03 WKB-THAIK                  PIC  X(01).
           02 WKB-CKUKG.
              03 WKB-CKUKA                  PIC  X(03).
              03 WKB-CKUCK                  PIC  X(02).
           02 WKB-TOKCD.
              03 WKB-TKCD1                  PIC  X(04).
              03 WKB-TKCD2                  PIC  X(03).
           02 WKB-SSOKO                     PIC  X(01).
           02 WKB-SCKUK                     PIC  X(05).
           02 WKB-SKYCD                     PIC  X(07).
           02 WKB-SNSCD                     PIC  X(02).
           02 WKB-HAKOD                     PIC  9(08).
           02 WKB-LINNO                     PIC  9(01).
           02 WKB-SYOCD                     PIC  X(09).
           02 WKB-SURYO                     PIC S9(05) COMP-3.
           02 WKB-TANKA                     PIC S9(08) COMP-3.
           02 WKB-NTANK                     PIC S9(08) COMP-3.
           02 WKB-HDNPN                     PIC  X(07).
           02 WKB-HDNPL                     PIC  9(01).
           02 WKB-HDNPD                     PIC  9(08).
           02 WKB-HDNPT                     PIC  X(02).
           02 WKB-URIDA                     PIC  9(08).
           02 WKB-ZHYOS                     PIC  X(01).
           02 WKB-HKHYO                     PIC  X(01).
           02 WKB-GIGYD                     PIC  9(08).
           02 WKB-SBMKG                     PIC  X(02).
           02 WKB-SYOME.
              03 WKB-SYOMH                  PIC  X(25).
              03 WKB-SYOMK                  PIC  X(10).
              03 WKB-SYOMY                  PIC  X(10).
           02 WKB-SHIMC                     PIC  X(04).
           02 WKB-SHIMM                     PIC  X(08).
           02 WKB-TSIME                     PIC  9(02).
           02 WKB-SEKMM                     PIC  9(02).
           02 WKB-TSYCD                     PIC  X(09).
           02 WKB-NOSCD                     PIC  X(02).
           02 WKB-TSFKB                     PIC  X(01).
           02 WKB-THAKB                     PIC  X(01).
           02 WKB-ADNPN                     PIC  X(07).
           02 WKB-ADNPL                     PIC  9(01).
           02 WKB-ADNPD                     PIC  9(08).
           02 WKB-ADNPT                     PIC  X(02).
           02 WKB-TOKME                     PIC  X(25).
           02 WKB-TKSYM                     PIC  9(06).
           02 WKB-OSOKO                     PIC  X(01).
           02 WKB-ZHAKB                     PIC  X(01).
           02 WKB-SSIMD                     PIC  9(08).
           02 WKB-KETKB                     PIC  X(01).
           02 WKB-SHZEI                     PIC S9(11) COMP-3.
           02 FILLER                        PIC  X(03).
       01  WKC-R.
           02 WKC-BUNO                      PIC  X(03).
           02 WKC-SKYCD                     PIC  X(07).
           02 WKC-SNSCD                     PIC  X(02).
           02 WKC-SOKOK                     PIC  X(01).
           02 WKC-CKUKG                     PIC  X(05).
           02 WKC-DNPNO.
              03 WKC-DNPN1                  PIC  X(01).
              03 WKC-DNPN2                  PIC  X(06).
           02 WKC-TORIK.
              03 WKC-TURIK                  PIC  X(01).
              03 WKC-THAIK                  PIC  X(01).
           02 WKC-TOKCD.
              03 WKC-TKCD1                  PIC  X(04).
              03 WKC-TKCD2                  PIC  X(03).
           02 WKC-LINNO                     PIC  9(01).
           02 WKC-SYOCD                     PIC  X(09).
           02 WKC-SURYO                     PIC S9(05) COMP-3.
           02 WKC-TANKA                     PIC S9(08) COMP-3.
           02 WKC-NTANK                     PIC S9(08) COMP-3.
           02 WKC-HAYMD                     PIC  9(08).
           02 WKC-SYOME.
              03 WKC-SYOMH                  PIC  X(25).
              03 WKC-SYOMK                  PIC  X(10).
              03 WKC-SYOMY                  PIC  X(10).
           02 WKC-HDNPN                     PIC  X(07).
           02 WKC-HDNPL                     PIC  9(01).
           02 WKC-HDNPD                     PIC  9(08).
           02 WKC-HDNPT                     PIC  X(02).
           02 WKC-NOSCD                     PIC  X(02).
           02 WKC-TSFKB                     PIC  X(01).
           02 WKC-THAKB                     PIC  X(01).
           02 WKC-ADNPN                     PIC  X(07).
           02 WKC-ADNPL                     PIC  9(01).
           02 WKC-ADNPD                     PIC  9(08).
           02 WKC-ADNPT                     PIC  X(02).
           02 WKC-KETKB                     PIC  X(01).
           02 WKC-URIDA                     PIC  9(08).
           02 WKC-SHIMM                     PIC  X(08).
           02 WKC-SHZEI                     PIC S9(09) COMP-3.
           02 WKC-SHIMC                     PIC  9(04).
           02 WKC-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKC-DSHZFG                    PIC  X(01).
           02 WKC-KGZKB                     PIC  X(01).
       01  WKD-R.
           02 WKD-BUKGO                     PIC  X(01).
           02 WKD-SSOKO                     PIC  X(01).
           02 WKD-SCKUK.
              03 WKD-SKKNO                  PIC  X(03).
              03 WKD-SKTKK                  PIC  X(02).
           02 WKD-SKYCD                     PIC  X(07).
           02 WKD-SIMEB                     PIC  9(02).
           02 WKD-SNSCD                     PIC  X(02).
           02 WKD-SKSKM                     PIC  X(30).
           02 WKD-SKSNM                     PIC  X(30).
           02 WKD-SKJYU.
              03 WKD-SKJYA                  PIC  X(40).
              03 WKD-SKJYB                  PIC  X(30).
              03 WKD-SKBLM                  PIC  X(30).
           02 WKD-YUBIN                     PIC  X(07).
           02 WKD-SMYMD                     PIC  9(08).
           02 WKD-SEYOS                     PIC  X(01).
           02 WKD-ZNSKY                     PIC S9(13) COMP-3.
           02 WKD-NYUKN                     PIC S9(13) COMP-3.
           02 WKD-NBNMG                     PIC S9(13) COMP-3.
           02 WKD-KURKG                     PIC S9(13) COMP-3.
           02 WKD-TOHNB                     PIC S9(13) COMP-3.
           02 WKD-TOSKY                     PIC S9(13) COMP-3.
           02 WKD-MISKY                     PIC S9(13) COMP-3.
           02 WKD-TOURZ                     PIC S9(13) COMP-3.
           02 WKD-ZANIC                     PIC S9(13) COMP-3.
           02 WKD-ZENBK                     PIC S9(13) COMP-3.
           02 WKD-TOZEI                     PIC S9(13) COMP-3.
           02 WKD-TOHNG                     PIC S9(13) COMP-3.
           02 FILLER                        PIC  X(02).
       01  WKE-R.
           02 WKE-SKYCD                     PIC  X(07).
           02 WKE-SNSCD                     PIC  X(02).
           02 WKE-ZENGT.
              03 WKE-ZGSKY                  PIC S9(13) COMP-3.
              03 WKE-GNYKD                  PIC S9(13) COMP-3.
              03 WKE-NEBIK                  PIC S9(09) COMP-3.
              03 WKE-KURIK                  PIC S9(13) COMP-3.
           02 WKE-TOGET.
              03 WKE-OKAID                  PIC S9(13) COMP-3.
              03 WKE-TGSKY                  PIC S9(13) COMP-3.
              03 WKE-MISKY                  PIC S9(13) COMP-3.
              03 WKE-URKZD                  PIC S9(13) COMP-3.
           02 WKE-ICYOK                     PIC  X(01).
           02 WKE-SHZEI                     PIC S9(13) COMP-3.
           02 WKE-OKAIG                     PIC S9(13) COMP-3.
           02 FILLER                        PIC  X(02).
       01  WKF-R.
           02 WKF-BUNO                      PIC  X(03).
           02 WKF-SKYCD                     PIC  X(07).
           02 WKF-SNSCD                     PIC  X(02).
           02 WKF-SOKOK                     PIC  X(01).
           02 WKF-CKUKG                     PIC  X(05).
           02 WKF-DNPNO.
              03 WKF-DNPN1                  PIC  X(01).
              03 WKF-DNPN2                  PIC  X(06).
           02 WKF-TORIK.
              03 WKF-TURIK                  PIC  X(01).
              03 WKF-THAIK                  PIC  X(01).
           02 WKF-TOKCD.
              03 WKF-TKCD1                  PIC  X(04).
              03 WKF-TKCD2                  PIC  X(03).
           02 WKF-LINNO                     PIC  9(01).
           02 WKF-SYOCD                     PIC  X(09).
           02 WKF-SURYO                     PIC S9(05) COMP-3.
           02 WKF-TANKA                     PIC S9(08) COMP-3.
           02 WKF-NTANK                     PIC S9(08) COMP-3.
           02 WKF-HAYMD                     PIC  9(08).
           02 WKF-SYOME.
              03 WKF-SYOMH                  PIC  X(25).
              03 WKF-SYOMK                  PIC  X(10).
              03 WKF-SYOMY                  PIC  X(10).
           02 WKF-HDNPN                     PIC  X(07).
           02 WKF-HDNPL                     PIC  9(01).
           02 WKF-HDNPD                     PIC  9(08).
           02 WKF-HDNPT                     PIC  X(02).
           02 WKF-NOSCD                     PIC  X(02).
           02 WKF-TSFKB                     PIC  X(01).
           02 WKF-THAKB                     PIC  X(01).
           02 WKF-ADNPN                     PIC  X(07).
           02 WKF-ADNPL                     PIC  9(01).
           02 WKF-ADNPD                     PIC  9(08).
           02 WKF-ADNPT                     PIC  X(02).
           02 WKF-KETKB                     PIC  X(01).
           02 WKF-URIDA                     PIC  9(08).
           02 WKF-SHIMM                     PIC  X(08).
           02 WKF-SHZEI                     PIC S9(09) COMP-3.
           02 WKF-SHIMC                     PIC  9(04).
           02 WKF-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKF-DSHZFG                    PIC  X(01).
           02 WKF-KGZKB                     PIC  X(01).
       01  WKG-R.
           02 WKG-BUKGO                     PIC  X(01).
           02 WKG-SOKKG                     PIC  X(01).
           02 WKG-DNPNO.
              03 WKG-DNPN1                  PIC  X(01).
              03 WKG-DNPN2                  PIC  X(06).
           02 WKG-TORIK.
              03 WKG-TURIK                  PIC  X(01).
              03 WKG-THAIK                  PIC  X(01).
           02 WKG-CKUKG.
              03 WKG-CKUKA                  PIC  X(03).
              03 WKG-CKUCK                  PIC  X(02).
           02 WKG-TOKCD.
              03 WKG-TKCD1                  PIC  X(04).
              03 WKG-TKCD2                  PIC  X(03).
           02 WKG-SSOKO                     PIC  X(01).
           02 WKG-SCKUK                     PIC  X(05).
           02 WKG-SKYCD                     PIC  X(07).
           02 WKG-SNSCD                     PIC  X(02).
           02 WKG-HAKOD                     PIC  9(08).
           02 WKG-LINNO                     PIC  9(01).
           02 WKG-SYOCD                     PIC  X(09).
           02 WKG-SURYO                     PIC S9(05) COMP-3.
           02 WKG-TANKA                     PIC S9(08) COMP-3.
           02 WKG-NTANK                     PIC S9(08) COMP-3.
           02 WKG-HDNPN                     PIC  X(07).
           02 WKG-HDNPL                     PIC  9(01).
           02 WKG-HDNPD                     PIC  9(08).
           02 WKG-HDNPT                     PIC  X(02).
           02 WKG-URIDA                     PIC  9(08).
           02 WKG-ZHYOS                     PIC  X(01).
           02 WKG-HKHYO                     PIC  X(01).
           02 WKG-GIGYD                     PIC  9(08).
           02 WKG-SBMKG                     PIC  X(02).
           02 WKG-SYOME.
              03 WKG-SYOMH                  PIC  X(25).
              03 WKG-SYOMK                  PIC  X(10).
              03 WKG-SYOMY                  PIC  X(10).
           02 WKG-SHIMC                     PIC  X(04).
           02 WKG-SHIMM                     PIC  X(08).
           02 WKG-TSIME                     PIC  9(02).
           02 WKG-SEKMM                     PIC  9(02).
           02 WKG-TSYCD                     PIC  X(09).
           02 WKG-NOSCD                     PIC  X(02).
           02 WKG-TSFKB                     PIC  X(01).
           02 WKG-THAKB                     PIC  X(01).
           02 WKG-ADNPN                     PIC  X(07).
           02 WKG-ADNPL                     PIC  9(01).
           02 WKG-ADNPD                     PIC  9(08).
           02 WKG-ADNPT                     PIC  X(02).
           02 WKG-TOKME                     PIC  X(25).
           02 WKG-TKSYM                     PIC  9(06).
           02 WKG-OSOKO                     PIC  X(01).
           02 WKG-ZHAKB                     PIC  X(01).
           02 WKG-SSIMD                     PIC  9(08).
           02 WKG-KETKB                     PIC  X(01).
           02 WKG-SHZEI                     PIC S9(11) COMP-3.
           02 FILLER                        PIC  X(03).
       01  FLAG-AREA.
           02 WFG1-TMS                      PIC  X(01).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-SKYCD                 PIC  X(07).
              03 WIN1-SNSCD                 PIC  X(02).
           02 WIN2-KEY.
              03 WIN2-SKYCD                 PIC  X(07).
              03 WIN2-SNSCD                 PIC  X(02).
       01  SAVE-AREA.
           02 WSV1-BUKGO                    PIC  X(01).
           02 WSV2-KEY.
              03 WSV2-SKYCD                 PIC  X(07).
              03 WSV2-SNSCD                 PIC  X(02).
           02 WSV3-TOKCD                    PIC  X(07).
           02 WSV3-SOKCD                    PIC  X(01).
       01  TOTAL-AREA.
           02 WTT1-KINGK                    PIC S9(13) COMP-3.
           02 WTT2-KINGK                    PIC S9(13) COMP-3.
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE  AND
                          WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  WKDFILE
                       WKAFILE
      *                TMSFILE
                OUTPUT WKEFILE
                       WKFFILE
                       WKCFILE
                       WKBFILE
                       WKGFILE.
           CALL 'VSBA0040' USING P04-PARM.
           INITIALIZE SAVE-AREA.
           PERFORM M11-READ-WKD THRU M11-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ WKDFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-WKD.
           READ WKDFILE INTO WKD-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M11-EXIT.
           MOVE WKD-SKYCD  TO WIN1-SKYCD.
           MOVE WKD-SNSCD  TO WIN1-SNSCD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M12-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M12-EXIT.
           MOVE WKA-SKYCD  TO WIN2-SKYCD.
           MOVE WKA-SNSCD  TO WIN2-SNSCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY = WIN2-KEY
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY < WIN2-KEY
                 PERFORM M22-WKDONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKAONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           PERFORM M211-WRITE-WKE THRU M211-EXIT.
       M21-L02.
           MOVE WIN2-KEY  TO WSV2-KEY.
           PERFORM M212-MATCH-BREAK THRU M212-EXIT
                   UNTIL  WIN2-KEY NOT = WSV2-KEY.
           PERFORM M11-READ-WKD THRU M11-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : WRITE WKEFILE                                      *
      *----------------------------------------------------------------*
       M211-WRITE-WKE.
           MOVE SPACE TO WKE-R.
           INITIALIZE WKE-R.
           MOVE WKD-SKYCD TO WKE-SKYCD.
           MOVE WKD-SNSCD TO WKE-SNSCD.
           MOVE WKD-ZNSKY TO WKE-ZGSKY.
           MOVE WKD-NYUKN TO WKE-GNYKD.
           COMPUTE WKE-NEBIK = WKD-NBNMG + WKD-ZENBK.
           COMPUTE WKE-KURIK = WKD-KURKG + WKD-NBNMG + WKD-ZENBK.
           MOVE WKD-TOHNB TO WKE-OKAID.
           MOVE WKD-TOSKY TO WKE-TGSKY.
           MOVE WKD-MISKY TO WKE-MISKY.
           MOVE WKD-TOURZ TO WKE-URKZD.
           IF WKD-ZANIC NOT = ZERO
              MOVE '*'    TO WKE-ICYOK.
           MOVE WKD-TOZEI TO WKE-SHZEI.
           MOVE WKD-TOHNG TO WKE-OKAIG.
           WRITE WKE-REC FROM WKE-R.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2 : MATCH BREAK                                        *
      *----------------------------------------------------------------*
       M212-MATCH-BREAK.
           IF WKD-SEYOS = '2'
              IF WKA-THAKB NOT = 'A'
                 MOVE WKA-R TO WKG-R
                 MOVE WKD-SSOKO TO WKG-SSOKO
                 MOVE WKD-SCKUK TO WKG-SCKUK
                 MOVE WKD-SSOKO TO WKG-OSOKO
                 WRITE WKG-REC FROM WKG-R
              ELSE
                 MOVE WKA-R TO WKB-R
                 MOVE WKD-SSOKO TO WKB-SSOKO
                 MOVE WKD-SCKUK TO WKB-SCKUK
                 WRITE WKB-REC FROM WKB-R.
           IF WKA-THAKB NOT = 'A'
              PERFORM M2121-WRITE-WKF THRU M2121-EXIT
           ELSE
              PERFORM M2122-WRITE-WKC THRU M2122-EXIT.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.1 : WRITE WKFFILE                                    *
      *----------------------------------------------------------------*
       M2121-WRITE-WKF.
           MOVE SPACE TO WKF-R.
           INITIALIZE WKF-R.
           IF WSV1-BUKGO = WKA-BUKGO
              GO TO M2121-L01.
           MOVE '6'       TO P05-HNBCD.
           MOVE WKA-BUKGO TO P05-STNCD.
           CALL 'VSBA0050' USING P05-PARM P04-PARM.
           MOVE WKA-BUKGO TO WSV1-BUKGO.
       M2121-L01.
           IF P05-RTNCD NOT = ZERO
              MOVE SPACE     TO WKF-BUNO
           ELSE
              MOVE P05-STNNO TO WKF-BUNO.
           MOVE WKA-SKYCD TO WKF-SKYCD.
           MOVE WKA-SNSCD TO WKF-SNSCD.
           MOVE WKA-SOKOK TO WKF-SOKOK.
           MOVE WKA-CKUKG TO WKF-CKUKG.
           MOVE WKA-DNPNO TO WKF-DNPNO.
           MOVE WKA-TORIK TO WKF-TORIK.
           MOVE WKA-TOKCD TO WKF-TOKCD.
           MOVE WKA-LINNO TO WKF-LINNO.
           MOVE WKA-SYOCD TO WKF-SYOCD.
           MOVE WKA-SURYO TO WKF-SURYO.
           MOVE WKA-TANKA TO WKF-TANKA.
           MOVE WKA-NTANK TO WKF-NTANK.
           MOVE WKA-HAYMD TO WKF-HAYMD.
           MOVE WKA-SYOME TO WKF-SYOME.
           MOVE WKA-HDNPN TO WKF-HDNPN.
           MOVE WKA-HDNPL TO WKF-HDNPL.
           MOVE WKA-HDNPD TO WKF-HDNPD.
           MOVE WKA-HDNPT TO WKF-HDNPT.
           MOVE WKA-NOSCD TO WKF-NOSCD.
           MOVE WKA-TSFKB TO WKF-TSFKB.
           MOVE WKA-THAKB TO WKF-THAKB.
           MOVE WKA-ADNPN TO WKF-ADNPN.
           MOVE WKA-ADNPL TO WKF-ADNPL.
           MOVE WKA-ADNPD TO WKF-ADNPD.
           MOVE WKA-ADNPT TO WKF-ADNPT.
           MOVE WKA-KETKB TO WKF-KETKB.
           MOVE WKA-URIDA TO WKF-URIDA.
           MOVE WKA-SHIMM TO WKF-SHIMM.
           MOVE WKA-SHZEI TO WKF-SHZEI.
           MOVE WKA-SHIMC TO WKF-SHIMC.
           MOVE WKA-ZEIRT TO WKF-ZEIRT.
           MOVE WKA-DSHZFG   TO WKF-DSHZFG.
           MOVE WKA-KGZKB TO WKF-KGZKB.
           WRITE WKF-REC FROM WKF-R.
       M2121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.2.2 : WRITE WKCFILE                                    *
      *----------------------------------------------------------------*
       M2122-WRITE-WKC.
           MOVE SPACE TO WKC-R.
           INITIALIZE WKC-R.
           IF WSV1-BUKGO = WKA-BUKGO
              GO TO M2122-L01.
           MOVE '6'       TO P05-HNBCD.
           MOVE WKA-BUKGO TO P05-STNCD.
           CALL 'VSBA0050' USING P05-PARM P04-PARM.
           MOVE WKA-BUKGO TO WSV1-BUKGO.
       M2122-L01.
           IF P05-RTNCD NOT = ZERO
              MOVE SPACE     TO WKC-BUNO
           ELSE
              MOVE P05-STNNO TO WKC-BUNO.
           MOVE WKA-SKYCD TO WKC-SKYCD.
           MOVE WKA-SNSCD TO WKC-SNSCD.
           MOVE WKA-SOKOK TO WKC-SOKOK.
           MOVE WKA-CKUKG TO WKC-CKUKG.
           MOVE WKA-DNPNO TO WKC-DNPNO.
           MOVE WKA-TORIK TO WKC-TORIK.
           MOVE WKA-TOKCD TO WKC-TOKCD.
           MOVE WKA-LINNO TO WKC-LINNO.
           MOVE WKA-SYOCD TO WKC-SYOCD.
           MOVE WKA-SURYO TO WKC-SURYO.
           MOVE WKA-TANKA TO WKC-TANKA.
           MOVE WKA-NTANK TO WKC-NTANK.
           MOVE WKA-HAYMD TO WKC-HAYMD.
           MOVE WKA-SYOME TO WKC-SYOME.
           MOVE WKA-HDNPN TO WKC-HDNPN.
           MOVE WKA-HDNPL TO WKC-HDNPL.
           MOVE WKA-HDNPD TO WKC-HDNPD.
           MOVE WKA-HDNPT TO WKC-HDNPT.
           MOVE WKA-NOSCD TO WKC-NOSCD.
           MOVE WKA-TSFKB TO WKC-TSFKB.
           MOVE WKA-THAKB TO WKC-THAKB.
           MOVE WKA-ADNPN TO WKC-ADNPN.
           MOVE WKA-ADNPL TO WKC-ADNPL.
           MOVE WKA-ADNPD TO WKC-ADNPD.
           MOVE WKA-ADNPT TO WKC-ADNPT.
           MOVE WKA-KETKB TO WKC-KETKB.
           MOVE WKA-URIDA TO WKC-URIDA.
           MOVE WKA-SHIMM TO WKC-SHIMM.
           MOVE WKA-SHZEI TO WKC-SHZEI.
           MOVE WKA-SHIMC TO WKC-SHIMC.
           MOVE WKA-ZEIRT TO WKC-ZEIRT.
           MOVE WKA-DSHZFG   TO WKC-DSHZFG.
           MOVE WKA-KGZKB TO WKC-KGZKB.
           WRITE WKC-REC FROM WKC-R.
       M2122-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : WKDFILE ONLY                                         *
      *----------------------------------------------------------------*
       M22-WKDONL.
           COMPUTE WTT1-KINGK = WKD-NBNMG + WKD-ZENBK.
           COMPUTE WTT2-KINGK = WKD-KURKG + WKD-NBNMG + WKD-ZENBK.
           IF ZERO = ( WKD-ZNSKY AND WKD-NYUKN AND WTT1-KINGK AND
                       WKD-TOHNB AND WKD-TOSKY AND WTT2-KINGK AND
                       WKD-MISKY AND WKD-TOURZ AND WKD-ZANIC  AND
                       WKD-TOZEI AND WKD-TOHNG )
              GO TO M22-L01.
           PERFORM M211-WRITE-WKE THRU M211-EXIT.
       M22-L01.
           PERFORM M11-READ-WKD THRU M11-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M23-WKAONL.
      *    IF WKA-BUKGO = '√'
      *       GO TO M23-L02.
           MOVE SPACE TO WKE-R.
           INITIALIZE WKE-R.
           MOVE WKA-SKYCD TO WKE-SKYCD.
           MOVE WKA-SNSCD TO WKE-SNSCD.
           WRITE WKE-REC FROM WKE-R.
       M23-L02.
           MOVE WIN2-KEY  TO WSV2-KEY.
      *    IF WKA-BUKGO = '√'
      *       PERFORM M2311-READ-TMS THRU M2311-EXIT.
           PERFORM M231-WKAONL-BREAK THRU M231-EXIT
                   UNTIL  WIN2-KEY NOT = WSV2-KEY.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1 : WKAFILE BREAK                                      *
      *----------------------------------------------------------------*
       M231-WKAONL-BREAK.
      *    IF WKA-BUKGO = '√'
      *       GO TO M231-L01.
           IF WKA-THAKB NOT = 'A'
              PERFORM M2121-WRITE-WKF THRU M2121-EXIT
           ELSE
              PERFORM M2122-WRITE-WKC THRU M2122-EXIT.
           GO TO M231-L02.
      *M231-L01.
      *    IF WFG1-TMS = '1'
      *       GO TO M231-L02.
      *    IF TMS-SEYOS NOT = 2
      *       GO TO M231-L02.
      *    IF TMS-TOKCD NOT = WSV3-TOKCD
      *       MOVE TMS-TOKCD TO WSV3-TOKCD
      *       MOVE 'A'       TO P05-HNBCD
      *       MOVE TMS-STNCD TO P05-STNNO
      *       MOVE TMS-CKUNO TO P05-KANO
      *       CALL 'VSBA0050' USING P05-PARM P04-PARM
      *       IF P05-RTNCD = ZERO
      *          IF P05-SOKCD = 'C'
      *             MOVE 'A'       TO WSV3-SOKCD
      *          ELSE
      *             MOVE P05-SOKCD TO WSV3-SOKCD
      *          END-IF
      *       ELSE
      *          MOVE 'A'          TO WSV3-SOKCD.
      *    IF WKA-THAKB NOT = 'A'
      *       MOVE WKA-R      TO WKG-R
      *       MOVE WSV3-SOKCD TO WKG-SSOKO
      *       MOVE TMS-CKUKG  TO WKG-SCKUK
      *       MOVE WSV3-SOKCD TO WKG-OSOKO
      *       WRITE WKG-REC FROM WKG-R
      *    ELSE
      *       MOVE WKA-R      TO WKB-R
      *       MOVE WSV3-SOKCD TO WKB-SSOKO
      *       MOVE TMS-CKUKG  TO WKB-SCKUK
      *       WRITE WKB-REC FROM WKB-R.
       M231-L02.
           PERFORM M12-READ-WKA THRU M12-EXIT.
       M231-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3.1.1  :  TMSFILE READ                                   *
      *----------------------------------------------------------------*
      *M2311-READ-TMS.
      *    MOVE '0'  TO WFG1-TMS.
      *    MOVE WKA-SKYCD TO KEY-TMS.
      *    MOVE 'Z '      TO S-CODE.
      *    READ TMSFILE INTO TMS-R
      *         INVALID KEY
      *                 MOVE SPACE TO TMS-R
      *                 MOVE '1' TO WFG1-TMS
      *                 GO TO M2311-EXIT.
      *    IF S-CODE NOT = ZERO
      *       DISPLAY 'R VBKA4150 TMSFILE READ ERROR !'
      *       UPON CONSOLE
      *       DISPLAY '  S-CODE = ' S-CODE
      *       UPON CONSOLE
      *       MOVE SPACE TO TMS-R.
      *M2311-EXIT.
      *    EXIT.
      *----------------------------------------------------------------*
      *    M3: FINAL                                                   *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE WKAFILE
                 WKBFILE
                 WKCFILE
                 WKDFILE
                 WKEFILE
                 WKFFILE
                 WKGFILE.
      *          TMSFILE.
       M3-EXIT.
           EXIT.
      *-------------------------------------------------------------*
      *      VBKA4150  END                                          *
      *-------------------------------------------------------------*
