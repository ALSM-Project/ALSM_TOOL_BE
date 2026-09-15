./ ADD NAME=VBKA1200  01.01 20150728 20190621 091953 00796 00792 MCSK012
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1200'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM         : SUZUKEN 10 SYSTEM             *
      *              <> SUBSYSTEM      : 20 SIME SEIKYU SHORI          *
      *              <> PG.OUTLINE     : SEIKYU TAISHO DATA NUKIDASHI  *
      *                                                                *
      *                 * PARM         : SHIMEBI '20' '31' '15'        *
      *                 * INPUT FILE   : TOKUISAKI MASTER (TMSFILE)    *
      *                 * INPUT FILE   : HANBAI SEIKYU HASSEI FILE     *
      *                                                   (DWKA)       *
      *                 * OUTPUT FILE  : SHIMEBI HENKOUSAKI TORIHIKI   *
      *                                  MEISAI DATA      (DWKB)       *
      *                 * OUTPUT FILE  : TORIHIKI MEISAI DATA          *
      *                                                   (DWKC)       *
      *                 * OUTPUT FILE  : SEIKYU TAISHOUGAI             *
      *                                  HANBAI SEIKYU HASSEI FILE     *
      *                                                   (HSHFILE)    *
      *                                                                *
      *              <> AUTHOR         : SUZUKEN-H.NIIKURA             *
      *              <> DATE           : 86/08/06                      *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) OUTPUT ¹Â/Ð¹Â ¸ÌÞÝ  Â²¶      : 86/09/13  SUZ-TO.YASUI     *
      * (02) ¼ÒËÞÍÝº³»· ¼¯Ø®¸ ¼Þ®³¹Ý ÍÝº³ : 86/12/25  CSK-T.HATANO     *
      * (03) TKSYM SAIKEISAN HENKOU       : 87/01/12  CSK-T.KOZAWA     *
      * (04) SYOHIZEI TUIKA               : 89/03/01  CSK-M.AOYAMA     *
      * (05) K-122                        : 95/06/01  CSK-H.MIYATA     *
      * (06) K-203                   ÍÝº³ : 96/10/25  CSK-K.ITO        *
      * (07) K-232                   ÍÝº³ : 97/03/05  CSK-H.TAKAHASHI  *
      * (08) K-275C                  ÍÝº³ : 98/04/27  CSK-R.TAKAYANAGI *
      * (09) H-010(O-3178)           ÍÝº³ : 01/02/07  CSK-K.INUKAI     *
      * (10) K-1549                  ÍÝº³ : 14/02/06  CSK-N.EGUCHI     *
      * (11) K-1780                  ÍÝº³ : 15/07/28  CSK-Y.YOSHIGA    *
      * (12) K-2163                  ÍÝº³ : 19/06/21  CSK-M.MATSUSHIMA *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TMSFILE ASSIGN    TO TMSFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-TMS
                          FILE  STATUS S-CODE.
           SELECT WKAFILE ASSIGN TO S-DWKA.
           SELECT WKBFILE ASSIGN TO S-DWKB.
           SELECT WKCFILE ASSIGN TO S-DWKC.
           SELECT HSHFILE ASSIGN TO S-HSHFILE.
       DATA DIVISION.
       FILE SECTION.
       FD  TMSFILE
           LABEL RECORD STANDARD.
       01  TMS-REC.
           02 KEY-TMS                       PIC  X(07).
           02 FILLER                        PIC  X(293).
       FD  WKAFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKA-REC.
           02 FILLER                        PIC  X(328).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(250).
       FD  WKCFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKC-REC.
           02 FILLER                        PIC  X(260).
       FD  HSHFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  HSH-REC.
           02 FILLER                        PIC  X(328).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  P234-PARM.
           COPY VPARB234 SUPPRESS.
       01  PC0-PARM.
           COPY VPARBC0 SUPPRESS.
       01  TMS-R.
           COPY VFILTMS SUPPRESS.
       01  WKA-R.
           02 WKA-SITEK                     PIC  X(01).
           02 WKA-SOKOK                     PIC  X(01).
           02 WKA-DNPNO.
              03 WKA-DNSYK                  PIC  X(01).
              03 WKA-DNREN                  PIC  9(06).
           02 WKA-TKSAI                     PIC  X(01).
           02 WKA-TORIK.
              03 WKA-TURIK                  PIC  X(01).
              03 WKA-THAIK                  PIC  X(01).
           02 WKA-CKUKG.
              03 WKA-CKUKA                  PIC  X(03).
              03 WKA-CKUCK                  PIC  X(02).
           02 WKA-CKUR1 REDEFINES WKA-CKUKG.
              03 WKA-CKR11                  PIC  X(04).
              03 FILLER                     PIC  X(01).
           02 WKA-CKUR2 REDEFINES WKA-CKUKG.
              03 FILLER                     PIC  X(03).
              03 WKA-CKR22                  PIC  X(02).
           02 WKA-TOKCD.
              03 WKA-TKCD1                  PIC  X(04).
              03 WKA-TKCD2                  PIC  X(03).
           02 WKA-HAYMD.
              03 WKA-HAYY                   PIC  9(04).
              03 WKA-HAMM                   PIC  9(02).
              03 WKA-HADD                   PIC  9(02).
           02 WKA-SZSYM.
              03 WKA-SZYY                   PIC  9(04).
              03 WKA-SZMM                   PIC  9(02).
           02 WKA-TKSYM.
              03 WKA-TKYY                   PIC  9(04).
              03 WKA-TKMM                   PIC  9(02).
           02 WKA-HNPNK                     PIC  X(01).
           02 WKA-LINNO                     PIC  9(01).
           02 WKA-SYOCD.
              03 WKA-SYCD1                  PIC  X(03).
              03 WKA-SYCD2                  PIC  X(06).
           02 WKA-SURYO                     PIC S9(05) COMP-3.
           02 WKA-TANKA                     PIC S9(08) COMP-3.
           02 WKA-GENKA                     PIC S9(08) COMP-3.
           02 WKA-FSZFG                     PIC  X(01).
           02 WKA-LDNPN                     PIC  X(07).
           02 WKA-LDNPL                     PIC  9(01).
           02 WKA-LDNPD.
              03 WKA-LDNYY                  PIC  9(04).
              03 WKA-LDNMM                  PIC  9(02).
              03 WKA-LDNDD                  PIC  9(02).
           02 WKA-LDNPT                     PIC  X(02).
           02 WKA-SNEBK                     PIC S9(08)   COMP-3.
           02 WKA-ARRIT                     PIC S9(02)V9 COMP-3.
           02 WKA-MKHAN                     PIC  X(01).
           02 WKA-NTANK                     PIC S9(08)   COMP-3.
           02 WKA-HDNPN                     PIC  X(07).
           02 WKA-HDNPL                     PIC  9(01).
           02 WKA-HDNPD.
              03 WKA-HDNYY                  PIC  9(04).
              03 WKA-HDNMM                  PIC  9(02).
              03 WKA-HDNDD                  PIC  9(02).
           02 WKA-HDNPT                     PIC  X(02).
           02 WKA-ZDNPN                     PIC  X(07).
           02 WKA-ZDNPL                     PIC  9(01).
           02 WKA-ZDNPD                     PIC  9(08).
           02 WKA-ZDNPT                     PIC  X(02).
           02 WKA-KETKB                     PIC  X(01).
           02 WKA-URIDA                     PIC  9(08).
           02 WKA-ZHYOS                     PIC  X(01).
           02 WKA-HKHYO                     PIC  X(01).
           02 WKA-GIGYD.
              03 WKA-GIYY                   PIC  9(04).
              03 WKA-GIMM                   PIC  9(02).
              03 WKA-GIDD                   PIC  9(02).
           02 WKA-TOKME                     PIC  X(25).
           02 WKA-HANKB                     PIC  X(01).
           02 WKA-SBMKG                     PIC  X(02).
           02 WKA-SYOME.
              03 WKA-SYOMH                  PIC  X(25).
              03 WKA-SYOMK                  PIC  X(10).
              03 WKA-SYOMY                  PIC  X(10).
           02 WKA-GOJUJ                     PIC  X(06).
           02 WKA-SHIMC                     PIC  9(04).
           02 WKA-SHIMM                     PIC  X(08).
           02 WKA-HJYOT                     PIC  X(01).
           02 WKA-JMKKB                     PIC  X(01).
           02 WKA-MKSDD                     PIC  9(02).
           02 WKA-GIGYK                     PIC  X(01).
           02 WKA-MAKCD                     PIC  9(04).
           02 WKA-SHICD                     PIC  9(04).
           02 WKA-TSIME                     PIC  9(02).
           02 WKA-SEKMM                     PIC  9(02).
           02 WKA-KEIYK                     PIC  X(01).
           02 WKA-MIKKB                     PIC  X(01).
           02 WKA-TSYCD.
              03 WKA-TSCD1                  PIC  X(03).
              03 WKA-TSCD2                  PIC  X(06).
           02 WKA-LEASE                     PIC  X(01).
           02 WKA-SYOKB                     PIC  X(01).
           02 WKA-NSKBN                     PIC  X(02).
           02 WKA-TSFKB                     PIC  X(01).
           02 WKA-THKBN                     PIC  X(01).
           02 WKA-ADNPN                     PIC  X(07).
           02 WKA-ADNPL                     PIC  9(01).
           02 WKA-ADNPD.
              03 WKA-ADNYY                  PIC  9(04).
              03 WKA-ADNMM                  PIC  9(02).
              03 WKA-ADNDD                  PIC  9(02).
           02 WKA-ADNPT                     PIC  X(02).
           02 WKA-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKA-SHZEI                     PIC S9(09)   COMP-3.
           02 FILLER                        PIC  X(08).
           02 WKA-SHZSK                     PIC  X(01).
           02 FILLER                        PIC  X(04).
           02 WKA-ZNGHK                     PIC  X(01).
           02 FILLER                        PIC  X(09).
       01  WKC-R.
           02 WKC-SITEK                     PIC  X(01).
           02 WKC-SOKOK                     PIC  X(01).
           02 WKC-DNPNO                     PIC  X(07).
           02 WKC-TORIK.
              03 WKC-TURIK                  PIC  X(01).
              03 WKC-THAIK                  PIC  X(01).
           02 WKC-CKUKG.
              03 WKC-CKUKA                  PIC  X(03).
              03 WKC-CKUCK                  PIC  X(02).
           02 WKC-TOKCD.
              03 WKC-TKCD1                  PIC  X(04).
              03 WKC-TKCD2                  PIC  X(03).
           02 WKC-SSOKO                     PIC  X(01).
           02 WKC-SCKUK                     PIC  X(05).
           02 WKC-SKYCD                     PIC  X(07).
           02 WKC-SNSCD                     PIC  X(02).
           02 WKC-HAYMD.
              03 WKC-HAYY                   PIC  9(04).
              03 WKC-HAMM                   PIC  9(02).
              03 WKC-HADD                   PIC  9(02).
           02 WKC-LINNO                     PIC  9(01).
           02 WKC-SYOCD                     PIC  X(09).
           02 WKC-SURYO                     PIC S9(05) COMP-3.
           02 WKC-TANKA                     PIC S9(08) COMP-3.
           02 WKC-NTANK                     PIC S9(08) COMP-3.
           02 WKC-HDNPN                     PIC  X(07).
           02 WKC-HDNPL                     PIC  9(01).
           02 WKC-HDNPD.
              03 WKC-HDNYY                  PIC  9(04).
              03 WKC-HDNMM                  PIC  9(02).
              03 WKC-HDNDD                  PIC  9(02).
           02 WKC-HDNPT                     PIC  X(02).
           02 WKC-URIDA                     PIC  9(08).
           02 WKC-ZHYOS                     PIC  X(01).
           02 WKC-HKHYO                     PIC  X(01).
           02 WKC-GIGYD.
              03 WKC-GIYY                   PIC  9(04).
              03 WKC-GIMM                   PIC  9(02).
              03 WKC-GIDD                   PIC  9(02).
           02 WKC-SBMKG                     PIC  X(02).
           02 WKC-SYOME.
              03 WKC-SYOMH                  PIC  X(25).
              03 WKC-SYOMK                  PIC  X(10).
              03 WKC-SYOMY                  PIC  X(10).
           02 WKC-SHIMC                     PIC  9(04).
           02 WKC-SHIMM                     PIC  X(08).
           02 WKC-TSIME                     PIC  9(02).
           02 WKC-SEKMM                     PIC  9(02).
           02 WKC-TSYCD                     PIC  X(09).
           02 WKC-NOSCD                     PIC  X(02).
           02 WKC-TSFKB                     PIC  X(01).
           02 WKC-THAKB                     PIC  X(01).
           02 WKC-ADNPN                     PIC  X(07).
           02 WKC-ADNPL                     PIC  9(01).
           02 WKC-ADNPD                     PIC  9(08).
           02 WKC-ADNPT                     PIC  X(02).
           02 WKC-TOKME                     PIC  X(25).
           02 WKC-TKSYM                     PIC  9(06).
           02 WKC-OSOKO                     PIC  X(01).
           02 WKC-ZHAKB                     PIC  X(01).
           02 WKC-SSIMD                     PIC  9(08).
           02 WKC-KETKB                     PIC  X(01).
           02 WKC-SHZEI                     PIC S9(11) COMP-3.
           02 WKC-ZEIRT                     PIC S9(02)V9 COMP-3.
           02 WKC-DSHZFG                    PIC  X(01).
           02 WKC-KGZKB                     PIC  X(01).
           02 FILLER                        PIC  X(09).
       01  HSH-R.
           COPY VFILHSH SUPPRESS.
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-KEY1.
                 04 WIN1-TOKCD              PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-KEY1.
                 04 WIN2-TOKCD              PIC  X(07).
       01  SAVE-AREA.
           02 WSV2-KEY1.
              03 WSV2-TOKCD.
                 04 WSV2-TOKCD1             PIC  X(01).
                 04 FILLER                  PIC  X(06).
       01  WORK-AREA.
           02 WOK-1                         PIC  X(02).
       01  GET-AREA.
           02 GET-YMD.
              03 GET-2.
                 04 GET-21                  PIC  9(04).
                 04 GET-22                  PIC  9(02).
              03 GET-1                      PIC  9(02).
           02 GET-3.
              03 FILLER                     PIC  9(06).
       01  WK-SYSIN.
           02 WK-TSIME                      PIC  9(02).
           02 FILLER                        PIC  X(78).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-PCHK THRU M1-EXIT.
           PERFORM M2-INIT THRU M2-EXIT.
           PERFORM M3-MAIN THRU M3-EXIT
                   UNTIL ( WIN2-KEY = HIGH-VALUE ).
           PERFORM M4-FINL THRU M4-EXIT.
      *
       M0-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : PARM CHECK                                             *
      *----------------------------------------------------------------*
       M1-PCHK.
           CALL 'VSBA2280' USING P228-PARM.
           PERFORM M11-PCHK THRU M11-EXIT.
      *
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M11 : PARAMETER CHECK                                       *
      *----------------------------------------------------------------*
       M11-PCHK.
           MOVE LOW-VALUE TO WK-SYSIN.
           ACCEPT WK-SYSIN.
           IF WK-SYSIN = LOW-VALUE
              CALL 'VSBABEND'.
           IF WK-TSIME = 20
              PERFORM M111-GET-DYM THRU M111-EXIT
           ELSE
              IF WK-TSIME = 31
                 PERFORM M112-GET-DYM THRU M112-EXIT
              ELSE
                 IF WK-TSIME = 15
                    PERFORM M113-GET-DYM THRU M113-EXIT
                 ELSE
                    CALL 'VSBABEND'.
      *
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M111 : SEIKYU SHIMEBI GET WHEN '20'                        *
      *----------------------------------------------------------------*
       M111-GET-DYM.
           MOVE 20     TO GET-1.
           MOVE P228-YY TO GET-21.
           MOVE P228-MM TO GET-22.
      *
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M112 : SEIKYU SHIMEBI GET WHEN '31'                        *
      *----------------------------------------------------------------*
       M112-GET-DYM.
           IF P228-DD < 25
              PERFORM M1121-GET-DYM THRU M1121-EXIT
           ELSE
              PERFORM M1122-GET-DYM THRU M1122-EXIT.
      *
       M112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1121 : SEIKYU SHIMEBI GET WHEN '31' SYSTEM-DD < 25         *
      *----------------------------------------------------------------*
       M1121-GET-DYM.
           COMPUTE P228-MM = P228-MM - 1.
           IF ( P228-MM = ZERO )
                MOVE 12 TO P228-MM
                COMPUTE P228-YY = P228-YY - 1.
           MOVE P228-DATE TO PC0-YMDS8.
           CALL 'VSBA1200' USING PC0-PARM.
           MOVE PC0-YMDS8 TO GET-YMD.
      *
       M1121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1122 : SEIKYU SHIMEBI GET WHEN '31' SYSTEM-DD > 25 = 25    *
      *----------------------------------------------------------------*
       M1122-GET-DYM.
           MOVE P228-DATE TO PC0-YMDS8.
           CALL 'VSBA1200' USING PC0-PARM.
           MOVE PC0-YMDS8 TO GET-YMD.
      *
       M1122-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M113 : SEIKYU SHIMEBI GET WHEN '15'                         *
      *----------------------------------------------------------------*
       M113-GET-DYM.
           MOVE 15     TO GET-1.
           MOVE P228-YY TO GET-21.
           MOVE P228-MM TO GET-22.
      *
       M113-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : INITIAL                                                *
      *----------------------------------------------------------------*
       M2-INIT.
           OPEN INPUT  TMSFILE
                       WKAFILE
                OUTPUT WKBFILE
                       WKCFILE
                       HSHFILE.
           PERFORM M21-READ-TMS THRU M21-EXIT.
           PERFORM M22-READ-WKA THRU M22-EXIT.
      *
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : READ TMSFILE                                         *
      *----------------------------------------------------------------*
       M21-READ-TMS.
           MOVE 'Z '    TO   S-CODE.
           READ TMSFILE INTO TMS-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M21-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'M VBKA1200 TMSFILE DATA 0 KEN ?!'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE HIGH-VALUE TO WIN1-KEY
              GO TO M21-EXIT.
           MOVE TMS-TOKCD TO WIN1-TOKCD.
      *
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : READ WKAFILE                                         *
      *----------------------------------------------------------------*
       M22-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M22-EXIT.
           MOVE WKA-TOKCD TO WIN2-TOKCD.
      *
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : MAIN        (MATCHING)                                 *
      *----------------------------------------------------------------*
       M3-MAIN.
           IF WIN1-KEY1 = WIN2-KEY1
              PERFORM M31-MATCH THRU M31-EXIT
           ELSE
              IF WIN1-KEY1 < WIN2-KEY1
                 PERFORM M32-TMSONL THRU M32-EXIT
              ELSE
                 PERFORM M33-WKAONL THRU M33-EXIT.
      *
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M31-MATCH.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M311-SELECT  THRU M311-EXIT
                   UNTIL WIN2-KEY1 NOT = WSV2-KEY1.
           PERFORM M21-READ-TMS THRU M21-EXIT.
      *
       M31-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1 : SEIKYU TAISHO DATA SHORI SELECTION                 *
      *----------------------------------------------------------------*
       M311-SELECT.
           IF ( WK-TSIME = TMS-SIMEB ) OR
              ( ( WK-TSIME = 20 ) AND
                ( NOT ( TMS-SIMEB = 15 OR 31 ) ) )
              PERFORM M3111-OUT THRU M3111-EXIT
           ELSE
              PERFORM M3112-OUT THRU M3112-EXIT.
           PERFORM M22-READ-WKA THRU M22-EXIT.
      *
       M311-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1 : SEIKYU TAISHO DATA SAKUSEI                       *
      *----------------------------------------------------------------*
       M3111-OUT.
           IF ( NOT ( TMS-LSIMEB = SPACES ) )
              PERFORM M31111-YM  THRU M31111-EXIT
           ELSE
              MOVE WKA-TKSYM TO GET-3.
           IF ( NOT ( GET-3 > GET-2 ) )
              PERFORM M31112-WKC THRU M31112-EXIT
           ELSE
              PERFORM M31113-HSH THRU M31113-EXIT.
           PERFORM M31114-WKB THRU M31114-EXIT.
      *
       M3111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.1 : TOKUISAKI SIMESEIKYU YM SAIKEISAN              *
      *----------------------------------------------------------------*
       M31111-YM.
           IF GET-2 NOT < WKA-TKSYM
              MOVE GET-2 TO GET-3
              GO TO M31111-EXIT.
           MOVE TMS-SIMEB TO P234-SIMEB.
           MOVE WKA-SEKMM TO P234-SEIMM.
           MOVE WKA-HAYMD TO P234-HYMD.
           MOVE WKA-GIGYD TO P234-GYOMD.
           MOVE WKA-TORIK TO P234-TORKB.
           CALL 'VSBA2340' USING P234-PARM.
           MOVE P234-SEIYM TO GET-3.
      *
       M31111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.2 : WKC                                            *
      *----------------------------------------------------------------*
       M31112-WKC.
           PERFORM M311121-WKC-HEN THRU M311121-EXIT.
           PERFORM M311122-WKC-OUT THRU M311122-EXIT.
      *
       M31112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.2.1 : WKC HENSHU                                   *
      *----------------------------------------------------------------*
       M311121-WKC-HEN.
           PERFORM M3111211-WKC-HEN THRU M3111211-EXIT.
           PERFORM M3111212-WKC-HEN THRU M3111212-EXIT.
           PERFORM M3111213-WKC-HEN THRU M3111213-EXIT.
           PERFORM M3111214-WKC-HEN THRU M3111214-EXIT.
           PERFORM M3111215-WKC-HEN THRU M3111215-EXIT.
      *
       M311121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.2.1.1 : WKC HENSHU-1                               *
      *----------------------------------------------------------------*
       M3111211-WKC-HEN.
           MOVE SPACES    TO WKC-R.
           MOVE WKA-SITEK TO WKC-SITEK.
           MOVE WKA-SOKOK TO WKC-SOKOK.
           MOVE WKA-DNPNO TO WKC-DNPNO.
           MOVE WKA-TORIK TO WKC-TORIK.
           MOVE WKA-CKUKG TO WKC-CKUKG.
           MOVE WKA-TOKCD TO WKC-TOKCD.
           MOVE SPACE     TO WKC-SSOKO.
           MOVE SPACES    TO WKC-SCKUK.
           MOVE TMS-SKUCD TO WKC-SKYCD.
      *
       M3111211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.2.1.2 : WKC HENSHU-2                               *
      *----------------------------------------------------------------*
       M3111212-WKC-HEN.
           PERFORM M31112121-CHU1 THRU M31112121-EXIT.
           MOVE WOK-1 TO WKC-SNSCD.
      *
       M3111212-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.2.1.2.1 : WKC OUTPUT CHU-1 SET                     *
      *----------------------------------------------------------------*
       M31112121-CHU1.
           IF ( TMS-SEBUS = '1' ) AND
              ( WKA-NSKBN = SPACE )
              GO TO M31112121-L01.
           IF ( TMS-SEBUS = ZERO ) AND
              ( WKA-NSKBN NOT = SPACE )
              GO TO M31112121-L02.
           MOVE WKA-NSKBN TO WOK-1.
           GO TO M31112121-EXIT.
       M31112121-L01.
           IF WSV2-TOKCD1 = '8'
              MOVE '00' TO WOK-1
           ELSE
              MOVE '01' TO WOK-1.
           GO TO M31112121-EXIT.
       M31112121-L02.
           MOVE SPACES TO WOK-1.
      *
       M31112121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.2.3 : WKC HENSHU-3                                 *
      *----------------------------------------------------------------*
       M3111213-WKC-HEN.
           MOVE WKA-HAYMD TO WKC-HAYMD.
           MOVE WKA-LINNO TO WKC-LINNO.
           MOVE WKA-SYOCD TO WKC-SYOCD.
           MOVE WKA-SURYO TO WKC-SURYO.
           MOVE WKA-TANKA TO WKC-TANKA.
           MOVE WKA-NTANK TO WKC-NTANK.
           MOVE WKA-HDNPN TO WKC-HDNPN.
           MOVE WKA-HDNPL TO WKC-HDNPL.
           MOVE WKA-HDNPD TO WKC-HDNPD.
           MOVE WKA-HDNPT TO WKC-HDNPT.
           MOVE WKA-URIDA TO WKC-URIDA.
           MOVE WKA-ZHYOS TO WKC-ZHYOS.
           MOVE WKA-HKHYO TO WKC-HKHYO.
           MOVE WKA-GIGYD TO WKC-GIGYD.
           MOVE WKA-SBMKG TO WKC-SBMKG.
           MOVE WKA-SYOME TO WKC-SYOME.
           MOVE WKA-SHIMC TO WKC-SHIMC.
           MOVE WKA-SHIMM TO WKC-SHIMM.
           MOVE TMS-SIMEB TO WKC-TSIME.
           MOVE WKA-SEKMM TO WKC-SEKMM.
           MOVE WKA-TSYCD TO WKC-TSYCD.
      *
       M3111213-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.2.4 : WKC HENSHU-4                                 *
      *----------------------------------------------------------------*
       M3111214-WKC-HEN.
           PERFORM M31112141-CHU2 THRU M31112141-EXIT.
           MOVE WOK-1 TO WKC-NOSCD.
      *
       M3111214-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.2.1.4.1 : WKC OUTPUT CHU-2 SET                     *
      *----------------------------------------------------------------*
       M31112141-CHU2.
           IF ( TMS-NOSEK = '1' ) AND
              ( WKA-NSKBN = SPACE )
              GO TO M31112141-L01.
           MOVE WKA-NSKBN TO WOK-1.
           GO TO M31112141-EXIT.
       M31112141-L01.
           IF WSV2-TOKCD1 = '8'
              MOVE '00' TO WOK-1
           ELSE
              MOVE '01' TO WOK-1.
      *
       M31112141-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.2.5 : WKC HENSHU-5                                 *
      *----------------------------------------------------------------*
       M3111215-WKC-HEN.
           MOVE WKA-TSFKB TO WKC-TSFKB.
           MOVE WKA-THKBN TO WKC-THAKB.
           MOVE WKA-ADNPN TO WKC-ADNPN.
           MOVE WKA-ADNPL TO WKC-ADNPL.
           MOVE WKA-ADNPD TO WKC-ADNPD.
           MOVE WKA-ADNPT TO WKC-ADNPT.
           MOVE WKA-TOKME TO WKC-TOKME.
           MOVE GET-3     TO WKC-TKSYM.
           MOVE SPACE     TO WKC-OSOKO.
           MOVE WKA-ZNGHK TO WKC-ZHAKB.
           MOVE GET-YMD   TO WKC-SSIMD.
           MOVE WKA-KETKB TO WKC-KETKB.
           MOVE WKA-SHZEI TO WKC-SHZEI.
           MOVE WKA-ZEIRT TO WKC-ZEIRT.
           IF WKA-SHZSK = '2'
              MOVE '1'    TO WKC-DSHZFG
           ELSE
              MOVE SPACE  TO WKC-DSHZFG.
           MOVE WKA-KEIYK TO WKC-KGZKB.
      *
       M3111215-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.2.2 : WKC OUTPUT                                   *
      *----------------------------------------------------------------*
       M311122-WKC-OUT.
           WRITE WKC-REC FROM WKC-R.
      *
       M311122-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.3 : HSH OUTPUT                                     *
      *----------------------------------------------------------------*
       M31113-HSH.
           MOVE WKA-R   TO HSH-R.
           MOVE '1'     TO HSH-ZNGHK.
           WRITE HSH-REC FROM HSH-R.
      *
       M31113-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.4 : WKB OUTPUT DECI                                *
      *----------------------------------------------------------------*
       M31114-WKB.
           IF TMS-LSIMEB NOT = SPACE  AND
              GET-YMD    NOT < WKA-GIGYD
              PERFORM M311141-WKB-HEN THRU M311141-EXIT
              PERFORM M311142-WKB-OUT THRU M311142-EXIT.
      *
       M31114-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.4.1 : WKB HENSHU                                   *
      *----------------------------------------------------------------*
       M311141-WKB-HEN.
           PERFORM M3111411-WKB-HEN THRU M3111411-EXIT.
           PERFORM M3111412-WKB-HEN THRU M3111412-EXIT.
           PERFORM M3111413-WKB-HEN THRU M3111413-EXIT.
           PERFORM M3111414-WKB-HEN THRU M3111414-EXIT.
           PERFORM M3111415-WKB-HEN THRU M3111415-EXIT.
      *
       M311141-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.4.1.1 : WKB HENSHU-1                                 *
      *----------------------------------------------------------------*
       M3111411-WKB-HEN.
           PERFORM M3111211-WKC-HEN THRU M3111211-EXIT.
      *
       M3111411-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.4.1.2 : WKB HENSHU-2                                 *
      *----------------------------------------------------------------*
       M3111412-WKB-HEN.
           PERFORM M3111212-WKC-HEN THRU M3111212-EXIT.
      *
       M3111412-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.4.1.3 : WKB HENSHU-3                                 *
      *----------------------------------------------------------------*
       M3111413-WKB-HEN.
           PERFORM M3111213-WKC-HEN THRU M3111213-EXIT.
      *
       M3111413-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.4.1.4 : WKB HENSHU-4                                 *
      *----------------------------------------------------------------*
       M3111414-WKB-HEN.
           PERFORM M31112121-CHU1 THRU M31112121-EXIT.
           MOVE WOK-1 TO WKC-NOSCD.
      *
       M3111414-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.4.1.5 : WKB HENSHU-5                                 *
      *----------------------------------------------------------------*
       M3111415-WKB-HEN.
           PERFORM M3111215-WKC-HEN THRU M3111215-EXIT.
      *
       M3111415-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.1.4.2 : WKBFILE WRITE                                *
      *----------------------------------------------------------------*
       M311142-WKB-OUT.
           WRITE WKB-REC FROM WKC-R.
      *
       M311142-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.1.1.2 : SEIKYU TAISHO GAI DATA SAKUSEI                   *
      *----------------------------------------------------------------*
       M3112-OUT.
           MOVE WKA-R   TO HSH-R.
           WRITE HSH-REC FROM HSH-R.
      *
       M3112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.2 : TMSFILE ONLY                                         *
      *----------------------------------------------------------------*
       M32-TMSONL.
           PERFORM M21-READ-TMS THRU M21-EXIT.
      *
       M32-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.3 : WKAFILE ONLY                                         *
      *----------------------------------------------------------------*
       M33-WKAONL.
           MOVE WIN2-KEY1 TO WSV2-KEY1.
           PERFORM M331-WKA THRU M331-EXIT
                   UNTIL ( WIN2-KEY1 NOT = WSV2-KEY1 ).
      *
       M33-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3.3.1 : WKAFILE ONLY OUTPUT SEIKYU TAISHOGAI HASSEI FILE   *
      *----------------------------------------------------------------*
       M331-WKA.
           MOVE WKA-R   TO HSH-R.
           WRITE HSH-REC FROM HSH-R.
           PERFORM M22-READ-WKA THRU M22-EXIT.
      *
       M331-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M4 : FINAL                                                  *
      *----------------------------------------------------------------*
       M4-FINL.
           CLOSE TMSFILE
                 WKAFILE
                 WKBFILE
                 WKCFILE
                 HSHFILE.
      *
       M4-EXIT.
           EXIT.
