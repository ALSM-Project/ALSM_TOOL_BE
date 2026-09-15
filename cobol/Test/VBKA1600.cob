./ ADD NAME=VBKA1600  01.25 20150805 20190716 101711 00647 00605 MCSK013
       ID  DIVISION.
       PROGRAM-ID.     'VBKA1600'.
      *----------------------------------------------------------------*
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10SYSTEM                 *
      *              <> SUBSYSTEM   :                                  *
      *              <> PG.OUTLINE  : ¾²·­³¼®ÃÞ-À É »¸¾²               *
      *                 * INPUT  FILE 1: Ä¸²»· Ï½À-          (TMSFILE) *
      *                 *        FILE 2: ²Á×Ý¾² ¾²·Õ³ Ò²»² ÃÞ-À (DWKA) *
      *                 * OUTPUT FILE 1: ¾²·­³¼® ÃÞ-À           (DWKB) *
      *                 *        FILE 2: ÄØÔÒ ÃÞ-À1             (DWKC) *
      *              <> AUTHOR      : SUZKEN-A.YAMAMOTO                *
      *              <> DATE        : 86/08/26                         *
      *-----------------  MAINTENANCE  HISTORY  -----------------------*
      * (01) WKA LAYOUT         ÍÝº³   : 86/11/13  CSK-T.KOZAWA        *
      * (02) TOKUISAKI MEI SET  ÍÝº³   : 86/12/18  CSK-T.HATANO        *
      * (03) SEIKYUSHO SIME YMD ÍÝº³   : 87/12/01  SUZ-H.NIIKURA.      *
      * (04) SYOHIZEI TUIKA     Â²¶    : 89/03/16  CSK-S.MUNEISHI      *
      * (05) KAKUJYU 105        ÍÝº³   : 89/04/26  CSK-A.ONITSUKA      *
      * (06) YOUKEN NO. K-263   ÍÝº³   : 90/11/01  SUZ-H.MIZUTANI      *
      * (07) YOUKEN NO. I-1906  ÍÝº³   : 92/02/12  SUZ-T.SEKIYA        *
      * (08) YOUKEN NO. K-122   ÍÝº³   : 95/06/06  CSK-H.MIYATA        *
      * (09) YOUKEN NO. K-261   ÍÝº³   : 97/10/03  CSK-S.WATANABE      *
      * (10) YOUKEN NO. K-275C  ÍÝº³   : 98/04/27  CSK-S.KOJIMA        *
      * (11) YOUKEN NO. K-407   ÍÝº³   : 99/12/17  CSK-C.FURUTA        *
      * (12) YOUKEN NO. K-484   ÍÝº³   : 01/07/09  CSK-K.KITAJIMA      *
      * (13) YOUKEN NO. K-568   ÍÝº³   : 03/07/24  CSK-C.YAMAGATA      *
      * (14) K-568  A-2199      ÍÝº³   : 03/09/29  CSK-Y.MORIO         *
      * (15) K-1758           ØºÝÊß²Ù  : 15/07/09  CSK-K.KOYAMA        *
      * (16) K-1780             ÍÝº³   : 15/08/05  CSK-Y.YOSHIGA       *
      * (17) K-02163            ÍÝº³   : 19/06/24  SCSK-A.ISIKAWA      *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TMSFILE ASSIGN    TO TMSFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE SEQUENTIAL
                          RECORD   KEY KEY-TMS
                          FILE  STATUS S-CODE.
           SELECT WKAFILE ASSIGN    TO S-DWKA.
           SELECT WKBFILE ASSIGN    TO S-DWKB.
           SELECT WKDFILE ASSIGN    TO S-DWKD.
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
           02 FILLER                        PIC  X(237).
       FD  WKBFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKB-REC.
           02 FILLER                        PIC  X(300).
       FD  WKDFILE BLOCK CONTAINS 0 RECORDS
           LABEL RECORD STANDARD.
       01  WKD-REC.
           02 FILLER                        PIC  X(30).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *    WORKING-STORAGE SECTION                                     *
      *----------------------------------------------------------------*
       77  S-CODE                           PIC  X(02).
       01  TMS-R.
           COPY VFILTMS SUPPRESS.
       01  P228-PARM.
           COPY VPARB228 SUPPRESS.
       01  PC0-PARM.
           COPY VPARBC0 SUPPRESS.
       01  P34-PARM.
           COPY VPARB34 SUPPRESS.
       01  P35-PARM.
           COPY VPARB35 SUPPRESS.
       01  P237-PARM.
           COPY VPARB237 SUPPRESS.
       01  P238-PARM.
           COPY VPARB238 SUPPRESS.
       01  PB2-PARM.
           COPY VPARBB2 SUPPRESS.
       01  WKA-R.
           02 WKA-SKYCD.
              03 WKA-SKCD1                  PIC  X(04).
              03 WKA-SKCD2                  PIC  X(03).
           02 WKA-SKNCD                     PIC  X(02).
           02 WKA-PGENO.
              03 WKA-PGKAM                  PIC  9(04).
              03 WKA-PGSIM                  PIC  9(04).
           02 WKA-LPDAT                     PIC  X(220).
           02 WKA-DATA50   REDEFINES  WKA-LPDAT.
              03 WKA-GYO50                  PIC  X(02).
              03 WKA-RKB50                  PIC  X(02).
              03 WKA-MEISI5.
                 04 WKA-ZENGT.
                    05 WKA-SEIKZ            PIC S9(13) COMP-3.
                    05 WKA-NYUKN            PIC S9(13) COMP-3.
                    05 WKA-SNEBK            PIC S9(09) COMP-3.
                    05 WKA-KRIKS            PIC S9(13) COMP-3.
                 04 WKA-TOUGT.
                    05 WKA-KAITK            PIC S9(13) COMP-3.
                    05 WKA-SEIKT            PIC S9(13) COMP-3.
                    05 WKA-MSEIK            PIC S9(13) COMP-3.
                    05 WKA-URIZN            PIC S9(13) COMP-3.
                 04 WKA-ZANKB               PIC  X(01).
                 04 WKA-SHZEI               PIC S9(13) COMP-3.
                 04 WKA-OKAIG               PIC S9(13) COMP-3.
                 04 WKA-SHZSK               PIC  X(01).
                 04 FILLER                  PIC  X(146).
           02 WKA-DATA60  REDEFINES  WKA-LPDAT.
              03 WKA-GYO60                  PIC  X(02).
              03 WKA-RKB60                  PIC  X(02).
              03 WKA-MEISI6.
                 04 WKA-DNYM1.
                    05 WKA-DNYY1            PIC  9(02).
                    05 WKA-DNMM1            PIC  9(02).
                 04 WKA-DNNO1               PIC  X(07).
                 04 WKA-TRIKA               PIC  X(08).
                 04 WKA-DENKN               PIC S9(09) COMP-3.
                 04 WKA-SHINME.
                    05 WKA-HINME            PIC  X(50).
                    05 WKA-KIYOU            PIC  X(40).
                 04 WKA-SURYO               PIC S9(05) COMP-3.
                 04 WKA-TANKA               PIC S9(08) COMP-3.
                 04 WKA-KINGK               PIC S9(09) COMP-3.
                 04 WKA-MESHZ               PIC S9(09) COMP-3.
                 04 WKA-SYOCD               PIC  X(09).
                 04 WKA-SYOMH.
                    05 WKA-SYOHM            PIC  X(25).
                    05 WKA-SYOMK            PIC  X(20).
                 04 WKA-BIKOU.
                    05 WKA-DNYM2.
                       06 WKA-DNYY2         PIC  9(02).
                       06 WKA-DNMM2         PIC  9(02).
                    05 WKA-DNNO2            PIC  X(07).
                    05 WKA-TOHKB            PIC  X(01).
                    05 WKA-BUNKB            PIC  X(01).
                 04 WKA-DSHZ1               PIC S9(09) COMP-3.
                 04 WKA-DSHZFG              PIC  X(01).
                 04 WKA-MSHZUM              PIC  X(01).
                 04 WKA-MSZRT               PIC S9(02)V9 COMP-3.
                 04 WKA-DPGK60              PIC S9(09)   COMP-3.
                 04 WKA-KGZH60              PIC  X(02).
                 04 FILLER                  PIC  X(01).
           02 WKA-DATA65  REDEFINES  WKA-LPDAT.
              03 WKA-GYO65                  PIC  X(02).
              03 WKA-RKB65                  PIC  X(02).
              03 WKA-MESI65.
                 04 WKA-TOKCD               PIC  X(07).
                 04 WKA-NOSCD               PIC  X(02).
                 04 WKA-SYOMA               PIC  X(50).
                 04 WKA-SYOMB               PIC  X(50).
                 04 WKA-TONGO               PIC S9(09) COMP-3.
                 04 WKA-TNSHZ               PIC S9(09) COMP-3.
                 04 WKA-CODEH               PIC  X(14).
                 04 WKA-TNGKK               PIC S9(09)  COMP-3.
                 04 FILLER                  PIC  X(78).
           02 WKA-DATA63-4 REDEFINES  WKA-LPDAT.
              03 WKA-GYO6X                  PIC  X(02).
              03 WKA-RKB6X                  PIC  X(02).
              03 WKA-MESI6X.
                 04 WKA-DNPYM-63-4          PIC  9(04).
                 04 WKA-MIDSA-63-4          PIC  X(50).
                 04 WKA-MIDSB-63-4          PIC  X(50).
                 04 WKA-DNPKG-63-4          PIC S9(11) COMP-3.
                 04 WKA-SHZEI-63-4          PIC S9(11) COMP-3.
                 04 WKA-DPGK6X              PIC S9(11)   COMP-3.
                 04 WKA-ZERT6X              PIC S9(02)V9 COMP-3.
                 04 WKA-KGZH6X              PIC  X(02).
                 04 FILLER                  PIC  X(90).
           02 WKA-DATA66  REDEFINES  WKA-LPDAT.
              03 WKA-GYO66                  PIC  X(02).
              03 WKA-RKB66                  PIC  X(02).
              03 WKA-MESI66.
                 04 WKA-MIDSAU6             PIC  X(50).
                 04 WKA-MIDSBD6             PIC  X(50).
                 04 WKA-ZOKDK               PIC S9(11)   COMP-3.
                 04 WKA-ZSHZI               PIC S9(11)   COMP-3.
                 04 WKA-ZOKGDK              PIC S9(11)   COMP-3.
                 04 WKA-ZEIRT6              PIC S9(02)V9 COMP-3.
                 04 WKA-KGZRT6              PIC  X(01).
                 04 FILLER                  PIC  X(95).

       01  WKB-R.
           02 WKB-CTLKEY.
              03 WKB-KAINO                  PIC  X(02).
              03 WKB-YTKBN                  PIC  X(01).
              03 WKB-YSYBN.
                 04 WKB-YSYB1               PIC  X(03).
                 04 WKB-YSYB2               PIC  X(04).
              03 WKB-YSBNO                  PIC  X(03).
              03 WKB-YSSTN                  PIC  X(03).
              03 WKB-YSCKG                  PIC  X(05).
              03 WKB-YUSCD                  PIC  X(07).
              03 WKB-YUNCD                  PIC  X(02).
              03 WKB-YSALL                  PIC  9(04).
              03 WKB-INFLG                  PIC  X(01).
              03 WKB-YSTPG                  PIC  9(04).
              03 WKB-PAGEN.
                 04 WKB-PAGE1               PIC  9(04).
                 04 WKB-PAGE2               PIC  9(04).
              03 WKB-SESCD                  PIC  X(07).
              03 WKB-SQNCD                  PIC  X(02).
           02 WKB-LPDAT                     PIC  X(219).
           02 WKB-DATA20   REDEFINES  WKB-LPDAT.
              03 WKB-GNO20                  PIC  9(02).
              03 WKB-RKB20                  PIC  X(02).
              03 WKB-MESAI2.
                 04 WKB-TMSJU               PIC  X(40).
                 04 WKB-TMSJL               PIC  X(40).
                 04 WKB-NOSMJ               PIC  X(40).
                 04 WKB-SQBNO               PIC  X(03).
                 04 WKB-SQSTN               PIC  X(03).
                 04 WKB-SQCKK               PIC  X(05).
                 04 WKB-SMEYMD.
                    05 WKB-SIMYY            PIC  9(04).
                    05 WKB-SIMMM            PIC  9(02).
                    05 WKB-SIMDD            PIC  9(02).
                 04 WKB-INKAN               PIC  X(01).
                 04 FILLER                  PIC  X(75).
           02 WKB-DATA50  REDEFINES  WKB-LPDAT.
              03 WKB-GNO50                  PIC  9(02).
              03 WKB-RKB50                  PIC  X(02).
              03 WKB-MEISA5.
                 04 WKB-ZENGT.
                    05 WKB-ZSKDK            PIC S9(13) COMP-3.
                    05 WKB-ZNKDK            PIC S9(13) COMP-3.
                    05 WKB-ZSYNB            PIC S9(09) COMP-3.
                    05 WKB-ZKRDK            PIC S9(13) COMP-3.
                 04 WKB-TOGTU.
                    05 WKB-TKIDK            PIC S9(13) COMP-3.
                    05 WKB-TSKDK            PIC S9(13) COMP-3.
                    05 WKB-MSKSK            PIC S9(13) COMP-3.
                    05 WKB-TURZN            PIC S9(13) COMP-3.
                 04 WKB-ZANKB               PIC  X(01).
                 04 WKB-SHZEI               PIC S9(13) COMP-3.
                 04 WKB-TKAIG               PIC S9(13) COMP-3.
                 04 WKB-SHZSK               PIC  X(01).
                 04 FILLER                  PIC  X(145).
           02 WKB-DATA60  REDEFINES  WKB-LPDAT.
              03 WKB-GNO60                  PIC  9(02).
              03 WKB-RKB60                  PIC  X(02).
              03 WKB-MEISA6.
                 04 WKB-DNHI1.
                    05 WKB-DNHMM            PIC  9(02).
                    05 WKB-DNHDD            PIC  9(02).
                 04 WKB-DNBN1               PIC  X(07).
                 04 WKB-TRKBN               PIC  X(08).
                 04 WKB-DNKIN               PIC S9(09) COMP-3.
                 04 WKB-SYMKJ.
                    05 WKB-SHMKJ            PIC  X(50).
                    05 WKB-SKYKJ            PIC  X(40).
                 04 WKB-SURYO               PIC S9(05) COMP-3.
                 04 WKB-TANKA               PIC S9(08) COMP-3.
                 04 WKB-KNGAK               PIC S9(09) COMP-3.
                 04 WKB-MESHZ               PIC S9(09) COMP-3.
                 04 WKB-SYOCD               PIC  X(09).
                 04 WKB-SYOME.
                    05 WKB-SHMKK            PIC  X(25).
                    05 WKB-SKYKK            PIC  X(20).
                 04 WKB-BIKOU.
                    05 WKB-DNHI2.
                       06 WKB-DNMM2         PIC  9(02).
                       06 WKB-DNDD2         PIC  9(02).
                    05 WKB-DNBN2            PIC  X(07).
                    05 WKB-THKBN            PIC  X(01).
                    05 WKB-BNKKB            PIC  X(01).
                 04 WKB-DSHZ3               PIC S9(09) COMP-3.
                 04 WKB-DSHZFG              PIC  X(01).
                 04 WKB-MSHZUM              PIC  X(01).
                 04 WKB-MSZRT               PIC S9(02)V9 COMP-3.
                 04 WKB-DPGK60              PIC S9(09)   COMP-3.
                 04 WKB-KGZH60              PIC  X(02).
           02 WKB-DATA6X  REDEFINES  WKB-LPDAT.
              03 WKB-GNO63                  PIC  9(02).
              03 WKB-RKB63                  PIC  X(02).
              03 WKB-MEISAX.
                 04 WKB-DNPH1               PIC  9(04).
                 04 WKB-MIDA1               PIC  X(50).
                 04 WKB-MIDA2               PIC  X(50).
                 04 WKB-DNPKG               PIC S9(11) COMP-3.
                 04 WKB-DSHZ4               PIC S9(11) COMP-3.
                 04 WKB-DPGK6X              PIC S9(11)   COMP-3.
                 04 WKB-ZERT6X              PIC S9(02)V9 COMP-3.
                 04 WKB-KGZH6X              PIC  X(02).
                 04 FILLER                  PIC  X(89).
           02 WKB-DATA65  REDEFINES  WKB-LPDAT.
              03 WKB-GNO65                  PIC  9(02).
              03 WKB-RKB65                  PIC  X(02).
              03 WKB-MESI65.
                 04 WKB-MIDAU               PIC  X(50).
                 04 WKB-MIDAD               PIC  X(50).
                 04 WKB-CDEDT               PIC  X(14).
                 04 WKB-GOKGK               PIC S9(09) COMP-3.
                 04 WKB-ZEGAK               PIC S9(09) COMP-3.
                 04 WKB-TOKCD               PIC  X(07).
                 04 WKB-NOSCD               PIC  X(02).
                 04 WKB-TNGKK               PIC S9(09)  COMP-3.
                 04 FILLER                  PIC  X(77).
           02 WKB-DAT66   REDEFINES  WKB-LPDAT.
              03 WKB-GNO66                  PIC  9(02).
              03 WKB-RKB66                  PIC  X(02).
              03 WKB-MESI66.
                 04 WKB-MIDAU6              PIC  X(50).
                 04 WKB-MIDAD6              PIC  X(50).
                 04 WKB-ZOKDK               PIC S9(11)   COMP-3.
                 04 WKB-ZSHZI               PIC S9(11)   COMP-3.
                 04 WKB-ZOKGDK              PIC S9(11)   COMP-3.
                 04 WKB-ZEIRT6              PIC S9(02)V9 COMP-3.
                 04 WKB-KGZRT6              PIC  X(01).
                 04 FILLER                  PIC  X(94).

           02 WKB-PGMSU                     PIC  X(02).
           02 WKB-SORTJ                     PIC  X(20).
           02 FILEER                        PIC  X(03).
       01  WKD-R.
           02 WKD-SKYCD                     PIC  X(07).
           02 WKD-SKNCD                     PIC  X(02).
           02 WKD-TRRYU                     OCCURS 15.
              03 WKD-TRIRU                  PIC  X(01).
           02 FILLER                        PIC  X(06).
       01  INPUT-AREA.
           02 WIN1-KEY.
              03 WIN1-TOKCD                 PIC  X(07).
           02 WIN2-KEY.
              03 WIN2-SKYCD                 PIC  X(07).
       01  SAVE-AREA.
           02 WSV2-KEY.
              03 WSV1-SKYCD                 PIC  X(07).
           02 WSV1-YMD.
              03 WSV1-YY                    PIC  9(04).
              03 WSV1-MM                    PIC  9(02).
              03 WSV1-DD                    PIC  9(02).
           02 WSV1-SWITCH.
              03 WSV1-SW1                   PIC  9(01).
              03 WSV1-SW2                   PIC  9(01).
              03 WSV1-SW3                   PIC  9(01).
              03 WSV1-SW4                   PIC  9(01).
       01  EDIT-AREA.
           02 WED1-TOKCD.
              03 WED1-TKCD1                 PIC  X(01).
              03 FILLER                     PIC  X(06).
           02 WED1-YMD.
              03 WED1-YY                    PIC  9(04).
              03 WED1-MM                    PIC  9(02).
              03 WED1-DD                    PIC  9(02).
       01  SYSIN-AREA.
           02 SYS-SIMEB                     PIC  X(02).
           02 FILLER                        PIC  X(78).
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT
                   UNTIL (WIN1-KEY = HIGH-VALUE OR
                          WIN2-KEY = HIGH-VALUE).
           PERFORM M3-FINL THRU M3-EXIT.
           STOP RUN.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  TMSFILE
                       WKAFILE
                OUTPUT WKBFILE
                       WKDFILE.
           CALL 'VSBA2280' USING P228-PARM.
           MOVE P228-PARM TO P237-KIJUN.
           CALL 'VSBA2370' USING P237-PARM.
           CALL 'VSBA0340' USING P34-PARM.
       M1-L01.
           PERFORM M11-SYSIN    THRU M11-EXIT.
       M1-L02.
           PERFORM M12-READ-TMS THRU M12-EXIT.
           PERFORM M13-READ-WKA THRU M13-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : SYSIN SHORI                                          *
      *----------------------------------------------------------------*
       M11-SYSIN.
           MOVE LOW-VALUE TO SYSIN-AREA.
           ACCEPT SYSIN-AREA.
           IF SYS-SIMEB = '20' OR '15' OR '31'
              NEXT SENTENCE
           ELSE
              DISPLAY 'R VBKA1600 SYSIN ERROR = ' SYS-SIMEB
              UPON CONSOLE
              PERFORM M3-FINL THRU M3-EXIT
              CALL 'VSBABEND'.
       M11-L01.
           IF SYS-SIMEB = '15'
              MOVE P228-YY   TO WSV1-YY
              MOVE P228-MM   TO WSV1-MM
              MOVE SYS-SIMEB TO WSV1-DD
              GO TO M11-EXIT.
       M11-L02.
           IF SYS-SIMEB = '31'
              PERFORM M111-SYSIN-MATU THRU M111-EXIT
           ELSE
              MOVE P228-YY TO WSV1-YY
              MOVE P228-MM TO WSV1-MM
              MOVE '20'   TO WSV1-DD.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1.1  :   SYSIN SHORI (SIMEB = '31')                      *
      *----------------------------------------------------------------*
       M111-SYSIN-MATU.
           MOVE P228-YY   TO WED1-YY.
           MOVE P228-MM   TO WED1-MM.
       M111-L01.
           IF P228-DD < 25
              COMPUTE WED1-MM = WED1-MM - 1
              IF WED1-MM = 0
                 MOVE 12          TO WED1-MM
                 COMPUTE WED1-YY = WED1-YY - 1.
           MOVE WED1-YMD  TO PC0-RYMDS8.
       M111-L02.
           CALL 'VSBA1200' USING PC0-PARM.
           MOVE PC0-RYMDS8 TO WSV1-YMD.
       M111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.2 : READ TMSFILE ( Ä¸²»· Ï½À- )                          *
      *----------------------------------------------------------------*
       M12-READ-TMS.
           MOVE 'Z '    TO   S-CODE.
           READ TMSFILE INTO TMS-R
                AT END
                   MOVE HIGH-VALUE TO WIN1-KEY
                   GO TO M12-EXIT.
           IF S-CODE NOT = ZERO
              DISPLAY 'M VBKA1600 TMSFILE DATA 0 KEN ?!'
              UPON CONSOLE
              DISPLAY '  S-CODE = ' S-CODE
              UPON CONSOLE
              MOVE HIGH-VALUE TO WIN1-KEY
              GO TO M12-EXIT.
           IF TMS-SEYOS NOT = ZERO
              GO TO M12-READ-TMS.
       M12-L01.
           MOVE TMS-TOKCD TO WIN1-TOKCD.
       M12-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.3 : READ WKAFILE ( ¾²·­³¼® Ò²»²ÃÞ-À )                    *
      *----------------------------------------------------------------*
       M13-READ-WKA.
           READ WKAFILE INTO WKA-R
                AT END
                   MOVE HIGH-VALUE TO WIN2-KEY
                   GO TO M13-EXIT.
       M13-L01.
           MOVE WKA-SKYCD TO WIN2-SKYCD.
       M13-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN        (MATCHING)                                 *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF WIN1-KEY = WIN2-KEY
              PERFORM M21-MATCH THRU M21-EXIT
           ELSE
              IF WIN1-KEY < WIN2-KEY
                 PERFORM M22-TMS-ONL THRU M22-EXIT
              ELSE
                 PERFORM M23-WKA-ONL THRU M23-EXIT.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : MATCH                                                *
      *----------------------------------------------------------------*
       M21-MATCH.
           MOVE WIN2-KEY TO WSV2-KEY.
           PERFORM M211-WKA-SELECT THRU M211-EXIT
                   UNTIL ( WIN2-KEY NOT = WSV2-KEY ).
           PERFORM M12-READ-TMS THRU M12-EXIT.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1 : ¾²·­³ Ö³¼·¸ÌÞÝ = ZERO É ÃÞ-À ¾Ú¸Ä                  *
      *----------------------------------------------------------------*
       M211-WKA-SELECT.
           PERFORM M2111-HENSHU-CTL THRU M2111-EXIT.
           IF WKA-RKB50 = '50'
              PERFORM M2112-HENSHU-501 THRU M2112-EXIT
              PERFORM M2113-HENSHU-502 THRU M2113-EXIT
              MOVE ZERO TO WSV1-SWITCH
              PERFORM M2114-YSKBN-CHECK THRU M2114-EXIT
              IF WSV1-SWITCH NOT = ZERO
                 PERFORM M2115-WKD-WRITE THRU M2115-EXIT
              END-IF
           ELSE
              PERFORM M2116-HENSHU-60 THRU M2116-EXIT
           END-IF.
           PERFORM M13-READ-WKA THRU M13-EXIT.
       M211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.1 : WKAFILE ( ºÝÄÛ°Ù ·° ) É HENSHU                   *
      *----------------------------------------------------------------*
       M2111-HENSHU-CTL.
           MOVE SPACE     TO WKB-R.
           INITIALIZE        WKB-R.
           MOVE '1'       TO PB2-HNBCD.
           MOVE TMS-STNCD TO PB2-BUNO.
           CALL 'VSBA1120' USING PB2-PARM.
           MOVE PB2-KAINO TO WKB-KAINO.
           MOVE '2'       TO P35-HNBCD.
           MOVE TMS-STNCD TO P35-STNNO.
           MOVE TMS-CKUNO TO P35-KANO.
           CALL 'VSBA0350' USING P35-PARM P34-PARM.
           IF P35-RTNCD = '0'
              MOVE P35-HSTNO TO WKB-YSBNO
           ELSE
              MOVE TMS-STNCD TO WKB-YSBNO
           END-IF.
           MOVE 'A'       TO P238-HNBCD.
           MOVE TMS-STNCD TO P238-STNNO.
           MOVE TMS-CKUNO TO P238-SYKNO.
           CALL 'VSBA2380' USING P238-PARM P237-PARM.
           MOVE P238-SOKNO TO WKB-YSSTN.
           MOVE TMS-CKUKG TO WKB-YSCKG.
           MOVE '2'       TO WKB-INFLG.
           MOVE WKA-SKYCD TO WKB-SESCD.
           MOVE WKA-SKNCD TO WKB-SQNCD.
           MOVE WKA-PGKAM TO WKB-PAGE1.
           MOVE WKA-PGSIM TO WKB-PAGE2.
           MOVE ZERO      TO WKB-PGMSU.
       M2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.2 : WKAFILE ( Úº-ÄÞ¸ÌÞÝ = 50 ) É HENSHU ( 1¹ÝÒ )     *
      *----------------------------------------------------------------*
       M2112-HENSHU-501.
           MOVE SPACE     TO WKB-DATA20.
           MOVE ZERO      TO WKB-GNO20.
           MOVE '20'      TO WKB-RKB20.
           MOVE WKB-YSBNO TO WKB-SQBNO.
           MOVE WKB-YSSTN TO WKB-SQSTN.
           MOVE TMS-CKUKG TO WKB-SQCKK.
           MOVE TMS-INKAN TO WKB-INKAN.
           MOVE WSV1-YMD  TO WKB-SMEYMD.
           WRITE WKB-REC FROM WKB-R.
       M2112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.3 : WKAFILE ( Úº-ÄÞ¸ÌÞÝ = 50 ) É HENSHU ( 2¹ÝÒ )     *
      *----------------------------------------------------------------*
       M2113-HENSHU-502.
           MOVE SPACE      TO WKB-DATA50.
           MOVE WKA-LPDAT  TO WKB-LPDAT.
           WRITE WKB-REC FROM WKB-R.
       M2113-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.4: Õ-¿³ÄØÔÒ ¸ÌÞÝ É HENSHU                            *
      *----------------------------------------------------------------*
       M2114-YSKBN-CHECK.
           IF TMS-YUTOS = 1
              MOVE 1 TO WSV1-SW1.
           MOVE TMS-TOKCD TO WED1-TKCD1.
           IF WED1-TKCD1 = '8'
              MOVE 1 TO WSV1-SW2.
           IF TMS-INKAN = '1'
              MOVE 1 TO WSV1-SW3.
           IF WKA-ZANKB = '*'
              MOVE 1 TO WSV1-SW4.
       M2114-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.5 : ÄØÔÒÃÞ-À É HENSHU-WRITE                          *
      *----------------------------------------------------------------*
       M2115-WKD-WRITE.
           MOVE SPACE     TO WKD-R.
           MOVE WKA-SKYCD TO WKD-SKYCD.
           MOVE WKA-SKNCD TO WKD-SKNCD.
           IF WSV1-SW1 = 1
              MOVE '*' TO WKD-TRIRU (1).
           IF WSV1-SW2 = 1
              MOVE '*' TO WKD-TRIRU (4).
           IF WSV1-SW3 = 1
              MOVE '*' TO WKD-TRIRU (5).
           IF WSV1-SW4 = 1
              MOVE '*' TO WKD-TRIRU (7).
           WRITE WKD-REC FROM WKD-R.
       M2115-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.6 : Úº-ÄÞ¸ÌÞÝ = '60' '65' '66' É HENSHU              *
      *----------------------------------------------------------------*
       M2116-HENSHU-60.
           MOVE SPACE      TO WKB-DATA60.
           IF WKA-RKB65 = '65'
              PERFORM M21161-REC65-HENSHU THRU M21161-EXIT
           ELSE
              MOVE WKA-LPDAT  TO  WKB-LPDAT
           END-IF.
           WRITE WKB-REC FROM WKB-R.
       M2116-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1.1.6.1 : Úº-ÄÞ¸ÌÞÝ = 65 É HENSHU-WRITE                  *
      *----------------------------------------------------------------*
       M21161-REC65-HENSHU.
           MOVE WKA-GYO65 TO WKB-GNO65.
           MOVE WKA-RKB65 TO WKB-RKB65.
           MOVE WKA-SYOMA TO WKB-MIDAU.
           MOVE WKA-SYOMB TO WKB-MIDAD.
           MOVE WKA-CODEH TO WKB-CDEDT.
           MOVE WKA-TONGO TO WKB-GOKGK.
           MOVE WKA-TNSHZ TO WKB-ZEGAK.
           MOVE WKA-TOKCD TO WKB-TOKCD.
           MOVE WKA-NOSCD TO WKB-NOSCD.
           MOVE WKA-TNGKK TO WKB-TNGKK.
       M21161-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.2 : TMSFILE ONLY ( Â·ÞÉ Ä¸²»·Ï½À- ¦ ÖÑ )                 *
      *----------------------------------------------------------------*
       M22-TMS-ONL.
           PERFORM M12-READ-TMS THRU M12-EXIT.
       M22-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.3 : WKAFILE ONLY ( Â·ÞÉ ¾²·­³¼® Ò²»²ÃÞ-À ¦ ÖÑ )          *
      *----------------------------------------------------------------*
       M23-WKA-ONL.
           PERFORM M13-READ-WKA THRU M13-EXIT.
       M23-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE TMSFILE
                 WKAFILE
                 WKBFILE
                 WKDFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    END    VBKA1600                                             *
      *----------------------------------------------------------------*
