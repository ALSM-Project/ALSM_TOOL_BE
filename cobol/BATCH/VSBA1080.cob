./ ADD NAME=VSBA1080  01.01 20200907 20200907 101738 00779 00779 MCSK008
       ID  DIVISION.
       PROGRAM-ID.      'VSBA1080'.
      *----------------------------------------------------------------*
      *  SUMMARY  <> SYSTEM      : SUZUKEN 10 SYSTEM                   *
      *           <> SUBSYSTEM   : SUBROUTINE                          *
      *           <> PG.OUTLINE  : SITEN SYUCHOJO JYUSHO MASTER        *
      *                                         SYUUTOKU               *
      *           *  INPUT FILE  : SITEN SYUCHOJO JYUSHO MASTER        *
      *                                            (SJMFILE)           *
      *           *  INPUT.PARM  : PA8-STNCD                           *
      *                          : PA8-BOTCD                           *
      *                          : PA8-SYKNO                           *
      *           *  OUTPUT.PARM : PA8-STNME , PA8-YUBNO ,             *
      *                            PA8-TELNO , PA8-ADRES ,             *
      *                            PA8-BANK1 , PA8-BANK2 ,             *
      *                            PA8-BANK3                           *
      *           <> AUTHOR      : CSK-K.KOYAMA                        *
      *           <> DATE        : 91/08/29                            *
      *------------------  MAINTENANCE   HISTORY  ---------------------*
      * (01) YOUKEN I-1852      Õ›∫≥   : 91/10/04  SUZ-N.MARUYAMA      *
      * (02) YOUKEN NO.K-589    Õ›∫≥   : 92/08/26  CSK-K.OHITA         *
      * (03) YOUKEN NO.H-020    Õ›∫≥   : 94/06/06  CSK-S.SUGIURA       *
      * (04) H-020  O-1592      Õ›∫≥   : 94/10/07  CSK-M.KATOH         *
      * (05) H-020  O-0003      Õ›∫≥   : 95/02/06  CSK-A.MII           *
      * (06) Z-103              Õ›∫≥   : 95/06/20  CSK-H.MIYATA        *
      * (07) K-138              Õ›∫≥   : 95/09/22  CSK-M.KATOH         *
      * (08) H-010              Õ›∫≥   : 96/04/05  CSK-Y.ITO           *
      * (09) Z-176              Õ›∫≥   : 96/07/17  CSK-C.FURUTA        *
      * (10) Z-183              Õ›∫≥   : 96/11/15  CSK-Y.MIO           *
      * (11) K-261              Õ›∫≥   : 97/09/30  CSK-S.HAMACHI       *
      * (12) K-526              Õ›∫≥   : 02/03/11  CSK-S.KOZAWA        *
      * (13) K-602               ¬≤∂   : 03/06/19  CSK-K.MURAYAMA      *
      * (14) H-010  O-4676      Õ›∫≥   : 04/01/26  CSK-M.YOSHIDA       *
      * (15) C-241              Õ›∫≥   : 05/03/23  CSK-S.HORII         *
      * (16) K-860              Õ›∫≥   : 05/12/14  CSK-Y.MATSUOKA      *
      * (17) K-1253             Õ›∫≥   : 10/10/18  CSK-S.KOZAWA        *
      * (18) K-1308             Õ›∫≥   : 11/02/24  CSK-N.EGUCHI        *
      * (19) H-010  O-8641       ¬≤∂   : 13/05/30  CSK-T.TAKADA        *
      * (20) H-010  O-9024      Õ›∫≥   : 14/02/27  CSK-T.TAKADA        *
      * (21) H-010  O-9162      Õ›∫≥   : 14/05/12  CSK-T.TAKADA        *
      * (22) K-1577              ¬≤∂   : 14/05/09  CSK-T.TAKADA        *
      * (23) H-010  O-9218       ¬≤∂   : 14/07/08  CSK-T.TAKADA        *
      * (24) H-010  O-9368      Õ›∫≥   : 14/12/26  CSK-T.TAKADA        *
      * (25) H-010  0-9744      Õ›∫≥   : 16/03/29  CSK-N.EGUCHI        *
      * (26) K-1716          º÷≥‘∏¬≤∂  : 16/02/29  CSK-K.KOYAMA        *
      * (27) K-1876             Õ›∫≥   : 16/04/19  CSK-N.EGUCHI        *
      * (28) K-1876             Õ›∫≥   : 16/06/03  CSK-N.EGUCHI        *
      * (29) K-1876             Õ›∫≥   : 16/07/07  CSK-N.EGUCHI        *
      * (30) H-010  O-10107     Õ›∫≥   : 17/04/12  CSK-N.EGUCHI        *
      * (31) H-010  O-10414     Õ›∫≥   : 18/03/05  CSK-T.TAKADA        *
      * (32) H-010  O-10603     Õ›∫≥   : 18/10/05  CSK-N.EGUCHI        *
      * (33) H-010  O-11151     Õ›∫≥   : 20/09/07  CSK-R.FUKUHARA      *
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SJMFILE ASSIGN    TO SJMFILE
                          ORGANIZATION INDEXED
                          ACCESS  MODE RANDOM
                          RECORD   KEY KEY-SJM.
       DATA DIVISION.
       FILE SECTION.
       FD  SJMFILE
           LABEL RECORD STANDARD.
       01  SJM-REC.
           02 KEY-SJM.
              03 KEY-STNCD                  PIC  X(03).
              03 KEY-BOTCD                  PIC  X(03).
           02 FILLER                        PIC  X(244).
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      *   WORKING-STORAGE SECTION                                      *
      *----------------------------------------------------------------*
       01  SJM-R.
           COPY VFILSJM SUPPRESS.
       01  EDIT-AREA.
           02 WKJ1-AREA.
              03 WKJ1-STNME                 PIC  X(08) VALUE
                       '•º¶ß#®≠'.
              03 WKJ1-ADRES                 PIC  X(36) VALUE
                       '•~•W•f•´•º¶•¨•b•Ø•§.¶m¢1¢7®z¢4¢3®S'.
              03 WKJ1-BKME                  PIC  X(20) VALUE
                       '§–§G§Œßª•œ  •º¶ß#®≠'.
              03 WKJ1-BKKZ                  PIC  X(04) VALUE
                       '¶˝®Ä'.
           02 WKJ3-AREA.
              03 WKJ3-STNME1.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ê¢ìcíJëÊìÒéxìXÅ@Å@'.
              03 WKJ3-STNME2.
                 04 FILLER                  PIC  N(09) VALUE
                  N'èÈñkëÊìÒéxìXÅ@Å@Å@'.
              03 WKJ3-STNME3.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ìåãûïaâ@ëÊàÍéxìXÅ@'.
              03 WKJ3-STNME4.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ìåãûïaâ@ëÊìÒéxìXÅ@'.
              03 WKJ3-STNME5.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ìåãûïaâ@ëÊéléxìXÅ@'.
              03 WKJ3-STNME6.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ï{íÜëÊìÒéxìXÅ@Å@Å@'.
              03 WKJ3-STNME7.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ñºå√âÆêfífñÚéxìXÅ@'.
              03 WKJ3-STNME8.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ìåãûêfífñÚéxìXÅ@Å@'.
              03 WKJ3-STNME9.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ëÊéOéxìXÅ@Å@Å@Å@Å@'.
              03 WKJ3-ADRES1.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ìåãûìsê¢ìcíJãÊë„ëÚéOíöñ⁄ÇPÇTî‘ÇPçÜÅ@'.
              03 WKJ3-ADRES2.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ìåãûìsë´óßãÊî~ìcéµíöñ⁄ÇQÇVî‘ÇUçÜÅ@Å@'.
              03 WKJ3-ADRES4.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ìåãûìsï{íÜésélíJòZíöñ⁄ÇPÇRî‘ínÇÃÇPÇO'.
              03 WKJ3-ADRES5.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ìåãûìsïiêÏãÊìåïiêÏélíöñ⁄ÇXî‘ÇQÇWçÜÅ@'.
              03 WKJ3-ADRES6.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ìåãûìsäãè¸ãÊêºêVè¨ä‚élíöñ⁄ÇPî‘ÇPÇRçÜ'.
              03 WKJ3-ADRES7.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ñºå√âÆésìåãÊìåï–í[í¨ÇPî‘ínÅ@Å@Å@Å@Å@'.
              03 WKJ3-ADRES8.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ìåãûìsñkãÊìåè\èòZíöñ⁄ÇRî‘ÇRÅ@Å@Å@Å@'.
              03 WKJ3-ADRES91.
                 04 FILLER                  PIC  N(18) VALUE
                  N'â´ìÍåßìáêKåSìÏïóå¥í¨éöêVêÏÅ@Å@Å@Å@Å@'.
              03 WKJ3-ADRES92.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ÇSÇRÇUî‘ínÇPÅ@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@Å@'.
              03 WKJ3-BANK1.
                 04 FILLER                  PIC  N(17) VALUE
                  N'éOïHÇtÇeÇiã‚çsÅ@éOå¨íÉâÆéxìXÅ@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '0330541'.
              03 WKJ3-BANK2.
                 04 FILLER                  PIC  N(17) VALUE
                  N'Ç›Ç∏ÇŸã‚çsÅ@ê¢ìcíJéxìXÅ@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '0101560'.
              03 WKJ3-BANK3.
                 04 FILLER                  PIC  N(17) VALUE
                  N'Ç›Ç∏ÇŸã‚çsÅ@ñkëÚéxìXÅ@Å@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '0005529'.
              03 WKJ3-BANK4.
                 04 FILLER                  PIC  N(17) VALUE
                  N'éOïHÇtÇeÇiã‚çsÅ@ëÂí√í¨éxìXÅ@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '330667 '.
              03 WKJ3-BANK5.
                 04 FILLER                  PIC  N(17) VALUE
                  N'Ç›Ç∏ÇŸã‚çsÅ@ñºå√âÆéxìXÅ@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '0156030'.
              03 WKJ3-BANK61.
                 04 FILLER                  PIC  N(17) VALUE
                  N'óÆãÖã‚çsÅ@äÒã{éxìXÅ@Å@Å@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '2003123'.
              03 WKJ3-BANK62.
                 04 FILLER                  PIC  N(17) VALUE
                  N'â´ìÍã‚çsÅ@èºèÈéxìXÅ@Å@Å@Å@Å@Å@ïÅí '.
                 04 FILLER                  PIC  X(07) VALUE '1338984'.
           02 WKJ4-AREA.
              03 WKJ4-STNME1.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ïüâ™âcã∆ïîÅ@Å@Å@Å@'.
              03 WKJ4-STNME2.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ñkã„èBâcã∆ïîÅ@Å@Å@'.
              03 WKJ4-STNME3.
                 04 FILLER                  PIC  N(09) VALUE
                  N'í}å„âcã∆ïîÅ@Å@Å@Å@'.
              03 WKJ4-STNME4.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ç≤âÍâcã∆ïîÅ@Å@Å@Å@'.
              03 WKJ4-STNME5.
                 04 FILLER                  PIC  N(09) VALUE
                  N'í∑çËâcã∆ïîÅ@Å@Å@Å@'.
              03 WKJ4-STNME6.
                 04 FILLER                  PIC  N(09) VALUE
                  N'åFñ{âcã∆ïîÅ@Å@Å@Å@'.
              03 WKJ4-STNME7.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ëÂï™âcã∆ïîÅ@Å@Å@Å@'.
              03 WKJ4-STNME8.
                 04 FILLER                  PIC  N(09) VALUE
                  N'ã{çËâcã∆ïîÅ@Å@Å@Å@'.
              03 WKJ4-STNME9.
                 04 FILLER                  PIC  N(09) VALUE
                  N'é≠éôìáâcã∆ïîÅ@Å@Å@'.
              03 WKJ4-STNME10.
                 04 FILLER                  PIC  N(09) VALUE
                  N'åoóùïîÅ@Å@Å@Å@Å@Å@'.
              03 WKJ4-ADRES1.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ïüâ™ésîéëΩãÊéRâ§ÇQÅ|ÇSÅ|ÇTÅ@Å@Å@Å@Å@'.
              03 WKJ4-ADRES2.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ñkã„èBésè¨ëqñkãÊíÜà‰ÇSÅ|ÇPÇQÅ|ÇPÇWÅ@'.
              03 WKJ4-ADRES3.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ãvóØïƒésí√ïüñ{í¨ÇSÇVÇOÅ|ÇPÅ@Å@Å@Å@Å@'.
              03 WKJ4-ADRES4.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ç≤âÍésäJê¨ÇUÅ|ÇPÇQÅ|ÇQÇTÅ@Å@Å@Å@Å@Å@'.
              03 WKJ4-ADRES5.
                 04 FILLER                  PIC  N(18) VALUE
                  N'í∑çËésñÌê∂í¨ÇWÅ|ÇPÇWÅ@Å@Å@Å@Å@Å@Å@Å@'.
              03 WKJ4-ADRES6.
                 04 FILLER                  PIC  N(18) VALUE
                  N'åFñ{ésìÏãÊìcå}ÇQÅ|ÇPÇQÅ|ÇQÇWÅ@Å@Å@Å@'.
              03 WKJ4-ADRES7.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ëÂï™ésêVí¨ÇPÇSÅ|ÇWÅ@Å@Å@Å@Å@Å@Å@Å@Å@'.
              03 WKJ4-ADRES8.
                 04 FILLER                  PIC  N(18) VALUE
                  N'ã{çËésëÂíÀí¨ó¨çáÇTÇOÇVÇWÅ|ÇQÅ@Å@Å@Å@'.
              03 WKJ4-ADRES9.
                 04 FILLER                  PIC  N(18) VALUE
                  N'é≠éôìáésêVâhí¨ÇQÅ|ÇQÇQÅ@Å@Å@Å@Å@Å@Å@'.
              03 WKJ4-BANK1A.
                 04 FILLER                  PIC  N(17) VALUE
                  N'ïüâ™ã‚çsÅ@îéëΩéxìXÅ@Å@Å@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '1702   '.
              03 WKJ4-BANK1B.
                 04 FILLER                  PIC  N(17) VALUE
                  N'êºì˙ñ{ÉVÉeÉBÅ@ïüâ™éxìXÅ@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '6984   '.
              03 WKJ4-BANK2A.
                 04 FILLER                  PIC  N(17) VALUE
                  N'ïüâ™ã‚çsÅ@åÀî®éxìXÅ@Å@Å@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '11405  '.
              03 WKJ4-BANK2B.
                 04 FILLER                  PIC  N(17) VALUE
                  N'êºì˙ñ{ÉVÉeÉBÅ@åÀî®éxìXÅ@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '2335   '.
              03 WKJ4-BANK3A.
                 04 FILLER                  PIC  N(17) VALUE
                  N'ïüâ™ã‚çsÅ@ãvóØïƒâcã∆ïîÅ@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '11575  '.
              03 WKJ4-BANK4A.
                 04 FILLER                  PIC  N(17) VALUE
                  N'ç≤âÍã‚çsÅ@å‡ïûí¨éxìXÅ@Å@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '001161 '.
              03 WKJ4-BANK4B.
                 04 FILLER                  PIC  N(17) VALUE
                  N'ç≤âÍã‚çsÅ@à…ñúó¢éxìXÅ@Å@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '002288 '.
              03 WKJ4-BANK5A.
                 04 FILLER                  PIC  N(17) VALUE
                  N'è\î™êeòaã‚çsÅ@äœåıí éxìXÅ@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '18728  '.
              03 WKJ4-BANK5B.
                 04 FILLER                  PIC  N(17) VALUE
                  N'è\î™êeòaã‚çsÅ@í∑çËâcã∆ïîÅ@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '27229  '.
              03 WKJ4-BANK6A.
                 04 FILLER                  PIC  N(17) VALUE
                  N'îÏå„ã‚çsÅ@ìcå}éxìXÅ@Å@Å@Å@Å@Å@ïÅí '.
                 04 FILLER                  PIC  X(07) VALUE '523597 '.
              03 WKJ4-BANK7A.
                 04 FILLER                  PIC  N(17) VALUE
                  N'ëÂï™ã‚çsÅ@ëÂï™âwëOéxìXÅ@Å@Å@Å@ìñç¿'.
                 04 FILLER                  PIC  X(07) VALUE '756460 '.
              03 WKJ4-BANK7B.
                 04 FILLER                  PIC  N(17) VALUE
                  N'êºì˙ñ{ÉVÉeÉBÅ@ï ï{éxìXÅ@Å@Å@Å@ïÅí '.
                 04 FILLER                  PIC  X(07) VALUE '526674 '.
              03 WKJ4-BANK8A.
                 04 FILLER                  PIC  N(17) VALUE
                  N'ã{çËã‚çsÅ@ëÂíÀíÜâõéxìXÅ@Å@Å@Å@ïÅí '.
                 04 FILLER                  PIC  X(07) VALUE '1138302'.
              03 WKJ4-BANK9A.
                 04 FILLER                  PIC  N(17) VALUE
                  N'é≠éôìáã‚çsÅ@âµñ{í¨éxìXÅ@Å@Å@Å@ïÅí '.
                 04 FILLER                  PIC  X(07) VALUE '27587  '.
       LINKAGE SECTION.
      *----------------------------------------------------------------*
      *   LINKAGE SECTION                                              *
      *----------------------------------------------------------------*
       01  PA8-PARM.
           COPY VPARBA8 SUPPRESS.
       PROCEDURE DIVISION USING PA8-PARM.
      *----------------------------------------------------------------*
      *    M0 : CONTROL                                                *
      *----------------------------------------------------------------*
       M0-CONTROL.
           PERFORM M1-INIT THRU M1-EXIT.
           PERFORM M2-MAIN THRU M2-EXIT.
           PERFORM M3-FINL THRU M3-EXIT.
       M0-EXIT.
           GOBACK.
      *----------------------------------------------------------------*
      *    M1 : INITIAL                                                *
      *----------------------------------------------------------------*
       M1-INIT.
           OPEN INPUT  SJMFILE.
           PERFORM M11-READ-SJM THRU M11-EXIT.
       M1-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M1.1 : READ SJMFILE                                         *
      *----------------------------------------------------------------*
       M11-READ-SJM.
           MOVE PA8-STNCD TO KEY-STNCD.
           MOVE PA8-BOTCD TO KEY-BOTCD.
           READ SJMFILE INTO SJM-R
                INVALID DISPLAY 'M VSBA1080 SJMFILE NOT FOUND !! '
                        PA8-STNCD PA8-BOTCD UPON CONSOLE
                        MOVE SPACE TO PA8-STNME
                        MOVE SPACE TO PA8-YUBNO
                        MOVE SPACE TO PA8-TELNO
                        MOVE SPACE TO PA8-ADRES
                        MOVE SPACE TO PA8-BANK1
                        MOVE SPACE TO PA8-BANK2
                        MOVE SPACE TO PA8-BANK3
                        GO TO M11-EXIT.
           MOVE SJM-STNME TO PA8-STNME.
           MOVE SJM-YUBNO TO PA8-YUBNO.
           MOVE SJM-TELNO TO PA8-TELNO.
           MOVE SJM-ADRES TO PA8-ADRES.
           MOVE SJM-BANK1 TO PA8-BANK1.
           MOVE SJM-BANK2 TO PA8-BANK2.
           MOVE SJM-BANK3 TO PA8-BANK3.
       M11-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2 : MAIN                                                   *
      *----------------------------------------------------------------*
       M2-MAIN.
           IF ((PA8-STNCD = '141') AND
               ((PA8-SYKNO = '334') OR (PA8-SYKNO = '330') OR
                (PA8-SYKNO = '331'))) OR
              ((PA8-STNCD = '142') AND
               ((PA8-SYKNO = '110') OR (PA8-SYKNO = '112') OR
                (PA8-SYKNO = '111')))
              PERFORM M21-EDIT-1 THRU M21-EXIT
              GO TO M2-EXIT.
           EVALUATE PA8-STNCD
           WHEN '131'
              EVALUATE PA8-SYKNO
              WHEN '110'
              WHEN '111'
              WHEN '112'
                 PERFORM M24-EDIT-4 THRU M24-EXIT,
              WHEN '370'
              WHEN '371'
              WHEN '372'
                 PERFORM M2D-EDIT-13 THRU M2D-EXIT,
              WHEN '470'
              WHEN '471'
              WHEN '472'
                 PERFORM M25-EDIT-5 THRU M25-EXIT,
              WHEN OTHER
                 CONTINUE
              END-EVALUATE,
           WHEN '132'
              EVALUATE PA8-SYKNO
              WHEN '072'
                 PERFORM M2E-EDIT-14 THRU M2E-EXIT,
              WHEN '710'
              WHEN '711'
              WHEN '712'
              WHEN '713'
              WHEN '714'
                 PERFORM M26-EDIT-6 THRU M26-EXIT,
              WHEN '720'
              WHEN '721'
              WHEN '723'
                 PERFORM M27-EDIT-7 THRU M27-EXIT,
              WHEN '740'
              WHEN '741'
                 PERFORM M28-EDIT-8 THRU M28-EXIT,
              WHEN '722'
                 PERFORM M2A-EDIT-10 THRU M2A-EXIT,
              WHEN '742'
                 PERFORM M2B-EDIT-11 THRU M2B-EXIT,
              WHEN OTHER
                 CONTINUE
              END-EVALUATE,
           WHEN '133'
              EVALUATE PA8-SYKNO
              WHEN '610'
              WHEN '611'
              WHEN '612'
              WHEN '613'
              WHEN '615'
                 PERFORM M29-EDIT-9 THRU M29-EXIT,
              WHEN OTHER
                 CONTINUE
              END-EVALUATE,
           WHEN '231'
              EVALUATE PA8-SYKNO
              WHEN '260'
              WHEN '261'
              WHEN '262'
              WHEN '263'
                 PERFORM M2C-EDIT-12 THRU M2C-EXIT,
              WHEN OTHER
                 CONTINUE
              END-EVALUATE,
           WHEN '471'
              EVALUATE PA8-SYKNO
              WHEN '130'
              WHEN '131'
              WHEN '132'
              WHEN '133'
              WHEN '134'
                 PERFORM M2P-EDIT-25 THRU M2P-EXIT,
              WHEN OTHER
                 CONTINUE
              END-EVALUATE,
           WHEN '402'
              PERFORM M2F-EDIT-15 THRU M2F-EXIT,
           WHEN '403'
              PERFORM M2G-EDIT-16 THRU M2G-EXIT,
           WHEN '404'
              PERFORM M2H-EDIT-17 THRU M2H-EXIT,
           WHEN '411'
              PERFORM M2I-EDIT-18 THRU M2I-EXIT,
           WHEN '421'
              PERFORM M2J-EDIT-19 THRU M2J-EXIT,
           WHEN '432'
              PERFORM M2K-EDIT-20 THRU M2K-EXIT,
           WHEN '441'
              PERFORM M2L-EDIT-21 THRU M2L-EXIT,
           WHEN '451'
              PERFORM M2M-EDIT-22 THRU M2M-EXIT,
           WHEN '462'
              PERFORM M2N-EDIT-23 THRU M2N-EXIT,
           WHEN '773'
              PERFORM M2O-EDIT-24 THRU M2O-EXIT,
           WHEN OTHER
              CONTINUE
           END-EVALUATE.
       M2-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.1 : EDIT-AGAIN (1)                                       *
      *----------------------------------------------------------------*
       M21-EDIT-1.
           MOVE SPACE           TO PA8-BANK1.
           MOVE SPACE           TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
           MOVE WKJ1-STNME      TO PA8-STNME.
           MOVE '254'           TO PA8-YBNO1.
           MOVE '0013'          TO PA8-YBNO2.
           MOVE '(0463)54-2251' TO PA8-TELNO.
           MOVE WKJ1-ADRES      TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ1-BKME       TO PA8-BKME1.
           MOVE WKJ1-BKKZ       TO PA8-BKKZ1.
           MOVE '0102292'       TO PA8-BKNO1.
       M21-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.4 : EDIT-AGAIN (4)                                       *
      *----------------------------------------------------------------*
       M24-EDIT-4.
           MOVE WKJ3-STNME1     TO PA8-STNME.
           MOVE '1558601'       TO PA8-YUBNO.
           MOVE '03-3422-5285 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES1     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ3-BANK1      TO PA8-BANK1.
           MOVE WKJ3-BANK2      TO PA8-BANK2.
           MOVE WKJ3-BANK3      TO PA8-BANK3.
       M24-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.5 : EDIT-AGAIN (5)                                       *
      *----------------------------------------------------------------*
       M25-EDIT-5.
           MOVE WKJ3-STNME2     TO PA8-STNME.
           MOVE '1230851'       TO PA8-YUBNO.
           MOVE '03-3848-3411 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES2     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ3-BANK1      TO PA8-BANK1.
           MOVE WKJ3-BANK2      TO PA8-BANK2.
           MOVE WKJ3-BANK3      TO PA8-BANK3.
       M25-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.6 : EDIT-AGAIN (6)                                       *
      *----------------------------------------------------------------*
       M26-EDIT-6.
           MOVE WKJ3-STNME3     TO PA8-STNME.
           MOVE '1558601'       TO PA8-YUBNO.
           MOVE '03-3422-5293 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES1     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ3-BANK1      TO PA8-BANK1.
           MOVE WKJ3-BANK2      TO PA8-BANK2.
           MOVE WKJ3-BANK3      TO PA8-BANK3.
       M26-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.7 : EDIT-AGAIN (7)                                       *
      *----------------------------------------------------------------*
       M27-EDIT-7.
           MOVE WKJ3-STNME4     TO PA8-STNME.
           MOVE '1558601'       TO PA8-YUBNO.
           MOVE '03-3422-5293 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES1     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ3-BANK1      TO PA8-BANK1.
           MOVE WKJ3-BANK2      TO PA8-BANK2.
           MOVE WKJ3-BANK3      TO PA8-BANK3.
       M27-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.8 : EDIT-AGAIN (8)                                       *
      *----------------------------------------------------------------*
       M28-EDIT-8.
           MOVE WKJ3-STNME5     TO PA8-STNME.
           MOVE '1140001'       TO PA8-YUBNO.
           MOVE '03-5249-5731 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES8     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ3-BANK1      TO PA8-BANK1.
           MOVE WKJ3-BANK2      TO PA8-BANK2.
           MOVE WKJ3-BANK3      TO PA8-BANK3.
       M28-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.9 : EDIT-AGAIN (9)                                       *
      *----------------------------------------------------------------*
       M29-EDIT-9.
           MOVE WKJ3-STNME6     TO PA8-STNME.
           MOVE '1830035'       TO PA8-YUBNO.
           MOVE '042-367-3511 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES4     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ3-BANK1      TO PA8-BANK1.
           MOVE WKJ3-BANK2      TO PA8-BANK2.
           MOVE WKJ3-BANK3      TO PA8-BANK3.
       M29-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.A : EDIT-AGAIN (10)                                      *
      *----------------------------------------------------------------*
       M2A-EDIT-10.
           MOVE WKJ3-STNME4     TO PA8-STNME.
           MOVE '1400002'       TO PA8-YUBNO.
           MOVE '03-3740-1511 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES5     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ3-BANK1      TO PA8-BANK1.
           MOVE WKJ3-BANK2      TO PA8-BANK2.
           MOVE WKJ3-BANK3      TO PA8-BANK3.
       M2A-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.B : EDIT-AGAIN (11)                                      *
      *----------------------------------------------------------------*
       M2B-EDIT-11.
           MOVE WKJ3-STNME5     TO PA8-STNME.
           MOVE '1240025'       TO PA8-YUBNO.
           MOVE '03-3695-6181 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES6     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ3-BANK1      TO PA8-BANK1.
           MOVE WKJ3-BANK2      TO PA8-BANK2.
           MOVE WKJ3-BANK3      TO PA8-BANK3.
       M2B-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.C : EDIT-AGAIN (12)                                      *
      *----------------------------------------------------------------*
       M2C-EDIT-12.
           MOVE WKJ3-STNME7     TO PA8-STNME.
           MOVE '4610015'       TO PA8-YUBNO.
           MOVE '052-953-7130 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES7     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ3-BANK4      TO PA8-BANK1.
           MOVE WKJ3-BANK5      TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2C-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.D : EDIT-AGAIN (13)                                      *
      *----------------------------------------------------------------*
       M2D-EDIT-13.
           MOVE WKJ3-STNME2     TO PA8-STNME.
           MOVE '1140001'       TO PA8-YUBNO.
           MOVE '03-5249-5721 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES8     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ3-BANK1      TO PA8-BANK1.
           MOVE WKJ3-BANK2      TO PA8-BANK2.
           MOVE WKJ3-BANK3      TO PA8-BANK3.
       M2D-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.E : EDIT-AGAIN (14)                                      *
      *----------------------------------------------------------------*
       M2E-EDIT-14.
           MOVE WKJ3-STNME8     TO PA8-STNME.
           MOVE '1140001'       TO PA8-YUBNO.
           MOVE '03-5249-5732 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES8     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ3-BANK1      TO PA8-BANK1.
           MOVE WKJ3-BANK2      TO PA8-BANK2.
           MOVE WKJ3-BANK3      TO PA8-BANK3.
       M2E-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.F : EDIT-AGAIN (15)  BU:402                              *
      *----------------------------------------------------------------*
       M2F-EDIT-15.
           MOVE WKJ4-STNME1     TO PA8-STNME.
           MOVE '8128681'       TO PA8-YUBNO.
           MOVE '(092)471-2220' TO PA8-TELNO.
           MOVE WKJ4-ADRES1     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ4-BANK1A     TO PA8-BANK1.
           MOVE WKJ4-BANK1B     TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2F-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.G : EDIT-AGAIN (16)  BU:403                              *
      *----------------------------------------------------------------*
       M2G-EDIT-16.
           MOVE WKJ4-STNME2     TO PA8-STNME.
           MOVE '8030836'       TO PA8-YUBNO.
           MOVE '(093)571-1681' TO PA8-TELNO.
           MOVE WKJ4-ADRES2     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ4-BANK2A     TO PA8-BANK1.
           MOVE WKJ4-BANK2B     TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2G-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.H : EDIT-AGAIN (17)  BU:404                              *
      *----------------------------------------------------------------*
       M2H-EDIT-17.
           MOVE WKJ4-STNME3     TO PA8-STNME.
           MOVE '8300047'       TO PA8-YUBNO.
           MOVE '(0942)32-6285' TO PA8-TELNO.
           MOVE WKJ4-ADRES3     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ4-BANK3A     TO PA8-BANK1.
           MOVE SPACE           TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2H-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.I : EDIT-AGAIN (18)  BU:411                              *
      *----------------------------------------------------------------*
       M2I-EDIT-18.
           MOVE WKJ4-STNME4     TO PA8-STNME.
           MOVE '8490934'       TO PA8-YUBNO.
           MOVE '(0952)30-3211' TO PA8-TELNO.
           MOVE WKJ4-ADRES4     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ4-BANK4A     TO PA8-BANK1.
           MOVE WKJ4-BANK4B     TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2I-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.J : EDIT-AGAIN (19)  BU:421                              *
      *----------------------------------------------------------------*
       M2J-EDIT-19.
           MOVE WKJ4-STNME5     TO PA8-STNME.
           MOVE '8500823'       TO PA8-YUBNO.
           MOVE '(095)823-2120' TO PA8-TELNO.
           MOVE WKJ4-ADRES5     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ4-BANK5A     TO PA8-BANK1.
           MOVE WKJ4-BANK5B     TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2J-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.K : EDIT-AGAIN (20)  BU:432                              *
      *----------------------------------------------------------------*
       M2K-EDIT-20.
           MOVE WKJ4-STNME6     TO PA8-STNME.
           MOVE '8620962'       TO PA8-YUBNO.
           MOVE '(096)370-3411' TO PA8-TELNO.
           MOVE WKJ4-ADRES6     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ4-BANK6A     TO PA8-BANK1.
           MOVE SPACE           TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2K-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.L : EDIT-AGAIN (21)  BU:441                              *
      *----------------------------------------------------------------*
       M2L-EDIT-21.
           MOVE WKJ4-STNME7     TO PA8-STNME.
           MOVE '8700028'       TO PA8-YUBNO.
           MOVE '(097)534-8155' TO PA8-TELNO.
           MOVE WKJ4-ADRES7     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ4-BANK7A     TO PA8-BANK1.
           MOVE WKJ4-BANK7B     TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2L-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.M : EDIT-AGAIN (22)  BU:451                              *
      *----------------------------------------------------------------*
       M2M-EDIT-22.
           MOVE WKJ4-STNME8     TO PA8-STNME.
           MOVE '8800951'       TO PA8-YUBNO.
           MOVE '(0985)53-8155' TO PA8-TELNO.
           MOVE WKJ4-ADRES8     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ4-BANK8A     TO PA8-BANK1.
           MOVE SPACE           TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2M-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.N : EDIT-AGAIN (23)  BU:462                              *
      *----------------------------------------------------------------*
       M2N-EDIT-23.
           MOVE WKJ4-STNME9     TO PA8-STNME.
           MOVE '8900072'       TO PA8-YUBNO.
           MOVE '(099)254-2501' TO PA8-TELNO.
           MOVE WKJ4-ADRES9     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ4-BANK9A     TO PA8-BANK1.
           MOVE SPACE           TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2N-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.O : EDIT-AGAIN (24)  BU:773                              *
      *----------------------------------------------------------------*
       M2O-EDIT-24.
           MOVE WKJ4-STNME10    TO PA8-STNME.
           MOVE '8128681'       TO PA8-YUBNO.
           MOVE '(092)471-2220' TO PA8-TELNO.
           MOVE WKJ4-ADRES1     TO PA8-ADRS1.
           MOVE SPACE           TO PA8-ADRS2.
           MOVE WKJ4-BANK1A     TO PA8-BANK1.
           MOVE WKJ4-BANK1B     TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2O-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M2.P : EDIT-AGAIN (25)                                      *
      *----------------------------------------------------------------*
       M2P-EDIT-25.
           MOVE WKJ3-STNME9     TO PA8-STNME.
           MOVE '9011105'       TO PA8-YUBNO.
           MOVE '098-835-6363 ' TO PA8-TELNO.
           MOVE WKJ3-ADRES91    TO PA8-ADRS1.
           MOVE WKJ3-ADRES92    TO PA8-ADRS2.
           MOVE WKJ3-BANK61     TO PA8-BANK1.
           MOVE WKJ3-BANK62     TO PA8-BANK2.
           MOVE SPACE           TO PA8-BANK3.
       M2P-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    M3 : FINAL                                                  *
      *----------------------------------------------------------------*
       M3-FINL.
           CLOSE SJMFILE.
       M3-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *        VSBA1080 END                                            *
      *----------------------------------------------------------------*
