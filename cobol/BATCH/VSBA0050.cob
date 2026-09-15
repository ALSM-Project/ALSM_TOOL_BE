./ ADD NAME=VSBA0050  01.01 20000801 20150709 104046 00308 00308 MCSK002
       ID  DIVISION.                                                    VSB00010
       PROGRAM-ID.      'VSBA0050'.                                     VSB00020
      *----------------------------------------------------------------*VSB00030
      *    SUMMARY   <> SYSTEM      : SUZUKEN 10 SYSTEM                *VSB00040
      *              <> SUBSYSTEM   : SUBROUTINE                       *VSB00050
      *              <> PG.OUTLINE  : SEARCH SOSHIKI TABLE             *VSB00060
      *                                                                *VSB00070
      *               *  INPUT.PARM1 :                                 *VSB00080
      *                     HANBETSU-CODE = 1 KIGO (BASHO)             *VSB00080
      *                                   = 2 SOUKO NO. + KIGO (SITEN) *VSB00090
      *                                   = 3 SYOKA NO. + KIGO (SITEN) *VSB00100
      *                                   = 4 SITEN NO.                *VSB00110
      *                                   = 5 KIGO (KA)                *VSB00120
      *                                   = 6 KIGO (SITEN)             *VSB00130
      *                                   = 7 KIGO (SITEN)             *VSB00140
      *                                   = 8 KIGO (BASHO)             *VSB00150
      *                                   = 9 SITEN NO. + SOUKO NO.    *VSB00150
      *                                   = A SITEN NO. + SYOKA NO.    *VSB00150
      *                     O KEY TOSHITE SEARCH SURU                  *VSB00160
      *                                                                *VSB00070
      *                  INPUT.PARM2 : GAITO KEY                       *VSB00170
      *                              : SOSIKI TABLE                    *VSB00180
      *                                                                *VSB00070
      *               *  OUTPUT.PARM1 : RTNCD = 0 / OK                 *VSB00190
      *                                       = 1 / ERROR              *VSB00200
      *                                 : SOSHIKI TABLE ENTRY          *VSB00210
      *              <> AUTHOR      : CSK-F.M                          *VSB00220
      *              <> DATE        : 82/05/26                         *VSB00230
      *---------------- MAINTENANCE  HISTORY --------------------------*VSB00240
      *  1. SOSHIKI-CODE NO HENKOU     : 85/01/25   CSK-M.TAKAHASHI    *VSB00290
      * (02) K-394  ØºÝÊß²Ù ÉÐ  ÍÝº³   : 00/08/01  CSK-S.KOZAWA        *
      * (03) K-1758           ØºÝÊß²Ù  : 15/07/09  CSK-Y.MIO           *
      * (04) YYYYYY YYYYYY       Â²¶   : YY/MM/DD  CSK-X.XXXXXX        *
      *----------------------------------------------------------------*VSB00300
       ENVIRONMENT DIVISION.                                            VSB00310
       DATA DIVISION.                                                   VSB00320
       LINKAGE SECTION.                                                 VSB00330
      *----------------------------------------------------------------*VSB00340
      *   LINCAGE SECTION                                              *VSB00350
      *----------------------------------------------------------------*VSB00360
       01  PARAM1.                                                      VSB00370
           COPY VPARB05.                                                VSB00380
       01  PARAM2.                                                      VSB00390
           COPY VPARTBL.                                                VSB00400
       PROCEDURE DIVISION USING PARAM1 PARAM2.                          VSB00410
      *----------------------------------------------------------------*VSB00420
      *    1.0 : MAIN CONTROL                                          *VSB00430
      *----------------------------------------------------------------*VSB00440
       M10-MAIN-CONTROL.                                                VSB00450
      *                                                                 VSB00460
           IF        P05-HNBCD = '1'                                    VSB00470
               PERFORM  M11-SEARCH-KIGO    THRU  M11-EXIT               VSB00480
           ELSE  IF  P05-HNBCD = '2'                                    VSB00490
               PERFORM  M12-SEARCH-SOKNO-S THRU  M12-EXIT               VSB00500
           ELSE  IF  P05-HNBCD = '3'                                    VSB00510
               PERFORM  M13-SEARCH-KANO-S  THRU  M13-EXIT               VSB00520
           ELSE  IF  P05-HNBCD = '4'                                    VSB00530
               PERFORM  M14-SEARCH-SITEN   THRU  M14-EXIT               VSB00540
           ELSE  IF  P05-HNBCD = '5'                                    VSB00550
               PERFORM  M15-SEARCH-KACD    THRU  M15-EXIT               VSB00560
           ELSE  IF  P05-HNBCD = '6'                                    VSB00570
               PERFORM  M16-SEARCH-SITENCD THRU  M16-EXIT               VSB00580
           ELSE  IF  P05-HNBCD = '7'                                    VSB00590
               PERFORM  M17-SEARCH-KIGO    THRU  M17-EXIT               VSB00600
           ELSE  IF  P05-HNBCD = '8'                                    VSB00610
               PERFORM  M18-SEARCH-BASHO   THRU  M18-EXIT               VSB00620
           ELSE  IF  P05-HNBCD = '9'                                    VSB00610
               PERFORM  M19-SEARCH-STNNO-S THRU  M19-EXIT               VSB00620
           ELSE  IF  P05-HNBCD = 'A'                                    VSB00610
               PERFORM  M1A-SEARCH-STNNO-K THRU  M1A-EXIT               VSB00620
           ELSE                                                         VSB00630
               MOVE   1   TO  P05-RTNCD.                                VSB00640
      *                                                                 VSB00650
       M10-EXIT.                                                        VSB00660
           GOBACK.                                                      VSB00670
      *----------------------------------------------------------------*VSB00680
      *    1.1 : SEARCH SOSHIKI TABLE KEY = SITEN & SOKO KIGOU         *VSB00690
      *----------------------------------------------------------------*VSB00700
       M11-SEARCH-KIGO.                                                 VSB00710
           MOVE  ZERO TO P05-RTNCD.                                     VSB00720
           SET  WK-I TO 1.                                              VSB00730
           SEARCH  TB-S                                                 VSB00740
               AT END MOVE  1  TO  P05-RTNCD                            VSB00750
               WHEN   TB-STNCD (WK-I) = '*'                             VSB00770
                      MOVE  1  TO  P05-RTNCD                            VSB00780
               WHEN   P05-STNCD = TB-STNCD (WK-I)   AND                 VSB00790
                      P05-SOKCD = TB-SOKCD (WK-I)                       VSB00800
                      MOVE  TB-SYSNO (WK-I) TO P05-SYSNO                VSB00810
                      MOVE  TB-KACD  (WK-I) TO P05-KACD                 VSB00820
                      MOVE  TB-STNNO (WK-I) TO P05-STNNO                VSB00830
                      MOVE  TB-SOKNO (WK-I) TO P05-SOKNO                VSB00840
                      MOVE  TB-KANO  (WK-I) TO P05-KANO                 VSB00850
                      MOVE  TB-SITEN (WK-I) TO P05-SITEN                VSB00860
                      MOVE  TB-SYOKA (WK-I) TO P05-SYOKA.               VSB00870
       M11-EXIT.                                                        VSB00880
           EXIT.                                                        VSB00890
      *----------------------------------------------------------------*VSB00900
      *    1.2 : SEARCH SOSHIKI TABLE KEY = SOKO-NO & SITEN-CODE       *VSB00910
      *----------------------------------------------------------------*VSB00920
       M12-SEARCH-SOKNO-S.                                              VSB00930
           IF  P05-SOKNO  IS NOT NUMERIC                                VSB00940
               MOVE  1  TO  P05-RTNCD                                   VSB00950
               GO  TO   M12-EXIT                                        VSB00960
           ELSE                                                         VSB00970
               MOVE  ZERO  TO  P05-RTNCD.                               VSB00980
           SET  WK-I TO 1.                                              VSB00990
           SEARCH  TB-S                                                 VSB01000
               AT END MOVE  1  TO  P05-RTNCD                            VSB01010
               WHEN   TB-STNCD (WK-I) = '*'                             VSB01030
                      MOVE  1  TO  P05-RTNCD                            VSB01040
               WHEN   P05-SOKNO = TB-SOKNO (WK-I)   AND                 VSB01050
                      P05-STNCD = TB-STNCD (WK-I)                       VSB01050
                      MOVE  TB-SYSNO (WK-I) TO P05-SYSNO                VSB01060
                      MOVE  TB-SOKCD (WK-I) TO P05-SOKCD                VSB01080
                      MOVE  TB-KACD  (WK-I) TO P05-KACD                 VSB01090
                      MOVE  TB-STNNO (WK-I) TO P05-STNNO                VSB01100
                      MOVE  TB-KANO  (WK-I) TO P05-KANO                 VSB01110
                      MOVE  TB-SITEN (WK-I) TO P05-SITEN                VSB01120
                      MOVE  TB-SYOKA (WK-I) TO P05-SYOKA.               VSB01130
       M12-EXIT.                                                        VSB01140
           EXIT.                                                        VSB01150
      *----------------------------------------------------------------*VSB01160
      *    1.3 : SEARCH SOSHIKI TABLE KEY = KANO & SITEN-CODE          *VSB01170
      *----------------------------------------------------------------*VSB01180
       M13-SEARCH-KANO-S.                                               VSB01190
           IF  P05-KANO  IS NOT NUMERIC                                 VSB01200
               MOVE  1  TO  P05-RTNCD                                   VSB01210
               GO  TO   M13-EXIT                                        VSB01220
           ELSE                                                         VSB01230
               MOVE  ZERO  TO  P05-RTNCD.                               VSB01240
           SET  WK-I TO 1.                                              VSB01250
           SEARCH  TB-S                                                 VSB01260
               AT END MOVE  1 TO P05-RTNCD                              VSB01270
               WHEN   TB-STNCD (WK-I) = '*'                             VSB01290
                      MOVE  1 TO P05-RTNCD                              VSB01300
               WHEN   P05-KANO  = TB-KANO  (WK-I)   AND                 VSB01310
                      P05-STNCD = TB-STNCD (WK-I)                       VSB01310
                      MOVE  TB-SYSNO (WK-I) TO P05-SYSNO                VSB01320
                      MOVE  TB-SOKCD (WK-I) TO P05-SOKCD                VSB01340
                      MOVE  TB-KACD  (WK-I) TO P05-KACD                 VSB01350
                      MOVE  TB-STNNO (WK-I) TO P05-STNNO                VSB01360
                      MOVE  TB-SOKNO (WK-I) TO P05-SOKNO                VSB01370
                      MOVE  TB-SITEN (WK-I) TO P05-SITEN                VSB01380
                      MOVE  TB-SYOKA (WK-I) TO P05-SYOKA.               VSB01390
       M13-EXIT.                                                        VSB01400
           EXIT.                                                        VSB01410
      *----------------------------------------------------------------*VSB01420
      *    1.4 : SEARCH SOSHIKI TABLE KEY = SITEN NO.                  *VSB01430
      *----------------------------------------------------------------*VSB01440
       M14-SEARCH-SITEN.                                                VSB01450
           IF  P05-STNNO  IS NOT NUMERIC                                VSB01460
               MOVE  1  TO  P05-RTNCD                                   VSB01470
               GO  TO   M14-EXIT                                        VSB01480
           ELSE                                                         VSB01490
               MOVE  ZERO  TO  P05-RTNCD.                               VSB01500
           SET  WK-I TO 1.                                              VSB01510
           SEARCH  TB-S                                                 VSB01520
               AT END MOVE  1 TO P05-RTNCD                              VSB01530
               WHEN   TB-STNCD (WK-I) = '*'                             VSB01550
                      MOVE  1 TO P05-RTNCD                              VSB01560
               WHEN   P05-STNNO = TB-STNNO (WK-I)                       VSB01570
                      MOVE  TB-SYSNO (WK-I) TO P05-SYSNO                VSB01580
                      MOVE  TB-STNCD (WK-I) TO P05-STNCD                VSB01590
                      MOVE  TB-SOKCD (WK-I) TO P05-SOKCD                VSB01600
                      MOVE  TB-KACD  (WK-I) TO P05-KACD                 VSB01610
                      MOVE  TB-SOKNO (WK-I) TO P05-SOKNO                VSB01620
                      MOVE  TB-KANO  (WK-I) TO P05-KANO                 VSB01630
                      MOVE  TB-SITEN (WK-I) TO P05-SITEN                VSB01640
                      MOVE  TB-SYOKA (WK-I) TO P05-SYOKA.               VSB01650
       M14-EXIT.                                                        VSB01660
           EXIT.                                                        VSB01670
      *----------------------------------------------------------------*VSB01680
      *    1.5 : SEARCH SOSHIKI TABLE KEY = SITEN & SYOKA KIGOU        *VSB01690
      *----------------------------------------------------------------*VSB01700
       M15-SEARCH-KACD.                                                 VSB01710
           MOVE  ZERO TO P05-RTNCD.                                     VSB01720
           SET  WK-I TO 1.                                              VSB01730
           SEARCH  TB-S                                                 VSB01740
               AT END MOVE  1 TO P05-RTNCD                              VSB01750
               WHEN   TB-STNCD (WK-I) = '*'                             VSB01770
                      MOVE  1 TO P05-RTNCD                              VSB01780
               WHEN   P05-STNCD = TB-STNCD (WK-I)    AND                VSB01790
                      P05-KACD  = TB-KACD (WK-I)                        VSB01800
                      MOVE  TB-SYSNO (WK-I) TO P05-SYSNO                VSB01810
                      MOVE  TB-SOKCD (WK-I) TO P05-SOKCD                VSB01820
                      MOVE  TB-STNNO (WK-I) TO P05-STNNO                VSB01830
                      MOVE  TB-SOKNO (WK-I) TO P05-SOKNO                VSB01840
                      MOVE  TB-KANO  (WK-I) TO P05-KANO                 VSB01850
                      MOVE  TB-SITEN (WK-I) TO P05-SITEN                VSB01860
                      MOVE  TB-SYOKA (WK-I) TO P05-SYOKA.               VSB01870
       M15-EXIT.                                                        VSB01880
           EXIT.                                                        VSB01890
      *----------------------------------------------------------------*VSB01900
      *    1.6 : SEARCH SOSHIKI TABLE KEY = SITEN KIGOU                *VSB01910
      *----------------------------------------------------------------*VSB01920
       M16-SEARCH-SITENCD.                                              VSB01930
           MOVE  ZERO TO P05-RTNCD.                                     VSB01940
           SET  WK-I TO 1.                                              VSB01950
           SEARCH  TB-S                                                 VSB01960
               AT END MOVE  1 TO P05-RTNCD                              VSB01970
               WHEN   TB-STNCD (WK-I) = '*'                             VSB01990
                      MOVE  1 TO P05-RTNCD                              VSB02000
               WHEN   P05-STNCD = TB-STNCD (WK-I)                       VSB02010
                      MOVE  TB-SYSNO (WK-I) TO P05-SYSNO                VSB02020
                      MOVE  TB-SOKCD (WK-I) TO P05-SOKCD                VSB02030
                      MOVE  TB-KACD  (WK-I) TO P05-KACD                 VSB02040
                      MOVE  TB-STNNO (WK-I) TO P05-STNNO                VSB02050
                      MOVE  TB-SOKNO (WK-I) TO P05-SOKNO                VSB02060
                      MOVE  TB-KANO  (WK-I) TO P05-KANO                 VSB02070
                      MOVE  TB-SITEN (WK-I) TO P05-SITEN                VSB02080
                      MOVE  TB-SYOKA (WK-I) TO P05-SYOKA.               VSB02090
       M16-EXIT.                                                        VSB02100
           EXIT.                                                        VSB02110
      *----------------------------------------------------------------*VSB02120
      *    1.7 : SEARCH SOSHIKI TABLE KEY = SITENNO & SOKO KIGOU       *VSB02130
      *----------------------------------------------------------------*VSB02140
       M17-SEARCH-KIGO.                                                 VSB02150
           MOVE  ZERO TO P05-RTNCD.                                     VSB02160
           SET  WK-I TO 1.                                              VSB02170
           SEARCH  TB-S                                                 VSB02180
               AT END MOVE  1  TO  P05-RTNCD                            VSB02190
               WHEN   TB-STNCD (WK-I) = '*'                             VSB02210
                      MOVE  1  TO  P05-RTNCD                            VSB02220
               WHEN   P05-STNNO = TB-STNNO (WK-I)   AND                 VSB02230
                      P05-SOKCD = TB-SOKCD (WK-I)                       VSB02240
                      MOVE  TB-SYSNO (WK-I) TO P05-SYSNO                VSB02250
                      MOVE  TB-KACD  (WK-I) TO P05-KACD                 VSB02260
                      MOVE  TB-STNNO (WK-I) TO P05-STNNO                VSB02270
                      MOVE  TB-SOKNO (WK-I) TO P05-SOKNO                VSB02280
                      MOVE  TB-KANO  (WK-I) TO P05-KANO                 VSB02290
                      MOVE  TB-SITEN (WK-I) TO P05-SITEN                VSB02300
                      MOVE  TB-SYOKA (WK-I) TO P05-SYOKA.               VSB02310
       M17-EXIT.                                                        VSB02320
           EXIT.                                                        VSB02330
      *----------------------------------------------------------------*VSB02340
      *    1.8 : SEARCH SOSHIKI TABLE KEY = SITEN & SOKO KIGOU         *VSB02350
      *----------------------------------------------------------------*VSB02360
       M18-SEARCH-BASHO.                                                VSB02370
           MOVE  ZERO TO P05-RTNCD.                                     VSB02380
           SET  WK-I TO 1.                                              VSB02390
           SEARCH  TB-S                                                 VSB02400
               AT END MOVE  1  TO  P05-RTNCD                            VSB02410
               WHEN   TB-STNCD (WK-I) = '*'                             VSB02430
                      MOVE  1  TO  P05-RTNCD                            VSB02440
               WHEN   P05-STNCD = TB-STNCD (WK-I)   AND                 VSB02450
                      P05-SOKCD = TB-SOKCD (WK-I)   AND                 VSB02460
                      TB-KACD (WK-I) NOT = '*'                          VSB02470
                      MOVE  TB-SYSNO (WK-I) TO P05-SYSNO                VSB02480
                      MOVE  TB-KACD  (WK-I) TO P05-KACD                 VSB02490
                      MOVE  TB-STNNO (WK-I) TO P05-STNNO                VSB02500
                      MOVE  TB-SOKNO (WK-I) TO P05-SOKNO                VSB02510
                      MOVE  TB-KANO  (WK-I) TO P05-KANO                 VSB02520
                      MOVE  TB-SITEN (WK-I) TO P05-SITEN                VSB02530
                      MOVE  TB-SYOKA (WK-I) TO P05-SYOKA.               VSB02540
       M18-EXIT.                                                        VSB02550
           EXIT.                                                        VSB02560
      *----------------------------------------------------------------*VSB02340
      *    1.9 : SEARCH SOSHIKI TABLE KEY = SITEN-NO & SOUKO-NO        *VSB02350
      *----------------------------------------------------------------*VSB02360
       M19-SEARCH-STNNO-S.                                              VSB02370
           IF  P05-STNNO  IS NOT NUMERIC   OR                           VSB01460
               P05-SOKNO  IS NOT NUMERIC                                VSB01460
               MOVE  1     TO  P05-RTNCD                                VSB01470
               GO  TO   M19-EXIT                                        VSB01480
           ELSE                                                         VSB01490
               MOVE  ZERO  TO  P05-RTNCD.                               VSB01500
           SET  WK-I TO 1.                                              VSB02390
           SEARCH  TB-S                                                 VSB02400
               AT END MOVE  1  TO  P05-RTNCD                            VSB02410
               WHEN   TB-STNCD (WK-I) = '*'                             VSB02430
                      MOVE  1  TO  P05-RTNCD                            VSB02440
               WHEN   P05-STNNO = TB-STNNO (WK-I)   AND                 VSB02450
                      P05-SOKNO = TB-SOKNO (WK-I)                       VSB02460
                      MOVE  TB-SYSNO (WK-I) TO P05-SYSNO                VSB02480
                      MOVE  TB-STNCD (WK-I) TO P05-STNCD                VSB02480
                      MOVE  TB-SOKCD (WK-I) TO P05-SOKCD                VSB02510
                      MOVE  TB-KACD  (WK-I) TO P05-KACD                 VSB02490
                      MOVE  TB-KANO  (WK-I) TO P05-KANO                 VSB02520
                      MOVE  TB-SITEN (WK-I) TO P05-SITEN                VSB02530
                      MOVE  TB-SYOKA (WK-I) TO P05-SYOKA.               VSB02540
       M19-EXIT.                                                        VSB02550
           EXIT.                                                        VSB02560
      *----------------------------------------------------------------*VSB02340
      *    2.0 : SEARCH SOSHIKI TABLE KEY = SITEN-NO & SYOKA-NO        *VSB02350
      *----------------------------------------------------------------*VSB02360
       M1A-SEARCH-STNNO-K.                                              VSB02370
           IF  P05-STNNO  IS NOT NUMERIC   OR                           VSB01460
               P05-KANO   IS NOT NUMERIC                                VSB01460
               MOVE  1     TO  P05-RTNCD                                VSB01470
               GO  TO   M1A-EXIT                                        VSB01480
           ELSE                                                         VSB01490
               MOVE  ZERO  TO  P05-RTNCD.                               VSB01500
           SET  WK-I TO 1.                                              VSB02390
           SEARCH  TB-S                                                 VSB02400
               AT END MOVE  1  TO  P05-RTNCD                            VSB02410
               WHEN   TB-STNCD (WK-I) = '*'                             VSB02430
                      MOVE  1  TO  P05-RTNCD                            VSB02440
               WHEN   P05-STNNO = TB-STNNO (WK-I)   AND                 VSB02450
                      P05-KANO  = TB-KANO  (WK-I)                       VSB02460
                      MOVE  TB-SYSNO (WK-I) TO P05-SYSNO                VSB02480
                      MOVE  TB-STNCD (WK-I) TO P05-STNCD                VSB02480
                      MOVE  TB-SOKCD (WK-I) TO P05-SOKCD                VSB02510
                      MOVE  TB-KACD  (WK-I) TO P05-KACD                 VSB02490
                      MOVE  TB-SOKNO (WK-I) TO P05-SOKNO                VSB02520
                      MOVE  TB-SITEN (WK-I) TO P05-SITEN                VSB02530
                      MOVE  TB-SYOKA (WK-I) TO P05-SYOKA.               VSB02540
       M1A-EXIT.                                                        VSB02550
           EXIT.                                                        VSB02560
