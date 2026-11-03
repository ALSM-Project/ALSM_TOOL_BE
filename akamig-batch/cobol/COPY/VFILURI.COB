      *                           /*  URIKAKEKIN  FILE            */    VFI00010
      *                           /*  LENGTH      850             */    VFI00010
      *                           /*  DATE  89/02/27   V-003      */    VFI00020
           02  URI-KEY.                                                 VFI00030
               03  URI-TOKCD.                                           VFI00040
                   04  URI-TKCD1        PIC  X(4).                      VFI00050
                   04  URI-TKCD2        PIC  X(3).                      VFI00060
               03  URI-NOSCD            PIC  X(2).                      VFI00070
           02  URI-SITEK                PIC  X(1).                      VFI00080
           02  URI-SUZUR.                                               VFI00090
               03  URI-DATYM.                                           VFI00090
                   04  URI-DATYY        PIC  9(4).                      VFI00100
                   04  URI-DATMM        PIC  9(2).                      VFI00110
               03  URI-ZMURI            PIC S9(11)   COMP-3.            VFI00120
               03  URI-ZMSEI            PIC S9(11)   COMP-3.            VFI00130
               03  URI-ICYO             PIC S9(11)   COMP-3.            VFI00140
               03  URI-TONEW.                                           VFI00150
                   04  URI-TOGEN        PIC S9(11)   COMP-3.            VFI00160
                   04  URI-TOKOG        PIC S9(11)   COMP-3.            VFI00170
                   04  URI-TOFKO        PIC S9(11)   COMP-3.            VFI00180
                   04  URI-TOFKA        PIC S9(11)   COMP-3.            VFI00190
                   04  URI-TOTEG        PIC S9(11)   COMP-3.            VFI00200
                   04  URI-TOSOS        PIC S9(11)   COMP-3.            VFI00210
               03  URI-TOHAT.                                           VFI00220
                   04  URI-TOET1        PIC S9(11)   COMP-3.            VFI00230
                   04  URI-TONE1        PIC S9(11)   COMP-3.            VFI00240
               03  URI-TOHAZ.                                           VFI00250
                   04  URI-TOET2        PIC S9(11)   COMP-3.            VFI00260
                   04  URI-TONE2        PIC S9(11)   COMP-3.            VFI00270
               03  URI-ZEHAZ.                                           VFI00280
                   04  URI-ZEET1        PIC S9(11)   COMP-3.            VFI00290
                   04  URI-ZENE1        PIC S9(11)   COMP-3.            VFI00300
               03  URI-ZETRN.                                           VFI00310
                   04  URI-ZENMM               OCCURS  12.              VFI00320
                       05  URI-ZENME    PIC S9(11)   COMP-3.            VFI00330
                       05  URI-ZENMN    PIC S9(11)   COMP-3.            VFI00340
           02  URI-TOKUR.                                               VFI00090
               03  URI-CDATY.                                           VFI00090
                   04  URI-CDTYY        PIC  9(4).                      VFI00100
                   04  URI-CDTMM        PIC  9(2).                      VFI00110
               03  URI-CZURI            PIC S9(11)   COMP-3.            VFI00120
               03  URI-CZSEI            PIC S9(11)   COMP-3.            VFI00130
               03  URI-CICYO            PIC S9(11)   COMP-3.            VFI00140
               03  URI-CTNEW.                                           VFI00150
                   04  URI-CTGEN        PIC S9(11)   COMP-3.            VFI00160
                   04  URI-CTKOG        PIC S9(11)   COMP-3.            VFI00170
                   04  URI-CTFKO        PIC S9(11)   COMP-3.            VFI00180
                   04  URI-CTFKA        PIC S9(11)   COMP-3.            VFI00190
                   04  URI-CTTEG        PIC S9(11)   COMP-3.            VFI00200
                   04  URI-CTSOS        PIC S9(11)   COMP-3.            VFI00210
               03  URI-CTHAT.                                           VFI00220
                   04  URI-CTET1        PIC S9(11)   COMP-3.            VFI00230
                   04  URI-CTNE1        PIC S9(11)   COMP-3.            VFI00240
               03  URI-CTHAZ.                                           VFI00250
                   04  URI-CTET2        PIC S9(11)   COMP-3.            VFI00260
                   04  URI-CTNE2        PIC S9(11)   COMP-3.            VFI00270
               03  URI-CZHAZ.                                           VFI00280
                   04  URI-CZET1        PIC S9(11)   COMP-3.            VFI00290
                   04  URI-CZNE1        PIC S9(11)   COMP-3.            VFI00300
               03  URI-CZTRN.                                           VFI00310
                   04  URI-CZNMM               OCCURS  12.              VFI00320
                       05  URI-CZNME    PIC S9(11)   COMP-3.            VFI00330
                       05  URI-CZNMN    PIC S9(11)   COMP-3.            VFI00340
           02  URI-SHZEI.                                               VFI00090
               03  URI-SSZUR.                                           VFI00090
                   04  URI-STHAT.                                       VFI00090
                       05  URI-STET1    PIC S9(11)   COMP-3.            VFI00100
                       05  URI-STNE1    PIC S9(11)   COMP-3.            VFI00100
                   04  URI-STHAZ.                                       VFI00120
                       05  URI-STET2    PIC S9(11)   COMP-3.            VFI00100
                       05  URI-STNE2    PIC S9(11)   COMP-3.            VFI00100
                   04  URI-SZHAZ.                                       VFI00120
                       05  URI-SZET1    PIC S9(11)   COMP-3.            VFI00100
                       05  URI-SZNE1    PIC S9(11)   COMP-3.            VFI00100
                   04  URI-SZTRN.                                       VFI00310
                       05  URI-SZNMM           OCCURS  12.              VFI00320
                           06 URI-SZNME PIC S9(11)   COMP-3.            VFI00330
                           06 URI-SZNMN PIC S9(11)   COMP-3.            VFI00340
               03  URI-STKUR.                                           VFI00090
                   04  URI-SCHAT.                                       VFI00090
                       05  URI-SCET1    PIC S9(11)   COMP-3.            VFI00100
                       05  URI-SCNE1    PIC S9(11)   COMP-3.            VFI00100
                   04  URI-SCHAZ.                                       VFI00090
                       05  URI-SCET2    PIC S9(11)   COMP-3.            VFI00100
                       05  URI-SCNE2    PIC S9(11)   COMP-3.            VFI00100
                   04  URI-SCZHA.                                       VFI00090
                       05  URI-SCZE1    PIC S9(11)   COMP-3.            VFI00100
                       05  URI-SCZN1    PIC S9(11)   COMP-3.            VFI00100
                   04  URI-SCZTR.                                       VFI00310
                       05  URI-SCZMM           OCCURS  12.              VFI00320
                           06 URI-SCZME PIC S9(11)   COMP-3.            VFI00330
                           06 URI-SCZMN PIC S9(11)   COMP-3.            VFI00340
