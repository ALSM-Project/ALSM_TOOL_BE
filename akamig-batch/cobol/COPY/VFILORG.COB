      *                                /* SOSHIKIZU     TBLFILE  */     MFI00010
      *                                /* DATE  87/03/06   V-001 */     MFI00020
           02 ORG-KEY.                                                  MFI00030
              03 ORG-DNEN              PIC  X(06).                      MFI00040
              03 ORG-BLKCD             PIC  X(02).                      MFI00050
              03 ORG-STNCD             PIC  X(03).                      MFI00060
              03 ORG-TENCD             PIC  X(03).                      MFI00070
              03 ORG-SKANO             PIC  X(03).                      MFI00080
           02 ORG-GPKNA                PIC  X(18).                      MFI00090
           02 ORG-GPKNJ                PIC  X(18).                      MFI00100
           02 ORG-STKNA                PIC  X(18).                      MFI00110
           02 ORG-STKNJ                PIC  X(18).                      MFI00120
           02 ORG-TNKNA                PIC  X(18).                      MFI00130
           02 ORG-TNKNJ                PIC  X(18).                      MFI00140
           02 ORG-KAKNA                PIC  X(18).                      MFI00150
           02 ORG-KAKNJ                PIC  X(18).                      MFI00160
           02 ORG-SGIGY                PIC  X(01).                      MFI00170
           02 ORG-KGIGY                PIC  X(01).                      MFI00180
           02 ORG-OTSEQ.                                                MFI00180
              03 ORG-GPSEQ             PIC  X(02).
              03 ORG-CKSEQ             PIC  X(01).
              03 ORG-STSEQ             PIC  X(02).
           02 FILLER                   PIC  X(02).                      MFI00190
