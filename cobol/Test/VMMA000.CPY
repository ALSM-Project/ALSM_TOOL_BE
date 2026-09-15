       01  VMMA000I.
           02  FILLER PIC X(12).
           02  ADATEL    COMP  PIC  S9(4).
           02  ADATEF    PICTURE X.
           02  FILLER REDEFINES ADATEF.
             03 ADATEA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ADATEI  PIC X(8).
           02  ASYORIL    COMP  PIC  S9(4).
           02  ASYORIF    PICTURE X.
           02  FILLER REDEFINES ASYORIF.
             03 ASYORIA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  ASYORII  PIC X(2).
           02  AIDNOL    COMP  PIC  S9(4).
           02  AIDNOF    PICTURE X.
           02  FILLER REDEFINES AIDNOF.
             03 AIDNOA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  AIDNOI  PIC X(8).
           02  APASWDL    COMP  PIC  S9(4).
           02  APASWDF    PICTURE X.
           02  FILLER REDEFINES APASWDF.
             03 APASWDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  APASWDI  PIC X(15).
           02  AMSGCDL    COMP  PIC  S9(4).
           02  AMSGCDF    PICTURE X.
           02  FILLER REDEFINES AMSGCDF.
             03 AMSGCDA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  AMSGCDI  PIC X(6).
           02  AMSGL    COMP  PIC  S9(4).
           02  AMSGF    PICTURE X.
           02  FILLER REDEFINES AMSGF.
             03 AMSGA    PICTURE X.
           02  FILLER   PICTURE X(4).
           02  AMSGI  PIC X(40).
       01  VMMA000O REDEFINES VMMA000I.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  ADATEC    PICTURE X.
           02  ADATEP    PICTURE X.
           02  ADATEH    PICTURE X.
           02  ADATEV    PICTURE X.
           02  ADATEO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  ASYORIC    PICTURE X.
           02  ASYORIP    PICTURE X.
           02  ASYORIH    PICTURE X.
           02  ASYORIV    PICTURE X.
           02  ASYORIO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  AIDNOC    PICTURE X.
           02  AIDNOP    PICTURE X.
           02  AIDNOH    PICTURE X.
           02  AIDNOV    PICTURE X.
           02  AIDNOO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  APASWDC    PICTURE X.
           02  APASWDP    PICTURE X.
           02  APASWDH    PICTURE X.
           02  APASWDV    PICTURE X.
           02  APASWDO  PIC X(15).
           02  FILLER PICTURE X(3).
           02  AMSGCDC    PICTURE X.
           02  AMSGCDP    PICTURE X.
           02  AMSGCDH    PICTURE X.
           02  AMSGCDV    PICTURE X.
           02  AMSGCDO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  AMSGC    PICTURE X.
           02  AMSGP    PICTURE X.
           02  AMSGH    PICTURE X.
           02  AMSGV    PICTURE X.
           02  AMSGO  PIC X(40).
