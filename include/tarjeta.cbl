      * Tabla TARJETA.
       01  REG-TARJETA.
           05  TARJETA-ID-TAR            PIC 9(10).
           05  TARJETA-ID-CLI            PIC 9(10).
           05  TARJETA-ID-CTA            PIC 9(10).
           05  TARJETA-NUM               PIC X(16).
           05  TARJETA-CRED              PIC S9(08)V99 LEADING SEPARATE.
           05  TARJETA-FEC               PIC 9(06).
           05  TARJETA-FEC-X             REDEFINES 
               TARJETA-FEC               PIC X(06).
           05  TARJETA-CCV               PIC X(03).
           
       01  REG-TARJETA-NULL.
           05  TARJETA-ID-TAR-NULL       PIC S9(04) COMP-5.
           05  TARJETA-ID-CLI-NULL       PIC S9(04) COMP-5.
           05  TARJETA-ID-CTA-NULL       PIC S9(04) COMP-5.
           05  TARJETA-NUM-NULL          PIC S9(04) COMP-5.
           05  TARJETA-CRED-NULL         PIC S9(04) COMP-5.
           05  TARJETA-FEC-NULL          PIC S9(04) COMP-5.
           05  TARJETA-CCV-NULL          PIC S9(04) COMP-5.