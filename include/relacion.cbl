      * Tabla RELACION-CLIENTE-CUENTA.
       01  REG-RELACION.
           05  RELACION-CLI-ID            PIC 9(10).
           05  RELACION-CTA-ID            PIC 9(10).
           05  RELACION-RLN               PIC X(05).

       01  REG-RELACION-NULL.
           05  RELACION-CLI-ID-NULL       PIC S9(04) COMP-5.
           05  RELACION-CTA-ID-NULL       PIC S9(04) COMP-5.
           05  RELACION-RLN-NULL          PIC S9(04) COMP-5.
