      * Tabla DOMICILIO.
       01  REG-DOMICILIO.
           05  DOMICILIO-ID              PIC 9(10).
           05  DOMICILIO-CALLE           PIC X(45).
           05  DOMICILIO-NUM             PIC X(03).
           05  DOMICILIO-PROV            PIC X(16).
           05  DOMICILIO-POBL            PIC X(16).
           05  DOMICILIO-COD-POS         PIC X(05).
           
       01  REG-DOMICILIO-NULL.
           05  DOMICILIO-ID-NULL         PIC S9(04) COMP-5.
           05  DOMICILIO-CALLE-NULL      PIC S9(04) COMP-5.
           05  DOMICILIO-NUM-NULL        PIC S9(04) COMP-5.
           05  DOMICILIO-PROV-NULL       PIC S9(04) COMP-5.
           05  DOMICILIO-POBL-NULL       PIC S9(04) COMP-5.
           05  DOMICILIO-COD-POS-NULL    PIC S9(04) COMP-5.
