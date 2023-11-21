      * Tabla CUENTA.
       01  REG-CUENTA.
           05  CUENTA-ID                PIC 9(10).
           05  CUENTA-NUM               PIC X(20).
           05  CUENTA-SALDO             PIC S9(08)V99 LEADING SEPARATE.

       01  REG-CUENTA-NULL.
           05  CUENTA-ID-NULL           PIC S9(04) COMP-5.
           05  CUENTA-NUM-NULL          PIC S9(04) COMP-5.
           05  CUENTA-SALDO-NULL        PIC S9(04) COMP-5.
