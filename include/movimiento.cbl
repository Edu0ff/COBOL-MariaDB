      * Tabla MOVIMIENTOS.
       01  REG-MOVIMIENTO.
           05  MOVIMIENTO-ID                PIC 9(10).
           05  MOVIMIENTO-FEC               PIC 9(20).
           05  MOVIMIENTO-FEC-X             REDEFINES MOVIMIENTO-FEC
                                            PIC X(20).
           05  MOVIMIENTO-CPT               PIC X(49).
           05  MOVIMIENTO-IMPT              PIC S9(08)V99 
                                            LEADING SEPARATE.
           
       01  REG-MOVIMIENTO-NULL.
           05  MOVIMIENTO-ID-NULL           PIC S9(04) COMP-5.
           05  MOVIMIENTO-FEC-NULL          PIC S9(04) COMP-5.
           05  MOVIMIENTO-CPT-NULL          PIC S9(04) COMP-5.
           05  MOVIMIENTO-IMPT-NULL         PIC S9(04) COMP-5.
