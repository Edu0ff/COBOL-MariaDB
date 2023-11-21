# Compilador.
COBC=cobc
COBC_OPTS=-freference-out-of-declaratives=ok -fstatic-call
COBC_DIR_COPY=-I ./copy

# Precompilador (traduce código SQL a llamadas a subprogramas COBOL).
ESQLOC=esqlOC
ESQLOC_OPTS=-Q -static
ESQLOC_DIR_INCL=-I ./include

# Librerías dinámicas.
LIB_OCSQL=-locsql

all: prbbanco banco prbaltacliente informe

informe: informe.cob conexmdb.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} ${LIB_OCSQL} -x -o $@ $?
	
informe.cob: informe.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

prbaltacliente: prbaltacliente.cob altacliente.o conexmdb.o busqcli.o busqdom.o busqcta.o busqtar.o busqmov.o busqrln.o inscli.o insdom.o inscta.o instar.o insrln.o insmov.o
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} ${LIB_OCSQL} -x -o $@ $?

prbaltacliente.cob: prbaltacliente.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

banco: banco.cob altacliente.cob conexmdb.o busqcli.o busqdom.o busqcta.o busqtar.o busqmov.o busqrln.o inscli.o insdom.o inscta.o instar.o insrln.o insmov.o
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} ${LIB_OCSQL} -x -o $@ $?

banco.cob: banco.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

altacliente.o: altacliente.cob conexmdb.o busqcli.o busqdom.o busqcta.o busqtar.o busqmov.o busqrln.o inscli.o insdom.o inscta.o instar.o insrln.o insmov.o
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?

altacliente.cob: altacliente.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

prbbanco: prbbanco.cob conexmdb.o busqcli.o busqdom.o busqcta.o busqtar.o busqmov.o busqrln.o inscli.o insdom.o inscta.o instar.o insrln.o insmov.o
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} ${LIB_OCSQL} -x -o $@ $?

prbbanco.cob: prbbanco.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

conexmdb.o: conexmdb.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?
	
conexmdb.cob: conexmdb.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

busqcli.o: busqcli.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?
	
busqcli.cob: busqcli.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null
	
busqdom.o: busqdom.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?
	
busqdom.cob: busqdom.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null
	
busqcta.o: busqcta.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?
	
busqcta.cob: busqcta.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null
	
busqtar.o: busqtar.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?
	
busqtar.cob: busqtar.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

busqmov.o: busqmov.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?
	
busqmov.cob: busqmov.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

busqrln.o: busqrln.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?
	
busqrln.cob: busqrln.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

inscli.o: inscli.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?

inscli.cob: inscli.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

insdom.o: insdom.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?

insdom.cob: insdom.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

inscta.o: inscta.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?

inscta.cob: inscta.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

instar.o: instar.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?

instar.cob: instar.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

insmov.o: insmov.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?

insmov.cob: insmov.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

insrln.o: insrln.cob
	${COBC} ${COBC_OPTS} ${COBC_DIR_COPY} -c $?

insrln.cob: insrln.cbl
	${ESQLOC} ${ESQLOC_OPTS} ${ESQLOC_DIR_INCL} -o $@ $? > /dev/null

clean:
	rm prbbanco altacliente banco prbaltacliente informe *.cob *.o
