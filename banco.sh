#!/bin/bash

export DIR_BIN=/home/eduarda/Documents/Programación/COBOL/Proyecto_Banco
export DIR_DAT=/home/eduarda/Documents/Programación/COBOL/Proyecto_Banco/dat
export DIR_LOG=/home/eduarda/Documents/Programación/COBOL/Proyecto_Banco/logs

export LST_FICH_DAT=$(ls "${DIR_DAT}"/clientes-banco-*)

for FICH in ${LST_FICH_DAT}; do
    export FICH_DAT=${FICH}
    export CASO_PRUEBA=$(basename --suffix=.dat ${FICH_DAT})
    export FICH_LOG=${DIR_LOG}'/'${CASO_PRUEBA}'.log'
    ${DIR_BIN}/banco.cbl > ${FICH_LOG}
    echo ${CASO_PRUEBA}':' $?
done
