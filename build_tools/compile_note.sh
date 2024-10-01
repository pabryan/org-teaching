#!/bin/bash

notename="${1}"
exportnotesdir="${2}"
template="${3}"

latexmk --outdir="${exportnotesdir}" -g -jobname="${notename}" -pdf -pdflatex="pdflatex %O -interaction=nonstopmode -synctex=1 '\def\notes_defn{${exportnotesdir}/${notename}-spec.tex} \input{%S}'" -cd "${template}" > /dev/null
