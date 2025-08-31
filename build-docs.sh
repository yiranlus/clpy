#!/usr/bin/env bash

# This script is used to build the documentation.

set -e

cd "$(dirname "$0")"

rm -f docs/references/clpy/clpy*
rm -f docs/references/clthon/clthon*

qlot exec ros \
  --eval "(ql:quickload :clpy/doc-exporter :silent t)" \
  --eval "(asdf:operate 'clpy.doc-exporter:generate-doc-op 'clpy/doc-exporter)"

cd docs
make clean & make html
cd ..
