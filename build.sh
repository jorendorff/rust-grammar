#!/bin/bash
set -eu

. ./setup.sh
alias antlr4='java -jar ${ANTLR4_DOWNLOAD_DIR}/antlr-4.5.1-complete.jar'
alias grun='CLASSPATH=".:$CLASSPATH" java org.antlr.v4.gui.TestRig'

antlr4 Rust.g4
javac Rust*.java
grun Rust crate -tree
