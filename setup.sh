ANTLR4_DOWNLOAD_DIR="$PWD"
## wget -N http://www.antlr.org/download/antlr-4.5.1-complete.jar
export CLASSPATH="${ANTLR4_DOWNLOAD_DIR}/antlr-4.5.1-complete.jar"

function antlr4 {
    java -jar "${ANTLR4_DOWNLOAD_DIR}/antlr-4.5.1-complete.jar" "$@"
}

function grun {
    CLASSPATH=".:$CLASSPATH" java org.antlr.v4.gui.TestRig "$@"
}
