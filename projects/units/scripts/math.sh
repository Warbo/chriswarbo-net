#!/usr/bin/env bash
set -eu

# Rendering defaults
NEG=bar
CONTEXT=inline

# Allow defaults to be overridden by arguments
for ARG in "$@"
do
    case "$ARG" in
        minus) NEG=minus ;;
        bar) NEG=bar ;;
        block) CONTEXT=block ;;
        inline) CONTEXT=inline ;;
    esac
done

# All MathML must occur in a math element with this namespace
wrap() {
    echo '<math xmlns="http://www.w3.org/1998/Math/MathML">'
    cat
    echo '</math>'
}

# Format negatives: defaults to using overbars, but may be overridden by giving
# this script the argument 'minus'
negs() {
    if [[ "$NEG" = 'bar' ]]
    then
        # Use custom stylesheet to transform unary_minus into overbar
        xsltproc "$BARS" -
    else
        # Leave unary_minus as-is, so CTOP stylesheet will render it "normally"
        cat
    fi
}

# Convert Content MathML to Presentation MathML
# TODO: Add a semantic element with the original Content MathML
format() {
    # The $OURS stylesheet lets us tweak the presentation unconditionally. We
    # apply it twice, so it can preprocess the Content MathML and postprocess
    # Presentation MathML. The $CTOP stylesheet comes from the W3C examples, and
    # converts C(ontent MathML) to P(resentation MathML).
    wrap | xsltproc "$OURS" - | negs | xsltproc "$CTOP" - | xsltproc "$OURS" -
}

# Makes Presentation MathML more browser-compatible
fixup() {
    # Run through $OURS stylesheet again, so it can fix up any generated
    # Presentation MathML.
    xsltproc "$OURS" - |
    sed -e 's@<?xml[^>]*?>@@g' \
        -e 's@<m:@<@g' \
        -e 's@</m:@</@g' \
        -e 's@xmlns:m=.http://www.w3.org/1998/Math/MathML.@@g' | tr -d '\n'
}

echo "BEGIN math.sh" 1>&2

MATHS=$(format | fixup)
echo "MATHS='$MATHS'" 1>&2

# Convert to Pandoc JSON. We can't just pipe it into Pandoc, since that will try
# to convert the MathML into bits of LaTeX which aren't adequate. Instead, we
# give Pandoc some raw inline HTML (a </span> tag), then swap that in the output
# MathML.
if [[ "$CONTEXT" = 'block' ]]
then
    JQ='.blocks[0].c[1] |= $maths'
    DUMMY='</div>'
else
    JQ='.blocks[0].c[0].c[1] |= $maths'
    DUMMY='</span>'
fi

echo "$DUMMY" | pandoc -f html+raw_html -t json |
    jq --arg maths "$MATHS" "$JQ" | tee >(cat 1>&2)
echo "END math.sh" 1>&2
