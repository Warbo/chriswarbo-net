#!/usr/bin/env bash
set -eu

# Rendering defaults
NEG=bar
CONTEXT=inline
SEM=1

# Allow defaults to be overridden by arguments
for ARG in "$@"
do
    case "$ARG" in
         minus)     NEG=minus  ;;
           bar)     NEG=bar    ;;
         block) CONTEXT=block  ;;
        inline) CONTEXT=inline ;;
         nosem)     SEM=0      ;;
           sem)     SEM=1      ;;
    esac
done

# Capture the markup we've been given on stdin
GIVEN=$(cat)

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
    sed -e 's@<?xml[^>]*?>@@g' \
        -e 's@<m:@<@g' \
        -e 's@</m:@</@g' \
        -e 's@xmlns:m=.http://www.w3.org/1998/Math/MathML.@@g' | tr -d '\n'
}

# Adds the original (untransformed) markup as a semantic annotation
semantics() {
    if [[ "$SEM" -eq 0 ]]
    then
        # Don't add semantics, just stick with the presentation markup
        cat
    else
        # Insert an opening <semantics> tag after the opening <math> tag, and
        # (temporarily) remove the closing </math> tag
        sed -e 's@\(<math[^>]*>\)@\1<semantics>@g' \
            -e 's@</math>@@g'

        # Add the given markup (assumed to be Content MathML) as an annotation
        printf '<annotation-xml encoding="MathML-Content">%s</annotation-xml>' \
               "$GIVEN"

        # Close everything off
        printf '</semantics></math>'
    fi
}

echo "BEGIN math.sh" 1>&2

MATHS=$(echo "$GIVEN" | format | fixup | semantics)
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
