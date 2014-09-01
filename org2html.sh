#!/bin/sh
# Make a clean directory to work in
rm -rf /tmp/org2html
mkdir  /tmp/org2html

# Dump the given content in a file
cat /dev/stdin > /tmp/org2html/__content.org

# Invoke Emacs to batch-process the file
emacs --batch \
      --eval "(progn (require 'org)
                     (require 'org-exp)
                     (require 'ob)
                     (require 'ob-tangle)
                     (find-file \"/tmp/org2html/__content.org\")
                     (org-babel-tangle)
                     (kill-buffer))" \
      --visit=/tmp/org2html/__content.org \
      --funcall org-export-as-html-batch

# Output the rendered content
cat /tmp/org2html/__content.html
