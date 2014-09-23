#!/bin/sh
# Make a clean directory to work in
rm -rf /tmp/org2html
mkdir  /tmp/org2html
cd     /tmp/org2html

# Dump the given content in a file
cat /dev/stdin > __content.org

# Invoke Emacs to batch-process the file
emacs --batch \
      --eval "(progn (require 'org-install)
                     (require 'org)
                     (require 'org-exp)
                     (require 'ob)
                     (require 'ob-tangle)
                     (find-file \"/tmp/org2html/__content.org\")
                     (org-babel-tangle)
                     (org-html-export-to-html)
                     (kill-buffer))"

# Output the rendered content
cat __content.html
