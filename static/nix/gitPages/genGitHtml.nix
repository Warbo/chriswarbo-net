{ artemis, bash, fail, git, git2html, mhonarc, pandoc, python3, runCommand
, withDeps, wrap, writeScript }:

with rec {
  script = wrap {
    name = "genGitHtml";
    paths = [ fail git git2html mhonarc pandoc ];
    vars = {
      splicer = wrap {
        name = "splicer";
        paths = [ python3 ];
        file = ./splicer.py;
      };

      cleaner = with { py = python3.withPackages (p: [ p.bleach ]); };
        wrap {
          name = "cleaner.py";
          paths = [ py ];
          file = ./cleaner.py;
        };
    };
    file = ./genGitHtml.sh;
  };

  test = runCommand "genGitHtml-test" {
    inherit script;
    buildInputs = [ artemis fail git git2html mhonarc pandoc ];
    EDITOR = wrap {
      name = "test-editor";
      script = ''
        #!${bash}/bin/bash
        sed -i -e "s@^Subject: .*@Subject: $SUBJECT@g" "$1"
        sed -i -e "s@Detailed description.@$BODY@g"    "$1"
      '';
    };
    testReadme = ''
      # Title 1 #

      Some text.

      ## Title 2 ##

      A [link](http://example.org).

      <script type="text/javascript">alert("XSS");</script>
    '';
  } "${./genGitHtml-test.sh}";
};
withDeps [ test ] script
