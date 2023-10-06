{ bash, inNixedDir, wrap, xmlstarlet }:

wrap {
  name = "pushGitPages";
  paths = [ inNixedDir xmlstarlet ];
  file = ./pushGitPages.sh;
}
