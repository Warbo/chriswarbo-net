{ attrsToDirs, bash, inNixedDir, ipfs, runCommand, wrap, writeScript }:

with rec {
  ipfsBin = wrap {
    name = "ipfsBin";
    script = ''
      #!${bash}/bin/bash
      if command -v ipfs > /dev/null
      then
        ipfs "$@"
      else
        echo "Can't find 'ipfs', using potentially incompatible fallback" 1>&2
        "${ipfs}/bin/ipfs" "$@"
      fi
    '';
  };
};
wrap {
  name = "git2ipfs";
  paths = [ (attrsToDirs { bin = { inherit ipfsBin; }; }) inNixedDir ];
  file = ./git2ipfs.sh;
}
