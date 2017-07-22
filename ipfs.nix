{ allPages, attrsToDirs, bins, commands, isPath, lib, repoName, repoUrls,
  rsync, runCommand, wrap }:

with builtins;
with lib;
rec {
  # Takes a set of { name1 = ipfsHash1; name2 = ipfsHash2; ... } values and
  # returns (the hash of) an IPFS object representing a directory, where each
  # `name` is an entry in the directory and each `ipfsHash` is the content
  # (file/directory) stored at that name.
  attrsToIpfs = attrs:
    with rec {
      addCmd = name: hash: ''
        if [[ -f "${hash}" ]]
        then
          THISHASH=$(cat "${hash}")
        else
          THISHASH="${hash}"
        fi
        RESULT=$(ipfs object patch "$RESULT" add-link "${name}" "$THISHASH")
        unset THISHASH
      '';

      addCmds = concatStringsSep "\n" (mapAttrsToList addCmd attrs);
  };
  runCommand "hashed-git-dir" (withIpfs {}) ''
    RESULT=$(ipfs object new unixfs-dir)
    ${addCmds}
    echo "$RESULT" > "$out"
  '';

  ipfsHash = attrsToIpfs allPageHashes;

    ipfsHashOf = name: content: runCommand "ipfs-hash-${name}"
    (withIpfs {
      buildInputs = [ rsync ];
      content = if isPath content || isDerivation content
                   then content
                   else if isAttrs content
                           then attrsToDirs content
                           else abort "Not path or attrs";
    })
    ''
      echo "Adding ${name} to IPFS" 1>&2

      F=$(basename "$content")

      # Dereference symlinks
      if [[ -f "$(readlink -f "$content")" ]]
      then
        SRC="$content"
      else
        SRC="$content"/
      fi
      rsync -a --copy-links "$SRC" "$F"

      ipfs add -q -r "$F" | tail -n1 > "$out"
      echo "Finished adding ${name} to IPFS" 1>&2
    '';

  withIpfs =
    with rec {
      path         = /run/current-system/sw/bin/ipfs;
      sys          = pathExists path;
      file         = if sys
                        then trace "Using system's IPFS binary, for compatibility"
                             toString path
                        else trace "No system-wide IPFS; beware incompatibility"
                             "${ipfs}/bin/ipfs";
      matchingIpfs = bins { ipfs = wrap { inherit file; }; };
    };
    env: env // {
      buildInputs = (env.buildInputs or []) ++ [ matchingIpfs ];
      IPFS_PATH   = "/var/lib/ipfs/.ipfs";
    };

  ipfsKeys =
    with rec {
      env  = withIpfs { inherit (builtins) currentTime; };
      keys = runCommand "ipfs-keys.nix" env ''
        {
          echo '{'
          ipfs key list -l | while read -r PAIR
          do
            NAME=$(echo "$PAIR" | cut -d' ' -f2)
             VAL=$(echo "$PAIR" | cut -d' ' -f1)
            printf '"%s" = "%s";\n' "$NAME" "$VAL"
          done
          echo '}'
        } > "$out"
      '';
    }; import keys;

  gitRedirect = repo: runCommand "redirect-${repo}-to-ipns"
    {
      inherit repo;
      key         = ipfsKeys."${repo}";
      buildInputs = [ commands.mkRedirectTo ];
    }
    ''
      RESULT=$(mkRedirectTo "https://ipfs.io/ipns/$key")
      echo "$RESULT" > "$out"
    '';

  gitRedirects = listToAttrs (map (url: let name = repoName url;
                                         in {
                                           inherit name;
                                           value = {
                                             "index.html" = gitRedirect name;
                                           };
                                         })
                                  repoUrls);

  allPageHashes = mapAttrs ipfsHashOf (allPages // { git = gitRedirects; });
}
