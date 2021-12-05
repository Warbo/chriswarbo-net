with {
  rev = "7e9b0dff974c89e070da1ad85713ff3c20b0ca97";
};
builtins.fetchTarball {
  name   = "nixpkgs";
  url    = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";
}
