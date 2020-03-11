{
  fetchgit   ? ((import <nixpkgs> {}).fetchgit),
  repoSource ? "http://chriswarbo.net/git"
}:

fetchgit {
  url    = "${repoSource}/warbo-packages.git";
  rev    = "2ad4989";
  sha256 = "0x2h5zw7z5fsxwn13ijgk0gq82140swr4sv40ip7jhrc7w7gh2xq";
}
