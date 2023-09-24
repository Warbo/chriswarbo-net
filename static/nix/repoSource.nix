# We host a bunch of git repos; look up their location from the environment
{ }:
with rec {
  inherit (builtins) getEnv pathExists trace;

  env = getEnv "GIT_REPO_DIR"; # User-provided override
  local = "/home/manjaro/repos"; # My local copy; fast
  remote = "http://chriswarbo.net/git"; # "Official" remote; default
};
if env != "" then
  env
else if pathExists local then
  local
else
  trace ''
    INFO: Getting repos from ${remote}, which may be slow
          as it's remote. You can override this by setting
          GIT_REPO_DIR.
  '' remote
