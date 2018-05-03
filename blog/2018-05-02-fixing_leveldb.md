---
title: Fixing leveldb
---

I ran into an issue after a hard reset, where IPFS was failing to start. The
messages looked like this:

```
May 02 18:05:52 nixos systemd[1]: Starting IPFS Daemon...
May 02 18:05:52 nixos ipfs-pre-start[4042]: Lockfiles have been removed.
May 02 18:05:52 nixos ipfs-pre-start[4042]: Error: unable to open leveldb datastore: leveldb: manifest corrupted (field 'comparer'): missing [file=MANIFEST-800946]
May 02 18:05:52 nixos systemd[1]: ^[[0;1;39mipfs.service: Control process exited, code=exited status=1
May 02 18:05:52 nixos systemd[1]: Failed to start IPFS Daemon.
May 02 18:05:52 nixos systemd[1]: ^[[0;1;39mipfs.service: Unit entered failed state.
May 02 18:05:52 nixos systemd[1]: ^[[0;1;39mipfs.service: Failed with result 'exit-code'.
```

This appears to be caused by a corrupt leveldb database, in the `datastore` of
the `IPFS_PATH` (e.g. `/var/lib/ipfs`).

It turns out that there's
[a simple way to repair such a leveldb](http://smithfarm-thebrain.blogspot.co.uk/2016/11/how-to-repair-leveldb-database.html),
but it turns out that the required `leveldb` Python module isn't available on
NixOS 17.09, and I'm loathe to start messing with `pip` and friends.

Instead, I've cooked up the following Nix expression, which defines a script for
performing this repair:

```
with rec {
  nixpkgsSrc = (import <nixpkgs> { config = {}; }).fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "39cd40f";
    sha256 = "0kpx4h9p1lhjbn1gsil111swa62hmjs9g93xmsavfiki910s73sh";
  };

  nixpkgs = import nixpkgsSrc { config = {}; };

  python = with nixpkgs; pythonPackages.buildPythonPackage {
    name = "leveldb";
    src = fetchurl {
      url    = https://files.pythonhosted.org/packages/ec/c1/ca3b4199bd4073e6430076f1edd8061f2f548e831eeddc3cbc077ebaa0ca/leveldb-0.194.tar.gz;
      sha256 = "9c3378b3b4336cc63303e9fe5d054a337d50bafec80ac4628db19a598c0fcd38";
    };
  };
};

nixpkgs.writeScript "fix-leveldb.py" ''
  #!${python}/bin/python
  import leveldb
  import sys
  if len(sys.argv) < 2: raise Exception('Please give leveldb dir as argument')
  db = sys.argv[1]
  sys.stderr.write('Attempting to repair leveldb at {0}\n'.format(db))
  leveldb.RepairDB(db)
  sys.stderr.write('Finished processing {0}\n'.format(db))
''
```

This is mostly self-contained, since it uses a particular revision of the
nixpkgs repo (version 17.09), and hence 'pins' the requirements like Python.

To use this script, first build it:

```bash
$ nix-build a-file-containing-the-above.nix
...
/nix/store/some-complicated-hash-fix-leveldb.py
```

Now we can run this script, giving it the location of our corrupt datastore (you
should probably make a backup first!) and the permissions needed to mangle it:

```
$ sudo /nix/store/some-complicated-hash-fix-leveldb.py /var/lib/ipfs/datastore
```

This took a while to run for me, and afterwards I had to `chown` the datastore
files back to my `ipfs` user. I then restarted IPFS and it seems to be working
again:

```
May 02 18:41:38 nixos systemd[1]: Starting IPFS Daemon...
May 02 18:41:39 nixos ipfs-pre-start[24129]: Lockfiles have been removed.
May 02 18:41:43 nixos systemd[1]: Started IPFS Daemon.
May 02 18:41:44 nixos ipfs[24936]: Initializing daemon...
May 02 18:41:44 nixos ipfs[24936]: Adjusting current ulimit to 2048...
May 02 18:41:44 nixos ipfs[24936]: Successfully raised file descriptor limit to 2048.
May 02 18:41:48 nixos ipfs[24936]: Swarm listening on /ip4/.../tcp/4001
May 02 18:41:48 nixos ipfs[24936]: Swarm listening on /ip4/127.0.0.1/tcp/4001
May 02 18:41:48 nixos ipfs[24936]: API server listening on /ip4/127.0.0.1/tcp/5001
May 02 18:42:08 nixos ipfs[24936]: IPFS mounted at: /ipfs
May 02 18:42:08 nixos ipfs[24936]: IPNS mounted at: /ipns
May 02 18:42:08 nixos ipfs[24936]: Gateway (readonly) server listening on /ip4/127.0.0.1/tcp/8080
May 02 18:42:08 nixos ipfs[24936]: Daemon is ready
```
