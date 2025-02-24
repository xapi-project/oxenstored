# oxenstored

OCaml implementation of Xenstore - a database mapping filename-like pathname keys
to values. It is used to share configuration and status between Xen domains.

Clients may read and write values, watch for changes, set permissions to allow
or deny access, and use transactions to ensure atomicity of a set of changes.

## History

Oxenstored used to reside [in the upstream Xen repo](http://xenbits.xen.org/gitweb/?p=xen.git;a=tree;f=tools/ocaml).
During the 4.20 development cycle, it was forked and is now developed separately.

Oxenstored uses unstable interfaces to Xen, but a stable dynamically-linked
"plugin" was introduced to handle this ([xsd_glue](./xsd_glue)).
It allows oxenstored to be compiled once for several version of Xen, with only
the plugin needing to be recompiled.

This fork introduced several improvements on top of the upstream version:
* It implements the `XS_DIRECTORY_PARTIAL` call allowing to list large directories
* It's around 3-4 times faster and scales much better with the number of watches,
connections, and simultaneous calls
* It uses grants instead of the unstable foreign mapping interface (which is only
available in dom0)

## Documentation

Upstream Xen repository contains documentation on the Xenstore protocol and
conventions:

* [docs/misc/xenstore-paths.pandoc](https://xenbits.xenproject.org/docs/unstable/misc/xenstore-paths.html)
* [docs/misc/xenstore-ring.txt](https://xenbits.xen.org/docs/unstable/misc/xenstore-ring.txt)
* [docs/misc/xenstore.txt](https://xenbits.xen.org/docs/unstable/misc/xenstore.txt)

## Build and develop

```
# Install Xen headers (preferably 4.20+), distro-dependent
apt-get install -y libxen-dev

# Install dependencies
opam install . --deps-only --with-test -v

# Build
make

# Stricter checks
make check

# Format code
make format
```

## Using

Oxenstored runs as root in Dom0, and is usually handled as a systemd unit
(which in turn runs the
[`launch-xenstore`](https://xenbits.xen.org/gitweb/?p=xen.git;a=blob;f=tools/hotplug/Linux/launch-xenstore.in) script).

Interfacing with oxenstored (either from dom0 or domU) is usually done through
scripts or libraries, for example:

* [pyxs](https://pyxs.readthedocs.io/en/latest/index.html) for Python
* [ocaml-xenstore](https://ocaml.org/p/xenstore/latest) for OCaml
* [libxenstore](http://xenbits.xen.org/gitweb/?p=xen.git;a=tree;f=tools/libs/store) for C
* xenstore-{read,write,chmod,ls,list,rm,watch,control,exists} scripts. Note that
  these are good for quick sessions but they create a new connection for every
  invocation - it is much better to create a single persistent connection if a
  program is expected to interact with Xenstore throughout its lifecycle.

### Logging and debugging

Oxenstored logs some accesses to `xenstored-access.log` (usually located in `/var/log`).
Other daemon logs are available alongside it in `daemon.log`

If oxenstored was compiled with `allow_debug=true`, then one can send debug commands
to query the runtime state of the daemon or start particular actions:

```
$ xenstore-control compact
Compacted

$ xenstore-control watches
watch DOM0: backend FFFFFFFF825ABB80
watch DOM0: memory/target FFFFFFFF825AC520
watch DOM0: control/sysrq FFFFFFFF825AB5A0
watch DOM0: control/shutdown FFFFFFFF825AB560
watch DOM0: /local/domain/0/backend/qdisk FFFF88810A4F0190
watch DOM0: cpu FFFFFFFF825AB2C0
watch DOM0: device FFFFFFFF825ABDA0
watch DOM0 (ANON CONN #9): /local/domain/0/memory/target xcp-rrdd-plugins/squeezed:domain-0
watch DOM0 (ANON CONN #9): @releaseDomain
[....]

$ xenstore-control mfn 0
0

$ xenstore-control quota 0
dom0 quota: 44/8192
```

The full list of commands is available in [process.ml](https://github.com/xapi-project/oxenstored/blob/2953174c82c7ae9ad0e0fe9f8b38626729975eb9/oxenstored/process.ml#L270)

## Known incompatibilities with C Xenstore

* chmod for dead domains will work with O, but not with C
