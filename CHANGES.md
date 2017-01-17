# 0.4.0 (???)

* Compatibility with MirageOS 3 module types.
* Add a benchmark.
* Obsolete `mirage-entropy-<BACKEND>`; the repository now contains only `mirage-entropy`.
* Support Unix, Xen, and Solo5 backends.
* Prune `oasis`.
* Move `noalloc` to 4.03-style annotations

# 0.3.0 (2015-05-02)

* Remove `mirage-entropy-unix` from the repository; it now only contains `mirage-entropy-xen`.
* Add internal entropy harvesting via timing and CPU RNG if available.
* Temporarily disable `xentropyd`.
* The API is no longer `V1.ENTROPY` compatible.

# 0.2.0 (09-Mar-2015)

* Do not wrap `Entropy_unix` in a functor as it is meant to be used directly.
* Xen: read entropy from a Xen PV device.  This is implemented by the `xentropyd` daemon.

# 0.1.6 (06-July-2014)

* Rework the module to be event-driven, more in line with entropy gathering.

# 0.1.5 (06-July-2014)

* Guarantee that all of the required entropy is read on Unix.
* Add a `Entropy_xen_weak` that uses the builtin `Random.self_init` as a measure of last-resort.

# 0.1.4 (04-July-2014)

* provide Mirage 1.2.0 interfaces (`V1_LWT.ENTROPY`).
* name modules `Entropy_xen` and `Entropy_unix` to not clash.

# 0.1.3 (03-July-2014)

* Unbreak build: ocamlfind wasn't able to locate the package previously.

# 0.1.2 (03-July-2014)

* Use `/dev/urandom` instead of `/dev/random` (for non-blocking behaviour).

# 0.1.1 (03-July-2014)

* Use Makefile instead of oasis as build system.

# 0.1.0 (03-July-2014)

* Initial release: on Unix, use `/dev/random`; on XEN, error out.
