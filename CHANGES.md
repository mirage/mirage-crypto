## (trunk)

* Change of the hashing interface, `Hash.S` (**Breaking**):
  - `t` is now immutable;
  - `feed` and `get` are therefore referentially transparent; and
  - `digestv` was replaced by `digesti`.
  Old behavior of `feed` and `get` can be replicated by wrapping a `t` in a
  reference cell. `digestv` can be obtained by combining `digesti` with
  `List.iter`.
* `Rsa.well_formed` can test if a triplet `(e, p, q)` looks enough like a key to
  be safe to use.
* Imported mirage-entropy v0.5.1 with the following changelog

### v0.5.1 (2020-02-24)

* use dune for building this package (#47 @hannesm)

### v0.5.0 (2019-11-01)

* Use mirage-runtime provided hooks (since 3.7.0), see mirage/mirage#1010 (#45 @samoht)
* Drop mirage-os-shim dependency (#45 @samoht)
* Raise minimum OCaml version to 4.06.0 (#46 @hannesm)

### 0.4.1 (2018-08-15)

* Aarch64 support (#39 by @reynir)

### 0.4.0 (2017-01-20)

* Compatibility with MirageOS 3 module types.
* Add a benchmark.
* Obsolete `mirage-entropy-<BACKEND>`; the repository now contains only `mirage-entropy`.
* Support Unix, Xen, and Solo5 backends.
* Prune `oasis`.
* Move `noalloc` to 4.03-style annotations

### 0.3.0 (2015-05-02)

* Remove `mirage-entropy-unix` from the repository; it now only contains `mirage-entropy-xen`.
* Add internal entropy harvesting via timing and CPU RNG if available.
* Temporarily disable `xentropyd`.
* The API is no longer `V1.ENTROPY` compatible.

### 0.2.0 (09-Mar-2015)

* Do not wrap `Entropy_unix` in a functor as it is meant to be used directly.
* Xen: read entropy from a Xen PV device.  This is implemented by the `xentropyd` daemon.

### 0.1.6 (06-July-2014)

* Rework the module to be event-driven, more in line with entropy gathering.

### 0.1.5 (06-July-2014)

* Guarantee that all of the required entropy is read on Unix.
* Add a `Entropy_xen_weak` that uses the builtin `Random.self_init` as a measure of last-resort.

### 0.1.4 (04-July-2014)

* provide Mirage 1.2.0 interfaces (`V1_LWT.ENTROPY`).
* name modules `Entropy_xen` and `Entropy_unix` to not clash.

### 0.1.3 (03-July-2014)

* Unbreak build: ocamlfind wasn't able to locate the package previously.

### 0.1.2 (03-July-2014)

* Use `/dev/urandom` instead of `/dev/random` (for non-blocking behaviour).

### 0.1.1 (03-July-2014)

* Use Makefile instead of oasis as build system.

### 0.1.0 (03-July-2014)

* Initial release: on Unix, use `/dev/random`; on XEN, error out.

## v0.5.4 2017-01-31:
* Relicense from BSD2 to ISC.
* Support MirageOS 3.0.
* Replace OASIS with topkg.
* Stricter base64 decoding.

## v0.5.3 2016-03-21:
* Move from Camlp4 to PPX.
* Tweaked the supporting Cstruct module's API.
* Dh.shared returns option instead of throwing if the public message is degenerate.
* Base64.decode returns option instead of throwing

## 0.5.2 2015-12-03:
* Avoid including intrinsics-related headers if SSE/AES-NI is disabled.
* Replace opam variable `nocrypto-inhibit-modernity` with `$NOCRYPTO_NO_ACCEL`.
* Remove cstruct equality.

## 0.5.1 2015-07-07:
* Disable AES-NI if not supported in the `./configure` step.
* Support the global opam variable `nocrypto-inhibit-modernity`.

## 0.5.0 2015-07-02:
* Support for AES-NI and SSE2.
* Support RSA-OAEP and RSA-PSS.
* Drop `ctypes` for internal C calls.
* Generate smaller secret exponents for DH, making operations on large groups much faster.
* Support dynamic switching of RNG algorithms and decouple `Rng` from `Fortuna`.
* Module for injectring entropy into RNG on pure Unix (optional).
* `Nocrypto_entropy_lwt.initialize` no longer needs to be synchronized on.
* Renamed module signatures and modules containing only signatures from `T` to `S`.
* Changes to `CTR`, `CBC`, `Rsa` and `Dh` APIs.

## 0.4.0 2015-05-02:
* Module for injecting entropy into RNG on Unix/Lwt (optional).
* Module for injecting entropy into RNG on Mirage/Xen (optional; depends on mirage-entropy-xen).
* API changes in `Rng`.
* Do not 0-pad DH public and shared representations.
* More named DH groups.

## 0.3.1 2015-02-01:
* Support for Mirage/Xen (contributed by Thomas Leonard <talex5@gmail.com>).

## 0.3.0 2014-12-21:
* Removed ad-hoc key marshalling functions as key material typically comes non-trivially encoded anyways.
* Changed how module interfaces for the packed module are handled: `module type of` constructs are gone.
* More consistent errors in `Rsa`.
* Small API breakage here and there.

## 0.2.2 2014-11-04:
* Replaced hashing sources with the ones from hs-cryptohash
  (https://github.com/vincenthz/hs-cryptohash) by Vincent Hanquez.
* Renamed various symbols likely to conflict with other crypto libraries.

## 0.2.0 2014-10-30:
* DSA (initial version contributed by Hannes Mehnert <hannes@mehnert.org>).
* CCM mode for AES (contributed by Hannes Mehnert <hannes@mehnert.org>).
* Switched from hand written stubs to ctypes for intefacing with the C code.
* Packed the module to avoid clobbering global namespace; some modules renamed.
* Various bugfixes and improvements.

## 0.1.0 2014-07-08:
* Initial (beta) release.
