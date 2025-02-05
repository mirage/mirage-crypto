#!/bin/sh
##########################################################################
# File: dktool/cmake/scripts/dkml/workflow/compilers-build-test.in.sh    #
#                                                                        #
# Copyright 2022 Diskuv, Inc.                                            #
#                                                                        #
# Licensed under the Apache License, Version 2.0 (the "License");        #
# you may not use this file except in compliance with the License.       #
# You may obtain a copy of the License at                                #
#                                                                        #
#     http://www.apache.org/licenses/LICENSE-2.0                         #
#                                                                        #
# Unless required by applicable law or agreed to in writing, software    #
# distributed under the License is distributed on an "AS IS" BASIS,      #
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or        #
# implied. See the License for the specific language governing           #
# permissions and limitations under the License.                         #
#                                                                        #
##########################################################################

# Updating
# --------
#
# 1. Delete this file.
# 2. Run dk with your original arguments:
#        ./dk dkml.workflow.compilers CI GitHub Desktop OS Windows
#    or get help to come up with new arguments:
#        ./dk dkml.workflow.compilers HELP

set -euf

# Set project directory
if [ -n "${CI_PROJECT_DIR:-}" ]; then
    PROJECT_DIR="$CI_PROJECT_DIR"
elif [ -n "${PC_PROJECT_DIR:-}" ]; then
    PROJECT_DIR="$PC_PROJECT_DIR"
elif [ -n "${GITHUB_WORKSPACE:-}" ]; then
    PROJECT_DIR="$GITHUB_WORKSPACE"
else
    PROJECT_DIR="$PWD"
fi
if [ -x /usr/bin/cygpath ]; then
    PROJECT_DIR=$(/usr/bin/cygpath -au "$PROJECT_DIR")
fi

# shellcheck disable=SC2154
echo "
=============
build-test.sh
=============
.
---------
Arguments
---------
$*
.
------
Matrix
------
dkml_host_abi=$dkml_host_abi
abi_pattern=$abi_pattern
opam_root=$opam_root
exe_ext=${exe_ext:-}
.
"

# PATH. Add opamrun
export PATH="$PROJECT_DIR/.ci/sd4/opamrun:$PATH"

# Initial Diagnostics (optional but useful)
opamrun switch
opamrun list
opamrun var
opamrun config report
opamrun option
opamrun exec -- ocamlc -config

# Update
opamrun update

# Build logic
#   2024-02-09: Remove mirage-crypto-pk on Windows since no portable GMP library (used by Zarith).
#   2024-02-24: Remove mirage-crypto-ec on Windows since it results in test failures.
packages_TOPOLOGICALSORT="mirage-crypto,mirage-crypto-rng,mirage-crypto-rng-mirage"
case "$dkml_host_abi" in
    windows_*)
        packages_TOPOLOGICALSORT="$packages_TOPOLOGICALSORT"
        ;;
    *)
        packages_TOPOLOGICALSORT="$packages_TOPOLOGICALSORT,mirage-crypto-pk,mirage-crypto-ec"
esac
#   shellcheck disable=SC2086
opamrun pin add -y -n .
opamrun install --yes --deps-only --with-test $(echo $packages_TOPOLOGICALSORT | tr ',' ' ')
opamrun exec -- dune build -p "$packages_TOPOLOGICALSORT"
opamrun exec -- dune runtest -p "$packages_TOPOLOGICALSORT"
