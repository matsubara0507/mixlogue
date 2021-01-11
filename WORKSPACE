# Give your project a name. :)
workspace(name = "mdium")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive"
)

# For Docker:
#  rules_docker is depends on rules_python 0.1.0.
#  But, rules_haskell_dependencies in rules_haskell install rules_python 0.0.1.
#  So, install rules_python 0.1.0 before exec rules_haskell_dependencies.
http_archive(
    name = "rules_python",
    sha256 = "b6d46438523a3ec0f3cead544190ee13223a52f6a6765a29eae7b7cc24cc83a0",
    urls = ["https://github.com/bazelbuild/rules_python/releases/download/0.1.0/rules_python-0.1.0.tar.gz"],
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-3b8182ca5287cf93687fff1cefd98910f683b679",
    urls = ["https://github.com/tweag/rules_haskell/archive/3b8182ca5287cf93687fff1cefd98910f683b679.tar.gz"],
    sha256 = "85f269adfecfc5760fae6017608f7efebfccb719c22c7e71af03c4887f54b08e",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot"
)

stack_snapshot(
    name = "stackage",
    packages = [
        "base",
        "rio",
        "aeson",
        "blaze-html",
        "dotenv",
        "elmap",
        "extensible",
        "extensible-elmap",
        "fallible",
        "http-api-data",
        "mix",
        "mix-json-logger",
        "req",
        "servant-blaze",
        "servant-server",
        "slackell",
        "unliftio",
        "warp",
    ],
    # Last snapshot published for ghc-8.6.5 the default version picked up by
    # rules_haskell
    # snapshot = "lts-14.27",
    local_snapshot = "//:stack-snapshot.yaml",
    # This uses an unpinned version of stack_snapshot, meaning that stack is invoked on every build.
    # To switch to pinned stackage dependencies, run `bazel run @stackage-unpinned//:pin` and
    # uncomment the following line.
    # stack_snapshot_json = "//:stackage_snapshot.json",
)

# Download a GHC binary distribution from haskell.org and register it as a toolchain.
rules_haskell_toolchains(version = "8.8.4")
