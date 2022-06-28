# Contribution Guide

Contributions and issue reports are encouraged and appreciated!

- [Opening Issues](#opening-issues)
- [Submitting Changes](#submitting-changes)
  - [Guidelines for Commit Messages](#guidelines-for-commit-messages)
  - [Guidelines for Pull Requests](#guidelines-for-pull-requests)
  - [Code Quality](#code-quality)
  - [Documentation](#documentation)

## Opening Issues

Before opening an issue, please check whether your issue has already been reported. Assuming it has not:

* Describe the issue you're encountering or the suggestion you're making
* Include any relevant steps to reproduce or code samples you can. It's always easier for us to debug if we have something that demonstrates the error.
* Let us know what version you were using. If you're using a github checkout, provide the git hash.
* Describe how you're building (i.e., via reflex-platform, cabal install, stack, obelisk, etc.). If you're using reflex-platform or obelisk, provide the git hash of your checkout.

## Submitting Changes

### Guidelines for Commit Messages

#### Summary Line
The summary line of your commit message should summarize the changes being made. Commit messages should be written in the imperative mood and should describe what happens when the commit is applied.

One way to think about it is that your commit message should be able to complete the sentence:
"When applied, this commit will..."

#### Body
For breaking changes, new features, refactors, or other major changes, the body of the commit message should describe the motivation behind the change in greater detail and may include references to the issue tracker. The body shouldn't repeat code/comments from the diff.

### Guidelines for Pull Requests

Wherever possible, pull requests should add a single feature or fix a single bug. Pull requests should not bundle several unrelated changes.

### Code Quality

#### Warnings

Your pull request should add no new warnings to the project. It should also generally not disable any warnings.

#### Build and Test

Make sure the project builds and that the tests pass! This will generally also be checked by CI before merge, but trying it yourself first means you'll catch problems earlier and your contribution can be merged that much sooner!

To run the same build as our CI server, you can execute `nix-build release.nix`.

##### Running the tests in a VM

To run the integration tests, you can execute `nix-build test.nix`. This will build and run a [NixOS virtual machine](https://nixos.org/guides/integration-testing-using-virtual-machines.html) with postgres and a few beam-automigrate executables (e.g., `readme` and the other executables defined in the [beam-automigrate.cabal](beam-automigrate.cabal)). Those executables will be run inside the VM to verify that they successfully produce the expected database migrations.

To learn more about the NixOS virtual machine testing framework, including how to debug tests interactively, check out the [NixOS manual section on integration testing](https://nixos.org/manual/nixos/stable/index.html#sec-nixos-tests).

##### Running the tests outside a VM

The integration tests can be run outside of the VM by running the following commands:

```bash
nix-shell
cabal repl exe:beam-automigrate-integration-tests
:main --with-gargoyle test-db
```

This will spin up a local postgresql database using gargoyle in the folder "test-db" (the name of this folder isn't important - you can replace it with whatever you like).

##### Adding New Tests

New tests should be added for every breaking change, including bug fixes. Tests can be added by defining an executable that runs a migration that demonstrates the error, and then adding that executable to [test.nix](test.nix) so that it is run by the CI server. The failing test case should be committed separately from (and, if possible, prior to) the fix that causes the test case to pass.

#### Dependencies

Include version bounds whenever adding a dependency to the library stanza of the cabal file.

### Documentation

#### In the code
We're always striving to improve documentation. Please include [haddock](https://haskell-haddock.readthedocs.io/en/latest/index.html) documentation for any added code, and update the documentation for any code you modify.

#### In the [Changelog](ChangeLog.md)
Add an entry to the changelog when your PR:
* Adds a feature
* Deprecates something
* Includes a breaking change
* Makes any other change that will impact users

#### In the [Readme](README.md)
The readme is the first place a lot of people look for information about the repository. Update any parts of the readme that are affected by your PR.
