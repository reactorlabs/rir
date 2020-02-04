# Updating GNU R version used by Ř

Our patched GNU R lives in https://github.com/reactorlabs/gnur.

It is a mirror of https://github.com/wch/r-source, which in turn mirrors GNU R's official svn (https://svn.r-project.org/R/).

To update, first set up the `wch/r-source` repo as a remote of our `reactorlabs/gnur` repo.

Rewrite the history of our patches, such that they are nice and clean. Usually, there will be a couple commits with our patches from the last version update, and then some more commits with the changes we made on the current version (see, eg., https://github.com/reactorlabs/gnur/compare/R-3.5.1...R-3.5.1-rir-patch).

Check out the branch that we want to target.

Create a new branch based on it and cherry-pick the cleaned patches into that branch, fixing the merges one by one.

Update the `custom-r` submodule in the `reactorlabs/rir` repo.
