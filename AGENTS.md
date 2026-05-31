# Agent Instructions

## Project overview

gptel-fn-complete is an Emacs Lisp package that rewrites the function at point
through gptel.

## Conventions

- Keep the package as a single-file Emacs Lisp library unless there is a clear
  reason to split it.
- Keep user-facing README examples in sync with command behavior.
- Prefer small, local Emacs batch checks for syntax and behavior changes.

## Checks

For syntax sanity after editing the package file, run:

```sh
emacs -Q --batch --eval '(progn (with-temp-buffer (insert-file-contents "gptel-fn-complete.el") (check-parens)))'
```

The GitHub workflow runs Melpazoid packaging checks on push and pull request.

## Releasing

Before release, fetch tags and review commits since the previous tag. Update the
`Version:` header in `gptel-fn-complete.el` and commit that bump separately with
message `chore: bump version to <version>`.

For GitHub release notes, start with a short summary, group related changes
under descriptive headings, and avoid a single generic `## Changes` section when
the release has multiple themes. Put user-visible behavior first and internal
maintenance later. Keep the "Full Changelog" link at the bottom when GitHub
generated one. Do not include routine verification sections or lists of check
commands in public release notes; report validation in the chat handoff instead.
