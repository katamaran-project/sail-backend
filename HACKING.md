## Workflow in short

Further details are in [`CLAUDE.md`](../CLAUDE.md).

### Create a feature worktree for a new feature

Not obligatory, but useful when developing a new feature that requires changes across more than just sail-backend's repo.

- In your local git config, name a number of repos.
- Also name a `<root>` for your worktrees.
- Then run `bin/new-feature <branch>`.
  It creates a separate directory `<root>/<branch>/` for your new feature.
  It will contain one directory for each repo you named (plus for the sail-backend repo itself).
  Such a directory `<root>/<branch>/<repo>/` will then be a Git _linked worktree_ for repo `<repo>` on its separate branch `<branch>`.

My `.git/config` contains:
```
[newFeature]
	root = ~/worktrees
	repo = ~/software/katamaran
	repo = ~/software/cheriot-sail
```
because katamaran and cheriot-sail both might require changes in order to advance the compilation of CHERIoT Sail.
You can add your own case study to the list of repos.

If you take over on another machine, set this config up yourself.
See the header of [`bin/new-feature`](../bin/new-feature) for more details.

Small warning on submodules: suppose you include repo A in the config and you create a new feature B.
If A contains a submodule C, then the new linked worktree `<root>/B/A` will include a _clone_ of C, with no disk space deduplication or shared git history with other versions of C.
This is inherent to how git submodules interact with linked worktrees I think.

### Run the end-to-end tests

```sh
dune build                    
# run all e2e tests using this checkout's plugin.
# Specify test with --test NAME
# You can use --no-coq
# Specify number of jobs with -j N
tests/coq-tests/run-tests.py
```

Do not run `make install` unless you want to replace the plugin in the opam switch.
Use `bin/sail-local` which runs sail using local-built sail-backend plugin.
This is better than using the sail binary installed globally in the switch, which uses the sail-backend plugin installed by `dune install`.

### Run the CHERIoT case study

It's just the Makefile of the CHERIoT case study sub-directory in the katamaran repo.
Make sure to pass it (the wrapper around) the sail binary that is local to your worktree.
And also pass it the location of the cheriot-sail repo, which contains all the relevant sail code that we aim to compile in this case study.

```sh
make -C <katamaran>/case_study/Cheriot test \
     SAIL=$PWD/bin/sail-local \
     CHERIOT_SAIL_DIR=<cheriot-sail> \
     OUT_DIR=/absolute/output/dir
```

`make test` translates only `prelude.sail`.
`make sail` translates the full model.


