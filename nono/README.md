# Agent-shell nono profiles

Personal profiles for **nono 0.78.0**, intended for `~/.emacs.d/nono/` in
the Emacs configuration repository.

```sh
nono profile validate ~/.emacs.d/nono/developer.json
nono profile show ~/.emacs.d/nono/developer.json
```

Use `M-x agent-shell-nono-select-profile` to choose a default for **new
agent buffers**. Each buffer retains its profile path, not a frozen copy
of the policy. Changes to JSON are read on the next process launch.

## Private temporary files on Linux

When `bwrap` is available, `agent-shell-nono` gives every sandboxed local
Linux launch a fresh tmpfs `/tmp` with mode `1777`, and sets `TMPDIR`, `TMP`
and `TEMP` to `/tmp`. Bubblewrap runs before nono so filesystem permissions
apply to the new mount. Host `/tmp` contents are hidden; temporary data
vanishes when the session's processes exit. When available, the outer
systemd scope also accounts for the tmpfs memory usage.

Bubblewrap is optional, like `systemd-run`. **Without it, `/tmp` remains
host-backed, and the same profile grant allows access to the host's
`/tmp`.** Running nono directly also does not create a private mount.
The launcher reports when bubblewrap is missing, but nono's filesystem
and network policy still applies. Temporary-directory environment variables
are left unchanged when there is no mount wrapper.

With bubblewrap, keep the nono executable, selected profile and workspace
outside `/tmp`, including symlink targets, so the mount does not hide them.
The home directory and `/var/tmp` are not replaced. Other platforms are
unchanged. Profiles only specify permissions; the launcher creates mounts.

## Profiles

### `base.json`

Extends nono's built-in `default` without adding permissions or restrictions.
It supplies upstream system/runtime grants and protections for credentials,
keychains, browser data and shell configuration. **It does not restrict
networking** and does not automatically grant workspace writes.

### `developer.json` (initial selection)

Extends `base`, enables workspace read/write access, and selects the
upstream **`developer` network preset**. This includes model APIs, package
registries, GitHub, Sigstore and documentation destinations. It is broad,
but not unrestricted native networking: network clients must use nono's
proxy or a suitable adapter. The preset can change with nono releases.

Filesystem grants cover repositories, build caches and agent state,
including shared Git metadata and six sibling repository paths. Missing
optional profile paths are skipped by nono; create them outside the
sandbox before launching if you need them granted.

On Linux, Landlock cannot subtract a deny from an allowed parent. Nono
refuses conflicting configurations rather than ignoring the deny. The
profile therefore uses specific grants:

- `~/.local/bin`, `~/.local/lib`, `~/.local/share/mise` and per-agent
  data/state directories. The whole `~/.local` directory would expose
  nono's protected runtime state and keyrings.
- Agent configuration directories, `.config/git`, `.config/mise`,
  `.gitconfig` and `.gitignore`. Broad `~/.config`, `~/.emacs.d` and
  `~/dotfiles` reads are not granted. Add specific paths as needed,
  including targets of individually symlinked files.
- `~/.ssh`, `~/.docker` and `~/.npmrc` are protected by nono's default
  credential policy. The trusted container profile makes an exception
  for Docker.
- `.config/mc`, `.config/onedrive`, `.config/pulse` and `.config/rclone`
  are explicitly denied.

The preset includes upstream temporary-directory grants. The Linux
launcher supplies a private `/tmp` when bubblewrap is available; other
granted temporary directories remain host-backed. There is no private
home mount.

It also retains nono's default Unix IPC policy. In particular, on Linux,
**a domain allowlist does not imply host Unix sockets are inaccessible**.
A reachable Docker, Podman, D-Bus or other powerful socket can provide an
escape from the effective filesystem/network restrictions. Use strict
pathname mediation on a supported native Linux system if that boundary
is required; do not assume this general developer profile provides it.

### `locked-down.json`

Extends `base`, not `developer`: inherited grants are additive, so a
restrictive profile must not inherit the broad developer permissions.

- No IP networking (`network.block: true`). This also blocks remote model
  APIs: it is an **offline** profile, not a generally usable cloud-agent
  preset.
- Workspace read/write, but no sibling repositories or external Git
  metadata. Standard runtime reads and read access to mise/nvm runtimes
  remain available.
- No automatic capability elevation.
- Broad upstream temp-directory writes are excluded. An explicit Linux-only
  grant permits reading and writing `/tmp`: private scratch space with
  bubblewrap, or the host's `/tmp` without it. Other platforms receive no
  extra temp-directory grant; use workspace-local scratch space there.
- Strict Linux pathname Unix-socket mediation, with no socket grants.

**Nono 0.78.0 refuses this strict Unix-socket mode on WSL2.** The profile
fails closed on that platform. Removing the setting is not an equivalent
secure workaround.

For a model-API-only online profile, make a separate profile using
`network.network_profile: "minimal"` instead of `network.block: true`,
and add the particular agent's necessary state/config grants. A child of
`locked-down` cannot undo `network.block` because it is sticky.

### `containers.json`

Extends `developer` and grants the usual local Docker and rootless Podman
API sockets. It also permits Docker configuration reads by explicitly
bypassing the default Docker credential-directory protection.

**Trusted agents only:** controlling a rootful Docker daemon is generally
equivalent to controlling the host as root. Rootless Podman can act with
the daemon user's host permissions. The daemon and its containers run
outside the agent's nono policy and systemd scope: filesystem, network,
CPU and RAM limits do **not** automatically carry over.

Examples, with a daemon already running outside the sandbox:

```sh
docker run --rm alpine echo hello
podman --remote run --rm alpine echo hello
```

For rootless Podman, the user can start the socket outside the sandbox:

```sh
systemctl --user enable --now podman.socket
```

This profile targets **remote/daemon-backed Podman**, not an unrestricted
local `podman` engine under inherited Landlock restrictions. On macOS,
Podman-machine connections need their particular socket/connection setup;
no blanket SSH or VM access is granted. Missing sockets do not get created
by nono. Docker context credentials may require additional per-tool grants.

## Worktrees and relative paths

The launcher passes the agent buffer's `default-directory` as `--workdir`.
Nono expands `$WORKDIR` against that directory, **not** the JSON directory.
For example, this `filesystem.allow` entry grants access to a sibling:

```json
"$WORKDIR/../graal"
```

The developer profile grants the current worktree through
`workdir.access: "readwrite"`, plus six sibling repository paths.

For a fanout layout such as:

```text
~/dev/graalpython/.agent-shell/worktrees/task-a/
```

`$WORKDIR/../graal` means:

```text
~/dev/graalpython/.agent-shell/worktrees/graal/
```

It does **not** mean `~/dev/graal`. If you want the latter checkout writable
regardless of where the agent starts, add `"$HOME/dev/graal"` explicitly.
There is no implicit search for related repositories.

A Git worktree's `.git` file usually points outside the worktree to the
main checkout's shared metadata. Workspace access alone is therefore not
enough for operations such as `git add` or `git commit`. The developer
profile grants writes to these shared metadata directories:

```text
$HOME/dev/ci-overlays/.git
$HOME/dev/graal/.git
$HOME/dev/graal-enterprise/.git
$HOME/dev/graalpython/.git
```

The integration test creates a real nested worktree and verifies staging a
file using these grants. For a repository elsewhere, inspect:

```sh
git -C /path/to/worktree rev-parse --path-format=absolute --git-common-dir
git -C /path/to/worktree rev-parse --absolute-git-dir
```

Add the required metadata path(s) to a project-specific profile's
`filesystem.allow`. Shared `.git` writes affect other worktrees too: they
are not worktree-exclusive isolation. The locked-down profile deliberately
omits them, so Git operations needing external metadata fail.

## Inheritance and policy ownership

`"extends": "base"` finds sibling `base.json`. Arrays of bases also work;
filesystem and domain grants generally accumulate rather than replace one
another. `network.block: true` is sticky. Do not add a sibling `default.json`
that extends `base`: it would shadow the upstream default that `base` uses
and create a cycle.

Keep profiles outside agent-writable directories when protecting future
session policy is important. A workspace-wide grant containing these JSON
files permits editing them; a contradictory child deny cannot be enforced
by Landlock. Selecting such a workspace is a trust decision, just as it is
for editing the launcher or other host-executed configuration.

These profiles make **no GET-only guarantee**. Nono 0.78.0's plain-HTTP
forward path does not enforce endpoint-method rules; do not use them as
complete GET-only protection.
