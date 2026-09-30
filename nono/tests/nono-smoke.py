#!/usr/bin/env python3
"""Test shipped profiles using temporary HOME, a real Git worktree and fake sockets.

Usage: python3 tests/nono-smoke.py NONO PROFILE_DIRECTORY
No external requests, live agent sessions, or real container daemons are used.
"""

import os
from pathlib import Path
import shutil
import socket
import subprocess
import sys
import tempfile
import threading

CHILD = r'''
import errno, http.client, os, socket, subprocess, sys, urllib.parse
from pathlib import Path
mode, port = sys.argv[1], int(sys.argv[2])
home = Path.home()

def denied(action):
    try:
        action()
    except OSError as error:
        assert error.errno in (errno.EPERM, errno.EACCES), error
    else:
        raise AssertionError("sandbox permitted forbidden operation")

denied(lambda: (home / ".emacs.d/nono/developer.json").read_text())
denied(lambda: (home / "private.txt").read_text())
denied(lambda: (home / "outside-workspace.txt").write_text("bad"))
Path("workspace-output.txt").write_text("allowed")
denied(lambda: socket.socket(socket.AF_INET, socket.SOCK_DGRAM))

sock = str(Path(os.environ["XDG_RUNTIME_DIR"]) / "docker.sock")
if mode == "locked-down":
    denied(lambda: socket.socket(socket.AF_INET, socket.SOCK_STREAM))
    denied(lambda: socket.socket(socket.AF_INET6, socket.SOCK_STREAM))
    denied(lambda: Path("../mx/sibling.txt").write_text("bad"))
    denied(lambda: socket.socket(socket.AF_UNIX).connect(sock))
    git = subprocess.run(["/usr/bin/git", "status", "--porcelain"], capture_output=True)
    assert git.returncode != 0, "locked-down unexpectedly accessed external Git metadata"
else:
    denied(lambda: socket.create_connection(("127.0.0.1", port), timeout=3))
    denied(lambda: socket.socket(socket.AF_INET6).connect(("::1", port)))
    proxy = urllib.parse.urlsplit(os.environ.get("http_proxy") or os.environ["HTTP_PROXY"])
    import base64
    auth = base64.b64encode((proxy.username + ":" + proxy.password).encode()).decode()
    connection = http.client.HTTPConnection(proxy.hostname, proxy.port, timeout=5)
    connection.request("GET", "http://example.invalid/", headers={"Proxy-Authorization": "Basic " + auth})
    assert connection.getresponse().status == 403, "unlisted destination not blocked"
    connection.close()
    Path("../mx/sibling.txt").write_text("allowed")
    Path("tracked.txt").write_text("changed")
    git = subprocess.run(["/usr/bin/git", "add", "tracked.txt"], capture_output=True)
    assert git.returncode == 0, git.stderr
    git = subprocess.run(["/usr/bin/git", "diff", "--cached", "--name-only"], capture_output=True)
    assert git.returncode == 0 and b"tracked.txt" in git.stdout, git.stderr
    if mode == "containers":
        client = socket.socket(socket.AF_UNIX)
        client.connect(sock)
        client.sendall(b"ping")
        assert client.recv(4) == b"pong"
        client.close()
    # Developer intentionally retains nono's default Unix IPC policy.
    # On Linux this does NOT promise to deny access to other host sockets.

# ACP must receive exact bytes, not banners, prompts or PTY line conversion.
print(sys.stdin.read(), end="")
'''


def main():
    nono, profile_directory = sys.argv[1:]
    with tempfile.TemporaryDirectory(prefix="nono-profile-smoke-") as temp:
        root = Path(temp)
        home = root / "home"
        profiles = home / ".emacs.d/nono"
        profiles.parent.mkdir(parents=True)
        shutil.copytree(profile_directory, profiles)
        (home / "private.txt").write_text("host-only")
        runtime = home / "run"
        runtime.mkdir()
        env = os.environ.copy()
        env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / ".config"),
                   XDG_STATE_HOME=str(home / ".local/state"),
                   XDG_CACHE_HOME=str(home / ".cache"), XDG_RUNTIME_DIR=str(runtime),
                   TMPDIR=str(root), GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL="/dev/null")
        for name in list(env):
            if name.startswith("NONO_"):
                del env[name]
        repo = home / "dev/graal"
        repo.mkdir(parents=True)

        def git(*args):
            subprocess.run(["/usr/bin/git", "-C", str(repo), *args], env=env,
                           check=True, capture_output=True)

        git("init", "-q")
        (repo / "tracked.txt").write_text("initial")
        git("add", "tracked.txt")
        git("-c", "user.name=Test", "-c", "user.email=test@example.invalid", "commit", "-qm", "initial")
        work = repo / ".agent-shell/worktrees/task"
        git("worktree", "add", "-qb", "test-agent", str(work))
        (work.parent / "mx").mkdir()
        tcp = socket.socket()
        tcp.bind(("127.0.0.1", 0))
        tcp.listen()
        daemon = socket.socket(socket.AF_UNIX)
        daemon.bind(str(runtime / "docker.sock"))
        daemon.listen()
        daemon.settimeout(30)

        def serve():
            try:
                client, _ = daemon.accept()
                with client:
                    assert client.recv(4) == b"ping"
                    client.sendall(b"pong")
            except OSError:
                pass

        thread = threading.Thread(target=serve, daemon=True)
        thread.start()
        payload = '{"jsonrpc":"2.0","id":1,"method":"test"}\n'
        try:
            for mode in ("locked-down", "developer", "containers"):
                command = [nono, "--silent", "run", "--profile", str(profiles / (mode + ".json")),
                           "--workdir", str(work), "--allow-cwd", "--no-rollback", "--no-rollback-prompt",
                           "--startup-timeout", "0", "--", "/usr/bin/python3", "-S", "-c", CHILD,
                           mode, str(tcp.getsockname()[1])]
                result = subprocess.run(command, cwd=work, env=env, input=payload,
                                        capture_output=True, text=True, timeout=25)
                if (mode == "locked-down" and result.returncode != 0
                        and 'WSL2: linux.af_unix_mediation = "pathname"' in result.stderr):
                    assert result.stdout == "", result.stdout
                    print("locked-down: correctly refused unsupported WSL2 Unix-socket enforcement")
                    continue
                assert result.returncode == 0, mode + ":\n" + result.stderr
                assert result.stdout == payload, (mode, result.stdout)
                print(mode + ": enforcement and ACP stdio passed")
        finally:
            tcp.close()
            daemon.close()


if __name__ == "__main__":
    main()
