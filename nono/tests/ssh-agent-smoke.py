#!/usr/bin/env python3
"""Test developer/container SSH with disposable keys and a host-side Linux agent.

Usage: python3 tests/ssh-agent-smoke.py NONO PROFILE_DIRECTORY
Requires bubblewrap and OpenSSH; no network requests or real credentials.
"""

import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time

CHILD = r'''
import errno, os, shlex, subprocess, sys
from pathlib import Path
home = Path.home()
assert os.environ['SSH_AUTH_SOCK'] == str(home / 'run/agent-shell-ssh/agent.sock')
assert not Path(sys.argv[1]).exists(), 'host /tmp was not hidden'
assert (home / '.ssh/known_hosts').read_text().startswith('probe.invalid ')

def denied(action):
    try:
        action()
    except OSError as error:
        assert error.errno in (errno.EPERM, errno.EACCES), error
    else:
        raise AssertionError('sandbox permitted forbidden operation')

denied(lambda: (home / '.ssh/id_ed25519').read_bytes())
denied(lambda: (home / '.ssh/known_hosts').write_text('bad'))
denied(lambda: (home / '.ssh/config').read_text())
denied(lambda: (home / '.emacs.d/nono/developer.json').read_text())
public_key = Path('identity.pub').read_text().strip()
listed = subprocess.check_output(['/usr/bin/ssh-add', '-L'], text=True)
assert listed.strip() == public_key
probe = subprocess.run(['/usr/bin/ssh-add', '-T', 'identity.pub'], capture_output=True)
assert probe.returncode == 0, probe.stderr
config = subprocess.check_output(shlex.split(os.environ['GIT_SSH_COMMAND']) +
                                 ['-G', 'probe.invalid'], text=True)
assert 'identityfile none\n' in config
assert 'batchmode yes\n' in config
assert 'stricthostkeychecking true\n' in config
print('SSH signing works; metadata read-only; private key/config denied')
'''


def main():
    nono, profile_directory = sys.argv[1:]
    nono = str(Path(nono).resolve())
    profiles_source = Path(profile_directory).resolve()
    if not sys.platform.startswith('linux'):
        raise SystemExit('This profile and test require Linux')
    if profiles_source.is_relative_to(Path('/tmp').resolve()):
        raise SystemExit('Keep profiles and test HOME outside /tmp')
    with tempfile.TemporaryDirectory(prefix='ssh-smoke-', dir=profiles_source.parent) as temp:
        home = Path(temp) / 'home'
        ssh = home / '.ssh'
        ssh.mkdir(parents=True)
        profiles = home / '.emacs.d/nono'
        shutil.copytree(profiles_source, profiles)
        runtime = home / 'run'
        socket_dir = runtime / 'agent-shell-ssh'
        socket_dir.mkdir(parents=True)
        work = home / 'dev/graalpython'
        work.mkdir(parents=True)
        env = {k: v for k, v in os.environ.items() if not k.startswith('NONO_')}
        env.update(HOME=str(home), XDG_CONFIG_HOME=str(home / '.config'),
                   XDG_STATE_HOME=str(home / '.local/state'),
                   XDG_CACHE_HOME=str(home / '.cache'), XDG_RUNTIME_DIR=str(runtime),
                   SSH_AUTH_SOCK=str(socket_dir / 'agent.sock'), SSH_ASKPASS_REQUIRE='never')
        for key in (ssh / 'id_ed25519', work / 'host-key'):
            subprocess.run(['/usr/bin/ssh-keygen', '-q', '-t', 'ed25519', '-N', '',
                            '-f', str(key)], check=True)
        (ssh / 'known_hosts').write_text('probe.invalid ' + (work / 'host-key.pub').read_text())
        (ssh / 'config').write_text('Host *\n    ProxyCommand false\n')
        shutil.copyfile(ssh / 'id_ed25519.pub', work / 'identity.pub')
        agent = subprocess.Popen(['/usr/bin/ssh-agent', '-D', '-a', env['SSH_AUTH_SOCK'], '-P', ''],
                                 env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        try:
            for _ in range(100):
                if Path(env['SSH_AUTH_SOCK']).exists():
                    break
                assert agent.poll() is None, 'ssh-agent exited before creating its socket'
                time.sleep(.02)
            subprocess.run(['/usr/bin/ssh-add', str(ssh / 'id_ed25519')], env=env,
                           capture_output=True, check=True)
            for profile_name in ('developer', 'containers'):
                with tempfile.NamedTemporaryFile(prefix='ssh-host-marker-', dir='/tmp') as marker:
                    command = ['/usr/bin/bwrap', '--die-with-parent', '--bind', '/', '/',
                               '--dev-bind', '/dev', '/dev', '--perms', '1777', '--tmpfs', '/tmp',
                               '--setenv', 'TMPDIR', '/tmp', '--', nono, '--silent', 'run',
                               '--profile', str(profiles / (profile_name + '.json')),
                               '--workdir', str(work), '--allow-cwd', '--no-rollback',
                               '--no-rollback-prompt', '--startup-timeout', '0', '--',
                               '/usr/bin/python3', '-S', '-c', CHILD, marker.name]
                    # Verify profile injection rather than relying on the host environment.
                    child_env = dict(env, SSH_AUTH_SOCK='/missing-host-agent.sock')
                    result = subprocess.run(command, cwd=work, env=child_env, text=True,
                                            capture_output=True, timeout=25)
                    assert result.returncode == 0, result.stdout + result.stderr
                    print(profile_name + ': ' + result.stdout, end='')
        finally:
            agent.terminate()
            agent.wait(timeout=5)


if __name__ == '__main__':
    main()
