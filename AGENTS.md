# Orchestration and Emacs

This is my emacs config.
It serves as a reproducible setup for the command and control center that is Emacs.

## About my Emacs config

Emacs is the main entry point I am using to interface with computers on Unix-like boxes, Windows, and Android (so far).

I try to keep the config relatively lean, I think twice before I pull in functionality that I can also get in a similar fashion using built-in Emacs facilities.
Sometimes the external packages offer just so much more that I would not miss them, but using built-in facilities where possible ensures I can take my config with me to new OS shores and Emacs versions without much hassle.

## How agents should use this

### When I talk about changing my emacs config

Use the `emacs` skill to find out about my context and installed packages when appropriate.
Work directly in this checkout.
Ask me before running things in my live emacs that actually change things more than simple queries.

### When I talk about work I am currently doing

I use Emacs to work on many projects, often at the same time.
Interesting buffers can be seen with `(agent-shell-buffers)`, or have names starting with `*Jira Issues*`, `*Jira Issue Detail:`, `*vc-dir*`, `magit:`, `*ci dashboard*`, `*ci build log`.
I am also using lsp-mode and eglot for coding, so any buffers with those modes are also potentially interesting.
You can query these when it seems necessary using `emacsclient`.
The file `~/.emacs.d/.agent-shell/live-agent-shell-set.el` also encodes where I recently am or was working with agent-shells, even if some of those directories' agent-shells are no longer open.

### When I talk about work I have to do

- `(timfel/jira)` opens a buffer that asynchronously fetches Jira issues that are assigned to me
- `(ci-dashboard)` opens a buffer that asynchronously fetches PRs I have open and PRs I need to review
- `(org-agenda nil "t")` opens a buffer with my current TODO items

### When I want to start working on something

I like to use worktrees for coding-related agent tasks.
I usually have a `d:/` drive or a `~/dev` folder where I keep my main git checkouts.

So when I ask to work on something, chances are I want you to ask me (interactively with emacs' `(read-directory-name "work where: ")`) what repository that work should be based on under these folders.
I might then respond with a non-existent folder, in which case it's a fresh project and I expect you to create it and call `git init` in the new folder.

The next step for you is to call `(timfel/agent-shell-fan-out-worktrees '(TITLE-OF-TASK . INITIAL-QUESTION-AND-CONTEXT-FOR-AGENT) CHOSEN-DIRECTORY-FROM-PREVIOUS-STEP)`.
It may be prudent to write the initial question/task and context into a file and just put something like "read [FILE] and act on it" as task.
