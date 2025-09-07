Skip to content
Chat history

You said:
I'm trying to use codex to configure emacs, but it keeps creating invalid .el files (non-matching parens).  How can I improve this?  I've suggested composing simpler functions, but that hasn't worked yet.  What tools can an LLM use to ensure that it creates correct lisp code (balanced parens)? 
ChatGPT said:

Thought for 33s
Got it. You’re using an LLM to generate Emacs Lisp for your Emacs config, but it sometimes emits unbalanced parentheses that break loading. You’ve already asked it to compose simpler functions, but that hasn’t fixed structural correctness. Below is a pragmatic, engineering-first approach to make Lisp generation robust by construction and to fail fast when something slips through.

Requirements (re-stated)
Ensure generated .el is structurally valid (balanced parens, readable forms).

Prevent bad files from landing in your config.

Provide immediate, machine-checkable feedback the LLM can react to in the same loop.

Keep it Emacs-native; runnable locally on Linux; no GitHub Actions required.

Architecture & High-Level Design
Guardrail pipeline around code generation:

Stage 1: Structural read check (use Emacs’ reader + check-parens in batch).

Stage 2: Byte-compile with warnings-as-errors (catches more than syntax).

Stage 3: Style/Lint (optional but helpful) via elisp-lint/eldev.

Stage 4: Minimal smoke tests with ERT/Buttercup.

Atomic writes: the LLM writes to a temp file, pipeline validates, and only then atomically moves to your live load-path.

Round-trip formatting: after passing Stage 1, autoformat (idempotent sexp layout reduces future mismatches).

Editor-time safety: enable electric-pair-mode, paredit/lispy, flymake on your editing buffers—this helps when you hand-edit.

LLM loop tool hooks: your agent calls the validator; on failure, it must read and act on the exact error.

Non-Functional Requirements
Deterministic: batch checks produce stable, parseable output.

Fast: << 1s for structural checks on small files; byte-compile may take longer, still fast.

Automatable: Makefile & shell scripts; usable from Emacs, CLI, or your agents.

Low-Level Design (files & scripts)
Below are drop-in scripts you can paste into a small guardrail repo. The LLM (or you) can call only ./validate_elisp.sh <candidate.el>; if exit code is 0, the file is safe to deploy.

validate_elisp.sh
#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 path/to/file.el" >&2
  exit 2
fi
FILE="$1"
if [[ ! -f "$FILE" ]]; then
  echo "File not found: $FILE" >&2
  exit 2
fi

# 1) Structural read + check-parens
emacs --batch --eval "(progn
  (condition-case err
      (with-temp-buffer
        (insert-file-contents \"$FILE\")
        (emacs-lisp-mode)
        ;; Fast structural check
        (check-parens)
        ;; Also try reading all top-level forms
        (goto-char (point-min))
        (while t
          (let ((form (condition-case nil (read (current-buffer)) (end-of-file nil))))
            (when (null form) (cl-return)))))
    (error
      (princ (format \"STRUCTURE-ERROR: %s\n\" (error-message-string err)))
      (kill-emacs 1))))"

# 2) Byte-compile with warnings as errors
emacs --batch \
  --eval "(setq byte-compile-error-on-warn t)" \
  --eval "(setq load-prefer-newer t)" \
  -f batch-byte-compile "$FILE"

# 3) Optional: lint (comment out if you don’t want it)
# emacs --batch -l elisp-lint.el --eval "(elisp-lint-files-batch (list \"$FILE\"))"

echo "OK: structural + byte-compile passed for $FILE"
Make it executable:

chmod +x validate_elisp.sh
Makefile
EL = $(wildcard *.el)
OK = $(EL:.el=.ok)

all: $(OK)

%.ok: %.el validate_elisp.sh
	@./validate_elisp.sh $<
	@touch $@

clean:
	rm -f *.elc *.ok
Run:

make
Minimal ERT Smoke Test Harness (optional but recommended)
Create tests.el to fail early if required symbols are missing or top-level side-effects misbehave.

;; tests.el
(require 'ert)

(ert-deftest config-loads-without-errors ()
  (let ((file (expand-file-name (or (getenv "EL_FILE") "init-candidate.el"))))
    (should (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Try to eval top-level forms safely
      (goto-char (point-min))
      (condition-case err
          (while t
            (let ((form (condition-case nil (read (current-buffer)) (end-of-file nil))))
              (when (null form) (cl-return))
              (eval form)))
        (error (ert-fail (format "Eval error: %S" err)))))))

;; Add more tests here that assert presence/behavior of expected defuns/vars.
Run ERT in batch after validation:

emacs --batch -L . -l tests.el -f ert-run-tests-batch-and-exit
You can add this as Stage 4 in validate_elisp.sh behind an env flag.

Optional: Autoformat Step
Two good options:

Built-in reindent (fast, no deps):

emacs --batch "$FILE" \
  --eval "(progn
    (find-file \"$FILE\")
    (emacs-lisp-mode)
    (indent-region (point-min) (point-max))
    (save-buffer))"
elisp-format package (nicer, requires install):

;; formatter.el (run via --script)
(require 'elisp-format)
(with-temp-buffer
  (insert-file-contents (car command-line-args-left))
  (emacs-lisp-mode)
  (elisp-format--buffer)
  (write-file (car command-line-args-left)))
How an LLM can use these tools to guarantee balanced parens
Here are concrete patterns you can enforce in your agent prompts / tools:

Write→Validate→Promote:

The agent writes to init-candidate.el.

It runs ./validate_elisp.sh init-candidate.el.

If exit code ≠ 0, it must parse the error line beginning with STRUCTURE-ERROR: (or compiler output), fix, and retry.

Only on success, move init-candidate.el to your live ~/.emacs.d/ location.

Force “read-back” self-check:

Require the agent to emit a final step reading its own code as Lisp by simulating the Emacs reader (even a simple read loop shown above). If it can’t be read, reject the output locally before saving.

S-expression envelope trick:

Ask the LLM to produce one top-level (progn …) that contains everything. That reduces the chance of trailing fragments and makes it easier for a single read to succeed or fail decisively.

No trailing commentary outside forms:

In the prompt, forbid trailing characters after the last closing paren. Your validator will catch it, but stating the constraint reduces retries.

Small diffs, strict boundaries:

Have the LLM propose a patch (unified diff) or append-only block to a specific marker region. Your script reconstructs a full file and validates. This dramatically reduces structural errors.

On-the-fly editor safety (when you hand edit):

Enable electric-pair-mode, plus either paredit or lispy. They preserve structure as you type.

Turn on flymake-mode in emacs-lisp-mode and wire it to run check-parens (via a very small custom backend if you like).

Pros/Cons of Tooling Options
Emacs batch + check-parens (Stage 1)

✅ Native, fast, definitive structural check.

❌ Only structural; logic errors slip through.

Byte-compile with byte-compile-error-on-warn (Stage 2)

✅ Catches syntax + many semantic issues early.

❌ Can be strict; you might need to suppress benign warnings.

Lint (elisp-lint, eldev) (Stage 3)

✅ Style & hygiene improvements; helps long-term maintainability.

❌ Extra setup; not essential for mere paren balance.

ERT/Buttercup smoke tests (Stage 4)

✅ Validates behavior, not just syntax.

❌ Requires you to write a few tests (worth it).

paredit/lispy in-editor

✅ Prevents structural edits that break balance.

❌ Doesn’t help for LLM output unless you route it through Emacs.

Autoformat step

✅ Normalizes layout; reduces accidental paren loss in edits/merges.

❌ If misconfigured, might fight your preferred style (tune once).

Methodologies & Workflow
Red/Green: treat the validator as your unit test; agent must not “ship” until it’s green.

Atomic promotion: only move files into ~/.emacs.d/ after passing validation.

Patch discipline: instruct the LLM to only modify between ;; BEGIN AGENT BLOCK … ;; END AGENT BLOCK markers; keep the rest hand-owned.

Log & parse errors: the agent should read stderr/stdout and extract the first failing line to guide regeneration (you can even make the script output JSON on failure for machine parsing).

“Why this works” (Benefits/Risks/Alternatives)
Benefits

Structural correctness is enforced by the Lisp reader itself—not heuristics.

Fast feedback allows multiple agent retries without human intervention.

Byte-compilation & optional lint/testing catch a wider blast radius of issues.

Risks

Over-strict compiler flags may block code that’s “good enough.” Tune thresholds.

If your config has environment-sensitive code at top-level, batch eval/compile may have side effects. Prefer defuns + controlled initialization.

Alternatives

Eldev as a single tool for build/test/lint.

Cask + ert-runner as a conventional Emacs Lisp project layout.

Org-babel tangling: generate config from init.org; tangling enforces complete blocks and you validate the tangled init.el as above.

Small extras you might like
Flymake “check-parens” backend (instant editor feedback):

;; put in your emacs-lisp-mode hook
(defun my-elisp-flymake-check-parens (report-fn &rest _args)
  (save-excursion
    (goto-char (point-min))
    (condition-case err
        (progn
          (check-parens)
          (funcall report-fn nil))
      (error
       ;; crude parse; you can refine by scanning buffer positions
       (funcall report-fn
                (list (flymake-make-diagnostic
                       (current-buffer) (point-min) (point-min)
                       :error (error-message-string err))))))))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'flymake-diagnostic-functions
                      #'my-elisp-flymake-check-parens nil t)))
Atomic promote script (use after validation):

#!/usr/bin/env bash
set -euo pipefail
SRC="$1"                 # init-candidate.el
DST="$HOME/.emacs.d/init.el"
./validate_elisp.sh "$SRC"
install -m 0644 "$SRC" "${DST}.new"
mv -f "${DST}.new" "$DST"
echo "Promoted $SRC -> $DST"
TL;DR
Don’t trust the LLM to balance parens—force it through a batch Emacs reader + check-parens → byte-compile pipeline.

Make the agent fix its output until ./validate_elisp.sh returns success.

Only then atomically swap into your live config.

Add ERT smoke tests over time to catch regressions.

In your editor, keep paredit/lispy + flymake on to prevent manual breakage.

If you want, tell me your exact agent runner (Emacs, shell, or a Rust harness), and I’ll wire these commands into that loop with parseable JSON failures so your agent can auto-retry intelligently.



No file chosenNo file chosen
ChatGPT can make mistakes. Check important info.
