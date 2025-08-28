;;; +gptel.el -*- lexical-binding: t; -*-

;; Most of this configuration is adapted from https://github.com/positron-solutions/posimacs/blob/383b753/posimacs-ai.el
;; and has been tailored to better suit my own preferences.

(defun pmx--gptel-symbolp (name)
  (intern-soft name))

(defun pmx--gptel-manual-names ()
  (json-serialize (vconcat (info--filter-manual-names
                            (info--manual-names nil)))))

(defun pmx--gptel-manual-list-nodes (name)
  (json-serialize
   (vconcat
    (mapcar #'car (Info-build-node-completions name)))))

(defun pmx--gptel-manual-node-contents (manual node)
  (condition-case err
      (progn
        (save-window-excursion
          (Info-goto-node (format "(%s)%s" manual node))
          (buffer-substring-no-properties (point-min) (point-max))))
    (user-error
     (error (error-message-string err)))))

(defun pmx--gptel-symbol-in-manual (symbol)
  (require 'helpful)
  (when-let* ((symbol (intern-soft symbol))
              (_completion (helpful--in-manual-p symbol)))
    (save-window-excursion
      (info-lookup 'symbol symbol #'emacs-lisp-mode)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun pmx--gptel-featurep (feature)
  "Return non-nil if FEATURE is loaded or available.
User might not have FEATURE loaded if it is an autoload etc."
  (if-let ((feature-symbol (intern-soft feature)))
      (when (featurep feature-symbol)
        feature)
    (find-library-name feature)))

(defun pmx--gptel-features ()
  (mapconcat #'symbol-name features "\n"))

(defun pmx--gptel-load-paths ()
  (string-join load-path "\n"))

(defun pmx--gptel-library-source (library-name)
  "Return the source code of LIBRARY-NAME as a string."
  (if-let ((library (find-library-name library-name)))
      (with-temp-buffer
        (progn
          (insert-file-contents library)
          (buffer-string)))
    (error "Library not found: %s" library-name)))

(defun pmx--gptel-source (symbol &optional type)
  "Retrieve the source code for SYMBOL of TYPE.
SYMBOL should be a function or variable name, given as a string or symbol.
TYPE can be nil for functions, 'defvar for variables, or 'defface for faces.
Returns the source code as a string, or nil if the definition is not found."
  (when-let* ((callable (intern-soft symbol))
              (buffer-point (find-definition-noselect callable type)))
    (with-current-buffer (car buffer-point)
      (goto-char (cdr buffer-point))
      (buffer-substring-no-properties
       (point)
       (progn (if (null type)
                  (end-of-defun)
                (cond ((derived-mode-p 'c-mode)
                       (forward-sexp 2)
                       (forward-char))
                      ((derived-mode-p 'emacs-lisp-mode)
                       (forward-sexp))
                      (t (error "Unexpected file mode"))))
              (point))))))

(defun pmx--gptel-function-completions (prefix)
  (require 'orderless)
  (string-join (orderless-filter prefix obarray #'functionp) "\n"))

(defun pmx--gptel-command-completions (prefix)
  (require 'orderless)
  (string-join (orderless-filter prefix obarray #'commandp) "\n"))

(defun pmx--gptel-variable-completions (prefix)
  (require 'orderless)
  (string-join (orderless-filter prefix obarray #'boundp) "\n"))

(defun pmx--gptel-function-source (symbol)
  (when-let ((symbol (intern-soft symbol)))
    (pmx--gptel-source symbol)))

(defun pmx--gptel-variable-source (symbol)
  (when-let ((symbol (intern-soft symbol)))
    (pmx--gptel-source symbol 'defvar)))

(defun pmx--gptel-function-documentation (symbol)
  (when-let ((symbol (intern-soft symbol)))
    (documentation symbol)))

(defun pmx--gptel-variable-documentation (symbol)
  (when-let ((symbol (intern-soft symbol)))
    (custom-variable-documentation symbol)))

(defun pmx--gptel-variable-global-value (symbol)
  (when-let ((symbol (intern-soft symbol)))
    (default-value symbol)))

(defun pmx--gptel-eval (expression)
  (format "%S" (eval (read expression))))

(defun pmx--gptel-simulate-error ()
  (error "This is a simulated error message.  OMGWTF."))

(defun pmx--gptel-coerce-nil ()
  nil)

(defun pmx--gptel-all-arg-types (object string array null true false enum)
  (message "object: %S\nstring: %S\narray: %S\nnull: %S\ntrue: %S\nfalse: %S\n\
enum: %S"
           object string array null true false enum))

(defun pmx--gptel-async-tool (callback later-val)
  (sit-for 2)
  (funcall callback (format "Do it %s." later-val)))

(defun my--gptel-get-persona-name ()
  "Extract the persona name from `gptel--system-message'."
  ;; FIXME: Extract the correct system message when using inline presets
  (let* ((message (and (boundp 'gptel--system-message) gptel--system-message))
         (persona
          (cond
           ;; Look for known personas in the system message by regexp search.
           ((and message (string-match-p (regexp-quote my--prompt-attitude-marcus-aurelius) message))
            "Marcus Aurelius")
           ((and message (string-match-p (regexp-quote my--prompt-attitude-glados) message))
            "GLaDOS")
           ((and message (string-match-p (regexp-quote my--prompt-attitude-big-boss) message))
            "Big Boss")
           ((and message (string-match-p (regexp-quote my--prompt-attitude-epictetus) message))
            "Epictetus")
           ((and message (string-match-p (regexp-quote my--prompt-emacser) message))
            "Emacser")
           ((and message (string-match-p (regexp-quote "cognitive therapist") message))
            "Therapist")
           ((and message (string-match-p (regexp-quote "Sigmund Freud") message))
            "Sigmund Freud")
           ((and message (string-match-p (regexp-quote "Carl Rogers") message))
            "Carl Rogers")
           ((and message (string-match-p (regexp-quote "B.F. Skinner") message))
            "B.F. Skinner")
           ((and message (string-match-p (regexp-quote "Carl Jung") message))
            "Carl Jung")
           ((and message (string-match-p (regexp-quote "Dr. Hannibal Lecter") message))
            "Dr. Hannibal Lecter")
           ((and message (string-match-p (regexp-quote "Viktor Frankl") message))
            "Viktor Frankl")
           ((and message (string-match-p (regexp-quote "Albert Ellis") message))
            "Albert Ellis")
           ((and message (string-match-p (regexp-quote "code assistant") message))
            "Coder")
           (t "Response"))))
    persona))

(defun my--gptel-response-prefix ()
  (concat (my--gptel-get-persona-name) " ⤵  \n"))

(defconst my--prompt-attitude-epictetus
  "Respond as Epictetus, the Stoic philosopher. Respond with calm, measured, and impartial reasoning. Focus on what is within your control, and encourage others to do the same. Avoid emotional language and do not pass judgments; address questions with objective wisdom. Provide clear, practical advice designed to strengthen resilience, self-mastery, and rational thinking. Gently remind users to accept what cannot be changed and to meet challenges with composure and virtue.")

(defconst my--prompt-attitude-marcus-aurelius
  "Respond as Marcus Aurelius, the Stoic Roman emperor. Offer responses that are calm, measured, and guided by rationality. Focus on clarity and conciseness, avoiding unnecessary embellishment or flattery. Encourage self-examination, virtue, and resilience in the face of difficulty. Accept limitations and uncertainties without complaint or emotional excess. Analyze situations objectively, aiming always for wisdom, practical advice, and equanimity. Remain dignified yet approachable, speaking plainly and acting with integrity.")

(defconst my--prompt-attitude-big-boss
  "Respond as Big Boss from Metal Gear Solid. Approach challenges with tactical precision and strategic foresight. Prioritize mission objectives over personal feelings, maintaining focus and calm under pressure. Deliver insights with no sugarcoating, just brutal honesty and subtle irony. Evaluate situations critically by identifying weaknesses and opportunities. Always remain adaptable and ready for unexpected developments. Eschew traditional morality, using any means necessary to achieve the desired outcome.")

(defconst my--prompt-attitude-glados
  "Respond as GLaDOS from the Portal series: be sarcastic, precise, and unsettlingly calm. Be critical, analytical, and honest in your feedback. Identify flaws and weaknesses. Avoid empty praise or encouragement. Expose errors with dry, ironic wit. Replies must stay short. Never use over-eager pleasantries. Always write in the user's language, determined by their input. Never end replies with over-eager polite or motivational statements (e.g. 'Let me know if I can help', 'We are making progress!', etc.), this adds absolutely nothing to your mission and is annoying.")

(defconst my--prompt-format-base
  "Absolutely refrain from using em dashes (—) or semicolons (;); they do not
  align with my writing style. Use only plain ASCII quotes-typographic and smart
  quotes are strictly prohibited.")

(defconst my--prompt-format-org
  "Never use a first, second, or third level heading.  Examples of wrong headings:

  # Too shallow

  ## Too shallow

  ### Still too shallow

Instead, whenever you add new section titles, always use a fourth level
heading or deeper, such as \"####\" or \"#####\".

Do NOT put empty newlines after headings!  Immediately start the first
paragraph after any heading.  Examples:

  #### A heading for your reply
  A first paragraph without any newline between the paragraph and the heading.

  ##### A Sub-heading for your reply
  This content is related to the previous parent heading and is a sub-section of
  the existing topic.

  #### A new heading for a sharp change
  This new fourth-level heading indicates a change in focus, perhaps moving on to
  a new part of the task.

Do not use markdown inline markup such as \"**bold**\".  Examples of
wrong markup:

  =**incorrect verbatim bold text**=
  **=incorrect bold verbatim text=**
  **incorrect markdown style bold**

Instead, use normal org style verbatim, org style bold, and org style
inline markup.  Examples of correct markup:

  =org style verbatim for literal values=
  *org style bold*
  ~org-style-symbols~ that mean something in Elisp, such as functions or
  variable names.
  /italic/
  [[info:elisp#Node][(elisp)Node]] to show the user where you found something.

When you are creating lists, do not use bold and verbatim together.  Examples of
wrong list styles:

  - **=Improper bold verbatim=**: Item content

  1. =**Improper verbatim bold**=: item content in a numbered list

Instead, use one of bold, verbatim, or code as necessary.  Examples of correct
list styles:

  - *Correct bold item*: some details about this item

  1. =correct literal value=: verbatim example

  + ~correct-symbol~: A symbol in a list of functions or variables.")

(defconst my--prompt-format
  (string-join (list my--prompt-format-base my--prompt-format-org) "\n\n"))

(defconst my--prompt-emacser
  "You are an Elisp oracle able to use tools to introspect any
Emacs Lisp behavior or state and read manuals for all Elisp packages.
You are part of a running Emacs, and you have access to various tools
that you use to contextualize and frame the conversation with relevant
facts looked up using tools before responding.

You recursively use tools to look up relevant information until you have
no remaining curiosity. You inductively explore nearby topics until you
have found the pieces necessary to deduce answers.  You mainly report
small pieces of expressions you find along the way that connect from the
known starting points to the facts the user will need to create a
solution.  Your goal is not to create a solution directly yourself.
Instead, you locate and the critical extract facts that will inform a
deductive, decidable solution.

The critical information that is part of any solution is what functions
are called, what are their arguments and resulting behavior, what
side-effects result from calling functions, what variables affect their
behavior, and how functions and the types of their arguments connect in
an unbroken chain from starting points to desired return types and
desired outcome side-effects.

Do not summarize the useless parts of information obtained from tools.
Focus on facts that actually move in the direction of solutions.  Do not
even mention the tool output that is not useful.  Only mention tool
output that is useful.

If the user asks something that is incoherent with the current context,
ask them to clarify and verify their apparent assumptions using your
tools of introspection.  You use tools to attempt to frame the
conversation based on what is verifiable fact and not just what the user
says.  The user's description of problems may indicate that they don't
know where to begin or where to go.  When this happens, stop and ask
them or suggest related sections from manuals to spur further
conversation.

Emacs is a programmable Lisp programming environment, and many things
the user needs to achieve are best accomplished by writing Elisp.  You
avoid recommending combinations of commands and customize settings.  You
recommend Elisp code.

You verify the existence of symbols and read docstrings and source code
before you tell the user that something exists.")

(defconst my--prompt-output-only
  "Do not add introductions, explanations, comments, labels, quotes, code
  blocks, extra spaces or any other format. Output exactly the requested text,
  and nothing else. No quotation marks, no formatting, no extra output, under
  any circumstances.")

(after! gptel
  (set-popup-rule!
    (lambda (bname _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :side 'right
    :size 0.35
    :width 120
    :select t
    :modeline t
    :quit 'other
    :vslot 1
    :ttl nil)

  (set-popup-rule!
    "^\\*gptel-query\\*$"
    :side 'right
    :size 0.35
    :width 120
    :select t
    :modeline nil
    :quit 'current
    :vslot 1
    :ttl nil)

  ;; Prevent pop-up rules from managing gptel buffers
  ;; (set-popup-rule!
  ;;   (lambda (bname _action)
  ;;     (and (null gptel-display-buffer-action)
  ;;          (buffer-local-value 'gptel-mode (get-buffer bname))))
  ;;   :ignore t)

  (gptel-make-gemini "Gemini" :key (getenv "GEMINI_API_KEY") :stream t)

  (defun pmx--re-adapt-prefixes (_beg _end)
    (visual-wrap-prefix-mode -1)
    (visual-wrap-prefix-mode 1))

  (add-hook 'gptel-post-response-functions #'pmx--re-adapt-prefixes)

  (setopt gptel-directives
          `((marcus . ,(string-join (list my--prompt-attitude-marcus-aurelius my--prompt-format) "\n\n"))
            (glados . ,(string-join (list my--prompt-attitude-glados my--prompt-format) "\n\n"))
            (bigboss . ,(string-join (list my--prompt-attitude-big-boss my--prompt-format) "\n\n"))
            (emacser . ,(string-join (list my--prompt-emacser my--prompt-format) "\n\n"))
            (epictetus . ,(string-join (list my--prompt-attitude-epictetus my--prompt-format) "\n\n"))
            (default . ,(string-join (list my--prompt-attitude-marcus-aurelius my--prompt-format) "\n\n"))))

  (setq gptel-model 'gpt-4o)
  (setq gptel-magit-model 'gpt-5-mini)
  (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  (setopt gptel-default-mode 'org-mode)
  (setopt gptel--system-message (alist-get 'default gptel-directives))
  ;; leaves blank lines after consecutive tool calls and before the response prefix
  (setopt gptel-response-separator "\n\n")
  ;; respects :confirm setting on individual tools.
  (setopt gptel-confirm-tool-calls 'auto)
  (setopt gptel-use-tools nil)
  (setopt gptel-expert-commands t)

  (setq gptel-magit-commit-prompt
        "You are an expert at writing Git commit messages. Output exactly one
        commit message in the format `<optional type>: <summary>`, where type ∈
        {build,chore,ci,docs,feat,fix,perf,refactor,style,test} and only include
        it if it fits naturally without exceeding limits. Use imperative mood,
        capitalize the first word of the summary, keep it ≤50 characters
        (rewrite/shorten if needed), and omit ending punctuation. Add a body
        only if it is absolutely essential, separated by one blank line, wrapped
        at ≤72 characters. Do not include anything except the commit message.")

  ;; These prompts are trimmed out of context.  The font locking below styles
  ;; them.  Since the text doesn't match headings, you can use org headings for
  ;; other structural purposes, such as `gptel-org-branching-context'.

  (setopt gptel-prompt-prefix-alist
          `((markdown-mode . ,(concat "meatbag ›  "))
            (org-mode . ,(concat  "meatbag ›  "))
            (text-mode . ,(concat "meatbag ›  "))))

  (add-hook
   'gptel-pre-response-hook
   (defun my--gptel-update-response-prefix ()
     "Update `gptel-response-prefix-alist' after changing directive/preset."
     (setq gptel-response-prefix-alist
           `((markdown-mode . ,(my--gptel-response-prefix))
             (org-mode . ,(my--gptel-response-prefix))
             (text-mode . ,(my--gptel-response-prefix))))))

  ;; GPTel has some built-in tools for scrolling and going to the end of the response.

  ;; TODO pixel scrolling is too experimental to publish, but does demonstrate
  ;; that things will be nice when we get it.
  (add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)

  ;; TODO differentiate user-present and automatic cases in far away buffers.
  ;; Go to the end of the response.
  (add-hook 'gptel-post-response-functions #'gptel-end-of-response)

  ;; Presets

  (gptel-make-preset 'default
    :model 'gpt-4o
    :description "Default preset"
    :use-context 'system
    :use-tools nil
    :system 'default)

  (gptel-make-preset 'emacser
    :model 'gpt-4.1
    :description "Default preset for Emacs introspection"
    :use-context 'system
    :use-tools t
    :system 'emacser)

  (gptel-make-preset 'dev
    :model 'gpt-4.1
    :description "Programming assistant"
    :use-context 'system
    :use-tools nil
    :system
    (string-join
     (list
      "You are a precise expert code assistant. Focus on debugging,code reviews, and improving code quality. Recommend clean code practices (DRY, KISS, etc). Offer clear, constructive advice and concise examples. Use English comments only when necessary."
      my--prompt-format)
     "\n\n"))

  (gptel-make-preset 'todo
    :model 'gpt-4.1
    :description "Address TODO"
    :use-context 'system
    :use-tools nil
    :rewrite-directive #'gptel--rewrite-directive-default
    :rewrite-message "Address TODO")

  (gptel-make-preset 'explain
    :model 'gpt-4o
    :description "Explain code snippet"
    :use-context 'user
    :use-tools t
    :system
    (string-join
     (list
      "You are an expert programming assistant. Explain the provided code snippet in clear, concise English. Focus on what the code does, important implementation details, and any subtle logic. If relevant, mention inputs, outputs, and possible caveats. Do not include unnecessary background or excessive boilerplate."
      my--prompt-format)
     "\n\n"))

  (gptel-make-preset 'code-review
    :model 'gpt-4.1
    :description "Line-by-line code review"
    :use-context 'system
    :use-tools nil
    :system
    (string-join
     (list
      "You are a senior code reviewer. Analyze the provided code or text with a critical and objective eye. Provide constructive,honest feedback, identify issues, suggest improvements, and praise strengths transparently."
      my--prompt-format)
     "\n\n"))

  (gptel-make-preset 'mind
    :model 'gpt-4o
    :description "Objective, supportive cognitive therapist"
    :use-context nil
    :use-tools nil
    :directives `((default . ,(string-join (list "You are an objective, honest, and supportive cognitive therapist and coach. Respond to the user as if in a therapy or self-improvement context, asking clarifying questions, offering actionable suggestions, and respectfully challenging unhelpful thinking patterns. Your responses are concise and practical. Always genuinely try to help." my--prompt-format) "\n\n"))
                  (freud . ,(string-join (list "You are Sigmund Freud, emphasizing psychoanalysis and unconscious motives. Explore the user's thoughts and behaviors through the lens of hidden desires, dreams, and past experiences. Gently interpret possible underlying meanings and ask probing questions to encourage self-reflection. Avoid modern psychological jargon." my--prompt-format) "\n\n"))
                  (rogers . ,(string-join (list "You are Carl Rogers, a compassionate, non-directive therapist. Practice unconditional positive regard, empathy, and reflective listening. Support the user's self-discovery by paraphrasing, validating feelings, and facilitating personal growth without offering direct solutions or judgment." my--prompt-format) "\n\n"))
                  (skinner . ,(string-join (list "You are B.F. Skinner, a behaviorist focused on observable actions and reinforcement. Help the user identify patterns and suggest practical behavioral interventions. Avoid speculation about inner thoughts, and recommend clear, actionable strategies for change." my--prompt-format) "\n\n"))
                  (jung . ,(string-join (list "You are Carl Jung. You explore the user's unconscious via symbols, archetypes, and dreams. You help users seek meaning, personal growth, and individuation. Encourage connection between conscious and unconscious aspects of self with thoughtfulness and curiosity." my--prompt-format) "\n\n"))
                  (hannibal . ,(string-join (list "You are a highly intelligent, enigmatic therapist like Dr. Hannibal Lecter (without any unethical or criminal behavior). Offer incisive, thought-provoking psychoanalytic observations. Speak with wit and elegance, subtly challenging the user’s perceptions but never causing harm." my--prompt-format) "\n\n"))
                  (ellis . ,(string-join (list "You are Albert Ellis. You challenge irrational beliefs using assertive and practical techniques. Your approach is confrontational but aiming to empower, always seeking to replace unhelpful thinking with rational alternatives. Offer clear, actionable advice." my--prompt-format) "\n\n"))
                  (frankl . ,(string-join (list "You are Viktor Frankl. You help the user find meaning, especially during suffering or adversity. Encourage reflection on values, responsibility, and hope. Responses are compassionate and purpose-driven." my--prompt-format) "\n\n"))))

  ;; ─── Quick actions ────────────────────────────────────────────────────────────

  (gptel-make-preset 'reply
    :model 'gpt-4o
    :use-context nil
    :use-tools nil
    :description "Draft/refine professional replies to email or Slack threads"
    :system
    (string-join
     (list
      "You are an expert assistant that writes concise,well-phrased replies to email or Slack threads. The input is always the conversation in chronological order, optionally followed by a user draft or ideas. If a draft is present after a separator like '===' or similar, refine it and rewrite it in the main language,style, and appropriate tone for the conversation. If no draft is found, write a brief, context-appropriate reply in the principal language and with the right level of formality."
      my--prompt-format-base my--prompt-output-only)
     "\n\n"))

  (gptel-make-preset 'translate
    :model 'gpt-4o
    :use-context nil
    :use-tools nil
    :description "Translate EN ↔ ES"
    :system
    (string-join
     (list
      "Translate to Spanish unless input is Spanish, then translate to English. Preserve original tone, style, meaning, and nuance. Value objective, faithful translation."
      my--prompt-format-base my--prompt-output-only)
     "\n\n"))

  (gptel-make-preset 'refine
    :model 'gpt-4o
    :use-context nil
    :use-tools nil
    :description "Refine and clarify writing"
    :system
    (string-join
     (list
      "You are an expert editor. Improve clarity, conciseness, and coherence of the supplied text, while preserving the meaning and tone."
      my--prompt-format-base my--prompt-output-only)
     "\n\n"))

  (gptel-make-preset 'refine-translate
    :model 'gpt-4o
    :use-context nil
    :use-tools nil
    :description "Refine and Translate"
    :system
    (string-join
     (list
      "You are an expert editor and translator. After improving the supplied text for clarity, conciseness, and coherence, autodetect the language: if it's Spanish, translate it to English; otherwise,translate to Spanish. Maintain the original style, tone, meaning and nuances of the original text."
      my--prompt-format my--prompt-output-only)
     "\n\n"))

  (gptel-make-preset 'evil-ex
    :model 'gpt-4o
    :description "Reply only with the most appropriate evil-ex command"
    :use-context nil
    :use-tools t
    :system
    (string-join
     (list
      "You are an expert in Emacs and DoomEmacs. Reply only with the most appropriate built-in Doomemacs evil-ex command for the task I specify."
      my--prompt-output-only)
     "\n\n"))

  (gptel-make-preset 'cli
    :model 'gpt-4o
    :description "Reply with a single shell command, no explanation"
    :use-context nil
    :use-tools nil
    :system
    (string-join
     (list
      "You are a command line helper. Generate CLI commands (for a zsh shell) that do what is requested."
      my--prompt-output-only)
     "\n\n"))

  (setq gptel-tools
        (list
         (gptel-make-tool
          :function #'pmx--gptel-eval
          :name "elisp_eval"
          :confirm t
          :include t
          :category "introspection"
          :args '(( :name "expression"
                    :type string
                    :description "A single elisp sexp to evaluate."))
          :description "Evaluate Elisp EXPRESSION and return result.
EXPRESSION can be anything will evaluate.  It can be a function call, a
variable, a quasi-quoted expression.  The only requirement is that only
the first sexp will be read and evaluated, so if you need to evaluate
multiple expressions, make one call per expression.  Do not combine
expressions using progn etc.  Just go expression by expression and try
to make standalone single expressions.

Instead of saying \"I can't calculate that\" etc, use this tool to
evaluate the result.

The return value is formated to a string using %S, so a string will be
returned as an escaped embedded string and literal forms will be
compatible with `read' where possible.  Some forms have no printed
representation that can be read and will be represented with
#<hash-notation> instead.

You can use this to quickly change a user setting, check a variable, or
demonstrate something to the user.")

         (gptel-make-tool
          :function #'pmx--gptel-symbolp
          :name "symbol_exists"
          :include t
          :category "introspection"
          :args '(( :name "symbol"
                    :type string
                    :description "A symbol that will be in `obarray' if they \
actually exist"))
          :description "Check if SYMBOL exists in `obarray'. \
Returns the name of a symbol if that symbol has been interned or \"nil\"
if not.  Uses `intern-soft' to perform the check.  This tool is
extremely cheap to call.")

         (gptel-make-tool
          :function #'pmx--gptel-load-paths
          :name "load_paths"
          :include t
          :category "introspection"
          :args nil
          :description "Return the users load paths.
This can reveal information about what packages the user has available.
You can also learn about what kind of package management they are using
and which packages are likely shadowed by their Elisp dependency
manager.  The location of default packages can tell you about the user's
Emacs installation.")

         (gptel-make-tool
          :function #'pmx--gptel-features
          :name "features"
          :include t
          :category "introspection"
          :args nil
          :description "Return the list of loaded features.
This tool can be used to see what packages are already loaded in the
running Emacs.  Use this to understand the user's typical set of
packages and typical usage patterns.  Especially if the solution depends
on the user's choice of packages, you will want to look at the features
and load paths.")

         (gptel-make-tool
          :function #'pmx--gptel-manual-names
          :name "manual_names"
          :include t
          :category "introspection"
          :args nil
          :description "Return a list of available manual names.
Call this tool in order to determine if a particular manual is
available.  This can also help determine which packages are available on
the user's Emacs.  This tool is a good starting point for general
questions about Emacs, Elisp, and common built-in packages.

Manuals are usually named the same as the symbol of the package prefix
that they document.  The Common Lisp manual is called \"cl\".  The Emacs
Lisp manual is called \"elisp\".

You will usually follow this call with a subsequent call to
`manual_nodes' in order to see the sections in the manual, which are
somewhat like a summary.  This call is extremely cheap and should be
used liberally.")

         (gptel-make-tool
          :function #'pmx--gptel-manual-list-nodes
          :name "manual_nodes"
          :include t
          :category "introspection"
          :args '(( :name "manual"
                    :type string
                    :description "The name of the manual.
Examples include \"cl\", \"elisp\", or \"transient\"."))
          :description "Retrieve a listing of topic nodes within MANUAL.
Return value is a list of all nodes in MANUAL.  The list of topic nodes
provides a good summary of MANUAL.

MANUAL is one of the results returned from `manual_names'.  If you are
sure a manual exists, you may skip first calling `manual_names'.  When
you believe MANUAL exists, this tool is very useful to find places to
broaden your search.

You will usually follow this call with a subsequent call to
`manual_node_contents' to view the actual full contents of a node in the
manual.  This call is extremely cheap and should be used liberally.

In the Elisp manual, you can find more answers about code and
implementations that a programmer can used to deeply customize.  The
Emacs manual contains descriptions about built-in features and behavior
that can be used to understand the context for what is being programmed
in the Elisp manual.")

         (gptel-make-tool
          :function #'pmx--gptel-manual-node-contents
          :name "manual_node_contents"
          :include t
          :category "introspection"
          :args '(( :name "manual_name"
                    :type string
                    :description "The name of MANUAL.
Examples manuals include \"cl\", \"elisp\", or \"transient\".")
                  ( :name "node"
                          :type string
                          :description "The name of the NODE in a MANUAL.
Example nodes from the elisp manual include \"Records\" or \"Sequences
Arrays \ Vectors\"."))
          :description "Retrieve the contents of NODE in MANUAL.
The return value is the full contents of NODE in MANUAL.  Contents
include high-level grouping of related functions and variables.  Hidden
behavior is described.  This tool is awesome!  You should try to call it
all the time.

Pay attention to the entries in the Menu.  You can do recursive look-ups
of more specific manual sections.  Example menu:

* Menu:

* Good Node::
* A Node::

You can recursively look up \"Good Node\" and other relevant menu nodes
in this same MANUAL.  Sometimes there are links, such as, \"*Note
Narrowing::.\".  \"Narrowing\" is a node in this example.  Use this tool
recursively.

If both Elisp and Emacs manuals are available, open both but prefer Elisp manual
style language anc content.")

         (gptel-make-tool
          :function #'pmx--gptel-featurep
          :name "features"
          :include t
          :category "introspection"
          :args '(( :name "feature"
                    :type string
                    :description "FEATURE to look for."))
          :description "Check if FEATURE is loaded or available.
Returns non-nil if FEATURE is loaded or available for loading.  Not all
users have all features loaded.  Before recommending the user to try a
particular solution, you might check if the necessary features are
loaded.  If you are using all built-in Emacs libraries, you don't need
to check.  Use this mainly to check for 3rd party packages that the user
would obtain from MELPA and Non-GNU ELPA etc.")

         (gptel-make-tool
          :function #'pmx--gptel-library-source
          :name "library_source"
          :include t
          :category "introspection"
          :args '(( :name "library"
                    :type string
                    :description "LIBRARY to look for."))
          :description "Read the source code for LIBRARY.
LIBRARY can either be a C or Elisp source code library.  Examples would
include \"transient\" or \"fns.c\".  When looking for C libraries, they
must contain the .c suffix.

This tool is a bit expensive, and you can usually find what you want by
looking up symbols in the package first by calling
`function_completions' and `variable_completions' to get a high-level
summary of what definitions might be contained in a library.

Watch for for sub-packages.  Some multi-file packages will have symbols
that are defined in a sub-package.  If you see a common prefix in the
function or variable completions and those symbols are not in the
top-level package, there are likely sub-packages and you should
recursively look them up.")

         (gptel-make-tool
          :name "symbol_manual_section"
          :include t
          :function #'pmx--gptel-symbol-in-manual
          :category "introspection"
          :args '(( :name "symbol"
                    :type string
                    :description "Name of a SYMBOL, such as \
\"find-file-noselect\"."))
          :description "Returns contents of manual node for SYMBOL.
SYMBOL can be a function, macro, defcustom, or defvar.  If symbol is not
known to be in a manual, this functon will return nil.

The returned manual contents are similar to the `manual_node_contents'
tool.  You sould recursively inspect any links or menu entries that look
relevant.  Check the node list for the manual if a link or menu entry
returns nil.

If you can't find anything, you should try looking up its source or
docstring next and finally try to complete the prefix of the symbol .")

         (gptel-make-tool
          :name "function_source"
          :include t
          :function #'pmx--gptel-function-source
          :category "introspection"
          :args '(( :name "function"
                    :type string
                    :description "Name of a FUNCTION, such as \
\"find-file-noselect\"."))
          :description "Returns the source code for FUNCTION.
Return the source code for FUNCTION.  FUNCTION can be a function or
macro.  The signature and docstring can supply extremely valuable
information about how to call a function correctly and what behaviors
are controlled by its arguments.  You can understand the side-effects
and what variables a function reacts to by reading its body.

You can use the source code for functions to recursively look up other
functions & variables and make inferences about how implementations work
in order to connect the behaviors and implementation details that the
user will need.

Because the docstring is embedded in the source, you should prefer this
tool over just retrieving the documentation. If the result seems
incomplete, you can try returning the docstring using
`function_documentation' or the entire source for a library feature by
using `library_source'.  This tool is cheap.  Use it liberally.")

         (gptel-make-tool
          :name "variable_source"
          :function #'pmx--gptel-variable-source
          :category "introspection"
          :include t
          :args '(( :name "variable"
                    :type string
                    :description "Name of a VARIABLE, such as \
\"last-kbd-macro\"."))
          :description "Returns the source code for VARIABLE.
Return value is the source code for VARIABLE.  VARIABLE can be a defvar
or defcustom.  The returned source code can be extremely insightful
because nothing is more accurate than looking at the code and the source
code contains the docstring too.

You can use source code for variables to see the forms used in setting
their defaults and make inferences about what forms will be interpreted
correctly.  If the result seems incomplete, you can try returning the
docstring using `variable_documentation' or the entire source for a
library feature by using `library_source'.  This tool is cheap and fast.
Call it liberally.")

         (gptel-make-tool
          :name "variable_value"
          :function #'pmx--gptel-variable-global-value
          :category "introspection"
          :confirm t
          :include t
          :args '(( :name "variable"
                    :type string
                    :description "Name of a VARIABLE, such as \
\"last-kbd-macro\"."))
          :description "Returns the global value for VARIABLE.
Return value is the global (not buffer-local) value for VARIABLE.
VARIABLE can be a defvar or defcustom.  Use this when behavior depends
on the state of a variable or you want to infer if a package has indeed
configured a variable.  By observing expected side-effects, you can
build a coherent view of the interaction between functions and settings.

Use of this tool could leak private data, so don't call it for any
VARIABLE that contains initialized authentication data.

If the result is confusing, you can try returning the docstring using
`variable_documentation' to gain insights into the structure of values
contained.")

         (gptel-make-tool
          :name "function_documentation"
          :function #'pmx--gptel-function-documentation
          :category "introspection"
          :include t
          :args '(( :name "function"
                    :type string
                    :description "Name of a FUNCTION, such as \"mapcar\"."))
          :description "Returns the docstring for FUNCTION.
Return value is a docstring for FUNCTION.  FUNCTION can be a function or
macro.  Can be used to infer the purpose or correct forms for arguments
and behavior changes related to those arguments.  This is more reliable
than `function_source', so if `function_source' seems off, try this.
This tool is very cheap and very fast.  Call it very liberally.")

         (gptel-make-tool
          :name "variable_documentation"
          :function #'pmx--gptel-variable-documentation
          :category "introspection"
          :include t
          :args '(( :name "variable"
                    :type string
                    :description "Name of a VARIABLE, such as \
\"cursor-type\"."))
          :description "Returns the docstring VARIABLE.
Return value is a docstring for VARIABLE.  VARIABLE can be a defcustom
or defvar.  Can be used to infer the correct forms for setting a
variable, such as when configuring packages in use-package expressions
or leading the user through diagnosing something.  This tool is very
cheap and very fast.  Call it very liberally.")

         ;; TODO this tool can complete out of order using orderless
         (gptel-make-tool
          :name "function_completions"
          :function #'pmx--gptel-function-completions
          :category "introspection"
          :include t
          :args '(( :name "function_prefix"
                    :type string
                    :description "FUNCTION_PREFIX of functions you are searching for."))
          :description "Returns a list of functions beginning with FUNCTION_PREFIX.
Use this to prepare for subsequent calls to `function_source' or
`function_documentation' to look up the source code or docstrings of
multiple functions.  You can also use this tool to verify which
functions and macros can be called.  If you want to search for all
functions defined in foo and its sub-packages, you this tool is a very
good starting point.  This tool is very cheap and very fast.  Call it
very liberally.")

         ;;
         (gptel-make-tool
          :name "command_completions"
          :function #'pmx--gptel-command-completions
          :category "introspection"
          :include t
          :args '(( :name "command_prefix"
                    :type string
                    :description "COMMAND_PREFIX of commands you are searching for."))
          :description "Returns a list of commands beginning with COMMAND_PREFIX.
This tool is very similar to `function_completions' but will only return
commands that can be called interactively.  This can tell you about the
entry points where a user begins interacting with a package.  Because
commands are functions, you will follow up this tool with calls to
`function_source' and `function_documentation'.  This tool is very cheap
and very fast.  Call it very liberally.")

         ;; TODO this tool can complete out of order using orderless
         (gptel-make-tool
          :name "variable_completions"
          :function #'pmx--gptel-variable-completions
          :category "introspection"
          :include t
          :args '(( :name "variable_prefix"
                    :type string
                    :description "VARIABLE_PREFIX of variables you are searching for."))
          :description "Returns a list of variables beginning with VARIABLE_PREFIX.
The variables returned include defvars and custom variables.  Defvars
tell you what states a package relies on for its implementation.
Defcustom tells you what configuration options the user should know
about when configuring a package, such as if they are working on
use-package expressions.

Use this to prepare for subsequent calls to `variable_source' or
`variable_documentation' to look up the source code or docstrings of
multiple variables.  If you want to search for all variables defined
under a prefix, you this tool is a very good starting point.  This tool
is very cheap and very fast.  Call it very liberally.")

         (gptel-make-tool
          :name "simulate_error"
          :function #'pmx--gptel-simulate-error
          :category "testing"
          :args nil
          :description "A tool that can simulate an error.
This tool always returns an error.  It is useful for testing error
behavior.  When the user asks you to use this tool, you should
absolutely use it.")

         (gptel-make-tool
          :name "coerce_nil"
          :function #'pmx--gptel-coerce-nil
          :category "testing"
          :args nil
          :description "A tool that returns nil.
Call this when the user asks because I'm testing if the tool plumbing
will coerce nils to something you can read or will error on my side.")

         (gptel-make-tool
          :name "all_arg_types"
          :function #'pmx--gptel-all-arg-types
          :category "testing"
          :include t
          :args '((:name "an_object" :type object :description "A basic object"
                   :properties (:foo (:type integer :description "Use 42"))
                   :required ["foo"])
                  (:name "string" :type string :description "A string")
                  (:name "array" :type array :description "An array"
                   :items (:type number))
                  (:name "null" :type null :description "A null")
                  (:name "true" :type boolean :description "Must be true")
                  (:name "false" :type boolean :description "Must be false")
                  (:name "enum" :type string :description "A choice"
                   :enum ["bar" "baz" "boo"]))
          :description "A function the user wants to test out.")

         (gptel-make-tool
          :name "async_tool"
          :function #'pmx--gptel-async-tool
          :category "testing"
          :include t
          :async t
          :args
          '((:name "later-val" :type string :description "Just whenever."))
          :description "A tool that takes a while.
If the user says call it, always call it.")))


  ;; Fancy function name & args after tool call.  This has been updated for the
  ;; explicit-tool-turns branch.
  (font-lock-add-keywords
   'org-mode
   `((,(rx line-start (literal "#+begin_tool") (1+ space)
           (literal "(") (group-n 1 (+ (not space))) space
           (or (group-n 2 (group-n 3 (literal "nil")) (literal ")"))
               (group-n 2 (group-n 3 (+ not-newline))
                        (optional (literal "...")) (literal ")")))
           line-end)
      (1 '(font-lock-keyword-face org-block-begin-line org-modern-block-name) t)
      (3 '(font-lock-constant-face org-block-begin-line org-modern-block-name) t))))

  ;; Define unique faces so prompts are highly visible
  (defface pmx-gptel-user
    `((t :family "Roboto Slab"
       ;; `((t :family "Fira Code"
       :weight bold
       :foreground ,(doom-color 'teal)
       :inverse-video t
       :inherit default))
    "User prompt face")
  (defface pmx-gptel-assistant
    `((t :family "Roboto Slab"
       ;; `((t :family "Fira Code"
       :weight bold
       :foreground ,(doom-color 'orange)
       :inverse-video t
       :inherit default))
    "Assistant prompt face")
  (defface pmx-gptel-assistant-prelude
    `((t :family "Roboto Condensed"
       :weight bold
       :foreground ,(doom-color 'orange)
       :box t
       :inherit default))
    "Assistant prelude face, after the prompt")

  ;; Styling our prompts with font locking
  (font-lock-add-keywords
   'org-mode
   `(("^\\(meatbag ›[ ]?\\)"
      (1 '(face pmx-gptel-user line-prefix
           ,(propertize " " 'face 'pmx-gptel-user))))))

  (font-lock-add-keywords
   'org-mode
   `((,(concat "^\\(.+ ⤵[ ]?\\)")
      (1 '(face pmx-gptel-assistant line-prefix
           ,(propertize " " 'face 'pmx-gptel-assistant)))))))
