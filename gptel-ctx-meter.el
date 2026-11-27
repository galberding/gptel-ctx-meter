;;; gptel-ctx-meter.el --- Context-window occupancy meter for gptel -*- lexical-binding: t; -*-

;; Author: Georg (with Claude)
;; Package-Requires: ((emacs "29.1") (gptel "0.9.9"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shows how full the model's context window is, in the mode line of
;; gptel buffers:
;;
;;     ctx 47k/180k 26% C45k
;;
;; USED = prompt_tokens + completion_tokens of the most recent *API
;; round* (one HTTP request/response).  It comes from the `usage' object
;; the server returns, counted with the server's own tokenizer, and is
;; read from the live request state (`gptel--fsm-last'), so it also
;; updates between tool calls during a long agent loop.
;;
;; This is NOT what gptel's header-line shows.  The "Last request" there
;; is `:tokens-full', the sum over every tool round-trip in one
;; `gptel-send', so a 30-round agent turn at 50k context shows ~1.5M↑.
;; The "This buffer" value is cumulative billing, not occupancy.
;;
;; WINDOW is resolved in this order:
;;   1. `gptel-ctx-meter-windows' (your explicit override, always wins)
;;   2. a server probe, cached per (host . model):
;;        llama-server  ->  GET /props?model=M   default_generation_settings.n_ctx
;;                          (per-slot n_ctx, already divided by --parallel)
;;        Copilot       ->  GET /models   capabilities.limits.max_prompt_tokens
;;                          (the limit Copilot actually enforces, often
;;                          below the model's native window)
;;   3. the model's `:context-window' property (thousands of tokens)
;;
;; C = cached prompt tokens (prompt_tokens_details.cached_tokens).  On
;; llama-server this is the prefix reused from the slot's KV cache; the
;; tooltip shows the cache-hit ratio.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'map)
(require 'url)
(require 'url-http)

(declare-function gptel--gh-auth "gptel-gh")
(declare-function gptel--gh-token "gptel-gh")
(declare-function gptel--gh-p "gptel-gh")

(defgroup gptel-ctx-meter nil
  "Context-window occupancy meter for gptel."
  :group 'gptel)

(defcustom gptel-ctx-meter-windows nil
  "Alist of (MODEL-NAME . TOKENS) overriding the context-window size.
MODEL-NAME is the string sent to the API, as returned by
`gptel--model-name'.  For llama-server, use the per-slot value,
i.e. ctx-size divided by the number of parallel slots."
  :type '(alist :key-type string :value-type integer))

(defcustom gptel-ctx-meter-warn 0.70
  "Fraction of the window at which the meter turns to `warning' face."
  :type 'float)

(defcustom gptel-ctx-meter-critical 0.85
  "Fraction of the window at which the meter turns to `error' face."
  :type 'float)

(defcustom gptel-ctx-meter-auto-probe t
  "When non-nil, probe the server once per (host . model) after a response.
The probe runs only right after the model answered, so for a
llama-server router the model is already loaded and the probe does
not cause a model load."
  :type 'boolean)

(defvar gptel-ctx-meter--windows-cache (make-hash-table :test #'equal)
  "Probed window sizes: (HOST . MODEL-NAME) -> tokens, or `failed'.")

(defvar-local gptel-ctx-meter--stale nil
  "Non-nil when the buffer was rewritten (e.g. compacted) after the last round.")

;;;; Reading usage

(defun gptel-ctx-meter-last-round ()
  "Token plist (:input :cached :output) of the latest API round, or nil.
:input excludes cached tokens, as gptel stores them."
  (and-let* (((boundp 'gptel--fsm-last))
             (fsm gptel--fsm-last)
             (info (gptel-fsm-info fsm))
             ((eq (plist-get info :buffer) (current-buffer))))
    (plist-get info :tokens)))

(defun gptel-ctx-meter-used (&optional tokens)
  "Tokens occupied after the latest round: prompt + completion.
TOKENS defaults to `gptel-ctx-meter-last-round'."
  (when-let* ((tok (or tokens (gptel-ctx-meter-last-round))))
    (+ (or (plist-get tok :input) 0)
       (or (plist-get tok :cached) 0)
       (or (plist-get tok :output) 0))))

(defun gptel-ctx-meter--key ()
  (cons (gptel-backend-host gptel-backend) (gptel--model-name gptel-model)))

(defun gptel-ctx-meter-window ()
  "Context window in tokens for the current buffer's backend/model, or nil."
  (let ((name (gptel--model-name gptel-model))
        (probed (gethash (gptel-ctx-meter--key) gptel-ctx-meter--windows-cache)))
    (or (cdr (assoc name gptel-ctx-meter-windows))
        (and (integerp probed) probed)
        (and-let* (((symbolp gptel-model))
                   (k (get gptel-model :context-window)))
          (round (* k 1000))))))

(defun gptel-ctx-meter-fraction ()
  "Fill fraction of the context window, or nil if unknown."
  (and-let* ((used (gptel-ctx-meter-used))
             (win (gptel-ctx-meter-window))
             ((> win 0)))
    (/ (float used) win)))

;;;; Display

(defun gptel-ctx-meter--h (n)
  (file-size-human-readable n 'si))

(defun gptel-ctx-meter-string ()
  "Mode-line string for the meter."
  (when-let* ((tok (gptel-ctx-meter-last-round)))
    (let* ((cached (or (plist-get tok :cached) 0))
           (input  (or (plist-get tok :input) 0))
           (output (or (plist-get tok :output) 0))
           (used   (+ input cached output))
           (win    (gptel-ctx-meter-window))
           (frac   (and win (> win 0) (/ (float used) win)))
           (prompt (+ input cached)))
      (propertize
       (concat " ctx " (if gptel-ctx-meter--stale "~" "")
               (gptel-ctx-meter--h used)
               (if frac
                   (format "/%s %d%%" (gptel-ctx-meter--h win) (round (* 100 frac)))
                 "/?")
               (if (> cached 0) (concat " C" (gptel-ctx-meter--h cached)) ""))
       'face (cond ((null frac) 'shadow)
                   ((>= frac gptel-ctx-meter-critical) 'error)
                   ((>= frac gptel-ctx-meter-warn) 'warning)
                   (t 'success))
       'help-echo
       (format "Latest API round:\n  prompt %d (cached %d, %s cache hit)\n  completion %d\nWindow: %s%s"
               prompt cached
               (if (> prompt 0) (format "%.0f%%" (* 100.0 (/ (float cached) prompt))) "n/a")
               output
               (if win (format "%d tokens" win) "unknown — M-x gptel-ctx-meter-probe")
               (if gptel-ctx-meter--stale
                   "\n(buffer rewritten since; value is pre-rewrite)" ""))))))

(defun gptel-ctx-meter--refresh (&rest _)
  (force-mode-line-update))

(defun gptel-ctx-meter--after-response (&rest _)
  (setq gptel-ctx-meter--stale nil)
  (when (and gptel-ctx-meter-auto-probe
             (null (gethash (gptel-ctx-meter--key) gptel-ctx-meter--windows-cache))
             (null (cdr (assoc (gptel--model-name gptel-model) gptel-ctx-meter-windows))))
    (gptel-ctx-meter-probe 'quiet))
  (force-mode-line-update))

(defun gptel-ctx-meter-mark-stale (&rest _)
  "Mark the meter value as stale, e.g. after compaction rewrote the buffer."
  (setq gptel-ctx-meter--stale t)
  (force-mode-line-update))

;;;; Probing the server

(defun gptel-ctx-meter--fetch (url headers callback)
  "GET URL with HEADERS asynchronously; call CALLBACK with parsed JSON or nil."
  (let ((url-request-method "GET")
        (url-request-extra-headers headers)
        ;; Same keepalive problem as gptel--gh-renew-token: reused
        ;; connections can return header-only responses.
        (url-http-attempt-keepalives nil))
    (url-retrieve
     url
     (lambda (status)
       (let ((json
              (unless (plist-get status :error)
                (ignore-errors
                  (goto-char (point-min))
                  (when (re-search-forward "\r?\n\r?\n" nil t)
                    (json-parse-buffer :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil))))))
         (kill-buffer (current-buffer))
         (funcall callback json)))
     nil t t)))

(defun gptel-ctx-meter--gh-backend-p (backend)
  (and (fboundp 'gptel--gh-p) (gptel--gh-p backend)))

;;;###autoload
(defun gptel-ctx-meter-probe (&optional quiet)
  "Ask the server for the current model's context window and cache it.
llama-server: /props?model=M -> default_generation_settings.n_ctx.
Copilot: /models -> capabilities.limits.max_prompt_tokens.
With QUIET, do not report in the echo area."
  (interactive)
  (let* ((buf (current-buffer))
         (backend gptel-backend)
         (key (gptel-ctx-meter--key))
         (model (cdr key))
         (base (concat (gptel-backend-protocol backend) "://"
                       (gptel-backend-host backend)))
         (report (lambda (win src)
                   (puthash key (or win 'failed) gptel-ctx-meter--windows-cache)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf (force-mode-line-update)))
                   (unless quiet
                     (message (if win "gptel-ctx-meter: %s window = %d (%s)"
                                "gptel-ctx-meter: probe failed for %s%s (%s)")
                              model (or win "") src)))))
    (puthash key 'pending gptel-ctx-meter--windows-cache)
    (if (gptel-ctx-meter--gh-backend-p backend)
        (let ((gptel-backend backend))
          (gptel--gh-auth)
          (gptel-ctx-meter--fetch
           (concat base "/models")
           `(("authorization" . ,(concat "Bearer "
                                         (plist-get (gptel--gh-token backend) :token)))
             ("copilot-integration-id" . "vscode-chat"))
           (lambda (json)
             (funcall report
                      (cl-loop for m in (plist-get json :data)
                               when (equal (plist-get m :id) model)
                               return (map-nested-elt
                                       m '(:capabilities :limits :max_prompt_tokens)))
                      "Copilot /models max_prompt_tokens"))))
      (gptel-ctx-meter--fetch
       (concat base "/props?model=" (url-hexify-string model))
       (when-let* ((key (gptel-backend-key backend))
                   (key (if (functionp key) (funcall key) key)))
         `(("Authorization" . ,(concat "Bearer " key))))
       (lambda (json)
         (funcall report
                  (or (map-nested-elt json '(:default_generation_settings :n_ctx))
                      (plist-get json :n_ctx))
                  "llama-server /props n_ctx"))))))

;;;; Minor mode

(defconst gptel-ctx-meter--mode-line-entry
  '(gptel-ctx-meter-mode (:eval (gptel-ctx-meter-string))))

;;;###autoload
(define-minor-mode gptel-ctx-meter-mode
  "Show context-window occupancy of the current gptel buffer in the mode line."
  :lighter nil
  (if gptel-ctx-meter-mode
      (progn
        (add-to-list 'mode-line-misc-info gptel-ctx-meter--mode-line-entry)
        (add-hook 'gptel-post-response-functions #'gptel-ctx-meter--after-response nil t)
        ;; Runs at every TPRE (before each tool call) and at DONE/ERRS/ABRT,
        ;; so the meter moves during long tool loops.
        (advice-add 'gptel--update-token-usage :after #'gptel-ctx-meter--refresh))
    (remove-hook 'gptel-post-response-functions #'gptel-ctx-meter--after-response t)))

(provide 'gptel-ctx-meter)
;;; gptel-ctx-meter.el ends here
