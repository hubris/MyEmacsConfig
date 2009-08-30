;;; nasm-mode.el --- major mode for editing NASM code

;; Copyright (C) 1999 chaos development

;; Author: Per Lundberg <plundis@debian.org>
;; Maintainer: chaos development
;; Keywords: tools, languages, NASM

;;; Commentary:

;; This mode was written by Per Lundberg <plundis@debian.org>,
;; based on an earlier asm-mode by Eric S. Raymond.

;; This mode is based on text mode.  It defines a private abbrev table
;; that can be used to save abbrevs for assembler mnemonics.  It binds just
;; three keys:
;;
;;	TAB		tab to next tab stop
;;	C-j, C-m	newline and jump to this line's indentation level

;; This mode runs two hooks:
;;   1) An nasm-mode-set-comment-hook before the part of the initialization
;; depending on nasm-comment-char, and
;;   2) an nasm-mode-hook at the end of initialization.

;;; Code:

(defgroup nasm nil
  "NASM programming"
  :group 'asm)

(defcustom nasm-comment-char ?		;
  "*The comment-start character assumed by NASM mode."
  :type 'sexp
  :group 'nasm)

(defcustom nasm-mode-syntax-table nil
  "Syntax table used while in NASM mode.")

(defvar nasm-mode-abbrev-table nil
  "Abbrev table used while in NASM mode.")
(define-abbrev-table 'nasm-mode-abbrev-table ())

(defvar nasm-mode-map nil
  "Keymap for NASM mode.")

(if nasm-mode-map
    nil
  (setq nasm-mode-map (make-sparse-keymap 'nasm-mode-map))
  (define-key nasm-mode-map ":"	        'asm-colon)
  (define-key nasm-mode-map "\C-c;"	'comment-region)
  (define-key nasm-mode-map "\C-i"	'tab-to-tab-stop)
  (define-key nasm-mode-map "\C-j"	'nasm-newline)
  (define-key nasm-mode-map "\C-m"	'nasm-newline)
  )

(defvar nasm-font-lock-keywords
  '(
    ("%endm" . font-lock-preprocessor-face)
    ("%include" . font-lock-preprocessor-face)
    (".+:$" . font-lock-variable-name-face)
    (";.*" . font-lock-comment-face)
    ("\\b[a-d]\\(l\\|h\\)\\b" . font-lock-type-face)
    ("\\be?[a-d]x\\b" . font-lock-type-face)
    ("\\be?[ds]i\\b" . font-lock-type-face)
    ("\\be?\\(b\\|s\\)p\\b" . font-lock-type-face)
    ("\\bd\\(b\\|d\\|w\\|q\\|t\\)\\b" . font-lock-type-face)
    ("\\b%[A-Za-z]+\\b" . font-lock-preprocessor-face)
    ("\".+\"" . font-lock-doc-string-face)
    ("'.+'" . font-lock-string-face)
    ("\\bres\\(b\\|d\\|w\\|q\\|t\\)\\b" . font-lock-keyword-face)
    ("incbin " . font-lock-keyword-face)
    ("equ " . font-lock-keyword-face)
    ("times " . font-lock-keyword-face)
    ("seg " . font-lock-keyword-face)
    ("wrt " . font-lock-keyword-face)
    ("bits " . font-lock-keyword-face)
    ("section " . font-lock-keyword-face)
    ("align " . font-lock-keyword-face)
    ("segment " . font-lock-keyword-face)
    ("absolute " . font-lock-keyword-face)
    ("extern " . font-lock-keyword-face)
    ("global " . font-lock-keyword-face)
    ("common " . font-lock-keyword-face)
    ("org " . font-lock-keyword-face)
    ("loop " . font-lock-keyword-face)
    ("\\b\\(MOV\\|XCHG\\|STC\\|CLC\\|CMC\\|STD\\|CLD\\|STI\\|CLI\\|PUSH\\|PUSHF\\|PUSHA\\|POP\\|POPF\\|POPA\\|CBW\\|CWD\\|CWDE\\|IN\\|OUT\\|ADD\\|ADC\\|SUB\\|SBB\\|DIV\\|DIV\\|DIV\\|IDIV\\|IDIV\\|IDIV\\|MUL\\|MUL\\|MUL\\|IMUL\\|IMUL\\|IMUL\\|INC\\|DEC\\|CMP\\|SAL\\|SAR\\|RCL\\|RCR\\|ROL\\|ROR\\|NEG\\|NOT\\|AND\\|OR\\|XOR\\|SHL\\|SHR\\|NOP\\|LEA\\|INT\\|CALL\\|JMP\\|JE\\|JZ\\|JCXZ\\|JP\\|JPE\\|RET\\|JNE\\|JNZ\\|JECXZ\\|JNP\\|JPO\\|JA\\|JAE\\|JB\\|JBE\\|JNA\\|JNAE\\|JNB\\|JNBE\\|JC\\|JNC\\|JG\\|JGE\\|JL\\|JLE\\|JNG\\|JNGE\\|JNL\\|JNLE\\|JO\\|JNO\\|JS\\|JNS\\|SCAS[BWD]\\|REP\\([ZE]\\|N[ZE]\\)?\\|MOVS[BWD]\\)\\b"  . font-lock-keyword-face)
    ("\\b\\(mov\\|xchg\\|stc\\|clc\\|cmc\\|std\\|cld\\|sti\\|cli\\|push\\|pushf\\|pusha\\|pop\\|popf\\|popa\\|cbw\\|cwd\\|cwde\\|in\\|out\\|add\\|adc\\|sub\\|sbb\\|div\\|div\\|div\\|idiv\\|idiv\\|idiv\\|mul\\|mul\\|mul\\|imul\\|imul\\|imul\\|inc\\|dec\\|cmp\\|sal\\|sar\\|rcl\\|rcr\\|rol\\|ror\\|neg\\|not\\|and\\|or\\|xor\\|shl\\|shr\\|nop\\|lea\\|int\\|call\\|jmp\\|je\\|jz\\|jcxz\\|jp\\|jpe\\|ret\\|jne\\|jnz\\|jecxz\\|jnp\\|jpo\\|ja\\|jae\\|jb\\|jbe\\|jna\\|jnae\\|jnb\\|jnbe\\|jc\\|jnc\\|jg\\|jge\\|jl\\|jle\\|jng\\|jnge\\|jnl\\|jnle\\|jo\\|jno\\|js\\|jns\\|scas[bwd]\\|rep\\([ze]\\|n[ze]\\)?\\|movs[bwd]\\|stos[bwd]\\)\\b" . font-lock-keyword-face)
    ;(".+:" . font-lock-function-name-face)
    )
  "Additional expressions to highlight in NASM mode.")

(put 'nasm-mode 'font-lock-defaults '(nasm-font-lock-keywords))
(defvar nasm-code-level-empty-comment-pattern nil)
(defvar nasm-flush-left-empty-comment-pattern nil)
(defvar nasm-inline-empty-comment-pattern nil)

;;;###autoload
(defun nasm-mode ()
  "Major mode for editing NASM code.
Features a private abbrev table and the following bindings:

\\[nasm-colon]\toutdent a preceding label, tab to next tab stop.
\\[tab-to-tab-stop]\ttab to next tab stop.
\\[nasm-newline]\tnewline, then tab to next tab stop.
\\[nasm-comment]\tsmart placement of assembler comments.

The character used for making comments is set by the variable
`nasm-comment-char' (which defaults to `? ;').

Alternatively, you may set this variable in `asm-mode-set-comment-hook',
which is called near the beginning of mode initialization.

Turning on NASM mode runs the hook `nasm-mode-hook' at the end of initialization.

Special commands:
\\{nasm-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "NASM")
  (setq major-mode 'nasm-mode)
  (setq local-abbrev-table nasm-mode-abbrev-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(nasm-font-lock-keywords))
  (make-local-variable 'nasm-mode-syntax-table)
  (setq nasm-mode-syntax-table (make-syntax-table))
  (set-syntax-table nasm-mode-syntax-table)

  (run-hooks 'nasm-mode-set-comment-hook)
  ;; Make our own local child of nasm-mode-map
  ;; so we can define our own comment character.
  ;(let ((ourmap (make-sparse-keymap)))
  ;  (set-keymap-parents ourmap (list nasm-mode-map))
  ;  (use-local-map ourmap))
  (let ((cs (regexp-quote (char-to-string nasm-comment-char))))
    (make-local-variable 'comment-start)
    (setq comment-start (concat cs " "))
    (make-local-variable 'comment-start-skip)
    (setq comment-start-skip (concat cs "+[ \t]*"))
    (setq nasm-inline-empty-comment-pattern (concat "^.+" cs "+ *$"))
    (setq nasm-code-level-empty-comment-pattern (concat "^[\t ]+" cs cs " *$"))
    (setq nasm-flush-left-empty-comment-pattern (concat "^" cs cs cs " *$"))
    )
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (setq fill-prefix "\t")
  (run-hooks 'nasm-mode-hook))

(defun asm-colon()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space))
  (insert ":")
  )

(defun nasm-newline ()
  "Insert LFD + fill-prefix, to bring us back to code-indent level."
  (interactive)
  (if (eolp) (delete-horizontal-space))
  (insert "\n")
  (tab-to-tab-stop)
;  (tab-to-tab-stop)
  )


(defun nasm-line-matches (pattern &optional withcomment)
  (save-excursion
    (beginning-of-line)
    (looking-at pattern)))

(defun nasm-pop-comment-level ()
  ;; Delete an empty comment ending current line.  Then set up for a new one,
  ;; on the current line if it was all comment, otherwise above it
  (end-of-line)
  (delete-horizontal-space)
  (while (= (preceding-char) asm-comment-char)
    (delete-backward-char 1))
  (delete-horizontal-space)
  (if (bolp)
      nil
    (beginning-of-line)
    (open-line 1))
  )

(defun nasm-comment ()
  "Convert an empty comment to a `larger' kind, or start a new one.
These are the known comment classes:

1 -- comment to the right of the code (at the comment-column)
2 -- comment on its own line, indented like code
3 -- comment on its own line, beginning at the left-most column.

Suggested usage:  while writing your code, trigger nasm-comment
repeatedly until you are satisfied with the kind of comment."
  (interactive)
  (cond

   ;; Flush-left comment present?  Just insert character.
   ((nasm-line-matches nasm-flush-left-empty-comment-pattern)
    (insert nasm-comment-char))

   ;; If all else fails, insert character
   (t
    (insert nasm-comment-char))

   ))
;;; nasm-mode.el ends here
