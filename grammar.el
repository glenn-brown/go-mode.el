;; Copyright 2013 The Go Authors. All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

;;;; *** Currently, this file is only a sketch. ***
;;;;
;;;; This file parses the Go language as specified at
;;;; http://golang.org/ref/spec .
;;;;
;;;; This file relies on Emacs's fast built-in syntax analyzer to
;;;; identify comments, keywords, and strings.

(require 'cl)				; For concatenate
(require 'newcomment)			; For comment skipping
(require 'rx)				; For rx-to-string

;;;;;;;;;;;;;;;;
;;; Character classes, in SRE form.
;;; See http://www.ccs.neu.edu/home/shivers/papers/sre.txt or lisp function rx.
;;;;;;;;;;;;;;;;

(setq go-newline		`"\n")
; The spec's unicode-char class is split into 3 classes because the spec requires
; certain characters to be excluded.
(setq go-unicode-char--nobq	`(not (any "`" ))) ; no backquote
(setq go-unicode-char--nodq	`(not (any "\""))) ; no double quote
(setq go-unicode-char--nosq	`(not (any "'" ))) ; no single quote
(setq go-unicode-letter		`alpha)
(setq go-unicode-digit		`digit)
(setq go-letter			`(| "_" ,go-unicode-letter))
(setq go-decimal-digit		`(in "0-9"))
(setq go-octal-digit		`(in "0-7"))
(setq go-hex-digit		`(in "0-9A-Fa-f"))

;; A regexp to match any single letter.
(setq go-letter-regexp (rx-to-string go-letter t)) ; start of an identifier

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tokenization a.k.a. Lexing a.k.a. Lexical Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a symbol with the same name as S, but with a 'go-' prefix.
(defun go-prefix-symbol (s) (intern (concatenate 'string "go-" (symbol-name s))))

;; Replace all non-SRE symbols in an s-expression with their values.
;; FIXME: This assumes SRE symbols are <= 3 chars and non-SRE are longer.
(defun go-valuate (sexp)
  (mapcar (lambda (x) (cond ((listp x)   (go-valuate x))
			    ((and (symbolp x)
				  (> (length (symbol-name x)) 3))
			     (symbol-value (go-prefix-symbol x)))
			    (x)))
	  sexp))

(defun go-prefix-raw-tokens (raw-tokens)
  (dolist (x raw-tokens) (set (go-prefix-symbol (car x)) (go-valuate (cadr x)))))

;;;;;;;;;;;;;;;;
;;; Go tokens, in SRE form.
;;; We use the following SRE tokens:
;;; ':' Sequence
;;; '|' First match among alternatives.
;;; '*' Match zero-or-more alternatives.  Prefer more.
;;; 'opt' Match 0 or 1 occurences.  Prefer 1.
;;; '+' Match 1 or more occurences.  Prefer more.
;;;;;;;;;;;;;;;;
(setq
 go-tokens
 (go-prefix-raw-tokens
  '(
    ;; We don't use the -comment tokens.  Instead, our tokenizer relies
    ;; on Emacs's built-in syntax analyzer to identify them and relies on
    ;; the newcomment package to skip over them.
    ;; (line-comment	(rx (: "//" (* ,unicode-char)) "\n"))
    ;; (general-comment	(rx (: "/*" (* ???)  "*/")))
    (identifier		(: letter (* (| letter unicode-digit))))
    (decimal-lit		(: (in "1-9") (* decimal-digit)))
    (octal-lit		(: "0" (* octal-digit)))
    (hex-lit		(: "0" (| "x" "X") hex-digit (* hex-digit)))
    (int-lit		(| decimal-lit octal-lit hex-lit))
    (decimals		(: decimal-digit (* decimal-digit)))
    (exponent		(: (| "e" "E") (opt (| "+" "-")) decimals))
    (float-lit		(: decimals "." (opt decimals) (opt exponent)))
    (imaginary-lit	(: (| decimals float-lit) "i"))
    (little-u-value	(: "\\u" (= 4 hex-digit)))
    (big-u-value		(: "\\U" (= 8 hex-digit)))
    (escaped-char	(: "\\" (in "abfnrtv\\'\"")))
    ;; The Go spec's unicode-value is here split into --nodq (no double quote) and
    ;; --nosq (no single quote) versions because the spec EBNF is slightly broken.
    (unicode-value--nodq (| unicode-char--nodq little-u-value big-u-value escaped-char))
    (unicode-value--nosq (| unicode-char--nosq little-u-value big-u-value escaped-char))
    (octal-byte-value	(: "\\" octal-digit octal-digit octal-digit))
    (hex-byte-value	(: "\\x" hex-digit hex-digit))
    (byte-value		(| octal-byte-value hex-byte-value))
    (char-lit		(: "'" (| unicode-value--nosq byte-value) "'"))
    (raw-string-lit	(: "`" (* (| unicode-char--nobq newline)) "`"))
    (interpreted-string-lit (: "\"" (* (| unicode-value--nodq byte-value)) "\""))
    (string-lit		(| raw-string-lit interpreted-string-lit))
    (keyword		(|
			 ;; Sort by length, then value, so
			 ;; regexp leftmost-longest matching
			 ;; will match the longest. I.e:
			 ;; prefer 'goto' to 'go'.
			 "fallthrough"
			 "interface"
			 "continue"
			 "default" "package"
			 "import" "return" "select" "struct" "switch"
			 "break" "const" "defer" "range"
			 "case" "chan" "else" "func" "goto" "type"
			 "for" "map" "var"
			 "go" "if"))
    ;; Order is critical here: Put the longest first, to prefer longer matches.
    ;; Sort tokens of the same length, to allow rx-to-string to combine prefixes.
    (operator-or-delimiter (|
			    ;; Sort by length, then value, so
			    ;; regexp leftmost-longest matching
			    ;; will match the longest.
			    ;; I.e.: prefer '&=' to '&'
			    "&^=" "..." "<<=" ">>="
			    "!=" "%=" "&&" "&=" "&^" "*="
			    "++" "+=" "--" "-=" "/=" ":="
			    "<-" "<<" "<=" "==" ">=" ">>"
			    "^=" "|=" "||"
			    "!" "%" "&" "(" ")" "*" "+" ","
			    "-" "." "/" ":" ";" "<" "="
			    ">" "[" "]" "^" "{" "|" "}"))
    ;; A regular expression that matches the longest token possible.
    ;; The order of imaginary/float/int is critical to ensure the longest match.
    (token		(| identifier
			   imaginary-lit float-lit int-lit
			   char-lit
			   string-lit
			   operator-or-delimiter)))))

;; A regexp to match the leftmost longest token.
(setq go-token-regexp  (rx-to-string go-token  t))

;;;;;;;;;;;;;;;;
;;;; Keyword support
;;;;;;;;;;;;;;;;

; Extract the list from the SRE expression.
(defvar go-keywords (cdr go-keyword) "A list of Go keywords")

;; Create a hash table of Go keywords.
(let ((hash (make-hash-table :test 'equal :size (length go-keywords))))
  (dolist (key go-keywords) (puthash key t hash))
  (defvar go-keyword-hash hash "A hashtable holding t for each keyword string"))
  
;; Return t if str is a keyword, else nil.
(defun go-keywordp (str)
  "Return t if STR is a keyword, else nil."
  (gethash str go-keyword-hash))

;;;;;;;;;;;;;;;;
;;;; Token object
;;;;;;;;;;;;;;;;

;; Constructor
(defun go-make-token (value start-pos end-pos)
  (setq value (substring-no-properties value))
  (cons (cons value nil) (cons start-pos end-pos)))

;; Accessors
(defun go-token--value (token) (caar token))
(defun go-token--start (token) (cadr token))
(defun go-token--end   (token) (cddr token))

;; Return the type of a token, given int string VALUE.
(defun go-token-type (value)
  (let ((v (substring value 0 1)))
    (cond
     ;; Numerical types.
     ;; ((progn (message "gtt0") nil))
     ((string-match "[0-9]'" v)
      ;; ((progn (message "gtt0a") nil))
      (cond ((char-equal (aref v 0) ?')			'go-char-lit)
	    ;; ((progn (message "gtt0b") nil))
	    ((string= (substring value -1) "i")		'go-imaginary-lit)
	    ;; ((progn (message "gtt0c") nil))
	    ((string-match "\." value)			'go-float-lit)
	    ;; ((progn (message "gtt0d") nil))
	    ('go-int-lit)))
     ;; Strings
     ;; ((progn (message "gtt1") nil))
     ((string-match "[`\"]" v)				'go-string-lit)
     ;; Identifiers and keywords.
     ;; ((progn (message "gtt2") nil))
     ((string-match go-letter-regexp v)
      ;; ((progn (message "gtt2a") nil))
      (cond ((go-keywordp value)			'go-keyword)
	    ;; ((progn (message "gtt2b") nil))
	    (						'go-identifier)))
     ;; Everything else is an operator or delimiter.
     ;; ((progn (message "gtt3") nil))
     (							'go-operator-or-delimiter))))

;; Return the type of a token, one of: 'go-identifier,
;; 'go-{string,imaginary,float,int,char}-lit, 'go-keyword, or
;; 'go-operator-or-delimeter.
(defun go-token--type  (token)
  ; Lazily initialize the token type, caching the computed value.
  (cond ((cdar token)) ; <- Return cached valued if available.  v-- Cache and return.
	((setcdr (car token) (go-token-type (go-token--value token))))))

;;;;;;;;;;;;;;;;

;; Map keywords, operators, and delimiters to t if they cause a semicolon-insertion
;; at the end of lines.  Map other keywords to nil.
(let ((hash (make-hash-table :test 'equal)))
  (dolist (key go-keywords) (puthash key nil hash)) ; Some are overridden below.
  (dolist (key '("break" "continue" "fallthrough" "++" "--" ")" "]" "}"))
    (puthash key t hash))
  (defvar go-continuation-hash hash))

;; Return true iff the token at the end of a line causes a line continuation.
(defun go-continuationp (token)
  (let ((name (go-token--value token)))
    (cond ((null name) nil)
	  ((string-match "[0-9'\"]" name) t) ; int- float- imaginary- char- or string-lit
	  ((string-match go-letter-regexp name)
	   (gethash name go-continuation-hash t)) ; identifier or keyword.
	  ((gethash name go-continuation-hash nil))))) ; operators, delimiters

;; Convert a region to a list of tokens.
(defun go-tokenize (start end)
  (setq tokens nil)
  (setq prev-token-on-line nil)
  (with-current-buffer buffer
    (save-excursion
      (goto-char start)
      ;; Process text in pieces: whitespace, comments, newlines, and tokens.
      (setq this (char-after (point)))
      (while this
	(let ((nxt (char-after (1+ (point)))))
	  (cond
	   ;; Insert elided semicolons at newlines.
	   ((char-equal this ?\n)
	    (forward-char)
	    (if (go-continuationp prev-token-on-line)
		(let ((pt (point)))
		  (setq tokens (cons (go-make-token ";" pt (1+ pt)) tokens))
		  (setq prev-token-on-line nil))))
	   ;; Skip whitespace characters.
	   ((re-search-forward "\\s-" (min end (1+ (point))) t))
	   ;; Skip comments, treating them as a newline if they contain a newline.
	   ((and (char-equal this ?/)
		 (or (char-equal nxt ?/)
		     (char-equal nxt ?*)))
	    (let ((eol (save-excursion (end-of-line) (point))))
	      (forward-comment 1)
	      (let ((pt (point)))
		(if (and (> pt eol)		; Contains newline
			 (go-continuationp prev-token-on-line))
		    (progn (setq tokens (cons (go-make-token ";" pt pt) tokens))
			   (setq prev-token-on-line nil))))))
	   ;; Record and advance over tokens.
	   ((re-search-forward go-token-regexp end t)
	    (let* ((ms (match-string 0))
		   (mb (match-beginning 0))
		   (me (match-end 0))
		   (tok (go-make-token ms mb me)))
	      (setq prev-token-on-line tok)
	      (setq tokens (cons prev-token-on-line tokens))))
	   ;; Something is wrong if the token did not match.
	   (t (error "Unrecognized token at %d\n" (point))))
	  )
	(setq this (char-after (point))))
      ;; Reverse and return the token list.
      (nreverse tokens))))

(defun go-tokenize-buffer (buffer)
  ""				   ;FIXME: user-visible for debugging.
  (interactive "b")
  (let ((tokens (with-current-buffer buffer
		  (go-tokenize (point-min) (point-max)))))
    ;; (message "%S" tokens)
    tokens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Match the first token if its type is SYMBOL.
;; Returns (REMAINING-TOKENS MATCHING-TOKEN) on match, else nil.
(defun go-tok (tokens xx)
  (let ((symbol (car xx))
	(type   (go-token--type (car tokens))))
    ;; (message "go-tok %S" symbol)
    (if (eq symbol type)
	(list (cdr tokens) (car tokens))
      ;; (message "!eq %S" (list symbol type))
      nil)))
;; Match a production rule expression or subexpression X.
;; The (sub)expression will be one of:
;;   (go-[*|:] expr...) ; A subexpression.
;;   (go-tok SYMBOL) ; Match the token if its type is SYMBOL
;;   SYMBOL ; Match the production rule identified by 'SYMBOL
;;   "string" ; Match the token if it's value matches exactly.
(defun go-parse (tokens x)
  (cond
   ((listp x) (funcall (car x) tokens (cdr x)))
   ((progn (message "go-parse %S" (list x (go-token--value (car tokens)))) nil))
   ((symbolp x) (let ((p (go-parse tokens (symbol-value x))))
		  (if p
		      (list (car p) (cons x (cadr p))))))
   ((stringp x) (if (equal x (go-token--value (car tokens)))
		    (list (cdr tokens) (car tokens))))
   ((error "Invalid grammar term."))))
;; Match the SRE expressions XX in order.
;; Return (NEWTOKENS MATCHES) or nil if no match.
;; MATCHES is the list of matches.
(defun go-: (tokens xx)
  ;; (message "go-: %S" xx)
  (let (matches nu)
    ;; (message "b")
    (while (and xx (setq nu (go-parse tokens (car xx))))
      (setq tokens (car nu))
      (setq matches (cons (cadr nu) matches))
      (setq xx (cdr xx)))
    (if (and nu (not xx)) (list tokens (nreverse matches)))))
;; Match 0 or more repetitions of the sequence XX.
;; Return (NEWTOKENS MATCH).  MATCH is nil if no match.
;; PARSE is the list of 0 or more matches.
(defun go-* (tokens xx)
  ;; (message "go-* %S" xx)
  (let (matches nu)
    ;; (message "c")
    (while (setq nu (go-: tokens xx))	;Check for another match.
      (setq tokens (car nu))	   ;Record after-match token position.
      (setq matches (cons (cadr nu) matches))) ;Accumulate matches.
    (list tokens (nreverse matches))))	;Return (NEWTOKENS MATCHES). 
;; Match 0 or 1 occurance of the sequence XX
;; Return (NEWTOKENS MATCH). MATCH is nil if no match.
(defun go-opt (tokens xx)
  ;; (message "go-opt %S" xx)
  (cond ((go-: tokens xx))
	((list tokens nil))))
;; Try to match an expression in the list XX.
;; Return the first match, else nil.
(defun go-| (tokens xx)
  ;; (message "go-| %S" xx)
  (let ((match))
    ;; (message "d")
    (while (and xx (not (setq match (go-parse tokens (car xx)))))
      (setq xx (cdr xx)))
    match))

;;;;;;;;;;;;;;;;
;; Golang EBNF Grammar
;;;;;;;;;;;;;;;;

;; Copy a tree, adding 'go-' to the start of all symbols.
(defun go-prefix-symbols (tree)
  (mapcar (lambda(x) (cond ((listp   x) (go-prefix-symbols x))
			   ((symbolp x) (go-prefix-symbol    x))
			   (x)				       )) tree))

;; Copy the go production rule tree, adding the 'go-' prefix to all symbol names.
;; This allows simpler token and production-rule tables."

(setq
 go-production-rules
 (go-prefix-symbols
  '(;; This is a straightforward representation of the production rules
    ;; in the Go spec, using SRE notation and '(tok TYPE)' to match
    ;; token types.  For example, the first rule is equivalent to the
    ;; production rule 'Type = TypeName | TypeLit | "(" Type ")" .'
    (Type		(| TypeName TypeLit (: "(" Type ")")))
    (TypeName		(| (tok identifier) QualifiedIdent))
    (TypeLit		(| ArrayType StructType PointerType FunctionType
			   InterfaceType SliceType MapType ChannelType))
    (ArrayType		(: "[" ArrayLength "]" ElementType))
    (ArrayLength		Expression)
    (ElementType		Type)
    (SliceType		(: "[" "]" ElementType))
    (StructType		(: "struct" "{" (* FieldDecl ";") "}"))
    (FieldDecl		(: (| (: IdentifierList Type) AnonymousField) (opt Tag)))
    (AnonymousField	(: (opt "*") TypeName))
    (Tag		(tok string-lit))
    (PointerType	(: "*" BaseType))
    (BaseType		Type)
    (FunctionType	(: "func" Signature))
    (Signature		(: Parameters (opt Result)))
    (Parameters		(: "(" (opt ParameterList (opt ",")) ")"))
    (ParameterList	(: ParamaterDecl (* "," ParameterDecl)))
    (ParameterDecl	(: (opt IdentifierList) (opt "...") Type))
    (InterfaceType	(: "interface" "{" (* MethodSpec ";") "}"))
    (MethodSpec		(| (: MethodName Signature) InterfaceTypeName))
    (MethodName		(tok identifier))
    (InterfaceTypeName	TypeName)
    (MapType		(: "map" "[" KeyType "]" ElementType))
    (KeyType		Type)
    (ChannelType	(: (| (: "chan" (opt "<-") (: "<-" "chan"))) ElementType))
    (Block		(: "{" (* Statement ";") "}"))
    (Declaration	(| ConstDecl TypeDecl VarDecl))
    (TopLevelDecl	(| Declaration FunctionDecl MethodDecl))
    (ConstDecl		(: "const" (| ConstSpec (: "(" (* ConstSpec ";") ")"))))
    (ConstSpec		(: IdentifierList (opt (opt Type) "=" ExpressionList)))
    (IdentifierList     (: (tok identifier) (* "," (tok identifier))))
    (ExpressionList	(: Expression (* "," Expression)))
    (TypeDecl		(: "type" (| TypeSpec (: "(" (* TypeSpec ";") ")"))))
    (TypeSpec		(: (tok identifier) Type))
    (VarDecl		(: "var" (| VarSpec (: "(" (* VarSpec ";") ")"))))
    (VarSpec		(: IdentifierList (| (: Type (opt "=" ExpressionList))
					       (: "=" ExpressionList))))
    (ShortVarDecl	(: IdentifierList ":=" ExpressionList))
    (FunctionDecl	(: "func" FunctionName Signature (opt Body)))
    (FunctionName	(tok identifier))
    (Body		Block)
    (MethodDecl		(: "func" Receiver MethodName Signature (opt Body)))
    (Receiver		(: "(" (opt (tok identifier)) (opt "*") BaseTypeName ")"))
    (BaseTypeName	(tok identifier))
    (Operand		(| Literal OperandName MethodExpr (: "(" Expression ")")))
    (Literal		(| BasicLit CompositeList FunctionLit))
    (BasicLit		(| (tok int-lit) (tok float-lit) (tok imaginary-lit)
			   (tok char-lit) (tok string-lit)))
    (OperandName		(| (tok identifier) QualifiedIdent))
    (QualifiedIdent	(: PackageName "." (tok identifier)))
    (CompositeList	(: LiteralType LiteralValue))
    (LiteralType	(| StructType ArrayType (: "[" "..." "]" ElementType)
			   SliceType MapType TypeName))
    (LiteralValue	(: "{" (opt ElementList (opt ",")) "}"))
    (ElementList	(: Element (* "," Element)))
    (Element		(: (opt Key ":") Value))
    (Key		(| FieldName ElementIndex))
    (FieldName		(tok identifier))
    (ElementIndex	Expression)
    (Value		(| Expression LiteralValue))
    (FunctionLit	(: FunctionType Body))
    ;; To prevent a cycle, we had to rewrite PrimaryTarget.  FIXME:
    ;; official tree structure should be restored by postprocessing.
    (PrimaryExpr	(: (| Operand Conversion BuiltinCall)
			   (* (| Selector Index Slice TypeAssertion Call))))
    ;; (PrimaryExpr	(| Operand Conversion BuiltinCall (: PrimaryExpr Selector)
    ;; 			   (: PrimaryExpr Index) (: PrimaryExpr Slice)
    ;; 			   (: PrimaryExpr TypeAssertion) (: PrimaryExpr Call)))
    (Selector		(: "." (tok identifier)))
    (Index		(: "[" Expression "]"))
    (Slice		(: "[" (opt Expression) ":" (opt Expression) "]"))
    (TypeAssertion	(: "." "(" Type ")"))
    (Call		(: "(" (opt ArgumentList (opt ",")) ")"))
    (ArgumentList	(: ExpressionList (opt "...")))
    ;; To prevent a cycle...
    (Expression		(: UnaryExpr (* binary-op UnaryExpr)))
    ; (Expression	(| UnaryExpr (: Expression binary-op UnaryExpr)))
    (UnaryExpr		(| PrimaryExpr (: unary-op UnaryExpr)))
    (binary-op		(| "||" "&&" rel-op add-op mul-op))
    (rel-op		(| "==" "!=" "<" "<=" ">" ">="))
    (add-op		(| "+" "-" "|" "^"))
    (mul-op		(| "*" "/" "%" "<<" ">>" "&" "&^"))
    (unary-op		(| "+" "-" "!" "^" "*" "&" "<-"))
    (MethodExpr		(: ReceiverType "." MethodName))
    (ReceiverType	(| TypeName (: "(" "*" TypeName ")")))
    (Conversion		(: Type "(" Expression ")"))
    (Statement		(| Declaration LabeledStmt SimpleStmt GoStmt ReturnStmt
			   BreakStmt ContinueStmt GotoStmt FallthroughStmt Block IfStmt
			   SwitchStmt SelectStmt ForStmt DeferStmt))
    (SimpleStmt		(| EmptyStmt ExpressionStmt SendStmt IncDecStmt Assignment
			   ShortVarDecl))
    (EmptyStmt		(:))
    (LabeledStmt	(: Label ":" Statement))
    (Label		(tok identifier))
    (ExpressionStmt	Expression)
    (SendStmt		(: Channel "<-" Expression))
    (Channel		Expression)
    (IncDecStmt		(: Expression (| "++" "--")))
    (Assignment		(: ExpressionList assign-op ExpressionList))
    ;; (assign-op	(opt (| add-op mul-op)) "=")
    (assign-op		(| "+=" "-=" "|=" "^="
			   "*=" "/=" "%=" "<<=" ">>=" "&=" "&^="
			   "="))
    (IfStmt		(: "if" (opt SimpleStmt ";") Expression
			     Block (opt "else" (| IfStmt Block))))
    (SwitchStmt		(| ExprSwitchStmt TypeSwitchStmt))
    (ExprSwitchStmt	(: "switch" (opt SimpleStmt ";") (opt Expression) "{"
			     (* ExprCaseClause)
			     "}"))
    (ExprCaseClause	(: ExprSwitchCase ":" (* Statement ";")))
    (ExprSwitchCase	(| (: "case" ExpressionList) "default"))
    (TypeSwitchStmt	(: "switch" (opt SimpleStmt ";") TypeSwitchGuard "{"
			     (* TypeCaseClause)
			     "}"))
    (TypeSwitchGuard	(: (opt (tok identifier) ":=") PrimaryExpr "." "(" "type" ")"))
    (TypeCaseClause	(: TypeSwitchCase ":" (* Statement ";")))
    (TypeSwitchCase	(| (: "case" TypeList) "default"))
    (TypeList		(: Type (* "," Type)))
    (ForStmt		(: "for" (opt (| Condition ForClause RangeClause)) Block))
    (Condition		Expression)
    (ForClause		(: (opt InitStmt) ";" (opt Condition) ";" (opt PostStmt)))
    (InitStmt		SimpleStmt)
    (PostStmt		SimpleStmt)
    (RangeClause	(: Expression (opt "," Expression)
			     (| "=" ":=") "range" Expression))
    (GoStmt		(: "go" Expression))
    (SelectStmt		(: "select" "{" (* CommClause ) "}"))
    (CommClause		(: CommCase ":" (* Statement ";")))
    (CommCase		(| (: "case" (| SendStmt RecvStmt)) "default"))
    (RecvStmt		(: (opt Expression (opt "," Expression) (| "=" ":="))
			     RecvExpr))
    (RecvExpr		Expression)
    (ReturnStmt		(: "return" (opt ExpressionList)))
    (BreakStmt		(: "break" (opt Label)))
    (ContinueStmt	(: "continue" (opt Label)))
    (GotoStmt		(: "goto" Label))
    (FallthroughStmt	"fallthrough")
    (DeferStmt		(: "defer" Expression))
    (BuiltinCall	(: (tok identifier) "(" (opt BuiltinArgs (opt ",")) ")"))
    (BuiltinArgs	(| (: Type (opt "," ExpressionList)) ExpressionList))
    (SourceFile		(: PackageClause ";" (* ImportDecl ";") (* TopLevelDecl ";")))
    (PackageClause	(: "package" PackageName))
    (PackageName	(tok identifier))
    (ImportDecl		(: "import" (| ImportSpec (: "(" (* ImportSpec ";") ")"))))
    (ImportSpec		(: (opt (| "." PackageName)) ImportPath))
    (ImportPath		(tok string-lit)))))

;; Define a variable for each production rule. That is:
;; (setq go-Type '(go-| go-Typename go-TypeList (go-: "(" go-Type ")")))
;; etc.
(dolist (x go-production-rules) (set (intern (symbol-name (car x))) (cadr x)))

;; Parse the entire buffer.
(defun go-parse-buffer (buffer) ""
  (interactive "b")
  (let ((result (go-parse (go-tokenize-buffer buffer) 'go-SourceFile)))
    (message "parse: %S" (cadr result))
    result))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ;; Keywords, comments, and strings are fontified by Emacs's builtin
;;; ;; syntax table entries.  The rest are handled here.
;;; (setq go-fonts
;;;      '((go-TypeName font-lock-type-face)
;;;        ((go-ShortVarDecl IdentifierList identifier) font-lock-variable-name-face)
;;;        ((go-VarSpec IdentifierList identifier) font-lock-variable-name-face)
;;;        ((go-FunctionName) font-lock-function-face)
;;;        ((go-MethodName) font-lock-function-face)
;;;        ((go-int-lit) font-lock-const-face)
;;;        ((go-float-lit) font-lock-const-face)
;;;        ((go-imaginary-lit) font-lock-const-face)
;;;        ((go-char-lit) font-lock-const-face)
;;;        ))
;;; 
;;; ;; Map keywords to subexpression type.	These are points in the code
;;; ;; where we can easily start parsing a subtree.
;;; (setq go-fixed-points
;;;       '(("struct" go-StructType)
;;; 	("func" (| go-FunctionType go-FunctionDecl go-MethodDecl))
;;; 	("interface" go-InterfaceType)
;;; 	("map" go-MapType)
;;; 	("const" go-ConstDecl)
;;; 	("type" go-TypeDecl)
;;; 	("var" go-VarDecl)
;;; 	("switch" (| go-ExprSwithStmt go-TypeSwitchStmt))
;;; 	("for" go-ForStmt)
;;; 	("go" go-GoStmt)
;;; 	("select" go-SelectStmt)
;;; 	("return" go-ReturnStmt)
;;; 	("break" go-BreakStmt)
;;; 	("continue" go-ContinueStmt)
;;; 	("goto" go-GotoStmt)
;;; 	("fallthrough" go-FallthroughStmt)
;;; 	("defer" go-DeferStmt)
;;; 	("package" go-PackageClause)
;;; 	("import" go-ImportDecl)))

