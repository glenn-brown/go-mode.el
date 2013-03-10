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

;;;;;;;;;;;;;;;;
;;; Go tokens, in SRE form.
;;; We use the following SRE tokens:
;;; ':' Sequence
;;; '|' First match among alternatives.
;;; '*' Match zero-or-more alternatives.  Prefer more.
;;; 'opt' Match 0 or 1 occurences.  Prefer 1.
;;; '+' Match 1 or more occurences.  Prefer more.
;;;;;;;;;;;;;;;;
(defvar
 go-tokens
 '(
   ;; We don't use the -comment tokens.  Instead, our tokenizer reilies
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
   (unicode-value--nodq	(| unicode-char--nodq little-u-value big-u-value escaped-char))
   (unicode-value--nosq	(| unicode-char--nosq little-u-value big-u-value escaped-char))
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
			   operator-or-delimiter))))

;; For each TOKEN in the table above, define go-TOKEN to be its SRE representation.
(dolist (x go-tokens) (set (go-prefix-symbol (car x)) (go-valuate (cadr x))))

;; Create regular expressions matching some important patterns.
(setq go-token-regexp  (rx-to-string go-token  t)) ; longest token at start
(setq go-letter-regexp (rx-to-string go-letter t)) ; start of an identifier

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
  (cons (cons nil value) (cons start-pos end-pos)))

;; Accessors
(defun go-token-value (token) (cdar token))
(defun go-token-start (token) (cadr token))
(defun go-token-end   (token) (cddr token))

;; Return the type of a token, given int string VALUE.
(defun go--token-type (value)
  (let ((s (substring s 0 1)))
    (cond
     ;; Numerical types.
     ((string-match "[0-9]'" s)
      (cond ((eql (aref s 0) ?')			'go-char-lit)
	    ((equal (substring str -1) "i")		'go-imaginary-lit)
	    ((string-match "\." str)			'go-float-lit)
	    ('go-int-lit)))
     ;; Strings
     ((string-match "[`\"]" s)				'go-string-lit)
     ;; Identifiers and keywords.
     ((string-match go-letter-regexp s)
      (cond ((go-keywordp str)				'go-keyword)
	    (						'go-identifier)))
     ;; Everything else is an operator or delimiter.
     (							'go-operator-or-delimiter))))

;; Return the type of a token, one of: 'go-identifier,
;; 'go-{string,imaginary,float,int,char}-lit, 'go-keyword, or
;; 'go-operator-or-delimeter.
(defun go-token-type  (token)
  ; Lazily initialize the token type, caching the computed value.
  (cond ((caar token)) ; <- Return cached valued if available.  v-- Cache and return.
	((setcar (car token) (go--token-type (go-token-value (token)))))))

;;;;;;;;;;;;;;;;

;; Map keywords, operators, and delimiters to t if they cause a semicolon-insertion
;; at the end of lines.  Map other keywords to nil.
(let ((hash (make-hash-table :test 'equal)))
  (dolist (key go-keywords) (puthash key nil hash))
  (puthash "break" t hash)
  (puthash "continue" t hash)
  (puthash "fallthrough" t hash)
  (puthash "return" t hash)
  (puthash "++" t hash)
  (puthash "--" t hash)
  (puthash ")" t hash)
  (puthash "]" t hash)
  (puthash "}" t hash)
  (defvar go-continuation-hash hash))

;; Return true iff the token at the end of a line causes a line continuation.
(defun go-continuationp (token)
  (let ((name (go-token-value token)))
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
	   ;; Skip whitespace characters.
	   ((re-search-forward "\\s-+" (min end (1+ (point))) t))
	   ;; Insert elided semicolons at newlines.
	   ((eql this ?\n)
	    (forward-char)
	    (if (go-continuationp prev-token-on-line)
		(let ((pt (point)))
		  (setq tokens (cons (go-make-token ";" pt (1+ pt)) tokens))
		  (setq prev-token-on-line nil))))
	   ;; Skip comments, treating them as a newline if they contain a newline.
	   ((and (eql this ?/)
		 (or (eql nxt ?/)
		     (eql nxt ?*)))
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
  (with-current-buffer buffer
    (go-tokenize (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Match 0 or more repetitions of SRE.
;; Return (NEWTOKENS PARSE).
;; PARSE is the list of 0 or more matches.
(defun go-* (tokens sre)
  (let* ((result (list tokens nil)
	 (nu (go-apply-sre tokens sre)))
    ;; Scan for as many matches as possible, accumulating the tentative
    ;; result, which should me (UNCONSUMED-TOKENS REVERSED-PARSE-MATCH-LIST).
    ;; Correct the reversed list upon returning.
    (while nu
      (if (not nu)
	  (list (car result) (nreverse (cadr result)))
	(setq result (list (car nu) (cons (cadr nu) (cadr result))))
	(setq nu (go-apply-sre (car nu) sre)))))))
;; Match the sequence of SRE expressions.
;; Return (NEWTOKENS PARSE) or nil if no match.
;; PARSE is the list of matches.
(defun go-: (tokens sre)
  (let* ((result (list tokens nil))
	 (s (car sre))
	 (sre (cdr sre)))
    (while s
      (let (nu (go-apply-sre tokens s))
	(if (not nu)
	    nil
	  (setq result (list (car nu) (cons (cadr nu) (cadr result))))
	  (setq s (car sre))
	  (setq sre (cdr sre)))))
    (list (car result) (nreverse (cadr result)))))
;; Match 0 or 1 occurance of SRE
;; Return (NEWTOKENS PARSE).
;; Parse is the list of 0 or 1 matches.
(defun go-opt (tokens sre)
  (let ((nu (go-apply-sre tokens sre)))
    (if nu
	nu
      (list tokens nil))))
;; Try to match an sre in the list SRE.  Elements are tried in order
;; and the first match is returned.
(defun go-| (tokens sre)
  (let* ((result nil) (s (car sre)) (sre (cdr sre)))
    (while (and s (not result))
      (setq result (go-apply-sre tokens s))
      (setq s (car sre))
      (setq sre (cdr sre)))
    result))
;; Try to match the regular expression (car SRE) to the first token (car TOKENS).
;; Returns (NEW-TOKENS STRING)
(defun go-tok (tokens sre)
  (let* ((re (car sre))
	 (str (go-token-value (car tokens)))
	 (match (match-string re str))
	 (matched (and match (eq (match-end 0) (length str)))))
    (if matched (list (cdr tokens) str) nil)))

;;;;;;;;;;;;;;;;
;; Golang EBNF Grammar
;;;;;;;;;;;;;;;;

(setq
 go-raw-production-rules
 '(;; Here, terminals (tokens) are writen '(token-name)' to distinguish
   ;; them from Nonterminals, which is what we are defining here.
   ;; This is a simple representation of the Go EBNF specification.
   ;; For example, the first rule is equivalent to the production rule
   ;; 'Type = TypeName | TypeLit | "(" Type ")" .'
   (Type		(| TypeName TypeLit (: "(" Type ")")))
   (TypeName		(| (tok identifier) QualifiedIdent))
   (TypeLit		(| ArrayType StructType PointerType FunctionType
			   InterfaceType SliceType MapType ChannelType))
   (ArrayType		(: "[" ArrayLength "]" ElementType))
   (ArrayLength		Expression)
   (ElementType		Type)
   (SliceType		(: "[" "]" ElementType))
   (StructType		(: "struct" "{" (* (: FieldDecl ";")) "}"))
   (FieldDecl		(: (| (: IdentifierList Type) AnonymousField) (opt Tag)))
   (AnonymousField	(: (opt "*") TypeName))
   (Tag			(tok string-lit))
   (PointerType		(: "*" BaseType))
   (BaseType		Type)
   (FunctionType	(: "func" Signature))
   (Signature		(: Parameters (opt Result)))
   (Parameters		(: "(" (opt ParameterList (opt ",")) ")"))
   (ParameterList	(: ParamaterDecl (* (: "," ParameterDecl))))
   (ParameterDecl	(: (opt IdentifierList) (opt "...") Type))
   (InterfaceType	(: "interface" "{" (* (: MethodSpec ";")) "}"))
   (MethodSpec		(| (: MethodName Signature) InterfaceTypeName))
   (MethodName		(tok identifier))
   (InterfaceTypeName	TypeName)
   (MapType		(: "map" "[" KeyType "]" ElementType))
   (KeyType		Type)
   (ChannelType		(: (| (: "chan" (opt "<-") (: "<-" "chan"))) ElementType))
   (Block		(: "{" (* (: Statement ";")) "}"))
   (Declaration		(| ConstDecl TypeDecl VarDecl))
   (TopLevelDecl	(| Declaration FunctionDecl MethodDecl))
   (ConstDecl		(: "const" (| ConstSpec (: "(" (* (: ConstSpec ";")) ")"))))
   (ConstSpec		(: IdentifierList (opt (: (opt Type) "=" ExpressionList))))
   (tok identifierList  (: (tok identifier) (* (: "," (tok identifier)))))
   (ExpressionList	(: Expression (* (: "," Expression))))
   (TypeDecl		(: "type" (| TypeSpec (: "(" (* (: TypeSpec ";")) ")"))))
   (TypeSpec		(: (tok identifier) Type))
   (VarDecl		(: "var" (| VarSpec (: "(" (* (: VarSpec ";")) ")"))))
   (VarSpec		(: IdentifierList (| (: Type (opt (: "=" ExpressionList)))
					     (: "=" ExpressionList))))
   (ShortVarDecl	(: IdentifierList ":=" ExpressionList))
   (FunctionDecl	(: "func" FunctionName Signature (opt Body)))
   (FunctionName	(tok identifier))
   (Body		Block)
   (MethodDecl		(: "func" Receiver MethodName Signature (opt Body)))
   (Receiver		(: "(" (opt (tok identifier)) (opt "*") BaseTypeName ")"))
   (BaseTypeName	(tok identifier))
   (Operand		(| Literal OperandName MethodExpr (: "(" Expression ")")))
   (Literal		(| BasicLit CompositList FunctionLit))
   (BasicLit		(| (tok int-lit) (tok float-lit) (tok imaginary-lit)
			   (tok char-lit) (tok string-lit)))
   (OperandName		(| (tok identifier) QualifiedIdent))
   (QualifiedIdent	(: PackageName "." (tok identifier)))
   (CompositeLit	(: LiteralType LiteralValue))
   (LiteralType		(| StructType ArrayType (: "[" "..." "]" ElementType)
			   SliceType MapType TypeName))
   (LiteralValue	(: "{" (opt (: ElementList (opt ","))) "}"))
   (ElementList		(: Element (* (: "," Element))))
   (Element		(: (opt (: Key ":")) Value))
   (Key			(| FieldName ElementIndex))
   (FieldName		(tok identifier))
   (ElementIndex	Expression)
   (Value		(| Expression LiteralValue))
   (FunctionLit		(: FunctionType Body))
   (PrimaryExpr		(| Operand Conversion BuiltinCall (: PrimaryExpr Selector)
			   (: PrimaryExpr Index) (: PrimaryExpr Slice)
			   (: PrimaryExpr TypeAssertion) (: PrimaryExpr Call)))
   (Selector		(: "." (tok identifier)))
   (Index		(: "[" Expression "]"))
   (Slice		(: "[" (opt Expression) ":" (opt Expression) "]"))
   (TypeAssertion	(: "." "(" Type ")"))
   (Call		(: "(" (opt (: ArgumentList (opt ","))) ")"))
   (ArgumentList	(: ExpressionList (opt "...")))
   (Expression		(| UnaryExpr (: Expression binary-op UnaryExpr)))
   (UnaryExpr		(| PrimaryExpr (: unary-op UnaryExpr)))
   ;; FIXME: These are funky, exprected to match partial-tokens.
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
   (LabeledStmt		(: Label ":" Statement))
   (Label		(tok identifier))
   (ExpressionStmt	Expression)
   (SendStmt		(: Channel "<-" Expression))
   (Channel		Expression)
   (IncDecStmt		(: Expression (| "++" "--")))
   (Assignment		(: ExpressionList assign-op ExpressionList))
   ;; (assign-op		(opt (| add-op mul-op)) "=")
   (assign-op		(| "+=" "-=" "|=" "^="
			   "*=" "/=" "%=" "<<=" ">>=" "&=" "&^="
			   "="))
   (IfStmt		(: "if" (opt (: SimpleStmt ";")) Expression
			   Block (opt (: "else" (| IfStmt Block)))))
   (SwitchStmt		(| ExprSwitchStmt TypeSwitchStmt))
   (ExprSwitchStmt	(: "switch" (opt (: SimpleStmt ";")) (opt Expression) "{"
			   (* ExprCaseClause)
			   "}"))
   (ExprCaseClause	(: ExprSwitchCase ":" (* (: Statement ";"))))
   (ExprSwitchCase	(| (: "case" ExpressionList) "default"))
   (TypeSwitchStmt	(: "switch" (opt (: SimpleStmt ";")) TypeSwitchGuard "{"
			   (* TypeCaseClause)
			   "}"))
   (TypeSwitchGuard	(: (opt (tok identifier) ":=") PrimaryExpr "." "(" "type" ")"))
   (TypeCaseClause	(: TypeSwitchCase ":" (* (: Statement ";"))))
   (TypeSwitchCase	(| (: "case" TypeList) "default"))
   (TypeList		(: Type (* (: "," Type))))
   (ForStmt		(: "for" (opt (| Condition ForClause RangeClause)) Block))
   (Condition		Expression)
   (ForClause		(: (opt InitStmt) ";" (opt Condition) ";" (opt PostStmt)))
   (InitStmt		SimpleStmt)
   (PostStmt		SimpleStmt)
   (RangeClause		(: Expression (opt (: "," Expression))
			   (| "=" ":=") "range" Expression))
   (GoStmt		(: "go" Expression))
   (SelectStmt		(: "select" "{" (* CommClause ) "}"))
   (CommClause		(: CommCase ":" (* (: Statement ";"))))
   (CommCase		(| (: "case" (| SendStmt RecvStmt)) "default"))
   (RecvStmt		(: (opt (: Expression (opt (: "," Expression)) (| "=" ":=")))
			   RecvExpr))
   (RecvExpr		Expression)
   (ReturnStmt		(: "return" (opt ExpressionList)))
   (BreakStmt		(: "break" (opt Label)))
   (ContinueStmt	(: "continue" (opt Label)))
   (GotoStmt		(:"goto" Label))
   (FallthroughStmt	"fallthrough")
   (DeferStmt		(: "defer" Expression))
   (BuiltinCall		(: (tok identifier) "(" (opt (: BuiltinArgs (opt ","))) ")"))
   (BuiltinArgs		(| (: Type (opt (: "," ExpressionList))) ExpressionList))
   (SourceFile		(: PackageClause ";" (* (: ImportDecl ";")) (* (: TopLevelDecl
									  ";"))))
   (PackageClause	(: "package" PackageName))
   (PackageName		(tok identifier))
   (ImportDecl		(: "import" (| ImportSpec (: "(" (* (: ImportSpec ";")) ")"))))
   (ImportSpec		(: (opt (| "." PackageName)) ImportPath))
   (ImportPath		(tok string-lit))))

;; Copy the go production rule tree, adding the 'go-' prefix to all symbol names.
;; This allows simpler token and production-rule tables."
(defun go-prefix-prodrules (tree)
  (mapcar (lambda(x) (cond ((listp   x) (go-prefix-prodrules x))
			   ((symbolp x) (go-prefix-symbol    x))
			   (x)				       )) tree))

;; Put 'go-' prefix on all symbols in the production rules.
(setq go-production-rules (go-prefix-prodrules go-raw-production-rules))

;; Define a variable for each production rule. That is:
;; (setq go-Type (go-| go-Typename go-TypeList (go-: "(" go-Type ")")))
;; etc.
(dolist (x go-production-rules) (setq (intern (symbol-name (car x))) (cadr x)))

;; Keywords, comments, and strings are handled by syntax table entries.
;; The rest are handled here.
(setq go-fonts
     '((go-TypeName font-lock-type-face)
       ((go-ShortVarDecl IdentifierList identifier) font-lock-variable-name-face)
       ((go-VarSpec IdentifierList identifier) font-lock-variable-name-face)
       ((go-FunctionName) font-lock-function-face)
       ((go-MethodName) font-lock-function-face)
       ((go-int-lit) font-lock-const-face)
       ((go-float-lit) font-lock-const-face)
       ((go-imaginary-lit) font-lock-const-face)
       ((go-char-lit) font-lock-const-face)
       ))

;; Map keywords to subexpression type.	These are points in the code
;; where we can easily start parsing a subtree.
(setq go-fixed-points
     (go-prefix
      '(("struct" StructType)
       ("func" (| FunctionType FunctionDecl MethodDecl))
       ("interface" InterfaceType)
       ("map" MapType)
       ("const" ConstDecl)
       ("type" TypeDecl)
       ("var" VarDecl)
       ("switch" (| ExprSwithStmt TypeSwitchStmt))
       ("for" ForStmt)
       ("go" GoStmt)
       ("select" SelectStmt)
       ("return" ReturnStmt)
       ("break" BreakStmt)
       ("continue" ContinueStmt)
       ("goto" GotoStmt)
       ("fallthrough" FallthroughStmt)
       ("defer" DeferStmt)
       ("package" PackageClause)
       ("import" ImportDecl))))

;;; SRE processing functions.  These take TOKENS SRE as arguments,
;;; and return (NEW-TOKENS PARSE-TREE).

;;; prodrule Object

(defun go-make-prodrule (nonterminal terms) (list nonterminal terms))
(defun go-prodrule-nonterminal (prodrule) (car prodrule))
(defun go-prodrule-terms (prodrule) (cdr prodrule))

;;; Parser object.

(defun go-parser-label-to-parser-hash (parser) (car parser))
(defun go-make-parser (grammar)
  (let ((table (make-hash-table)))
    (mapcar (lambda (x) (puthash (car x) (cdr x) table)))
    table))

; Return a parse tree, which looks like
; (TYPE SUBTREE...)
(defun go-parser-parse (parser label tokens)
  (let* ((rule (gethash label parser))
	 (cmd (car rule))
	 (args (cdr rule)))
    (cmd label tokens . args)))

(defun go-seq (parser production-rule tokens)
  (let ((nonterminal-name (car production-rule))
	(args (cdr production-rule)))
    (while args
      (let ((a (car args)))
	(setq args (cdr args))
	(let ((match (go-match a tokens))) ; FIXME advance tokens.
	  (if match
	      (progn (setq result (cons match result))
		     (setq tokens (cdr tokens)))
	    (setq result nil)
	    (setq args nil)))))))

(defun go-parser-match (parser term tokens)
  (cond ((stringp term) (if (eql term (token-value)) term nil)
	((listp term) (let* ((cmd (car term))
			     (terms (cdr term)))
			(cmd parser terms)))
	((symbolp term) (go-parser-parse (parser term, tokens)))
	(t (error "Internal error: malformed term %S" term)))))

; We will need this.
; (put-text-property start end 'face face)
