((name syntx-utl) (dependencies (syntx-env errors extras0 arith eulisp0 props pass class-names init list-fns boot abs-syntx)) (exported-ids ((address mod-cache import-module) (name . import-module) (mutable ()) (class . unknown)) ((address comp-utl get-module-stream) (name . get-module-stream) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address mod-cache read-exportations) (name . read-exportations) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address mod-cache imported-module-props) (name . imported-module-props) (mutable ()) (class . unknown)) ((address abs-syntx syntactic-properties) (name . syntactic-properties) (mutable ()) (class . unknown)) ((address abs-syntx make-syntactic-properties) (name . make-syntactic-properties) (mutable ()) (class . unknown)) ((address abs-syntx assignment-term) (name . assignment-term) (mutable ()) (class . unknown)) ((address abs-syntx sequence) (name . sequence) (mutable ()) (class . unknown)) ((address abs-syntx lambda-term) (name . lambda-term) (mutable ()) (class . unknown)) ((address abs-syntx block-term) (name . block-term) (mutable ()) (class . unknown)) ((address abs-syntx block-obj) (name . block-obj) (mutable ()) (class . unknown)) ((address abs-syntx applic-term) (name . applic-term) (mutable ()) (class . unknown)) ((address abs-syntx condition-term) (name . condition-term) (mutable ()) (class . unknown)) ((address abs-syntx error-term) (name . error-term) (mutable ()) (class . unknown)) ((address abs-syntx make-error-term) (name . make-error-term) (mutable ()) (class . bytefunction) (argtype (() . 0))) ((address abs-syntx literal-term) (name . literal-term) (mutable ()) (class . unknown)) ((address abs-syntx ident-term) (name . ident-term) (mutable ()) (class . unknown)) ((address abs-syntx term) (name . term) (mutable ()) (class . unknown)) ((address abs-syntx syntax-obj) (name . syntax-obj) (mutable ()) (class . unknown)) ((address abs-syntx extended-lambda-term) (name . extended-lambda-term) (mutable ()) (class . unknown)) ((address abs-syntx extended-lambda-comment) (name . extended-lambda-comment) (mutable ()) (class . unknown)) ((address abs-syntx extended-lambda-name) (name . extended-lambda-name) (mutable ()) (class . unknown)) ((address abs-syntx make-extended-lambda) (name . make-extended-lambda) (mutable ()) (class . bytefunction) (argtype (() . 4))) ((address abs-syntx assignment) (name . assignment) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address abs-syntx make-sequence) (name . make-sequence) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address abs-syntx macro-lambda-term) (name . macro-lambda-term) (mutable ()) (class . unknown)) ((address abs-syntx make-macro-lambda) (name . make-macro-lambda) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address abs-syntx make-lambda) (name . make-lambda) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address abs-syntx make-block) (name . make-block) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address abs-syntx make-applic) (name . make-applic) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address abs-syntx make-cond) (name . make-cond) (mutable ()) (class . bytefunction) (argtype (() . 3))) ((address abs-syntx literal) (name . literal) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address abs-syntx literal-p) (name . literal-p) (mutable ()) (class . unknown)) ((address abs-syntx ident) (name . ident) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address abs-syntx ident-p) (name . ident-p) (mutable ()) (class . unknown)) ((address abs-syntx special-term) (name . special-term) (mutable ()) (class . unknown)) ((address abs-syntx special-term-objects) (name . special-term-objects) (mutable ()) (class . unknown)) ((address abs-syntx special-term-data) (name . special-term-data) (mutable ()) (class . unknown)) ((address abs-syntx special-term-name) (name . special-term-name) (mutable ()) (class . unknown)) ((address abs-syntx mk-special-term) (name . mk-special-term) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address abs-syntx mk-special-term2) (name . mk-special-term2) (mutable ()) (class . bytefunction) (argtype (() . 3))) ((address abs-syntx error-term-p) (name . error-term-p) (mutable ()) (class . unknown)) ((address abs-syntx leaf-term) (name . leaf-term) (mutable ()) (class . unknown)) ((address abs-syntx leaf-term-p) (name . leaf-term-p) (mutable ()) (class . unknown)) ((address abs-syntx term-p) (name . term-p) (mutable ()) (class . unknown)) ((address abs-syntx term-id) (name . term-id) (mutable ()) (class . unknown)) ((address abs-syntx literal-content) (name . literal-content) (mutable ()) (class . unknown)) ((address abs-syntx cond-f-part) (name . cond-f-part) (mutable ()) (class . unknown)) ((address abs-syntx cond-t-part) (name . cond-t-part) (mutable ()) (class . unknown)) ((address abs-syntx cond-test) (name . cond-test) (mutable ()) (class . unknown)) ((address abs-syntx lambda-body) (name . lambda-body) (mutable ()) (class . unknown)) ((address abs-syntx lambda-ids) (name . lambda-ids) (mutable ()) (class . unknown)) ((address abs-syntx applic-args) (name . applic-args) (mutable ()) (class . unknown)) ((address abs-syntx applic-fun) (name . applic-fun) (mutable ()) (class . unknown)) ((address abs-syntx block-decl) (name . block-decl) (mutable ()) (class . unknown)) ((address abs-syntx block-body) (name . block-body) (mutable ()) (class . unknown)) ((address abs-syntx sequence-body) (name . sequence-body) (mutable ()) (class . unknown)) ((address abs-syntx sequence-end) (name . sequence-end) (mutable ()) (class . unknown)) ((address abs-syntx sequence-content) (name . sequence-content) (mutable ()) (class . unknown)) ((address abs-syntx assign-body) (name . assign-body) (mutable ()) (class . unknown)) ((address abs-syntx assign-var) (name . assign-var) (mutable ()) (class . unknown)) ((address abs-syntx lambda-id) (name . lambda-id) (mutable ()) (class . unknown)) ((address abs-syntx lambda-id-name) (name . lambda-id-name) (mutable ()) (class . unknown)) ((address abs-syntx make-lambda-id) (name . make-lambda-id) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address abs-syntx call-next-method-term) (name . call-next-method-term) (mutable ()) (class . unknown)) ((address abs-syntx mk-call-next-method-term) (name . mk-call-next-method-term) (mutable ()) (class . bytefunction) (argtype (() . 0))) ((address abs-syntx rec-decl) (name . rec-decl) (mutable ()) (class . unknown)) ((address abs-syntx and-decl) (name . and-decl) (mutable ()) (class . unknown)) ((address abs-syntx definition) (name . definition) (mutable ()) (class . unknown)) ((address abs-syntx module-definition) (name . module-definition) (mutable ()) (class . unknown)) ((address abs-syntx decl) (name . decl) (mutable ()) (class . unknown)) ((address abs-syntx rec-decl-p) (name . rec-decl-p) (mutable ()) (class . unknown)) ((address abs-syntx and-decl-p) (name . and-decl-p) (mutable ()) (class . unknown)) ((address abs-syntx local-definition) (name . local-definition) (mutable ()) (class . unknown)) ((address abs-syntx definition-p) (name . definition-p) (mutable ()) (class . unknown)) ((address abs-syntx abs-definition) (name . abs-definition) (mutable ()) (class . unknown)) ((address abs-syntx abs-definition-p) (name . abs-definition-p) (mutable ()) (class . unknown)) ((address abs-syntx decl-p) (name . decl-p) (mutable ()) (class . unknown)) ((address abs-syntx make-definition) (name . make-definition) (mutable ()) (class . bytefunction) (argtype (() . 3))) ((address abs-syntx defn-mutable-p) (name . defn-mutable-p) (mutable ()) (class . unknown)) ((address abs-syntx defn-body) (name . defn-body) (mutable ()) (class . unknown)) ((address abs-syntx add-defn-prop) (name . add-defn-prop) (mutable ()) (class . bytefunction) (argtype (() . 3))) ((address abs-syntx defn-prop-ref) (name . defn-prop-ref) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address abs-syntx defn-properties) (name . defn-properties) (mutable ()) (class . unknown)) ((address abs-syntx defn-ide) (name . defn-ide) (mutable ()) (class . unknown)) ((address abs-syntx make-module-definition) (name . make-module-definition) (mutable ()) (class . bytefunction) (argtype (() . 3))) ((address abs-syntx module-definition-p) (name . module-definition-p) (mutable ()) (class . unknown)) ((address abs-syntx and-decl-decls) (name . and-decl-decls) (mutable ()) (class . unknown)) ((address abs-syntx make-and-decl) (name . make-and-decl) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address abs-syntx rec-decl-decl) (name . rec-decl-decl) (mutable ()) (class . unknown)) ((address abs-syntx make-rec-decl) (name . make-rec-decl) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address abs-syntx module-block) (name . module-block) (mutable ()) (class . unknown)) ((address abs-syntx module-body) (name . module-body) (mutable ()) (class . unknown)) ((address abs-syntx module-export-spec) (name . module-export-spec) (mutable ()) (class . unknown)) ((address abs-syntx module-syntax-spec) (name . module-syntax-spec) (mutable ()) (class . unknown)) ((address abs-syntx module-import-spec) (name . module-import-spec) (mutable ()) (class . unknown)) ((address abs-syntx union-directive) (name . union-directive) (mutable ()) (class . unknown)) ((address abs-syntx union-content) (name . union-content) (mutable ()) (class . unknown)) ((address abs-syntx make-union-directive) (name . make-union-directive) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address abs-syntx only-directive) (name . only-directive) (mutable ()) (class . unknown)) ((address abs-syntx only-imports) (name . only-imports) (mutable ()) (class . unknown)) ((address abs-syntx only-name-list) (name . only-name-list) (mutable ()) (class . unknown)) ((address abs-syntx make-only-directive) (name . make-only-directive) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address abs-syntx except-directive) (name . except-directive) (mutable ()) (class . unknown)) ((address abs-syntx except-imports) (name . except-imports) (mutable ()) (class . unknown)) ((address abs-syntx except-name-list) (name . except-name-list) (mutable ()) (class . unknown)) ((address abs-syntx make-except-directive) (name . make-except-directive) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address abs-syntx rename-directive) (name . rename-directive) (mutable ()) (class . unknown)) ((address abs-syntx rename-imports) (name . rename-imports) (mutable ()) (class . unknown)) ((address abs-syntx rename-name-list) (name . rename-name-list) (mutable ()) (class . unknown)) ((address abs-syntx make-rename-directive) (name . make-rename-directive) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address abs-syntx import-directive) (name . import-directive) (mutable ()) (class . unknown)) ((address abs-syntx import-directive-name) (name . import-directive-name) (mutable ()) (class . unknown)) ((address abs-syntx make-import-directive) (name . make-import-directive) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address abs-syntx export-directive) (name . export-directive) (mutable ()) (class . unknown)) ((address abs-syntx export-spec-name) (name . export-spec-name) (mutable ()) (class . unknown)) ((address abs-syntx make-export-directive) (name . make-export-directive) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address abs-syntx export-spec) (name . export-spec) (mutable ()) (class . unknown)) ((address abs-syntx syntax-spec-p) (name . syntax-spec-p) (mutable ()) (class . unknown)) ((address abs-syntx make-syntax-spex) (name . make-syntax-spex) (mutable ()) (class . bytefunction) (argtype (() . 0))) ((address abs-syntx import-spec) (name . import-spec) (mutable ()) (class . unknown)) ((address abs-syntx import-spec-p) (name . import-spec-p) (mutable ()) (class . unknown)) ((address abs-syntx module-name) (name . module-name) (mutable ()) (class . unknown)) ((address abs-syntx module-p) (name . module-p) (mutable ()) (class . unknown)) ((address abs-syntx make-module) (name . make-module) (mutable ()) (class . bytefunction) (argtype (t . 6))) ((address abs-syntx expose-directive) (name . expose-directive) (mutable ()) (class . unknown)) ((address abs-syntx expose-spec-importer) (name . expose-spec-importer) (mutable ()) (class . unknown)) ((address abs-syntx make-expose-directive) (name . make-expose-directive) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address abs-syntx print-term) (name . print-term) (mutable ()) (class . unknown)) ((address abs-syntx print-decl) (name . print-decl) (mutable ()) (class . unknown)) ((address syntx-env module-imports-list) (name . module-imports-list) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-env module-import-desc) (name . module-import-desc) (mutable ()) (class . unknown)) ((address syntx-env imported-definition) (name . imported-definition) (mutable ()) (class . unknown)) ((address syntx-env imported-defn-p) (name . imported-defn-p) (mutable ()) (class . unknown)) ((address syntx-env external-name) (name . external-name) (mutable ()) (class . unknown)) ((address abs-syntx defn-prop-ref) (name . defn-prop-ref) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address syntx-env import-function-nargs) (name . import-function-nargs) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-env import-defn-setter) (name . import-defn-setter) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-env import-object-type) (name . import-object-type) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-env import-real-name) (name . import-real-name) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-env import-home) (name . import-home) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-env compile-macro-expand) (name . compile-macro-expand) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address syntx-env find-name) (name . find-name) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address syntx-env find-macro) (name . find-macro) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address syntx-env expander) (name . expander) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-env read-imports) (name . read-imports) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-env flatten-imports) (name . flatten-imports) (mutable ()) (class . unknown)) ((address syntx-env module-imported-names) (name . module-imported-names) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-env module-exported-names) (name . module-exported-names) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-utl find-decls) (name . find-decls) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-utl enclosing-lambda) (name . enclosing-lambda) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-utl get-internal-closed-bindings) (name . get-internal-closed-bindings) (mutable ()) (class . unknown)) ((address syntx-utl find-fn) (name . find-fn) (mutable ()) (class . unknown)) ((address syntx-utl function-nary-p) (name . function-nary-p) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-utl function-nargs) (name . function-nargs) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-utl function-prop) (name . function-prop) (mutable ()) (class . bytefunction) (argtype (() . 2))) ((address syntx-utl function-type) (name . function-type) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-utl function-fn) (name . function-fn) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-utl decl-class) (name . decl-class) (mutable ()) (class . bytefunction) (argtype (() . 1))) ((address syntx-utl add-dependency) (name . add-dependency) (mutable ()) (class . bytefunction) (argtype (() . 2)))))