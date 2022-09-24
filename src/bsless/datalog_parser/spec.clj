(ns bsless.datalog-parser.spec
  (:require
   [clojure.spec.gen.alpha :as gen]
   [clojure.spec.alpha :as s]))

(defmacro |
  [& ks]
  `(s/alt ~@(interleave (map (comp keyword name) ks) ks)))

(defmacro or*
  [& ks]
  `(s/or ~@(interleave (map (comp keyword name) ks) ks)))

(defmacro &&
  [& forms]
  `(s/cat ~@(for [form forms
                  :let [t (cond
                            (keyword? form) (keyword (name form))

                            (or (coll? form)
                                (symbol? form))
                            (if-let [t (->> form
                                            meta
                                            keys
                                            (filter (fn [k] (= (namespace k) (str *ns*))))
                                            first)]
                              (keyword (name t))
                              :_)

                            :else :_)]
                  x [t form]]
              x)))

(defmacro &&* [& forms] `(s/spec (&& ~@forms)))

;;; query                      = [find-spec return-map-spec? with-clause? inputs? where-clauses?]

(s/def ::query (&& ::find-spec ::return-map? ::with-clause? ::inputs? ::where-clauses?))

(s/def ::inputs? (s/? ::inputs))
(s/def ::with-clause? (s/? ::with-clause))
(s/def ::return-map? (s/? ::return-map))
(s/def ::where-clauses (s/+ ::where-clause))
(s/def ::where-clauses? (s/? ::where-clauses))

;;; find-spec                  = ':find' (find-rel | find-coll | find-tuple | find-scalar)
(s/def ::find-spec (&& #{:find} ^::find (| ::find-rel ::find-coll ::find-tuple ::find-scalar)))

;;; return-map                 = (return-keys | return-syms | return-strs)
(s/def ::return-map (| ::return-keys ::return-syms ::return-strs))

;;; find-rel                   = find-elem+
(s/def ::find-rel (s/+ ::find-elem))

;;; find-coll                  = [find-elem '...']
(s/def ::find-coll (&&* ::find-elem #{'...}))

;;; find-scalar                = find-elem '.'
(s/def ::find-scalar (&& ::find-elem #{'.}))

;;; find-tuple                 = [find-elem+]
(s/def ::find-tuple (s/spec (s/+ ::find-elem)))

;;; find-elem                  = (variable | pull-expr | aggregate)
(s/def ::find-elem (| ::variable ::pull-expr ::aggregate))

;;; return-keys                = ':keys' symbol+
(s/def ::return-keys (&& #{:keys} ^::return (s/+ symbol?)))

;;; return-syms                = ':syms' symbol+
(s/def ::return-syms (&& #{:syms} ^::return (s/+ symbol?)))

;;; return-strs                = ':strs' symbol+
(s/def ::return-strs (&& #{:strs} ^::return (s/+ symbol?)))

;;; pull-expr                  = ['pull' variable pattern]
(s/def ::pull-expr (&&* #{'pull} ::variable ::pull-pattern))

;;; pattern                    = (pattern-name | pattern-data-literal)
(s/def ::pattern (| ::pattern-name ::pattern-data-literal))

;;; aggregate                  = [aggregate-fn-name fn-arg+]
(s/def ::aggregate (&&* ::aggregate-fn-name ::fn-args))
(s/def ::aggregate-fn-name
  '#{sum count min max sample count-distinct avg median variance stddev distinct rand})

;;; fn-arg                     = (variable | constant | src-var)
(s/def ::fn-arg (| ::variable ::constant ::src-var))

;;; with-clause                = ':with' variable+
(s/def ::with-clause (&& #{:with} ::variables))

(s/def ::clauses (s/+ ::clause))

;;; where-clauses              = ':where' clause+
(s/def ::where-clause (&& #{:where} ::clauses))

;;; inputs                     = ':in' (src-var | binding | pattern-name | rules-var)+
(s/def ::inputs (&& #{:in} ^::inputs (s/+ (| ::src-var ::binding ::pattern-name ::rules-var))))

;;; src-var                    = symbol starting with "$"
(s/def ::src-var (s/with-gen
                   (s/and simple-symbol? #(= \$ (first (name %))))
                   (fn [] (gen/fmap #(str "$" (name %))  gen/symbol))))

;;; variable                   = symbol starting with "?"
(s/def ::variable (s/with-gen
                    (s/and simple-symbol? #(= \? (first (name %))))
                    (fn [] (gen/fmap #(str "?" (name %))  gen/symbol))))

(s/def ::variables (s/+ ::variable))

;;; rules-var                  = the symbol "%"
(s/def ::rules-var #{'%})

;;; plain-symbol               = symbol that does not begin with "$" or "?"
(s/def ::plain-symbol (s/and simple-symbol? #(not (#{\$ \?} (first (name %))))))

;;; pattern-name               = plain-symbol
(s/def ::pattern-name ::plain-symbol)

;;; and-clause                 = [ 'and' clause+ ]
(s/def ::and-clause (&&* #{'and} ::clauses))

;;; expression-clause          = (data-pattern | pred-expr | fn-expr | rule-expr)
(s/def ::expression-clause (| ::data-pattern ::pred-expr ::fn-expr ::rule-expr))

;;;

(s/def ::blank #{'_})
(s/def ::src-var? (s/? ::src-var))

;;; rule-expr                  = [ src-var? rule-name (variable | constant | '_')+]

(s/def ::rule-expr (&&* ::src-var? ::rule-name ^::args (s/+ (| ::variable ::constant ::blank))))

;;; not-clause                 = [ src-var? 'not' clause+ ]
(s/def ::not-clause (&&* ::src-var? #{'not} ::clauses))

;;; not-join-clause            = [ src-var? 'not-join' [variable+] clause+ ]
(s/def ::not-join-clause (&&* ::src-var? #{'not-join} ^::variables (s/spec (s/+ ::variable)) ::clauses))

;;; or-clause                  = [ src-var? 'or' (clause | and-clause)+]
(s/def ::or-clause (&&* ::src-var? #{'or} ^::clauses (s/+ (| ::clause ::and-clause))))

;;; or-join-clause             = [ src-var? 'or-join' rule-vars (clause | and-clause)+ ]
(s/def ::or-join-clause (&&* ::src-var? #{'or-join} ::rule-vars ^::clauses (s/+ (| ::clause ::and-clause))))

;;; rule-vars                  = [variable+ | ([variable+] variable*)]
(s/def ::rule-vars (s/spec (s/alt :simple (s/+ ::variable)
                                  :nested (s/spec (s/cat :nested (s/spec (s/+ ::variable))
                                                         :more (s/* ::variable))))))

;;; clause                     = (not-clause | not-join-clause | or-clause | or-join-clause | expression-clause)
(s/def ::clause (| ::not-clause ::not-join-clause ::or-clause ::or-join-clause ::expression-clause))

;;; data-pattern               = [ src-var? (variable | constant | '_')+ ]
(s/def ::data-pattern (&&* ::src-var? ^::pattern (s/+ (| ::variable ::constant ::blank))))

;;; constant                   = any non-variable data literal
(s/def ::constant
  (s/nonconforming
   (s/or :number number?
         :string string?
         :boolean boolean?
         :keyword keyword?
         :set (s/and set? (s/coll-of ::constant :into #{}))
         :vector (s/and vector? (s/coll-of ::constant :into []))
         :list (s/and list? (s/coll-of ::constant :into ()))
         :map (s/map-of ::constant ::constant))))

;;; pred-expr                  = [ [pred fn-arg+] ]
(s/def ::pred '#{even? odd? < > = <= >=})

(s/def ::fn-args (s/+ ::fn-arg))

(s/def ::pred-expr (&&* ^::expr (&&* ::pred ::fn-args)))

;;; fn-expr                    = [ [fn fn-arg+] binding]
;; (s/def ::fn '#{+ - * / count quot})
(def fns (->> 'clojure.core
              ns-publics
              (filter (fn [[_ v]] (let [m (meta v)] (and (not (:macro m)) (:arglists m)))))
              (map key)
              (into #{})))
(s/def ::fn fns)

(s/def ::fn-expr (&&* ^::expr (&&* ::fn ::fn-args) ::binding))

;;; binding                    = (bind-scalar | bind-tuple | bind-coll | bind-rel)
(s/def ::binding (| ::bind-scalar ::bind-tuple ::bind-coll ::bind-rel))

;;; bind-scalar                = variable
(s/def ::bind-scalar ::variable)

;;; bind-tuple                 = [ (variable | '_')+]
(s/def ::bind-tuple (s/spec (s/+ (| ::variable ::blank))))

;;; bind-coll                  = [variable '...']
(s/def ::bind-coll (&&* ::variable #{'...}))

;;; bind-rel                   = [ [(variable | '_')+] ]
(s/def ::bind-rel (s/tuple (s/+ (| ::variable ::blank))))

;;; rule                       = [ [rule-head clause+]+ ]
(s/def ::rule (s/spec (s/+ (&&* ::rule-head ::clauses))) )

;;; rule-head                  = [rule-name rule-vars]
(s/def ::rule-head (&&* ::rule-name ::rule-vars))

;;; rule-name                  = unqualified plain-symbol
(s/def ::rule-name ::plain-symbol)

;;; pull

;;; pattern             = [attr-spec+]
(s/def ::pull-pattern (s/+ ::attr-spec))

;;; attr-spec           = attr-name | wildcard | map-spec | attr-expr
(s/def ::attr-spec (| ::attr-name ::wildcard ::map-spec ::attr-expr))

;;; attr-name           = an edn keyword that names an attr
(s/def ::attr-name keyword?)

;;; map-spec            = { ((attr-name | attr-expr) (pattern | recursion-limit))+ }
(s/def ::map-spec (s/map-of (or* ::attr-name ::attr-expr)
                            (or* ::pull-pattern ::recursion-limit)
                            :conform-keys true))

;;; attr-spec           = attr-name | wildcard | map-spec | attr-expr
(s/def ::attr-spec (| ::attr-name ::wildcard ::map-spec ::attr-expr))

;;; attr-expr           = [attr-name attr-option+] | legacy-attr-expr
#_(s/def ::attr-expr (s/or :regular (s/cat :attr-name ::attr-name
                                         :attr-options (s/* ::attr-option))
                         :legacy ::legacy-attr-expr))
(s/def ::attr-expr (s/spec (s/cat :attr-name ::attr-name
                                  :attr-options (s/* ::attr-option))))

;;; attr-option         = as-expr | limit-expr | default-expr
(s/def ::attr-option (| ::as-expr ::limit-expr ::default-expr))

;;; as-expr             = [:as any-value]
(s/def ::as-expr (&& #{:as} ^::value any?))

;;; limit-expr          = [:limit (positive-number | nil)]
(s/def ::limit-expr (&& #{:limit} ^::limit (s/nilable pos-int?)))

;;; default-expr        = [:default any-value]
(s/def ::default-expr (&& #{:default} ^::default any?))

;;; wildcard            = "*" or '*'
(s/def ::wildcard #{"*" '*})

;;; recursion-limit     = positive-number | '...'
(s/def ::recursion-limit (s/or :number pos-int? :none #{'...}))

;;; legacy-attr-expr    = legacy-limit-expr | legacy-default-expr
;;; legacy-limit-expr   = [("limit" | 'limit') attr-name (positive-number | nil)]
;;; legacy-default-expr = [("default" | 'default') attr-name any-value]
