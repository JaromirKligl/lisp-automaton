(defsystem "automation"
  :description "automation: finite automata in Cl."
  :version "0.0.1"
  :author "Jaromir Kligl <jarek.kligl7@gmail.com>"
  :components ((:file "alphabet")
               (:file "automaton" :depends-on ("alphabet"))))
  